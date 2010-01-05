/* sockets.c -- BSD sockets plugin

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#if !defined (AF_LOCAL) && defined (AF_UNIX)
# define AF_LOCAL AF_UNIX
#endif
#if !defined (PF_LOCAL) && defined (PF_UNIX)
# define PF_LOCAL PF_UNIX
#endif

#ifdef DEBUG
# define DB(x) printf x
#else
# define DB(x)
#endif

typedef struct rep_socket_struct rep_socket;

static rep_socket *socket_list;
static int socket_type;

struct rep_socket_struct {
    repv car;
    rep_socket *next;

    int sock;
    int namespace, style;
    repv addr, port;
    repv p_addr, p_port;
    repv stream, sentinel;
};

#define IS_ACTIVE		(1 << (rep_CELL16_TYPE_BITS + 0))
#define IS_REGISTERED		(1 << (rep_CELL16_TYPE_BITS + 1))
#define SOCKET_IS_ACTIVE(s)	((s)->car & IS_ACTIVE)
#define SOCKET_IS_REGISTERED(s)	((s)->car & IS_REGISTERED)

#define SOCKETP(x)		rep_CELL16_TYPEP (x, socket_type)
#define SOCKET(x)		((rep_socket *) rep_PTR (x))

#define ACTIVE_SOCKET_P(x)	(SOCKETP (x) \
				 && (SOCKET_IS_ACTIVE (SOCKET (x))))


/* data structure management */

static rep_socket *
make_socket_ (int sock_fd, int namespace, int style)
{
    rep_socket *s = rep_ALLOC_CELL (sizeof (rep_socket));
    rep_data_after_gc += sizeof (rep_socket);

    s->car = socket_type | IS_ACTIVE;
    s->sock = sock_fd;
    s->namespace = namespace;
    s->style = style;
    s->addr = rep_NULL;
    s->p_addr = rep_NULL;
    s->sentinel = s->stream = Qnil;

    s->next = socket_list;
    socket_list = s;

    rep_unix_set_fd_cloexec (sock_fd);

    DB (("made socket proxy for fd %d\n", s->sock));

    return s;
}

static rep_socket *
make_socket (int namespace, int style)
{
    int sock_fd = socket (namespace, style, 0);
    if (sock_fd != -1)
	return make_socket_ (sock_fd, namespace, style);
    else
	return 0;
}

static void
shutdown_socket (rep_socket *s)
{
    if (s->sock >= 0)
    {
	close (s->sock);

	if (SOCKET_IS_REGISTERED (s))
	    rep_deregister_input_fd (s->sock);
    }

    DB (("shutdown socket fd %d\n", s->sock));

    s->sock = -1;
    s->car &= ~IS_ACTIVE;
}

static void
shutdown_socket_and_call_sentinel (rep_socket *s)
{
    shutdown_socket (s);
    if (s->sentinel != Qnil)
	rep_call_lisp1 (s->sentinel, rep_VAL (s));
}

static void
delete_socket (rep_socket *s)
{
    if (SOCKET_IS_ACTIVE (s))
	shutdown_socket (s);

    rep_FREE_CELL (s);
}

static rep_socket *
socket_for_fd (int fd)
{
    rep_socket *s;
    for (s = socket_list; s != 0; s = s->next)
    {
	if (s->sock == fd)
	    return s;
    }
    abort ();
}


/* clients */

static void
client_socket_output (int fd)
{
    rep_socket *s = socket_for_fd (fd);
    char buf[1025];
    int actual;

    DB (("client_socket_output for %d\n", fd));

    do {
	actual = read (fd, buf, 1024);
	if (actual > 0)
	{
	    buf[actual] = 0;
	    if (s->stream != Qnil)
		rep_stream_puts (s->stream, buf, actual, rep_FALSE);
	}
    } while (actual > 0 || (actual < 0 && errno == EINTR));

    if (actual == 0 || (actual < 0 && errno != EWOULDBLOCK && errno != EAGAIN))
    {
	/* assume EOF  */

	shutdown_socket_and_call_sentinel (s);
    }
}

static rep_socket *
make_client_socket (int namespace, int style, void *addr, size_t length)
{
    rep_socket *s = make_socket (namespace, style);

    if (s != 0)
    {
	if (connect (s->sock, addr, length) == 0)
	{
	    rep_unix_set_fd_nonblocking (s->sock);
	    rep_register_input_fd (s->sock, client_socket_output);
	    s->car |= IS_REGISTERED;
	    return s;
	}
	shutdown_socket (s);
    }
    return 0;
}


/* servers */

static void
server_socket_output (int fd)
{
    rep_socket *s = socket_for_fd (fd);

    DB (("server_socket_output for %d\n", fd));

    if (s->stream != Qnil)
	rep_call_lisp1 (s->stream, rep_VAL (s));
}
    
static rep_socket *
make_server_socket (int namespace, int style, void *addr, size_t length)
{
    rep_socket *s = make_socket (namespace, style);

    if (s != 0)
    {
	if (bind (s->sock, addr, length) == 0)
	{
	    if (listen (s->sock, 5) == 0)
	    {
		rep_unix_set_fd_nonblocking (s->sock);
		rep_register_input_fd (s->sock, server_socket_output);
		s->car |= IS_REGISTERED;
		return s;
	    }
	}
	shutdown_socket (s);
    }
    return 0;
}


/* Unix domain sockets */

static repv
make_local_socket (repv addr, rep_socket *(maker)(int, int, void *, size_t),
		   repv stream, repv sentinel)
{
    struct sockaddr_un name;
    size_t length;
    rep_socket *s;
    rep_GC_root gc_addr, gc_stream, gc_sentinel;
    repv local;

    rep_PUSHGC (gc_addr, addr);
    rep_PUSHGC (gc_stream, stream);
    rep_PUSHGC (gc_sentinel, sentinel);
    local = Flocal_file_name (addr);
    rep_POPGC; rep_POPGC; rep_POPGC;

    if (local == rep_NULL)
	return rep_NULL;
    if (!rep_STRINGP (local))
    {
	DEFSTRING (err, "Not a local file");
	return Fsignal (Qfile_error, rep_list_2 (rep_VAL (&err), addr));
    }

    name.sun_family = AF_LOCAL;
    strncpy (name.sun_path, rep_STR (local), sizeof (name.sun_path));

    length = (offsetof (struct sockaddr_un, sun_path)
	      + strlen (name.sun_path) + 1);

    s = maker (PF_LOCAL, SOCK_STREAM, &name, length);
    if (s != 0)
    {
	s->addr = addr;
	s->sentinel = sentinel;
	s->stream = stream;
	return rep_VAL (s);
    }
    else
	return rep_signal_file_error (addr);
}

DEFUN ("socket-local-client", Fsocket_local_client, Ssocket_local_client,
       (repv addr, repv stream, repv sentinel), rep_Subr3) /*
::doc:rep.io.sockets#socket-local-client::
socket-local-client ADDRESS [STREAM] [SENTINEL]

Create and return a socket connected to the unix domain socket at
ADDRESS (a special node in the local filing system).

All output from this socket will be copied to STREAM; when the socket
is closed down remotely SENTINEL will be called with the socket as its
single argument.
::end:: */
{
    rep_DECLARE (1, addr, rep_STRINGP (addr));

    return make_local_socket (addr, make_client_socket, stream, sentinel);
}

DEFUN ("socket-local-server", Fsocket_local_server, Ssocket_local_server,
       (repv addr, repv callback, repv sentinel), rep_Subr3) /*
::doc:rep.io.sockets#socket-local-server::
socket-local-server ADDRESS [CALLBACK] [SENTINEL]

Create and return a socket listening for connections on the unix domain
socket at ADDRESS (a special node in the local filing system).

When a connection is requested CALLBACK is called with the server
socket as its sole argument. It must call `socket-accept' to make the
connection.

When the socket is shutdown remotely, SENTINEL is called with the
socket as its only argument.
::end:: */
{
    rep_DECLARE (1, addr, rep_STRINGP (addr));

    return make_local_socket (addr, make_server_socket, callback, sentinel);
}


/* Internet domain sockets */

static repv
make_inet_socket (repv hostname, int port,
		  rep_socket *(maker)(int, int, void *, size_t),
		  repv stream, repv sentinel)
{
    struct sockaddr_in name;
    struct hostent *hostinfo;
    rep_socket *s = 0;

    name.sin_family = AF_INET;
    name.sin_port = htons (port);
    if (rep_STRINGP (hostname))
    {
	hostinfo = gethostbyname (rep_STR (hostname));
	if (hostinfo != 0)
	    name.sin_addr = * (struct in_addr *) hostinfo->h_addr;
	else
	{
	    errno = ENOENT;
	    return rep_signal_file_error (hostname);
	}
    }
    else
	name.sin_addr.s_addr = INADDR_ANY;

    s = maker (PF_INET, SOCK_STREAM, &name, sizeof (name));

    if (s != 0)
    {
	s->sentinel = sentinel;
	s->stream = stream;
	return rep_VAL (s);
    }
    else
	return rep_signal_file_error (hostname);
}

DEFUN ("socket-client", Fsocket_client, Ssocket_client,
       (repv host, repv port, repv stream, repv sentinel), rep_Subr4) /*
::doc:rep.io.sockets#socket-client::
socket-client HOSTNAME PORT [STREAM] [SENTINEL]

Create and return a socket connected to the socket on the host called
HOSTNAME (a string) with port number PORT.

All output from this socket will be copied to STREAM; when the socket
is closed down remotely SENTINEL will be called with the socket as its
single argument.
::end:: */
{
    rep_DECLARE (1, host, rep_STRINGP (host));
    rep_DECLARE (2, port, rep_INTP (port));

    return make_inet_socket (host, rep_INT (port),
			     make_client_socket, stream, sentinel);
}

DEFUN ("socket-server", Fsocket_server, Ssocket_server,
       (repv host, repv port, repv callback, repv sentinel), rep_Subr4) /*
::doc:rep.io.sockets#socket-server::
socket-server [HOSTNAME] [PORT] [CALLBACK] [SENTINEL]

Create and return a socket connected listening for connections on the
host called HOSTNAME (a string) with port number PORT. If HOSTNAME is
false, listen for any incoming addresses. If PORT is undefined a random
port will be chosen.

When a connection is requested CALLBACK is called with the server
socket as its sole argument. It must call `socket-accept' to make the
connection.

When the socket is shutdown remotely, SENTINEL is called with the
socket as its only argument.
::end:: */
{
    rep_DECLARE (1, host, rep_NILP (host) || rep_STRINGP (host));
    rep_DECLARE (2, port, rep_NILP (port) || rep_INTP (port));

    return make_inet_socket (host, rep_INTP (port) ? rep_INT (port) : 0,
			     make_server_socket, callback, sentinel);
}


/* Misc lisp functions */

DEFUN ("close-socket", Fclose_socket, Sclose_socket, (repv sock), rep_Subr1) /*
::doc:rep.io.sockets#close-socket::
close-socket SOCKET

Shutdown the connection associate with SOCKET. Note that this does not
cause the SENTINEL function associated with SOCKET to run.
::end:: */
{
    rep_DECLARE (1, sock, SOCKETP (sock));
    shutdown_socket (SOCKET (sock));
    return Qnil;
}

DEFUN ("socket-accept", Fsocket_accept, Ssocket_accept,
       (repv sock, repv stream, repv sentinel), rep_Subr3) /*
::doc:rep.io.sockets#socket-accept::
socket-accept SOCKET [STREAM] [SENTINEL]

Accept the pending connection request on server socket SOCKET. This
will create and return a client socket forming the end point of the
connection.

Any output received will be copied to the output stream STREAM, when
the connection is terminated remotely SENTINEL will be called with the
closed socket as its sole argument.

Note that this function must be called every time a connection request
is received. If the server wants to reject the connection it should
subsequently call `close-socket' on the created client.
::end:: */
{
    rep_socket *s;
    struct sockaddr_in in_name;
    struct sockaddr_un un_name;
    void *addr;
    socklen_t length;
    int new;

    rep_DECLARE (1, sock, ACTIVE_SOCKET_P (sock));
    s = SOCKET (sock);

    if (s->namespace == PF_LOCAL)
    {
	addr = &un_name;
	length = sizeof (un_name);
    }
    else
    {
	addr = &in_name;
	length = sizeof (in_name);
    }

    new = accept (s->sock, addr, &length);
    if (new != -1)
    {
	rep_socket *client = make_socket_ (new, s->namespace, s->style);
	rep_unix_set_fd_nonblocking (new);
	rep_register_input_fd (new, client_socket_output);
	client->car |= IS_REGISTERED;
	client->stream = stream;
	client->sentinel = sentinel;
	return rep_VAL (client);
    }
    else
	return Qnil;
}

static void
fill_in_address (rep_socket *s)
{
    if (s->addr == rep_NULL)
    {
	if (s->namespace == PF_INET)
	{
	    struct sockaddr_in name;
	    socklen_t length = sizeof (name);
	    if (getsockname (s->sock, (struct sockaddr *) &name, &length) == 0)
	    {
		if (name.sin_addr.s_addr == INADDR_ANY)
		{
		    /* Try to guess the ip address we're listening on */
		    char hname[128];
		    struct hostent *ent;
		    gethostname (hname, sizeof (hname) - 1);
		    ent = gethostbyname (hname);
		    if (ent != 0)
		    {
			struct in_addr *addr = ((struct in_addr *)
						ent->h_addr_list[0]);
			s->addr = rep_string_dup (inet_ntoa (*addr));
		    }
		    else
			s->addr = rep_string_dup (inet_ntoa (name.sin_addr));
		}
		else
		    s->addr = rep_string_dup (inet_ntoa (name.sin_addr));

		s->port = rep_MAKE_INT (ntohs (name.sin_port));
	    }
	}
	if (s->addr == rep_NULL)
	{
	    s->addr = Qnil;
	    s->port = Qnil;
	}
    }
}

static void
fill_in_peer_address (rep_socket *s)
{
    if (s->p_addr == rep_NULL)
    {
	if (s->namespace == PF_INET)
	{
	    struct sockaddr_in name;
	    socklen_t length = sizeof (name);
	    if (getpeername (s->sock, (struct sockaddr *) &name, &length) == 0)
	    {
		char *addr = inet_ntoa (name.sin_addr);
		if (addr != 0)
		{
		    s->p_addr = rep_string_dup (addr);
		    s->p_port = rep_MAKE_INT (ntohs (name.sin_port));
		}
	    }
	}
	if (s->p_addr == rep_NULL)
	{
	    s->p_addr = Qnil;
	    s->p_port = Qnil;
	}
    }
}

DEFUN ("socket-address", Fsocket_address,
       Ssocket_address, (repv sock), rep_Subr1) /*
::doc:rep.io.sockets#socket-address::
socket-address SOCKET

Return the address associated with SOCKET, or false if this is unknown.
::end:: */
{
    rep_DECLARE (1, sock, SOCKETP (sock));
    fill_in_address (SOCKET (sock));
    return SOCKET (sock)->addr;
}

DEFUN ("socket-port", Fsocket_port, Ssocket_port, (repv sock), rep_Subr1) /*
::doc:rep.io.sockets#socket-port::
socket-port SOCKET

Return the port associated with SOCKET, or false if this is unknown.
::end:: */
{
    rep_DECLARE (1, sock, SOCKETP (sock));
    fill_in_address (SOCKET (sock));
    return SOCKET (sock)->port;
}

DEFUN ("socket-peer-address", Fsocket_peer_address,
       Ssocket_peer_address, (repv sock), rep_Subr1) /*
::doc:rep.io.sockets#socket-peer-address::
socket-peer-address SOCKET

Return the address of the peer connected to SOCKET, or false if this
is unknown.
::end:: */
{
    rep_DECLARE (1, sock, SOCKETP (sock));
    fill_in_peer_address (SOCKET (sock));
    return SOCKET (sock)->p_addr;
}

DEFUN ("socket-peer-port", Fsocket_peer_port, Ssocket_peer_port,
       (repv sock), rep_Subr1) /*
::doc:rep.io.sockets#socket-peer-port::
socket-peer-port SOCKET

Return the port of the peer connected to SOCKET, or false if this is
unknown.
::end:: */
{
    rep_DECLARE (1, sock, SOCKETP (sock));
    fill_in_peer_address (SOCKET (sock));
    return SOCKET (sock)->p_port;
}

DEFUN ("accept-socket-output-1", Faccept_socket_output_1,
       Saccept_socket_output_1, (repv sock, repv secs, repv msecs), rep_Subr3) /*
::doc:rep.io.sockets#accept-socket-output-1::
accept-socket-output-1 SOCKET [SECS] [MSECS]

Process any pending output from SOCKET (this includes connection
requests, data transfer and shutdown notifications).

Waits for SECS seconds and MSECS milliseconds. Returns true if the
timeout was reached without any output being processed, otherwise
returns false.
::end:: */
{
    rep_DECLARE (1, sock, ACTIVE_SOCKET_P (sock));

    return (rep_accept_input_for_fds
	    ((rep_INTP(secs) ? rep_INT(secs) * 1000 : 0)
	     + (rep_INTP(msecs) ? rep_INT(msecs) : 0),
	     1, &SOCKET (sock)->sock));
}

DEFUN ("socketp", Fsocketp, Ssocketp, (repv arg), rep_Subr1) /*
::doc:rep.io.sockets#socketp::
socketp ARG

Return true if ARG is an unclosed socket object.
::end:: */
{
    return (SOCKETP (arg) && SOCKET_IS_ACTIVE (SOCKET (arg))) ? Qt : Qnil;
}


/* type hooks */

DEFSTRING (inactive_socket, "Inactive socket");

static rep_bool
poll_for_input (int fd)
{
    fd_set inputs;
    int ready;

    FD_ZERO (&inputs);
    FD_SET (fd, &inputs);
    ready = select (FD_SETSIZE, 0, &inputs, 0, 0);

    return ready == 1;
}

/* Returns the number of bytes actually written. */
static unsigned int
blocking_write (rep_socket *s, char *data, unsigned int bytes)
{
    unsigned int done = 0;

    if (!SOCKET_IS_ACTIVE (s))
    {
	Fsignal (Qfile_error, rep_list_2 (rep_VAL (&inactive_socket),
					  rep_VAL (s)));
	return -1;
    }

    do {
	int actual = write (s->sock, data + done, bytes - done);
	if (actual < 0)
	{
	    if (errno == EAGAIN || errno == EWOULDBLOCK)
	    {
		if (!poll_for_input (s->sock))
		    goto error;
	    }
	    else if (errno != EINTR)
		goto error;
	}
	else
	    done += actual;
    } while (done < bytes);

    return done;

error:
    rep_signal_file_error (rep_VAL (s));
    shutdown_socket_and_call_sentinel (s);
    return -1;
}

static int
socket_putc (repv stream, int c)
{
    char data = c;
    return blocking_write (SOCKET (stream), &data, 1);
}

static int
socket_puts (repv stream, void *data, int len, rep_bool is_lisp)
{
    char *buf = is_lisp ? rep_STR(data) : data;
    return blocking_write (SOCKET (stream), buf, len);
}

static void
socket_mark (repv val)
{
    rep_MARKVAL (SOCKET (val)->addr);
    rep_MARKVAL (SOCKET (val)->stream);
    rep_MARKVAL (SOCKET (val)->sentinel);
}

static void
socket_mark_active (void)
{
    rep_socket *s;
    for (s = socket_list; s != 0; s = s->next)
    {
	if (SOCKET_IS_ACTIVE (s))
	    rep_MARKVAL (rep_VAL (s));
    }
}

static void
socket_sweep (void)
{
    rep_socket *x = socket_list;
    socket_list = 0;
    while (x != 0)
    {
	rep_socket *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL (x)))
	    delete_socket (x);
	else
	{
	    rep_GC_CLR_CELL (rep_VAL (x));
	    x->next = socket_list;
	    socket_list = x;
	}
	x = next;
    }
}

static void
socket_print (repv stream, repv arg)
{
    rep_stream_puts (stream, "#<socket>", -1, rep_FALSE);
}


/* dl hooks */

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.io.sockets");

    socket_type = rep_register_new_type ("socket", 0, socket_print,
					 socket_print, socket_sweep,
					 socket_mark, socket_mark_active,
					 0, 0, socket_putc, socket_puts, 0, 0);

    rep_ADD_SUBR (Ssocket_local_client);
    rep_ADD_SUBR (Ssocket_local_server);
    rep_ADD_SUBR (Ssocket_client);
    rep_ADD_SUBR (Ssocket_server);
    rep_ADD_SUBR (Sclose_socket);
    rep_ADD_SUBR (Ssocket_accept);
    rep_ADD_SUBR (Ssocket_address);
    rep_ADD_SUBR (Ssocket_port);
    rep_ADD_SUBR (Ssocket_peer_address);
    rep_ADD_SUBR (Ssocket_peer_port);
    rep_ADD_SUBR (Saccept_socket_output_1);
    rep_ADD_SUBR (Ssocketp);

    rep_register_process_input_handler (client_socket_output);
    rep_register_process_input_handler (server_socket_output);

    return rep_pop_structure (tem);
}

void
rep_dl_kill (void)
{
    rep_socket *s;
    for (s = socket_list; s != 0; s = s->next)
	shutdown_socket (s);
    socket_list = 0;
}
