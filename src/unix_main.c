/* unix_main.c -- Miscellaneous functions for Unix
   Copyright (C) 1993, 1994 John Harper <john@dcs.warwick.ac.uk>
   $Id$

   This file is part of Jade.

   Jade is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Jade is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jade; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "jade.h"
#include <lib/jade_protos.h>

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <limits.h>
#include <sys/stat.h>
#include <time.h>
#include <pwd.h>
#include <netdb.h>
#include <signal.h>

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>
#endif

#ifdef HAVE_STRERROR
# include <errno.h>
#else
  extern int sys_nerr, errno;
  extern char *sys_errlist[];
#endif

#ifdef ENVIRON_UNDECLARED
  extern char **environ;
#endif

_PR VALUE lookup_errno(void);
_PR u_long sys_time(void);
_PR void sys_sleep_for(long secs, long msecs);
_PR VALUE sys_user_login_name(void);
_PR VALUE sys_user_full_name(void);
_PR VALUE sys_user_home_directory(VALUE user);
_PR VALUE sys_system_name(void);

_PR void sys_register_input_fd(int fd, void (*callback)(int fd));
_PR void sys_deregister_input_fd(int fd);
_PR void sys_mark_input_pending(int fd);
_PR void unix_set_fd_nonblocking(int fd);
_PR void unix_set_fd_blocking(int fd);
_PR void unix_set_fd_cloexec(int fd);
_PR VALUE sys_event_loop(void);
_PR VALUE sys_sit_for(u_long timeout_msecs);
_PR VALUE sys_accept_input(u_long timeout_msecs, void *callback);
_PR bool sys_poll_input(int fd);
_PR void *sys_alloc(u_int length);
_PR void *sys_realloc(void *ptr, u_int length);
_PR void sys_print_allocations(void);
_PR void pre_sys_init(void);
_PR void sys_misc_init(void);


/* Support functions */

#ifndef HAVE_STRERROR
DEFSTRING(unknown_err, "Unknown system error");
#endif
VALUE
lookup_errno(void)
{
#ifdef HAVE_STRERROR
    return string_dup(strerror(errno));
#else
    if(errno >= sys_nerr)
        return string_dup(sys_errlist[errno]);
    else
        return VAL(&unknown_err);
#endif
}

u_long
sys_time(void)
{
    return time(0);
}

void
sys_sleep_for(long secs, long msecs)
{
    struct timeval timeout;
    timeout.tv_sec = secs + msecs / 1000;
    timeout.tv_usec = (msecs % 1000) * 1000;
    select(FD_SETSIZE, NULL, NULL, NULL, &timeout);
}

VALUE
sys_user_login_name(void)
{
    /* Just look this up once, then use the saved copy.	 */
    static VALUE user_login_name;
    char *tmp;
    if(user_login_name)
	return user_login_name;

    if(!(tmp = getlogin()))
    {
	struct passwd *pwd;
	if(!(pwd = getpwuid(geteuid())))
	    return LISP_NULL;
	tmp = pwd->pw_name;
    }
    user_login_name = string_dup(tmp);
    mark_static(&user_login_name);
    return user_login_name;
}

VALUE
sys_user_full_name(void)
{
    struct passwd *pwd;
    static VALUE user_full_name;
    if(user_full_name)
	return user_full_name;

    if(!(pwd = getpwuid(geteuid())))
	return LISP_NULL;
#ifndef FULL_NAME_TERMINATOR
    user_full_name = string_dup(pwd->pw_gecos);
#else
    {
	char *end = strchr(pwd->pw_gecos, FULL_NAME_TERMINATOR);
	if(end)
	    user_full_name = string_dupn(pwd->pw_gecos, end - pwd->pw_gecos);
	else
	    user_full_name = string_dup(pwd->pw_gecos);
    }
#endif
    mark_static(&user_full_name);
    return user_full_name;
}

DEFSTRING(no_home, "Can't find home directory");
VALUE
sys_user_home_directory(VALUE user)
{
    static VALUE user_home_directory;
    if(NILP(user) && user_home_directory)
	return user_home_directory;
    else
    {
	VALUE dir;
	char *src = 0;
	int len;

	if(NILP(user))
	    src = getenv("HOME");

	if(src == 0)
	{
	    struct passwd *pwd;
	    if(NILP(user))
		pwd = getpwuid(geteuid());
	    else
		pwd = getpwnam(VSTR(user));

	    if(pwd == 0 || pwd->pw_dir == 0)
		return cmd_signal(sym_error, LIST_2(VAL(&no_home), user));

	    src = pwd->pw_dir;
	}

	len = strlen(src);
	if(src[len] != '/')
	{
	    dir = string_dupn(src, len + 1);
	    VSTR(dir)[len] = '/';
	    VSTR(dir)[len+1] = 0;
	}
	else
	    dir = string_dup(src);

	if(NILP(user))
	{
	    user_home_directory = dir;
	    mark_static(&user_home_directory);
	}

	return dir;
    }
}

VALUE
sys_system_name(void)
{
    u_char buf[256];
    struct hostent *h;

    static VALUE system_name;
    if(system_name)
	return system_name;

#ifdef HAVE_GETHOSTNAME
    if(gethostname(buf, 256))
	return LISP_NULL;
#else
    {
	struct utsname uts;
	uname(&uts);
	strncpy(buf, uts.nodename, 256);
    }
#endif
    h = gethostbyname(buf);
    if(h)
    {
	if(!strchr(h->h_name, '.'))
	{
	    /* The official name is not fully qualified. Try looking
	       through the list of alternatives. */
	    char **aliases = h->h_aliases;
	    while(*aliases && !strchr(*aliases, '.'))
		aliases++;
	    system_name = string_dup(*aliases ? *aliases : h->h_name);
	}
	else
	    system_name = string_dup((u_char *)h->h_name);
    }
    else
	system_name = string_dup(buf);
    mark_static(&system_name);
    return system_name;
}


/* Main input loop */

/* This is the set of fds we're waiting for input from. */
static fd_set input_fdset;

/* For every bit set in unix_fd_read_set there should be a corresponding
   function in here that will be called when input becomes available.
   -- Is this really such a good idea, it's a lot of wasted space.. */
static void (*input_actions[FD_SETSIZE])(int);

/* A bit set in this array means that the corresponding fd has input read
   but not yet handled. */
static fd_set input_pending;
static int input_pending_count;

void
sys_register_input_fd(int fd, void (*callback)(int fd))
{
    FD_SET(fd, &input_fdset);
    input_actions[fd] = callback;

    unix_set_fd_cloexec(fd);
}

void
sys_deregister_input_fd(int fd)
{
    FD_CLR(fd, &input_fdset);
    input_actions[fd] = NULL;
}

void
sys_mark_input_pending(int fd)
{
    if(!FD_ISSET(fd, &input_pending))
    {
	FD_SET(fd, &input_pending);
	input_pending_count++;
    }    
}

void
unix_set_fd_nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if(flags != -1)
	fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

void
unix_set_fd_blocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if(flags != -1)
	fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
}

void
unix_set_fd_cloexec(int fd)
{
    /* Set close on exec flag. */
    int tem = fcntl(fd, F_GETFD, 0);
    if(tem != -1)
	fcntl(fd, F_SETFD, tem | FD_CLOEXEC);
}

/* Wait for input for no longer than TIMEOUT-MSECS for input fds defined
   by INPUTS. If input arrived return the number of ready fds, with the
   actual fds defined by the fdset INPUTS. Return zero if the timeout
   was reached. */
static int
wait_for_input(fd_set *inputs, u_long timeout_msecs)
{
    struct timeval timeout;
    int ready;

    if(curr_win == 0)
	return 0;

    if(input_pending_count > 0)
    {
	/* Check the pending inputs first.. */
	fd_set out;
	int i, count = 0, seen = 0;
	for(i = 0; seen < input_pending_count && i < FD_SETSIZE; i++)
	{
	    if(FD_ISSET(i, &input_pending))
	    {
		seen++;
		if(FD_ISSET(i, inputs))
		{
		    if(count == 0)
			FD_ZERO(&out);
		    FD_SET(i, &out);
		    count++;
		}
	    }
	}
	if(count > 0)
	{
	    memcpy(inputs, &out, sizeof(out));
	    return count;
	}
    }

    timeout.tv_sec = timeout_msecs / 1000;
    timeout.tv_usec = (timeout_msecs % 1000) * 1000;

#ifdef HAVE_SUBPROCESSES
    /* Don't want select() to restart after a SIGCHLD; there may be
       a notification to dispatch.  */
    sigchld_restart(FALSE);
#endif

    ready = select(FD_SETSIZE, inputs, NULL, NULL, &timeout);

#ifdef HAVE_SUBPROCESSES
    sigchld_restart(TRUE);
#endif

    return ready;
}

/* Handle the READY fds with pending input (defined by fdset INPUTS).
   If CALLBACK is non-null, only invoke this function for inputs
   whose callback function == CALLBACK. Return true if the display
   might require updating. Returns immediately if a Lisp error has
   occurred. */
static bool
handle_input(fd_set *inputs, int ready, void *callback)
{
    static long idle_period;
    bool refreshp = FALSE;

    if(ready > 0)
    {
	int i;

	idle_period = 0;

	for(i = 0; i < FD_SETSIZE && ready > 0 && !INT_P; i++)
	{
	    if(FD_ISSET(i, inputs))
	    {
		ready--;
		if(!callback || input_actions[i] == callback)
		{
		    if(FD_ISSET(i, &input_pending))
		    {
			FD_CLR(i, &input_pending);
			input_pending_count--;
		    }
		    if(input_actions[i] != NULL)
		    {
			input_actions[i](i);
			refreshp = TRUE;
		    }
		}
	    }
	}
    }
    else if(ready == 0)
    {
	/* A timeout. */
	if(INT_P || on_idle(idle_period))
	    refreshp = TRUE;

	idle_period++;
    }

#ifdef HAVE_SUBPROCESSES
    if(!INT_P && proc_periodically())
	refreshp = TRUE;
#endif

    return refreshp;
}

/* The input handler loop. */
VALUE
sys_event_loop(void)
{
    VALUE result = sym_nil;
    recurse_depth++;

    cmd_redisplay(sym_nil);
    while(curr_win != NULL)
    {
	int ready;
	bool refreshp = FALSE;
	fd_set copy;

	memcpy(&copy, &input_fdset, sizeof(copy));
	ready = wait_for_input(&copy, EVENT_TIMEOUT_LENGTH * 1000);
	refreshp = handle_input(&copy, ready, 0);

	/* Check for exceptional conditions. */
	if(throw_value != LISP_NULL)
	{
	    if(handle_input_exception(&result))
		goto end;
	    else
		refreshp = TRUE;
	}

	if(refreshp)
	{
	    undo_end_of_command();
	    cmd_redisplay(sym_nil);
	}

#ifdef C_ALLOCA
	/* Using the C implementation of alloca. So garbage collect
	   anything below the current stack depth. */
	alloca(0);
#endif
    }

end:
    recurse_depth--;
    return result;
}

VALUE
sys_sit_for(u_long timeout_msecs)
{
    fd_set copy;
    int ready;
    if(timeout_msecs != 0)
	cmd_redisplay(sym_nil);
    memcpy(&copy, &input_fdset, sizeof(copy));
    ready = wait_for_input(&copy, timeout_msecs);
    if(INT_P)
	return LISP_NULL;
    else
	return (ready > 0) ? sym_nil : sym_t;
}

/* Wait TIMEOUT_MSECS for input, ignoring any input fds that would
   invoke any callback function except CALLBACK. Return the number of
   input fds serviced. */
VALUE
sys_accept_input(u_long timeout_msecs, void *callback)
{
    fd_set copy;
    int ready, i;
    FD_ZERO(&copy);
    for(i = 0; i < FD_SETSIZE; i++)
    {
	if(FD_ISSET(i, &input_fdset) && input_actions[i] == callback)
	    FD_SET(i, &copy);
    }
    ready = wait_for_input(&copy, timeout_msecs);
    if(ready > 0 && !INT_P)
	handle_input(&copy, ready, callback);
    if(INT_P)
	return LISP_NULL;
    else
	return ready > 0 ? sym_nil : sym_t;
}

bool
sys_poll_input(int fd)
{
    fd_set in;
    FD_ZERO(&in);
    FD_SET(fd, &in);
    return wait_for_input(&in, 0);
}


/* Memory allocation; most of these are normally macros in unix_defs.h */

#if defined(DOUG_LEA_MALLOC) && !defined(LIBC_MALLOC)
void *
__jade_morecore(long size)
{
    void *ptr = sbrk(size);
    /* functions in the __morecore hook are expected to return zero
       when failing */
    return (ptr == (void *)-1) ? 0 : ptr;
}
#endif

#ifdef DEBUG_SYS_ALLOC
struct alloc_data {
    struct alloc_data *next;
    size_t size;
    void *function;
    char data[0] CONCAT(ALIGN_, MALLOC_ALIGNMENT);
};

static struct alloc_data *allocations;

void *
sys_alloc(u_int length)
{
    void *mem;
    length += sizeof(struct alloc_data);
    mem = malloc(length);
    if(mem != 0)
    {
	struct alloc_data *x = mem;

	/* Check that the alignment promised actually occurs */
	assert((((PTR_SIZED_INT)mem) & (MALLOC_ALIGNMENT - 1)) == 0);

	mem = ((char *)mem) + sizeof(struct alloc_data);
	x->next = allocations;
	allocations = x;
	x->size = length - sizeof(struct alloc_data);
	x->function = db_return_address();
    }
    return mem;
}

void *
sys_realloc(void *ptr, u_int length)
{
    void *mem;
    length += sizeof(struct alloc_data);
    ptr = (void *)(((char *)ptr) - sizeof(struct alloc_data));
    mem = realloc(ptr, length);
    if(mem != 0)
    {
	struct alloc_data *x = mem;

	/* Check that the alignment promised actually occurs */
	assert((((PTR_SIZED_INT)mem) & (MALLOC_ALIGNMENT - 1)) == 0);

	if(allocations == ptr)
	    allocations = x;
	else
	{
	    struct alloc_data *p = allocations;
	    while(p->next != ptr)
		p = p->next;
	    p->next = x;
	}
	mem = ((char *)mem) + sizeof(struct alloc_data);
	x->size = length - sizeof(struct alloc_data);
	x->function = db_return_address();
    }
    return mem;
}

void
sys_free(void *ptr)
{
    struct alloc_data *x = (struct alloc_data *)(((char *)ptr) - sizeof(struct alloc_data));
    struct alloc_data **p = &allocations;
    while(*p != 0 && (*p) != x)
	p = &((*p)->next);
    assert(*p != 0);
    (*p) = x->next;
    free(x);
}

void
sys_print_allocations(void)
{
    if(allocations != 0)
    {
	struct alloc_data *x = allocations;
	fprintf(stderr, "\n\nOutstanding allocations:\n\n");
	while(x != 0)
	{
	    char *sname;
	    void *saddr;
	    fprintf(stderr, "\t(%p, %d)", x->data, x->size);
	    if(find_c_symbol(x->function, &sname, &saddr))
		fprintf(stderr, "\t\t<%s+%d>", sname, x->function - saddr);
	    fprintf(stderr, "\n");
	    x = x->next;
	}
    }
}

_PR VALUE cmd_unix_print_allocations(void);
DEFUN("unix-print-allocations", cmd_unix_print_allocations,
      subr_unix_print_allocations, (void), V_Subr0,
      DOC_unix_print_allocations) /*
::doc:unix_print_allocations::
unix-print-allocations

Output a list of all allocated memory blocks to standard error.
::end:: */
{
    sys_print_allocations();
    return sym_t;
}
#endif


/* Standard signal handlers */

static volatile bool in_fatal_signal_handler;

/* Invoked by any of the handlable error reporting signals */
static RETSIGTYPE
fatal_signal_handler(int sig)
{
    /* Sometimes this function can get in an infinite loop, even with the
       in_fatal_signal_handler exclusion? Does this help..? */
    signal(sig, SIG_DFL);

    /* Check for nested calls to this function */
    if(in_fatal_signal_handler)
	raise(sig);
    in_fatal_signal_handler = TRUE;

#ifdef HAVE_PSIGNAL
    psignal(sig, "jade: received fatal signal");
#else
# ifdef HAVE_STRSIGNAL
    fprintf(stderr, "jade: received fatal signal: %s\n", strsignal(sig));
# else
    fprintf(stderr, "jade: received fatal signal: %d\n", sig);
# endif
#endif

    /* Save the C backtrace */
    db_print_backtrace(common_db, "fatal_signal_handler");

    /* Output all debug buffers */
    db_spew_all();

    /* Try and output the Lisp call stack; this may or may not
       provoke another error, but who cares.. */
    fprintf(stderr, "\nLisp backtrace:");
    cmd_backtrace(cmd_stderr_file());
    fputs("\n\n", stderr);

    /* Now reraise the signal, since it's currently blocked
       the default action will occur, i.e. termination */
    raise(sig);
}

/* Invoked by SIGINT (i.e. ^C) */
static RETSIGTYPE
interrupt_signal_handler(int sig)
{
    throw_value = int_cell;
    signal(SIGINT, interrupt_signal_handler);
}

/* Invoked by trappable termination signals */
static RETSIGTYPE
termination_signal_handler(int sig)
{
    throw_value = term_cell;
    signal(sig, termination_signal_handler);
}


/* Initialisation */

/* This function is called _before_ almost anything else; but
   most importantly, it's called before sys_init() (i.e. we
   start opening displays) */
void
pre_sys_init(void)
{
    FD_ZERO(&input_fdset);
    FD_ZERO(&input_pending);

    /* First the error signals */
#ifdef SIGFPE
    if(signal(SIGFPE, fatal_signal_handler) == SIG_IGN)
	signal(SIGFPE, SIG_IGN);
#endif
#ifdef SIGILL
    if(signal(SIGILL, fatal_signal_handler) == SIG_IGN)
	signal(SIGILL, SIG_IGN);
#endif
#ifdef SIGSEGV
    if(signal(SIGSEGV, fatal_signal_handler) == SIG_IGN)
	signal(SIGSEGV, SIG_IGN);
#endif
#ifdef SIGBUS
    if(signal(SIGBUS, fatal_signal_handler) == SIG_IGN)
	signal(SIGBUS, SIG_IGN);
#endif
#ifdef SIGQUIT
    if(signal(SIGQUIT, fatal_signal_handler) == SIG_IGN)
	signal(SIGQUIT, SIG_IGN);
#endif
#ifdef SIGABRT
    if(signal(SIGABRT, fatal_signal_handler) == SIG_IGN)
	signal(SIGABRT, SIG_IGN);
#endif

    /* Install the interrupt handler */
#ifdef SIGINT
    if(signal(SIGINT, interrupt_signal_handler) == SIG_IGN)
	signal(SIGINT, SIG_IGN);
#endif

    /* Finally, the termination signals */
#ifdef SIGTERM
    if(signal(SIGTERM, termination_signal_handler) == SIG_IGN)
	signal(SIGTERM, SIG_IGN);
#endif
#ifdef SIGHUP
    if(signal(SIGHUP, termination_signal_handler) == SIG_IGN)
	signal(SIGHUP, SIG_IGN);
#endif
}

/* More normal initialisation. */
void
sys_misc_init(void)
{
    VALUE env;
    char **ptr;

    /* Initialise process-environment variable */
    env = sym_nil;
    ptr = environ;
    while(*ptr != 0)
	env = cmd_cons(string_dup(*ptr++), env);
    VSYM(sym_process_environment)->value = env;

#ifdef DEBUG_SYS_ALLOC
    ADD_SUBR(subr_unix_print_allocations);
#endif
}
