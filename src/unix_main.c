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

#define _GNU_SOURCE

#include "repint.h"

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

void (*rep_redisplay_fun)(void);
int rep_input_timeout_secs = 1;


/* Support functions */

#ifndef HAVE_STRERROR
DEFSTRING(unknown_err, "Unknown system error");
#endif
repv
rep_lookup_errno(void)
{
#ifdef HAVE_STRERROR
    return rep_string_dup(strerror(errno));
#else
    if(errno >= sys_nerr)
        return rep_string_dup(sys_errlist[errno]);
    else
        return rep_VAL(&unknown_err);
#endif
}

u_long
rep_time(void)
{
    return time(0);
}

void
rep_sleep_for(long secs, long msecs)
{
    struct timeval timeout;
    timeout.tv_sec = secs + msecs / 1000;
    timeout.tv_usec = (msecs % 1000) * 1000;
    select(FD_SETSIZE, NULL, NULL, NULL, &timeout);
}

repv
rep_user_login_name(void)
{
    /* Just look this up once, then use the saved copy.	 */
    static repv user_login_name;
    char *tmp;
    if(user_login_name)
	return user_login_name;

    if(!(tmp = getlogin()))
    {
	struct passwd *pwd;
	if(!(pwd = getpwuid(geteuid())))
	    return rep_NULL;
	tmp = pwd->pw_name;
    }
    user_login_name = rep_string_dup(tmp);
    rep_mark_static(&user_login_name);
    return user_login_name;
}

repv
rep_user_full_name(void)
{
    struct passwd *pwd;
    static repv user_full_name;
    if(user_full_name)
	return user_full_name;

    if(!(pwd = getpwuid(geteuid())))
	return rep_NULL;
#ifndef FULL_NAME_TERMINATOR
    user_full_name = rep_string_dup(pwd->pw_gecos);
#else
    {
	char *end = strchr(pwd->pw_gecos, FULL_NAME_TERMINATOR);
	if(end)
	    user_full_name = rep_string_dupn(pwd->pw_gecos, end - pwd->pw_gecos);
	else
	    user_full_name = rep_string_dup(pwd->pw_gecos);
    }
#endif
    rep_mark_static(&user_full_name);
    return user_full_name;
}

DEFSTRING(no_home, "Can't find home directory");
repv
rep_user_home_directory(repv user)
{
    static repv user_home_directory;
    if(rep_NILP(user) && user_home_directory)
	return user_home_directory;
    else
    {
	repv dir;
	char *src = 0;
	int len;

	if(rep_NILP(user))
	    src = getenv("HOME");

	if(src == 0)
	{
	    struct passwd *pwd;
	    if(rep_NILP(user))
		pwd = getpwuid(geteuid());
	    else
		pwd = getpwnam(rep_STR(user));

	    if(pwd == 0 || pwd->pw_dir == 0)
		return Fsignal(Qerror, rep_LIST_2(rep_VAL(&no_home), user));

	    src = pwd->pw_dir;
	}

	len = strlen(src);
	if(src[len] != '/')
	{
	    dir = rep_string_dupn(src, len + 1);
	    rep_STR(dir)[len] = '/';
	    rep_STR(dir)[len+1] = 0;
	}
	else
	    dir = rep_string_dup(src);

	if(rep_NILP(user))
	{
	    user_home_directory = dir;
	    rep_mark_static(&user_home_directory);
	}

	return dir;
    }
}

repv
rep_system_name(void)
{
    u_char buf[256];
    struct hostent *h;

    static repv system_name;
    if(system_name)
	return system_name;

#ifdef HAVE_GETHOSTNAME
    if(gethostname(buf, 256))
	return rep_NULL;
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
	    system_name = rep_string_dup(*aliases ? *aliases : h->h_name);
	}
	else
	    system_name = rep_string_dup((u_char *)h->h_name);
    }
    else
	system_name = rep_string_dup(buf);
    rep_mark_static(&system_name);
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

void (*rep_register_input_fd_fun)(int fd, void (*callback)(int fd)) = 0;
void (*rep_deregister_input_fd_fun)(int fd) = 0;

void
rep_register_input_fd(int fd, void (*callback)(int fd))
{
    FD_SET(fd, &input_fdset);
    input_actions[fd] = callback;

    if (rep_register_input_fd_fun != 0)
	(*rep_register_input_fd_fun) (fd, callback);

    rep_unix_set_fd_cloexec(fd);
}

void
rep_deregister_input_fd(int fd)
{
    FD_CLR(fd, &input_fdset);
    input_actions[fd] = NULL;

    if (rep_deregister_input_fd_fun != 0)
	(*rep_deregister_input_fd_fun) (fd);
}

void
rep_mark_input_pending(int fd)
{
    if(!FD_ISSET(fd, &input_pending))
    {
	FD_SET(fd, &input_pending);
	input_pending_count++;
    }    
}

void
rep_unix_set_fd_nonblocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if(flags != -1)
	fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

void
rep_unix_set_fd_blocking(int fd)
{
    int flags = fcntl(fd, F_GETFL, 0);
    if(flags != -1)
	fcntl(fd, F_SETFL, flags & ~O_NONBLOCK);
}

void
rep_unix_set_fd_cloexec(int fd)
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
    int ready = -1;

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

    /* Break the timeout into one-section chunks, then check for
       interrupt between each call to select. */
    do {
	struct timeval timeout;
	u_long this_timeout_msecs = MIN (timeout_msecs,
					 rep_input_timeout_secs * 1000);
	timeout.tv_sec = this_timeout_msecs / 1000;
	timeout.tv_usec = (this_timeout_msecs % 1000) * 1000;

	/* Dont test for interrupts before the first call to select() */
	if (ready == 0)
	{
	    rep_TEST_INT_SLOW;
	    if (rep_INTERRUPTP)
		break;
	}

	/* Don't want select() to restart after a SIGCHLD;
	   there may be a notification to dispatch.  */
	rep_sigchld_restart(rep_FALSE);
	ready = select(FD_SETSIZE, inputs, NULL, NULL, &timeout);
	rep_sigchld_restart(rep_TRUE);

	timeout_msecs -= this_timeout_msecs;
    } while (ready == 0 && timeout_msecs > 0);

    return ready;
}

/* Handle the READY fds with pending input (defined by fdset INPUTS).
   If CALLBACK is non-null, only invoke this function for inputs
   whose callback function == CALLBACK. Return true if the display
   might require updating. Returns immediately if a Lisp error has
   occurred. */
static rep_bool
handle_input(fd_set *inputs, int ready, void *callback)
{
    static long idle_period;
    rep_bool refreshp = rep_FALSE;

    if(ready > 0)
    {
	int i;

	idle_period = 0;

	for(i = 0; i < FD_SETSIZE && ready > 0 && !rep_INTERRUPTP; i++)
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
			refreshp = rep_TRUE;
		    }
		}
	    }
	}
    }
    else if(ready == 0)
    {
	/* A timeout. */
	if(rep_INTERRUPTP || rep_on_idle(idle_period))
	    refreshp = rep_TRUE;

	idle_period++;
    }

    if(!rep_INTERRUPTP && rep_proc_periodically())
	refreshp = rep_TRUE;

    return refreshp;
}

/* The input handler loop. */
repv
rep_event_loop(void)
{
    repv result = Qnil;

    if (rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();

    while(1)
    {
	int ready;
	rep_bool refreshp = rep_FALSE;
	fd_set copy;

	memcpy(&copy, &input_fdset, sizeof(copy));
	ready = wait_for_input(&copy, rep_input_timeout_secs * 1000);
	refreshp = handle_input(&copy, ready, 0);

	/* Check for exceptional conditions. */
	if(rep_throw_value != rep_NULL)
	{
	    if(rep_handle_input_exception(&result))
		return result;
	    else
		refreshp = rep_TRUE;
	}

	if(refreshp && rep_redisplay_fun != 0)
	    (*rep_redisplay_fun)();

#ifdef C_ALLOCA
	/* Using the C implementation of alloca. So garbage collect
	   anything below the current stack depth. */
	alloca(0);
#endif
    }

    return result;
}

repv
rep_sit_for(u_long timeout_msecs)
{
    fd_set copy;
    int ready;
    if(timeout_msecs != 0 && rep_redisplay_fun != 0)
	(*rep_redisplay_fun)();
    memcpy(&copy, &input_fdset, sizeof(copy));
    ready = wait_for_input(&copy, timeout_msecs);
    if(rep_INTERRUPTP)
	return rep_NULL;
    else
	return (ready > 0) ? Qnil : Qt;
}

/* Wait TIMEOUT_MSECS for input, ignoring any input fds that would
   invoke any callback function except CALLBACK. Return the number of
   input fds serviced. */
repv
rep_accept_input(u_long timeout_msecs, void *callback)
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
    if(ready > 0 && !rep_INTERRUPTP)
	handle_input(&copy, ready, callback);
    if(rep_INTERRUPTP)
	return rep_NULL;
    else
	return ready > 0 ? Qnil : Qt;
}

rep_bool
rep_poll_input(int fd)
{
    fd_set in;
    FD_ZERO(&in);
    FD_SET(fd, &in);
    return wait_for_input(&in, 0);
}


/* Memory allocation; most of these are normally macros in unix_defs.h */

#ifdef DEBUG_SYS_ALLOC
struct alloc_data {
    struct alloc_data *next;
    size_t size;
    void *function;
    char data[0] CONCAT(ALIGN_, rep_MALLOC_ALIGNMENT);
};

static struct alloc_data *allocations;

void *
rep_alloc(u_int length)
{
    void *mem;
    length += sizeof(struct alloc_data);
    mem = malloc(length);
    if(mem != 0)
    {
	struct alloc_data *x = mem;

	/* Check that the alignment promised actually occurs */
	assert((((rep_PTR_SIZED_INT)mem) & (rep_MALLOC_ALIGNMENT - 1)) == 0);

	mem = ((char *)mem) + sizeof(struct alloc_data);
	x->next = allocations;
	allocations = x;
	x->size = length - sizeof(struct alloc_data);
	x->function = rep_db_return_address();
    }
    return mem;
}

void *
rep_realloc(void *ptr, u_int length)
{
    void *mem;
    length += sizeof(struct alloc_data);
    ptr = (void *)(((char *)ptr) - sizeof(struct alloc_data));
    mem = realloc(ptr, length);
    if(mem != 0)
    {
	struct alloc_data *x = mem;

	/* Check that the alignment promised actually occurs */
	assert((((rep_PTR_SIZED_INT)mem) & (rep_MALLOC_ALIGNMENT - 1)) == 0);

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
	x->function = rep_db_return_address();
    }
    return mem;
}

void
rep_free(void *ptr)
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
rep_print_allocations(void)
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
	    if(rep_find_c_symbol(x->function, &sname, &saddr))
		fprintf(stderr, "\t\t<%s+%d>", sname, x->function - saddr);
	    fprintf(stderr, "\n");
	    x = x->next;
	}
    }
}

DEFUN("unix-print-allocations", Funix_print_allocations,
      Sunix_print_allocations, (void), rep_Subr0) /*
::doc:Sunix-print-allocations::
unix-print-allocations

Output a list of all allocated memory blocks to standard error.
::end:: */
{
    rep_print_allocations();
    return Qt;
}
#endif


/* Standard signal handlers */

static volatile rep_bool in_fatal_signal_handler;

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
    in_fatal_signal_handler = rep_TRUE;

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
    rep_db_print_backtrace(rep_common_db, "fatal_signal_handler");

    /* Output all debug buffers */
    rep_db_spew_all();

    /* Try and output the Lisp call stack; this may or may not
       provoke another error, but who cares.. */
    fprintf(stderr, "\nLisp backtrace:");
    Fbacktrace(Fstderr_file());
    fputs("\n\n", stderr);

    /* Now reraise the signal, since it's currently blocked
       the default action will occur, i.e. termination */
    raise(sig);
}

/* Invoked by SIGINT (i.e. ^C) */
static RETSIGTYPE
interrupt_signal_handler(int sig)
{
    rep_throw_value = rep_int_cell;
    signal(SIGINT, interrupt_signal_handler);
}

/* Invoked by trappable termination signals */
static RETSIGTYPE
termination_signal_handler(int sig)
{
    rep_throw_value = rep_term_cell;
    signal(sig, termination_signal_handler);
}


/* Initialisation */

/* This function is called _before_ almost anything else; but
   most importantly, it's called before sys_init() (i.e. we
   start opening displays) */
void
rep_pre_sys_os_init(void)
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
rep_sys_os_init(void)
{
    repv env;
    char **ptr;

    /* Initialise process-environment variable */
    env = Qnil;
    ptr = environ;
    while(*ptr != 0)
	env = Fcons(rep_string_dup(*ptr++), env);
    rep_SYM(Qprocess_environment)->value = env;

#ifdef DEBUG_SYS_ALLOC
    rep_ADD_SUBR(Sunix_print_allocations);
#endif

    rep_proc_init();
}

void
rep_sys_os_kill(void)
{
    rep_proc_kill();
}
