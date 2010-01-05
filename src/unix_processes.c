/* unix_processes.c -- Subprocess handling for Unix
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

/* Note that I have no idea how portable this code will be. It has
   been tested under Solaris and Linux, but beyond that, I really don't
   have the experience... */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#else
# include <sys/fcntl.h>
#endif

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#if HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifndef WEXITSTATUS
# define WEXITSTATUS(stat_val) ((unsigned)(stat_val) >> 8)
#endif
#ifndef WIFEXITED
# define WIFEXITED(stat_val) (((stat_val) & 255) == 0)
#endif

#ifdef HAVE_SYS_IOCTL_H
# include <sys/ioctl.h>
#endif

#ifdef HAVE_TERMIOS_H
# include <termios.h>
#endif

#ifdef HAVE_DEV_PTMX
# ifdef HAVE_STROPTS_H
#  include <stropts.h>
# endif
#endif

#ifdef ENVIRON_UNDECLARED
  extern char **environ;
#endif

void (*rep_sigchld_fun) (void) = 0;

static struct sigaction chld_sigact;
static sigset_t chld_sigset;

struct Proc
{
    repv	pr_Car;		/* status in high bits */
    struct Proc *pr_Next;
    /* Chain of all processes waiting to be notified of a change of state. */
    struct Proc *pr_NotifyNext;
    pid_t	pr_Pid;
    /* pr_Stdin is where we write, pr_Stdout where we read, they may be the
       same.  pr_Stderr is only used with pipes--it may be a separate
       connection to the stderr stream of the process. At all other times
       it will be equal to pr_Stdout. */
    int		pr_Stdin, pr_Stdout, pr_Stderr;
    repv	pr_OutputStream, pr_ErrorStream;
    int		pr_ExitStatus;
    repv	pr_NotifyFun;
    repv	pr_Prog;
    repv	pr_Args;
    repv	pr_Dir;
    repv	pr_ConnType;
};

/* Status is two bits above the type code (presently 8->9) */
#define PR_ACTIVE  (1 << (rep_CELL16_TYPE_BITS + 0))	/* active, may be stopped */
#define PR_STOPPED (2 << (rep_CELL16_TYPE_BITS + 1))	/* stopped */
#define PR_DEAD    0
#define PR_RUNNING PR_ACTIVE

#define PR_ACTIVE_P(p)  ((p)->pr_Car & PR_ACTIVE)
#define PR_STOPPED_P(p) ((p)->pr_Car & PR_STOPPED)
#define PR_RUNNING_P(p) (PR_ACTIVE_P(p) && !PR_STOPPED_P(p))
#define PR_DEAD_P(p)    !PR_ACTIVE_P(p)

#define PR_SET_STATUS(p,s) \
    ((p)->pr_Car = (((p)->pr_Car & ~(PR_ACTIVE | PR_STOPPED)) | (s)))

/* Connection types */
DEFSYM(pipe, "pipe");
DEFSYM(pty, "pty");
DEFSYM(socketpair, "socketpair");

#define PR_CONN_PTY_P(p) \
    ((p)->pr_ConnType == Qpty)

#define PR_CONN_PIPE_P(p) \
    ((p)->pr_ConnType == Qpipe)

#define PR_CONN_SOCKETPAIR_P(p) \
    ((p)->pr_ConnType == Qsocketpair)

#define VPROC(v)	((struct Proc *)rep_PTR(v))
#define PROCESSP(v)	rep_CELL16_TYPEP(v, process_type)

/* Handy debugging macro */
#if 0
# define DB(x) fprintf x
#else
# define DB(x)
#endif

static struct Proc *process_chain;
static struct Proc *notify_chain;
static int process_run_count;

static int process_type;

/* Set to rep_TRUE by the SIGCHLD handler */
static volatile rep_bool got_sigchld;

static void read_from_one_fd(struct Proc *pr, int fd);
static void read_from_process(int);

DEFSTRING(not_running, "Not running");
DEFSTRING(not_stopped, "Not stopped");
DEFSTRING(no_link, "No link to input");
DEFSTRING(in_use, "Process in use");
DEFSTRING(no_pty, "Can't find unused pty");
DEFSTRING(already_running, "Already running");
DEFSTRING(no_prog, "No program");
DEFSTRING(cant_start, "Can't start");
DEFSTRING(dev_null, "/dev/null");
DEFSTRING(dot, ".");
DEFSTRING(not_local, "Need a local file");
DEFSTRING(forkstr, "fork");
DEFSTRING(nosig, "Unknown signal");



static RETSIGTYPE
sigchld_handler(int sig)
{
    got_sigchld = rep_TRUE;
    if (rep_sigchld_fun != 0)
	(*rep_sigchld_fun) ();
}

static void
close_proc_files(struct Proc *pr)
{
    if(pr->pr_Stdout)
    {
	rep_deregister_input_fd(pr->pr_Stdout);
	close(pr->pr_Stdout);
    }
    if(pr->pr_Stderr && pr->pr_Stderr != pr->pr_Stdout)
    {
	rep_deregister_input_fd(pr->pr_Stderr);
	close(pr->pr_Stderr);
    }
    if(pr->pr_Stdin && (pr->pr_Stdin != pr->pr_Stdout))
	close(pr->pr_Stdin);
    pr->pr_Stdout = pr->pr_Stdin = pr->pr_Stderr = 0;
}
    
/* PR's NotifyFun will be called when possible. This function is safe
   to call from signal handlers.  */
static void
queue_notify(struct Proc *pr)
{
    if(pr->pr_NotifyNext == NULL)
    {
	pr->pr_NotifyNext = notify_chain;
	notify_chain = pr;
    }
}

/* Dispatch all queued notification.  */
static rep_bool
proc_notification(void)
{
    if(!notify_chain)
	return(rep_FALSE);
    while(notify_chain != NULL && !rep_INTERRUPTP)
    {
	struct Proc *pr = notify_chain;
	notify_chain = pr->pr_NotifyNext;
	pr->pr_NotifyNext = NULL;
	if(pr->pr_NotifyFun && !rep_NILP(pr->pr_NotifyFun))
	    rep_call_lisp1(pr->pr_NotifyFun, rep_VAL(pr));
    }
    return rep_TRUE;
}

static inline rep_bool
notify_queued_p (struct Proc *pr)
{
    return pr->pr_NotifyNext != 0;
}

static void
notify_1 (struct Proc *pr)
{
    if (notify_queued_p (pr))
    {
	struct Proc **ptr = &notify_chain;
	while (*ptr != pr)
	    ptr = &((*ptr)->pr_NotifyNext);
	*ptr = pr->pr_NotifyNext;
	pr->pr_NotifyNext = NULL;
	if (pr->pr_NotifyFun && pr->pr_NotifyFun != Qnil)
	    rep_call_lisp1 (pr->pr_NotifyFun, rep_VAL (pr));
    }
}

/* Checks if any of my children are zombies, takes appropriate action. */
static rep_bool
check_for_zombies(void)
{
    if(!got_sigchld)
	return rep_FALSE;

    got_sigchld = rep_FALSE;
    while(process_run_count > 0)
    {
	struct Proc *pr;
	int status;
	pid_t pid;

	pid = waitpid(-1, &status, WNOHANG | WUNTRACED);
	if(pid > 0)
	{
	    /* Got a process id, find its process structure. */
	    for(pr = process_chain; pr != 0; pr = pr->pr_Next)
	    {
		if(PR_ACTIVE_P(pr) && (pr->pr_Pid == pid))
		{
		    /* Got it. */
#ifdef WIFSTOPPED
		    if(WIFSTOPPED(status))
		    {
			/* Process is suspended. */
			PR_SET_STATUS(pr, PR_ACTIVE | PR_STOPPED);
			queue_notify(pr);
		    }
		    else
#endif
		    {
			/* Process is dead. */
			pr->pr_ExitStatus = status;
			process_run_count--;
			PR_SET_STATUS(pr, PR_DEAD);

			/* Try to read any pending output */
			if(pr->pr_Stdout)
			    read_from_one_fd(pr, pr->pr_Stdout);
			if(pr->pr_Stderr && pr->pr_Stderr != pr->pr_Stdout)
			    read_from_one_fd(pr, pr->pr_Stderr);

			/* Then close the streams */
			close_proc_files(pr);

			queue_notify(pr);
		    }
		    break;
		}
	    }
	}
	else if(pid == 0)
	    break;
	else if(pid < 0)
	{
	    if(errno == EINTR)
		continue;
	    else
		break;
	}
    }
    return rep_TRUE;
}

/* Called by the event loop after each event or timeout. Returns true
   if the display should be updated. */
static rep_bool
proc_periodically(void)
{
    rep_bool rc = check_for_zombies();
    if(proc_notification())
	rc = rep_TRUE;
    return rc;
}

/* Read data from FD out of PROC. If necessary it will handle
   clean up and notification. */
static void
read_from_one_fd(struct Proc *pr, int fd)
{
    repv stream = ((fd != pr->pr_Stdout)
		    ? pr->pr_ErrorStream : pr->pr_OutputStream);
    char buf[1025];
    int actual;
    do {
	if((actual = read(fd, buf, 1024)) > 0)
	{
	    buf[actual] = 0;
	    if(!rep_NILP(stream))
		rep_stream_puts(stream, buf, actual, rep_FALSE);
	}
    } while((actual > 0) || (actual < 0 && errno == EINTR));

    if (actual == 0 || (actual < 0 && errno != EWOULDBLOCK && errno != EAGAIN))
    {
	/* We assume EOF  */

	rep_deregister_input_fd(fd);
	close(fd);

	/* Could be either pr_Stdout or pr_Stderr */
	if(fd != pr->pr_Stdout)
	    pr->pr_Stderr = 0;
	else
	{
	    if(pr->pr_Stdin && (pr->pr_Stdin == pr->pr_Stdout))
		pr->pr_Stdin = 0;
	    if(pr->pr_Stderr && (pr->pr_Stderr == pr->pr_Stdout))
		pr->pr_Stderr = 0;
	    pr->pr_Stdout = 0;
	}
    }
}

static void
read_from_process(int fd)
{
    struct Proc *pr;
    pr = process_chain;
    while(pr)
    {
	if(PR_ACTIVE_P(pr) && (pr->pr_Stdout == fd || pr->pr_Stderr == fd))
	    read_from_one_fd(pr, fd);
	pr = pr->pr_Next;
    }
}

static int
write_to_process(repv pr, char *buf, int bufLen)
{
    int act = 0;
    if(!PROCESSP(pr))
	return(0);
    if(PR_ACTIVE_P(VPROC(pr)))
    {
	if(VPROC(pr)->pr_Stdin == 0)
	{
	    Fsignal(Qprocess_error, rep_list_2(pr, rep_VAL(&no_link)));
	}
	else
	{
	    do {
		/* This will block */
		int this = write(VPROC(pr)->pr_Stdin, buf + act, bufLen - act);
		if (this < 0)
		{
		    if (errno != EINTR)
		    {
			rep_signal_file_error(pr);
			break;
		    }
		}
		else
		    act += this;
	    } while (act < bufLen);
	}
    }
    else
	Fsignal(Qprocess_error, rep_list_2(pr, rep_VAL(&not_running)));
    return(act);
}

static rep_bool
signal_process(struct Proc *pr, int sig, rep_bool do_grp)
{
    rep_bool rc = rep_TRUE;
    if(do_grp)
    {
	if(pr->pr_Stdin && PR_CONN_PTY_P(pr))
	{
	    pid_t gid = tcgetpgrp(pr->pr_Stdin);
	    if(gid != -1)
		kill(-gid, sig);
	    else if(PR_ACTIVE_P(pr))
		kill(-pr->pr_Pid, sig);
	    else
		rc = rep_FALSE;
	}
	else
	{
	    if(PR_ACTIVE_P(pr))
		kill(-pr->pr_Pid, sig);
	    else
		rc = rep_FALSE;
	}
    }
    else
    {
	if(PR_ACTIVE_P(pr))
	    kill(pr->pr_Pid, sig);
	else
	    rc = rep_FALSE;
    }
    return(rc);
}

/* This is only called during GC, when the process isn't being referenced.
   it will already have been taken out of the chain. Also active processes
   should have been marked anyway. */
static void
kill_process(struct Proc *pr)
{
    if(PR_ACTIVE_P(pr))
    {
	/* is this too heavy-handed?? */
	if(!signal_process(pr, SIGKILL, rep_TRUE))
	    kill(-pr->pr_Pid, SIGKILL);
	waitpid(pr->pr_Pid, &pr->pr_ExitStatus, 0);
	process_run_count--;
	close_proc_files(pr);
    }
    rep_FREE_CELL(pr);
}

/* Return the file descriptor (or 0 if an error) of the first available
   pty master. SLAVENAM will contain the name of the associated slave. */
static int
get_pty(char *slavenam)
{
#if defined(HAVE_PTYS)
    int master;

# if defined(HAVE_DEV_PTMX) && defined(HAVE_GRANTPT)
    master = open("/dev/ptmx", O_RDWR);
    if(master >= 0)
    {
	char *tem;
	grantpt(master);
	unlockpt(master);
	tem = ptsname(master);
	if(tem != 0)
	{
	    strcpy(slavenam, tem);
	    return master;
	}
	close(master);
    }
# endif

# if defined(FIRST_PTY_LETTER)
    /* Assume /dev/ptyXNN and /dev/ttyXN naming system.
       The FIRST_PTY_LETTER gives the first X to try. We try in the 
       sequence FIRST_PTY_LETTER, .., 'z', 'a', .., FIRST_PTY_LETTER.
       Is this worthwhile, or just over-zealous? */
    char c = FIRST_PTY_LETTER;
    do {
	int i;
	for(i = 0; i < 16; i++)
	{
	    struct stat statb;
	    sprintf(slavenam, "/dev/pty%c%x", c, i);
	    if(stat(slavenam, &statb) < 0)
		goto none;
	    if((master = open(slavenam, O_RDWR)) >= 0)
	    {
		slavenam[sizeof("/dev/")-1] = 't';
		if(access(slavenam, R_OK | W_OK) == 0)
		    return master;
		close(master);
	    }
	}
	if(++c > 'z')
	    c = 'a';
    } while(c != FIRST_PTY_LETTER);
none:
# endif /* FIRST_PTY_LETTER */
#endif /* HAVE_PTYS */

    /* Couldn't find a pty. Signal an error. */
    Fsignal(Qprocess_error, rep_LIST_1(rep_VAL(&no_pty)));
    return 0;
}

static void
child_build_environ (void)
{
    /* Build the environment */
    repv tem = Fsymbol_value(Qprocess_environment, Qt);
    if(rep_CONSP(tem))
    {
	repv len = Flength(tem);
	if(len && rep_INTP(len))
	{
	    environ = rep_alloc(sizeof(char *) * (rep_INT(len) + 1));
	    if(environ != 0)
	    {
		char **ptr = environ;
		while(rep_CONSP(tem))
		{
		    *ptr++ = rep_STR(rep_CAR(tem));
		    tem = rep_CDR(tem);
		}
		*ptr++ = 0;
	    }
	}
    }
}

/* does the dirty stuff of getting the process running. if SYNC_INPUT
   is non-NULL it means to run the process synchronously with it's
   stdin connected to the file SYNC_INPUT. Otherwise this function returns
   immediately after starting the process.  */
static rep_bool
run_process(struct Proc *pr, char **argv, char *sync_input)
{
    rep_bool rc = rep_FALSE;
    if(PR_DEAD_P(pr))
    {
	rep_bool usepty = PR_CONN_PTY_P(pr);
	char slavenam[32];
	int stdin_fds[2], stdout_fds[2], stderr_fds[2]; /* only for pipes */
	pr->pr_ExitStatus = -1;

	if(sync_input != NULL || !usepty)
	{
	    usepty = rep_FALSE;
	    pr->pr_ConnType = Qpipe;
	    if(pipe(stdout_fds) == 0)
	    {
		if(pipe(stderr_fds) == 0)
		{
		    if(sync_input)
		    {
			stdin_fds[0] = open(sync_input, O_RDONLY);
			if(stdin_fds[0] >= 0)
			    pr->pr_Stdin = stdin_fds[0]; /* fake */
		    }
		    else
		    {
			if(pipe(stdin_fds) == 0)
			    pr->pr_Stdin = stdin_fds[1];
		    }
		    if(pr->pr_Stdin != 0)
		    {
			pr->pr_Stdout = stdout_fds[0];
			pr->pr_Stderr = stderr_fds[0];
		    }
		    else
		    {
			close(stderr_fds[0]);
			close(stderr_fds[1]);
		    }
		}
		else
		{
		    close(stdout_fds[0]);
		    close(stdout_fds[1]);
		}
	    }
	}
	else if (PR_CONN_SOCKETPAIR_P(pr))
	{
	    /* XXX separate stdout from stderr.. */
	    if (socketpair (AF_UNIX, SOCK_STREAM, 0, stdin_fds) == 0)
	    {
		pr->pr_Stdin = stdin_fds[0];
		pr->pr_Stdout = stdin_fds[0];
		pr->pr_Stderr = stdin_fds[0];
	    }
	}
	else if(usepty)
	{
	    pr->pr_Stdin = get_pty(slavenam);
	    pr->pr_Stdout = pr->pr_Stdin;
	    pr->pr_Stderr = pr->pr_Stdin;
	}
	if(pr->pr_Stdin)
	{
	    int pty_slave_fd = -1;

	    /* Must set up pty slave before forking, to avoid race
	       condition if master writes to it first */
	    if(usepty)
	    {
		struct termios st;
		pty_slave_fd = open(slavenam, O_RDWR);
		if (pty_slave_fd >= 0)
		{
#ifdef HAVE_DEV_PTMX
# ifdef I_PUSH
		    /* Push the necessary modules onto the slave to
		       get terminal semantics. */
		    ioctl(pty_slave_fd, I_PUSH, "ptem");
		    ioctl(pty_slave_fd, I_PUSH, "ldterm");
# endif
#endif
#ifdef TIOCSCTTY
		    ioctl(pty_slave_fd, TIOCSCTTY, 0);
#endif
		    tcgetattr(pty_slave_fd, &st);
		    st.c_iflag &= ~(ISTRIP | IGNCR | INLCR | IXOFF);
		    st.c_iflag |= (ICRNL | IGNPAR | BRKINT | IXON);
		    st.c_oflag &= ~OPOST;
		    st.c_cflag &= ~CSIZE;
		    st.c_cflag |= CREAD | CS8 | CLOCAL;
		    st.c_lflag &= ~(ECHO | ECHOE | ECHOK | NOFLSH | TOSTOP);
		    st.c_lflag |= ISIG;
#if 0
		    st.c_cc[VMIN] = 1;
		    st.c_cc[VTIME] = 0;
#endif
		    /* Set some control codes to default values */
		    st.c_cc[VINTR]  = '\003';	/* ^c */
		    st.c_cc[VQUIT]  = '\034';	/* ^| */
		    st.c_cc[VERASE] = '\177';	/* ^? */
		    st.c_cc[VKILL]  = '\025';	/* ^u */
		    st.c_cc[VEOF]   = '\004';	/* ^d */
		    tcsetattr(pty_slave_fd, TCSANOW, &st);
		}
	    }

	    switch(pr->pr_Pid = fork())
	    {
	    case 0:
		/* Child process */

		child_build_environ ();

		if(usepty)
		{
		    if(setsid() < 0)
		    {
			perror("child: setsid()");
			_exit(255);
		    }
		    if(pty_slave_fd < 0)
		    {
			perror("child: open(slave)");
			_exit(255);
		    }
		    close(pr->pr_Stdin);

		    dup2(pty_slave_fd, 0);
		    dup2(pty_slave_fd, 1);
		    dup2(pty_slave_fd, 2);
		    if(pty_slave_fd > 2)
		    {
			close(pty_slave_fd);
			pty_slave_fd = -1;
		    }
		}
		else if (PR_CONN_SOCKETPAIR_P(pr))
		{
		    /* startup for socketpair */
		    if(setpgid(0, 0) != 0)
		    {
			perror("setpgid");
			_exit(255);
		    }
		    close (stdin_fds[0]);
		    dup2 (stdin_fds[1], 0);
		    dup2 (stdin_fds[1], 1);
		    dup2 (stdin_fds[1], 2);
		    close (stdin_fds[1]);
		}
		else
		{
		    /* startup for pipes */
		    if(setpgid(0, 0) != 0)
		    {
			perror("setpgid");
			_exit(255);
		    }
		    dup2(stdin_fds[0], 0);
		    close(stdin_fds[0]);
		    if(sync_input == NULL)
			close(stdin_fds[1]);

		    dup2(stdout_fds[1], 1);
		    dup2(stderr_fds[1], 2);
		    close(stdout_fds[0]);
		    close(stdout_fds[1]);
		    close(stderr_fds[0]);
		    close(stderr_fds[1]);
		}
		if(rep_STRINGP(pr->pr_Dir))
		{
		    if(rep_STRING_LEN(pr->pr_Dir) > 0)
			chdir(rep_STR(pr->pr_Dir));
		}
		signal (SIGPIPE, SIG_DFL);

		execvp(argv[0], argv);
		perror("child subprocess can't exec");
		_exit(255);

	    case -1:
		/* Clean up all open files */
		if (pty_slave_fd != -1)
		    close (pty_slave_fd);
		if (PR_CONN_SOCKETPAIR_P(pr))
		{
		    close (stdin_fds[0]);
		    close (stdin_fds[1]);
		}
		if (sync_input != 0 || !usepty)
		{
		    /* pipes */
		    close(stdout_fds[0]); close(stdout_fds[1]);
		    close(stderr_fds[0]); close(stderr_fds[1]);
		    close(stdin_fds[0]);
		    if (sync_input != 0)
			close(stdin_fds[1]);
		}
		else
		    close(pr->pr_Stdin);
		pr->pr_Stdin = pr->pr_Stdout = pr->pr_Stderr = 0;
		rep_signal_file_error(rep_VAL(&forkstr));
		break;

	    default:
		/* Parent process */

		if (pty_slave_fd != -1)
		    close (pty_slave_fd);
		PR_SET_STATUS(pr, PR_RUNNING);
		if (PR_CONN_SOCKETPAIR_P(pr))
		{
		    close (stdin_fds[1]);
		}
		else if(!usepty)
		{
		    close(stdin_fds[0]);
		    close(stdout_fds[1]);
		    close(stderr_fds[1]);
		}
		if(sync_input == NULL)
		{
		    if(pr->pr_Stdin == pr->pr_Stdout)
		    {
			/* So that pr_Stdout can be made non-blocking
			   set up another fd for writing to.  */
			if((pr->pr_Stdin = dup(pr->pr_Stdout)) < 0)
			{
			    /* Maybe this is unwise? */
			    perror("dup(pr->pr_Stdout)");
			    pr->pr_Stdin = pr->pr_Stdout;
			}
		    }
		    rep_unix_set_fd_cloexec(pr->pr_Stdin);
		    rep_unix_set_fd_nonblocking(pr->pr_Stdout);
		    rep_register_input_fd(pr->pr_Stdout, read_from_process);
		    if(pr->pr_Stderr != pr->pr_Stdout)
		    {
			rep_unix_set_fd_nonblocking(pr->pr_Stderr);
			rep_register_input_fd(pr->pr_Stderr,
					      read_from_process);
		    }
		    process_run_count++;
		}
		else
		{
		    /* Run synchronously.  */
		    char buf[1025];
		    int actual;
		    fd_set inputs;
		    rep_bool done_out = rep_FALSE, done_err = rep_FALSE;
		    rep_bool exited = rep_FALSE;
		    int interrupt_count = 0;
#ifdef KLUDGE_SYNCHRONOUS_OUTPUT
		    int post_exit_count = 0;
#endif

		    FD_ZERO(&inputs);
		    FD_SET(pr->pr_Stdout, &inputs);
		    FD_SET(pr->pr_Stderr, &inputs);
		    pr->pr_Stdin = 0;
		    fcntl(pr->pr_Stdout, F_SETFL, O_NONBLOCK);
		    fcntl(pr->pr_Stderr, F_SETFL, O_NONBLOCK);

		    while(!(done_out && done_err))
		    {
			fd_set copy = inputs;
			struct timeval timeout;
			int number;
			timeout.tv_sec = 1;
			timeout.tv_usec = 0;

			rep_sig_restart(SIGCHLD, rep_FALSE);
			number = select(FD_SETSIZE, &copy, NULL,
					NULL, &timeout);
			rep_sig_restart(SIGCHLD, rep_TRUE);

			rep_TEST_INT_SLOW;
			if(rep_INTERRUPTP)
			{
			    int signal;
			    /* What to do here? */
			    switch(++interrupt_count)
			    {
			    case 1:
				signal = SIGINT;
				break;
			    case 2:
				signal = SIGTERM;
				break;
			    default:
				signal = SIGKILL;
			    }
			    signal_process(pr, signal, rep_TRUE);
			    if(rep_throw_value == rep_int_cell)
				rep_throw_value = 0;
			}

			if(number > 0)
			{
			    rep_GC_root gc_pr;
			    repv vpr = rep_VAL(pr);
			    rep_PUSHGC(gc_pr, vpr);
			    if(!done_out && FD_ISSET(pr->pr_Stdout, &copy))
			    {
				actual = read(pr->pr_Stdout, buf, 1024);
				if(actual > 0)
				{
				    buf[actual] = 0;
				    if(!rep_NILP(pr->pr_OutputStream))
				    {
					rep_stream_puts(pr->pr_OutputStream, buf,
						    actual, rep_FALSE);
				    }
				}
				else if(actual == 0
					|| (errno != EINTR
					    && errno != EAGAIN
					    && errno != EWOULDBLOCK))
				{
				    done_out = rep_TRUE;
				    FD_CLR(pr->pr_Stdout, &inputs);
				}
			    }
			    if(!done_err && FD_ISSET(pr->pr_Stderr, &copy))
			    {
				actual = read(pr->pr_Stderr, buf, 1024);
				if(actual > 0)
				{
				    buf[actual] = 0;
				    if(!rep_NILP(pr->pr_ErrorStream))
				    {
					rep_stream_puts(pr->pr_ErrorStream, buf,
						    actual, rep_FALSE);
				    }
				}
				else if(actual == 0
					|| (errno != EINTR
					    && errno != EAGAIN
					    && errno != EWOULDBLOCK))
				{
				    done_err = rep_TRUE;
				    FD_CLR(pr->pr_Stderr, &inputs);
				}
			    }
			    rep_POPGC;
			}
#ifdef KLUDGE_SYNCHRONOUS_OUTPUT
			/* This still doesn't work. The best way to
			   solve this problem is to move the onus to
			   the caller. If a command is called which
			   spawns on its streams, they should be
			   redirected somewhere safe beforehand. */

			/* The next two statements are a bit kludgey.

			   Problem: If the child process exits, but has
			   spawned an orphan of its own on the same input
			   and output streams, the done_out and done_err
			   flags won't get set until the _orphan_ quits.

			   Solution: Check for process exit here. If it
			   has exited, allow a few more timeouts, before
			   breaking the loop. */

			if(exited && number == 0 && ++post_exit_count > 2)
			    break;

			if(!exited && got_sigchld
			   && waitpid(pr->pr_Pid,
				      &pr->pr_ExitStatus,
				      WNOHANG) == pr->pr_Pid)
			    exited = rep_TRUE;
#endif
		    }
		    if(!exited)
			waitpid(pr->pr_Pid, &pr->pr_ExitStatus, 0);

		    close(pr->pr_Stdout);
		    close(pr->pr_Stderr);
		    pr->pr_Stdout = 0;
		    pr->pr_Stderr = 0;
		    PR_SET_STATUS(pr, PR_DEAD);
		    queue_notify(pr);
		}
		rc = rep_TRUE;
		break;
	    }
	}
	else if(rep_throw_value == rep_NULL)
	    Fsignal(Qprocess_error, rep_LIST_1(rep_lookup_errno()));
    }
    else
	Fsignal(Qprocess_error, rep_list_2(rep_VAL(pr), rep_VAL(&already_running)));
    return(rc);
}

static void
proc_mark(repv pr)
{
    rep_MARKVAL(VPROC(pr)->pr_OutputStream);
    rep_MARKVAL(VPROC(pr)->pr_ErrorStream);
    rep_MARKVAL(VPROC(pr)->pr_NotifyFun);
    rep_MARKVAL(VPROC(pr)->pr_Prog);
    rep_MARKVAL(VPROC(pr)->pr_Args);
    rep_MARKVAL(VPROC(pr)->pr_Dir);
    rep_MARKVAL(VPROC(pr)->pr_ConnType);
}

static void
mark_active_processes(void)
{
    struct Proc *pr = process_chain;
    while(pr != 0)
    {
	if(PR_ACTIVE_P(pr))
	    rep_MARKVAL(rep_VAL(pr));
	pr = pr->pr_Next;
    }
}

static void
proc_sweep(void)
{
    struct Proc *pr;

    /* First weed out any unused processes from the notify chain...  */
    pr = notify_chain;
    notify_chain = NULL;
    while(pr)
    {
	if(rep_GC_CELL_MARKEDP(rep_VAL(pr)))
	{
	    pr->pr_NotifyNext = notify_chain;
	    notify_chain = pr;
	}
	pr = pr->pr_NotifyNext;
    }

    /* ...then do the normal sweep stuff.  */
    pr = process_chain;
    process_chain = NULL;
    while(pr)
    {
	struct Proc *nxt = pr->pr_Next;
	if(!rep_GC_CELL_MARKEDP(rep_VAL(pr)))
	    kill_process(pr);
	else
	{
	    rep_GC_CLR_CELL(rep_VAL(pr));
	    pr->pr_Next = process_chain;
	    process_chain = pr;
	}
	pr = nxt;
    }
}

static void
proc_prin(repv strm, repv obj)
{
    struct Proc *pr = VPROC(obj);
    char buf[40];
    rep_stream_puts(strm, "#<process", -1, rep_FALSE);
    if(PR_RUNNING_P(pr))
    {
	rep_stream_puts(strm, " running: ", -1, rep_FALSE);
	rep_stream_puts(strm, rep_PTR(pr->pr_Prog), -1, rep_TRUE);
    }
    else if(PR_STOPPED_P(pr))
    {
	rep_stream_puts(strm, " stopped: ", -1, rep_FALSE);
	rep_stream_puts(strm, rep_PTR(pr->pr_Prog), -1, rep_TRUE);
    }
    else
    {
	if(pr->pr_ExitStatus != -1)
	{
#ifdef HAVE_SNPRINTF
	    snprintf(buf, sizeof(buf), " exited: 0x%x", pr->pr_ExitStatus);
#else
	    sprintf(buf, " exited: 0x%x", pr->pr_ExitStatus);
#endif
	    rep_stream_puts(strm, buf, -1, rep_FALSE);
	}
    }
    rep_stream_putc(strm, '>');
}

static int
proc_putc(repv stream, int c)
{
    char tmps[2];
    tmps[0] = (char)c;
    tmps[1] = 0;
    return write_to_process(stream, tmps, 1);
}

static int
proc_puts(repv stream, void *data, int len, rep_bool is_lisp)
{
    char *buf = is_lisp ? rep_STR(data) : data;
    return write_to_process(stream, buf, len);
}

DEFUN("make-process", Fmake_process, Smake_process, (repv stream, repv fun, repv dir, repv prog, repv args), rep_Subr5) /*
::doc:rep.io.processes#make-process::
make-process [OUTPUT-STREAM] [FUN] [DIR] [PROGRAM] [ARGS]

Creates a new process-object, OUTPUT-STREAM is where all output from this
process goes, both stdout and stderr, FUN is a function to call each time
the process running on this object changes state. DIR is the process'
current directory, PROGRAM the filename of the program to run and ARGS a
list of arguments passed to the process.

Any of the arguments may be unspecified, in which case they can be set
either by the functions provided or by the function called to create the
actual running process.

If the DIR parameter is nil it will be inherited from the
`default-directory' variable of the current buffer.
::end:: */
{
    repv pr = rep_VAL(rep_ALLOC_CELL(sizeof(struct Proc)));
    if(pr != rep_NULL)
    {
	rep_GC_root gc_pr;
	rep_data_after_gc += sizeof (struct Proc);
	VPROC(pr)->pr_Car = process_type;
	VPROC(pr)->pr_Next = process_chain;
	process_chain = VPROC(pr);
	VPROC(pr)->pr_NotifyNext = NULL;
	PR_SET_STATUS(VPROC(pr), PR_DEAD);
	VPROC(pr)->pr_Pid = 0;
	VPROC(pr)->pr_Stdin = VPROC(pr)->pr_Stdout = 0;
	VPROC(pr)->pr_ExitStatus = -1;
	VPROC(pr)->pr_OutputStream = stream;
	VPROC(pr)->pr_ErrorStream = stream;
	VPROC(pr)->pr_NotifyFun = fun;
	VPROC(pr)->pr_Prog = prog;
	VPROC(pr)->pr_Args = args;
	VPROC(pr)->pr_ConnType = Qpipe;
	VPROC(pr)->pr_Dir = dir;

	/* Ensure that pr_Dir refers to an absolute local file */
	rep_PUSHGC(gc_pr, pr);
	dir = Flocal_file_name(rep_STRINGP(dir) ? dir : rep_VAL(&dot));
	rep_POPGC;
	if(dir && rep_STRINGP(dir))
	    VPROC(pr)->pr_Dir = dir;
	else
	    VPROC(pr)->pr_Dir = Qnil;

	return pr;
    }
    else
	return rep_mem_error();
}

DEFUN("close-process", Fclose_process,
      Sclose_process, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#close-process::
close-processes [PROCESS]

Closes the stdin, stdout, and stderr streams of the asynchronous process-
object PROCESS.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    close_proc_files(VPROC(proc));
    return(Qnil); 
}

DEFUN("start-process", Fstart_process, Sstart_process, (repv arg_list), rep_SubrN) /*
::doc:rep.io.processes#start-process::
start-process [PROCESS] [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. The child-process runs
asynchronously with the editor. If PROCESS is unspecified the make-process
function will be called (with zero arguments) to create one.

PROGRAM is the filename of the binary image, it will be searched for in
all directories listed in the `PATH' environment variable.
ARGS are the arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
    struct Proc *pr = NULL;
    repv res = Qnil;
    if(rep_CONSP(arg_list))
    {
	if(PROCESSP(rep_CAR(arg_list)))
	    pr = VPROC(rep_CAR(arg_list));
	arg_list = rep_CDR(arg_list);
    }
    if(pr == NULL)
    {
	pr = VPROC(Fmake_process(Qnil, Qnil, Qnil,
				    Qnil, Qnil));
	if(pr == NULL)
	    return rep_NULL;
    }
    if(rep_CONSP(arg_list))
    {
	if(rep_STRINGP(rep_CAR(arg_list)))
	    pr->pr_Prog = rep_CAR(arg_list);
	arg_list = rep_CDR(arg_list);
	if(rep_CONSP(arg_list))
	    pr->pr_Args = arg_list;
    }
    if(!rep_STRINGP(pr->pr_Prog))
    {
	res = Fsignal(Qprocess_error, rep_list_2(rep_VAL(&no_prog), rep_VAL(pr)));
    }
    else
    {
	int numargs = rep_list_length(pr->pr_Args) + 1;
	char **argv = rep_alloc(sizeof(char *) * (numargs + 1));
	if(argv)
	{
	    int i;
	    arg_list = pr->pr_Args;
	    argv[0] = rep_STR(pr->pr_Prog);
	    for(i = 1; i < numargs; i++)
	    {
		if(rep_STRINGP(rep_CAR(arg_list)))
		    argv[i] = rep_STR(rep_CAR(arg_list));
		else
		    argv[i] = "";
		arg_list = rep_CDR(arg_list);
	    }
	    argv[i] = NULL;
	    if(run_process(pr, argv, NULL))
		res = rep_VAL(pr);
	    else
	    {
		res = Fsignal(Qprocess_error, rep_list_2(rep_VAL(&cant_start),
							   rep_VAL(pr)));
	    }
	    rep_free(argv);
	}
    }
    return(res);
}

DEFUN("call-process", Fcall_process, Scall_process, (repv arg_list), rep_SubrN) /*
::doc:rep.io.processes#call-process::
call-process [PROCESS] [IN-FILE] [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. Waits for the child to
exit, then returns the exit-value of the child. If PROCESS is unspecified
the make-process function will be called (with zero arguments) to create one.

IN-FILE is the name of the file to connect to the process' standard input,
if this is not defined `/dev/null' is used.
PROGRAM is the filename of the binary image, it will be searched for in
all directories listed in the `PATH' environment variable.
ARGS are the arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
    struct Proc *pr = NULL;
    repv res = Qnil, infile = rep_VAL(&dev_null);
    if(rep_CONSP(arg_list))
    {
	if(PROCESSP(rep_CAR(arg_list)))
	    pr = VPROC(rep_CAR(arg_list));
	arg_list = rep_CDR(arg_list);
    }
    if(pr == NULL)
    {
	pr = VPROC(Fmake_process(Qnil, Qnil, Qnil,
				    Qnil, Qnil));
	if(pr == NULL)
	    return rep_NULL;
    }
    if(rep_CONSP(arg_list))
    {
	if(rep_STRINGP(rep_CAR(arg_list)))
	    infile = rep_CAR(arg_list);
	arg_list = rep_CDR(arg_list);
	if(rep_CONSP(arg_list))
	{
	    if(rep_STRINGP(rep_CAR(arg_list)))
		pr->pr_Prog = rep_CAR(arg_list);
	    arg_list = rep_CDR(arg_list);
	    if(rep_CONSP(arg_list))
		pr->pr_Args = arg_list;
	}
    }
    if(infile != rep_VAL(&dev_null))
    {
	/* Ensure that INFILE is a real name in the local file
	   system, and that the file actually exists. */
	rep_GC_root gc_arg_list, gc_pr, gc_infile;
	repv _pr = rep_VAL(pr);
	rep_PUSHGC(gc_arg_list, arg_list);
	rep_PUSHGC(gc_pr, _pr);
	rep_PUSHGC(gc_infile, infile);
	infile = Flocal_file_name(infile);
	if(infile && rep_STRINGP(infile))
	{
	    if(rep_NILP(rep_file_exists_p(infile)))
		res = rep_signal_file_error(infile);
	}
	else
	    res = Fsignal(Qprocess_error,
			     rep_LIST_2(rep_VAL(&not_local), rep_VAL(pr)));
	rep_POPGC; rep_POPGC; rep_POPGC;
    }
    if(rep_NILP(res) && !rep_STRINGP(pr->pr_Prog))
	res = Fsignal(Qprocess_error, rep_LIST_2(rep_VAL(&no_prog), rep_VAL(pr)));
    if(rep_NILP(res))
    {
	int numargs = rep_list_length(pr->pr_Args) + 1;
	char **argv = rep_alloc(sizeof(char *) * (numargs + 1));
	if(argv)
	{
	    int i;
	    arg_list = pr->pr_Args;
	    argv[0] = rep_STR(pr->pr_Prog);
	    for(i = 1; i < numargs; i++)
	    {
		if(rep_STRINGP(rep_CAR(arg_list)))
		    argv[i] = rep_STR(rep_CAR(arg_list));
		else
		    argv[i] = "";
		arg_list = rep_CDR(arg_list);
	    }
	    argv[i] = NULL;
	    if(run_process(pr, argv, rep_STR(infile)))
		res = rep_MAKE_INT(pr->pr_ExitStatus);
	    else
	    {
		res = Fsignal(Qprocess_error, rep_list_2(rep_VAL(&cant_start),
							   rep_VAL(pr)));
	    }
	    rep_free(argv);
	}
    }
    return(res);
}

/* If PROC is running asynchronously then send signal number SIGNAL
   to it. If SIGNAL-GROUP is non-nil send the signal to all processes
   in the process group of PROC. Returns t if successful. */
static repv
do_signal_command(repv proc, int signal, repv signal_group)
{
    repv res = Qnil;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
    {
	if(signal_process(VPROC(proc), signal, !rep_NILP(signal_group)))
	    res = Qt;
    }
    else
    {
	res = Fsignal(Qprocess_error, rep_list_2(proc, rep_VAL(&not_running)));
    }
    return res;
}

DEFUN("interrupt-process", Finterrupt_process, Sinterrupt_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#interrupt-process::
interrupt-process PROCESS [SIGNAL-GROUP]

Interrupt the asynchronous process PROCESS. If SIGNAL-GROUP is t, interrupt
all child processes of PROCESS (it's process group).
::end:: */
{
    return do_signal_command(proc, SIGINT, grp);
}

DEFUN("kill-process", Fkill_process, Skill_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#kill-process::
kill-process PROCESS [SIGNAL-GROUP]

Kill the asynchronous process PROCESS. If SIGNAL-GROUP is t, kill all
child processes of PROCESS (it's process group).
::end:: */
{
    return do_signal_command(proc, SIGKILL, grp);
}

DEFUN("stop-process", Fstop_process, Sstop_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#stop-process::
stop-process PROCESS [SIGNAL-GROUP]

Suspends execution of PROCESS, see `continue-process'. If SIGNAL-GROUP is
non-nil also suspends the processes in the process group of PROCESS.
::end:: */
{
    return do_signal_command(proc, SIGSTOP, grp);
}

DEFUN("continue-process", Fcontinue_process, Scontinue_process, (repv proc, repv grp), rep_Subr2) /*
::doc:rep.io.processes#continue-process::
continue-process PROCESS [SIGNAL-GROUP]

Restarts PROCESS after it has been stopped (via `stop-process'). If
SIGNAL-GROUP is non-nil also continues the processes in the process group of
PROCESS.
::end:: */
{
    repv res = Qt;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_STOPPED_P(VPROC(proc)))
    {
	if(signal_process(VPROC(proc), SIGCONT, !rep_NILP(grp)))
	{
	    PR_SET_STATUS(VPROC(proc), PR_RUNNING);
	    res = Qt;
	    queue_notify(VPROC(proc));
	}
    }
    else
    {
	res = Fsignal(Qprocess_error, rep_list_2(proc, rep_VAL(&not_stopped)));
    }
    return(res);
}

DEFUN("signal-process", Fsignal_process, Ssignal_process,
      (repv proc, repv sig, repv grp), rep_Subr3) /*
::doc:rep.io.processes#signal_process::
signal-process PROCESS SIGNAL [SIGNAL-GROUP]

Sends the signal SIGNAL to the process PROCESS. If SIGNAL-GROUP is
non-nil also continues the processes in the process group of PROCESS.

PROCESS may be either a Lisp process object, or an integer giving the
process-id of a process (not necessarily started by rep).

SIGNAL may either be a numeric signal, or a symbol naming a signal, i.e.
the symbol `INT' for the UNIX SIGINT signal.
::end:: */
{
    static const struct {
	const char *name;
	int sig;
    } signals[] = {
#ifdef SIGFPE
	{ "FPE", SIGFPE },
#endif
#ifdef SIGILL
	{ "ILL", SIGILL },
#endif
#ifdef SIGSEGV
	{ "SEGV", SIGSEGV },
#endif
#ifdef SIGBUS
	{ "BUS", SIGBUS },
#endif
#ifdef SIGABRT
 	{ "ABRT", SIGABRT },
#endif
#ifdef SIGIOT
	{ "IOT", SIGIOT },
#endif
#ifdef SIGTRAP
	{ "TRAP", SIGTRAP },
#endif
#ifdef SIGEMT
	{ "EMT", SIGEMT },
#endif
#ifdef SIGSYS
	{ "SYS", SIGSYS },
#endif
#ifdef SIGTERM
	{ "TERM", SIGTERM },
#endif
#ifdef SIGINT
	{ "INT", SIGINT },
#endif
#ifdef SIGQUIT
	{ "QUIT", SIGQUIT },
#endif
#ifdef SIGKILL
	{ "KILL", SIGKILL },
#endif
#ifdef SIGHUP
	{ "HUP", SIGHUP },
#endif
#ifdef SIGALRM
	{ "ALRM", SIGALRM },
#endif
#ifdef SIGVTALRM
	{ "VTALRM", SIGVTALRM },
#endif
#ifdef SIGPROF
	{ "PROF", SIGPROF },
#endif
#ifdef SIGIO
	{ "IO", SIGIO },
#endif
#ifdef SIGURG
	{ "URG", SIGURG },
#endif
#ifdef SIGPOLL
	{ "POLL", SIGPOLL },
#endif
#ifdef SIGCHLD
	{ "CHLD", SIGCHLD }, { "CLD", SIGCHLD },
#endif
#ifdef SIGCONT
	{ "CONT", SIGCONT },
#endif
#ifdef SIGSTOP
	{ "STOP", SIGSTOP },
#endif
#ifdef SIGTSTP
	{ "TSTP", SIGTSTP },
#endif
#ifdef SIGTTIN
	{ "TTIN", SIGTTIN },
#endif
#ifdef SIGTTOU
	{ "TTOU", SIGTTOU },
#endif
#ifdef SIGPIPE
	{ "PIPE", SIGPIPE },
#endif
#ifdef SIGLOST
	{ "LOST", SIGLOST },
#endif
#ifdef SIGXCPU
	{ "XCPU", SIGXCPU },
#endif
#ifdef SIGXFSZ
	{ "XFSZ", SIGXFSZ },
#endif
#ifdef SIGUSR1
	{ "USR1", SIGUSR1 },
#endif
#ifdef SIGUSR2
	{ "USR2", SIGUSR2 },
#endif
#ifdef SIGWINCH
	{ "WINCH", SIGWINCH },
#endif
#ifdef SIGINFO
	{ "INFO", SIGINFO },
#endif
	{ 0 }
    };

    int signal = -1;

    rep_DECLARE(1, proc, PROCESSP(proc) || rep_INTP(proc));
    rep_DECLARE(2, sig, rep_INTP(sig) || rep_SYMBOLP(sig));

    if (rep_INTP(sig))
	signal = rep_INT(sig);
    else
    {
	char *s = rep_STR(rep_SYM(sig)->name);
	int i;
	for (i = 0; signals[i].name != 0; i++)
	{
	    if (strcmp (s, signals[i].name) == 0)
	    {
		signal = signals[i].sig;
		break;
	    }
	}
	if (signal == -1)
	    return Fsignal (Qerror, rep_list_2 (rep_VAL(&nosig), sig));
    }

    if (rep_INTP(proc) && rep_INT(proc) > 0)
    {
	struct Proc *pr = process_chain;
	while (pr != 0 && pr->pr_Pid != rep_INT(proc))
	    pr = pr->pr_Next;
	if (pr != 0)
	    proc = rep_VAL(pr);
    }

    if (PROCESSP(proc))
	return do_signal_command (proc, signal, grp);
    else
    {
	int r;
	if (grp != Qnil)
	    r = kill (- rep_INT(proc), signal);
	else
	    r = kill (rep_INT(proc), signal);
	return (r == 0) ? Qt : Qnil;
    }
}

DEFUN("process-exit-status", Fprocess_exit_status, Sprocess_exit_status, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-exit-status::
process-exit-status PROCESS

Returns the unprocessed exit-status of the last process to be run on the
process-object PROCESS. If PROCESS is currently running, return nil.
::end:: */
{
    repv res = Qnil;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_DEAD_P(VPROC(proc)))
    {
	if(VPROC(proc)->pr_ExitStatus != -1)
	    res = rep_MAKE_INT(VPROC(proc)->pr_ExitStatus);
    }
    return(res);
}

DEFUN("process-exit-value", Fprocess_exit_value, Sprocess_exit_value, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-exit-value::
process-exit-value PROCESS

Returns the return-value of the last process to be run on PROCESS, or nil if:
  a) no process has run on PROCESS
  b) PROCESS is still running
  c) PROCESS exited abnormally
::end:: */
{
    repv res = Qnil;
    rep_DECLARE1(proc, PROCESSP);
    if((PR_DEAD_P(VPROC(proc)))
       && (VPROC(proc)->pr_ExitStatus != -1))
	res = rep_MAKE_INT(WEXITSTATUS(VPROC(proc)->pr_ExitStatus));
    return(res);
}

DEFUN("process-id", Fprocess_id, Sprocess_id, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-id::
process-id [PROCESS]

If PROCESS is running or stopped, return the process-identifier associated
with it (ie, its pid).

If PROCESS is nil, return the process id of the Lisp interpreter.
::end:: */
{
    if (proc == Qnil)
	return rep_MAKE_INT(getpid ());
    else
    {
	repv res = Qnil;
	rep_DECLARE1(proc, PROCESSP);
	if(PR_ACTIVE_P(VPROC(proc)))
	    res = rep_MAKE_INT(VPROC(proc)->pr_Pid);
	return(res);
    }
}

DEFUN("process-running-p", Fprocess_running_p, Sprocess_running_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-running-p::
process-running-p PROCESS

Return t if PROCESS is running.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_RUNNING_P(VPROC(proc)))
	res = Qt;
    else
	res = Qnil;
    return(res);
}

DEFUN("process-stopped-p", Fprocess_stopped_p, Sprocess_stopped_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-stopped-p::
process-stopped-p PROCESS

Return t if PROCESS has been stopped.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_STOPPED_P(VPROC(proc)))
	res = Qt;
    else
	res = Qnil;
    return(res);
}

DEFUN("process-in-use-p", Fprocess_in_use_p, Sprocess_in_use_p, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-in-use-p::
process-in-use-p PROCESS

Similar to `process-running-p' except that this returns t even when the
process has stopped.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
	res = Qt;
    else
	res = Qnil;
    return(res);
}

DEFUN("processp", Fprocessp, Sprocessp, (repv arg), rep_Subr1) /*
::doc:rep.io.processes#process-p::
processp ARG

Return t is ARG is a process-object.
::end:: */
{
    if(PROCESSP(arg))
	return(Qt);
    return(Qnil);
}

DEFUN("process-prog", Fprocess_prog, Sprocess_prog, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-prog::
process-prog PROCESS

Return the name of the program in PROCESS.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Prog;
    return(res);
}

DEFUN("set-process-prog", Fset_process_prog, Sset_process_prog, (repv proc, repv prog), rep_Subr2) /*
::doc:rep.io.processes#set-process-prog::
set-process-prog PROCESS PROGRAM

Sets the name of the program to run on PROCESS to FILE.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    rep_DECLARE2(prog, rep_STRINGP);
    VPROC(proc)->pr_Prog = prog;
    return(prog);
}

DEFUN("process-args", Fprocess_args, Sprocess_args, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-args::
process-args PROCESS

Return the list of arguments to PROCESS.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Args;
    return(res);
}

DEFUN("set-process-args", Fset_process_args, Sset_process_args, (repv proc, repv args), rep_Subr2) /*
::doc:rep.io.processes#set-process-args::
set-process-args PROCESS ARG-LIST

Set the arguments to PROCESS.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    if(!rep_NILP(args) && !rep_CONSP(args))
	return(rep_signal_arg_error(args, 2));
    VPROC(proc)->pr_Args = args;
    return(args);
}

DEFUN("process-output-stream", Fprocess_output_stream, Sprocess_output_stream, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-output-stream::
process-output-stream PROCESS

Return the stream to which all output from PROCESS is sent.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_OutputStream;
    return(res);
}

DEFUN("set-process-output-stream", Fset_process_output_stream, Sset_process_output_stream, (repv proc, repv stream), rep_Subr2) /*
::doc:rep.io.processes#set-process-output-stream::
set-process-output-stream PROCESS STREAM

Set the output-stream of PROCESS to STREAM. nil means discard all output.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_OutputStream = stream;
    return(stream);
}

DEFUN("process-error-stream", Fprocess_error_stream, Sprocess_error_stream, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-error-stream::
process-error-stream PROCESS

Return the stream to which all standard-error output from PROCESS is sent.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_ErrorStream;
    return(res);
}

DEFUN("set-process-error-stream", Fset_process_error_stream, Sset_process_error_stream, (repv proc, repv stream), rep_Subr2) /*
::doc:rep.io.processes#set-process-error-stream::
set-process-error-stream PROCESS STREAM

Set the error-stream of PROCESS to STREAM. nil means discard all output.

Note that this currently only works correctly with pipe connections.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_ErrorStream = stream;
    return(stream);
}

DEFUN("process-function", Fprocess_function, Sprocess_function, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-function::
process-function PROCESS

Return the function which is called when PROCESS changes state (i.e. it
exits or is stopped).
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_NotifyFun;
    return(res);
}

DEFUN("set-process-function", Fset_process_function, Sset_process_function, (repv proc, repv fn), rep_Subr2) /*
::doc:rep.io.processes#set-process-function::
set-process-function PROCESS FUNCTION

Set the function which is called when PROCESS changes state to FUNCTION.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_NotifyFun = fn;
    return(fn);
}

DEFUN("process-dir", Fprocess_dir, Sprocess_dir, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-dir::
process-dir PROCESS

Return the name of the directory which becomes the working directory of
PROCESS when it is started.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Dir;
    return(res);
}

DEFUN("set-process-dir", Fset_process_dir, Sset_process_dir, (repv proc, repv dir), rep_Subr2) /*
::doc:rep.io.processes#set-process-dir::
set-process-dir PROCESS DIR

Set the directory of PROCESS to DIR.
::end:: */
{
    rep_GC_root gc_proc;
    rep_DECLARE1(proc, PROCESSP);
    rep_DECLARE2(dir, rep_STRINGP);

    /* Ensure that pr_Dir refers to an absolute local file */
    rep_PUSHGC(gc_proc, proc);
    dir = Flocal_file_name(rep_STRINGP(dir) ? dir : rep_VAL(&dot));
    rep_POPGC;
    if(dir && rep_STRINGP(dir))
	VPROC(proc)->pr_Dir = dir;
    else
	VPROC(proc)->pr_Dir = Qnil;

    return VPROC(proc)->pr_Dir;;
}

DEFUN("process-connection-type", Fprocess_connection_type, Sprocess_connection_type, (repv proc), rep_Subr1) /*
::doc:rep.io.processes#process-connection-type::
process-connection-type PROCESS

Returns a symbol defining the type of stream (i.e. pipe, pty, or
socketpair) used to connect PROCESS with its physical process.
::end:: */
{
    repv res;
    rep_DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_ConnType;
    return(res);
}

DEFUN("set-process-connection-type", Fset_process_connection_type, Sset_process_connection_type, (repv proc, repv type), rep_Subr2) /*
::doc:rep.io.processes#set-process-connection-type::
set-process-connection-type PROCESS TYPE

Define how PROCESS communicates with it's child process, TYPE may be
one of the following symbols:

  pty		Use a pty
  pipe		Three pipes are used
  socketpair	Use a socketpair

This function can only be used when PROCESS is not in use.

Note that only the `pipe' connection type allows process output and
process error output to be differentiated.
::end:: */
{
    rep_DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
	type = Fsignal(Qprocess_error, rep_list_2(rep_VAL(&in_use), proc));
    else
	VPROC(proc)->pr_ConnType = type;
    return(type);
}

DEFUN("active-processes", Factive_processes, Sactive_processes, (void),
      rep_Subr0) /*
::doc:rep.io.processes#active-processes::
active-processes

Return a list containing all active process objects.
::end:: */
{
    repv head = Qnil;
    repv *ptr = &head;
    struct Proc *p = process_chain;
    while(p != 0)
    {
	if(PR_ACTIVE_P(p))
	{
	    *ptr = Fcons(rep_VAL(p), Qnil);
	    ptr = &(rep_CDR(*ptr));
	}
	p = p->pr_Next;
    }
    return head;
}

#define MAX_HANDLERS 16
static void (*input_handlers[MAX_HANDLERS])(int);
static int n_input_handlers = 0;

void
rep_register_process_input_handler (void (*handler)(int))
{
    assert (n_input_handlers < MAX_HANDLERS);
    input_handlers[n_input_handlers++] = handler;
}

DEFUN("accept-process-output", Faccept_process_output,
      Saccept_process_output, (repv secs, repv msecs), rep_Subr2) /*
::doc:rep.io.processes#accept-process-output::
accept-process-output [SECONDS] [MILLISECONDS]

Wait SECONDS plus MILLISECONDS for output from any asynchronous subprocesses.
If any arrives, process it, then return nil. Otherwise return t.

Note that output includes notification of process termination.
::end:: */
{
    repv result = Qt;
    rep_DECLARE2_OPT(secs, rep_NUMERICP);
    rep_DECLARE3_OPT(msecs, rep_NUMERICP);
    /* Only wait for output if nothing already waiting. */
    if(!got_sigchld && !notify_chain)
    {
	result = (rep_accept_input_for_callbacks
		  ((rep_get_long_int (secs) * 1000)
		   + (rep_get_long_int (msecs)),
		   n_input_handlers, input_handlers));
    }
    if(got_sigchld || notify_chain)
    {
	result = Qnil;
	rep_proc_periodically();
    }
    return result;
}

DEFUN("accept-process-output-1", Faccept_process_output_1,
      Saccept_process_output_1,
      (repv process, repv secs, repv msecs), rep_Subr3) /*
::doc:rep.io.processes#accept-process-output-1::
accept-process-output-1 PROCESS [SECONDS] [MILLISECONDS]

Wait SECONDS plus MILLISECONDS for output from the asynchronous
subprocess PROCESS. If any arrives, process it, then return nil.
Otherwise return t.

Note that output includes notification of process termination.
::end:: */
{
    repv result = Qt;
    rep_DECLARE1 (process, PROCESSP);
    rep_DECLARE2_OPT(secs, rep_NUMERICP);
    rep_DECLARE3_OPT(msecs, rep_NUMERICP);

    /* Only wait for output if nothing already waiting. */
    if (got_sigchld)
	check_for_zombies ();

    if (!notify_queued_p (VPROC (process)))
    {
	int fds[2];
	fds[0] = VPROC (process)->pr_Stdout;
	fds[1] = VPROC (process)->pr_Stderr;
	result = (rep_accept_input_for_fds
		  ((rep_get_long_int (secs) * 1000)
		   + rep_get_long_int (msecs), 2, fds));
    }

    if (got_sigchld)
	check_for_zombies ();

    if (notify_queued_p (VPROC (process)))
    {
	notify_1 (VPROC (process));
	result = Qt;
    }

    return result;
}

/* Don't use libc system (), since it blocks signals. */
repv
rep_system (char *command)
{
    int pid, status;
    int interrupt_count = 0;

    pid = fork ();
    switch (pid)
    {
	char *argv[4];
	repv ret;
	DEFSTRING (cant_fork, "can't fork ()");

    case -1:
	return Fsignal (Qerror, Fcons (rep_VAL (&cant_fork), Qnil));

    case 0:
	child_build_environ ();
	argv[0] = "sh";
	argv[1] = "-c";
	argv[2] = command;
	argv[3] = 0;
	signal (SIGPIPE, SIG_DFL);
	execve ("/bin/sh", argv, environ);
	perror ("can't exec /bin/sh");
	_exit (255);

    default:
	ret = Qnil;
	rep_sig_restart (SIGCHLD, rep_FALSE);
	while (1)
	{
	    struct timeval timeout;
	    int x;

	    rep_TEST_INT_SLOW;
	    if (rep_INTERRUPTP)
	    {
		static int signals[] = { SIGINT, SIGTERM, SIGQUIT };
		if (interrupt_count < 3)
		    interrupt_count++;
		kill (pid, signals[interrupt_count - 1]);
		if (rep_throw_value == rep_int_cell)
		    rep_throw_value = rep_NULL;
	    }

	    x = waitpid (pid, &status, WNOHANG);
	    if (x == -1)
	    {
		if (errno != EINTR && errno != EAGAIN)
		{
		    DEFSTRING (cant_waitpid, "can't waitpid ()");
		    ret = Fsignal (Qerror,
				   Fcons (rep_VAL (&cant_waitpid), Qnil));
		    break;
		}
	    }
	    else if (x == pid)
	    {
		ret = rep_MAKE_INT (status);
		break;
	    }

	    timeout.tv_sec = 1;
	    timeout.tv_usec = 0;
	    select (FD_SETSIZE, NULL, NULL, NULL, &timeout);
	}
	rep_sig_restart (SIGCHLD, rep_TRUE);
	return ret;
    }
}

void
rep_proc_init(void)
{
    repv tem;

    /* Setup SIGCHLD stuff.  */
    sigemptyset(&chld_sigset);
    sigaddset(&chld_sigset, SIGCHLD);
    chld_sigact.sa_handler = sigchld_handler;
    chld_sigact.sa_mask = chld_sigset;
#ifdef SA_RESTART
    chld_sigact.sa_flags = SA_RESTART;
#else
    chld_sigact.sa_flags = 0;
#endif
    sigaction(SIGCHLD, &chld_sigact, NULL);

    /* Is this necessary?? Better safe than core-dumped ;-)  */
    signal(SIGPIPE, SIG_IGN);

    rep_INTERN(pipe);
    rep_INTERN(pty);
    rep_INTERN(socketpair);

    tem = rep_push_structure ("rep.io.processes");
    rep_ADD_SUBR(Sclose_process);
    rep_ADD_SUBR(Smake_process);
    rep_ADD_SUBR(Sstart_process);
    rep_ADD_SUBR(Scall_process);
    rep_ADD_SUBR(Sinterrupt_process);
    rep_ADD_SUBR(Skill_process);
    rep_ADD_SUBR(Sstop_process);
    rep_ADD_SUBR(Scontinue_process);
    rep_ADD_SUBR(Ssignal_process);
    rep_ADD_SUBR(Sprocess_exit_status);
    rep_ADD_SUBR(Sprocess_exit_value);
    rep_ADD_SUBR(Sprocess_id);
    rep_ADD_SUBR(Sprocess_running_p);
    rep_ADD_SUBR(Sprocess_stopped_p);
    rep_ADD_SUBR(Sprocess_in_use_p);
    rep_ADD_SUBR(Sprocessp);
    rep_ADD_SUBR(Sprocess_prog);
    rep_ADD_SUBR(Sset_process_prog);
    rep_ADD_SUBR(Sprocess_args);
    rep_ADD_SUBR(Sset_process_args);
    rep_ADD_SUBR(Sprocess_output_stream);
    rep_ADD_SUBR(Sset_process_output_stream);
    rep_ADD_SUBR(Sprocess_error_stream);
    rep_ADD_SUBR(Sset_process_error_stream);
    rep_ADD_SUBR(Sprocess_function);
    rep_ADD_SUBR(Sset_process_function);
    rep_ADD_SUBR(Sprocess_dir);
    rep_ADD_SUBR(Sset_process_dir);
    rep_ADD_SUBR(Sprocess_connection_type);
    rep_ADD_SUBR(Sset_process_connection_type);
    rep_ADD_SUBR(Sactive_processes);
    rep_ADD_SUBR(Saccept_process_output);
    rep_ADD_SUBR(Saccept_process_output_1);
    rep_pop_structure (tem);

    process_type = rep_register_new_type ("subprocess", rep_ptr_cmp,
					  proc_prin, proc_prin,
					  proc_sweep, proc_mark,
					  mark_active_processes,
					  0, 0, proc_putc, proc_puts, 0, 0);
    rep_register_process_input_handler (read_from_process);
    rep_add_event_loop_callback (proc_periodically);
}

void
rep_proc_kill(void)
{
    struct Proc *pr;
    signal(SIGCHLD, SIG_DFL);
    pr = process_chain;
    while(pr)
    {
	struct Proc *nxt = pr->pr_Next;
	kill_process(pr);
	pr = nxt;
    }
    process_chain = NULL;
}
