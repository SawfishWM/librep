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

#include "jade.h"
#include "jade_protos.h"

#include <sys/types.h>
#include <sys/wait.h>
#include <sys/fcntl.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <unistd.h>
#include <termios.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

static struct sigaction chld_sigact;
static sigset_t chld_sigset;

struct Proc
{
    VALUE	pr_Car;		/* status in high bits */
    struct Proc *pr_Next;
    /* Chain of all processes waiting to be notified of a change of state. */
    struct Proc *pr_NotifyNext;
    pid_t	pr_Pid;
    /* pr_Stdin is where we write, pr_Stdout where we read, they may be the
       same.  pr_Stderr is only used with pipes--it may be a separate
       connection to the stderr stream of the process. At all other times
       it will be equal to pr_Stdout. */
    int		pr_Stdin, pr_Stdout, pr_Stderr;
    VALUE	pr_OutputStream, pr_ErrorStream;
    int		pr_ExitStatus;
    VALUE	pr_NotifyFun;
    VALUE	pr_Prog;
    VALUE	pr_Args;
    VALUE	pr_Dir;
    VALUE	pr_ConnType;
};

/* Status is two bits above the type code (presently 8->9) */
#define PR_ACTIVE  (1 << (CELL8_TYPE_BITS + 0))	/* active, may be stopped */
#define PR_STOPPED (2 << (CELL8_TYPE_BITS + 1))	/* stopped */
#define PR_DEAD    0
#define PR_RUNNING PR_ACTIVE

#define PR_ACTIVE_P(p)  ((p)->pr_Car & PR_ACTIVE)
#define PR_STOPPED_P(p) ((p)->pr_Car & PR_STOPPED)
#define PR_RUNNING_P(p) (PR_ACTIVE_P(p) && !PR_STOPPED_P(p))
#define PR_DEAD_P(p)    !PR_ACTIVE_P(p)

#define PR_SET_STATUS(p,s) \
    ((p)->pr_Car = (((p)->pr_Car & ~(PR_ACTIVE | PR_STOPPED)) | (s)))

/* Connection types, pty-echo is a pty with the ECHO bit set in c_lflag */
_PR VALUE sym_pipe, sym_pty, sym_pty_echo;
DEFSYM(pipe, "pipe");
DEFSYM(pty, "pty");
DEFSYM(pty_echo, "pty-echo");

#define PR_CONN_PTY_P(p) \
    (((p)->pr_ConnType == sym_pty) || ((p)->pr_ConnType == sym_pty_echo))

#define PR_CONN_PTY_ECHO_P(p) \
    ((p)->pr_ConnType == sym_pty_echo)

#define PR_CONN_PIPE_P(p) \
    ((p)->pr_ConnType == sym_pipe)

/* Handy debugging macro */
#if 0
# define DB(x) fprintf x
#else
# define DB(x)
#endif

static struct Proc *process_chain;
static struct Proc *notify_chain;
static int process_run_count;

/* Set to TRUE by the SIGCHLD handler */
static volatile bool got_sigchld;

_PR bool proc_periodically(void);
static void read_from_one_fd(struct Proc *pr, int fd, bool cursor_on);
static void read_from_process(int);
_PR int	 write_to_process(VALUE, u_char *, int);
_PR void proc_mark(VALUE);
_PR void proc_prin(VALUE, VALUE);
_PR void sigchld_restart(bool);
_PR void proc_init(void);
_PR void proc_kill(void);

DEFSTRING(not_running, "Not running");
DEFSTRING(not_stopped, "Not stopped");
DEFSTRING(no_link, "No link to input");
DEFSTRING(in_use, "Process in use");
DEFSTRING(no_pty, "Can't find unused pty");
DEFSTRING(already_running, "Already running");
DEFSTRING(no_prog, "No program");
DEFSTRING(cant_start, "Can't start");
DEFSTRING(dev_null, "/dev/null");



static void
sigchld_handler(int sig)
{
    got_sigchld = TRUE;
}

static void
close_proc_files(struct Proc *pr)
{
    if(pr->pr_Stdout)
    {
	deregister_input_fd(pr->pr_Stdout);
	close(pr->pr_Stdout);
    }
    if(pr->pr_Stderr && pr->pr_Stderr != pr->pr_Stdout)
    {
	deregister_input_fd(pr->pr_Stderr);
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
static bool
proc_notification(void)
{
    if(!notify_chain)
	return(FALSE);
    cursor(curr_vw, CURS_OFF);
    while(notify_chain != NULL)
    {
	struct Proc *pr = notify_chain;
	notify_chain = pr->pr_NotifyNext;
	pr->pr_NotifyNext = NULL;
	if(pr->pr_NotifyFun && !NILP(pr->pr_NotifyFun))
	    funcall(pr->pr_NotifyFun, sym_nil, FALSE);
    }
    cursor(curr_vw, CURS_ON);
    return(TRUE);
}

/* Checks if any of my children are zombies, takes appropriate action. */
static bool
check_for_zombies(void)
{
    if(!got_sigchld)
	return FALSE;

    got_sigchld = FALSE;
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
		    if(WIFSTOPPED(status))
		    {
			/* Process is suspended. */
			PR_SET_STATUS(pr, PR_ACTIVE | PR_STOPPED);
			queue_notify(pr);
		    }
		    else
		    {
			/* Process is dead. */
			pr->pr_ExitStatus = status;
			process_run_count--;
			close_proc_files(pr);
			PR_SET_STATUS(pr, PR_DEAD);
			queue_notify(pr);
		    }
		    break;
		}
	    }
	    assert(pr != 0);
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
    return TRUE;
}

/* Called by the event loop after each event or timeout. Returns true
   if the display should be updated. */
bool
proc_periodically(void)
{
    bool rc = check_for_zombies();
    if(proc_notification())
	rc = TRUE;
    return rc;
}

/* Read data from FD out of PROC. If necessary it will handle
   clean up and notification. */
static void
read_from_one_fd(struct Proc *pr, int fd, bool cursor_on)
{
    VALUE stream = ((fd != pr->pr_Stdout)
		    ? pr->pr_ErrorStream : pr->pr_OutputStream);
    u_char buf[1025];
    int actual;
    if(cursor_on)
	cursor(curr_vw, CURS_OFF);
    do {
	if((actual = read(fd, buf, 1024)) > 0)
	{
	    buf[actual] = 0;
	    if(!NILP(stream))
		stream_puts(stream, buf, actual, FALSE);
	}
    } while((actual > 0) || (errno == EINTR));

    if((actual <= 0) && (errno != EWOULDBLOCK) && (errno != EAGAIN))
    {
	/* We assume EOF  */

	deregister_input_fd(fd);
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
    if(cursor_on)
	cursor(curr_vw, CURS_ON);
}

static void
read_from_process(int fd)
{
    struct Proc *pr;
    pr = process_chain;
    while(pr)
    {
	if(PR_ACTIVE_P(pr) && (pr->pr_Stdout == fd || pr->pr_Stderr == fd))
	{
	    read_from_one_fd(pr, fd, TRUE);
	}
	pr = pr->pr_Next;
    }
}

int
write_to_process(VALUE pr, u_char *buf, int bufLen)
{
    int act = 0;
    if(!PROCESSP(pr))
	return(0);
    if(PR_ACTIVE_P(VPROC(pr)))
    {
	if(VPROC(pr)->pr_Stdin == 0)
	{
	    cmd_signal(sym_process_error, list_2(pr, VAL(&no_link)));
	}
	else
	{
	    /* This will block */
	    act = write(VPROC(pr)->pr_Stdin, buf, bufLen);
	    if(act < 0)
	    {
		signal_file_error(pr);
		act = 0;
	    }
	}
    }
    else
	cmd_signal(sym_process_error, list_2(pr, VAL(&not_running)));
    return(act);
}

static bool
signal_process(struct Proc *pr, int sig, bool do_grp)
{
    bool rc = TRUE;
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
		rc = FALSE;
	}
	else
	{
	    if(PR_ACTIVE_P(pr))
		kill(-pr->pr_Pid, sig);
	    else
		rc = FALSE;
	}
    }
    else
    {
	if(PR_ACTIVE_P(pr))
	    kill(pr->pr_Pid, sig);
	else
	    rc = FALSE;
    }
    return(rc);
}

/* This is only called during GC, when the process isn't being referenced.
   it will already have been taken out of the chain.  */
static void
kill_process(struct Proc *pr)
{
    if(PR_ACTIVE_P(pr))
    {
	/* is this too heavy-handed?? */
	if(!signal_process(pr, SIGKILL, TRUE))
	    kill(-pr->pr_Pid, SIGKILL);
	waitpid(pr->pr_Pid, &pr->pr_ExitStatus, 0);
	process_run_count--;
	close_proc_files(pr);
    }
    FREE_OBJECT(pr);
}

static int
get_pty(char *slavenam)
{
    char c;
    int i, master;
    struct stat statb;
    for(c = FIRST_PTY_LETTER; c < 'z'; c++)
    {
	for(i = 0; i < 16; i++)
	{
	    sprintf(slavenam, "/dev/pty%c%x", c, i);
	    if(stat(slavenam, &statb) < 0)
		goto none;
	    if((master = open(slavenam, O_RDWR)) >= 0)
	    {
		slavenam[sizeof("/dev/")-1] = 't';
		if(access(slavenam, R_OK | W_OK) == 0)
		    return(master);
		close(master);
	    }
	}
    }
none:
    cmd_signal(sym_process_error, LIST_1(VAL(&no_pty)));
    return(-1);
}

/* does the dirty stuff of getting the process running. if SYNC_INPUT
   is non-NULL it means to run the process synchronously with it's
   stdin connected to the file SYNC_INPUT. Otherwise this function returns
   immediately after starting the process.  */
static bool
run_process(struct Proc *pr, char **argv, u_char *sync_input)
{
    bool rc = FALSE;
    if(PR_DEAD_P(pr))
    {
	bool usepty = PR_CONN_PTY_P(pr);
	char slavenam[32];
	int stdin_fds[2], stdout_fds[2], stderr_fds[2]; /* only for pipes */
	pr->pr_ExitStatus = -1;

	if(sync_input != NULL || !usepty)
	{
	    usepty = FALSE;
	    pr->pr_ConnType = sym_pipe;
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
	else if(usepty)
	{
	    pr->pr_Stdin = get_pty(slavenam);
	    pr->pr_Stdout = pr->pr_Stdin;
	    pr->pr_Stderr = pr->pr_Stdin;
	}
	if(pr->pr_Stdin)
	{
	    switch(pr->pr_Pid = fork())
	    {
	    case 0:
		if(usepty)
		{
		    int slave;
		    struct termios st;
		    if(setsid() < 0)
		    {
			perror("child: setsid()");
			exit(255);
		    }
		    if((slave = open(slavenam, O_RDWR)) < 0)
		    {
			perror("child: open(slave)");
			exit(255);
		    }
		    close(pr->pr_Stdin);
		    dup2(slave, 0);
		    dup2(slave, 1);
		    dup2(slave, 2);
		    if(slave > 2)
			close(slave);
#ifdef TIOCSCTTY
		    ioctl(slave, TIOCSCTTY, 0);
#endif
		    tcgetattr(0, &st);
		    st.c_iflag &= ~(ISTRIP | IGNCR | INLCR | IXOFF);
		    st.c_iflag |= (ICRNL | IGNPAR | BRKINT | IXON);
		    st.c_oflag &= ~OPOST;
		    st.c_cflag &= ~CSIZE;
		    st.c_cflag |= CREAD | CS8 | CLOCAL;
		    st.c_lflag &= ~(ECHO | ECHOE | ECHOK | NOFLSH | TOSTOP);
		    st.c_lflag |= ISIG;
		    if(PR_CONN_PTY_ECHO_P(pr))
			st.c_lflag |= ECHO;
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
		    tcsetattr(0, TCSANOW, &st);
		}
		else
		{
		    /* startup for pipes */
		    if(setpgid(0, 0) != 0)
		    {
			perror("setpgid");
			exit(255);
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
		if(STRINGP(pr->pr_Dir) && (STRING_LEN(pr->pr_Dir) > 0))
		    chdir(VSTR(pr->pr_Dir));
		execvp(argv[0], argv);
		perror("child: execvp");
		exit(255);
	    case -1:
		perror("fork()");
		break;
	    default:
		PR_SET_STATUS(pr, PR_RUNNING);
		if(!usepty)
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
		    fcntl(pr->pr_Stdin, F_SETFD, 1);
		    fcntl(pr->pr_Stdout, F_SETFD, 1);
		    fcntl(pr->pr_Stdout, F_SETFL, O_NONBLOCK);
		    register_input_fd(pr->pr_Stdout, read_from_process);
		    if(pr->pr_Stderr != pr->pr_Stdout)
		    {
			fcntl(pr->pr_Stderr, F_SETFD, 1);
			fcntl(pr->pr_Stderr, F_SETFL, O_NONBLOCK);
			register_input_fd(pr->pr_Stderr, read_from_process);
		    }
		    process_run_count++;
		}
		else
		{
		    /* Run synchronously.  */
		    u_char buf[1025];
		    int actual;
		    fd_set inputs;
		    bool done_out = FALSE, done_err = FALSE;
		    int interrupt_count = 0;

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
			number = select(FD_SETSIZE, &copy, NULL,
					NULL, &timeout);
			TEST_INT;
			if(INT_P)
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
			    signal_process(pr, signal, TRUE);
			}
			if(number > 0)
			{
			    if(!done_out && FD_ISSET(pr->pr_Stdout, &copy))
			    {
				actual = read(pr->pr_Stdout, buf, 1024);
				if(actual > 0)
				{
				    buf[actual] = 0;
				    if(!NILP(pr->pr_OutputStream))
				    {
					stream_puts(pr->pr_OutputStream, buf,
						    actual, FALSE);
				    }
				}
				else if(actual == 0
					|| (errno != EINTR
					    && errno != EAGAIN
					    && errno != EWOULDBLOCK))
				{
				    done_out = TRUE;
				    FD_CLR(pr->pr_Stdout, &inputs);
				}
			    }
			    if(!done_err && FD_ISSET(pr->pr_Stderr, &copy))
			    {
				actual = read(pr->pr_Stderr, buf, 1024);
				if(actual > 0)
				{
				    buf[actual] = 0;
				    if(!NILP(pr->pr_ErrorStream))
				    {
					stream_puts(pr->pr_ErrorStream, buf,
						    actual, FALSE);
				    }
				}
				else if(actual == 0
					|| (errno != EINTR
					    && errno != EAGAIN
					    && errno != EWOULDBLOCK))
				{
				    done_err = TRUE;
				    FD_CLR(pr->pr_Stderr, &inputs);
				}
			    }
			}
		    }
		    waitpid(pr->pr_Pid, &pr->pr_ExitStatus, 0);
		    close(pr->pr_Stdout);
		    close(pr->pr_Stderr);
		    pr->pr_Stdout = 0;
		    pr->pr_Stderr = 0;
		    PR_SET_STATUS(pr, PR_DEAD);
		    queue_notify(pr);
		}
		rc = TRUE;
		break;
	    }
	}
    }
    else
    {
	cmd_signal(sym_process_error, list_2(VAL(pr), VAL(&already_running)));
    }
    return(rc);
}

void
proc_mark(VALUE pr)
{
    MARKVAL(VPROC(pr)->pr_OutputStream);
    MARKVAL(VPROC(pr)->pr_ErrorStream);
    MARKVAL(VPROC(pr)->pr_NotifyFun);
    MARKVAL(VPROC(pr)->pr_Prog);
    MARKVAL(VPROC(pr)->pr_Args);
    MARKVAL(VPROC(pr)->pr_Dir);
    MARKVAL(VPROC(pr)->pr_ConnType);
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
	if(GC_CELL_MARKEDP(VAL(pr)))
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
	if(!GC_CELL_MARKEDP(VAL(pr)))
	    kill_process(pr);
	else
	{
	    GC_CLR_CELL(VAL(pr));
	    pr->pr_Next = process_chain;
	    process_chain = pr;
	}
	pr = nxt;
    }
}

void
proc_prin(VALUE strm, VALUE obj)
{
    struct Proc *pr = VPROC(obj);
    u_char buf[40];
    stream_puts(strm, "#<process", -1, FALSE);
    if(PR_RUNNING_P(pr))
    {
	stream_puts(strm, " running: ", -1, FALSE);
	stream_puts(strm, VPTR(pr->pr_Prog), -1, TRUE);
    }
    else if(PR_STOPPED_P(pr))
    {
	stream_puts(strm, " stopped: ", -1, FALSE);
	stream_puts(strm, VPTR(pr->pr_Prog), -1, TRUE);
    }
    else
    {
	if(pr->pr_ExitStatus != -1)
	{
	    sprintf(buf, " exited: 0x%x", pr->pr_ExitStatus);
	    stream_puts(strm, buf, -1, FALSE);
	}
    }
    stream_putc(strm, '>');
}

_PR VALUE cmd_make_process(VALUE stream, VALUE fun, VALUE dir, VALUE prog, VALUE args);
DEFUN("make-process", cmd_make_process, subr_make_process, (VALUE stream, VALUE fun, VALUE dir, VALUE prog, VALUE args), V_Subr5, DOC_make_process) /*
::doc:make_process::
make-process [OUTPUT-STREAM] [FUN] [DIR] [PROGRAM] [ARGS]

Creates a new process-object, OUTPUT-STREAM is where all output from this
process goes, both stdout and stderr, FUN is a function to call each time
the process running on this object changes state. DIR is the process'
current directory, PROGRAM the filename of the program to run and ARGS a
list of arguments passed to the process.

Any of the arguments may be unspecified, in which case they can be set
either by the functions provided or by the function called to create the
actual running process.
::end:: */
{
    struct Proc *pr = ALLOC_OBJECT(sizeof(struct Proc));
    if(pr)
    {
	pr->pr_Car = V_Process;
	pr->pr_Next = process_chain;
	process_chain = pr;
	pr->pr_NotifyNext = NULL;
	PR_SET_STATUS(pr, PR_DEAD);
	pr->pr_Pid = 0;
	pr->pr_Stdin = pr->pr_Stdout = 0;
	pr->pr_ExitStatus = -1;
	pr->pr_OutputStream = stream;
	pr->pr_ErrorStream = stream;
	pr->pr_NotifyFun = fun;
	pr->pr_Prog = prog;
	pr->pr_Args = args;
	pr->pr_Dir = dir;
	pr->pr_ConnType = sym_pipe;
	return(VAL(pr));
    }
    return(mem_error());
}

_PR VALUE cmd_start_process(VALUE arg_list);
DEFUN("start-process", cmd_start_process, subr_start_process, (VALUE arg_list), V_SubrN, DOC_start_process) /*
::doc:start_process::
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
    VALUE res = sym_nil;
    if(CONSP(arg_list))
    {
	if(PROCESSP(VCAR(arg_list)))
	    pr = VPROC(VCAR(arg_list));
	arg_list = VCDR(arg_list);
    }
    if(pr == NULL)
    {
	pr = VPROC(cmd_make_process(sym_nil, sym_nil, sym_nil,
				    sym_nil, sym_nil));
	if(pr == NULL)
	    return LISP_NULL;
    }
    if(CONSP(arg_list))
    {
	if(STRINGP(VCAR(arg_list)))
	    pr->pr_Prog = VCAR(arg_list);
	arg_list = VCDR(arg_list);
	if(CONSP(arg_list))
	    pr->pr_Args = arg_list;
    }
    if(!STRINGP(pr->pr_Prog))
    {
	res = cmd_signal(sym_process_error, list_2(VAL(&no_prog), VAL(pr)));
    }
    else
    {
	int numargs = list_length(pr->pr_Args) + 1;
	char **argv = str_alloc(sizeof(char *) * (numargs + 1));
	if(argv)
	{
	    int i;
	    arg_list = pr->pr_Args;
	    argv[0] = VSTR(pr->pr_Prog);
	    for(i = 1; i < numargs; i++)
	    {
		if(STRINGP(VCAR(arg_list)))
		    argv[i] = VSTR(VCAR(arg_list));
		else
		    argv[i] = "";
		arg_list = VCDR(arg_list);
	    }
	    argv[i] = NULL;
	    if(run_process(pr, argv, NULL))
		res = VAL(pr);
	    else
	    {
		res = cmd_signal(sym_process_error, list_2(VAL(&cant_start),
							   VAL(pr)));
	    }
	    str_free(argv);
	}
    }
    return(res);
}

_PR VALUE cmd_call_process(VALUE arg_list);
DEFUN("call-process", cmd_call_process, subr_call_process, (VALUE arg_list), V_SubrN, DOC_call_process) /*
::doc:call_process::
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
    VALUE res = sym_nil, infile = VAL(&dev_null);
    if(CONSP(arg_list))
    {
	if(PROCESSP(VCAR(arg_list)))
	    pr = VPROC(VCAR(arg_list));
	arg_list = VCDR(arg_list);
    }
    if(pr == NULL)
    {
	pr = VPROC(cmd_make_process(sym_nil, sym_nil, sym_nil,
				    sym_nil, sym_nil));
	if(pr == NULL)
	    return LISP_NULL;
    }
    if(CONSP(arg_list))
    {
	if(STRINGP(VCAR(arg_list)))
	    infile = VCAR(arg_list);
	arg_list = VCDR(arg_list);
	if(CONSP(arg_list))
	{
	    if(STRINGP(VCAR(arg_list)))
		pr->pr_Prog = VCAR(arg_list);
	    arg_list = VCDR(arg_list);
	    if(CONSP(arg_list))
		pr->pr_Args = arg_list;
	}
    }
    if(!STRINGP(pr->pr_Prog))
    {
	res = cmd_signal(sym_process_error, LIST_2(VAL(&no_prog), VAL(pr)));
    }
    else if(!file_exists(VSTR(infile)))
	res = signal_file_error(infile);
    else
    {
	int numargs = list_length(pr->pr_Args) + 1;
	char **argv = str_alloc(sizeof(char *) * (numargs + 1));
	if(argv)
	{
	    int i;
	    arg_list = pr->pr_Args;
	    argv[0] = VSTR(pr->pr_Prog);
	    for(i = 1; i < numargs; i++)
	    {
		if(STRINGP(VCAR(arg_list)))
		    argv[i] = VSTR(VCAR(arg_list));
		else
		    argv[i] = "";
		arg_list = VCDR(arg_list);
	    }
	    argv[i] = NULL;
	    if(run_process(pr, argv, VSTR(infile)))
		res = MAKE_INT(pr->pr_ExitStatus);
	    else
	    {
		res = cmd_signal(sym_process_error, list_2(VAL(&cant_start),
							   VAL(pr)));
	    }
	    str_free(argv);
	}
    }
    return(res);
}

_PR VALUE cmd_call_process_area(VALUE arg_list);
DEFUN("call-process-area", cmd_call_process_area, subr_call_process_area, (VALUE arg_list), V_SubrN, DOC_call_process_area) /*
::doc:call_process_area::
call-process-area [PROCESS] START END DELETEP [PROGRAM] [ARGS...]

Starts a process running on process-object PROCESS. Waits for the child to
exit, then returns the exit-value of the child. If PROCESS is unspecified
the make-process function will be called (with zero arguments) to create one.

The area of the current buffer between START and END is used as the
input stream of the new process. If DELETE-P is non-nil the area will
be deleted from the buffer before the process is started.

PROGRAM is the filename of the binary image, it will be searched for in
all directories listed in the `PATH' environment variable.
ARGS are the arguments to give to the process.

If any of the optional parameters are unspecified they should have been
set in the PROCESS prior to calling this function.
::end:: */
{
    if(CONSP(arg_list))
    {
	VALUE proc = VCAR(arg_list);
	arg_list = VCDR(arg_list);
	if(CONSP(arg_list) && POSP(VCAR(arg_list)))
	{
	    VALUE start = VCAR(arg_list);
	    arg_list = VCDR(arg_list);
	    if(CONSP(arg_list) && POSP(VCAR(arg_list)))
	    {
		VALUE end = VCAR(arg_list);
		arg_list = VCDR(arg_list);
		if(CONSP(arg_list))
		{
		    bool deletep = !NILP(VCAR(arg_list));
		    VALUE temp_file;
		    VALUE ret;
		    arg_list = VCDR(arg_list);
		    temp_file = cmd_tmp_file_name();
		    if(temp_file && STRINGP(temp_file))
		    {
			/* Open the file to make it private. */
			int fd = open(VSTR(temp_file),
				      O_RDWR | O_CREAT | O_TRUNC,
				      S_IRUSR | S_IWUSR);
			if(fd < 0)
			    return signal_file_error(temp_file);
			close(fd);
		    }
		    ret = cmd_write_buffer_contents(temp_file, start, end,
						    sym_nil);
		    if(ret && !NILP(ret))
		    {
			if(deletep)
			{
			    ret = cmd_delete_area(start, end, sym_nil);
			    if(!ret || NILP(ret))
				goto error;
			}
			/* Splice together the arguments to call-process.
			   PROC FILE-NAME REST.. */
			arg_list = cmd_cons(proc, cmd_cons(temp_file,
							   arg_list));
			ret = cmd_call_process(arg_list);
		    }
		error:
		    unlink(VSTR(temp_file));	/* ignore errors! */
		    return ret;
		}
	    }
	}
    }
    return cmd_signal(sym_bad_arg, list_1(arg_list));
}

/* If PROC is running asynchronously then send signal number SIGNAL
   to it. If SIGNAL-GROUP is non-nil send the signal to all processes
   in the process group of PROC. Returns t if successful. */
static VALUE
do_signal_command(VALUE proc, int signal, VALUE signal_group)
{
    VALUE res = sym_nil;
    DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
    {
	if(signal_process(VPROC(proc), signal, !NILP(signal_group)))
	    res = sym_t;
    }
    else
    {
	res = cmd_signal(sym_process_error, list_2(proc, VAL(&not_running)));
    }
    return res;
}

_PR VALUE cmd_interrupt_process(VALUE proc, VALUE grp);
DEFUN("interrupt-process", cmd_interrupt_process, subr_interrupt_process, (VALUE proc, VALUE grp), V_Subr2, DOC_interrupt_process) /*
::doc:interrupt_process::
interrupt-process PROCESS [SIGNAL-GROUP]

Interrupt the asynchronous process PROCESS. If SIGNAL-GROUP is t, interrupt
all child processes of PROCESS (it's process group).
::end:: */
{
    return do_signal_command(proc, SIGINT, grp);
}

_PR VALUE cmd_kill_process(VALUE proc, VALUE grp);
DEFUN("kill-process", cmd_kill_process, subr_kill_process, (VALUE proc, VALUE grp), V_Subr2, DOC_kill_process) /*
::doc:kill_process::
kill-process PROCESS [SIGNAL-GROUP]

Kill the asynchronous process PROCESS. If SIGNAL-GROUP is t, kill all
child processes of PROCESS (it's process group).
::end:: */
{
    return do_signal_command(proc, SIGKILL, grp);
}

_PR VALUE cmd_stop_process(VALUE proc, VALUE grp);
DEFUN("stop-process", cmd_stop_process, subr_stop_process, (VALUE proc, VALUE grp), V_Subr2, DOC_stop_process) /*
::doc:stop_process::
stop-process PROCESS [SIGNAL-GROUP]

Suspends execution of PROCESS, see `continue-process'. If SIGNAL-GROUP is
non-nil also suspends the processes in the process group of PROCESS.
::end:: */
{
    return do_signal_command(proc, SIGSTOP, grp);
}

_PR VALUE cmd_continue_process(VALUE proc, VALUE grp);
DEFUN("continue-process", cmd_continue_process, subr_continue_process, (VALUE proc, VALUE grp), V_Subr2, DOC_continue_process) /*
::doc:continue_process::
continue-process PROCESS [SIGNAL-GROUP]

Restarts PROCESS after it has been stopped (via `stop-process'). If
SIGNAL-GROUP is non-nil also continues the processes in the process group of
PROCESS.
::end:: */
{
    VALUE res = sym_t;
    DECLARE1(proc, PROCESSP);
    if(PR_STOPPED_P(VPROC(proc)))
    {
	if(signal_process(VPROC(proc), SIGCONT, !NILP(grp)))
	{
	    PR_SET_STATUS(VPROC(proc), PR_RUNNING);
	    res = sym_t;
	    queue_notify(VPROC(proc));
	}
    }
    else
    {
	res = cmd_signal(sym_process_error, list_2(proc, VAL(&not_stopped)));
    }
    return(res);
}

_PR VALUE cmd_process_exit_status(VALUE proc);
DEFUN("process-exit-status", cmd_process_exit_status, subr_process_exit_status, (VALUE proc), V_Subr1, DOC_process_exit_status) /*
::doc:process_exit_status::
process-exit-status PROCESS

Returns the unprocessed exit-status of the last process to be run on the
process-object PROCESS. If PROCESS is currently running, return nil.
::end:: */
{
    VALUE res = sym_nil;
    DECLARE1(proc, PROCESSP);
    if(PR_DEAD_P(VPROC(proc)))
    {
	if(VPROC(proc)->pr_ExitStatus != -1)
	    res = MAKE_INT(VPROC(proc)->pr_ExitStatus);
    }
    return(res);
}

_PR VALUE cmd_process_exit_value(VALUE proc);
DEFUN("process-exit-value", cmd_process_exit_value, subr_process_exit_value, (VALUE proc), V_Subr1, DOC_process_exit_value) /*
::doc:process_exit_value::
process-exit-value PROCESS

Returns the return-value of the last process to be run on PROCESS, or nil if:
  a) no process has run on PROCESS
  b) PROCESS is still running
  c) PROCESS exited abnormally
::end:: */
{
    VALUE res = sym_nil;
    DECLARE1(proc, PROCESSP);
    if((PR_DEAD_P(VPROC(proc)))
       && (VPROC(proc)->pr_ExitStatus != -1))
	res = MAKE_INT(WEXITSTATUS(VPROC(proc)->pr_ExitStatus));
    return(res);
}

_PR VALUE cmd_process_id(VALUE proc);
DEFUN("process-id", cmd_process_id, subr_process_id, (VALUE proc), V_Subr1, DOC_process_id) /*
::doc:process_id::
process-id PROCESS

If PROCESS is running or stopped, return the process-identifier associated
with it (ie, its pid).
::end:: */
{
    VALUE res = sym_nil;
    DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
	res = MAKE_INT(VPROC(proc)->pr_Pid);
    return(res);
}

_PR VALUE cmd_process_running_p(VALUE proc);
DEFUN("process-running-p", cmd_process_running_p, subr_process_running_p, (VALUE proc), V_Subr1, DOC_process_running_p) /*
::doc:process_running_p::
process-running-p PROCESS

Return t if PROCESS is running.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    if(PR_RUNNING_P(VPROC(proc)))
	res = sym_t;
    else
	res = sym_nil;
    return(res);
}

_PR VALUE cmd_process_stopped_p(VALUE proc);
DEFUN("process-stopped-p", cmd_process_stopped_p, subr_process_stopped_p, (VALUE proc), V_Subr1, DOC_process_stopped_p) /*
::doc:process_stopped_p::
process-stopped-p PROCESS

Return t if PROCESS has been stopped.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    if(PR_STOPPED_P(VPROC(proc)))
	res = sym_t;
    else
	res = sym_nil;
    return(res);
}

_PR VALUE cmd_process_in_use_p(VALUE proc);
DEFUN("process-in-use-p", cmd_process_in_use_p, subr_process_in_use_p, (VALUE proc), V_Subr1, DOC_process_in_use_p) /*
::doc:process_in_use_p::
process-in-use-p PROCESS

Similar to `process-running-p' except that this returns t even when the
process has stopped.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
	res = sym_t;
    else
	res = sym_nil;
    return(res);
}

_PR VALUE cmd_processp(VALUE arg);
DEFUN("processp", cmd_processp, subr_processp, (VALUE arg), V_Subr1, DOC_process_p) /*
::doc:process_p::
processp ARG

Return t is ARG is a process-object.
::end:: */
{
    if(PROCESSP(arg))
	return(sym_t);
    return(sym_nil);
}

_PR VALUE cmd_process_prog(VALUE proc);
DEFUN("process-prog", cmd_process_prog, subr_process_prog, (VALUE proc), V_Subr1, DOC_process_prog) /*
::doc:process_prog::
process-prog PROCESS

Return the name of the program in PROCESS.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Prog;
    return(res);
}

_PR VALUE cmd_set_process_prog(VALUE proc, VALUE prog);
DEFUN("set-process-prog", cmd_set_process_prog, subr_set_process_prog, (VALUE proc, VALUE prog), V_Subr2, DOC_set_process_prog) /*
::doc:set_process_prog::
set-process-prog PROCESS PROGRAM

Sets the name of the program to run on PROCESS to FILE.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    DECLARE2(prog, STRINGP);
    VPROC(proc)->pr_Prog = prog;
    return(prog);
}

_PR VALUE cmd_process_args(VALUE proc);
DEFUN("process-args", cmd_process_args, subr_process_args, (VALUE proc), V_Subr1, DOC_process_args) /*
::doc:process_args::
process-args PROCESS

Return the list of arguments to PROCESS.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Args;
    return(res);
}

_PR VALUE cmd_set_process_args(VALUE proc, VALUE args);
DEFUN("set-process-args", cmd_set_process_args, subr_set_process_args, (VALUE proc, VALUE args), V_Subr2, DOC_set_process_args) /*
::doc:set_process_args::
set-process-args PROCESS ARG-LIST

Set the arguments to PROCESS.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    if(!NILP(args) && !CONSP(args))
	return(signal_arg_error(args, 2));
    VPROC(proc)->pr_Args = args;
    return(args);
}

_PR VALUE cmd_process_output_stream(VALUE proc);
DEFUN("process-output-stream", cmd_process_output_stream, subr_process_output_stream, (VALUE proc), V_Subr1, DOC_process_output_stream) /*
::doc:process_output_stream::
process-output-stream PROCESS

Return the stream to which all output from PROCESS is sent.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_OutputStream;
    return(res);
}

_PR VALUE cmd_set_process_output_stream(VALUE proc, VALUE stream);
DEFUN("set-process-output-stream", cmd_set_process_output_stream, subr_set_process_output_stream, (VALUE proc, VALUE stream), V_Subr2, DOC_set_process_output_stream) /*
::doc:set_process_output_stream::
set-process-output-stream PROCESS STREAM

Set the output-stream of PROCESS to STREAM. nil means discard all output.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_OutputStream = stream;
    return(stream);
}

_PR VALUE cmd_process_error_stream(VALUE proc);
DEFUN("process-error-stream", cmd_process_error_stream, subr_process_error_stream, (VALUE proc), V_Subr1, DOC_process_error_stream) /*
::doc:process_error_stream::
process-error-stream PROCESS

Return the stream to which all standard-error output from PROCESS is sent.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_ErrorStream;
    return(res);
}

_PR VALUE cmd_set_process_error_stream(VALUE proc, VALUE stream);
DEFUN("set-process-error-stream", cmd_set_process_error_stream, subr_set_process_error_stream, (VALUE proc, VALUE stream), V_Subr2, DOC_set_process_error_stream) /*
::doc:set_process_error_stream::
set-process-error-stream PROCESS STREAM

Set the error-stream of PROCESS to STREAM. nil means discard all output.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_ErrorStream = stream;
    return(stream);
}

_PR VALUE cmd_process_function(VALUE proc);
DEFUN("process-function", cmd_process_function, subr_process_function, (VALUE proc), V_Subr1, DOC_process_function) /*
::doc:process_function::
process-function PROCESS

Return the function which is called when PROCESS changes state (i.e. it
exits or is stopped).
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_NotifyFun;
    return(res);
}

_PR VALUE cmd_set_process_function(VALUE proc, VALUE fn);
DEFUN("set-process-function", cmd_set_process_function, subr_set_process_function, (VALUE proc, VALUE fn), V_Subr2, DOC_set_process_function) /*
::doc:set_process_function::
set-process-function PROCESS FUNCTION

Set the function which is called when PROCESS changes state to FUNCTION.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_NotifyFun = fn;
    return(fn);
}

_PR VALUE cmd_process_dir(VALUE proc);
DEFUN("process-dir", cmd_process_dir, subr_process_dir, (VALUE proc), V_Subr1, DOC_process_dir) /*
::doc:process_dir::
process-dir PROCESS

Return the name of the directory which becomes the working directory of
PROCESS when it is started.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_Dir;
    return(res);
}

_PR VALUE cmd_set_process_dir(VALUE proc, VALUE dir);
DEFUN("set-process-dir", cmd_set_process_dir, subr_set_process_dir, (VALUE proc, VALUE dir), V_Subr2, DOC_set_process_dir) /*
::doc:set_process_dir::
set-process-dir PROCESS DIR

Set the directory of PROCESS to DIR.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    VPROC(proc)->pr_Dir = dir;
    return(dir);
}

_PR VALUE cmd_process_connection_type(VALUE proc);
DEFUN("process-connection-type", cmd_process_connection_type, subr_process_connection_type, (VALUE proc), V_Subr1, DOC_process_connection_type) /*
::doc:process_connection_type::
process-connection-type PROCESS

Returns a symbol defining the type of stream (i.e. pipe or pty) used to
connect PROCESS with its physical process.
::end:: */
{
    VALUE res;
    DECLARE1(proc, PROCESSP);
    res = VPROC(proc)->pr_ConnType;
    return(res);
}

_PR VALUE cmd_set_process_connection_type(VALUE proc, VALUE type);
DEFUN("set-process-connection-type", cmd_set_process_connection_type, subr_set_process_connection_type, (VALUE proc, VALUE type), V_Subr2, DOC_set_process_connection_type) /*
::doc:set_process_connection_type::
set-process-connection-type PROCESS TYPE

Define how PROCESS communicates with it's child process, TYPE can be one of
the following symbols,
  pty		Use a pty
  pty-echo	Similar to `pty' but the ECHO flag is set in the slave
  pipe		Two pipes are used

This function can only be used when PROCESS is not in use.
::end:: */
{
    DECLARE1(proc, PROCESSP);
    if(PR_ACTIVE_P(VPROC(proc)))
	type = cmd_signal(sym_process_error, list_2(VAL(&in_use), proc));
    else
	VPROC(proc)->pr_ConnType = type;
    return(type);
}

/* Turns on or off restarted system calls */
void
sigchld_restart(bool flag)
{
    if(flag)
    {
#ifdef SA_RESTART
	chld_sigact.sa_flags |= SA_RESTART;
#else
# ifdef SA_INTERRUPT
	chld_sigact.sa_flags &= ~SA_INTERRUPT;
# endif
#endif
    }
    else
    {
#ifdef SA_RESTART
	chld_sigact.sa_flags &= ~SA_RESTART;
#else
# ifdef SA_INTERRUPT
	chld_sigact.sa_flags |= SA_INTERRUPT;
# endif
#endif
    }
    sigaction(SIGCHLD, &chld_sigact, NULL);
}

void
proc_init(void)
{
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

    setpgid(0, 0);

    INTERN(pipe);
    INTERN(pty);
    INTERN(pty_echo);
    ADD_SUBR(subr_make_process);
    ADD_SUBR(subr_start_process);
    ADD_SUBR(subr_call_process);
    ADD_SUBR(subr_call_process_area);
    ADD_SUBR(subr_interrupt_process);
    ADD_SUBR(subr_kill_process);
    ADD_SUBR(subr_stop_process);
    ADD_SUBR(subr_continue_process);
    ADD_SUBR(subr_process_exit_status);
    ADD_SUBR(subr_process_exit_value);
    ADD_SUBR(subr_process_id);
    ADD_SUBR(subr_process_running_p);
    ADD_SUBR(subr_process_stopped_p);
    ADD_SUBR(subr_process_in_use_p);
    ADD_SUBR(subr_processp);
    ADD_SUBR(subr_process_prog);
    ADD_SUBR(subr_set_process_prog);
    ADD_SUBR(subr_process_args);
    ADD_SUBR(subr_set_process_args);
    ADD_SUBR(subr_process_output_stream);
    ADD_SUBR(subr_set_process_output_stream);
    ADD_SUBR(subr_process_error_stream);
    ADD_SUBR(subr_set_process_error_stream);
    ADD_SUBR(subr_process_function);
    ADD_SUBR(subr_set_process_function);
    ADD_SUBR(subr_process_dir);
    ADD_SUBR(subr_set_process_dir);
    ADD_SUBR(subr_process_connection_type);
    ADD_SUBR(subr_set_process_connection_type);

    /* Initialise the type information. */
    data_types[V_Process].compare = ptr_cmp;
    data_types[V_Process].princ = proc_prin;
    data_types[V_Process].print = proc_prin;
    data_types[V_Process].sweep = proc_sweep;
}

void
proc_kill(void)
{
    struct Proc *pr;
    pr = process_chain;
    while(pr)
    {
	struct Proc *nxt = pr->pr_Next;
	kill_process(pr);
	pr = nxt;
    }
    process_chain = NULL;
    signal(SIGCHLD, SIG_IGN);
}
