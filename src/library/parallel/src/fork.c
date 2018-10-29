/*
 *  R : A Computer Language for Statistical Data Analysis
 *  (C) Copyright 2008-2011 Simon Urbanek
 *      Copyright 2011-2018 R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/

   fork.c
   interface to system-level tools for spawning copies of the current
   process and IPC
   
   Derived from multicore version 0.1-8 by Simon Urbanek
*/
/* #define MC_DEBUG */

#ifdef HAVE_CONFIG_H
# include <config.h> /* for affinity function checks and sigaction */
#endif
#define NO_NLS
#include <Defn.h> /* for R_isForkedChild */

#include "parallel.h"

#include <sys/types.h>
#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif
#include <sys/select.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>

#include <Rinterface.h> /* for R_Interactive */
#include <R_ext/eventloop.h> /* for R_SelectEx */

/* NOTE: the logging is not safe to use in signal handler because printf is
   not async-signal-safe */
#ifndef FILE_LOG
/* use printf instead of Rprintf for debugging to avoid forked console interactions */
#define Dprintf printf
#else
/* logging into a file */
#include <stdarg.h>
void Dprintf(char *format, ...) {
    va_list (args);
    va_start (args, format);
    FILE *f = fopen("mc_debug.txt", "a");
    if (f) {
	fprintf(f, "%d> ", getpid());
	vfprintf(f, format, args);
	fclose(f);
    }
    va_end (args);
}
#endif

/* A child is created in mc_fork as detached (sEstranged=TRUE, has sifd
   and pfd set to -1) or attached (sifd and pfd connected to pipes). The list
   of children also includes cleanup marks.

   A detached child is not visible to R user code (children(), mccollect(),
   readChild(), selectChildren(), rmChild(), etc). Upon receiving sigchld,
   a detached child is waited-for from the signal handler and waitedfor is
   set. The child is eventually removed from the list by compact_children.
   User R code must never do anything with a detached child to avoid race
   conditions/unpredictable behavior due to PID reuse. A detached child
   can never become attached.

   An attached child is visible to R user code and always has file descriptors
   sifd and pifd open and >= 0. It becomes detached via readChild() when it
   returns an integer (signalling to user that the child is finishing or has
   failed). An attached child is never waited for in the signal handler as
   user R code is allowed to invoke operations on the child, such as kill - if
   an attached child could have been waited for, PID reuse may lead to a
   wrong process being killed. Instead, the child will be waited for when
   detached. Detaching a child atomically involves a non-blocking waitpid to
   check whether it already terminated, and closing the pfd and sifd. An
   attached child can also be terminated and detached by user code via rmChild.

   Note that if an attached child crashes (and sigchld is received though not
   acted upon), its file descriptors will be closed, and hence selectChildren
   in select will be notified of EOF on pfd and eventually readChild will
   detach the child.

   A cleanup mark is a child entry that is detached, waited-for, and has a
   pid of -1. Hence, it is easy to skip in most code accessing the list.
   The mark is created by mc_prepare_cleanup. mc_cleanup removes all children
   up to and including the cleanup mark (terminates and detaches the children
   and optionally also sends them kill signals and actively waits for them
   to terminate).

   When R is forked by mc_fork, the children list is cleaned in the child,
   freeing the entries and closing the file descriptors, so that the entries
   belonging to the parent do not end up in the child.  However, R could
   also be forked by some other function outside of parallel.  To be robust
   against that, parallel always checks whether the child still belongs to
   it.  We could use getppid for that, but it would not work for cleanup
   marks, hence there is also ppid field in child entries.
*/
typedef struct child_info {
    pid_t pid;      /* child's pid */
    int pfd, sifd;  /* master's ends of pipes */
    int detached;   /* run with mcfork(estranged=TRUE) or manually removed */
    int waitedfor;  /* the child has been reaped */
    pid_t ppid;     /* parent's pid when the child/mark is created */
    struct child_info *next;
} child_info_t;

static child_info_t *children; /* in master, linked list of details of children */

static int master_fd = -1; /* in child, write end of data pipe */
static int is_master = 1; /* 0 in child */

/* must only be called on detached child, not waited for,
   not re-entrant (ok when called with sigchld blocked or
   from signal handler established without SA_NODEFER) */
static void wait_for_child_ci(child_info_t *ci) {
    int wstat;

    if (waitpid(ci->pid, &wstat, WNOHANG) == ci->pid &&
        (WIFEXITED(wstat) || WIFSIGNALED(wstat))) {

	ci->waitedfor = 1;
#ifdef MC_DEBUG
	/* FIXME: printing not safe from signal handler */
	if (WIFEXITED(wstat))
	    Dprintf("child %d terminated with exit status %d\n", ci->pid,
	            WEXITSTATUS(wstat));
	else
	    Dprintf("child %d terminated by signal %d\n", ci->pid,
	            WTERMSIG(wstat));
#endif
    }
}

static void block_sigchld(sigset_t *oldset)
{
    sigset_t ss;
    sigemptyset(&ss);
    sigaddset(&ss, SIGCHLD);
    sigprocmask(SIG_BLOCK, &ss, oldset);
}

static void restore_sigchld(sigset_t *oldset)
{
    sigprocmask(SIG_SETMASK, oldset, NULL);
}

/* must not be called from a signal handler */
static void close_fds_child_ci(child_info_t *ci)
{
    /* note the check and close is not atomic */
    if (ci->pfd > 0) { close(ci->pfd); ci->pfd = -1; }
    if (ci->sifd > 0) { close(ci->sifd); ci->sifd = -1; }
}

/* must only be called on attached child */
static void kill_and_detach_child_ci(child_info_t *ci, int sig)
{
    /* need to atomically wait and set detached */
    sigset_t ss;
    block_sigchld(&ss);

    close_fds_child_ci(ci);

    if (kill(ci->pid, sig) == -1)
	warning(_("unable to terminate child process: %s"), strerror(errno));

    ci->detached = 1;
    /* check if the child has exited already, as sigchld may already have
       been received */
    wait_for_child_ci(ci);
#ifdef MC_DEBUG
    Dprintf("detached child %d (signal %d)\n", ci->pid, sig);
#endif
    restore_sigchld(&ss);
}

/* must only be called on attached child */
static void terminate_and_detach_child_ci(child_info_t *ci)
{
    kill_and_detach_child_ci(ci, SIGUSR1);
}

/* must only be called on a detached child */
static void kill_detached_child_ci(child_info_t *ci, int sig)
{
    /* need to atomically check waitedfor and kill */
    sigset_t ss;
    block_sigchld(&ss);

    if (!ci->waitedfor) {
        if (kill(ci->pid, sig) == -1) {
	    warning(_("unable to terminate child: %s"), strerror(errno));
#ifdef MC_DEBUG
	    Dprintf("failed to kill detached child %d (signal %d)\n", ci->pid, sig);
#endif
	}
#ifdef MC_DEBUG
	else
	    Dprintf("killed detached child %d (signal %d)\n", ci->pid, sig);
#endif
    }
    restore_sigchld(&ss);
}

/* detach and terminate a child */
static int rm_child(int pid)
{
    child_info_t *ci = children;
#ifdef MC_DEBUG
    Dprintf("removing child %d\n", pid);
#endif
    pid_t ppid = getpid();
    while (ci) {
	/* detached children are not visible to R code */
	if (!ci->detached && ci->pid == pid && ci->ppid == ppid) {
	    terminate_and_detach_child_ci(ci);
	    return 1;
	}
	ci = ci->next;
    }
#ifdef MC_DEBUG
    Dprintf("WARNING: child %d was to be removed but it doesn't exist\n", pid);
#endif
    return 0;
}

/* delete entries for waited-for children and children that are not of this process */
static void compact_children() {
    child_info_t *ci = children, *prev = NULL;
    pid_t ppid = getpid();

    /* prevent interference with signal handler accessing the list */
    sigset_t ss;
    block_sigchld(&ss);

    while(ci) {
	if ((ci->waitedfor && ci->pid >= 0) || ci->ppid != ppid) {
	    /* ci->pid >= to skip cleanup mark */
	    /* fds of waited-for children have been closed when detaching */
	    if (ci->ppid != ppid) {
		close_fds_child_ci(ci);
#ifdef MC_DEBUG
		Dprintf("removing child %d from the list as it is not ours\n", ci->pid);
#endif
	    }
#ifdef MC_DEBUG
	    else
		Dprintf("removing waited-for child %d from the list\n", ci->pid);
#endif
	    child_info_t *next = ci->next;
	    if (prev) prev->next = next;
	    else children = next;
	    free(ci);
	    ci = next;
	} else {
	    prev = ci;
	    ci = ci->next;
	}
    }

    restore_sigchld(&ss);
}

/* insert a cleanup mark into children */
SEXP mc_prepare_cleanup()
{
    child_info_t *ci;

    compact_children();
    ci = (child_info_t*) malloc(sizeof(child_info_t));
    if (!ci) error(_("memory allocation error"));
    ci->waitedfor = 1;
    ci->detached = 1;
    ci->pid = -1; /* a cleanup mark */
    ci->pfd = -1;
    ci->sifd = -1; /* set fds to -1 to simplify close */
    ci->ppid = getpid();
    ci->next = children;
    children = ci;

    return R_NilValue;
}

/* Terminate and detach all children up to the first cleanup mark. Compact
   children.

   sKill - like mc.cleanup from mclapply;
             TRUE  - send SIGTERM to all children,
             FALSE - do not kill children (but detach until sDetach = FALSE)
             num>0 - send signal of this number

   sDetach - TRUE  - detach children (with a signal specified via sKill, or
                     with SIGUSR1 when sKill = FALSE)

   sShutdown - shut down parallel (when unloading the package or exiting R)
             TRUE  - act on all children, not just up to the first mark,
                     wait until they all terminate, unregister signal handler
             FALSE - none of the above

   Typical use:
     sKill = TRUE, sDetach = TRUE, sShutdown = FALSE
       (mclapply, pvec)
     sKill = FALSE, sDetach = FALSE, sShutdown = FALSE
       (mccollect - only compact children)
     sKill = tools:SIGKILL, sDetach = TRUE, sShutdown = TRUE
       (clean_pids - finalizer on a private object in the package namespace)
*/
static void restore_sig_handler();

SEXP mc_cleanup(SEXP sKill, SEXP sDetach, SEXP sShutdown)
{
    int sig = -1; /* signal from sKill/mc.cleanup */
    if (isLogical(sKill)) {
	int lkill = asLogical(sKill);
	if (lkill == TRUE) sig = SIGTERM;
	else if (lkill == FALSE) sig = 0;
    } else {
	int ikill = asInteger(sKill);
	if (ikill >= 1 && ikill != NA_INTEGER)
	    sig = ikill;
    }
    if (sig == -1)
	error(_("invalid '%s' argument"), "mc.cleanup");

    int detach = asLogical(sDetach);
    if (detach == NA_LOGICAL)
	error(_("invalid '%s' argument"), "detach");

    int shutdown = asLogical(sShutdown);
    if (shutdown == NA_LOGICAL)
	error(_("invalid '%s' argument"), "shutdown");

    compact_children(); /* also removes children that are not ours */

    child_info_t *ci = children;
    int nattached = 0;
    while(ci) {
	if (ci->detached && ci->waitedfor && ci->pid == -1) {
	    /* cleanup mark */
	    /* set pid to a nonzero number so that the child will be removed
	       from the list by compact_children; as it is waitedfor, it will
	       not be sent any signal */
	    ci->pid = INT_MAX;
	    if (!shutdown) break;
	}
	if (ci->detached && sig)
	    /* only kills if not waited for */
	    kill_detached_child_ci(ci, sig);
	if (!ci->detached && detach) {
	    /* With sKill == FALSE (mclapply mc.cleanup=FALSE), send
	       SIGUSR1 to just detach the child. Detaching also closes the file
	       descriptors which contributes to termination probably even more,
	       as it is not likely that the child will be finished and just
	       waiting for SIGUSR1. */
	    kill_and_detach_child_ci(ci, sig ? sig : SIGUSR1);
	    nattached++;
	}
	ci = ci->next;
    }

    /* now all children in the range are detached */
    if (nattached > 0) {
#ifdef MC_DEBUG
	Dprintf("  %d children detached during cleanup\n", nattached);
#endif
	sleep(1); /* wait a tiny bit for at least the first child */
    }
    compact_children(); /* also removes the cleanup mark(s) */

    if (shutdown) {
	double before = currentTime();
	while(children) {
	    sleep(1); /* can be interrupted by SIGCHLD */
	    compact_children();
	    if (!children)
		break;
#ifdef MC_DEBUG
	    ci = children;
	    while(ci) {
		Dprintf(" still existing child: %d (waitedfor=%d detached=%d)\n",
		        ci->pid, ci->waitedfor, ci->detached);
		ci = ci->next;
	    }
#endif
	    double now = currentTime();
	    if (now - before > 10) { /* give up after 10 seconds */
		REprintf(_("Error while shutting down parallel: unable to terminate some child processes\n"));
		restore_sig_handler();
		return R_NilValue;
	    }
	}
	restore_sig_handler();
#ifdef MC_DEBUG
	Dprintf("process %d parallel shutdown ok\n", getpid());
#endif
    }
    return R_NilValue;
}

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

/* FIXME: child_exit_status is always -1 */
/* FIXME: do we still need SIGUSR1 to signal children they can exit? */

static int child_can_exit = 0, child_exit_status = -1;

static void child_sig_handler(int sig) 
{
    if (sig == SIGUSR1) {
#ifdef MC_DEBUG
	Dprintf("child process %d got SIGUSR1; child_exit_status=%d\n", 
		getpid(), child_exit_status);
#endif
	child_can_exit = 1;
	if (child_exit_status >= 0)
	    _exit(child_exit_status);
    }
}

static struct sigaction old_sig_handler;
static int parent_handler_set = 0;

/* The siginfo version of the sigchld handler gives the PID of the exiting
   process, but because of merging of signals one would have to poll through
   all detached children, anyway, so it won't really help. */
static void parent_sig_handler(int sig)
{
    child_info_t *ci = children;
    while(ci) {
	if (ci->detached && !ci->waitedfor)
	    wait_for_child_ci(ci);
	ci = ci->next;
    }

    /* TODO: chain to old sig handler */
}

static void setup_sig_handler()
{
    if (!parent_handler_set) {
	parent_handler_set = 1;
	struct sigaction sa;
	sigemptyset(&sa.sa_mask);
	sa.sa_handler = parent_sig_handler;
	sa.sa_flags = SA_RESTART;
	sigaction(SIGCHLD, &sa, &old_sig_handler);
    }
}

static void restore_sig_handler()
{
    if (parent_handler_set) {
	parent_handler_set = 0;
	sigaction(SIGCHLD, &old_sig_handler, NULL);
    }
}

SEXP mc_fork(SEXP sEstranged)
{
    int pipefd[2]; /* write end, read end */
    int sipfd[2];
    pid_t pid;
    int estranged = (asInteger(sEstranged) > 0);
    SEXP res = allocVector(INTSXP, 3);
    int *res_i = INTEGER(res);

    if (!estranged) {
	if (pipe(pipefd)) error(_("unable to create a pipe"));
	if (pipe(sipfd)) {
	    close(pipefd[0]); close(pipefd[1]);
	    error(_("unable to create a pipe"));
	}
#ifdef MC_DEBUG
	Dprintf("parent[%d] created pipes: comm (%d->%d), sir (%d->%d)\n",
		getpid(), pipefd[1], pipefd[0], sipfd[1], sipfd[0]);
#endif
    }

    /* make sure we get SIGCHLD to clean up the child process */
    setup_sig_handler();
    sigset_t ss;
    /* Block sigchld to make sure the child does not exist before registered
       in children list. Also make sure that the parent signal handler is not
       used in the child. */
    block_sigchld(&ss);
    fflush(stdout); // or children may output pending text
    pid = fork();
    if (pid == -1) {
	if (!estranged) {
	    close(pipefd[0]); close(pipefd[1]);
	    close(sipfd[0]); close(sipfd[1]);
	}
	error(_("unable to fork, possible reason: %s"), strerror(errno));
    }
    res_i[0] = (int) pid;
    if (pid == 0) { /* child */
	R_isForkedChild = 1;
	/* free children entries inherited from parent */
	while(children) {
	    close_fds_child_ci(children);
	    child_info_t *ci = children->next;
	    free(children);
	    children = ci;
	}
	restore_sigchld(&ss);
	restore_sig_handler(); /* enable the previous signal handler */

	if (estranged)
	    res_i[1] = res_i[2] = NA_INTEGER;
	else {
	    close(pipefd[0]); /* close read end */
	    master_fd = res_i[1] = pipefd[1];
	    res_i[2] = NA_INTEGER;
	    /* re-map stdin */
	    dup2(sipfd[0], STDIN_FILENO);
	    close(sipfd[0]);
	}
	is_master = 0;
	/* master uses USR1 to signal that the child process can terminate */
	child_exit_status = -1;
	if (estranged)
	    child_can_exit = 1;
	else {
	    child_can_exit = 0;
	    signal(SIGUSR1, child_sig_handler);
	}
#ifdef MC_DEBUG
	Dprintf("child process %d started\n", getpid());
#endif
    } else { /* master process */
	child_info_t *ci;

	ci = (child_info_t*) malloc(sizeof(child_info_t));
	if (!ci) error(_("memory allocation error"));
	ci->pid = pid;
	ci->ppid = getpid();
	ci->waitedfor = 0;

	if (estranged) {
	    ci->detached = 1;
	    res_i[1] = res_i[2] = NA_INTEGER;
	    ci->pfd = -1;
	    ci->sifd = -1;
	} else {
	    ci->detached = 0;
	    close(pipefd[1]); /* close write end of the data pipe */
	    close(sipfd[0]);  /* close read end of the child-stdin pipe */
	    res_i[1] = pipefd[0];
	    res_i[2] = sipfd[1];

	    /* register the new child and its pipes */
	    ci->pfd = pipefd[0];
	    ci->sifd= sipfd[1];
	}

    #ifdef MC_DEBUG
	    Dprintf("parent registers new child %d\n", pid);
    #endif
	ci->next = children;
	children = ci;
	restore_sigchld(&ss);
    }
    return res; /* (pid, fd of data pipe, fd of child-stdin pipe) */
}

SEXP mc_close_stdout(SEXP toNULL) 
{
    if (asLogical(toNULL) == 1) {
	int fd = open("/dev/null", O_WRONLY);
	if (fd != -1) {
	    dup2(fd, STDOUT_FILENO);
	    close(fd);
	} else close(STDOUT_FILENO);
    } else
	close(STDOUT_FILENO);
    return R_NilValue;
}

/* not used */
SEXP mc_close_stderr(SEXP toNULL) 
{
    if (asLogical(toNULL) == 1) {
	int fd = open("/dev/null", O_WRONLY);
	if (fd != -1) {
	    dup2(fd, STDERR_FILENO);
	    close(fd);
	} else close(STDERR_FILENO);
    } else
	close(STDERR_FILENO);
    return R_NilValue;
}

/* not used */
SEXP mc_close_fds(SEXP sFDS) 
{
    int *fd, fds, i = 0;
    if (TYPEOF(sFDS) != INTSXP) error("descriptors must be integers");
    fds = LENGTH(sFDS);
    fd = INTEGER(sFDS);
    while (i < fds) close(fd[i++]);
    return ScalarLogical(1);
}

/* Read from descriptor with restart on signal interrupt. While we register
   SIGCHLD with SA_RESTART, some other package might register a signal
   without it. */
static ssize_t readrep(int fildes, void *buf, size_t nbyte)
{
    size_t rbyte = 0;
    char *ptr = (char *)buf;
    for(;;) {
	ssize_t r = read(fildes, ptr + rbyte, nbyte - rbyte);
	if (r == -1) {
	    if (errno == EINTR)
		continue;
	    else
		return -1;
	}
	if (r == 0) /* EOF */
	    return rbyte;
	rbyte += r;
	if (rbyte == nbyte)
	    return rbyte;
    }
}

/* Write to descriptor with restart on signal interrupt. While we register
   SIGCHLD with SA_RESTART, some other package might register a signal
   without it. */
static ssize_t writerep(int fildes, const void *buf, size_t nbyte)
{
    size_t wbyte = 0;
    const char *ptr = (const char *)buf;
    for(;;) {
	ssize_t w = write(fildes, ptr + wbyte, nbyte - wbyte);
	if (w == -1) {
	    if (errno == EINTR)
		continue;
	    else
		return -1;
	}
	if (w == 0) {
	    /* possibly sending EOF on some systems via nbyte == 0,
	       or an old indication that non-blocking read of not
	       even a single more byte is possible */
	    return wbyte;
	}
	wbyte += w;
	if (wbyte == nbyte)
	    return wbyte;
    }
}

/* This format is read by read_child_ci (only).
   Prior to R 3.4.0 len was unsigned int and the format did not
   allow long vectors.
 */
SEXP mc_send_master(SEXP what)
{
    if (is_master)
	error(_("only children can send data to the master process"));
    if (master_fd == -1) 
	error(_("there is no pipe to the master process"));
    if (TYPEOF(what) != RAWSXP) 
	error(_("content to send must be RAW, use serialize() if needed"));
    R_xlen_t len = XLENGTH(what);
    unsigned char *b = RAW(what);
#ifdef MC_DEBUG
    Dprintf("child %d: send_master (%lld bytes)\n", getpid(), (long long)len);
#endif
    if (writerep(master_fd, &len, sizeof(len)) != sizeof(len)) {
	close(master_fd);
	master_fd = -1;
	error(_("write error, closing pipe to the master"));
    }
    ssize_t n;
    for (R_xlen_t i = 0; i < len; i += n) {
	n = writerep(master_fd, b + i, len - i);
	if (n < 1) {
	    close(master_fd);
	    master_fd = -1;
	    error(_("write error, closing pipe to the master"));
	}
    }
    return ScalarLogical(1);
}

SEXP mc_send_child_stdin(SEXP sPid, SEXP what) 
{
    int pid = asInteger(sPid);
    if (!is_master) 
	error(_("only the master process can send data to a child process"));
    if (TYPEOF(what) != RAWSXP) error("what must be a raw vector");
    child_info_t *ci = children;
    pid_t ppid = getpid();
    while (ci) {
	if (!ci->detached && ci->pid == pid && ci->ppid == ppid) break;
	ci = ci->next;
    }
    if (!ci || ci->sifd < 0) error(_("child %d does not exist"), pid);
    R_xlen_t  len = XLENGTH(what);
    unsigned char *b = RAW(what);
    unsigned int fd = ci -> sifd;
    for (R_xlen_t i = 0; i < len;) {
	ssize_t n = writerep(fd, b + i, len - i);
	if (n < 1) error(_("write error"));
	i += n;
    }
    return ScalarLogical(1);
}

/* some systems have FD_COPY */
void fdcopy(fd_set *dst, fd_set *src, int nfds)
{
    FD_ZERO(dst);
    for(int i = 0; i < nfds; i++)
	if (FD_ISSET(i, src)) FD_SET(i, dst);
}

SEXP mc_select_children(SEXP sTimeout, SEXP sWhich) 
{
    int maxfd = -1, sr;
    unsigned int wlen = 0, wcount = 0;
    SEXP res;
    int *res_i, *which = 0;
    child_info_t *ci = children;
    fd_set fs;
    double timeout = 0;
    pid_t ppid = getpid();

    if (isReal(sTimeout) && LENGTH(sTimeout) == 1)
	timeout = asReal(sTimeout);

    if (TYPEOF(sWhich) == INTSXP && LENGTH(sWhich)) {
	which = INTEGER(sWhich);
	wlen = LENGTH(sWhich);
    }

    FD_ZERO(&fs);
    while (ci) {
	if (!ci->detached && ci->ppid == ppid) {
	    /* attached children have ci->pfd > 0 */
	    if (which) { /* check for the FD only if it's on the list */
		unsigned int k = 0;
		while (k < wlen) {
		    if (which[k++] == ci->pid) {
			if (ci->pfd > FD_SETSIZE)
			    error("file descriptor is too large for select()");
			FD_SET(ci->pfd, &fs);
			if (ci->pfd > maxfd) maxfd = ci->pfd;
#ifdef MC_DEBUG
			Dprintf("select_children: added child %d (%d)\n", ci->pid, ci->pfd);
#endif
			wcount++;
			break; 
		    }
		}
	    } else {
		/* not sure if this should be allowed */
		if (ci->pfd > FD_SETSIZE)
		    error("file descriptor is too large for select()");
		FD_SET(ci->pfd, &fs);
		if (ci->pfd > maxfd) maxfd = ci->pfd;
#ifdef MC_DEBUG
		Dprintf("select_children: added child %d (%d)\n", ci->pid, ci->pfd);
#endif
	    }
	}
	ci = ci->next;
    }

    if (which && wcount < wlen) {
	/* children specified multiple times or not found */
	for(unsigned int k = 0; k < wlen; k++) {
	    ci = children;
	    int found = 0;
	    while(ci) {
		if (!ci->detached && ci->ppid == ppid &&
		    ci->pid == which[k] && FD_ISSET(ci->pfd, &fs)) {

		    found = 1;
		    break;
		}
		ci = ci->next;
	    }
	    if (!found) {
#ifdef MC_DEBUG
		Dprintf("select_children: cannot wait for child %d (idx %d) as it does not exist\n",
		        which[k], k);
#endif
		/* FIXME: probably should be an error */
		warning(_("cannot wait for child %d as it does not exist"), which[k]);
	    }
	}
    }

#ifdef MC_DEBUG
    Dprintf("select_children: maxfd=%d, wlen=%d, wcount=%d, timeout=%f\n", maxfd, wlen, wcount, timeout);
#endif

    if (maxfd == -1)
	return R_NilValue; /* NULL signifies no children to tend to */

    if (timeout == 0) {
	/* polling */
	struct timeval tv;
	tv.tv_sec = 0;
	tv.tv_usec = 0;
	sr = R_SelectEx(maxfd + 1, &fs, 0, 0, &tv, NULL);
    } else {
	double before = currentTime();
	double remains = timeout;
	struct timeval tv;
	fd_set savefs;
	fdcopy(&savefs, &fs, maxfd + 1);

	for(;;) {
	    R_ProcessEvents();
	    /* re-set tv as it may get updated by select */
	    if (R_wait_usec > 0) {
		tv.tv_sec = 0;
		tv.tv_usec = R_wait_usec;
		/* FIXME: ?Rg_wait_usec */
	    } else if (timeout > 0) {
		tv.tv_sec = (int) remains;
		tv.tv_usec = (int) ((remains - ((double) tv.tv_sec)) * 1e6);
	    } else {
		/* Note: I'm not sure we really should allow this .. */
		tv.tv_sec = 1; /* still allow to process events */
		tv.tv_usec = 0;
	    }
	    sr = R_SelectEx(maxfd + 1, &fs, 0, 0, &tv, NULL);
	    if (sr > 0 || (sr < 0 && errno != EINTR))
		break;
	    if (timeout > 0) {
		remains = timeout - (currentTime() - before);
		if (remains <= 0)
		    /* sr == 0 (timed out) or sr<0 && errno==EINTR */
		    break;
	    }
	    fdcopy(&fs, &savefs, maxfd + 1);
	}
    }
#ifdef MC_DEBUG
    Dprintf("  sr = %d\n", sr);
#endif
    if (sr < 0) {
	if (errno == EINTR) /* treat as timeout */
	    return ScalarLogical(TRUE);

	warning(_("error '%s' in select"), strerror(errno));
	return ScalarLogical(FALSE); /* FALSE on select error */
	}
    if (sr < 1) return ScalarLogical(TRUE); /* TRUE on timeout */
    ci = children;
#ifdef MC_DEBUG
    Dprintf(" - read select %d children: ", sr);
#endif
    res = allocVector(INTSXP, sr);
    res_i = INTEGER(res);
    while (ci) { /* fill the array */
	if (!ci->detached && ci->ppid == ppid && FD_ISSET(ci->pfd, &fs)) {
	    (res_i++)[0] = ci->pid;
#ifdef MC_DEBUG
	    Dprintf("%d ", ci->pid);
#endif
	}
	ci = ci->next;
    }
#ifdef MC_DEBUG
    Dprintf("\n");
#endif
    return res;
}

static SEXP read_child_ci(child_info_t *ci) 
{
    if (ci->detached)
	/* should not happen */
	return R_NilValue; /* not visible to R code */

    R_xlen_t len;
    int fd = ci->pfd;
    int pid = ci->pid;
    ssize_t n = readrep(fd, &len, sizeof(len));
#ifdef MC_DEBUG
    Dprintf("read_child_ci(%d) - read length returned %lld\n", pid, (long long)n);
#endif
    if (n != sizeof(len) || len == 0) {
	/* child is exiting (n==0), or error */
	terminate_and_detach_child_ci(ci);
	return ScalarInteger(pid);
    } else {
	SEXP rv = allocVector(RAWSXP, len);
	unsigned char *rvb = RAW(rv);
	R_xlen_t i = 0;
	while (i < len) {
	    n = readrep(fd, rvb + i, len - i);
#ifdef MC_DEBUG
	    Dprintf("read_child_ci(%d) - read %lld at %lld returned %lld\n",
	            ci->pid, (long long)len-i, (long long)i, (long long)n);
#endif
	    if (n < 1) { /* error */
		terminate_and_detach_child_ci(ci);
		return ScalarInteger(pid);
	    }
	    i += n;
	}
	PROTECT(rv);
	{
	    SEXP pa;
	    PROTECT(pa = ScalarInteger(ci->pid));
	    setAttrib(rv, install("pid"), pa);
	    UNPROTECT(1); /* pa */
	}
	UNPROTECT(1); /* rv */
	return rv;
    }
}

SEXP mc_read_child(SEXP sPid) 
{
    int pid = asInteger(sPid);
    child_info_t *ci = children;
    pid_t ppid = getpid();
    while (ci) {
	if (!ci->detached && ci->pid == pid && ci->ppid == ppid) break;
	ci = ci->next;
    }
#ifdef MC_DEBUG
    if (!ci) Dprintf("read_child(%d) - pid is not in the list of children\n", pid);
#endif
    if (!ci) return R_NilValue; /* if the child doesn't exist anymore, returns NULL */
    return read_child_ci(ci);	
}

/* not used */
SEXP mc_read_children(SEXP sTimeout) 
{
    int maxfd = 0, sr;
    child_info_t *ci = children;
    fd_set fs;
    struct timeval tv = { 0, 0 }, *tvp = &tv;
    if (isReal(sTimeout) && LENGTH(sTimeout) == 1) {
	double tov = asReal(sTimeout);
	if (tov < 0.0) tvp = 0; /* Note: I'm not sure we really should allow this .. */
	else {
	    tv.tv_sec = (int) tov;
	    tv.tv_usec = (int) ((tov - ((double) tv.tv_sec)) * 1000000.0);
	}
    }
    { 
	int wstat; 
	while (waitpid(-1, &wstat, WNOHANG) > 0) ; /* check for zombies */
    }
    FD_ZERO(&fs);
    pid_t ppid = getpid();
    while (ci) {
	if (!ci->detached && ci->ppid == ppid) {
	    if (ci->pfd > maxfd) maxfd = ci->pfd;
	    if (ci->pfd > 0) FD_SET(ci->pfd, &fs);
	}
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("read_children: maxfd=%d, timeout=%d:%d\n", maxfd, (int)tv.tv_sec, (int)tv.tv_usec);
#endif
    if (maxfd == 0) return R_NilValue; /* NULL signifies no children to tend to */
    sr = R_SelectEx(maxfd+1, &fs, 0, 0, tvp, NULL);
#ifdef MC_DEBUG
    Dprintf("sr = %d\n", sr);
#endif
    if (sr < 0) {
	warning(_("error '%s' in select"), strerror(errno));
	return ScalarLogical(0); /* FALSE on select error */
    }
    if (sr < 1) return ScalarLogical(1); /* TRUE on timeout */
    ci = children;
    while (ci) {
	if (!ci->detached && ci->ppid == ppid) {
	    if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) break;
	}
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("set ci=%p (%d, %d)\n", (void*) ci, ci?ci->pid:0, ci?ci->pfd:0);
#endif
    /* this should never occur really - select signalled a read handle
       but none of the handles is set - let's treat it as a timeout */
    if (!ci) return ScalarLogical(1);
    else return read_child_ci(ci);
}

/* not used */
SEXP mc_rm_child(SEXP sPid) 
{
    int pid = asInteger(sPid);
    return ScalarLogical(rm_child(pid));
}

SEXP mc_children() 
{
    child_info_t *ci = children;
    unsigned int count = 0;
    pid_t ppid = getpid();
    while (ci) {
	if (!ci->detached && ci->ppid == ppid) count++;
	ci = ci->next;
    }
    SEXP res = allocVector(INTSXP, count);
    /* the list of attached children cannot be changed by signal handlers */
    if (count) {
	int *pids = INTEGER(res);
	ci = children;
	while (ci) {
	    if (!ci->detached && ci->ppid == ppid) (pids++)[0] = ci->pid;
	    ci = ci->next;
	}
    }
    return res;
}

/* not used */
SEXP mc_fds(SEXP sFdi) 
{
    int fdi = asInteger(sFdi);
    unsigned int count = 0;
    SEXP res;
    child_info_t *ci = children;
    pid_t ppid = getpid();
    while (ci) {
	if (!ci->detached && ci->ppid == ppid) count++;
	ci = ci->next;
    }
    res = allocVector(INTSXP, count);
    if (count) {
	int *fds = INTEGER(res);
	ci = children;
	while (ci) {
	    if (!ci->detached && ci->ppid == ppid)
		(fds++)[0] = (fdi == 0) ? ci->pfd : ci->sifd;
	    ci = ci->next;
	}
    }
    return res;
}

/* not used */
SEXP mc_master_fd() 
{
    return ScalarInteger(master_fd);
}

SEXP mc_is_child() 
{
    return ScalarLogical(is_master ? FALSE : TRUE);
}

/* not used */
SEXP mc_kill(SEXP sPid, SEXP sSig) 
{
    int pid = asInteger(sPid);
    int sig = asInteger(sSig);
    if (kill((pid_t) pid, sig))
	error(_("'mckill' failed"));
    return ScalarLogical(1);
}

SEXP NORET mc_exit(SEXP sRes)
{
    int res = asInteger(sRes);
#ifdef MC_DEBUG
    Dprintf("child %d: 'mcexit' called\n", getpid());
#endif
    if (is_master) error(_("'mcexit' can only be used in a child process"));
    if (master_fd != -1) { /* send 0 to signify that we're leaving */
	size_t len = 0;
	/* assign result for Fedora security settings */
	ssize_t n = writerep(master_fd, &len, sizeof(len));
	/* make sure the pipe is closed before we enter any waiting */
	close(master_fd);
	master_fd = -1;
	if (n < 0) error(_("write error, closing pipe to the master"));
    }
    if (!child_can_exit) {
#ifdef MC_DEBUG
	Dprintf("child %d is waiting for permission to exit\n", getpid());
#endif
	while (!child_can_exit) sleep(1);
	    /* SIGUSR1 will terminate the sleep */
    }
		
#ifdef MC_DEBUG
    Dprintf("child %d: exiting\n", getpid());
#endif
    _exit(res);
    error(_("'mcexit' failed"));
}

/* NA = query, TRUE/FALSE = set R_Interactive accordingly */
SEXP mc_interactive(SEXP sWhat) {
    int what = asInteger(sWhat);
    if (what != NA_INTEGER)
	R_Interactive = what;
    return ScalarLogical(R_Interactive);
}

/*--  mcaffinity --
  FIXME: we may want to move this outside fork.c in case Windows can do that */
#ifdef HAVE_SCHED_SETAFFINITY

#ifdef HAVE_SCHED_H
#include <sched.h>
#endif

#if defined(CPU_ZERO) && defined(CPU_COUNT) && defined(CPU_SETSIZE) && defined(CPU_SET) && defined(CPU_SET_S) && defined(CPU_ISSET)
#define WORKING_MC_AFFINITY
#endif
#endif

#ifdef WORKING_MC_AFFINITY

/* req is one-based, cpu_set is zero-based */
SEXP mc_affinity(SEXP req) {
    if (req != R_NilValue && TYPEOF(req) != INTSXP && TYPEOF(req) != REALSXP)
	error(_("invalid CPU affinity specification"));
    if (TYPEOF(req) == REALSXP)
	req = coerceVector(req, INTSXP);
    if (TYPEOF(req) == INTSXP) {
	int max_cpu = 0, i, n = LENGTH(req), *v = INTEGER(req);
	for (i = 0; i < n; i++) {
	    if (v[i] > max_cpu)
		max_cpu = v[i];
	    if (v[i] < 1)
		error(_("invalid CPU affinity specification"));
	}
	/* These are both one-based */
	if (max_cpu <= CPU_SETSIZE) { /* can use static set */
	    cpu_set_t cs;
	    CPU_ZERO(&cs);
	    for (i = 0; i < n; i++)
		CPU_SET(v[i] - 1, &cs);
	    sched_setaffinity(0, sizeof(cpu_set_t), &cs);
	} else {
#ifndef CPU_ALLOC
	    error(_("requested CPU set is too large for this system"));
#else
	    size_t css = CPU_ALLOC_SIZE(max_cpu);
	    cpu_set_t *cs = CPU_ALLOC(max_cpu);
	    CPU_ZERO_S(css, cs);
	    for (i = 0; i < n; i++)
		CPU_SET_S(v[i] - 1, css, cs);
	    sched_setaffinity(0, css, cs);
#endif
	}
    }

    {
	/* FIXME: in theory we may want to use *_S versions as well,
	 but that would require some knowledge about the number of
	 available CPUs and comparing that to CPU_SETSIZE, so for now
	 we just use static cpu_set -- the mask will be still set
	 correctly, just the returned set will be truncated at
	 CPU_SETSIZE */
	cpu_set_t cs;
	CPU_ZERO(&cs);
	if (sched_getaffinity(0, sizeof(cs), &cs)) {
	    if (req == R_NilValue)
		error(_("retrieving CPU affinity set failed"));
	    return R_NilValue;
	} else {
	    SEXP res = allocVector(INTSXP, CPU_COUNT(&cs));
	    int i, *v = INTEGER(res);
	    for (i = 0; i < CPU_SETSIZE; i++)
		if (CPU_ISSET(i, &cs))
		    *(v++) = i + 1;
	    return res;
	}
    }
}
#else /* ! WORKING_MC_AFFINITY */

SEXP mc_affinity(SEXP req) {
    return R_NilValue;
}

#endif /* WORKING_MC_AFFINITY */

