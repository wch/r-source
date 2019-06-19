/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2017  The R Core Team
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
 */

/* <UTF8>
   char here is mainly handled as a whole string.
   Does handle file names.
   Chopping final \n is OK in UTF-8.
 */


/* See system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <Rmath.h> /* for fround */
#include "Runix.h"

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifndef HAVE_GETRUSAGE
# ifdef HAVE_SYS_TIME_H
#  include <sys/times.h>
# endif
#endif

#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_GETRUSAGE)
/* on macOS it seems sys/resource.h needs sys/time.h first */
# include <sys/time.h>
# include <sys/resource.h>
#endif

#include <errno.h>

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

attribute_hidden
FILE *R_OpenInitFile(void)
{
    char buf[PATH_MAX], *home, *p = getenv("R_PROFILE_USER");
    FILE *fp;

    fp = NULL;
    if (LoadInitFile) {
	if(p) {
	    if(!*p) return NULL;  /* set to "" */
	    return R_fopen(R_ExpandFileName(p), "r");
	}
	if((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	if((home = getenv("HOME")) == NULL)
	    return NULL;
	snprintf(buf, PATH_MAX, "%s/.Rprofile", home);
	if((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;
}
    /*
     *   R_CleanUp is interface-specific
     */

/*
 *  5) FILESYSTEM INTERACTION
 */


    /*
     *   R_ShowFiles is interface-specific
     */

    /*
     *   R_ChooseFile is interface-specific
     */

#if defined(HAVE_LIBREADLINE) && defined(HAVE_TILDE_EXPAND_WORD)
char *R_ExpandFileName_readline(const char *s, char *buff);  /* sys-std.c */
#endif

static char newFileName[PATH_MAX];
static int HaveHOME=-1;
static char UserHOME[PATH_MAX];

/* Only interpret inputs of the form ~ and ~/... */
static const char *R_ExpandFileName_unix(const char *s, char *buff)
{
    char *p;

    if(s[0] != '~') return s;
    if(strlen(s) > 1 && s[1] != '/') return s;
    if(HaveHOME < 0) {
	p = getenv("HOME");
	if(p && *p && (strlen(p) < PATH_MAX)) {
	    strcpy(UserHOME, p);
	    HaveHOME = 1;
	} else
	    HaveHOME = 0;
    }
    if(HaveHOME > 0 && (strlen(UserHOME) + strlen(s+1) < PATH_MAX)) {
	strcpy(buff, UserHOME);
	strcat(buff, s+1);
	return buff;
    } else return s;
}

/* tilde_expand (in libreadline) mallocs storage for its return value.
   The R entry point does not require that storage to be freed, so we
   copy the value to a static buffer, to void a memory leak in R<=1.6.0.

   This is not thread-safe, but as R_ExpandFileName is a public entry
   point (in R-exts.texi) it will need to deprecated and replaced by a
   version which takes a buffer as an argument.

   BDR 10/2002
*/

extern Rboolean UsingReadline;

const char *R_ExpandFileName(const char *s)
{
#if defined(HAVE_LIBREADLINE) && defined(HAVE_TILDE_EXPAND_WORD)
    if(UsingReadline) {
	const char * c = R_ExpandFileName_readline(s, newFileName);
	/* we can return the result only if tilde_expand is not broken */
	if (!c || c[0]!='~' || (c[1]!='\0' && c[1]!='/'))
	    return c;
    }
#endif
    return R_ExpandFileName_unix(s, newFileName);
}


/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

SEXP attribute_hidden do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return mkString("Unix");
}

# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h> /* times */
# endif

static double clk_tck, StartTime;

void R_setStartTime(void)
{
#ifdef HAVE_SYSCONF
    clk_tck = (double) sysconf(_SC_CLK_TCK);
#else
# ifndef CLK_TCK
/* this is in ticks/second, generally 60 on BSD style Unix, 100? on SysV
 */
#  ifdef HZ
#   define CLK_TCK HZ
#  else
#   define CLK_TCK 60
#  endif
# endif /* not CLK_TCK */
    clk_tck = (double) CLK_TCK;
#endif
    /* printf("CLK_TCK = %d\n", CLK_TCK); */
    StartTime = currentTime();
}

/* NOTE
   This used to use times() for elapsed times, which is measured in
   clock ticks (which can overflow).  It is possible this version uses
   time() and so is in seconds.  But even Cygwin has gettimeofday.
 */
attribute_hidden
void R_getProcTime(double *data)
{
    /* docs say this is rounded to the nearest ms */
    double et = currentTime() - StartTime;
    data[2] = 1e-3 * rint(1000*et);
#ifdef HAVE_GETRUSAGE
    /* all known current OSes */
    struct rusage self, children;
    getrusage(RUSAGE_SELF, &self);
    getrusage(RUSAGE_CHILDREN, &children);
    data[0] = (double) self.ru_utime.tv_sec +
	1e-3 * (self.ru_utime.tv_usec/1000);
    data[1] = (double) self.ru_stime.tv_sec +
	1e-3 * (self.ru_stime.tv_usec/1000);
    data[3] = (double) children.ru_utime.tv_sec +
	1e-3 * (children.ru_utime.tv_usec/1000);
    data[4] = (double) children.ru_stime.tv_sec +
	1e-3 * (children.ru_stime.tv_usec/1000);
#else
    /* Not known to be currently used */
    struct tms timeinfo;
    times(&timeinfo);
    data[0] = fround(timeinfo.tms_utime / clk_tck, 3);
    data[1] = fround(timeinfo.tms_stime / clk_tck, 3);
    data[3] = fround(timeinfo.tms_cutime / clk_tck, 3);
    data[4] = fround(timeinfo.tms_cstime / clk_tck, 3);
#endif
}

/* used in memory.c */
/* FIXME: maybe should try to find the increment for getrusage */
attribute_hidden
double R_getClockIncrement(void)
{
    return 1.0 / clk_tck;
}


#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif

/* The timeout support is inspired by timeout utility from coreutils.
   However, here the child process creates a new process group rather than
   the parent (R) process, because changing the group leader for the whole
   of R might have undesirable consequences.  According to comments in
   coreutils, this could lead to issues with propagating signals between
   foreground and background process groups.  Like with coreutils, the
   timeout is not always enforced: an external application can run longer
   than the timeout when it creates a new process group or when it spawns a
   child process and exits without waiting for it to finish (becomes a
   daemon).  This implementation only works for processes that do not read
   from the standard input - the new process group is always created, and
   hence the executed process can no longer access the terminal.  To prevent
   interference with job control, the new process is thus started with
   standard input redirected from /dev/null.  Note that while the timeout
   utility allows to run processes also without creating the new group
   (option `foreground`), that approach would interfere with job control in
   "/bin/sh" that is documented to be used by the R system call.  There does
   not seem to be a simple way to address this issue, and hence interactive
   applications cannot be executed with timeout (and note the same issues
   arise when timeout utility is used with /bin/sh).

   Currently we only have a single global structure and hence only one call
   to R_popen_timeout/R_system_timeout may be active at the same time. A more
   general implementation could use a linked list and identify entries by
   file pointer and child pid.

   Background jobs (ending with &) are not supported. */

#define KILL_SIGNAL1 SIGINT
#define KILL_SIGNAL2 SIGTERM
#define KILL_SIGNAL3 SIGKILL
#define EMERGENCY_TIMEOUT 20

/* The child processes are sent KILL_SIGNAL1 after the specified timeout.
   As a backup, KILL_SIGNAL2 would be sent after additional EMERGENCY_TIMEOUT
   seconds. As a backup of the backup, KILL_SIGNAL3 would be sent after yet
   additional EMERGENCY_TIMEOUT seconds.

   SIGINT is used first because it seems to be handled better by applications:
   applications happen to wait for child processes to terminate, and hence
   their execution is included into getrusage/RUSAGE_CHILDREN (proc.time).
   As follows from empirical observations, SIGTERM can sometimes terminate
   applications that cannot be terminated by SIGINT. */

int kill_signals[] = { KILL_SIGNAL1, KILL_SIGNAL2, KILL_SIGNAL3 };
static struct {
    pid_t child_pid;
    int timedout; /* set when the child has been timed out */
    int kill_attempts; /* 1 after sending KILL_SIGNAL1, etc */
    sigset_t oldset;
    struct sigaction oldalrm, oldint, oldquit, oldhup, oldterm, oldttin,
                     oldttou, oldchld;
    RCNTXT cntxt; /* for popen/pclose */
    FILE *fp;     /* for popen/pclose, sanity check */
} tost;

static void timeout_handler(int sig);
static void timeout_init()
{
    tost.child_pid = 0;
    tost.timedout = 0;
    tost.kill_attempts = 0;
    sigprocmask(0, NULL, &tost.oldset);
    sigaction(SIGALRM, NULL, &tost.oldalrm);
    sigaction(SIGINT, NULL, &tost.oldint);
    sigaction(SIGQUIT, NULL, &tost.oldquit);
    sigaction(SIGHUP, NULL, &tost.oldhup);
    sigaction(SIGTERM, NULL, &tost.oldterm);
    sigaction(SIGTTIN, NULL, &tost.oldttin);
    sigaction(SIGTTOU, NULL, &tost.oldttou);
    sigaction(SIGCHLD, NULL, &tost.oldchld);
    tost.fp = NULL;

    /* install handler */
    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_handler = &timeout_handler;
    sa.sa_flags = SA_RESTART;
    sigaction(SIGALRM, &sa, NULL);
    sigaction(SIGINT, &sa, NULL);
    sigaction(SIGQUIT, &sa, NULL);
    sigaction(SIGHUP, &sa, NULL);
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGCHLD, &sa, NULL);
}

static void timeout_cleanup_set(sigset_t *ss)
{
    sigemptyset(ss);
    sigaddset(ss, SIGALRM);
    sigaddset(ss, SIGINT);
    sigaddset(ss, SIGQUIT);
    sigaddset(ss, SIGHUP);
    sigaddset(ss, SIGTERM);
    sigaddset(ss, SIGTTIN);
    sigaddset(ss, SIGTTOU);
    sigaddset(ss, SIGCHLD);
}

static void timeout_cleanup()
{
    sigset_t ss;
    timeout_cleanup_set(&ss);
    sigprocmask(SIG_BLOCK, &ss, NULL);
    alarm(0); /* clear alarm */

    sigaction(SIGALRM, &tost.oldalrm, NULL);
    sigaction(SIGINT, &tost.oldint, NULL);
    sigaction(SIGQUIT, &tost.oldquit, NULL);
    sigaction(SIGHUP, &tost.oldhup, NULL);
    sigaction(SIGTERM, &tost.oldterm, NULL);
    sigaction(SIGTTIN, &tost.oldttin, NULL);
    sigaction(SIGTTOU, &tost.oldttou, NULL);
    sigaction(SIGCHLD, &tost.oldchld, NULL);

    sigprocmask(SIG_SETMASK, &tost.oldset, NULL);
}

static void timeout_handler(int sig)
{
    if (sig == SIGCHLD)
	return; /* needed for sigsuspend() to be interrupted */
    if (tost.child_pid > 0 && sig == SIGALRM) {
	tost.timedout = 1;
	if (tost.kill_attempts < 3) {
	    sig = kill_signals[tost.kill_attempts];
	    if (tost.kill_attempts < 2) {
		int saveerrno = errno;
		alarm(EMERGENCY_TIMEOUT);
		errno = saveerrno;
	    }
	    tost.kill_attempts++;
	} else
	    sig = KILL_SIGNAL1; /* should not happen */
    }
    if (tost.child_pid > 0) {
	/* parent, received a signal */

	kill(tost.child_pid, sig);
	/* NOTE: don't signal the group and  don't send SIGCONT
	         for interactive jobs */
	int saveerrno = errno;
	/* on macOS, killpg fails with EPERM for groups with zombies */
	killpg(tost.child_pid, sig);
	errno = saveerrno;
	if (sig != SIGKILL && sig != SIGCONT) {
	    kill(tost.child_pid, SIGCONT);
	    saveerrno = errno;
	    /* on macOS, killpg fails with EPERM for groups with zombies */
	    killpg(tost.child_pid, SIGCONT);
	    errno = saveerrno;
	}
    } else if (tost.child_pid == 0) {
	/* child */
	_exit(128 + sig); /* arbitrary status, such as in timeout utility */
    }
    /* tost.child_pid is -1 when child process no longer exists */
}

static pid_t timeout_wait(int *wstatus)
{
    pid_t wres;

    /* make sure we do not accidentally send signals to a new process
       with re-used pid from the child */
    sigset_t ss;
    timeout_cleanup_set(&ss);
    sigset_t unblocked_ss;
    sigprocmask(SIG_BLOCK, &ss, &unblocked_ss);

    int saveerrno = errno;
    while((wres = waitpid(tost.child_pid, wstatus, WNOHANG)) == 0)
	sigsuspend(&unblocked_ss);

    if (errno == EINTR)
	/* EINTR is not really an error but expected situation here, however,
	   R's "system" call would report any non-zero errno as an error. */
	errno = saveerrno;
    if (wres == tost.child_pid)
	tost.child_pid = -1; /* the process no longer exists */
    timeout_cleanup();
    return wres;
}

static void timeout_cend(void *data)
{
    if (tost.child_pid > 0) {
	timeout_handler(SIGALRM);
	timeout_wait(NULL);
    }
    timeout_cleanup();
}

/* Fork with blocked SIGCHLD to make sure that tost.child_pid is set
   in the parent before the signal is received. Also makes sure
   SIGCHLD is unblocked in the parent after the call. */
static void timeout_fork()
{
    sigset_t css; 
    sigemptyset(&css);
    sigaddset(&css, SIGCHLD);
    sigprocmask(SIG_BLOCK, &css, NULL);
    tost.child_pid = fork();
    sigprocmask(SIG_UNBLOCK, &css, NULL);
}

/* R_popen_timeout, R_pclose_timeout - a partial implementation of popen/close
   with support for timeout. The POSIX/Unix popen/pclose cannot be re-used,
   because the PID of the child process is not accessible via POSIX API.

   This simple implementation only supports a single pipe to be open at a time
   and R_system_timeout cannot be used at the same time.

   It does not support close-on-exec ("e" flag).
   A pipe opened with R_popen_timeout cannot be closed by pclose.
   A pipe opened with popen cannot be closed by R_pclose_timeout.
   Timeout is in seconds. After timing out, the child process is interrupted.
*/
static FILE *R_popen_timeout(const char *cmd, const char *type, int timeout)
{
    /* close-on-exec is not supported */
    if (!type || type[1] ||  (type[0] != 'r' && type[0] != 'w')) {
	errno = EINVAL;
	return NULL;
    }
    int doread = (type[0] == 'r');
    int pipefd[2];
    int parent_end, child_end;
    if (pipe(pipefd) < 0)
	return NULL;
    if (doread) {
	parent_end = pipefd[0];
	child_end = pipefd[1];
    } else {
	parent_end = pipefd[1];
	child_end = pipefd[0];
    }

    /* Earlier version of R would block SIGPROF here on old Apple systems
       following Luke's recommendation on how to fix PR#1140 (see R_open,
       R_system). */

    timeout_init();

    /* set up a context to recover from R error between popen and pclose */
    begincontext(&tost.cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    tost.cntxt.cenddata = NULL;
    tost.cntxt.cend = &timeout_cend;

    signal(SIGTTIN, SIG_IGN);
    signal(SIGTTOU, SIG_IGN);
    timeout_fork();

    if (tost.child_pid == 0) {
	/* child */
	setpgid(0, 0); /* NOTE: don't create new group in interactive jobs */
	signal(SIGTTIN, SIG_DFL);
	signal(SIGTTOU, SIG_DFL);
	dup2(child_end, doread ? 1 : 0);
	close(child_end);
	close(parent_end);
	close(doread ? 0 : 1);
	/* ensure there is no read from terminal to avoid SIGTTIN */
	if (open("/dev/null", O_RDONLY) < 0) {
	    perror("Cannot open /dev/null for reading:");
	    _exit(127);
	}
	execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
	_exit(127); /* execl failed */
    } else if (tost.child_pid > 0) {
	/* parent */
	close(child_end);
        tost.fp = fdopen(parent_end, type);
	if (!tost.fp) {
	    close(parent_end);
	    return NULL;
	}

	sigset_t ss;
	sigemptyset(&ss);
	sigaddset(&ss, SIGALRM);
	sigprocmask(SIG_UNBLOCK, &ss, NULL);
	alarm(timeout); /* will get SIGALRM on timeout */

	return tost.fp;
    } else {
	close(parent_end);
	return NULL;
    }
}

int R_pclose_timeout(FILE *fp)
{
    if (fp != tost.fp)
	/* should not happen */
	error("Invalid file pointer in pclose");

    /* Do not use fclose, because on Solaris it sets errno to "Invalid seek"
       when the pipe is already closed (e.g. because of timeout). fclose would
       not return an error, but it would set errno and the non-zero errno would
       then be reported by R's "system" function. */
    int fd = fileno(fp);
    if (fd >= 0)
	close(fd);

    pid_t wres;
    int wstatus;

    wres = timeout_wait(&wstatus);
    endcontext(&tost.cntxt);

    if (wres < 0)
	return -1;
    return wstatus;
}

/* Similar to system, but supports timeout in seconds.
   Calls to R_system_timeout cannot be used when a pipe is open using
   R_popen_timeout.
*/
static int R_system_timeout(const char *cmd, int timeout)
{
    if (!cmd)
	return R_system(cmd);

    /* Earlier version of R would block SIGPROF here on old Apple systems
       following Luke's recommendation on how to fix PR#1140 (see R_open,
       R_system). */

    timeout_init();
    signal(SIGTTIN, SIG_IGN);
    signal(SIGTTOU, SIG_IGN);
    timeout_fork();

    if (tost.child_pid == 0) {
	/* child */
	close(0);
	/* ensure there is no read from terminal to avoid SIGTTIN */
	if (open("/dev/null", O_RDONLY) < 0) {
	    perror("Cannot open /dev/null for reading:");
	    _exit(127);
	}
	setpgid(0, 0);
	signal(SIGTTIN, SIG_DFL);
	signal(SIGTTOU, SIG_DFL);

	execl("/bin/sh", "sh", "-c", cmd, (char *)NULL);
	_exit(127); /* execl failed */
    } else if (tost.child_pid > 0) {
	/* parent */
	sigset_t ss;
	sigemptyset(&ss);
	sigaddset(&ss, SIGALRM);
	sigprocmask(SIG_UNBLOCK, &ss, NULL);
	alarm(timeout); /* will get SIGALRM on timeout */

	int wstatus;
	timeout_wait(&wstatus);
	if (tost.child_pid != -1)
	    return -1;
#ifdef HAVE_SYS_WAIT_H
	if (WIFEXITED(wstatus)) wstatus = WEXITSTATUS(wstatus);
#else
	/* assume that this is shifted if a multiple of 256 */
	if ((wstatus % 256) == 0) wstatus = wstatus/256;
#endif
        if (wstatus == -1) {
	    /* this means that system() failed badly - it didn't
	       even get to try to run the shell */
	    warning(_("system call failed: %s"), strerror(errno));
	    /* R system() is documented to return 127 on failure, and a lot of
	       code relies on that - it will misinterpret -1 as success */
	    wstatus = 127;
	}
	return wstatus;
    } else
	return -1;
}

static void warn_status(const char *cmd, int res)
{
    if (!res)
	return;

    if (errno)
	/* FIXME: TK: non-zero errno is a sign of an error only when
	   a function that modified it also signals an error by its
	   return value, usually -1 or EOF. We should not be reporting
	   an error here (CERT ERR30-C).*/
	/* on Solaris, if the command ends with non-zero status and timeout
	   is 0, "Illegal seek" error is reported; the timeout version
	   works this around by using close(fileno) */
	warning(_("running command '%s' had status %d and error message '%s'"),
		cmd, res, strerror(errno));
    else
	warning(_("running command '%s' had status %d"), cmd, res);
}

#define INTERN_BUFSIZE 8096
SEXP attribute_hidden do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP tlist = R_NilValue;
    int intern = 0;
    int timeout = 0;

    checkArity(op, args);
    if (!isValidStringF(CAR(args)))
	error(_("non-empty character argument expected"));
    intern = asLogical(CADR(args));
    if (intern == NA_INTEGER)
	error(_("'intern' must be logical and not NA"));
    timeout = asInteger(CADDR(args));
    if (timeout == NA_INTEGER || timeout < 0)
	error(_("invalid '%s' argument"), "timeout");
    const char *cmd = translateChar(STRING_ELT(CAR(args), 0));
    if (timeout > 0) {
	/* command ending with & is not supported by timeout */
	const void *vmax = vmaxget();
	const char *c = translateCharUTF8(STRING_ELT(CAR(args), 0));
	int last_is_amp = 0;
	int len = 0;
	for(;*c; c += len) {
	    len = utf8clen(*c);
	    if (len == 1) {
		if (*c == '&')
		    last_is_amp = 1;
		else if (*c != ' ' && *c != '\t' && *c != '\r' && *c != '\n')
		    last_is_amp = 0;
	    } else
		last_is_amp = 0;
	}
	if (last_is_amp)
	    error("Timeout with background running processes is not supported.");
	vmaxset(vmax);
    }
    if (intern) { /* intern = TRUE */
	FILE *fp;
	char *x = "r",
#ifdef HAVE_GETLINE
	    *buf = NULL;
	size_t buf_len = 0;
#else
	    buf[INTERN_BUFSIZE];
#endif
	int i, j, res;
	SEXP tchar, rval;

	PROTECT(tlist);
	errno = 0; /* precaution */
	if (timeout == 0)
	    fp = R_popen(cmd, x);
	else
	    fp = R_popen_timeout(cmd, x, timeout);
	if(!fp)
	    error(_("cannot popen '%s', probable reason '%s'"),
		  cmd, strerror(errno));
#ifdef HAVE_GETLINE
        size_t read;
        for(i = 0; (read = getline(&buf, &buf_len, fp)) != (size_t)-1; i++) {
	    if (buf[read - 1] == '\n')
#else
	for (i = 0; fgets(buf, INTERN_BUFSIZE, fp); i++) {
	    size_t read = strlen(buf);
	    if(read >= INTERN_BUFSIZE - 1)
		warning(_("line %d may be truncated in call to system(, intern = TRUE)"), i + 1);
	    if (read > 0 && buf[read-1] == '\n')
#endif
		buf[read - 1] = '\0'; /* chop final CR */
	    tchar = mkChar(buf);
	    UNPROTECT(1);
	    PROTECT(tlist = CONS(tchar, tlist));
	}
#ifdef HAVE_GETLINE
        if (buf != NULL)
          free(buf);
#endif
	if (timeout == 0)
	    res = pclose(fp);
	else 
	    res = R_pclose_timeout(fp);

	/* On Solaris, pclose sometimes returns -1 and sets errno to ESPIPE
	   (Illegal seek). In that case, do_system reports 0 exit status and
	   displays a warning via warn_status. ESPIPE is not mentioned by
	   POSIX as possible outcome of pclose. */

#ifdef HAVE_SYS_WAIT_H
	if (WIFEXITED(res)) res = WEXITSTATUS(res);
	else res = 0;
#else
	/* assume that this is shifted if a multiple of 256 */
	if ((res % 256) == 0) res = res/256;
#endif
	if ((res & 0xff)  == 127) {/* 127, aka -1 */
	    if (errno)
		error(_("error in running command: '%s'"), strerror(errno));
	    else
		error(_("error in running command"));
	}

	if (timeout && tost.timedout) {
	    res = 124;
	    warning(_("command '%s' timed out after %ds"), cmd, timeout);
	} else
	    warn_status(cmd, res);

	rval = PROTECT(allocVector(STRSXP, i));
	for (j = (i - 1); j >= 0; j--) {
	    SET_STRING_ELT(rval, j, CAR(tlist));
	    tlist = CDR(tlist);
	}
	if(res) {
	    SEXP lsym = install("status");
	    setAttrib(rval, lsym, ScalarInteger(res));
	    if(errno) {
		lsym = install("errmsg");
		setAttrib(rval, lsym, mkString(strerror(errno)));
	    }
	}
	UNPROTECT(2);
	return rval;
    }
    else { /* intern =  FALSE */
#ifdef HAVE_AQUA
	R_Busy(1);
#endif
	tlist = PROTECT(allocVector(INTSXP, 1));
	fflush(stdout);
	int res;
	if (timeout == 0)
	    res = R_system(cmd);
	else 
	    res = R_system_timeout(cmd, timeout);
	if (res == 127) 
	    warning(_("error in running command"));
	if (timeout && tost.timedout) {
	    res = 124;
	    warning(_("command '%s' timed out after %ds"), cmd, timeout);
	} 
	INTEGER(tlist)[0] = res;
#ifdef HAVE_AQUA
	R_Busy(0);
#endif
	UNPROTECT(1);
	R_Visible = 0;
	return tlist;
    }
}

#ifdef HAVE_SYS_UTSNAME_H
# include <sys/utsname.h>

# ifdef HAVE_UNISTD_H
#  include <unistd.h>
# endif

# ifdef HAVE_PWD_H
#  include <pwd.h>
# endif

SEXP attribute_hidden do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ansnames;
    struct utsname name;
    char *login;

    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 8));
    if(uname(&name) == -1) {
	UNPROTECT(1);
	return R_NilValue;
    }
    SET_STRING_ELT(ans, 0, mkChar(name.sysname));
    SET_STRING_ELT(ans, 1, mkChar(name.release));
    SET_STRING_ELT(ans, 2, mkChar(name.version));
    SET_STRING_ELT(ans, 3, mkChar(name.nodename));
    SET_STRING_ELT(ans, 4, mkChar(name.machine));
    login = getlogin();
    SET_STRING_ELT(ans, 5, login ? mkChar(login) : mkChar("unknown"));
#if defined(HAVE_PWD_H) && defined(HAVE_GETPWUID) && defined(HAVE_GETUID)
    {
	struct passwd *stpwd;
	stpwd = getpwuid(getuid());
	SET_STRING_ELT(ans, 6, stpwd ? mkChar(stpwd->pw_name) : mkChar("unknown"));
    }
#else
    SET_STRING_ELT(ans, 6, mkChar("unknown"));
#endif
#if defined(HAVE_PWD_H) && defined(HAVE_GETPWUID) && defined(HAVE_GETEUID)
    {
	struct passwd *stpwd;
	stpwd = getpwuid(geteuid());
	SET_STRING_ELT(ans, 7, stpwd ? mkChar(stpwd->pw_name) : mkChar("unknown"));
    }
#else
    SET_STRING_ELT(ans, 7, mkChar("unknown"));
#endif
    PROTECT(ansnames = allocVector(STRSXP, 8));
    SET_STRING_ELT(ansnames, 0, mkChar("sysname"));
    SET_STRING_ELT(ansnames, 1, mkChar("release"));
    SET_STRING_ELT(ansnames, 2, mkChar("version"));
    SET_STRING_ELT(ansnames, 3, mkChar("nodename"));
    SET_STRING_ELT(ansnames, 4, mkChar("machine"));
    SET_STRING_ELT(ansnames, 5, mkChar("login"));
    SET_STRING_ELT(ansnames, 6, mkChar("user"));
    SET_STRING_ELT(ansnames, 7, mkChar("effective_user"));
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}
#else /* not HAVE_SYS_UTSNAME_H */
SEXP do_sysinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    warning(_("Sys.info() is not implemented on this system"));
    return R_NilValue;		/* -Wall */
}
#endif /* not HAVE_SYS_UTSNAME_H */

/* The pointer here is used in the Mac GUI */
#include <R_ext/eventloop.h> /* for R_PolledEvents */
#include <R_ext/Rdynload.h>
DL_FUNC ptr_R_ProcessEvents;
void R_ProcessEvents(void)
{
#ifdef HAVE_AQUA
    /* disable ProcessEvents in child,
       since we can't call CoreFoundation there. */
    if (ptr_R_ProcessEvents && !R_isForkedChild) ptr_R_ProcessEvents();
#else
    /* We might in due course want to always inhibit in a child */
    if (ptr_R_ProcessEvents) ptr_R_ProcessEvents();
#endif
    R_PolledEvents();
    if (cpuLimit > 0.0 || elapsedLimit > 0.0) {
	double cpu, data[5];
	R_getProcTime(data);
	cpu = data[0] + data[1] + data[3] + data[4];
	if (elapsedLimit > 0.0 && data[2] > elapsedLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (elapsedLimit2 > 0.0 && data[2] > elapsedLimit2) {
		elapsedLimit2 = -1.0;
		error(_("reached session elapsed time limit"));
	    } else
		error(_("reached elapsed time limit"));
	}
	if (cpuLimit > 0.0 && cpu > cpuLimit) {
	    cpuLimit = elapsedLimit = -1;
	    if (cpuLimit2 > 0.0 && cpu > cpuLimit2) {
		cpuLimit2 = -1.0;
		error(_("reached session CPU time limit"));
	    } else
		error(_("reached CPU time limit"));
	}
    }
}


/*
 *  helpers for start-up code
 */

#ifdef __FreeBSD__
# ifdef HAVE_FLOATINGPOINT_H
#  include <floatingpoint.h>
# endif
#endif

/* patch from Ei-ji Nakama for Intel compilers on ix86.
   From http://www.nakama.ne.jp/memo/ia32_linux/R-2.1.1.iccftzdaz.patch.txt.
   Since updated to include x86_64.
 */
#if (defined(__i386) || defined(__x86_64)) && defined(__INTEL_COMPILER) && __INTEL_COMPILER > 800
#include <xmmintrin.h>
#include <pmmintrin.h>
#endif

/* exported for Rembedded.h */
void fpu_setup(Rboolean start)
{
    if (start) {
#ifdef __FreeBSD__
    fpsetmask(0);
#endif

#if (defined(__i386) || defined(__x86_64)) && defined(__INTEL_COMPILER) && __INTEL_COMPILER > 800
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_OFF);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
#endif
    } else {
#ifdef __FreeBSD__
    fpsetmask(~0);
#endif
    }
}
