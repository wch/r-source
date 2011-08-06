/* multicore R package

   fork.c
   interface to system-level tools for sawning copies of the current
   process and IPC

   (C)Copyright 2008-11 Simon Urbanek

   see package DESCRIPTION for licensing terms (GPL-2) */

#include "parallel.h"

#include <sys/types.h>
#include <unistd.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <signal.h>
#include <errno.h>

#include <R.h>
#include <Rinternals.h>
#include <Rconfig.h> /* for AQUA */
#if HAVE_AQUA
# include <R_ext/Rdynload.h>
#endif

#define Dprintf printf

typedef struct child_info {
    pid_t pid; /* child's pid */
    int pfd, sifd; /* master's ends of pipes */
    struct child_info *next;
} child_info_t;

static child_info_t *children; /* in master, linked list of details of children */

static int master_fd = -1; /* in child, write end of data pipe */
static int is_master = 1; /* 0 in child */

static int rm_child_(int pid) 
{
    child_info_t *ci = children, *prev = 0;
#ifdef MC_DEBUG
    Dprintf("removing child %d\n", pid);
#endif
    while (ci) {
	if (ci->pid == pid) {
	    /* make sure we close all descriptors */
	    if (ci->pfd > 0) { close(ci->pfd); ci->pfd = -1; }
	    if (ci->sifd > 0) { close(ci->sifd); ci->sifd = -1; }
	    /* now remove it from the list */
	    if (prev) prev->next = ci->next;
	    else children = ci->next;
	    free(ci);
	    kill(pid, SIGUSR1); /* send USR1 to the child to make sure it exits */
	    return 1;
	}
	prev = ci;
	ci = ci->next;
    }
#ifdef MC_DEBUG
    Dprintf("WARNING: child %d was to be removed but it doesn't exist\n", pid);
#endif
    return 0;
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
	    exit(child_exit_status);
    }
}

#if HAVE_AQUA
/* from aqua.c */
extern void (*ptr_R_ProcessEvents)(void);

static int find_quartz_symbols = 1;
void (*QuartzCocoa_InhibitEventLoop)(int);
typedef void (*QuartzCocoa_InhibitEventLoop_t)(int);

/* unfortunately Rdynload.h forgets to declare it so the API is broken
   - we need to fix it */
struct Rf_RegisteredNativeSymbol {
  NativeSymbolType type;
  void *fn, *dll;
};

/* check whether Quartz is loaded (if not, returns -1) and if so
  returns 1 is QuartzCocoa_InhibitEventLoop has been found, 0
  otherwise */
static int getQuartzSymbols() 
{
    if (find_quartz_symbols) {
	R_RegisteredNativeSymbol symbol = {R_ANY_SYM, NULL, NULL};
	if (R_FindSymbol("getQuartzAPI", "", &symbol)) { 
	    /* is Quartz loaded? if not, we have nothing to worry about */
	    /* unfortunately R disables dynamic lookup in grDevices so
	       we need to get at it manually this means that we need
	       to get the corresponding DllInfo to enable it, then
	       look up the symbol and disable it again */
	    SEXP getNativeSymbolInfo = install("getNativeSymbolInfo");
	    SEXP nsi = eval(lang2(getNativeSymbolInfo, 
				  mkString("getQuartzAPI")), R_GlobalEnv);
	    /* get nsi[[3]][[2]] which should be the path (we verify every step) */
	    if (TYPEOF(nsi) == VECSXP && LENGTH(nsi) > 2) {
		SEXP pkg = VECTOR_ELT(nsi, 2);
		if (TYPEOF(pkg) == VECSXP && LENGTH(pkg) > 1) {
		    SEXP dpath = VECTOR_ELT(pkg, 1);
		    if (TYPEOF(dpath) == STRSXP && LENGTH(dpath) > 0) {
			/* this is technically unnecessary since nsi
			   actually contains the EXTPTR holding the
			   DllInfo, but we'll play it safe here */
			DllInfo *dll = R_getDllInfo(CHAR(STRING_ELT(dpath, 0)));
			if (dll) {
			    struct Rf_RegisteredNativeSymbol {
				NativeSymbolType type; void *fn, *dll; 
			    } symbol = { R_ANY_SYM, NULL, NULL };
			    R_useDynamicSymbols(dll, TRUE); /* turn on dynamic symbols */
			    /* it would be faster to use R_dlsym since we already have DllInfo but that is hidden so let's waste more cycles.. */
			    QuartzCocoa_InhibitEventLoop = (QuartzCocoa_InhibitEventLoop_t) R_FindSymbol("QuartzCocoa_InhibitEventLoop", "grDevices", (R_RegisteredNativeSymbol*) &symbol);
			    R_useDynamicSymbols(dll, FALSE); /* turn them off - we got what we want */
			}
		    }
		}
	    }
	    /* do not try again since we did all the work */
	    find_quartz_symbols = 0;
	}
    }
    return find_quartz_symbols ? -1 : ((QuartzCocoa_InhibitEventLoop) ? 1 : 0);
}
#else
static int getQuartzSymbols() { return -1; }
#endif

SEXP mc_fork() 
{
    int pipefd[2]; /* write end, read end */
    int sipfd[2];
    pid_t pid;
    SEXP res = allocVector(INTSXP, 3);
    int *res_i = INTEGER(res);
    if (pipe(pipefd)) error(_("unable to create a pipe"));
    if (pipe(sipfd)) {
	close(pipefd[0]); close(pipefd[1]);
	error(_("unable to create a pipe"));
    }

    getQuartzSymbols(); /* initialize Quartz symbols if needed (no-op
			   on non-Aqua systems) */
    pid = fork();
    if (pid == -1) {
	close(pipefd[0]); close(pipefd[1]);
	close(sipfd[0]); close(sipfd[1]);
	error(_("unable to fork, possible reason: %s"), strerror(errno));
    }
    res_i[0] = (int) pid;
    if (pid == 0) { /* child */
	close(pipefd[0]); /* close read end */
	master_fd = res_i[1] = pipefd[1];
	is_master = 0;
#if HAVE_AQUA
	ptr_R_ProcessEvents = NULL; /* disable ProcessEvent since we can't call CF from now on */
#endif
	/* re-map stdin */
	dup2(sipfd[0], STDIN_FILENO);
	close(sipfd[0]);
	/* master uses USR1 to signal that the child process can terminate */
	child_exit_status = -1;
	child_can_exit = 0;
	signal(SIGUSR1, child_sig_handler);
#if HAVE_AQUA
	/* Quartz runs the event loop so we need to stop it if we can */
	if (QuartzCocoa_InhibitEventLoop)
	    QuartzCocoa_InhibitEventLoop(1);
#endif
#ifdef MC_DEBUG
	Dprintf("child process %d started\n", getpid());
#endif
    } else { /* master process */
	child_info_t *ci;
	close(pipefd[1]); /* close write end of the data pipe */
	close(sipfd[0]);  /* close read end of the child-stdin pipe */
	res_i[1] = pipefd[0];
	res_i[2] = sipfd[1];
#ifdef MC_DEBUG
	Dprintf("parent registers new child %d\n", pid);
#endif
	/* register the new child and its pipes */
	ci = (child_info_t*) malloc(sizeof(child_info_t));
	if (!ci) error(_("memory allocation error"));
	ci->pid = pid;
	ci->pfd = pipefd[0];
	ci->sifd= sipfd[1];
	ci->next = children;
	children = ci;
    }
    return res; /* (pid, fd of data pipe, fd of child-stdin pipe) */
}


SEXP mc_close_stdout() 
{
    close(STDOUT_FILENO);
    return R_NilValue;
}

SEXP mc_close_stderr() 
{
    close(STDERR_FILENO);
    return R_NilValue;
}

SEXP mc_close_fds(SEXP sFDS) 
{
    int *fd, fds, i = 0;
    if (TYPEOF(sFDS) != INTSXP) error("descriptors must be integers");
    fds = LENGTH(sFDS);
    fd = INTEGER(sFDS);
    while (i < fds) close(fd[i++]);
    return ScalarLogical(1);
}

SEXP mc_send_master(SEXP what)
{
    unsigned char *b;
    unsigned int len = 0, i = 0;
    if (is_master)
	error(_("only children can send data to the master process"));
    if (master_fd == -1) 
	error(_("there is no pipe to the master process"));
    if (TYPEOF(what) != RAWSXP) 
	error(_("content to send must be RAW, use serialize() if needed"));
    len = LENGTH(what);
    b = RAW(what);
#ifdef MC_DEBUG
    Dprintf("child %d: send_master (%d bytes)\n", getpid(), len);
#endif
    if (write(master_fd, &len, sizeof(len)) != sizeof(len)) {
	close(master_fd);
	master_fd = -1;
	error(_("write error, closing pipe to the master"));
    }
    while (i < len) {
	int n = write(master_fd, b + i, len - i);
	if (n < 1) {
	    close(master_fd);
	    master_fd = -1;
	    error(_("write error, closing pipe to the master"));
	}
	i += n;
    }
    return ScalarLogical(1);
}

SEXP mc_send_child_stdin(SEXP sPid, SEXP what) 
{
    unsigned char *b;
    unsigned int len = 0, i = 0, fd;
    int pid = asInteger(sPid);
    if (!is_master) 
	error(_("only the master process can send data to a child process"));
    if (TYPEOF(what) != RAWSXP) error("what must be a raw vector");
    child_info_t *ci = children;
    while (ci) {
	if (ci->pid == pid) break;
	ci = ci -> next;
    }
    if (!ci) error(_("child %d does not exist"), pid);
    len = LENGTH(what);
    b = RAW(what);
    fd = ci -> sifd;
    while (i < len) {
	int n = write(fd, b + i, len - i);
	if (n < 1) error(_("write error"));
	i += n;
    }
    return ScalarLogical(1);
}

SEXP mc_select_children(SEXP sTimeout, SEXP sWhich) 
{
    int maxfd = 0, sr, zombies = 0;
    unsigned int wlen = 0, wcount = 0;
    SEXP res;
    int *res_i, *which = 0;
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
    if (TYPEOF(sWhich) == INTSXP && LENGTH(sWhich)) {
	which = INTEGER(sWhich);
	wlen = LENGTH(sWhich);
    }
    { 
	int wstat; 
	while (waitpid(-1, &wstat, WNOHANG) > 0) ; /* check for zombies */
    }
    FD_ZERO(&fs);
    while (ci && ci->pid) {
	if (ci->pfd == -1) zombies++;
	if (ci->pfd > maxfd) maxfd = ci->pfd;
	if (ci->pfd > 0) {
	    if (which) { /* check for the FD only if it's on the list */
		unsigned int k = 0;
		while (k < wlen) 
		    if (which[k++] == ci->pid) { 
			FD_SET(ci->pfd, &fs);
			wcount++;
			break; 
		    }
	    } else FD_SET(ci->pfd, &fs);
	}
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("select_children: maxfd=%d, wlen=%d, wcount=%d, zombies=%d, timeout=%d:%d\n", maxfd, wlen, wcount, zombies, (int)tv.tv_sec, (int)tv.tv_usec);
#endif
    if (zombies) { /* oops, this should never really happen - it did
		    * while we had a bug in rm_child_ but hopefully
		    * not anymore */
	while (zombies) { /* this is rather more complicated than it
			   * should be if we used pointers to delete,
			   * but well ... */
	    ci = children;
	    while (ci) {
		if (ci->pfd == -1) {
#ifdef MC_DEBUG
		    Dprintf("detected zombie: pid=%d, pfd=%d, sifd=%d\n", 
			    ci->pid, ci->pfd, ci->sifd);
#endif
		    rm_child_(ci->pid);
		    zombies--;
		    break;
		}
		ci = ci->next;
	    }
	    if (!ci) break;
	}
    }
    if (maxfd == 0 || (wlen && !wcount)) 
	return R_NilValue; /* NULL signifies no children to tend to */
    sr = select(maxfd + 1, &fs, 0, 0, tvp);
#ifdef MC_DEBUG
    Dprintf("  sr = %d\n", sr);
#endif
    if (sr < 0) {
	warning(_("error '%s' in select"), strerror(errno));
	return ScalarLogical(0); /* FALSE on select error */
    }
    if (sr < 1) return ScalarLogical(1); /* TRUE on timeout */
    ci = children;
    maxfd = 0;
    while (ci && ci->pid) { /* pass 1 - count the FDs (in theory not
			       necessary since that's what select
			       should have returned)  */
	if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) maxfd++;
	ci = ci -> next;
    }
    ci = children;
#ifdef MC_DEBUG
    Dprintf(" - read select %d children: ", maxfd);
#endif
    res = allocVector(INTSXP, maxfd);
    res_i = INTEGER(res);
    while (ci && ci->pid) { /* pass 2 - fill the array */
	if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) (res_i++)[0] = ci->pid;
#ifdef MC_DEBUG
	if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) Dprintf("%d ", ci->pid);
#endif
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("\n");
#endif
    return res;
}

static SEXP read_child_ci(child_info_t *ci) 
{
    unsigned int len = 0;
    int fd = ci->pfd;
    int n = read(fd, &len, sizeof(len));
#ifdef MC_DEBUG
    Dprintf(" read_child_ci(%d) - read length returned %d\n", ci->pid, n);
#endif
    if (n != sizeof(len) || len == 0) { /* error or child is exiting */
	int pid = ci->pid;
	close(fd);
	ci->pfd = -1;
	rm_child_(pid);
	return ScalarInteger(pid);
    } else {
	SEXP rv = allocVector(RAWSXP, len);
	unsigned char *rvb = RAW(rv);
	unsigned int i = 0;
	while (i < len) {
	    n = read(fd, rvb + i, len - i);
#ifdef MC_DEBUG
	    Dprintf(" read_child_ci(%d) - read %d at %d returned %d\n", ci->pid, len-i, i, n);
#endif
	    if (n < 1) {
		int pid = ci->pid;
		close(fd);
		ci->pfd = -1;
		rm_child_(pid);
		return ScalarInteger(pid);
	    }
	    i += n;
	}
	PROTECT(rv);
	{
	    SEXP pa = allocVector(INTSXP, 1);
	    INTEGER(pa)[0] = ci->pid;
	    setAttrib(rv, install("pid"), pa);
	}
	UNPROTECT(1);
	return rv;
    }
}

SEXP mc_read_child(SEXP sPid) 
{
    int pid = asInteger(sPid);
    child_info_t *ci = children;
    while (ci) {
	if (ci->pid == pid) break;
	ci = ci->next;
    }
#ifdef MC_DEBUG
    if (!ci) Dprintf("read_child(%d) - pid is not in the list of children\n", pid);
#endif
    if (!ci) return R_NilValue; /* if the child doesn't exist anymore, returns NULL */
    return read_child_ci(ci);	
}

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
    while (ci && ci->pid) {
	if (ci->pfd > maxfd) maxfd = ci->pfd;
	if (ci->pfd > 0) FD_SET(ci->pfd, &fs);
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("read_children: maxfd=%d, timeout=%d:%d\n", maxfd, (int)tv.tv_sec, (int)tv.tv_usec);
#endif
    if (maxfd == 0) return R_NilValue; /* NULL signifies no children to tend to */
    sr = select(maxfd+1, &fs, 0, 0, tvp);
#ifdef MC_DEBUG
    Dprintf("sr = %d\n", sr);
#endif
    if (sr < 0) {
	warning(_("error '%s' in select"), strerror(errno));
	return ScalarLogical(0); /* FALSE on select error */
    }
    if (sr < 1) return ScalarLogical(1); /* TRUE on timeout */
    ci = children;
    while (ci && ci->pid) {
	if (ci->pfd > 0 && FD_ISSET(ci->pfd, &fs)) break;
	ci = ci -> next;
    }
#ifdef MC_DEBUG
    Dprintf("set ci=%p (%d, %d)\n", (void*) ci, ci?ci->pid:0, ci?ci->pfd:0);
#endif
    /* this should never occur really - select signalled a read handle
       but none of the handles is set - let's treat it as a timeout */
    if (!ci) return ScalarLogical(1);
    else return read_child_ci(ci);
    /* we should never land here */
    return R_NilValue;
}

SEXP mc_rm_child(SEXP sPid) 
{
    int pid = asInteger(sPid);
    return ScalarLogical(rm_child_(pid));
}

SEXP mc_children() 
{
    unsigned int count = 0;
    SEXP res;
    int *pids;
    child_info_t *ci = children;
    while (ci && ci->pid > 0) {
	count++;
	ci = ci->next;
    }
    res = allocVector(INTSXP, count);
    if (count) {
	pids = INTEGER(res);
	ci = children;
	while (ci && ci->pid > 0) {
	    (pids++)[0] = ci->pid;
	    ci = ci->next;
	}
    }
    return res;
}

SEXP mc_fds(SEXP sFdi) 
{
    int fdi = asInteger(sFdi);
    unsigned int count = 0;
    SEXP res;
    child_info_t *ci = children;
    while (ci && ci->pid > 0) {
	count++;
	ci = ci->next;
    }
    res = allocVector(INTSXP, count);
    if (count) {
	int *fds = INTEGER(res);
	ci = children;
	while (ci && ci->pid > 0) {
	    (fds++)[0] = (fdi == 0) ? ci->pfd : ci->sifd;
	    ci = ci->next;
	}
    }
    return res;
}


SEXP mc_master_fd() 
{
    return ScalarInteger(master_fd);
}

SEXP mc_is_child() 
{
    return ScalarLogical(is_master?0:1);
}

SEXP mc_kill(SEXP sPid, SEXP sSig) 
{
    int pid = asInteger(sPid);
    int sig = asInteger(sSig);
    if (kill((pid_t) pid, sig))
	error(_("mckill failed"));
    return ScalarLogical(1);
}

SEXP mc_exit(SEXP sRes) 
{
    int res = asInteger(sRes);
#ifdef MC_DEBUG
    Dprintf("child %d: mcexit called\n", getpid());
#endif
    if (is_master) error(_("mcexit can only be used in a child process"));
    if (master_fd != -1) { /* send 0 to signify that we're leaving */
	unsigned int len = 0;
	write(master_fd, &len, sizeof(len));
	/* make sure the pipe is closed before we enter any waiting */
	close(master_fd);
	master_fd = -1;
    }
    if (!child_can_exit) {
#ifdef MC_DEBUG
	Dprintf("child %d is waiting for permission to exit\n", getpid());
#endif
	while (!child_can_exit) sleep(1);
    }
		
#ifdef MC_DEBUG
    Dprintf("child %d: exiting\n", getpid());
#endif
    exit(res);
    error(_("mcexit failed"));
    return R_NilValue;
}

/* this is not really necessary, since from R you can simply use
   is.loaded("QuartzCocoa_InhibitEventLoop") and it will be TRUE if we
   got to it. */

SEXP mc_can_disable_quartz() {
    return Rf_ScalarLogical(getQuartzSymbols());
}
