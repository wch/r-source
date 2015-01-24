/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file run.c: a simple 'reading' pipe (and a command executor)
 *  Copyright  (C) 1999-2001  Guido Masarotto and Brian Ripley
 *             (C) 2007-2014  The R Core Team
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include "win-nls.h"

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "run.h"

#include <Startup.h> /* for CharacterMode and RGui */

static char RunError[501] = "";

/* This might be given a command line (whole = 0) or just the
   executable (whole = 1).  In the later case the path may or may not
   be quoted */
static char *expandcmd(const char *cmd, int whole)
{
    char c = '\0';
    char *s, *p, *q = NULL, *f, *dest, *src;
    int   d, ext, len = strlen(cmd)+1;
    char buf[len], fl[len], fn[MAX_PATH];

    /* make a copy as we manipulate in place */
    strcpy(buf, cmd);

    // This is the return value.
    if (!(s = (char *) malloc(MAX_PATH + strlen(cmd)))) {
	strcpy(RunError, "Insufficient memory (expandcmd)");
	return NULL;
    }
    /* skip leading spaces */
    for (p = buf; *p && isspace(*p); p++);
    /* find the command itself, possibly double-quoted */
    if (whole) {
	d = 0;
    } else { // command line
	for (q = p, d = 0; *q && ( d || !isspace(*q) ); q++)
	    if (*q == '\"') d = d ? 0 : 1;
	if (d) {
	    strcpy(RunError, "A \" is missing (expandcmd)");
	    return NULL;
	}
	c = *q; /* character after the command, normally a space */
	*q = '\0';
    }

    /*
     * Guido resorted to this since SearchPath returned FOUND also
     * for file name without extension -> explicitly set
     *  extension
     */
    for (f = p, ext = 0 ; *f ; f++) {
	if ((*f == '\\') || (*f == '/')) ext = 0;
	else if (*f == '.') ext = 1;
    }
    /* SearchPath doesn't like ", so strip out quotes */
    for (dest = fl , src = p; *src ; src++)
	if (*src != '"') *dest++ = *src;
    *dest = '\0';
    if (ext) {
	/*
	 * user set extension; we don't check that it is executable;
	 * it might get an error after; but maybe sometimes
	 * in the future every extension will be executable
	 */
	d = SearchPath(NULL, fl, NULL, MAX_PATH, fn, &f);
    } else {
	int iexts = 0;
	const char *exts[] = { ".exe" , ".com" , ".cmd" , ".bat" , NULL };
	while (exts[iexts]) {
	    strcpy(dest, exts[iexts]);
	    if ((d = SearchPath(NULL, fl, NULL, MAX_PATH, fn, &f))) break;
	    iexts++ ;
	}
    }
    if (!d) {
	free(s);
	snprintf(RunError, 500, "'%s' not found", p);
	if(!whole) *q = c;
	return NULL;
    }
    /*
      NB: as of Windows 7 SearchPath does not return short names any more.

      Paranoia : on my system switching to short names is not needed
      since SearchPath already returns 'short names'. However,
      this is not documented so I prefer to be explicit.
    */
    GetShortPathName(fn, s, MAX_PATH);
    if (!whole) {
	*q = c;
	strcat(s, q);
    }
    return s;
}

/*
   finput is either NULL or the name of a file from which to
     redirect stdin for the child.
   newconsole != 0 to use a new console (if not waiting)
   visible = -1, 0, 1 for hide, minimized, default
   inpipe != 0 to duplicate I/O handles
   pi is set based on the newly created process,
   with the hThread handle closed.
*/

extern size_t Rf_utf8towcs(wchar_t *wc, const char *s, size_t n);

static void pcreate(const char* cmd, cetype_t enc,
		      int newconsole, int visible,
		      HANDLE hIN, HANDLE hOUT, HANDLE hERR,
		      PROCESS_INFORMATION *pi)
{
    DWORD ret;
    STARTUPINFO si;
    STARTUPINFOW wsi;
    HANDLE dupIN, dupOUT, dupERR;
    WORD showWindow = SW_SHOWDEFAULT;
    int inpipe;
    char *ecmd;
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;

    /* FIXME: this might need to be done in wchar_t */
    if (!(ecmd = expandcmd(cmd, 0))) return; /* error message already set */

    inpipe = (hIN != INVALID_HANDLE_VALUE)
	|| (hOUT != INVALID_HANDLE_VALUE)
	|| (hERR != INVALID_HANDLE_VALUE);

    if (inpipe) {
	HANDLE hNULL = CreateFile("NUL:", GENERIC_READ | GENERIC_WRITE, 0,
			   &sa, OPEN_EXISTING, 0, NULL);
	HANDLE hTHIS = GetCurrentProcess();

	if (hIN == INVALID_HANDLE_VALUE) hIN = hNULL;
	if (hOUT == INVALID_HANDLE_VALUE) hOUT = hNULL;
	if (hERR == INVALID_HANDLE_VALUE) hERR = hNULL;

	DuplicateHandle(hTHIS, hIN,
			hTHIS, &dupIN, 0, TRUE, DUPLICATE_SAME_ACCESS);
	DuplicateHandle(hTHIS, hOUT,
			hTHIS, &dupOUT, 0, TRUE, DUPLICATE_SAME_ACCESS);
	DuplicateHandle(hTHIS, hERR,
			hTHIS, &dupERR, 0, TRUE, DUPLICATE_SAME_ACCESS);
	CloseHandle(hTHIS);
	CloseHandle(hNULL);
    }

    switch (visible) {
    case -1:
	showWindow = SW_HIDE;
	break;
    case 0:
	showWindow = SW_SHOWMINIMIZED;
	break;
    }

    if(enc == CE_UTF8) {
	wsi.cb = sizeof(wsi);
	wsi.lpReserved = NULL;
	wsi.lpReserved2 = NULL;
	wsi.cbReserved2 = 0;
	wsi.lpDesktop = NULL;
	wsi.lpTitle = NULL;
	wsi.dwFlags = STARTF_USESHOWWINDOW;
	wsi.wShowWindow = showWindow;
	if (inpipe) {
	    wsi.dwFlags |= STARTF_USESTDHANDLES;
	    wsi.hStdInput  = dupIN;
	    wsi.hStdOutput = dupOUT;
	    wsi.hStdError  = dupERR;
	}
    } else {
	si.cb = sizeof(si);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = STARTF_USESHOWWINDOW;
	si.wShowWindow = showWindow;
	if (inpipe) {
	    si.dwFlags |= STARTF_USESTDHANDLES;
	    si.hStdInput  = dupIN;
	    si.hStdOutput = dupOUT;
	    si.hStdError  = dupERR;
	}
    }

    if(enc == CE_UTF8) {
	int n = strlen(ecmd); /* max no of chars */
	wchar_t wcmd[n+1];
	Rf_utf8towcs(wcmd, ecmd, n+1);
	ret = CreateProcessW(NULL, wcmd, &sa, &sa, TRUE,
			     (newconsole && (visible == 1)) ?
			     CREATE_NEW_CONSOLE : 0,
			     NULL, NULL, &wsi, pi);
    } else
	ret = CreateProcess(NULL, ecmd, &sa, &sa, TRUE,
			    (newconsole && (visible == 1)) ?
			    CREATE_NEW_CONSOLE : 0,
			    NULL, NULL, &si, pi);

    if (inpipe) {
	CloseHandle(dupIN);
	CloseHandle(dupOUT);
	CloseHandle(dupERR);
    }
    if (!ret)
	snprintf(RunError, 500, _("'CreateProcess' failed to run '%s'"), ecmd);
    else CloseHandle(pi->hThread);
    free(ecmd);
    return;
}

static int pwait(HANDLE p)
{
    DWORD ret;

    WaitForSingleObject(p, INFINITE);
    GetExitCodeProcess(p, &ret);
    return ret;
}

/* used in rpipeOpen */
static DWORD CALLBACK
threadedwait(LPVOID param)
{
    rpipe *p = (rpipe *) param;

    p->exitcode = pwait(p->pi.hProcess);
    FlushFileBuffers(p->write);
    FlushFileBuffers(p->read);
    p->active = 0;
    CloseHandle(p->thread);
    p->thread = NULL;
    return 0;
}

char *runerror(void)
{
    return RunError;
}

static HANDLE getInputHandle(const char *fin)
{
    if (fin && fin[0]) {
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;
	HANDLE hIN = CreateFile(fin, GENERIC_READ, 0,
				&sa, OPEN_EXISTING, 0, NULL);
	if (hIN == INVALID_HANDLE_VALUE) {
	    snprintf(RunError, 500, 
		     "unable to redirect input from '%s'", fin);
	    return NULL;
	}
	return hIN;
    } else if (fin) {
        /* GetStdHandle returns NULL for processes like RGui with no standard handles defined */
    	HANDLE result = GetStdHandle(STD_INPUT_HANDLE);
    	if (result) return result;
    }
    return INVALID_HANDLE_VALUE;
}

static HANDLE getOutputHandle(const char *fout, int type)
{
    if (fout && fout[0]) {
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = TRUE;
	HANDLE hOUT = CreateFile(fout, GENERIC_WRITE, 0,
				 &sa, CREATE_ALWAYS, 0, NULL);
	if (hOUT == INVALID_HANDLE_VALUE) {
	    snprintf(RunError, 500, 
		     "unable to redirect output to '%s'", fout);
	    return NULL;
	} else return hOUT;
    } else if (fout) {
        /* GetStdHandle returns NULL for processes like RGui */
        HANDLE result = GetStdHandle(type ? STD_ERROR_HANDLE : STD_OUTPUT_HANDLE);
        if (result) return result;
    }
    return INVALID_HANDLE_VALUE;
}

BOOL CALLBACK TerminateWindow(HWND hwnd, LPARAM lParam)
{
    DWORD ID ;

    GetWindowThreadProcessId(hwnd, &ID);

    if (ID == (DWORD)lParam)
	PostMessage(hwnd, WM_CLOSE, 0, 0);
    return TRUE;
}

/* Terminate the process pwait2 is waiting for. */

extern void GA_askok(const char *info);

static void terminate_process(void *p)
{
    PROCESS_INFORMATION *pi = (PROCESS_INFORMATION*)p;
    EnumWindows((WNDENUMPROC)TerminateWindow, (LPARAM)pi->dwProcessId);

    if (WaitForSingleObject(pi->hProcess, 5000) == WAIT_TIMEOUT) {
	if (R_Interactive)
	    GA_askok(_("Child process not responding.  R will terminate it."));
	TerminateProcess(pi->hProcess, 99);
    }
}

static int pwait2(HANDLE p)
{
    DWORD ret;

    while( WaitForSingleObject(p, 100) == WAIT_TIMEOUT )
	R_CheckUserInterrupt();

    GetExitCodeProcess(p, &ret);
    return ret;
}

/*
  Used for external commands in file.show() and edit(), and for
  system(intern=FALSE).  Also called from postscript().

  wait != 0 says wait for child to terminate before returning.
  visible = -1, 0, 1 for hide, minimized, default
  fin is either NULL or the name of a file from which to
  redirect stdin for the child.
  fout/ferr are NULL (use NUL:), "" (use standard streams) or filenames.
*/
int runcmd(const char *cmd, cetype_t enc, int wait, int visible,
	   const char *fin, const char *fout, const char *ferr)
{
    HANDLE hIN = getInputHandle(fin), hOUT, hERR;
    int ret = 0;
    PROCESS_INFORMATION pi;
    int close1 = 0, close2 = 0, close3 = 0;
    
    if (hIN && fin && fin[0]) close1 = 1;

    hOUT = getOutputHandle(fout, 0);
    if (!hOUT) return 1;
    if (fout && fout[0]) close2 = 1;
    if (fout && fout[0] && ferr && streql(fout, ferr)) hERR = hOUT;
    else { 
	hERR = getOutputHandle(ferr, 1);
	if (!hERR) return 1;
	if (ferr && ferr[0]) close3 = 1;
    }


    memset(&pi, 0, sizeof(pi));
    pcreate(cmd, enc, !wait, visible, hIN, hOUT, hERR, &pi);
    if (pi.hProcess) {
	if (wait) {
	    RCNTXT cntxt;
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	    cntxt.cend = &terminate_process;
	    cntxt.cenddata = &pi;
	    ret = pwait2(pi.hProcess);
	    endcontext(&cntxt);
	    snprintf(RunError, 501, _("Exit code was %d"), ret);
	    ret &= 0xffff;
	} else ret = 0;
	CloseHandle(pi.hProcess);
    } else {
    	ret = NOLAUNCH;
    }
    if (close1) CloseHandle(hIN);
    if (close2) CloseHandle(hOUT);
    if (close3) CloseHandle(hERR);
    return ret;
}

/*
   finput is either NULL or the name of a file from which to
     redirect stdin for the child.
   visible = -1, 0, 1 for hide, minimized, default
   io = 0 to read stdout from pipe, 1 to write to pipe,
   2 to read stderr from pipe, 
   3 to read both stdout and stderr from pipe.
 */
rpipe * rpipeOpen(const char *cmd, cetype_t enc, int visible,
		  const char *finput, int io,
		  const char *fout, const char *ferr)
{
    rpipe *r;
    HANDLE hTHIS, hIN, hOUT, hERR, hReadPipe, hWritePipe;
    DWORD id;
    BOOL res;
    int close1 = 0, close2 = 0, close3 = 0;

    if (!(r = (rpipe *) malloc(sizeof(struct structRPIPE)))) {
	strcpy(RunError, _("Insufficient memory (rpipeOpen)"));
	return NULL;
    }
    r->active = 0;
    r->pi.hProcess = NULL;
    r->thread = NULL;
    res = CreatePipe(&hReadPipe, &hWritePipe, NULL, 0);
    if (res == FALSE) {
	rpipeClose(r);
	strcpy(RunError, "CreatePipe failed");
	return NULL;
    }
    if(io == 1) { /* pipe for R to write to */
	hTHIS = GetCurrentProcess();
	r->read = hReadPipe;
	DuplicateHandle(hTHIS, hWritePipe, hTHIS, &r->write,
			0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(hWritePipe);
	CloseHandle(hTHIS);
	/* This sends stdout and stderr to NUL: */
	pcreate(cmd, enc, 1, visible,
		r->read, INVALID_HANDLE_VALUE, INVALID_HANDLE_VALUE,
		&(r->pi));
	r->active = 1;
	if (!r->pi.hProcess) return NULL; else return r;
    }

    /* pipe for R to read from */
    hTHIS = GetCurrentProcess();
    r->write = hWritePipe;
    DuplicateHandle(hTHIS, hReadPipe, hTHIS, &r->read,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    CloseHandle(hReadPipe);
    CloseHandle(hTHIS);

    hIN = getInputHandle(finput); /* a file or (usually NUL:) */
    
    if (hIN && finput && finput[0]) close1 = 1;
    
    if ((io == 0 || io == 3)) 
	hOUT = r->write;
    else {
	if (fout && fout[0]) close2 = 1;
 	hOUT = getOutputHandle(fout, 0);
    }
    if (io >= 2) 
	hERR = r->write;
    else {
	if (ferr && ferr[0]) close3 = 1;
	hERR = getOutputHandle(ferr, 1);
    }
    pcreate(cmd, enc, 0, visible, hIN, hOUT, hERR, &(r->pi));
    if (close1) CloseHandle(hIN);
    if (close2) CloseHandle(hOUT);
    if (close3) CloseHandle(hERR);

    r->active = 1;
    if (!r->pi.hProcess)
	return NULL;
    if (!(r->thread = CreateThread(NULL, 0, threadedwait, r, 0, &id))) {
	rpipeClose(r);
	strcpy(RunError, "CreateThread failed");
	return NULL;
    }
    return r;
}

static void
rpipeTerminate(rpipe * r)
{
    if (r->thread) {
	TerminateThread(r->thread, 0);
	CloseHandle(r->thread);
	r->thread = NULL;
    }
    if (r->active) {
	terminate_process(&(r->pi));
	r->active = 0;
    }
}

#include "graphapp/ga.h"
extern Rboolean UserBreak;

int
rpipeGetc(rpipe * r)
{
    DWORD a, b;
    char  c;

    if (!r)
	return NOLAUNCH;
    while (PeekNamedPipe(r->read, NULL, 0, NULL, &a, NULL)) {
	if (!a && !r->active) {
	    /* I got a case in which process terminated after Peek.. */
	    PeekNamedPipe(r->read, NULL, 0, NULL, &a, NULL);
	    if (!a) return NOLAUNCH;/* end of pipe */
	}
	if (a) {
	    if (ReadFile(r->read, &c, 1, &b, NULL) == TRUE)
		return c;
	    else
		return NOLAUNCH;/* error but...treated as eof */
	}
	/* we want to look for user break here */
	while (peekevent()) doevent();
	if (UserBreak) {
	    rpipeTerminate(r);
	    break;
	}
	R_ProcessEvents();
	Sleep(100);
    }
    return NOLAUNCH;		/* again.. */
}


char * rpipeGets(rpipe * r, char *buf, int len)
{
    int   i, c;

    if ((len < 2) || !r) return NULL;
    for (i = 0; i < (len - 1); i++) {
	if ((c = rpipeGetc(r)) == NOLAUNCH) {
	    if (i == 0) return NULL;
	    else {
		buf[i] = '\0';
		return buf;
	    }
	}
	buf[i] = c;
	if (c == '\n') {
	    if ((i > 0) && (buf[i - 1] == '\r')) {
		buf[i - 1] = '\n';
		buf[i] = '\0';
	    } else
		buf[i + 1] = '\0';
	    return buf;
	}
    }
    buf[len - 1] = '\0';
    return buf;
}

int rpipeClose(rpipe * r)
{
    int   i;

    if (!r) return NOLAUNCH;
    rpipeTerminate(r);
    CloseHandle(r->read);
    CloseHandle(r->write);
    CloseHandle(r->pi.hProcess);
    i = r->exitcode;
    free(r);
    return i &= 0xffff;
}

/* ------------------- Windows pipe connections --------------------- */

#include <Fileio.h>
#include <Rconnections.h>

typedef struct Wpipeconn {
    rpipe *rp;
} *RWpipeconn;


static Rboolean Wpipe_open(Rconnection con)
{
    rpipe *rp;
    int visible = -1, io;

    io = con->mode[0] == 'w';
    if(io) visible = 1; /* Somewhere to put the output */
    rp = rpipeOpen(con->description, con->enc, visible, NULL, io, NULL, NULL);
    if(!rp) {
	warning("cannot open cmd `%s'", con->description);
	return FALSE;
    }
    ((RWpipeconn)(con->private))->rp = rp;
    con->isopen = TRUE;
    con->canwrite = io;
    con->canread = !con->canwrite;
    if(strlen(con->mode) >= 2 && con->mode[1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    return TRUE;
}

static void Wpipe_close(Rconnection con)
{
    con->status = rpipeClose( ((RWpipeconn)con->private) ->rp);
    con->isopen = FALSE;
}

static void Wpipe_destroy(Rconnection con)
{
    free(con->private);
}


static int Wpipe_fgetc(Rconnection con)
{
    rpipe *rp = ((RWpipeconn)con->private) ->rp;
    int c;

    c = rpipeGetc(rp);
    return c == NOLAUNCH ? R_EOF : c;
}


static double null_seek(Rconnection con, double where, int origin, int rw)
{
    error(_("seek not enabled for this connection"));
    return 0; /* -Wall */
}

static void null_truncate(Rconnection con)
{
    error(_("truncate not enabled for this connection"));
}

static int Wpipe_fflush(Rconnection con)
{
    BOOL res;

    rpipe *rp = ((RWpipeconn)con->private) ->rp;
    res = FlushFileBuffers(rp->write);
    return res ? 0 : EOF;
}

static size_t Wpipe_read(void *ptr, size_t size, size_t nitems,
			Rconnection con)
{
    rpipe *rp = ((RWpipeconn)con->private) ->rp;
    DWORD ntoread, read;

    while (PeekNamedPipe(rp->read, NULL, 0, NULL, &ntoread, NULL)) {
	if (!ntoread && !rp->active) {
	    /* I got a case in which process terminated after Peek.. */
	    PeekNamedPipe(rp->read, NULL, 0, NULL, &ntoread, NULL);
	    if (!ntoread) return 0; /* end of pipe */
	}
	if (ntoread) {
	    if (ReadFile(rp->read, ptr, nitems * size, &read, NULL) == TRUE)
		return read/size;
	    else return 0; /* error */
	}
    }
    return 0;
}

static size_t Wpipe_write(const void *ptr, size_t size, size_t nitems,
			 Rconnection con)
{
    rpipe *rp = ((RWpipeconn)con->private) ->rp;
    DWORD towrite = nitems * size, write, ret;

    if(!rp->active) return 0;
    GetExitCodeProcess(rp->pi.hProcess, &ret);
    if(ret != STILL_ACTIVE) {
	rp->active = 0;
	warning("broken Windows pipe");
	return 0;
    }
    if (WriteFile(rp->write, ptr, towrite, &write, NULL) != 0)
	return write/size;
    else return 0;
}

#define BUFSIZE 10000
static int Wpipe_vfprintf(Rconnection con, const char *format, va_list ap)
{
    R_CheckStack2(BUFSIZE);
    char buf[BUFSIZE], *b = buf;
    int res = 0;

    res = vsnprintf(b, BUFSIZE, format, ap);
    if(res < 0) { /* a failure indication, so try again */
	b[BUFSIZE -1] = '\0';
	warning("printing of extremely long output is truncated");
	res = BUFSIZE;
    }
    return Wpipe_write(buf, res, 1, con);
}


Rconnection newWpipe(const char *description, int ienc, const char *mode)
{
    Rconnection new;
    char *command;
    int len;

    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of pipe connection failed"));
    new->class = (char *) malloc(strlen("pipe") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of pipe connection failed"));
    }
    strcpy(new->class, "pipe");

    len = strlen(getenv("COMSPEC")) + strlen(description) + 5;
    command = (char *) malloc(len);
    if (command)
	new->description = (char *) malloc(len);
    else
	new->description = NULL;

    if(!new->description) {
	free(command); free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
    }

    /* We always use COMSPEC here, not R_SHELL or SHELL,
       for compatibility with Rterm.
       We also use /c for the same reason.
    */

    strcpy(command, getenv("COMSPEC"));
    strcat(command, " /c ");
    strcat(command, description);

    init_con(new, command, ienc, mode);
    free(command);

    new->open = &Wpipe_open;
    new->close = &Wpipe_close;
    new->destroy = &Wpipe_destroy;
    new->vfprintf = &Wpipe_vfprintf;
    new->fgetc = &Wpipe_fgetc;
    new->seek = &null_seek;
    new->truncate = &null_truncate;
    new->fflush = &Wpipe_fflush;
    new->read = &Wpipe_read;
    new->write = &Wpipe_write;
    new->private = (void *) malloc(sizeof(struct Wpipeconn));
    if(!new->private) {
	free(new->description); free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
    }
    return new;
}


SEXP do_syswhich(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nm, ans;
    int i, n;

    checkArity(op, args);
    nm = CAR(args);
    if(!isString(nm))
	error(_("'names' is not a character vector"));
    n = LENGTH(nm);
    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
	if (STRING_ELT(nm, i) == NA_STRING) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    const char *this = CHAR(STRING_ELT(nm, i));
	    char *that = expandcmd(this, 1);
	    SET_STRING_ELT(ans, i, mkChar(that ? that : ""));
	    free(that);
	}
    }
    setAttrib(ans, R_NamesSymbol, nm);
    UNPROTECT(1);
    return ans;
}
