/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file run.c: a simple 'reading' pipe (and a command executor)
 *  Copyright  (C) 1999-2001  Guido Masarotto and Brian Ripley
 *             (C) 2007-2023  The R Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include "win-nls.h"

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <versionhelpers.h>
#include <mmsystem.h> /* for timeGetTime */
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "run.h"

#include <Startup.h> /* for CharacterMode and RGui */

#include <trioremap.h>

static char RunError[501] = "";

static Rboolean hasspace(const char *s)
{
    if (!s)
	return FALSE;
    for(;*s;s++)
	if (isspace(*s)) return TRUE;
    return FALSE;
} 

/* This might be given a command line (whole = 0) or just the
   executable (whole = 1).  In the later case the path may or may not
   be quoted. 

   When whole = 0, the command will be quoted in the result if it contains
   space. */
static char *expandcmd(const char *cmd, int whole)
{
    char c = '\0';
    char *s = NULL, *p, *q = NULL, *f, *dest, *src, *fn = NULL;
    int  ext, len = strlen(cmd)+1;
    char buf[len], fl[len + 4];
    DWORD d, res = 0;

    /* make a copy as we manipulate in place */
    strcpy(buf, cmd);

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
	*q = '\0'; /* modifies buf */
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
	d = SearchPath(NULL, fl, NULL, 0, NULL, &f);
    } else {
	int iexts = 0;
	/* update the size of fl above if adding extensions longer than 3 chars */
	const char *exts[] = { ".exe" , ".com" , ".cmd" , ".bat" , NULL };
	while (exts[iexts]) {
	    strcpy(dest, exts[iexts]); /* modifies fl */
	    if ((d = SearchPath(NULL, fl, NULL, 0, NULL, &f))) break;
	    iexts++ ;
	}
    }
    if (d > 0) {
	/* perform the search again with the right buffer size */

	/* The +10 below is a hack to work-around what appears to be a bug
	   observed on Windows 10 (build 19045). When the corresponding PATH
	   entry ends with one or more extra separators (e.g. dir\/,
	   dir\\ or dir//), the nBufferLength argument must be increased by
	   that number, otherwise SearchPath reports the path doesn't fit.
	   When the number is increased, the path is returned correctly
	   without the extra separators. */
	if (!(fn = (char *) malloc(d + 10))) {
	    strcpy(RunError, "Insufficient memory (expandcmd)");
	    return NULL;
	}
	DWORD oldd = d;
	d = SearchPath(NULL, fl, NULL, d + 10, fn, &f);
	if (d >= oldd)
	    /* treat as error when path doesn't fit now */
	    d = 0;
    }
    if (!d)    {
	if (fn) free(fn);
	snprintf(RunError, 500, "'%s' not found", p);
	return NULL;
    }
    /*
      NB: as of Windows 7 SearchPath does not return short names any more.

      Paranoia : on my system switching to short names is not needed
      since SearchPath already returns 'short names'. However,
      this is not documented so I prefer to be explicit.
    */
    /* NOTE: short names are not always enabled/available. In that case,
       GetShortPathName may succeed and return the original (long) name. */

    res = GetShortPathName(fn, NULL, 0);
    if (res > 0) {
	/* perform the translation again with sufficient buffer size */
	// This is the return value.
	if (!(s = (char *) malloc(res + len + 2))) {
	    /* the size over-estimate, +2 in case quotes will be needed */
	    if (fn) free(fn);
	    strcpy(RunError, "Insufficient memory (expandcmd)");
	    return NULL;
	}
	res = GetShortPathName(fn, s, res);
    }
    if (res == 0) {
	/* Use full name if GetShortPathName fails, i.e. due to insufficient
	   permissions for some component of the path. */
	if (s) free(s);
	// This is the return value.
	if (!(s = (char *) malloc(d + len + 2))) { /* over-estimate */
	    if (fn) free(fn);
	    strcpy(RunError, "Insufficient memory (expandcmd)");
	    return NULL;
	}
	if (!whole && hasspace(fn)) 
	    snprintf(s, d + 3, "\"%s\"", fn);
	else
	    strncpy(s, fn, d + 1);
    } else if (!whole && hasspace(s)) {
	/* GetShortPathName succeeded but it may still have returned the
	   long path (so with spaces) */

	memmove(s + 1, s, res + 1); 
	s[0] = '"';
	s[1 + res] = '"';
	s[1 + res + 1] = '\0';
    }

    if (!whole) {
	*q = c;         /* restore character after command */
	strcat(s, q);   /* add the rest of input (usually arguments) */
    }
    if (fn) free(fn);
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

/* NOTE: this doesn't work for CE_UTF8 due to expandcmd() */
static void pcreate(const char* cmd, cetype_t enc,
		      int newconsole, int visible,
		      HANDLE hIN, HANDLE hOUT, HANDLE hERR,
		      pinfo *pi, int consignals)
{
    DWORD ret;
    STARTUPINFO si;
    STARTUPINFOW wsi;
    HANDLE dupIN, dupOUT, dupERR, job, port = NULL;
    WORD showWindow = SW_SHOWDEFAULT;
    DWORD flags;
    BOOL inJob;
    Rboolean breakaway;
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;
    JOBOBJECT_ASSOCIATE_COMPLETION_PORT cport;
    int inpipe;
    char *ecmd;
    SECURITY_ATTRIBUTES sa;
    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;

    /* FIXME: this would have to be done in wchar_t/UTF-16LE to support
       CE_UTF8; the enc==CE_UTF8 branch could be phased out once UTF-8 as
       native encoding can be assumed on all Windows systems  */
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

    /* Originally, the external process has been waited for only using
       waitForSingleObject, but that has been proven unreliable: sometimes
       the output file would still be opened (and hence locked) by some
       child process after waitForSingleObject would finish. This has been
       observed also while running tests and particularly when building
       vignettes, resulting in spurious "Permission denied" errors.

       This has been happening almost surely due to a child process not
       waiting for its own children to finish, which has been reported
       to happen with Linux utilities ported to Windows as used for tests
       in Haskell/GHC. Inspired by Haskell process and a blog post about
       waiting for a process tree to finish, we now use job objects to
       wait also for process trees with this issue:

	https://github.com/haskell/process
	https://blogs.msdn.microsoft.com/oldnewthing/20130405-00/?p=4743

       In addition, we try to be easy on applications coded to rely on that
       they do not run in a job, when running in old Windows that do not
       support nested jobs. On newer versions of Windows, we use nested jobs.
    */

    /* Creating the process with CREATE_BREAKAWAY_FROM_JOB is safe when
       the process is not in any job or when it is in a job that allows it.
       The documentation does not say what would happen if we set the flag,
       but run in a job that does not allow it, so better don't.

       Do not consider breakaway on Windows 8 (Windows Server 2012) and newer,
       but instead use nested jobs.
    */
    breakaway = FALSE;
    if (!IsWindows8OrGreater() &&
        IsProcessInJob(GetCurrentProcess(), NULL, &inJob) && inJob) {
	/* The documentation does not say that it would be ok to use
	   QueryInformationJobObject when the process is not in the job,
	   so we have better tested that upfront. */
	ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
	ret = QueryInformationJobObject(
		NULL,
	        JobObjectExtendedLimitInformation,
	        &jeli,
	        sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION),
		NULL);
	breakaway = ret &&
		(jeli.BasicLimitInformation.LimitFlags &
	         JOB_OBJECT_LIMIT_BREAKAWAY_OK);
    }

    /* create a job that allows breakaway */
    job = CreateJobObject(NULL, NULL);
    if (job) {
	ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
	jeli.BasicLimitInformation.LimitFlags =
	    /* JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE helps to terminate grand
	       child processes when the child process executed is R
	       and breakaway is used. */
	    JOB_OBJECT_LIMIT_BREAKAWAY_OK | JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
	ret = SetInformationJobObject(
		job,
		JobObjectExtendedLimitInformation,
		&jeli,
                sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
	if (!ret) {
	    CloseHandle(job);
	    job = NULL;
	}
    }

    /* create a completion port to learn when processes exit */
    if (job) {
	port = CreateIoCompletionPort(INVALID_HANDLE_VALUE, NULL, 0, 1);
	if (!port) {
	    CloseHandle(job);
	    job = NULL;
	}
    }
    if (job) {
	ZeroMemory(&cport, sizeof(JOBOBJECT_ASSOCIATE_COMPLETION_PORT));
	cport.CompletionKey = job; /* use job handle as key */
	cport.CompletionPort = port;
	ret = SetInformationJobObject(
	    job,
	    JobObjectAssociateCompletionPortInformation,
	    &cport,
	    sizeof(JOBOBJECT_ASSOCIATE_COMPLETION_PORT));
	if (!ret) {
	    CloseHandle(job);
	    CloseHandle(port);
	    job = NULL;
	}
    }

    flags = 0;
    if (job)
	flags |= CREATE_SUSPENDED; /* assign to job before it runs */
    if (newconsole && (visible == 1))
	flags |= CREATE_NEW_CONSOLE;
    else if (newconsole && !consignals)
	/* prevent interruption of background processes by Ctrl-C, PR#17764 */
	flags |= CREATE_NEW_PROCESS_GROUP;
    if (job && breakaway)
	flags |= CREATE_BREAKAWAY_FROM_JOB;

    if(enc == CE_UTF8) {
	int n = strlen(ecmd); /* max no of chars */
	wchar_t wcmd[n+1];
	Rf_utf8towcs(wcmd, ecmd, n+1);
	ret = CreateProcessW(NULL, wcmd, &sa, &sa, TRUE, flags,
			     NULL, NULL, &wsi, &(pi->pi));
    } else
	ret = CreateProcess(NULL, ecmd, &sa, &sa, TRUE, flags,
			    NULL, NULL, &si, &(pi->pi));

    if (ret && job) {
	/* process was created as suspended */
	if (!AssignProcessToJobObject(job, pi->pi.hProcess)) {
	    /* will fail running on Windows without support for nested jobs,
	       when running in a job that does not allow breakaway */
	    CloseHandle(job);
	    CloseHandle(port);
	    job = NULL;
	}
	ResumeThread(pi->pi.hThread);
    }

    if (ret && job) {
	/* process is running in new job */
	pi->job = job;
	pi->port = port;
    } else {
	if (job) {
	    CloseHandle(job);
	    CloseHandle(port);
	    job = NULL;
	}
	pi->job = NULL;
	pi->port = NULL;
    }

    if (inpipe) {
	CloseHandle(dupIN);
	CloseHandle(dupOUT);
	CloseHandle(dupERR);
    }
    if (!ret)
	snprintf(RunError, 500, _("'CreateProcess' failed to run '%s'"), ecmd);
    else CloseHandle(pi->pi.hThread);
    free(ecmd);
    return;
}

/* used in rpipeOpen */
static DWORD CALLBACK
threadedwait(LPVOID param)
{
    rpipe *p = (rpipe *) param;

    if (p->timeoutMillis) {
	DWORD wres = WaitForSingleObject(p->pi.pi.hProcess, p->timeoutMillis);
	if (wres == WAIT_TIMEOUT) {
	    TerminateProcess(p->pi.pi.hProcess, 124);
	    p->timedout = 1;
	    /* wait up to 10s for the  process to actually terminate */
	    WaitForSingleObject(p->pi.pi.hProcess, 10000);
	}
    } else 
	WaitForSingleObject(p->pi.pi.hProcess, INFINITE);

    DWORD ret;
    GetExitCodeProcess(p->pi.pi.hProcess, &ret);
    p->exitcode = ret;
    
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

static void waitForJob(pinfo *pi, DWORD timeoutMillis, int* timedout)
{
    DWORD code, ret;
    ULONG_PTR key;
    DWORD beforeMillis;
    JOBOBJECT_BASIC_ACCOUNTING_INFORMATION jbai;
    LPOVERLAPPED overlapped; /* not used */
    DWORD queryMillis;

    if (timeoutMillis)
	beforeMillis = timeGetTime();

    queryMillis = 0;
    for(;;) {
	ret = GetQueuedCompletionStatus(pi->port, &code, &key,
					&overlapped, queryMillis);
	if (ret && code == JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO &&
	    (HANDLE)key == pi->job)
	    break;

	/* start with short query timeouts because notifications often get lost,
	   this is essentially polling */

	if (queryMillis == 0)
	    queryMillis = 1;
	else if (queryMillis < 100)
	    queryMillis *= 2;

	if (timeoutMillis && (timeGetTime() - beforeMillis >= timeoutMillis)) {
	    if (timedout)
		*timedout = 1;
	    break;
	}

	/* Check also explicitly because notifications are documented to get
	   lost and they often do. */
	ZeroMemory(&jbai, sizeof(JOBOBJECT_BASIC_ACCOUNTING_INFORMATION));
	ret = QueryInformationJobObject(
		pi->job,
		JobObjectBasicAccountingInformation,
		&jbai,
		sizeof(JOBOBJECT_BASIC_ACCOUNTING_INFORMATION),
		NULL);
	if (ret && jbai.ActiveProcesses == 0)
	    break;
    }
    CloseHandle(pi->port);
    CloseHandle(pi->job);
}

static void terminate_process(void *p)
{
    pinfo *pi = (pinfo*) p;
    EnumWindows((WNDENUMPROC)TerminateWindow, (LPARAM)pi->pi.dwProcessId);

    if (WaitForSingleObject(pi->pi.hProcess, 5000) == WAIT_TIMEOUT) {
	if (R_Interactive)
	    GA_askok(_("Child process not responding.  R will terminate it."));
	TerminateProcess(pi->pi.hProcess, 99);
    }

    if (pi->job) {
	TerminateJobObject(pi->job, 99);
	waitForJob(pi, 2000, NULL);
    }
}

static int pwait2(pinfo *pi, DWORD timeoutMillis, int* timedout)
{
    DWORD ret;

    if (!timeoutMillis) {
	while( WaitForSingleObject(pi->pi.hProcess, 100) == WAIT_TIMEOUT )
	    R_CheckUserInterrupt();
    } else {
	DWORD beforeMillis = timeGetTime();
	while( WaitForSingleObject(pi->pi.hProcess, 100) == WAIT_TIMEOUT ) {
	    R_CheckUserInterrupt();
	    DWORD afterMillis = timeGetTime();
	    if (afterMillis - beforeMillis >= timeoutMillis) {
		TerminateProcess(pi->pi.hProcess, 124);
		if (timedout)
		    *timedout = 1;
		/* wait up to 10s for the process to actually terminate */
		WaitForSingleObject(pi->pi.hProcess, 10000);
		if (pi->job)
		    TerminateJobObject(pi->job, 124);
		break;
	    }
	}
    }

    GetExitCodeProcess(pi->pi.hProcess, &ret);

    if (pi->job)
	waitForJob(pi, timeoutMillis, timedout);

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
    return runcmd_timeout(cmd, enc, wait, visible, fin, fout, ferr, 0, NULL, 1);
}

int runcmd_timeout(const char *cmd, cetype_t enc, int wait, int visible,
                   const char *fin, const char *fout, const char *ferr,
                   int timeout, int *timedout, int consignals)
{
    if (!wait && timeout)
	error("Timeout with background running processes is not supported.");
    
    HANDLE hIN = getInputHandle(fin), hOUT, hERR;
    int ret = 0;
    pinfo pi;
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


    memset(&(pi.pi), 0, sizeof(PROCESS_INFORMATION));
    pcreate(cmd, enc, !wait, visible, hIN, hOUT, hERR, &pi, consignals);
    if (pi.pi.hProcess) {
	if (wait) {
	    RCNTXT cntxt;
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		     R_NilValue, R_NilValue);
	    cntxt.cend = &terminate_process;
	    cntxt.cenddata = &pi;
	    DWORD timeoutMillis = (DWORD) (1000*timeout);
	    ret = pwait2(&pi, timeoutMillis, timedout);
	    endcontext(&cntxt);
	    snprintf(RunError, 501, _("Exit code was %d"), ret);
	    ret &= 0xffff;
	} else ret = 0;
	CloseHandle(pi.pi.hProcess);
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
		  const char *fout, const char *ferr,
		  int timeout, int newconsole)
{
    rpipe *r;
    HANDLE hTHIS, hIN, hOUT, hERR, hReadPipe, hWritePipe;
    DWORD id;
    BOOL res;
    int close1 = 0, close2 = 0, close3 = 0;
    /* newconsole (~"!wait") means ignore Ctrl handler attribute
       is set for child. When also visible==1, an actual text
       console is created. */

    if (!(r = (rpipe *) malloc(sizeof(struct structRPIPE)))) {
	strcpy(RunError, _("Insufficient memory (rpipeOpen)"));
	return NULL;
    }
    r->active = 0;
    r->pi.pi.hProcess = NULL;
    r->pi.job = NULL;
    r->thread = NULL;
    r->timedout = 0;
    r->timeoutMillis = (DWORD) (1000*timeout);
    res = CreatePipe(&hReadPipe, &hWritePipe, NULL, 0);
    if (res == FALSE) {
	rpipeClose(r, NULL);
	strcpy(RunError, "CreatePipe failed");
	return NULL;
    }
    hTHIS = GetCurrentProcess();
    if (io == 1) { /* pipe for R to write to */
	r->read = hReadPipe;
	DuplicateHandle(hTHIS, hWritePipe, hTHIS, &r->write,
			0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(hWritePipe);
    } else { /* pipe for R to read from */
	r->write = hWritePipe;
	DuplicateHandle(hTHIS, hReadPipe, hTHIS, &r->read,
			0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(hReadPipe);
    }
    CloseHandle(hTHIS);

    if (io == 1)
	hIN = r->read;
    else {
	hIN = getInputHandle(finput); /* a file or (usually NUL:) */
	if (hIN && finput && finput[0]) close1 = 1;
    }    
    if (io == 0 || io == 3) 
	hOUT = r->write;
    else {
 	hOUT = getOutputHandle(fout, 0);
	if (hOUT && fout && fout[0]) close2 = 1;
    }
    if (io >= 2) 
	hERR = r->write;
    else {
	hERR = getOutputHandle(ferr, 1);
	if (hERR && ferr && ferr[0]) close3 = 1;
    }
    
    pcreate(cmd, enc, newconsole, visible, hIN, hOUT, hERR, &(r->pi), 0);

    if (close1) CloseHandle(hIN);
    if (close2) CloseHandle(hOUT);
    if (close3) CloseHandle(hERR);

    r->active = 1;
    if (!r->pi.pi.hProcess)
	return NULL;
    if (io != 1) {
	/* FIXME: if still needed, should it be used also for io == 1? */
	if (!(r->thread = CreateThread(NULL, 0, threadedwait, r, 0, &id))) {
	    rpipeClose(r, NULL);
	    strcpy(RunError, "CreateThread failed");
	    return NULL;
	}
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
	    /* FIXME: close handles */
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

int rpipeClose(rpipe *r, int *timedout)
{
    int   i;

    if (!r) return NOLAUNCH;
    /* Close both pipe ends before forcibly terminating the child process to
       let it read all data (if it is reading) and exit gracefully.

       r->write and r->read are set to hNULL for the case that threadedwait
       ends up flushing file buffers

       FIXME: should we be forcing the termination at all? */
    HANDLE hNULL = CreateFile("NUL:", GENERIC_READ | GENERIC_WRITE, 0,
                              NULL, OPEN_EXISTING, 0, NULL);
    HANDLE tmp;
    tmp = r->read;
    r->read = hNULL;
    CloseHandle(tmp);
    tmp = r->write;
    r->write = hNULL;
    CloseHandle(tmp);

    rpipeTerminate(r);
    /* threadedwait may have obtained the exit code of the pipe process,
       but also may have been terminated too early; retrieve the exit
       code again to avoid race condition */
    DWORD ret;
    GetExitCodeProcess(r->pi.pi.hProcess, &ret);
    r->exitcode = ret;
    CloseHandle(r->pi.pi.hProcess);
    CloseHandle(hNULL);
    i = r->exitcode;
    if (timedout)
	*timedout = r->timedout;
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
    int visible = -1, io, mlen, newconsole;
    const char *fin = NULL, *fout = NULL, *ferr = NULL;

    io = con->mode[0] == 'w'; /* 1 for write, 0 for read */
    if (CharacterMode == RTerm) {
	fin = fout = ferr = "";
	newconsole = 1; /* ensures the child process runs in a new
	                   process group (ignores Ctrl handler),
	                   PR#17764 */
    } else if(io) {
	/* FIXME: this opens the extra console in Rgui, but it does not
	   get the output */
	visible = 1; /* Somewhere to put the output */
	newconsole = 1;
    } else
	newconsole = 0;
    rp = rpipeOpen(con->description, con->enc, visible, fin, io, fout, ferr, 0,
                   newconsole);
    if(!rp) {
	warning("cannot open cmd `%s'", con->description);
	return FALSE;
    }
    ((RWpipeconn)(con->private))->rp = rp;
    con->isopen = TRUE;
    con->canwrite = io;
    con->canread = !con->canwrite;
    mlen = (int) strlen(con->mode);
    if(mlen >= 2 && con->mode[mlen-1] == 'b') con->text = FALSE;
    else con->text = TRUE;
    con->save = -1000;
    return TRUE;
}

static void Wpipe_close(Rconnection con)
{
    con->status = rpipeClose( ((RWpipeconn)con->private) ->rp, NULL);
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
    GetExitCodeProcess(rp->pi.pi.hProcess, &ret);
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

    res = Rvsnprintf_mbcs(b, BUFSIZE, format, ap);
    if(res < 0 || res >= BUFSIZE) {
	warning(_("printing of extremely long output is truncated"));
	res = strlen(b);
    }
    return Wpipe_write(buf, (size_t)1, (size_t)res, con);
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
    const void *vmax = NULL;

    vmax = vmaxget();
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
	    const char *this = translateChar(STRING_ELT(nm, i));
	    char *that = expandcmd(this, 1);
	    SET_STRING_ELT(ans, i, mkChar(that ? that : ""));
	    free(that);
	}
    }
    setAttrib(ans, R_NamesSymbol, nm);
    vmaxset(vmax);
    UNPROTECT(1);
    return ans;
}
