/*
 *  R : A Computer Language for Statistical Data Analysis
 *  file run.c: a simple 'reading' pipe (and a command executor)
 *  Copyright (C) 1999-2001  Guido Masarotto  and Brian Ripley
 *            (C) 2007       the R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>
#include "win-nls.h"

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "run.h"

static char RunError[256] = "";


static char * expandcmd(const char *cmd)
{
    char *buf;
    char  c;
    char *s, *p, *q, *f, *dest, *src, *fl, *fn;
    int   d , ext, len = strlen(cmd)+1;

    /* make a copy as we manipulate in place */
    buf = alloca(strlen(cmd) + 1);
    strcpy(buf, cmd);

    if (!(s = (char *) malloc(MAX_PATH + strlen(cmd)))) {
	strcpy(RunError, _("Insufficient memory (expandcmd)"));
	return NULL;
    }
    /* skip leading spaces */
    for (p = buf; *p && isspace(*p); p++);
    /* find the command itself, possibly double-quoted */
    for (q = p, d = 0; *q && ( d || !isspace(*q) ); q++)
	if (*q == '\"') d = d ? 0 : 1;
    if (d) {
	strcpy(RunError, _("A \" is missing (expandcmd)"));
	return NULL;
    }
    c = *q; /* character after the command, normally a space */
    *q = '\0';

/*
 * I resort to this since SearchPath returned FOUND also
 * for file name without extension -> explicitly set
 *  extension
 */
    for ( f = p, ext = 0 ; *f ; f++) {
	if ((*f == '\\') || (*f == '/')) ext = 0;
	else if (*f == '.') ext = 1;
    }
    /* SearchPath doesn't like ", so strip out quotes */
    fl = alloca(len);
    fn = alloca(len);
    for ( dest = fl , src = p; *src ; src++)
	if (*src != '\"') *dest++ = *src;
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
	char *exts[] = { ".exe" , ".com" , ".cmd" , ".bat" , NULL };
	while (exts[iexts]) {
	    strcpy(dest, exts[iexts]);
	    if ((d = SearchPath(NULL, fl, NULL, MAX_PATH, fn, &f))) break;
	    iexts++ ;
	}
    }
    if (!d) {
	free(s);
	strncpy(RunError, p, 200);
	strcat(RunError, _(" not found"));
	*q = c;
	return NULL;
    }
    /*
      Paranoia : on my system switching to short names is not needed
      since SearchPath already returns 'short names'. However,
      this is not documented so I prefer to be explicit.
      Problem is that we have removed \" from the executable since
      SearchPath seems dislikes them
    */
    GetShortPathName(fn, s, MAX_PATH);
    *q = c;
    strcat(s, q);
    return s;
}

/*
   finput is either NULL or the name of a file from which to
     redirect stdin for the child.
   newconsole != 0 to use a new console (if not waiting)
   visible = -1, 0, 1 for hide, minimized, default
   inpipe != 0 to duplicate I/O handles
*/

static HANDLE pcreate(const char* cmd, const char *finput,
		      int newconsole, int visible, int inpipe)
{
    DWORD ret;
    SECURITY_ATTRIBUTES sa;
    PROCESS_INFORMATION pi;
    STARTUPINFO si;
    HANDLE hIN = INVALID_HANDLE_VALUE,
	hSAVED = INVALID_HANDLE_VALUE, hTHIS;
    char *ecmd;

    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = NULL;
    sa.bInheritHandle = TRUE;

    if (!(ecmd = expandcmd(cmd)))
	return NULL;
    hTHIS = GetCurrentProcess();
    if (finput && finput[0]) {
	hSAVED = GetStdHandle(STD_INPUT_HANDLE) ;
	hIN = CreateFile(finput, GENERIC_READ, 0,
			 &sa, OPEN_EXISTING, 0, NULL);
	if (hIN == INVALID_HANDLE_VALUE) {
	    free(ecmd);
	    strcpy(RunError, _("Impossible to redirect input"));
	    return NULL;
	}
	SetStdHandle(STD_INPUT_HANDLE, hIN);
    }
    si.cb = sizeof(si);
    si.lpReserved = NULL;
    si.lpReserved2 = NULL;
    si.cbReserved2 = 0;
    si.lpDesktop = NULL;
    si.lpTitle = NULL;
    if ((finput && finput[0]) || inpipe) {
	si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
	DuplicateHandle(hTHIS, GetStdHandle(STD_INPUT_HANDLE),
		      hTHIS, &si.hStdInput, 0, TRUE, DUPLICATE_SAME_ACCESS);
	DuplicateHandle(hTHIS, GetStdHandle(STD_OUTPUT_HANDLE),
		     hTHIS, &si.hStdOutput, 0, TRUE, DUPLICATE_SAME_ACCESS);
	DuplicateHandle(hTHIS, GetStdHandle(STD_ERROR_HANDLE),
		      hTHIS, &si.hStdError, 0, TRUE, DUPLICATE_SAME_ACCESS);
    } else
	si.dwFlags = STARTF_USESHOWWINDOW;
    switch (visible) {
      case -1:
	si.wShowWindow = SW_HIDE;
	break;
      case 0:
	si.wShowWindow = SW_SHOWMINIMIZED;
	break;
      case 1:
	si.wShowWindow = SW_SHOWDEFAULT;
	break;
    }
    ret = CreateProcess(NULL, ecmd, &sa, &sa, TRUE,
		    (newconsole && (visible == 1)) ? CREATE_NEW_CONSOLE : 0,
			NULL, NULL, &si, &pi);
    CloseHandle(hTHIS);
    if (finput && finput[0]) {
	SetStdHandle(STD_INPUT_HANDLE, hSAVED);
	CloseHandle(hIN);
    }
    if (si.dwFlags & STARTF_USESTDHANDLES) {
	CloseHandle(si.hStdInput);
	CloseHandle(si.hStdOutput);
	CloseHandle(si.hStdError);
    }
    if (!ret) {
	strcpy(RunError, _("Impossible to run "));
	strncat(RunError, ecmd, 200);
	free(ecmd);
	return NULL;
    }
    free(ecmd);
    CloseHandle(pi.hThread);
    return pi.hProcess;
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

    p->exitcode = pwait(p->process);
    FlushFileBuffers(p->write);
    FlushFileBuffers(p->read);
    p->active = 0;
    return 0;
}

char *runerror()
{
    return RunError;
}


/*
   wait != 0 says wait for child to terminate before returning.
   visible = -1, 0, 1 for hide, minimized, default
   finput is either NULL or the name of a file from which to
     redirect stdin for the child.
 */
int runcmd(const char *cmd, int wait, int visible, const char *finput)
{
    HANDLE p;
    int ret;

/* I hope no program will use this as an error code */
    if (!(p = pcreate(cmd, finput, !wait, visible, 0))) return NOLAUNCH;
    if (wait) ret = pwait(p);
    else ret = 0;
    CloseHandle(p);
    return ret;
}

/*
   finput is either NULL or the name of a file from which to
     redirect stdin for the child.
   newconsole != 0 to use a new console (if not waiting)
   visible = -1, 0, 1 for hide, minimized, default
   io = 0 to read stdout from pipe, 1 to write to pipe,
   2 to read stdout and stderr from pipe.
 */
rpipe * rpipeOpen(const char *cmd, int visible, const char *finput, int io)
{
    rpipe *r;
    HANDLE hIN, hOUT, hERR, hThread, hTHIS, hTemp;
    DWORD id;
    BOOL res;

    if (!(r = (rpipe *) malloc(sizeof(struct structRPIPE)))) {
	strcpy(RunError, _("Insufficient memory (rpipeOpen)"));
	return NULL;
    }
    r->process = NULL;
    if(io == 1) { /* pipe to write to */
	res = CreatePipe(&(r->read), &hTemp, NULL, 0);
	if (res == FALSE) {
	    rpipeClose(r);
	    strcpy(RunError, _("Impossible to create pipe"));
	    return NULL;
	}
	hTHIS = GetCurrentProcess();
	hIN = GetStdHandle(STD_INPUT_HANDLE);
	DuplicateHandle(hTHIS, hTemp, hTHIS, &r->write,
			0, FALSE, DUPLICATE_SAME_ACCESS);
	CloseHandle(hTemp);
	CloseHandle(hTHIS);
	SetStdHandle(STD_INPUT_HANDLE, r->read);
	r->process = pcreate(cmd, NULL, 1, visible, 1);
	r->active = 1;
	SetStdHandle(STD_INPUT_HANDLE, hIN);
	if (!r->process) return NULL; else return r;
    }
    res = CreatePipe(&hTemp, &(r->write), NULL, 0);
    if (res == FALSE) {
	rpipeClose(r);
	strcpy(RunError, _("Impossible to create pipe"));
	return NULL;
    }
    hTHIS = GetCurrentProcess();
    hOUT = GetStdHandle(STD_OUTPUT_HANDLE) ;
    hERR = GetStdHandle(STD_ERROR_HANDLE) ;
    DuplicateHandle(hTHIS, hTemp, hTHIS, &r->read,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    CloseHandle(hTemp);
    CloseHandle(hTHIS);
    SetStdHandle(STD_OUTPUT_HANDLE, r->write);
    if(io > 0) SetStdHandle(STD_ERROR_HANDLE, r->write);
    r->process = pcreate(cmd, finput, 0, visible, 1);
    r->active = 1;
    SetStdHandle(STD_OUTPUT_HANDLE, hOUT);
    if(io > 0) SetStdHandle(STD_ERROR_HANDLE, hERR);
    if (!r->process)
	return NULL;
    if (!(hThread = CreateThread(NULL, 0, threadedwait, r, 0, &id))) {
	rpipeClose(r);
	strcpy(RunError, _("Impossible to create thread/pipe"));
	return NULL;
    }
    CloseHandle(hThread);
    return r;
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
	    rpipeClose(r);
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
    if (r->active) TerminateProcess(r->process, 99);
    CloseHandle(r->read);
    CloseHandle(r->write);
    CloseHandle(r->process);
    i = r->exitcode;
    free(r);
    return i;
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
    rp = rpipeOpen(con->description, visible, NULL, io);
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
    rpipeClose( ((RWpipeconn)con->private) ->rp);
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
    GetExitCodeProcess(rp->process, &ret);
    if(ret != STILL_ACTIVE) {
	rp->active = 0;
	warning("broken Windows pipe");
	return 0;
    }
    if (WriteFile(rp->write, ptr, towrite, &write, NULL) != 0)
	return write/size;
    else return 0;
}

#define BUFSIZE 1000
static int Wpipe_vfprintf(Rconnection con, const char *format, va_list ap)
{
    char buf[BUFSIZE], *b = buf, *vmax = vmaxget();
    int res = 0, usedRalloc = FALSE;

    res = vsnprintf(b, BUFSIZE, format, ap);
    if(res < 0) { /* a failure indication, so try again */
	usedRalloc = TRUE;
	b = R_alloc(10*BUFSIZE, sizeof(char));
	res = vsnprintf(b, 10*BUFSIZE, format, ap);
	if (res < 0) {
	    *(b + 10*BUFSIZE) = '\0';
	    warning("printing of extremely long output is truncated");
	    res = 10*BUFSIZE;
	}
    }
    res = Wpipe_write(buf, res, 1, con);
    if(usedRalloc) vmaxset(vmax);
    return res;
}

Rconnection newWpipe(const char *description, const char *mode)
{
    Rconnection new;
    new = (Rconnection) malloc(sizeof(struct Rconn));
    if(!new) error(_("allocation of pipe connection failed"));
    new->class = (char *) malloc(strlen("pipe") + 1);
    if(!new->class) {
	free(new);
	error(_("allocation of pipe connection failed"));
    }
    strcpy(new->class, "pipe");
    new->description = (char *) malloc(strlen(description) + 1);
    if(!new->description) {
	free(new->class); free(new);
	error(_("allocation of pipe connection failed"));
    }
    init_con(new, description, mode);
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
