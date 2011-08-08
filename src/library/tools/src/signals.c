/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Development Core Team.
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

#include <R.h>
#include "tools.h"
#include <signal.h> // C99

#ifdef WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

SEXP ps_kill(SEXP spid, SEXP ssignal)
{
    SEXP sspid, sres;
    int *pid, *res, signal = asInteger(ssignal);
    PROTECT(sspid = coerceVector(spid, INTSXP));
    unsigned int ns = LENGTH(spid);
    PROTECT(sres = allocVector(LGLSXP, ns));
    pid = INTEGER(sspid);
    res = INTEGER(sres);
    if(signal != NA_INTEGER) {
	for (int i = 0; i < ns; i++) {
#ifdef WIN32
	    HANDLE hProc = OpenProcess(PROCESS_TERMINATE, FALSE, pid[i]);
	    if (hProc) {
                TerminateProcess(hProc, 1);
                CloseHandle(hProc);
	    }
#else
	    if (pid[i] != NA_INTEGER) res[i] = kill(pid[i], signal);
#endif
	}
    }
    UNPROTECT(2);
    return sres;
}

#ifndef WIN32
/* on MacOS X it seems sys/resource.h needed sys/time.h first */
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
SEXP ps_priority(SEXP spid, SEXP svalue)
{
    SEXP sspid, sres;
    int *pid, *res, val;
    val = asInteger(svalue);
    PROTECT(sspid = coerceVector(spid, INTSXP));
    unsigned int ns = LENGTH(spid);
    PROTECT(sres = allocVector(INTSXP, ns));
    pid = INTEGER(sspid);
    res = INTEGER(sres);
    for (int i = 0; i < ns; i++) {
	if (pid[i] != NA_INTEGER) {
	    /* return value -1 is both an error value 
	       and a legitimate niceness */
	    errno = 0;
	    res[i] = getpriority(PRIO_PROCESS, pid[i]);
	    if(errno) res[i] = NA_INTEGER;
	    if(val != NA_INTEGER) setpriority(PRIO_PROCESS, pid[i], val);
	} else res[i] = NA_INTEGER;
    }
    UNPROTECT(2);
    return sres;
}
#else
SEXP ps_setpriority(SEXP spid, SEXP svalue)
{
    error(_("setting process priorities is not supported on Windows"));
}
#endif

SEXP ps_sigs(SEXP signo)
{
    int res = NA_INTEGER;
    switch(asInteger(signo)) {
	/* only SIGINT and SIGTERM are in C99 */
#ifdef SIGHUP
    case 1: res = SIGHUP; break;
#endif
#ifdef SIGINT
    case 2: res = SIGINT; break;
#endif
#ifdef SIGQUIT
    case 3: res = SIGQUIT; break;
#endif
#ifdef SIGKILL
    case 9: res = SIGKILL; break;
#endif
#ifdef SIGTERM
    case 15: res = SIGTERM; break;
#endif
#ifdef SIGSTOP
    case 17: res = SIGSTOP; break;
#endif
#ifdef SIGTSTP
    case 18: res = SIGTSTP; break;
#endif
#ifdef SIGCONT
    case 19: res = SIGCONT; break;
#endif
#ifdef SIGCHLD
    case 20: res = SIGCHLD; break;
#endif
#ifdef SIGUSR1
    case 30: res = SIGUSR1; break;
#endif
#ifdef SIGUSR2
    case 31: res = SIGUSR2; break;
#endif
    default: break;
    }
    return ScalarInteger(res);
}
