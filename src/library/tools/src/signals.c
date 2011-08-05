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
