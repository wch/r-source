/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Core Team.
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

#include <R.h>
#include "tools.h"
#include <signal.h> // C99

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

SEXP ps_kill(SEXP spid, SEXP ssignal)
{
    SEXP sspid, sres;
    int *pid, *res, signal = asInteger(ssignal);
    PROTECT(sspid = coerceVector(spid, INTSXP));
    unsigned int ns = LENGTH(sspid);
    PROTECT(sres = allocVector(LGLSXP, ns));
    pid = INTEGER(sspid);
    res = INTEGER(sres);
#if !defined(_WIN32) && !defined(HAVE_KILL)
    warning(_("pskill() is not supported on this platform"));
#endif
    if(signal != NA_INTEGER) {
	for (int i = 0; i < ns; i++) {
#ifdef _WIN32
	    HANDLE hProc = OpenProcess(PROCESS_TERMINATE, FALSE, pid[i]);
	    if (hProc) {
                TerminateProcess(hProc, 1);
                CloseHandle(hProc);
	    }
#elif defined(HAVE_KILL)
	    if (pid[i] <= 0) continue;
	    if (pid[i] != NA_INTEGER) res[i] = kill(pid[i], signal);
#endif
	}
    }
    UNPROTECT(2);
    return sres;
}

#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_GETPRIORITY)
/* on OS X it seems sys/resource.h needed sys/time.h first at one time */
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>
SEXP ps_priority(SEXP spid, SEXP svalue)
{
    SEXP sspid, sres;
    int *pid, *res, val;
    val = asInteger(svalue);
    PROTECT(sspid = coerceVector(spid, INTSXP));
    unsigned int ns = LENGTH(sspid);
    PROTECT(sres = allocVector(INTSXP, ns));
    pid = INTEGER(sspid);
    res = INTEGER(sres);
    for (int i = 0; i < ns; i++) {
	if (pid[i] <= 0) {
	    res[i] = NA_INTEGER;
	    continue;
	}
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
#elif defined(_WIN32)
SEXP ps_priority(SEXP spid, SEXP svalue)
{
    SEXP sspid, sres;
    int *pid, *res, val;
    val = asInteger(svalue);
    PROTECT(sspid = coerceVector(spid, INTSXP));
    unsigned int ns = LENGTH(sspid);
    PROTECT(sres = allocVector(INTSXP, ns));
    pid = INTEGER(sspid);
    res = INTEGER(sres);
    for (int i = 0; i < ns; i++) {
	HANDLE hProc = OpenProcess(val != NA_INTEGER ?
				   PROCESS_SET_INFORMATION
				   : PROCESS_QUERY_INFORMATION, 
				   FALSE, pid[i]);
	if (hProc && pid[i] != NA_INTEGER) {
	    DWORD tmp = GetPriorityClass(hProc);
	    switch(tmp) {
	    case ABOVE_NORMAL_PRIORITY_CLASS: res[i] = -5; break;
	    case BELOW_NORMAL_PRIORITY_CLASS: res[i] = 15; break;
	    case HIGH_PRIORITY_CLASS: res[i] = -10; break;
	    case IDLE_PRIORITY_CLASS: res[i] = 19; break;
	    case NORMAL_PRIORITY_CLASS: res[i] = 0; break;
	    case REALTIME_PRIORITY_CLASS: res[i] = -20; break;
	    }
	    if(val != NA_INTEGER) {
		switch(val) {
		case 19: tmp = IDLE_PRIORITY_CLASS; break;
		case 15: tmp = BELOW_NORMAL_PRIORITY_CLASS; break;
		case 0: tmp = NORMAL_PRIORITY_CLASS; break;
		case -5: tmp = ABOVE_NORMAL_PRIORITY_CLASS; break;
		case -10: tmp = HIGH_PRIORITY_CLASS; break;
		}
		SetPriorityClass(hProc, tmp);
	    }
	    CloseHandle(hProc);
	} else res[i] = NA_INTEGER;
    }
    UNPROTECT(2);
    return sres;
}
#else
SEXP ps_priority(SEXP spid, SEXP svalue)
{
    error(_("psnice() is not supported on this platform"));
    return R_NilValue; /* -Wall */
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
