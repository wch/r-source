/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka
 *                            and the R Development Core Team
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

         /* See ../unix/system.txt for a description of functions */

        /* Windows analogue of unix/sys-unix.c: often rather similar */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Fileio.h"
#include "Startup.h"

extern Rboolean LoadInitFile;
extern UImode  CharacterMode;

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

FILE *R_OpenInitFile(void)
{
    char  buf[256];
    FILE *fp;

    fp = NULL;
    if (LoadInitFile) {
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	sprintf(buf, "%s/.Rprofile", getenv("R_USER"));
	if ((fp = R_fopen(buf, "r")))
	    return fp;
    }
    return fp;
}
/*
 *  5) FILESYSTEM INTERACTION
 */


static int HaveHOME=-1;
static char UserHOME[PATH_MAX];
static char newFileName[PATH_MAX];
char *R_ExpandFileName(char *s)
{
    char *p;

    if(s[0] != '~') return s;
    if(isalpha(s[1])) return s;
    if(HaveHOME < 0) {
	HaveHOME = 0;
 	p = getenv("HOME");
	if(p && strlen(p) && strlen(p) < PATH_MAX) {
	    strcpy(UserHOME, p);
	    HaveHOME = 1;
	} else {
	    p = getenv("HOMEDRIVE");
	    if(p && strlen(p) < PATH_MAX) {
		strcpy(UserHOME, p);
		p = getenv("HOMEPATH");
		if(p && strlen(UserHOME) + strlen(p) < PATH_MAX) {
		    strcat(UserHOME, p);
		    HaveHOME = 1;
		}
	    }
	}
    }
    if(HaveHOME > 0 && strlen(UserHOME) + strlen(s+1) < PATH_MAX) {
	strcpy(newFileName, UserHOME);
	strcat(newFileName, s+1);
	return newFileName;
    } else return s;
}

/*
 *  7) PLATFORM DEPENDENT FUNCTIONS
 */

SEXP do_machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return mkString("Win32");
}

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>

#ifdef _R_HAVE_TIMING_

static DWORD StartTime;

static FILETIME Create, Exit, Kernel, User;

void R_setStartTime(void)
{
    StartTime = GetTickCount();
}

/*
typedef struct _FILETIME {
    DWORD dwLowDateTime; 
    DWORD dwHighDateTime; 
} FILETIME; 
*/
 
void R_getProcTime(double *data)
{
    long  elapsed;
    double kernel, user;
    OSVERSIONINFO verinfo;
    /* This is in msec, but to clock-tick accuracy,
       said to be 10ms on NT and 55ms on Win95 */
    elapsed = (GetTickCount() - StartTime) / 10;

    verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
    GetVersionEx(&verinfo);
    switch(verinfo.dwPlatformId) {
    case VER_PLATFORM_WIN32_NT:
	/* These are in units of 100ns, but with an accuracy only
	   in clock ticks.  So we round to 0.01s */
	GetProcessTimes(GetCurrentProcess(), &Create, &Exit, &Kernel, &User);
	user = 1e-5 * ((double) User.dwLowDateTime + 
		       (double) User.dwHighDateTime * 4294967296.0);
	user = floor(user)/100.0;
	kernel = 1e-5 * ((double) Kernel.dwLowDateTime + 
			 (double) Kernel.dwHighDateTime * 4294967296.0);
	kernel = floor(kernel)/100.0;
	break;
    default:
	user = R_NaReal;
	kernel = R_NaReal;
    }
    data[0] = user;
    data[1] = kernel;
    data[2] = (double) elapsed / 100.0;
    data[3] = R_NaReal;
    data[4] = R_NaReal;
}

double R_getClockIncrement(void)
{
  return 1.0 / 100.0;
}
#endif /* _R_HAVE_TIMING_ */

/*
 * flag =0 don't wait/ignore stdout
 * flag =1 wait/ignore stdout
 * flag =2 wait/copy stdout to the console
 * flag =3 wait/return stdout
 * Add 10 to minimize application
 * Add 20 to make application "invisible"
*/

#include "run.h"

#define INTERN_BUFSIZE 8096
SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    rpipe *fp;
    char  buf[INTERN_BUFSIZE];
    int   vis = 0, flag = 2, i = 0, j, ll, ignore_stderr = 0;
    SEXP  tlist = R_NilValue, tchar, rval;
    HANDLE hERR;

    checkArity(op, args);
    if (!isString(CAR(args)))
	errorcall(call, _("character string expected as first argument"));
    if (isInteger(CADR(args)))
	flag = INTEGER(CADR(args))[0];
    if (flag >= 100) {
	ignore_stderr = 1;
	flag -= 100;
    }
    if (flag >= 20) {
	vis = -1;
	flag -= 20;
    } else if (flag >= 10) {
	vis = 0;
	flag -= 10;
    } else
	vis = 1;
    if (!isString(CADDR(args)))
	errorcall(call, _("character string expected as third argument"));
    if ((CharacterMode != RGui) && (flag == 2)) flag = 1;
    if (CharacterMode == RGui) {
	SetStdHandle(STD_INPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_OUTPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_ERROR_HANDLE, INVALID_HANDLE_VALUE);
    }
    if ((CharacterMode != RGui) && ignore_stderr) {
	hERR = GetStdHandle(STD_ERROR_HANDLE) ;
	SetStdHandle(STD_ERROR_HANDLE, INVALID_HANDLE_VALUE);
    }
    if (flag < 2) {
	ll = runcmd(CHAR(STRING_ELT(CAR(args), 0)), flag, vis,
		    CHAR(STRING_ELT(CADDR(args), 0)));
	if (ll == NOLAUNCH)
	    warning(runerror());
    } else {
	int m = 0;
	if(flag == 2 /* show on console */ || CharacterMode == RGui) m = 2;
	if(ignore_stderr) m = 0;
	fp = rpipeOpen(CHAR(STRING_ELT(CAR(args), 0)), vis,
		       CHAR(STRING_ELT(CADDR(args), 0)), m);
	if (!fp) {
	    /* If we are capturing standard output generate an error */
	    if (flag == 3)
		error(runerror());
	    warning(runerror());
	    ll = NOLAUNCH;
	} else {
	    if (flag == 3)
		PROTECT(tlist);
	    for (i = 0; rpipeGets(fp, buf, INTERN_BUFSIZE); i++) {
		if (flag == 3) {
		    ll = strlen(buf) - 1;
		    if ((ll >= 0) && (buf[ll] == '\n'))
			buf[ll] = '\0';
		    tchar = mkChar(buf);
		    UNPROTECT(1);
		    PROTECT(tlist = CONS(tchar, tlist));
		} else
		    R_WriteConsole(buf, strlen(buf));
	    }
	    ll = rpipeClose(fp);
	}
    }
    if ((CharacterMode != RGui) && ignore_stderr)
	SetStdHandle(STD_ERROR_HANDLE, hERR);
    if (flag == 3) {
	rval = allocVector(STRSXP, i);;
	for (j = (i - 1); j >= 0; j--) {
	    SET_STRING_ELT(rval, j, CAR(tlist));
	    tlist = CDR(tlist);
	}
	UNPROTECT(1);
	return (rval);
    } else {
	tlist = allocVector(INTSXP, 1);
	INTEGER(tlist)[0] = ll;
	R_Visible = 0;
	return tlist;
    }
}
