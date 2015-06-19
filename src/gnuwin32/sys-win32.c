/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
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

	 /* See ../unix/system.txt for a description of functions */

	/* Windows analogue of unix/sys-unix.c: often rather similar */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Fileio.h>
#include <Startup.h>

#include <ctype.h> /* for isalpha */

extern Rboolean LoadInitFile;
extern UImode  CharacterMode;

/*
 *  4) INITIALIZATION AND TERMINATION ACTIONS
 */

FILE *R_OpenInitFile(void)
{
    char  buf[PATH_MAX], *p = getenv("R_PROFILE_USER");
    FILE *fp;

    fp = NULL;
    if (LoadInitFile) {
	if(p) {
	    if(!*p) return NULL;  /* set to "" */
	    return R_fopen(R_ExpandFileName(p), "r");
	}
	if ((fp = R_fopen(".Rprofile", "r")))
	    return fp;
	snprintf(buf, PATH_MAX, "%s/.Rprofile", getenv("R_USER"));
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
const char *R_ExpandFileName(const char *s)
{
    char *p;

    if(s[0] != '~') return s;
    if(isalpha(s[1])) return s;
    if(HaveHOME < 0) {
	HaveHOME = 0;
	p = getenv("R_USER"); /* should be set so the rest is a safety measure */
	if(p && strlen(p) && strlen(p) < PATH_MAX) {
	    strcpy(UserHOME, p);
	    HaveHOME = 1;
	} else {
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

static DWORD StartTime;

static FILETIME Create, Exit, Kernel, User;

void R_setStartTime(void)
{
    StartTime = GetTickCount();
}

void R_getProcTime(double *data)
{
    DWORD elapsed;
    double kernel, user;

    /* This is in msec, but to clock-tick accuracy,
       said to be 10ms on NT and 55ms on Win95 */
    elapsed = (GetTickCount() - StartTime) / 10;

    /* These are in units of 100ns, but with an accuracy only
       in clock ticks.  So we round to 0.01s */
    GetProcessTimes(GetCurrentProcess(), &Create, &Exit, &Kernel, &User);
    user = 1e-5 * ((double) User.dwLowDateTime +
		   (double) User.dwHighDateTime * 4294967296.0);
    user = floor(user)/100.0;
    kernel = 1e-5 * ((double) Kernel.dwLowDateTime +
		     (double) Kernel.dwHighDateTime * 4294967296.0);
    kernel = floor(kernel)/100.0;
    data[0] = user;
    data[1] = kernel;
    data[2] = (double) elapsed / 100.0;
    data[3] = R_NaReal;
    data[4] = R_NaReal;
}

/* use in memory.c: increments for CPU times */
double R_getClockIncrement(void)
{
    return 1.0 / 100.0;
}

/*
 * flag =0 don't wait/ignore stdout
 * flag =1 wait/ignore stdout
 * flag =2 wait/copy stdout to the console
 * flag =3 wait/return stdout (intern=TRUE)
 * Add 10 to minimize application
 * Add 20 to make application "invisible"
*/

#include "run.h"

#define INTERN_BUFSIZE 8096
SEXP do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    rpipe *fp;
    char  buf[INTERN_BUFSIZE];
    const char *fout = "", *ferr = "";
    int   vis = 0, flag = 2, i = 0, j, ll = 0;
    SEXP  cmd, fin, Stdout, Stderr, tlist = R_NilValue, tchar, rval;

    checkArity(op, args);
    cmd = CAR(args);
    if (!isString(cmd) || LENGTH(cmd) != 1)
	errorcall(call, _("character string expected as first argument"));
    args = CDR(args);
    flag = asInteger(CAR(args)); args = CDR(args);
    if (flag >= 20) {vis = -1; flag -= 20;}
    else if (flag >= 10) {vis = 0; flag -= 10;}
    else vis = 1;

    fin = CAR(args);
    if (!isString(fin))
	errorcall(call, _("character string expected as third argument"));
    args = CDR(args);
    Stdout = CAR(args);
    args = CDR(args);
    Stderr = CAR(args);
    
    if (CharacterMode == RGui) {
	/* This is a rather conservative approach: if
	   Rgui is launched from a console window it does have
	   standard handles -- but users might well not expect that.
	*/
	SetStdHandle(STD_INPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_OUTPUT_HANDLE, INVALID_HANDLE_VALUE);
	SetStdHandle(STD_ERROR_HANDLE, INVALID_HANDLE_VALUE);
	if (TYPEOF(Stdout) == STRSXP) fout = CHAR(STRING_ELT(Stdout, 0));
	if (TYPEOF(Stderr) == STRSXP) ferr = CHAR(STRING_ELT(Stderr, 0));
    } else {
	if (flag == 2) flag = 1; /* ignore std.output.on.console */
	if (TYPEOF(Stdout) == STRSXP) fout = CHAR(STRING_ELT(Stdout, 0));
	else if (asLogical(Stdout) == 0) fout = NULL;
	if (TYPEOF(Stderr) == STRSXP) ferr = CHAR(STRING_ELT(Stderr, 0));
	else if (asLogical(Stderr) == 0) ferr = NULL;
    }

    if (flag < 2) { /* Neither intern = TRUE nor
		       show.output.on.console for Rgui */
	ll = runcmd(CHAR(STRING_ELT(cmd, 0)),
		    getCharCE(STRING_ELT(cmd, 0)),
		    flag, vis, CHAR(STRING_ELT(fin, 0)), fout, ferr);
    } else {
	/* read stdout +/- stderr from pipe */
	int m = 0;
	if(flag == 2 /* show on console */ || CharacterMode == RGui) m = 3;
	if(TYPEOF(Stderr) == LGLSXP)
	    m = asLogical(Stderr) ? 2 : 0;
	if(m  && TYPEOF(Stdout) == LGLSXP && asLogical(Stdout)) m = 3;
	fp = rpipeOpen(CHAR(STRING_ELT(cmd, 0)), getCharCE(STRING_ELT(cmd, 0)),
		       vis, CHAR(STRING_ELT(fin, 0)), m, fout, ferr);
	if (!fp) {
	    /* If intern = TRUE generate an error */
	    if (flag == 3) error(runerror());
	    ll = NOLAUNCH;
	} else {
	    /* FIXME: use REPROTECT */
	    if (flag == 3) {
		PROTECT(tlist);
		/* honour intern = FALSE, ignore.stdout = TRUE */
		if (m > 0 ||
		    (!(TYPEOF(Stdout) == LGLSXP && !asLogical(Stdout))))
		    for (i = 0; rpipeGets(fp, buf, INTERN_BUFSIZE); i++) {
			ll = strlen(buf) - 1;
			if ((ll >= 0) && (buf[ll] == '\n')) buf[ll] = '\0';
			tchar = mkChar(buf);
			UNPROTECT(1); /* tlist */
			PROTECT(tlist = CONS(tchar, tlist));
		    }

	    } else {
		for (i = 0; rpipeGets(fp, buf, INTERN_BUFSIZE); i++)
		    R_WriteConsole(buf, strlen(buf));
	    }
	    ll = rpipeClose(fp);
	}
    }
    if(ll) {
	warningcall(R_NilValue, 
		    _("running command '%s' had status %d"), 
		    CHAR(STRING_ELT(cmd, 0)), ll);
    }
    if (flag == 3) { /* intern = TRUE: convert pairlist to list */
	PROTECT(rval = allocVector(STRSXP, i));
	for (j = (i - 1); j >= 0; j--) {
	    SET_STRING_ELT(rval, j, CAR(tlist));
	    tlist = CDR(tlist);
	}
	if(ll) {
	    SEXP lsym = install("status");
	    setAttrib(rval, lsym, ScalarInteger(ll));
	}
	UNPROTECT(2);
	return rval;
    } else {
	rval = ScalarInteger(ll);
	R_Visible = 0;
	return rval;
    }
}
