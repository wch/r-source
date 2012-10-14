/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2011  The R Core Team
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

/* <UTF8>
   char here is mainly handled as a whole string.
   Does handle file names.
   Chopping final \n is OK in UTF-8.
 */


/* See system.txt for a description of functions */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

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

#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_GETRUSAGE)
/* on MacOS X it seems sys/resource.h needs sys/time.h first */
# include <sys/time.h>
# include <sys/resource.h>
#endif

#include <errno.h>

extern Rboolean LoadInitFile;

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

char *R_ExpandFileName_readline(const char *s, char *buff);  /* sys-std.c */

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
#ifdef HAVE_LIBREADLINE
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
    return mkString("Unix");
}

# ifdef HAVE_SYS_TIMES_H
#  include <sys/times.h> /* times */
# endif

static double clk_tck, StartTime;

extern double currentTime(void); /* from datetime.c */

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

#define INTERN_BUFSIZE 8096
SEXP attribute_hidden do_system(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP tlist = R_NilValue;
    int intern = 0;

    checkArity(op, args);
    if (!isValidStringF(CAR(args)))
	error(_("non-empty character argument expected"));
    intern = asLogical(CADR(args));
    if (intern == NA_INTEGER)
	error(_("'intern' must be logical and not NA"));
    if (intern) { /* intern = TRUE */
	FILE *fp;
	char *x = "r", buf[INTERN_BUFSIZE];
	const char *cmd;
	int i, j, res;
	SEXP tchar, rval;

	PROTECT(tlist);
	cmd = translateChar(STRING_ELT(CAR(args), 0));
	errno = 0; /* precaution */
	if(!(fp = R_popen(cmd, x)))
	    error(_("cannot popen '%s', probable reason '%s'"),
		  cmd, strerror(errno));
	for (i = 0; fgets(buf, INTERN_BUFSIZE, fp); i++) {
	    size_t read = strlen(buf);
	    if(read >= INTERN_BUFSIZE - 1)
		warning(_("line %d may be truncated in call to system(, intern = TRUE)"), i + 1);
	    if (read > 0 && buf[read-1] == '\n')
		buf[read - 1] = '\0'; /* chop final CR */
	    tchar = mkChar(buf);
	    UNPROTECT(1);
	    PROTECT(tlist = CONS(tchar, tlist));
	}
	res = pclose(fp);
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
	} else if (res) {
	    if (errno)
		warningcall(R_NilValue, 
			    _("running command '%s' had status %d and error message '%s'"), 
			    cmd, res, 
			    strerror(errno));
	    else 
		warningcall(R_NilValue, 
			    _("running command '%s' had status %d"), 
			    cmd, res);
	}
	
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
	INTEGER(tlist)[0] = R_system(translateChar(STRING_ELT(CAR(args), 0)));
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

#ifdef linux
# ifdef HAVE_FPU_CONTROL_H
#  include <fpu_control.h>
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

#ifdef NEED___SETFPUCW
    __setfpucw(_FPU_IEEE);
#endif
#if (defined(__i386) || defined(__x86_64)) && defined(__INTEL_COMPILER) && __INTEL_COMPILER > 800
    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_OFF);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_OFF);
#endif
    } else {
#ifdef __FreeBSD__
    fpsetmask(~0);
#endif

#ifdef NEED___SETFPUCW
    __setfpucw(_FPU_DEFAULT);
#endif
    }
}
