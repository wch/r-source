/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2007  The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 *
 *
 *      Interfaces to POSIX date and time functions.
 */

/* <UTF8> char here is either ASCII or handled as a whole */

/*
    These use POSIX functions that are not available on all platforms,
    and where they are they may be partially or incorrectly implemented.
    A number of lightweight alternatives are supplied, but generally
    timezone support is only available if the OS supplies it.

    A particular problem is the setting of the timezone TZ on Unix/Linux.
    POSIX appears to require it, yet many Linux systems do not set it
    and do not give the correct results/crash strftime if it is not set
    (or even if it is: see the workaround below).
    We use unsetenv() to work around this: that is a BSD construct but
    seems to be available on the affected platforms.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if defined(HAVE_GLIBC2)
#include <features.h>
# ifndef __USE_POSIX
#  define __USE_POSIX		/* for tzset */
# endif
# ifndef __USE_BSD
#  define __USE_BSD		/* so that we get unsetenv() */
# endif
# ifndef __USE_MISC
#  define __USE_MISC		/* for finite */
# endif
#endif

#include <time.h>
#include <stdlib.h> /* for setenv or putenv */
#include <Defn.h>

/* The glibc in RH8.0 is broken and assumes that dates before 1970-01-01
   do not exist. So does Windows, but at least there we do not need a
   run-time test.  As from 1.6.2, test the actual mktime code and cache the
   result on glibc >= 2.2. (It seems this started between 2.2.5 and 2.3,
   and RH8.0 has an unreleased version in that gap.)

   Sometime in late 2004 this was reverted in glibc.
*/

static Rboolean have_broken_mktime(void)
{
#if defined(Win32) || defined(_AIX)
    return TRUE;
#elif defined(__GLIBC__) && defined(__GLIBC_MINOR__) && __GLIBC__ >= 2 && __GLIBC_MINOR__ >= 2
    static int test_result = -1;

    if (test_result == -1) {
	struct tm t;
	time_t res;
	t.tm_sec = t.tm_min = t.tm_hour = 0;
	t.tm_mday = t.tm_mon = 1;
	t.tm_year = 68;
	t.tm_isdst = -1;
	res = mktime(&t);
	test_result = (res == (time_t)-1);
    }
    return test_result > 0;
#else
    return FALSE;
#endif


}

/* Substitute based on glibc code. */
#include "Rstrptime.h"

static const int days_in_month[12] =
{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define days_in_year(year) (isleap(year) ? 366 : 365)

#ifndef HAVE_POSIX_LEAPSECONDS
/* There have been 23 leapseconds, the last being on 2005-12-31.
   But older OSes will not necessarily know about number 23, so we do 
   a run-time test (the OS could have been patched since configure).
 */
static int n_leapseconds = -1;
static const time_t leapseconds[] =
{  78796800, 94694400,126230400,157766400,189302400,220924800,252460800,
  283996800,315532800,362793600,394329600,425865600,489024000,567993600,
  631152000,662688000,709948800,741484800,773020800,820454400,867715200,
  915148800,1136073600};

static void set_n_leapseconds(void)
{
    struct tm tm;
    int t1, t2;

    tm.tm_year = 105;
    tm.tm_mon = 11;
    tm.tm_mday = 31;
    tm.tm_hour = 12;
    tm.tm_min = 0;
    tm.tm_sec = 0;
    t1 = mktime(&tm);
    tm.tm_year = 106;
    tm.tm_mon = 0;
    tm.tm_mday = 1;
    t2 = mktime(&tm);
    n_leapseconds = t2 - t1 == 84601) ? 23 : 22;
}
#endif

/*
  Adjust a struct tm to be a valid date-time.
  Return 0 if valid, -1 if invalid and uncorrectable, or a positive
  integer approximating the number of corrections needed.
  */
static int validate_tm (struct tm *tm)
{
    int tmp, res = 0;

    if (tm->tm_sec < 0 || tm->tm_sec > 60) { /* 61 POSIX, 60 draft ISO C */
	res++;
	tmp = tm->tm_sec/60;
	tm->tm_sec -= 60 * tmp; tm->tm_min += tmp;
	if(tm->tm_sec < 0) {tm->tm_sec += 60; tm->tm_min--;}
    }

    if (tm->tm_min < 0 || tm->tm_min > 59) {
	res++;
	tmp = tm->tm_min/60;
	tm->tm_min -= 60 * tmp; tm->tm_hour += tmp;
	if(tm->tm_min < 0) {tm->tm_min += 60; tm->tm_hour--;}
    }

    if (tm->tm_hour < 0 || tm->tm_hour > 23) {
	res++;
	tmp = tm->tm_hour/24;
	tm->tm_hour -= 24 * tmp; tm->tm_mday += tmp;
	if(tm->tm_hour < 0) {tm->tm_hour += 24; tm->tm_mday--;}
    }

    /* defer fixing mday until we know the year */
    if (tm->tm_mon < 0 || tm->tm_mon > 11) {
	res++;
	tmp = tm->tm_mon/12;
	tm->tm_mon -= 12 * tmp; tm->tm_year += tmp;
	if(tm->tm_mon < 0) {tm->tm_mon += 12; tm->tm_year--;}
    }

    /* A limit on the loops of about 3000x round */
    if(tm->tm_mday < -1000000 || tm->tm_mday > 1000000) return -1;

    if(abs(tm->tm_mday) > 366) {
	res++;
	/* first spin back until January */
	while(tm->tm_mon > 0) {
	    --tm->tm_mon;
	    tm->tm_mday += days_in_month[tm->tm_mon] +
	    ((tm->tm_mon==1 && isleap(1900+tm->tm_year))? 1 : 0);
	}
	/* then spin on/back by years */
	while(tm->tm_mday < 1) {
	    --tm->tm_year;
	    tm->tm_mday += 365 + (isleap(1900+tm->tm_year)? 1 : 0);
	}
	while(tm->tm_mday >
	      (tmp = 365 + (isleap(1900+tm->tm_year)? 1 : 0))) {
	    tm->tm_mday -= tmp; tm->tm_year++;
	}
    }

    while(tm->tm_mday < 1) {
	res++;
	if(--tm->tm_mon < 0) {tm->tm_mon += 12; tm->tm_year--;}
	tm->tm_mday += days_in_month[tm->tm_mon] +
	    ((tm->tm_mon==1 && isleap(1900+tm->tm_year))? 1 : 0);
    }

    while(tm->tm_mday >
	  (tmp = days_in_month[tm->tm_mon] +
	   ((tm->tm_mon==1 && isleap(1900+tm->tm_year))? 1 : 0))) {
	res++;
	if(++tm->tm_mon > 11) {tm->tm_mon -= 12; tm->tm_year++;}
	tm->tm_mday -= tmp;
    }
    return res;
}


/* Substitute for mktime -- no checking, always in GMT */
static double mktime00 (struct tm *tm)
{
    int day = 0;
    int i, year, year0;
    double excess = 0.0;
    
    day = tm->tm_mday - 1;
    year0 = 1900 + tm->tm_year;
    /* safety check for unbounded loops */
    if (year0 > 3000) {
	excess = (int)(year0/2000) - 1;
	year0 -= excess * 2000;
    } else if (year0 < 0) {
	excess = -1 - (int)(-year0/2000);
	year0 -= excess * 2000;
    }

    for(i = 0; i < tm->tm_mon; i++) day += days_in_month[i];
    if (tm->tm_mon > 1 && isleap(year0)) day++;
    tm->tm_yday = day;

    if (year0 > 1970) {
	for (year = 1970; year < year0; year++)
	    day += days_in_year(year);
    } else if (year0 < 1970) {
	for (year = 1969; year >= year0; year--)
	    day -= days_in_year(year);
    }

    /* weekday: Epoch day was a Thursday */
    if ((tm->tm_wday = (day + 4) % 7) < 0) tm->tm_wday += 7;

    return tm->tm_sec + (tm->tm_min * 60) + (tm->tm_hour * 3600)
	+ (day + excess * 730485) * 86400.0;
}

static double guess_offset (struct tm *tm)
{
    double offset, offset1, offset2;
    int i, wday, year, oldmonth, oldisdst, oldmday;
    struct tm oldtm;
    /*
       Adjust as best we can for timezones: if isdst is unknown, use
       the smaller offset at same day in Jan or July of a valid year.
       We don't know the timezone rules, but if we choose a year with
       July 1 on the same day of the week we will likely get guess
       right (since they are usually on Sunday mornings).
    */

    memcpy(&oldtm, tm, sizeof(struct tm));
    oldmonth = tm->tm_mon;
    oldmday = tm->tm_mday;
    oldisdst = tm->tm_isdst;

    /* so now look for a suitable year */
    tm->tm_mon = 6;
    tm->tm_mday = 1;
    tm->tm_isdst = -1;
    mktime00(tm);  /* to get wday valid */
    wday = tm->tm_wday;
    /* We cannot use 1970 because of the Windows bug with 1970-01-01 east
       of GMT. */
    for(i = 71; i < 82; i++) { /* These cover all the possibilities */
	tm->tm_year = i;
	mktime(tm);
	if(tm->tm_wday == wday) break;
    }
    year = i;

    /* Now look up offset in January */
    tm->tm_mday = oldmday;
    tm->tm_mon = 0;
    tm->tm_year = year;
    tm->tm_isdst = -1;
    offset1 = (double) mktime(tm) - mktime00(tm);
    /* and in July */
    tm->tm_year = year;
    tm->tm_mon = 6;
    tm->tm_isdst = -1;
    offset2 = (double) mktime(tm) - mktime00(tm);
    if(oldisdst > 0) {
	offset = (offset1 > offset2) ? offset2 : offset1;
    } else {
	offset = (offset1 > offset2) ? offset1 : offset2;
    }
    /* now try to guess dst if unknown */
    tm->tm_mon = oldmonth;
    tm->tm_isdst = -1;
    if(oldisdst < 0) {
	offset1 = (double) mktime(tm) - mktime00(tm);
	oldisdst = (offset1 < offset) ? 1:0;
	if(oldisdst) offset = offset1;
    }
    /* restore all as mktime might alter it */
    memcpy(tm, &oldtm, sizeof(struct tm));
    /* and then set isdst */
    tm->tm_isdst = oldisdst;
    return offset;
}

/* Interface to mktime or mktime00 */
static double mktime0 (struct tm *tm, const int local)
{
    double res;
    Rboolean OK;
#ifndef HAVE_POSIX_LEAPSECONDS
    int i;
#endif

    if(validate_tm(tm) < 0) return (double)(-1);
    if(!local) return mktime00(tm);

    OK = tm->tm_year < 138 && tm->tm_year >= (have_broken_mktime() ? 70 : 02);
#ifdef Win32
    /* Microsoft's mktime regards times before 1970-01-01 00:00:00 GMT as
       invalid! */
    if(tm->tm_year == 70 && tm->tm_mon == 0 && tm->tm_mday <= 1) OK = FALSE;
#endif
    if(OK) {
	res = (double) mktime(tm);
	if (res == (double)-1) return res;
#ifndef HAVE_POSIX_LEAPSECONDS
	if (n_leapseconds < 0) set_n_leapseconds();
        for(i = 0; i < n_leapseconds; i++)
            if(res > leapseconds[i]) res -= 1.0;
#endif
        return res;
/* watch the side effect here: both calls alter their arg */
    } else return guess_offset(tm) + mktime00(tm);
}

/* Interface for localtime or gmtime or internal substitute */
static struct tm * localtime0(const double *tp, const int local, struct tm *ltm)
{
    double d = *tp;
    int day;
    int y, tmp, mon, left, diff, diff2;
    struct tm *res= ltm;
    time_t t;

    if(d < 2147483647.0 && d > (have_broken_mktime() ? 0. : -2147483647.0)) {
	t = (time_t) d;
#ifndef HAVE_POSIX_LEAPSECONDS
	if (n_leapseconds < 0) set_n_leapseconds();
        for(y = 0; y < n_leapseconds; y++) if(t > leapseconds[y] + y - 1) t++;
#endif
	return local ? localtime(&t) : gmtime(&t);
    }

    day = (int) floor(d/86400.0);
    left = (int) (d - day * 86400.0 + 0.5);

    /* hour, min, and sec */
    res->tm_hour = left / 3600;
    left %= 3600;
    res->tm_min = left / 60;
    res->tm_sec = left % 60;

    /* weekday: 1970-01-01 was a Thursday */
    if ((res->tm_wday = ((4 + day) % 7)) < 0) res->tm_wday += 7;

    /* year & day within year */
    y = 1970;
    if (day >= 0)
	for ( ; day >= (tmp = days_in_year(y)); day -= tmp, y++);
    else
	for ( ; day < 0; --y, day += days_in_year(y) );

    y = res->tm_year = y - 1900;
    res->tm_yday = day;

    /* month within year */
    for (mon = 0;
	 day >= (tmp = (days_in_month[mon]) + ((mon==1 && isleap(y+1900))?1:0));
	 day -= tmp, mon++);
    res->tm_mon = mon;
    res->tm_mday = day + 1;

    if(local) {
	int shift;
	/*  daylight saving time is unknown */
	res->tm_isdst = -1;

	/* Try to fix up timezone differences */
        diff = guess_offset(res)/60;
	shift = res->tm_min + 60*res->tm_hour;
	res->tm_min -= diff;
	validate_tm(res);
	res->tm_isdst = -1;
	/* now this might be a different day */
	if(shift - diff < 0) res->tm_yday--;
	if(shift - diff > 24) res->tm_yday++;	
	diff2 = guess_offset(res)/60;
	if(diff2 != diff) {
	    res->tm_min += (diff - diff2);
	    validate_tm(res);
	}
	return res;
    } else {
	res->tm_isdst = 0; /* no dst in GMT */
	return res;
    }
}


#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif
#ifdef Win32
# define WIN32_LEAN_AND_MEAN 1
# include <windows.h>
#endif

SEXP attribute_hidden do_systime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = allocVector(REALSXP, 1);
#ifdef HAVE_GETTIMEOFDAY
    struct timeval tv;
    int res = gettimeofday(&tv, NULL);
    if(res == 0) {
	double tmp = (double) tv.tv_sec + 1e-6 * (double) tv.tv_usec;
#ifndef HAVE_POSIX_LEAPSECONDS
	if (n_leapseconds < 0) set_n_leapseconds();
	tmp -= n_leapseconds;
#endif
	REAL(ans)[0] = tmp;
    } else 
	REAL(ans)[0] = NA_REAL;
    return ans;
#else
    time_t res = time(NULL);
    double tmp = res;
    if(res != (time_t)(-1)) {
#ifndef HAVE_POSIX_LEAPSECONDS
	if (n_leapseconds < 0) set_n_leapseconds();
	tmp -= n_leapseconds;
#endif
#ifdef Win32
	{
	    SYSTEMTIME st;
	    GetSystemTime(&st);
	    tmp += 1e-3 * st.wMilliseconds;
	}
#endif
	REAL(ans)[0] = tmp;
    }
    return ans;
#endif
}


#ifdef Win32
#define tzname _tzname
#else /* Unix */
extern char *tzname[2];
#endif

static int set_tz(char *tz, char *oldtz)
{
    char *p = NULL;
    int settz = 0;

    strcpy(oldtz, "");
    p = getenv("TZ");
    if(p) strcpy(oldtz, p);
#ifdef HAVE_SETENV
    if(setenv("TZ", tz, 1)) warning(_("problem with setting timezone"));
    settz = 1;
#elif defined(HAVE_PUTENV)
    {
	static char buff[200];
	strcpy(buff, "TZ="); strcat(buff, tz);
	if(putenv(buff)) warning(_("problem with setting timezone"));
    }
    settz = 1;
#else
    warning(_("cannot set timezones on this system"));
#endif
    tzset();
    return settz;
}

static void reset_tz(char *tz)
{
    if(strlen(tz)) {
#ifdef HAVE_SETENV
	if(setenv("TZ", tz, 1)) warning(_("problem with setting timezone"));
#elif defined(HAVE_PUTENV)
	{
	    static char buff[200];
	    strcpy(buff, "TZ="); strcat(buff, tz);
	    if(putenv(buff)) warning(_("problem with setting timezone"));
	}
#endif
    } else {
#ifdef HAVE_UNSETENV
	unsetenv("TZ"); /* FreeBSD variants do not return a value */
#elif defined(HAVE_PUTENV_UNSET)
	if(putenv("TZ")) warning(_("problem with unsetting timezone"));
#elif defined(HAVE_PUTENV_UNSET2)
	if(putenv("TZ=")) warning(_("problem with unsetting timezone"));
#endif
    }
    tzset();
}


static const char ltnames [][6] =
{ "sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst" };


static void makelt(struct tm *tm, SEXP ans, int i, int valid, double frac_secs)
{
    int j;

    if(valid) {
	REAL(VECTOR_ELT(ans, 0))[i] = tm->tm_sec + frac_secs;
	INTEGER(VECTOR_ELT(ans, 1))[i] = tm->tm_min;
	INTEGER(VECTOR_ELT(ans, 2))[i] = tm->tm_hour;
	INTEGER(VECTOR_ELT(ans, 3))[i] = tm->tm_mday;
	INTEGER(VECTOR_ELT(ans, 4))[i] = tm->tm_mon;
	INTEGER(VECTOR_ELT(ans, 5))[i] = tm->tm_year;
	INTEGER(VECTOR_ELT(ans, 6))[i] = tm->tm_wday;
	INTEGER(VECTOR_ELT(ans, 7))[i] = tm->tm_yday;
	INTEGER(VECTOR_ELT(ans, 8))[i] = tm->tm_isdst;
    } else {
	REAL(VECTOR_ELT(ans, 0))[i] = NA_REAL;
	for(j = 1; j < 8; j++)
	    INTEGER(VECTOR_ELT(ans, j))[i] = NA_INTEGER;
	INTEGER(VECTOR_ELT(ans, 8))[i] = -1;
    }
}


SEXP attribute_hidden do_asPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans, ansnames, klass, tzone;
    int i, n, isgmt = 0, valid, settz = 0;
    char *tz = NULL, oldtz[20] = "";

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) tz = p;
    }
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    n = LENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(i = 0; i < n; i++) {
        struct tm dummy, *ptm = &dummy;
	double d = REAL(x)[i];
	if(R_FINITE(d)) {
	    ptm = localtime0(&d, 1 - isgmt, &dummy);
	    /* in theory localtime/gmtime always return a valid
	       struct tm pointer, but Windows uses NULL for error
	       conditions (like negative times). */
	    valid = (ptm != NULL);
	} else valid = 0;
	makelt(ptm, ans, i, valid, d - floor(d));
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("POSIXt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXlt"));
    classgets(ans, klass);
    if (isgmt) {
	PROTECT(tzone = allocVector(STRSXP, 1));
	SET_STRING_ELT(tzone, 0, mkChar(tz));
    } else {
	PROTECT(tzone = allocVector(STRSXP, 3));
	SET_STRING_ELT(tzone, 0, mkChar(tz));
	SET_STRING_ELT(tzone, 1, mkChar(tzname[0]));
	SET_STRING_ELT(tzone, 2, mkChar(tzname[1]));
    }
    setAttrib(ans, install("tzone"), tzone);
    UNPROTECT(5);

    if(settz) reset_tz(oldtz);
    return ans;
}

SEXP attribute_hidden do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans;
    int i, n = 0, isgmt = 0, nlen[9], settz = 0;
    char *tz = NULL, oldtz[20] = "";
    struct tm tm;
    double tmp;

    checkArity(op, args);
    PROTECT(x = duplicate(CAR(args))); /* coerced below */
    if(!isVectorList(x) || LENGTH(x) != 9)
	error(_("invalid '%s' argument"), "x");
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");

    tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) tz = p;
    }
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    for(i = 0; i < 6; i++)
	if((nlen[i] = LENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = LENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(i = 0; i < 6; i++)
	    if(nlen[i] == 0)
		error(_("zero length component in non-empty POSIXlt structure"));
	if(nlen[8] == 0)
	    error(_("zero length component in non-empty POSIXlt structure"));
    }
    /* coerce fields to integer or real */
    SET_VECTOR_ELT(x, 0, coerceVector(VECTOR_ELT(x, 0), REALSXP));
    for(i = 0; i < 6; i++)
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i),
					  i > 0 ? INTSXP: REALSXP));
    SET_VECTOR_ELT(x, 8, coerceVector(VECTOR_ELT(x, 8), INTSXP));

    PROTECT(ans = allocVector(REALSXP, n));
    for(i = 0; i < n; i++) {
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	tm.tm_sec   = fsecs;
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	/* mktime ignores tm.tm_wday and tm.tm_yday */
	tm.tm_isdst = isgmt ? 0:INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(!R_FINITE(secs) || tm.tm_min == NA_INTEGER ||
	   tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
	   tm.tm_mon == NA_INTEGER || tm.tm_year == NA_INTEGER)
	    REAL(ans)[i] = NA_REAL;
	else {
	    tmp = mktime0(&tm, 1 - isgmt);
	    REAL(ans)[i] = (tmp == (double)(-1)) ? 
		NA_REAL : tmp + (secs - fsecs);
	}
    }

    if(settz) reset_tz(oldtz);

    UNPROTECT(2);
    return ans;
}

SEXP attribute_hidden do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans, tz;
    int i, n = 0, m, N, nlen[9], UseTZ;
    char buff[300], *p;
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = duplicate(CAR(args))); /* coerced below */
    if(!isVectorList(x) || LENGTH(x) != 9)
	error(_("invalid '%s' argument"), "x");
    if(!isString((sformat = CADR(args))) || LENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "format");
    m = LENGTH(sformat);
    UseTZ = asLogical(CADDR(args));
    if(UseTZ == NA_LOGICAL)
	error(_("invalid '%s' argument"), "usetz");
    tz = getAttrib(x, install("tzone"));

    /* workaround for glibc & MacOS X bugs in strftime: they have
       undocumented and non-POSIX/C99 time zone components 
     */
    memset(&tm, 0, sizeof(tm));

    /* coerce fields to integer or real, find length of longest one */
    for(i = 0; i < 9; i++) {
	nlen[i] = LENGTH(VECTOR_ELT(x, i));
	if(nlen[i] > n) n = nlen[i];
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), 
					  i > 0 ? INTSXP : REALSXP));
    }
    if(n > 0) N = (m > n) ? m:n; else N = 0;

    PROTECT(ans = allocVector(STRSXP, N));
    for(i = 0; i < N; i++) {
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	tm.tm_sec   = fsecs;
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	tm.tm_wday  = INTEGER(VECTOR_ELT(x, 6))[i%nlen[6]];
	tm.tm_yday  = INTEGER(VECTOR_ELT(x, 7))[i%nlen[7]];
	tm.tm_isdst = INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(!R_FINITE(secs) || tm.tm_min == NA_INTEGER ||
	   tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
	   tm.tm_mon == NA_INTEGER || tm.tm_year == NA_INTEGER) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    if(validate_tm(&tm) < 0) SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		char *q = CHAR(STRING_ELT(sformat, i%m)), buf2[500];
		strcpy(buf2,  q);
		p = strstr(q, "%OS");
		if(p) {
		    int ns, nused = 4;
		    char *p2 = strstr(buf2, "%OS");
		    *p2 = '\0';
		    ns = *(p+3) - '0';
		    if(ns < 0 || ns > 9) { /* not a digit */
			ns = asInteger(GetOption(install("digits.secs"),
						 R_BaseEnv));
			if(ns == NA_INTEGER) ns = 0;
			nused = 3;
		    }
		    if(ns > 6) ns = 6;
		    if(ns > 0) {
			sprintf(p2, "%0*.*f", ns+3, ns, secs);
			strcat(buf2, p+nused);
		    } else {
			strcat(p2, "%S");
			strcat(buf2, p+nused);
		    }
		}
		strftime(buff, 256, buf2, &tm);
		if(UseTZ && !isNull(tz)) {
		    int i = 0;
		    if(LENGTH(tz) == 3) {
			if(tm.tm_isdst > 0) i = 2;
		        else if(tm.tm_isdst == 0) i = 1;
			else i = 0; /* Use base timezone name */
		    }
		    p = CHAR(STRING_ELT(tz, i));
		    if(strlen(p)) {
			strcat(buff, " ");
			strcat(buff, p);
		    }
		}
		SET_STRING_ELT(ans, i, mkChar(buff));
	    }
	}
    }
    UNPROTECT(2);
    return ans;
}

static void glibc_fix(struct tm *tm, int *invalid)
{
    /* set mon and mday which glibc does not always set.
       Use current year/... if none has been specified.

       Specifying mon but not mday nor yday is invalid.
    */
    time_t t = time(NULL);
    struct tm *tm0;
    int tmp;
#ifndef HAVE_POSIX_LEAPSECONDS
    if (n_leapseconds < 0) set_n_leapseconds();
    t -= n_leapseconds;
#endif
    tm0 = localtime(&t);
    if(tm->tm_year == NA_INTEGER) tm->tm_year = tm0->tm_year;
    if(tm->tm_mon != NA_INTEGER && tm->tm_mday != NA_INTEGER) return;
    /* at least one of the month and the day of the month is missing */
    if(tm->tm_yday != NA_INTEGER) {
	/* since we have yday, let that take precedence over mon/mday */
	int yday = tm->tm_yday, mon = 0;
	while(yday >= (tmp = days_in_month[mon] +
		      ((mon==1 && isleap(1900+tm->tm_year))? 1 : 0))) {
	    yday -= tmp;
	    mon++;
	}
	tm->tm_mon = mon;
	tm->tm_mday = yday + 1;
    } else {
	if(tm->tm_mday == NA_INTEGER) {
	    if(tm->tm_mon != NA_INTEGER) {
		*invalid = 1;
		return;
	    } else tm->tm_mday = tm0->tm_mday;
	}
	if(tm->tm_mon == NA_INTEGER) tm->tm_mon = tm0->tm_mon;
    }
}


SEXP attribute_hidden do_strptime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans, ansnames, klass, stz, tzone;
    int i, n, m, N, invalid, isgmt = 0, settz = 0;
    struct tm tm, tm2;
    char *tz = NULL, oldtz[20] = "";
    double psecs = 0.0;

    checkArity(op, args);
    if(!isString((x= CAR(args))))
	error(_("invalid '%s' argument"), "x");
    if(!isString((sformat = CADR(args))) || LENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "x");
    if(!isString((stz = CADDR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) tz = p;
    }
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    n = LENGTH(x); m = LENGTH(sformat);
    if(n > 0) N = (m > n)?m:n; else N = 0;

    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, N));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));


    for(i = 0; i < N; i++) {
	/* for glibc's sake. That only sets some unspecified fields,
	   sometimes. */
	tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
	tm.tm_year = tm.tm_mon = tm.tm_mday = tm.tm_yday = 
	    tm.tm_wday = NA_INTEGER;
	tm.tm_isdst = -1;
	invalid = STRING_ELT(x, i%n) == NA_STRING ||
	    !R_strptime(CHAR(STRING_ELT(x, i%n)),
			CHAR(STRING_ELT(sformat, i%m)), &tm, &psecs);
	if(!invalid) {
	    /* Solaris sets missing fields to 0 */
	    if(tm.tm_mday == 0) tm.tm_mday = NA_INTEGER;
	    if(tm.tm_mon == NA_INTEGER || tm.tm_mday == NA_INTEGER
	       || tm.tm_year == NA_INTEGER)
		glibc_fix(&tm, &invalid);
	    tm.tm_isdst = -1;
	    /* we do want to set wday, yday, isdst, but not to
	       adjust structure at DST boundaries */
	    memcpy(&tm2, &tm, sizeof(struct tm));
	    mktime0(&tm2, 1-isgmt); /* set wday, yday, isdst */
	    tm.tm_wday = tm2.tm_wday;
	    tm.tm_yday = tm2.tm_yday;
	    tm.tm_isdst = isgmt ? 0: tm2.tm_isdst;
	}
	invalid = invalid || validate_tm(&tm) != 0;
	makelt(&tm, ans, i, !invalid, psecs - floor(psecs));
    }

    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("POSIXt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXlt"));
    classgets(ans, klass);
    if (isgmt) {
	PROTECT(tzone = allocVector(STRSXP, 1));
	SET_STRING_ELT(tzone, 0, mkChar(tz));
	setAttrib(ans, install("tzone"), tzone);
	UNPROTECT(1);
    } else if(strlen(tz)) {
	PROTECT(tzone = allocVector(STRSXP, 3));
	SET_STRING_ELT(tzone, 0, mkChar(tz));
	SET_STRING_ELT(tzone, 1, mkChar(tzname[0]));
	SET_STRING_ELT(tzone, 2, mkChar(tzname[1]));
	setAttrib(ans, install("tzone"), tzone);
	UNPROTECT(1);
    }
    if(settz) reset_tz(oldtz);

    UNPROTECT(3);
    return ans;
}

SEXP attribute_hidden do_D2POSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans, ansnames, klass;
    int n, i, valid;
    int day;
    int y, tmp, mon;
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    n = LENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(i = 0; i < n; i++) {
	if(R_FINITE(REAL(x)[i])) {
	    day = (int) REAL(x)[i];
	    tm.tm_hour = tm.tm_min = tm.tm_sec = 0;
	    /* weekday: 1970-01-01 was a Thursday */
	    if ((tm.tm_wday = ((4 + day) % 7)) < 0) tm.tm_wday += 7;

	    /* year & day within year */
	    y = 1970;
	    if (day >= 0)
		for ( ; day >= (tmp = days_in_year(y)); day -= tmp, y++);
	    else
		for ( ; day < 0; --y, day += days_in_year(y) );

	    y = tm.tm_year = y - 1900;
	    tm.tm_yday = day;

	    /* month within year */
	    for (mon = 0;
		 day >= (tmp = (days_in_month[mon]) +
			 ((mon==1 && isleap(y+1900))?1:0));
		 day -= tmp, mon++);
	    tm.tm_mon = mon;
	    tm.tm_mday = day + 1;
	    tm.tm_isdst = 0; /* no dst in GMT */

	    valid = 1;
	} else valid = 0;
	makelt(&tm, ans, i, valid, 0.0);
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("POSIXt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXlt"));
    classgets(ans, klass);
    setAttrib(ans, install("tzone"), mkString("UTC"));
    UNPROTECT(4);

    return ans;
}

SEXP attribute_hidden do_POSIXlt2D(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans, klass;
    int i, n = 0, nlen[9];
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = duplicate(CAR(args)));
    if(!isVectorList(x) || LENGTH(x) != 9)
	error(_("invalid '%s' argument"), "x");

    for(i = 3; i < 6; i++)
	if((nlen[i] = LENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = LENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(i = 3; i < 6; i++)
	    if(nlen[i] == 0)
		error(_("zero length component in non-empty POSIXlt structure"));
	if(nlen[8] == 0)
	    error(_("zero length component in non-empty POSIXlt structure"));
    }
    /* coerce relevant fields to integer */
    for(i = 3; i < 6; i++)
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), INTSXP));

    PROTECT(ans = allocVector(REALSXP, n));
    for(i = 0; i < n; i++) {
	tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	/* mktime ignores tm.tm_wday and tm.tm_yday */
	tm.tm_isdst = 0;
	if(tm.tm_mday == NA_INTEGER || tm.tm_mon == NA_INTEGER ||
	   tm.tm_year == NA_INTEGER || validate_tm(&tm) < 0)
	    REAL(ans)[i] = NA_REAL;
	else {
	    double tmp = mktime00(&tm);
	    REAL(ans)[i] = (tmp == -1) ? NA_REAL : tmp/86400;
	}
    }

    PROTECT(klass = allocVector(STRSXP, 1));
    SET_STRING_ELT(klass, 0, mkChar("Date"));
    classgets(ans, klass);
    UNPROTECT(3);
    return ans;
}
