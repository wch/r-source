/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000, 2001  The R Development Core Team.
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *      Interfaces to POSIX date and time functions.
 */

/*
    These use POSIX functions that are not available on all platforms,
    and where they are they may be partially or incorrectly implemented.
    A number of lightweight alternatives are supplied, but generally
    timezone support is only available if the OS supplies it.

    A particular problem is the setting of the timezone TZ on Unix/Linux.
    POSIX appears to require it, yet many Linux systems do not set it
    and do not give the correct results/crash strftime if it is not set.
    We use unsetenv() to work around this: that is a BSD construct but
    seems to be available on the affected platforms.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#if defined(HAVE_GLIBC2) && !defined(__USE_BSD)
# define __USE_BSD		/* so that we get unsetenv() */
# include <stdlib.h>
# undef __USE_BSD		/* just to make sure */
#else
# include <stdlib.h>
#endif

#if defined(HAVE_GLIBC2) && !defined(__USE_XOPEN)
# define __USE_XOPEN		/* so that we get strptime() */
# include <time.h>
# undef __USE_XOPEN		/* just to make sure */
#else
# include <time.h>
#endif

#include "Defn.h"

/* The glibc in RH8.0 is broken and assumes that dates before 1970-01-01
   do not exist. So does Windows, but at least there we do not need a
   run-time test.  As from 1.6.2, test the actual mktime code and cache the
   result on glibc >= 2.2. (It seems this started between 2.2.5 and 2.3,
   and RH8.0 has an unreleased version in that gap.)
*/

static Rboolean have_broken_mktime(void)
{
#ifdef Win32
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

#if (defined(Macintosh) & defined(__MRC__))
#define mktime POSIXMakeTime
#endif

 
#ifndef HAVE_WORKING_STRPTIME
/* Substitute based on glibc code. */
# include "Rstrptime.h"
#endif

static const int days_in_month[12] =
{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define days_in_year(year) (isleap(year) ? 366 : 365)

#ifndef HAVE_POSIX_LEAPSECONDS
static const time_t leapseconds[] =
{  78796800, 94694400,126230400,157766400,189302400,220924800,252460800,
  283996800,315532800,362793600,425865600,489024000,520560000,567993600,
  631152000,662688000,709948800,741484800,773020800,820454400,867715200,
  915148800 };
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

    if(tm->tm_mday < -1000 || tm->tm_mday > 1000) return -1;


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
    long day = 0;
    int i, year, year0;

    day = tm->tm_mday - 1;
    year0 = 1900 + tm->tm_year;
    /* safety check for unbounded loops */
    if (abs(year0 - 1970) > 5000) return (double)(-1);

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
	+ (day * 86400.0);
}

static double guess_offset (struct tm *tm)
{
    double offset, offset1, offset2;
    int oldmonth, oldyear, olddst;

    /*
       adjust as best we can for timezones: if isdst is unknown,
       use the smaller offset at same day in Jan or July 2000
    */
    oldmonth = tm->tm_mon;
    oldyear = tm->tm_year;
    olddst = tm->tm_isdst;
    tm->tm_mon = 0;
    tm->tm_year = 100;
    tm->tm_isdst = -1;
    offset1 = (double) mktime(tm) - mktime00(tm);
    tm->tm_year = 100;
    tm->tm_mon = 6;
    tm->tm_isdst = -1;
    offset2 = (double) mktime(tm) - mktime00(tm);
    if(olddst > 0) {
	offset = (offset1 > offset2) ? offset2 : offset1;
    } else {
	offset = (offset1 > offset2) ? offset1 : offset2;
    }
    /* now try to guess dst if unknown */
    tm->tm_mon = oldmonth;
    tm->tm_isdst = -1;
    if(olddst < 0) {
	offset1 = (double) mktime(tm) - mktime00(tm);
	olddst = (offset1 < offset) ? 1:0;
	if(olddst) offset = offset1;
    }
    tm->tm_year = oldyear;
    tm->tm_isdst = olddst;
    return offset;
}

/* Interface to mktime or mktime00 */
static double mktime0 (struct tm *tm, const int local)
{
    double res;
#ifndef HAVE_POSIX_LEAPSECONDS
    int i;
#endif

    if(validate_tm(tm) < 0) return (double)(-1);
    if(!local) return mktime00(tm);

    if(tm->tm_year < 138 &&
       tm->tm_year >= (have_broken_mktime() ? 70 : 02))
    {   res = (double) mktime(tm);
#ifndef HAVE_POSIX_LEAPSECONDS
        for(i = 0; i < 22; i++)
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
    long day;
    int y, tmp, mon, left, diff;
    struct tm *res= ltm;
    time_t t;

    if(d < 2147483647.0 && d > (have_broken_mktime() ? 0. : -2147483647.0)) {
	t = (time_t) d;
#ifndef HAVE_POSIX_LEAPSECONDS
        for(y = 0; y < 22; y++) if(t > leapseconds[y] + y - 1) t++;
#endif
	return local ? localtime(&t) : gmtime(&t);
    }

    day = (long) floor(d/86400.0);
    left = (int) (d - day * 86400.0 +0.5);

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
	 day >= (tmp = (days_in_month[mon]) + ((mon==1 && isleap(y))?1:0));
	 day -= tmp, mon++);
    res->tm_mon = mon;
    res->tm_mday = day + 1;

    if(local) {
	/*  daylight saving time is unknown */
	res->tm_isdst = -1;

	/* Try to fix up timezone differences */
        diff = guess_offset(res);
	res->tm_min -= diff/60;
	validate_tm(res);
	return res;
    } else {
	res->tm_isdst = 0; /* no dst in GMT */
	return res;
    }
}



SEXP do_systime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    time_t res = time(NULL);
    SEXP ans = allocVector(REALSXP, 1);
#ifndef HAVE_POSIX_LEAPSECONDS
    res -= 22;
#endif
    if(res != (time_t)(-1)) REAL(ans)[0] = (double) res;
    else REAL(ans)[0] = NA_REAL;
    return ans;
}


#ifdef Win32
#define tzname _tzname
#else
# ifdef Macintosh
#define tzname mytzname
static char mytzname[2][21];
static int tz_is_set = 0;
static void mac_find_tznames(void)
{
    time_t ct;
    struct tm *ltm;

    ct = time(NULL); ltm = localtime(&ct);
    ltm->tm_isdst = 0; strftime(tzname[0], 20, "%Z", ltm);
    ltm->tm_isdst = 1; strftime(tzname[1], 20, "%Z", ltm);
    tz_is_set = 1;
}
# else /* Unix */
extern char *tzname[2];
# endif
#endif

static int set_tz(char *tz, char *oldtz)
{
#ifdef Macintosh
    warning("timezones except "" and UTC are not supported on the Mac");
    return 0;
#else
    char *p = NULL;
    int settz = 0;
    static char buff[200];

    strcpy(oldtz, "");
    p = getenv("TZ");
    if(p) strcpy(oldtz, p);
#ifdef HAVE_PUTENV
    strcpy(buff, "TZ="); strcat(buff, tz);
    putenv(buff);
    settz = 1;
#else
# ifdef HAVE_SETENV
    setenv("TZ", tz, 1);
    settz = 1;
# else
    warning("cannot set timezones on this system");
# endif
#endif
    tzset();
    return settz;
#endif /* Macintosh */
}

static void reset_tz(char *tz)
{
#ifdef Macintosh
    return;
#else
    if(strlen(tz)) {
#ifdef HAVE_PUTENV
        static char buff[200];
	strcpy(buff, "TZ="); strcat(buff, tz);
	putenv(buff);
#else
# ifdef HAVE_SETENV
	setenv("TZ", tz, 1);
# endif
#endif
    } else {
#ifdef HAVE_UNSETENV
	unsetenv("TZ");
#else
# ifdef HAVE_PUTENV
	putenv("TZ=");
# endif
#endif
    }
    tzset();
#endif /* Macintosh */
}


static const char ltnames [][6] =
{ "sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst" };


static void makelt(struct tm *tm, SEXP ans, int i, int valid)
{
    int j;

    if(valid) {
	INTEGER(VECTOR_ELT(ans, 0))[i] = tm->tm_sec;
	INTEGER(VECTOR_ELT(ans, 1))[i] = tm->tm_min;
	INTEGER(VECTOR_ELT(ans, 2))[i] = tm->tm_hour;
	INTEGER(VECTOR_ELT(ans, 3))[i] = tm->tm_mday;
	INTEGER(VECTOR_ELT(ans, 4))[i] = tm->tm_mon;
	INTEGER(VECTOR_ELT(ans, 5))[i] = tm->tm_year;
	INTEGER(VECTOR_ELT(ans, 6))[i] = tm->tm_wday;
	INTEGER(VECTOR_ELT(ans, 7))[i] = tm->tm_yday;
	INTEGER(VECTOR_ELT(ans, 8))[i] = tm->tm_isdst;
    } else {
	for(j = 0; j < 8; j++)
	    INTEGER(VECTOR_ELT(ans, j))[i] = NA_INTEGER;
	INTEGER(VECTOR_ELT(ans, 8))[i] = -1;
    }
}


SEXP do_asPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans, ansnames, class, tzone;
    int i, n, isgmt = 0, valid, settz = 0;
    char *tz = NULL, oldtz[20] = "";

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error("invalid `tz' value");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);
#ifdef Macintosh
    if(!isgmt && !tz_is_set) mac_find_tznames();
#endif

    n = LENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(INTSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(i = 0; i < n; i++) {
        struct tm dummy, *ptm = &dummy;
	if(R_FINITE(REAL(x)[i])){
	    double d = REAL(x)[i];
	    ptm = localtime0(&d, 1 - isgmt, &dummy);
	    /* in theory localtime/gmtime always return a valid
	       struct tm pointer, but Windows uses NULL for error
	       conditions (like negative times). */
	    valid = (ptm != NULL);
	} else valid = 0;
	makelt(ptm, ans, i, valid);
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("POSIXt"));
    SET_STRING_ELT(class, 1, mkChar("POSIXlt"));
    classgets(ans, class);
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

SEXP do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans;
    int i, n = 0, isgmt = 0, nlen[9], settz = 0;
    char *tz = NULL, oldtz[20] = "";
    struct tm tm;
    double tmp;

    checkArity(op, args);
    x = CAR(args);
    if(!isVectorList(x) || LENGTH(x) != 9)
	error("invalid `x' argument");
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error("invalid `tz' value");

    tz = CHAR(STRING_ELT(stz, 0));
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    for(i = 0; i < 6; i++)
	if((nlen[i] = LENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = LENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(i = 0; i < 6; i++)
	    if(nlen[i] == 0)
		error("zero length component in non-empty POSIXlt structure");
	if(nlen[8] == 0)
	    error("zero length component in non-empty POSIXlt structure");
    }
    /* coerce fields to integer */
    for(i = 0; i < 6; i++)
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), INTSXP));
    SET_VECTOR_ELT(x, 8, coerceVector(VECTOR_ELT(x, 8), INTSXP));

    PROTECT(ans = allocVector(REALSXP, n));
    for(i = 0; i < n; i++) {
	tm.tm_sec   = INTEGER(VECTOR_ELT(x, 0))[i%nlen[0]];
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	/* mktime ignores tm.tm_wday and tm.tm_yday */
	tm.tm_isdst = isgmt ? 0:INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(tm.tm_sec == NA_INTEGER || tm.tm_min == NA_INTEGER ||
	   tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
	   tm.tm_mon == NA_INTEGER || tm.tm_year == NA_INTEGER)
	    REAL(ans)[i] = NA_REAL;
	else {
	    tmp = mktime0(&tm, 1 - isgmt);
	    REAL(ans)[i] = (tmp == (double)(-1)) ? NA_REAL : tmp;
	}
    }

    if(settz) reset_tz(oldtz);

    UNPROTECT(1);
    return ans;
}

SEXP do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans, tz;
    int i, n = 0, m, N, nlen[9], UseTZ;
    char buff[300], *p;
    struct tm tm;

    checkArity(op, args);
    x = CAR(args);
    if(!isVectorList(x) || LENGTH(x) != 9)
	error("invalid `x' argument");
    if(!isString((sformat = CADR(args))) || LENGTH(sformat) == 0)
	error("invalid `format' argument");
    m = LENGTH(sformat);
    UseTZ = asLogical(CADDR(args));
    if(UseTZ == NA_LOGICAL)
	error("invalid `usetz' argument");
    tz = getAttrib(x, install("tzone"));

    /* coerce fields to integer, find length of longest one */
    for(i = 0; i < 9; i++) {
	nlen[i] = LENGTH(VECTOR_ELT(x, i));
	if(nlen[i] > n) n = nlen[i];
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), INTSXP));
    }
    if(n > 0) N = (m > n) ? m:n; else N = 0;

    PROTECT(ans = allocVector(STRSXP, N));
    for(i = 0; i < N; i++) {
	tm.tm_sec   = INTEGER(VECTOR_ELT(x, 0))[i%nlen[0]];
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	tm.tm_wday  = INTEGER(VECTOR_ELT(x, 6))[i%nlen[6]];
	tm.tm_yday  = INTEGER(VECTOR_ELT(x, 7))[i%nlen[7]];
	tm.tm_isdst = INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(tm.tm_sec == NA_INTEGER || tm.tm_min == NA_INTEGER ||
	   tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
	   tm.tm_mon == NA_INTEGER || tm.tm_year == NA_INTEGER) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    if(validate_tm(&tm) < 0) SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		strftime(buff, 256, CHAR(STRING_ELT(sformat, i%m)), &tm);
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
    UNPROTECT(1);
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
    t -= 22;
#endif
    tm0 = localtime(&t);
    if(tm->tm_year == NA_INTEGER) tm->tm_year = tm0->tm_year;
    if(tm->tm_mon != NA_INTEGER && tm->tm_mday != NA_INTEGER) return;
    /* at least one of the month and the day of the month is missing */
    if(tm->tm_yday != NA_INTEGER) {
	/* since we have yday, let that take precedence over mon/mday */
	int yday = tm->tm_yday, mon = 0;
	while(yday > (tmp = days_in_month[mon] +
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


SEXP do_strptime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans, ansnames, class;
    int i, n, m, N, invalid;
    struct tm tm;

    checkArity(op, args);
    if(!isString((x= CAR(args))))
	error("invalid `x' argument");
    if(!isString((sformat = CADR(args))) || LENGTH(sformat) == 0)
	error("invalid `format' argument");
    n = LENGTH(x); m = LENGTH(sformat);
    if(n > 0) N = (m > n)?m:n; else N = 0;

    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(INTSXP, N));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));


    for(i = 0; i < N; i++) {
	/* for glibc's sake. That only sets some unspecified fields,
	   sometimes. */
	tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
	tm.tm_year = tm.tm_mon = tm.tm_mday = tm.tm_yday = NA_INTEGER;
	invalid = STRING_ELT(x, i%n) == NA_STRING ||
	    !strptime(CHAR(STRING_ELT(x, i%n)),
		      CHAR(STRING_ELT(sformat, i%m)), &tm);
	if(!invalid) {
	    /* Solaris sets missing fields to 0 */
	    if(tm.tm_mday == 0) tm.tm_mday = NA_INTEGER;
	    if(tm.tm_mon == NA_INTEGER || tm.tm_mday == NA_INTEGER
	       || tm.tm_year == NA_INTEGER)
		glibc_fix(&tm, &invalid);
	    tm.tm_isdst = -1;
	    mktime0(&tm, 1); /* set wday, yday, isdst */
	}
	makelt(&tm, ans, i, !invalid);
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(class = allocVector(STRSXP, 2));
    SET_STRING_ELT(class, 0, mkChar("POSIXt"));
    SET_STRING_ELT(class, 1, mkChar("POSIXlt"));
    classgets(ans, class);
    UNPROTECT(3);
    return ans;
}
