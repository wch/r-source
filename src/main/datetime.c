/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000  The R Development Core Team.
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

#ifdef HAVE_GLIBC2
# define _XOPEN_SOURCE		/* so that we get strptime() */
# include <time.h>
# undef _XOPEN_SOURCE		/* just to make sure */
#else
# include <time.h>
#endif

#include "Defn.h"

#ifndef HAVE_STRPTIME
/* Substitute based on glibc code. */
# include "Rstrptime.h"
#endif

static const int days_in_month[12] =
{31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define days_in_year(year) (isleap(year) ? 366 : 365)

#ifdef USING_LEAPSECONDS
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
    int oldmonth, oldyear;

    /*
       adjust as best we can for timezones: if isdst is unknown,
       use the smaller offset at same day in Jan or July 2000
    */
    oldmonth = tm->tm_mon;
    oldyear = tm->tm_year;
    tm->tm_mon = 0;
    tm->tm_year = 100;
    offset1 = (double) mktime(tm) - mktime00(tm);
    tm->tm_mon = 6;
    offset2 = (double) mktime(tm) - mktime00(tm);
    if(tm->tm_isdst > 0) {
	offset = (offset1 > offset2) ? offset1 : offset2;
    } else {
	offset = (offset1 > offset2) ? offset2 : offset1;
    }
    tm->tm_year = oldyear;
    tm->tm_mon = oldmonth;
    return offset;
}

/* Interface to mktime or mktime00 */
static double mktime0 (struct tm *tm)
{
    double res;
#ifdef USING_LEAPSECONDS
    int i;
#endif

    if(validate_tm(tm) < 0) return (double)(-1);

    if(tm->tm_year < 138 &&
#ifdef WIN32
       tm->tm_year >= 70)
#else
       tm->tm_year > 02)
#endif
    {   res = (double) mktime(tm);
#ifdef USING_LEAPSECONDS
        for(i = 0; i < 22; i++)
            if(res > leapseconds[i]) res -= 1.0;
#endif
        return res;
/* watch the side effect here: both calls alter their arg */
    } else return guess_offset(tm) + mktime00(tm);
}

static struct tm ltm;

/* Interface for localtime or gmtime or internal substitute */
static struct tm * localtime0(const double *tp, const int local)
{
    double d = *tp;
    long day;
    int y, tmp, mon, left, diff;
    struct tm *res= &ltm;
    time_t t;

    if(d < 2147483647.0 &&
#ifdef WIN32
       d >= 0.0) {
#else
       d > -2147483647.0) {
#endif
	t = (time_t) d;
#ifdef USING_LEAPSECONDS
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
#ifdef USING_LEAPSECONDS
    res -= 22;
#endif
    SEXP ans = allocVector(REALSXP, 1);
    if(res != (time_t)(-1)) REAL(ans)[0] = (double) res;
    else REAL(ans)[0] = NA_REAL;
    return ans;
}

#ifdef WIN32
#define tzname _tzname
#else
extern char *tzname[2];
#endif

static char buff[20]; /* for putenv */

static char ltnames[][6] =
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
    int i, n, isgmt = 0, valid, settz=0;
    char *tz = NULL, oldtz[20] = "", *p = NULL;
    struct tm *ptm = NULL;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error("invalid `tz' value");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) {
#ifdef WIN32
	tzset();
	strcpy(oldtz, _daylight ? _tzname[1] : _tzname[0]);
#else
	strcpy(oldtz, "");
#endif
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
    }

    n = LENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(INTSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(i = 0; i < n; i++) {
	if(R_FINITE(REAL(x)[i])){
	    double d = REAL(x)[i];
	    ptm = localtime0(&d, 1 - isgmt);
	    /* in theory localtime/gmtime always return a valid
	       struct tm pointer, but Windows uses NULL for error
	       conditions (like negative times). */
	    valid = (ptm != NULL);
	} else valid = 0;
	makelt(ptm, ans, i, valid);
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(class = allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("POSIXlt"));
    classgets(ans, class);
    PROTECT(tzone = allocVector(STRSXP, 3));
    SET_STRING_ELT(tzone, 0, mkChar(tz));
    SET_STRING_ELT(tzone, 1, mkChar(tzname[0]));
    SET_STRING_ELT(tzone, 2, mkChar(tzname[1]));
    setAttrib(ans, install("tzone"), tzone);
    UNPROTECT(5);

    /* reset timezone */
    if(settz) {
	if(strlen(oldtz)) {
#ifdef HAVE_PUTENV
	    strcpy(buff, "TZ="); strcat(buff, oldtz);
	    putenv(buff);
#else
# ifdef HAVE_SETENV
	    setenv("TZ", oldtz, 1);
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
    }
    return ans;
}

SEXP do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans;
    int i, n = 0, isgmt = 0, nlen[9], settz = 0;
    char *tz = NULL, oldtz[20] = "", *p = NULL;
    struct tm tm;

    checkArity(op, args);
    x = CAR(args);
    if(!isVectorList(x) || LENGTH(x) != 9)
	error("invalid `x' argument");
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error("invalid `tz' value");

    tz = CHAR(STRING_ELT(stz, 0));
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(strlen(tz) > 0) {
#ifdef WIN32
	tzset();
	strcpy(oldtz, _daylight ? _tzname[1] : _tzname[0]);
#else
	strcpy(oldtz, "");
#endif
	if((p = getenv("TZ"))) strcpy(oldtz, p);
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
    }

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
	else REAL(ans)[i] = mktime0(&tm);
    }

    /* reset timezone */
    if(settz) {
	if(strlen(oldtz)) {
#ifdef HAVE_PUTENV
	    strcpy(buff, "TZ="); strcat(buff, oldtz);
	    putenv(buff);
#else
# ifdef HAVE_SETENV
	    setenv("TZ", oldtz, 1);
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
    }

    UNPROTECT(1);
    return ans;
}

SEXP do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans;
    int i, n = 0, m, N, nlen[9];
    char buff[256];
    struct tm tm;

    checkArity(op, args);
    x = CAR(args);
    if(!isVectorList(x) || LENGTH(x) != 9)
	error("invalid `x' argument");
    if(!isString((sformat = CADR(args))) || LENGTH(sformat) == 0)
	error("invalid `format' argument");
    m = LENGTH(sformat);

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
		SET_STRING_ELT(ans, i, mkChar(buff));
	    }
	}
    }
    UNPROTECT(1);
    return ans;
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
	/* for glibc's sake */
	tm.tm_sec = tm.tm_min = tm.tm_hour = tm.tm_mon = tm.tm_year = 0; 
	tm.tm_mday = 1;
	invalid = STRING_ELT(x, i%n) == NA_STRING ||
	    !strptime(CHAR(STRING_ELT(x, i%n)),
		      CHAR(STRING_ELT(sformat, i%m)), &tm);
	if(!invalid) {
	    tm.tm_isdst = -1;
	    mktime0(&tm); /* set wday, yday, isdst */
	}
	makelt(&tm, ans, i, !invalid);
    }
    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(class = allocVector(STRSXP, 1));
    SET_STRING_ELT(class, 0, mkChar("POSIXlt"));
    classgets(ans, class);
    UNPROTECT(3);
    return ans;
}
