/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2012  The R Core Team.
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
 *
 *
 *      Interfaces to POSIX date and time functions.
 */

/*
    These use POSIX functions that are not available on all platforms,
    and where they are they may be partially or incorrectly
    implemented.  A number of lightweight alternatives are supplied,
    but generally timezone support is only available if the OS
    supplies it (or as on Windows, we replace it).  However, as these
    are now also mandated by C99, they are almost universally
    available, albeit with more room for implementation variations.

    A particular problem is the setting of the timezone TZ on
    Unix/Linux.  POSIX appears to require it, yet older Linux systems
    do not set it and do not give the correct results/crash strftime
    if it is not set (or even if it is: see the workaround below).  We
    use unsetenv() to work around this: that is a BSD (and POSIX 2001)
    construct but seems to be available on the affected platforms.

    Notes on various time functions:
    ===============================

    The current (2008) POSIX recommendation to find the calendar time
    is to call clock_gettime(), defined in <time.h>.  This may also be
    used to find time since some unspecified starting point
    (e.g. machine reboot), but is not currently so used in R.  It
    returns in second and nanoseconds, although not necessarily to
    more than clock-tick accuracy.

    C11 adds 'struct timespec' to <time.h>.  And timespec_get() can get
    the current time or interval after a base time.

    The previous POSIX recommendation was gettimeofday(), defined in
    <sys/time.h>.  This returns in seconds and microseconds (with
    unspecified granularity).

    Many systems (including AIX, FreeBSD, Linux, Solaris) have
    clock_gettime().  Mac OS X and Cygwin have gettimeofday().

    Function time() is C99 and defined in <time.h>.  C99 does not
    mandate the units, but POSIX does (as the number of seconds since
    the epoch: although not mandated, time_t seems always to be an
    integer type).

    Function clock() is C99 and defined in <time.h>.  It measures CPU
    time at CLOCKS_PER_SEC: there is a small danger of integer
    overflow.

    Function times() is POSIX and defined in <sys/times.h>.  It
    returns the elapsed time in clock ticks, plus CPU times in a
    struct tms* argument (also in clock ticks).

    More precise information on CPU times may be available from the
    POSIX function getrusage() defined in <sys/resource.h>.  This
    returns the same time structure as gettimeofday() and on some
    systems offers millisecond resolution.
    It is available on Cygwin, FreeBSD, Mac OS X, Linux and Solaris.

    currentTime() (in this file) uses
    clock_gettime(): AIX, FreeBSD, Linux, Solaris
    gettimeofday():  Mac OS X, Windows, Cygwin
    time() (as ultimate fallback, AFAIK unused).

    proc.time() uses currentTime() for elapsed time,
    and getrusage, then times for CPU times on a Unix-alike,
    GetProcessTimes on Windows.

    devPS.c uses time() and localtime() for timestamps.

    do_date (platform.c) uses ctime.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


#include <errno.h>

#ifdef Win32
/* needed on Windows to avoid redefinition of tzname as _tzname */
# define _NO_OLDNAMES
# include <time.h>
# undef _NO_OLDNAMES
# include <stdint.h>
typedef int64_t R_time_t;
#define time_t R_time_t
# define gmtime R_gmtime
# define localtime R_localtime
# define mktime R_mktime
extern struct tm*  gmtime (const time_t*);
extern struct tm*  localtime (const time_t*);
extern time_t mktime (struct tm*);
# define HAVE_WORKING_64BIT_MKTIME
#else
# include <time.h>
#endif

#include <stdlib.h> /* for setenv or putenv */
#include <Defn.h>
#include <Internal.h>

/* The glibc in RH8.0 was broken and assumed that dates before
   1970-01-01 do not exist.  So does Windows, but its code was replaced
   in R 2.7.0.  As from 1.6.2, test the actual mktime code and cache
   the result on glibc >= 2.2. (It seems this started between 2.2.5
   and 2.3, and RH8.0 had an unreleased version in that gap.)

   Sometime in late 2004 this was reverted in glibc.
*/

static Rboolean have_broken_mktime(void)
{
#if defined(_AIX)
    return TRUE;  // maybe not so for AIX >= 6, which allegedly uses Olson code
#elif defined(__GLIBC__) && defined(__GLIBC_MINOR__) && __GLIBC__ == 2 && __GLIBC_MINOR__ >= 2 &&  __GLIBC_MINOR__ < 10
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
/* There have been 25 leapseconds: see .leap.seconds in R
 */
static int n_leapseconds = 25;
static const time_t leapseconds[] =
{  78796800, 94694400,126230400,157766400,189302400,220924800,252460800,
  283996800,315532800,362793600,394329600,425865600,489024000,567993600,
  631152000,662688000,709948800,741484800,773020800,820454400,867715200,
   915148800,1136073600,1230768000,1341100800};
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

    if(tm->tm_hour == 24 && tm->tm_min == 0 && tm->tm_sec == 0) {
	tm->tm_hour = 0; tm->tm_mday++;
	if(tm->tm_mon >= 0 && tm->tm_mon <= 11) {
	    if(tm->tm_mday > days_in_month[tm->tm_mon] +
	       ((tm->tm_mon==1 && isleap(1900+tm->tm_year) ? 1 : 0))) {
		   tm->tm_mon++; tm->tm_mday = 1;
		   if(tm->tm_mon == 12) {
		       tm->tm_year++; tm->tm_mon = 0;
		   }
	       }
	}
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
	year0 -= (int)(excess * 2000);
    } else if (year0 < 0) {
	excess = -1 - (int)(-year0/2000);
	year0 -= (int)(excess * 2000);
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
       right (since they are usually on Sunday mornings not in Jan/Feb).

       Update for 2.7.0: no one had DST before 1916, so just use the offset
       in 1902, if available.
    */

    memcpy(&oldtm, tm, sizeof(struct tm));
    if(!have_broken_mktime() && tm->tm_year < 2) { /* no DST */
	tm->tm_year = 2;
	mktime(tm);
	offset1 = (double) mktime(tm) - mktime00(tm);
	memcpy(tm, &oldtm, sizeof(struct tm));
	tm->tm_isdst = 0;
	return offset1;
    }
    oldmonth = tm->tm_mon;
    oldmday = tm->tm_mday;
    /* We know there was no DST prior to 1916 */
    oldisdst = (tm->tm_year < 16) ? 0 : tm->tm_isdst;

    /* so now look for a suitable year */
    tm->tm_mon = 6;
    tm->tm_mday = 1;
    tm->tm_isdst = -1;
    mktime00(tm);  /* to get wday valid */
    wday = tm->tm_wday;
    if (oldtm.tm_year > 137) { /* in the unknown future */
	for(i = 130; i < 137; i++) { /* These cover all the possibilities */
	    tm->tm_year = i;
	    mktime(tm);
	    if(tm->tm_wday == wday) break;
	}
    } else { /* a benighted OS with date before 1970 */
	/* We could not use 1970 because of the Windows bug with
	   1970-01-01 east of GMT. */
	for(i = 71; i < 82; i++) { /* These cover all the possibilities */
	    tm->tm_year = i;
	    mktime(tm);
	    if(tm->tm_wday == wday) break;
	}
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

    if(validate_tm(tm) < 0) {
#ifdef EOVERFLOW
	errno = EOVERFLOW;
#else
	errno = 79;
#endif
	return -1.;
    }
    if(!local) return mktime00(tm);

/* OS X 10.9 gives -1 for dates prior to 1902, and ignores DST after 2037 */
#ifdef HAVE_WORKING_64BIT_MKTIME
    if(sizeof(time_t) == 8)
	OK = !have_broken_mktime() || tm->tm_year >= 70;
    else
#endif
	OK = tm->tm_year < 138 && tm->tm_year >= (have_broken_mktime() ? 70 : 02);
    if(OK) {
	res = (double) mktime(tm);
	if (res == -1.) return res;
#ifndef HAVE_POSIX_LEAPSECONDS
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
    int y, tmp, mon, left, diff, diff2;
    struct tm *res = ltm;
    time_t t;

    Rboolean OK;
/* as mktime is broken, do not trust localtime */
#ifdef HAVE_WORKING_64BIT_MKTIME
    if (sizeof(time_t) == 8)
	OK = !have_broken_mktime() || d > 0.;
    else
#endif
	OK = d < 2147483647.0 && 
	    d > (have_broken_mktime() ? 0. : -2147483647.0);
    if(OK) {
	t = (time_t) d;
	/* if d is negative and non-integer then t will be off by one day
	   since we really need floor(). But floor() is slow, so we just
	   fix t instead as needed. */
	if (d < 0.0 && (double) t != d) t--;
#ifndef HAVE_POSIX_LEAPSECONDS
	for(y = 0; y < n_leapseconds; y++) if(t > leapseconds[y] + y - 1) t++;
#endif
	return local ? localtime(&t) : gmtime(&t);
    }

    int day = (int) floor(d/86400.0);
    left = (int) (d - day * 86400.0 + 1e-6); // allow for fractional secs

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
	double shift;
	/*  daylight saving time is unknown */
	res->tm_isdst = -1;

	/* Try to fix up time zone differences: cf PR#15480 */
	diff = (int)(guess_offset(res)/60);
	// just in case secs are out of range and might affect this.
	shift = 60.*res->tm_hour + res->tm_min + res->tm_sec/60.;
	res->tm_min -= diff;
	validate_tm(res);
	res->tm_isdst = -1;
	/* now this might be a different day */
	if(shift - diff < 0.) {
	    res->tm_yday--;
	    res->tm_wday--;
	}
	else if(shift - diff >= 24. * 60.) {
	    res->tm_yday++;
	    res->tm_wday++;
	}
	diff2 = (int)(guess_offset(res)/60);
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


/* clock_gettime, timespec_get time are in <time.h>, already included */
#ifdef HAVE_SYS_TIME_H
/* gettimeoday, including on Windows */
# include <sys/time.h>
#endif

double currentTime(void)
{
    double ans = NA_REAL;

#ifdef HAVE_TIMESPEC_GET
    struct timespec tp;
    int res = timespec_get(&tp, TIME_UTC);
    if(res != 0)
	ans = (double) tp.tv_sec + 1e-9 * (double) tp.tv_nsec;
#elif defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
    /* Has 2038 issue if time_t: tv.tv_sec is 32-bit. */
    struct timespec tp;
    int res = clock_gettime(CLOCK_REALTIME, &tp);
    if(res == 0)
	ans = (double) tp.tv_sec + 1e-9 * (double) tp.tv_nsec;

#elif defined(HAVE_GETTIMEOFDAY)
    /* Mac OS X, mingw.org, used on mingw-w64.
       Has 2038 issue if time_t: tv.tv_sec is 32-bit.
     */
    struct timeval tv;
    int res = gettimeofday(&tv, NULL);
    if(res == 0)
	ans = (double) tv.tv_sec + 1e-6 * (double) tv.tv_usec;

#else
    /* No known current OSes */
    time_t res = time(NULL);
    if(res != (time_t)(-1)) /* -1 must be an error as the real value -1
			       was ca 1969 */
	ans = (double) res;
#endif

#ifndef HAVE_POSIX_LEAPSECONDS
    /* No known current OSes */
    /* Disallowed by POSIX (1988-):
       http://www.mail-archive.com/leapsecs@rom.usno.navy.mil/msg00109.html
       http://en.wikipedia.org/wiki/Unix_time
    */
    if (!ISNAN(ans)) {
	ans -= n_leapseconds;
    }
#endif
    return ans;
}

SEXP attribute_hidden do_systime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    return ScalarReal(currentTime());
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for getpid */
#endif

/* For RNG.c, main.c, mkdtemp.c */
attribute_hidden
unsigned int TimeToSeed(void)
{
    unsigned int seed, pid = getpid();
#if defined(HAVE_CLOCK_GETTIME) && defined(CLOCK_REALTIME)
    {
	struct timespec tp;
	clock_gettime(CLOCK_REALTIME, &tp);
	seed = (unsigned int)(((uint_least64_t) tp.tv_nsec << 16) ^ tp.tv_sec);
    }
#elif defined(HAVE_GETTIMEOFDAY)
    {
	struct timeval tv;
	gettimeofday (&tv, NULL);
	seed = (unsigned int)(((uint_least64_t) tv.tv_usec << 16) ^ tv.tv_sec);
    }
#else
    /* C89, so must work */
    seed = (Int32) time(NULL);
#endif
    seed ^= (pid <<16);
    return seed;
}


#ifdef Win32
extern void tzset(void);
/* tzname is in the headers as an import on MinGW-w64 */
#define tzname Rtzname
extern char *Rtzname[2];
#elif defined(__CYGWIN__)
extern __declspec(dllimport) char *tzname[2];
#else
extern char *tzname[2];
#endif

static int set_tz(const char *tz, char *oldtz)
{
    char *p = NULL;
    int settz = 0;

    strcpy(oldtz, "");
    p = getenv("TZ");
    if(p) {
	if (strlen(p) > 1000)
	    error("time zone specification is too long");
	strcpy(oldtz, p);
    }
#ifdef HAVE_SETENV
    if(setenv("TZ", tz, 1)) warning(_("problem with setting timezone"));
    settz = 1;
#elif defined(HAVE_PUTENV)
    {
	static char buff[1010];
	if (strlen(tz) > 1000)
	    error("time zone specification is too long");
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


static void 
makelt(struct tm *tm, SEXP ans, R_xlen_t i, int valid, double frac_secs)
{
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
	for(int j = 1; j < 8; j++)
	    INTEGER(VECTOR_ELT(ans, j))[i] = NA_INTEGER;
	INTEGER(VECTOR_ELT(ans, 8))[i] = -1;
    }
}


SEXP attribute_hidden do_asPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans, ansnames, klass, tzone;
    int isgmt = 0, valid, settz = 0;
    char oldtz[1001] = "";
    const char *tz = NULL;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) {
	    stz = mkString(p); /* make a copy */
	    tz = CHAR(STRING_ELT(stz, 0));
	}
    }
    PROTECT(stz); /* it might be new */
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    R_xlen_t n = XLENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(int i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(int i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(R_xlen_t i = 0; i < n; i++) {
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
    SET_STRING_ELT(klass, 0, mkChar("POSIXlt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXt"));
    classgets(ans, klass);
    if (isgmt) {
	PROTECT(tzone = mkString(tz));
    } else {
	PROTECT(tzone = allocVector(STRSXP, 3));
	SET_STRING_ELT(tzone, 0, mkChar(tz));
	SET_STRING_ELT(tzone, 1, mkChar(tzname[0]));
	SET_STRING_ELT(tzone, 2, mkChar(tzname[1]));
    }
    setAttrib(ans, install("tzone"), tzone);
    UNPROTECT(6);

    if(settz) reset_tz(oldtz);
    return ans;
}

SEXP attribute_hidden do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP stz, x, ans;
    R_xlen_t n = 0, nlen[9];
    int isgmt = 0, settz = 0;
    char oldtz[20] = "";
    const char *tz = NULL;
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
	if(p) {
	    stz = mkString(p);
	    tz = CHAR(STRING_ELT(stz, 0));
	}
    }
    PROTECT(stz); /* it might be new */
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    for(int i = 0; i < 6; i++)
	if((nlen[i] = XLENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = XLENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(int i = 0; i < 6; i++)
	    if(nlen[i] == 0)
		error(_("zero-length component in non-empty \"POSIXlt\" structure"));
	if(nlen[8] == 0)
	    error(_("zero-length component in non-empty \"POSIXlt\" structure"));
    }
    /* coerce fields to integer or real */
    SET_VECTOR_ELT(x, 0, coerceVector(VECTOR_ELT(x, 0), REALSXP));
    for(int i = 0; i < 6; i++)
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i),
					  i > 0 ? INTSXP: REALSXP));
    SET_VECTOR_ELT(x, 8, coerceVector(VECTOR_ELT(x, 8), INTSXP));

    PROTECT(ans = allocVector(REALSXP, n));
    for(R_xlen_t i = 0; i < n; i++) {
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	// avoid (int) NAN
	tm.tm_sec   = R_FINITE(secs) ? (int) fsecs: NA_INTEGER;
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
	    errno = 0;
	    tmp = mktime0(&tm, 1 - isgmt);
#ifdef MKTIME_SETS_ERRNO
	    REAL(ans)[i] = errno ? NA_REAL : tmp + (secs - fsecs);
#else
	    REAL(ans)[i] = ((tmp == -1.)
			    /* avoid silly gotcha at epoch minus one sec */
			    && (tm.tm_sec != 59)
			    && ((tm.tm_sec = 58), (mktime0(&tm, 1 - isgmt) != -2.))
			    ) ?
	      NA_REAL : tmp + (secs - fsecs);
#endif
	}
    }

    if(settz) reset_tz(oldtz);

    UNPROTECT(3);
    return ans;
}

SEXP attribute_hidden do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, sformat, ans, tz;
    R_xlen_t i, n = 0, m, N, nlen[9];
    int UseTZ, settz = 0;
    char buff[300];
    char oldtz[20] = "";
    const char *p, *tz1;
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = duplicate(CAR(args))); /* coerced below */
    if(!isVectorList(x) || LENGTH(x) != 9)
	error(_("invalid '%s' argument"), "x");
    if(!isString((sformat = CADR(args))) || XLENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "format");
    m = XLENGTH(sformat);
    UseTZ = asLogical(CADDR(args));
    if(UseTZ == NA_LOGICAL)
	error(_("invalid '%s' argument"), "usetz");
    tz = getAttrib(x, install("tzone"));

    if (!isNull(tz) && strlen(tz1 = CHAR(STRING_ELT(tz, 0)))) {
	/* If the format includes %Z or %z
	   we need to try to set TZ accordingly */
	int needTZ = 0;
	for(i = 0; i < m; i++) {
	    const char *p = CHAR(STRING_ELT(sformat, i));
	    if (strstr(p, "%Z") || strstr(p, "%z")) {needTZ = 1; break;}
	}
	if(needTZ) settz = set_tz(tz1, oldtz);
    }

    /* workaround for glibc/FreeBSD/MacOS X bugs in strftime: they have
       non-POSIX/C99 time zone components
     */
    memset(&tm, 0, sizeof(tm));

    /* coerce fields to integer or real, find length of longest one */
    for(i = 0; i < 9; i++) {
	nlen[i] = XLENGTH(VECTOR_ELT(x, i));
	if(nlen[i] > n) n = nlen[i];
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i),
					  i > 0 ? INTSXP : REALSXP));
    }
    if(n > 0) {
	for(i = 0; i < 9; i++)
	    if(nlen[i] == 0)
		error(_("zero-length component in non-empty \"POSIXlt\" structure"));
    }
    if(n > 0) N = (m > n) ? m:n; else N = 0;
    PROTECT(ans = allocVector(STRSXP, N));
    for(i = 0; i < N; i++) {
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	// avoid (int) NAN
	tm.tm_sec   = R_FINITE(secs) ? (int) fsecs: NA_INTEGER;
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
		const char *q = CHAR(STRING_ELT(sformat, i%m));
		int n = (int) strlen(q) + 50;
		char buf2[n];
#ifdef Win32
		/* We want to override Windows' TZ names */
		p = strstr(q, "%Z");
		if (p) {
		    memset(buf2, 0, n);
		    strncpy(buf2, q, p - q);
		    strcat(buf2, tm.tm_isdst > 0 ? tzname[1] : tzname[0]);
		    strcat(buf2, p+2);
		} else
#endif
		    strcpy(buf2, q);

		p = strstr(q, "%OS");
		if(p) {
		    /* FIXME some of this should be outside the loop */
		    int ns, nused = 4;
		    char *p2 = strstr(buf2, "%OS");
		    *p2 = '\0';
		    ns = *(p+3) - '0';
		    if(ns < 0 || ns > 9) { /* not a digit */
			ns = asInteger(GetOption1(install("digits.secs")));
			if(ns == NA_INTEGER) ns = 0;
			nused = 3;
		    }
		    if(ns > 6) ns = 6;
		    if(ns > 0) {
			/* truncate to avoid nuisances such as PR#14579 */
			double s = secs, t = pow(10.0, (double) ns);
			s = ((int) (s*t))/t;
			sprintf(p2, "%0*.*f", ns+3, ns, s);
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
    if(settz) reset_tz(oldtz);
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
    int invalid, isgmt = 0, settz = 0, offset;
    struct tm tm, tm2, *ptm = &tm;
    const char *tz = NULL;
    char oldtz[20] = "";
    double psecs = 0.0;
    R_xlen_t i, n, m, N;

    checkArity(op, args);
    if(!isString((x= CAR(args))))
	error(_("invalid '%s' argument"), "x");
    if(!isString((sformat = CADR(args))) || XLENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "x");
    if(!isString((stz = CADDR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) {
	    stz = mkString(p);
	    tz = CHAR(STRING_ELT(stz, 0));
	}
    }
    PROTECT(stz); /* it might be new */
    if(strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) isgmt = 1;
    if(!isgmt && strlen(tz) > 0) settz = set_tz(tz, oldtz);

    n = XLENGTH(x); m = XLENGTH(sformat);
    if(n > 0) N = (m > n) ? m : n; else N = 0;

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
	offset = NA_INTEGER;
	invalid = STRING_ELT(x, i%n) == NA_STRING ||
	    !R_strptime(CHAR(STRING_ELT(x, i%n)),
			CHAR(STRING_ELT(sformat, i%m)), &tm, &psecs, &offset);
	if(!invalid) {
	    /* Solaris sets missing fields to 0 */
	    if(tm.tm_mday == 0) tm.tm_mday = NA_INTEGER;
	    if(tm.tm_mon == NA_INTEGER || tm.tm_mday == NA_INTEGER
	       || tm.tm_year == NA_INTEGER)
		glibc_fix(&tm, &invalid);
	    tm.tm_isdst = -1;
	    if (offset != NA_INTEGER) {
		/* we know the offset, but not the timezone
		   so all we can do is to convert to time_t,
		   adjust and convert back */
		double t0;
		memcpy(&tm2, &tm, sizeof(struct tm));
		t0 = mktime0(&tm2, 0);
		if (t0 != -1) {
		    t0 -= offset; /* offset = -0800 is Seattle */
		    ptm = localtime0(&t0, 1-isgmt, &tm2);
		} else invalid = 1;
	    } else {
		/* we do want to set wday, yday, isdst, but not to
		   adjust structure at DST boundaries */
		memcpy(&tm2, &tm, sizeof(struct tm));
		mktime0(&tm2, 1-isgmt); /* set wday, yday, isdst */
		tm.tm_wday = tm2.tm_wday;
		tm.tm_yday = tm2.tm_yday;
		tm.tm_isdst = isgmt ? 0: tm2.tm_isdst;
	    }
	    invalid = validate_tm(&tm) != 0;
	}
	makelt(ptm, ans, i, !invalid, psecs - floor(psecs));
    }

    setAttrib(ans, R_NamesSymbol, ansnames);
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("POSIXlt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXt"));
    classgets(ans, klass);
    if (isgmt) {
	PROTECT(tzone = mkString(tz));
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

    UNPROTECT(4);
    return ans;
}

SEXP attribute_hidden do_D2POSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans, ansnames, klass;
    R_xlen_t n, i;
    int valid, day, y, tmp, mon;
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), REALSXP));
    n = XLENGTH(x);
    PROTECT(ans = allocVector(VECSXP, 9));
    for(i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));

    PROTECT(ansnames = allocVector(STRSXP, 9));
    for(i = 0; i < 9; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(i = 0; i < n; i++) {
	if(R_FINITE(REAL(x)[i])) {
	    day = (int) floor(REAL(x)[i]);
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
    SET_STRING_ELT(klass, 0, mkChar("POSIXlt"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXt"));
    classgets(ans, klass);
    setAttrib(ans, install("tzone"), mkString("UTC"));
    UNPROTECT(4);

    return ans;
}

SEXP attribute_hidden do_POSIXlt2D(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans, klass;
    R_xlen_t i, n = 0, nlen[9];
    struct tm tm;

    checkArity(op, args);
    PROTECT(x = duplicate(CAR(args)));
    if(!isVectorList(x) || LENGTH(x) != 9)
	error(_("invalid '%s' argument"), "x");

    for(i = 3; i < 6; i++)
	if((nlen[i] = XLENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = XLENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(i = 3; i < 6; i++)
	    if(nlen[i] == 0)
		error(_("zero-length component in non-empty \"POSIXlt\" structure"));
	if(nlen[8] == 0)
	    error(_("zero-length component in non-empty \"POSIXlt\" structure"));
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
	    /* -1 must be error as seconds were zeroed */
	    double tmp = mktime00(&tm);
	    REAL(ans)[i] = (tmp == -1) ? NA_REAL : tmp/86400;
	}
    }

    PROTECT(klass = mkString("Date"));
    classgets(ans, klass);
    UNPROTECT(3);
    return ans;
}
