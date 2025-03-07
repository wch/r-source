/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2025  The R Core Team.
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
 *
 *
 *      Interfaces to POSIX date-time conversion functions.
 */

/*
    These use POSIX functions which are also part of C99 so are almost
    universally available, albeit with more room for implementation
    variations.

    A particular problem is the setting of the timezone TZ on
    Unix/Linux.  POSIX appears to require it, yet older Linux systems
    do not set it and do not give the correct results/crash strftime
    if it is not set (or even if it is: see the workaround below).  We
    use unsetenv() to work around this: that is a BSD (and POSIX 2001)
    construct but seems to be available on the affected platforms.

    The system date-time entry points we use (or substitute) are

    gmtime_r (or gmtime)
    localtime_r (or localtime)
    mktime
    strftime (in do_formatPOSIXlt)
    tzname
    tzset

    PATH 2) also uses R_timegm and R_tzsetwall (a version of tzset
    which ignores TZ and loads the current timezone).

    Via Rstrptime.h we use the system strftime or wcsftime to get the
    language-specific names.
*/

/*
  R class "POSIXlt" is a list of 9 components, with two optional ones
  (but always added as from R 4.3.0).

  Objects of this class are most often created from character inputs
  via strptime() (called by as.POSIXlt.character) or from "POSIXct"
  objects. In the first case they may or may not have an associated
  time zone: in the second they must.

  On a system with tm_gmtoff (all current platforms) the components
  zone and gmtoff are included, but gmtoff may be NA and usually will
  be unless the value was supplied by strptime(, "%z") or by
  conversion from "POSIXct" or "Date".

  There will usually be a "tzone" attribute, of length 1 if only the
  name is known or is "UTC', of length 3 including the abbreviations
  for all other timezones. (If the timezone does not use DST, the
  second abbreviation may be empty or may repeat the first, depending
  on the platform., but it will always be present.)  However, if the
  call to strptime() does not specify 'tz', this attribute is omitted.

  Names for the date-times are optional (and rarely supplied):
  they are attached to the 'year' element' and used by strptime().
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

// to get tm_zone, tm_gmtoff defined in glibc.
// some other header, e.g. math.h, might define the macro so do this first
#if defined HAVE_FEATURES_H
# include <features.h>
# ifdef __GNUC_PREREQ
#  if __GNUC_PREREQ(2,20) && !defined(_DEFAULT_SOURCE_)
#   define _DEFAULT_SOURCE 1
#  endif
# endif
#endif
#if defined(HAVE_GLIBC2) && !defined(_DEFAULT_SOURCE_) && !defined(_BSD_SOURCE)
# define _BSD_SOURCE 1
#endif

/*
  glibc (with these macros), macOS and internal tzcode all have tm_zone
  and tm_gmtoff fields in struct tm.

  musl has __tm_zone and __tm_gmtoff, which it re-defines without __
  if _BSD_SOURCE or _GNU_SOURCE is defined.  But with no macro to
  detect musl.  However, they were redefined on the tested Alpine
  Linux system which did define _GNU_SOURCE.
*/

#include <time.h>
#include <errno.h> // mktime or substitute may set errno.
#include <Rmath.h> // for imin2()

/*

There are two implementation paths here.

1) Use the system functions for mktime, gmtime[_r], localtime[_r], strftime.
   Use the system time_t, struct tm and time-zone tables.

   This can be use on glibc, macOS and Solaris (and probably FreeBSD),
   but all except 64-bit glibc have issues we can try to work around.
   It could in principle be used om Windows but the issues there are
   too severe (no support for before 1970) to work around.

   The system facilities are used for 1902-2037 and outside those
   limits where there is a 64-bit time_t and the conversions work
   (some OSes have only 32-bit time-zone tables and macOS 13 only
   works from 1900).  Otherwise there is code below to extrapolate
   from 1902-2037.

   Other known issues are with strftime (macOS only supports offsets
   in multiple of half-hours), not having tzdata tables (possible on
   Alpine and now fatal when configuring) and odd issues reading the
   time-zone tables, especially for 1939-1945.

2) USE_INTERNAL_MKTIME : Use substitutes from src/extra/tzone for
   mktime, gmtime_r, localtime_r, strftime with a R_ prefix.  The
   system strftime is used for locale-dependent names in R_strptime
   and R_strftime.  This uses the time-zone tables shipped with R and
   installed into R_HOME/share/zoneinfo , with facilities to switch to
   others using environment variable TZDIR.

   Our own versions of time_t (64-bit) and struct tm (including the
   BSD-style fields tm_zone and tm_gmtoff) are used.

   PATH 2) was added for R 3.1.0 (2014-04), is the only one
   supported on Windows and is the current default on macOS.

*/

#ifdef USE_INTERNAL_MKTIME
// PATH 2)
# include "datetime.h"
// configure might have checked the system versions, so we override.
# undef HAVE_LOCALTIME_R
# define HAVE_LOCALTIME_R 1
# undef HAVE_TM_ZONE
# define HAVE_TM_ZONE 1
# undef HAVE_TM_GMTOFF
# define HAVE_TM_GMTOFF 1
// latterly these are set by configure, but not on Windows
# undef MKTIME_SETS_ERRNO
# define MKTIME_SETS_ERRNO
// these should only be used in PATH 1), but we set them for completeness.
# undef HAVE_WORKING_MKTIME_AFTER_2037
# define HAVE_WORKING_MKTIME_AFTER_2037 1
# undef HAVE_WORKING_MKTIME_BEFORE_1902
# define HAVE_WORKING_MKTIME_BEFORE_1902 1
# undef HAVE_WORKING_MKTIME_BEFORE_1970
# define HAVE_WORKING_MKTIME_BEFORE_1970 1
#else // PATH 1)

typedef struct tm stm;
#define R_tzname tzname
extern char *tzname[2];

#endif

#include <stdlib.h> /* for setenv or putenv */
#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

#ifndef USE_INTERNAL_MKTIME
/* PATH 1 */
static bool warn1902 = false;
#endif

/* Substitute based on glibc code. */
#include "Rstrptime.h"
/* --> Def.  R_strptime()  etc */

static const int month_days[12] =
  {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

// Careful : days_in_year is for base-0 years, days_in_month for base-1970.
#define isleap(y) ((((y) % 4) == 0 && ((y) % 100) != 0) || ((y) % 400) == 0)
#define days_in_year(year) (isleap(year) ? 366 : 365)
#define days_in_month(mon, yr) ((mon == 1 && isleap(1900+yr)) ? 29 : month_days[mon])

/*
  Adjust a struct tm to be a valid scalar date-time.
  Return 0 if valid, -1 if invalid and uncorrectable, or a positive
  integer approximating the number of corrections done.

  Used in both paths in mktime0, in localtime0 in PATH 1) and in
  do_formatPOSIXlt, do_strptime, do_POSIXlt2D, do_balancePOSIXlt.
*/
static int validate_tm (stm *tm)
{
    int tmp, res = 0;

    if (tm->tm_sec < 0 || tm->tm_sec > 60) { /* 61 POSIX, 60 draft ISO C */
	res++;
	int tmp = tm->tm_sec/60;
	tm->tm_sec -= 60 * tmp; tm->tm_min += tmp;
	if(tm->tm_sec < 0) {tm->tm_sec += 60; tm->tm_min--;}
    }

    if (tm->tm_min < 0 || tm->tm_min > 59) {
	res++;
	int tmp = tm->tm_min/60;
	tm->tm_min -= 60 * tmp; tm->tm_hour += tmp;
	if(tm->tm_min < 0) {tm->tm_min += 60; tm->tm_hour--;}
    }

    if(tm->tm_hour == 24 && tm->tm_min == 0 && tm->tm_sec == 0) { /* 24:00:00 */
	tm->tm_hour = 0; tm->tm_mday++;
	if(tm->tm_mon >= 0 && tm->tm_mon <= 11) {
	    if(tm->tm_mday > days_in_month(tm->tm_mon, tm->tm_year)) {
		tm->tm_mon++; tm->tm_mday = 1;
		if(tm->tm_mon == 12) {
		    tm->tm_year++; tm->tm_mon = 0;
		}
	    }
	}
    }
    else if (tm->tm_hour < 0 || tm->tm_hour > 23) {
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

    /* A limit on the loops of about 3000x round.
       We could spin backwards or forwards in multiples of 400 years.
     */
    if(tm->tm_mday < -1000000 || tm->tm_mday > 1000000) return -1;

    if(abs(tm->tm_mday) > 366) {
	res++;
	/* first spin back until January */
	while(tm->tm_mon > 0) {
	    --tm->tm_mon;
	    tm->tm_mday += days_in_month(tm->tm_mon, tm->tm_year);
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
	tm->tm_mday += days_in_month(tm->tm_mon, tm->tm_year);
    }

    while(tm->tm_mday > (tmp = days_in_month(tm->tm_mon, tm->tm_year))) {
	res++;
	if(++tm->tm_mon > 11) {tm->tm_mon -= 12; tm->tm_year++;}
	tm->tm_mday -= tmp;
    }
    return res;
} // validate_tm

/*
  glibc and internal strftime are subject to integer overflow when
  tm->tm_year + 1900 does not fit into an integer
*/
static int likely_strftime_overflow (stm *tm)
{
  double year = 1900.0 + tm->tm_year;

#if SIZEOF_INT <= 4
  return (year > INT_MAX || year < INT_MIN);
#else
  /* err on the safe side to avoid surprise due to imprecise floating point
     representation of the limits */
  return !(year < INT_MAX && year > INT_MIN);
#endif
}

/*
   days_in_year is the same for year mod 400.
   We could avoid loops altogether by computing how many leap years
   there are between 1900 + tm->tm_year and 1900.

   This will fix up tm->tm_yday and tm->tm_wday.

   Used in timegm00 (possibly) and guess_offset in PATH 1),
   POSIXlt2D and do_balancePOSIXlt
*/
static double mkdate00 (stm *tm)
{
    if(tm->tm_mday == NA_INTEGER || tm->tm_year == NA_INTEGER
       || tm->tm_mon == NA_INTEGER) {
	tm->tm_yday = tm->tm_wday = NA_INTEGER;
	return NA_REAL;
    }

    int day = tm->tm_mday - 1,	/* not ok if it's NA_INTEGER */
	year0 = 1900 + tm->tm_year;
    double excess = 0.0;
    if (year0 >= 400) {
	excess = (int)(year0/400) - 1;
	year0 -= (int)(excess * 400);
    } else if (year0 < 0) {
	excess = -1 - (int)(-year0/400);
	year0 -= (int)(excess * 400);
    }

    for(int i = 0; i < tm->tm_mon; i++) day += month_days[i];
    if (tm->tm_mon > 1 && isleap(year0)) day++;
    tm->tm_yday = day;

    if (year0 > 1970) {
	for (int year = 1970; year < year0; year++)
	    day += days_in_year(year);
    } else if (year0 < 1970) {
	for (int year = 1969; year >= year0; year--)
	    day -= days_in_year(year);
    }

    /* weekday: Epoch day was a Thursday */
    if ((tm->tm_wday = ((day % 7) + 4) % 7) < 0) tm->tm_wday += 7;

    return (day + excess * 146097);
}


/* if d is negative and non-integer then t will be off by one second
   since we really need floor(). But floor() is slow, so we just
   fix t instead as needed. */
#define LOCALTIME_MK_(t_)			\
        time_t t_ = (time_t) d;			\
	if (d < 0. && d != (double) t_) t_--


#ifdef USE_INTERNAL_MKTIME
/*
   PATH 2), internal tzcode

   Interface to mktime or timegm, version in each PATH. This is PATH 2).
   Called from do_asPOSIXct and do_strptime.
*/
static double mktime0 (stm *tm, const int local)
{
    if(validate_tm(tm) < 0) {
#ifdef EOVERFLOW
	errno = EOVERFLOW;
#else
	errno = 79;
#endif
	return -1.;
    }
    return local ? R_mktime(tm) : R_timegm(tm);
}

/*
   Interface to localtime_r or gmtime_r.
   Version in each PATH.  This is PATH 2).
   Used in do_asPOSIXlt and do_strptime.
*/
static stm * localtime0(const double *tp, const int local, stm *ltm)
{
    double d = *tp;
    LOCALTIME_MK_(t);
    return local ? R_localtime_r(&t, ltm) : R_gmtime_r(&t, ltm);
}

#else
//--------------------------------------------------------- long clause ----
// PATH 1), using system functions.
/*
   Substitute for timegm (which is non-POSIX) -- with no checking.  Also,
   returns double and needs to be wider than a 32-bit time_t.  So we
   could use timegm if it exists _and_ time_t is 64-bit _and_ it supports
   a full range of dates (macOS's does not).

   Used in guess_offset, mktime0 in PATH 1).
*/
static double timegm00 (stm *tm)
{
    // NA handling may no longer be needed, but left in for safety
    // (it caused UBSAN errors).
    double day = mkdate00(tm); // handles NA inputs
    if (day == NA_REAL) return NA_REAL;
    return tm->tm_sec + (tm->tm_min * 60) + (tm->tm_hour * 3600)
	+ day * 86400.0;
}

// no known examples recently
#ifndef HAVE_POSIX_LEAPSECONDS
static int n_leapseconds = 27; // 2017-01, sync with .leap.seconds in R (!)
static const time_t leapseconds[] = // dput(unclass(.leap.seconds)) :
{  78796800, 94694400,126230400,157766400,189302400,220924800,252460800,
  283996800,315532800,362793600,394329600,425865600,489024000,567993600,
  631152000,662688000,709948800,741484800,773020800,820454400,867715200,
   915148800,1136073600,1230768000,1341100800,1435708800,1483228800};
#endif

static double guess_offset (stm *tm)
{
    double offset, offset1, offset2;
    int i, wday, year, oldmonth, oldisdst, oldmday;
    stm oldtm;
    /*
       Adjust as best we can for timezones: if isdst is unknown, use
       the smaller offset at same day in Jan or July of a valid year.
       We don't know the timezone rules, but if we choose a year with
       July 1 on the same day of the week we will likely get guess
       right (since they are usually on Sunday mornings not in Jan/Feb).

       Update for R 2.7.0: no one had DST before 1916, so just use the offset
       in 1902, if available.
    */

    memcpy(&oldtm, tm, sizeof(stm));
    if(tm->tm_year < 2) { /* no DST */
	tm->tm_year = 2;
	mktime(tm);
	offset1 = (double) mktime(tm) - timegm00(tm);
	memcpy(tm, &oldtm, sizeof(stm));
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
    mkdate00(tm); // fixes tm_wday and tm_yday
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
    offset1 = (double) mktime(tm) - timegm00(tm);
    /* and in July */
    tm->tm_year = year;
    tm->tm_mon = 6;
    tm->tm_isdst = -1;
    offset2 = (double) mktime(tm) - timegm00(tm);
    if(oldisdst > 0) {
	offset = (offset1 > offset2) ? offset2 : offset1;
    } else {
	offset = (offset1 > offset2) ? offset1 : offset2;
    }
    /* now try to guess dst if unknown */
    tm->tm_mon = oldmonth;
    tm->tm_isdst = -1;
    if(oldisdst < 0) {
	offset1 = (double) mktime(tm) - timegm00(tm);
	oldisdst = (offset1 < offset) ? 1:0;
	if(oldisdst) offset = offset1;
    }
    /* restore all as mktime might alter it */
    memcpy(tm, &oldtm, sizeof(stm));
    /* and then set isdst */
    tm->tm_isdst = oldisdst;
    return offset;
}

/*
   Interface to mktime or timegm00, version in each PATH.
   Called from do_asPOSIXct and do_strptime.

   This is the version for PATH 1)
*/
static double mktime0 (stm *tm, const int local)
{
    double res;
    bool OK;

    if(validate_tm(tm) < 0) {
#ifdef EOVERFLOW
	errno = EOVERFLOW;
#else
	errno = 79;
#endif
	return -1.;
    }
    if(!local) return timegm00(tm);

/*
   Platforms with a 32-bit time_t will fail before 1901-12-13 and after 2038-01-19.
   macOS 10.9 gave -1 for dates prior to 1902 and ignored DST after 2037
   macOS 13 gives -1 for dates prior to 1900
   Windows UCRT (and in 2004) gives -1 for dates prior to 1970
   https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_16
   states 'the relationship is undefined' prior to 1970.
   glibc from 2.2.5 until late 2004 also gave -1 for such dates.
*/
    if(sizeof(time_t) == 8) {
	OK = true;
#ifndef HAVE_WORKING_MKTIME_AFTER_2037
	OK = OK && tm->tm_year < 138;
#endif
#ifndef HAVE_WORKING_MKTIME_BEFORE_1902
	OK = OK && tm->tm_year >= 02;
	if (tm->tm_year < 02) {
	    if(!warn1902)
		warning(_("datetimes before 1902 may not be accurate: warns once per session"));
	    warn1902 = true;
	}
#endif
#ifndef HAVE_WORKING_MKTIME_BEFORE_1970
	OK = OK && tm->tm_year >= 70;
#endif
    } else {  // 32-bit time_t
	OK = tm->tm_year < 138 && tm->tm_year >= 02;
	if (tm->tm_year < 02) {
	    if(!warn1902)
		warning(_("datetimes before 1902 may not be accurate: warns once per session"));
	    warn1902 = true;
	}
#ifndef HAVE_WORKING_MKTIME_BEFORE_1970
	OK = OK && tm->tm_year >= 70;
#endif
    }
    if(OK) {
	res = (double) mktime(tm);
	if (res == -1.) return res;
#ifndef HAVE_POSIX_LEAPSECONDS
	for(int i = 0; i < n_leapseconds; i++)
	    if(res > leapseconds[i]) res -= 1.0;
#endif
	return res;
/* watch the side effect here: both calls alter their arg */
    } else {
	double offset = guess_offset(tm);
#ifdef HAVE_TM_GMTOFF
	tm->tm_gmtoff = (long)offset;
#endif
	return offset + timegm00(tm);
    }
}

/*
   Interface to localtime[_r] or gmtime[_r] or internal substitute.
   Version in each PATH: this is PATH 1)
   Used in do_asPOSIXlt and do_strptime.
*/
static stm * localtime0(const double *tp, const int local, stm *ltm)
{
    double d = *tp;

    bool OK = true;;
/* as mktime is broken, do not trust localtime */
    if (sizeof(time_t) == 8) {
	OK = true;
#ifndef HAVE_WORKING_MKTIME_AFTER_2037
	OK = OK && d < 2147483647.0;
#endif
#ifndef HAVE_WORKING_MKTIME_BEFORE_1902
	if (d <= -2147483647.0) {
	    if(!warn1902)
		warning(_("datetimes before 1902 may not be accurate: warns once per session"));
	    warn1902 = true;
	    OK = false;
	}
	OK = OK && d > -2147483647.0;
#endif
#ifndef HAVE_WORKING_MKTIME_BEFORE_1970
	OK = OK && d >= 0.0;
#endif
    } else { // 32-bit time_t
	if (d <= -2147483647.0) {
	    if(!warn1902)
		warning(_("datetimes before 1902 may not be accurate: warns once per session"));
	    warn1902 = true;
	    OK = false;
	}
	OK = OK && d < 2147483647.0;
#ifndef HAVE_WORKING_MKTIME_BEFORE_1970
	OK = OK && d >= 0.0;
#endif
    }
    if(OK) {
	LOCALTIME_MK_(t);
#ifndef HAVE_POSIX_LEAPSECONDS
	for(int y = 0; y < n_leapseconds; y++) if(t > leapseconds[y] + y - 1) t++;
#endif
	// Recent Linux and macOS have localtime_r
#ifdef HAVE_LOCALTIME_R
	return local ? localtime_r(&t, ltm) : gmtime_r(&t, ltm);
#else
	return local ? localtime(&t) : gmtime(&t);
#endif
    } // end of OK

    // internal substitute code.

    double dday = floor(d/86400.0);
//    static stm ltm0, *res = &ltm0;
    stm *res = ltm; // be like localtime_r
    // This cannot exceed (2^31-1) years in either direction from 1970
    if (fabs(dday) > 784368402400) { //bail out
	res->tm_year = NA_INTEGER;
	res->tm_mon = NA_INTEGER;
	res->tm_mday = NA_INTEGER;
	res->tm_yday = NA_INTEGER;
	res->tm_wday = NA_INTEGER;
	res->tm_hour = NA_INTEGER;
	res->tm_min = NA_INTEGER;
	res->tm_sec = NA_INTEGER;
	res->tm_isdst = -1;
	return res;
    }
    int left = (int) (d - dday * 86400.0 + 1e-6); // allow for fractional secs

    memset(res, 0, sizeof(stm));
    /* hour, min, and sec */
    res->tm_hour = left / 3600;
    left %= 3600;
    res->tm_min = left / 60;
    res->tm_sec = left % 60;

    /* weekday: 1970-01-01 was a Thursday */
    int tmp = (int)(dday - 7 * floor(dday/7)); // day % 7
    if ((res->tm_wday = ((tmp + 4) % 7)) < 0) res->tm_wday += 7;

    /* year & day within year */
    /* every 400 years is exactly 146097 days long and the
       pattern is repeated */
    double rounds = floor(floor(dday) / 146097.0);
    dday -= 146097.0 * rounds;
    int y = (int)(1970 + rounds * 400);
    if (dday >= 0)
	for ( ; dday >= (tmp = days_in_year(y)); dday -= tmp, y++);
    else
	for ( ; dday < 0; --y, dday += days_in_year(y) );

    y = res->tm_year = y - 1900;
    int day = (int) dday;
    res->tm_yday = day;

    /* month within year */
    int mon;
    for (mon = 0;
	 day >= (tmp = days_in_month(mon, y)); // days_in_month in 1900-based
	 day -= tmp, mon++);
    res->tm_mon = mon;
    res->tm_mday = day + 1;

    if(local) {
	/*  daylight saving time is unknown */
	res->tm_isdst = -1;
	/* Try to fix up time zone differences: cf PR#15480 */
	int sdiff = (int) guess_offset(res);

	/* New strategy in 4.3.0: convert as if in UTC, then guess
	   offset and redo. */
	d -= sdiff;
	dday = floor(d/86400.0);
	left = (int) (d - dday * 86400.0 + 1e-6);
	res->tm_hour = left / 3600;
	left %= 3600;
	res->tm_min = left / 60;
	res->tm_sec = left % 60;
	tmp = (int)(dday - 7 * floor(dday/7)); // day % 7
	if ((res->tm_wday = ((tmp + 4) % 7)) < 0) res->tm_wday += 7;
	rounds = floor(floor(dday) / 146097.0);
	dday -= 146097.0 * rounds;
	int y = (int)(1970 + rounds * 400);
	if (dday >= 0)
	    for ( ; dday >= (tmp = days_in_year(y)); dday -= tmp, y++);
	else
	    for ( ; dday < 0; --y, dday += days_in_year(y) );

	y = res->tm_year = y - 1900;
	int day = (int) dday;
	res->tm_yday = day;

	/* month within year */
	int mon;
	for (mon = 0;
	     day >= (tmp = days_in_month(mon, y));
	     day -= tmp, mon++);
	res->tm_mon = mon;
	res->tm_mday = day + 1;
	validate_tm(res);
#ifdef HAVE_TM_GMTOFF
	res->tm_gmtoff = -sdiff;
#endif

	// No DST before 1916
	if(res->tm_year < 16) res->tm_isdst = 0;
	return res;
    } else {
	res->tm_isdst = 0; /* no dst in UTC */
	return res;
    }
} /* localtime0() */
#endif // end of PATH 1) ---------------------------------------------

/* Some functions below need to set environment variable TZ and then
   (attempt to) set the time-zone accordingly (via set_tz, tzset).
   On exit, they attempt to restore the previous TZ and re-set the time-zone
   (via reset_tz, tzset). Functions set_tz, reset_tz and the internal tzcode
   tzset() may issue warnings, which may be turned into errors and cause a long
   jump (PR#17966), before or after setting the time-zone (to the required one,
   or as a fallback to UTC).

   struct tzset_info ... holds information for resetting the time zone
                         during local and non-local returns

   prepare_reset_tz() .. initializes tzset_info and sets up context
   set_tz()           .. local setting of tz
   reset_tz()         .. local re-setting, also invoked non-locally

   prepare_dummy_reset_tz()
                      .. initializes tzset_info for local invocation
                         so that it does nothing (could go away
                         after some refactoring)
*/

typedef struct tzset_info {
    char oldtz[1001];	/* previous value of TZ variable */
    bool hadtz;	/* TZ variable existed previously */
    bool settz;	/* TZ variable was set by us */
    RCNTXT cntxt;
    bool end_context_on_reset;
			/* should endcontext() be called from reset_tz()? */
} tzset_info;

static void reset_tz(tzset_info *si);

static void cend_reset_tz(void *data)
{
    tzset_info *si = (tzset_info *)data;
    si->end_context_on_reset = false;
    reset_tz(si);
}

static void prepare_reset_tz(tzset_info *si)
{
    si->settz = false;
    /* set up a context which will reset tz if there is an error */
    begincontext(&si->cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
                 R_NilValue, R_NilValue);
    si->cntxt.cend = &cend_reset_tz;
    si->cntxt.cenddata = si;
    si->end_context_on_reset = true;
}

static void prepare_dummy_reset_tz(tzset_info *si)
{
    si->settz = false;
    si->end_context_on_reset = false;
}

static bool set_tz(const char *tz, tzset_info *si)
{
    si->settz = false;

    char *p = getenv("TZ");
    if(p) {
	if (strlen(p) > 1000)
	    error("time zone specification is too long");
	strcpy(si->oldtz, p);
	si->hadtz = true;
    } else
	si->hadtz = false;
#ifdef HAVE_SETENV
    if(setenv("TZ", tz, 1)) warning(_("problem with setting timezone"));
    else si->settz = true;
#elif defined(HAVE_PUTENV)
    {
	/* This could be dynamic, but setenv is strongly preferred
	   (but not available on Windows)
	   "A program should not alter or free the string"
	*/
	static char buff[1010];
	if (strlen(tz) > 1000)
	    error("time zone specification is too long");
	strcpy(buff, "TZ="); strcat(buff, tz);
	if(putenv(buff)) warning(_("problem with setting timezone"));
	else si->settz = true;
    }
#else
    warning(_("cannot set timezones on this system"));
#endif
    tzset();
    return si->settz;
}

static void reset_tz(tzset_info *si)
{
    if (si->end_context_on_reset) {
	endcontext(&si->cntxt);
	si->end_context_on_reset = false; /* guard against double reset */
    }
    if (!si->settz)
	return;

    si->settz = false; /* better avoid recursive attempts */
    if(si->hadtz) {
#ifdef HAVE_SETENV
	if(setenv("TZ", si->oldtz, 1))
	    warning(_("problem with setting timezone"));
#elif defined(HAVE_PUTENV)
	{
	    static char buff[1010];
	    strcpy(buff, "TZ="); strcat(buff, si->oldtz); // could use strncat
	    if(putenv(buff)) warning(_("problem with setting timezone"));
	}
#endif
    } else {
#ifdef HAVE_UNSETENV
	// FreeBSD variants used not to return a value, but POSIX requires this
	if(unsetenv("TZ")) warning(_("problem with unsetting timezone"));
#elif defined(HAVE_PUTENV_UNSET)
	if(putenv("TZ")) warning(_("problem with unsetting timezone"));
#elif defined(HAVE_PUTENV_UNSET2)
	if(putenv("TZ=")) warning(_("problem with unsetting timezone"));
#endif
    }
    tzset();
}

// called from do_strptime
static void glibc_fix(stm *tm, bool *invalid)
{
    /* set mon and mday which glibc does not always set.
       Use current year/... if none has been specified.

       Specifying mon but not mday nor yday is invalid.
    */
    time_t t = time(NULL);
    stm *tm0;
    int tmp;
#ifndef HAVE_POSIX_LEAPSECONDS
    t -= n_leapseconds;
#endif
    // Recent Linux and macOS have localtime_r
#ifdef HAVE_LOCALTIME_R
    stm tm2;
    tm0 = localtime_r(&t, &tm2);
#else
    tm0 = localtime(&t);
#endif
    if(tm->tm_year == NA_INTEGER) tm->tm_year = tm0->tm_year;
    if(tm->tm_mon != NA_INTEGER && tm->tm_mday != NA_INTEGER) return;
    // at least one of the month and the day of the month is missing
    if(tm->tm_yday != NA_INTEGER) {
	// since we have yday, let that take precedence over mon/mday
	int yday = tm->tm_yday, mon = 0;
	while(yday >= (tmp = days_in_month(mon, tm->tm_year))) {
	    yday -= tmp;
	    mon++;
	}
	tm->tm_mon = mon;
	tm->tm_mday = yday + 1;
    } else {
	if(tm->tm_mday == NA_INTEGER) {
	    if(tm->tm_mon != NA_INTEGER) {
		*invalid = true;
		return;
	    } else tm->tm_mday = tm0->tm_mday;
	}
	if(tm->tm_mon == NA_INTEGER) tm->tm_mon = tm0->tm_mon;
    }
}

// Used in do_asPOSIXlt do_strptime do_D2POSIXlt do_balancePOSIXlt
static void
makelt(stm *tm, SEXP ans, R_xlen_t i, bool valid, double frac_secs)
{
    if(valid) {
	REAL(   VECTOR_ELT(ans, 0))[i] = tm->tm_sec + frac_secs;
	INTEGER(VECTOR_ELT(ans, 1))[i] = tm->tm_min;
	INTEGER(VECTOR_ELT(ans, 2))[i] = tm->tm_hour;
	INTEGER(VECTOR_ELT(ans, 3))[i] = tm->tm_mday;
	INTEGER(VECTOR_ELT(ans, 4))[i] = tm->tm_mon;
	INTEGER(VECTOR_ELT(ans, 5))[i] = tm->tm_year;
	INTEGER(VECTOR_ELT(ans, 6))[i] = tm->tm_wday;
	INTEGER(VECTOR_ELT(ans, 7))[i] = tm->tm_yday;
	INTEGER(VECTOR_ELT(ans, 8))[i] = tm->tm_isdst;
    } else {
	REAL(VECTOR_ELT(ans, 0))[i] = frac_secs;
	for(int j = 1; j < 8; j++)
	    INTEGER(VECTOR_ELT(ans, j))[i] = NA_INTEGER;
	INTEGER(VECTOR_ELT(ans, 8))[i] = -1; // isdst
    }
}

// Used in do_asPOSIXlt do_strptime
// Uses tz from enclosing function
#define BEGIN_MAKElt					\
    SEXP tzone;						\
    if (isUTC) {					\
	tzone = PROTECT(mkString(tz));			\
    } else {						\
	tzone = PROTECT(allocVector(STRSXP, 3));	\
	SET_STRING_ELT(tzone, 0, mkChar(tz));		\
	SET_STRING_ELT(tzone, 1, mkChar(R_tzname[0]));	\
	SET_STRING_ELT(tzone, 2, mkChar(R_tzname[1]));	\
    }

// Used in do_asPOSIXlt do_strptime do_D2POSIXlt
// Uses ans ansnames tzone tzsi from enclosing function
#define END_MAKElt					\
    setAttrib(ans, R_NamesSymbol, ansnames);		\
    SEXP klass = PROTECT(allocVector(STRSXP, 2));	\
    SET_STRING_ELT(klass, 0, mkChar("POSIXlt"));	\
    SET_STRING_ELT(klass, 1, mkChar("POSIXt"));		\
    classgets(ans, klass);				\
    if(isString(tzone)) setAttrib(ans, install("tzone"), tzone);	\
    reset_tz(&tzsi);				\
    SEXP nm = getAttrib(x, R_NamesSymbol);				\
    if(nm != R_NilValue) setAttrib(VECTOR_ELT(ans, 5), R_NamesSymbol, nm); \
    MAYBE_INIT_balanced							\
    setAttrib(ans, lt_balancedSymbol, _balanced_);


/*
   A POSIXlt object may have 9, 10 or 11 components, but newly created
  ones always have 11..  The optional ones are zone and gmtoff, and it
  may have either or both.  The description does not specify the order
  of the components.  The code assumes the first nine are secs
  ... isdst.  and that the 10th and 11th are zone and gmtoff if
  present.

  Object can have gmtoff without HAVE_TM_GMTOFF.

  Called from do_asPOSIXct do_formatPOSIXlt do_balancePOSIXlt
*/

// used by valid_POSIX do_asPOSIXlt do_strptime do_D2POSIXlt do_balancePOSIXlt
static const char ltnames[][11] =
  // 0     1      2       3       4      5       6       7       8
{ "sec", "min", "hour", "mday", "mon", "year", "wday", "yday", "isdst",
  // 9       10
  "zone",  "gmtoff"};

// validate components 1 ... nm
#define isNum(s) ((TYPEOF(s) == INTSXP) || (TYPEOF(s) == REALSXP))
// NB: this can change its argument.
static bool valid_POSIXlt(SEXP x, int nm)
{
    int n_comp = LENGTH(x); // >= 9, 11 for fresh objects
    int n_check = imin2(n_comp, nm);
    if(!isVectorList(x) || n_comp < 9)
	error(_("a valid \"POSIXlt\" object is a list of at least 9 elements"));

    SEXP nms = getAttrib(x, R_NamesSymbol);
    if(LENGTH(nms) < 9)
	error(_("a valid \"POSIXlt\" object has names"));

    // Now check the names
    for (int i = 0; i < n_check ; i++) {
	const char *nm = CHAR(STRING_ELT(nms, i));
	if (strcmp(nm, ltnames[i]))
	    error(_("a valid \"POSIXlt\" object has element %d with name '%s' which should be '%s'"),
		  i+1, nm, ltnames[i]);
    }

    // And check the types and coerce if necessary
    for (int i = 0; i < imin2(9, nm) ; i++) {
	if(!isNum(VECTOR_ELT(x, i)))
	    error(_("a valid \"POSIXlt\" object has a numeric element %s"),
		ltnames[i]);
    }
    SET_VECTOR_ELT(x, 0, coerceVector(VECTOR_ELT(x, 0), REALSXP));
    for(int i = 1; i < n_check; i++) {
	if (i == 9) continue; //skip zone
	SET_VECTOR_ELT(x, i, coerceVector(VECTOR_ELT(x, i), INTSXP));
    }

    if(n_check >= 10) {
	if(!isString(VECTOR_ELT(x, 9)))
	    error(_("a valid \"POSIXlt\" object has a character element %s"),
		  ltnames[9]);
    }
    if(n_check >= 11) {
	if(!isNum(VECTOR_ELT(x, 10)))
	    error(_("a valid \"POSIXlt\" object has a numeric element %s"),
		  ltnames[10]);
    }

    // check the tzone attribute.
    SEXP tz = getAttrib(x, install("tzone"));
    if (!isNull(tz)) {
	if(!isString(tz))
	    error(_("invalid '%s'"), "attr(x, \"tzone\")");
	int l = LENGTH(tz);
	if(l != 1 && l != 3)
	    error(_("attr(x, \"tzone\") should have length 1 or 3"));
    }

    return true;
}


	     /* --------- R interfaces --------- */

static SEXP  /* 'const' globals */
    lt_balancedSymbol = NULL,
    _balanced_ = NULL;

// called from do_asPOSIXlt, do_strptime, do_D2POSIXlt, do_balancePOSIXlt
#define MAYBE_INIT_balanced /* initialize when first used */	\
    if(lt_balancedSymbol == NULL) {				\
	lt_balancedSymbol = install("balanced");		\
	_balanced_ = ScalarLogical(1);				\
    }

// We assume time zone names/abbreviations are ASCII, as all known ones are.

// .Internal(as.POSIXlt(x, tz)) -- called only from  as.POSIXlt.POSIXct()
attribute_hidden SEXP do_asPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = PROTECT(coerceVector(CAR(args), REALSXP));
    SEXP stz = CADR(args);
    if(!isString((stz)) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    const char *tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) {
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) {
	    stz = mkString(p); /* make a copy */
	    tz = CHAR(STRING_ELT(stz, 0));
	}
    }
    PROTECT(stz); // it might be new
    /*
       In this function isUTC means that the timezone has been set to
       UTC either by default, for example as the system timezone or
       via TZ="UTC", or via a 'tz' argument.

       It controls setting TZ, the use of gmtime vs localtime, forcing
       isdst = 0 and how the "tzone" attribute is set.
    */
    bool isUTC = (strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0);

    tzset_info tzsi;
    prepare_reset_tz(&tzsi);

    if(!isUTC && strlen(tz) > 0) set_tz(tz, &tzsi);
#ifdef USE_INTERNAL_MKTIME
    else R_tzsetwall(); // to get the system timezone recorded
#else
    tzset();
#endif

    // Do now as localtime may change tzname.
    BEGIN_MAKElt

    R_xlen_t n = XLENGTH(x);
    int nans = 11;
    SEXP ans = PROTECT(allocVector(VECSXP, nans));
    for(int i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));
    SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, n));
    SET_VECTOR_ELT(ans, 10, allocVector(INTSXP, n));

    SEXP ansnames = PROTECT(allocVector(STRSXP, nans));
    for(int i = 0; i < nans; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(R_xlen_t i = 0; i < n; i++) {
	stm dummy, *ptm = &dummy;
	double d = REAL(x)[i];
	bool valid;
	if(R_FINITE(d)) {
	    ptm = localtime0(&d, !isUTC, &dummy);
	    /*
	       In theory localtime/gmtime always return a valid struct
	       tm pointer, but Windows uses NULL for error conditions
	       (like negative times). Not that we use this for
	       Windows, but other OSes might also get it wrong.
	    */
	    valid = (ptm != NULL);
	} else {
	    valid = false;
	}
	makelt(ptm, ans, i, valid, valid ? d - floor(d) : d);
	if (isUTC) {
	    SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(tz));
	    INTEGER(VECTOR_ELT(ans, 10))[i] = 0;
	} else {
	    char *p = "";
	    // or ptm->tm_zone (but not specified by POSIX)
	    if(valid && ptm->tm_isdst >= 0)
		p = R_tzname[ptm->tm_isdst];
	    SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(p));
#ifdef HAVE_TM_GMTOFF
	    INTEGER(VECTOR_ELT(ans, 10))[i] =
		valid ? (int)ptm->tm_gmtoff : NA_INTEGER;
#else
	    INTEGER(VECTOR_ELT(ans, 10))[i] = NA_INTEGER;
#endif
	}
    }
    END_MAKElt
    UNPROTECT(6);
    return ans;
} // asPOSIXlt

#define check_nlen(_i_)							\
  if(nlen[_i_] == 0)							\
    error(_("zero-length component [[%d]] in non-empty \"POSIXlt\" structure"), (_i_)+1)

// .Internal(as.POSIXct(x, tz)) -- called only from  as.POSIXct.POSIXlt()
attribute_hidden SEXP do_asPOSIXct(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = PROTECT(duplicate(CAR(args))); // maybe coerced on next line
    valid_POSIXlt(x, 9);

    SEXP stz;
    if(!isString((stz = CADR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    const char *tz = CHAR(STRING_ELT(stz, 0));
    if(strlen(tz) == 0) { // tz = ""
	/* do a direct look up here as this does not otherwise
	   work on Windows */
	char *p = getenv("TZ");
	if(p) {
	    stz = mkString(p);
	    tz = CHAR(STRING_ELT(stz, 0));
	}
    }

    PROTECT(stz); // it might be new
    int isUTC = (strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0) ? 1 : 0;
    /*
      if !isUTC we need to set the tz, not set tm_isdst and use mktime
      not timegm (or an emulation).
    */
    tzset_info tzsi;
    prepare_reset_tz(&tzsi);
    if(!isUTC && strlen(tz) > 0) set_tz(tz, &tzsi);
#ifdef USE_INTERNAL_MKTIME
    else R_tzsetwall(); // to get the system timezone recorded
#else
    tzset();
#endif

    R_xlen_t n = 0, nlen[9];
    for(int i = 0; i < 6; i++)
	if((nlen[i] = XLENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];
    if((nlen[8] = XLENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8];
    if(n > 0) {
	for(int i = 0; i < 6; i++)
	    check_nlen(i);
	check_nlen(8);
    }

    SEXP ans = PROTECT(allocVector(REALSXP, n));
    for(R_xlen_t i = 0; i < n; i++) {
	// This codes assumes a fixed order of components.
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	stm tm;
	// avoid (int) NAN
	tm.tm_sec   = R_FINITE(secs) ? (int) fsecs: NA_INTEGER;
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	/* mktime ignores tm.tm_wday and tm.tm_yday */
	tm.tm_isdst = isUTC ? 0 : INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(!R_FINITE(secs))
	    REAL(ans)[i] = secs;
	else if(tm.tm_min  == NA_INTEGER ||
		tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
		tm.tm_mon  == NA_INTEGER || tm.tm_year == NA_INTEGER)
	    REAL(ans)[i] = NA_REAL;
	else {
	    errno = 0;
	    // Interface to mktime or timegm00, PATH-specific
	    double tmp = mktime0(&tm, !isUTC);
/*
    POSIX

    POSIX requires that on error, mktime() returns (time_t)-1 and sets errno,
    but previous versions of the specification made setting of errno optional.
    errno on its own is not a reliable indication of error (PR#18532).
*/
	    REAL(ans)[i] = ((tmp == -1.)
#ifdef MKTIME_SETS_ERRNO
	                    && errno
#else
			    /* avoid silly gotcha at epoch minus one sec */
			    && (tm.tm_sec != 59)
			    && ((tm.tm_sec = 58), (mktime0(&tm, !isUTC) != -2.))
#endif
			    ) ?
	      NA_REAL : tmp + (secs - fsecs);
	}
    }

    // set names() and class() :
    SEXP nm = getAttrib(VECTOR_ELT(x, 5), R_NamesSymbol);
    if (nm != R_NilValue) setAttrib(ans, R_NamesSymbol, nm);
    SEXP klass = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("POSIXct"));
    SET_STRING_ELT(klass, 1, mkChar("POSIXt"));
    classgets(ans, klass);

    reset_tz(&tzsi);
    UNPROTECT(4);
    return ans;
} // as.POSIXct()

// .Internal(format.POSIXlt(x, format, usetz)) aka strftime
attribute_hidden SEXP do_formatPOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = PROTECT(duplicate(CAR(args))); /* maybe coerced in next line */
    valid_POSIXlt(x, 11);

    SEXP sformat;
    if(!isString((sformat = CADR(args))) || XLENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "format");
    R_xlen_t m = XLENGTH(sformat);
    int UseTZ = asLogical(CADDR(args));
    if(UseTZ == NA_LOGICAL)
	error(_("invalid '%s' argument"), "usetz");
    int digits = asInteger(CADDDR(args)); // checked for NA below
    SEXP tz = getAttrib(x, install("tzone"));
    if(!isNull(tz) && !isString(tz))
	error(_("invalid '%s'"), "attr(x, \"tzone\")");

    tzset_info tzsi;
    prepare_reset_tz(&tzsi);
    const char *tz1;
    if (!isNull(tz) && strlen(tz1 = CHAR(STRING_ELT(tz, 0)))) {
	/* If the format includes %Z or %z
	   we need to try to set TZ accordingly */
	int needTZ = 0;
	for(R_xlen_t i = 0; i < m; i++) {
	    const char *p = translateChar(STRING_ELT(sformat, i));
	    if (strstr(p, "%Z") || strstr(p, "%z")) {needTZ = 1; break;}
	}
	/* strftime (per POSIX) calls settz(), so we need to set TZ, but
	   we would not have to call settz() directly (except for the
	   old OLD_Win32 code) */
	if(needTZ) set_tz(tz1, &tzsi);
    }

    /* workaround for glibc/FreeBSD/macOS strftime: they have
       non-POSIX/C99 time zone components
     */
    stm tm;
    memset(&tm, 0, sizeof(tm));

    /* find length of longest one */
    R_xlen_t n = 0, nlen[11];
    int nn = imin2(LENGTH(x), 11);
    for(int i = 0; i < nn; i++) {
	nlen[i] = XLENGTH(VECTOR_ELT(x, i));
	if(nlen[i] > n) n = nlen[i];
    }
    if(n > 0) {
	for(int i = 0; i < nn; i++)
	  check_nlen(i);
    }
    R_xlen_t N = (n > 0) ? ((m > n) ? m : n) : 0;
    SEXP ans = PROTECT(allocVector(STRSXP, N));
    char tm_zone[20];
#ifdef HAVE_TM_GMTOFF
    bool have_zone = LENGTH(x) >= 11;// and components w/ length >= 1
#else
    bool have_zone = LENGTH(x) >= 10;
#endif
    // in case it is needed
    int ns0 = -1;
    for(R_xlen_t i = 0; i < N; i++) {
	// This codes assumes a fixed order of components.
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	// avoid (int) NAN
	if (R_FINITE(secs) && fsecs >= INT_MIN && fsecs <= INT_MAX)
	    tm.tm_sec = (int) fsecs;
	else
	    tm.tm_sec = NA_INTEGER;
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	tm.tm_wday  = INTEGER(VECTOR_ELT(x, 6))[i%nlen[6]];
	tm.tm_yday  = INTEGER(VECTOR_ELT(x, 7))[i%nlen[7]];
	tm.tm_isdst = INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	if(have_zone) {
	    strncpy(tm_zone,
		    CHAR(STRING_ELT(VECTOR_ELT(x, 9), i%nlen[9])),
		    20 - 1);
	    tm_zone[20 - 1] = '\0';
#ifdef HAVE_TM_ZONE
	    tm.tm_zone = tm_zone;
#else
	    /* This used to be
	       if(tm.tm_isdst >= 0) tzname[tm.tm_isdst] = tm_zone;
	       Modifying tzname causes memory corruption on Solaris. It
	       is not specified to have any effect and strftime is documented
	       to call settz().*/
//	    if(tm.tm_isdst >= 0 && strcmp(tzname[tm.tm_isdst], tm_zone))
//		warning(_("Timezone specified in the object field cannot be used on this system."));
#endif
	}
	if(!R_FINITE(secs)) {
	    SET_STRING_ELT(ans, i,
			   ISNA(secs)  ? NA_STRING :
			   ISNAN(secs) ? mkChar("NaN") :
			   (secs > 0)  ? mkChar("Inf") : mkChar("-Inf"));
	} else if(tm.tm_min == NA_INTEGER || tm.tm_hour == NA_INTEGER || tm.tm_mday == NA_INTEGER ||
		  tm.tm_mon == NA_INTEGER || tm.tm_year == NA_INTEGER) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else if(validate_tm(&tm) < 0 || likely_strftime_overflow(&tm)) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    /* We could translate to wchar_t and use wcsftime if we
	       have it.  But there is no R_wcsftime, nor support in
	       IANA's tcode.  It might be safe enough to translate to
	       UTF-8 and use strftime -- this is only looking to
	       replace short ASCII character sequences. */
	    const char *q = translateChar(STRING_ELT(sformat, i%m));
	    int nn = (int) strlen(q) + 50;
	    char buf2[nn];
	    const char *p;
	    strcpy(buf2, q);

	    p = strstr(q, "%OS");
	    if(p) {
		int ns, nused = 4;
		char *p2 = strstr(buf2, "%OS");
		*p2 = '\0';
		ns = *(p + 3) - '0';
		if(ns < 0 || ns > 9) { /* not a digit */
		    if (ns0 == -1) {
 			ns0 = digits;
			if(ns0 == NA_INTEGER) ns0 = 0;
		    }
		    ns = ns0;
		    nused = 3;
		}
		if(ns > 6) ns = 6;
		if(ns > 0) {
		    /* truncate to avoid nuisances such as PR#14579 */
		    double s = tm.tm_sec + (secs - fsecs), t = Rexp10((double) ns);
		    s = ((int) (s*t))/t;
		    snprintf(p2, sizeof(buf2) - (p2 - buf2), "%0*.*f",
			     ns+3, ns, s);
		    strcat(buf2, p+nused);
		} else {
		    strcat(p2, "%S");
		    strcat(buf2, p+nused);
		}
	    }

#ifdef HAVE_TM_GMTOFF
	    if(have_zone) {  // so not in UTC
		// Got coerced if necessary above
		int tmp = INTEGER(VECTOR_ELT(x, 10))[i%nlen[10]];
		if (tmp == NA_INTEGER && strstr(buf2, "%z")) { // only need it for %z
		    tm.tm_gmtoff = 0;
# ifdef USE_INTERNAL_MKTIME
		    R_mktime(&tm);
# else
		    // At least on glibc this corrects it
		    mktime(&tm);
# endif
		} else tm.tm_gmtoff = tmp;
	    }
#endif
	    // The on-overflow behaviour is not determined by C99-C23.
	    // However, this should return 0 so we can throw an error.
	    char buff[2049];
	    size_t res;
#ifdef USE_INTERNAL_MKTIME
	    res = R_strftime(buff, 2049, buf2, &tm);
#else
	    res = strftime(buff, 2049, buf2, &tm);
#endif
	    if (res == 0 // overflow for at least internal and glibc
	        // if not from a format string that may give zero bytes
	        && strcmp(buf2, "%Z") && strcmp(buf2, "%z")
	        && strcmp(buf2, "%P") && strcmp(buf2, "%p")) {
		Rf_error("output string exceeded 2048 bytes");
	    }

	    /* Now assume tzone abbreviated name is < 40 bytes,
	       but they are currently 3 or 4 bytes.
	    */
	    if(UseTZ) {
		if(have_zone) {
		    const char *p = CHAR(STRING_ELT(VECTOR_ELT(x, 9), i%nlen[9]));
		    if(strlen(p)) {strcat(buff, " "); strcat(buff, p);}
		} else if(!isNull(tz)) {
		    int ii = 0;
		    if(LENGTH(tz) == 3) {
			if(tm.tm_isdst > 0) ii = 2;
			else if(tm.tm_isdst == 0) ii = 1;
			else ii = 0; /* Use base timezone name */
		    }
		    const char *p = CHAR(STRING_ELT(tz, ii));
		    if(strlen(p)) {strcat(buff, " "); strcat(buff, p);}
		}
	    }
	    SET_STRING_ELT(ans, i, mkChar(buff));
	}
    }


    SEXP nm = getAttrib(VECTOR_ELT(x, 5), R_NamesSymbol);
    PROTECT(nm);
    // nm has length nlen[5] ; ans has length N
    // so first pad nm to length n, then recycle to length N.
    if (nm != R_NilValue) {
	SEXP nm2 = PROTECT(xlengthgets(nm, n));
	SEXP nm3 = allocVector(STRSXP, N);
	for (R_xlen_t j = 0; j < N; j++)
	    SET_STRING_ELT(nm3, j, STRING_ELT(nm2, j % n));
	setAttrib(ans, R_NamesSymbol, nm3);
	UNPROTECT(1);
    }

    reset_tz(&tzsi);
    UNPROTECT(3);
    return ans;
}


// .Internal(strptime(as.character(x), format, tz))
attribute_hidden SEXP do_strptime(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x, sformat, stz;
    if(!isString((x = CAR(args))))
	error(_("invalid '%s' argument"), "x");
    if(!isString((sformat = CADR(args))) || XLENGTH(sformat) == 0)
	error(_("invalid '%s' argument"), "format");
    if(!isString((stz = CADDR(args))) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    const char *tz = CHAR(STRING_ELT(stz, 0));
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

    // Usage of isUTC here follows do_asPOSIXlt
    bool isUTC = (strcmp(tz, "GMT") == 0  || strcmp(tz, "UTC") == 0);

    tzset_info tzsi;
    prepare_reset_tz(&tzsi);

    if(!isUTC && strlen(tz) > 0) set_tz(tz, &tzsi);
#ifdef USE_INTERNAL_MKTIME
    else R_tzsetwall(); // to get the system timezone recorded
#else
    tzset();
#endif

    // do now in case this gets changed by conversions.
    BEGIN_MAKElt

    R_xlen_t
      n = XLENGTH(x),
      m = XLENGTH(sformat),
      N = (n > 0) ? ((m > n) ? m : n) : 0;

    int nans = 11;
    SEXP ans = PROTECT(allocVector(VECSXP, nans));
    for(int i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, N));
    SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, N));
    SET_VECTOR_ELT(ans, 10, allocVector(INTSXP, N));

    SEXP ansnames = PROTECT(allocVector(STRSXP, nans));
    for(int i = 0; i < nans; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));


    for(R_xlen_t i = 0; i < N; i++) {
	stm tm, tm2, *ptm = &tm;
	double psecs = 0.0;

	/* for glibc's sake. That only sets some unspecified fields,
	   sometimes. */
	memset(&tm, 0, sizeof(stm));
	tm.tm_sec = tm.tm_min = tm.tm_hour = 0;
	tm.tm_year = tm.tm_mon = tm.tm_mday = tm.tm_yday =
	    tm.tm_wday = NA_INTEGER;
#ifdef HAVE_TM_GMTOFF
	tm.tm_gmtoff = (long) NA_INTEGER;
	tm.tm_isdst = -1;
#endif
	int offset = NA_INTEGER;
	bool invalid =
	    STRING_ELT(x, i%n) == NA_STRING ||
	    !R_strptime(translateChar(STRING_ELT(x, i%n)),
			translateChar(STRING_ELT(sformat, i%m)),
			&tm, &psecs, &offset);
	if(!invalid) {
	    /* Solaris sets missing fields to 0 */
	    if(tm.tm_mday == 0) tm.tm_mday = NA_INTEGER;
	    if(tm.tm_mon == NA_INTEGER || tm.tm_mday == NA_INTEGER || tm.tm_year == NA_INTEGER)
		glibc_fix(&tm, &invalid);
	    tm.tm_isdst = -1;
	    if (offset != NA_INTEGER) {
#ifdef HAVE_TM_GMTOFF
//		tm.tm_gmtoff = offset;
#endif
		/* we know the offset, but not the timezone
		   so all we can do is to convert to time_t,
		   adjust and convert back */
		double t0;
		memcpy(&tm2, &tm, sizeof(stm));
		// Interface to mktime or timegm00, PATH-specific
		t0 = mktime0(&tm2, 0);
		if (t0 != -1) {
		    t0 -= offset; /* offset = -0800 is Seattle */
		    ptm = localtime0(&t0, !isUTC, &tm2);
		} else invalid = true;
	    } else {
		/* we do want to set wday, yday, isdst, but not to
		   adjust structure at DST boundaries */
		memcpy(&tm2, &tm, sizeof(stm));
		if (isUTC) tm.tm_isdst = 0;
		/* mktime _may_ result in error e.g. during the spring-forward gap */
		if (mktime0(&tm2, !isUTC) != -1) {
		    /* set wday, yday, isdst */
		    tm.tm_wday = tm2.tm_wday;
		    tm.tm_yday = tm2.tm_yday;
		    if (!isUTC && tm.tm_hour == tm2.tm_hour
		               && tm.tm_min == tm2.tm_min) {
			/* do not adjust tm_isdst when the hours/minutes have
			   been adjusted; some mktime implementations adjust
			   the (non-existent) time in the spring-forward gap to
			   the time after the gap and they adjust the tm_isdst
			   value accordingly; taking just one of the two of
			   these adjustments would be incorrect (PR#18581). */
			tm.tm_isdst = tm2.tm_isdst;
		    }
		}
	    }
	    invalid = validate_tm(&tm) != 0;
	}
	makelt(ptm, ans, i, !invalid, invalid ? NA_REAL : psecs - floor(psecs));
	if (isUTC) {
	    SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(tz));
	    INTEGER(VECTOR_ELT(ans, 10))[i] = 0;
	} else {
	    const char *p = "";
	    if(!invalid && tm.tm_isdst >= 0) {
#ifdef HAVE_TM_ZONE
		p = tm.tm_zone;
		if(!p)
#endif
		    p = R_tzname[tm.tm_isdst];
	    }
	    SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(p));
#ifdef HAVE_TM_GMTOFF
	    INTEGER(VECTOR_ELT(ans, 10))[i] =
		invalid ? NA_INTEGER : (int)tm.tm_gmtoff;
#else
	    INTEGER(VECTOR_ELT(ans, 10))[i] = NA_INTEGER;
#endif
	}
    } /* for(i ..) */

    END_MAKElt
    if(nm != R_NilValue) {
	if (N > n) {// we need to recycle names
	    SEXP nm3 = allocVector(STRSXP, N);
	    for (R_xlen_t j = 0; j < N; j++)
		SET_STRING_ELT(nm3, j, STRING_ELT(nm, j % n));
	    setAttrib(VECTOR_ELT(ans, 5), R_NamesSymbol, nm3);
	}
    }
    UNPROTECT(5);
    return ans;
} // strptime()

// .Internal(Date2POSIXlt(x)) called from as.POSIXlt.Date
// It always returns a date-time in UTC.
attribute_hidden SEXP do_D2POSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = PROTECT(coerceVector(CAR(args), REALSXP));
    SEXP stz = CADR(args);
    if(!isString((stz)) || LENGTH(stz) != 1)
	error(_("invalid '%s' value"), "tz");
    const char *tz = CHAR(STRING_ELT(stz, 0));
    if (!tz[0]) tz = "UTC";;
    R_xlen_t n = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(VECSXP, 11));
    for(int i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));
    SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, n));
    SET_VECTOR_ELT(ans, 10, allocVector(INTSXP, n));

    SEXP ansnames = PROTECT(allocVector(STRSXP, 11));
    for(int i = 0; i < 11; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(R_xlen_t i = 0; i < n; i++) {
	stm tm;
	double x_i = REAL(x)[i];
	bool valid = R_FINITE(x_i) != 0;
	if(valid) {
	    /* every 400 years is exactly 146097 days long and the
	       pattern is repeated */
	    double rounds = floor(floor(x_i) / 146097.0);
	    int day = (int) (floor(x_i) - 146097.0 * rounds);
	    tm.tm_hour = tm.tm_min = tm.tm_sec = 0;
	    /* weekday: 1970-01-01 was a Thursday */
	    if ((tm.tm_wday = (((day % 7) + 4) % 7)) < 0) tm.tm_wday += 7;

	    /* year & day within year */
	    int y = 1970, tmp, mon;
	    if (day >= 0)
		for ( ; day >= (tmp = days_in_year(y)); day -= tmp, y++);
	    else
		for ( ; day < 0; --y, day += days_in_year(y) );
	    // Avoid overflows
	    double year0 =  y - 1900 + rounds * 400;
	    if (year0 > INT_MAX || year0 < INT_MIN)
		valid = false;
	    else {
		y = tm.tm_year = (int)year0;
		tm.tm_yday = day;
		/* month within year */
		for (mon = 0;
		     day >= (tmp = days_in_month(mon, y));
		     day -= tmp, mon++);
		tm.tm_mon = mon;
		tm.tm_mday = day + 1;
		tm.tm_isdst = 0; /* no dst in GMT */
	    }
	}
	makelt(&tm, ans, i, valid, valid ? 0.0 : x_i);
	SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(tz));
	INTEGER(VECTOR_ELT(ans, 10))[i] = 0;
    }
    SEXP tzone = mkString(tz);
    PROTECT(tzone);

    tzset_info tzsi; /* tz reset not used */
    prepare_dummy_reset_tz(&tzsi);
    END_MAKElt

    UNPROTECT(5);
    return ans;
}

// .Internal(POSIXlt2Date(x)), called from as.Date.POSIXlt(x)
attribute_hidden SEXP do_POSIXlt2D(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP x = PROTECT(duplicate(CAR(args)));
    valid_POSIXlt(x, 6);

    R_xlen_t n = 0, nlen[9];
    for(int i = 0; i < 6; i++)
	if((nlen[i] = XLENGTH(VECTOR_ELT(x, i))) > n) n = nlen[i];// incl {sec,min,hour}
    if((nlen[8] = XLENGTH(VECTOR_ELT(x, 8))) > n) n = nlen[8]; // isdst
    if(n > 0) {
	for(int i = 0; i < 6; i++)
	    check_nlen(i);
	check_nlen(8);
    }

    SEXP ans = PROTECT(allocVector(REALSXP, n));
    for(R_xlen_t i = 0; i < n; i++) {
	/* need to treat {sec, min, hour} in out-of-range case
	   {where fixup *may* change day,month... */
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	stm tm;
	// avoid (int) NAN
	//  this assumes a fixed order of components.
	tm.tm_sec   = R_FINITE(secs) ? (int) fsecs: NA_INTEGER;
	tm.tm_min   = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour  = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday  = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon   = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year  = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	/* mktime ignores tm.tm_wday and tm.tm_yday */
	tm.tm_isdst = 0;
	if(!R_FINITE(secs)) // +/-Inf, NA, NaN
	    REAL(ans)[i] = secs;
	else if(tm.tm_min  == NA_INTEGER || tm.tm_hour == NA_INTEGER ||
		tm.tm_mday == NA_INTEGER ||
		tm.tm_mon  == NA_INTEGER || tm.tm_year == NA_INTEGER)
	    REAL(ans)[i] = NA_REAL;
	else if(validate_tm(&tm) < 0) // validate_tm() fixes up out-of-range {sec,min,...}
	    REAL(ans)[i] = NA_REAL;
	else { // normal case:
	    REAL(ans)[i] = mkdate00(&tm);
	}
    }

    SEXP nm = getAttrib(VECTOR_ELT(x, 5), R_NamesSymbol);
    if (nm != R_NilValue) setAttrib(ans, R_NamesSymbol, nm);
    SEXP klass = PROTECT(mkString("Date"));
    classgets(ans, klass);
    UNPROTECT(3);
    return ans;
}

static SEXP balancePOSIXlt(SEXP x, bool fill_only, bool do_class)
{
    MAYBE_INIT_balanced
    const SEXP _filled_ = ScalarLogical(NA_LOGICAL);
    SEXP bal = getAttrib(x, lt_balancedSymbol);
    /*  bal in  (TRUE, NA, NULL) <==> ("balanced", "filled", <unset>) */
    if(bal == _balanced_ || (fill_only && bal == _filled_)) {
	if(!do_class) {
	    x = duplicate(x);
	    setAttrib(x, R_ClassSymbol, R_NilValue);
	}
	return(x);
    }

    valid_POSIXlt(x, 11);
    int n_comp = LENGTH(x);

    bool need_fill = false;
    R_xlen_t n = 0, nlen[n_comp];
    for(int i = 0; i < n_comp; i++) {
	if((nlen[i] = XLENGTH(VECTOR_ELT(x, i))) > n)
	    n = nlen[i];
	else if(!need_fill && nlen[i] < n)
	    need_fill = true;
    }
    if(fill_only && !need_fill) { // already filled; be fast
	x = PROTECT(duplicate(x)); // (could mutate in the do_class case)
	setAttrib(x, lt_balancedSymbol, _filled_); /* not there; checked above*/
	if(!do_class)
	    setAttrib(x, R_ClassSymbol, R_NilValue);
	UNPROTECT(1);
	return(x);
    }

    // check only now as we cannot return quickly :
    if(!inherits(x, "POSIXlt"))
	error(_("'%s' is not a \"%s\""), "x", "POSIXlt");
    x = PROTECT(duplicate(x));
    if(n > 0) {
	for(int i = 0; i < n_comp; i++)
	  check_nlen(i);
	// ==>  n := max(nlen[i]) and all  nlen[i] > 0
    }

    // get names(.) [possibly empty]
    SEXP nm = getAttrib(VECTOR_ELT(x, 5), R_NamesSymbol);
    bool set_nm = (nlen[5] < n || !fill_only) && nm != R_NilValue;
    if(set_nm && !fill_only)
	PROTECT(nm);

    if(fill_only) { // & need_fill
	R_xlen_t ni;
	// x[0] : sec (double)
	if((ni = nlen[0]) != n) { // recycle sec = x[[0]] to length n
	    SET_VECTOR_ELT(x, 0, xlengthgets(VECTOR_ELT(x, 0), n));
	    double *xi = REAL(VECTOR_ELT(x, 0));
	    for(R_xlen_t ii=ni; ii < n; ii++)
		xi[ii] = xi[ii % ni];
	}
	// x[1:8] = {min, hour, mday, mon, year, wday, yday, isdst} :
	for(int i = 1; i < 9; i++) {
	    ni = nlen[i];
	    if(ni != n) { // recycle x[[i]] to length n
		// 1. extend to length (filling with NA; names with ""):
		SET_VECTOR_ELT(x, i, xlengthgets(VECTOR_ELT(x, i), n));
		// 2. fill by recycling:
		int *xi = INTEGER(VECTOR_ELT(x, i));
		for(R_xlen_t ii=ni; ii < n; ii++)
		    xi[ii] = xi[ii % ni];
	    }
	    if(i == 5 && set_nm) { /* set names(.) = names(x[[5]]) = names(x$year) : */
		nm = PROTECT(getAttrib(VECTOR_ELT(x, 5), R_NamesSymbol)); // of full length n
		// fill names, recycling:
		for(R_xlen_t ii=ni; ii < n; ii++)
		    SET_STRING_ELT(nm, ii, STRING_ELT(nm, ii % ni));
		setAttrib(VECTOR_ELT(x, 5), R_NamesSymbol, nm);
		UNPROTECT(1);
	    }
	}

	if(n_comp >= 10 && (ni = nlen[9]) != n) { // x[9] : zone (character)
	    SET_VECTOR_ELT(x, 9, xlengthgets(VECTOR_ELT(x, 9), n));
	    SEXP xi = VECTOR_ELT(x, 9);
	    for(R_xlen_t ii = ni; ii < n; ii++)
		SET_STRING_ELT(xi, ii, STRING_ELT(xi, ii % ni));
	}

	if(n_comp >= 11 && (ni = nlen[10]) != n) { // x[10] : gmtoff
	    SET_VECTOR_ELT(x, 10, xlengthgets(VECTOR_ELT(x, 10), n));
	    int *xi = INTEGER(VECTOR_ELT(x, 10));
	    for(R_xlen_t ii = ni; ii < n; ii++)
		xi[ii] = xi[ii % ni];
	}
	if(!do_class) setAttrib(x, R_ClassSymbol, R_NilValue);
	setAttrib(x, lt_balancedSymbol, _filled_);
	UNPROTECT(1);
	return(x);
    }

    // fill *and* validate from now on:

    bool have_10 = n_comp >= 10, have_11 = n_comp >= 11;
    SEXP ans = PROTECT(allocVector(VECSXP, n_comp));
    for(int i = 0; i < 9; i++)
	SET_VECTOR_ELT(ans, i, allocVector(i > 0 ? INTSXP : REALSXP, n));
    if(have_10) SET_VECTOR_ELT(ans, 9, allocVector(STRSXP, n));
    if(have_11) SET_VECTOR_ELT(ans, 10, allocVector(INTSXP, n));

    SEXP ansnames = PROTECT(allocVector(STRSXP, n_comp));
    for(int i = 0; i < n_comp; i++)
	SET_STRING_ELT(ansnames, i, mkChar(ltnames[i]));

    for(R_xlen_t i = 0; i < n; i++) {
      // 1. fill 'tm'
	double secs = REAL(VECTOR_ELT(x, 0))[i%nlen[0]], fsecs = floor(secs);
	stm tm;
	// this assumes a fixed order of components.
	// avoid (int) NAN
	tm.tm_sec  = R_FINITE(secs) ? (int) fsecs: NA_INTEGER;
	tm.tm_min  = INTEGER(VECTOR_ELT(x, 1))[i%nlen[1]];
	tm.tm_hour = INTEGER(VECTOR_ELT(x, 2))[i%nlen[2]];
	tm.tm_mday = INTEGER(VECTOR_ELT(x, 3))[i%nlen[3]];
	tm.tm_mon  = INTEGER(VECTOR_ELT(x, 4))[i%nlen[4]];
	tm.tm_year = INTEGER(VECTOR_ELT(x, 5))[i%nlen[5]];
	tm.tm_wday = INTEGER(VECTOR_ELT(x, 6))[i%nlen[6]];
	tm.tm_yday = INTEGER(VECTOR_ELT(x, 7))[i%nlen[7]];
	tm.tm_isdst = INTEGER(VECTOR_ELT(x, 8))[i%nlen[8]];
	char tm_zone[20];
	if(have_10) {
	    strncpy(tm_zone, CHAR(STRING_ELT(VECTOR_ELT(x, 9), i%nlen[9])), 20 - 1);
	    tm_zone[20 - 1] = '\0';
#ifdef HAVE_TM_ZONE
	    tm.tm_zone = tm_zone;
#else
	    /* Modifying tzname causes memory corruption on Solaris. It
	       is not specified to have any effect and strftime is documented
	       to call settz().*/
//	    if(tm.tm_isdst >= 0 && strcmp(tzname[tm.tm_isdst], tm_zone))
//		warning(_("Timezone specified in the object's 'zone' component cannot be used on this system."));
#endif
	}
#ifdef HAVE_TM_GMTOFF
	if (have_11)
	    tm.tm_gmtoff = INTEGER(VECTOR_ELT(x, 10))[i%nlen[10]];
	else
	    tm.tm_gmtoff = NA_INTEGER; // or -1 or 0 ??
#endif

	/* 2. checking for NA/non-finite --------------
	 * ----------- careful:
	 * validate_tm() must *not* be called if any other components are NA.
	 */
	bool valid =
	    (R_FINITE(secs) &&
	     tm.tm_min  != NA_INTEGER &&
	     tm.tm_hour != NA_INTEGER &&
	     tm.tm_mday != NA_INTEGER &&
	     tm.tm_mon  != NA_INTEGER &&
	     tm.tm_year != NA_INTEGER);

	if(valid) {
	    validate_tm(&tm);
	    // Set correct {yday, wday}:
	    // The standards-conformant way to get these set
	    // is to call mktime (or timegm where supported).
	    mkdate00(&tm);
	}

	makelt(&tm, ans, i, valid,
	       valid ? secs - fsecs : (R_FINITE(secs) ? NA_REAL : secs)); // fills ans[0..8]

	if (have_10) {
	    const char *p = "";
	    if(valid && tm.tm_isdst >= 0) {
#ifdef HAVE_TM_ZONE
		p = tm.tm_zone;
		if(!p)
#endif
		    p = R_tzname[tm.tm_isdst];
	    }
	    SET_STRING_ELT(VECTOR_ELT(ans, 9), i, mkChar(p));
	}
	if(have_11) {
#ifdef HAVE_TM_GMTOFF
		INTEGER(VECTOR_ELT(ans, 10))[i] =
		    valid ? (int)tm.tm_gmtoff : NA_INTEGER;
#else
		INTEGER(VECTOR_ELT(ans, 10))[i] = NA_INTEGER;
#endif
	}
    } // end for(i ..)

    setAttrib(ans, R_NamesSymbol, ansnames); // sec, min, ...
    if(do_class) {
	SEXP klass = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(klass, 0, mkChar("POSIXlt"));
	SET_STRING_ELT(klass, 1, mkChar("POSIXt"));
	classgets(ans, klass);
	UNPROTECT(1);
    }

    SEXP tz = getAttrib(x, install("tzone"));
    if(!isNull(tz)) {
	if(!isString(tz)) error(_("invalid '%s'"), "attr(x, \"tzone\")");
	setAttrib(ans, install("tzone"), tz);
    }
    if(set_nm) { // names(.), attached to x[[5]] = x$year:
	SEXP nmN = PROTECT(allocVector(STRSXP, n));
	R_xlen_t ni = XLENGTH(nm); // typically = nlen[5]
	// names(.) will have to become length n;  $year is already
	// fill names, recycling:
	for(R_xlen_t i = 0; i < n; i++)
	    SET_STRING_ELT(nmN, i, STRING_ELT(nm, i % ni));
	setAttrib(VECTOR_ELT(ans, 5), R_NamesSymbol, nmN);
	UNPROTECT(2); // nm, nmN
    }
    setAttrib(ans, lt_balancedSymbol, _balanced_);
    UNPROTECT(3);
    return ans;
}

// .Internal(balancePOSIXlt(x, fill.only, classed)) called from R's balancePOSIXlt()
// or primitive  unCfillPOSIXlt(x)
attribute_hidden SEXP do_balancePOSIXlt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    /*
       This may be called on objects generated on other versions of R
       with/without tm_zone/rm_offset, or even different versions of
       R.  Let alone hand-edited objects, as in datetime3.R, or those
       created in packages.
    */
    checkArity(op, args);
    SEXP x = CAR(args);

    int fill_only, do_class;
    if(PRIMVAL(op) == 1) { // unCfillPOSIXlt(x)
	fill_only = true;
	do_class = false;
    } else { // op == 0 :  .Internal(balancePOSIXlt(x, fill.only, classed))
	fill_only = asLogical(CADR(args));
	if(fill_only == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "fill.only");
	do_class = asLogical(CADDR(args));
	if(do_class == NA_LOGICAL)
	    error(_("invalid '%s' argument"), "classed");
    }
    return balancePOSIXlt(x, (bool) fill_only, (bool) do_class);
}

