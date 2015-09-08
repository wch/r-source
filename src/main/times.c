/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2014  The R Core Team.
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
 *      Interfaces to time functions.
 */

/*
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

#include <Defn.h>
#include <Internal.h>

#include <time.h>

/* clock_gettime, timespec_get time are in <time.h> */
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
    checkArity(op, args);
    return ScalarReal(currentTime());
}

#ifdef HAVE_UNISTD_H
#include <unistd.h> /* for getpid */
#endif

/* For RNG.c, main.c, mkdtemp.c */
attribute_hidden unsigned int TimeToSeed(void)
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
