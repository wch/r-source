/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (c) 1989 The Regents of the University of California.
 *  Copyright (C) 2013-2022 The R Core Team
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

/*
  Based on code from tzcode, which is turn said to be
  'Based on the UCB version with the copyright notice appearing below.'

** Copyright (c) 1989 The Regents of the University of California.
** All rights reserved.
**
** Redistribution and use in source and binary forms are permitted
** provided that the above copyright notice and this paragraph are
** duplicated in all such forms and that any documentation,
** advertising materials, and other materials related to such
** distribution and use acknowledge that the software was developed
** by the University of California, Berkeley. The name of the
** University may not be used to endorse or promote products derived
** from this software without specific prior written permission.
** THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
** IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.

  Extensive changes for use with R, which are copyright by R Core.
  These include
  - using const
  - converted to use NL_LANGINFO
  - use snprintf
  - add support for %P
  - get locale-specific names via nl_langinfo or system strftime
  - use 0/1 not false/true
  - we do not support warnp, which allow warning on less portable formats
*/


#include <config.h>

#undef HAVE_TM_ZONE
#define HAVE_TM_ZONE 1
#undef HAVE_TM_GMTOFF
#define HAVE_TM_GMTOFF 1

// added in R 4.3.0, but not default to agree with glibc
// #define XPG4_1994_04_09

#include "tzfile.h"
#include <fcntl.h>
#include <locale.h>
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h> // memcpy
#include <ctype.h>
#include <limits.h> // for INT_MAX

#include "datetime.h"

#ifdef Win32
#include <trioremap.h> /* for %lld */
#endif

static char * _add(const char *, char *, const char *);
static char * _conv(int, const char *, char *, const char *);
static char * _fmt(const char *, const stm *, char *, const char *);
static char * _yconv(int, int, int, int, char *, const char *);

size_t
R_strftime(char * const s, const size_t maxsize, const char *const format,
	   const stm *const t)
{
    char *p;
    R_tzset();

    p = _fmt(((format == NULL) ? "%c" : format), t, s, s + maxsize);
    if (p == s + maxsize)
	return 0;
    *p = '\0';
    return p - s;
}

/* On modern platforms we get day names etc from nl_langinfo.
   If not, we use the system strftime. */
#ifdef HAVE_NL_LANGINFO
// This was part of the configure check
# include <langinfo.h>

#else
static char *orig(const char *fmt, const stm *const t)
{
    static char buff[100]; // no known name is over 20 bytes
    struct tm tm;
    memset(&tm, 0, sizeof(struct tm));
    tm.tm_sec = t->tm_sec; tm.tm_min = t->tm_min; tm.tm_hour = t->tm_hour;
    tm.tm_mday = t->tm_mday; tm.tm_mon = t->tm_mon; tm.tm_year = t->tm_year;
    tm.tm_wday = t->tm_wday; tm.tm_yday = t->tm_yday;
    tm.tm_isdst = t->tm_isdst;
    strftime(buff, 100, fmt, &tm);
    return buff;
}
#endif

// tzcode has warnp, to warn about the use of less portable formats.
static char *
_fmt(const char *format, const stm *const t, char * pt, const char *const ptlim)
{
    for ( ; *format; ++format) {
	if (*format == '%') {
	    /* First check for POSIX 2008 / GNU modifiers for %Y */
	    char pad = '\0'; const char *f; int width = -1;
	    // first look to see if this is %..Y
	    for (f = format+1; *f ; f++)
		if(! (isdigit(*f) || *f == '_' || *f == '+')) break;
	    if (*f == 'Y')  {
		while (1)
		{
		    switch (*++format) {
		    case '0':
		    case '+': // pad with zeroes, and more (not here)
		    case '_': // pad with spaces: GNU extension
			pad = *format;
			continue;
		    default:
			break;
		    }
		    break;
		}
		if (isdigit (*format))
		{
		    width = 0;
		    do
		    {
			if (width > INT_MAX / 10 || 
			    (width == INT_MAX / 10 && *format - '0' > INT_MAX % 10))
			    width = INT_MAX;
			else {width *= 10; width += *format - '0';}
			format++;
		    }
		    while (isdigit (*format));
		}
		--format;
	    }

	label:
	    switch (*++format) {
	    case '\0':
		--format;
		break;

		/* next the locale-dependent cases */
#ifdef HAVE_NL_LANGINFO
	    case 'A':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= DAYSPERWEEK) ?
			  "?" : nl_langinfo(DAY_1 + t->tm_wday),
			  pt, ptlim);
		continue;
	    case 'a':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= DAYSPERWEEK) ?
			  "?" : nl_langinfo(ABDAY_1 + t->tm_wday),
			  pt, ptlim);
		continue;
	    case 'B':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= MONSPERYEAR) ?
			  "?" : nl_langinfo(MON_1 + t->tm_mon),
			  pt, ptlim);
		continue;
	    case 'b':
	    case 'h':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= MONSPERYEAR) ?
			  "?" : nl_langinfo(ABMON_1 + t->tm_mon),
			  pt, ptlim);
		continue;
	    case 'c':
		pt = _fmt(nl_langinfo(D_T_FMT), t, pt, ptlim);
		continue;
	    case 'p':
		pt = _add(nl_langinfo(t->tm_hour < 12 ? AM_STR : PM_STR),
			  pt, ptlim);
		continue;
	    case 'P': // R addition not in tzcode
		{
		    char *p = nl_langinfo(t->tm_hour < 12 ? AM_STR : PM_STR),
			*q, buff[20];
		    for (q = buff; *p; ) *q++ = (char) tolower(*p++);
		    *q = '\0'; 
		    pt = _add(buff, pt, ptlim);
		}
		continue;
	    case 'X':
		pt = _fmt(nl_langinfo(T_FMT), t, pt, ptlim);
		continue;
	    case 'x':
		pt = _fmt(nl_langinfo(D_FMT), t, pt, ptlim);
		continue;
#else
	    case 'A':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= DAYSPERWEEK) ?
			  "?" : orig("%A", t),
			  pt, ptlim);
		continue;
	    case 'a':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= DAYSPERWEEK) ?
			  "?" : orig("%a", t),
			  pt, ptlim);
		continue;
	    case 'B':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= MONSPERYEAR) ?
			  "?" : orig("%B", t),
			  pt, ptlim);
		continue;
	    case 'b':
	    case 'h':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= MONSPERYEAR) ?
			  "?" : orig("%b", t),
			  pt, ptlim);
		continue;
	    case 'c':
		// In a C locale this is supposed to be
		// "%a %b %e %T %Y". It is not on Windows ....
#ifdef _WIN32
		pt = _fmt("%a %b %e %T %Y", t, pt, ptlim);
#else
		pt = _fmt(orig("%c", t), t, pt, ptlim);
#endif
		continue;
	    case 'p':
		pt = _add(orig("%p", t), pt, ptlim);
		continue;
	    case 'P':
		{
		    char *p = orig("%p", t), *q, buff[20];
		    for (q = buff; *p; ) *q++ = (char) tolower(*p++);
		    *q = '\0'; 
		    pt = _add(buff, pt, ptlim);
		}
		continue;
	    case 'X':
		pt = _fmt(orig("%X", t), t, pt, ptlim);
		continue;
	    case 'x':
		pt = _fmt(orig("%x", t), t, pt, ptlim);
		continue;
#endif
		/* now the locale-independent ones */
		// 'a' 'A' 'b' 'B' 'c' are locale-dependent
	    case 'C':
		pt = _yconv(t->tm_year, TM_YEAR_BASE, 1, 0, pt, ptlim);
		continue;
	    case 'D':
		pt = _fmt("%m/%d/%y", t, pt, ptlim);
		continue;
	    case 'd':
		pt = _conv(t->tm_mday, "%02d", pt, ptlim);
		continue;
	    case 'E':
	    case 'O':
		/*
		** Locale modifiers of C99 and later.
		** The sequences
		**	%Ec %EC %Ex %EX %Ey %EY
		**	%Od %oe %OH %OI %Om %OM
		**	%OS %Ou %OU %OV %Ow %OW %Oy
		** are supposed to provide alternate
		** representations.
		*/
		goto label;
	    case 'e':
		pt = _conv(t->tm_mday, "%2d", pt, ptlim);
		continue;
	    case 'F':
		pt = _fmt("%Y-%m-%d", t, pt, ptlim);
		continue;
		// case 'h' is localw-dependenr (same as 'b')
	    case 'H':
		pt = _conv(t->tm_hour, "%02d", pt, ptlim);
		continue;
	    case 'I':
		pt = _conv((t->tm_hour % 12) ? (t->tm_hour % 12) : 12,
			   "%02d", pt, ptlim);
		continue;
	    case 'j':
		pt = _conv(t->tm_yday + 1, "%03d", pt, ptlim);
		continue;
	    case 'k':
		pt = _conv(t->tm_hour, "%2d", pt, ptlim);
		continue;
	    case 'l':
		pt = _conv((t->tm_hour % 12) ? (t->tm_hour % 12) : 12,
			   "%2d", pt, ptlim);
		continue;
	    case 'M':
		pt = _conv(t->tm_min, "%02d", pt, ptlim);
		continue;
	    case 'm':
		pt = _conv(t->tm_mon + 1, "%02d", pt, ptlim);
		continue;
	    case 'n':
		pt = _add("\n", pt, ptlim);
		continue;
	    /* 'p' and "P' are locale-dependent */
	    case 'R':
		pt = _fmt("%H:%M", t, pt, ptlim);
		continue;
	    case 'r':
		pt = _fmt("%I:%M:%S %p", t, pt, ptlim);
		continue;
	    case 'S':
		pt = _conv(t->tm_sec, "%02d", pt, ptlim);
		continue;
	    case 's':
	    {
		stm  tm = *t;
		char buf[22]; // <= 19 digs + sign + terminator
		int_fast64_t mkt = R_mktime(&tm); // we know -1 is valid tine
		(void) snprintf(buf, 22, "%lld", (long long) mkt);
		pt = _add(buf, pt, ptlim);
	    }
	    continue;
	    case 'T':
		pt = _fmt("%H:%M:%S", t, pt, ptlim);
		continue;
	    case 't':
		pt = _add("\t", pt, ptlim);
		continue;
	    case 'U':
		pt = _conv((t->tm_yday + DAYSPERWEEK -
			    t->tm_wday) / DAYSPERWEEK,
			   "%02d", pt, ptlim);
		continue;
	    case 'u':
		pt = _conv((t->tm_wday == 0) ?
			   DAYSPERWEEK : t->tm_wday,
			   "%d", pt, ptlim);
		continue;
	    case 'V':	/* ISO 8601 week number */
	    case 'G':	/* ISO 8601 year (four digits) */
	    case 'g':	/* ISO 8601 year (two digits) */
	    {
		int year, base, yday, wday, w;

		year = t->tm_year;
		base = TM_YEAR_BASE;
		yday = t->tm_yday;
		wday = t->tm_wday;
		for ( ; ; ) {
		    int	len, bot, top;

		    len = isleap_sum(year, base) ? DAYSPERLYEAR : DAYSPERNYEAR;
		    /*
		    ** What yday (-3 ... 3) does
		    ** the ISO year begin on?
		    */
		    bot = ((yday + 11 - wday) % DAYSPERWEEK) - 3;
		    /*
		    ** What yday does the NEXT
		    ** ISO year begin on?
		    */
		    top = bot - (len % DAYSPERWEEK);
		    if (top < -3)
			top += DAYSPERWEEK;
		    top += len;
		    if (yday >= top) {
			++base;
			w = 1;
			break;
		    }
		    if (yday >= bot) {
			w = 1 + ((yday - bot) / DAYSPERWEEK);
			break;
		    }
		    --base;
		    yday += isleap_sum(year, base) ?
			DAYSPERLYEAR : DAYSPERNYEAR;
		}
#ifdef XPG4_1994_04_09
		if ((w == 52 && t->tm_mon == TM_JANUARY) ||
		    (w == 1 && t->tm_mon == TM_DECEMBER))
		    w = 53;
#endif /* defined XPG4_1994_04_09 */
		if (*format == 'V')
		    pt = _conv(w, "%02d", pt, ptlim);
		else if (*format == 'g')
		    pt = _yconv(year, base, 0, 1, pt, ptlim);
		else // %G
		    pt = _yconv(year, base, 1, 1, pt, ptlim);
	    }
	    continue;
	    case 'v':
		pt = _fmt("%e-%b-%Y", t, pt, ptlim);
		continue;
	    case 'W':
		pt = _conv((t->tm_yday + DAYSPERWEEK -
			    (t->tm_wday ?
			     (t->tm_wday - 1) :
			     (DAYSPERWEEK - 1))) / DAYSPERWEEK,
			   "%02d", pt, ptlim);
		continue;
	    case 'w':
		pt = _conv(t->tm_wday, "%d", pt, ptlim);
		continue;
	    case 'y':
		pt = _yconv(t->tm_year, TM_YEAR_BASE, 0, 1, pt, ptlim);
		continue;
	    case 'Y':
		// Changed to allow glibc's way (no padding)
//		pt = _yconv(t->tm_year, TM_YEAR_BASE, 1, 1, pt, ptlim);
	    {
		char buf[20] = "%";
		int year = TM_YEAR_BASE + t->tm_year;
		char *p = getenv("R_PAD_YEARS_BY_ZERO");
		if (!p) p = "yes";
		if( strcmp(p, "yes") == 0 && pad == '\0') {
		    pad = '0'; width = 4;
		}
		if (pad == '0' || pad == '+') strcat(buf, "0");
		if (width > 0) {
		    size_t sz = strlen(buf);
		    snprintf(buf+sz, 20-sz, "%u", width);
		}
		if (pad == '+' && year > 9999) strcat(buf, "+");
		strcat(buf, "d");
		pt = _conv(year, buf, pt, ptlim);
	    }
	    continue;
	    case 'Z':
#ifdef HAVE_TM_ZONE
		if (t->tm_zone != NULL)
		    pt = _add(t->tm_zone, pt, ptlim);
		else
#endif
		if (t->tm_isdst >= 0)
		    pt = _add(R_tzname[t->tm_isdst != 0], pt, ptlim);
		/*
		** C99 says that %Z must be replaced by the
		** empty string if the time zone is not
		** determinable.
		*/
		continue;
	    case 'z':
	    {
		long  diff; // should be R_time_t
		char const *sign;

		if (t->tm_isdst < 0)
		    continue;
		// always used as this is part of USE_INTERNAL_TZONE
		diff = t->tm_gmtoff;
		if (diff < 0) {
		    sign = "-";
		    diff = -diff;
		} else	sign = "+";
		pt = _add(sign, pt, ptlim);
		diff /= SECSPERMIN;
		diff = (diff / MINSPERHOUR) * 100 + (diff % MINSPERHOUR);
		pt = _conv((int) diff, "%04d", pt, ptlim);
	    }
	    continue;
	    case '+':
		pt = _fmt("%a %b %e %H:%M:%S %Z %Y", t, pt, ptlim);
		continue;
	    case '%':
	    default:
		break;
	    }
	}
	if (pt == ptlim)
	    break;
	*pt++ = *format;
    }
    return pt;
}

static char *
_conv(const int n, const char *const format, char *const pt,
      const char *const ptlim)
{
    char  buf[12];

    (void) snprintf(buf, 12, format, n);
    return _add(buf, pt, ptlim);
}

static char *
_add(const char *str, char *pt, const char *const ptlim)
{
    while (pt < ptlim && (*pt = *str++) != '\0')
	++pt;
    return pt;
}

/*
** POSIX and the C Standard are unclear or inconsistent about
** what %C and %y do if the year is negative or exceeds 9999.
** Use the convention that %C concatenated with %y yields the
** same output as %Y, and that %Y contains at least 4 bytes,
** with more only if necessary.

* Explained in POSIX 2008, at least.
*/

static char *
_yconv(const int a, const int b, 
       const int convert_top, const int convert_yy,
       char *pt, const char *const ptlim)
{
    int lead, trail;

#define DIVISOR	100
    trail = a % DIVISOR + b % DIVISOR;
    lead = a / DIVISOR + b / DIVISOR + trail / DIVISOR;
    trail %= DIVISOR;
    if (trail < 0 && lead > 0) {
	trail += DIVISOR;
	--lead;
    } else if (lead < 0 && trail > 0) {
	trail -= DIVISOR;
	++lead;
    }
    if (convert_top) {
	if (lead == 0 && trail < 0)
	    pt = _add("-0", pt, ptlim);
	else pt = _conv(lead, "%02d", pt, ptlim);
    }
    if (convert_yy)
	pt = _conv(((trail < 0) ? -trail : trail), "%02d", pt, ptlim);
    return pt;
}
