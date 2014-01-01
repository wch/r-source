/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (c) 1989 The Regents of the University of California.
 *  Copyright (C) 2013 The R Core Team
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

/*
  Based on code from tzcode, which is turn said to be
  'Based on the UCB version with the copyright notice appearing below.'

  Extensive changes for use with R.

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
*/


#include <config.h>

#undef HAVE_TM_ZONE
#define HAVE_TM_ZONE 1
#undef HAVE_TM_GMTOFF
#define HAVE_TM_GMTOFF 1

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

#ifdef HAVE_NL_LANGINFO
// This was part of the configure check
# include <langinfo.h>

#else
static char *orig(const char *fmt, const stm *const t)
{
    static char buff[100];
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

static char *
_fmt(const char *format, const stm *const t, char * pt, const char *const ptlim)
{
    for ( ; *format; ++format) {
	if (*format == '%') {
	    /* Check for POSIX 2008 modifiers */
	    char pad = '+'; int width = -1;
	    while (1)
	    {
		switch (*++format) {
		case '_': // pad with spaces: GNU extension
		case '0':
		case '+': // pad with zeroes, and more (not here)
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

	label:
	    switch (*++format) {
	    case '\0':
		--format;
		break;

		/* now the locale-dependent cases */
#ifdef HAVE_NL_LANGINFO
	    case 'A':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= 7) ?
			  "?" : nl_langinfo(DAY_1 + t->tm_wday),
			  pt, ptlim);
		continue;
	    case 'a':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= 7) ?
			  "?" : nl_langinfo(ABDAY_1 + t->tm_wday),
			  pt, ptlim);
		continue;
	    case 'B':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= 12) ?
			  "?" : nl_langinfo(MON_1 + t->tm_mon),
			  pt, ptlim);
		continue;
	    case 'b':
	    case 'h':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= 12) ?
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
	    case 'X':
		pt = _fmt(nl_langinfo(T_FMT), t, pt, ptlim);
		continue;
	    case 'x':
		pt = _fmt(nl_langinfo(D_FMT), t, pt, ptlim);
		continue;
#else
	    case 'A':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= 7) ?
			  "?" : orig("%A", t),
			  pt, ptlim);
		continue;
	    case 'a':
		pt = _add((t->tm_wday < 0 || t->tm_wday >= 7) ?
			  "?" : orig("%a", t),
			  pt, ptlim);
		continue;
	    case 'B':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= 12) ?
			  "?" : orig("%B", t),
			  pt, ptlim);
		continue;
	    case 'b':
	    case 'h':
		pt = _add((t->tm_mon < 0 || t->tm_mon >= 12) ?
			  "?" : orig("%b", t),
			  pt, ptlim);
		continue;
	    case 'c':
		pt = _fmt(orig("%c", t), t, pt, ptlim);
		continue;
	    case 'p':
		pt = _add(orig("%p", t), pt, ptlim);
		continue;
	    case 'X':
		pt = _fmt(orig("%X", t), t, pt, ptlim);
		continue;
	    case 'x':
		pt = _fmt(orig("%x", t), t, pt, ptlim);
		continue;
#endif
		/* now the locale-independent ones */
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
		** C99 locale modifiers.
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
		int_fast64_t mkt = R_mktime(&tm);
#ifdef WIN32
		// not ISO C99, so warns
		(void) snprintf(buf, 22, "%I64d", mkt);
#else
		(void) snprintf(buf, 22, "%lld", (long long) mkt);
#endif
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
		pt = _conv((t->tm_yday + 7 - t->tm_wday) / 7,
			   "%02d", pt, ptlim);
		continue;
	    case 'u':
		pt = _conv((t->tm_wday == 0) ? 7 : t->tm_wday, "%d", pt, ptlim);
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
		    bot = ((yday + 11 - wday) % 7) - 3;
		    /*
		    ** What yday does the NEXT
		    ** ISO year begin on?
		    */
		    top = bot - (len % 7);
		    if (top < -3)
			top += 7;
		    top += len;
		    if (yday >= top) {
			++base;
			w = 1;
			break;
		    }
		    if (yday >= bot) {
			w = 1 + ((yday - bot) / 7);
			break;
		    }
		    --base;
		    yday += isleap_sum(year, base) ? DAYSPERLYEAR : DAYSPERNYEAR;
		}
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
		pt = _conv((t->tm_yday + 7 -
			    (t->tm_wday ? (t->tm_wday - 1) : (7 - 1))) / 7,
			   "%02d", pt, ptlim);
		continue;
	    case 'w':
		pt = _conv(t->tm_wday, "%d", pt, ptlim);
		continue;
	    case 'y':
		pt = _yconv(t->tm_year, TM_YEAR_BASE, 0, 1, pt, ptlim);
		continue;
	    case 'Y':
//		pt = _yconv(t->tm_year, TM_YEAR_BASE, 1, 1, pt, ptlim);
	    {
		char buf[20] = "%";
		if (pad == '0' || pad == '+') strcat(buf, "0");
		if (pad == '+' && width < 0) width = 4;
		if (width > 0) sprintf(buf+strlen(buf), "%u", width);
		strcat(buf, "d");
		pt = _conv(TM_YEAR_BASE + t->tm_year, buf, pt, ptlim);
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
		long  diff;
		char const *sign;

		if (t->tm_isdst < 0)
		    continue;
#ifdef HAVE_TM_GMTOFF
		diff = t->tm_gmtoff;
#else
		diff = R_timegm(t) - R_mktime(t);
#endif
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
