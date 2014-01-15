/* For inclusion by datetime.c. 

   A modified version of code from the GNU C library with locale
   support removed and wchar support added.
*/

/* Convert a string representation of time to a time value.
   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   This file is part of the GNU C Library.
   Contributed by Ulrich Drepper <drepper@cygnus.com>, 1996.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with the GNU C Library; see the file COPYING.LIB.  If not,
   a copy is available at http://www.r-project.org/licenses/
*/
/* XXX This version of the implementation is not really complete.
   Some of the fields cannot add information alone.  But if seeing
   some of them in the same format (such as year, week and weekday)
   this is enough information for determining the date.  */

/* #include <ctype.h>
#include <limits.h>
#include <string.h>*/

/* This is C90 */
#ifndef HAVE_LOCALE_H
# define HAVE_LOCALE_H 1
#endif

static int locale_strings_set = 0;
static int locale_w_strings_set = 0;
static void get_locale_strings(void);
static void get_locale_w_strings(void);


#ifdef HAVE_STRINGS_H
#include <strings.h>  /* for strncasecmp */
#endif

#include <ctype.h> /* for isspace */

#define match_char(ch1, ch2) if (ch1 != ch2) return NULL

/* we guarantee to have strncasecmp in R */
#if defined __GNUC__ && __GNUC__ >= 2
# define match_string(cs1, s2) \
  (__extension__ ({ size_t len = strlen (cs1);						      \
     int result = strncasecmp ((cs1), (s2), len) == 0;			      \
     if (result) (s2) += len;						      \
     result; }))
#else
/* Oh come on.  Get a reasonable compiler.  */
# define match_string(cs1, s2) \
  (strncasecmp ((cs1), (s2), strlen (cs1)) ? 0 : ((s2) += strlen (cs1), 1))
#endif

/* We intentionally do not use isdigit() for testing because this will
   lead to problems with the wide character version.  */
#define get_number(from, to, n) \
  do {									      \
    int __n = n;							      \
    val = 0;								      \
    while (*rp == ' ')							      \
      ++rp;								      \
    if (*rp < '0' || *rp > '9')						      \
      return NULL;							      \
    do {								      \
      val *= 10;							      \
      val += *rp++ - '0';						      \
/*  } while (--__n > 0 && val * 10 <= to && *rp >= '0' && *rp <= '9');*/      \
    } while (--__n > 0 && *rp >= '0' && *rp <= '9');	      \
    if (val < from || val > to)						      \
      return NULL;							      \
  } while (0)
# define get_alt_number(from, to, n) \
  /* We don't have the alternate representation.  */			      \
  get_number(from, to, n)
#define recursive(new_fmt) \
  (*(new_fmt) != '\0'							      \
   && (rp = strptime_internal (rp, (new_fmt), tm, decided, psecs, poffset)) != NULL)

/* This version: may overwrite these with versions for the locale,
 * hence the extra length of the fields
 */
static char weekday_name[][20] =
{
    "Sunday", "Monday", "Tuesday", "Wednesday",
    "Thursday", "Friday", "Saturday"
};
static char ab_weekday_name[][10] =
{
    "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
};
static char month_name[][20] =
{
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
};
static char ab_month_name[][10] =
{
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

static char am_pm[][4] = {"AM", "PM"};


# define HERE_D_T_FMT "%a %b %e %H:%M:%S %Y"
# define HERE_D_FMT "%y/%m/%d"
# define HERE_T_FMT_AMPM "%I:%M:%S %p"
# define HERE_T_FMT "%H:%M:%S"

static const unsigned short int __mon_yday[2][13] =
{
    /* Normal years.  */
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
    /* Leap years.  */
    { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
};


/* Status of lookup: do we use the locale data or the raw data?  */
enum locale_status { Not, loc, raw };

# define __isleap(year)	\
  ((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))

/* Compute the day of the week.  */
static void
day_of_the_week (stm *tm)
{
    /* We know that January 1st 1970 was a Thursday (= 4).  Compute the
       the difference between this data in the one on TM and so determine
       the weekday.  */
    int corr_year, wday;

    /* R bug fix: day_of_the_week needs year, month, mday set */
    if(tm->tm_year == NA_INTEGER ||
       tm->tm_mon == NA_INTEGER ||
       tm->tm_mday == NA_INTEGER) return;

    corr_year = 1900 + tm->tm_year - (tm->tm_mon < 2);
    wday = (-473
	    + (365 * (tm->tm_year - 70))
	    + (corr_year / 4)
	    - ((corr_year / 4) / 25) + ((corr_year / 4) % 25 < 0)
	    + (((corr_year / 4) / 25) / 4)
	    + __mon_yday[0][tm->tm_mon]
	    + tm->tm_mday - 1);
    tm->tm_wday = ((wday % 7) + 7) % 7;
}

/* Compute the day of the year.  */
static void
day_of_the_year (stm *tm)
{
    /* R bug fix: day_of_the_year needs year, month, mday set */
    if(tm->tm_year == NA_INTEGER ||
       tm->tm_mon == NA_INTEGER ||
       tm->tm_mday == NA_INTEGER) return;

    tm->tm_yday = (__mon_yday[__isleap (1900 + tm->tm_year)][tm->tm_mon]
		   + (tm->tm_mday - 1));
}

#include <wchar.h>
#include <wctype.h>

static wchar_t w_weekday_name[][20] =
{
    L"Sunday", L"Monday", L"Tuesday", L"Wednesday",
    L"Thursday", L"Friday", L"Saturday"
};
static wchar_t w_ab_weekday_name[][10] =
{
    L"Sun", L"Mon", L"Tue", L"Wed", L"Thu", L"Fri", L"Sat"
};
static wchar_t w_month_name[][20] =
{
    L"January", L"February", L"March", L"April", L"May", L"June",
    L"July", L"August", L"September", L"October", L"November", L"December"
};
static wchar_t w_ab_month_name[][10] =
{
    L"Jan", L"Feb", L"Mar", L"Apr", L"May", L"Jun",
    L"Jul", L"Aug", L"Sep", L"Oct", L"Nov", L"Dec"
};

static wchar_t w_am_pm[][4] = {L"AM", L"PM"};

/* Need case-insensitive version */
static int Rwcsncasecmp(const wchar_t *cs1, const wchar_t *s2)
{
    size_t i, n = wcslen(cs1);
    const wchar_t *a = cs1, *b = s2;
    for(i = 0; i < n; i++, a++, b++) {
	if(*b == L'\0' || towlower(*a) != towlower(*b)) return 1;
    }
    return 0;
}

#define w_match_string(cs1, s2) \
  (Rwcsncasecmp ((cs1), (s2)) ? 0 : ((s2) += wcslen (cs1), 1))

#define w_recursive(new_fmt) \
  (*(new_fmt) != '\0'							      \
   && (rp = w_strptime_internal (rp, (new_fmt), tm, decided, psecs, poffset)) != NULL)

static wchar_t *
w_strptime_internal (wchar_t *rp, const wchar_t *fmt, stm *tm,
		     enum locale_status *decided, double *psecs, 
		     int *poffset)
{
    int cnt;
    int val;
    int have_I, is_pm;
    int century, want_century;
    int have_wday, want_xday;
    int have_yday;
    int have_mon, have_mday;
    int have_uweek, have_wweek;
    int week_no = 0; /* -Wall */

    have_I = is_pm = 0;
    century = -1;
    want_century = 0;
    have_wday = want_xday = have_yday = have_mon = have_mday = 0;
    have_uweek = have_wweek = 0;

    while (*fmt != L'\0')
    {
	/* A white space in the format string matches 0 more or white
	   space in the input string.  */
	if (iswspace (*fmt))
	{
	    while (iswspace (*rp))
		++rp;
	    ++fmt;
	    continue;
	}

	/* Any character but `%' must be matched by the same character
	   in the input string.  */
	if (*fmt != L'%')
	{
	    match_char (*fmt++, *rp++);
	    continue;
	}

	++fmt;

	/* We need this for handling the `E' modifier.  */
    start_over:

	switch (*fmt++)
	{
	case L'%':
	    /* Match the `%' character itself.  */
	    match_char (L'%', *rp++);
	    break;
	case L'a':
	case L'A':
	    /* Match day of week.  */
#if defined(HAVE_WCSFTIME)
	    if(!locale_w_strings_set) get_locale_w_strings();
#endif
	    for (cnt = 0; cnt < 7; ++cnt)
	    {
		if (*decided != loc
		    && (w_match_string (w_weekday_name[cnt], rp)
			|| w_match_string (w_ab_weekday_name[cnt], rp)))
		{
		    *decided = raw;
		    break;
		}
	    }
	    if (cnt == 7)
		/* Does not match a weekday name.  */
		return NULL;
	    tm->tm_wday = cnt;
	    have_wday = 1;
	    break;
	case L'b':
	case L'B':
	case L'h':
	    /* Match month name.  */
#if defined(HAVE_WCSFTIME)
	    if(!locale_w_strings_set) get_locale_w_strings();
#endif
	    for (cnt = 0; cnt < 12; ++cnt)
	    {
		if (w_match_string (w_month_name[cnt], rp)
		    || w_match_string (w_ab_month_name[cnt], rp))
		{
		    *decided = raw;
		    break;
		}
	    }
	    if (cnt == 12)
		/* Does not match a month name.  */
		return NULL;
	    tm->tm_mon = cnt;
	    want_xday = 1;
	    break;
	case L'c':
	    /* Match locale's date and time format.  */
	    if (!w_recursive (L"%a %b %e %H:%M:%S %Y")) /* HERE_D_T_FMT */
		return NULL;
	    break;
	case L'C':
	  /* Match century number.  */
	  get_number (0, 99, 2);
	  century = val;
	  want_xday = 1;
	  break;
	case L'd':
	case L'e':
	  /* Match day of month.  */
	  get_number (1, 31, 2);
	  tm->tm_mday = val;
	  have_mday = 1;
	  want_xday = 1;
	  break;
	case L'F':
	  if (!w_recursive (L"%Y-%m-%d"))
	    return NULL;
	  want_xday = 1;
	  break;
	case L'x':
	  /* Fall through.  */
	case L'D':
	  /* Match standard day format.  */
	    if (!w_recursive (L"%y/%m/%d")) /* HERE_D_FMT */
	    return NULL;
	  want_xday = 1;
	  break;
	case L'k':
	case L'H':
	  /* Match hour in 24-hour clock.  */
	  get_number (0, 24, 2); /* allow 24:00:00 */
	  tm->tm_hour = val;
	  have_I = 0;
	  break;
	case L'l':
	  /* Match hour in 12-hour clock.  GNU extension.  */
	case L'I':
	  /* Match hour in 12-hour clock.  */
	  get_number (1, 12, 2);
	  tm->tm_hour = val % 12;
	  have_I = 1;
	  break;
	case L'j':
	  /* Match day number of year.  */
	  get_number (1, 366, 3);
	  tm->tm_yday = val - 1;
	  have_yday = 1;
	  break;
	case L'm':
	  /* Match number of month.  */
	  get_number (1, 12, 2);
	  tm->tm_mon = val - 1;
	  have_mon = 1;
	  want_xday = 1;
	  break;
	case L'M':
	  /* Match minute.  */
	  get_number (0, 59, 2);
	  tm->tm_min = val;
	  break;
	case L'n':
	case L't':
	  /* Match any white space.  */
	  while (iswspace (*rp))
	    ++rp;
	  break;
	case L'p':
	  /* Match locale's equivalent of AM/PM.  */
#if defined(HAVE_WCSFTIME)
	  if(!locale_w_strings_set) get_locale_w_strings();
#endif
	  if (!w_match_string (w_am_pm[0], rp)) {
	    if (w_match_string (w_am_pm[1], rp))
	      is_pm = 1;
	    else
		return NULL;
	  }
	  break;
	case L'r':
	    if (!w_recursive (L"%I:%M:%S %p")) /* HERE_T_FMT_AMPM */
		return NULL;
	  break;
	case L'R':
	    if (!w_recursive (L"%H:%M"))
		return NULL;
	    break;
	case L's':
	{
	    /* The number of seconds may be very high so we cannot use
	       the `get_number' macro.  Instead read the number
	       character for character and construct the result while
	       doing this.  */
	    time_t secs = 0;
	    if (*rp < L'0' || *rp > L'9')
		/* We need at least one digit.  */
		return NULL;

	    do
	    {
		secs *= 10;
		secs += *rp++ - L'0';
	    }
	    while (*rp >= L'0' && *rp <= L'9');

#ifdef HAVE_LOCALTIME_R
	    if ((tm = localtime_r (&secs, tm)) == NULL) return NULL;
#else
	    if ((tm = localtime (&secs)) == NULL) return NULL;
#endif
	}
	break;
	case L'S':
	    get_number (0, 61, 2);
	    tm->tm_sec = val;
	    break;
	case L'X':
	    /* Fall through.  */
	case L'T':
	    if (!w_recursive (L"%H:%M:%S")) /* HERE_T_FMT */
		return NULL;
	    break;
	case L'u':
	    get_number (1, 7, 1);
	    tm->tm_wday = val % 7;
	    have_wday = 1;
	    break;
	case L'g':
	    get_number (0, 99, 2);
	    /* XXX This cannot determine any field in TM.  */
	    break;
	case L'G':
	    if (*rp < L'0' || *rp > L'9')
		return NULL;
	    /* XXX Ignore the number since we would need some more
	       information to compute a real date.  */
	    do
		++rp;
	    while (*rp >= L'0' && *rp <= L'9');
	    break;
	case L'U':
	  get_number (0, 53, 2);
	  week_no = val;
	  have_uweek = 1;
	  break;
	case L'W':
	  get_number (0, 53, 2);
	  week_no = val;
	  have_wweek = 1;
	  break;
	case L'V':
	    get_number (0, 53, 2);
	    /* XXX This cannot determine any field in TM without some
	       information.  */
	    break;
	case L'w':
	    /* Match number of weekday.  */
	    get_number (0, 6, 1);
	    tm->tm_wday = val;
	    have_wday = 1;
	    break;
	case L'y':
	    /* Match year within century.  */
	    get_number (0, 99, 2);
	    /* The "Year 2000: The Millennium Rollover" paper suggests that
	       values in the range 69-99 refer to the twentieth century.  */
	    int ival = val;
	    tm->tm_year = ival >= 69 ? ival : ival + 100;
	    /* Indicate that we want to use the century, if specified.  */
	    want_century = 1;
	    want_xday = 1;
	    break;
	case L'Y':
	    /* Match year including century number.  */
	    get_number (0, 9999, 4);
	    tm->tm_year = val - 1900;
	    want_century = 0;
	    want_xday = 1;
	    break;
	case L'z':
	    {
		int n = 0, neg, off = 0;
		val = 0;
		while (*rp == L' ') ++rp;
		if (*rp != L'+' && *rp != L'-') return NULL;
		neg = *rp++ == L'-';
		while (n < 4 && *rp >= L'0' && *rp <= L'9') {
		    val = val * 10 + *rp++ - L'0';
		    ++n;
		}
		if (n != 4) return NULL;
		else {
		    /* We have to convert the minutes into decimal.  */
		    if (val % 100 >= 60) return NULL;
		    val = (val / 100) * 100 + ((val % 100) * 50) / 30;
		}
		if (val > 1200) return NULL;
		off = ((val * 3600) / 100);
		if (neg) off = -off;
		*poffset = off;
	    }
	    break;
	case L'Z':
	    error(_("use of %s for input is not supported"), "%Z");
	    return NULL;
	    break;
	case L'E':
	    /* We have no information about the era format.  Just use
	       the normal format.  */
	    if (*fmt != L'c' && *fmt != L'C' && *fmt != L'y' && *fmt != L'Y'
		&& *fmt != L'x' && *fmt != L'X')
		/* This is an illegal format.  */
		return NULL;

	    goto start_over;
	case L'O':
	    switch (*fmt++)
	    {
	    case L'd':
	    case L'e':
		/* Match day of month using alternate numeric symbols.  */
		get_alt_number (1, 31, 2);
	        tm->tm_mday = val;
		have_mday = 1;
		want_xday = 1;
		break;
	    case L'H':
		/* Match hour in 24-hour clock using alternate numeric
		   symbols.  */
		get_alt_number (0, 23, 2);
	        tm->tm_hour = val;
		have_I = 0;
		break;
	    case L'I':
		/* Match hour in 12-hour clock using alternate numeric
		   symbols.  */
		get_alt_number (1, 12, 2);
	        tm->tm_hour = val % 12;
		have_I = 1;
		break;
	    case L'm':
		/* Match month using alternate numeric symbols.  */
		get_alt_number (1, 12, 2);
	        tm->tm_mon = val - 1;
		have_mon = 1;
		want_xday = 1;
		break;
	    case L'M':
		/* Match minutes using alternate numeric symbols.  */
		get_alt_number (0, 59, 2);
	        tm->tm_min = val;
		break;
	    case L'S':
		/* Match seconds using alternate numeric symbols.
		get_alt_number (0, 61, 2); */
		{
		    double sval;
		    wchar_t *end;
		    sval = wcstod(rp, &end);
		    if( sval >= 0.0 && sval <= 61.0) {
			tm->tm_sec = (int) sval;
			*psecs = sval;
		    }
		    rp = end;
		}
	    break;
	    case L'U':
	      get_alt_number (0, 53, 2);
	      week_no = val;
	      have_uweek = 1;
	      break;
	    case L'W':
	      get_alt_number (0, 53, 2);
	      week_no = val;
	      have_wweek = 1;
	      break;
	    case L'V':
		get_alt_number (0, 53, 2);
		/* XXX This cannot determine any field in TM without
		   further information.  */
		break;
	    case L'w':
		/* Match number of weekday using alternate numeric symbols.  */
		get_alt_number (0, 6, 1);
		tm->tm_wday = val;
		have_wday = 1;
		break;
	    case L'y':
		/* Match year within century using alternate numeric symbols.  */
		get_alt_number (0, 99, 2);
	        int ival = val;
	        tm->tm_year = ival >= 69 ? ival : ival + 100;
		want_xday = 1;
		break;
	    default:
		return NULL;
	    }
	    break;
	default:
	    return NULL;
	}
    }

    if (have_I && is_pm)
	tm->tm_hour += 12;

    if (century != -1)
    {
	if (want_century)
	    tm->tm_year = tm->tm_year % 100 + (century - 19) * 100;
	else
	    /* Only the century, but not the year.  Strange, but so be it.  */
	    tm->tm_year = (century - 19) * 100;
    }

    if (want_xday && !have_wday) {
	if ( !(have_mon && have_mday) && have_yday)  {
	    /* We don't have tm_mon and/or tm_mday, compute them. */
	    int t_mon = 0;
	    while (__mon_yday[__isleap(1900 + tm->tm_year)][t_mon] <= tm->tm_yday)
		t_mon++;
	    if (!have_mon)
		tm->tm_mon = t_mon - 1;
	    if (!have_mday)
		tm->tm_mday = (tm->tm_yday - __mon_yday[__isleap(1900 + tm->tm_year)][t_mon - 1] + 1);
	}
	day_of_the_week (tm);
    }

    if (want_xday && !have_yday)
	day_of_the_year (tm);

  if ((have_uweek || have_wweek) && have_wday) {
      int save_wday = tm->tm_wday;
      int save_mday = tm->tm_mday;
      int save_mon = tm->tm_mon;
      int w_offset = have_uweek ? 0 : 1;

      tm->tm_mday = 1;
      tm->tm_mon = 0;
      day_of_the_week (tm);
      if (have_mday)
	  tm->tm_mday = save_mday;
      if (have_mon)
	  tm->tm_mon = save_mon;

      if (!have_yday) {
	  tm->tm_yday = ((7 - (tm->tm_wday - w_offset)) % 7
			 + (week_no - 1) *7
			 + save_wday - w_offset);
	  if(tm->tm_yday < 0) tm->tm_yday += 7;
      }

      if (!have_mday || !have_mon)
      {
	  int t_mon = 0;
	  while (__mon_yday[__isleap(1900 + tm->tm_year)][t_mon]
		 <= tm->tm_yday)
	      t_mon++;
	  if (!have_mon)
	      tm->tm_mon = t_mon - 1;
	  if (!have_mday)
	      tm->tm_mday =
		  (tm->tm_yday
		   - __mon_yday[__isleap(1900 + tm->tm_year)][t_mon - 1] + 1);
      }

      tm->tm_wday = save_wday;
  }

  return rp;
}


static char *
strptime_internal (const char *rp, const char *fmt, stm *tm,
		   enum locale_status *decided, double *psecs,
		   int *poffset)
{
    int cnt;
    int val;
    int have_I, is_pm;
    int century, want_century;
    int have_wday, want_xday;
    int have_yday;
    int have_mon, have_mday;
    int have_uweek, have_wweek;
    int week_no = 0; /* -Wall */

    have_I = is_pm = 0;
    century = -1;
    want_century = 0;
    have_wday = want_xday = have_yday = have_mon = have_mday = 0;
    have_uweek = have_wweek = 0;

    while (*fmt != '\0')
    {
	/* A white space in the format string matches 0 more or white
	   space in the input string.  */
	if (isspace ((int)*fmt))
	{
	    while (isspace ((int)*rp))
		++rp;
	    ++fmt;
	    continue;
	}

	/* Any character but `%' must be matched by the same character
	   in the input string.  */
	if (*fmt != '%')
	{
	    match_char (*fmt++, *rp++);
	    continue;
	}

	++fmt;

	/* We need this for handling the `E' modifier.  */
    start_over:

	switch (*fmt++)
	{
	case '%':
	    /* Match the `%' character itself.  */
	    match_char ('%', *rp++);
	    break;
	case 'a':
	case 'A':
	    /* Match day of week.  */
	    if(!locale_strings_set) get_locale_strings();
	    for (cnt = 0; cnt < 7; ++cnt)
	    {
		if (*decided != loc
		    && (match_string (weekday_name[cnt], rp)
			|| match_string (ab_weekday_name[cnt], rp)))
		{
		    *decided = raw;
		    break;
		}
	    }
	    if (cnt == 7)
		/* Does not match a weekday name.  */
		return NULL;
	    tm->tm_wday = cnt;
	    have_wday = 1;
	    break;
	case 'b':
	case 'B':
	case 'h':
	    /* Match month name.  */
	    if(!locale_strings_set) get_locale_strings();
	    for (cnt = 0; cnt < 12; ++cnt)
	    {
		if (match_string (month_name[cnt], rp)
		    || match_string (ab_month_name[cnt], rp))
		{
		    *decided = raw;
		    break;
		}
	    }
	    if (cnt == 12)
		/* Does not match a month name.  */
		return NULL;
	    tm->tm_mon = cnt;
	    want_xday = 1;
	    break;
	case 'c':
	    /* Match locale's date and time format.  */
	    if (!recursive (HERE_D_T_FMT))
		return NULL;
	    break;
	case 'C':
	  /* Match century number.  */
	  get_number (0, 99, 2);
	  century = val;
	  want_xday = 1;
	  break;
	case 'd':
	case 'e':
	  /* Match day of month.  */
	  get_number (1, 31, 2);
	  tm->tm_mday = val;
	  have_mday = 1;
	  want_xday = 1;
	  break;
	case 'F':
	  if (!recursive ("%Y-%m-%d"))
	    return NULL;
	  want_xday = 1;
	  break;
	case 'x':
	  /* Fall through.  */
	case 'D':
	  /* Match standard day format.  */
	  if (!recursive (HERE_D_FMT))
	    return NULL;
	  want_xday = 1;
	  break;
	case 'k':
	case 'H':
	  /* Match hour in 24-hour clock.  */
	  get_number (0, 24, 2);  /* allow 24:00:00 */
	  tm->tm_hour = val;
	  have_I = 0;
	  break;
	case 'l':
	  /* Match hour in 12-hour clock.  GNU extension.  */
	case 'I':
	  /* Match hour in 12-hour clock.  */
	  get_number (1, 12, 2);
	  tm->tm_hour = val % 12;
	  have_I = 1;
	  break;
	case 'j':
	  /* Match day number of year.  */
	  get_number (1, 366, 3);
	  tm->tm_yday = val - 1;
	  have_yday = 1;
	  break;
	case 'm':
	  /* Match number of month.  */
	  get_number (1, 12, 2);
	  tm->tm_mon = val - 1;
	  have_mon = 1;
	  want_xday = 1;
	  break;
	case 'M':
	  /* Match minute.  */
	  get_number (0, 59, 2);
	  tm->tm_min = val;
	  break;
	case 'n':
	case 't':
	  /* Match any white space.  */
	  while (isspace ((int)*rp))
	    ++rp;
	  break;
	case 'p':
	  /* Match locale's equivalent of AM/PM.  */
	  if(!locale_strings_set) get_locale_strings();
	  if (!match_string (am_pm[0], rp)) {
	    if (match_string (am_pm[1], rp))
	      is_pm = 1;
	    else
		return NULL;
	  }
	  break;
	case 'r':
	  if (!recursive (HERE_T_FMT_AMPM))
	    return NULL;
	  break;
	case 'R':
	    if (!recursive ("%H:%M"))
		return NULL;
	    break;
	case 's':
	{
	    /* The number of seconds may be very high so we cannot use
	       the `get_number' macro.  Instead read the number
	       character for character and construct the result while
	       doing this.  */
	    time_t secs = 0;
	    if (*rp < '0' || *rp > '9')
		/* We need at least one digit.  */
		return NULL;

	    do
	    {
		secs *= 10;
		secs += *rp++ - '0';
	    }
	    while (*rp >= '0' && *rp <= '9');

#ifdef HAVE_LOCALTIME_R
	    if ((tm = localtime_r (&secs, tm)) == NULL) return NULL;
#else
	    if ((tm = localtime (&secs)) == NULL) return NULL;
#endif
	}
	break;
	case 'S':
	    get_number (0, 61, 2);
	    tm->tm_sec = val;
	    break;
	case 'X':
	    /* Fall through.  */
	case 'T':
	    if (!recursive (HERE_T_FMT))
		return NULL;
	    break;
	case 'u':
	    get_number (1, 7, 1);
	    tm->tm_wday = val % 7;
	    have_wday = 1;
	    break;
	case 'g':
	    get_number (0, 99, 2);
	    /* XXX This cannot determine any field in TM.  */
	    break;
	case 'G':
	    if (*rp < '0' || *rp > '9')
		return NULL;
	    /* XXX Ignore the number since we would need some more
	       information to compute a real date.  */
	    do
		++rp;
	    while (*rp >= '0' && *rp <= '9');
	    break;
	case 'U':
	  get_number (0, 53, 2);
	  week_no = val;
	  have_uweek = 1;
	  break;
	case 'W':
	  get_number (0, 53, 2);
	  week_no = val;
	  have_wweek = 1;
	  break;
	case 'V':
	    get_number (0, 53, 2);
	    /* XXX This cannot determine any field in TM without some
	       information.  */
	    break;
	case 'w':
	    /* Match number of weekday.  */
	    get_number (0, 6, 1);
	    tm->tm_wday = val;
	    have_wday = 1;
	    break;
	case 'y':
	    /* Match year within century.  */
	    get_number (0, 99, 2);
	    /* The "Year 2000: The Millennium Rollover" paper suggests that
	       values in the range 69-99 refer to the twentieth century.
	       And this is mandated by the POSIX 2001 standard, with a
	       caveat that it might change in future.
	    */
	    int ival = val;
	    tm->tm_year = ival >= 69 ? ival : ival + 100;
	    /* Indicate that we want to use the century, if specified.  */
	    want_century = 1;
	    want_xday = 1;
	    break;
	case 'Y':
	    /* Match year including century number.  */
	    get_number (0, 9999, 4);
	    tm->tm_year = val - 1900;
	    want_century = 0;
	    want_xday = 1;
	    break;
	case 'z':
	    /* Only recognize RFC 822 form */
	    {
		int n = 0, neg, off = 0;
		val = 0;
		while (*rp == ' ') ++rp;
		if (*rp != '+' && *rp != '-') return NULL;
		neg = *rp++ == '-';
		while (n < 4 && *rp >= '0' && *rp <= '9') {
		    val = val * 10 + *rp++ - '0';
		    ++n;
		}
		if (n != 4) return NULL;
		else {
		    /* We have to convert the minutes into decimal.  */
		    if (val % 100 >= 60) return NULL;
		    val = (val / 100) * 100 + ((val % 100) * 50) / 30;
		}
		if (val > 1200) return NULL;
		off = (val * 3600) / 100;
		if (neg) off = -off;
		*poffset = off;
	    }
	    break;
	case 'Z':
	    error(_("use of %s for input is not supported"), "%Z");
	    return NULL;
	    break;
	case 'E':
	    /* We have no information about the era format.  Just use
	       the normal format.  */
	    if (*fmt != 'c' && *fmt != 'C' && *fmt != 'y' && *fmt != 'Y'
		&& *fmt != 'x' && *fmt != 'X')
		/* This is an illegal format.  */
		return NULL;

	    goto start_over;
	case 'O':
	    switch (*fmt++)
	    {
	    case 'd':
	    case 'e':
		/* Match day of month using alternate numeric symbols.  */
		get_alt_number (1, 31, 2);
		tm->tm_mday = val;
		have_mday = 1;
		want_xday = 1;
		break;
	    case 'H':
		/* Match hour in 24-hour clock using alternate numeric
		   symbols.  */
		get_alt_number (0, 23, 2);
		tm->tm_hour = val;
		have_I = 0;
		break;
	    case 'I':
		/* Match hour in 12-hour clock using alternate numeric
		   symbols.  */
		get_alt_number (1, 12, 2);
		tm->tm_hour = val % 12;
		have_I = 1;
		break;
	    case 'm':
		/* Match month using alternate numeric symbols.  */
		get_alt_number (1, 12, 2);
		tm->tm_mon = val - 1;
		have_mon = 1;
		want_xday = 1;
		break;
	    case 'M':
		/* Match minutes using alternate numeric symbols.  */
		get_alt_number (0, 59, 2);
		tm->tm_min = val;
		break;
	    case 'S':
		/* Match seconds using alternate numeric symbols.
		   get_alt_number (0, 61, 2); */
		   {
		       double sval;
		       char *end;
		       sval = strtod(rp, &end);
		       if( sval >= 0.0 && sval <= 61.0) {
			   tm->tm_sec = (int) sval;
			   *psecs = sval;
		       }
		       rp = end;
		   }
		break;
	    case 'U':
	      get_alt_number (0, 53, 2);
	      week_no = val;
	      have_uweek = 1;
	      break;
	    case 'W':
	      get_alt_number (0, 53, 2);
	      week_no = val;
	      have_wweek = 1;
	      break;
	    case 'V':
		get_alt_number (0, 53, 2);
		/* XXX This cannot determine any field in TM without
		   further information.  */
		break;
	    case 'w':
		/* Match number of weekday using alternate numeric symbols.  */
		get_alt_number (0, 6, 1);
		tm->tm_wday = val;
		have_wday = 1;
		break;
	    case 'y':
		/* Match year within century using alternate numeric symbols.  */
		get_alt_number (0, 99, 2);
		int ival = val;
		tm->tm_year = ival >= 69 ? ival : ival + 100;
		want_xday = 1;
		break;
	    default:
		return NULL;
	    }
	    break;
	default:
	    return NULL;
	}
    }

    if (have_I && is_pm)
	tm->tm_hour += 12;

    if (century != -1)
    {
	if (want_century)
	    tm->tm_year = tm->tm_year % 100 + (century - 19) * 100;
	else
	    /* Only the century, but not the year.  Strange, but so be it.  */
	    tm->tm_year = (century - 19) * 100;
    }

    if (want_xday && !have_wday) {
	if ( !(have_mon && have_mday) && have_yday)  {
	    /* We don't have tm_mon and/or tm_mday, compute them. */
	    int t_mon = 0;
	    while (__mon_yday[__isleap(1900 + tm->tm_year)][t_mon] <= tm->tm_yday)
		t_mon++;
	    if (!have_mon)
		tm->tm_mon = t_mon - 1;
	    if (!have_mday)
		tm->tm_mday = (tm->tm_yday - __mon_yday[__isleap(1900 + tm->tm_year)][t_mon - 1] + 1);
	}
	day_of_the_week (tm);
    }

    if (want_xday && !have_yday)
	day_of_the_year (tm);

  if ((have_uweek || have_wweek) && have_wday) {
      int save_wday = tm->tm_wday;
      int save_mday = tm->tm_mday;
      int save_mon = tm->tm_mon;
      int w_offset = have_uweek ? 0 : 1;

      tm->tm_mday = 1;
      tm->tm_mon = 0;
      day_of_the_week (tm);
      if (have_mday)
	  tm->tm_mday = save_mday;
      if (have_mon)
	  tm->tm_mon = save_mon;

      if (!have_yday)
	  tm->tm_yday = ((7 - (tm->tm_wday - w_offset)) % 7
			 + (week_no - 1) *7
			 + save_wday - w_offset);

      if (!have_mday || !have_mon)
      {
	  int t_mon = 0;
	  while (__mon_yday[__isleap(1900 + tm->tm_year)][t_mon]
		 <= tm->tm_yday)
	      t_mon++;
	  if (!have_mon)
	      tm->tm_mon = t_mon - 1;
	  if (!have_mday)
	      tm->tm_mday =
		  (tm->tm_yday
		   - __mon_yday[__isleap(1900 + tm->tm_year)][t_mon - 1] + 1);
      }

      tm->tm_wday = save_wday;
  }

    return (char *) rp;
}


void dt_invalidate_locale() // used in plaform.c
{
    locale_strings_set = 0;
    locale_w_strings_set = 0;
}

/* use system stuct tm and strftime/wcstime here */
static void get_locale_strings(void)
{
    int i;
    struct tm tm;
    char buff[4];

    tm.tm_sec = tm.tm_min = tm.tm_hour = tm.tm_mday = tm.tm_mon
	= tm.tm_isdst = 0;
    tm.tm_year = 30;
    for(i = 0; i < 12; i++) {
	tm.tm_mon = i;
	strftime(ab_month_name[i], 10, "%b", &tm);
	strftime(month_name[i], 20, "%B", &tm);
    }
    tm.tm_mon = 0;
    for(i = 0; i < 7; i++) {
	tm.tm_mday = tm.tm_yday = i+1; /* 2000-1-2 was a Sunday */
	tm.tm_wday = i;
	strftime(ab_weekday_name[i], 10, "%a", &tm);
	strftime(weekday_name[i], 20, "%A", &tm);
    }
    tm.tm_hour = 1;
    /* in locales where these are unused, they may be empty: better
       not to reset them then */
    strftime(buff, 4, "%p", &tm);
    if(strlen(buff)) strcpy(am_pm[0], buff);
    tm.tm_hour = 13;
    strftime(buff, 4, "%p", &tm);
    if(strlen(buff)) strcpy(am_pm[1], buff);
    locale_strings_set = 1;
}

#if defined(HAVE_WCSTOD) && defined(HAVE_WCSFTIME)
static void get_locale_w_strings(void)
{
    int i;
    struct tm tm;
    wchar_t buff[4];

    tm.tm_sec = tm.tm_min = tm.tm_hour = tm.tm_mday = tm.tm_mon
	= tm.tm_isdst = 0;
    tm.tm_year = 30;
    for(i = 0; i < 12; i++) {
	tm.tm_mon = i;
	wcsftime(w_ab_month_name[i], 10, L"%b", &tm);
	wcsftime(w_month_name[i], 20, L"%B", &tm);
    }
    tm.tm_mon = 0;
    for(i = 0; i < 7; i++) {
	tm.tm_mday = tm.tm_yday = i+1; /* 2000-1-2 was a Sunday */
	tm.tm_wday = i;
	wcsftime(w_ab_weekday_name[i], 10, L"%a", &tm);
	wcsftime(w_weekday_name[i], 20, L"%A", &tm);
    }
    tm.tm_hour = 1;
    /* in locales where these are unused, they may be empty: better
       not to reset them then */
    wcsftime(buff, 4, L"%p", &tm);
    if(wcslen(buff)) wcscpy(w_am_pm[0], buff);
    tm.tm_hour = 13;
    wcsftime(buff, 4, L"%p", &tm);
    if(wcslen(buff)) wcscpy(w_am_pm[1], buff);
    locale_w_strings_set = 1;
}
#endif


/* We only care if the result is null or not */
static char *
R_strptime (const char *buf, const char *format, stm *tm, 
	    double *psecs, int *poffset)
{
    enum locale_status decided;
    decided = raw;
#if defined(HAVE_WCSTOD)
    if(mbcslocale) {
	wchar_t wbuf[1001], wfmt[1001]; size_t n;
	n = mbstowcs(NULL, buf, 1000);
	if(n > 1000) error(_("input string is too long"));
	n = mbstowcs(wbuf, buf, 1000);
	if(n == -1) error(_("invalid multibyte input string"));

	n = mbstowcs(NULL, format, 1000);
	if(n > 1000) error(_("format string is too long"));
	n = mbstowcs(wfmt, format, 1000);
	if(n == -1) error(_("invalid multibyte format string"));
	return (char *) w_strptime_internal (wbuf, wfmt, tm, &decided, psecs, poffset);
    } else
#endif
    {
	return strptime_internal (buf, format, tm, &decided, psecs, poffset);
    }
}
