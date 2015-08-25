/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Modifications copyright (C) 2007-2015  The R Core Team
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
The orginal version of this file stated

** This file is in the public domain, so clarified as of
** 1996-06-05 by Arthur David Olson.

The modified version is copyrighted.  Modifications include:
setting EOVERFLOW
where to find the zi database
Mingw-w64 changes
removing ATTRIBUTE_PURE, conditional parts for e.g. ALL_STATE
use of 'unknown' isdst
use of 64-bit time_t irrespective of platform.
use of tm_zone and tm_gmtoff on all platforms.
*/

#include <config.h>
#include <string.h>
#include <limits.h>	/* for CHAR_BIT et al. */

// to get tm_zone, tm_gmtoff defined in glibc.
// some other header, e.g. math.h, might define the macro.
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
#include <time.h>

#include <errno.h>
#ifndef EOVERFLOW
# define EOVERFLOW 79
#endif

#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h> // for open + modes

#ifndef _WIN32
# include <unistd.h> // for access, read, close
#endif

#include "datetime.h"
#define tzname R_tzname

#ifndef TRUE
#define TRUE	1
#endif /* !defined TRUE */

#ifndef FALSE
#define FALSE	0
#endif /* !defined FALSE */

/* merged from private.h */
#ifndef TYPE_BIT
#define TYPE_BIT(type)	(sizeof (type) * CHAR_BIT)
#endif /* !defined TYPE_BIT */

#ifndef TYPE_SIGNED
#define TYPE_SIGNED(type) (((type) -1) < 0)
#endif /* !defined TYPE_SIGNED */

#define TWOS_COMPLEMENT(t) ((t) ~ (t) 0 < 0)

#define GRANDPARENTED	"Local time zone must be set--see zic manual page"
#define YEARSPERREPEAT	 400	/* years before a Gregorian repeat */
#define AVGSECSPERYEAR	 31556952L
#define SECSPERREPEAT ((int_fast64_t) YEARSPERREPEAT * (int_fast64_t) AVGSECSPERYEAR)
#define SECSPERREPEAT_BITS  34	/* ceil(log2(SECSPERREPEAT)) */
#define is_digit(c) ((unsigned)(c) - '0' <= 9)
#define INITIALIZE(x) (x = 0)

/* Max and min values of the integer type T, of which only the bottom
   B bits are used, and where the highest-order used bit is considered
   to be a sign bit if T is signed.  */
#define MAXVAL(t, b)						\
  ((t) (((t) 1 << ((b) - 1 - TYPE_SIGNED(t)))			\
	- 1 + ((t) 1 << ((b) - 1 - TYPE_SIGNED(t)))))
#define MINVAL(t, b)						\
  ((t) (TYPE_SIGNED(t) ? - TWOS_COMPLEMENT(t) - MAXVAL(t, b) : 0))

/* The minimum and maximum finite time values.  This assumes no padding.  */
static time_t const time_t_min = MINVAL(time_t, TYPE_BIT(time_t));
static time_t const time_t_max = MAXVAL(time_t, TYPE_BIT(time_t));


#include "tzfile.h"

#ifndef TZ_ABBR_MAX_LEN
#define TZ_ABBR_MAX_LEN	16
#endif /* !defined TZ_ABBR_MAX_LEN */

#ifndef TZ_ABBR_CHAR_SET
#define TZ_ABBR_CHAR_SET \
	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 :+-._"
#endif /* !defined TZ_ABBR_CHAR_SET */

#ifndef TZ_ABBR_ERR_CHAR
#define TZ_ABBR_ERR_CHAR	'_'
#endif /* !defined TZ_ABBR_ERR_CHAR */

/*
** SunOS 4.1.1 headers lack O_BINARY.
*/

#ifdef O_BINARY
#define OPEN_MODE	(O_RDONLY | O_BINARY)
#endif /* defined O_BINARY */
#ifndef O_BINARY
#define OPEN_MODE	O_RDONLY
#endif /* !defined O_BINARY */

#ifndef WILDABBR
/*
** Someone might make incorrect use of a time zone abbreviation:
**	1.	They might reference tzname[0] before calling tzset (explicitly
**		or implicitly).
**	2.	They might reference tzname[1] before calling tzset (explicitly
**		or implicitly).
**	3.	They might reference tzname[1] after setting to a time zone
**		in which Daylight Saving Time is never observed.
**	4.	They might reference tzname[0] after setting to a time zone
**		in which Standard Time is never observed.
**	5.	They might reference tm.TM_ZONE after calling offtime.
** What's best to do in the above cases is open to debate;
** for now, we just set things up so that in any of the five cases
** WILDABBR is used. Another possibility: initialize tzname[0] to the
** string "tzname[0] used before set", and similarly for the other cases.
** And another: initialize tzname[0] to "ERA", with an explanation in the
** manual page of what this "time zone abbreviation" means (doing this so
** that tzname[0] has the "normal" length of three characters).
*/
#define WILDABBR	"   "
#endif /* !defined WILDABBR */

static char		wildabbr[] = WILDABBR;

static const char	gmt[] = "GMT";

/*
** The DST rules to use if TZ has no rules and we can't load TZDEFRULES.
** We default to US rules as of 1999-08-17.
** POSIX 1003.1 section 8.1.1 says that the default DST rules are
** implementation dependent; for historical reasons, US rules are a
** common default.
*/
#ifndef TZDEFRULESTRING
#define TZDEFRULESTRING ",M4.1.0,M10.5.0"
#endif /* !defined TZDEFDST */

struct ttinfo {				/* time type information */
	int_fast32_t	tt_gmtoff;	/* UT offset in seconds */
	int		tt_isdst;	/* used to set tm_isdst */
	int		tt_abbrind;	/* abbreviation list index */
	int		tt_ttisstd;	/* TRUE if transition is std time */
	int		tt_ttisgmt;	/* TRUE if transition is UT */
};

struct lsinfo {				/* leap second information */
	time_t		ls_trans;	/* transition time */
	int_fast64_t	ls_corr;	/* correction to apply */
};

#define BIGGEST(a, b)	(((a) > (b)) ? (a) : (b))

#ifdef TZNAME_MAX
#define MY_TZNAME_MAX	TZNAME_MAX
#endif /* defined TZNAME_MAX */
#ifndef TZNAME_MAX
#define MY_TZNAME_MAX	255
#endif /* !defined TZNAME_MAX */

struct state {
    int		leapcnt;
    int		timecnt;
    int		typecnt;
    int		charcnt;
    int		goback;
    int		goahead;
    time_t		ats[TZ_MAX_TIMES];
    unsigned char	types[TZ_MAX_TIMES];
    struct ttinfo	ttis[TZ_MAX_TYPES];
    char		chars[BIGGEST(BIGGEST(TZ_MAX_CHARS + 1, sizeof gmt),
				      (2 * (MY_TZNAME_MAX + 1)))];
    struct lsinfo	lsis[TZ_MAX_LEAPS];
    int		defaulttype; /* for early times or if no transitions */
};

struct rule {
    int		r_type;		/* type of rule--see below */
    int		r_day;		/* day number of rule */
    int		r_week;		/* week number of rule */
    int		r_mon;		/* month number of rule */
    int_fast32_t	r_time;		/* transition time of rule */
};

#define JULIAN_DAY		0	/* Jn - Julian day */
#define DAY_OF_YEAR		1	/* n - day of year */
#define MONTH_NTH_DAY_OF_WEEK	2	/* Mm.n.d - month, week, day of week */

/*
** Prototypes for static functions.
*/

static int_fast32_t	detzcode(const char * codep);
static int_fast64_t	detzcode64(const char * codep);
static int		differ_by_repeat(time_t t1, time_t t0);
static const char *	getzname(const char * strp);
static const char *	getqzname(const char * strp, const int delim);
static const char *	getnum(const char * strp, int * nump, int min,
				int max);
static const char *	getsecs(const char * strp, int_fast32_t * secsp);
static const char *	getoffset(const char * strp, int_fast32_t * offsetp);
static const char *	getrule(const char * strp, struct rule * rulep);
static void		gmtload(struct state * sp);
static stm *	gmtsub(const time_t * timep, int_fast32_t offset, stm * tmp);
static stm *	localsub(const time_t * timep, int_fast32_t offset, stm * tmp);
static int		increment_overflow(int * number, int delta);
static int		leaps_thru_end_of(int y);
static int		increment_overflow32(int_fast32_t * number, int delta);
static int		increment_overflow_time(time_t *t, int_fast32_t delta);
static int		normalize_overflow32(int_fast32_t * tensptr,
				int * unitsptr, int base);
static int		normalize_overflow(int * tensptr, int * unitsptr,
				int base);
static void		settzname(void);
static time_t		time1(stm * tmp,
				stm * (*funcp)(const time_t *,
				int_fast32_t, stm *),
				int_fast32_t offset);
static time_t		time2(stm *tmp,
				stm * (*funcp)(const time_t *,
				int_fast32_t, stm*),
				int_fast32_t offset, int * okayp);
static time_t		time2sub(stm *tmp,
				stm * (*funcp)(const time_t *,
				int_fast32_t, stm*),
				int_fast32_t offset, int * okayp, int do_norm_secs);
static stm *	timesub(const time_t * timep, int_fast32_t offset,
				const struct state * sp, stm * tmp);
static int		tmcomp(const stm * atmp,
				const stm * btmp);
static int_fast32_t	transtime(int year, const struct rule * rulep,
				  int_fast32_t offset);
static int		typesequiv(const struct state * sp, int a, int b);
static int		tzload(const char * name, struct state * sp,
				int doextend);
static int		tzparse(const char * name, struct state * sp,
				int lastditch);

static struct state	lclmem;
static struct state	gmtmem;
#define lclptr		(&lclmem)
#define gmtptr		(&gmtmem)

#ifndef TZ_STRLEN_MAX
#define TZ_STRLEN_MAX 255
#endif /* !defined TZ_STRLEN_MAX */

static char		lcl_TZname[TZ_STRLEN_MAX + 1];
static int		lcl_is_set;
static int		gmt_is_set;

char * tzname[2] = {
    wildabbr,
    wildabbr
};

/*
** Section 4.12.3 of X3.159-1989 requires that
**	Except for the strftime function, these functions [asctime,
**	ctime, gmtime, localtime] return values in one of two static
**	objects: a broken-down time structure and an array of char.
** Thanks to Paul Eggert for noting this.
*/

static stm  tm;

static int_fast32_t
detzcode(const char *const codep)
{
    int_fast32_t result = (codep[0] & 0x80) ? -1 : 0;
    for (int i = 0; i < 4; ++i)
	result = (result << 8) | (codep[i] & 0xff);
    return result;
}

static int_fast64_t
detzcode64(const char *const codep)
{
    int_fast64_t result = (codep[0] & 0x80) ? -1 : 0;
    for (int i = 0; i < 8; ++i)
	result = (result << 8) | (codep[i] & 0xff);
    return result;
}

static void
settzname(void)
{
    struct state * const sp = lclptr;

    tzname[0] = wildabbr;
    tzname[1] = wildabbr;
    /*
    ** And to get the latest zone names into tzname. . .
    */
    for (int i = 0; i < sp->typecnt; ++i) {
	const struct ttinfo * const ttisp = &sp->ttis[i];
	tzname[ttisp->tt_isdst] = &sp->chars[ttisp->tt_abbrind];
    }
    for (int i = 0; i < sp->timecnt; ++i) {
	const struct ttinfo * const ttisp = &sp->ttis[sp->types[i]];
	tzname[ttisp->tt_isdst] = &sp->chars[ttisp->tt_abbrind];
    }
    /*
    ** Finally, scrub the abbreviations.
    ** First, replace bogus characters.
    */
    for (int i = 0; i < sp->charcnt; ++i)
	if (strchr(TZ_ABBR_CHAR_SET, sp->chars[i]) == NULL)
	    sp->chars[i] = TZ_ABBR_ERR_CHAR;
    /*
    ** Second, truncate long abbreviations.
    */
    for (int i = 0; i < sp->typecnt; ++i) {
	const struct ttinfo * const ttisp = &sp->ttis[i];
	char * cp = &sp->chars[ttisp->tt_abbrind];

	if (strlen(cp) > TZ_ABBR_MAX_LEN && strcmp(cp, GRANDPARENTED) != 0)
	    *(cp + TZ_ABBR_MAX_LEN) = '\0';
    }
}

static int
differ_by_repeat(const time_t t1, const time_t t0)
{
    if (TYPE_BIT(time_t) - TYPE_SIGNED(time_t) < SECSPERREPEAT_BITS)
	return 0;
    /* R change */
    return (int_fast64_t)t1 - (int_fast64_t)t0 == SECSPERREPEAT;
}

extern const char *getTZinfo(void);
extern void Rf_warning(const char *, ...);

static int
tzload(const char * name, struct state * const sp, const int doextend)
{
    const char * p;
    int	 i;
    int	 fid;
    ssize_t nread;
    typedef union {
	struct tzhead  tzhead;
	char  buf[2 * sizeof(struct tzhead) + 
		  2 * sizeof *sp + 4 * TZ_MAX_TIMES];
    } u_t;

    u_t	 u;
    u_t * const	up = &u;

    sp->goback = sp->goahead = FALSE;
    /* if (name == NULL && (name = TZDEFAULT) == NULL) return -1; */
    if (name == NULL) {
	name = getTZinfo();
	if( strcmp(name, "unknown") == 0 ) name = TZDEFAULT;
    }
	
    {
	int  doaccess;
	/*
	** Section 4.9.1 of the C standard says that
	** "FILENAME_MAX expands to an integral constant expression
	** that is the size needed for an array of char large enough
	** to hold the longest file name string that the implementation
	** guarantees can be opened."
	*/
	char fullname[FILENAME_MAX + 1];
	const char *sname = name;

	if (name[0] == ':')
	    ++name;
	doaccess = name[0] == '/';
	if (!doaccess) {
	    char buf[1000];
	    p = getenv("TZDIR");
	    if (p == NULL) {
		p = getenv("R_SHARE_DIR");
		if(p)
		    snprintf(buf, 1000, "%s/zoneinfo", p);
		else
		    snprintf(buf, 1000, "%s/share/zoneinfo", getenv("R_HOME"));
		buf[999] = '\0';
		p = buf;
	    }
	    /* if ((p = TZDIR) == NULL) return -1; */
	    if ((strlen(p) + strlen(name) + 1) >= sizeof fullname)
		return -1;
	    (void) strcpy(fullname, p);
	    (void) strcat(fullname, "/");
	    (void) strcat(fullname, name);
	    /*
	    ** Set doaccess if '.' (as in "../") shows up in name.
	    */
	    if (strchr(name, '.') != NULL) doaccess = TRUE;
	    name = fullname;
	}
	if (doaccess && access(name, R_OK) != 0) {
	    Rf_warning("unknown timezone '%s'", sname);
	    return -1;
	}
	if ((fid = open(name, OPEN_MODE)) == -1) {
	    Rf_warning("unknown timezone '%s'", sname);
	    return -1;
	}
		
    }
    nread = read(fid, up->buf, sizeof up->buf);
    if (close(fid) < 0 || nread <= 0)
	return -1;
    for (int stored = 4; stored <= 8; stored *= 2) {
	int  ttisstdcnt, ttisgmtcnt, timecnt;

	ttisstdcnt = (int) detzcode(up->tzhead.tzh_ttisstdcnt);
	ttisgmtcnt = (int) detzcode(up->tzhead.tzh_ttisgmtcnt);
	sp->leapcnt = (int) detzcode(up->tzhead.tzh_leapcnt);
	sp->timecnt = (int) detzcode(up->tzhead.tzh_timecnt);
	sp->typecnt = (int) detzcode(up->tzhead.tzh_typecnt);
	sp->charcnt = (int) detzcode(up->tzhead.tzh_charcnt);
	p = up->tzhead.tzh_charcnt + sizeof up->tzhead.tzh_charcnt;
	if (sp->leapcnt < 0 || sp->leapcnt > TZ_MAX_LEAPS ||
	    sp->typecnt <= 0 || sp->typecnt > TZ_MAX_TYPES ||
	    sp->timecnt < 0 || sp->timecnt > TZ_MAX_TIMES ||
	    sp->charcnt < 0 || sp->charcnt > TZ_MAX_CHARS ||
	    (ttisstdcnt != sp->typecnt && ttisstdcnt != 0) ||
	    (ttisgmtcnt != sp->typecnt && ttisgmtcnt != 0))
	    return -1;
	if (nread - (p - up->buf) <
	    sp->timecnt * stored +	  /* ats */
	    sp->timecnt +		  /* types */
	    sp->typecnt * 6 +		  /* ttinfos */
	    sp->charcnt +		  /* chars */
	    sp->leapcnt * (stored + 4) +  /* lsinfos */
	    ttisstdcnt +		  /* ttisstds */
	    ttisgmtcnt)			  /* ttisgmts */
	    return -1;
	timecnt = 0;
	for (int i = 0; i < sp->timecnt; ++i) {
	    int_fast64_t at = stored == 4 ? detzcode(p) : detzcode64(p);
	    sp->types[i] = ((TYPE_SIGNED(time_t) ? time_t_min <= at : 0 <= at)
			    && at <= time_t_max);
	    if (sp->types[i]) {
		if (i && !timecnt && at != time_t_min) {
		    /*
		    ** Keep the earlier record, but tweak
		    ** it so that it starts with the
		    ** minimum time_t value.
		    */
		    sp->types[i - 1] = 1;
		    sp->ats[timecnt++] = time_t_min;
		}
		sp->ats[timecnt++] = at;
	    }
	    p += stored;
	}
	timecnt = 0;
	for (int i = 0; i < sp->timecnt; ++i) {
	    unsigned char typ = *p++;
	    if (sp->typecnt <= typ) return -1;
	    if (sp->types[i])
		sp->types[timecnt++] = typ;
	}
	sp->timecnt = timecnt;
	for (int i = 0; i < sp->typecnt; ++i) {
	    struct ttinfo * ttisp;

	    ttisp = &sp->ttis[i];
	    ttisp->tt_gmtoff = detzcode(p);
	    p += 4;
	    ttisp->tt_isdst = (unsigned char) *p++;
	    if (ttisp->tt_isdst != 0 && ttisp->tt_isdst != 1)
		return -1;
	    ttisp->tt_abbrind = (unsigned char) *p++;
	    if (ttisp->tt_abbrind < 0 ||
		ttisp->tt_abbrind > sp->charcnt)
		return -1;
	}
	for (i = 0; i < sp->charcnt; ++i)
	    sp->chars[i] = *p++;
	sp->chars[i] = '\0';	/* ensure '\0' at end */
	for (int i = 0; i < sp->leapcnt; ++i) {
	    struct lsinfo * lsisp;

	    lsisp = &sp->lsis[i];
	    lsisp->ls_trans = (stored == 4) ? detzcode(p) : detzcode64(p);
	    p += stored;
	    lsisp->ls_corr = detzcode(p);
	    p += 4;
	}
	for (int i = 0; i < sp->typecnt; ++i) {
	    struct ttinfo * ttisp;

	    ttisp = &sp->ttis[i];
	    if (ttisstdcnt == 0)
		ttisp->tt_ttisstd = FALSE;
	    else {
		ttisp->tt_ttisstd = *p++;
		if (ttisp->tt_ttisstd != TRUE && ttisp->tt_ttisstd != FALSE)
		    return -1;
	    }
	}
	for (int i = 0; i < sp->typecnt; ++i) {
	    struct ttinfo * ttisp;

	    ttisp = &sp->ttis[i];
	    if (ttisgmtcnt == 0)
		ttisp->tt_ttisgmt = FALSE;
	    else {
		ttisp->tt_ttisgmt = *p++;
		if (ttisp->tt_ttisgmt != TRUE && ttisp->tt_ttisgmt != FALSE)
		    return -1;
	    }
	}
	/*
	** If this is an old file, we're done.
	*/
	if (up->tzhead.tzh_version[0] == '\0')
	    break;
	nread -= p - up->buf;
	for (int i = 0; i < nread; ++i)
	    up->buf[i] = p[i];
	/*
	** If this is a signed narrow time_t system, we're done.
	*/
	if (TYPE_SIGNED(time_t) && stored >= (int) sizeof(time_t))
	    break;
    }
    if (doextend && nread > 2 &&
	up->buf[0] == '\n' && up->buf[nread - 1] == '\n' &&
	sp->typecnt + 2 <= TZ_MAX_TYPES) {
	struct state ts;
	int result;

	up->buf[nread - 1] = '\0';
	result = tzparse(&up->buf[1], &ts, FALSE);
	if (result == 0 && ts.typecnt == 2 &&
	    sp->charcnt + ts.charcnt <= TZ_MAX_CHARS) {
	    for (int i = 0; i < 2; ++i)
		ts.ttis[i].tt_abbrind += sp->charcnt;
	    for (int i = 0; i < ts.charcnt; ++i)
		sp->chars[sp->charcnt++] = ts.chars[i];
	    i = 0;
	    while (i < ts.timecnt && ts.ats[i] <= sp->ats[sp->timecnt - 1])
		++i;
	    while (i < ts.timecnt &&
		   sp->timecnt < TZ_MAX_TIMES) {
		sp->ats[sp->timecnt] = ts.ats[i];
		sp->types[sp->timecnt] = 
		    (unsigned char)(sp->typecnt + ts.types[i]);
		++sp->timecnt;
		++i;
	    }
	    sp->ttis[sp->typecnt++] = ts.ttis[0];
	    sp->ttis[sp->typecnt++] = ts.ttis[1];
	}
    }
    if (sp->timecnt > 1) {
	for (int i = 1; i < sp->timecnt; ++i)
	    if (typesequiv(sp, sp->types[i], sp->types[0]) &&
		differ_by_repeat(sp->ats[i], sp->ats[0])) {
		sp->goback = TRUE;
		break;
	    }
	for (int i = sp->timecnt - 2; i >= 0; --i)
	    if (typesequiv(sp, sp->types[sp->timecnt - 1],
			   sp->types[i]) &&
		differ_by_repeat(sp->ats[sp->timecnt - 1],
				 sp->ats[i])) {
		sp->goahead = TRUE;
		break;
	    }
    }
    /*
    ** If type 0 is is unused in transitions,
    ** it's the type to use for early times.
    */
    for (i = 0; i < sp->typecnt; ++i)
	if (sp->types[i] == 0)
	    break;
    i = (i >= sp->typecnt) ? 0 : -1;
    /*
    ** Absent the above,
    ** if there are transition times
    ** and the first transition is to a daylight time
    ** find the standard type less than and closest to
    ** the type of the first transition.
    */
    if (i < 0 && sp->timecnt > 0 && sp->ttis[sp->types[0]].tt_isdst) {
	i = sp->types[0];
	while (--i >= 0)
	    if (!sp->ttis[i].tt_isdst)
		break;
    }
    /*
    ** If no result yet, find the first standard type.
    ** If there is none, punt to type zero.
    */
    if (i < 0) {
	i = 0;
	while (sp->ttis[i].tt_isdst)
	    if (++i >= sp->typecnt) {
		i = 0;
		break;
	    }
    }
    sp->defaulttype = i;
    return 0;
}

static int
typesequiv(const struct state * const sp, const int a, const int b)
{
    int	result;

    if (sp == NULL ||
	a < 0 || a >= sp->typecnt ||
	b < 0 || b >= sp->typecnt)
	result = FALSE;
    else {
	const struct ttinfo * ap = &sp->ttis[a];
	const struct ttinfo * bp = &sp->ttis[b];
	result = ap->tt_gmtoff == bp->tt_gmtoff &&
	    ap->tt_isdst == bp->tt_isdst &&
	    ap->tt_ttisstd == bp->tt_ttisstd &&
	    ap->tt_ttisgmt == bp->tt_ttisgmt &&
	    strcmp(&sp->chars[ap->tt_abbrind],
		   &sp->chars[bp->tt_abbrind]) == 0;
    }
    return result;
}

static const int mon_lengths[2][MONSPERYEAR] = {
    { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 },
    { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 }
};

static const int year_lengths[2] = {
    DAYSPERNYEAR, DAYSPERLYEAR
};

/*
** Given a pointer into a time zone string, scan until a character that is not
** a valid character in a zone name is found. Return a pointer to that
** character.
*/

static const char *
getzname(const char * strp)
{
    char c;

    while ((c = *strp) != '\0' && !is_digit(c) && c != ',' && c != '-' &&
	   c != '+')
	++strp;
    return strp;
}

/*
** Given a pointer into an extended time zone string, scan until the ending
** delimiter of the zone name is located. Return a pointer to the delimiter.
**
** As with getzname above, the legal character set is actually quite
** restricted, with other characters producing undefined results.
** We don't do any checking here; checking is done later in common-case code.
*/

static const char *
getqzname(const char *strp, const int delim)
{
    int	c;

    while ((c = *strp) != '\0' && c != delim)
	++strp;
    return strp;
}

/*
** Given a pointer into a time zone string, extract a number from that string.
** Check that the number is within a specified range; if it is not, return
** NULL.
** Otherwise, return a pointer to the first character not part of the number.
*/

static const char *
getnum(const char * strp, int * const nump, const int min, const int max)
{
    char c;
    int	num;

    if (strp == NULL || !is_digit(c = *strp))
	return NULL;
    num = 0;
    do {
	num = num * 10 + (c - '0');
	if (num > max)
	    return NULL;	/* illegal value */
	c = *++strp;
    } while (is_digit(c));
    if (num < min)
	return NULL;		/* illegal value */
    *nump = num;
    return strp;
}

/*
** Given a pointer into a time zone string, extract a number of seconds,
** in hh[:mm[:ss]] form, from the string.
** If any error occurs, return NULL.
** Otherwise, return a pointer to the first character not part of the number
** of seconds.
*/

static const char *
getsecs(const char *strp, int_fast32_t *const secsp)
{
    int	num;

    /*
    ** 'HOURSPERDAY * DAYSPERWEEK - 1' allows quasi-Posix rules like
    ** "M10.4.6/26", which does not conform to Posix,
    ** but which specifies the equivalent of
    ** "02:00 on the first Sunday on or after 23 Oct".
    */
    strp = getnum(strp, &num, 0, HOURSPERDAY * DAYSPERWEEK - 1);
    if (strp == NULL)
	return NULL;
    *secsp = num * (int_fast32_t) SECSPERHOUR;
    if (*strp == ':') {
	++strp;
	strp = getnum(strp, &num, 0, MINSPERHOUR - 1);
	if (strp == NULL)
	    return NULL;
	*secsp += num * SECSPERMIN;
	if (*strp == ':') {
	    ++strp;
	    /* 'SECSPERMIN' allows for leap seconds.  */
	    strp = getnum(strp, &num, 0, SECSPERMIN);
	    if (strp == NULL)
		return NULL;
	    *secsp += num;
	}
    }
    return strp;
}

/*
** Given a pointer into a time zone string, extract an offset, in
** [+-]hh[:mm[:ss]] form, from the string.
** If any error occurs, return NULL.
** Otherwise, return a pointer to the first character not part of the time.
*/

static const char *
getoffset(const char *strp, int_fast32_t *const offsetp)
{
    int	neg = 0;

    if (*strp == '-') {
	neg = 1;
	++strp;
    } else if (*strp == '+')
	++strp;
    strp = getsecs(strp, offsetp);
    if (strp == NULL)
	return NULL;		/* illegal time */
    if (neg)
	*offsetp = -*offsetp;
    return strp;
}

/*
** Given a pointer into a time zone string, extract a rule in the form
** date[/time]. See POSIX section 8 for the format of "date" and "time".
** If a valid rule is not found, return NULL.
** Otherwise, return a pointer to the first character not part of the rule.
*/

static const char *
getrule(const char * strp, struct rule * const rulep)
{
    if (*strp == 'J') {
	/*
	** Julian day.
	*/
	rulep->r_type = JULIAN_DAY;
	++strp;
	strp = getnum(strp, &rulep->r_day, 1, DAYSPERNYEAR);
    } else if (*strp == 'M') {
	/*
	** Month, week, day.
	*/
	rulep->r_type = MONTH_NTH_DAY_OF_WEEK;
	++strp;
	strp = getnum(strp, &rulep->r_mon, 1, MONSPERYEAR);
	if (strp == NULL)
	    return NULL;
	if (*strp++ != '.')
	    return NULL;
	strp = getnum(strp, &rulep->r_week, 1, 5);
	if (strp == NULL)
	    return NULL;
	if (*strp++ != '.')
	    return NULL;
	strp = getnum(strp, &rulep->r_day, 0, DAYSPERWEEK - 1);
    } else if (is_digit(*strp)) {
	/*
	** Day of year.
	*/
	rulep->r_type = DAY_OF_YEAR;
	strp = getnum(strp, &rulep->r_day, 0, DAYSPERLYEAR - 1);
    } else	return NULL;		/* invalid format */
    if (strp == NULL)
	return NULL;
    if (*strp == '/') {
	/*
	** Time specified.
	*/
	++strp;
	strp = getoffset(strp, &rulep->r_time);
    } else	rulep->r_time = 2 * SECSPERHOUR;	/* default = 2:00:00 */
    return strp;
}

/*
** Given a year, a rule, and the offset from UT at the time that rule takes
** effect, calculate the year-relative time that rule takes effect.
*/

static int_fast32_t
transtime(const int year, const struct rule *const rulep,
	  const int_fast32_t offset)
{
    int	leapyear;
    int_fast32_t value;
    int	d, m1, yy0, yy1, yy2, dow;

    INITIALIZE(value);
    leapyear = isleap(year);
    switch (rulep->r_type) {

    case JULIAN_DAY:
	/*
	** Jn - Julian day, 1 == January 1, 60 == March 1 even in leap
	** years.
	** In non-leap years, or if the day number is 59 or less, just
	** add SECSPERDAY times the day number-1 to the time of
	** January 1, midnight, to get the day.
	*/
	value = (rulep->r_day - 1) * SECSPERDAY;
	if (leapyear && rulep->r_day >= 60)
	    value += SECSPERDAY;
	break;

    case DAY_OF_YEAR:
	/*
	** n - day of year.
	** Just add SECSPERDAY times the day number to the time of
	** January 1, midnight, to get the day.
	*/
	value = rulep->r_day * SECSPERDAY;
	break;

    case MONTH_NTH_DAY_OF_WEEK:
	/*
	** Mm.n.d - nth "dth day" of month m.
	*/

	/*
	** Use Zeller's Congruence to get day-of-week of first day of
	** month.
	*/
	m1 = (rulep->r_mon + 9) % 12 + 1;
	yy0 = (rulep->r_mon <= 2) ? (year - 1) : year;
	yy1 = yy0 / 100;
	yy2 = yy0 % 100;
	dow = ((26 * m1 - 2) / 10 +
	       1 + yy2 + yy2 / 4 + yy1 / 4 - 2 * yy1) % 7;
	if (dow < 0)
	    dow += DAYSPERWEEK;

	/*
	** "dow" is the day-of-week of the first day of the month. Get
	** the day-of-month (zero-origin) of the first "dow" day of the
	** month.
	*/
	d = rulep->r_day - dow;
	if (d < 0)
	    d += DAYSPERWEEK;
	for (int i = 1; i < rulep->r_week; ++i) {
	    if (d + DAYSPERWEEK >=
		mon_lengths[leapyear][rulep->r_mon - 1])
		break;
	    d += DAYSPERWEEK;
	}

	/*
	** "d" is the day-of-month (zero-origin) of the day we want.
	*/
	value = d * SECSPERDAY;
	for (int i = 0; i < rulep->r_mon - 1; ++i)
	    value += mon_lengths[leapyear][i] * SECSPERDAY;
	break;
    }

    /*
    ** "value" is the year-relative time of 00:00:00 UT on the day in
    ** question. To get the year-relative time of the specified local
    ** time on that day, add the transition time and the current offset
    ** from UT.
    */
    return value + rulep->r_time + offset;
}

/*
** Given a POSIX section 8-style TZ string, fill in the rule tables as
** appropriate.
*/

static int
tzparse(const char * name, struct state * const sp, const int lastditch)
{
    const char *			stdname;
    const char *			dstname;
    size_t				stdlen;
    size_t				dstlen;
    int_fast32_t			stdoffset;
    int_fast32_t			dstoffset;
    char *			cp;
    int			load_result;
    static struct ttinfo		zttinfo;

    INITIALIZE(dstname);
    stdname = name;
    if (lastditch) {
	stdlen = strlen(name);	/* length of standard zone name */
	name += stdlen;
	if (stdlen >= sizeof sp->chars)
	    stdlen = (sizeof sp->chars) - 1;
	stdoffset = 0;
    } else {
	if (*name == '<') {
	    name++;
	    stdname = name;
	    name = getqzname(name, '>');
	    if (*name != '>')
		return (-1);
	    stdlen = name - stdname;
	    name++;
	} else {
	    name = getzname(name);
	    stdlen = name - stdname;
	}
	if (*name == '\0')
	    return -1;
	name = getoffset(name, &stdoffset);
	if (name == NULL)
	    return -1;
    }
    load_result = tzload(TZDEFRULES, sp, FALSE);
    if (load_result != 0)
	sp->leapcnt = 0;		/* so, we're off a little */
    if (*name != '\0') {
	if (*name == '<') {
	    dstname = ++name;
	    name = getqzname(name, '>');
	    if (*name != '>')
		return -1;
	    dstlen = name - dstname;
	    name++;
	} else {
	    dstname = name;
	    name = getzname(name);
	    dstlen = name - dstname; /* length of DST zone name */
	}
	if (*name != '\0' && *name != ',' && *name != ';') {
	    name = getoffset(name, &dstoffset);
	    if (name == NULL)
		return -1;
	} else	dstoffset = stdoffset - SECSPERHOUR;
	if (*name == '\0' && load_result != 0)
	    name = TZDEFRULESTRING;
	if (*name == ',' || *name == ';') {
	    struct rule	start;
	    struct rule	end;
	    int	year;
	    int	yearlim;
	    int	timecnt;
	    time_t		janfirst;

	    ++name;
	    if ((name = getrule(name, &start)) == NULL)
		return -1;
	    if (*name++ != ',')
		return -1;
	    if ((name = getrule(name, &end)) == NULL)
		return -1;
	    if (*name != '\0')
		return -1;
	    sp->typecnt = 2;	/* standard time and DST */
	    /*
	    ** Two transitions per year, from EPOCH_YEAR forward.
	    */
	    sp->ttis[0] = sp->ttis[1] = zttinfo;
	    sp->ttis[0].tt_gmtoff = -dstoffset;
	    sp->ttis[0].tt_isdst = 1;
	    sp->ttis[0].tt_abbrind = (int)(stdlen + 1);
	    sp->ttis[1].tt_gmtoff = -stdoffset;
	    sp->ttis[1].tt_isdst = 0;
	    sp->ttis[1].tt_abbrind = 0;
	    timecnt = 0;
	    janfirst = 0;
	    yearlim = EPOCH_YEAR + YEARSPERREPEAT;
	    for (year = EPOCH_YEAR; year < yearlim; year++) {
		int_fast32_t
		    starttime = transtime(year, &start, stdoffset),
		    endtime = transtime(year, &end, dstoffset);
		int_fast32_t
		    yearsecs = (year_lengths[isleap(year)]
				* SECSPERDAY);
		int reversed = endtime < starttime;
		if (reversed) {
		    int_fast32_t swap = starttime;
		    starttime = endtime;
		    endtime = swap;
		}
		if (reversed
		    || (starttime < endtime
			&& (endtime - starttime
			    < (yearsecs
			       + (stdoffset - dstoffset))))) {
		    if (TZ_MAX_TIMES - 2 < timecnt)
			break;
		    yearlim = year + YEARSPERREPEAT + 1;
		    sp->ats[timecnt] = janfirst;
		    if (increment_overflow_time
			(&sp->ats[timecnt], starttime))
			break;
		    sp->types[timecnt++] = (unsigned char) reversed;
		    sp->ats[timecnt] = janfirst;
		    if (increment_overflow_time
			(&sp->ats[timecnt], endtime))
			break;
		    sp->types[timecnt++] = !reversed;
		}
		if (increment_overflow_time(&janfirst, yearsecs))
		    break;
	    }
	    sp->timecnt = timecnt;
	    if (!timecnt)
		sp->typecnt = 1;	/* Perpetual DST.  */
	} else {
	    int_fast32_t theirstdoffset, theirdstoffset, theiroffset;
	    int	 isdst;

	    if (*name != '\0')
		return -1;
	    /*
	    ** Initial values of theirstdoffset and theirdstoffset.
	    */
	    theirstdoffset = 0;
	    for (int i = 0; i < sp->timecnt; ++i) {
		int j = sp->types[i];
		if (!sp->ttis[j].tt_isdst) {
		    theirstdoffset =
			-sp->ttis[j].tt_gmtoff;
		    break;
		}
	    }
	    theirdstoffset = 0;
	    for (int i = 0; i < sp->timecnt; ++i) {
		int j = sp->types[i];
		if (sp->ttis[j].tt_isdst) {
		    theirdstoffset =
			-sp->ttis[j].tt_gmtoff;
		    break;
		}
	    }
	    /*
	    ** Initially we're assumed to be in standard time.
	    */
	    isdst = FALSE;
	    theiroffset = theirstdoffset;
	    /*
	    ** Now juggle transition times and types
	    ** tracking offsets as you do.
	    */
	    for (int i = 0; i < sp->timecnt; ++i) {
		int j = sp->types[i];
		sp->types[i] = (unsigned char)sp->ttis[j].tt_isdst;
		if (sp->ttis[j].tt_ttisgmt) {
		    /* No adjustment to transition time */
		} else {
		    /*
		    ** If summer time is in effect, and the
		    ** transition time was not specified as
		    ** standard time, add the summer time
		    ** offset to the transition time;
		    ** otherwise, add the standard time
		    ** offset to the transition time.
		    */
		    /*
		    ** Transitions from DST to DDST
		    ** will effectively disappear since
		    ** POSIX provides for only one DST
		    ** offset.
		    */
		    if (isdst && !sp->ttis[j].tt_ttisstd) {
			sp->ats[i] += dstoffset -
			    theirdstoffset;
		    } else {
			sp->ats[i] += stdoffset -
			    theirstdoffset;
		    }
		}
		theiroffset = -sp->ttis[j].tt_gmtoff;
		if (sp->ttis[j].tt_isdst)
		    theirdstoffset = theiroffset;
		else	theirstdoffset = theiroffset;
	    }
	    /*
	    ** Finally, fill in ttis.
	    */
	    sp->ttis[0] = sp->ttis[1] = zttinfo;
	    sp->ttis[0].tt_gmtoff = -stdoffset;
	    sp->ttis[0].tt_isdst = FALSE;
	    sp->ttis[0].tt_abbrind = 0;
	    sp->ttis[1].tt_gmtoff = -dstoffset;
	    sp->ttis[1].tt_isdst = TRUE;
	    sp->ttis[1].tt_abbrind = (int)(stdlen + 1);
	    sp->typecnt = 2;
	}
    } else {
	dstlen = 0;
	sp->typecnt = 1;		/* only standard time */
	sp->timecnt = 0;
	sp->ttis[0] = zttinfo;
	sp->ttis[0].tt_gmtoff = -stdoffset;
	sp->ttis[0].tt_isdst = 0;
	sp->ttis[0].tt_abbrind = 0;
    }
    sp->charcnt = (int)(stdlen + 1);
    if (dstlen != 0)
	sp->charcnt += dstlen + 1;
    if ((size_t) sp->charcnt > sizeof sp->chars)
	return -1;
    cp = sp->chars;
    (void) strncpy(cp, stdname, stdlen);
    cp += stdlen;
    *cp++ = '\0';
    if (dstlen != 0) {
	(void) strncpy(cp, dstname, dstlen);
	*(cp + dstlen) = '\0';
    }
    return 0;
}

static void
gmtload(struct state * const sp)
{
    if (tzload(gmt, sp, TRUE) != 0)
	(void) tzparse(gmt, sp, TRUE);
}

void
R_tzsetwall(void)
{
    if (lcl_is_set < 0) return;
    lcl_is_set = -1;

    if (tzload((char *) NULL, lclptr, TRUE) != 0) gmtload(lclptr);
    settzname();
}

void
tzset(void)
{
    const char * name;

    name = getenv("TZ");
    if (name == NULL) {
	R_tzsetwall();
	return;
    }

    if (lcl_is_set > 0 && strcmp(lcl_TZname, name) == 0)
	return;
    lcl_is_set = strlen(name) < sizeof lcl_TZname;
    if (lcl_is_set)
	(void) strcpy(lcl_TZname, name);

    if (*name == '\0') {
	/*
	** User wants it fast rather than right.
	*/
	lclptr->leapcnt = 0;		/* so, we're off a little */
	lclptr->timecnt = 0;
	lclptr->typecnt = 0;
	lclptr->ttis[0].tt_isdst = 0;
	lclptr->ttis[0].tt_gmtoff = 0;
	lclptr->ttis[0].tt_abbrind = 0;
	(void) strcpy(lclptr->chars, gmt);
    } else if (tzload(name, lclptr, TRUE) != 0)
	if (name[0] == ':' || tzparse(name, lclptr, FALSE) != 0)
	    (void) gmtload(lclptr);
    settzname();
}

/*
** The easy way to behave "as if no library function calls" localtime
** is to not call it--so we drop its guts into "localsub", which can be
** freely called. (And no, the PANS doesn't require the above behavior--
** but it *is* desirable.)
**
** The unused offset argument is for the benefit of mktime variants.
*/

/*ARGSUSED*/
static stm *
localsub(const time_t *const timep, const int_fast32_t offset, stm *const tmp)
{
    struct state * sp;
    const struct ttinfo * ttisp;
    int i;
    stm * result;
    const time_t t = *timep;

    sp = lclptr;
    if ((sp->goback && t < sp->ats[0]) ||
	(sp->goahead && t > sp->ats[sp->timecnt - 1])) {
	time_t			newt = t;
	time_t		seconds;
	time_t		years;

	if (t < sp->ats[0])
	    seconds = sp->ats[0] - t;
	else	seconds = t - sp->ats[sp->timecnt - 1];
	--seconds;
	years = (seconds / SECSPERREPEAT + 1) * YEARSPERREPEAT;
	seconds = years * AVGSECSPERYEAR;
	if (t < sp->ats[0])
	    newt += seconds;
	else	newt -= seconds;
	if (newt < sp->ats[0] ||
	    newt > sp->ats[sp->timecnt - 1])
	    return NULL;	/* "cannot happen" */
	result = localsub(&newt, offset, tmp);
	if (result == tmp) {
	    time_t	newy;

	    newy = tmp->tm_year;
	    if (t < sp->ats[0])
		newy -= years;
	    else	newy += years;
	    tmp->tm_year = (int)newy;
	    if (tmp->tm_year != newy)
		return NULL;
	}
	return result;
    }
    if (sp->timecnt == 0 || t < sp->ats[0]) {
	i = sp->defaulttype;
    } else {
	int lo = 1;
	int hi = sp->timecnt;

	while (lo < hi) {
	    int	mid = (lo + hi) >> 1;

	    if (t < sp->ats[mid])
		hi = mid;
	    else	lo = mid + 1;
	}
	i = (int) sp->types[lo - 1];
    }
    ttisp = &sp->ttis[i];
    /*
    ** To get (wrong) behavior that's compatible with System V Release 2.0
    ** you'd replace the statement below with
    **	t += ttisp->tt_gmtoff;
    **	timesub(&t, 0L, sp, tmp);
    */
    result = timesub(&t, ttisp->tt_gmtoff, sp, tmp);
    tmp->tm_isdst = ttisp->tt_isdst;
    tzname[tmp->tm_isdst] = &sp->chars[ttisp->tt_abbrind];
//#ifdef HAVE_TM_ZONE
    tmp->tm_zone = &sp->chars[ttisp->tt_abbrind];
//#endif
    return result;
}

stm * localtime(const time_t * const timep)
{
    tzset();
    return localsub(timep, 0L, &tm);
}

/*
** Re-entrant version of localtime.
*/

stm *
localtime_r(const time_t *const timep, stm *tmp)
{
	return localsub(timep, 0L, tmp);
}

/*
** gmtsub is to gmtime as localsub is to localtime.
*/

static stm *
gmtsub(const time_t *const timep, const int_fast32_t offset, stm *const tmp)
{
    stm * result;

    if (!gmt_is_set) {
	gmt_is_set = TRUE;
	gmtload(gmtptr);
    }
    result = timesub(timep, offset, gmtptr, tmp);
    return result;
}

stm * gmtime(const time_t * const timep)
{
    return gmtsub(timep, 0L, &tm);
}

/*
* Re-entrant version of gmtime.
*/

stm *
gmtime_r(const time_t *const timep, stm *tmp)
{
    return gmtsub(timep, 0L, tmp);
}

/*
** Return the number of leap years through the end of the given year
** where, to make the math easy, the answer for year zero is defined as zero.
*/

static int leaps_thru_end_of(const int y)
{
	return (y >= 0) ? (y / 4 - y / 100 + y / 400) :
		-(leaps_thru_end_of(-(y + 1)) + 1);
}

static stm *
timesub(const time_t *const timep, const int_fast32_t offset,
	const struct state *const sp, stm *const tmp)
{
    const struct lsinfo *	lp;
    time_t			tdays;
    int			idays;	/* unsigned would be so 2003 */
    int_fast64_t		rem;
    int				y;
    const int *		ip;
    int_fast64_t		corr;
    int			hit;
    int			i;

    corr = 0;
    hit = 0;
    i = sp->leapcnt;
    while (--i >= 0) {
	lp = &sp->lsis[i];
	if (*timep >= lp->ls_trans) {
	    if (*timep == lp->ls_trans) {
		hit = ((i == 0 && lp->ls_corr > 0) ||
		       lp->ls_corr > sp->lsis[i - 1].ls_corr);
		if (hit)
		    while (i > 0 &&
			   sp->lsis[i].ls_trans ==
			   sp->lsis[i - 1].ls_trans + 1 &&
			   sp->lsis[i].ls_corr ==
			   sp->lsis[i - 1].ls_corr + 1) {
			++hit;
			--i;
		    }
	    }
	    corr = lp->ls_corr;
	    break;
	}
    }
    y = EPOCH_YEAR;
    tdays = *timep / SECSPERDAY;
    rem = *timep - tdays * SECSPERDAY;
    while (tdays < 0 || tdays >= year_lengths[isleap(y)]) {
	int  newy;
	time_t tdelta;
	int idelta;
	int leapdays;

	tdelta = tdays / DAYSPERLYEAR;
	if (! ((! TYPE_SIGNED(time_t) || INT_MIN <= tdelta)
	       && tdelta <= INT_MAX))
	    return NULL;
	idelta = (int)tdelta;
	if (idelta == 0)
	    idelta = (tdays < 0) ? -1 : 1;
	newy = y;
	if (increment_overflow(&newy, idelta))
	    return NULL;
	leapdays = leaps_thru_end_of(newy - 1) -
	    leaps_thru_end_of(y - 1);
	tdays -= ((time_t) newy - y) * DAYSPERNYEAR;
	tdays -= leapdays;
	y = newy;
    }
    {
	int_fast32_t	seconds;

	seconds = (int_fast32_t)(tdays * SECSPERDAY);
	tdays = seconds / SECSPERDAY;
	rem += seconds - tdays * SECSPERDAY;
    }
    /*
    ** Given the range, we can now fearlessly cast...
    */
    idays = (int)tdays;
    rem += offset - corr;
    while (rem < 0) {
	rem += SECSPERDAY;
	--idays;
    }
    while (rem >= SECSPERDAY) {
	rem -= SECSPERDAY;
	++idays;
    }
    while (idays < 0) {
	if (increment_overflow(&y, -1))
	    return NULL;
	idays += year_lengths[isleap(y)];
    }
    while (idays >= year_lengths[isleap(y)]) {
	idays -= year_lengths[isleap(y)];
	if (increment_overflow(&y, 1))
	    return NULL;
    }
    tmp->tm_year = y;
    if (increment_overflow(&tmp->tm_year, -TM_YEAR_BASE))
	return NULL;
    tmp->tm_yday = idays;
    /*
    ** The "extra" mods below avoid overflow problems.
    */
    tmp->tm_wday = EPOCH_WDAY +
	((y - EPOCH_YEAR) % DAYSPERWEEK) *
	(DAYSPERNYEAR % DAYSPERWEEK) +
	leaps_thru_end_of(y - 1) -
	leaps_thru_end_of(EPOCH_YEAR - 1) +
	idays;
    tmp->tm_wday %= DAYSPERWEEK;
    if (tmp->tm_wday < 0)
	tmp->tm_wday += DAYSPERWEEK;
    tmp->tm_hour = (int) (rem / SECSPERHOUR);
    rem %= SECSPERHOUR;
    tmp->tm_min = (int) (rem / SECSPERMIN);
    /*
    ** A positive leap second requires a special
    ** representation. This uses "... ??:59:60" et seq.
    */
    tmp->tm_sec = (int) (rem % SECSPERMIN) + hit;
    ip = mon_lengths[isleap(y)];
    for (tmp->tm_mon = 0; idays >= ip[tmp->tm_mon]; ++(tmp->tm_mon))
	idays -= ip[tmp->tm_mon];
    tmp->tm_mday = (int) (idays + 1);
    tmp->tm_isdst = 0;
//#ifdef HAVE_TM_GMTOFF
    tmp->tm_gmtoff = offset;
//#endif
    return tmp;
}

#ifdef UNUSED
char *
ctime(const time_t *const timep)
{
/*
** Section 4.12.3.2 of X3.159-1989 requires that
**	The ctime function converts the calendar time pointed to by timer
**	to local time in the form of a string. It is equivalent to
**		asctime(localtime(timer))
*/
    return asctime(localtime(timep));
}

char *
ctime_r(const time_t *const timep, char *buf)
{
    stm	mytm;

    return asctime_r(localtime_r(timep, &mytm), buf);
}
#endif

/*
** Adapted from code provided by Robert Elz, who writes:
**	The "best" way to do mktime I think is based on an idea of Bob
**	Kridle's (so its said...) from a long time ago.
**	It does a binary search of the time_t space. Since time_t's are
**	just 32 bits, its a max of 32 iterations (even at 64 bits it
**	would still be very reasonable).
*/

#ifndef WRONG
#define WRONG	(-1)
#endif /* !defined WRONG */

/*
** Normalize logic courtesy Paul Eggert.
*/

static int
increment_overflow(int *const ip, int j)
{
    int const	i = *ip;

    /*
    ** If i >= 0 there can only be overflow if i + j > INT_MAX
    ** or if j > INT_MAX - i; given i >= 0, INT_MAX - i cannot overflow.
    ** If i < 0 there can only be overflow if i + j < INT_MIN
    ** or if j < INT_MIN - i; given i < 0, INT_MIN - i cannot overflow.
    */
    if ((i >= 0) ? (j > INT_MAX - i) : (j < INT_MIN - i))
	return TRUE;
    *ip += j;
    return FALSE;
}

static int
increment_overflow32(int_fast32_t *const lp, int const m)
{
    int_fast32_t const	l = *lp;

    if ((l >= 0) ? (m > INT_FAST32_MAX - l) : (m < INT_FAST32_MIN - l))
	return TRUE;
    *lp += m;
    return FALSE;
}

static int
increment_overflow_time(time_t *tp, int_fast32_t j)
{
    /*
    ** This is like
    ** 'if (! (time_t_min <= *tp + j && *tp + j <= time_t_max)) ...',
    ** except that it does the right thing even if *tp + j would overflow.
    */
    if (! (j < 0
	   ? (TYPE_SIGNED(time_t) ? time_t_min - j <= *tp : -1 - j < *tp)
	   : *tp <= time_t_max - j))
	return TRUE;
    *tp += j;
    return FALSE;
}

static int
normalize_overflow(int * const tensptr, int * const unitsptr, const int base)
{
    int	tensdelta;

    tensdelta = (*unitsptr >= 0) ?
	(*unitsptr / base) :
	(-1 - (-1 - *unitsptr) / base);
    *unitsptr -= tensdelta * base;
    return increment_overflow(tensptr, tensdelta);
}

static int
normalize_overflow32(int_fast32_t *const tensptr, int *const unitsptr,
		     const int base)
{
    int	tensdelta;

    tensdelta = (*unitsptr >= 0) ?
	(*unitsptr / base) :
	(-1 - (-1 - *unitsptr) / base);
    *unitsptr -= tensdelta * base;
    return increment_overflow32(tensptr, tensdelta);
}

static int
tmcomp(const stm * const atmp, const stm * const btmp)
{
    int	result;

    if (atmp->tm_year != btmp->tm_year)
	return atmp->tm_year < btmp->tm_year ? -1 : 1;
    if ((result = (atmp->tm_mon - btmp->tm_mon)) == 0 &&
	(result = (atmp->tm_mday - btmp->tm_mday)) == 0 &&
	(result = (atmp->tm_hour - btmp->tm_hour)) == 0 &&
	(result = (atmp->tm_min - btmp->tm_min)) == 0)
	result = atmp->tm_sec - btmp->tm_sec;
    return result;
}

static time_t
time2sub(stm *const tmp,
	 stm *(*const funcp)(const time_t *, int_fast32_t, stm *),
	 const int_fast32_t offset,
	 int *const okayp,
	 const int do_norm_secs)
{
    const struct state * sp;
    int	dir;
    int	i;
    int	 saved_seconds;
    int_fast32_t li;
    time_t lo, hi;
    int_fast32_t y;
    time_t newt, t;
    stm	yourtm = *tmp, mytm;

    *okayp = FALSE;
    if (do_norm_secs) {
	if (normalize_overflow(&yourtm.tm_min, &yourtm.tm_sec, SECSPERMIN)) {
	    errno = EOVERFLOW;
	    return WRONG;
	}
    }
    if (normalize_overflow(&yourtm.tm_hour, &yourtm.tm_min, MINSPERHOUR)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    if (normalize_overflow(&yourtm.tm_mday, &yourtm.tm_hour, HOURSPERDAY)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    y = yourtm.tm_year;
    if (normalize_overflow32(&y, &yourtm.tm_mon, MONSPERYEAR)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    /*
    ** Turn y into an actual year number for now.
    ** It is converted back to an offset from TM_YEAR_BASE later.
    */
    if (increment_overflow32(&y, TM_YEAR_BASE)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    while (yourtm.tm_mday <= 0) {
	if (increment_overflow32(&y, -1)) {
	    errno = EOVERFLOW;
	    return WRONG;
	}
	li = y + (1 < yourtm.tm_mon);
	yourtm.tm_mday += year_lengths[isleap(li)];
    }
    while (yourtm.tm_mday > DAYSPERLYEAR) {
	li = y + (1 < yourtm.tm_mon);
	yourtm.tm_mday -= year_lengths[isleap(li)];
	if (increment_overflow32(&y, 1)) {
	    errno = EOVERFLOW;
	    return WRONG;
	}
    }
    for ( ; ; ) {
	i = mon_lengths[isleap(y)][yourtm.tm_mon];
	if (yourtm.tm_mday <= i)
	    break;
	yourtm.tm_mday -= i;
	if (++yourtm.tm_mon >= MONSPERYEAR) {
	    yourtm.tm_mon = 0;
	    if (increment_overflow32(&y, 1)) {
		errno = EOVERFLOW;
		return WRONG;
	    }
	}
    }
    if (increment_overflow32(&y, -TM_YEAR_BASE)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    yourtm.tm_year = y;
    if (yourtm.tm_year != y) {
	errno = EOVERFLOW;
	return WRONG;
    }
    if (yourtm.tm_sec >= 0 && yourtm.tm_sec < SECSPERMIN)
	saved_seconds = 0;
    else if (y + TM_YEAR_BASE < EPOCH_YEAR) {
	/*
	** We can't set tm_sec to 0, because that might push the
	** time below the minimum representable time.
	** Set tm_sec to 59 instead.
	** This assumes that the minimum representable time is
	** not in the same minute that a leap second was deleted from,
	** which is a safer assumption than using 58 would be.
	*/
	if (increment_overflow(&yourtm.tm_sec, 1 - SECSPERMIN)) {
	    errno = EOVERFLOW;
	    return WRONG;
	}
	saved_seconds = yourtm.tm_sec;
	yourtm.tm_sec = SECSPERMIN - 1;
    } else {
	saved_seconds = yourtm.tm_sec;
	yourtm.tm_sec = 0;
    }
    /*
    ** Do a binary search (this works whatever time_t's type is).
    */
    if (!TYPE_SIGNED(time_t)) {
	lo = 0;
	hi = lo - 1;
    } else {
	lo = 1;
	for (int i = 0; i < (int) TYPE_BIT(time_t) - 1; ++i)
	    lo *= 2;
	hi = -(lo + 1);
    }
    for ( ; ; ) {
	t = lo / 2 + hi / 2;
	if (t < lo)
	    t = lo;
	else if (t > hi)
	    t = hi;
	if ((*funcp)(&t, offset, &mytm) == NULL) {
	    /*
	    ** Assume that t is too extreme to be represented in
	    ** a struct tm; arrange things so that it is less
	    ** extreme on the next pass.
	    */
	    dir = (t > 0) ? 1 : -1;
	} else	dir = tmcomp(&mytm, &yourtm);
	if (dir != 0) {
	    if (t == lo) {
		if (t == time_t_max) {
		    errno = EOVERFLOW;
		    return WRONG;
		}
		++t;
		++lo;
	    } else if (t == hi) {
		if (t == time_t_min) {
		    errno = EOVERFLOW;
		    return WRONG;
		}
		--t;
		--hi;
	    }
	    if (lo > hi) {
		errno = EOVERFLOW;
		return WRONG;
	    }
	    if (dir > 0)
		hi = t;
	    else lo = t;
	    continue;
	}
	if (yourtm.tm_isdst < 0 || mytm.tm_isdst == yourtm.tm_isdst)
	    break;
	/*
	** Right time, wrong type.
	** Hunt for right time, right type.
	** It's okay to guess wrong since the guess
	** gets checked.
	*/
	sp = (const struct state *)
	    ((funcp == localsub) ? lclptr : gmtptr);
	for (int i = sp->typecnt - 1; i >= 0; --i) {
	    if (sp->ttis[i].tt_isdst != yourtm.tm_isdst)
		continue;
	    for (int j = sp->typecnt - 1; j >= 0; --j) {
		if (sp->ttis[j].tt_isdst == yourtm.tm_isdst)
		    continue;
		newt = t + sp->ttis[j].tt_gmtoff -
		    sp->ttis[i].tt_gmtoff;
		if ((*funcp)(&newt, offset, &mytm) == NULL)
		    continue;
		if (tmcomp(&mytm, &yourtm) != 0)
		    continue;
		if (mytm.tm_isdst != yourtm.tm_isdst)
		    continue;
		/*
		** We have a match.
		*/
		t = newt;
		goto label;
	    }
	}
	errno = EOVERFLOW;
	return WRONG;
    }
label:
    newt = t + saved_seconds;
    if ((newt < t) != (saved_seconds < 0)) {
	errno = EOVERFLOW;
	return WRONG;
    }
    t = newt;
    if ((*funcp)(&t, offset, tmp))
	*okayp = TRUE;
    return t;
}

static time_t
time2(stm * const tmp,
      stm * (*const funcp)(const time_t *, int_fast32_t, stm *),
      const int_fast32_t offset,
      int *const okayp)
{
    time_t t;

    /*
    ** First try without normalization of seconds
    ** (in case tm_sec contains a value associated with a leap second).
    ** If that fails, try with normalization of seconds.
    */
    t = time2sub(tmp, funcp, offset, okayp, FALSE);
    return *okayp ? t : time2sub(tmp, funcp, offset, okayp, TRUE);
}

static time_t
time1(stm *const tmp,
      stm *(*const funcp) (const time_t *, int_fast32_t, stm *),
      const int_fast32_t offset)
{
    time_t t;
    const struct state *sp;
    int	seen[TZ_MAX_TYPES];
    int types[TZ_MAX_TYPES];
    int okay;

    if (tmp == NULL) {
	errno = EINVAL;
	return WRONG;
    }
    if (tmp->tm_isdst > 1)
	tmp->tm_isdst = 1;
    t = time2(tmp, funcp, offset, &okay);
    if (okay || tmp->tm_isdst < 0)
	return t;

    /* R change.  This appears to be required by POSIX (it says
       the setting is used 'initially') and is documented for
       Solaris.
	
       Try unknown DST setting, if it was set.
    */
    if (tmp->tm_isdst >= 0) {
	tmp->tm_isdst = -1;
	errno = 0; // previous attempt will have set it
	t = time2(tmp, funcp, offset, &okay);
	if (okay) return t;
    }
	
    /*
    ** We're supposed to assume that somebody took a time of one type
    ** and did some math on it that yielded a "struct tm" that's bad.
    ** We try to divine the type they started from and adjust to the
    ** type they need.
    */
    sp = (const struct state *) ((funcp == localsub) ?  lclptr : gmtptr);
    for (int i = 0; i < sp->typecnt; ++i)
	seen[i] = FALSE;
    int nseen = 0;
    for (int i = sp->timecnt - 1; i >= 0; --i)
	if (!seen[sp->types[i]]) {
	    seen[sp->types[i]] = TRUE;
	    types[nseen++] = sp->types[i];
	}
    for (int sameind = 0; sameind < nseen; ++sameind) {
	int samei = types[sameind];
	if (sp->ttis[samei].tt_isdst != tmp->tm_isdst)
	    continue;
	for (int otherind = 0; otherind < nseen; ++otherind) {
	    int otheri = types[otherind];
	    if (sp->ttis[otheri].tt_isdst == tmp->tm_isdst)
		continue;
	    tmp->tm_sec += sp->ttis[otheri].tt_gmtoff -
		sp->ttis[samei].tt_gmtoff;
	    tmp->tm_isdst = !tmp->tm_isdst;
	    t = time2(tmp, funcp, offset, &okay);
	    if (okay)
		return t;
	    tmp->tm_sec -= sp->ttis[otheri].tt_gmtoff -
		sp->ttis[samei].tt_gmtoff;
	    tmp->tm_isdst = !tmp->tm_isdst;
	}
    }
    errno = EOVERFLOW;
    return WRONG;
}

time_t mktime(stm * const tmp)
{
    tzset();
    return time1(tmp, localsub, 0L);
}

time_t
R_timegm(stm *tmp)
{
    if (tmp != NULL)
	tmp->tm_isdst = 0;
    return time1(tmp, gmtsub, 0L);
}

#ifdef STD_INSPIRED

time_t
timelocal(stm *const tmp)
{
    if (tmp != NULL)
	tmp->tm_isdst = -1;	/* in case it wasn't initialized */
    return mktime(tmp);
}


time_t
timeoff(stm *const tmp, const long offset)
{
    if (tmp != NULL)
	tmp->tm_isdst = 0;
    return time1(tmp, gmtsub, offset);
}

#endif /* defined STD_INSPIRED */
