/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-2022   The R Core Team
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

/*  The original version of this file was contributed in 2005 by Ei-ji
 *  Nakama, along with rlocale_data.h and ../include/rlocale.h.
 *
 *  The naming is misleading: apart from the width data this is not
 *  locale-specfic.  It is rather about the use of non-Latin
 *  characters (including symbols, emojis ...).
 *
 *  It provides replacements for the wctype (iswxxxxx) functions on
 *  Windows (where they are not correct in e.g. Japanese)
 *  AIX (missing)
 *  macOS in CJK (where these just called the ctype functions)
 *
 *  It also provides wc[s]width, where widths of CJK fonts are often
 *  wrong in vendor-supplied versions and in Markus Kuhn's version
 *  used for Windows in R 2.[12].x.
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Visibility.h>
/*
#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_hidden
#endif
*/

#include <string.h>
#include <stdlib.h>

#define IN_RLOCALE_C 1 /* used in rlocale.h */
#include <rlocale.h>

#include <wctype.h>
#include <wchar.h>
#include <ctype.h>
#include <locale.h>
#include <limits.h>
#include <R_ext/Riconv.h>
#include <Defn.h> /* for localeCP */

#if defined(USE_RI18N_WIDTH) || defined(USE_RI18N_FNS)

/* used for zero-width table and in rlocale_data.h */
struct interval {
    int first;
    int last;
};

// This seems based on Markus Kuhn's function but with 1-based 'max'
static int wcsearch(int wint, const struct interval *table, int max)
{
    int min = 0;
    int mid;
    max--;

    if (wint < table[0].first || wint > table[max].last)
	return 0;
    while (max >= min) {
	mid = (min + max) / 2;
	if (wint > table[mid].last)
	    min = mid + 1;
	else if (wint < table[mid].first)
	    max = mid - 1;
	else
	    return 1;
    }
    return 0;
}
#endif

// ------------------------- width functions --------------------
#ifdef USE_RI18N_WIDTH

/* wcwidth and wcswidth, from POSIX 2001 (formerly in some draft C
 * standards but not implemented in Windows).  One could argue that
 * the width of non-printable / unassigned characters is immaterial
 * (they will be represented by escapes) so could be given a
 * conventional value such as 0 or 1.  POSIX suggests returning -1 for
 * non-printable characters, but these were not written that way in
 * 2005.
 *
 * It is not always clear what to do for unassigned code points
 * (especially 'private use' ones).
 *
 * Although what a character represents may be locale-specific,
 * reference images are available at
 * https://www.unicode.org/charts/PDF/ whose width can be assessed.
 *
 * There is a problem with character-by-character appoaches: apart
 * from surrogate pairs, some glyphs are defined in combinations of
 * others.  For human languages this is largely (but not entirely)
 * covered by giving combining characters zero width.  However, quite
 * a few emoji are defined as combinations of others: e.g.
 * 'polar bear' is bear+snowflake with a zero-width joiner,
 * "\U1f43b\u200d\u2744" which gets width 2+0+1.
 * (https://emojipedia.org/polar-bear/,
 * https://emojipedia.org/emoji-zwj-sequence/).  There are a few such
 * in human languages:
 * https://en.wikipedia.org/wiki/Zero-width_joiner.
 */

#include "rlocale_widths.h"

static int wcwidthsearch(int wint, const struct interval_wcwidth *table,
			 int max, int locale)
{
    int min = 0;
    int mid;
    max--;

    /* This quickly gives one for printing ASCII characters */
    if (wint > 0x1F && wint < 0x7F) return 1;
    else if (wint < table[min].first || wint > table[max].last) return -1;
    while (max >= min) {
	mid = (min + max) / 2;
	if (wint > table[mid].last)
	    min = mid + 1;
	else if (wint < table[mid].first)
	    max = mid - 1;
	else{
	    return(table[mid].mb[locale]);
	}
    }
    return -1;
}

/* The idea here here has never been explained.
   See also the comments in ../include/rlocale.h.

  That does not explain the separate entries for Singapore
   (simplified) and Hong Kong/Macau (traditional) where it seems the
   Windows system font is not different from PRC/Taiwan respectively,
   nor what font was used for non-Windows, nor where the values came
   from.

   Except perhaps on macOS, the non-Windows locale names are for the
   default MBCS encodings (e.g. GBK, GB1312, BIG5, EUCJP, EUCKR).
   There are other non-UTF-8 encodings for those locales,
   e.g. ja_JP.SJIS, ko_KR.CP949, zh_CN.eucCN, zh_HK.Big5HKSCS.
*/

typedef struct {
    char *name;
    int locale;
} cjk_locale_name_t;

static cjk_locale_name_t cjk_locale_name[] = {
    // Windows locale names
    {"CHINESE(SINGAPORE)_SIGNAPORE",		MB_zh_SG},
    {"CHINESE_SIGNAPORE",			MB_zh_SG},
    {"CHINESE(PRC)_PEOPLE'S REPUBLIC OF CHINA",	MB_zh_CN},
    {"CHINESE_PEOPLE'S REPUBLIC OF CHINA",	MB_zh_CN},
    {"CHINESE_MACAU S.A.R.",			MB_zh_HK},
    {"CHINESE(PRC)_HONG KONG",		        MB_zh_HK},
    {"CHINESE_HONG KONG S.A.R.",		MB_zh_HK},
    {"CHINESE(TAIWAN)_TAIWAN",			MB_zh_TW},
    {"CHINESE_TAIWAN",				MB_zh_TW},
    {"CHINESE-S",                               MB_zh_CN},
    {"CHINESE-T",                               MB_zh_TW},
    {"JAPANESE_JAPAN",				MB_ja_JP},
    {"JAPANESE",				MB_ja_JP},
    {"KOREAN_KOREA",				MB_ko_KR},
    {"KOREAN",				        MB_ko_KR},
    // Other OSes, but only in default encodings.
    {"ZH_TW",                                   MB_zh_TW},
    {"ZH_CN",                                   MB_zh_CN},
    {"ZH_CN.BIG5",                              MB_zh_TW},
    {"ZH_HK",                                   MB_zh_HK},
    {"ZH_SG",                                   MB_zh_SG},
    {"JA_JP",					MB_ja_JP},
    {"KO_KR",				        MB_ko_KR},
    {"ZH",				        MB_zh_CN},
    {"JA",					MB_ja_JP},
    {"KO",				        MB_ko_KR},
    // Default, where all EA Ambiguous characters have width one.
    {"",				        MB_Default},
};


static int get_locale_id(void)
{
    char lc_str[128];
    unsigned int i, j;

    strncpy(lc_str, setlocale(LC_CTYPE, NULL), sizeof(lc_str) - 1);
    lc_str[sizeof(lc_str) - 1] = '\0';
    for (i = 0, j = (int) strlen(lc_str); i < j && i < sizeof(lc_str); i++)
	lc_str[i] = (char) toupper(lc_str[i]);
    for (i = 0; i < (sizeof(cjk_locale_name)/sizeof(cjk_locale_name_t));
	 i++) {
	if (0 == strncmp(cjk_locale_name[i].name, lc_str,
			 strlen(cjk_locale_name[i].name)))
	    return cjk_locale_name[i].locale;
    }
    return 0;
}

/* used in character.c, ../gnuwin32/console.c (for an MBCS locale) ,
   ../library/grDevices/src/devP*.c 

   Unlike the POSIX description this does not return -1 for
   non-printable Unicode points.
   
   NB: Windows (at least MinGW-W64) does not have this function.
*/
int Ri18n_wcwidth(R_wchar_t c)
{

    /* This quickly gives one for printing ASCII characters */
    if (c > 0x1F && c < 0x7F) return 1;

    static int lc = 0;
#ifdef Win32
    static int localeCP_lc = -1;
    if (localeCP_lc != localeCP) {
	lc = get_locale_id();
	localeCP_lc = localeCP;
    }
#else
    static char encname_lc[R_CODESET_MAX + 1] = "";
    if (strcmp(encname_lc, R_nativeEncoding())) {
	lc = get_locale_id();
	strncpy(encname_lc, R_nativeEncoding(), R_CODESET_MAX);
	encname_lc[R_CODESET_MAX] = '\0';
    }
#endif

    int wd = wcwidthsearch(c, table_wcwidth,
			   (sizeof(table_wcwidth)/sizeof(struct interval_wcwidth)),
			   lc);
    if (wd >= 0) return wd; // currently all are 1 or 2.
    int zw = wcsearch(c, zero_width, zero_width_count);
    return zw ? 0 : 1; // assume unknown chars are width one.
}

/* Used in character.c, errors.c, ../gnuwin32/console.c (for an MBCS locale)
   
   Strings in R are restricted to 2^31-1 bytes but could conceivably
   have a width exceeding that.
   
   Unlike the POSIX description this does not return -1 for strings
   containing non-printable Unicode points.

   NB: Windows (at least MinGW-W64) does not have this function.
*/
attribute_hidden
int Ri18n_wcswidth (const wchar_t *wc, size_t n)
{
    int rs = 0;
    while ((n-- > 0) && (*wc != L'\0'))
    {
	if (IS_SURROGATE_PAIR(*wc, *(wc+1))) {
	    /* surrogate pairs should only occur with 'short' wchar_t,
	     * that is Windows and perhaps 32-bit AIX */
	    R_wchar_t val =
		((*wc & 0x3FF) << 10) + (*(wc+1) & 0x3FF) + 0x010000;
	    int now = Ri18n_wcwidth (val);
	    if (now == -1) return -1;
	    rs += now;
	    wc += 2;
	} else {
	    int now = Ri18n_wcwidth (*wc);
	    if (now == -1) return -1;
	    rs += now;
	    wc++;
	}
    }
    return rs;
}
#endif

// ------------------- end of width functions --------------------

/*********************************************************************
 *  macOS's wide character type functions are based on NetBSD
 *  and only work(ed) correctly for Latin-1 characters.
 *  So we replace them.  May also be needed on *BSD, and are on AIX
 ********************************************************************/


#ifdef USE_RI18N_FNS
# define ISWFUNC(ISWNAME) static int Ri18n_isw ## ISWNAME (wint_t wc) \
{									\
    return wcsearch(wc,table_w ## ISWNAME , table_w ## ISWNAME ## _count); \
}
#include "rlocale_data.h"
/* These are the functions which C99 and POSIX define.  However,
   not all are used elsewhere in R (so are static),
   but they are used in Ri18n_iswctype. */

    ISWFUNC(upper)
    ISWFUNC(lower)
    ISWFUNC(alpha)
    ISWFUNC(digit)
    ISWFUNC(xdigit)
    ISWFUNC(space)
    ISWFUNC(print)
    /* derived below from print and space
    ISWFUNC(graph)
    */
    ISWFUNC(blank)
    ISWFUNC(cntrl)
    ISWFUNC(punct)
    /*  defined below in terms of digit and alpha
    ISWFUNC(alnum)
    */

wctype_t Ri18n_wctype(const char *);
int Ri18n_iswctype(wint_t, wctype_t);

static int Ri18n_iswalnum (wint_t wc)
{
    return (Ri18n_iswctype(wc, Ri18n_wctype("digit")) ||
	    Ri18n_iswctype(wc, Ri18n_wctype("alpha"))    );
}

/* Defined in the C99 standard as

   'any wide character for which iswprint is true and iswspace is false'

   As this is used rarely (and iswprint is used a lot), we chose to
   derive this one.
*/

static int Ri18n_iswgraph (wint_t wc)
{
    return (Ri18n_iswctype(wc, Ri18n_wctype("print")) &&
	    !Ri18n_iswctype(wc, Ri18n_wctype("space"))    );
}


/*
 * iswctype
 */
typedef struct {
    char * name;
    wctype_t wctype;
    int(*func)(wint_t);
} Ri18n_wctype_func_l ;

static const Ri18n_wctype_func_l Ri18n_wctype_func[] = {
    {"upper",  1<<0,  Ri18n_iswupper},
    {"lower",  1<<1,  Ri18n_iswlower},
    {"alpha",  1<<2,  Ri18n_iswalpha},
    {"digit",  1<<3,  Ri18n_iswdigit},
    {"xdigit", 1<<4,  Ri18n_iswxdigit},
    {"space",  1<<5,  Ri18n_iswspace},
    {"print",  1<<6,  Ri18n_iswprint},
    {"graph",  1<<7,  Ri18n_iswgraph},
    {"blank",  1<<8,  Ri18n_iswblank},
    {"cntrl",  1<<9,  Ri18n_iswcntrl},
    {"punct",  1<<10, Ri18n_iswpunct},
    {"alnum",  1<<11, Ri18n_iswalnum},
    {NULL,     0,     NULL}
};

/* These two are used (via macros) in X11 dataentry so need to be visible. */
wctype_t Ri18n_wctype(const char *name)
{
    int i;

    for (i = 0 ; Ri18n_wctype_func[i].name != NULL &&
	     0 != strcmp(Ri18n_wctype_func[i].name, name) ; i++ );
    return Ri18n_wctype_func[i].wctype;
}

int Ri18n_iswctype(wint_t wc, wctype_t desc)
{
    int i;

    for (i = 0 ; Ri18n_wctype_func[i].wctype != 0 &&
	     Ri18n_wctype_func[i].wctype != desc ; i++ );
    return (*Ri18n_wctype_func[i].func)(wc);
}
#endif

// ------------------------- tolower/upper functions --------------------
#ifdef USE_RI18N_CASE
/* 
   These tables were prepared by the R code

tab <- read.table('UnicodeData.txt', sep = ';', header = FALSE)
tab <- tab[, c("V1", "V13", "V14")]
names(tab) <- c('pt', 'uc', 'lc')
toupper <- tab[tab$uc !="", 1:2]
tolower <- tab[tab$lc !="", c(1,3)]
cat(with(toupper, sprintf("  { 0x%s, 0x%s },", pt, uc)),
   sep = "\n", file = "rlocale_toupper.h")
cat(with(tolower, sprintf("  { 0x%s, 0x%s },", pt, lc)),
   sep = "\n", file = "rlocale_tolower.h")

   from https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt
*/

struct pair {int from; int to;};

static const struct pair table_toupper[] = {
#include "rlocale_toupper.h"
};

static const struct pair table_tolower[] = {
#include "rlocale_tolower.h"
};

static int tlsearch(int wint, const struct pair *table, int max)
{
    int min = 0, mid;
    max--;

    if (wint < table[0].from || wint > table[max].from)
	return -1;
    while (max >= min) {
	mid = (min + max) / 2;
	if (wint > table[mid].from)
	    min = mid + 1;
	else if (wint < table[mid].from)
	    max = mid - 1;
	else
	    return table[mid].to;
    }
    return -1;
}

R_wchar_t Ri18n_towupper(R_wchar_t wc)
{
    int res = tlsearch(wc, table_toupper,
		       sizeof(table_toupper)/sizeof(struct pair));
    return (res >= 0 ? res : wc);
}

R_wchar_t Ri18n_towlower(R_wchar_t wc)
{
    int res = tlsearch(wc, table_tolower,
		       sizeof(table_tolower)/sizeof(struct pair));
    return (res >= 0 ? res : wc);
}
// ----------------- end of tolower/upper functions --------------------
#endif
