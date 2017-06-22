/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-2017   The R Core Team
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

/* Internal header, not installed */

/*  This file was contributed by Ei-ji Nakama.
 *  See also the comments in  ../main/rlocale.c.

 *  It does 2 things:
 * (a) supplies wrapper/substitute wc[s]width functions for use in 
 *    character.c, errors.c, printutils.c, devPS.c, RGui console.
 * (b) Defines a replacment for iswctype to be used on Windows, maxOS and AIX.
 * in gram.c 
 *
 * It is not an installed header.
 */

#ifndef R_LOCALE_H
#define R_LOCALE_H

#include <wchar.h>
#include <ctype.h>
#include <wctype.h>

/*
 * The Rwchar_t typedef represents a single Unicode code point.  On most systems it's the same
 * as wchar_t, but on Windows (and others?) where wchar_t is too small and UTF-16 is used,
 * it is an unsigned int instead.
 */
 
#ifdef Win32
typedef unsigned int Rwchar_t;
#else
typedef wchar_t Rwchar_t;
#endif 

/*
 * Windows CJK
 * In Unicode, there is not a rule about character width. 
 * A letter of breadth is used in a CJK (China, Japan, Korea,
 * Taiwan, Hong Kong, and Singapore) area, and there are a
 * letter and a standard (character width is not still prescribed)
 * of a cord in a country.
 * Letter width is a problem of a font, but it is a rule route
 * besides a alphanumeric character that use a breadth letter.
 * It is generally defined as a breadth letter for a font such
 * as Japanese.
 * - Win32

 *  Attempted explanation by BDR
 *  The display widths of characters are not prescribed in Unicode.
 *  Double-width characters are used in the CJK area: their width can
 *  be font-specific, with different fonts in use in different parts
 *  of the CJK area.  The tables supplied in many OSes and by Markus
 *  Kuhn are not do not take the exact locale into account.  The
 *  tables supplied in rlocale_data.h allow different widths for
 *  different parts of the CJK area, and also where needed different
 *  widths on Windows.  (The Windows differences are in zh_CN, and
 *  apply to European characters.)
 *
 * The differences are mainly (but not exclusively) in the
 * Unicode 'East Asian Ambiguous' class.
 *
 */
 
extern int Ri18n_wcwidth(Rwchar_t);
extern int Ri18n_wcswidth (const wchar_t *, size_t);

/* macOS CJK and WindowXP(Japanese)
 * iswctypes of macOS calls isctypes. no i18n.
 * For example, iswprint of Windows does not accept a macron of
 * Japanese "a-ru" of R as a letter. 
 * Therefore Japanese "Buraian.Ripuri-" of "Brian Ripley" is
 * shown of hex-string.:-)
 * We define alternatives to be used if
 * defined(Win32) || defined(__APPLE__) || defined(_AIX)
 */
extern wctype_t Ri18n_wctype(const char *);
extern int      Ri18n_iswctype(wint_t, wctype_t);

#ifndef IN_RLOCALE_C
/* We want to avoid these redefinitions in rlocale.c itself */
#undef iswupper
#undef iswlower
#undef iswalpha
#undef iswdigit
#undef iswxdigit
#undef iswspace
#undef iswprint
#undef iswgraph
#undef iswblank
#undef iswcntrl
#undef iswpunct
#undef iswalnum
#undef wctype
#undef iswctype

#define iswupper(__x)     Ri18n_iswctype(__x, Ri18n_wctype("upper"))
#define iswlower(__x)     Ri18n_iswctype(__x, Ri18n_wctype("lower"))
#define iswalpha(__x)     Ri18n_iswctype(__x, Ri18n_wctype("alpha"))
#define iswdigit(__x)     Ri18n_iswctype(__x, Ri18n_wctype("digit"))
#define iswxdigit(__x)    Ri18n_iswctype(__x, Ri18n_wctype("xdigit"))
#define iswspace(__x)     Ri18n_iswctype(__x, Ri18n_wctype("space"))
#define iswprint(__x)     Ri18n_iswctype(__x, Ri18n_wctype("print"))
#define iswgraph(__x)     Ri18n_iswctype(__x, Ri18n_wctype("graph"))
#define iswblank(__x)     Ri18n_iswctype(__x, Ri18n_wctype("blank"))
#define iswcntrl(__x)     Ri18n_iswctype(__x, Ri18n_wctype("cntrl"))
#define iswpunct(__x)     Ri18n_iswctype(__x, Ri18n_wctype("punct"))
#define iswalnum(__x)     Ri18n_iswctype(__x, Ri18n_wctype("alnum"))
#define wctype(__x)       Ri18n_wctype(__x)
#define iswctype(__x,__y) Ri18n_iswctype(__x,__y)
#endif

/* These definitions are from winnls.h in Mingw_w64.  We don't need the rest of that file. */

#define HIGH_SURROGATE_START 0xd800
#define HIGH_SURROGATE_END 0xdbff
#define LOW_SURROGATE_START 0xdc00
#define LOW_SURROGATE_END 0xdfff

/* The first two of these definitions use the argument twice which is bad, but we include them here in
 * the original form for consistency with Mingw_w64.  Users should be careful that evaluating
 * the argument doesn't result in side effects.
 */

#define IS_HIGH_SURROGATE(wch) (((wch) >= HIGH_SURROGATE_START) && ((wch) <= HIGH_SURROGATE_END))
#define IS_LOW_SURROGATE(wch) (((wch) >= LOW_SURROGATE_START) && ((wch) <= LOW_SURROGATE_END))
#define IS_SURROGATE_PAIR(hs, ls) (IS_HIGH_SURROGATE (hs) && IS_LOW_SURROGATE (ls))

# define utf8toucs32		Rf_utf8toucs32
Rwchar_t utf8toucs32(wchar_t high, const char *s);

#endif /* R_LOCALE_H */
