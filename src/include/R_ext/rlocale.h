/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005   The R Development Core Team
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 59 Temple Place,
 *  Suite 330, Boston, MA  02111-1307  USA.
 */

#ifndef R_LOCALE_H
#define R_LOCALE_H

#include <wchar.h>
#include <ctype.h>
#include <wctype.h>

#ifdef  __cplusplus
extern "C" {
#endif

extern char *locale2charset(const char *);

#ifdef SUPPORT_MBCS
/*
 * Windows CJK
 * In Unicode, there is not a rule about character width. 
 * A letter of breadth is used in a CJK (Caina, Japan, Korea,
 * Taiwan, Hong Kong, and Singapore) area, and there are a
 * letter and a standard (character width is not still prescribed)
 * of a cord in a country.
 * Letter width is a problem of a font, but it is a rule route
 * besides a alphanumeric character that use a breadth letter.
 * It is generally defined as a breadth letter for a font such
 * as Japanese.
 * - Win32
 */
extern int Ri18n_wcwidth(wchar_t);
extern int Ri18n_wcswidth (const wchar_t *, size_t);
#undef wcwidth
#undef wcswidth
#define wcwidth(__wc)           Ri18n_wcwidth(__wc)
#define wcswidth(__wcs,__n)     Ri18n_wcswidth(__wcs,__n)

/* Mac OSX CJK and WindowXP(Japanese)
 * iswctypes of MacOSX calls isctypes. no i18n.
 * For example, isprint of Windows does not accept a macron of
 * Japanese "a-ru" of R as a letter. 
 * Therefore Japanese "Buraian.Ripuri-" of "Brian Ripley" is
 * shown of hex-string.:-)
 * defined(Win32) || defined(__APPLE_CC__) || defined(_AIX)
 */
extern wctype_t Ri18n_wctype(const char *);
extern int      Ri18n_iswctype(wint_t, wctype_t);
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

#endif /* SUPPORT_MBCS */
#ifdef  __cplusplus
}
#endif
#endif /* R_LOCALE_H */
