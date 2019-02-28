/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2019  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Pulic License as published by
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

/* The character functions in this file are

nzchar nchar substr substr<- abbreviate tolower toupper chartr strtrim

and the utility

make.names

The regex functions

strsplit grep [g]sub [g]regexpr agrep

here prior to 2.10.0 are now in grep.c and agrep.c

make.unique, duplicated, unique, match, pmatch, charmatch are in unique.c
iconv is in sysutils.c

Character strings in R are less than 2^31-1 bytes, so we use int not size_t.

Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

Comparison is done directly unless you happen to be comparing the same
string in different encodings.

nzchar and nchar(, "bytes") are independent of the encoding
nchar(, "char") nchar(, "width") handle UTF-8 directly, translate Latin-1
substr substr<-  handle UTF-8 and Latin-1 directly
tolower toupper chartr  translate UTF-8 to wchar, rest to current charset
  which needs Unicode wide characters
abbreviate strtrim  translate

All the string matching functions handle UTF-8 directly, otherwise
translate (latin1 to UTF-8, otherwise to native).

Support for "bytes" marked encoding
===================================

nzchar and nchar(, "bytes") are independent of the encoding.

nchar(, "char") nchar(, "width") give NA (if allowed) or error.
substr substr<-  work in bytes

abbreviate chartr make.names strtrim tolower toupper give error.

*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <errno.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <R_ext/Itermacros.h>
#include <rlocale.h>

/* We use a shared buffer here to avoid reallocing small buffers, and
   keep a standard-size (MAXELTSIZE = 8192) buffer allocated shared
   between the various functions.

   If we want to make this thread-safe, we would need to initialize an
   instance non-statically in each using function, but this would add
   to the overhead.
 */

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

/* primitive */
SEXP attribute_hidden do_nzchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    int nargs = length(args);

    // checkArity(op, args);  .Primitive()  &  may have 1 or 2 args now
    if (nargs < 1 || nargs > 2)
	errorcall(call,
		  ngettext("%d argument passed to '%s' which requires %d to %d",
			   "%d arguments passed to '%s' which requires %d to %d",
			   (unsigned long) nargs),
		  nargs, PRIMNAME(op), 1, 2);
    check1arg(args, call, "x");

    if (isFactor(CAR(args)))
	error(_("'%s' requires a character vector"), "nzchar()");
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	error(_("'%s' requires a character vector"), "nzchar()");

    int keepNA = FALSE; // the default
    if(nargs > 1) {
	keepNA = asLogical(CADR(args));
	if (keepNA == NA_LOGICAL) keepNA = FALSE;
    }
    R_xlen_t i, len = XLENGTH(x);
    PROTECT(ans = allocVector(LGLSXP, len));
    if (keepNA)
	for (i = 0; i < len; i++) {
	    SEXP sxi = STRING_ELT(x, i);
	    LOGICAL(ans)[i] = (sxi == NA_STRING) ? NA_LOGICAL : LENGTH(sxi) > 0;
	}
    else
	for (i = 0; i < len; i++)
	    LOGICAL(ans)[i] = LENGTH(STRING_ELT(x, i)) > 0;
    UNPROTECT(2);
    return ans;
}

/* R strings are limited to 2^31 - 1 bytes on all platforms */
int R_nchar(SEXP string, nchar_type type_,
	    Rboolean allowNA, Rboolean keepNA, const char* msg_name)
{
    if (string == NA_STRING)
	return keepNA ? NA_INTEGER : 2;
    // else :
    switch(type_) {
    case Bytes:
	return LENGTH(string);
	break;
    case Chars:
	if (IS_UTF8(string)) {
	    const char *p = CHAR(string);
	    if (!utf8Valid(p)) {
		if (!allowNA)
		    error(_("invalid multibyte string, %s"), msg_name);
		return NA_INTEGER;
	    } else {
		int nc = 0;
		for( ; *p; p += utf8clen(*p)) nc++;
		return nc;
	    }
	} else if (IS_BYTES(string)) {
	    if (!allowNA) /* could do chars 0 */
		error(_("number of characters is not computable in \"bytes\" encoding, %s"),
		      msg_name);
	    return NA_INTEGER;
	} else if (mbcslocale) {
	    int nc = (int) mbstowcs(NULL, translateChar(string), 0);
	    if (!allowNA && nc < 0)
		error(_("invalid multibyte string, %s"), msg_name);
	    return (nc >= 0 ? nc : NA_INTEGER);
	} else
	    return ((int) strlen(translateChar(string)));
	break;
    case Width:
	if (IS_UTF8(string)) {
	    const char *p = CHAR(string);
	    if (!utf8Valid(p)) {
		if (!allowNA)
		    error(_("invalid multibyte string, %s"), msg_name);
		return NA_INTEGER;
	    } else {
		wchar_t wc1;
		Rwchar_t ucs;
		int nc = 0;
		for( ; *p; p += utf8clen(*p)) {
		    utf8toucs(&wc1, p);
		    if (IS_HIGH_SURROGATE(wc1))
		    	ucs = utf8toucs32(wc1, p);
		    else
		    	ucs = wc1;
		    nc += Ri18n_wcwidth(ucs);
		}
		return nc;
	    }
	} else if (IS_BYTES(string)) {
	    if (!allowNA) /* could do width 0 */
		error(_("width is not computable for %s in \"bytes\" encoding"),
		      msg_name);
	    return NA_INTEGER;
	} else if (mbcslocale) {
	    const char *xi = translateChar(string);
	    int nc = (int) mbstowcs(NULL, xi, 0);
	    if (nc >= 0) {
		const void *vmax = vmaxget();
		wchar_t *wc = (wchar_t *)
		    R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
		mbstowcs(wc, xi, nc + 1);
		int nci18n = Ri18n_wcswidth(wc, 2147483647);
		vmaxset(vmax);
		return (nci18n < 1) ? nc : nci18n;
	    } else if (allowNA)
		error(_("invalid multibyte string, %s"), msg_name);
	    else
		return NA_INTEGER;
	} else
	    return (int) strlen(translateChar(string));

    } // switch
    return NA_INTEGER; // -Wall
} // R_nchar()

SEXP attribute_hidden do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x, stype, ans;
    int nargs = length(args);

#ifdef R_version_3_4_or_so
    checkArity(op, args);
#else
    // will work also for code byte-compiled *before* 'keepNA' was introduced
    if (nargs < 3 || nargs > 4)
	error(ngettext("%d argument passed to '%s' which requires %d to %d",
		       "%d arguments passed to '%s' which requires %d to %d",
			   (unsigned long) nargs),
	      nargs, PRIMNAME(op), 3, 4);
#endif
    if (DispatchOrEval(call, op, "nchar", args, env, &ans, 0, 1))
      return(ans);
    if (isFactor(CAR(args)))
	error(_("'%s' requires a character vector"), "nchar()");
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	error(_("'%s' requires a character vector"), "nchar()");
    R_xlen_t len = XLENGTH(x);
    stype = CADR(args);
    if (!isString(stype) || LENGTH(stype) != 1)
	error(_("invalid '%s' argument"), "type");
    const char *type = CHAR(STRING_ELT(stype, 0)); /* always ASCII */
    size_t ntype = strlen(type);
    if (ntype == 0) error(_("invalid '%s' argument"), "type");
    nchar_type type_;
    if (strncmp(type, "bytes", ntype) == 0)	 type_ = Bytes;
    else if (strncmp(type, "chars", ntype) == 0) type_ = Chars;
    else if (strncmp(type, "width", ntype) == 0) type_ = Width;
    else error(_("invalid '%s' argument"), "type");
    int allowNA = asLogical(CADDR(args));
    if (allowNA == NA_LOGICAL) allowNA = 0;
    int keepNA;
    if(nargs >= 4) {
	keepNA = asLogical(CADDDR(args));
	if (keepNA == NA_LOGICAL) // default
	    keepNA = (type_ == Width) ? FALSE : TRUE;
    } else  keepNA = (type_ == Width) ? FALSE : TRUE;
    PROTECT(s = allocVector(INTSXP, len));
    int *s_ = INTEGER(s);
    for (R_xlen_t i = 0; i < len; i++) {
	SEXP sxi = STRING_ELT(x, i);
	char msg_i[30]; sprintf(msg_i, "element %ld", (long)i+1);
	s_[i] = R_nchar(sxi, type_, allowNA, keepNA, msg_i);
    }
    R_FreeStringBufferL(&cbuff);
    if ((d = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	setAttrib(s, R_NamesSymbol, d);
    if ((d = getAttrib(x, R_DimSymbol)) != R_NilValue)
	setAttrib(s, R_DimSymbol, d);
    if ((d = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	setAttrib(s, R_DimNamesSymbol, d);
    UNPROTECT(2);
    return s;
}

/* Assumes sa < so; sa, so are 1-based indices in character units to str,
   len is length of str in bytes, excluding the terminator.

   Returns pointer to result string in rfrom, of length rlen (in bytes,
   excluding the terminator - the string is not terminated).

   *rfrom may be invalid pointer when rlen is zero.
*/
static void substr(const char *str, int len, int ienc, int sa, int so,
                   R_xlen_t idx, int isascii, const char **rfrom,
	           int *rlen, int assumevalid)
{
    int i;
    const char *end = str + len;

    if (ienc == CE_UTF8) {
	if (!assumevalid && !utf8Valid(str)) {
	    char msg[30];
	    sprintf(msg, "element %ld", (long)idx+1);
	    error(_("invalid multibyte string, %s"), msg);
	}
	for (i = 0; i < sa - 1 && str < end; i++)
	    str += utf8clen(*str);
	*rfrom = str;
	for(; i < so && str < end; i++)
	    str += utf8clen(*str);
	*rlen = str - *rfrom;
    } else if (mbcslocale && !isascii) {
	mbstate_t mb_st;
	mbs_init(&mb_st);
	for (i = 0; i < sa - 1 && str < end; i++)
	    /* throws error on invalid multi-byte string */
	    str += Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
	*rfrom = str;
	for (; i < so && str < end; i++)
	    /* throws error on invalid multi-byte string */
	    str += (int) Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
	*rlen = str - *rfrom;
    } else {
	if (so - 1 < len) {
	    *rfrom = str + sa - 1;
	    *rlen = so - sa + 1;
	} else if (sa - 1 < len) {
	    *rfrom = str + sa - 1;
	    *rlen = len - (sa - 1);
	} else {
	    *rfrom = NULL;
	    *rlen = 0;
	}
    }
}

SEXP attribute_hidden
do_substr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x;
    checkArity(op, args);
    x = CAR(args);
    if (!isString(x))
	error(_("extracting substrings from a non-character object"));
    R_xlen_t len = XLENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    SEXP lastel = NULL;
    if (len > 0) {
	SEXP sa = CADR(args),
	    so = CADDR(args);
	int
	    k = LENGTH(sa),
	    l = LENGTH(so);
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    error(_("invalid substring arguments"));

	for (R_xlen_t i = 0; i < len; i++) {
	    int start = INTEGER(sa)[i % k],
		stop  = INTEGER(so)[i % l];
	    SEXP el = STRING_ELT(x,i);
	    if (el == NA_STRING || start == NA_INTEGER || stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    cetype_t ienc = getCharCE(el);
	    const char *ss = CHAR(el);
	    size_t slen = LENGTH(el);
	    if (start < 1) start = 1;
	    if (start > stop) {
		SET_STRING_ELT(s, i, R_BlankString);
	    } else {
		const char *rfrom;
		int rlen;
		/* Skip checking UTF-8 validity if the string is the same
		   R object as previously. This improves performance of
		   substring() used on a single string but many substrings
		   to be extracted from it */
		substr(ss, slen, ienc, start, stop, i,
		       IS_ASCII(el), &rfrom, &rlen, el == lastel);
		SET_STRING_ELT(s, i, mkCharLenCE(rfrom, rlen, ienc));
	    }
	    lastel = el;
	}
    }
    SHALLOW_DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

// .Internal( startsWith(x, prefix) )  and
// .Internal( endsWith  (x, suffix) )
SEXP attribute_hidden
do_startsWith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    SEXP x = CAR(args), Xfix = CADR(args); // 'prefix' or 'suffix'
    if (!isString(x) || !isString(Xfix))
	error(_("non-character object(s)"));
    R_xlen_t
	n1 = XLENGTH(x),
	n2 = XLENGTH(Xfix),
	n = (n1 > 0 && n2 > 0) ? ((n1 >= n2) ? n1 : n2) : 0;
    if (n == 0) return allocVector(LGLSXP, 0);
    SEXP ans = PROTECT(allocVector(LGLSXP, n));

    typedef const char * cp;
    if (n2 == 1) { // optimize the most common case
	SEXP el = STRING_ELT(Xfix, 0);
	if (el == NA_STRING) {
	    for (R_xlen_t i = 0; i < n1; i++)
		LOGICAL(ans)[i] = NA_LOGICAL;
	} else {
	    // ASCII matching will do for ASCII Xfix except in non-UTF-8 MBCS
	    Rboolean need_translate = TRUE;
	    if (strIsASCII(CHAR(el)) && (utf8locale || !mbcslocale))
		need_translate = FALSE;
	    cp y0 = need_translate ? translateCharUTF8(el) : CHAR(el);
	    int ylen = (int) strlen(y0);
	    for (R_xlen_t i = 0; i < n1; i++) {
		SEXP el = STRING_ELT(x, i);
		if (el == NA_STRING) {
		    LOGICAL(ans)[i] = NA_LOGICAL;
		} else {
		    cp x0 = need_translate ? translateCharUTF8(el) : CHAR(el);
		    if(PRIMVAL(op) == 0) { // startsWith
			LOGICAL(ans)[i] = strncmp(x0, y0, ylen) == 0;
		    } else { // endsWith
			int off = (int)strlen(x0) - ylen;
			if (off < 0)
			    LOGICAL(ans)[i] = 0;
			else {
			    LOGICAL(ans)[i] = memcmp(x0 + off, y0, ylen) == 0;
			}
		    }
		}
	    }
	}
    } else { // n2 > 1
	// convert both inputs to UTF-8
	cp *x0 = (cp *) R_alloc(n1, sizeof(char *));
	cp *y0 = (cp *) R_alloc(n2, sizeof(char *));
	// and record lengths, -1 for NA
	int *x1 = (int *) R_alloc(n1, sizeof(int *));
	int *y1 = (int *) R_alloc(n2, sizeof(int *));
	for (R_xlen_t i = 0; i < n1; i++) {
	    SEXP el = STRING_ELT(x, i);
	    if (el == NA_STRING)
		x1[i] = -1;
	    else {
		x0[i] = translateCharUTF8(el);
		x1[i] = (int) strlen(x0[i]);
	    }
	}
	for (R_xlen_t i = 0; i < n2; i++) {
	    SEXP el = STRING_ELT(Xfix, i);
	    if (el == NA_STRING)
		y1[i] = -1;
	    else {
		y0[i] = translateCharUTF8(el);
		y1[i] = (int) strlen(y0[i]);
	    }
	}
	R_xlen_t i, i1, i2;
	if(PRIMVAL(op) == 0) { // 0 = startsWith, 1 = endsWith
	    MOD_ITERATE2(n, n1, n2, i, i1, i2, {
		    if (x1[i1] < 0 || y1[i2] < 0)
			LOGICAL(ans)[i] = NA_LOGICAL;
		    else if (x1[i1] < y1[i2])
			LOGICAL(ans)[i] = 0;
		    else // memcmp should be faster than strncmp
			LOGICAL(ans)[i] =
			    memcmp(x0[i1], y0[i2], y1[i2]) == 0;
		});
	} else { // endsWith
	    MOD_ITERATE2(n, n1, n2, i, i1, i2, {
		    if (x1[i1] < 0 || y1[i2] < 0)
			LOGICAL(ans)[i] = NA_LOGICAL;
		    else {
			int off = x1[i1] - y1[i2];
			if (off < 0)
			    LOGICAL(ans)[i] = 0;
			else {
			    LOGICAL(ans)[i] =
				memcmp(x0[i1] + off, y0[i2], y1[i2]) == 0;
			}
		    }
		});
	}
    }
    UNPROTECT(1);
    return ans;
}


static void
substrset(char *buf, const char *const str, cetype_t ienc, int sa, int so,
          R_xlen_t xidx, R_xlen_t vidx)
{
    /* Replace the substring buf[sa:so] by str[] */
    int i, in = 0, out = 0;

    if (ienc == CE_UTF8) {
	if (!utf8Valid(buf)) {
	    char msg[30];
	    sprintf(msg, "element %ld", (long)xidx+1);
	    error(_("invalid multibyte string, %s"), msg);
	}
	if (!utf8Valid(str)) {
	    char msg[30];
	    sprintf(msg, "value element %ld", (long)vidx+1);
	    error(_("invalid multibyte string, %s"), msg);
	}
	for (i = 1; i < sa; i++) buf += utf8clen(*buf);
	for (i = sa; i <= so && in < strlen(str); i++) {
	    in +=  utf8clen(str[in]);
	    out += utf8clen(buf[out]);
	    if (!str[in]) break;
	}
	if (in != out) memmove(buf+in, buf+out, strlen(buf+out)+1);
	memcpy(buf, str, in);
    } else if (ienc == CE_LATIN1 || ienc == CE_BYTES) {
	in = (int) strlen(str);
	out = so - sa + 1;
	memcpy(buf + sa - 1, str, (in < out) ? in : out);
    } else {
	/* This cannot work for stateful encodings */
	if (mbcslocale) {
	    for (i = 1; i < sa; i++) buf += Mbrtowc(NULL, buf, MB_CUR_MAX, NULL);
	    /* now work out how many bytes to replace by how many */
	    for (i = sa; i <= so && in < strlen(str); i++) {
		in += (int) Mbrtowc(NULL, str+in, MB_CUR_MAX, NULL);
		out += (int) Mbrtowc(NULL, buf+out, MB_CUR_MAX, NULL);
		if (!str[in]) break;
	    }
	    if (in != out) memmove(buf+in, buf+out, strlen(buf+out)+1);
	    memcpy(buf, str, in);
	} else {
	    in = (int) strlen(str);
	    out = so - sa + 1;
	    memcpy(buf + sa - 1, str, (in < out) ? in : out);
	}
    }
}

SEXP attribute_hidden do_substrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so, value, el, v_el;
    R_xlen_t i, len;
    int start, stop, k, l, v;
    size_t slen;
    cetype_t ienc, venc;
    const char *ss, *v_ss;
    char *buf;
    const void *vmax;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    value = CADDDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if (!isString(x))
	error(_("replacing substrings in a non-character object"));
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if (len > 0) {
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    error(_("invalid substring arguments"));

	v = LENGTH(value);
	if (!isString(value) || v == 0) error(_("invalid value"));

	vmax = vmaxget();
	for (i = 0; i < len; i++) {
	    el = STRING_ELT(x, i);
	    v_el = STRING_ELT(value, i % v);
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    if (el == NA_STRING || v_el == NA_STRING ||
		start == NA_INTEGER || stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    ienc = getCharCE(el);
	    ss = CHAR(el);
	    slen = strlen(ss);
	    if (start < 1) start = 1;
	    if (stop > slen) stop = (int) slen; /* SBCS optimization */
	    if (start > stop) {
		/* just copy element across */
		SET_STRING_ELT(s, i, STRING_ELT(x, i));
	    } else {
		int ienc2 = ienc;
		v_ss = CHAR(v_el);
		/* is the value in the same encoding?
		   FIXME: could prefer UTF-8 here
		 */
		venc = getCharCE(v_el);
		if (venc != ienc && !strIsASCII(v_ss)) {
		    ss = translateChar(el);
		    slen = strlen(ss);
		    v_ss = translateChar(v_el);
		    ienc2 = CE_NATIVE;
		}
		/* might expand under MBCS */
		buf = R_AllocStringBuffer(slen+strlen(v_ss), &cbuff);
		strcpy(buf, ss);
		substrset(buf, v_ss, ienc2, start, stop, i, i % v);
		SET_STRING_ELT(s, i, mkCharCE(buf, ienc2));
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    }
    UNPROTECT(1);
    return s;
}

/* Abbreviate
   long names in the S-designated fashion:
   1) spaces
   2) lower case vowels
   3) lower case consonants
   4) upper case letters
   5) special characters.

   Letters are dropped from the end of words
   and at least one letter is retained from each word.

   If unique abbreviations are not produced letters are added until the
   results are unique (duplicated names are removed prior to entry).
   names, minlength, use.classes, dot
*/


#define FIRSTCHAR(i) (isspace((int)s[i-1]))
#define LASTCHAR(i) (!isspace((int)s[i-1]) && (!s[i+1] || isspace((int)s[i+1])))
#define LC_VOWEL(i) (s[i] == 'a' || s[i] == 'e' || s[i] == 'i' || \
		   s[i] == 'o' || s[i] == 'u')
#define UPPER (int)(strlen(s) - 1)

/* memmove does allow overlapping src and dest */
static void mystrcpy(char *dest, const char *src)
{
    memmove(dest, src, strlen(src)+1);
}

static SEXP stripchars(const char * const inchar, int minlen, int usecl)
{
    int i, j, nspace = 0;
    char *s = cbuff.data;

    /* The R wrapper removed leading and trailing spces */
    mystrcpy(s, inchar);
    if (strlen(s) < minlen) goto donesc;

    /* The for() loops never touch the first character */

    /*  record spaces for removal later (as they act as word boundaries) */
    for (i = UPPER, j = 1; i > 0; i--) {
	if (isspace((int)s[i])) {
	    if (j) s[i] = '\0'; // trailing space
	    else nspace++;
	} else j = 0;
	if (strlen(s) - nspace <= minlen)
	    goto donesc;
    }

    if(usecl) {
	/* remove l/case vowels,
	   which are not at the beginning of a word but are at the end */
	for (i = UPPER; i > 0; i--) {
	    if (LC_VOWEL(i) && LASTCHAR(i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	/* remove those not at the beginning of a word */
	for (i = UPPER; i > 0; i--) {
	    if (LC_VOWEL(i) && !FIRSTCHAR(i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	/* Now do the same for remaining l/case chars */
	for (i = UPPER; i > 0; i--) {
	    if (islower((int)s[i]) && LASTCHAR(i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}

	for (i = UPPER; i > 0; i--) {
	    if (islower((int)s[i]) && !FIRSTCHAR(i))
		mystrcpy(s + i, s + i + 1);
	    if (strlen(s) - nspace <= minlen)
		goto donesc;
	}
    }

    /* all else has failed so we use brute force */

    for (i = UPPER; i > 0; i--) {
	if (!FIRSTCHAR(i) && !isspace((int)s[i]))
	    mystrcpy(s + i, s + i + 1);
	if (strlen(s) - nspace <= minlen)
	    goto donesc;
    }

donesc:
    {  // remove internal spaces as required
	int upper = (int) strlen(s);
	if (upper > minlen)
	    for (i = upper - 1; i > 0; i--)
		if (isspace((int)s[i]))
		    mystrcpy(s + i, s + i + 1);
    }

    return mkChar(s);
}

#define FIRSTCHARW(i) (iswspace((int)wc[i-1]))
#define LASTCHARW(i) (!iswspace((int)wc[i-1]) && (!wc[i+1] || iswspace((int)wc[i+1])))
#define WUP (int)(wcslen(wc) - 1)

// lower-case vowels in English plus accented versions
static int vowels[] = {
    0x61, 0x65, 0x69, 0x6f, 0x75,
    0xe0, 0xe1, 0x2e, 0xe3, 0xe4, 0xe5,
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc,
    0x101, 0x103, 0x105, 0x113, 0x115, 0x117, 0x118, 0x11b,
    0x129, 0x12b, 0x12d, 0x12f, 0x131, 0x14d, 0x14f, 0x151,
    0x169, 0x16b, 0x16d, 0x16f, 0x171, 0x173
};

static Rboolean iswvowel(wchar_t w)
{
    int v = (int) w, n = sizeof(vowels)/sizeof(int);
    Rboolean found = FALSE;
    for(int i = 0; i < n; i++)
	if(v == vowels[i]) {found = TRUE; break;}

    return found;
}

static void mywcscpy(wchar_t *dest, const wchar_t *src)
{
    memmove(dest, src, sizeof(wchar_t) * (wcslen(src)+1));
}

static SEXP wstripchars(const wchar_t * const inchar, int minlen, int usecl)
{
    int i, j, nspace = 0;
    wchar_t *wc = (wchar_t *)cbuff.data;

    mywcscpy(wc, inchar);
    if (wcslen(wc) < minlen) goto donewsc;

    for (i = WUP, j = 1; i > 0; i--) {
	if (iswspace((int)wc[i])) {
	    if (j) wc[i] = '\0' ; else nspace++;
	} else j = 0;
	if (wcslen(wc) - nspace <= minlen)
	    goto donewsc;
    }

    if(usecl) {
	for (i = WUP; i > 0; i--) {
	    if (iswvowel(wc[i]) && LASTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (iswvowel(wc[i]) && !FIRSTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (islower((int)wc[i]) && LASTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}

	for (i = WUP; i > 0; i--) {
	    if (islower((int)wc[i]) && !FIRSTCHARW(i))
		mywcscpy(wc + i, wc + i + 1);
	    if (wcslen(wc) - nspace <= minlen)
		goto donewsc;
	}
    }

    for (i = WUP; i > 0; i--) {
	if (!FIRSTCHARW(i) && !iswspace((int)wc[i]))
	    mywcscpy(wc + i, wc + i + 1);
	if (wcslen(wc) - nspace <= minlen)
	    goto donewsc;
    }

donewsc:

    {
	int upper = (int) wcslen(wc);
	if (upper > minlen)
	    for (i = upper - 1; i > 0; i--)
		if (iswspace((int)wc[i])) mywcscpy(wc + i, wc + i + 1);
    }

    int nb = (int) wcstoutf8(NULL, wc, INT_MAX);
    char *cbuf = CallocCharBuf(nb);
    wcstoutf8(cbuf, wc, nb);
    SEXP ans = mkCharCE(cbuf, CE_UTF8);
    Free(cbuf);
    return ans;
}


SEXP attribute_hidden do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op,args);
    SEXP x = CAR(args);

    if (!isString(x))
	error(_("the first argument must be a character vector"));
    int minlen = asInteger(CADR(args));
    if (minlen == NA_INTEGER)
	error(_("invalid '%s' argument"), "minlength");
    int usecl = asLogical(CADDR(args));
    if (usecl == NA_INTEGER)
	error(_("invalid '%s' argument"), "use.classes");

    R_xlen_t len = XLENGTH(x);
    SEXP ans = PROTECT(allocVector(STRSXP, len));
    const void *vmax = vmaxget();
    Rboolean warn = FALSE;
    for (R_xlen_t i = 0 ; i < len ; i++) {
	SEXP el = STRING_ELT(x, i);
	if (el  == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    const char *s = CHAR(el);
	    if (strIsASCII(s)) {
		if(strlen(s) > minlen) {
		    R_AllocStringBuffer(strlen(s)+1, &cbuff);
		    SET_STRING_ELT(ans, i, stripchars(s, minlen, usecl));
		} else SET_STRING_ELT(ans, i, el);
	    } else {
		s = translateCharUTF8(el);
		int nc = (int) utf8towcs(NULL, s, 0);
		if (nc > minlen) {
		    warn = TRUE;
		    const wchar_t *wc = wtransChar(el);
		    nc = (int) wcslen(wc);
		    R_AllocStringBuffer(sizeof(wchar_t)*(nc+1), &cbuff);
		    SET_STRING_ELT(ans, i, wstripchars(wc, minlen, usecl));
		} else SET_STRING_ELT(ans, i, el);
	    }
	}
	vmaxset(vmax); // this throws away the result of wtransChar
    }
    if (usecl && warn) warning(_("abbreviate used with non-ASCII chars"));
    SHALLOW_DUPLICATE_ATTRIB(ans, x);
    /* This copied the class, if any */
    R_FreeStringBufferL(&cbuff);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_makenames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, ans;
    R_xlen_t i, n;
    int l, allow_;
    char *p, *tmp = NULL, *cbuf;
    const char *This;
    Rboolean need_prefix;
    const void *vmax;

    checkArity(op ,args);
    arg = CAR(args);
    if (!isString(arg))
	error(_("non-character names"));
    n = XLENGTH(arg);
    allow_ = asLogical(CADR(args));
    if (allow_ == NA_LOGICAL)
	error(_("invalid '%s' value"), "allow_");
    PROTECT(ans = allocVector(STRSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
	This = translateChar(STRING_ELT(arg, i));
	l = (int) strlen(This);
	/* need to prefix names not beginning with alpha or ., as
	   well as . followed by a number */
	need_prefix = FALSE;
	if (mbcslocale && This[0]) {
	    int nc = l, used;
	    wchar_t wc;
	    mbstate_t mb_st;
	    const char *pp = This;
	    mbs_init(&mb_st);
	    used = (int) Mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st);
	    pp += used; nc -= used;
	    if (wc == L'.') {
		if (nc > 0) {
		    Mbrtowc(&wc, pp, MB_CUR_MAX, &mb_st);
		    if (iswdigit(wc))  need_prefix = TRUE;
		}
	    } else if (!iswalpha(wc)) need_prefix = TRUE;
	} else {
	    if (This[0] == '.') {
		if (l >= 1 && isdigit(0xff & (int) This[1])) need_prefix = TRUE;
	    } else if (!isalpha(0xff & (int) This[0])) need_prefix = TRUE;
	}
	if (need_prefix) {
	    tmp = Calloc(l+2, char);
	    strcpy(tmp, "X");
	    strcat(tmp, translateChar(STRING_ELT(arg, i)));
	} else {
	    tmp = Calloc(l+1, char);
	    strcpy(tmp, translateChar(STRING_ELT(arg, i)));
	}
	if (mbcslocale) {
	    /* This cannot lengthen the string, so safe to overwrite it. */
	    int nc = (int) mbstowcs(NULL, tmp, 0);
	    if (nc >= 0) {
		wchar_t *wstr = Calloc(nc+1, wchar_t);
		mbstowcs(wstr, tmp, nc+1);
		for (wchar_t * wc = wstr; *wc; wc++) {
		    if (*wc == L'.' || (allow_ && *wc == L'_'))
			/* leave alone */;
		    else if (!iswalnum((int)*wc)) *wc = L'.';
		}
		wcstombs(tmp, wstr, strlen(tmp)+1);
		Free(wstr);
	    } else error(_("invalid multibyte string %d"), i+1);
	} else {
	    for (p = tmp; *p; p++) {
		if (*p == '.' || (allow_ && *p == '_')) /* leave alone */;
		else if (!isalnum(0xff & (int)*p)) *p = '.';
		/* else leave alone */
	    }
	}
//	l = (int) strlen(tmp);        /* needed? */
	SET_STRING_ELT(ans, i, mkChar(tmp));
	/* do we have a reserved word?  If so the name is invalid */
	if (!isValidName(tmp)) {
	    /* FIXME: could use R_Realloc instead */
	    cbuf = CallocCharBuf(strlen(tmp) + 1);
	    strcpy(cbuf, tmp);
	    strcat(cbuf, ".");
	    SET_STRING_ELT(ans, i, mkChar(cbuf));
	    Free(cbuf);
	}
	Free(tmp);
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_tolower(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    R_xlen_t i, n;
    int ul;
    char *p;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    checkArity(op, args);
    ul = PRIMVAL(op); /* 0 = tolower, 1 = toupper */

    x = CAR(args);
    /* coercion is done in wrapper */
    if (!isString(x)) error(_("non-character argument"));
    n = XLENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE__) || defined(__FreeBSD__)
    /* utf8towcs is really to UCS-4/2 */
    for (i = 0; i < n; i++)
	if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;
    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int nb, nc, j;
	wctrans_t tr = wctrans(ul ? "toupper" : "tolower");
	wchar_t * wc;
	char * cbuf;

	vmax = vmaxget();
	/* the translated string need not be the same length in bytes */
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x, i);
	    if (el == NA_STRING) SET_STRING_ELT(y, i, NA_STRING);
	    else {
		const char *xi;
		ienc = getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = CHAR(el);
		    nc = (int) utf8towcs(NULL, xi, 0);
		} else {
		    xi = translateChar(el);
		    nc = (int) mbstowcs(NULL, xi, 0);
		    ienc = CE_NATIVE;
		}
		if (nc >= 0) {
		    /* FIXME use this buffer for new string as well */
		    wc = (wchar_t *)
			R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
		    if (ienc == CE_UTF8) {
			utf8towcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = (int) wcstoutf8(NULL, wc, INT_MAX);
			cbuf = CallocCharBuf(nb);
			wcstoutf8(cbuf, wc, nb);
			SET_STRING_ELT(y, i, mkCharCE(cbuf, CE_UTF8));
		    } else {
			mbstowcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = (int) wcstombs(NULL, wc, 0);
			cbuf = CallocCharBuf(nb);
			wcstombs(cbuf, wc, nb + 1);
			SET_STRING_ELT(y, i, markKnown(cbuf, el));
		    }
		    Free(cbuf);
		} else {
		    error(_("invalid multibyte string %d"), i+1);
		}
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	char *xi;
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x, i) == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		xi = CallocCharBuf(strlen(CHAR(STRING_ELT(x, i))));
		strcpy(xi, translateChar(STRING_ELT(x, i)));
		for (p = xi; *p != '\0'; p++)
		    *p = (char) (ul ? toupper(*p) : tolower(*p));
		SET_STRING_ELT(y, i, markKnown(xi, STRING_ELT(x, i)));
		Free(xi);
	    }
	    vmaxset(vmax);
	}
    }
    SHALLOW_DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}


typedef enum { WTR_INIT, WTR_CHAR, WTR_RANGE } wtr_type;
struct wtr_spec {
    wtr_type type;
    struct wtr_spec *next;
    union {
	wchar_t c;
	struct {
	    wchar_t first;
	    wchar_t last;
	} r;
    } u;
};

static void
wtr_build_spec(const wchar_t *s, struct wtr_spec *trs) {
    int i, len = (int) wcslen(s);
    struct wtr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = NULL;
	if (s[i + 1] == L'-') {
	    _new->type = WTR_RANGE;
	    if (s[i] > s[i + 2])
		error(_("decreasing range specification ('%lc-%lc')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = WTR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct wtr_spec);
	_new->next = NULL;
	_new->type = WTR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
wtr_free_spec(struct wtr_spec *trs) {
    struct wtr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static wchar_t
wtr_get_next_char_from_spec(struct wtr_spec **p) {
    wchar_t c;
    struct wtr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the WTR_INIT case. */
    case WTR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case WTR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = L'\0';
	break;
    }
    return(c);
}

typedef enum { TR_INIT, TR_CHAR, TR_RANGE } tr_spec_type;
struct tr_spec {
    tr_spec_type type;
    struct tr_spec *next;
    union {
	unsigned char c;
	struct {
	    unsigned char first;
	    unsigned char last;
	} r;
    } u;
};

static void
tr_build_spec(const char *s, struct tr_spec *trs) {
    int i, len = (int) strlen(s);
    struct tr_spec *This, *_new;

    This = trs;
    for (i = 0; i < len - 2; ) {
	_new = Calloc(1, struct tr_spec);
	_new->next = NULL;
	if (s[i + 1] == '-') {
	    _new->type = TR_RANGE;
	    if (s[i] > s[i + 2])
		error(_("decreasing range specification ('%c-%c')"),
		      s[i], s[i + 2]);
	    _new->u.r.first = s[i];
	    _new->u.r.last = s[i + 2];
	    i = i + 3;
	} else {
	    _new->type = TR_CHAR;
	    _new->u.c = s[i];
	    i++;
	}
	This = This->next = _new;
    }
    for ( ; i < len; i++) {
	_new = Calloc(1, struct tr_spec);
	_new->next = NULL;
	_new->type = TR_CHAR;
	_new->u.c = s[i];
	This = This->next = _new;
    }
}

static void
tr_free_spec(struct tr_spec *trs) {
    struct tr_spec *This, *next;
    This = trs;
    while(This) {
	next = This->next;
	Free(This);
	This = next;
    }
}

static unsigned char
tr_get_next_char_from_spec(struct tr_spec **p) {
    unsigned char c;
    struct tr_spec *This;

    This = *p;
    if (!This)
	return('\0');
    switch(This->type) {
	/* Note: this code does not deal with the TR_INIT case. */
    case TR_CHAR:
	c = This->u.c;
	*p = This->next;
	break;
    case TR_RANGE:
	c = This->u.r.first;
	if (c == This->u.r.last) {
	    *p = This->next;
	} else {
	    (This->u.r.first)++;
	}
	break;
    default:
	c = '\0';
	break;
    }
    return(c);
}

typedef struct { wchar_t c_old, c_new; } xtable_t;

static R_INLINE int xtable_comp(const void *a, const void *b)
{
    return ((xtable_t *)a)->c_old - ((xtable_t *)b)->c_old;
}

static R_INLINE int xtable_key_comp(const void *a, const void *b)
{
    return *((wchar_t *)a) - ((xtable_t *)b)->c_old;
}

#define SWAP(_a, _b, _TYPE)                                    \
{                                                              \
    _TYPE _t;                                                  \
    _t    = *(_a);                                             \
    *(_a) = *(_b);                                             \
    *(_b) = _t;                                                \
}

#define ISORT(_base,_num,_TYPE,_comp)                          \
{                                                              \
/* insert sort */                                              \
/* require stable data */                                      \
    int _i, _j ;                                               \
    for ( _i = 1 ; _i < _num ; _i++ )                          \
	for ( _j = _i; _j > 0 &&                               \
		      (*_comp)(_base+_j-1, _base+_j)>0; _j--)  \
	   SWAP(_base+_j-1, _base+_j, _TYPE);                  \
}

#define COMPRESS(_base,_num,_TYPE,_comp)                       \
{                                                              \
/* supress even c_old. last use */                             \
    int _i,_j ;                                                \
    for ( _i = 0 ; _i < (*(_num)) - 1 ; _i++ ){                \
	int rc = (*_comp)(_base+_i, _base+_i+1);               \
	if (rc == 0){                                          \
	   for ( _j = _i, _i-- ; _j < (*(_num)) - 1; _j++ )     \
		*((_base)+_j) = *((_base)+_j+1);               \
	    (*(_num))--;                                       \
	}                                                      \
    }                                                          \
}

#define BSEARCH(_rc,_key,_base,_nmemb,_TYPE,_comp)             \
{                                                              \
    size_t l, u, idx;                                          \
    _TYPE *p;                                                  \
    int comp;                                                  \
    l = 0;                                                     \
    u = _nmemb;                                                \
    _rc = NULL;                                                \
    while (l < u)                                              \
    {                                                          \
	idx = (l + u) / 2;                                     \
	p =  (_base) + idx;                                    \
	comp = (*_comp)(_key, p);                              \
	if (comp < 0)                                          \
	    u = idx;                                           \
	else if (comp > 0)                                     \
	    l = idx + 1;                                       \
	else{                                                  \
	  _rc = p;                                             \
	  break;                                               \
	}                                                      \
    }                                                          \
}

SEXP attribute_hidden do_chartr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP old, _new, x, y;
    R_xlen_t i, n;
    char *cbuf;
    SEXP el;
    cetype_t ienc;
    Rboolean use_UTF8 = FALSE;
    const void *vmax;

    checkArity(op, args);
    old = CAR(args); args = CDR(args);
    _new = CAR(args); args = CDR(args);
    x = CAR(args);
    n = XLENGTH(x);
    if (!isString(old) || LENGTH(old) < 1 || STRING_ELT(old, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "old");
    if (LENGTH(old) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "old");
    if (!isString(_new) || LENGTH(_new) < 1 || STRING_ELT(_new, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "new");
    if (LENGTH(_new) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "new");
    if (!isString(x)) error("invalid '%s' argument", "x");

    /* utf8towcs is really to UCS-4/2 */
#if defined(Win32) || defined(__STDC_ISO_10646__) || defined(__APPLE__)  || defined(__FreeBSD__)
    for (i = 0; i < n; i++)
	if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;

    if (getCharCE(STRING_ELT(old, 0)) == CE_UTF8) use_UTF8 = TRUE;
    if (getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) use_UTF8 = TRUE;

    if (mbcslocale || use_UTF8 == TRUE)
#else
    if (mbcslocale)
#endif
    {
	int j, nb, nc;
	xtable_t *xtable, *tbl;
	int xtable_cnt;
	struct wtr_spec *trs_cnt, **trs_cnt_ptr;
	wchar_t c_old, c_new, *wc;
	const char *xi, *s;
	struct wtr_spec *trs_old, **trs_old_ptr;
	struct wtr_spec *trs_new, **trs_new_ptr;

	/* Initialize the old and new wtr_spec lists. */
	trs_old = Calloc(1, struct wtr_spec);
	trs_old->type = WTR_INIT;
	trs_old->next = NULL;
	trs_new = Calloc(1, struct wtr_spec);
	trs_new->type = WTR_INIT;
	trs_new->next = NULL;
	/* Build the old and new wtr_spec lists. */
	if (use_UTF8 && getCharCE(STRING_ELT(old, 0)) == CE_UTF8) {
	    s = CHAR(STRING_ELT(old, 0));
	    nc = (int) utf8towcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid UTF-8 string 'old'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    utf8towcs(wc, s, nc + 1);
	} else {
	    s = translateChar(STRING_ELT(old, 0));
	    nc = (int) mbstowcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid multibyte string 'old'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_old);
	trs_cnt = Calloc(1, struct wtr_spec);
	trs_cnt->type = WTR_INIT;
	trs_cnt->next = NULL;
	wtr_build_spec(wc, trs_cnt); /* use count only */

	if (use_UTF8 && getCharCE(STRING_ELT(_new, 0)) == CE_UTF8) {
	    s = CHAR(STRING_ELT(_new, 0));
	    nc = (int) utf8towcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid UTF-8 string 'new'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    utf8towcs(wc, s, nc + 1);
	} else {
	    s = translateChar(STRING_ELT(_new, 0));
	    nc = (int) mbstowcs(NULL, s, 0);
	    if (nc < 0) error(_("invalid multibyte string 'new'"));
	    wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t), &cbuff);
	    mbstowcs(wc, s, nc + 1);
	}
	wtr_build_spec(wc, trs_new);

	/* Initialize the pointers for walking through the old and new
	   wtr_spec lists and retrieving the next chars from the lists.
	*/

	trs_cnt_ptr = Calloc(1, struct wtr_spec *);
	*trs_cnt_ptr = trs_cnt->next;
	for (xtable_cnt = 0 ; wtr_get_next_char_from_spec(trs_cnt_ptr);
	      xtable_cnt++) ;
	wtr_free_spec(trs_cnt);
	Free(trs_cnt_ptr);
	xtable = (xtable_t *) R_alloc(xtable_cnt+1, sizeof(xtable_t));

	trs_old_ptr = Calloc(1, struct wtr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct wtr_spec *);
	*trs_new_ptr = trs_new->next;
	for (i = 0; ; i++) {
	    c_old = wtr_get_next_char_from_spec(trs_old_ptr);
	    c_new = wtr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		error(_("'old' is longer than 'new'"));
	    else {
		xtable[i].c_old = c_old;
		xtable[i].c_new = c_new;
	    }
	}

	/* Free the memory occupied by the wtr_spec lists. */
	wtr_free_spec(trs_old);
	wtr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	ISORT(xtable, xtable_cnt, xtable_t , xtable_comp);
	COMPRESS(xtable, &xtable_cnt, xtable_t, xtable_comp);

	PROTECT(y = allocVector(STRSXP, n));
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    el = STRING_ELT(x,i);
	    if (el == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		ienc = getCharCE(el);
		if (use_UTF8 && ienc == CE_UTF8) {
		    xi = CHAR(el);
		    nc = (int) utf8towcs(NULL, xi, 0);
		} else {
		    xi = translateChar(el);
		    nc = (int) mbstowcs(NULL, xi, 0);
		    ienc = CE_NATIVE;
		}
		if (nc < 0)
		    error(_("invalid input multibyte string %d"), i+1);
		wc = (wchar_t *) R_AllocStringBuffer((nc+1)*sizeof(wchar_t),
						     &cbuff);
		if (ienc == CE_UTF8) utf8towcs(wc, xi, nc + 1);
		else mbstowcs(wc, xi, nc + 1);
		for (j = 0; j < nc; j++){
		    BSEARCH(tbl,&wc[j], xtable, xtable_cnt,
			    xtable_t, xtable_key_comp);
		    if (tbl) wc[j] = tbl->c_new;
		}
		if (ienc == CE_UTF8) {
		    nb = (int) wcstoutf8(NULL, wc, INT_MAX);
		    cbuf = CallocCharBuf(nb);
		    wcstoutf8(cbuf, wc, nb);
		    SET_STRING_ELT(y, i, mkCharCE(cbuf, CE_UTF8));
		} else {
		    nb = (int) wcstombs(NULL, wc, 0);
		    cbuf = CallocCharBuf(nb);
		    wcstombs(cbuf, wc, nb + 1);
		    SET_STRING_ELT(y, i, markKnown(cbuf, el));
		}
		Free(cbuf);
	    }
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
    } else {
	unsigned char xtable[UCHAR_MAX + 1], *p, c_old, c_new;
	struct tr_spec *trs_old, **trs_old_ptr;
	struct tr_spec *trs_new, **trs_new_ptr;

	for (unsigned int ii = 0; ii <= UCHAR_MAX; ii++)
	    xtable[ii] = (unsigned char) ii;

	/* Initialize the old and new tr_spec lists. */
	trs_old = Calloc(1, struct tr_spec);
	trs_old->type = TR_INIT;
	trs_old->next = NULL;
	trs_new = Calloc(1, struct tr_spec);
	trs_new->type = TR_INIT;
	trs_new->next = NULL;
	/* Build the old and new tr_spec lists. */
	tr_build_spec(translateChar(STRING_ELT(old, 0)), trs_old);
	tr_build_spec(translateChar(STRING_ELT(_new, 0)), trs_new);
	/* Initialize the pointers for walking through the old and new
	   tr_spec lists and retrieving the next chars from the lists.
	*/
	trs_old_ptr = Calloc(1, struct tr_spec *);
	*trs_old_ptr = trs_old->next;
	trs_new_ptr = Calloc(1, struct tr_spec *);
	*trs_new_ptr = trs_new->next;
	for (;;) {
	    c_old = tr_get_next_char_from_spec(trs_old_ptr);
	    c_new = tr_get_next_char_from_spec(trs_new_ptr);
	    if (c_old == '\0')
		break;
	    else if (c_new == '\0')
		error(_("'old' is longer than 'new'"));
	    else
		xtable[c_old] = c_new;
	}
	/* Free the memory occupied by the tr_spec lists. */
	tr_free_spec(trs_old);
	tr_free_spec(trs_new);
	Free(trs_old_ptr); Free(trs_new_ptr);

	n = LENGTH(x);
	PROTECT(y = allocVector(STRSXP, n));
	vmax = vmaxget();
	for (i = 0; i < n; i++) {
	    if (STRING_ELT(x,i) == NA_STRING)
		SET_STRING_ELT(y, i, NA_STRING);
	    else {
		const char *xi = translateChar(STRING_ELT(x, i));
		cbuf = CallocCharBuf(strlen(xi));
		strcpy(cbuf, xi);
		for (p = (unsigned char *) cbuf; *p != '\0'; p++)
		    *p = xtable[*p];
		SET_STRING_ELT(y, i, markKnown(cbuf, STRING_ELT(x, i)));
		Free(cbuf);
	    }
	}
	vmaxset(vmax);
    }

    SHALLOW_DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}

SEXP attribute_hidden do_strtrim(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, width;
    R_xlen_t i, len;
    int nw, w, nc;
    const char *This;
    char *buf;
    const char *p; char *q;
    int w0, wsum, k, nb;
    wchar_t wc;
    mbstate_t mb_st;
    const void *vmax;

    checkArity(op, args);
    /* as.character happens at R level now */
    if (!isString(x = CAR(args)))
	error(_("strtrim() requires a character vector"));
    len = XLENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if(len > 0) {
	PROTECT(width = coerceVector(CADR(args), INTSXP));
	nw = LENGTH(width);
	if (!nw || (nw < len && len % nw))
	    error(_("invalid '%s' argument"), "width");
	for (i = 0; i < nw; i++)
	    if (INTEGER(width)[i] == NA_INTEGER ||
		INTEGER(width)[i] < 0)
		error(_("invalid '%s' argument"), "width");
	vmax = vmaxget();
	for (i = 0; i < len; i++) {
	    if (STRING_ELT(x, i) == NA_STRING) {
		SET_STRING_ELT(s, i, STRING_ELT(x, i));
		continue;
	    }
	    w = INTEGER(width)[i % nw];
	    This = translateChar(STRING_ELT(x, i));
	    nc = (int) strlen(This);
	    buf = R_AllocStringBuffer(nc, &cbuff);
	    wsum = 0;
	    mbs_init(&mb_st);
	    for (p = This, w0 = 0, q = buf; *p ;) {
		nb =  (int) Mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
		w0 = Ri18n_wcwidth((Rwchar_t)wc);
		if (w0 < 0) { p += nb; continue; } /* skip non-printable chars */
		wsum += w0;
		if (wsum <= w) {
		    for (k = 0; k < nb; k++) *q++ = *p++;
		} else break;
	    }
	    *q = '\0';
	    SET_STRING_ELT(s, i, markKnown(buf, STRING_ELT(x, i)));
	    vmaxset(vmax);
	}
	R_FreeStringBufferL(&cbuff);
	UNPROTECT(1);
    }
    SHALLOW_DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

static int strtoi(SEXP s, int base)
{
    if(s == NA_STRING || CHAR(s)[0] == '\0') return(NA_INTEGER);

    /* strtol might return extreme values on error */
    errno = 0;
    char *endp;
    long int res = strtol(CHAR(s), &endp, base); /* ASCII */
    return (errno || *endp != '\0' ||
	    res > INT_MAX || res < INT_MIN)
	? NA_INTEGER
	: (int) res;
}

SEXP attribute_hidden do_strtoi(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x, b;
    R_xlen_t i, n;
    int base;

    checkArity(op, args);

    x = CAR(args); args = CDR(args);
    b = CAR(args);

    if(!isInteger(b) || (LENGTH(b) < 1))
	error(_("invalid '%s' argument"), "base");
    base = INTEGER(b)[0];
    if((base != 0) && ((base < 2) || (base > 36)))
	error(_("invalid '%s' argument"), "base");

    PROTECT(ans = allocVector(INTSXP, n = LENGTH(x)));
    for(i = 0; i < n; i++)
	INTEGER(ans)[i] = strtoi(STRING_ELT(x, i), base);
    UNPROTECT(1);

    return ans;
}

/* creates a new STRSXP which is a suffix of string, starting
   with given index; the result is returned unprotected  */

SEXP attribute_hidden stringSuffix(SEXP string, int fromIndex) {

    int origLen = LENGTH(string);
    int newLen = origLen - fromIndex;

    SEXP res = PROTECT(allocVector(STRSXP, newLen));
    int i;
    for(i = 0; i < newLen; i++) {
	SET_STRING_ELT(res, i, STRING_ELT(string, fromIndex++));
    }

    UNPROTECT(1); /* res */
    return res;
}

SEXP attribute_hidden do_strrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x, n, el;
    R_xlen_t is, ix, in, ns, nx, nn;
    const char *xi;
    int j, ni, nc;
    const char *cbuf;
    char *buf;
    const void *vmax;

    checkArity(op, args);

    x = CAR(args); args = CDR(args);
    n = CAR(args);

    nx = XLENGTH(x);
    nn = XLENGTH(n);
    if((nx == 0) || (nn == 0))
	return allocVector(STRSXP, 0);

    ns = (nx > nn) ? nx : nn;

    PROTECT(s = allocVector(STRSXP, ns));
    vmax = vmaxget();
    is = ix = in = 0;
    for(; is < ns; is++) {
	el = STRING_ELT(x, ix);
	ni = INTEGER(n)[in];
	if((el == NA_STRING) || (ni == NA_INTEGER)) {
	    SET_STRING_ELT(s, is, NA_STRING);
	} else {
	    if(ni < 0)
		error(_("invalid '%s' value"), "times");
	    xi = CHAR(el);
	    nc = (int) strlen(xi);

	    /* check for feasible result length; use double to protect
	       against integer overflow */
	    double len = ((double) nc) * ni;
	    if (len > INT_MAX)
		error("R character strings are limited to 2^31-1 bytes");

	    cbuf = buf = CallocCharBuf(nc * ni);
	    for(j = 0; j < ni; j++) {
		strcpy(buf, xi);
		buf += nc;
	    }
	    SET_STRING_ELT(s, is, mkCharCE(cbuf, getCharCE(el)));
	    Free(cbuf);
	    vmaxset(vmax);
	}
	ix = (++ix == nx) ? 0 : ix;
	in = (++in == nn) ? 0 : in;
    }
    /* Copy names if not recycled. */
    if((ns == nx) &&
       (d = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	setAttrib(s, R_NamesSymbol, d);
    UNPROTECT(1);
    return s;
}
