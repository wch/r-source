/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8>
   abbreviate needs to be fixed, if possible.

   Changes already made:
   abbreviate needs to be fixed, if possible, but warns for now.
   Regex code should be OK, substitution does ASCII comparisons only.
   charToRaw/rawToChar should work at byte level, so is OK.
   agrep needed to test for non-ASCII input.
   make.names worked at byte not char level.
   substr() should work at char not byte level.
   Semantics of nchar() have been fixed.
   regexpr returned pos and match length in bytes not chars.
   tolower/toupper added wchar versions
   chartr works at char not byte level.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

#include <sys/types.h> /* probably not needed */
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <Rmath.h>     /* for imax2 */


#ifdef SUPPORT_MBCS
# include <R_ext/rlocale.h>
# include <wchar.h>
# include <wctype.h>
#endif


/* The next must come after other header files to redefine RE_DUP_MAX */
#include "Rregex.h"

#include <Print.h> /* for R_print */

#include "apse.h"

#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

#ifndef MAX
# define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

/* We use a shared buffer here to avoid reallocing small buffers, and
   keep a standard-size (8192) buffer allocated shared between the 
   various functions.

   If we want to make this thread-safe, we would need to initialize an
   instance non-statically in each using function.
 */

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};
static void AllocBuffer(int len, R_StringBuffer *cbuff)
{
    if(len >= 0) R_AllocStringBuffer(len, cbuff);
}
/* de-allocation is always safe as we statically initialize */
static void DeallocBuffer(R_StringBuffer *cbuff)
{
    if(cbuff->bufsize != MAXELTSIZE) R_FreeStringBuffer(cbuff);
}

static SEXP mkChar2(const char *name, SEXP ref)
{
    SEXP c = allocString(strlen(name));
    strcpy(CHAR(c), name);
    if(IS_LATIN1(ref) || IS_UTF8(ref)) {
	if(known_to_be_latin1) SET_LATIN1(c);
	if(known_to_be_utf8) SET_UTF8(c);
    }
    return c;
}


/* Functions to perform analogues of the standard C string library. */
/* Most are vectorized */

SEXP attribute_hidden do_nchar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP d, s, x, stype;
    int i, len, ntype;
    char *type;
#ifdef SUPPORT_MBCS
    int nc;
    char *xi;
    wchar_t *wc;
#endif

    checkArity(op, args);
    PROTECT(x = coerceVector(CAR(args), STRSXP));
    if (!isString(x))
	errorcall(call, _("nchar() requires a character vector"));
    len = LENGTH(x);
    stype = CADR(args);
    if(!isString(stype) || LENGTH(stype) != 1)
	errorcall(call, _("invalid '%s' argument"), "type");
    type = CHAR(STRING_ELT(stype, 0)); /* always ASCII */
    ntype = strlen(type);
    if(ntype == 0) errorcall(call, _("invalid '%s' argument"), "type");
    PROTECT(s = allocVector(INTSXP, len));
    for (i = 0; i < len; i++) {
	if(strncmp(type, "bytes", ntype) == 0) {
	    /* This works for NA strings too */	
	    INTEGER(s)[i] = length(STRING_ELT(x, i));
	} else if(strncmp(type, "chars", ntype) == 0) {
	    if(STRING_ELT(x, i) == NA_STRING) {
		INTEGER(s)[i] = 2;
	    } else {
#ifdef SUPPORT_MBCS
		if(mbcslocale) {
		    nc = mbstowcs(NULL, translateChar(STRING_ELT(x, i)), 0);
		    INTEGER(s)[i] = nc >= 0 ? nc : NA_INTEGER;
		} else
#endif
		    INTEGER(s)[i] = strlen(translateChar(STRING_ELT(x, i)));
	    }
	} else if(strncmp(type, "width", ntype) == 0) {
	    if(STRING_ELT(x, i) == NA_STRING) {
		INTEGER(s)[i] = 2;
	    } else {
#ifdef SUPPORT_MBCS
		if(mbcslocale) {
		    xi = translateChar(STRING_ELT(x, i));
		    nc = mbstowcs(NULL, xi, 0);
		    if(nc >= 0) {
			AllocBuffer((nc+1)*sizeof(wchar_t), &cbuff);
			wc = (wchar_t *) cbuff.data;
			mbstowcs(wc, xi, nc + 1);
			INTEGER(s)[i] = Ri18n_wcswidth(wc, 2147483647);
			if(INTEGER(s)[i] < 1) INTEGER(s)[i] = nc;
		    } else
			INTEGER(s)[i] = NA_INTEGER;
		} else
#endif
		INTEGER(s)[i] = strlen(translateChar(STRING_ELT(x, i)));
	    }
	} else
	    errorcall(call, _("invalid '%s' argument"), "type");
    }
#if defined(SUPPORT_MBCS)
    DeallocBuffer(&cbuff);
#endif
    if ((d = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	setAttrib(s, R_NamesSymbol, d);
    if ((d = getAttrib(x, R_DimSymbol)) != R_NilValue)
	setAttrib(s, R_DimSymbol, d);
    if ((d = getAttrib(x, R_DimNamesSymbol)) != R_NilValue)
	setAttrib(s, R_DimNamesSymbol, d);
    UNPROTECT(2);
    return s;
}

static void substr(char *buf, char *str, int sa, int so)
{
/* Store the substring	str [sa:so]  into buf[] */
    int i;
#ifdef SUPPORT_MBCS
    if(mbcslocale && !utf8strIsASCII(str)) {
	int j, used;
	mbstate_t mb_st;
	mbs_init(&mb_st);
	for(i = 1; i < sa; i++) str += Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
	for(i = sa; i <= so; i++) {
	    used = Mbrtowc(NULL, str, MB_CUR_MAX, &mb_st);
	    for (j = 0; j < used; j++) *buf++ = *str++;
	}
    } else
#endif
	for (str += (sa - 1), i = sa; i <= so; i++) *buf++ = *str++;
    *buf = '\0';
}

SEXP attribute_hidden do_substr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so;
    int i, len, start, stop, slen, k, l;
    char *buff, *ss;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if(!isString(x))
	errorcall(call, 
		  _("extracting substrings from a non-character object"));
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if(len > 0) {
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    errorcall(call, _("invalid substring argument(s) in substr()"));

	/* Calculate the buffer size needed.  The substring could be a lot
	   shorter, but in an MBCS locale it is tedious to calculate how long.
	*/
	slen = 0;
	for (i = 0; i < len; i++) 
	    if (STRING_ELT(x,i) != NA_STRING)
		slen = imax2(slen, strlen(translateChar(STRING_ELT(x, i))) + 1);
	buff = Calloc(slen, char);

	for (i = 0; i < len; i++) {
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    if (STRING_ELT(x,i) == NA_STRING 
		|| start == NA_INTEGER
		|| stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    ss = translateChar(STRING_ELT(x, i));
	    slen = strlen(ss);
	    if (start < 1) start = 1;
	    if (start > stop || start > slen) {
		buff[0] = '\0';
	    } else {
		if (stop > slen) stop = slen;
		substr(buff, ss, start, stop);
	    }
	    SET_STRING_ELT(s, i, mkChar2(buff, STRING_ELT(x, i)));
	}
	Free(buff);
    }
    DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return s;
}

static void substrset(char *buf, char *str, int sa, int so)
{
/* Replace the substring buf [sa:so] by str[] */
#ifdef SUPPORT_MBCS
    /* This cannot work for stateful encodings */
    if(mbcslocale) { /* probably not worth optimizing for non-utf8 strings */
	int i, in = 0, out = 0;

	for(i = 1; i < sa; i++) buf += Mbrtowc(NULL, buf, MB_CUR_MAX, NULL);
	/* now work out how many bytes to replace by how many */
	for(i = sa; i <= so; i++) {
	    in += Mbrtowc(NULL, str+in, MB_CUR_MAX, NULL);
	    out += Mbrtowc(NULL, buf+out, MB_CUR_MAX, NULL);
	}
	if(in != out) memmove(buf+in, buf+out, strlen(buf+out)+1);
	memcpy(buf, str, in);
    } else
#endif
	memcpy(buf + sa - 1, str, so - sa + 1);
}

SEXP attribute_hidden do_substrgets(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, sa, so, value;
    int i, len, start, stop, slen, vlen, k, l, v;
    char *ss;

    checkArity(op, args);
    x = CAR(args);
    sa = CADR(args);
    so = CADDR(args);
    value = CADDDR(args);
    k = LENGTH(sa);
    l = LENGTH(so);

    if(!isString(x))
      errorcall(call, _("replacing substrings in a non-character object"));
    len = LENGTH(x);
    PROTECT(s = allocVector(STRSXP, len));
    if(len > 0) {
	if (!isInteger(sa) || !isInteger(so) || k == 0 || l == 0)
	    errorcall(call, _("invalid substring argument(s) in substr<-()"));

	v = LENGTH(value);
	if (!isString(value) || v == 0)
	    errorcall(call, _("invalid right-hand side in substr<-()"));

	for (i = 0; i < len; i++) {
	    start = INTEGER(sa)[i % k];
	    stop = INTEGER(so)[i % l];
	    if (STRING_ELT(x, i) == NA_STRING
		|| STRING_ELT(value, i % v) == NA_STRING
		|| start == NA_INTEGER
		|| stop == NA_INTEGER) {
		SET_STRING_ELT(s, i, NA_STRING);
		continue;
	    }
	    ss = translateChar(STRING_ELT(x, i));
	    slen = strlen(ss);
	    if (start < 1) start = 1;
	    if (stop > slen) stop = slen;
	    if (start > stop) {
		/* just copy element across */
		SET_STRING_ELT(s, i, STRING_ELT(x, i));
	    } else {
		vlen = strlen(translateChar(STRING_ELT(value, i % v)));
#ifdef SUPPORT_MBCS
		AllocBuffer(slen+vlen, &cbuff);  /* might expand under MBCS */
#else
		AllocBuffer(slen, &cbuff);
#endif
		strcpy(cbuff.data, ss);
		if(stop > start + vlen - 1) stop = start + vlen - 1;
		substrset(cbuff.data, translateChar(STRING_ELT(value, i % v)), 
			  start, stop);
		SET_STRING_ELT(s, i, mkChar2(cbuff.data, STRING_ELT(x, i)));
	    }
	}
	DeallocBuffer(&cbuff);
    }
    UNPROTECT(1);
    return s;
}

/* strsplit is going to split the strings in the first argument into
 * tokens depending on the second argument. The characters of the second
 * argument are used to split the first argument.  A list of vectors is
 * returned of length equal to the input vector x, each element of the
 * list is the collection of splits for the corresponding element of x.
*/
SEXP attribute_hidden do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tok, x;
    int i, j, len, tlen, ntok, slen;
    int extended_opt, cflags, fixed, perl;
    char *buf, *pt = NULL, *split = "", *bufp, *laststart;
    regex_t reg;
    regmatch_t regmatch[1];
    pcre *re_pcre = NULL;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL;
    int options = 0;
    int erroffset, ovector[30];
    const char *errorptr;
    Rboolean usedRegex = FALSE, usedPCRE = FALSE;

    checkArity(op, args);
    x = CAR(args);
    tok = CADR(args);
    extended_opt = asLogical(CADDR(args));
    fixed = asLogical(CADDDR(args));
    perl = asLogical(CAD4R(args));

    if(!isString(x) || !isString(tok))
	errorcall_return(call, _("non-character argument in strsplit()"));
    if(extended_opt == NA_INTEGER) extended_opt = 1;
    if(perl == NA_INTEGER) perl = 0;

#ifdef SUPPORT_MBCS
    if(!fixed && perl) {
	if(utf8locale) options = PCRE_UTF8;
	else if(mbcslocale)
	    warning(_("perl = TRUE is only fully implemented in UTF-8 locales"));
    }
#endif

    cflags = 0;
    if(extended_opt) cflags = cflags | REG_EXTENDED;

    len = LENGTH(x);
    tlen = LENGTH(tok);
    /* special case split="" for efficiency */
    if(tlen == 1 && strlen(translateChar(STRING_ELT(tok, 0))) == 0) tlen = 0;

    PROTECT(s = allocVector(VECSXP, len));
    for(i = 0; i < len; i++) {
	if (STRING_ELT(x, i) == NA_STRING){
	    PROTECT(t = allocVector(STRSXP, 1));
	    SET_STRING_ELT(t, 0, NA_STRING);
	    SET_VECTOR_ELT(s, i, t);
	    UNPROTECT(1);
	    continue;
	}
	buf = translateChar(STRING_ELT(x, i));
	if(tlen > 0) {
	    /* NA token doesn't split */
	    if (STRING_ELT(tok,i % tlen) == NA_STRING){
		    PROTECT(t = allocVector(STRSXP, 1));
		    bufp = buf;
		    SET_STRING_ELT(t, 0, mkChar2(bufp, STRING_ELT(x, i)));
		    SET_VECTOR_ELT(s, i, t);
		    UNPROTECT(1);
		    continue;
	    }
	    /* find out how many splits there will be */
	    split = translateChar(STRING_ELT(tok, i % tlen));
	    slen = strlen(split);
	    ntok = 0;
	    if(fixed) {
		/* This is UTF-8 safe since it compares whole strings */
		laststart = buf;
		for(bufp = buf; bufp-buf < strlen(buf); bufp++) {
		    if((slen == 1 && *bufp != *split) ||
		       (slen > 1 && strncmp(bufp, split, slen))) continue;
		    ntok++;
		    bufp += MAX(slen - 1, 0);
		    laststart = bufp+1;
		}
		bufp = laststart;
	    } else if(perl) {
		usedPCRE = TRUE;
		tables = pcre_maketables();
		re_pcre = pcre_compile(split, options,
				       &errorptr, &erroffset, tables);
		if (!re_pcre)
		    errorcall(call, _("invalid split pattern '%s'"), split);
		re_pe = pcre_study(re_pcre, 0, &errorptr);
		bufp = buf;
		if(*bufp != '\0') {
		    while(pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0,
				    ovector, 30) >= 0) {
			/* Empty matches get the next char, so move by one. */
			bufp += MAX(ovector[1], 1);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		}
	    } else {
		/* Careful: need to distinguish empty (rm_eo == 0) from
		   non-empty (rm_eo > 0) matches.  In the former case, the
		   token extracted is the next character.  Otherwise, it is
		   everything before the start of the match, which may be
		   the empty string (not a ``token'' in the strict sense).
		*/
		usedRegex = TRUE;
		if(regcomp(&reg, split, cflags))
		    errorcall(call, _("invalid split pattern '%s'"), split);
		bufp = buf;
		if(*bufp != '\0') {
		    while(regexec(&reg, bufp, 1, regmatch, 0) == 0) {
			/* Empty matches get the next char, so move by
			   one. */
			bufp += MAX(regmatch[0].rm_eo, 1);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		}
	    }
	    if(*bufp == '\0')
		PROTECT(t = allocVector(STRSXP, ntok));
	    else
		PROTECT(t = allocVector(STRSXP, ntok + 1));
	    /* and fill with the splits */
	    laststart = bufp = buf;
	    pt = (char *) realloc(pt, (strlen(buf)+1) * sizeof(char));
	    for(j = 0; j < ntok; j++) {
		if(fixed) {
		    /* This is UTF-8 safe since it compares whole strings,
		       but it would be more efficient to skip along by chars.
		     */
		    for(; bufp - buf < strlen(buf); bufp++) {
			if((slen == 1 && *bufp != *split) ||
			   (slen > 1 && strncmp(bufp, split, slen))) continue;
			if(slen) {
			    strncpy(pt, laststart, bufp - laststart);
			    pt[bufp - laststart] = '\0';
			} else {
			    pt[0] = *bufp; pt[1] ='\0';
			}
			bufp += MAX(slen-1, 0);
			laststart = bufp+1;
			SET_STRING_ELT(t, j, mkChar2(pt, STRING_ELT(x, i)));
			break;
		    }
		    bufp = laststart;
		} else if(perl) {
		    pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0,
			      ovector, 30);
		    if(ovector[1] > 0) {
			/* Match was non-empty. */
			if(ovector[0] > 0)
			    strncpy(pt, bufp, ovector[0]);
			pt[ovector[0]] = '\0';
			bufp += ovector[1];
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;
		    }
		    SET_STRING_ELT(t, j, mkChar2(pt, STRING_ELT(x, i)));
		} else {
		    regexec(&reg, bufp, 1, regmatch, 0);
		    if(regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if(regmatch[0].rm_so > 0)
			    strncpy(pt, bufp, regmatch[0].rm_so);
			pt[regmatch[0].rm_so] = '\0';
			bufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;
		    }
		    SET_STRING_ELT(t, j, mkChar2(pt, STRING_ELT(x, i)));
		}
	    }
	    if(*bufp != '\0')
		SET_STRING_ELT(t, ntok, mkChar2(bufp, STRING_ELT(x, i)));
	} else {
	    /* split into individual characters (not bytes) */
#ifdef SUPPORT_MBCS
	    if(mbcslocale && !utf8strIsASCII(buf)) {
		char bf[20 /* > MB_CUR_MAX */], *p = buf;
		int used;
		mbstate_t mb_st;

		ntok = mbstowcs(NULL, buf, 0);
		if(ntok < 0) {
		    PROTECT(t = allocVector(STRSXP, 1));
		    SET_STRING_ELT(t, 0, NA_STRING);
		} else {
		    mbs_init(&mb_st);
		    PROTECT(t = allocVector(STRSXP, ntok));
		    for (j = 0; j < ntok; j++, p += used) {
			/* This is valid as we have already checked */
			used = mbrtowc(NULL, p, MB_CUR_MAX, &mb_st);
			memcpy(bf, p, used); bf[used] = '\0';
			SET_STRING_ELT(t, j, mkChar2(bf, STRING_ELT(x, i)));
		    }
		}
	    } else
#endif
	    {
		char bf[2];
		ntok = strlen(buf);
		PROTECT(t = allocVector(STRSXP, ntok));
		bf[1] = '\0';
		for (j = 0; j < ntok; j++) {
		    bf[0] = buf[j];
		    SET_STRING_ELT(t, j, mkChar2(bf, STRING_ELT(x, i)));
		}
	    }
	}
	UNPROTECT(1);
	SET_VECTOR_ELT(s, i, t);
	if(usedRegex) {
	    regfree(&reg);
	    usedRegex = FALSE;
	}
	if(usedPCRE) {
	    pcre_free(re_pe);
	    pcre_free(re_pcre);
	    pcre_free((void *)tables);
	    usedPCRE = FALSE;
	}
    }

    if (getAttrib(x, R_NamesSymbol) != R_NilValue)
	namesgets(s, getAttrib(x, R_NamesSymbol));
    UNPROTECT(1);
    free(pt);
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


#define FIRSTCHAR(i) (isspace((int)buff1[i-1]))
#define LASTCHAR(i) (!isspace((int)buff1[i-1]) && (!buff1[i+1] || isspace((int)buff1[i+1])))
#define LOWVOW(i) (buff1[i] == 'a' || buff1[i] == 'e' || buff1[i] == 'i' || \
		   buff1[i] == 'o' || buff1[i] == 'u')


/* memmove does allow overlapping src and dest */
static void mystrcpy(char *dest, const char *src)
{
    memmove(dest, src, strlen(src)+1);
}


/* abbreviate(inchar, minlen) */
static SEXP stripchars(char *inchar, int minlen)
{
/* This routine used to use strcpy with overlapping dest and src.
   That is not allowed by ISO C.
 */
    int i, j, nspace = 0, upper;
    char *buff1 = cbuff.data;

    mystrcpy(buff1, inchar);
    upper = strlen(buff1)-1;

    /* remove leading blanks */
    j = 0;
    for (i = 0 ; i < upper ; i++)
	if (isspace((int)buff1[i]))
	    j++;
	else
	    break;

    mystrcpy(buff1, &buff1[j]);
    upper = strlen(buff1) - 1;

    if (strlen(buff1) < minlen)
	goto donesc;

    for (i = upper, j = 1; i > 0; i--) {
	if (isspace((int)buff1[i])) {
	    if (j)
		buff1[i] = '\0' ;
	    else
		nspace++;
	}
	else
	    j = 0;
	/*strcpy(buff1[i],buff1[i+1]);*/
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if(LOWVOW(i) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (LOWVOW(i) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && LASTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    upper = strlen(buff1) -1;
    for (i = upper; i > 0; i--) {
	if (islower((int)buff1[i]) && !FIRSTCHAR(i))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

    /* all else has failed so we use brute force */

    upper = strlen(buff1) - 1;
    for (i = upper; i > 0; i--) {
	if (!FIRSTCHAR(i) && !isspace((int)buff1[i]))
	    mystrcpy(&buff1[i], &buff1[i + 1]);
	if (strlen(buff1) - nspace <= minlen)
	    goto donesc;
    }

donesc:

    upper = strlen(buff1);
    if (upper > minlen)
	for (i = upper - 1; i > 0; i--)
	    if (isspace((int)buff1[i]))
		mystrcpy(&buff1[i], &buff1[i + 1]);

    return(mkChar(buff1));
}


SEXP attribute_hidden do_abbrev(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    int i, len, minlen, uclass;
    Rboolean warn = FALSE;
    char *s;

    checkArity(op,args);
    x = CAR(args);

    if (!isString(x))
	errorcall_return(call, 
			 _("the first argument must be a character vector"));
    len = length(x);

    PROTECT(ans = allocVector(STRSXP, len));
    minlen = asInteger(CADR(args));
    uclass = asLogical(CADDR(args));
    for (i = 0 ; i < len ; i++) {
	if (STRING_ELT(x, i) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    s = translateChar(STRING_ELT(x, i));
	    warn = warn | !utf8strIsASCII(s);
	    AllocBuffer(strlen(s) + 1, &cbuff);
	    SET_STRING_ELT(ans, i, stripchars(s, minlen));
	}
    }
    if(warn) warningcall(call, _("abbreviate used with non-ASCII chars"));
    DUPLICATE_ATTRIB(ans, x);
    /* This copied the class, if any */
    DeallocBuffer(&cbuff);
    UNPROTECT(1);
    return(ans);
}

SEXP attribute_hidden do_makenames(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP arg, ans;
    int i, l, n, allow_;
    char *p, *_this, *tmp;
    Rboolean need_prefix;

    checkArity(op ,args);
    arg = CAR(args);
    if (!isString(arg))
	errorcall(call, _("non-character names"));
    n = length(arg);
    allow_ = asLogical(CADR(args));
    if(allow_ == NA_LOGICAL)
	errorcall(call, _("invalid value of 'allow_'"));
    PROTECT(ans = allocVector(STRSXP, n));
    for (i = 0 ; i < n ; i++) {
	_this = translateChar(STRING_ELT(arg, i));
	l = strlen(_this);
	/* need to prefix names not beginning with alpha or ., as
	   well as . followed by a number */
	need_prefix = FALSE;
#ifdef SUPPORT_MBCS
	if (mbcslocale && _this[0]) {
	    int nc = l, used;
	    wchar_t wc;
	    mbstate_t mb_st;

	    p = _this;
	    mbs_init(&mb_st);
	    used = Mbrtowc(&wc, p, MB_CUR_MAX, &mb_st); p += used; nc -= used;
	    if (wc == L'.') {
		if (nc > 0) {
		    Mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
		    if(iswdigit(wc))  need_prefix = TRUE;
		}
	    } else if (!iswalpha(wc)) need_prefix = TRUE;
	} else
#endif
	{
	    if (_this[0] == '.') {
		if (l >= 1 && isdigit(0xff & (int) _this[1])) need_prefix = TRUE;
	    } else if (!isalpha(0xff & (int) _this[0])) need_prefix = TRUE;
	}
	if (need_prefix) {
	    tmp = Calloc(l+2, char);
	    strcpy(tmp, "X");
	    strcat(tmp, translateChar(STRING_ELT(arg, i)));
	} else {
	    tmp = Calloc(l+1, char);
	    strcpy(tmp, translateChar(STRING_ELT(arg, i)));
	}
#ifdef SUPPORT_MBCS
	if (mbcslocale) {
	    /* This cannot lengthen the string, so safe to overwrite it.
	       Would also be possible a char at a time.
	     */
	    int nc = mbstowcs(NULL, tmp, 0);
	    wchar_t *wstr = Calloc(nc+1, wchar_t), *wc;
	    if(nc >= 0) {
		mbstowcs(wstr, tmp, nc+1);
		for(wc = wstr; *wc; wc++) {
		    if (*wc == L'.' || (allow_ && *wc == L'_')) 
			/* leave alone */;
		    else if(!iswalnum((int)*wc)) *wc = L'.';
		    /* If it changes into dot here,
		     * length will become short on mbcs.
		     * The name which became short will contain garbage.
		     * cf. 
		     *   >  make.names(c("\u30fb"))
		     *   [1] "X.\0"
		     */
		}
		wcstombs(tmp, wstr, strlen(tmp)+1);
		Free(wstr);
	    } else errorcall(call, _("invalid multibyte string %d"), i+1);
	} else
#endif
	{
	    for (p = tmp; *p; p++) {
		if (*p == '.' || (allow_ && *p == '_')) /* leave alone */;
		else if(!isalnum(0xff & (int)*p)) *p = '.';
		/* else leave alone */
	    }
	}
	l = strlen(tmp);
	SET_STRING_ELT(ans, i, allocString(l));
	strcpy(CHAR(STRING_ELT(ans, i)), tmp);
	/* do we have a reserved word?  If so the name is invalid */
	if (!isValidName(tmp)) {
	    SET_STRING_ELT(ans, i, allocString(strlen(tmp) + 1));
	    strcpy(CHAR(STRING_ELT(ans, i)), tmp);
	    strcat(CHAR(STRING_ELT(ans, i)), ".");
	}
	Free(tmp);
    }
    UNPROTECT(1);
    return ans;
}

/* This could be faster for plen > 1, but uses in R are for small strings */
static int fgrep_one(char *pat, char *target, int useBytes)
{
    int i = -1, plen=strlen(pat), len=strlen(target);
    char *p;

    if(plen == 0) return 0;
    if(plen == 1) {
    /* a single char is a common case */
	for(i = 0, p = target; *p; p++, i++)
	    if(*p == pat[0]) return i;
	return -1;
    }
#ifdef SUPPORT_MBCS
    if(!useBytes && mbcslocale) { /* skip along by chars */
	mbstate_t mb_st;
	int ib, used;
	mbs_init(&mb_st);
	for(ib = 0, i = 0; ib <= len-plen; i++) {
	    if(strncmp(pat, target+ib, plen) == 0) return i;
	    used = Mbrtowc(NULL,  target+ib, MB_CUR_MAX, &mb_st);
	    if(used <= 0) break;
	    ib += used;
	}
    } else
#endif
	for(i = 0; i <= len-plen; i++)
	    if(strncmp(pat, target+i, plen) == 0) return i;
    return -1;
}

SEXP attribute_hidden do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    regex_t reg;
    int i, j, n, nmatches;
    int igcase_opt, extended_opt, value_opt, fixed_opt, useBytes, cflags;
    char *cpat;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (useBytes == NA_INTEGER || !fixed_opt) useBytes = 0;

    if (length(pat) < 1) errorcall(call, R_MSG_IA);

    cpat = translateChar(STRING_ELT(pat, 0));
#ifdef SUPPORT_MBCS
    if(!useBytes && mbcslocale && !mbcsValid(cpat))
	errorcall(call, _("regular expression is invalid in this locale"));
#endif
    n = length(vec);
    nmatches = 0;
    PROTECT(ind = allocVector(LGLSXP, n));
    /* NAs are removed in R code so this isn't used */
    /* it's left in case we change our minds again */
    /* special case: NA pattern matches only NAs in vector */
    if (STRING_ELT(pat, 0) == NA_STRING){
	for(i = 0; i < n; i++){
	    if(STRING_ELT(vec, i) == NA_STRING){
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	}
	/* end NA pattern handling */
    } else {
	cflags = 0;
	if (extended_opt) cflags = cflags | REG_EXTENDED;
	if (igcase_opt) cflags = cflags | REG_ICASE;

	if (!fixed_opt && regcomp(&reg, cpat, cflags))
	    errorcall(call, _("invalid regular expression '%s'"), cpat);

	for (i = 0 ; i < n ; i++) {
	    LOGICAL(ind)[i] = 0;
	    if (STRING_ELT(vec, i) != NA_STRING) {
		char *s = translateChar(STRING_ELT(vec, i));
#ifdef SUPPORT_MBCS
		if(!useBytes && mbcslocale && !mbcsValid(s)) {
		    warningcall(call,
				_("input string %d is invalid in this locale"),
				i+1);
		    continue;
		}
#endif
		if (fixed_opt) LOGICAL(ind)[i] =
				   fgrep_one(cpat, s, useBytes) >= 0;
		else if(regexec(&reg, s, 0, NULL, 0) == 0)
		    LOGICAL(ind)[i] = 1;
	    }
	    if(LOGICAL(ind)[i]) nmatches++;
	}
	if (!fixed_opt) regfree(&reg);
    }

    if (value_opt) {
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
	ans = allocVector(STRSXP, nmatches);
	for (i = 0, j = 0; i < n ; i++)
	    if (LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
	/* copy across names and subset */
	if (!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if (LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
    } else {
	ans = allocVector(INTSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (LOGICAL(ind)[i]) INTEGER(ans)[j++] = i + 1;
    }
    UNPROTECT(1);
    return ans;
}

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static int length_adj(char *repl, regmatch_t *regmatch, int nsubexpr)
{
    int k, n;
    char *p = repl;
    n = strlen(repl) - (regmatch[0].rm_eo - regmatch[0].rm_so);
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		if (k > nsubexpr)
		    error(_("invalid backreference %d in regular expression"), k);
		n += (regmatch[k].rm_eo - regmatch[k].rm_so) - 2;
		p++;
	    }
	    else if (p[1] == 0) {
				/* can't escape the final '\0' */
		n -= 1;
	    }
	    else {
		n -= 1;
		p++;
	    }
	}
	p++;
    }
    return n;
}

static char *string_adj(char *target, char *orig, char *repl,
			regmatch_t *regmatch)
{
    int i, k;
    char *p = repl, *t = target;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) {
		p += 1;
	    }
	    else {
		p += 1;
		*t++ = *p++;
	    }
	}
	else *t++ = *p++;
    }
    return t;
}


SEXP attribute_hidden do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, vec, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, j, n, ns, nmatch, offset;
    int global, igcase_opt, extended_opt, fixed_opt, useBytes,
	cflags, eflags, last_end;
    char *s, *t, *u;
    char *spat, *srep;
    int patlen = 0, replen = 0, st, nr;

    checkArity(op, args);

    global = PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (useBytes == NA_INTEGER || !fixed_opt) useBytes = 0;

    if (length(pat) < 1 || length(rep) < 1)
	errorcall(call, R_MSG_IA);

    cflags = 0;
    if (extended_opt) cflags = cflags | REG_EXTENDED;
    if (igcase_opt) cflags = cflags | REG_ICASE;

    spat = translateChar(STRING_ELT(pat, 0));
    srep = translateChar(STRING_ELT(rep, 0));
#ifdef SUPPORT_MBCS
    if(mbcslocale && !mbcsValid(spat))
	errorcall(call, _("'pattern' is invalid in this locale"));
    if(mbcslocale && !mbcsValid(srep))
	errorcall(call, _("'replacement' is invalid in this locale"));
#endif
    if (!fixed_opt && regcomp(&reg, spat, cflags))
	errorcall(call, _("invalid regular expression '%s'"), spat);
    if (fixed_opt) {
	patlen = strlen(spat);
	if(!patlen)
	    errorcall(call, _("zero-length pattern"));
	replen = strlen(srep);
    }

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
      /* NA `pat' are removed in R code */
      /* the C code is left in case we change our minds again,
	 but this code _is_ used if 'x' contains NAs */
      /* NA matches only itself */
        if (STRING_ELT(vec,i) == NA_STRING) {
	    if (STRING_ELT(pat, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, STRING_ELT(rep, 0));
	    else
		SET_STRING_ELT(ans, i, NA_STRING);
	    continue;
	}
	if (STRING_ELT(pat, 0) == NA_STRING) {
	    SET_STRING_ELT(ans, i, STRING_ELT(vec,i));
	    continue;
	}
	/* end NA handling */
	offset = 0;
	nmatch = 0;
	s = translateChar(STRING_ELT(vec, i));
	t = srep;
	ns = strlen(s);

#ifdef SUPPORT_MBCS
	if(mbcslocale && !mbcsValid(s))
	    errorcall(call, ("input string %d is invalid in this locale"), i+1);
#endif
	if(fixed_opt) {
	    st = fgrep_one(spat, s, useBytes);
	    if(st < 0)
		SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		if(global) { /* need to find number of matches */
		    nr = 0;
		    do {
			nr++;
			s += st+patlen;
		    } while((st = fgrep_one(spat, s, useBytes)) >= 0);
		    /* and reset */
		    s = translateChar(STRING_ELT(vec, i));
		    st = fgrep_one(spat, s, useBytes);
		} else nr = 1;
		SET_STRING_ELT(ans, i, allocString(ns + nr*(replen - patlen)));
		u = CHAR(STRING_ELT(ans, i)); *u ='\0';
		do {
		    nr = strlen(u);
		    strncat(u, s, st); u[nr+st] = '\0'; s += st+patlen;
		    strcat(u, t);
		} while(global && (st = fgrep_one(spat, s, useBytes)) >= 0);
		strcat(u, s);
	    }
	} else {
	    /* Looks like REG_NOTBOL is no longer needed in this version,
	       but leave in as a precaution */
	    eflags = 0; last_end = -1;
	    /* We need to use private version of regexec here, as
	       head-chopping the string does not work with e.g. \b.
	     */
	    while (Rregexec(&reg, s, 10, regmatch, eflags, offset) == 0) {
		nmatch += 1;
		offset = regmatch[0].rm_eo;
		/* Do not repeat a 0-length match after a match, so
		   gsub("a*", "x", "baaac") is "xbxcx" not "xbxxcx" */
		if(offset > last_end) {
		    ns += length_adj(t, regmatch, reg.re_nsub);
		    last_end = offset;
		}
		if (s[offset] == '\0' || !global) break;
		/* If we have a 0-length match, move on */
		/* <MBCS FIXME> advance by a char */
		if (regmatch[0].rm_eo == regmatch[0].rm_so) offset++;
		eflags = REG_NOTBOL;
	    }
	    if (nmatch == 0)
		SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		SET_STRING_ELT(ans, i, allocString(ns));
		offset = 0;
		nmatch = 0;
		s = translateChar(STRING_ELT(vec, i));
		t = srep;
		u = CHAR(STRING_ELT(ans, i));
		ns = strlen(s);
		eflags = 0; last_end = -1;
		while (Rregexec(&reg, s, 10, regmatch, eflags, offset) == 0) {
		    /* printf("%s, %d %d\n", &s[offset],
		       regmatch[0].rm_so, regmatch[0].rm_eo); */
		    for (j = offset; j < regmatch[0].rm_so ; j++) *u++ = s[j];
		    if(regmatch[0].rm_eo > last_end) {
			u = string_adj(u, s, t, regmatch);
			last_end = regmatch[0].rm_eo;
		    }
		    offset = regmatch[0].rm_eo;
		    if (s[offset] == '\0' || !global) break;
		    /* <MBCS FIXME> advance by a char */
		    if (regmatch[0].rm_eo == regmatch[0].rm_so) 
			*u++ = s[offset++];
		    eflags = REG_NOTBOL;
		}
		if (offset < ns)
		    for (j = offset ; s[j] ; j++)
			*u++ = s[j];
		*u = '\0';
	    }
	}
    }
    if (!fixed_opt) regfree(&reg);
    DUPLICATE_ATTRIB(ans, vec);
    /* This copied the class, if any */
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchlen;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, n, st, extended_opt, fixed_opt, useBytes, cflags;
    char *spat = NULL; /* -Wall */

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (useBytes == NA_INTEGER || !fixed_opt) useBytes = 0;

    /* allow 'text' to be zero-length from 2.3.1 */
    if (length(pat) < 1) errorcall(call, R_MSG_IA);
    if ( STRING_ELT(pat,0) == NA_STRING)
	errorcall(call, R_MSG_IA);

    cflags = extended_opt ? REG_EXTENDED : 0;

    spat = translateChar(STRING_ELT(pat, 0));
#ifdef SUPPORT_MBCS
    if(!useBytes && mbcslocale && !mbcsValid(spat))
	errorcall(call, _("regular expression is invalid in this locale"));
#endif
    if (!fixed_opt && regcomp(&reg, spat, cflags))
	errorcall(call, _("invalid regular expression '%s'"), spat);
    n = length(text);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(text, i) == NA_STRING) {
	    INTEGER(matchlen)[i] = INTEGER(ans)[i] = R_NaInt;
	} else {
	    char *s = translateChar(STRING_ELT(text, i));
#ifdef SUPPORT_MBCS
	    if(!useBytes && mbcslocale && !mbcsValid(s)) {
		warningcall(call,
			    _("input string %d is invalid in this locale"),
			    i+1);
		INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
		continue;
	    }
#endif
	    if (fixed_opt) {
		st = fgrep_one(spat, s, useBytes);
		INTEGER(ans)[i] = (st > -1)?(st+1):-1;
#ifdef SUPPORT_MBCS
		if(!useBytes && mbcslocale) {
		    INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			mbstowcs(NULL, spat, 0):-1;
		} else
#endif
		    INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			strlen(spat):-1;
	    } else {
		if(regexec(&reg, s, 1, regmatch, 0)
		   == 0) {
		    st = regmatch[0].rm_so;
		    INTEGER(ans)[i] = st + 1; /* index from one */
		    INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
#ifdef SUPPORT_MBCS
		    if(!useBytes && mbcslocale) {
			int mlen = regmatch[0].rm_eo - st;
			/* Unfortunately these are in bytes, so we need to
			   use chars instead */
			if(st > 0) {
			    AllocBuffer(st, &cbuff);
			    memcpy(cbuff.data, s, st);
			    cbuff.data[st] = '\0';
			    INTEGER(ans)[i] = 1+mbstowcs(NULL, cbuff.data, 0);
			    if(INTEGER(ans)[i] <= 0) /* an invalid string */
				INTEGER(ans)[i] = NA_INTEGER;
			}
			AllocBuffer(mlen+1, &cbuff);
			memcpy(cbuff.data, s+st, mlen);
			cbuff.data[mlen] = '\0';
			INTEGER(matchlen)[i] = mbstowcs(NULL, cbuff.data, 0);
			if(INTEGER(matchlen)[i] < 0) /* an invalid string */
			    INTEGER(matchlen)[i] = NA_INTEGER;
		    }
#endif
		} else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	    }
	}
    }
#ifdef SUPPORT_MBCS
    DeallocBuffer(&cbuff);
#endif
    if (!fixed_opt) regfree(&reg);
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}

static SEXP gregexpr_Regexc(const regex_t *reg, const char *string, 
                            int useBytes)
{
    int matchIndex, j, st, foundAll, foundAny, offset, len;
    regmatch_t regmatch[10];
    SEXP ans, matchlen;         /* Return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* Buffers for storing multiple matches */
    int bufsize = 1024;         /* Starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    matchIndex = -1;
    foundAll = foundAny = offset = 0;
    len = strlen(string);
    while (!foundAll) {
        if( offset < len &&
             Rregexec(reg, string, 1, regmatch, 0, offset) == 0) {
            if ((matchIndex + 1) == bufsize) {
                /* Reallocate match buffers */
                int newbufsize = bufsize * 2;
                SEXP tmp;
                tmp = allocVector(INTSXP, 2 * bufsize);
                for (j = 0; j < bufsize; j++)
                    INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
                UNPROTECT(1);
                matchlenbuf = tmp;
                PROTECT(matchlenbuf);
                tmp = allocVector(INTSXP, 2 * bufsize);
                for (j = 0; j < bufsize; j++)
                    INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
                matchbuf = tmp;
                UNPROTECT(2);
                PROTECT(matchbuf);
                PROTECT(matchlenbuf);
                bufsize = newbufsize;
            }
            matchIndex++;
            foundAny = 1;
            st = regmatch[0].rm_so;
            INTEGER(matchbuf)[matchIndex] = st + 1; /* index from one */
            INTEGER(matchlenbuf)[matchIndex] = regmatch[0].rm_eo - st;
            if (INTEGER(matchlenbuf)[matchIndex] == 0)
                offset = st + 1;
            else
                offset = regmatch[0].rm_eo;
#ifdef SUPPORT_MBCS
            if(!useBytes && mbcslocale) {
                int mlen = regmatch[0].rm_eo - st;
                /* Unfortunately these are in bytes, so we need to
                   use chars instead */
                if(st > 0) {
                    AllocBuffer(st, &cbuff);
                    memcpy(cbuff.data, string, st);
                    cbuff.data[st] = '\0';
                    INTEGER(matchbuf)[matchIndex] = 1+mbstowcs(NULL, cbuff.data, 0);
                    if(INTEGER(matchbuf)[matchIndex] <= 0) { /* an invalid string */
                        INTEGER(matchbuf)[matchIndex] = NA_INTEGER;
                        foundAll = 1;
                    }
                }
                AllocBuffer(mlen+1, &cbuff);
                memcpy(cbuff.data, string+st, mlen);
                cbuff.data[mlen] = '\0';
                INTEGER(matchlenbuf)[matchIndex] = mbstowcs(NULL, cbuff.data, 0);
                if(INTEGER(matchlenbuf)[matchIndex] < 0) { /* an invalid string */
                    INTEGER(matchlenbuf)[matchIndex] = NA_INTEGER;
                    foundAll = 1;
                }
            }
#endif
        } else {
            foundAll = 1;
            if (!foundAny) {
                matchIndex++;
                INTEGER(matchbuf)[matchIndex] = -1;
                INTEGER(matchlenbuf)[matchIndex] = -1;
            }
        }

    }
#ifdef SUPPORT_MBCS
    DeallocBuffer(&cbuff);
#endif
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    /* copy from buffers */
    for (j = 0; j <= matchIndex; j++) {
        INTEGER(ans)[j] = INTEGER(matchbuf)[j];
        INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
    }
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(4);
    return ans;
}

static SEXP gregexpr_fixed(char *pattern, char *string, int useBytes)
{
    int patlen, matchIndex, st, foundAll, foundAny, curpos, j, ansSize;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
#ifdef SUPPORT_MBCS
    if(!useBytes && mbcslocale)
        patlen = mbstowcs(NULL, pattern, 0);
    else
#endif
        patlen = strlen(pattern);
    foundAll = curpos = st = foundAny = 0;
    st = fgrep_one(pattern, string, useBytes);
    matchIndex = -1;
    if (st < 0) {
        INTEGER(matchbuf)[0] = -1;
        INTEGER(matchlenbuf)[0] = -1;
    } else {
        foundAny = 1;
        matchIndex++;
        INTEGER(matchbuf)[matchIndex] = st + 1; /* index from one */
        INTEGER(matchlenbuf)[matchIndex] = patlen;
        while(!foundAll) {
            string += st + patlen;
            curpos += st + patlen;
            st = fgrep_one(pattern, string, useBytes);
            if (st >= 0) {
                if ((matchIndex + 1) == bufsize) {
                    /* Reallocate match buffers */
                    int newbufsize = bufsize * 2;
                    SEXP tmp;
                    tmp = allocVector(INTSXP, 2 * bufsize);
                    for (j = 0; j < bufsize; j++)
                        INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
                    UNPROTECT(1);
                    matchlenbuf = tmp;
                    PROTECT(matchlenbuf);
                    tmp = allocVector(INTSXP, 2 * bufsize);
                    for (j = 0; j < bufsize; j++)
                        INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
                    matchbuf = tmp;
                    UNPROTECT(2);
                    PROTECT(matchbuf);
                    PROTECT(matchlenbuf);
                    bufsize = newbufsize;
                }
                matchIndex++;
                /* index from one */
                INTEGER(matchbuf)[matchIndex] = curpos + st + 1; 
                INTEGER(matchlenbuf)[matchIndex] = patlen;
            } else 
                foundAll = 1;
        }
    }
    ansSize = foundAny ? (matchIndex + 1) : 1;
    PROTECT(ans = allocVector(INTSXP, ansSize));
    PROTECT(matchlen = allocVector(INTSXP, ansSize));
    /* copy from buffers */
    for (j = 0; j < ansSize; j++) {
        INTEGER(ans)[j] = INTEGER(matchbuf)[j];
        INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
    }
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(4);
    return ans;
}

static SEXP gregexpr_NAInputAns(void)
{
    SEXP ans, matchlen;
    PROTECT(ans = allocVector(INTSXP, 1)); 
    PROTECT(matchlen = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = INTEGER(matchlen)[0] = R_NaInt;
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}

#ifdef SUPPORT_MBCS
static SEXP gregexpr_BadStringAns(void)
{
    SEXP ans, matchlen;
    PROTECT(ans = allocVector(INTSXP, 1)); 
    PROTECT(matchlen = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}
#endif

SEXP attribute_hidden do_gregexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ansList, ans;
    regex_t reg;
    int i, n, extended_opt, fixed_opt, useBytes, cflags;
    char *spat;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (useBytes == NA_INTEGER || !fixed_opt) useBytes = 0;

    if (length(pat) < 1 || length(text) < 1)
	errorcall(call, R_MSG_IA);
    if ( STRING_ELT(pat,0) == NA_STRING)
	errorcall(call, R_MSG_IA);

    cflags = extended_opt ? REG_EXTENDED : 0;
    spat = translateChar(STRING_ELT(pat, 0));

#ifdef SUPPORT_MBCS
    if(!useBytes && mbcslocale && !mbcsValid(spat))
	errorcall(call, _("regular expression is invalid in this locale"));
#endif
    if (!fixed_opt && regcomp(&reg, spat, cflags))
	errorcall(call, _("invalid regular expression '%s'"), spat);
    n = length(text);
    PROTECT(ansList = allocVector(VECSXP, n));
    for (i = 0 ; i < n ; i++) {
        int foundAll, foundAny, matchIndex, offset;
        foundAll = foundAny = offset = 0;
        matchIndex = -1;
	if (STRING_ELT(text, i) == NA_STRING) {
            PROTECT(ans = gregexpr_NAInputAns());
	} else {
	    char *s = translateChar(STRING_ELT(text, i));
#ifdef SUPPORT_MBCS
	    if(!useBytes && mbcslocale && !mbcsValid(s)) {
		warningcall(call,
			    _("input string %d is invalid in this locale"),
			    i+1);
                PROTECT(ans = gregexpr_BadStringAns());
	    } else
#endif
                {
                    if (fixed_opt)
                        PROTECT(ans = gregexpr_fixed(spat, s, useBytes));
                    else
                        PROTECT(ans = gregexpr_Regexc(&reg, s, useBytes));
                }
        }
        SET_VECTOR_ELT(ansList, i, ans);
        UNPROTECT(1);
    } 
    if (!fixed_opt) regfree(&reg);
    UNPROTECT(1);
    return ansList;
}

SEXP attribute_hidden do_tolower(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    int i, n, ul;
    char *p, *xi;

    checkArity(op, args);
    ul = PRIMVAL(op); /* 0 = tolower, 1 = toupper */

    x = CAR(args);
    if(!isString(x))
        errorcall(call, _("non-character argument to tolower()"));
    n = LENGTH(x);
    PROTECT(y = allocVector(STRSXP, n));
#ifdef SUPPORT_MBCS
    if(mbcslocale) {
        int nb, nc, j;
        wctrans_t tr = wctrans(ul ? "toupper" : "tolower");
        wchar_t * wc;

        /* the translated string need not be the same length in bytes */
        for(i = 0; i < n; i++) {
            if (STRING_ELT(x, i) == NA_STRING)
                SET_STRING_ELT(y, i, NA_STRING);
            else {
                xi = translateChar(STRING_ELT(x, i));
                nc = mbstowcs(NULL, xi, 0);
                if(nc >= 0) {
                    AllocBuffer((nc+1)*sizeof(wchar_t), &cbuff);
                    wc = (wchar_t *) cbuff.data;
                    mbstowcs(wc, xi, nc + 1);
                    for(j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
                    nb = wcstombs(NULL, wc, 0);
                    SET_STRING_ELT(y, i, allocString(nb));
                    wcstombs(CHAR(STRING_ELT(y, i)), wc, nb + 1);
		    markKnown(STRING_ELT(y, i), STRING_ELT(x, i));
                } else {
                    errorcall(call, _("invalid multibyte string %d"), i+1);
                }
            }
        }
        DeallocBuffer(&cbuff);
    } else
#endif
        {
            for(i = 0; i < n; i++) {
                if (STRING_ELT(x, i) == NA_STRING)
                    SET_STRING_ELT(y, i, NA_STRING);
                else {
                    xi = translateChar(STRING_ELT(x, i));
                    SET_STRING_ELT(y, i, allocString(strlen(xi)));
                    strcpy(CHAR(STRING_ELT(y, i)), xi);
                    for(p = CHAR(STRING_ELT(y, i)); *p != '\0'; p++)
                        *p = ul ? toupper(*p) : tolower(*p);
		    markKnown(STRING_ELT(y, i), STRING_ELT(x, i));
                }
            }
        }
    DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}


#ifdef SUPPORT_MBCS
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
    int i, len = wcslen(s);
    struct wtr_spec *_this, *_new;

    _this = trs;
    for(i = 0; i < len - 2; ) {
        _new = Calloc(1, struct wtr_spec);
        _new->next = NULL;
        if(s[i + 1] == L'-') {
            _new->type = WTR_RANGE;
            if(s[i] > s[i + 2])
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
        _this = _this->next = _new;
    }
    for( ; i < len; i++) {
        _new = Calloc(1, struct wtr_spec);
        _new->next = NULL;
        _new->type = WTR_CHAR;
        _new->u.c = s[i];
        _this = _this->next = _new;
    }
}

static void
wtr_free_spec(struct wtr_spec *trs) {
    struct wtr_spec *_this, *next;
    _this = trs;
    while(_this) {
        next = _this->next;
        Free(_this);
        _this = next;
    }
}

static wchar_t
wtr_get_next_char_from_spec(struct wtr_spec **p) {
    wchar_t c;
    struct wtr_spec *_this;

    _this = *p;
    if(!_this)
        return('\0');
    switch(_this->type) {
        /* Note: this code does not deal with the WTR_INIT case. */
    case WTR_CHAR:
        c = _this->u.c;
        *p = _this->next;
        break;
    case WTR_RANGE:
        c = _this->u.r.first;
        if(c == _this->u.r.last) {
            *p = _this->next;
        } else {
            (_this->u.r.first)++;
        }
        break;
    default:
        c = L'\0';
        break;
    }
    return(c);
}
#endif
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
    int i, len = strlen(s);
    struct tr_spec *_this, *_new;

    _this = trs;
    for(i = 0; i < len - 2; ) {
        _new = Calloc(1, struct tr_spec);
        _new->next = NULL;
        if(s[i + 1] == '-') {
            _new->type = TR_RANGE;
            if(s[i] > s[i + 2])
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
        _this = _this->next = _new;
    }
    for( ; i < len; i++) {
        _new = Calloc(1, struct tr_spec);
        _new->next = NULL;
        _new->type = TR_CHAR;
        _new->u.c = s[i];
        _this = _this->next = _new;
    }
}

static void
tr_free_spec(struct tr_spec *trs) {
    struct tr_spec *_this, *next;
    _this = trs;
    while(_this) {
        next = _this->next;
        Free(_this);
        _this = next;
    }
}

static unsigned char
tr_get_next_char_from_spec(struct tr_spec **p) {
    unsigned char c;
    struct tr_spec *_this;

    _this = *p;
    if(!_this)
        return('\0');
    switch(_this->type) {
        /* Note: this code does not deal with the TR_INIT case. */
    case TR_CHAR:
        c = _this->u.c;
        *p = _this->next;
        break;
    case TR_RANGE:
        c = _this->u.r.first;
        if(c == _this->u.r.last) {
            *p = _this->next;
        } else {
            (_this->u.r.first)++;
        }
        break;
    default:
        c = '\0';
        break;
    }
    return(c);
}

SEXP attribute_hidden do_chartr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP old, _new, x, y;
    int i, n;

    checkArity(op, args);
    old = CAR(args); args = CDR(args);
    _new = CAR(args); args = CDR(args);
    x = CAR(args);
    if(!isString(old) || (length(old) < 1) ||
       !isString(_new) || (length(_new) < 1) ||
       !isString(x) )
        errorcall(call, R_MSG_IA);

    if (STRING_ELT(old,0) == NA_STRING ||
        STRING_ELT(_new,0) == NA_STRING) {
        errorcall(call, _("invalid (NA) arguments."));
    }

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
        int j, nb, nc;
        wchar_t xtable[65536 + 1], c_old, c_new, *wc;
        char *xi, *s;
        struct wtr_spec *trs_old, **trs_old_ptr;
        struct wtr_spec *trs_new, **trs_new_ptr;

        for(i = 0; i <= UCHAR_MAX; i++) xtable[i] = i;

        /* Initialize the old and new wtr_spec lists. */
        trs_old = Calloc(1, struct wtr_spec);
        trs_old->type = WTR_INIT;
        trs_old->next = NULL;
        trs_new = Calloc(1, struct wtr_spec);
        trs_new->type = WTR_INIT;
        trs_new->next = NULL;
        /* Build the old and new wtr_spec lists. */
	s = translateChar(STRING_ELT(old, 0));
        nc = mbstowcs(NULL, s, 0);
        if(nc < 0) errorcall(call, _("invalid multibyte string 'old'"));
        AllocBuffer((nc+1)*sizeof(wchar_t), &cbuff);
        wc = (wchar_t *) cbuff.data;
        mbstowcs(wc, s, nc + 1);
        wtr_build_spec(wc, trs_old);

	s = translateChar(STRING_ELT(_new, 0));
        nc = mbstowcs(NULL, s, 0);
        if(nc < 0) errorcall(call, _("invalid multibyte string 'new'"));
        AllocBuffer((nc+1)*sizeof(wchar_t), &cbuff);
        wc = (wchar_t *) cbuff.data;
        mbstowcs(wc, s, nc + 1);
        wtr_build_spec(wc, trs_new);

        /* Initialize the pointers for walking through the old and new
           wtr_spec lists and retrieving the next chars from the lists.
        */
        trs_old_ptr = Calloc(1, struct wtr_spec *);
        *trs_old_ptr = trs_old->next;
        trs_new_ptr = Calloc(1, struct wtr_spec *);
        *trs_new_ptr = trs_new->next;
        for(;;) {
            c_old = wtr_get_next_char_from_spec(trs_old_ptr);
            c_new = wtr_get_next_char_from_spec(trs_new_ptr);
            if(c_old == '\0')
                break;
            else if(c_new == '\0')
                errorcall(call, _("'old' is longer than 'new'"));
            else
                xtable[c_old] = c_new;
        }
        /* Free the memory occupied by the wtr_spec lists. */
        wtr_free_spec(trs_old);
        wtr_free_spec(trs_new);
        Free(trs_old_ptr); Free(trs_new_ptr);

        n = LENGTH(x);
        PROTECT(y = allocVector(STRSXP, n));
        for(i = 0; i < n; i++) {
            if (STRING_ELT(x,i) == NA_STRING)
                SET_STRING_ELT(y, i, NA_STRING);
            else {
                xi = translateChar(STRING_ELT(x, i));
                nc = mbstowcs(NULL, xi, 0);
                if(nc < 0)
                    errorcall(call, _("invalid input multibyte string %d"),
                              i+1);
                AllocBuffer((nc+1)*sizeof(wchar_t), &cbuff);
                wc = (wchar_t *) cbuff.data;
                mbstowcs(wc, xi, nc + 1);
                for(j = 0; j < nc; j++) wc[j] = xtable[wc[j]];
                nb = wcstombs(NULL, wc, 0);
                SET_STRING_ELT(y, i, allocString(nb));
                wcstombs(CHAR(STRING_ELT(y, i)), wc, nb + 1);
		markKnown(STRING_ELT(y, i), STRING_ELT(x, i));
            }
        }
        DeallocBuffer(&cbuff);
    } else
#endif
        {
            unsigned char xtable[UCHAR_MAX + 1], *p, c_old, c_new;
            struct tr_spec *trs_old, **trs_old_ptr;
            struct tr_spec *trs_new, **trs_new_ptr;

            for(i = 0; i <= UCHAR_MAX; i++) xtable[i] = i;

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
            for(;;) {
                c_old = tr_get_next_char_from_spec(trs_old_ptr);
                c_new = tr_get_next_char_from_spec(trs_new_ptr);
                if(c_old == '\0')
                    break;
                else if(c_new == '\0')
                    errorcall(call, _("'old' is longer than 'new'"));
                else
                    xtable[c_old] = c_new;
            }
            /* Free the memory occupied by the tr_spec lists. */
            tr_free_spec(trs_old);
            tr_free_spec(trs_new);
            Free(trs_old_ptr); Free(trs_new_ptr);

            n = LENGTH(x);
            PROTECT(y = allocVector(STRSXP, n));
            for(i = 0; i < n; i++) {
                if (STRING_ELT(x,i) == NA_STRING) 
		    SET_STRING_ELT(y, i, NA_STRING);
                else {
		    char *xi = translateChar(STRING_ELT(x, i));
                    SET_STRING_ELT(y, i, allocString(strlen(xi)));
                    strcpy(CHAR(STRING_ELT(y, i)), xi);
                    for(p = (unsigned char *) CHAR(STRING_ELT(y, i));
                        *p != '\0'; p++) *p = xtable[*p];
		    markKnown(STRING_ELT(y, i), STRING_ELT(x, i));
                }
            }
        }

    DUPLICATE_ATTRIB(y, x);
    /* This copied the class, if any */
    UNPROTECT(1);
    return(y);
}

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches;
    int igcase_opt, value_opt, max_distance_opt;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    apse_t *aps;
    char *str;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    max_distance_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_deletions_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_insertions_opt = (apse_size_t)asInteger(CAR(args));
    args = CDR(args);
    max_substitutions_opt = (apse_size_t)asInteger(CAR(args));

    if(igcase_opt == NA_INTEGER) igcase_opt = 0;
    if(value_opt == NA_INTEGER) value_opt = 0;

    if(!isString(pat) || length(pat) < 1 || !isString(vec))
        errorcall(call, R_MSG_IA);

    /* NAs are removed in R code so this isn't used */
    /* it's left in case we change our minds again */
    /* special case: NA pattern matches only NAs in vector */
    if (STRING_ELT(pat,0)==NA_STRING){
        n = length(vec);
        nmatches=0;
        PROTECT(ind = allocVector(LGLSXP, n));
        for(i=0; i<n; i++){
            if(STRING_ELT(vec,i)==NA_STRING){
                INTEGER(ind)[i]=1;
                nmatches++;
            }
            else
                INTEGER(ind)[i]=0;
        }
        if (value_opt) {
            ans = allocVector(STRSXP, nmatches);
            j = 0;
            for (i = 0 ; i < n ; i++)
                if (INTEGER(ind)[i]) {
                    SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
                }
        }
        else {
            ans = allocVector(INTSXP, nmatches);
            j = 0;
            for (i = 0 ; i < n ; i++)
                if (INTEGER(ind)[i]) INTEGER(ans)[j++] = i + 1;
        }
        UNPROTECT(1);
        return ans;
    }
    /* end NA pattern handling */

#ifdef SUPPORT_UTF8
    /* test for non-ASCII strings */
    if(mbcslocale) {
        Rboolean warn = !utf8strIsASCII(CHAR(STRING_ELT(pat, 0)));
        if(!warn)
            for(i = 0 ; i < length(vec) ; i++)
                if(!utf8strIsASCII(CHAR(STRING_ELT(vec, i)))) {
                    warn = TRUE; break;
                }
        if(warn)
            warning(_("use of agrep() in a UTF-8 locale may only work for ASCII strings"));
    }
#endif

    /* Create search pattern object. */
    str = translateChar(STRING_ELT(pat, 0));
    aps = apse_create((unsigned char *)str, (apse_size_t)strlen(str),
                      max_distance_opt);
    if(!aps)
        error(_("could not allocate memory for approximate matching"));

    /* Set further restrictions on search distances. */
    apse_set_deletions(aps, max_deletions_opt);
    apse_set_insertions(aps, max_insertions_opt);
    apse_set_substitutions(aps, max_substitutions_opt);

    /* Matching. */
    n = length(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    for(i = 0 ; i < n ; i++) {
        if (STRING_ELT(vec, i) == NA_STRING) {
            LOGICAL(ind)[i] = 0;
            continue;
        }
        str = translateChar(STRING_ELT(vec, i));
        /* Set case ignore flag for the whole string to be matched. */
        if(!apse_set_caseignore_slice(aps, 0,
                                      (apse_ssize_t)strlen(str),
                                      (apse_bool_t)igcase_opt)) {
            /* Most likely, an error in apse_set_caseignore_slice()
             * means that allocating memory failed (as we ensure that
             * the slice is contained in the string) ... */
            errorcall(call, _("could not perform case insensitive matching"));
        }
        /* Perform match. */
        if(apse_match(aps,
                      (unsigned char *)str,
                      (apse_size_t)strlen(str))) {
            LOGICAL(ind)[i] = 1;
            nmatches++;
        }
        else LOGICAL(ind)[i] = 0;
    }
    apse_destroy(aps);

    PROTECT(ans = value_opt
            ? allocVector(STRSXP, nmatches)
            : allocVector(INTSXP, nmatches));
    if(value_opt) {
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
        for(j = i = 0 ; i < n ; i++) {
            if(LOGICAL(ind)[i])
                SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
        }
	/* copy across names and subset */
	if (!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if (LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
    }
    else {
        for(j = i = 0 ; i < n ; i++) {
            if(LOGICAL(ind)[i]==1)
                INTEGER(ans)[j++] = i + 1;
        }
    }

    UNPROTECT(2);
    return ans;
}

#define isRaw(x) (TYPEOF(x) == RAWSXP)

/* <UTF8>  charToRaw should work at byte level, ignore encoding */
SEXP attribute_hidden do_charToRaw(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int nc;

    checkArity(op, args);
    if(!isString(x) || LENGTH(x) == 0)
        errorcall(call, _("argument must be a character vector of length 1"));
    if(LENGTH(x) > 1)
        warningcall(call, _("argument should be a character vector of length 1\nall but the first element will be ignored"));
    nc = LENGTH(STRING_ELT(x, 0));
    ans = allocVector(RAWSXP, nc);
    memcpy(RAW(ans), CHAR(STRING_ELT(x, 0)), nc);
    return ans;
}

/* <UTF8>  rawToChar should work at byte level */
SEXP attribute_hidden do_rawToChar(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, c, x = CAR(args);
    int i, nc = LENGTH(x), multiple, len;
    char buf[2];

    checkArity(op, args);
    if(!isRaw(x))
        errorcall(call, _("argument 'x' must be a raw vector"));
    multiple = asLogical(CADR(args));
    if(multiple == NA_LOGICAL)
        errorcall(call, _("argument 'multiple' must be TRUE or FALSE"));
    if(multiple) {
        buf[1] = '\0';
        PROTECT(ans = allocVector(STRSXP, nc));
        for(i = 0; i < nc; i++) {
            buf[0] = (char) RAW(x)[i];
            SET_STRING_ELT(ans, i, mkChar(buf));
        }
        /* do we want to copy e.g. names here? */
    } else {
        len = LENGTH(x);
        PROTECT(ans = allocVector(STRSXP, 1));
        /* String is not necessarily 0-terminated and may contain nuls
           so don't use mkChar */
        c = allocString(len); /* adds zero terminator */
        memcpy(CHAR(c), RAW(x), len);
        SET_STRING_ELT(ans, 0, c);
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_rawShift(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, shift = asInteger(CADR(args));

    if(!isRaw(x))
        errorcall(call, _("argument 'x' must be a raw vector"));
    if(shift == NA_INTEGER || shift < -8 || shift > 8)
        errorcall(call, _("argument 'shift' must be a small integer"));
    PROTECT(ans = duplicate(x));
    if (shift > 0)
        for(i = 0; i < LENGTH(x); i++)
            RAW(ans)[i] <<= shift;
    else
        for(i = 0; i < LENGTH(x); i++)
            RAW(ans)[i] >>= (-shift);
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_rawToBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, j = 0, k;
    unsigned int tmp;

    if(!isRaw(x))
        errorcall(call, _("argument 'x' must be a raw vector"));
    PROTECT(ans = allocVector(RAWSXP, 8*LENGTH(x)));
    for(i = 0; i < LENGTH(x); i++) {
        tmp = (unsigned int) RAW(x)[i];
        for(k = 0; k < 8; k++, tmp >>= 1)
            RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_intToBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, j = 0, k;
    unsigned int tmp;

    if(!isInteger(x))
        errorcall(call, _("argument 'x' must be a integer vector"));
    PROTECT(ans = allocVector(RAWSXP, 32*LENGTH(x)));
    for(i = 0; i < LENGTH(x); i++) {
        tmp = (unsigned int) INTEGER(x)[i];
        for(k = 0; k < 32; k++, tmp >>= 1)
            RAW(ans)[j++] = tmp & 0x1;
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_packBits(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args), stype = CADR(args);
    Rboolean useRaw;
    int i, j, k, fac, len = LENGTH(x), slen;
    unsigned int itmp;
    Rbyte btmp;

    if (TYPEOF(x) != RAWSXP && TYPEOF(x) != LGLSXP && TYPEOF(x) != INTSXP)
        errorcall(call, _("argument 'x' must be raw, integer or logical"));
    if (!isString(stype)  || LENGTH(stype) != 1)
        errorcall(call, _("argument 'type' must be a character string"));
    useRaw = strcmp(CHAR(STRING_ELT(stype, 0)), "integer");
    fac = useRaw ? 8 : 32;
    if (len% fac)
        errorcall(call, _("argument 'x' must be a multiple of %d long"), fac);
    slen = len/fac;
    PROTECT(ans = allocVector(useRaw ? RAWSXP : INTSXP, slen));
    for(i = 0; i < slen; i++)
        if(useRaw) {
            btmp = 0;
            for(k = 7; k >= 0; k--) {
                btmp <<= 1;
                if(isRaw(x))
                    btmp |= RAW(x)[8*i + k] & 0x1;
                else if(isLogical(x) || isInteger(x)) {
                    j = INTEGER(x)[8*i+k];
                    if(j == NA_INTEGER)
                        errorcall(call,
                                  _("argument 'x' must not contain NAs"));
                    btmp |= j & 0x1;
                }
            }
            RAW(ans)[i] = btmp;
        } else {
            itmp = 0;
            for(k = 31; k >= 0; k--) {
                itmp <<= 1;
                if(isRaw(x))
                    itmp |= RAW(x)[32*i + k] & 0x1;
                else if(isLogical(x) || isInteger(x)) {
                    j = INTEGER(x)[32*i+k];
                    if(j == NA_INTEGER)
                        errorcall(call,
                                  _("argument 'x' must not contain NAs"));
                    itmp |= j & 0x1;
                }
            }
            INTEGER(ans)[i] = (int) itmp;
        }
    UNPROTECT(1);
    return ans;
}

static int mbrtoint(int *w, const char *s)
{
    unsigned int byte;
    byte = *((unsigned char *)s);

    if (byte == 0) {
        *w = 0;
        return 0;	
    } else if (byte < 0xC0) {
        *w = (int) byte;
        return 1;
    } else if (byte < 0xE0) {
        if(strlen(s) < 2) return -2;
        if ((s[1] & 0xC0) == 0x80) {
            *w = (int) (((byte & 0x1F) << 6) | (s[1] & 0x3F));
            return 2;
        } else return -1;
    } else if (byte < 0xF0) {
        if(strlen(s) < 3) return -2;
        if (((s[1] & 0xC0) == 0x80) && ((s[2] & 0xC0) == 0x80)) {
            *w = (int) (((byte & 0x0F) << 12)
                        | ((s[1] & 0x3F) << 6) | (s[2] & 0x3F));
            byte = *w;
            if(byte >= 0xD800 && byte <= 0xDFFF) return -1; /* surrogate */
            if(byte == 0xFFFE || byte == 0xFFFF) return -1;
            return 3;
        } else return -1;
    } else if (byte < 0xF8) {
        if(strlen(s) < 4) return -2;
        if (((s[1] & 0xC0) == 0x80) 
            && ((s[2] & 0xC0) == 0x80)
            && ((s[3] & 0xC0) == 0x80)) {
            *w = (int) (((byte & 0x07) << 18)
                        | ((s[1] & 0x3F) << 12) 
                        | ((s[2] & 0x3F) << 6)
                        | (s[3] & 0x3F));
            byte = *w;
            return 4;
        } else return -1;
    } else if (byte < 0xFC) {
        if(strlen(s) < 5) return -2;
        if (((s[1] & 0xC0) == 0x80) 
            && ((s[2] & 0xC0) == 0x80)
            && ((s[3] & 0xC0) == 0x80)
            && ((s[4] & 0xC0) == 0x80)) {
            *w = (int) (((byte & 0x03) << 24)
                        | ((s[1] & 0x3F) << 18) 
                        | ((s[2] & 0x3F) << 12) 
                        | ((s[3] & 0x3F) << 6)
                        | (s[4] & 0x3F));
            byte = *w;
            return 5;
        } else return -1;
    } else {
        if(strlen(s) < 6) return -2;
        if (((s[1] & 0xC0) == 0x80) 
            && ((s[2] & 0xC0) == 0x80)
            && ((s[3] & 0xC0) == 0x80)
            && ((s[4] & 0xC0) == 0x80)
            && ((s[5] & 0xC0) == 0x80)) {
            *w = (int) (((byte & 0x01) << 30)
                        | ((s[1] & 0x3F) << 24) 
                        | ((s[2] & 0x3F) << 18) 
                        | ((s[3] & 0x3F) << 12)
                        | ((s[5] & 0x3F) << 6)
                        | (s[5] & 0x3F));
            byte = *w;
            return 5;
        } else return -1;
    }
    return -2; /* not reached */
}

SEXP attribute_hidden do_utf8ToInt(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, x = CAR(args);
    int i, j, nc, *ians, tmp, used = 0; /* -Wall */
    char *s = CHAR(STRING_ELT(x, 0));

    checkArity(op, args);
    if(!isString(x) || LENGTH(x) == 0)
        errorcall(call, _("argument must be a character vector of length 1"));
    if(LENGTH(x) > 1)
        warningcall(call, _("argument should be a character vector of length 1\nall but the first element will be ignored"));
    nc = LENGTH(STRING_ELT(x, 0)); /* ints will be shorter */
    ians = (int *) R_alloc(nc,  sizeof(int *));
    for(i = 0, j = 0; i < nc; i++) {
        used = mbrtoint(&tmp, s);
        if(used <= 0) break;
        ians[j++] = tmp;
        s += used;
    }
    if(used < 0) error("invalid UTF-8 string");
    ans = allocVector(INTSXP, j);
    for(i = 0; i < j; i++) INTEGER(ans)[i] = ians[i];
    return ans;
}

/* based on pcre.c */
static const int utf8_table1[] =
    { 0x7f, 0x7ff, 0xffff, 0x1fffff, 0x3ffffff, 0x7fffffff};
static const int utf8_table2[] = { 0, 0xc0, 0xe0, 0xf0, 0xf8, 0xfc};

static size_t inttomb(char *s, const int wc)
{
    register int i, j;
    unsigned int cvalue = wc;
    char buf[10], *b;

    b = s ? s : buf;
    if(cvalue == 0) {*b = 0; return 0;}
    for (i = 0; i < sizeof(utf8_table1)/sizeof(int); i++)
        if (cvalue <= utf8_table1[i]) break;
    b += i;
    for (j = i; j > 0; j--) {
        *b-- = 0x80 | (cvalue & 0x3f);
        cvalue >>= 6;
    }
    *b = utf8_table2[i] | cvalue;
    return i + 1;
}

SEXP attribute_hidden do_intToUtf8(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, c, x = CAR(args);
    int i, nc = LENGTH(x), multiple, len, used;
    char buf[10];

    checkArity(op, args);
    if(!isInteger(x))
        errorcall(call, _("argument 'x' must be an integer vector"));
    multiple = asLogical(CADR(args));
    if(multiple == NA_LOGICAL)
        errorcall(call, _("argument 'multiple' must be TRUE or FALSE"));
    if(multiple) {
        PROTECT(ans = allocVector(STRSXP, nc));
        for(i = 0; i < nc; i++) {
            used = inttomb(buf, INTEGER(x)[i]);
            buf[used] = '\0';
            SET_STRING_ELT(ans, i, mkChar(buf));
        }
        /* do we want to copy e.g. names here? */
    } else {
        for(i = 0, len = 0; i < nc; i++)
            len += inttomb(NULL, INTEGER(x)[i]);
        PROTECT(ans = allocVector(STRSXP, 1));
        /* String is not necessarily 0-terminated and may contain nuls
           so don't use mkChar */
        c = allocString(len); /* adds zero terminator */
        for(i = 0, len = 0; i < nc; i++) {
            used = inttomb(buf, INTEGER(x)[i]);
            strncpy(CHAR(c) + len, buf, used);
            len += used;
        }
        SET_STRING_ELT(ans, 0, c);
    }
    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_strtrim(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, x, width;
    int i, len, nw, w, nc;
    char *_this;
#if defined(SUPPORT_MBCS)
    char *p, *q;
    int w0, wsum, k, nb;
    wchar_t wc;
    mbstate_t mb_st;
#endif

    checkArity(op, args);
    /* conversion happens at R level now */
    if (!isString(x = CAR(args)))
        errorcall(call, _("strtrim() requires a character vector"));
    len = LENGTH(x);
    PROTECT(width = coerceVector(CADR(args), INTSXP));
    nw = LENGTH(width);
    if(!nw || (nw < len && len % nw))
        errorcall(call, _("invalid '%s' argument"), "width");
    for(i = 0; i < nw; i++)
        if(INTEGER(width)[i] == NA_INTEGER ||
           INTEGER(width)[i] < 0)
            errorcall(call, _("invalid '%s' argument"), "width");
    PROTECT(s = allocVector(STRSXP, len));
    for (i = 0; i < len; i++) {
        if(STRING_ELT(x, i) == NA_STRING) {
            SET_STRING_ELT(s, i, STRING_ELT(x, i));
            continue;
        }
        w = INTEGER(width)[i % nw];
        _this = translateChar(STRING_ELT(x, i));
        nc = strlen(_this);
        AllocBuffer(nc, &cbuff);
#if defined(SUPPORT_MBCS)
        wsum = 0;
        mbs_init(&mb_st);
        for(p = _this, w0 = 0, q = cbuff.data; *p ;) {
            nb =  Mbrtowc(&wc, p, MB_CUR_MAX, &mb_st);
            w0 = Ri18n_wcwidth(wc);
            if(w0 < 0) { p += nb; continue; } /* skip non-printable chars */
            wsum += w0;
            if(wsum <= w) {
                for(k = 0; k < nb; k++) *q++ = *p++;
            } else break;
        }
        *q = '\0';
#else
        /* <FIXME> what about non-printing chars? */
        if(w >= nc) {
            SET_STRING_ELT(s, i, STRING_ELT(x, i));
            continue;
        }
        strncpy(cbuff.data, _this, w);
        cbuff.data[w] = '\0';
#endif
        SET_STRING_ELT(s, i, mkChar2(cbuff.data, STRING_ELT(x, i)));
    }
    if(len > 0) DeallocBuffer(&cbuff);
    DUPLICATE_ATTRIB(s, x);
    /* This copied the class, if any */
    UNPROTECT(2);
    return s;
}

#if defined(HAVE_GLOB) || defined(Win32)
#ifdef HAVE_GLOB_H
# include <glob.h>
#endif
#ifdef Win32
#include <dos_glob.h>
#define glob dos_glob
#define globfree dos_globfree
#else
# ifndef GLOB_QUOTE
#  define GLOB_QUOTE 0
# endif
#endif
SEXP attribute_hidden do_glob(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, ans;
    int i, n, res, dirmark;
    glob_t globbuf;

    checkArity(op, args);
    if(!isString(x = CAR(args)))
	errorcall(call, _("invalid '%s' argument"), "paths");
    if(!LENGTH(x)) return allocVector(STRSXP, 0);
    dirmark = asLogical(CADR(args));
    if(dirmark == NA_LOGICAL)
	errorcall(call, _("invalid '%s' argument"), "dirmark");
#ifndef GLOB_MARK
    if(dirmark)
	errorcall(call, 
		  _("'dirmark = TRUE' is not supported on this platform"));
#endif
    
    for(i = 0; i < LENGTH(x); i++) {
	res = glob(translateChar(STRING_ELT(x, i)), 
#ifdef GLOB_MARK
		   (dirmark ? GLOB_MARK : 0) |
#endif
		   GLOB_QUOTE | (i ? GLOB_APPEND : 0),
		   NULL, &globbuf);
#ifdef GLOB_ABORTED
	if(res == GLOB_ABORTED)
	    warningcall(call, _("read error on '%s'"), 
			translateChar(STRING_ELT(x, i)));
#endif
#ifdef GLOB_NOSPACE
	if(res == GLOB_NOSPACE)
	    errorcall(call, _("internal out-of-memory condition"));
#endif
    }
    n = globbuf.gl_pathc;
    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0; i < n; i++)
	SET_STRING_ELT(ans, i, mkChar2(globbuf.gl_pathv[i], STRING_ELT(x, i)));
    UNPROTECT(1);
    globfree(&globbuf);
    return ans;
}
#else
SEXP attribute_hidden do_glob(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x;

    checkArity(op, args);
    if(!isString(x = CAR(args)))
	errorcall(call, _("invalid '%s' argument"), "paths");
    return x;
}
#endif
