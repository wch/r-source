/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2009  The R Development Core Team
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
 *  http://www.r-project.org/Licenses/
 */

/*

Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

strsplit grep [g]sub [g]regexpr
  handle UTF-8 directly if fixed/perl = TRUE, via wchar_t for extended

  We currrently translate latin1 strings to the native encoding.
  We could use UTF-8 in a non-latin1-locale instead.

*/

/* It is possible to use TRE for fixed = TRUE.
   The main benefit would be code simplification: however, the
   special-purpose code is substantially faster, so we no longer
   plan to do so.
*/

/* PCRE supports only single-byte locales and UTF-8, so we convert
   inputs in all other MBCS locales to UTF-8.

   In [g]sub and [g]regexpr we need to know match postions in
   characters.  To avoid yet more cases we handle all MBCS locales in
   wchar in ERE for those functions.  (Byte positions suffice for
   [g]sub(fixed = TRUE), and [g]regexpr needs to convert to char
   positions for all MBCSs.)
*/


#ifdef HAVE_CONFIG_H
# include <config.h>
#endif


#include <Defn.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <ctype.h>
#include <wchar.h>
#include <wctype.h>    /* for wctrans_t */

/* As from TRE 0.8.0, tre.h replaces regex.h */
#include <tre/tre.h>

/* Some systems using --with-system-pcre might have pcre headers in
   a subdirectory -- not seen recently.
*/
#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

#ifndef MAX
# define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif

static void reg_report(int rc,  regex_t *reg, const char *pat)
{
    char errbuf[1001];
    tre_regerror(rc, reg, errbuf, 1001);
    warning(_("regcomp error:  '%s'"), errbuf);
    error(_("invalid regular expression '%s'"), pat);
}

/* FIXME: make more robust, and public */
static SEXP mkCharWLen(const wchar_t *wc, int nc)
{
    int nb; char *xi; wchar_t *wt;
    wt = (wchar_t *) alloca((nc+1)*sizeof(wchar_t));
    R_CheckStack();
    wcsncpy(wt, wc, nc); wt[nc] = 0;
    nb = wcstoutf8(NULL, wt, nc);
    xi = (char *) alloca((nb+1)*sizeof(char));
    R_CheckStack();
    wcstoutf8(xi, wt, nb + 1);
    return mkCharLenCE(xi, nb, CE_UTF8);
}

static SEXP mkCharW(const wchar_t *wc)
{
    int nb = wcstoutf8(NULL, wc, 0);
    char *xi = (char *) Calloc(nb+1, char);
    SEXP ans;
    wcstoutf8(xi, wc, nb + 1);
    ans = mkCharCE(xi, CE_UTF8);
    Free(xi);
    return ans;
}


/* strsplit is going to split the strings in the first argument into
 * tokens depending on the second argument. The characters of the second
 * argument are used to split the first argument.  A list of vectors is
 * returned of length equal to the input vector x, each element of the
 * list is the collection of splits for the corresponding element of x.
*/

SEXP attribute_hidden do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP args0 = args, ans, tok, x;
    int i, itok, j, len, tlen, ntok;
    int fixed_opt, perl_opt, useBytes;
    char *pt = NULL; wchar_t *wpt = NULL;
    const char *buf, *split = "", *bufp;
    const unsigned char *tables = NULL;
    Rboolean use_UTF8 = FALSE;
    const void *vmax, *vmax2;

    checkArity(op, args);
    x = CAR(args); args = CDR(args);
    tok = CAR(args); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args));
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(x) || !isString(tok)) error(_("non-character argument"));


    len = LENGTH(x);
    tlen = LENGTH(tok);

    /* treat split = NULL as split = "" */
    if (!tlen) { tlen = 1; SETCADR(args0, tok = mkString("")); }

    if (!useBytes) {
	if (perl_opt && mbcslocale) use_UTF8 = TRUE;
	if (!use_UTF8)
	    for (i = 0; i < tlen; i++)
		if (getCharCE(STRING_ELT(tok, i)) == CE_UTF8) {
		    use_UTF8 = TRUE; break;
		}
	if (!use_UTF8)
	    for (i = 0; i < len; i++)
		if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    /* group by token for efficiency with PCRE/TRE versions */
    PROTECT(ans = allocVector(VECSXP, len));
    vmax = vmaxget();
    for (itok = 0; itok < tlen; itok++) {
	SEXP this = STRING_ELT(tok, itok);

	if (this == NA_STRING) { /* NA token doesn't split */
	    for (i = itok; i < len; i += tlen)
		SET_VECTOR_ELT(ans, i, ScalarString(STRING_ELT(x, i)));
	    continue;
	} else if (!CHAR(this)[0]) { /* empty */
	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = translateCharUTF8(STRING_ELT(x, i));
		    if (!utf8Valid(buf)) {
			warning(_("input string %d is invalid UTF-8"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateChar(STRING_ELT(x, i));
		    if (mbcslocale && !mbcsValid(buf)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		if (!useBytes && (use_UTF8 || mbcslocale) && !strIsASCII(buf)) {
		/* split into individual characters (not bytes) */
		    char bf[20 /* > MB_CUR_MAX */];
		    const char *p = buf;
		    int used;
		    mbstate_t mb_st;

		    if (use_UTF8) {
			for (ntok = 0; *p; p += used, ntok++)
			    used = utf8clen(*p);
			p = buf;
			PROTECT(t = allocVector(STRSXP, ntok));
			for (j = 0; j < ntok; j++, p += used) {
			    used = utf8clen(*p);
			    memcpy(bf, p, used); bf[used] = '\0';
			    SET_STRING_ELT(t, j, mkCharCE(bf, CE_UTF8));
			}
		    } else if ((ntok = mbstowcs(NULL, buf, 0)) < 0) {
			PROTECT(t = ScalarString(NA_STRING));
		    } else {
			mbs_init(&mb_st);
			PROTECT(t = allocVector(STRSXP, ntok));
			for (j = 0; j < ntok; j++, p += used) {
			    /* This is valid as we have already checked */
			    used = mbrtowc(NULL, p, MB_CUR_MAX, &mb_st);
			    memcpy(bf, p, used); bf[used] = '\0';
			    SET_STRING_ELT(t, j, markKnown(bf, STRING_ELT(x, i)));
			}
		    }
		} else {
		    /* useBytes or ASCII or 
		       single-byte locale and not marked as UTF-8 */
		    char bf[2];
		    ntok = strlen(buf);
		    PROTECT(t = allocVector(STRSXP, ntok));
		    bf[1] = '\0';
		    for (j = 0; j < ntok; j++) {
			bf[0] = buf[j];
			SET_STRING_ELT(t, j, markKnown(bf, STRING_ELT(x, i)));
		    }
		}
		SET_VECTOR_ELT(ans, i, t);
		UNPROTECT(1);
		vmaxset(vmax2);
	    }
	} else if (fixed_opt) {
	    const char *laststart, *ebuf;
	    int slen;
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else if (use_UTF8) {
		split = translateCharUTF8(STRING_ELT(tok, itok));
		if (!utf8Valid(split))
		    error(_("'split' string %d is invalid UTF-8"), itok+1);
	    } else {
		split = translateChar(STRING_ELT(tok, itok));
		if (mbcslocale && !mbcsValid(split))
		    error(_("'split' string %d is invalid in this locale"), 
			  itok+1);
	    }
	    slen = strlen(split);

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}

		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = translateCharUTF8(STRING_ELT(x, i));
		    if (!utf8Valid(buf)) {
			warning(_("input string %d is invalid UTF-8"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateChar(STRING_ELT(x, i));
		    if (mbcslocale && !mbcsValid(buf)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		/* find out how many splits there will be */
		ntok = 0;
		/* This is UTF-8 safe since it compares whole strings */
		laststart = buf;
		ebuf = buf + strlen(buf);
		for (bufp = buf; bufp < ebuf; bufp++) {
		    if ((slen == 1 && *bufp != *split) ||
			(slen > 1 && strncmp(bufp, split, slen))) continue;
		    ntok++;
		    bufp += MAX(slen - 1, 0);
		    laststart = bufp+1;
		}
		bufp = laststart;
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		laststart = bufp = buf;
		pt = Realloc(pt, strlen(buf)+1, char);
		for (j = 0; j < ntok; j++) {
		    /* This is UTF-8 safe since it compares whole
		       strings, but <MBCS-FIXME> it would be more
		       efficient to skip along by chars.
		    */
		    for (; bufp < ebuf; bufp++) {
			if ((slen == 1 && *bufp != *split) ||
			    (slen > 1 && strncmp(bufp, split, slen))) continue;
			if (slen) {
			    strncpy(pt, laststart, bufp - laststart);
			    pt[bufp - laststart] = '\0';
			} else {
			    pt[0] = *bufp; pt[1] ='\0';
			}
			bufp += MAX(slen-1, 0);
			laststart = bufp+1;
			if (use_UTF8)
			    SET_STRING_ELT(t, j, mkCharCE(pt, CE_UTF8));
			else
			    SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
			break;
		    }
		    bufp = laststart;
		}
		if (*bufp) {
		    if (use_UTF8)
		        SET_STRING_ELT(t, ntok, mkCharCE(bufp, CE_UTF8));
		    else
		    	SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		}
		vmaxset(vmax2);
	    }
	} else if (perl_opt) {
	    pcre *re_pcre;
	    pcre_extra *re_pe;
	    int erroffset, ovector[30];
	    const char *errorptr;
	    int options = 0;

	    if (use_UTF8) options = PCRE_UTF8;
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else if (use_UTF8) {
		split = translateCharUTF8(STRING_ELT(tok, itok));
		if (!utf8Valid(split))
		    error(_("'split' string %d is invalid UTF-8"), itok+1);
	    } else {
		split = translateChar(STRING_ELT(tok, itok));
		if (mbcslocale && !mbcsValid(split))
		    error(_("'split' string %d is invalid in this locale"), itok+1);
	    }

	    if (!tables) tables = pcre_maketables();
	    re_pcre = pcre_compile(split, options,
				   &errorptr, &erroffset, tables);
	    if (!re_pcre) {
		if (errorptr)
		    warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			    errorptr, split+erroffset);
		error(_("invalid split pattern '%s'"), split);
	    }
	    re_pe = pcre_study(re_pcre, 0, &errorptr);
	    if (errorptr)
		warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}

		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else if (use_UTF8) {
		    buf = translateCharUTF8(STRING_ELT(x, i));
		    if (!utf8Valid(buf)) {
			warning(_("input string %d is invalid UTF-8"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		} else {
		    buf = translateChar(STRING_ELT(x, i));
		    if (mbcslocale && !mbcsValid(buf)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}
		/* find out how many splits there will be */
		ntok = 0;
		bufp = buf;
		if (*bufp) {
		    while(pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0,
				    ovector, 30) >= 0) {
			/* Empty matches get the next char, so move by one. */
			bufp += MAX(ovector[1], 1);
			ntok++;
			if (*bufp == '\0')
			    break;
		    }
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		bufp = buf;
		pt = Realloc(pt, strlen(buf)+1, char);
		for (j = 0; j < ntok; j++) {
		    pcre_exec(re_pcre, re_pe, bufp, strlen(bufp), 0, 0,
			      ovector, 30);
		    if (ovector[1] > 0) {
			/* Match was non-empty. */
			if (ovector[0] > 0)
			    strncpy(pt, bufp, ovector[0]);
			pt[ovector[0]] = '\0';
			bufp += ovector[1];
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;
		    }
		    if (use_UTF8)
			SET_STRING_ELT(t, j, mkCharCE(pt, CE_UTF8));
		    else
			SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
		}
		if (*bufp) {
		    if (use_UTF8)
		        SET_STRING_ELT(t, ntok, mkCharCE(bufp, CE_UTF8));
		    else
		    	SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		}
		vmaxset(vmax2);
	    }
	    pcre_free(re_pe);
	    pcre_free(re_pcre);
	} else if (!useBytes && use_UTF8) { /* ERE in wchar_t */
	    regex_t reg;
	    regmatch_t regmatch[1];
	    int rc;
	    int cflags = REG_EXTENDED;
	    const wchar_t *wbuf, *wbufp, *wsplit;

	    /* Careful: need to distinguish empty (rm_eo == 0) from
	       non-empty (rm_eo > 0) matches.  In the former case, the
	       token extracted is the next character.  Otherwise, it is
	       everything before the start of the match, which may be
	       the empty string (not a ``token'' in the strict sense).
	    */

	    wsplit = wtransChar(STRING_ELT(tok, itok));
	    if ((rc = tre_regwcomp(&reg, wsplit, cflags)))
		reg_report(rc, &reg, translateChar(STRING_ELT(tok, itok)));

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		wbuf = wtransChar(STRING_ELT(x, i));

		/* find out how many splits there will be */
		ntok = 0;
		wbufp = wbuf;
		if (*wbufp) {
		    while(tre_regwexec(&reg, wbufp, 1, regmatch, 0) == 0) {
			/* Empty matches get the next char, so move by one. */
			wbufp += MAX(regmatch[0].rm_eo, 1);
			ntok++;
			if (!*wbufp) break;
		    }
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*wbufp ? 1 : 0)));
		/* and fill with the splits */
		wbufp = wbuf;
		wpt = Realloc(wpt, wcslen(wbuf)+1, wchar_t);
		for (j = 0; j < ntok; j++) {
		    tre_regwexec(&reg, wbufp, 1, regmatch, 0);
		    if (regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if (regmatch[0].rm_so > 0)
			    wcsncpy(wpt, wbufp, regmatch[0].rm_so);
			wpt[regmatch[0].rm_so] = 0;
			wbufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			wpt[0] = *wbufp;
			wpt[1] = 0;
			wbufp++;
		    }
		    SET_STRING_ELT(t, j, 
				   mkCharWLen(wpt, regmatch[0].rm_so));
		}
		if (*wbufp)
		    SET_STRING_ELT(t, ntok,
				   mkCharWLen(wbufp, wcslen(wbufp)));
		vmaxset(vmax2);
	    }
	    tre_regfree(&reg);
	} else { /* ERE in normal chars -- single byte or MBCS */
	    regex_t reg;
	    regmatch_t regmatch[1];
	    int rc;
	    int cflags = REG_EXTENDED;

	    /* Careful: need to distinguish empty (rm_eo == 0) from
	       non-empty (rm_eo > 0) matches.  In the former case, the
	       token extracted is the next character.  Otherwise, it is
	       everything before the start of the match, which may be
	       the empty string (not a ``token'' in the strict sense).
	    */
	    /* never use_UTF8 */
	    if (useBytes)
		split = CHAR(STRING_ELT(tok, itok));
	    else {
		split = translateChar(STRING_ELT(tok, itok));
		if (mbcslocale && !mbcsValid(split))
		    error(_("'split' string %d is invalid in this locale"), itok+1);
	    }
	    if ((rc = tre_regcomp(&reg, split, cflags)))
		reg_report(rc, &reg, split);

	    vmax2 = vmaxget();
	    for (i = itok; i < len; i += tlen) {
		SEXP t;
		if (STRING_ELT(x, i) == NA_STRING) {
		    SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
		    continue;
		}
		/* never use_UTF8 */
		if (useBytes)
		    buf = CHAR(STRING_ELT(x, i));
		else {
		    buf = translateChar(STRING_ELT(x, i));
		    if (mbcslocale && !mbcsValid(buf)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			SET_VECTOR_ELT(ans, i, ScalarString(NA_STRING));
			continue;
		    }
		}

		/* find out how many splits there will be */
		ntok = 0;
		bufp = buf;
		if (*bufp) {
		    while(tre_regexec(&reg, bufp, 1, regmatch, 0) == 0) {
			/* Empty matches get the next char, so move by one. */
			bufp += MAX(regmatch[0].rm_eo, 1);
			ntok++;
			if (*bufp == '\0') break;
		    }
		}
		SET_VECTOR_ELT(ans, i,
			       t = allocVector(STRSXP, ntok + (*bufp ? 1 : 0)));
		/* and fill with the splits */
		bufp = buf;
		pt = Realloc(pt, strlen(buf)+1, char);
		for (j = 0; j < ntok; j++) {
		    tre_regexec(&reg, bufp, 1, regmatch, 0);
		    if (regmatch[0].rm_eo > 0) {
			/* Match was non-empty. */
			if (regmatch[0].rm_so > 0)
			    strncpy(pt, bufp, regmatch[0].rm_so);
			pt[regmatch[0].rm_so] = '\0';
			bufp += regmatch[0].rm_eo;
		    } else {
			/* Match was empty. */
			pt[0] = *bufp;
			pt[1] = '\0';
			bufp++;
		    }
		    SET_STRING_ELT(t, j, markKnown(pt, STRING_ELT(x, i)));
		}
		if (*bufp)
		    SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
		vmaxset(vmax2);
	    }
	    tre_regfree(&reg);
	}
	vmaxset(vmax);
    }

    if (getAttrib(x, R_NamesSymbol) != R_NilValue)
	namesgets(ans, getAttrib(x, R_NamesSymbol));
    UNPROTECT(1);
    Free(pt); Free(wpt);
    if (tables) pcre_free((void *)tables);
    return ans;
}

/* Used by grep[l] and [g]regexpr, with return value the match
   position in characters */
/* This could be faster for plen > 1, but uses in R are for small strings */
static int fgrep_one(const char *pat, const char *target,
		     Rboolean useBytes, Rboolean use_UTF8, int *next)
{
    int i = -1, plen=strlen(pat), len=strlen(target);
    const char *p;

    if (plen == 0) {
	if (next != NULL) *next = 1;
	return 0;
    }
    if (plen == 1 && (useBytes || !(mbcslocale || use_UTF8))) {
	/* a single byte is a common case */
	for (i = 0, p = target; *p; p++, i++)
	    if (*p == pat[0]) {
		if (next != NULL) *next = i + 1;
		return i;
	    }
	return -1;
    }
    if (!useBytes && mbcslocale) { /* skip along by chars */
	mbstate_t mb_st;
	int ib, used;
	mbs_init(&mb_st);
	for (ib = 0, i = 0; ib <= len-plen; i++) {
	    if (strncmp(pat, target+ib, plen) == 0) {
		if (next != NULL) *next = ib + plen;
		return i;
	    }
	    used = Mbrtowc(NULL,  target+ib, MB_CUR_MAX, &mb_st);
	    if (used <= 0) break;
	    ib += used;
	}
    } else if (!useBytes && use_UTF8) {
	int ib, used;
	for (ib = 0, i = 0; ib <= len-plen; i++) {
	    if (strncmp(pat, target+ib, plen) == 0) {
		if (next != NULL) *next = ib + plen;
		return i;
	    }
	    used = utf8clen(target[ib]);
	    if (used <= 0) break;
	    ib += used;
	}
    } else
	for (i = 0; i <= len-plen; i++)
	    if (strncmp(pat, target+i, plen) == 0) {
		if (next != NULL) *next = i + plen;
		return i;
	    }
    return -1;
}

/* Returns the match position in bytes, for use in [g]sub.
   len is the length of target.
*/

static int fgrep_one_bytes(const char *pat, const char *target, int len,
			   Rboolean useBytes, Rboolean use_UTF8)
{
    int i = -1, plen=strlen(pat);
    const char *p;

    if (plen == 0) return 0;
    if (plen == 1 && (useBytes || !(mbcslocale || use_UTF8))) {
	/* a single byte is a common case */
	for (i = 0, p = target; *p; p++, i++)
	    if (*p == pat[0]) return i;
	return -1;
    }
    if (!useBytes && mbcslocale) { /* skip along by chars */
	mbstate_t mb_st;
	int ib, used;
	mbs_init(&mb_st);
	for (ib = 0, i = 0; ib <= len-plen; i++) {
	    if (strncmp(pat, target+ib, plen) == 0) return ib;
	    used = Mbrtowc(NULL, target+ib, MB_CUR_MAX, &mb_st);
	    if (used <= 0) break;
	    ib += used;
	}
    } else if (!useBytes && use_UTF8) { /* not really needed */
	int ib, used;
	for (ib = 0, i = 0; ib <= len-plen; i++) {
	    if (strncmp(pat, target+ib, plen) == 0) return ib;
	    used = utf8clen(target[ib]);
	    if (used <= 0) break;
	    ib += used;
	}
    } else
	for (i = 0; i <= len-plen; i++)
	    if (strncmp(pat, target+i, plen) == 0) return i;
    return -1;
}

SEXP attribute_hidden do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ind, ans;
    regex_t reg;
    int i, j, n, nmatches = 0, ov, rc;
    int igcase_opt, value_opt, perl_opt, fixed_opt, useBytes, invert;
    const char *spat = NULL;
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    Rboolean use_UTF8 = FALSE, use_WC =  FALSE;
    const void *vmax;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    invert = asLogical(CAR(args));
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (invert == NA_INTEGER) invert = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = LENGTH(text);
    if (STRING_ELT(pat, 0) == NA_STRING) {
	if (value_opt) {
	    SEXP nmold = getAttrib(text, R_NamesSymbol);
	    PROTECT(ans = allocVector(STRSXP, n));
	    for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);
	    if (!isNull(nmold))
		setAttrib(ans, R_NamesSymbol, duplicate(nmold));
	} else {
	    PROTECT(ans = allocVector(INTSXP, n));
	    for (i = 0; i < n; i++)  INTEGER(ans)[i] = NA_INTEGER;
	}
	UNPROTECT(1);
	return ans;
    }

    if (!useBytes) {
	Rboolean onlyASCII = strIsASCII(CHAR(STRING_ELT(pat, 0)));
	if (onlyASCII)
	    for (i = 0; i < n; i++)
		if (!strIsASCII(CHAR(STRING_ELT(text, i)))) {
		    onlyASCII = FALSE;
		    break;
		}
	useBytes = onlyASCII;
    }

    if (!useBytes) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales */
	if (perl_opt && mbcslocale) use_UTF8 = TRUE;
	else if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if (!use_UTF8)
	    for (i = 0; i < n; i++)
		if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    if (!fixed_opt && !perl_opt) { use_WC = use_UTF8; use_UTF8 = FALSE; }

    if (useBytes)
	spat = CHAR(STRING_ELT(pat, 0));
    else if (use_WC) ;
    else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	if (!utf8Valid(spat)) error(_("regular expression is invalid UTF-8"));
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	if (mbcslocale && !mbcsValid(spat))
	    error(_("regular expression is invalid in this locale"));
    }

    if (fixed_opt) ; 
    else if (perl_opt) {
	int cflags = 0, erroffset;
	const char *errorptr;
	if (igcase_opt) cflags |= PCRE_CASELESS;
	if (!useBytes && use_UTF8) cflags |= PCRE_UTF8;
	tables = pcre_maketables();
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if (errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	    if (n > 10) {
		re_pe = pcre_study(re_pcre, 0, &errorptr);
		if (errorptr)
		    warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	    }
	}
    } else {
	int cflags = REG_NOSUB | REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC)
	    rc = tre_regcompb(&reg, spat, cflags);
	else
	    rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
	if (rc) reg_report(rc, &reg, spat);
    }

    PROTECT(ind = allocVector(LGLSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
	LOGICAL(ind)[i] = 0;
	if (STRING_ELT(text, i) != NA_STRING) {
	    const char *s = NULL;
	    if (useBytes)
		s = CHAR(STRING_ELT(text, i));
	    else if (use_WC) ;
	    else if (use_UTF8) {
		s = translateCharUTF8(STRING_ELT(text, i));
		if (!utf8Valid(s)) {
		    warning(_("input string %d is invalid UTF-8"), i+1);
		    continue;
		}
	    } else {
		s = translateChar(STRING_ELT(text, i));
		if (mbcslocale && !mbcsValid(s)) {
		    warning(_("input string %d is invalid in this locale"), i+1);
		    continue;
		}
	    }

	    if (fixed_opt)
		LOGICAL(ind)[i] = fgrep_one(spat, s, useBytes, use_UTF8, NULL) >= 0;
	    else if (perl_opt) {
		if (pcre_exec(re_pcre, re_pe, s, strlen(s), 0, 0, &ov, 0) >= 0)
		    INTEGER(ind)[i] = 1;
	    } else {
		if (!use_WC)
		    rc = tre_regexecb(&reg, s, 0, NULL, 0);
		else
		    rc = tre_regwexec(&reg, wtransChar(STRING_ELT(text, i)),
				      0, NULL, 0);
		if (rc == 0) LOGICAL(ind)[i] = 1;
	    }
	}
	vmaxset(vmax);
	if (invert ^ LOGICAL(ind)[i]) nmatches++;
    }

    if (fixed_opt);
    else if (perl_opt) {
	if (re_pe) pcre_free(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
    } else
	tre_regfree(&reg);

    if (PRIMVAL(op)) {/* grepl case */
	UNPROTECT(1);
	return ind;
    }

    if (value_opt) {
	SEXP nmold = getAttrib(text, R_NamesSymbol), nm;
	PROTECT(ans = allocVector(STRSXP, nmatches));
	for (i = 0, j = 0; i < n ; i++)
	    if (invert ^ LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(text, i));
	/* copy across names and subset */
	if (!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if (invert ^ LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
	UNPROTECT(1);
    } else {
	ans = allocVector(INTSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (invert ^ LOGICAL(ind)[i]) INTEGER(ans)[j++] = i + 1;
    }
    UNPROTECT(1);
    return ans;
}

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static char *string_adj(char *target, const char *orig, const char *repl,
			regmatch_t *regmatch)
{
    int i, k;
    const char *p = repl; char *t = target;

    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) p++; else {p++; *t++ = *p++;}
	}
	else *t++ = *p++;
    }
    return t;
}

/* used for single-byte locales, and UTF-8 for perl = TRUE */
static int count_subs(const char *repl)
{
    int i = 0;
    const char *p = repl;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {i++; p += 2;}
	    else if (p[1] == 0) p++; else p += 2;
	}
	else p++;
    }
    return i;
}

/* FIXME: use UCP for upper/lower conversion */
static 
char *pcre_string_adj(char *target, const char *orig, const char *repl,
		      int *ovec, Rboolean use_UTF8)
{
    int i, k, nb;
    const char *p = repl; 
    char *t = target, c;
    Rboolean upper = FALSE, lower = FALSE;

    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		/* Here we need to work in chars */
		nb = ovec[2*k+1] - ovec[2*k];
		if (nb > 0 && use_UTF8 && (upper || lower)) {
		    wctrans_t tr = wctrans(upper ? "toupper" : "tolower");
		    int j, nc;
		    char *xi, *p;
		    wchar_t *wc;
		    p = xi = (char *) alloca((nb+1)*sizeof(char));
		    R_CheckStack();
		    for (j = 0; j < nb; j++) *p++ = orig[ovec[2*k]+j];
		    *p = '\0';
		    nc = utf8towcs(NULL, xi, 0);
		    if (nc >= 0) {
			wc = (wchar_t *) alloca((nc+1)*sizeof(wchar_t));
			R_CheckStack();
			utf8towcs(wc, xi, nc + 1);
			for (j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = wcstoutf8(NULL, wc, 0);
			wcstoutf8(xi, wc, nb + 1);
			for (j = 0; j < nb; j++) *t++ = *xi++;
		    }
		} else
		    for (i = ovec[2*k] ; i < ovec[2*k+1] ; i++) {
			c = orig[i];
			*t++ = upper ? toupper(c) : (lower ? tolower(c) : c);
		    }
		p += 2;
	    } else if (p[1] == 'U') {
		p += 2;
		upper = TRUE; lower = FALSE;
	    } else if (p[1] == 'L') {
		p += 2;
		upper = FALSE; lower = TRUE;
	    } else if (p[1] == 'E') { /* end case modification */
		p += 2;
		upper = FALSE; lower = FALSE;
	    } else if (p[1] == 0) {
		p += 1;
	    } else {
		p += 1;
		*t++ = *p++;
	    }
	} else *t++ = *p++;
    }
    return t;
}

static wchar_t *wstring_adj(wchar_t *target, const wchar_t *orig,
			    const wchar_t *repl, regmatch_t *regmatch)
{
    int i, k;
    const wchar_t *p = repl;
    wchar_t *t = target;

    while (*p) {
	if (*p == L'\\') {
	    if (L'1' <= p[1] && p[1] <= L'9') {
		k = p[1] - L'0';
		for (i = regmatch[k].rm_so ; i < regmatch[k].rm_eo ; i++)
		    *t++ = orig[i];
		p += 2;
	    }
	    else if (p[1] == 0) p++; else {p++; *t++ = *p++;}
	}
	else *t++ = *p++;
    }
    return t;
}

static int wcount_subs(const wchar_t *repl)
{
    int i = 0;
    const wchar_t *p = repl;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {i++; p += 2;}
	    else if (p[1] == 0) p++; else p += 2;
	}
	else p++;
    }
    return i;
}


/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

SEXP attribute_hidden do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, text, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, j, n, ns, nns, nmatch, offset, rc;
    int global, igcase_opt, perl_opt, fixed_opt, useBytes, eflags, last_end;
    char *u, *cbuf;
    const char *spat = NULL, *srep = NULL, *s = NULL;
    int patlen = 0, replen = 0;
    Rboolean use_UTF8 = FALSE, use_WC = FALSE;
    const wchar_t *wrep = NULL;
    pcre *re_pcre = NULL;
    pcre_extra *re_pe  = NULL;
    const unsigned char *tables = NULL;
    const void *vmax = vmaxget();

    checkArity(op, args);

    global = PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");
    if (!isString(rep) || length(rep) < 1)
	error(_("invalid '%s' argument"), "replacement");
    if (length(rep) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "replacement");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = LENGTH(text);
    /* This contradicts the code below that has NA matching NA */
    if (STRING_ELT(pat, 0) == NA_STRING) {
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);
	UNPROTECT(1);
	return ans;
    }

    if (!useBytes) {
	Rboolean onlyASCII = strIsASCII(CHAR(STRING_ELT(pat, 0)));
	if (onlyASCII)
	    for (i = 0; i < n; i++)
		if (!strIsASCII(CHAR(STRING_ELT(text, i)))) {
		    onlyASCII = FALSE;
		    break;
		}
	useBytes = onlyASCII;
    }

    if (!useBytes) {
	if (!fixed_opt && mbcslocale) use_UTF8 = TRUE;
	else if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if (!use_UTF8)
	    for (i = 0; i < n; i++)
		if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    if (!fixed_opt && !perl_opt) { use_WC = use_UTF8; use_UTF8 = FALSE; }

    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	srep = CHAR(STRING_ELT(rep, 0));
    } else if (use_WC) ;
    else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	if (!utf8Valid(spat)) error(_("'pattern' is invalid UTF-8"));
	srep = translateCharUTF8(STRING_ELT(rep, 0));
	if (!utf8Valid(srep)) error(_("'replacement' is invalid UTF-8"));
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	if (mbcslocale && !mbcsValid(spat))
	    error(_("'pattern' is invalid in this locale"));
	srep = translateChar(STRING_ELT(rep, 0));
	if (mbcslocale && !mbcsValid(srep))
	    error(_("'replacement' is invalid in this locale"));
    }

    if (fixed_opt) {
	patlen = strlen(spat);
	if (!patlen) error(_("zero-length pattern"));
	replen = strlen(srep);
    } else if(perl_opt) {
	int cflags = 0, erroffset;
	const char *errorptr;
	if (use_UTF8) cflags |= PCRE_UTF8;
	if (igcase_opt) cflags |= PCRE_CASELESS;
	tables = pcre_maketables();
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if (errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	}
	if (n > 10) {
	    re_pe = pcre_study(re_pcre, 0, &errorptr);
	    if (errorptr)
		warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	}
	replen = strlen(srep);
    } else {
	int cflags = REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC) {
	    rc =  tre_regcompb(&reg, spat, cflags);
	    if (rc) reg_report(rc, &reg, spat);
	    replen = strlen(srep);
	} else {
	    rc  = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
	    if (rc) reg_report(rc, &reg, CHAR(STRING_ELT(pat, 0)));
	    wrep = wtransChar(STRING_ELT(rep, 0));
	    replen = wcslen(wrep);
	}
    }

    PROTECT(ans = allocVector(STRSXP, n));
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
	/* NA pattern was handled above */
	if (STRING_ELT(text,i) == NA_STRING) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	    continue;
	}

	if (useBytes)
	    s = CHAR(STRING_ELT(text, i));
	else if (use_WC) ;
	else if (use_UTF8) {
	    s = translateCharUTF8(STRING_ELT(text, i));
	    if (!utf8Valid(s)) error(("input string %d is invalid UTF-8"), i+1);
	} else {
	    s = translateChar(STRING_ELT(text, i));
	    if (mbcslocale && !mbcsValid(s))
		error(("input string %d is invalid in this locale"), i+1);
	}

	if (fixed_opt) {
	    int st, nr, slen = strlen(s);
	    ns = slen;
	    st = fgrep_one_bytes(spat, s, ns, useBytes, use_UTF8);
	    if (st < 0)
		SET_STRING_ELT(ans, i, STRING_ELT(text, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		if (global) { /* need to find max number of matches */
		    const char *ss= s;
		    int sst = st;
		    nr = 0;
		    do {
			nr++;
			ss += sst+patlen;
                        slen -= sst+patlen;
		    } while((sst = fgrep_one_bytes(spat, ss, slen, useBytes, use_UTF8)) >= 0);
		} else nr = 1;
		cbuf = u = Calloc(ns + nr*(replen - patlen) + 1, char);
		*u = '\0';
                slen = ns;
		do {
                    strncpy(u, s, st);
                    u += st;
                    s += st+patlen;
                    slen -= st+patlen;
		    strncpy(u, srep, replen);
                    u += replen;
		} while(global && (st = fgrep_one_bytes(spat, s, slen, useBytes, use_UTF8)) >= 0);
		strcpy(u, s);
		if (useBytes)
		    SET_STRING_ELT(ans, i, mkChar(cbuf));
		else if (use_UTF8)
		    SET_STRING_ELT(ans, i, mkCharCE(cbuf, CE_UTF8));
		else
		    SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
		Free(cbuf);
	    }
	} else if (perl_opt) {
	   int maxrep, ovector[30], eflag;
	   ns = strlen(s);
	   /* worst possible scenario is to put a copy of the
	      replacement after every character, unless there are
	      backrefs */
	   maxrep = replen + (ns-2) * count_subs(srep);
	   if (global) {
	       /* Integer overflow has been seen */
	       double dnns = ns * (maxrep + 1.) + 1000;
	       if (dnns > 10000) dnns = 2*ns + replen + 1000;
	       nns = dnns;
	   } else nns = ns + maxrep + 1000;
	   u = cbuf = Calloc(nns, char);
	   offset = 0; nmatch = 0; eflag = 0; last_end = -1;
	   while (pcre_exec(re_pcre, re_pe, s, ns, offset, eflag,
			    ovector, 30) >= 0) {
	       /* printf("%s, %d, %d %d\n", s, offset,
		  ovector[0], ovector[1]); */
	       nmatch++;
	       for (j = offset; j < ovector[0]; j++) *u++ = s[j];
	       if (ovector[1] > last_end) {
		   u = pcre_string_adj(u, s, srep, ovector, use_UTF8);
		   last_end = ovector[1];
	       }
	       offset = ovector[1];
	       if (s[offset] == '\0' || !global) break;
	       if (ovector[1] == ovector[0]) {
		   /* advance by a char */
		   if (use_UTF8) {
		       int used, pos = 0;
		       while( (used = utf8clen(s[pos])) ) {
			   pos += used;
			   if (pos > offset) {
			       for (j = offset; j < pos; j++) *u++ = s[j];
			       offset = pos;
			       break;
			   }
		       }
		   } else
		       *u++ = s[offset++];
	       }
	       if (nns < (u - cbuf) + (ns-offset) + maxrep + 100) {
		   char *tmp;
		   if (nns > INT_MAX/2) error(_("result string is too long"));
		   nns *= 2;
		   tmp = Realloc(cbuf, nns, char);
		   u = tmp + (u - cbuf);
		   cbuf = tmp;
	       }
	       eflag = PCRE_NOTBOL;  /* probably not needed */
	   }
	   if (nmatch == 0)
	       SET_STRING_ELT(ans, i, STRING_ELT(text, i));
	   else if (STRING_ELT(rep, 0) == NA_STRING)
	       SET_STRING_ELT(ans, i, NA_STRING);
	   else {
	       /* copy the tail */
	       if (nns < (u - cbuf) + (ns-offset)+1) {
		   char *tmp;
		   if (nns > INT_MAX/2) error(_("result string is too long"));
		   nns *= 2;
		   tmp = Realloc(cbuf, nns, char);
		   u = tmp + (u - cbuf);
		   cbuf = tmp;
	       }
	       for (j = offset ; s[j] ; j++) *u++ = s[j];
	       *u = '\0';
	       if (useBytes)
		   SET_STRING_ELT(ans, i, mkChar(cbuf));
	       else if (use_UTF8)
		   SET_STRING_ELT(ans, i, mkCharCE(cbuf, CE_UTF8));
	       else
		   SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
	   }	
	   Free(cbuf);
       } else if (!use_WC) {
	    int maxrep;
	    /* extended regexp in bytes */

	    ns = strlen(s);
	    /* worst possible scenario is to put a copy of the
	       replacement after every character, unless there are
	       backrefs */
	    maxrep = replen + (ns-2) * count_subs(srep);
	    if (global) {
		double dnns = ns * (maxrep + 1.) + 1000;
		if (dnns > 10000) dnns = 2*ns + replen + 1000;
		nns = dnns;
	    } else nns = ns + maxrep + 1000;
	    u = cbuf = Calloc(nns, char);
	    offset = 0; nmatch = 0; eflags = 0; last_end = -1;
	    while (tre_regexecb(&reg, s+offset, 10, regmatch, eflags) == 0) {
		/* printf("%s, %d %d\n", &s[offset],
		   regmatch[0].rm_so, regmatch[0].rm_eo); */
		nmatch++;
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = s[offset+j];
		if (offset+regmatch[0].rm_eo > last_end) {
		    u = string_adj(u, s+offset, srep, regmatch);
		    last_end = offset+regmatch[0].rm_eo;
		}
		offset += regmatch[0].rm_eo;
		if (s[offset] == '\0' || !global) break;
		if (regmatch[0].rm_eo == regmatch[0].rm_so)
		    *u++ = s[offset++];
		if (nns < (u - cbuf) + (ns-offset) + maxrep + 100) {
		    char *tmp;
		    if (nns > INT_MAX/2) error(_("result string is too long"));
		    nns *= 2;
		    tmp = Realloc(cbuf, nns, char);
		    u = tmp + (u - cbuf);
		    cbuf = tmp;
		}
		eflags = REG_NOTBOL;
	    }
	    if (nmatch == 0)
		SET_STRING_ELT(ans, i, STRING_ELT(text, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		/* copy the tail */
		if (nns < (u - cbuf) + (ns-offset)+1) {
		    char *tmp;
		    if (nns > INT_MAX/2) error(_("result string is too long"));
		    nns *= 2;
		    tmp = Realloc(cbuf, nns, char);
		    u = tmp + (u - cbuf);
		    cbuf = tmp;
		}
		for (j = offset ; s[j] ; j++) *u++ = s[j];
		*u = '\0';
		if (useBytes)
		    SET_STRING_ELT(ans, i, mkChar(cbuf));
		else
		    SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(text, i)));
	    }
	    Free(cbuf);
	} else  {
	    /* extended regexp in wchar_t */
	    const wchar_t *s = wtransChar(STRING_ELT(text, i));
	    wchar_t *u, *cbuf;
	    int maxrep;

	    ns = wcslen(s);
	    maxrep = replen + (ns-2) * wcount_subs(wrep);
	    if (global) {
		/* worst possible scenario is to put a copy of the
		   replacement after every character */
		double dnns = ns * (maxrep + 1.) + 1000;
		if (dnns > 10000) dnns = 2*ns + maxrep + 1000;
		nns = dnns;
	    } else nns = ns + maxrep + 1000;
	    u = cbuf = Calloc(nns, wchar_t);
	    offset = 0; nmatch = 0; eflags = 0; last_end = -1;
	    while (tre_regwexec(&reg, s+offset, 10, regmatch, eflags) == 0) {
		nmatch++;
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = s[offset+j];
		if (offset+regmatch[0].rm_eo > last_end) {
		    u = wstring_adj(u, s+offset, wrep, regmatch);
		    last_end = offset+regmatch[0].rm_eo;
		}
		offset += regmatch[0].rm_eo;
		if (s[offset] == L'\0' || !global) break;
		if (regmatch[0].rm_eo == regmatch[0].rm_so)
		    *u++ = s[offset++];
		if (nns < (u - cbuf) + (ns-offset) + maxrep + 100) {
		    wchar_t *tmp;
		    /* This could fail at smaller value on a 32-bit platform:
		       it is merely an integer overflow check */
		    if (nns > INT_MAX/2) error(_("result string is too long"));
		    nns *= 2;
		    tmp = Realloc(cbuf, nns, wchar_t);
		    u = tmp + (u - cbuf);
		    cbuf = tmp;
		}
		eflags = REG_NOTBOL;
	    }
	    if (nmatch == 0)
		SET_STRING_ELT(ans, i, STRING_ELT(text, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		/* copy the tail */
		if (nns < (u - cbuf) + (ns-offset)+1) {
		    wchar_t *tmp;
		    if (nns > INT_MAX/2) error(_("result string is too long"));
		    nns *= 2;
		    tmp = Realloc(cbuf, nns, wchar_t);
		    u = tmp + (u - cbuf);
		    cbuf = tmp;
		}
		for (j = offset ; s[j] ; j++) *u++ = s[j];
		*u = L'\0';
		SET_STRING_ELT(ans, i, mkCharW(cbuf));
	    }
	    Free(cbuf);
	}
	vmaxset(vmax);
    }

    if (fixed_opt) ; 
    else if(perl_opt) {
	if (re_pe) pcre_free(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
    } else tre_regfree(&reg);
    DUPLICATE_ATTRIB(ans, text);
    /* This copied the class, if any */
    UNPROTECT(1);
    return ans;
}

static int getNc(const char *s, int st)
{
    char *buf = alloca(st+1);
    R_CheckStack();
    memcpy(buf, s, st);
    buf[st] = '\0';
    return utf8towcs(NULL, buf, 0);
}



static SEXP 
gregexpr_Regexc(const regex_t *reg, SEXP sstr, int useBytes, int use_WC)
{
    int matchIndex, j, st, foundAll, foundAny, offset, len;
    regmatch_t regmatch[10];
    SEXP ans, matchlen;         /* Return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* Buffers for storing multiple matches */
    int bufsize = 1024;         /* Starting size for buffers */
    int eflags = 0;
    const char *string = NULL;
    const wchar_t *ws = NULL;

    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    matchIndex = -1;
    foundAll = foundAny = offset = 0;

    if (useBytes) {
	string = CHAR(sstr);
	len = strlen(string);
	use_WC = FALSE; /* to be sure */
    } else if (!use_WC) {
	string = translateChar(sstr);
	/* FIXME perhaps we ought to check validity here */
	len = strlen(string);
     } else {
	ws = wtransChar(sstr);
	len = wcslen(ws);
    }

    while (!foundAll) {
	if ( offset < len &&
	     (!use_WC ? tre_regexecb(reg, string+offset, 1, regmatch, eflags) :
	      tre_regwexec(reg, ws+offset, 1, regmatch, eflags))
	     == 0) {
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
	    INTEGER(matchbuf)[matchIndex] = offset + st + 1; /* index from one */
	    INTEGER(matchlenbuf)[matchIndex] = regmatch[0].rm_eo - st;
	    if (INTEGER(matchlenbuf)[matchIndex] == 0)
		offset += st + 1;
	    else
		offset += regmatch[0].rm_eo;
	} else {
	    foundAll = 1;
	    if (!foundAny) {
		matchIndex++;
		INTEGER(matchbuf)[matchIndex] = -1;
		INTEGER(matchlenbuf)[matchIndex] = -1;
	    }
	}
	eflags = REG_NOTBOL;
    }
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

static SEXP
gregexpr_fixed(const char *pattern, const char *string, 
	       Rboolean useBytes, Rboolean use_UTF8)
{
    int patlen, matchIndex, st, foundAll, foundAny, curpos, j, ansSize, nb=0;
    int slen;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    if (!useBytes && use_UTF8)
	patlen = utf8towcs(NULL, pattern, 0);
    else if (!useBytes && mbcslocale)
	patlen = mbstowcs(NULL, pattern, 0);
    else
	patlen = strlen(pattern);
    slen = strlen(string);
    foundAll = curpos = st = foundAny = 0;
    st = fgrep_one(pattern, string, useBytes, use_UTF8, &nb);
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
	    string += nb;
	    if (patlen == 0)
		curpos += st + 1;
	    else
		curpos += st + patlen;
	    if (curpos >= slen)
		break;
	    st = fgrep_one(pattern, string, useBytes, use_UTF8, &nb);
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
	    } else foundAll = 1;
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

static SEXP
gregexpr_perl(const char *pattern, const char *string,
	      pcre *re_pcre, pcre_extra *re_pe,
	      Rboolean useBytes, Rboolean use_UTF8)
{
    int matchIndex = -1, st = 0, foundAll = 0, foundAny = 0, j, start=0;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    while (!foundAll) {
	int rc, ovector[3], slen = strlen(string);
	rc = pcre_exec(re_pcre, re_pe, string, slen, start, 0, ovector, 3);
	if (rc >= 0) {
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
	    st = ovector[0];
	    INTEGER(matchbuf)[matchIndex] = st + 1; /* index from one */
	    INTEGER(matchlenbuf)[matchIndex] = ovector[1] - st;
	    /* we need to advance 'start' in bytes */
	    if (INTEGER(matchlenbuf)[matchIndex] == 0)
		start = ovector[0] + 1;
	    else
		start = ovector[1];
	    if (use_UTF8) {
		int mlen = ovector[1] - st;
		/* Unfortunately these are in bytes */
		if (st > 0) {
		    INTEGER(matchbuf)[matchIndex] = 1 + getNc(string, st);
		    if (INTEGER(matchbuf)[matchIndex] <= 0) { /* an invalid string */
			INTEGER(matchbuf)[matchIndex] = NA_INTEGER;
			foundAll = 1; /* if we get here, we are done */
		    }
		}
		INTEGER(matchlenbuf)[matchIndex] = getNc(string+st, mlen);
		if (INTEGER(matchlenbuf)[matchIndex] < 0) {/* an invalid string */
		    INTEGER(matchlenbuf)[matchIndex] = NA_INTEGER;
		    foundAll = 1;
		}
	    }
	    if (start >= slen) foundAll = 1;
	} else {
	    foundAll = 1;
	    if (!foundAny) matchIndex = 0;
	}
    }
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    if (foundAny) {
	/* copy from buffers */
	for (j = 0; j <= matchIndex; j++) {
	    INTEGER(ans)[j] = INTEGER(matchbuf)[j];
	    INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
	}
    } else INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
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

SEXP attribute_hidden do_regexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, rc, n, igcase_opt, perl_opt, fixed_opt, useBytes;
    const char *spat = NULL; /* -Wall */
    const char *s = NULL;
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    Rboolean use_UTF8 = FALSE, use_WC = FALSE;
    const void *vmax;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt) {
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
	perl_opt = 0;
    }

    /* Note that excluding NAs differs from grep/sub */
    if (!isString(pat) || length(pat) < 1 || STRING_ELT(pat, 0) == NA_STRING)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (!isString(text))
	error(_("invalid '%s' argument"), "text");

    n = LENGTH(text);
    if (!useBytes) {
	Rboolean onlyASCII = strIsASCII(CHAR(STRING_ELT(pat, 0)));
	if (onlyASCII)
	    for (i = 0; i < n; i++)
		if (!strIsASCII(CHAR(STRING_ELT(text, i)))) {
		    onlyASCII = FALSE;
		    break;
		}
	useBytes = onlyASCII;
    }


    if (!useBytes && !use_UTF8) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales,
	   and as from 2.11.0 in TRE too. */
	if (!fixed_opt && mbcslocale) use_UTF8 = TRUE;
	else if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if (!use_UTF8)
	    for (i = 0; i < n; i++)
		if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    if (!fixed_opt && !perl_opt) { use_WC = use_UTF8; use_UTF8 = FALSE; }

    if (useBytes)
	spat = CHAR(STRING_ELT(pat, 0));
    else if (use_WC) ;
    else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	if (!utf8Valid(spat)) error(_("regular expression is invalid UTF-8"));
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	if (mbcslocale && !mbcsValid(spat))
	    error(_("regular expression is invalid in this locale"));
    }

    if (fixed_opt) ; 
    else if (perl_opt) {
	int cflags = 0, erroffset;
	const char *errorptr;
	if (igcase_opt) cflags |= PCRE_CASELESS;
	if (!useBytes && use_UTF8) cflags |= PCRE_UTF8;
	tables = pcre_maketables();
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if (errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	}
	if (n > 10) {
	    re_pe = pcre_study(re_pcre, 0, &errorptr);
	    if (errorptr)
		warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	}
    } else {
	int cflags = REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
	if (!use_WC)
	    rc = tre_regcompb(&reg, spat, cflags);
	else
	    rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
	if (rc) reg_report(rc, &reg, spat);
    }

    if (PRIMVAL(op) == 0) { /* regexpr */
	SEXP matchlen;
	PROTECT(ans = allocVector(INTSXP, n));
	matchlen = allocVector(INTSXP, n); /* protected by next line */
	setAttrib(ans, install("match.length"), matchlen);

	vmax = vmaxget();
	for (i = 0 ; i < n ; i++) {
	    if (STRING_ELT(text, i) == NA_STRING) {
		INTEGER(matchlen)[i] = INTEGER(ans)[i] = NA_INTEGER;
	    } else {
		if (useBytes)
		    s = CHAR(STRING_ELT(text, i));
		else if (use_WC) ;
		else if (use_UTF8) {
		    s = translateCharUTF8(STRING_ELT(text, i));
		    if (!utf8Valid(s)) {
			warning(_("input string %d is invalid UTF-8"), i+1);
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			continue;
		    }
		} else {
		    s = translateChar(STRING_ELT(text, i));
		    if (mbcslocale && !mbcsValid(s)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
			continue;
		    }
		}
		if (fixed_opt) {
		    int st = fgrep_one(spat, s, useBytes, use_UTF8, NULL);
		    INTEGER(ans)[i] = (st > -1)?(st+1):-1;
		    if (!useBytes && use_UTF8) {
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    utf8towcs(NULL, spat, 0):-1;
		    } else if (!useBytes && mbcslocale) {
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    mbstowcs(NULL, spat, 0):-1;
		    } else
			INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			    strlen(spat):-1;
		} else if (perl_opt) {
		    int rc, ovector[3];
		    rc = pcre_exec(re_pcre, re_pe, s, strlen(s), 0, 0, ovector, 3);
		    if (rc >= 0) {
			int st = ovector[0];
			INTEGER(ans)[i] = st + 1; /* index from one */
			INTEGER(matchlen)[i] = ovector[1] - st;
			if (use_UTF8) {
			    int mlen = ovector[1] - st, nc;
			    /* Unfortunately these are in bytes */
			    if (st > 0) {
				nc = getNc(s, st); 
				INTEGER(ans)[i] = (nc >= 0) ? nc+1 : NA_INTEGER; 
			    }
			    nc = getNc(s + st, mlen);
			    INTEGER(matchlen)[i] = (nc >= 0) ? nc : NA_INTEGER;
			}
		    } else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
		} else {
		    if (!use_WC)
			rc = tre_regexecb(&reg, s, 1, regmatch, 0);
		    else
			rc = tre_regwexec(&reg, wtransChar(STRING_ELT(text, i)),
					  1, regmatch, 0);
		    if (rc == 0) {
			int st = regmatch[0].rm_so;
			INTEGER(ans)[i] = st + 1; /* index from one */
			INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
		    } else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
		}
	    }
	    vmaxset(vmax);
	}
    } else {
	SEXP elt;
	PROTECT(ans = allocVector(VECSXP, n));
	vmax = vmaxget();
	for (i = 0 ; i < n ; i++) {
	    if (STRING_ELT(text, i) == NA_STRING) {
		elt = gregexpr_NAInputAns();
	    } else {
		if (fixed_opt || perl_opt) {
		    if (useBytes)
			s = CHAR(STRING_ELT(text, i));
		    else if (use_UTF8) {
			s = translateCharUTF8(STRING_ELT(text, i));
		    } else
			s = translateChar(STRING_ELT(text, i));
		    if (!useBytes && !use_UTF8 && mbcslocale && !mbcsValid(s)) {
			warning(_("input string %d is invalid in this locale"), i+1);
			elt = gregexpr_BadStringAns();
		    } else {
			if (fixed_opt)
			    elt = gregexpr_fixed(spat, s, useBytes, use_UTF8);
			else
			    elt = gregexpr_perl(spat, s, re_pcre, re_pe, useBytes, use_UTF8);
		    }
		} else
		    elt = gregexpr_Regexc(&reg, STRING_ELT(text, i), useBytes, use_WC);
	    }
	    SET_VECTOR_ELT(ans, i, elt);
	    vmaxset(vmax);
	}
    }

    if (fixed_opt) ; 
    else if (perl_opt) {
	if (re_pe) pcre_free(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
    } else
	tre_regfree(&reg);

    UNPROTECT(1);
    return ans;
}
