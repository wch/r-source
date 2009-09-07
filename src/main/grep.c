/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2009  Robert Gentleman, Ross Ihaka and the
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/*

Support for UTF-8-encoded strings in non-UTF-8 locales
======================================================

strsplit grep [g]sub [g]regexpr
  handle UTF-8 directly if fixed/perl = TRUE, otherwise translate.

agrep translates

*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <wchar.h>


/* The next must come after other header files to redefine RE_DUP_MAX */
#include "Rregex.h"

//#include <tre/regex.h>

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
   keep a standard-size (MAXELTSIZE = 8192) buffer allocated shared
   between the various functions.

   If we want to make this thread-safe, we would need to initialize an
   instance non-statically in each using function, but this would add
   to the overhead.
 */

#include "RBufferUtils.h"
static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};


/* strsplit is going to split the strings in the first argument into
 * tokens depending on the second argument. The characters of the second
 * argument are used to split the first argument.  A list of vectors is
 * returned of length equal to the input vector x, each element of the
 * list is the collection of splits for the corresponding element of x.
*/
SEXP attribute_hidden do_strsplit(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, t, tok, x;
    int i, j, len, tlen, ntok, slen, rc;
    int extended_opt, cflags, fixed_opt, perl_opt;
    char *pt = NULL;
    const char *buf, *split = "", *bufp, *laststart, *ebuf = NULL;
    regex_t reg;
    regmatch_t regmatch[1];
    pcre *re_pcre = NULL;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL;
    int options = 0;
    int erroffset, ovector[30];
    const char *errorptr;
    Rboolean usedRegex = FALSE, usedPCRE = FALSE, use_UTF8 = FALSE;

    checkArity(op, args);
    x = CAR(args);
    tok = CADR(args);
    extended_opt = asLogical(CADDR(args));
    fixed_opt = asLogical(CADDDR(args));
    perl_opt = asLogical(CAD4R(args));
    if (fixed_opt && perl_opt)
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
    if (fixed_opt && !extended_opt)
	warning(_("argument '%s' will be ignored"), "extended = FALSE");

    if (!isString(x) || !isString(tok)) error(_("non-character argument"));
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (perl_opt == NA_INTEGER) perl_opt = 0;

    if (!fixed_opt && perl_opt) {
	if (utf8locale) options = PCRE_UTF8;
	else if (mbcslocale) {
	    options = PCRE_UTF8;
	    use_UTF8 = TRUE;
	}
    }

    cflags = 0;
    if (extended_opt) cflags = cflags | REG_EXTENDED;

    len = LENGTH(x);
    tlen = LENGTH(tok);
    /* special case split="" for efficiency */
    if (tlen == 1 && CHAR(STRING_ELT(tok, 0))[0] == 0) tlen = 0;

    if (fixed_opt || perl_opt) {
	for (i = 0; i < tlen; i++)
	if (getCharCE(STRING_ELT(tok, i)) == CE_UTF8) use_UTF8 = TRUE;
	for (i = 0; i < len; i++)
	    if (getCharCE(STRING_ELT(x, i)) == CE_UTF8) use_UTF8 = TRUE;
    }
    if (use_UTF8 && !fixed_opt && perl_opt) options = PCRE_UTF8;

    PROTECT(s = allocVector(VECSXP, len));
    for (i = 0; i < len; i++) {
	if (STRING_ELT(x, i) == NA_STRING) {
	    SET_VECTOR_ELT(s, i, ScalarString(NA_STRING));
	    continue;
	}
	if (use_UTF8)
	    buf = translateCharUTF8(STRING_ELT(x, i));
	else
	    buf = translateChar(STRING_ELT(x, i));

	if (tlen > 0) {
	    /* NA token doesn't split */
	    if (STRING_ELT(tok, i % tlen) == NA_STRING) {
		    SET_VECTOR_ELT(s, i, ScalarString(markKnown(buf, STRING_ELT(x, i))));
		    continue;
	    }
	    /* find out how many splits there will be */
	    if (use_UTF8)
		split = translateCharUTF8(STRING_ELT(tok, i % tlen));
	    else
		split = translateChar(STRING_ELT(tok, i % tlen));
	    slen = strlen(split);
	    ntok = 0;
	    if (fixed_opt) {
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
	    } else if (perl_opt) {
		usedPCRE = TRUE;
		tables = pcre_maketables();
		re_pcre = pcre_compile(split, options,
				       &errorptr, &erroffset, tables);
		if (!re_pcre) {
		    if(errorptr)
			warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
				errorptr, split+erroffset);
		    error(_("invalid split pattern '%s'"), split);
		}
		re_pe = pcre_study(re_pcre, 0, &errorptr);
		if(errorptr)
		    warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
		bufp = buf;
		if (*bufp != '\0') {
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
		if ((rc = regcomp(&reg, split, cflags))) {
		    char errbuf[1001];
		    regerror(rc, &reg, errbuf, 1001);
		    warning(_("regcomp error:  '%s'"), errbuf);
		    error(_("invalid split pattern '%s'"), split);
		}
		bufp = buf;
		if (*bufp != '\0') {
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
	    if (*bufp == '\0')
		PROTECT(t = allocVector(STRSXP, ntok));
	    else
		PROTECT(t = allocVector(STRSXP, ntok + 1));
	    /* and fill with the splits */
	    laststart = bufp = buf;
	    pt = (char *) realloc(pt, (strlen(buf)+1) * sizeof(char));
	    for (j = 0; j < ntok; j++) {
		if (fixed_opt) {
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
		} else if (perl_opt) {
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
		} else {
		    regexec(&reg, bufp, 1, regmatch, 0);
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
	    }
	    if (*bufp != '\0')
		SET_STRING_ELT(t, ntok, markKnown(bufp, STRING_ELT(x, i)));
	} else {
	    /* split into individual characters (not bytes) */
	    if ((use_UTF8 || mbcslocale) && !strIsASCII(buf)) {
		char bf[20 /* > MB_CUR_MAX */];
		const char *p = buf;
		int used;
		mbstate_t mb_st;

		ntok = mbstowcs(NULL, buf, 0);
		if (ntok < 0) {
		    PROTECT(t = ScalarString(NA_STRING));
		} else if (use_UTF8) {
		    PROTECT(t = allocVector(STRSXP, ntok));
		    for (j = 0; j < ntok; j++, p += used) {
			used = utf8clen(*p);
			memcpy(bf, p, used); bf[used] = '\0';
			SET_STRING_ELT(t, j, mkCharCE(bf, CE_UTF8));
		    }
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
		char bf[2];
		ntok = strlen(buf);
		PROTECT(t = allocVector(STRSXP, ntok));
		bf[1] = '\0';
		for (j = 0; j < ntok; j++) {
		    bf[0] = buf[j];
		    SET_STRING_ELT(t, j, markKnown(bf, STRING_ELT(x, i)));
		}
	    }
	}
	UNPROTECT(1);
	SET_VECTOR_ELT(s, i, t);
	if (usedRegex) {
	    regfree(&reg);
	    usedRegex = FALSE;
	}
	if (usedPCRE) {
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


/* This could be faster for plen > 1, but uses in R are for small strings */
static int fgrep_one(const char *pat, const char *target,
		     int useBytes, int ienc, int *next)
{
    int i = -1, plen=strlen(pat), len=strlen(target);
    const char *p;

    if (plen == 0) {
	if (next != NULL) *next = 1;
	return 0;
    }
    if (plen == 1) {
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
    } else
	if(!useBytes && ienc == CE_UTF8) {
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

static int fgrep_one_bytes(const char *pat, const char *target, int useBytes)
{
    int i = -1, plen=strlen(pat), len=strlen(target);
    const char *p;

    if (plen == 0) return 0;
    if (plen == 1) {
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
    } else
	for (i = 0; i <= len-plen; i++)
	    if (strncmp(pat, target+i, plen) == 0) return i;
    return -1;
}

/* This should be using UTF-8 when the strings concerned are
   UTF-8, but we can only do that for perl and fixed.  */
SEXP attribute_hidden do_grep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    regex_t reg;
    int i, j, n, nmatches = 0, cflags = 0, ov, erroffset, ienc, rc;
    int igcase_opt, extended_opt, value_opt, perl_opt, fixed_opt, useBytes, invert;
    const char *cpat, *errorptr;
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    Rboolean use_UTF8 = FALSE;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    invert = asLogical(CAR(args));
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (invert == NA_INTEGER) invert = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt)
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
    if ((fixed_opt || perl_opt) && !extended_opt)
	warning(_("argument '%s' will be ignored"), "extended = FALSE");
    if (!(fixed_opt || perl_opt) && useBytes) {
	warning(_("argument '%s' will be ignored"), "useBytes = TRUE");
	useBytes = 0;
    }

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    n = length(vec);
    if (STRING_ELT(pat, 0) == NA_STRING) {
	if (value_opt) {
	    SEXP nmold = getAttrib(vec, R_NamesSymbol);
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

    if ((fixed_opt || perl_opt) && !useBytes) {
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	for (i = 0; i < n; i++)
	    if (getCharCE(STRING_ELT(vec, i)) == CE_UTF8) use_UTF8 = TRUE;
    }
    if (useBytes) {
	cpat = CHAR(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;	
    } else if (use_UTF8) {
	cpat = translateCharUTF8(STRING_ELT(pat, 0));
	ienc = CE_UTF8;
    } else {
	cpat = translateChar(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;
    }
    if (perl_opt) {
	if (igcase_opt) {
	    cflags |= PCRE_CASELESS;
	    if (useBytes && utf8locale && !strIsASCII(cpat))
		warning(_("ignore.case = TRUE, perl = TRUE, useBytes = TRUE\n  in UTF-8 locales only works caselessly for ASCII patterns"));
	}
	if (useBytes) ;
	else {
	    if (mbcslocale && !utf8locale) use_UTF8 = TRUE;
	    if (utf8locale || use_UTF8) cflags |= PCRE_UTF8;
	}
    } else {
	if (extended_opt) cflags |= REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
    }

    if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(cpat))
	error(_("regular expression is invalid in this locale"));

    if (fixed_opt) ;
    else if (perl_opt) {
	tables = pcre_maketables();
	re_pcre = pcre_compile(cpat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if(errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, cpat+erroffset);
	    error(_("invalid regular expression '%s'"), cpat);
	    if(n > 10) {
		re_pe = pcre_study(re_pcre, 0, &errorptr);
		if(errorptr)
		    warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	    }
	}
    } else if ((rc = regcomp(&reg, cpat, cflags))) {
	char errbuf[1001];
	regerror(rc, &reg, errbuf, 1001);
	warning(_("regcomp error:  '%s'"), errbuf);
	error(_("invalid regular expression '%s'"), cpat);
    }

    PROTECT(ind = allocVector(LGLSXP, n));
    for (i = 0 ; i < n ; i++) {
	LOGICAL(ind)[i] = 0;
	if (STRING_ELT(vec, i) != NA_STRING) {
	    const char *s;
	    if (useBytes)
		s = CHAR(STRING_ELT(vec, i));
	    else if (use_UTF8)
		s = translateCharUTF8(STRING_ELT(vec, i));
	    else {
		s = translateChar(STRING_ELT(vec, i));
		if (!useBytes && mbcslocale && !mbcsValid(s)) {
		    warning(_("input string %d is invalid in this locale"), i+1);
		    continue;
		}
	    }

	    if (fixed_opt)
		LOGICAL(ind)[i] = fgrep_one(cpat, s, useBytes, ienc, NULL) >= 0;
	    else if (perl_opt) {
		if (pcre_exec(re_pcre, re_pe, s, strlen(s), 0, 0, &ov, 0) >= 0)
		    INTEGER(ind)[i] = 1;
	    } else if (regexec(&reg, s, 0, NULL, 0) == 0) LOGICAL(ind)[i] = 1;
	}
	if (invert ^ LOGICAL(ind)[i]) nmatches++;
    }
    if (fixed_opt);
    else if (perl_opt) {
	if(re_pe) pcre_free(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
    } else
	regfree(&reg);

    if (PRIMVAL(op)) {/* grepl case */
	UNPROTECT(1);
	return ind;
    }	

    if (value_opt) {
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
	PROTECT(ans = allocVector(STRSXP, nmatches));
	for (i = 0, j = 0; i < n ; i++)
	    if (invert ^ LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
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

static int length_adj(const char *repl, regmatch_t *regmatch, int nsubexpr)
{
    int k, n;
    const char *p = repl;
    mbstate_t mb_st;
    mbs_init(&mb_st);

    n = strlen(repl) - (regmatch[0].rm_eo - regmatch[0].rm_so);
    while (*p) {
	if (mbcslocale) { /* not a problem in UTF-8 */
	    /* skip over multibyte chars, since they could have
	       an embedded \ */
	    int clen;
	    if ((clen = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) > 1) {
		p += clen;
		continue;
	    }
	}
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

static char *string_adj(char *target, const char *orig, const char *repl,
			regmatch_t *regmatch)
{
    int i, k;
    const char *p = repl; char *t = target;
    mbstate_t mb_st;
    mbs_init(&mb_st);

    while (*p) {
	if (mbcslocale) { /* not a problem in UTF-8 */
	    /* skip over multibyte chars, since they could have
	       an embedded \ */
	    int clen;
	    if ((clen = Mbrtowc(NULL, p, MB_CUR_MAX, &mb_st)) > 1) {
		for (i = 0; i < clen; i++) *t++ = *p++;
		continue;
	    }
	}
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

/* From pcre.c */
extern SEXP do_pgsub(SEXP pat, SEXP rep, SEXP vec,
		     int global, int igcase_opt, int useBytes);

SEXP attribute_hidden do_gsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, vec, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, j, n, ns, nmatch, offset, rc;
    int global, igcase_opt, extended_opt, perl_opt, fixed_opt, useBytes,
	cflags = 0, eflags, last_end;
    char *u, *cbuf;
    const char *spat, *srep, *s, *t;
    int patlen = 0, replen = 0, st, nr;
    Rboolean use_UTF8 = FALSE;

    checkArity(op, args);

    global = PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt)
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
    if ((fixed_opt || perl_opt) && !extended_opt)
	warning(_("argument '%s' will be ignored"), "extended = FALSE");
    if (!(fixed_opt || perl_opt) && useBytes) {
	warning(_("argument '%s' will be ignored"), "useBytes = TRUE");
	useBytes = 0;
    }

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");
    if (!isString(rep) || length(rep) < 1)
	error(_("invalid '%s' argument"), "replacement");
    if (length(rep) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "replacement");

    n = LENGTH(vec);
    if (STRING_ELT(pat, 0) == NA_STRING) {
	PROTECT(ans = allocVector(STRSXP, n));
	for (i = 0; i < n; i++)  SET_STRING_ELT(ans, i, NA_STRING);
	UNPROTECT(1);
	return ans;
    }

    if (perl_opt && !fixed_opt)
	return do_pgsub(pat, rep, vec, global, igcase_opt, useBytes);

    if (fixed_opt) { /* we don't have UTF-8 version of regex.c */
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if (getCharCE(STRING_ELT(rep, 0)) == CE_UTF8) use_UTF8 = TRUE;
	for (i = 0; i < n; i++)
	    if (getCharCE(STRING_ELT(vec, i)) == CE_UTF8) use_UTF8 = TRUE;
    }

    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	srep = CHAR(STRING_ELT(rep, 0));	
    } else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	srep = translateCharUTF8(STRING_ELT(rep, 0));
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	srep = translateChar(STRING_ELT(rep, 0));
    }

    if (mbcslocale && !mbcsValid(spat))
	error(_("'pattern' is invalid in this locale"));
    if (mbcslocale && !mbcsValid(srep))
	error(_("'replacement' is invalid in this locale"));

    if (extended_opt) cflags |= REG_EXTENDED;
    if (igcase_opt) cflags |= REG_ICASE;
    if (!fixed_opt && (rc = regcomp(&reg, spat, cflags))) {
	char errbuf[1001];
	regerror(rc, &reg, errbuf, 1001);
	warning(_("regcomp error:  '%s'"), errbuf);
	error(_("invalid regular expression '%s'"), spat);
    }
    if (fixed_opt) {
	patlen = strlen(spat);
	if (!patlen)
	    error(_("zero-length pattern"));
	replen = strlen(srep);
    }

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
	if (useBytes)  /* this and next only used for fixed = TRUE */
	    s = CHAR(STRING_ELT(vec, i));
	else if (use_UTF8)
	    s = translateCharUTF8(STRING_ELT(vec, i));
	else
	    s = translateChar(STRING_ELT(vec, i));
	t = srep;
	ns = strlen(s);

	if (mbcslocale && !mbcsValid(s))
	    error(("input string %d is invalid in this locale"), i+1);

	if (fixed_opt) {
	    st = fgrep_one_bytes(spat, s, useBytes);
	    if (st < 0)
		SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	    else if (STRING_ELT(rep, 0) == NA_STRING)
		SET_STRING_ELT(ans, i, NA_STRING);
	    else {
		if (global) { /* need to find number of matches */
		    const char *ss= s;
		    int sst = st;
		    nr = 0;
		    do {
			nr++;
			ss += sst+patlen;
		    } while((sst = fgrep_one_bytes(spat, ss, useBytes)) >= 0);
		} else nr = 1;
		cbuf = u = CallocCharBuf(ns + nr*(replen - patlen));
		*u = '\0';
		do {
		    nr = strlen(u);
		    strncat(u, s, st); u[nr+st] = '\0'; s += st+patlen;
		    strcat(u, t);
		} while(global && (st = fgrep_one_bytes(spat, s, useBytes)) >= 0);
		strcat(u, s);
		if (useBytes)
		    SET_STRING_ELT(ans, i, mkChar(cbuf));
		else if (use_UTF8)
		    SET_STRING_ELT(ans, i, mkCharCE(cbuf, CE_UTF8));
		else
		    SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(vec, i)));
		Free(cbuf);
	    }
	} else {
	    /* regular regexp, no useBytes nor use_UTF8 */

	    eflags = 0; last_end = -1;
	    /* We need to use private version of regexec here, as
	       head-chopping the string does not work with e.g. \b.
	     */
	    while (regexec(&reg, s+offset, 10, regmatch, eflags) == 0) {
		nmatch += 1;
		offset += regmatch[0].rm_eo;
		/* Do not repeat a 0-length match after a match, so
		   gsub("a*", "x", "baaac") is "xbxcx" not "xbxxcx" */
		if (offset > last_end) {
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
		offset = 0;
		nmatch = 0;
		s = translateChar(STRING_ELT(vec, i));
		t = srep;
		cbuf = u = CallocCharBuf(ns);
		ns = strlen(s);
		eflags = 0; last_end = -1;
		while (regexec(&reg, s+offset, 10, regmatch, eflags) == 0) {
		    /* printf("%s, %d %d\n", &s[offset],
		       regmatch[0].rm_so, regmatch[0].rm_eo); */
		    for (j = 0; j < regmatch[0].rm_so ; j++)
			*u++ = s[offset+j];
		    if(offset+regmatch[0].rm_eo > last_end) {
			u = string_adj(u, s+offset, t, regmatch);
			last_end = offset+regmatch[0].rm_eo;
		    }
		    offset += regmatch[0].rm_eo;
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
		SET_STRING_ELT(ans, i, markKnown(cbuf, STRING_ELT(vec, i)));
		Free(cbuf);
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
    int i, n, st, igcase_opt, extended_opt, perl_opt, fixed_opt, useBytes,
	cflags = 0, erroffset, ienc;
    int rc, ovector[3];
    const char *spat = NULL; /* -Wall */
    const char *s, *errorptr;
    pcre *re_pcre = NULL /* -Wall */;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables = NULL /* -Wall */;
    Rboolean use_UTF8 = FALSE;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt)
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
    if ((fixed_opt || perl_opt) && !extended_opt)
	warning(_("argument '%s' will be ignored"), "extended = FALSE");
    if (!(fixed_opt || perl_opt) && useBytes) {
	warning(_("argument '%s' will be ignored"), "useBytes = TRUE");
	useBytes = 0;
    }

    /* allow 'text' to be zero-length from 2.3.1 */
    if (!isString(pat) || length(pat) < 1 || STRING_ELT(pat,0) == NA_STRING)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    n = LENGTH(text);
    if ((fixed_opt || perl_opt) && !useBytes) {
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	for (i = 0; i < n; i++)
	    if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) use_UTF8 = TRUE;
    }
    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;
    } else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	ienc = CE_UTF8;
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;
    }
    if (perl_opt) {
	if (useBytes) ;
	else {
	    if (mbcslocale && !utf8locale) use_UTF8 = TRUE;
	    if (utf8locale || use_UTF8) cflags |= PCRE_UTF8;
	}
	if (igcase_opt) {
	    cflags |= PCRE_CASELESS;
	    if (useBytes && utf8locale && !strIsASCII(spat))
		warning(_("ignore.case = TRUE, perl = TRUE, useBytes = TRUE\n  in UTF-8 locales only works caselessly for ASCII patterns"));
	}
    } else {
	if (extended_opt) cflags |= REG_EXTENDED;
	if (igcase_opt) cflags |= REG_ICASE;
    }

    if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(spat))
	error(_("regular expression is invalid in this locale"));
    if (fixed_opt) ;
    else if (perl_opt) {
	tables = pcre_maketables();
	re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
	if (!re_pcre) {
	    if(errorptr)
		warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
			errorptr, spat+erroffset);
	    error(_("invalid regular expression '%s'"), spat);
	}
	if(n > 10) {
	    re_pe = pcre_study(re_pcre, 0, &errorptr);
	    if(errorptr)
		warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
	}
    } else
	if ((rc = regcomp(&reg, spat, cflags))) {
	    char errbuf[1001];
	    regerror(rc, &reg, errbuf, 1001);
	    warning(_("regcomp error:  '%s'"), errbuf);
	    error(_("invalid regular expression '%s'"), spat);
	}

    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(text, i) == NA_STRING) {
	    INTEGER(matchlen)[i] = INTEGER(ans)[i] = NA_INTEGER;
	} else {
	    if (useBytes)
		s = CHAR(STRING_ELT(text, i));
	    else if (ienc == CE_UTF8)
		s = translateCharUTF8(STRING_ELT(text, i));
	    else
		s = translateChar(STRING_ELT(text, i));
	    if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(s)) {
		warning(_("input string %d is invalid in this locale"), i+1);
		INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
		continue;
	    }
	    if (fixed_opt) {
		st = fgrep_one(spat, s, useBytes, ienc, NULL);
		INTEGER(ans)[i] = (st > -1)?(st+1):-1;
		if (!useBytes && ienc == CE_UTF8) {
		    INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			utf8towcs(NULL, spat, 0):-1;
		} else if (!useBytes && mbcslocale) {
		    INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			mbstowcs(NULL, spat, 0):-1;
		} else
		    INTEGER(matchlen)[i] = INTEGER(ans)[i] >= 0 ?
			strlen(spat):-1;
	    } else if (perl_opt) {
		rc = pcre_exec(re_pcre, re_pe, s, strlen(s), 0, 0, ovector, 3);
		if (rc >= 0) {
		    st = ovector[0];
		    INTEGER(ans)[i] = st + 1; /* index from one */
		    INTEGER(matchlen)[i] = ovector[1] - st;
		    if (!useBytes && ienc == CE_UTF8) {
			char *buf;
			int mlen = ovector[1] - st;
			/* Unfortunately these are in bytes, so we need to
			   use chars instead */
			if (st > 0) {
			    buf = R_AllocStringBuffer(st, &cbuff);
			    memcpy(buf, s, st);
			    buf[st] = '\0';
			    INTEGER(ans)[i] = 1+utf8towcs(NULL, buf, 0);
			    if (INTEGER(ans)[i] <= 0) /* an invalid string */
				INTEGER(ans)[i] = NA_INTEGER;
			}
			buf = R_AllocStringBuffer(mlen+1, &cbuff);
			memcpy(buf, s+st, mlen);
			buf[mlen] = '\0';
			INTEGER(matchlen)[i] = utf8towcs(NULL, buf, 0);
			if (INTEGER(matchlen)[i] < 0) /* an invalid string */
			    INTEGER(matchlen)[i] = NA_INTEGER;
		    } else if (!useBytes && mbcslocale) {
			char *buf;
			int mlen = ovector[1] - st;
			/* Unfortunately these are in bytes, so we need to
			   use chars instead */
			if (st > 0) {
			    buf = R_AllocStringBuffer(st, &cbuff);
			    memcpy(buf, s, st);
			    buf[st] = '\0';
			    INTEGER(ans)[i] = 1+mbstowcs(NULL, buf, 0);
			    if (INTEGER(ans)[i] <= 0) /* an invalid string */
				INTEGER(ans)[i] = NA_INTEGER;
			}
			buf = R_AllocStringBuffer(mlen+1, &cbuff);
			memcpy(buf, s+st, mlen);
			buf[mlen] = '\0';
			INTEGER(matchlen)[i] = mbstowcs(NULL, buf, 0);
			if (INTEGER(matchlen)[i] < 0) /* an invalid string */
			    INTEGER(matchlen)[i] = NA_INTEGER;
		    }
		} else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	    } else {
		if (regexec(&reg, s, 1, regmatch, 0) == 0) {
		    st = regmatch[0].rm_so;
		    INTEGER(ans)[i] = st + 1; /* index from one */
		    INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
		    /* we don't support useBytes here */
		    if (mbcslocale) {
			char *buf;
			int mlen = regmatch[0].rm_eo - st;
			/* Unfortunately these are in bytes, so we need to
			   use chars instead */
			if (st > 0) {
			    buf = R_AllocStringBuffer(st, &cbuff);
			    memcpy(buf, s, st);
			    buf[st] = '\0';
			    INTEGER(ans)[i] = 1+mbstowcs(NULL, buf, 0);
			    if (INTEGER(ans)[i] <= 0) /* an invalid string */
				INTEGER(ans)[i] = NA_INTEGER;
			}
			buf = R_AllocStringBuffer(mlen+1, &cbuff);
			memcpy(buf, s+st, mlen);
			buf[mlen] = '\0';
			INTEGER(matchlen)[i] = mbstowcs(NULL, buf, 0);
			if (INTEGER(matchlen)[i] < 0) /* an invalid string */
			    INTEGER(matchlen)[i] = NA_INTEGER;
		    }
		} else INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	    }
	}
    }
    R_FreeStringBufferL(&cbuff);
    if (fixed_opt) ;
    else if (perl_opt) {
	if(re_pe) pcre_free(re_pe);
	pcre_free(re_pcre);
	pcre_free((void *)tables);
    } else
	regfree(&reg);
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
	if ( offset < len &&
	     regexec(reg, string+offset, 1, regmatch, 0) == 0) {
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
	    if (!useBytes && mbcslocale) {
		char *buf;
		int mlen = regmatch[0].rm_eo - st;
		/* Unfortunately these are in bytes, so we need to
		   use chars instead */
		if (st > 0) {
		    buf = R_AllocStringBuffer(st, &cbuff);
		    memcpy(buf, string, st);
		    buf[st] = '\0';
		    INTEGER(matchbuf)[matchIndex] = 1+mbstowcs(NULL, buf, 0);
		    if (INTEGER(matchbuf)[matchIndex] <= 0) { /* an invalid string */
			INTEGER(matchbuf)[matchIndex] = NA_INTEGER;
			foundAll = 1;
		    }
		}
		buf = R_AllocStringBuffer(mlen+1, &cbuff);
		memcpy(buf, string+st, mlen);
		buf[mlen] = '\0';
		INTEGER(matchlenbuf)[matchIndex] = mbstowcs(NULL, buf, 0);
		if (INTEGER(matchlenbuf)[matchIndex] < 0) { /* an invalid string */
		    INTEGER(matchlenbuf)[matchIndex] = NA_INTEGER;
		    foundAll = 1;
		}
	    }
	} else {
	    foundAll = 1;
	    if (!foundAny) {
		matchIndex++;
		INTEGER(matchbuf)[matchIndex] = -1;
		INTEGER(matchlenbuf)[matchIndex] = -1;
	    }
	}

    }
    R_FreeStringBufferL(&cbuff);
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
gregexpr_fixed(const char *pattern, const char *string, int useBytes, int ienc)
{
    int patlen, matchIndex, st, foundAll, foundAny, curpos, j, ansSize, nb=0;
    int slen;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    if (!useBytes && ienc == CE_UTF8)
	patlen = utf8towcs(NULL, pattern, 0);
    else if (!useBytes && mbcslocale)
	patlen = mbstowcs(NULL, pattern, 0);
    else
	patlen = strlen(pattern);
    slen = strlen(string);
    foundAll = curpos = st = foundAny = 0;
    st = fgrep_one(pattern, string, useBytes, ienc, &nb);
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
	    st = fgrep_one(pattern, string, useBytes, ienc, &nb);
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

/* in pcre.c */
extern SEXP
do_gpregexpr(SEXP pat, SEXP vec, int igcase_opt, int useBytes);

SEXP attribute_hidden do_gregexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ansList, ans;
    regex_t reg;
    int i, n, igcase_opt, extended_opt, perl_opt, fixed_opt, useBytes,
	cflags = 0, rc, ienc;
    const char *spat, *s;
    Rboolean use_UTF8 = FALSE;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    extended_opt = asLogical(CAR(args)); args = CDR(args);
    perl_opt = asLogical(CAR(args)); args = CDR(args);
    fixed_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (extended_opt == NA_INTEGER) extended_opt = 1;
    if (perl_opt == NA_INTEGER) perl_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 0;
    if (fixed_opt && igcase_opt)
	warning(_("argument '%s' will be ignored"), "ignore.case = TRUE");
    if (fixed_opt && perl_opt)
	warning(_("argument '%s' will be ignored"), "perl = TRUE");
    if ((fixed_opt || perl_opt) && !extended_opt)
	warning(_("argument '%s' will be ignored"), "extended = FALSE");
    if (!(fixed_opt || perl_opt) && useBytes) {
	warning(_("argument '%s' will be ignored"), "useBytes = TRUE");
	useBytes = 0;
    }

    if (!isString(text) || length(text) < 1)
	error(_("invalid '%s' argument"), "text");
    if (!isString(pat) || length(pat) < 1 || STRING_ELT(pat,0) == NA_STRING)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");

    if (perl_opt && !fixed_opt)
	return do_gpregexpr(pat, text, igcase_opt, useBytes);


    n = LENGTH(text);
    if (fixed_opt && !useBytes) {
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	for (i = 0; i < n; i++)
	    if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) use_UTF8 = TRUE;
    }
    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;	
    } else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	ienc = CE_UTF8;
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	ienc = CE_NATIVE;
    }

    if (extended_opt) cflags |= REG_EXTENDED;
    if (igcase_opt) cflags |= REG_ICASE;

    if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(spat))
	error(_("regular expression is invalid in this locale"));

    if (!fixed_opt && (rc = regcomp(&reg, spat, cflags))) {
	error(_("invalid regular expression '%s'"), spat);
    }

    PROTECT(ansList = allocVector(VECSXP, n));
    for (i = 0 ; i < n ; i++) {
	int foundAll, foundAny, matchIndex, offset;
	foundAll = foundAny = offset = 0;
	matchIndex = -1;
	if (STRING_ELT(text, i) == NA_STRING) {
	    PROTECT(ans = gregexpr_NAInputAns());
	} else {
	    if (useBytes)
		s = CHAR(STRING_ELT(text, i));
	    else if (use_UTF8)
		s = translateCharUTF8(STRING_ELT(text, i));
	    else
		s = translateChar(STRING_ELT(text, i));
	    if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(s)) {
		warning(_("input string %d is invalid in this locale"), i+1);
		PROTECT(ans = gregexpr_BadStringAns());
	    } else {
		if (fixed_opt)
		    PROTECT(ans = gregexpr_fixed(spat, s, useBytes, CE_NATIVE));
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

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches, nc;
    int igcase_opt, value_opt, max_distance_opt, useBytes;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    apse_t *aps;
    const char *str;
    Rboolean useMBCS = FALSE;
    wchar_t *wstr, *wpat = NULL;

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
    args = CDR(args);
    useBytes = asLogical(CAR(args));

    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pat");
    if (!isString(vec)) error(_("invalid '%s' argument"), "x");

    /* Create search pattern object. */
    str = translateChar(STRING_ELT(pat, 0));
    if (mbcslocale) {
	useMBCS = !strIsASCII(str) && !useBytes;
	if (!useMBCS) {
	    for (i = 0 ; i < LENGTH(vec) ; i++) {
		if (STRING_ELT(vec, i) == NA_STRING) continue;
		if (!strIsASCII(translateChar(STRING_ELT(vec, i)))) {
		    useMBCS = !useBytes;
		    break;
		}
	    }
	}
    }
    if (useMBCS) {
	nc = mbstowcs(NULL, str, 0);
	wpat = Calloc(nc+1, wchar_t);
	mbstowcs(wpat, str, nc+1);
	aps = apse_create((unsigned char *) wpat, (apse_size_t) nc,
			  max_distance_opt, 65536);
    } else {
	nc = strlen(str);
	aps = apse_create((unsigned char *) str, (apse_size_t) nc,
			  max_distance_opt, 256);
    }
    if (!aps)
	error(_("could not allocate memory for approximate matching"));

    /* Set further restrictions on search distances. */
    apse_set_deletions(aps, max_deletions_opt);
    apse_set_insertions(aps, max_insertions_opt);
    apse_set_substitutions(aps, max_substitutions_opt);

    /* Matching. */
    n = LENGTH(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(vec, i) == NA_STRING) {
	    LOGICAL(ind)[i] = 0;
	    continue;
	}
	str = translateChar(STRING_ELT(vec, i));
	/* Perform match. */
	if (useMBCS) {
	    nc = mbstowcs(NULL, str, 0);
	    wstr = Calloc(nc+1, wchar_t);
	    mbstowcs(wstr, str, nc+1);
	    /* Set case ignore flag for the whole string to be matched. */
	    if (!apse_set_caseignore_slice(aps, 0, nc,
					  (apse_bool_t) igcase_opt))
		error(_("could not perform case insensitive matching"));
	    if (apse_match(aps, (unsigned char *) wstr, (apse_size_t) nc)) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	    Free(wstr);
	} else {
	    /* Set case ignore flag for the whole string to be matched. */
	    if (!apse_set_caseignore_slice(aps, 0, strlen(str),
					  (apse_bool_t) igcase_opt))
		error(_("could not perform case insensitive matching"));
	    if (apse_match(aps, (unsigned char *) str,
			  (apse_size_t) strlen(str))) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	}
    }
    apse_destroy(aps);

    PROTECT(ans = value_opt
	    ? allocVector(STRSXP, nmatches)
	    : allocVector(INTSXP, nmatches));
    if (value_opt) {
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
	for (j = i = 0 ; i < n ; i++) {
	    if (LOGICAL(ind)[i])
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
	for (j = i = 0 ; i < n ; i++) {
	    if (LOGICAL(ind)[i]==1)
		INTEGER(ans)[j++] = i + 1;
	}
    }

    if (wpat) Free(wpat);
    UNPROTECT(2);
    return ans;
}

