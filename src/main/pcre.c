/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <sys/types.h>		/* probably not needed */
#define imax2(x, y) ((x < y) ? y : x)

#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

#include <R_ext/RS.h>           /* for Calloc and Free */
#include <wchar.h>
#include <wctype.h>

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

/* FIXME: use UCP for upper/lower conversion */
static char *string_adj(char *target, const char *orig, const char *repl,
			int *ovec, int ienc)
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
		if (nb > 0 && ienc == CE_UTF8 && (upper || lower)) {
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

/* safe as the only MBCS used in UTF-8 */
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


SEXP attribute_hidden
do_pgsub(SEXP pat, SEXP rep, SEXP text, int global,
	 int igcase_opt, int useBytes)
{
    SEXP ans;
    int i, j, n, ns, nns, nmatch, offset, ienc;
    int erroffset, eflag, last_end, replen, maxrep;
    int cflags = 0;
    const char *spat, *srep, *s;
    char *u, *cbuf, *tmp;
    const char *errorptr;
    pcre *re_pcre;
    pcre_extra *re_pe  = NULL;
    const unsigned char *tables;
    Rboolean use_UTF8 = FALSE;

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
    if (!useBytes) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales */
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if(!use_UTF8)
	    for (i = 0; i < n; i++)
		if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    if (igcase_opt) cflags |= PCRE_CASELESS;
    if (!useBytes) {
	if (mbcslocale) use_UTF8 = TRUE;
	if (use_UTF8) cflags |= PCRE_UTF8;
    }

    if (useBytes) {
	spat = CHAR(STRING_ELT(pat, 0));
	srep = CHAR(STRING_ELT(rep, 0));
	ienc = CE_NATIVE;
    } else if (use_UTF8) {
	spat = translateCharUTF8(STRING_ELT(pat, 0));
	srep = translateCharUTF8(STRING_ELT(rep, 0));
	ienc = CE_UTF8;
    } else {
	spat = translateChar(STRING_ELT(pat, 0));
	srep = translateChar(STRING_ELT(rep, 0));
	ienc = CE_NATIVE;
    }
    replen = strlen(srep);

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
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
	int ovector[30];

	if (useBytes)
	    s = CHAR(STRING_ELT(text, i));
	else if (use_UTF8)
	    s = translateCharUTF8(STRING_ELT(text, i));
	else
	    s = translateChar(STRING_ELT(text, i));

	if (!useBytes && mbcslocale && !mbcsValid(s))
	    error(_("input string %d is invalid in this locale"), i+1);

	/* Looks like PCRE_NOTBOL is not needed in this version,
	   but leave in as a precaution */

	if (useBytes)
	    s = CHAR(STRING_ELT(text, i));
	else if (use_UTF8)
	    s = translateCharUTF8(STRING_ELT(text, i));
	else
	    s = translateChar(STRING_ELT(text, i));
	ns = strlen(s);
	/* worst possible scenario is to put a copy of the
	   replacement after every character, unless there are
	   backrefs */
	maxrep = replen + (ns-2) * count_subs(srep);
	if (global) {
	    nns = ns * (maxrep + 1) + 1000;
	    if (nns > 10000) nns = 2*ns + replen + 1000;
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
		u = string_adj(u, s, srep, ovector, ienc);
		last_end = ovector[1];
	    }
	    offset = ovector[1];
	    if (s[offset] == '\0' || !global) break;
	    if (ovector[1] == ovector[0]) {
		/* advance by a char */
		if (ienc == CE_UTF8) {
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
		nns *= 2;
		tmp = Realloc(cbuf, nns, char);
		u = tmp + (u - cbuf);
		cbuf = tmp;
	    }
	    eflag = PCRE_NOTBOL;
	}
	if (nmatch == 0)
	    SET_STRING_ELT(ans, i, STRING_ELT(text, i));
	else if (STRING_ELT(rep, 0) == NA_STRING)
	    SET_STRING_ELT(ans, i, NA_STRING);
	else {
	    /* copy the tail */
	    if (nns < (u - cbuf) + (ns-offset)+1) {
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
    }
    if (re_pe) pcre_free(re_pe);
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    DUPLICATE_ATTRIB(ans, text);
    UNPROTECT(1);
    return ans;
}


#include "RBufferUtils.h"

SEXP attribute_hidden
do_gpregexpr(SEXP pat, SEXP text, int igcase_opt, int useBytes)
{
    SEXP ansList, ans, matchlen;
    SEXP matchbuf, matchlenbuf;
    int bufsize = 1024;
    int i, n, st, erroffset, ienc;
    int cflags = 0;
    const char *spat, *errorptr;
    pcre *re_pcre;
    pcre_extra *re_pe = NULL;
    const unsigned char *tables;
    Rboolean use_UTF8 = FALSE;

    /* To make this thread-safe remove static here use
       R_FreeStringBuffer below */
    static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

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
    if (!useBytes && !mbcslocale) {
	/* As from R 2.10.0 we use UTF-8 mode in PCRE in all MBCS locales */
	if (getCharCE(STRING_ELT(pat, 0)) == CE_UTF8) use_UTF8 = TRUE;
	if(!use_UTF8)
	    for (i = 0; i < n; i++)
		if (getCharCE(STRING_ELT(text, i)) == CE_UTF8) {
		    use_UTF8 = TRUE;
		    break;
		}
    }

    if (igcase_opt) cflags |= PCRE_CASELESS;
    if (!useBytes) {
	if (mbcslocale) use_UTF8 = TRUE;
	if (use_UTF8) cflags |= PCRE_UTF8;
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

    if (!useBytes && mbcslocale && !mbcsValid(spat))
	error(_("regular expression is invalid in this locale"));

    tables = pcre_maketables();
    re_pcre = pcre_compile(spat, cflags, &errorptr, &erroffset, tables);
    if (!re_pcre) {
	if (errorptr)
	    warning(_("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n"),
		    errorptr, spat+erroffset);
	error(_("invalid regular expression '%s'"), spat);
    }
    n = LENGTH(text);
    if(n > 10) {
	re_pe = pcre_study(re_pcre, 0, &errorptr);
	if (errorptr)
	    warning(_("PCRE pattern study error\n\t'%s'\n"), errorptr);
    }
    PROTECT(ansList = allocVector(VECSXP, n));
    matchbuf = PROTECT(allocVector(INTSXP, bufsize));
    matchlenbuf = PROTECT(allocVector(INTSXP, bufsize));

    for (i = 0 ; i < n ; i++) {
	const char *s;
	int j, foundAll, foundAny, matchIndex, start;
	foundAll = foundAny = start = 0;
	matchIndex = -1;
	if (STRING_ELT(text,i) == NA_STRING){
	    PROTECT(ans = allocVector(INTSXP, 1));
	    PROTECT(matchlen = allocVector(INTSXP, 1));
	    INTEGER(ans)[0] = INTEGER(matchlen)[0] = R_NaInt;
	    setAttrib(ans, install("match.length"), matchlen);
	    SET_VECTOR_ELT(ansList, i, ans);
	    UNPROTECT(2);
	    continue;
	}
	if (useBytes)
	    s = CHAR(STRING_ELT(text, i));
	else if (ienc == CE_UTF8)
	    s = translateCharUTF8(STRING_ELT(text, i));
	else
	    s = translateChar(STRING_ELT(text, i));
	if (!useBytes && ienc != CE_UTF8 && mbcslocale && !mbcsValid(s)) {
	    warning(_("input string %d is invalid in this locale"), i+1);
	    PROTECT(ans = allocVector(INTSXP, 1));
	    PROTECT(matchlen = allocVector(INTSXP, 1));
	    INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
	    setAttrib(ans, install("match.length"), matchlen);
	    SET_VECTOR_ELT(ansList, i, ans);
	    UNPROTECT(2);
	    continue;
	}
	while (!foundAll) {
	    int rc, ovector[3], slen = strlen(s);
	    rc = pcre_exec(re_pcre, re_pe, s, slen, start, 0, ovector, 3);
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
		    /* Unfortunately these are in bytes, so we need to
		       use chars instead */
		    R_AllocStringBuffer(imax2(st, mlen+1), &cbuff);
		    if (st > 0) {
			memcpy(cbuff.data, s, st);
			cbuff.data[st] = '\0';
			INTEGER(matchbuf)[matchIndex] = 1 + utf8towcs(NULL, cbuff.data, 0);
			if (INTEGER(matchbuf)[matchIndex] <= 0) { /* an invalid string */
			    INTEGER(matchbuf)[matchIndex] = NA_INTEGER;
			    foundAll = 1; /* if we get here, we are done */
			}
		    }
		    memcpy(cbuff.data, s+st, mlen);
		    cbuff.data[mlen] = '\0';
		    INTEGER(matchlenbuf)[matchIndex] = utf8towcs(NULL, cbuff.data, 0);
		    if (INTEGER(matchlenbuf)[matchIndex] < 0) {/* an invalid string */
			INTEGER(matchlenbuf)[matchIndex] = NA_INTEGER;
			foundAll = 1;
		    }
		}
		if (start >= slen)
		    foundAll = 1;
	    } else {
		foundAll = 1;
		if (!foundAny)
		    matchIndex = 0;
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
	} else {
	    INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
	}
	setAttrib(ans, install("match.length"), matchlen);
	SET_VECTOR_ELT(ansList, i, ans);
	UNPROTECT(2);
    }
    /* see comment above */
    R_FreeStringBufferL(&cbuff);
    if(re_pe) pcre_free(re_pe);
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    UNPROTECT(3);
    return ansList;
}
