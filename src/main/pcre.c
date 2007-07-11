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
   regex code should be OK.
   substitution code does ASCII comparisons only.
   regexpr returned pos and match length in bytes not chars.
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

#ifdef SUPPORT_UTF8
# include <R_ext/rlocale.h>
# include <wchar.h>
# include <wctype.h>
#endif

#include <R_ext/RS.h>           /* for CallocCharBuf and Free */


/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static int length_adj(const char *orig, const char *repl, int *ovec,
		      int nsubexpr, Rboolean useBytes)
{
    int k, n, nb;
    const char *p = repl;
    Rboolean upper = FALSE, lower = FALSE;

    n = strlen(repl) - (ovec[1] - ovec[0]);
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		if (k > nsubexpr)
		    error(_("invalid backreference %d in regular expression"), k);
		nb = ovec[2*k+1] - ovec[2*k];
#ifdef SUPPORT_UTF8
		if(nb >0 && !useBytes && mbcslocale && (upper || lower)) {
		    wctrans_t tr = wctrans(upper ? "toupper" : "tolower");
		    int j, nc;
		    char *xi, *p;
		    wchar_t *wc;
		    p = xi = (char *) alloca((nb+1)*sizeof(char));
		    R_CheckStack();
		    for(j = 0; j < nb; j++) *p++ = orig[ovec[2*k]+j];
		    *p = '\0';
		    nc = mbstowcs(NULL, xi, 0);
		    if(nc >= 0) {
			wc = (wchar_t *) alloca((nc+1)*sizeof(wchar_t));
			R_CheckStack();
			mbstowcs(wc, xi, nc + 1);
			for(j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = wcstombs(NULL, wc, 0);
		    }
		}
#endif
		n += nb - 2;
		p++;
	    } else if (p[1] == 'U') {
		p++; n -= 2;
		upper = TRUE; lower = FALSE;
	    } else if (p[1] == 'L') {
		p++; n -= 2;
		upper = FALSE; lower = TRUE;
	    } else if (p[1] == 0) {
				/* can't escape the final '\0' */
		n--;
	    } else {
		n--;
		p++;
	    }
	}
	p++;
    }
    return n;
}

static char *string_adj(char *target, const char *orig, const char *repl,
			int *ovec, Rboolean useBytes)
{
    int i, k, nb;
    const char *p = repl; char *t = target, c;
    Rboolean upper = FALSE, lower = FALSE;

    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		/* Here we need to work in chars */
		nb = ovec[2*k+1] - ovec[2*k];
#ifdef SUPPORT_UTF8
		if(nb > 0 && !useBytes && mbcslocale && (upper || lower)) {
		    wctrans_t tr = wctrans(upper ? "toupper" : "tolower");
		    int j, nc;
		    char *xi, *p;
		    wchar_t *wc;
		    p = xi = (char *) alloca((nb+1)*sizeof(char));
		    R_CheckStack();
		    for(j = 0; j < nb; j++) *p++ = orig[ovec[2*k]+j];
		    *p = '\0';
		    nc = mbstowcs(NULL, xi, 0);
		    if(nc >= 0) {
			wc = (wchar_t *) alloca((nc+1)*sizeof(wchar_t));
			R_CheckStack();
			mbstowcs(wc, xi, nc + 1);
			for(j = 0; j < nc; j++) wc[j] = towctrans(wc[j], tr);
			nb = wcstombs(NULL, wc, 0);
			wcstombs(xi, wc, nb + 1);
			for(j = 0; j < nb; j++) *t++ = *xi++;
		    }
		} else
#endif
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

SEXP attribute_hidden
do_pgsub(const char *spat, const char *srep, SEXP vec, int global,
	 int igcase_opt, int useBytes)
{
    SEXP ans;
    int i, j, n, ns, nns, nmatch, offset, re_nsub;
    int erroffset, eflag, last_end;
    int options = 0;
    const char *s, *t; char *u, *cbuf;
    const char *errorptr;
    pcre *re_pcre;
    pcre_extra *re_pe;
    const unsigned char *tables;

#ifdef SUPPORT_UTF8
    if(useBytes) ;
    else if(utf8locale) options = PCRE_UTF8;
    else if(mbcslocale)
	warning(_("perl = TRUE is only fully implemented in UTF-8 locales"));
    if(!useBytes && mbcslocale && !mbcsValid(spat))
	error(_("'pattern' is invalid in this locale"));
    if(!useBytes && mbcslocale && !mbcsValid(srep))
	error(_("'replacement' is invalid in this locale"));
#endif


    if (igcase_opt) {
	options |= PCRE_CASELESS;
	if(useBytes && utf8locale && !utf8strIsASCII(spat))
	    warning(_("ignore.case = TRUE, perl = TRUE in UTF-8 locales\n  only works caselessly for ASCII patterns"));
    }

    tables = pcre_maketables();
    re_pcre = pcre_compile(spat, options, &errorptr, &erroffset, tables);
    if (!re_pcre) error(_("invalid regular expression '%s'"), spat);
    re_nsub = pcre_info(re_pcre, NULL, NULL);
    re_pe = pcre_study(re_pcre, 0, &errorptr);

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
	int ovector[30];
	offset = 0;
	nmatch = 0;

	s = translateChar(STRING_ELT(vec, i));
	t = srep;
	nns = ns = strlen(s);

#ifdef SUPPORT_UTF8
	if(!useBytes && mbcslocale && !mbcsValid(s))
	    error(_("input string %d is invalid in this locale"), i+1);
#endif
	/* Looks like PCRE_NOTBOL is not needed in this version,
	   but leave in as a precaution */
	eflag = 0; last_end = -1;
	while (pcre_exec(re_pcre, re_pe, s, nns, offset, eflag,
			 ovector, 30) >= 0) {
	    nmatch += 1;
	    /* Do not repeat a 0-length match after a match, so
	       gsub("a*", "x", "baaac") is "xbxcx" not "xbxxcx" */
	    if(ovector[1] > last_end) {
		ns += length_adj(s, t, ovector, re_nsub, useBytes);
		last_end = ovector[1];
	    }
	    offset = ovector[1];
	    if (s[offset] == '\0' || !global) break;
	    /* If we have a 0-length match, move on a char */
	    if(ovector[1] == ovector[0]) {
#ifdef SUPPORT_UTF8
		if(!useBytes && mbcslocale) {
		    wchar_t wc; int used, pos = 0; mbstate_t mb_st;
		    mbs_init(&mb_st);
		    while( (used = Mbrtowc(&wc, s+pos, MB_CUR_MAX, &mb_st)) ) {
			pos += used;
			if(pos > offset) {
			    offset = pos;
			    break;
			}
		    }
		} else
#endif
		    offset++;
	    }
	    eflag = PCRE_NOTBOL;
	}
	if (nmatch == 0)
	    SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	else {
	    offset = 0;
	    s = translateChar(STRING_ELT(vec, i));
	    t = srep;
            cbuf = u = CallocCharBuf(ns);
	    eflag = 0; last_end = -1;
	    while (pcre_exec(re_pcre, re_pe, s, nns, offset, eflag,
			     ovector, 30) >= 0) {
		/* printf("%s, %d, %d %d\n", s, offset,
		   ovector[0], ovector[1]); */
		for (j = offset; j < ovector[0]; j++) *u++ = s[j];
		if(ovector[1] > last_end) {
		    u = string_adj(u, s, t, ovector, useBytes);
		    last_end = ovector[1];
		}
		offset = ovector[1];
		if (s[offset] == '\0' || !global) break;
		if(ovector[1] == ovector[0]) { 
		    /* advance by a char */
#ifdef SUPPORT_UTF8
		    if(!useBytes && mbcslocale) {
			wchar_t wc; int used, pos = 0; mbstate_t mb_st;
			mbs_init(&mb_st);
			while( (used = Mbrtowc(&wc, s+pos, MB_CUR_MAX, &mb_st)) ) {
			    pos += used;
			    if(pos > offset) {
				for(j = offset; j < pos; j++) *u++ = s[j]; 
				offset = pos;
				break;
			    }
			}
		    } else
#endif
			*u++ = s[offset++];
		}

		eflag = PCRE_NOTBOL;
	    }
	    for (j = offset ; s[j] ; j++)
		*u++ = s[j];
	    *u = '\0';
            SET_STRING_ELT(ans, i, mkChar(cbuf));
            Free(cbuf);
	}
	markKnown(STRING_ELT(ans, i), STRING_ELT(vec, i));
    }
    pcre_free(re_pe);
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    DUPLICATE_ATTRIB(ans, vec);
    UNPROTECT(1);
    return ans;
}


#include "RBufferUtils.h"

SEXP attribute_hidden
do_gpregexpr(const char *spat, SEXP text, int igcase_opt, int useBytes)
{
    SEXP ansList, ans, matchlen;
    SEXP matchbuf, matchlenbuf;
    int bufsize = 1024;
    int i, n, st, erroffset;
    int options = 0;
    const char *errorptr;
    pcre *re_pcre;
    const unsigned char *tables;

    /* To make this thread-safe remove static here use
       R_FreeStringBuffer below */
    static R_StringBuffer cbuff = {NULL, 0, MAXELTSIZE};

#ifdef SUPPORT_UTF8
    if(useBytes) ;
    else if(utf8locale) options = PCRE_UTF8;
    else if(mbcslocale)
	warning(_("perl = TRUE is only fully implemented in UTF-8 locales"));
#endif
    if (igcase_opt) {
	options |= PCRE_CASELESS;
	if(useBytes && utf8locale && !utf8strIsASCII(spat))
	    warning(_("ignore.case = TRUE, perl = TRUE in UTF-8 locales\n  only works caselessly for ASCII patterns"));
    }

#ifdef SUPPORT_UTF8
    if(!useBytes && mbcslocale && !mbcsValid(spat))
	error(_("regular expression is invalid in this locale"));
#endif
    tables = pcre_maketables();
    re_pcre = pcre_compile(spat, options, &errorptr, &erroffset, tables);
    if (!re_pcre) error(_("invalid regular expression '%s'"), spat);
    n = length(text);
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
	s = translateChar(STRING_ELT(text, i));
#ifdef SUPPORT_UTF8
	if(!useBytes && mbcslocale && !mbcsValid(s)) {
	    warning(_("input string %d is invalid in this locale"), i+1);
            PROTECT(ans = allocVector(INTSXP, 1)); 
            PROTECT(matchlen = allocVector(INTSXP, 1));
	    INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
            setAttrib(ans, install("match.length"), matchlen);
            SET_VECTOR_ELT(ansList, i, ans);
            UNPROTECT(2);
	    continue;
	}
#endif
        while (!foundAll) {
            int rc, ovector[3];
            rc = pcre_exec(re_pcre, NULL, s, strlen(s), start, 0, ovector, 3);
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
#ifdef SUPPORT_UTF8
                if(!useBytes && mbcslocale) {
                    int mlen = ovector[1] - st;
                    /* Unfortunately these are in bytes, so we need to
                       use chars instead */
                    R_AllocStringBuffer(imax2(st, mlen+1), &cbuff);
                    if(st > 0) {
                        memcpy(cbuff.data, s, st);
                        cbuff.data[st] = '\0';
                        INTEGER(matchbuf)[matchIndex] = 1 + mbstowcs(NULL, cbuff.data, 0);
                        if(INTEGER(matchbuf)[matchIndex] <= 0) { /* an invalid string */
                            INTEGER(matchbuf)[matchIndex] = NA_INTEGER;
                            foundAll = 1; /* if we get here, we are done */
                        }
                    }
                    memcpy(cbuff.data, s+st, mlen);
                    cbuff.data[mlen] = '\0';
                    INTEGER(matchlenbuf)[matchIndex] = mbstowcs(NULL, cbuff.data, 0);
                    if(INTEGER(matchlenbuf)[matchIndex] < 0) {/* an invalid string */
                        INTEGER(matchlenbuf)[matchIndex] = NA_INTEGER;
                        foundAll = 1; 
                    }
                }
#endif
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
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    UNPROTECT(3);
    return ansList;
}
