/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>

#include "Defn.h"

#ifdef HAVE_PCRE_PCRE_H
# include <pcre/pcre.h>
#else
# include <pcre.h>
#endif

SEXP do_pgrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches;
    int igcase_opt, value_opt, options = 0, erroffset;
    const char *errorptr;
    pcre *re_pcre;
    const unsigned char *tables;
    
    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;

    if (!isString(pat) || length(pat) < 1 || !isString(vec))
	errorcall(call, R_MSG_IA);

    /* NAs are removed in R code so this isn't used */
    /* it's left in case we change our minds again */
    /* special case: NA pattern matches only NAs in vector */
    if (STRING_ELT(pat,0)==NA_STRING){
	n = length(vec);\
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

    if (igcase_opt) options |= PCRE_CASELESS;

    tables = pcre_maketables();
    re_pcre = pcre_compile(CHAR(STRING_ELT(pat, 0)), options, &errorptr, 
			   &erroffset, tables);
    if (!re_pcre) errorcall(call, "invalid regular expression");

    n = length(vec);
    ind = allocVector(LGLSXP, n);
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
	int rc, ovector;
	char *s = CHAR(STRING_ELT(vec, i));
	if (STRING_ELT(vec,i)==NA_STRING){
	    INTEGER(ind)[i]=0;
	    continue;
	}
	rc = pcre_exec(re_pcre, NULL, s, strlen(s), 0, 0, &ovector, 0);
	if (rc >= 0) {
	    INTEGER(ind)[i] = 1;
	    nmatches++;
	}
	else INTEGER(ind)[i] = 0;
    }
    (pcre_free)(re_pcre);
    pcre_free((void *)tables);
    PROTECT(ind);
    if (value_opt) {
	ans = allocVector(STRSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (INTEGER(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
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

/* The following R functions do substitution for regular expressions,
 * either once or globally.
 * The functions are loosely patterned on the "sub" and "gsub" in "nawk". */

static int length_adj(char *repl, int *ovec, int nsubexpr)
{
    int k, n;
    char *p = repl;
    n = strlen(repl) - (ovec[1] - ovec[0]);
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		if (k > nsubexpr)
		    error("invalid backreference in regular expression");
		n += (ovec[2*k+1] - ovec[2*k]) - 2;
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
			int *ovec, int nsubexpr)
{
    int i, k;
    char *p = repl, *t = target;
    while (*p) {
	if (*p == '\\') {
	    if ('1' <= p[1] && p[1] <= '9') {
		k = p[1] - '0';
		for (i = ovec[2*k] ; i < ovec[2*k+1] ; i++) *t++ = orig[i];
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


SEXP do_pgsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, vec, ans;
    int i, j, n, ns, nns, nmatch, offset, re_nsub;
    int global, igcase_opt, options = 0, erroffset;
    char *s, *t, *u, *uu;
    const char *errorptr;
    pcre *re_pcre;
    pcre_extra *re_pe;
    const unsigned char *tables;

    checkArity(op, args);

    global = PRIMVAL(op);

    pat = CAR(args); args = CDR(args);
    rep = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;

    if (!isString(pat) || length(pat) < 1 ||
	!isString(rep) || length(rep) < 1 ||
	!isString(vec))
	errorcall(call, R_MSG_IA);

    if (igcase_opt) options |= PCRE_CASELESS;

    tables = pcre_maketables();
    re_pcre = pcre_compile(CHAR(STRING_ELT(pat, 0)), options, &errorptr, 
			   &erroffset, tables);
    if (!re_pcre) errorcall(call, "invalid regular expression");
    re_nsub = pcre_info(re_pcre, NULL, NULL);
    re_pe = pcre_study(re_pcre, 0, &errorptr);

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
	int ovector[30];
	offset = 0;
	nmatch = 0;
	/* NA `pat' are removed in R code, the C code is left */
	/* in case we change our minds again */
	/* NA matches only itself */
        if (STRING_ELT(vec,i)==NA_STRING){
	    if (STRING_ELT(pat,0)==NA_STRING) 
		SET_STRING_ELT(ans, i, STRING_ELT(rep,0));
	    else
		SET_STRING_ELT(ans, i, NA_STRING);
	    continue;
	}
	if (STRING_ELT(pat, 0)==NA_STRING){
	    SET_STRING_ELT(ans, i, STRING_ELT(vec,i));
	    continue;
	}
	/* end NA handling */
	s = CHAR(STRING_ELT(vec, i));
	t = CHAR(STRING_ELT(rep, 0));
	nns = ns = strlen(s);
	while (pcre_exec(re_pcre, re_pe, s+offset, nns-offset, 0, 0, 
			 ovector, 30) >= 0) {
	    nmatch += 1;
	    if (ovector[0] == 0)
		offset++;
	    else {
		ns += length_adj(t, ovector, re_nsub);
		offset += ovector[1];
	    }
	    if (s[offset] == '\0' || !global)
		break;
	}
	if (nmatch == 0)
	    SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	else {
	    SET_STRING_ELT(ans, i, allocString(ns));
	    offset = 0;
	    s = CHAR(STRING_ELT(vec, i));
	    t = CHAR(STRING_ELT(rep, 0));
	    uu = u = CHAR(STRING_ELT(ans, i));
	    while (pcre_exec(re_pcre, re_pe, s+offset, nns-offset, 0, 0, 
			     ovector, 30) >= 0) {
		for (j = 0; j < ovector[0]; j++)
		    *u++ = s[offset+j];
		if (ovector[1] == 0) {
		    *u++ = s[offset];
		    offset++;
		} else {
		    u = string_adj(u, &s[offset], t, ovector, re_nsub);
		    offset += ovector[1];
		}
		if (s[offset] == '\0' || !global)
		    break;
	    }
	    for (j = offset ; s[j] ; j++)
		*u++ = s[j];
	    *u = '\0';
	}
    }
    (pcre_free)(re_pe);
    (pcre_free)(re_pcre);
    pcre_free((void *)tables);
    UNPROTECT(1);
    return ans;
}


SEXP do_pregexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchlen;
    int i, n, st, erroffset;
    const char *errorptr;
    pcre *re_pcre;
    const unsigned char *tables;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);

    if (!isString(pat) || length(pat) < 1 ||
	!isString(text) || length(text) < 1 )
	errorcall(call, R_MSG_IA);

    tables = pcre_maketables();
    re_pcre = pcre_compile(CHAR(STRING_ELT(pat, 0)), 0, &errorptr, 
			   &erroffset, tables);
    if (!re_pcre) errorcall(call, "invalid regular expression");
    n = length(text);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	int rc, ovector[3];
	char *s = CHAR(STRING_ELT(text, i));
	if (STRING_ELT(text,i)==NA_STRING){
	    INTEGER(ans)[i]=INTEGER(matchlen)[i]=R_NaInt;
	    continue;
	}
	rc = pcre_exec(re_pcre, NULL, s, strlen(s), 0, 0, ovector, 3);
	if (rc >= 0) {
	    st = ovector[0];
	    INTEGER(ans)[i] = st + 1; /* index from one */
	    INTEGER(matchlen)[i] = ovector[1] - st;
	} else {
	    INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	}
    }
    (pcre_free)(re_pcre);
    pcre_free((void *)tables);
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}
