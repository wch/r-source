/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka and the
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

#ifndef Macintosh
#include <sys/types.h>
#else 
#include <types.h>
#endif

#include "Defn.h"

#ifdef HAVE_PCRE
/* care needed as pcreposix defines various regex structures
   in a non-standard way, so keep this file separate */
#ifdef HAVE_PCRE_IN_PCRE
#include <pcre/pcreposix.h>
#else
#include <pcreposix.h>
#endif

SEXP do_pgrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    regex_t reg;
    int i, j, n, nmatches;
    int igcase_opt, value_opt, eflags = 0;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;

    if (!isString(pat) || length(pat) < 1 || !isString(vec))
	errorcall(call, R_MSG_IA);

    if (igcase_opt) eflags = eflags | REG_ICASE;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");

    n = length(vec);
    ind = allocVector(LGLSXP, n);
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
	if (regexec(&reg, CHAR(STRING_ELT(vec, i)), 0, NULL, 0) == 0) {
	    INTEGER(ind)[i] = 1;
	    nmatches++;
	}
	else INTEGER(ind)[i] = 0;
    }
    regfree(&reg);
    PROTECT(ind);
    if (value_opt) {
	ans = allocVector(STRSXP, nmatches);
	j = 0;
	for (i = 0 ; i < n ; i++)
	    if (INTEGER(ind)[i]) {
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
		/* FIXME: Want to inherit 'names(vec)': [the following is wrong]
		   TAG	 (ans)[j]   = TAG(vec)[i]; */
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
		    error("invalid backreference in regular expression");
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
			regmatch_t *regmatch, int nsubexpr)
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


SEXP do_pgsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, rep, vec, ans;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, j, n, ns, nmatch, offset;
    int global, igcase_opt, eflags = 0;
    char *s, *t, *u;

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

    if (igcase_opt) eflags = eflags | REG_ICASE;

    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), eflags))
	errorcall(call, "invalid regular expression");

    n = length(vec);
    PROTECT(ans = allocVector(STRSXP, n));

    for (i = 0 ; i < n ; i++) {
	offset = 0;
	nmatch = 0;
	s = CHAR(STRING_ELT(vec, i));
	t = CHAR(STRING_ELT(rep, 0));
	ns = strlen(s);
	while (regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
	    nmatch += 1;
	    if (regmatch[0].rm_eo == 0)
		offset++;
	    else {
		ns += length_adj(t, regmatch, reg.re_nsub);
		offset += regmatch[0].rm_eo;
	    }
	    if (s[offset] == '\0' || !global)
		break;
	}
	if (nmatch == 0)
	    SET_STRING_ELT(ans, i, STRING_ELT(vec, i));
	else {
	    SET_STRING_ELT(ans, i, allocString(ns));
	    offset = 0;
	    nmatch = 0;
	    s = CHAR(STRING_ELT(vec, i));
	    t = CHAR(STRING_ELT(rep, 0));
	    u = CHAR(STRING_ELT(ans, i));
	    ns = strlen(s);
	    while (regexec(&reg, &s[offset], 10, regmatch, 0) == 0) {
		for (j = 0; j < regmatch[0].rm_so ; j++)
		    *u++ = s[offset+j];
		if (regmatch[0].rm_eo == 0) {
		    *u++ = s[offset];
		    offset++;
		}
		else {
		    u = string_adj(u, &s[offset], t, regmatch,
				   reg.re_nsub);
		    offset += regmatch[0].rm_eo;
		}
		if (s[offset] == '\0' || !global)
		    break;
	    }
	    for (j = offset ; s[j] ; j++)
		*u++ = s[j];
	    *u = '\0';
	}
    }
    regfree(&reg);
    UNPROTECT(1);
    return ans;
}

SEXP do_pregexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, text, ans, matchlen;
    regex_t reg;
    regmatch_t regmatch[10];
    int i, n, st;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    text = CAR(args); args = CDR(args);

    if (!isString(pat) || length(pat) < 1 ||
	!isString(text) || length(text) < 1 )
	errorcall(call, R_MSG_IA);


    if (regcomp(&reg, CHAR(STRING_ELT(pat, 0)), 0))
	errorcall(call, "invalid regular expression");
    n = length(text);
    PROTECT(ans = allocVector(INTSXP, n));
    PROTECT(matchlen = allocVector(INTSXP, n));

    for (i = 0 ; i < n ; i++) {
	if(regexec(&reg, CHAR(STRING_ELT(text, i)), 1, regmatch, 0) == 0) {
	    st = regmatch[0].rm_so;
	    INTEGER(ans)[i] = st + 1; /* index from one */
	    INTEGER(matchlen)[i] = regmatch[0].rm_eo - st;
	} else {
	    INTEGER(ans)[i] = INTEGER(matchlen)[i] = -1;
	}
    }
    regfree(&reg);
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(2);
    return ans;
}
#else
SEXP do_pgrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("`perl = TRUE' is not support on this platform");
}
SEXP do_pgsub(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("`perl = TRUE' is not support on this platform");
}
SEXP do_pregexpr(SEXP call, SEXP op, SEXP args, SEXP env)
{
    error("`perl = TRUE' is not support on this platform");
}
#endif
