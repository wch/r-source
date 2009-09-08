/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002--2009  The R Development Core Team
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

/* This needs to be separate from grep.c, as TRE has a conflicting
   regcomp and the two headers cannot both be included in one file */

/* FIXME: this could handle marked UTF-8 encodings */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#undef USE_TRE
#define USE_TRE 1

#include <Defn.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <wchar.h>

# include <tre/regex.h>

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches, nc;
    int igcase_opt, value_opt, max_distance_opt, useBytes;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    const char *str;
    Rboolean useWC = FALSE;
    wchar_t *wstr, *wpat = NULL;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_NOSUB | REG_LITERAL;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    max_distance_opt = asInteger(CAR(args));
    args = CDR(args);
    max_deletions_opt = asInteger(CAR(args));
    args = CDR(args);
    max_insertions_opt = asInteger(CAR(args));
    args = CDR(args);
    max_substitutions_opt = asInteger(CAR(args));
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

#ifdef USE_TRE
    if (igcase_opt) cflags |= REG_ICASE;
#endif

    str = CHAR(STRING_ELT(pat, 0));
    useWC = !strIsASCII(str) && !useBytes;
    if (!useWC) {
	for (i = 0 ; i < LENGTH(vec) ; i++) {
	    if (STRING_ELT(vec, i) == NA_STRING) continue;
	    if (!strIsASCII(CHAR(STRING_ELT(vec, i)))) {
		useWC = !useBytes;
		break;
	    }
	}
    }
    if (useWC) {
	nc = mbstowcs(NULL, str, 0);
	wpat = Calloc(nc+1, wchar_t);
	mbstowcs(wpat, str, nc+1);
	if ((rc = regwcomp(&reg, wpat, cflags))) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
    } else {
	str = translateChar(STRING_ELT(pat, 0));
	nc = strlen(str);
	if ((rc = tre_regcomp(&reg, str, cflags))) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
    }

    regaparams_default(&params);
    params.max_cost = max_distance_opt;
    params.max_del = max_deletions_opt;
    params.max_ins = max_insertions_opt;
    params.max_subst = max_substitutions_opt;
    params.max_err = max_distance_opt;

    /* Matching. */
    n = LENGTH(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(vec, i) == NA_STRING) {
	    LOGICAL(ind)[i] = 0;
	    continue;
	}
	/* Perform match. */
	/* undocumented, must be zeroed */
	memset(&match, 0, sizeof(match));
	if (useWC) {
	    str = translateChar(STRING_ELT(vec, i));
	    nc = mbstowcs(NULL, str, 0);
	    wstr = Calloc(nc+1, wchar_t);
	    mbstowcs(wstr, str, nc+1);
	    if (regawexec(&reg, wstr, &match, params, 0) == REG_OK) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	    Free(wstr);
	} else {
	    str = translateChar(STRING_ELT(vec, i));
	    if (regaexec(&reg, str, &match, params, 0) == REG_OK) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
	}
    }
    tre_regfree(&reg);

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
    } else {
	for (j = i = 0 ; i < n ; i++)
	    if (LOGICAL(ind)[i] == 1)
		INTEGER(ans)[j++] = i + 1;
    }

    if (wpat) Free(wpat);
    UNPROTECT(2);
    return ans;
}
