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

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <R_ext/RS.h>  /* for Calloc/Free */
#include <wchar.h>

#ifdef USE_TRE
# include <tre/regex.h>
typedef int apse_size_t;
# ifdef TRE_NO_REMAP 
#  define tre_regcomp regcomp
#  define tre_regerror regerror
#  define tre_regfree regfree
# endif
#else
# include "apse.h"
#endif

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches, nc;
    int igcase_opt, value_opt, max_distance_opt, useBytes;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    const char *str;
    Rboolean useMBCS = FALSE;
    wchar_t *wstr, *wpat = NULL;

#ifdef USE_TRE
    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_NOSUB | REG_LITERAL;
#else
    apse_t *aps;
#endif

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    igcase_opt = asLogical(CAR(args)); args = CDR(args);
    value_opt = asLogical(CAR(args)); args = CDR(args);
    max_distance_opt = (apse_size_t) asInteger(CAR(args));
    args = CDR(args);
    max_deletions_opt = (apse_size_t) asInteger(CAR(args));
    args = CDR(args);
    max_insertions_opt = (apse_size_t) asInteger(CAR(args));
    args = CDR(args);
    max_substitutions_opt = (apse_size_t) asInteger(CAR(args));
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

    /* Create search pattern object. */
    str = translateChar(STRING_ELT(pat, 0));
    if (mbcslocale) {
	useMBCS = !strIsASCII(str) && !useBytes;
	if (!useMBCS) {
	    for (i = 0 ; i < LENGTH(vec) ; i++) {
		if (STRING_ELT(vec, i) == NA_STRING) continue;
		if (!strIsASCII(CHAR(STRING_ELT(vec, i)))) {
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
#ifdef USE_TRE
	if ((rc = regwcomp(&reg, wpat, cflags))) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
#else
	aps = apse_create((unsigned char *) wpat, (apse_size_t) nc,
			  max_distance_opt, 65536);
	if (!aps)
	    error(_("could not allocate memory for approximate matching"));
#endif
    } else {
	nc = strlen(str);
#ifdef USE_TRE
	if ((rc = tre_regcomp(&reg, str, cflags))) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
#else
	aps = apse_create((unsigned char *) str, (apse_size_t) nc,
			  max_distance_opt, 256);
	if (!aps)
	    error(_("could not allocate memory for approximate matching"));
#endif
    }

#ifdef USE_TRE
    regaparams_default(&params);
    params.cost_ins = params.cost_del = params.cost_subst = 1;
    params.max_cost = max_distance_opt;
    params.max_del = max_deletions_opt;
    params.max_ins = max_insertions_opt;
    params.max_subst = max_substitutions_opt;
    params.max_err = max_distance_opt;
#else
    /* Set further restrictions on search distances. */
    apse_set_deletions(aps, max_deletions_opt);
    apse_set_insertions(aps, max_insertions_opt);
    apse_set_substitutions(aps, max_substitutions_opt);
#endif

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
#ifdef USE_TRE
	    /* undocumented, must be zeroed */
	    memset(&match, 0, sizeof(match));
	    if (regawexec(&reg, wstr, &match, params, 0) == REG_OK) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
#else
	    /* Set case ignore flag for the whole string to be matched. */
	    if (!apse_set_caseignore_slice(aps, 0, nc,
					  (apse_bool_t) igcase_opt))
		error(_("could not perform case insensitive matching"));
	    if (apse_match(aps, (unsigned char *) wstr, (apse_size_t) nc)) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
#endif
	    Free(wstr);
	} else {
#ifdef USE_TRE
	    /* undocumented, must be zeroed */
	    memset(&match, 0, sizeof(match));
	    if (regaexec(&reg, str, &match, params, 0) == REG_OK) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
#else
	    /* Set case ignore flag for the whole string to be matched. */
	    if (!apse_set_caseignore_slice(aps, 0, strlen(str),
					  (apse_bool_t) igcase_opt))
		error(_("could not perform case insensitive matching"));
	    if (apse_match(aps, (unsigned char *) str,
			  (apse_size_t) strlen(str))) {
		LOGICAL(ind)[i] = 1;
		nmatches++;
	    } else LOGICAL(ind)[i] = 0;
#endif
	}
    }
#ifdef USE_TRE
    tre_regfree(&reg);
#else
    apse_destroy(aps);
#endif

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
