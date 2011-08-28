/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002--2011  The R Development Core Team
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

/* This at times needed to be separate from grep.c, as TRE has a
   conflicting regcomp and the two headers cannot both be included in
   one file 
*/

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <wchar.h>
#include <tre/tre.h>

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    int i, j, n, nmatches;
    int igcase_opt, value_opt, max_distance_opt, useBytes;
    int max_deletions_opt, max_insertions_opt, max_substitutions_opt;
    int fixed_opt;
    Rboolean useWC = FALSE;
    const void *vmax = NULL;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_NOSUB;

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
    args = CDR(args);
    fixed_opt = asLogical(CAR(args));

    if (igcase_opt == NA_INTEGER) igcase_opt = 0;
    if (value_opt == NA_INTEGER) value_opt = 0;
    if (useBytes == NA_INTEGER) useBytes = 0;
    if (fixed_opt == NA_INTEGER) fixed_opt = 1;

    if(fixed_opt) cflags |= REG_LITERAL;

    if (!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if (length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pat");
    if (!isString(vec)) error(_("invalid '%s' argument"), "x");

    if (igcase_opt) cflags |= REG_ICASE;

    n = LENGTH(vec);
    if (!useBytes) {
	Rboolean haveBytes = IS_BYTES(STRING_ELT(pat, 0));
	if (!haveBytes)
	    for (i = 0; i < n; i++)
		if (IS_BYTES(STRING_ELT(vec, i))) {
		    haveBytes = TRUE;
		    break;
		}
	if (haveBytes) {
	    warning(_("string marked as \"bytes\" found, so using useBytes = TRUE"));
	    useBytes = TRUE;
	}
    }
    if (!useBytes) {
	useWC = !strIsASCII(CHAR(STRING_ELT(pat, 0)));
	if (!useWC) {
	    for (i = 0 ; i < n ; i++) {
		if (STRING_ELT(vec, i) == NA_STRING) continue;
		if (!strIsASCII(CHAR(STRING_ELT(vec, i)))) {
		    useWC = TRUE;
		    break;
		}
	    }
	}
    }
    if (useBytes)
	rc = tre_regcompb(&reg, CHAR(STRING_ELT(pat, 0)), cflags);
    else if (useWC)
	rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
    else {
	const char *spat = translateChar(STRING_ELT(pat, 0));
	if (mbcslocale && !mbcsValid(spat))
	    error(_("regular expression is invalid in this locale"));
	rc = tre_regcomp(&reg, spat, cflags);
    }
    if (rc) {
	char errbuf[1001];
	tre_regerror(rc, &reg, errbuf, 1001);
	error(_("regcomp error:  '%s'"), errbuf);
    }

    tre_regaparams_default(&params);
    params.max_cost = max_distance_opt;
    params.max_del = max_deletions_opt;
    params.max_ins = max_insertions_opt;
    params.max_subst = max_substitutions_opt;
    params.max_err = max_distance_opt;

    /* Matching. */
    n = LENGTH(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    /* wtransChar and translateChar can R_alloc */
    vmax = vmaxget();
    for (i = 0 ; i < n ; i++) {
	if (STRING_ELT(vec, i) == NA_STRING) {
	    LOGICAL(ind)[i] = 0;
	    continue;
	}
	/* Perform match. */
	/* undocumented, must be zeroed */
	memset(&match, 0, sizeof(match));
	if (useBytes)
	    rc = tre_regaexecb(&reg,
			       CHAR(STRING_ELT(vec, i)),
			       &match, params, 0);
	else if (useWC) {
	    rc = tre_regawexec(&reg,
			       wtransChar(STRING_ELT(vec, i)), 
			       &match, params, 0);
	    vmaxset(vmax);
	} else {
	    const char *s = translateChar(STRING_ELT(vec, i));
	    if (mbcslocale && !mbcsValid(s))
		error(_("input string %d is invalid in this locale"), i+1);
	    rc = tre_regaexec(&reg, s, &match, params, 0);
	    vmaxset(vmax);
	}
	if (rc == REG_OK) {
	    LOGICAL(ind)[i] = 1;
	    nmatches++;
	} else LOGICAL(ind)[i] = 0;
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

    UNPROTECT(2);
    return ans;
}

#define ANS(I, J)          REAL(ans)[I + J * nx]
#define COUNTS(I, J, K)	   REAL(counts)[I + J * nx + K * nxy]

SEXP attribute_hidden do_adist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pattern, x, y, ans, dim, dimnames, counts, names;
    int opt_cost_ins, opt_cost_del, opt_cost_sub,
	opt_partial, opt_counts, opt_icase, useBytes;
    int i, j, k, nx, ny, nxy;
    const char *s, *t;
    size_t delta;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_EXTENDED | REG_NOSUB;
    
    checkArity(op, args);
    pattern = CAR(args); args = CDR(args);
    x = CAR(args); args = CDR(args);
    y = CAR(args); args = CDR(args);
    opt_cost_ins = asInteger(CAR(args)); args = CDR(args);
    opt_cost_del = asInteger(CAR(args)); args = CDR(args);
    opt_cost_sub = asInteger(CAR(args)); args = CDR(args);
    opt_counts = asLogical(CAR(args)); args = CDR(args);
    opt_partial = asInteger(CAR(args)); args = CDR(args);
    opt_icase = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args));

    if(opt_counts == NA_INTEGER) opt_counts = 0;
    if(opt_partial == NA_INTEGER) opt_partial = 0;
    if(opt_icase == NA_INTEGER) opt_icase = 0;
    if(useBytes == NA_INTEGER) useBytes = 0;

    if(opt_partial) cflags |= REG_LITERAL;
    if(opt_icase) cflags |= REG_ICASE;

    counts = R_NilValue;	/* -Wall */

    if(!isString(x))
	error(_("invalid '%s' argument"), "x");
    nx = length(x);
    if(!isString(y))
	error(_("invalid '%s' argument"), "y");
    ny = length(y);
    nxy = nx * ny;

    PROTECT(ans = allocMatrix(REALSXP, nx, ny));
    if(opt_counts) {
	PROTECT(dim = allocVector(INTSXP, 3));
	INTEGER(dim)[0] = nx;
	INTEGER(dim)[1] = ny;
	INTEGER(dim)[2] = 3;
	PROTECT(counts = allocArray(REALSXP, dim));
    }

    tre_regaparams_default(&params);
    params.max_cost = INT_MAX;
    params.cost_ins = opt_cost_ins;
    params.cost_del = opt_cost_del;
    params.cost_subst = opt_cost_sub;

    /* Handle encoding stuff etc lateron. */
    for(i = 0; i < nx; i++) {
	if(STRING_ELT(pattern, i) == NA_STRING) {
	    for(j = 0; j < ny; j++) {
		if(opt_counts) {
		    for(k = 0; k < 3; k++) {
			COUNTS(i, j, k) = NA_REAL;
		    }
		} else {
		    ANS(i, j) = NA_REAL;
		}
	    }
	    continue;
	}
	rc = tre_regcomp(&reg, CHAR(STRING_ELT(pattern, i)), cflags);
	if(rc) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
	for(j = 0; j < ny; j++) {
	    if(STRING_ELT(y, j) == NA_STRING) {
		if(opt_counts) {
		    for(k = 0; k < 3; k++) {
			COUNTS(i, j, k) = NA_REAL;
		    }
		} else {
		    ANS(i, j) = NA_REAL;
		}   
		continue;
	    }
	    /* Argh. */
	    /* For full matching (where we use regexps of regexp-quoted
	     * strings anchored by ^ and $) there is a problem in case
	     * of substring matches, where TRE finds a substitution
	     * where there is none.  Cf.
	     *   $ echo "AB" | tre-agrep -5 -s A
	     *   0:AB
	     *   $ echo "AB" | tre-agrep -5 -s -w A
	     *   2:AB
	     * So we need to explicitly catch these cases.
	     */
	    if(!opt_partial) {
		s = CHAR(STRING_ELT(x, i));
		t = CHAR(STRING_ELT(y, j));
		if(strstr(t, s) != NULL) {
		    delta = (strlen(t) - strlen(s));
		    ANS(i, j) = (double) (delta * opt_cost_ins);
		    if(opt_counts) {
			COUNTS(i, j, 0) = (double) delta;
			COUNTS(i, j, 1) = (double) 0.;
			COUNTS(i, j, 2) = (double) 0.;
		    }
		    continue;
		}
	    }
	    /* Perform match. */
	    /* undocumented, must be zeroed */
	    memset(&match, 0, sizeof(match));
	    rc = tre_regaexec(&reg, CHAR(STRING_ELT(y, j)),
			      &match, params, 0);
	    ANS(i, j) = (double) match.cost;	    
	    if(opt_counts) {
		COUNTS(i, j, 0) = (double) match.num_ins;
		COUNTS(i, j, 1) = (double) match.num_del;
		COUNTS(i, j, 2) = (double) match.num_subst;
	    } else {

	    }
	}

	tre_regfree(&reg);
    }

    x = getAttrib(x, R_NamesSymbol);
    y = getAttrib(y, R_NamesSymbol);
    if(!isNull(x) || !isNull(y)) {
	PROTECT(dimnames = allocVector(VECSXP, 2));	    
	SET_VECTOR_ELT(dimnames, 0, x);
	SET_VECTOR_ELT(dimnames, 1, y);
	setAttrib(ans, R_DimNamesSymbol, dimnames);
	UNPROTECT(1);
    }
    if(opt_counts) {
	PROTECT(dimnames = allocVector(VECSXP, 3));
	PROTECT(names = allocVector(STRSXP, 3));
	SET_STRING_ELT(names, 0, mkChar("ins"));
	SET_STRING_ELT(names, 1, mkChar("del"));
	SET_STRING_ELT(names, 2, mkChar("sub"));
	SET_VECTOR_ELT(dimnames, 0, x);
	SET_VECTOR_ELT(dimnames, 1, y);
	SET_VECTOR_ELT(dimnames, 2, names);
	setAttrib(counts, R_DimNamesSymbol, dimnames);
	setAttrib(ans, install("counts"), counts);	
	UNPROTECT(4);
    }

    UNPROTECT(1);
    return ans;
}
