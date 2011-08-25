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

#define MAT_ANS(I, J)      REAL(ans)[I + J * nx]
#define ARR_ANS(I, J, K)   REAL(ans)[I + J * nx + K * nxy]

SEXP attribute_hidden do_adist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y, ans, dim, dimnames, names;
    int cost_ins_opt, cost_del_opt, cost_sub_opt,
	partial_opt, all_opt, icase_opt, useBytes;
    int i, j, k, nx, ny, nxy;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_EXTENDED | REG_NOSUB;
    
    checkArity(op, args);
    x = CAR(args); args = CDR(args);
    y = CAR(args); args = CDR(args);
    cost_ins_opt = asInteger(CAR(args)); args = CDR(args);
    cost_del_opt = asInteger(CAR(args)); args = CDR(args);
    cost_sub_opt = asInteger(CAR(args)); args = CDR(args);
    partial_opt = asInteger(CAR(args)); args = CDR(args);
    all_opt = asLogical(CAR(args)); args = CDR(args);
    icase_opt = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args));

    if(partial_opt == NA_INTEGER) partial_opt = 0;
    if(all_opt == NA_INTEGER) all_opt = 0;
    if(icase_opt == NA_INTEGER) icase_opt = 0;
    if(useBytes == NA_INTEGER) useBytes = 0;

    if(partial_opt) cflags |= REG_LITERAL;
    if(icase_opt) cflags |= REG_ICASE;
    
    nx = length(x);
    if(!isString(x) || (nx < 1))
	error(_("invalid '%s' argument"), "x");
    ny = length(y);
    if(!isString(y) || (ny < 1))
	error(_("invalid '%s' argument"), "y");
    nxy = nx * ny;
    
    if(all_opt) {
	PROTECT(dim = allocVector(INTSXP, 3));
	INTEGER(dim)[0] = nx;
	INTEGER(dim)[1] = ny;
	INTEGER(dim)[2] = 4;
	UNPROTECT(1);
	PROTECT(ans = allocArray(REALSXP, dim));
    } else {
	PROTECT(ans = allocMatrix(REALSXP, nx, ny));
    }

    tre_regaparams_default(&params);
    params.max_cost = INT_MAX;
    params.cost_ins = cost_ins_opt;
    params.cost_del = cost_del_opt;
    params.cost_subst = cost_sub_opt;

    /* Handle encoding stuff etc lateron. */
    for(i = 0; i < nx; i++) {
	if(STRING_ELT(x, i) == NA_STRING) {
	    for(j = 0; j < ny; j++) {
		if(all_opt) {
		    for(k = 0; k < 4; k++) {
			ARR_ANS(i, j, k) = NA_REAL;
		    }
		} else {
		    MAT_ANS(i, j) = NA_REAL;
		}
	    }
	    continue;
	}
	rc = tre_regcomp(&reg, CHAR(STRING_ELT(x, i)), cflags);
	if(rc) {
	    char errbuf[1001];
	    tre_regerror(rc, &reg, errbuf, 1001);
	    error(_("regcomp error:  '%s'"), errbuf);
	}
	for(j = 0; j < ny; j++) {
	    if(STRING_ELT(y, j) == NA_STRING) {
		if(all_opt) {
		    for(k = 0; k < 4; k++) {
			ARR_ANS(i, j, k) = NA_REAL;
		    }
		} else {
		    MAT_ANS(i, j) = NA_REAL;
		}   
		continue;
	    }
	    /* Perform match. */
	    /* undocumented, must be zeroed */
	    memset(&match, 0, sizeof(match));
	    rc = tre_regaexec(&reg, CHAR(STRING_ELT(y, j)),
			      &match, params, 0);
	    if(all_opt) {
		ARR_ANS(i, j, 0) = (double) match.cost;
		ARR_ANS(i, j, 1) = (double) match.num_ins;
		ARR_ANS(i, j, 2) = (double) match.num_del;
		ARR_ANS(i, j, 3) = (double) match.num_subst;
	    } else {
		MAT_ANS(i, j) = (double) match.cost;
	    }
	}

	tre_regfree(&reg);
    }

    x = getAttrib(x, R_NamesSymbol);
    y = getAttrib(y, R_NamesSymbol);
    if(all_opt) {
	PROTECT(dimnames = allocVector(VECSXP, 3));
	PROTECT(names = allocVector(STRSXP, 4));
	SET_STRING_ELT(names, 0, mkChar("cost"));
	SET_STRING_ELT(names, 1, mkChar("ins"));
	SET_STRING_ELT(names, 2, mkChar("del"));
	SET_STRING_ELT(names, 3, mkChar("sub"));
	SET_VECTOR_ELT(dimnames, 0, x);
	SET_VECTOR_ELT(dimnames, 1, y);
	SET_VECTOR_ELT(dimnames, 2, names);
	setAttrib(ans, R_DimNamesSymbol, dimnames);
	UNPROTECT(2);
    } else {
	if(!isNull(x) || !isNull(y)) {
	    PROTECT(dimnames = allocVector(VECSXP, 2));	    
	    SET_VECTOR_ELT(dimnames, 0, x);
	    SET_VECTOR_ELT(dimnames, 1, y);
	    setAttrib(ans, R_DimNamesSymbol, dimnames);
	    UNPROTECT(1);
	}
    }

    UNPROTECT(1);
    return ans;
}
