/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2002--2012  The R Core Team
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
#include <Internal.h>

/* This is remapped */
#undef pmatch 

/* interval at which to check interrupts */
#define NINTERRUPT 1000000

#include <R_ext/RS.h>		/* for Calloc/Free */

#include <wchar.h>
#include <tre/tre.h>

static void
amatch_regaparams(regaparams_t *params, int patlen,
		  double *bounds, int *costs)
{
    int cost, max_cost, warn = 0;
    double bound;

    cost = params->cost_ins = max_cost = costs[0];
    cost = params->cost_del = costs[1];
    if(cost > max_cost) max_cost = cost;
    cost = params->cost_subst = costs[2];
    if(cost > max_cost) max_cost = cost;
    bound = bounds[0];
    if(ISNA(bound)) {
	params->max_cost = INT_MAX;
    } else {
	if(bound < 1) bound *= (patlen * max_cost);
	params->max_cost = IntegerFromReal(ceil(bound), &warn);
	CoercionWarning(warn);
    }
    bound = bounds[1];
    if(ISNA(bound)) {
	params->max_del = INT_MAX;
    } else {
	if(bound < 1) bound *= patlen;
	params->max_del = IntegerFromReal(ceil(bound), &warn);
	CoercionWarning(warn);
    }
    bound = bounds[2];
    if(ISNA(bound)) {
	params->max_ins = INT_MAX;
    } else {
	if(bound < 1) bound *= patlen;
	params->max_ins = IntegerFromReal(ceil(bound), &warn);
	CoercionWarning(warn);
    }
    bound = bounds[3];
    if(ISNA(bound)) {
	params->max_subst = INT_MAX;
    } else {
	if(bound < 1) bound *= patlen;
	params->max_subst = IntegerFromReal(ceil(bound), &warn);
	CoercionWarning(warn);
    }
    bound = bounds[4];
    if(ISNA(bound)) {
	params->max_err = INT_MAX;
    } else {
	if(bound < 1) bound *= patlen;
	params->max_err = IntegerFromReal(ceil(bound), &warn);
	CoercionWarning(warn);
    }
}

SEXP attribute_hidden do_agrep(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ind, ans;
    SEXP opt_costs, opt_bounds;
    int opt_icase, opt_value, opt_fixed, useBytes;
    R_xlen_t i, j, n;
    int nmatches, patlen;
    Rboolean useWC = FALSE;
    const void *vmax = NULL;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    int rc, cflags = REG_NOSUB;

    checkArity(op, args);
    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    opt_icase = asLogical(CAR(args)); args = CDR(args);
    opt_value = asLogical(CAR(args)); args = CDR(args);
    opt_costs = CAR(args); args = CDR(args);
    opt_bounds = CAR(args); args = CDR(args);
    useBytes = asLogical(CAR(args));
    args = CDR(args);
    opt_fixed = asLogical(CAR(args));

    if(opt_icase == NA_INTEGER) opt_icase = 0;
    if(opt_value == NA_INTEGER) opt_value = 0;
    if(useBytes == NA_INTEGER) useBytes = 0;
    if(opt_fixed == NA_INTEGER) opt_fixed = 1;

    if(opt_fixed) cflags |= REG_LITERAL;

    if(!isString(pat) || length(pat) < 1)
	error(_("invalid '%s' argument"), "pattern");
    if(length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");
    
    if(!isString(vec)) error(_("invalid '%s' argument"), "x");

    if(opt_icase) cflags |= REG_ICASE;

    n = XLENGTH(vec);
    if(!useBytes) {
	Rboolean haveBytes = IS_BYTES(STRING_ELT(pat, 0));
	if(!haveBytes)
	    for (i = 0; i < n; i++)
		if(IS_BYTES(STRING_ELT(vec, i))) {
		    haveBytes = TRUE;
		    break;
		}
	if(haveBytes) useBytes = TRUE;
    }
    if(!useBytes) {
	useWC = !IS_ASCII(STRING_ELT(pat, 0));
	if(!useWC) {
	    for (i = 0 ; i < n ; i++) {
		if(STRING_ELT(vec, i) == NA_STRING) continue;
		if(!IS_ASCII(STRING_ELT(vec, i))) {
		    useWC = TRUE;
		    break;
		}
	    }
	}
    }

    if(STRING_ELT(pat, 0) == NA_STRING) {
	if(opt_value) {
	    PROTECT(ans = allocVector(STRSXP, n));
	    for(i = 0; i < n; i++)
		SET_STRING_ELT(ans, i, NA_STRING);
	    SEXP nms = getAttrib(vec, R_NamesSymbol);
	    if(!isNull(nms))
		setAttrib(ans, R_NamesSymbol, nms);
	} else {
	    PROTECT(ans = allocVector(INTSXP, n));
	    for(i = 0; i < n; i++)
		INTEGER(ans)[i] = NA_INTEGER;
	}
	UNPROTECT(1);
	return ans;
    }

    if(useBytes)
	PROTECT(call = lang3(install("nchar"), pat,
			     ScalarString(mkChar("bytes"))));
    else
	PROTECT(call = lang3(install("nchar"), pat,
			     ScalarString(mkChar("chars"))));
    patlen = asInteger(eval(call, env));
    UNPROTECT(1);
    if(!patlen)
	error(_("'pattern' must be a non-empty character string"));

    /* wtransChar and translateChar can R_alloc */
    vmax = vmaxget();
    if(useBytes)
	rc = tre_regcompb(&reg, CHAR(STRING_ELT(pat, 0)), cflags);
    else if(useWC)
	rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
    else {
	const char *spat = translateChar(STRING_ELT(pat, 0));
	if(mbcslocale && !mbcsValid(spat))
	    error(_("regular expression is invalid in this locale"));
	rc = tre_regcomp(&reg, spat, cflags);
    }
    if(rc) {
	char errbuf[1001];
	tre_regerror(rc, &reg, errbuf, 1001);
	error(_("regcomp error:  '%s'"), errbuf);
    }

    tre_regaparams_default(&params);
    amatch_regaparams(&params, patlen,
		      REAL(opt_bounds), INTEGER(opt_costs));
    
    /* Matching. */
    n = LENGTH(vec);
    PROTECT(ind = allocVector(LGLSXP, n));
    nmatches = 0;
    for (i = 0 ; i < n ; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if(STRING_ELT(vec, i) == NA_STRING) {
	    LOGICAL(ind)[i] = 0;
	    continue;
	}
	/* Perform match. */
	/* undocumented, must be zeroed */
	memset(&match, 0, sizeof(match));
	if(useBytes)
	    rc = tre_regaexecb(&reg,
			       CHAR(STRING_ELT(vec, i)),
			       &match, params, 0);
	else if(useWC) {
	    rc = tre_regawexec(&reg,
			       wtransChar(STRING_ELT(vec, i)), 
			       &match, params, 0);
	    vmaxset(vmax);
	} else {
	    const char *s = translateChar(STRING_ELT(vec, i));
	    if(mbcslocale && !mbcsValid(s))
		error(_("input string %d is invalid in this locale"), i+1);
	    rc = tre_regaexec(&reg, s, &match, params, 0);
	    vmaxset(vmax);
	}
	if(rc == REG_OK) {
	    LOGICAL(ind)[i] = 1;
	    nmatches++;
	} else LOGICAL(ind)[i] = 0;
    }
    tre_regfree(&reg);

    if(opt_value) {
	PROTECT(ans = allocVector(STRSXP, nmatches));
	SEXP nmold = getAttrib(vec, R_NamesSymbol), nm;
	for (j = i = 0 ; i < n ; i++) {
	    if(LOGICAL(ind)[i])
		SET_STRING_ELT(ans, j++, STRING_ELT(vec, i));
	}
	/* copy across names and subset */
	if(!isNull(nmold)) {
	    nm = allocVector(STRSXP, nmatches);
	    for (i = 0, j = 0; i < n ; i++)
		if(LOGICAL(ind)[i])
		    SET_STRING_ELT(nm, j++, STRING_ELT(nmold, i));
	    setAttrib(ans, R_NamesSymbol, nm);
	}
    }
#ifdef LONG_VECTOR_SUPPORT
    else if (n > INT_MAX) {
	PROTECT(ans = allocVector(REALSXP, nmatches));
	for (j = i = 0 ; i < n ; i++)
	    if(LOGICAL(ind)[i] == 1)
		REAL(ans)[j++] = (double)(i + 1);
    }
#endif
    else {
	PROTECT(ans = allocVector(INTSXP, nmatches));
	for (j = i = 0 ; i < n ; i++)
	    if(LOGICAL(ind)[i] == 1)
		INTEGER(ans)[j++] = (int)(i + 1);
    }

    UNPROTECT(2);
    return ans;
}

#define ANS(I, J)		REAL(ans)[I + J * nx]
#define COUNTS(I, J, K)		INTEGER(counts)[I + J * nx + K * nxy]

#define MAT(X, I, J)		X[I + (J) * nr]

static SEXP
adist_full(SEXP x, SEXP y, double *costs, Rboolean opt_counts)
{
    SEXP ans, counts, trafos = R_NilValue /* -Wall */, dimnames, names;
    double cost_ins, cost_del, cost_sub;
    double *dists, d, d_ins, d_del, d_sub;
    char *paths = NULL, p, *buf = NULL;
    int i, j, k, l, m, nx, ny, nxy, *xi, *yj, nxi, nyj, nr, nc, nz;
    int nins, ndel, nsub, buflen = 100, need;

    counts = R_NilValue;	/* -Wall */

    nx = LENGTH(x);
    ny = LENGTH(y);
    nxy = nx * ny;

    cost_ins = costs[0];
    cost_del = costs[1];
    cost_sub = costs[2];

    PROTECT(ans = allocMatrix(REALSXP, nx, ny));
    if(opt_counts) {
	PROTECT(counts = alloc3DArray(INTSXP, nx, ny, 3));
	PROTECT(trafos = allocMatrix(STRSXP, nx, ny));
	buf = Calloc(buflen, char);
    }

    for(i = 0; i < nx; i++) {
	nxi = LENGTH(VECTOR_ELT(x, i));	
	xi = INTEGER(VECTOR_ELT(x, i));
	if(nxi && (xi[0] == NA_INTEGER)) {
	    for(j = 0; j < ny; j++) {
		ANS(i, j) = NA_REAL;
	    }
	    if(opt_counts) {
		for(m = 0; m < 3; m++) {
		    COUNTS(i, j, m) = NA_INTEGER;
		}
	    }
	} else {
	    for(j = 0; j < ny; j++) {
		nyj = LENGTH(VECTOR_ELT(y, j));
		yj = INTEGER(VECTOR_ELT(y, j));
		if(nyj && (yj[0] == NA_INTEGER)) {
		    ANS(i, j) = NA_REAL;
		    if(opt_counts) {
			for(m = 0; m < 3; m++) {
			    COUNTS(i, j, m) = NA_INTEGER;
			}
		    }
		}
		else {
		    /* Determine operation-weighted edit distance via
		     * straightforward dynamic programming.
		     */
		    nr = nxi + 1;
		    nc = nyj + 1;
		    dists = Calloc(nr * nc, double);
		    MAT(dists, 0, 0) = 0;
		    for(k = 1; k < nr; k++)
			MAT(dists, k, 0) = k * cost_del;
		    for(l = 1; l < nc; l++)
			MAT(dists, 0, l) = l * cost_ins;
		    if(opt_counts) {
			paths = Calloc(nr * nc, char);
			for(k = 1; k < nr; k++)
			    MAT(paths, k, 0) = 'D';
			for(l = 1; l < nc; l++)
			    MAT(paths, 0, l) = 'I';
		    }
		    for(k = 1; k < nr; k++) {
			for(l = 1; l < nc; l++) {
			    if(xi[k - 1] == yj[l - 1]) {
				MAT(dists, k, l) =
				    MAT(dists, k - 1, l - 1);
				if(opt_counts)
				    MAT(paths, k, l) = 'M';
			    } else {
				d_ins = MAT(dists, k, l - 1) + cost_ins;
				d_del = MAT(dists, k - 1, l) + cost_del;
				d_sub = MAT(dists, k - 1, l - 1) + cost_sub;
				if(opt_counts) {
				    if(d_ins <= d_del) {
					d = d_ins;
					p = 'I';
				    } else {
					d = d_del;
					p = 'D';
				    }
				    if(d_sub < d) {
					d = d_sub;
					p = 'S';
				    }
				    MAT(paths, k, l) = p;
				} else {
				    d = fmin(fmin(d_ins, d_del), d_sub);
				}
				MAT(dists, k, l) = d;
			    }
			}
		    }
		    ANS(i, j) = MAT(dists, nxi, nyj);
		    if(opt_counts) {
			if(!R_finite(ANS(i, j))) {
			    for(m = 0; m < 3; m++) {
				COUNTS(i, j, m) = NA_INTEGER;
			    }
			    SET_STRING_ELT(trafos, i + nx * j, NA_STRING);
			} else {
			    nins = ndel = nsub = 0;
			    k = nxi; l = nyj; m = k + l; nz = m;
			    need = 2 * m + 1;
			    if(buflen < need) {
				buf = Realloc(buf, need , char);
				buflen = need;
			    }
			    /* Need to read backwards and fill forwards. */
			    while((k > 0) || (l > 0)) {
				p = MAT(paths, k, l);
				if(p == 'I') {
				    nins++;
				    l--;
				} else if(p == 'D') {
				    ndel++;
				    k--;
				} else {
				    if(p == 'S')
					nsub++;
				    k--;
				    l--;
				}
				buf[m] = p;
				m++;
			    }
			    /* Now reverse the transcript. */
			    for(k = 0, l = --m; l >= nz; k++, l--)
				buf[k] = buf[l];
			    buf[++k] = '\0';
			    COUNTS(i, j, 0) = nins;
			    COUNTS(i, j, 1) = ndel;
			    COUNTS(i, j, 2) = nsub;
			    SET_STRING_ELT(trafos, i + nx * j, mkChar(buf));
			}
			Free(paths);
		    }
		    Free(dists);
		}
	    }
	}
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
	Free(buf);
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
	UNPROTECT(2);
	if(!isNull(x) || !isNull(y)) {
	    PROTECT(dimnames = allocVector(VECSXP, 2));	    
	    SET_VECTOR_ELT(dimnames, 0, x);
	    SET_VECTOR_ELT(dimnames, 1, y);
	    setAttrib(trafos, R_DimNamesSymbol, dimnames);
	    UNPROTECT(1);
	}
	setAttrib(ans, install("trafos"), trafos);
	UNPROTECT(2);
    }
		
    UNPROTECT(1);
    return ans;
}

#define OFFSETS(I, J, K)	INTEGER(offsets)[I + J * nx + K * nxy]

SEXP attribute_hidden do_adist(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, y;
    SEXP ans, counts, offsets, dimnames, names, elt;
    SEXP opt_costs;
    int opt_fixed, opt_partial, opt_counts, opt_icase, useBytes;
    int i = 0, j = 0, m, nx, ny, nxy;
    const char *s, *t;
    const void *vmax = NULL;

    Rboolean haveBytes, useWC = FALSE;

    regex_t reg;
    regaparams_t params;
    regamatch_t match;
    size_t nmatch = 0 /* -Wall */;
    regmatch_t *pmatch = NULL; /* -Wall */

    int rc, cflags = REG_EXTENDED;

    checkArity(op, args);
    x = CAR(args); args = CDR(args);
    y = CAR(args); args = CDR(args);
    opt_costs = CAR(args); args = CDR(args);
    opt_counts = asLogical(CAR(args)); args = CDR(args);
    opt_fixed = asInteger(CAR(args)); args = CDR(args);    
    opt_partial = asInteger(CAR(args)); args = CDR(args);
    opt_icase = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args));

    if(opt_counts == NA_INTEGER) opt_counts = 0;
    if(opt_fixed == NA_INTEGER) opt_fixed = 1;
    if(opt_partial == NA_INTEGER) opt_partial = 0;
    if(opt_icase == NA_INTEGER) opt_icase = 0;
    if(useBytes == NA_INTEGER) useBytes = 0;

    if(opt_fixed) cflags |= REG_LITERAL;
    if(opt_icase) cflags |= REG_ICASE;

    if(!opt_fixed && !opt_partial) {
	warning(_("argument '%s' will be ignored"), "partial = FALSE");
    }

    if(!opt_partial)
	return(adist_full(x, y, REAL(opt_costs), opt_counts));

    counts = R_NilValue;	/* -Wall */
    offsets = R_NilValue;	/* -Wall */

    if(!opt_counts) cflags |= REG_NOSUB;

    nx = length(x);
    ny = length(y);
    nxy = nx * ny;    

    if(!useBytes) {
	haveBytes = FALSE;
	for(i = 0; i < nx; i++) {
	    if(IS_BYTES(STRING_ELT(x, i))) {
		haveBytes = TRUE;
		break;
	    }
	}
	if(!haveBytes) {
	    for(j = 0; j < ny; j++) {
		if(IS_BYTES(STRING_ELT(y, j))) {
		    haveBytes = TRUE;
		    break;
		}
	    }
	}
	if(haveBytes) useBytes = TRUE;
    }

    if(!useBytes) {
	for(i = 0; i < nx; i++) {
	    if(STRING_ELT(x, i) == NA_STRING) continue;
	    if(!IS_ASCII(STRING_ELT(x, i))) {
		useWC = TRUE;
		break;
	    }
	}
	if(!useWC) {
	    for(j = 0; j < ny; j++) {
		if(STRING_ELT(y, j) == NA_STRING) continue;
		if(!IS_ASCII(STRING_ELT(y, j))) {
		    useWC = TRUE;
		    break;
		}
	    }
	}
    }

    tre_regaparams_default(&params);
    params.max_cost = INT_MAX;
    params.cost_ins = INTEGER(opt_costs)[0];;
    params.cost_del = INTEGER(opt_costs)[1];
    params.cost_subst = INTEGER(opt_costs)[2];

    PROTECT(ans = allocMatrix(REALSXP, nx, ny));
    if(opt_counts) {
	PROTECT(counts = alloc3DArray(INTSXP, nx, ny, 3));
	PROTECT(offsets = alloc3DArray(INTSXP, nx, ny, 2));
    }

    /* wtransChar and translateChar can R_alloc */
    vmax = vmaxget();
    for(i = 0; i < nx; i++) {
	elt = STRING_ELT(x, i);
	if(elt == NA_STRING) {
	    for(j = 0; j < ny; j++) {
		ANS(i, j) = NA_REAL;
		if(opt_counts) {
		    for(m = 0; m < 3; m++) {
			COUNTS(i, j, m) = NA_INTEGER;
		    }
		    OFFSETS(i, j, 0) = -1;
		    OFFSETS(i, j, 1) = -1;
		}
	    }
	} else {
	    if(useBytes)
		rc = tre_regcompb(&reg, CHAR(elt), cflags);
	    else if(useWC) {
		rc = tre_regwcomp(&reg, wtransChar(elt), cflags);
		vmaxset(vmax);
	    } else {
		s = translateChar(elt);
		if(mbcslocale && !mbcsValid(s)) {
		    error(_("input string x[%d] is invalid in this locale"),
			  i + 1);
		}
		rc = tre_regcomp(&reg, s, cflags);
		vmaxset(vmax);
	    }
	    if(rc) {
		char errbuf[1001];
		tre_regerror(rc, &reg, errbuf, 1001);
		error(_("regcomp error:  '%s'"), errbuf);
	    }
	    if(opt_counts) {
		nmatch = reg.re_nsub + 1;
		pmatch = (regmatch_t *) malloc(nmatch * sizeof(regmatch_t));
	    }

	    for(j = 0; j < ny; j++) {
		elt = STRING_ELT(y, j);
		if(elt == NA_STRING) {
		    ANS(i, j) = NA_REAL;
		    if(opt_counts) {
			for(m = 0; m < 3; m++) {
			    COUNTS(i, j, m) = NA_INTEGER;
			}
			OFFSETS(i, j, 0) = -1;
			OFFSETS(i, j, 1) = -1;
		    }
		}
		else {
		    /* Perform match. */
		    /* undocumented, must be zeroed */
		    memset(&match, 0, sizeof(match));
		    if(opt_counts) {
			match.nmatch = nmatch;
			match.pmatch = pmatch;
		    }
		    if(useBytes)
			rc = tre_regaexecb(&reg, CHAR(elt),
					   &match, params, 0);
		    else if(useWC) {
			rc = tre_regawexec(&reg, wtransChar(elt), 
					   &match, params, 0);
			vmaxset(vmax);
		    } else {
			t = translateChar(elt);
			if(mbcslocale && !mbcsValid(t)) {
			    error(_("input string y[%d] is invalid in this locale"),
				  j + 1);
			}
			rc = tre_regaexec(&reg, t,
					  &match, params, 0);
			vmaxset(vmax);
		    }
		    if(rc == REG_OK) {
			ANS(i, j) = (double) match.cost;
			if(opt_counts) {
			    COUNTS(i, j, 0) = match.num_ins;
			    COUNTS(i, j, 1) = match.num_del;
			    COUNTS(i, j, 2) = match.num_subst;
			    OFFSETS(i, j, 0) = match.pmatch[0].rm_so + 1;
			    OFFSETS(i, j, 1) = match.pmatch[0].rm_eo;
			}
		    } else {
			/* Should maybe check for REG_NOMATCH? */
			ANS(i, j) = R_PosInf;
			if(opt_counts) {
			    for(m = 0; m < 3; m++) {
				COUNTS(i, j, m) = NA_INTEGER;
			    }
			    OFFSETS(i, j, 0) = -1;
			    OFFSETS(i, j, 1) = -1;
			}
		    }
		}
	    }
	    if(opt_counts)
		free(pmatch);
	    tre_regfree(&reg);
	}
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
	UNPROTECT(2);
	PROTECT(dimnames = allocVector(VECSXP, 3));
	PROTECT(names = allocVector(STRSXP, 2));
	SET_STRING_ELT(names, 0, mkChar("first"));
	SET_STRING_ELT(names, 1, mkChar("last"));
	SET_VECTOR_ELT(dimnames, 0, x);
	SET_VECTOR_ELT(dimnames, 1, y);
	SET_VECTOR_ELT(dimnames, 2, names);
	setAttrib(offsets, R_DimNamesSymbol, dimnames);
	setAttrib(ans, install("offsets"), offsets);
	UNPROTECT(4);
    }

    UNPROTECT(1);
    return ans;
}

SEXP attribute_hidden do_aregexec(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP pat, vec, ans, matchpos, matchlen;
    SEXP opt_bounds, opt_costs;
    int opt_icase, opt_fixed, useBytes;

    Rboolean haveBytes, useWC = FALSE;
    const char *s, *t;
    const void *vmax = NULL;
    
    regex_t reg;
    size_t nmatch;
    regmatch_t *pmatch;
    regaparams_t params;
    regamatch_t match;
    int j, so, patlen;
    R_xlen_t i, n;
    int rc, cflags = REG_EXTENDED;

    checkArity(op, args);

    pat = CAR(args); args = CDR(args);
    vec = CAR(args); args = CDR(args);
    opt_bounds = CAR(args); args = CDR(args);
    opt_costs = CAR(args); args = CDR(args);
    opt_icase = asLogical(CAR(args)); args = CDR(args);
    opt_fixed = asLogical(CAR(args)); args = CDR(args);
    useBytes = asLogical(CAR(args));
    
    if(opt_icase == NA_INTEGER) opt_icase = 0;
    if(opt_fixed == NA_INTEGER) opt_fixed = 0;
    if(useBytes == NA_INTEGER) useBytes = 0;
    if(opt_fixed && opt_icase) {
	warning(_("argument '%s' will be ignored"),
		"ignore.case = TRUE");
	opt_icase = 0;
    }
    if(opt_fixed) cflags |= REG_LITERAL;
    if(opt_icase) cflags |= REG_ICASE;

    if(!isString(pat) ||
       (length(pat) < 1) ||
       (STRING_ELT(pat, 0) == NA_STRING))
	error(_("invalid '%s' argument"), "pattern");
    if(length(pat) > 1)
	warning(_("argument '%s' has length > 1 and only the first element will be used"), "pattern");
    
    if(!isString(vec))
	error(_("invalid '%s' argument"), "text");

    n = XLENGTH(vec);

    if(!useBytes) {
        haveBytes = IS_BYTES(STRING_ELT(pat, 0));
	if(!haveBytes)
            for(i = 0; i < n; i++) {
                if(IS_BYTES(STRING_ELT(vec, i))) {
                    haveBytes = TRUE;
                    break;
                }
	    }
	if(haveBytes) useBytes = TRUE;
    }

    if(!useBytes) {
        useWC = !IS_ASCII(STRING_ELT(pat, 0));
        if(!useWC) {
            for(i = 0 ; i < n ; i++) {
                if(STRING_ELT(vec, i) == NA_STRING) continue;
                if(!IS_ASCII(STRING_ELT(vec, i))) {
                    useWC = TRUE;
                    break;
                }
            }
        }
    }
    
    if(useBytes)
	PROTECT(call = lang3(install("nchar"), pat,
			     ScalarString(mkChar("bytes"))));
    else
	PROTECT(call = lang3(install("nchar"), pat,
			     ScalarString(mkChar("chars"))));
    patlen = asInteger(eval(call, env));
    UNPROTECT(1);
    if(!patlen)
	error(_("'pattern' must be a non-empty character string"));

    if(useBytes)
	rc = tre_regcompb(&reg, CHAR(STRING_ELT(pat, 0)), cflags);
    else if(useWC)
	rc = tre_regwcomp(&reg, wtransChar(STRING_ELT(pat, 0)), cflags);
    else {
        s = translateChar(STRING_ELT(pat, 0));
        if(mbcslocale && !mbcsValid(s))
            error(_("regular expression is invalid in this locale"));
        rc = tre_regcomp(&reg, s, cflags);
    }
    if(rc) {
        char errbuf[1001];
        tre_regerror(rc, &reg, errbuf, 1001);
        error(_("regcomp error: '%s'"), errbuf);
    }

    nmatch = reg.re_nsub + 1;

    pmatch = (regmatch_t *) malloc(nmatch * sizeof(regmatch_t));

    tre_regaparams_default(&params);
    amatch_regaparams(&params, patlen,
		      REAL(opt_bounds), INTEGER(opt_costs));

    PROTECT(ans = allocVector(VECSXP, n));

    for(i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	if(STRING_ELT(vec, i) == NA_STRING) {
	    PROTECT(matchpos = ScalarInteger(NA_INTEGER));
	    setAttrib(matchpos, install("match.length"),
		      ScalarInteger(NA_INTEGER));
	    SET_VECTOR_ELT(ans, i, matchpos);
	    UNPROTECT(1);
	} else {
	    vmax = vmaxget();
	    /* Perform match. */
	    memset(&match, 0, sizeof(match));
	    match.nmatch = nmatch;
	    match.pmatch = pmatch;
	    if(useBytes)
		rc = tre_regaexecb(&reg, CHAR(STRING_ELT(vec, i)),
				   &match, params, 0);
	    else if(useWC) {
		rc = tre_regawexec(&reg, wtransChar(STRING_ELT(vec, i)),
				   &match, params, 0);
		vmaxset(vmax);
	    }
	    else {
		t = translateChar(STRING_ELT(vec, i));
		if(mbcslocale && !mbcsValid(t))
		    error(_("input string %d is invalid in this locale"),
			  i + 1);
		rc = tre_regaexec(&reg, t,
				  &match, params, 0);
		vmaxset(vmax);		
	    }
	    if(rc == REG_OK) {
		PROTECT(matchpos = allocVector(INTSXP, nmatch));
		PROTECT(matchlen = allocVector(INTSXP, nmatch));
		for(j = 0; j < match.nmatch; j++) {
		    so = match.pmatch[j].rm_so;
		    INTEGER(matchpos)[j] = so + 1;
		    INTEGER(matchlen)[j] = match.pmatch[j].rm_eo - so;
		}
		setAttrib(matchpos, install("match.length"), matchlen);
		if(useBytes)
		    setAttrib(matchpos, install("useBytes"),
			      ScalarLogical(TRUE));
		SET_VECTOR_ELT(ans, i, matchpos);
		UNPROTECT(2);
	    } else {
		/* No match (or could there be an error?). */
		/* Alternatively, could return nmatch -1 values.
		*/
		PROTECT(matchpos = ScalarInteger(-1));
		PROTECT(matchlen = ScalarInteger(-1));
		setAttrib(matchpos, install("match.length"), matchlen);
		SET_VECTOR_ELT(ans, i, matchpos);
		UNPROTECT(2);
	    }
	}
    }
    
    free(pmatch);

    tre_regfree(&reg);

    UNPROTECT(1);

    return ans;
}
