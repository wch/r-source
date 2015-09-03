/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2015   The R Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
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
 *  https://www.R-project.org/Licenses/
 *
 *
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

/* JMC convinced MM that this was not a good idea: */
#undef _S4_subsettable


static R_INLINE SEXP VECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = 2.
       Duplicating might be safer/more consistent (fix bug reported by
       Radford Neal; similar to PR15098) */
    SEXP val = VECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	if (NAMED(val) < 2)
	    SET_NAMED(val, 2);
    return val;
}

/* ExtractSubset does the transfer of elements from "x" to "result"
   according to the integer/real subscripts given in "indx". */

static SEXP ExtractSubset(SEXP x, SEXP result, SEXP indx, SEXP call)
{
    R_xlen_t i, ii, n, nx;
    int mode, mi;
    SEXP tmp, tmp2;
    mode = TYPEOF(x);
    mi = TYPEOF(indx);
    n = XLENGTH(indx);
    nx = xlength(x);
    tmp = result;

    if (x == R_NilValue)
	return x;

    for (i = 0; i < n; i++) {
	switch(mi) {
	case REALSXP:
	    if(!R_FINITE(REAL(indx)[i])) ii = NA_INTEGER;
	    else ii = (R_xlen_t) (REAL(indx)[i] - 1);
	    break;
	default:
	    ii = INTEGER(indx)[i];
	    if (ii != NA_INTEGER) ii--;
	}
	switch (mode) {
	    /* NA_INTEGER < 0, so some of this is redundant */
	case LGLSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		LOGICAL(result)[i] = LOGICAL(x)[ii];
	    else
		LOGICAL(result)[i] = NA_INTEGER;
	    break;
	case INTSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		INTEGER(result)[i] = INTEGER(x)[ii];
	    else
		INTEGER(result)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		REAL(result)[i] = REAL(x)[ii];
	    else
		REAL(result)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER) {
		COMPLEX(result)[i] = COMPLEX(x)[ii];
	    } else {
		COMPLEX(result)[i].r = NA_REAL;
		COMPLEX(result)[i].i = NA_REAL;
	    }
	    break;
	case STRSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		SET_STRING_ELT(result, i, STRING_ELT(x, ii));
	    else
		SET_STRING_ELT(result, i, NA_STRING);
	    break;
	case VECSXP:
	case EXPRSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		SET_VECTOR_ELT(result, i, VECTOR_ELT_FIX_NAMED(x, ii));
	    else
		SET_VECTOR_ELT(result, i, R_NilValue);
	    break;
	case LISTSXP:
	    /* cannot happen: pairlists are coerced to lists */
	case LANGSXP:
#ifdef LONG_VECTOR_SUPPORT
	    if (ii > R_SHORT_LEN_MAX)
		error("invalid subscript for pairlist");
#endif
	    if (0 <= ii && ii < nx && ii != NA_INTEGER) {
		tmp2 = nthcdr(x, (int) ii);
		SETCAR(tmp, CAR(tmp2));
		SET_TAG(tmp, TAG(tmp2));
	    }
	    else
		SETCAR(tmp, R_NilValue);
	    tmp = CDR(tmp);
	    break;
	case RAWSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		RAW(result)[i] = RAW(x)[ii];
	    else
		RAW(result)[i] = (Rbyte) 0;
	    break;
	default:
	    errorcall(call, R_MSG_ob_nonsub, type2char(mode));
	}
    }
    return result;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP s, SEXP call)
{
    R_xlen_t n;
    int mode;
    R_xlen_t stretch = 1;
    SEXP indx, result, attrib, nattrib;

    if (s == R_MissingArg) return duplicate(x);

    PROTECT(s);
    attrib = getAttrib(x, R_DimSymbol);

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    if (isMatrix(s) && isArray(x) && ncols(s) == length(attrib)) {
	if (isString(s)) {
	    s = strmat2intmat(s, GetArrayDimnames(x), call);
	    UNPROTECT(1);
	    PROTECT(s);
	}
	if (isInteger(s) || isReal(s)) {
	    s = mat2indsub(attrib, s, call);
	    UNPROTECT(1);
	    PROTECT(s);
	}
    }

    /* Convert to a vector of integer subscripts */
    /* in the range 1:length(x). */

    PROTECT(indx = makeSubscript(x, s, &stretch, call));
    n = XLENGTH(indx);

    /* Allocate the result. */

    mode = TYPEOF(x);
    /* No protection needed as ExtractSubset does not allocate */
    result = allocVector(mode, n);
    if (mode == VECSXP || mode == EXPRSXP)
	/* we do not duplicate the values when extracting the subset,
	   so to be conservative mark the result as NAMED = 2 */
	SET_NAMED(result, 2);

    PROTECT(result = ExtractSubset(x, result, indx, call));
    if (result != R_NilValue) {
	if (
	    ((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
	    ( /* here we might have an array.  Use row names if 1D */
		isArray(x) && LENGTH(getAttrib(x, R_DimNamesSymbol)) == 1 &&
		(attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
		(attrib = GetRowNames(attrib)) != R_NilValue
		)
	    ) {
	    PROTECT(attrib);
	    nattrib = allocVector(TYPEOF(attrib), n);
	    PROTECT(nattrib); /* seems unneeded */
	    nattrib = ExtractSubset(attrib, nattrib, indx, call);
	    setAttrib(result, R_NamesSymbol, nattrib);
	    UNPROTECT(2); /* attrib, nattrib */
	}
	if ((attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue &&
	    TYPEOF(attrib) == VECSXP) {
	    nattrib = allocVector(VECSXP, n);
	    PROTECT(nattrib); /* seems unneeded */
	    nattrib = ExtractSubset(attrib, nattrib, indx, call);
	    setAttrib(result, R_SrcrefSymbol, nattrib);
	    UNPROTECT(1);
	}
	/* FIXME:  this is wrong, because the slots are gone, so result is an invalid object of the S4 class! JMC 3/3/09 */
#ifdef _S4_subsettable
	if(IS_S4_OBJECT(x)) { /* e.g. contains = "list" */
	    setAttrib(result, R_ClassSymbol, getAttrib(x, R_ClassSymbol));
	    SET_S4_OBJECT(result);
	}
#endif
    }
    UNPROTECT(3);
    return result;
}

SEXP int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call);


static SEXP MatrixSubset(SEXP x, SEXP s, SEXP call, int drop)
{
    SEXP attr, result, sr, sc, dim;
    int nr, nc, nrs, ncs;
    R_xlen_t i, j, ii, jj, ij, iijj;

    nr = nrows(x);
    nc = ncols(x);

    /* Note that "s" is protected on entry. */
    /* The following ensures that pointers remain protected. */
    dim = getAttrib(x, R_DimSymbol);

    sr = SETCAR(s, int_arraySubscript(0, CAR(s), dim, x, call));
    sc = SETCADR(s, int_arraySubscript(1, CADR(s), dim, x, call));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    /* Check this does not overflow: currently only possible on 32-bit */
    if ((double)nrs * (double)ncs > R_XLEN_T_MAX)
	error(_("dimensions would exceed maximum size of array"));
    PROTECT(sr);
    PROTECT(sc);
    result = allocVector(TYPEOF(x), (R_xlen_t) nrs * (R_xlen_t) ncs);
    PROTECT(result);
    for (i = 0; i < nrs; i++) {
	ii = INTEGER(sr)[i];
	if (ii != NA_INTEGER) {
	    if (ii < 1 || ii > nr)
		errorcall(call, R_MSG_subs_o_b);
	    ii--;
	}
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj != NA_INTEGER) {
		if (jj < 1 || jj > nc)
		    errorcall(call, R_MSG_subs_o_b);
		jj--;
	    }
	    ij = i + j * nrs;
	    if (ii == NA_INTEGER || jj == NA_INTEGER) {
		switch (TYPEOF(x)) {
		case LGLSXP:
		case INTSXP:
		    INTEGER(result)[ij] = NA_INTEGER;
		    break;
		case REALSXP:
		    REAL(result)[ij] = NA_REAL;
		    break;
		case CPLXSXP:
		    COMPLEX(result)[ij].r = NA_REAL;
		    COMPLEX(result)[ij].i = NA_REAL;
		    break;
		case STRSXP:
		    SET_STRING_ELT(result, ij, NA_STRING);
		    break;
		case VECSXP:
		case EXPRSXP:
		    SET_VECTOR_ELT(result, ij, R_NilValue);
		    break;
		case RAWSXP:
		    RAW(result)[ij] = (Rbyte) 0;
		    break;
		default:
		    errorcall(call, _("matrix subscripting not handled for this type"));
		    break;
		}
	    }
	    else {
		iijj = ii + jj * nr;
		switch (TYPEOF(x)) {
		case LGLSXP:
		    LOGICAL(result)[ij] = LOGICAL(x)[iijj];
		    break;
		case INTSXP:
		    INTEGER(result)[ij] = INTEGER(x)[iijj];
		    break;
		case REALSXP:
		    REAL(result)[ij] = REAL(x)[iijj];
		    break;
		case CPLXSXP:
		    COMPLEX(result)[ij] = COMPLEX(x)[iijj];
		    break;
		case STRSXP:
		    SET_STRING_ELT(result, ij, STRING_ELT(x, iijj));
		    break;
		case VECSXP:
		case EXPRSXP:
		    SET_VECTOR_ELT(result, ij, VECTOR_ELT_FIX_NAMED(x, iijj));
		    break;
		case RAWSXP:
		    RAW(result)[ij] = RAW(x)[iijj];
		    break;
		default:
		    errorcall(call, _("matrix subscripting not handled for this type"));
		    break;
		}
	    }
	}
    }
    if(nrs >= 0 && ncs >= 0) {
	PROTECT(attr = allocVector(INTSXP, 2));
	INTEGER(attr)[0] = nrs;
	INTEGER(attr)[1] = ncs;
	setAttrib(result, R_DimSymbol, attr);
	UNPROTECT(1);
    }

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.	 Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    if (nrs >= 0 && ncs >= 0) {
	SEXP dimnames, dimnamesnames, newdimnames;
	dimnames = getAttrib(x, R_DimNamesSymbol);
	PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
	if (!isNull(dimnames)) {
	    PROTECT(newdimnames = allocVector(VECSXP, 2));
	    if (TYPEOF(dimnames) == VECSXP) {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(VECTOR_ELT(dimnames, 0),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(VECTOR_ELT(dimnames, 1),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    else {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(CAR(dimnames),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(CADR(dimnames),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(result, R_DimNamesSymbol, newdimnames);
	    UNPROTECT(1); /* newdimnames */
	}
	UNPROTECT(1); /* dimnamesnames */
    }
    /*  Probably should not do this:
    copyMostAttrib(x, result); */
    if (drop)
	DropDims(result);
    UNPROTECT(3);
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
    int k, mode;
    SEXP dimnames, dimnamesnames, p, q, r, result, xdims;
    const void *vmaxsave = vmaxget();

    mode = TYPEOF(x);
    xdims = getAttrib(x, R_DimSymbol);
    k = length(xdims);

    /* k is now the number of dims */
    int **subs = (int**)R_alloc(k, sizeof(int*));
    int *indx = (int*)R_alloc(k, sizeof(int));
    int *bound = (int*)R_alloc(k, sizeof(int));
    R_xlen_t *offset = (R_xlen_t*)R_alloc(k, sizeof(R_xlen_t));

    /* Construct a vector to contain the returned values. */
    /* Store its extents. */

    R_xlen_t n = 1;
    r = s;
    for (int i = 0; i < k; i++) {
	SETCAR(r, int_arraySubscript(i, CAR(r), xdims, x, call));
	bound[i] = LENGTH(CAR(r));
	n *= bound[i];
	r = CDR(r);
    }
    PROTECT(result = allocVector(mode, n));
    r = s;
    for (int i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER(CAR(r));
	r = CDR(r);
    }
    offset[0] = 1;
    for (int i = 1; i < k; i++)
	offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];

    /* Transfer the subset elements from "x" to "a". */

    for (R_xlen_t i = 0; i < n; i++) {
	R_xlen_t ii = 0;
	for (int j = 0; j < k; j++) {
	    int jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) {
		ii = NA_INTEGER;
		goto assignLoop;
	    }
	    if (jj < 1 || jj > INTEGER(xdims)[j])
		errorcall(call, R_MSG_subs_o_b);
	    ii += (jj - 1) * offset[j];
	}

      assignLoop:
	switch (mode) {
	case LGLSXP:
	    if (ii != NA_INTEGER)
		LOGICAL(result)[i] = LOGICAL(x)[ii];
	    else
		LOGICAL(result)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    if (ii != NA_INTEGER)
		INTEGER(result)[i] = INTEGER(x)[ii];
	    else
		INTEGER(result)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (ii != NA_INTEGER)
		REAL(result)[i] = REAL(x)[ii];
	    else
		REAL(result)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    if (ii != NA_INTEGER) {
		COMPLEX(result)[i] = COMPLEX(x)[ii];
	    }
	    else {
		COMPLEX(result)[i].r = NA_REAL;
		COMPLEX(result)[i].i = NA_REAL;
	    }
	    break;
	case STRSXP:
	    if (ii != NA_INTEGER)
		SET_STRING_ELT(result, i, STRING_ELT(x, ii));
	    else
		SET_STRING_ELT(result, i, NA_STRING);
	    break;
	case VECSXP:
	case EXPRSXP:
	    if (ii != NA_INTEGER)
		SET_VECTOR_ELT(result, i, VECTOR_ELT_FIX_NAMED(x, ii));
	    else
		SET_VECTOR_ELT(result, i, R_NilValue);
	    break;
	case RAWSXP:
	    if (ii != NA_INTEGER)
		RAW(result)[i] = RAW(x)[ii];
	    else
		RAW(result)[i] = (Rbyte) 0;
	    break;
	default:
	    errorcall(call, _("array subscripting not handled for this type"));
	    break;
	}
	if (n > 1) {
	    int j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % k;
	    }
	}
    }

    PROTECT(xdims = allocVector(INTSXP, k));
    for(int i = 0 ; i < k ; i++)
	INTEGER(xdims)[i] = bound[i];
    setAttrib(result, R_DimSymbol, xdims);
    UNPROTECT(1); /* xdims */

    /* The array elements have been transferred. */
    /* Now we need to transfer the attributes. */
    /* Most importantly, we need to subset the */
    /* dimnames of the returned value. */

    dimnames = getAttrib(x, R_DimNamesSymbol);
    PROTECT(dimnamesnames = getAttrib(dimnames, R_NamesSymbol));
    if (dimnames != R_NilValue) {
	int j = 0;
	PROTECT(xdims = allocVector(VECSXP, k));
	if (TYPEOF(dimnames) == VECSXP) {
	    r = s;
	    for (int i = 0; i < k ; i++) {
		if (bound[i] > 0) {
		  SET_VECTOR_ELT(xdims, j++,
			ExtractSubset(VECTOR_ELT(dimnames, i),
				      allocVector(STRSXP, bound[i]),
				      CAR(r), call));
		} else { /* 0-length dims have NULL dimnames */
		    SET_VECTOR_ELT(xdims, j++, R_NilValue);
		}
		r = CDR(r);
	    }
	}
	else {
	    p = dimnames;
	    q = xdims;
	    r = s;
	    for(int i = 0 ; i < k; i++) {
		SETCAR(q, allocVector(STRSXP, bound[i]));
		SETCAR(q, ExtractSubset(CAR(p), CAR(q), CAR(r), call));
		p = CDR(p);
		q = CDR(q);
		r = CDR(r);
	    }
	}
	setAttrib(xdims, R_NamesSymbol, dimnamesnames);
	setAttrib(result, R_DimNamesSymbol, xdims);
	UNPROTECT(1); /* xdims */
    }
    /* This was removed for matrices in 1998
       copyMostAttrib(x, result); */
    /* Free temporary memory */
    vmaxset(vmaxsave);
    if (drop)
	DropDims(result);
    UNPROTECT(2); /* dimnamesnames, result */
    return result;
}


/* Returns and removes a named argument from argument list args.
   The search ends as soon as a matching argument is found.  If
   the argument is not found, the argument list is not modified
   and R_NilValue is returned.
 */
static SEXP ExtractArg(SEXP args, SEXP arg_sym)
{
    SEXP arg, prev_arg;
    int found = 0;

    for (arg = prev_arg = args; arg != R_NilValue; arg = CDR(arg)) {
	if(TAG(arg) == arg_sym) {
	    if (arg == prev_arg) /* found at head of args */
		args = CDR(args);
	    else
		SETCDR(prev_arg, CDR(arg));
	    found = 1;
	    break;
	}
	else  prev_arg = arg;
    }
    return found ? CAR(arg) : R_NilValue;
}

/* Extracts the drop argument, if present, from the argument list.
   The object being subsetted must be the first argument. */
static void ExtractDropArg(SEXP el, int *drop)
{
    SEXP dropArg = ExtractArg(el, R_DropSymbol);
    *drop = asLogical(dropArg);
    if (*drop == NA_LOGICAL) *drop = 1;
}


/* Extracts and, if present, removes the 'exact' argument from the
   argument list.  An integer code giving the desired exact matching
   behavior is returned:
       0  not exact
       1  exact
      -1  not exact, but warn when partial matching is used
 */
static int ExtractExactArg(SEXP args)
{
    SEXP argval = ExtractArg(args, R_ExactSymbol);
    int exact;
    if(isNull(argval)) return 1; /* Default is true as from R 2.7.0 */
    exact = asLogical(argval);
    if (exact == NA_LOGICAL) exact = -1;
    return exact;
}

/* Version of DispatchOrEval for "[" and friends that speeds up simple cases.
   Also defined in subassign.c */
static R_INLINE
int R_DispatchOrEvalSP(SEXP call, SEXP op, const char *generic, SEXP args,
		    SEXP rho, SEXP *ans)
{
    SEXP prom = NULL;
    if (args != R_NilValue && CAR(args) != R_DotsSymbol) {
	SEXP x = eval(CAR(args), rho);
	PROTECT(x);
	if (! OBJECT(x)) {
	    *ans = CONS_NR(x, evalListKeepMissing(CDR(args), rho));
	    UNPROTECT(1);
	    return FALSE;
	}
	prom = mkPROMISE(CAR(args), R_GlobalEnv);
	SET_PRVALUE(prom, x);
	args = CONS(prom, CDR(args));
	UNPROTECT(1);
    }
    PROTECT(args);
    int disp = DispatchOrEval(call, op, generic, args, rho, ans, 0, 0);
    if (prom) DECREMENT_REFCNT(PRVALUE(prom));
    UNPROTECT(1);
    return disp;
}

/* The "[" subset operator.
 * This provides the most general form of subsetting. */

SEXP attribute_hidden do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is an */
    /* approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall through */
    /* to the generic code below.  Note that evaluation */
    /* retains any missing argument indicators. */

    if(R_DispatchOrEvalSP(call, op, "[", args, rho, &ans)) {
/*     if(DispatchAnyOrEval(call, op, "[", args, rho, &ans, 0, 0)) */
	if (NAMED(ans))
	    SET_NAMED(ans, 2);
	return(ans);
    }

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    return do_subset_dflt(call, op, ans, rho);
}

static R_INLINE R_xlen_t scalarIndex(SEXP s)
{
    if (ATTRIB(s) == R_NilValue)
	switch (TYPEOF(s)) {
	case REALSXP: // treat infinite indices as NA, like asInteger
	    if (XLENGTH(s) == 1 && R_FINITE(REAL(s)[0]))
		return (R_xlen_t) REAL(s)[0];
	    else return -1;
	case INTSXP:
	    if (XLENGTH(s) == 1 && INTEGER(s)[0] != NA_INTEGER)
		return INTEGER(s)[0];
	    else return -1;
	default: return -1;
	}
    else return -1;
}

SEXP attribute_hidden do_subset_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ax, px, x, subs;
    int drop, i, nsubs, type;

    /* By default we drop extents of length 1 */

    /* Handle cases of extracting a single element from a simple vector
       or matrix directly to improve speed for these simple cases. */
    SEXP cdrArgs = CDR(args);
    SEXP cddrArgs = CDR(cdrArgs);
    if (cdrArgs != R_NilValue && cddrArgs == R_NilValue &&
	TAG(cdrArgs) == R_NilValue) {
	/* one index, not named */
	SEXP x = CAR(args);
	if (ATTRIB(x) == R_NilValue) {
	    SEXP s = CAR(cdrArgs);
	    R_xlen_t i = scalarIndex(s);
	    switch (TYPEOF(x)) {
	    case REALSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarReal( REAL(x)[i-1] );
		break;
	    case INTSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarInteger( INTEGER(x)[i-1] );
		break;
	    case LGLSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarLogical( LOGICAL(x)[i-1] );
		break;
//	    do the more rare cases as well, since we've already prepared everything:
	    case CPLXSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarComplex( COMPLEX(x)[i-1] );
		break;
	    case RAWSXP:
		if (i >= 1 && i <= XLENGTH(x))
		    return ScalarRaw( RAW(x)[i-1] );
		break;
	    default: break;
	    }
	}
    }
    else if (cddrArgs != R_NilValue && CDR(cddrArgs) == R_NilValue &&
	     TAG(cdrArgs) == R_NilValue && TAG(cddrArgs) == R_NilValue) {
	/* two indices, not named */
	SEXP x = CAR(args);
	SEXP attr = ATTRIB(x);
	if (TAG(attr) == R_DimSymbol && CDR(attr) == R_NilValue) {
	    /* only attribute of x is 'dim' */
	    SEXP dim = CAR(attr);
	    if (TYPEOF(dim) == INTSXP && LENGTH(dim) == 2) {
		/* x is a matrix */
		SEXP si = CAR(cdrArgs);
		SEXP sj = CAR(cddrArgs);
		R_xlen_t i = scalarIndex(si);
		R_xlen_t j = scalarIndex(sj);
		int nrow = INTEGER(dim)[0];
		int ncol = INTEGER(dim)[1];
		if (i > 0 && j > 0 && i <= nrow && j <= ncol) {
		    /* indices are legal scalars */
		    R_xlen_t k = i - 1 + nrow * (j - 1);
		    switch (TYPEOF(x)) {
		    case REALSXP:
			if (k < LENGTH(x))
			    return ScalarReal( REAL(x)[k] );
			break;
		    case INTSXP:
			if (k < LENGTH(x))
			    return ScalarInteger( INTEGER(x)[k] );
			break;
		    case LGLSXP:
			if (k < LENGTH(x))
			    return ScalarLogical( LOGICAL(x)[k] );
			break;
		    case CPLXSXP:
			if (k < LENGTH(x))
			    return ScalarComplex( COMPLEX(x)[k] );
			break;
		    case RAWSXP:
			if (k < LENGTH(x))
			    return ScalarRaw( RAW(x)[k] );
			break;
		    default: break;
		    }
		}
	    }
	}
    }

    PROTECT(args);

    drop = 1;
    ExtractDropArg(args, &drop);
    x = CAR(args);

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */
    /* FIXME: replace the test by isNull ... ? */

    if (x == R_NilValue) {
	UNPROTECT(1);
	return x;
    }
    subs = CDR(args);
    nsubs = length(subs); /* Will be short */
    type = TYPEOF(x);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    ax = x;
    if (isVector(x))
	PROTECT(ax);
    else if (isPairList(x)) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	if (ndim > 1) {
	    PROTECT(ax = allocArray(VECSXP, dim));
	    setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    PROTECT(ax = allocVector(VECSXP, length(x)));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
	for(px = x, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    if(nsubs < 2) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	PROTECT(ans = VectorSubset(ax, (nsubs == 1 ? CAR(subs) : R_MissingArg),
				   call));
	/* one-dimensional arrays went through here, and they should
	   have their dimensions dropped only if the result has
	   length one and drop == TRUE
	*/
	if(ndim == 1) {
	    SEXP attr, attrib, nattrib;
	    int len = length(ans);

	    if(!drop || len > 1) {
		// must grab these before the dim is set.
		SEXP nm = PROTECT(getAttrib(ans, R_NamesSymbol));
		PROTECT(attr = allocVector(INTSXP, 1));
		INTEGER(attr)[0] = length(ans);
		setAttrib(ans, R_DimSymbol, attr);
		if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
		    /* reinstate dimnames, include names of dimnames */
		    PROTECT(nattrib = duplicate(attrib));
		    SET_VECTOR_ELT(nattrib, 0, nm);
		    setAttrib(ans, R_DimNamesSymbol, nattrib);
		    setAttrib(ans, R_NamesSymbol, R_NilValue);
		    UNPROTECT(1);
		}
		UNPROTECT(2);
	    }
	}
    } else {
	if (nsubs != length(getAttrib(x, R_DimSymbol)))
	    errorcall(call, _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, subs, call, drop);
	else
	    ans = ArraySubset(ax, subs, call, drop);
	PROTECT(ans);
    }

    /* Note: we do not coerce back to pair-based lists. */
    /* They are "defunct" in this version of R. */

    if (type == LANGSXP) {
	ax = ans;
	PROTECT(ans = allocList(LENGTH(ax)));
	if ( LENGTH(ax) > 0 )
	    SET_TYPEOF(ans, LANGSXP);
	for(px = ans, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SETCAR(px, VECTOR_ELT(ax, i++));
	setAttrib(ans, R_DimSymbol, getAttrib(ax, R_DimSymbol));
	setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
	setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
	SET_NAMED(ans, NAMED(ax)); /* PR#7924 */
    }
    else {
	PROTECT(ans);
    }
    if (ATTRIB(ans) != R_NilValue) { /* remove probably erroneous attr's */
	setAttrib(ans, R_TspSymbol, R_NilValue);
#ifdef _S4_subsettable
	if(!IS_S4_OBJECT(x))
#endif
	    setAttrib(ans, R_ClassSymbol, R_NilValue);
    }
    UNPROTECT(4);
    return ans;
}


/* The [[ subset operator.  It needs to be fast. */
/* The arguments to this call are evaluated on entry. */

SEXP attribute_hidden do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(R_DispatchOrEvalSP(call, op, "[[", args, rho, &ans)) {
	if (NAMED(ans))
	    SET_NAMED(ans, 2);
	return(ans);
    }

    /* Method dispatch has failed. */
    /* We now run the generic internal code. */

    return do_subset2_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, dimnames, indx, subs, x;
    int i, ndims, nsubs;
    int drop = 1, pok, exact = -1;
    int named_x;
    R_xlen_t offset = 0;

    PROTECT(args);
    ExtractDropArg(args, &drop);
    /* Is partial matching ok?  When the exact arg is NA, a warning is
       issued if partial matching occurs.
     */
    exact = ExtractExactArg(args);
    if (exact == -1)
	pok = exact;
    else
	pok = !exact;

    x = CAR(args);

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */

    if (x == R_NilValue) {
	UNPROTECT(1); /* args */
	return x;
    }

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    subs = CDR(args);
    if(0 == (nsubs = length(subs)))
	errorcall(call, _("no index specified"));
    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    if(nsubs > 1 && nsubs != ndims)
	errorcall(call, _("incorrect number of subscripts"));

    /* code to allow classes to extend environment */
    if(TYPEOF(x) == S4SXP) {
	x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	  errorcall(call, _("this S4 class is not subsettable"));
    }
    PROTECT(x);

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
	if( nsubs != 1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	    errorcall(call, _("wrong arguments for subsetting an environment"));
	ans = findVarInFrame(x, installTrChar(STRING_ELT(CAR(subs), 0)));
	if( TYPEOF(ans) == PROMSXP ) {
	    PROTECT(ans);
	    ans = eval(ans, R_GlobalEnv);
	    UNPROTECT(1); /* ans */
	} else SET_NAMED(ans, 2);

	UNPROTECT(2); /* args, x */
	if(ans == R_UnboundValue)
	    return(R_NilValue);
	if (NAMED(ans))
	    SET_NAMED(ans, 2);
	return ans;
    }

    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    named_x = NAMED(x);  /* x may change below; save this now.  See PR#13411 */

    if(nsubs == 1) { /* vector indexing */
	SEXP thesub = CAR(subs);
	int len = length(thesub);

	if (len > 1) {
#ifdef SWITCH_TO_REFCNT
	    if (IS_GETTER_CALL(call)) {
		/* this is (most likely) a getter call in a complex
		   assighment so we duplicate as needed. The original
		   x should have been duplicated if it might be
		   shared */
		if (MAYBE_SHARED(x))
		    error("getter call used outside of a complex assignment.");
		x = vectorIndex(x, thesub, 0, len-1, pok, call, TRUE);
	    }
	    else
		x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
#else
	    x = vectorIndex(x, thesub, 0, len-1, pok, call, FALSE);
#endif
	    named_x = NAMED(x);
	    UNPROTECT(1); /* x */
	    PROTECT(x);
	}

	SEXP xnames = PROTECT(getAttrib(x, R_NamesSymbol));
	offset = get1index(thesub, xnames,
			   xlength(x), pok, len > 1 ? len-1 : -1, call);
	UNPROTECT(1); /* xnames */
	if (offset < 0 || offset >= xlength(x)) {
	    /* a bold attempt to get the same behaviour for $ and [[ */
	    if (offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(2); /* args, x */
		return R_NilValue;
	    }
	    else errorcall(call, R_MSG_subs_o_b);
	}
    } else { /* matrix indexing */
	/* Here we use the fact that: */
	/* CAR(R_NilValue) = R_NilValue */
	/* CDR(R_NilValue) = R_NilValue */

	int ndn; /* Number of dimnames. Unlikely to be anything but
		    0 or nsubs, but just in case... */

	PROTECT(indx = allocVector(INTSXP, nsubs));
	dimnames = getAttrib(x, R_DimNamesSymbol);
	ndn = length(dimnames);
	for (i = 0; i < nsubs; i++) {
	    INTEGER(indx)[i] = (int)
		get1index(CAR(subs),
			  (i < ndn) ? VECTOR_ELT(dimnames, i) : R_NilValue,
			  INTEGER(indx)[i], pok, -1, call);
	    subs = CDR(subs);
	    if (INTEGER(indx)[i] < 0 ||
		INTEGER(indx)[i] >= INTEGER(dims)[i])
		errorcall(call, R_MSG_subs_o_b);
	}
	offset = 0;
	for (i = (nsubs - 1); i > 0; i--)
	    offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	offset += INTEGER(indx)[0];
	UNPROTECT(1); /* indx */
    }

    if(isPairList(x)) {
#ifdef LONG_VECTOR_SUPPORT
	if (offset > R_SHORT_LEN_MAX)
	    error("invalid subscript for pairlist");
#endif
	ans = CAR(nthcdr(x, (int) offset));
	if (named_x > NAMED(ans))
	    SET_NAMED(ans, named_x);
    } else if(isVectorList(x)) {
	/* did unconditional duplication before 2.4.0 */
	ans = VECTOR_ELT(x, offset);
	if (named_x > NAMED(ans))
	    SET_NAMED(ans, named_x);
    } else {
	ans = allocVector(TYPEOF(x), 1);
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    INTEGER(ans)[0] = INTEGER(x)[offset];
	    break;
	case REALSXP:
	    REAL(ans)[0] = REAL(x)[offset];
	    break;
	case CPLXSXP:
	    COMPLEX(ans)[0] = COMPLEX(x)[offset];
	    break;
	case STRSXP:
	    SET_STRING_ELT(ans, 0, STRING_ELT(x, offset));
	    break;
	case RAWSXP:
	    RAW(ans)[0] = RAW(x)[offset];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("do_subset2", x);
	}
    }
    UNPROTECT(2); /* args, x */
    return ans;
}


enum pmatch {
    NO_MATCH,
    EXACT_MATCH,
    PARTIAL_MATCH
};

/* A helper to partially match tags against a candidate.
   Tags are always in the native charset.
 */
/* Returns: */
static
enum pmatch
pstrmatch(SEXP target, SEXP input, size_t slen)
{
    const char *st = "";
    const void *vmax = vmaxget();

    if(target == R_NilValue)
	return NO_MATCH;

    switch (TYPEOF(target)) {
    case SYMSXP:
	st = CHAR(PRINTNAME(target));
	break;
    case CHARSXP:
	st = translateChar(target);
	break;
    }
    if(strncmp(st, translateChar(input), slen) == 0) {
	vmaxset(vmax);
	return (strlen(st) == slen) ?  EXACT_MATCH : PARTIAL_MATCH;
    } else {
	vmaxset(vmax);
	return NO_MATCH;
    }
}


/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
SEXP attribute_hidden do_subset3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP input, nlist, ans;

    checkArity(op, args);

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */
    input = PROTECT(allocVector(STRSXP, 1));

    nlist = CADR(args);
    if(isSymbol(nlist) )
	SET_STRING_ELT(input, 0, PRINTNAME(nlist));
    else if(isString(nlist) )
	SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
    else {
	errorcall(call,_("invalid subscript type '%s'"),
		  type2char(TYPEOF(nlist)));
    }

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when nlist was */
    /* ..., as in PR#8718 */
    PROTECT(args = CONS(CAR(args), CONS(input, R_NilValue)));

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(R_DispatchOrEvalSP(call, op, "$", args, env, &ans)) {
	UNPROTECT(2);
	if (NAMED(ans))
	    SET_NAMED(ans, 2);
	return(ans);
    }

    UNPROTECT(2);
    return R_subset3_dflt(CAR(ans), STRING_ELT(input, 0), call);
}

/* used in eval.c */
SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input, SEXP call)
{
    SEXP y, nlist;
    size_t slen;

    PROTECT(input);
    PROTECT(x);

    /* Optimisation to prevent repeated recalculation */
    slen = strlen(translateChar(input));
     /* The mechanism to allow a class extending "environment" */
    if( IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP ){
	x = R_getS4DataSlot(x, ANYSXP);
	if(x == R_NilValue)
	    errorcall(call, "$ operator not defined for this S4 class");
    }
    UNPROTECT(1); /* x */
    PROTECT(x);

    /* If this is not a list object we return NULL. */

    if (isPairList(x)) {
	SEXP xmatch = R_NilValue;
	int havematch;
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
	    switch(pstrmatch(TAG(y), input, slen)) {
	    case EXACT_MATCH:
		y = CAR(y);
		if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
		UNPROTECT(2); /* input, x */
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		xmatch = y;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if (havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = TAG(xmatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = translateChar(target);
		    break;
		}
		warningcall(call, _("partial match of '%s' to '%s'"),
			    translateChar(input), st);
	    }
	    y = CAR(xmatch);
	    if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
	    UNPROTECT(2); /* input, x */
	    return y;
	}
	UNPROTECT(2); /* input, x */
	return R_NilValue;
    }
    else if (isVectorList(x)) {
	R_xlen_t i, n, imatch = -1;
	int havematch;
	nlist = getAttrib(x, R_NamesSymbol);

	n = xlength(nlist);
	havematch = 0;
	for (i = 0 ; i < n ; i = i + 1) {
	    switch(pstrmatch(STRING_ELT(nlist, i), input, slen)) {
	    case EXACT_MATCH:
		y = VECTOR_ELT(x, i);
		if (NAMED(x) > NAMED(y))
		    SET_NAMED(y, NAMED(x));
		UNPROTECT(2); /* input, x */
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		if (havematch == 1) {
		    /* partial matches can cause aliasing in eval.c:evalseq
		       This is overkill, but alternative ways to prevent
		       the aliasing appear to be even worse */
		    y = VECTOR_ELT(x,i);
		    SET_NAMED(y,2);
		    SET_VECTOR_ELT(x,i,y);
		}
		imatch = i;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if(havematch == 1) { /* unique partial match */
	    if(R_warn_partial_match_dollar) {
		const char *st = "";
		SEXP target = STRING_ELT(nlist, imatch);
		switch (TYPEOF(target)) {
		case SYMSXP:
		    st = CHAR(PRINTNAME(target));
		    break;
		case CHARSXP:
		    st = translateChar(target);
		    break;
		}
		warningcall(call, _("partial match of '%s' to '%s'"),
			    translateChar(input), st);
	    }
	    y = VECTOR_ELT(x, imatch);
	    if (NAMED(x) > NAMED(y)) SET_NAMED(y, NAMED(x));
	    UNPROTECT(2); /* input, x */
	    return y;
	}
	UNPROTECT(2); /* input, x */
	return R_NilValue;
    }
    else if( isEnvironment(x) ){
	y = findVarInFrame(x, installTrChar(input));
	if( TYPEOF(y) == PROMSXP ) {
	    PROTECT(y);
	    y = eval(y, R_GlobalEnv);
	    UNPROTECT(1); /* y */
	}
	UNPROTECT(2); /* input, x */
	if( y != R_UnboundValue ) {
	    if (NAMED(y))
		SET_NAMED(y, 2);
	    else if (NAMED(x) > NAMED(y))
		SET_NAMED(y, NAMED(x));
	    return(y);
	}
	return R_NilValue;
    }
    else if( isVectorAtomic(x) ){
	errorcall(call, "$ operator is invalid for atomic vectors");
    }
    else /* e.g. a function */
	errorcall(call, R_MSG_ob_nonsub, type2char(TYPEOF(x)));
    UNPROTECT(2); /* input, x */
    return R_NilValue;
}
