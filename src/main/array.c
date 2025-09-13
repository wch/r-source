/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2025   The R Core Team
 *  Copyright (C) 2002-2025   The R Foundation
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <R_ext/RS.h>     /* for R_Calloc/R_Free, F77_CALL */

// calls BLAS routines dgemm dgemv zgemm
#ifdef USE_NEW_ACCELERATE
# define ACCELERATE_NEW_LAPACK
// avoid conflicts over COMPLEX
# define USE_NON_APPLE_STANDARD_DATATYPES 0
# include <Accelerate/Accelerate.h>
# define FCONE
# pragma clang diagnostic ignored "-Wincompatible-pointer-types"
#else
#include <R_ext/BLAS.h>
#endif
#include <R_ext/Itermacros.h>

#ifdef Win32
#include <trioremap.h> /* for %lld */
#endif

#include "duplicate.h"

#include <complex.h>
#include "Rcomplex.h"	/* toC99 */

/* "GetRowNames" and "GetColNames" are utility routines which
 * locate and return the row names and column names from the
 * dimnames attribute of a matrix.  They are useful because
 * old versions of R used pair-based lists for dimnames
 * whereas recent versions use vector based lists.

 * These are now very old, plus
 * ``When the "dimnames" attribute is
 *   grabbed off an array it is always adjusted to be a vector.''

 They are used in bind.c and subset.c, and advertised in Rinternals.h
*/
SEXP GetRowNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 0);
    else
	return R_NilValue;
}

SEXP GetColNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 1);
    else
	return R_NilValue;
}

// .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow), missing(ncol)))
attribute_hidden SEXP do_matrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, ans, snr, snc, dimnames;
    int nr = 1, nc = 1, byrow0, miss_nr, miss_nc;
    R_xlen_t lendat;

    checkArity(op, args);
    vals = CAR(args); args = CDR(args);
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    error(_("'data' must be of a vector type, was '%s'"),
		R_typeToChar(vals));
    }
    lendat = XLENGTH(vals);
    snr = CAR(args); args = CDR(args);
    snc = CAR(args); args = CDR(args);
    byrow0 = asLogical(CAR(args)); args = CDR(args);
    if (byrow0 == NA_INTEGER)
	error(_("invalid '%s' argument"), "byrow");
    bool byrow = (bool) byrow0;
    dimnames = CAR(args);
    args = CDR(args);
    miss_nr = asLogical(CAR(args)); args = CDR(args);
    miss_nc = asLogical(CAR(args));

    static int nowarn = -1;
    if (nowarn == -1) {
	char *p = getenv("_R_CHECK_MATRIX_DATA_");
	nowarn = (p && StringTrue(p)) ? 1 : 0; // if(nowarn) <error>
    }
    if (!miss_nr) {
	if (!isNumeric(snr)) error(_("non-numeric matrix extent"));
	nr = asInteger(snr);
	if (nr == NA_INTEGER)
	    error(_("invalid 'nrow' value (too large or NA)"));
	if (nr < 0)
	    error(_("invalid 'nrow' value (< 0)"));
    }
    if (!miss_nc) {
	if (!isNumeric(snc)) error(_("non-numeric matrix extent"));
	nc = asInteger(snc);
	if (nc == NA_INTEGER)
	    error(_("invalid 'ncol' value (too large or NA)"));
	if (nc < 0)
	    error(_("invalid 'ncol' value (< 0)"));
    }
    if (miss_nr && miss_nc) {
	if (lendat > INT_MAX) error("data is too long");
	nr = (int) lendat;
    } else if (miss_nr) {
	if (lendat > (double) nc * INT_MAX) error("data is too long"); // incl lendat > nc == 0
	if (nc == 0) // as lendat <= nc, have lendat == 0
	    nr = 0;
	else
	    nr = (int) ceil((double) lendat / (double) nc);
    } else if (miss_nc) {
	if (lendat > (double) nr * INT_MAX) error("data is too long"); // incl lendat > nr == 0
	if (nr == 0) // then lendat == 0
	    nc = 0;
	else
	    nc = (int) ceil((double) lendat / (double) nr);
    }

    if (lendat > 0) {
	R_xlen_t nrc = (R_xlen_t) nr * nc;
	if (lendat > 1 && (nrc % lendat) != 0) { // ==> nrc > 0
	    if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
		((lendat < nr) && (nr / lendat) * lendat != nr))
		warning(_("data length [%lld] is not a sub-multiple or multiple of the number of rows [%d]"),
			(long long)lendat, nr);
	    else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		     ((lendat < nc) && (nc / lendat) * lendat != nc))
		warning(_("data length [%lld] is not a sub-multiple or multiple of the number of columns [%d]"),
			(long long)lendat, nc);
	    else if (nrc != lendat) {
		if(nowarn)
		    error(_("data length differs from size of matrix: [%lld != %d x %d]"),
			  (long long)lendat, nr, nc);
		else
		    warning(_("data length differs from size of matrix: [%lld != %d x %d]"),
			    (long long)lendat, nr, nc);
	    }
	}
	else if (lendat > 1 && nrc == 0) // for now *not* warning for e.g., matrix(NA, 0, 4)
	    warning(_("non-empty data for zero-extent matrix"));
    }

#ifndef LONG_VECTOR_SUPPORT
    if ((double)nr * (double)nc > INT_MAX)
	error(_("too many elements specified"));
#endif

    PROTECT(ans = allocMatrix(TYPEOF(vals), nr, nc));
    if(lendat) {
	if (isVector(vals))
	    copyMatrix(ans, vals, byrow);
	else
	    copyListMatrix(ans, vals, byrow);
    } else if (isVector(vals)) { /* fill with NAs */
	R_xlen_t N = (R_xlen_t) nr * nc, i;
	switch(TYPEOF(vals)) {
	case STRSXP:
	    for (i = 0; i < N; i++)
		SET_STRING_ELT(ans, i, NA_STRING);
	    break;
	case LGLSXP:
	    for (i = 0; i < N; i++)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    for (i = 0; i < N; i++)
		INTEGER(ans)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    for (i = 0; i < N; i++)
		REAL(ans)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    {
		Rcomplex na_cmplx;
		na_cmplx.r = NA_REAL;
		na_cmplx.i = 0;
		for (i = 0; i < N; i++)
		    COMPLEX(ans)[i] = na_cmplx;
	    }
	    break;
	case RAWSXP:
	    if (N) memset(RAW(ans), 0, N);
	    break;
	default:
	    /* don't fill with anything */
	    ;
	}
    }
    if(!isNull(dimnames) && length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);
    UNPROTECT(1);
    return ans;
}


SEXP allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0)
	error(_("negative extents to matrix"));
#ifndef LONG_VECTOR_SUPPORT
    if ((double)nrow * (double)ncol > INT_MAX)
	error(_("allocMatrix: too many elements specified"));
#endif
    n = ((R_xlen_t) nrow) * ncol;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 2));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}

/**
 * Allocate a 3-dimensional array
 *
 * @param mode The R mode (e.g. INTSXP)
 * @param nrow number of rows
 * @param ncol number of columns
 * @param nface number of faces
 *
 * @return A 3-dimensional array of the indicated dimensions and mode
 */
SEXP alloc3DArray(SEXPTYPE mode, int nrow, int ncol, int nface)
{
    SEXP s, t;
    R_xlen_t n;

    if (nrow < 0 || ncol < 0 || nface < 0)
	error(_("negative extents to 3D array"));
#ifndef LONG_VECTOR_SUPPORT
    if ((double)nrow * (double)ncol * (double)nface > INT_MAX)
	error(_("'alloc3DArray': too many elements specified"));
#endif
    n = ((R_xlen_t) nrow) * ncol * nface;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 3));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    INTEGER(t)[2] = nface;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}


SEXP allocArray(SEXPTYPE mode, SEXP dims)
{
    SEXP array;
    int i;
    R_xlen_t n = 1;
#ifndef LONG_VECTOR_SUPPORT
    double dn = 1;
#endif

    for (i = 0; i < LENGTH(dims); i++) {
#ifndef LONG_VECTOR_SUPPORT
	dn *= INTEGER(dims)[i];
	if(dn > INT_MAX)
	    error(_("'allocArray': too many elements specified by 'dims'"));
#endif
	n *= INTEGER(dims)[i];
    }

    PROTECT(dims = duplicate(dims));
    PROTECT(array = allocVector(mode, n));
    setAttrib(array, R_DimSymbol, dims);
    UNPROTECT(2);
    return array;
}

/* DropDims strips away redundant dimensioning information. */
/* If there is an appropriate dimnames attribute the correct */
/* element is extracted and attached to the vector as a names */
/* attribute.  Note that this function mutates x. */
/* Duplication should occur before this is called. */

attribute_hidden SEXP DropDims(SEXP x)
{
    PROTECT(x);
    SEXP dims = getAttrib(x, R_DimSymbol);

    /* Check that dropping will actually do something. */
    /* (1) Check that there is a "dim" attribute. */

    if (dims == R_NilValue) {
	UNPROTECT(1); /* x */
	return x;
    }

    int ndims = LENGTH(dims);
    int *dim = INTEGER(dims); // used several times

    /* (2) Check whether there are redundant extents */
    int i, n = 0;
    for (i = 0; i < ndims; i++)
	if (dim[i] != 1) n++;
    if (n == ndims) {
	UNPROTECT(1); /* x */
	return x;
    }

    SEXP dimnames = PROTECT(getAttrib(x, R_DimNamesSymbol)),
	newnames = R_NilValue;
    if (n <= 1) {
	/* We have reduced to a vector result.
	   If that has length one, it is ambiguous which dimnames to use,
	   so use it if there is only one (as from R 2.7.0).
	 */
	if (dimnames != R_NilValue) {
	    if(XLENGTH(x) != 1) {
		for (i = 0; i < LENGTH(dims); i++) {
		    if (dim[i] != 1) {
			newnames = VECTOR_ELT(dimnames, i);
			break;
		    }
		}
	    } else { /* drop all dims: keep names if unambiguous */
		int cnt;
		for(i = 0, cnt = 0; i < LENGTH(dims); i++)
		    if(VECTOR_ELT(dimnames, i) != R_NilValue) cnt++;
		if(cnt == 1)
		    for (i = 0; i < LENGTH(dims); i++) {
			newnames = VECTOR_ELT(dimnames, i);
			if(newnames != R_NilValue) break;
		    }
	    }
	}
	PROTECT(newnames);
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, R_NilValue);
	setAttrib(x, R_NamesSymbol, newnames);
	/* FIXME: the following is desirable, but pointless as long as
	   subset.c & others have a contrary version that leaves the
	   S4 class in, incorrectly, in the case of vectors.  JMC
	   3/3/09 */
/*	if(IS_S4_OBJECT(x)) {/\* no longer valid subclass of array or
	matrix *\/ */
/*	    setAttrib(x, R_ClassSymbol, R_NilValue); */
/*	    UNSET_S4_OBJECT(x); */
/*	} */
	UNPROTECT(1); /* newnames */
    } else {
	// We have a lower dimensional array, and  n == length(newdims)
	SEXP newdims, dnn, newnamesnames = R_NilValue;
	PROTECT(dnn = getAttrib(dimnames, R_NamesSymbol));
	PROTECT(newdims = allocVector(INTSXP, n));
	for (i = 0, n = 0; i < ndims; i++)
	    if (dim[i] != 1)
		INTEGER(newdims)[n++] = dim[i];
	if(!isNull(getAttrib(dims, R_NamesSymbol))) {
	    SEXP new_nms = PROTECT(allocVector(STRSXP, n));
	    SEXP nms_d = getAttrib(dims, R_NamesSymbol);
	    for (i = 0, n = 0; i < ndims; i++)
		if (dim[i] != 1)
		    SET_STRING_ELT(new_nms, n++, STRING_ELT(nms_d, i));
	    setAttrib(newdims, R_NamesSymbol, new_nms);
	    UNPROTECT(1);
	}
	bool havenames = false;
	if (!isNull(dimnames)) {
	    for (i = 0; i < ndims; i++)
		if (dim[i] != 1 &&
		    VECTOR_ELT(dimnames, i) != R_NilValue)
		    havenames = true;
	    if (havenames) {
		PROTECT(newnames = allocVector(VECSXP, n));
		PROTECT(newnamesnames = allocVector(STRSXP, n));
		for (i = 0, n = 0; i < ndims; i++) {
		    if (dim[i] != 1) {
			if(!isNull(dnn))
			    SET_STRING_ELT(newnamesnames, n,
					   STRING_ELT(dnn, i));
			SET_VECTOR_ELT(newnames, n++, VECTOR_ELT(dimnames, i));
		    }
		}
	    }
	    else dimnames = R_NilValue;
	}
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, newdims);
	if (havenames)
	{
	    if(!isNull(dnn))
		setAttrib(newnames, R_NamesSymbol, newnamesnames);
	    setAttrib(x, R_DimNamesSymbol, newnames);
	    UNPROTECT(2); /* newnamesnames, newnames */
	}
	UNPROTECT(2); /* newdims, dnn */
    }
    UNPROTECT(2); /* dimnames, x */
    return x;
}

attribute_hidden SEXP do_drop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, xdims;
    int i, n, shorten;

    checkArity(op, args);
    x = CAR(args);
    if ((xdims = getAttrib(x, R_DimSymbol)) != R_NilValue) {
	n = LENGTH(xdims);
	shorten = 0;
	for (i = 0; i < n; i++)
	    if (INTEGER(xdims)[i] == 1) shorten = 1;
	if (shorten) {
	    if (MAYBE_REFERENCED(x)) x = R_duplicate_attr(x);
	    x = DropDims(x);
	}
    }
    return x;
}

/* Length of Primitive Objects */

attribute_hidden SEXP do_length(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    SEXP x = CAR(args), ans;

    /* DispatchOrEval internal generic: length */
    if (isObject(x) &&
       DispatchOrEval(call, op, "length", args, rho, &ans, 0, 1)) {
	if (length(ans) == 1 && TYPEOF(ans) == REALSXP) {
	    double d = REAL(ans)[0];
	    if (R_FINITE(d) && d >= 0. && d <= INT_MAX && floor(d) == d) {
                PROTECT(ans);
                ans = coerceVector(ans, INTSXP);
                UNPROTECT(1);
                return(ans);
            }
	}
	return(ans);
    }


#ifdef LONG_VECTOR_SUPPORT
    // or use IS_LONG_VEC
    R_xlen_t len = xlength(x);
    if (len > INT_MAX) return ScalarReal((double) len);
#endif
    return ScalarInteger(length(x));
}

attribute_hidden R_len_t dispatch_length(SEXP x, SEXP call, SEXP rho) {
    R_xlen_t len = dispatch_xlength(x, call, rho);
#ifdef LONG_VECTOR_SUPPORT
    if (len > INT_MAX) return R_BadLongVector(x, __FILE__, __LINE__);
#endif
    return (R_len_t) len;
}

attribute_hidden R_xlen_t dispatch_xlength(SEXP x, SEXP call, SEXP rho) {
    static SEXP length_op = NULL;
    if (isObject(x)) {
        SEXP len, args;
        if (length_op == NULL)
            length_op = R_Primitive("length");
        PROTECT(args = list1(x));
	/* DispatchOrEval internal generic: length */
        if (DispatchOrEval(call, length_op, "length", args, rho, &len, 0, 1)) {
            UNPROTECT(1);
            return (R_xlen_t)
                (TYPEOF(len) == REALSXP ? REAL(len)[0] : asInteger(len));
        }
        UNPROTECT(1);
    }
    return(xlength(x));
}

// auxiliary for do_lengths_*(), i.e., R's lengths()
static R_xlen_t getElementLength(SEXP x, R_xlen_t i, SEXP call, SEXP rho) {
    SEXP x_elt;
    R_xlen_t ans;

    PROTECT(x_elt = dispatch_subset2(x, i, call, rho));
    ans = dispatch_xlength(x_elt, call, rho);
    UNPROTECT(1); /* x_elt */
    return ans;
}

#ifdef LONG_VECTOR_SUPPORT
static SEXP do_lengths_long(SEXP x, SEXP call, SEXP rho)
{
    SEXP ans;
    R_xlen_t x_len, i;
    double *ans_elt;

    x_len = dispatch_xlength(x, call, rho);
    PROTECT(ans = allocVector(REALSXP, x_len));
    for (i = 0, ans_elt = REAL(ans); i < x_len; i++, ans_elt++)
        *ans_elt = (double) getElementLength(x, i, call, rho);
    UNPROTECT(1);
    return ans;
}
#endif

attribute_hidden SEXP do_lengths(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args), ans;
    R_xlen_t x_len, i;
    int *ans_elt;
    int useNames = asLogical(CADR(args));
    if (useNames == NA_LOGICAL)
	error(_("invalid '%s' value"), "use.names");

    /* DispatchOrEval internal generic: lengths */
    if (DispatchOrEval(call, op, "lengths", args, rho, &ans, 0, 1))
      return(ans);

    bool isList = isVectorList(x) || isS4(x);
    if(!isList) switch(TYPEOF(x)) {
	case NILSXP:
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    break;
	default:
	    error(_("'%s' must be a list or atomic vector"), "x");
    }
    x_len = dispatch_xlength(x, call, rho);
    PROTECT(ans = allocVector(INTSXP, x_len));
    if(isList) {
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++) {
	    R_xlen_t x_elt_len = getElementLength(x, i, call, rho);
#ifdef LONG_VECTOR_SUPPORT
	    if (x_elt_len > INT_MAX) {
		ans = do_lengths_long(x, call, rho);
		UNPROTECT(1);
		PROTECT(ans);
		break;
	    }
#endif
	    *ans_elt = (int)x_elt_len;
	}
    } else { // atomic: every element has length 1
	for (i = 0, ans_elt = INTEGER(ans); i < x_len; i++, ans_elt++)
	    *ans_elt = 1;
    }
    SEXP dim = getAttrib(x, R_DimSymbol);
    if(!isNull(dim)) {
        setAttrib(ans, R_DimSymbol, dim);
    }
    if(useNames) {
	SEXP names = getAttrib(x, R_NamesSymbol);
	if(!isNull(names)) setAttrib(ans, R_NamesSymbol, names);
        SEXP dimnames = getAttrib(x, R_DimNamesSymbol);
        if(!isNull(dimnames)) setAttrib(ans, R_DimNamesSymbol, dimnames);
    }
    UNPROTECT(1);
    return ans;
}

attribute_hidden SEXP do_rowscols(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP dim = CAR(args);
    int nprot = 0;
    if (!isInteger(dim)) {
	PROTECT(dim = coerceVector(dim, INTSXP)); nprot++;
    }
    if (LENGTH(dim) != 2)
	error(_("a matrix-like object is required as argument to '%s'"),
	      (PRIMVAL(op) == 2) ? "col" : "row");

    int nr = INTEGER(dim)[0],
	nc = INTEGER(dim)[1];
    if(nprot) UNPROTECT(nprot);

    SEXP ans = allocMatrix(INTSXP, nr, nc);

    R_xlen_t NR = nr;
    switch (PRIMVAL(op)) {
    case 1: // row() & .row()
	for (int i = 0; i < nr; i++)
	    for (int j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = i + 1;
	break;
    case 2: // col() & .col()
	for (int i = 0; i < nr; i++)
	    for (int j = 0; j < nc; j++)
		INTEGER(ans)[i + j * NR] = j + 1;
	break;
    }
    return ans;
}

/*
 Whenever vector x contains NaN or Inf (or -Inf), the function returns TRUE.
 It can be imprecise: it can return TRUE in other cases as well.

 A precise version of the function could be implemented as

       for (R_xlen_t i = 0; i < n; i++)
           if (!R_FINITE(x[i])) return TRUE;
       return false;

 The present version is imprecise, but faster.
*/
static bool mayHaveNaNOrInf(double *x, R_xlen_t n)
{
    if ((n&1) != 0 && !R_FINITE(x[0]))
	return true;
    for (R_xlen_t i = n&1; i < n; i += 2)
	/* A precise version could use this condition:
	 *
	 * !R_FINITE(x[i]+x[i+1]) && (!R_FINITE(x[i]) || !R_FINITE(x[i+1]))
	 *
	 * The present imprecise version has been found to be faster
	 * with GCC and ICC in the common case when the sum of the two
	 * values is always finite.
	 *
	 * The present version is imprecise because the sum of two very
	 * large finite values (e.g. 1e308) may be infinite.
	 */
	if (!R_FINITE(x[i]+x[i+1]))
	    return true;
    return false;
}

/*
 This is an experimental version that has been observed to run fast on some
 SIMD hardware with GCC and ICC.
 Note that the OpenMP reduction assumes associativity of addition, which is
 safe here, because the result is only used for an imprecise test for
 the presence of NaN and Inf values.
*/
static bool mayHaveNaNOrInf_simd(double *x, R_xlen_t n)
{
    double s = 0;
    /* SIMD reduction is supported since OpenMP 4.0. The value of _OPENMP is
       unreliable in some compilers, so we depend on HAVE_OPENMP_SIMDRED,
       which is normally set by configure based on a test. */
    /* _OPENMP >= 201307 */
#if defined(_OPENMP) && HAVE_OPENMP_SIMDRED
    #pragma omp simd reduction(+:s)
#endif
    for (R_xlen_t i = 0; i < n; i++)
	s += x[i];
    return !R_FINITE(s);
}

static bool cmayHaveNaNOrInf(Rcomplex *x, R_xlen_t n)
{
    /* With HAVE_FORTRAN_DOUBLE_COMPLEX set, it should be clear that
       Rcomplex has no padding, so we could probably use mayHaveNaNOrInf,
       but better safe than sorry... */
    if ((n&1) != 0 && (!R_FINITE(x[0].r) || !R_FINITE(x[0].i)))
	return true;
    for (R_xlen_t i = n&1; i < n; i += 2)
	if (!R_FINITE(x[i].r+x[i].i+x[i+1].r+x[i+1].i))
	    return true;
    return false;
}

/* experimental version for SIMD hardware (see also mayHaveNaNOrInf_simd) */
static bool cmayHaveNaNOrInf_simd(Rcomplex *x, R_xlen_t n)
{
    double s = 0;
    /* _OPENMP >= 201307 - see mayHaveNaNOrInf_simd */
#if defined(_OPENMP) && HAVE_OPENMP_SIMDRED
    #pragma omp simd reduction(+:s)
#endif
    for (R_xlen_t i = 0; i < n; i++) {
	s += x[i].r;
	s += x[i].i;
    }
    return !R_FINITE(s);
}

static void internal_matprod(double *x, int nrx, int ncx,
                             double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define MATPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry;			\
    for (int i = 0; i < nrx; i++)			\
	for (int k = 0; k < ncy; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < ncx; j++)		\
		sum += x[i + j * NRX] * y[j + k * NRY];	\
	    z[i + k * NRX] = (double) sum;		\
	}
    MATPROD_BODY;
}

static void simple_matprod(double *x, int nrx, int ncx,
                           double *y, int nry, int ncy, double *z)
{
    double sum;
    MATPROD_BODY;
}

static void internal_crossprod(double *x, int nrx, int ncx,
                               double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define CROSSPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry, NCX = ncx;		\
    for (int i = 0; i < ncx; i++)			\
	for (int k = 0; k < ncy; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < nrx; j++)		\
		sum += x[j + i * NRX] * y[j + k * NRY];	\
	    z[i + k * NCX] = (double) sum;		\
	}
    CROSSPROD_BODY;
}

static void simple_crossprod(double *x, int nrx, int ncx,
                             double *y, int nry, int ncy, double *z)
{
    double sum;
    CROSSPROD_BODY;
}

static void internal_tcrossprod(double *x, int nrx, int ncx,
                                double *y, int nry, int ncy, double *z)
{
    LDOUBLE sum;
#define TCROSSPROD_BODY					\
    R_xlen_t NRX = nrx, NRY = nry;			\
    for (int i = 0; i < nrx; i++)			\
	for (int k = 0; k < nry; k++) {			\
	    sum = 0.0;					\
	    for (int j = 0; j < ncx; j++)		\
		sum += x[i + j * NRX] * y[k + j * NRY];	\
	    z[i + k * NRX] = (double) sum;		\
	}
    TCROSSPROD_BODY;
}

static void simple_tcrossprod(double *x, int nrx, int ncx,
                              double *y, int nry, int ncy, double *z)
{
    double sum;
    TCROSSPROD_BODY;
}


static void matprod(double *x, int nrx, int ncx,
		    double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	/* Don't trust the BLAS to handle NA/NaNs correctly: PR#4582
	 * The test is only O(n) here.
	 *
	 * MKL disclaimer: "LAPACK routines assume that input matrices
	 * do not contain IEEE 754 special values such as INF or NaN values.
	 * Using these special values may cause LAPACK to return unexpected
	 * results or become unstable."
	 */
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_matprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_matprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_matprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transN = "N", *transT = "T";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (ncy == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transN, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione FCONE);
    else if (nrx == 1) /* vector-matrix */
	/* Instead of xY, compute (xY)^T == (Y^T)(x^T)
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transT, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione FCONE);
    else /* matrix-matrix or outer product */
	F77_CALL(dgemm)(transN, transN, &nrx, &ncy, &ncx, &one, x,
			&nrx, y, &nry, &zero, z, &nrx FCONE FCONE);
}

static void internal_cmatprod(Rcomplex *x, int nrx, int ncx,
                              Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define CMATPROD_BODY					    \
    int i, j, k;					    \
    double complex xij, yjk;				    \
    R_xlen_t NRX = nrx, NRY = nry;			    \
    for (i = 0; i < nrx; i++)				    \
	for (k = 0; k < ncy; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < ncx; j++) {			    \
		xij = toC99(x + (i + j * NRX));		    \
		yjk = toC99(y + (j + k * NRY));		    \
		sum_r += creal(xij * yjk);		    \
		sum_i += cimag(xij * yjk);		    \
	    }						    \
	    z[i + k * NRX].r = (double) sum_r;		    \
	    z[i + k * NRX].i = (double) sum_i;		    \
	}
    CMATPROD_BODY;
}

static void simple_cmatprod(Rcomplex *x, int nrx, int ncx,
                            Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    CMATPROD_BODY;
}

static void internal_ccrossprod(Rcomplex *x, int nrx, int ncx,
                                Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define CCROSSPROD_BODY					    \
    int i, j, k;					    \
    double complex xji, yjk;				    \
    R_xlen_t NRX = nrx, NRY = nry, NCX = ncx;		    \
    for (i = 0; i < ncx; i++)				    \
	for (k = 0; k < ncy; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < nrx; j++) {			    \
		xji = toC99(x + (j + i * NRX));		    \
		yjk = toC99(y + (j + k * NRY));		    \
		sum_r += creal(xji * yjk);		    \
		sum_i += cimag(xji * yjk);		    \
	    }						    \
	    z[i + k * NCX].r = (double) sum_r;		    \
	    z[i + k * NCX].i = (double) sum_i;		    \
	}
    CCROSSPROD_BODY;
}

static void simple_ccrossprod(Rcomplex *x, int nrx, int ncx,
                              Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    CCROSSPROD_BODY;
}

static void internal_tccrossprod(Rcomplex *x, int nrx, int ncx,
                                 Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    LDOUBLE sum_i, sum_r;
#define TCCROSSPROD_BODY				    \
    int i, j, k;					    \
    double complex xij, ykj;				    \
    R_xlen_t NRX = nrx, NRY = nry;			    \
    for (i = 0; i < nrx; i++)				    \
	for (k = 0; k < nry; k++) {			    \
	    sum_r = 0.0;				    \
	    sum_i = 0.0;				    \
	    for (j = 0; j < ncx; j++) {			    \
		xij = toC99(x + (i + j * NRX));		    \
		ykj = toC99(y + (k + j * NRY));		    \
		sum_r += creal(xij * ykj);		    \
		sum_i += cimag(xij * ykj);		    \
	    }						    \
	    z[i + k * NRX].r = (double) sum_r;		    \
	    z[i + k * NRX].i = (double) sum_i;		    \
	}
    TCCROSSPROD_BODY;
}

static void simple_tccrossprod(Rcomplex *x, int nrx, int ncx,
                               Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    double sum_i, sum_r;
    TCCROSSPROD_BODY;
}

static void cmatprod(Rcomplex *x, int nrx, int ncx,
		     Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*ncy; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_cmatprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_cmatprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_cmatprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transa = "N", *transb = "N";
    Rcomplex one, zero;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
                    x, &nrx, y, &nry, &zero, z, &nrx FCONE FCONE);
#endif
}

static void symcrossprod(double *x, int nr, int nc, double *z)
{
    R_xlen_t NR = nr, NC = nc;
    if (nr == 0 || nc == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NC*NC; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NR*nc)) {
		simple_crossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_crossprod(x, nr, nc, x, nr, nc, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NR*nc))  {
		simple_crossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
    }

    char *trans = "T", *uplo = "U";
    double one = 1.0, zero = 0.0;

    F77_CALL(dsyrk)(uplo, trans, &nc, &nr, &one, x, &nr, &zero, z, &nc
		    FCONE FCONE);
    for (int i = 1; i < nc; i++)
	for (int j = 0; j < i; j++) z[i + NC *j] = z[j + NC * i];
}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_crossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_crossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_crossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transT = "T", *transN = "N";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (ncy == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transT, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione FCONE);
    else if (ncx == 1) /* vector-matrix */
	/* Instead of (x^T)Y, compute ((x^T)Y)^T == (Y^T)x
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transT, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione FCONE);
    else /* matrix-matrix  or outer product */
	F77_CALL(dgemm)(transT, transN, &ncx, &ncy, &nrx, &one,
		        x, &nrx, y, &nry, &zero, z, &ncx FCONE FCONE);
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	R_xlen_t NCX = ncx;
	for(R_xlen_t i = 0; i < NCX*ncy; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_ccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transa = "T", *transb = "N";
    Rcomplex one, zero;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
                    x, &nrx, y, &nry, &zero, z, &ncx FCONE FCONE);
#endif
}

static void symtcrossprod(double *x, int nr, int nc, double *z)
{
    R_xlen_t NR = nr;
    if (nr == 0 || nc == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NR*NR; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    /* see matprod for more details */
	    if (mayHaveNaNOrInf(x, NR*nc)) {
		simple_tcrossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tcrossprod(x, nr, nc, x, nr, nc, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NR*nc))  {
		simple_tcrossprod(x, nr, nc, x, nr, nc, z);
		return;
	    }
	    break; /* use blas */
    }

    char *trans = "N", *uplo = "U";
    double one = 1.0, zero = 0.0;

    F77_CALL(dsyrk)(uplo, trans, &nr, &nc, &one, x, &nr, &zero, z, &nr
		    FCONE FCONE);
    for (int i = 1; i < nr; i++)
	for (int j = 0; j < i; j++) z[i + nr *j] = z[j + nr * i];
}

static void tcrossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i] = 0;
	return;
    }

    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (mayHaveNaNOrInf(x, NRX*ncx) || mayHaveNaNOrInf(y, NRY*ncy)) {
		simple_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (mayHaveNaNOrInf_simd(x, NRX*ncx) ||
		mayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_tcrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transN = "N", *transT = "T";
    double one = 1.0, zero = 0.0;
    int ione = 1;

    if (nry == 1) /* matrix-vector or dot product */
	F77_CALL(dgemv)(transN, &nrx, &ncx, &one, x,
			&nrx, y, &ione, &zero, z, &ione FCONE);
    else if (nrx == 1) /* vector-matrix */
	/* Instead of x(Y^T), compute (x(Y^T))^T == Y(x^T)
	   The result is a vector, so transposing its content is no-op */
	F77_CALL(dgemv)(transN, &nry, &ncy, &one, y,
			&nry, x, &ione, &zero, z, &ione FCONE);
    else /* matrix-matrix or outer product */
	F77_CALL(dgemm)(transN, transT, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx FCONE FCONE);
}

static void tccrossprod(Rcomplex *x, int nrx, int ncx,
			Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    R_xlen_t NRX = nrx, NRY = nry;
    if (nrx == 0 || ncx == 0 || nry == 0 || ncy == 0) {
	/* zero-extent operations should return zeroes */
	for(R_xlen_t i = 0; i < NRX*nry; i++) z[i].r = z[i].i = 0;
	return;
    }

#ifndef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (R_Matprod == MATPROD_INTERNAL)
	internal_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
    else
	simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
#else
    switch(R_Matprod) {
	case MATPROD_DEFAULT:
	    if (cmayHaveNaNOrInf(x, NRX*ncx) || cmayHaveNaNOrInf(y, NRY*ncy)) {
		simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
	case MATPROD_INTERNAL:
	    internal_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
	    return;
	case MATPROD_BLAS:
	    break;
	case MATPROD_DEFAULT_SIMD:
	    if (cmayHaveNaNOrInf_simd(x, NRX*ncx) ||
		cmayHaveNaNOrInf_simd(y, NRY*ncy)) {
		simple_tccrossprod(x, nrx, ncx, y, nry, ncy, z);
		return;
	    }
	    break; /* use blas */
    }

    char *transa = "N", *transb = "T";
    Rcomplex one, zero;
    one.r = 1.0; one.i = zero.r = zero.i = 0.0;

    F77_CALL(zgemm)(transa, transb, &nrx, &nry, &ncx, &one,
                    x, &nrx, y, &nry, &zero, z, &nrx FCONE FCONE);
#endif
}


/* "%*%" (op = 0), crossprod (op = 1) or tcrossprod (op = 2) */
attribute_hidden SEXP do_matprod(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    // .Primitive() ; may have 1 or 2 args, but some methods have more
    bool cross = PRIMVAL(op) != 0;
    int nargs, min_nargs = cross ? 1 : 2;
    if (args == R_NilValue)
	nargs = 0;
    else if (CDR(args) == R_NilValue)
	nargs = 1;
    else // if (CDDR(args) == R_NilValue)
	nargs = 2;
    /* else   // not relevant
	nargs = length(args);
    */
    if (nargs < min_nargs)
	errorcall(call,
		  ngettext("%d argument passed to '%s' which requires at least %d",
			   "%d arguments passed to '%s' which requires at least %d",
			   (unsigned long) nargs),
		  nargs, PRIMNAME(op), min_nargs);

    SEXP x = CAR(args), y = CADR(args), ans;
    if (OBJECT(x) || OBJECT(y)) {
	SEXP s; //, value;
	/* Remove argument names to ensure positional matching */
	for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);

	if (DispatchGroup("matrixOps", call, op, args, rho, &ans))
	    return ans;
    }
    // the default method:
    if (CDDR(args) != R_NilValue)
	warningcall(call, _("more than 2 arguments passed to default method of '%s'"),
		    PRIMNAME(op));
    bool sym = isNull(y);
    if (sym && (PRIMVAL(op) > 0)) y = x;
    if ( !(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y)) )
	errorcall(call, _("requires numeric/complex matrix/vector arguments"));

    SEXP xdims = getAttrib(x, R_DimSymbol),
	 ydims = getAttrib(y, R_DimSymbol);
    int ldx = length(xdims),
	ldy = length(ydims), nrx, ncx, nry, ncy;

    if (ldx != 2 && ldy != 2) {		/* x and y non-matrices */
	// for crossprod, allow two cases: n x n ==> (1,n) x (n,1);  1 x n = (n, 1) x (1, n)
	if (PRIMVAL(op) == 1 && LENGTH(x) == 1) {
	    nrx = ncx = nry = 1;
	    ncy = LENGTH(y);
	}
	else {
	    nry = LENGTH(y);
	    ncy = 1;
	    if (PRIMVAL(op) == 0) {
		nrx = 1;
		ncx = LENGTH(x);
		if(ncx == 1) {	        // y as row vector
		    ncy = nry;
		    nry = 1;
		}
	    }
	    else {
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldx != 2) {		/* x not a matrix */
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
	nrx = 0;
	ncx = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(x) == nry) {	/* x as row vector */
		nrx = 1;
		ncx = nry; /* == LENGTH(x) */
	    }
	    else if (nry == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
	else if (PRIMVAL(op) == 1) { /* crossprod() */
	    if (LENGTH(x) == nry) {	/* x is a col vector */
		nrx = nry; /* == LENGTH(x) */
		ncx = 1;
	    }
	    /* else if (nry == 1) ... not being too tolerant
	       to treat x as row vector, as t(x) *is* row vector */
	}
	else { /* tcrossprod */
	    if (LENGTH(x) == ncy) {	/* x as row vector */
		nrx = 1;
		ncx = ncy; /* == LENGTH(x) */
	    }
	    else if (ncy == 1) {	/* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldy != 2) {		/* y not a matrix */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = 0;
	ncy = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(y) == ncx) {	/* y as col vector */
		nry = ncx;
		ncy = 1;
	    }
	    else if (ncx == 1) {	/* y as row vector */
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else if (PRIMVAL(op) == 1) { /* crossprod() */
	    if (LENGTH(y) == nrx) {	/* y is a col vector */
		nry = nrx;
		ncy = 1;
	    } else if (nrx == 1) {	// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else { // tcrossprod
	    if (nrx == 1) {		// y as row vector
		nry = 1;
		ncy = LENGTH(y);
	    }
	    else {			// y is a col vector
		nry = LENGTH(y);
		ncy = 1;
	    }
	}
    }
    else {				/* x and y matrices */
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
    }
    /* nr[ow](.) and nc[ol](.) are now defined for x and y */

    if (PRIMVAL(op) == 0) {
	/* primitive, so use call */
	if (ncx != nry)
	    errorcall(call, _("non-conformable arguments"));
    }
    else if (PRIMVAL(op) == 1) {
	if (nrx != nry)
	    error(_("non-conformable arguments"));
    }
    else {
	if (ncx != ncy)
	    error(_("non-conformable arguments"));
    }

    int mode;
    if (isComplex(CAR(args)) || isComplex(CADR(args)))
	mode = CPLXSXP;
    else
	mode = REALSXP;
    x = PROTECT(coerceVector(x, mode));
    y = PROTECT(coerceVector(y, mode));

    if (PRIMVAL(op) == 0) {			/* op == 0 : matprod() =~= %*% */

	PROTECT(ans = allocMatrix(mode, nrx, ncy));
	if (mode == CPLXSXP)
	    cmatprod(COMPLEX(x), nrx, ncx,
		     COMPLEX(y), nry, ncy, COMPLEX(ans));
	else
	    matprod(REAL(x), nrx, ncx,
		    REAL(y), nry, ncy, REAL(ans));

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
#define ALLOC_DIMNAMES_NAMES						\
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue; \
									\
	    /* allocate dimnames and dimnamesnames */			\
	    PROTECT(dimnames = allocVector(VECSXP, 2));			\
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2))

	    ALLOC_DIMNAMES_NAMES;
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx == 1) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }

/* Since R 2.1.0, no longer attach a dimnames attribute whose elements are all NULL: */
#define SET_DIMNAMES_NAMES						\
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||			\
		VECTOR_ELT(dimnames,1) != R_NilValue) {			\
		if (dnx != R_NilValue || dny != R_NilValue)		\
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);	\
		setAttrib(ans, R_DimNamesSymbol, dimnames);		\
	    }								\
	    UNPROTECT(2)

#define YDIMS_ET_CETERA							\
	    if (ydims != R_NilValue) {					\
		if (ldy == 2) {						\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 1)); \
		} else if (nry == 1) {					\
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));	\
		    dny = getAttrib(ydims, R_NamesSymbol);		\
		    if(!isNull(dny))					\
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0)); \
		}							\
	    }								\
	    SET_DIMNAMES_NAMES;


	    YDIMS_ET_CETERA;
	}
    }

    else if (PRIMVAL(op) == 1) {	/* op == 1: crossprod() */

	PROTECT(ans = allocMatrix(mode, ncx, ncy));
	if (mode == CPLXSXP)
	    if(sym)
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		ccrossprod(COMPLEX(x), nrx, ncx,
			   COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		crossprod(REAL(x), nrx, ncx,
			  REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {

	    ALLOC_DIMNAMES_NAMES;
	    if (xdims != R_NilValue) {
		if (ldx == 2) {/* not nrx==1 : .. fixed, ihaka 2003-09-30 */
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 1));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 1));
		}
	    }

	    YDIMS_ET_CETERA;
	}

    }
    else {					/* op == 2: tcrossprod() */

	PROTECT(ans = allocMatrix(mode, nrx, nry));
	if (mode == CPLXSXP)
	    if(sym)
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(x), nry, ncy, COMPLEX(ans));
	    else
		tccrossprod(COMPLEX(x), nrx, ncx,
			    COMPLEX(y), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symtcrossprod(REAL(x), nrx, ncx, REAL(ans));
	    else
		tcrossprod(REAL(x), nrx, ncx,
			   REAL(y), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(x, R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(y, R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {

	    ALLOC_DIMNAMES_NAMES;
	    if (xdims != R_NilValue) {
		if (ldx == 2) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }
	    if (ydims != R_NilValue) {
		if (ldy == 2) {
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));
		    dny = getAttrib(ydims, R_NamesSymbol);
		    if(!isNull(dny))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 0));
		}
	    }
            SET_DIMNAMES_NAMES;
	}
    }
    UNPROTECT(5);
    return ans;
}
#undef YDIMS_ET_CETERA

attribute_hidden SEXP do_transpose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, r, dims, dimnames, dimnamesnames = R_NilValue,
	ndimnamesnames, rnames, cnames;
    int ldim, ncol = 0, nrow = 0;
    R_xlen_t len = 0;

    checkArity(op, args);
    a = CAR(args);

    if (isVector(a)) {
	dims = getAttrib(a, R_DimSymbol);
	ldim = length(dims);
	rnames = R_NilValue;
	cnames = R_NilValue;
	switch(ldim) {
	case 0:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    rnames = getAttrib(a, R_NamesSymbol);
	    dimnames = rnames;/* for isNull() below*/
	    break;
	case 1:
	    len = nrow = LENGTH(a);
	    ncol = 1;
	    dimnames = getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	case 2:
	    ncol = ncols(a);
	    nrow = nrows(a);
	    len = XLENGTH(a);
	    dimnames = getAttrib(a, R_DimNamesSymbol);
	    if (dimnames != R_NilValue) {
		rnames = VECTOR_ELT(dimnames, 0);
		cnames = VECTOR_ELT(dimnames, 1);
		dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	    }
	    break;
	default:
	    goto not_matrix;
	}
    }
    else
	goto not_matrix;
    PROTECT(dimnamesnames);
    PROTECT(r = allocVector(TYPEOF(a), len));
    R_xlen_t i, j, l_1 = len-1;
    switch (TYPEOF(a)) {
    case LGLSXP:
    case INTSXP:
	// filling in columnwise, "accessing row-wise":
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    INTEGER(r)[i] = INTEGER(a)[j];
	}
	break;
    case REALSXP:
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    REAL(r)[i] = REAL(a)[j];
	}
	break;
    case CPLXSXP:
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    COMPLEX(r)[i] = COMPLEX(a)[j];
	}
	break;
    case STRSXP:
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    SET_STRING_ELT(r, i, STRING_ELT(a,j));
	}
	break;
    case VECSXP:
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    SET_VECTOR_ELT(r, i, VECTOR_ELT(a,j));
	}
	break;
    case RAWSXP:
	for (i = 0, j = 0; i < len; i++, j += nrow) {
	    if (j > l_1) j -= l_1;
	    RAW(r)[i] = RAW(a)[j];
	}
	break;
    default:
	UNPROTECT(2); /* r, dimnamesnames */
	goto not_matrix;
    }
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1); /* dims */
    /* R <= 2.2.0: dropped list(NULL,NULL) dimnames :
     * if(rnames != R_NilValue || cnames != R_NilValue) */
    if(!isNull(dimnames)) {
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, cnames);
	SET_VECTOR_ELT(dimnames, 1, rnames);
	if(!isNull(dimnamesnames)) {
	    PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
	    SET_VECTOR_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
	    SET_VECTOR_ELT(ndimnamesnames, 0,
			   (ldim == 2) ? STRING_ELT(dimnamesnames, 1):
			   R_BlankString);
	    setAttrib(dimnames, R_NamesSymbol, ndimnamesnames);
	    UNPROTECT(1); /* ndimnamesnames */
	}
	setAttrib(r, R_DimNamesSymbol, dimnames);
	UNPROTECT(1); /* dimnames */
    }
    copyMostAttrib(a, r);
    UNPROTECT(2); /* r, dimnamesnames */
    return r;
 not_matrix:
    error(_("argument is not a matrix"));
    return call;/* never used; just for -Wall */
}

/*
 New version of aperm, using strides for speed.
 Jonathan Rougier <J.C.Rougier@durham.ac.uk>

 v1.0 30.01.01

 M.Maechler : expanded	all ../include/Rdefines.h macros
 */

/* aperm (a, perm, resize = TRUE) */
attribute_hidden SEXP do_aperm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP a = CAR(args);
    if (!isArray(a))
	error(_("invalid first argument, must be %s"), "an array");

    SEXP dimsa = PROTECT(getAttrib(a, R_DimSymbol));
    int n = LENGTH(dimsa),
	*isa = INTEGER(dimsa);

    /* check the permutation */

    int i;
    int *pp = (int *) R_alloc((size_t) n, sizeof(int));
    SEXP perm = CADR(args);
    if (length(perm) == 0) {
	for (i = 0; i < n; i++) pp[i] = n-1-i;
    } else {
	if (LENGTH(perm) != n)
	    error(_("'perm' is of wrong length %d (!= %d)"),
		  LENGTH(perm), n);
	if (isString(perm)) {
	    SEXP dna = getAttrib(a, R_DimNamesSymbol);
	    if (isNull(dna))
		error(_("'a' does not have named dimnames"));
	    SEXP dnna = getAttrib(dna, R_NamesSymbol);
	    if (isNull(dnna))
		error(_("'a' does not have named dimnames"));
	    for (i = 0; i < n; i++) {
		const char *this = translateChar(STRING_ELT(perm, i));
		int j;
		for (j = 0; j < n; j++)
		    if (streql(translateChar(STRING_ELT(dnna, j)),
			       this)) {pp[i] = j; break;}
		if (j >= n)
		    error(_("'perm[%d]' does not match a dimension name"), i+1);
	    }
	} else {
	    PROTECT(perm = coerceVector(perm, INTSXP));
	    for (i = 0; i < n; i++) pp[i] = INTEGER(perm)[i] - 1;
	    UNPROTECT(1);
	}
    }

    R_xlen_t *iip = (R_xlen_t *) R_alloc((size_t) n, sizeof(R_xlen_t));
    Memzero(iip, n);
    for (i = 0; i < n; i++)
	if (pp[i] >= 0 && pp[i] < n) iip[pp[i]]++;
	else error(_("value out of range in 'perm'"));
    for (i = 0; i < n; i++)
	if (iip[i] == 0) error(_("invalid '%s' argument"), "perm");

    /* create the stride object and permute */

    R_xlen_t *stride = (R_xlen_t *) R_alloc((size_t) n, sizeof(R_xlen_t));
    for (iip[0] = 1, i = 1; i<n; i++) iip[i] = iip[i-1] * isa[i-1];
    for (i = 0; i < n; i++) stride[i] = iip[pp[i]];

    /* also need to have the dimensions of r */

    SEXP dimsr = PROTECT(allocVector(INTSXP, n));
    int *isr = INTEGER(dimsr);
    for (i = 0; i < n; i++) isr[i] = isa[pp[i]];

    /* and away we go! iip will hold the incrementer */
    Memzero(iip, n);

    R_xlen_t len = XLENGTH(a);
    SEXP r = PROTECT(allocVector(TYPEOF(a), len));

    R_xlen_t li, lj;

/* this increments iip and sets lj using strides */
#define CLICKJ						\
	for (int i_ = 0; i_ < n; i_++)			\
	    if (iip[i_] == isr[i_]-1) iip[i_] = 0;	\
	    else {					\
		iip[i_]++;				\
		break;					\
	    }						\
    lj = 0;						\
    for (int i_ = 0; i_ < n; i_++)			\
	    lj += iip[i_] * stride[i_]

    switch (TYPEOF(a)) {
    case INTSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    INTEGER(r)[li] = INTEGER(a)[lj];
	    CLICKJ;
	}
	break;

    case LGLSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    LOGICAL(r)[li] = LOGICAL(a)[lj];
	    CLICKJ;
	}
	break;

    case REALSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    REAL(r)[li] = REAL(a)[lj];
	    CLICKJ;
	}
	break;

    case CPLXSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    COMPLEX(r)[li].r = COMPLEX(a)[lj].r;
	    COMPLEX(r)[li].i = COMPLEX(a)[lj].i;
	    CLICKJ;
	}
	break;

    case STRSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_STRING_ELT(r, li, STRING_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case VECSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    SET_VECTOR_ELT(r, li, VECTOR_ELT(a, lj));
	    CLICKJ;
	}
	break;

    case RAWSXP:
	for (lj = 0, li = 0; li < len; li++) {
	    RAW(r)[li] = RAW(a)[lj];
	    CLICKJ;
	}
	break;

    default:
	UNIMPLEMENTED_TYPE("aperm", a);
    }

    /* handle the resize */
    int resize = asLogical(CADDR(args));
    if (resize == NA_LOGICAL) error(_("'resize' must be TRUE or FALSE"));

    /* and handle names(dim(.)) and the dimnames if any */
    if (resize) {
	SEXP nmdm = getAttrib(dimsa, R_NamesSymbol);
	if(nmdm != R_NilValue) { // dimsr needs correctly permuted names()
	    PROTECT(nmdm);
	    SEXP nm_dr = PROTECT(allocVector(STRSXP, n));
	    for (i = 0; i < n; i++) {
		SET_STRING_ELT(nm_dr, i, STRING_ELT(nmdm, pp[i]));
	    }
	    setAttrib(dimsr, R_NamesSymbol, nm_dr);
	    UNPROTECT(2);
	}
	setAttrib(r, R_DimSymbol, dimsr);

	SEXP dna = PROTECT(getAttrib(a, R_DimNamesSymbol));
	if (dna != R_NilValue) {
	    SEXP dnna, dnr, dnnr;

	    PROTECT(dnr  = allocVector(VECSXP, n));
	    PROTECT(dnna = getAttrib(dna, R_NamesSymbol));
	    if (dnna != R_NilValue) {
		PROTECT(dnnr = allocVector(STRSXP, n));
		for (i = 0; i < n; i++) {
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
		    SET_STRING_ELT(dnnr, i, STRING_ELT(dnna, pp[i]));
		}
		setAttrib(dnr, R_NamesSymbol, dnnr);
		UNPROTECT(1);
	    } else {
		for (i = 0; i < n; i++)
		    SET_VECTOR_ELT(dnr, i, VECTOR_ELT(dna, pp[i]));
	    }
	    setAttrib(r, R_DimNamesSymbol, dnr);
	    UNPROTECT(2);
	}
	UNPROTECT(1);
    }
    else // !resize
	setAttrib(r, R_DimSymbol, dimsa);

    UNPROTECT(3); /* dimsa, r, dimsr */
    return r;
}

/* colSums(x, n, p, na.rm) and friends */
attribute_hidden SEXP do_colsum(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args); args = CDR(args);
    R_xlen_t n = asVecSize(CAR(args)); args = CDR(args);
    R_xlen_t p = asVecSize(CAR(args)); args = CDR(args);
    int NaRm = asLogical(CAR(args));
    if (n == NA_INTEGER || n < 0)
	error(_("invalid '%s' argument"), "n");
    if (p == NA_INTEGER || p < 0)
	error(_("invalid '%s' argument"), "p");
    if (NaRm == NA_LOGICAL) error(_("invalid '%s' argument"), "na.rm");
    bool keepNA = !NaRm;

    int type = TYPEOF(x);
    switch (type) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP: break;
    default:
	error(_("'x' must be numeric or complex"));
    }
    if ((double)n * (double)p > XLENGTH(x))
	error(_("'x' is too short")); /* PR#16367 */

    SEXP ans = R_NilValue;
    int OP = PRIMVAL(op);
    if (OP == 0 || OP == 1) { /* columns */
	ans = PROTECT(allocVector((type == CPLXSXP) ? CPLXSXP : REALSXP, p));
#ifdef _OPENMP
	int nthreads;
	/* This gives a spurious -Wunused-but-set-variable error */
	if (R_num_math_threads > 0)
	    nthreads = R_num_math_threads;
	else
	    nthreads = 1; /* for now */
#pragma omp parallel for num_threads(nthreads) default(none) \
    firstprivate(x, ans, n, p, type, NaRm, keepNA, R_NaReal, R_NaInt, OP)
#endif
	for (R_xlen_t j = 0; j < p; j++) {
	    R_xlen_t i, cnt = n; // number of non-NA entries per col.
	    LDOUBLE sum = 0.; /* -Wall */
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + (R_xlen_t)n*j;
		if (keepNA)
		    for (sum = 0., i = 0; i < n; i++) sum += *rx++;
		else {
		    for (cnt = 0, sum = 0., i = 0; i < n; i++, rx++)
			if (!ISNAN(*rx)) {cnt++; sum += *rx;}
		    // else if (keepNA) {sum = NA_REAL; break;}
		}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + (R_xlen_t)n*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_INTEGER) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + (R_xlen_t)n*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_LOGICAL) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    }
	    case CPLXSXP:
	    {
		LDOUBLE i_sum = 0.;
		Rcomplex *cx = COMPLEX(x) + (R_xlen_t)n*j;
		if (keepNA)
		    for (sum = 0., i = 0; i < n; i++, cx++) {
			sum   += cx->r;
			i_sum += cx->i;
		    }
		else {
		    for (cnt = 0, sum = 0., i_sum = 0., i = 0; i < n; i++, cx++) {
			if (!ISNAN(cx->r) && !ISNAN(cx->i)) {
			    cnt++;
			    sum   += cx->r;
			    i_sum += cx->i;
			}
		    }
		}
		if (OP == 1) {
		    sum   /= cnt; /* NaN for cnt = 0 */
		    i_sum /= cnt;
		}
		Rcomplex csum = { .r = sum, .i = i_sum };
		COMPLEX(ans)[j] = csum;
		continue; /* for(j ...) loop */
	    }
	    } // switch()
	    if (OP == 1) sum /= cnt; /* mean() -- gives NaN for cnt = 0 */
	    REAL(ans)[j] = (double) sum;
	} // for (j ..)
    }
    else { /* rows */
	ans = PROTECT(allocVector((type == CPLXSXP) ? CPLXSXP : REALSXP, n));
	/* allocate scratch storage to allow accumulating by columns
	   to improve cache hits */
	int *Cnt = NULL;
	LDOUBLE *rans;
	LDOUBLE *ians = NULL; // unused unless CPLXSXP
	if(n <= 10000) {
	    R_CheckStack2(n * sizeof(LDOUBLE));
	    rans = (LDOUBLE *) alloca(n * sizeof(LDOUBLE));
	    if(type == CPLXSXP) {
		R_CheckStack2(n * sizeof(LDOUBLE));
		ians = (LDOUBLE *) alloca(n * sizeof(LDOUBLE));
		Memzero(ians, n);
	    }
	    Memzero(rans, n);
	} else {
	    rans = R_Calloc(n, LDOUBLE);
	    if(type == CPLXSXP)
		ians = R_Calloc(n, LDOUBLE);
	}
	if (!keepNA && OP == 3) Cnt = R_Calloc(n, int);

	for (R_xlen_t j = 0; j < p; j++) {
	    LDOUBLE *ra = rans;
	    switch (type) {
	    case REALSXP:
	    {
		double *rx = REAL(x) + (R_xlen_t)n * j;
		if (keepNA)
		    for (R_xlen_t i = 0; i < n; i++) *ra++ += *rx++;
		else
		    for (R_xlen_t i = 0; i < n; i++, ra++, rx++)
			if (!ISNAN(*rx)) {
			    *ra += *rx;
			    if (OP == 3) Cnt[i]++;
			}
		break;
	    }
	    case INTSXP:
	    {
		int *ix = INTEGER(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_INTEGER) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_INTEGER) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    case LGLSXP:
	    {
		int *ix = LOGICAL(x) + (R_xlen_t)n * j;
		for (R_xlen_t i = 0; i < n; i++, ra++, ix++)
		    if (keepNA) {
			if (*ix != NA_LOGICAL) *ra += *ix;
			else *ra = NA_REAL;
		    }
		    else if (*ix != NA_LOGICAL) {
			*ra += *ix;
			if (OP == 3) Cnt[i]++;
		    }
		break;
	    }
	    case CPLXSXP:
	    {
		LDOUBLE *ia = ians;
		Rcomplex *cx = COMPLEX(x) + (R_xlen_t)n * j;

		if (keepNA)
		    for (R_xlen_t i = 0; i < n; i++, cx++) {
			*ra++ += cx->r;
			*ia++ += cx->i;
		    }
		else
		    for (R_xlen_t i = 0; i < n; i++, ra++, ia++, cx++)
			if (!ISNAN(cx->r) && !ISNAN(cx->i)) {
			    *ra += cx->r;
			    *ia += cx->i;
			    if (OP == 3) Cnt[i]++;
			}
		break;
	    }

	    } // switch()
	} // for(j ...)

	if (OP == 3) { /* mean() */
	    if (keepNA) {
		if(type == CPLXSXP) {
		    for (R_xlen_t i = 0; i < n; i++) { rans[i] /= p; ians[i] /= p; }
		} else {
		    for (R_xlen_t i = 0; i < n; i++) rans[i] /= p;
		}
	    } else {
		if(type == CPLXSXP) {
		    for (R_xlen_t i = 0; i < n; i++) {
			rans[i] /= Cnt[i]; ians[i] /= Cnt[i]; /* gives NaN for Cnt[i] = 0 */
		    }
		} else {
		    for (R_xlen_t i = 0; i < n; i++) rans[i] /= Cnt[i]; /* gives NaN for Cnt[i] = 0 */
		}
	    }
	}
	if(type == CPLXSXP) {
	    for (R_xlen_t i = 0; i < n; i++) {
		Rcomplex csum = { .r = rans[i], .i = ians[i] };
		COMPLEX(ans)[i] = csum;
	    }
	} else {
	    for (R_xlen_t i = 0; i < n; i++)
		REAL(ans)[i] = (double) rans[i];
	}

	if (!keepNA && OP == 3) R_Free(Cnt);
	if(n > 10000) {
	    R_Free(rans);
	    if(type == CPLXSXP)
		R_Free(ians);
	}
    }

    UNPROTECT(1);
    return ans;
}

/* Former  R version   of  array(data, dim, dimnames) :
{
    data <- as.vector(data)
    dim <- as.integer(dim)
    vl <- prod(dim)
    if (length(data) != vl) {
	if (vl > .Machine$integer.max)
	    stop("'dim' specifies too large an array")
	data <- rep(data, length.out = vl)
    }
    if (length(dim))
	dim(data) <- dim
    if (is.list(dimnames) && length(dimnames))
	dimnames(data) <- dimnames
    data
}
*/

/* array(data, dim, dimnames) */
attribute_hidden SEXP do_array(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, ans, dims, dimnames;
    R_xlen_t lendat, i, nans;

    checkArity(op, args);
    vals = CAR(args); // = data
    /* at least NULL can get here */
    switch(TYPEOF(vals)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case EXPRSXP:
	case VECSXP:
	    break;
	default:
	    error(_("'data' must be of a vector type, was '%s'"),
		R_typeToChar(vals));
    }
    lendat = XLENGTH(vals);
    dims = CADR(args);
    dimnames = CADDR(args);
    PROTECT(dims = coerceVector(dims, INTSXP));
    int nd = LENGTH(dims);
    if (nd == 0) error(_("'dims' cannot be of length 0"));
    double d = 1.0;
    for (int j = 0; j < nd; j++) d *= INTEGER(dims)[j];
#ifndef LONG_VECTOR_SUPPORT
    if (d > INT_MAX) error(_("too many elements specified"));
#endif
    nans = (R_xlen_t) d;

    PROTECT(ans = allocVector(TYPEOF(vals), nans));
    switch(TYPEOF(vals)) {
    case LGLSXP:
	if (nans && lendat)
	    xcopyLogicalWithRecycle(LOGICAL(ans), LOGICAL(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) LOGICAL(ans)[i] = NA_LOGICAL;
	break;
    case INTSXP:
	if (nans && lendat)
	    xcopyIntegerWithRecycle(INTEGER(ans), INTEGER(vals), 0, nans,
				    lendat);
	else
	    for (i = 0; i < nans; i++) INTEGER(ans)[i] = NA_INTEGER;
	break;
    case REALSXP:
	if (nans && lendat)
	    xcopyRealWithRecycle(REAL(ans), REAL(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) REAL(ans)[i] = NA_REAL;
	break;
    case CPLXSXP:
	if (nans && lendat)
	    xcopyComplexWithRecycle(COMPLEX(ans), COMPLEX(vals), 0, nans,
				    lendat);
	else {
	    Rcomplex na_cmplx;
	    na_cmplx.r = NA_REAL;
	    na_cmplx.i = 0;
	    for (i = 0; i < nans; i++) COMPLEX(ans)[i] = na_cmplx;
	}
	break;
    case RAWSXP:
	if (nans && lendat)
	    xcopyRawWithRecycle(RAW(ans), RAW(vals), 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) RAW(ans)[i] = 0;
	break;
    case STRSXP:
	if (nans && lendat)
	    xcopyStringWithRecycle(ans, vals, 0, nans, lendat);
	else
	    for (i = 0; i < nans; i++) SET_STRING_ELT(ans, i, NA_STRING);
	break;
    /* Rest are already initialized */
    case VECSXP:
    case EXPRSXP:
#ifdef SWITCH_TO_REFCNT
	if (nans && lendat)
	    xcopyVectorWithRecycle(ans, vals, 0, nans, lendat);
#else
	if (nans && lendat) {
	    /* Need to guard against possible sharing of values under
	       NAMED.  This is not needed with reference
	       coutning. (PR#15919) */
	    bool needsmark = (lendat < nans || MAYBE_REFERENCED(vals));
	    for (i = 0; i < nans; i++) {
		SEXP elt = VECTOR_ELT(vals, i % lendat);
		if (needsmark || MAYBE_REFERENCED(elt))
		    MARK_NOT_MUTABLE(elt);
		SET_VECTOR_ELT(ans, i, elt);
	    }
	}
#endif
	break;
    default:
	// excluded above
	break;
    }

    ans = dimgets(ans, dims);
    if(!isNull(dimnames) && length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);

    UNPROTECT(2);
    return ans;
}

attribute_hidden SEXP do_diag(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, snr, snc;
    int nr = 1, nc = 1, nprotect = 1;

    checkArity(op, args);
    x = CAR(args);
    snr = CADR(args);
    snc = CADDR(args);
    nr = asInteger(snr);
    if (nr == NA_INTEGER)
	error(_("invalid 'nrow' value (too large or NA)"));
    if (nr < 0)
	error(_("invalid 'nrow' value (< 0)"));
    nc = asInteger(snc);
    if (nc == NA_INTEGER)
	error(_("invalid 'ncol' value (too large or NA)"));
    if (nc < 0)
	error(_("invalid 'ncol' value (< 0)"));
    int mn = (nr < nc) ? nr : nc;
    if (mn > 0 && length(x) == 0)
	error(_("'x' must have positive length"));

#ifndef LONG_VECTOR_SUPPORT
   if ((double)nr * (double)nc > INT_MAX)
	error(_("too many elements specified"));
#endif

   int nx = LENGTH(x);
   R_xlen_t NR = nr;

#define mk_DIAG(_zero_)					\
   for (R_xlen_t i = 0; i < NR*nc; i++) ra[i] = _zero_;	\
   R_xlen_t i, i1;					\
   MOD_ITERATE1(mn, nx, i, i1, {			\
	   ra[i * (NR+1)] = rx[i1];			\
   });

   switch(TYPEOF(x)) {

   case REALSXP:
   {
#define mk_REAL_DIAG					\
       PROTECT(ans = allocMatrix(REALSXP, nr, nc));	\
       double *rx = REAL(x), *ra = REAL(ans);		\
       mk_DIAG(0.0)

       mk_REAL_DIAG;
       break;
   }
   case CPLXSXP:
   {
       PROTECT(ans = allocMatrix(CPLXSXP, nr, nc));
       int nx = LENGTH(x);
       R_xlen_t NR = nr;
       Rcomplex *rx = COMPLEX(x), *ra = COMPLEX(ans), zero;
       zero.r = zero.i = 0.0;
       mk_DIAG(zero);
       break;
   }
   case INTSXP:
   {
       PROTECT(ans = allocMatrix(INTSXP, nr, nc));
       int *rx = INTEGER(x), *ra = INTEGER(ans);
       mk_DIAG(0);
       break;
   }
   case LGLSXP:
   {
       PROTECT(ans = allocMatrix(LGLSXP, nr, nc));
       int *rx = LOGICAL(x), *ra = LOGICAL(ans);
       mk_DIAG(0);
       break;
   }
   case RAWSXP:
   {
       PROTECT(ans = allocMatrix(RAWSXP, nr, nc));
       Rbyte *rx = RAW(x), *ra = RAW(ans);
       mk_DIAG((Rbyte) 0);
       break;
   }
   default: {
       PROTECT(x = coerceVector(x, REALSXP));
       nprotect++;
       mk_REAL_DIAG;
     }
   }
#undef mk_REAL_DIAG
#undef mk_DIAG
   UNPROTECT(nprotect);
   return ans;
}


/* backsolve(r, b, k, upper.tri, transpose) */
attribute_hidden SEXP do_backsolve(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int nprot = 1;
    checkArity(op, args);

    SEXP r = CAR(args); args = CDR(args);
    SEXP b = CAR(args); args = CDR(args);
    int nrr = nrows(r), nrb = nrows(b), ncb = ncols(b);
    int k = asInteger(CAR(args)); args = CDR(args);
    /* k is the number of rows to be used: there must be at least that
       many rows and cols in the rhs and at least that many rows on
       the rhs.
    */
    if (k == NA_INTEGER || k <= 0 || k > nrr || k > ncols(r) || k > nrb)
	error(_("invalid '%s' argument"), "k");
    int upper = asLogical(CAR(args)); args = CDR(args);
    if (upper == NA_INTEGER) error(_("invalid '%s' argument"), "upper.tri");
    int trans = asLogical(CAR(args));
    if (trans == NA_INTEGER) error(_("invalid '%s' argument"), "transpose");
    if (TYPEOF(r) != REALSXP) {PROTECT(r = coerceVector(r, REALSXP)); nprot++;}
    if (TYPEOF(b) != REALSXP) {PROTECT(b = coerceVector(b, REALSXP)); nprot++;}
    double *rr = REAL(r);

    /* check for zeros on diagonal of r: only k row/cols are used. */
    size_t incr = nrr + 1;
    for(int i = 0; i < k; i++) { /* check for zeros on diagonal */
	if (rr[i * incr] == 0.0)
	    error(_("singular matrix in 'backsolve'. First zero in diagonal [%d]"),
		  i + 1);
    }

    SEXP ans = PROTECT(allocMatrix(REALSXP, k, ncb));
    if (k > 0 && ncb > 0) {
       /* copy (part) cols of b to ans */
	for(R_xlen_t j = 0; j < ncb; j++)
	    memcpy(REAL(ans) + j*k, REAL(b) + j*nrb, (size_t)k *sizeof(double));
	double one = 1.0;
	F77_CALL(dtrsm)("L", upper ? "U" : "L", trans ? "T" : "N", "N",
			&k, &ncb, &one, rr, &nrr, REAL(ans), &k
			FCONE FCONE FCONE FCONE);
    }
    UNPROTECT(nprot);
    return ans;
}

/* max.col(m, ties.method) */
attribute_hidden SEXP do_maxcol(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP m = CAR(args);
    int method = asInteger(CADR(args));
    int nr = nrows(m), nc = ncols(m), nprot = 1;
    if (TYPEOF(m) != REALSXP) {PROTECT(m = coerceVector(m, REALSXP)); nprot++;}
    SEXP ans = PROTECT(allocVector(INTSXP, nr));
    R_max_col(REAL(m), &nr, &nc, INTEGER(ans), &method);
    UNPROTECT(nprot);
    return ans;
}

#define ASPLIT_ITERATE( __body__ ) do {			\
	for(i = 0; i < n2; i++) {			\
	    PROTECT(e = allocVector(TYPEOF(x), n1));	\
	    for(j = 0; j < n1; j++, k++) {		\
		__body__ ;				\
	    }						\
	    if(keepdim) {				\
		e = dimgets(e, dc);			\
		if(havednc) {				\
		    e = dimnamesgets(e, dnc);		\
		}					\
	    }						\
	    UNPROTECT(1);				\
	    SET_VECTOR_ELT(y, i, e);			\
	}						\
    } while (0)

attribute_hidden SEXP do_asplit(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP x = CAR(args);   args = CDR(args);
    SEXP da = CAR(args);  args = CDR(args);
    SEXP dc = CAR(args);  args = CDR(args);
    SEXP dna = CAR(args); args = CDR(args);
    SEXP dnc = CAR(args); args = CDR(args);
    SEXP d1 = CAR(args);  args = CDR(args);
    SEXP d2 = CAR(args);  args = CDR(args);
    SEXP drop = CAR(args);
    SEXP y, e;
    int i, j, k, n1, n2;
    bool havednc, keepdim;
    n1 = asInteger(d1);
    n2 = asInteger(d2);
    havednc = (!isNull(dnc) && length(dnc) > 0);
    keepdim = (!asLogical(drop));
    PROTECT(y = allocVector(VECSXP, n2));
    y = dimgets(y, da);
    if(!isNull(dna) && (length(dna) > 0)) {
	y = dimnamesgets(y, dna);
    }
    k = 0;
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	ASPLIT_ITERATE( INTEGER(e)[j] = INTEGER(x)[k] );
	break;
    case REALSXP:
	ASPLIT_ITERATE( REAL(e)[j] = REAL(x)[k] );
	break;
    case CPLXSXP:
	ASPLIT_ITERATE( COMPLEX(e)[j] = COMPLEX(x)[k] );
	break;
    case STRSXP:
	ASPLIT_ITERATE( SET_STRING_ELT(e, j, STRING_ELT(x, k)) );
	break;
    case VECSXP:
	ASPLIT_ITERATE( SET_VECTOR_ELT(e, j, VECTOR_ELT(x, k)) );
	break;
    case RAWSXP:
	ASPLIT_ITERATE( RAW(e)[j] = RAW(x)[k] );
	break;
    default:
	UNIMPLEMENTED_TYPE("asplit", x);
    }
    UNPROTECT(1);
    return y;
}
