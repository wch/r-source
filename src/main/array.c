/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2012   The R Core Team
 *  Copyright (C) 2002-2008   The R Foundation
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <R_ext/RS.h>     /* for Calloc/Free */
#include <R_ext/Applic.h> /* for dgemm */

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

SEXP attribute_hidden do_matrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, ans, snr, snc, dimnames;
    int nr = 1, nc = 1, byrow, lendat, miss_nr, miss_nc;

    checkArity(op, args);
    vals = CAR(args); args = CDR(args);
    /* Supposedly as.vector() gave a vector type, but we check */
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
	    error(_("'data' must be of a vector type"));
    }
    lendat = length(vals);
    snr = CAR(args); args = CDR(args);
    snc = CAR(args); args = CDR(args);
    byrow = asLogical(CAR(args)); args = CDR(args);
    if (byrow == NA_INTEGER)
	error(_("invalid '%s' argument"), "byrow");
    dimnames = CAR(args);
    args = CDR(args);
    miss_nr = asLogical(CAR(args)); args = CDR(args);
    miss_nc = asLogical(CAR(args));

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
    if (miss_nr && miss_nc) nr = lendat;
    else if (miss_nr) nr = ceil(lendat/(double) nc);
    else if (miss_nc) nc = ceil(lendat/(double) nr);

    if(lendat > 0 ) {
	if (lendat > 1 && (nr * nc) % lendat != 0) {
	    if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
		((lendat < nr) && (nr / lendat) * lendat != nr))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of rows [%d]"), lendat, nr);
	    else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		     ((lendat < nc) && (nc / lendat) * lendat != nc))
		warning(_("data length [%d] is not a sub-multiple or multiple of the number of columns [%d]"), lendat, nc);
	}
	else if ((lendat > 1) && (nr * nc == 0)){
	    warning(_("data length exceeds size of matrix"));
	}
    }

    if ((double)nr * (double)nc > INT_MAX)
	error(_("too many elements specified"));

    PROTECT(ans = allocMatrix(TYPEOF(vals), nr, nc));
    if(lendat) {
	if (isVector(vals))
	    copyMatrix(ans, vals, byrow);
	else
	    copyListMatrix(ans, vals, byrow);
    } else if (isVector(vals)) { /* fill with NAs */
	int i, j;
	switch(TYPEOF(vals)) {
	case STRSXP:
	    for (j = 0; j < nc; j++)
		for (i = 0; i < nr; i++)
		    SET_STRING_ELT(ans, i + j * nr, NA_STRING);
	    break;
	case LGLSXP:
	    for (j = 0; j < nc; j++)
		for (i = 0; i < nr; i++)
		    LOGICAL(ans)[i + j * nr] = NA_LOGICAL;
	    break;
	case INTSXP:
	    for (j = 0; j < nc; j++)
		for (i = 0; i < nr; i++)
		    INTEGER(ans)[i + j * nr] = NA_INTEGER;
	    break;
	case REALSXP:
	    for (j = 0; j < nc; j++)
		for (i = 0; i < nr; i++)
		    REAL(ans)[i + j * nr] = NA_REAL;
	    break;
	case CPLXSXP:
	    {
		Rcomplex na_cmplx;
		na_cmplx.r = NA_REAL;
		na_cmplx.i = 0;
		for (j = 0; j < nc; j++)
		    for (i = 0; i < nr; i++)
			COMPLEX(ans)[i + j * nr] = na_cmplx;
	    }
	    break;
	case RAWSXP:
	    for (j = 0; j < nc; j++)
		for (i = 0; i < nr; i++)
		    RAW(ans)[i + j * nr] = 0;
	    break;
	default:
	    /* don't fill with anything */
	    ;
	}
    }
    if(!isNull(dimnames)&& length(dimnames) > 0)
	ans = dimnamesgets(ans, dimnames);
    UNPROTECT(1);
    return ans;
}


SEXP allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    SEXP s, t;
    int n;

    if (nrow < 0 || ncol < 0)
	error(_("negative extents to matrix"));
    if ((double)nrow * (double)ncol > INT_MAX)
	error(_("allocMatrix: too many elements specified"));
    n = nrow * ncol;
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
    int n;

    if (nrow < 0 || ncol < 0 || nface < 0)
	error(_("negative extents to 3D array"));
    if ((double)nrow * (double)ncol * (double)nface > INT_MAX)
	error(_("alloc3Darray: too many elements specified"));
    n = nrow * ncol * nface;
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
    int i, n;
    double dn;

    dn = n = 1;
    for (i = 0; i < LENGTH(dims); i++) {
	dn *= INTEGER(dims)[i];
	if(dn > INT_MAX)
	    error(_("allocArray: too many elements specified by 'dims'"));
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

SEXP DropDims(SEXP x)
{
    SEXP dims, dimnames, newnames = R_NilValue;
    int i, n, ndims;

    PROTECT(x);
    dims = getAttrib(x, R_DimSymbol);
    dimnames = getAttrib(x, R_DimNamesSymbol);

    /* Check that dropping will actually do something. */
    /* (1) Check that there is a "dim" attribute. */

    if (dims == R_NilValue) {
	UNPROTECT(1);
	return x;
    }
    ndims = LENGTH(dims);

    /* (2) Check whether there are redundant extents */
    n = 0;
    for (i = 0; i < ndims; i++)
	if (INTEGER(dims)[i] != 1) n++;
    if (n == ndims) {
	UNPROTECT(1);
	return x;
    }

    if (n <= 1) {
	/* We have reduced to a vector result.
	   If that has length one, it is ambiguous which dimnames to use,
	   so use it if there is only one (as from R 2.7.0).
	 */
	if (dimnames != R_NilValue) {
	    if(LENGTH(x) != 1) {
		for (i = 0; i < LENGTH(dims); i++) {
		    if (INTEGER(dims)[i] != 1) {
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
/* 	if(IS_S4_OBJECT(x)) {/\* no longer valid subclass of array or
 	matrix *\/ */
/* 	    setAttrib(x, R_ClassSymbol, R_NilValue); */
/* 	    UNSET_S4_OBJECT(x); */
/* 	} */
	UNPROTECT(1);
    } else {
	/* We have a lower dimensional array. */
	SEXP newdims, dnn, newnamesnames = R_NilValue;
	dnn = getAttrib(dimnames, R_NamesSymbol);
	PROTECT(newdims = allocVector(INTSXP, n));
	for (i = 0, n = 0; i < ndims; i++)
	    if (INTEGER(dims)[i] != 1)
		INTEGER(newdims)[n++] = INTEGER(dims)[i];
	if (!isNull(dimnames)) {
	    int havenames = 0;
	    for (i = 0; i < ndims; i++)
		if (INTEGER(dims)[i] != 1 &&
		    VECTOR_ELT(dimnames, i) != R_NilValue)
		    havenames = 1;
	    if (havenames) {
		PROTECT(newnames = allocVector(VECSXP, n));
		PROTECT(newnamesnames = allocVector(STRSXP, n));
		for (i = 0, n = 0; i < ndims; i++) {
		    if (INTEGER(dims)[i] != 1) {
			if(!isNull(dnn))
			    SET_STRING_ELT(newnamesnames, n,
					   STRING_ELT(dnn, i));
			SET_VECTOR_ELT(newnames, n++, VECTOR_ELT(dimnames, i));
		    }
		}
	    }
	    else dimnames = R_NilValue;
	}
	PROTECT(dimnames);
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, newdims);
	if (dimnames != R_NilValue)
	{
	    if(!isNull(dnn))
		setAttrib(newnames, R_NamesSymbol, newnamesnames);
	    setAttrib(x, R_DimNamesSymbol, newnames);
	    UNPROTECT(2);
	}
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return x;
}

SEXP attribute_hidden do_drop(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	    if (NAMED(x)) x = duplicate(x);
	    x = DropDims(x);
	}
    }
    return x;
}

/* Length of Primitive Objects */

SEXP attribute_hidden do_length(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    SEXP x = CAR(args), ans;

    if (isObject(x) &&
       DispatchOrEval(call, op, "length", args, rho, &ans, 0, 1)) {
	if (length(ans) == 1 && TYPEOF(ans) == REALSXP) {
	    double d = REAL(ans)[0];
	    if (R_FINITE(d) && d >= 0. && d <= INT_MAX && floor(d) == d)
		return coerceVector(ans, INTSXP);
	}
	return(ans);
    }

    /* with current defn of R_len_t this cannot happen */
    R_len_t len = length(x);
    return ScalarInteger((len <= INT_MAX) ? len : NA_INTEGER);
}


SEXP attribute_hidden do_rowscols(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, ans;
    int i, j, nr, nc;

    checkArity(op, args);
    x = CAR(args);
    if (!isInteger(x) || LENGTH(x) != 2)
	error(_("a matrix-like object is required as argument to 'row/col'"));

    nr = INTEGER(x)[0];
    nc = INTEGER(x)[1];

    ans = allocMatrix(INTSXP, nr, nc);

    switch (PRIMVAL(op)) {
    case 1:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * nr] = i + 1;
	break;
    case 2:
	for (i = 0; i < nr; i++)
	    for (j = 0; j < nc; j++)
		INTEGER(ans)[i + j * nr] = j + 1;
	break;
    }
    return ans;
}

static void matprod(double *x, int nrx, int ncx,
		    double *y, int nry, int ncy, double *z)
{
    char *transa = "N", *transb = "N";
    int i,  j, k;
    double one = 1.0, zero = 0.0;
    long double sum;
    Rboolean have_na = FALSE;

    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	/* Don't trust the BLAS to handle NA/NaNs correctly: PR#4582
	 * The test is only O(n) here
	 */
	for (i = 0; i < nrx*ncx; i++)
	    if (ISNAN(x[i])) {have_na = TRUE; break;}
	if (!have_na)
	    for (i = 0; i < nry*ncy; i++)
		if (ISNAN(y[i])) {have_na = TRUE; break;}
	if (have_na) {
	    for (i = 0; i < nrx; i++)
		for (k = 0; k < ncy; k++) {
		    sum = 0.0;
		    for (j = 0; j < ncx; j++)
			sum += x[i + j * nrx] * y[j + k * nry];
		    z[i + k * nrx] = sum;
		}
	} else
	    F77_CALL(dgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
			    x, &nrx, y, &nry, &zero, z, &nrx);
    } else /* zero-extent operations should return zeroes */
	for(i = 0; i < nrx*ncy; i++) z[i] = 0;
}

static void cmatprod(Rcomplex *x, int nrx, int ncx,
		     Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    char *transa = "N", *transb = "N";
    int i;
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &ncy, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nrx*ncy; i++) z[i].r = z[i].i = 0;
    }
#else
    int i, j, k;
    double xij_r, xij_i, yjk_r, yjk_i;
    long double sum_i, sum_r;

    for (i = 0; i < nrx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * nrx].r = NA_REAL;
	    z[i + k * nrx].i = NA_REAL;
	    sum_r = 0.0;
	    sum_i = 0.0;
	    for (j = 0; j < ncx; j++) {
		xij_r = x[i + j * nrx].r;
		xij_i = x[i + j * nrx].i;
		yjk_r = y[j + k * nry].r;
		yjk_i = y[j + k * nry].i;
		if (ISNAN(xij_r) || ISNAN(xij_i)
		    || ISNAN(yjk_r) || ISNAN(yjk_i))
		    goto next_ik;
		sum_r += (xij_r * yjk_r - xij_i * yjk_i);
		sum_i += (xij_r * yjk_i + xij_i * yjk_r);
	    }
	    z[i + k * nrx].r = sum_r;
	    z[i + k * nrx].i = sum_i;
	next_ik:
	    ;
	}
#endif
}

static void symcrossprod(double *x, int nr, int nc, double *z)
{
    char *trans = "T", *uplo = "U";
    double one = 1.0, zero = 0.0;
    int i, j;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nc, &nr, &one, x, &nr, &zero, z, &nc);
	for (i = 1; i < nc; i++)
	    for (j = 0; j < i; j++) z[i + nc *j] = z[j + nc * i];
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nc*nc; i++) z[i] = 0;
    }

}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    char *transa = "T", *transb = "N";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < ncx*ncy; i++) z[i] = 0;
    }
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    char *transa = "T", *transb = "N";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &ncx, &ncy, &nrx, &one,
			x, &nrx, y, &nry, &zero, z, &ncx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < ncx*ncy; i++) z[i].r = z[i].i = 0;
    }
}

static void symtcrossprod(double *x, int nr, int nc, double *z)
{
    char *trans = "N", *uplo = "U";
    double one = 1.0, zero = 0.0;
    int i, j;
    if (nr > 0 && nc > 0) {
	F77_CALL(dsyrk)(uplo, trans, &nr, &nc, &one, x, &nr, &zero, z, &nr);
	for (i = 1; i < nr; i++)
	    for (j = 0; j < i; j++) z[i + nr *j] = z[j + nr * i];
    } else { /* zero-extent operations should return zeroes */
	for(i = 0; i < nr*nr; i++) z[i] = 0;
    }

}

static void tcrossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    char *transa = "N", *transb = "T";
    double one = 1.0, zero = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(dgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < nrx*nry; i++) z[i] = 0;
    }
}

static void tccrossprod(Rcomplex *x, int nrx, int ncx,
			Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    char *transa = "N", *transb = "T";
    Rcomplex one, zero;

    one.r = 1.0; one.i = zero.r = zero.i = 0.0;
    if (nrx > 0 && ncx > 0 && nry > 0 && ncy > 0) {
	F77_CALL(zgemm)(transa, transb, &nrx, &nry, &ncx, &one,
			x, &nrx, y, &nry, &zero, z, &nrx);
    } else { /* zero-extent operations should return zeroes */
	int i;
	for(i = 0; i < nrx*nry; i++) z[i].r = z[i].i = 0;
    }
}


/* "%*%" (op = 0), crossprod (op = 1) or tcrossprod (op = 2) */
SEXP attribute_hidden do_matprod(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ldx, ldy, nrx, ncx, nry, ncy, mode;
    SEXP x = CAR(args), y = CADR(args), xdims, ydims, ans;
    Rboolean sym;

    if(PRIMVAL(op) == 0 && /* %*% is primitive, the others are .Internal() */
       (IS_S4_OBJECT(x) || IS_S4_OBJECT(y))
       && R_has_methods(op)) {
	SEXP s, value;
	/* Remove argument names to ensure positional matching */
	for(s = args; s != R_NilValue; s = CDR(s)) SET_TAG(s, R_NilValue);
	value = R_possible_dispatch(call, op, args, rho, FALSE);
	if(value) return value;
    }

    sym = isNull(y);
    if (sym && (PRIMVAL(op) > 0)) y = x;
    if ( !(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y)) )
	errorcall(call, _("requires numeric/complex matrix/vector arguments"));

    xdims = getAttrib(x, R_DimSymbol);
    ydims = getAttrib(y, R_DimSymbol);
    ldx = length(xdims);
    ldy = length(ydims);

    if (ldx != 2 && ldy != 2) {		/* x and y non-matrices */
	if (PRIMVAL(op) == 0) {
	    nrx = 1;
	    ncx = LENGTH(x);
	}
	else {
	    nrx = LENGTH(x);
	    ncx = 1;
	}
	nry = LENGTH(y);
	ncy = 1;
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
	    }
	}
	else { /* tcrossprod --		y is a col vector */
	    nry = LENGTH(y);
	    ncy = 1;
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

    if (isComplex(CAR(args)) || isComplex(CADR(args)))
	mode = CPLXSXP;
    else
	mode = REALSXP;
    SETCAR(args, coerceVector(CAR(args), mode));
    SETCADR(args, coerceVector(CADR(args), mode));

    if (PRIMVAL(op) == 0) {			/* op == 0 : matprod() */

	PROTECT(ans = allocMatrix(mode, nrx, ncy));
	if (mode == CPLXSXP)
	    cmatprod(COMPLEX(CAR(args)), nrx, ncx,
		     COMPLEX(CADR(args)), nry, ncy, COMPLEX(ans));
	else
	    matprod(REAL(CAR(args)), nrx, ncx,
		    REAL(CADR(args)), nry, ncy, REAL(ans));

	PROTECT(xdims = getAttrib(CAR(args), R_DimNamesSymbol));
	PROTECT(ydims = getAttrib(CADR(args), R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx == 1) {
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    dnx = getAttrib(xdims, R_NamesSymbol);
		    if(!isNull(dnx))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 0));
		}
	    }

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
									\
	    /* We sometimes attach a dimnames attribute			\
	     * whose elements are all NULL ...				\
	     * This is ugly but causes no real damage.			\
	     * Now (2.1.0 ff), we don't anymore: */			\
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||			\
		VECTOR_ELT(dimnames,1) != R_NilValue) {			\
		if (dnx != R_NilValue || dny != R_NilValue)		\
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);	\
		setAttrib(ans, R_DimNamesSymbol, dimnames);		\
	    }								\
	    UNPROTECT(2)

	    YDIMS_ET_CETERA;
	}
    }

    else if (PRIMVAL(op) == 1) {	/* op == 1: crossprod() */

	PROTECT(ans = allocMatrix(mode, ncx, ncy));
	if (mode == CPLXSXP)
	    if(sym)
		ccrossprod(COMPLEX(CAR(args)), nrx, ncx,
			   COMPLEX(CAR(args)), nry, ncy, COMPLEX(ans));
	    else
		ccrossprod(COMPLEX(CAR(args)), nrx, ncx,
			   COMPLEX(CADR(args)), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symcrossprod(REAL(CAR(args)), nrx, ncx, REAL(ans));
	    else
		crossprod(REAL(CAR(args)), nrx, ncx,
			  REAL(CADR(args)), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(CAR(args), R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(CADR(args), R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

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
		tccrossprod(COMPLEX(CAR(args)), nrx, ncx,
			    COMPLEX(CAR(args)), nry, ncy, COMPLEX(ans));
	    else
		tccrossprod(COMPLEX(CAR(args)), nrx, ncx,
			    COMPLEX(CADR(args)), nry, ncy, COMPLEX(ans));
	else {
	    if(sym)
		symtcrossprod(REAL(CAR(args)), nrx, ncx, REAL(ans));
	    else
		tcrossprod(REAL(CAR(args)), nrx, ncx,
			   REAL(CADR(args)), nry, ncy, REAL(ans));
	}

	PROTECT(xdims = getAttrib(CAR(args), R_DimNamesSymbol));
	if (sym)
	    PROTECT(ydims = xdims);
	else
	    PROTECT(ydims = getAttrib(CADR(args), R_DimNamesSymbol));

	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;

	    /* allocate dimnames and dimnamesnames */

	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));

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
	    if (VECTOR_ELT(dimnames,0) != R_NilValue ||
		VECTOR_ELT(dimnames,1) != R_NilValue) {
		if (dnx != R_NilValue || dny != R_NilValue)
		    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
		setAttrib(ans, R_DimNamesSymbol, dimnames);
	    }

	    UNPROTECT(2);
	}
    }
    UNPROTECT(3);
    return ans;
}
#undef YDIMS_ET_CETERA

SEXP attribute_hidden do_transpose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, r, dims, dimnames, dimnamesnames=R_NilValue,
	ndimnamesnames, rnames, cnames;
    int i, ldim, len = 0, ncol=0, nrow=0;

    checkArity(op, args);
    a = CAR(args);

    if (isVector(a)) {
	dims = getAttrib(a, R_DimSymbol);
	ldim = length(dims);
	rnames = R_NilValue;
	cnames = R_NilValue;
	switch(ldim) {
	case 0:
	    nrow = len = length(a);
	    ncol = 1;
	    rnames = getAttrib(a, R_NamesSymbol);
	    dimnames = rnames;/* for isNull() below*/
	    break;
	case 1:
	    nrow = len = length(a);
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
	    len = length(a);
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
    PROTECT(r = allocVector(TYPEOF(a), len));
    int j, l_1 = len-1;
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
        UNPROTECT(1);
        goto not_matrix;
    }
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1);
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
	    UNPROTECT(1);
	}
	setAttrib(r, R_DimNamesSymbol, dimnames);
	UNPROTECT(1);
    }
    copyMostAttrib(a, r);
    UNPROTECT(1);
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

/* this increments iip and sets j using strides */

#define CLICKJ						\
    for (itmp = 0; itmp < n; itmp++)			\
	if (iip[itmp] == isr[itmp]-1) iip[itmp] = 0;	\
	else {						\
	    iip[itmp]++;				\
	    break;					\
	}						\
    for (j = 0, itmp = 0; itmp < n; itmp++)	       	\
	j += iip[itmp] * stride[itmp];

/* aperm (a, perm, resize = TRUE) */
SEXP attribute_hidden do_aperm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, perm, r, dimsa, dimsr, dna;
    int i, j, n, len, itmp;

    checkArity(op, args);

    a = CAR(args);
    if (!isArray(a))
	error(_("invalid first argument, must be an array"));

    PROTECT(dimsa = getAttrib(a, R_DimSymbol));
    n = LENGTH(dimsa);
    int *isa = INTEGER(dimsa);

    /* check the permutation */

    int *pp = (int *) R_alloc((size_t) n, sizeof(int));
    perm = CADR(args);
    if (length(perm) == 0) {
	for (i = 0; i < n; i++) pp[i] = n-1-i;
    } else if (isString(perm)) {
	SEXP dna = getAttrib(a, R_DimNamesSymbol);
	if (isNull(dna))
	    error(_("'a' does not have named dimnames"));
	SEXP dnna = getAttrib(dna, R_NamesSymbol);
	if (isNull(dnna))
	    error(_("'a' does not have named dimnames"));
	for (i = 0; i < n; i++) {
	    const char *this = translateChar(STRING_ELT(perm, i));
	    for (j = 0; j < n; j++)
		if (streql(translateChar(STRING_ELT(dnna, j)),
			   this)) {pp[i] = j; break;}
	    if (j >= n)
		error(_("perm[%d] does not match a dimension name"), i+1);
	}
    } else {
	PROTECT(perm = coerceVector(perm, INTSXP));
	if (length(perm) == n) {
	    for (i = 0; i < n; i++) pp[i] = INTEGER(perm)[i] - 1;
	    UNPROTECT(1);
	} else error(_("'perm' is of wrong length"));
    }

    int *iip = (int *) R_alloc((size_t) n, sizeof(int));
    for (i = 0; i < n; iip[i++] = 0);
    for (i = 0; i < n; i++)
	if (pp[i] >= 0 && pp[i] < n) iip[pp[i]]++;
	else error(_("value out of range in 'perm'"));
    for (i = 0; i < n; i++)
	if (iip[i] == 0) error(_("invalid '%s' argument"), "perm");

    /* create the stride object and permute */

    int *stride = (int *) R_alloc((size_t) n, sizeof(int));
    for (iip[0] = 1, i = 1; i<n; i++) iip[i] = iip[i-1] * isa[i-1];
    for (i = 0; i < n; i++) stride[i] = iip[pp[i]];

    /* also need to have the dimensions of r */

    PROTECT(dimsr = allocVector(INTSXP, n));
    int *isr = INTEGER(dimsr);
    for (i = 0; i < n; i++) isr[i] = isa[pp[i]];

    /* and away we go! iip will hold the incrementer */

    len = LENGTH(a);
    len = length(a);
    PROTECT(r = allocVector(TYPEOF(a), len));

    for (i = 0; i < n; iip[i++] = 0);

    switch (TYPEOF(a)) {

    case INTSXP:
	for (j=0, i=0; i < len; i++) {
	    INTEGER(r)[i] = INTEGER(a)[j];
	    CLICKJ;
	}
	break;

    case LGLSXP:
	for (j=0, i=0; i < len; i++) {
	    LOGICAL(r)[i] = LOGICAL(a)[j];
	    CLICKJ;
	}
	break;

    case REALSXP:
	for (j=0, i=0; i < len; i++) {
	    REAL(r)[i] = REAL(a)[j];
	    CLICKJ;
	}
	break;

    case CPLXSXP:
	for (j=0, i=0; i < len; i++) {
	    COMPLEX(r)[i].r = COMPLEX(a)[j].r;
	    COMPLEX(r)[i].i = COMPLEX(a)[j].i;
	    CLICKJ;
	}
	break;

    case STRSXP:
	for (j=0, i=0; i < len; i++) {
	    SET_STRING_ELT(r, i, STRING_ELT(a, j));
	    CLICKJ;
	}
	break;

    case VECSXP:
	for (j=0, i=0; i < len; i++) {
	    SET_VECTOR_ELT(r, i, VECTOR_ELT(a, j));
	    CLICKJ;
	}
	break;

    case RAWSXP:
	for (j=0, i=0; i < len; i++) {
	    RAW(r)[i] = RAW(a)[j];
	    CLICKJ;
	}
	break;

    default:
	UNIMPLEMENTED_TYPE("aperm", a);
    }

    /* handle the resize */
    int resize = asLogical(CADDR(args));
    if (resize == NA_LOGICAL) error(_("'resize' must be TRUE or FALSE"));
    setAttrib(r, R_DimSymbol, resize ? dimsr : dimsa);

    /* and handle the dimnames, if any */
    if (resize) {
	PROTECT(dna = getAttrib(a, R_DimNamesSymbol));
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

    UNPROTECT(3); /* dimsa, r, dimsr */
    return r;
}

/* colSums(x, n, p, na.rm) and friends */
SEXP attribute_hidden do_colsum(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, ans = R_NilValue;
    int OP, n, p, cnt = 0, i, j, type;
    Rboolean NaRm, keepNA;
    int *ix;
    double *rx;
    long double sum = 0.0;
#ifdef HAVE_OPENMP
    int nthreads;
#endif

    checkArity(op, args);
    x = CAR(args); args = CDR(args);
    n = asInteger(CAR(args)); args = CDR(args);
    p = asInteger(CAR(args)); args = CDR(args);
    NaRm = asLogical(CAR(args));
    if (n == NA_INTEGER || n < 0)
	error(_("invalid '%s' argument"), "n");
    if (p == NA_INTEGER || p < 0)
	error(_("invalid '%s' argument"), "p");
    if (NaRm == NA_LOGICAL) error(_("invalid '%s' argument"), "na.rm");
    keepNA = !NaRm;

    OP = PRIMVAL(op);
    switch (type = TYPEOF(x)) {
    case LGLSXP: break;
    case INTSXP: break;
    case REALSXP: break;
    default:
	error(_("'x' must be numeric"));
    }

    if (OP == 0 || OP == 1) { /* columns */
	cnt = n;
	PROTECT(ans = allocVector(REALSXP, p));
#ifdef HAVE_OPENMP
	/* This gives a spurious -Wunused-but-set-variable error */
	if (R_num_math_threads > 0)
	    nthreads = R_num_math_threads;
	else
	    nthreads = 1; /* for now */
#pragma omp parallel for num_threads(nthreads) default(none) \
    private(j, i, ix, rx) \
    firstprivate(x, ans, n, p, type, cnt, sum, \
		 NaRm, keepNA, R_NaReal, R_NaInt, OP)
#endif
	for (j = 0; j < p; j++) {
	    switch (type) {
	    case REALSXP:
		rx = REAL(x) + n*j;
		if (keepNA)
		    for (sum = 0., i = 0; i < n; i++) sum += *rx++;
		else {
		    for (cnt = 0, sum = 0., i = 0; i < n; i++, rx++)
			if (!ISNAN(*rx)) {cnt++; sum += *rx;}
			else if (keepNA) {sum = NA_REAL; break;}
		}
		break;
	    case INTSXP:
		ix = INTEGER(x) + n*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_INTEGER) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    case LGLSXP:
		ix = LOGICAL(x) + n*j;
		for (cnt = 0, sum = 0., i = 0; i < n; i++, ix++)
		    if (*ix != NA_LOGICAL) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    default:
		/* we checked the type above, but be sure */
		UNIMPLEMENTED_TYPEt("do_colsum", type);
	    }
	    if (OP == 1) sum /= cnt;
	    REAL(ans)[j] = sum;
	}
    }

    if (OP == 2 || OP == 3) { /* rows */
	cnt = p;
	PROTECT(ans = allocVector(REALSXP, n));

	/* interchange summation order to improve cache hits */
	if (type == REALSXP) {
	    int *Cnt = NULL, *c;
	    long double *rans, *ra;
	    if(n <= 10000) {
		rans = (long double *) alloca(n * sizeof(long double));
		R_CheckStack();
		memset(rans, 0, n*sizeof(long double));
	    } else rans = Calloc(n, long double);
	    rx = REAL(x);
	    if (!keepNA && OP == 3) Cnt = Calloc(n, int);
	    for (j = 0; j < p; j++) {
		ra = rans;
		if (keepNA)
		    for (i = 0; i < n; i++) *ra++ += *rx++;
		else
		    for (c = Cnt, i = 0; i < n; i++, ra++, rx++, c++)
			if (!ISNAN(*rx)) {
			    *ra += *rx;
			    if (OP == 3) (*c)++;
			}
	    }
	    if (OP == 3) {
		if (keepNA)
		    for (ra = rans, i = 0; i < n; i++) *ra++ /= p;
		else {
		    for (ra = rans, c = Cnt, i = 0; i < n; i++, c++)
			*ra++ /= *c;
		    Free(Cnt);
		}
	    }
	    for (i = 0; i < n; i++) REAL(ans)[i] = rans[i];
	    if(n > 10000) Free(rans);
	    UNPROTECT(1);
	    return ans;
	}

	for (i = 0; i < n; i++) {
	    switch (type) {
	    case INTSXP:
		ix = INTEGER(x) + i;
		for (cnt = 0, sum = 0., j = 0; j < p; j++, ix += n)
		    if (*ix != NA_INTEGER) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    case LGLSXP:
		ix = LOGICAL(x) + i;
		for (cnt = 0, sum = 0., j = 0; j < p; j++, ix += n)
		    if (*ix != NA_LOGICAL) {cnt++; sum += *ix;}
		    else if (keepNA) {sum = NA_REAL; break;}
		break;
	    default:
		/* we checked the type above, but be sure */
		UNIMPLEMENTED_TYPEt("do_colsum", type);
	    }
	    if (OP == 3) sum /= cnt; /* gives NaN for cnt = 0 */
	    REAL(ans)[i] = sum;
	}
    }

    UNPROTECT(1);
    return ans;
}

/*
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
SEXP attribute_hidden do_array(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, ans, dims, dimnames;
    R_len_t lendat, i, nans;

    checkArity(op, args);
    vals = CAR(args);
    /* Supposedly as.vector() gave a vector type, but we check before
       calling XLENGTH */
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
	    error(_("'data' must be of a vector type"));
    }
    lendat = LENGTH(vals);
    dims = CADR(args);
    dimnames = CADDR(args);
    PROTECT(dims = coerceVector(dims, INTSXP));
    int nd = LENGTH(dims);
    if (nd) {
	double d = 1.0;
	for (int j = 0; j < nd; j++) d *= INTEGER(dims)[j];
	if (d > INT_MAX) error(_("'dim' specifies too large an array"));
	nans = (R_len_t) d;
    } else {
	// tmp message so do not translate.
	warning("use of 0-length dim is deprecated");
	nans = 1;
    }

    PROTECT(ans = allocVector(TYPEOF(vals), nans));
    switch(TYPEOF(vals)) {
    case LGLSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++)
		LOGICAL(ans)[i] = LOGICAL(vals)[i % lendat];
	else
	    for (i = 0; i < nans; i++) LOGICAL(ans)[i] = NA_LOGICAL;
	break;
    case INTSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++)
		INTEGER(ans)[i] = INTEGER(vals)[i % lendat];
	else
	    for (i = 0; i < nans; i++) INTEGER(ans)[i] = NA_INTEGER;
	break;
    case REALSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++) REAL(ans)[i] = REAL(vals)[i % lendat];
	else
	    for (i = 0; i < nans; i++) REAL(ans)[i] = NA_REAL;
	break;
    case CPLXSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++)
		COMPLEX(ans)[i] = COMPLEX(vals)[i % lendat];
	else {
	    Rcomplex na_cmplx;
	    na_cmplx.r = NA_REAL;
	    na_cmplx.i = 0;
	    for (i = 0; i < nans; i++) COMPLEX(ans)[i] = na_cmplx;
	}
	break;
    case RAWSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++) RAW(ans)[i] = RAW(vals)[i % lendat];
	else
	    for (i = 0; i < nans; i++) RAW(ans)[i] = 0;
	break;
    /* Rest are already initialized */
    case STRSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++)
		SET_STRING_ELT(ans, i, STRING_ELT(vals, i % lendat));
	break;
    case VECSXP:
    case EXPRSXP:
	if (nans && lendat)
	    for (i = 0; i < nans; i++)
		SET_VECTOR_ELT(ans, i, VECTOR_ELT(vals, i % lendat));
	break;
    default:
	// excluded above
	break;
    }

    if (nd) {
	ans = dimgets(ans, dims);
	if (TYPEOF(dimnames) == VECSXP && LENGTH(dimnames)) {
	    PROTECT(ans);
	    ans = dimnamesgets(ans, dimnames);
	    UNPROTECT(1);
	}
    } else if (TYPEOF(dimnames) == VECSXP && LENGTH(dimnames))
	error(_("'dimnames' applied to non-array"));

    UNPROTECT(2);
    return ans;
}
