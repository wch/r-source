/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001   The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>

/* "GetRowNames" and "GetColNames" are utility routines which */
/* locate and return the row names and column names from the */
/* dimnames attribute of a matrix.  They are useful because */
/* old versions of R used pair-based lists for dimnames */
/* whereas recent versions use vector bassed lists */

/* FIXME : This is nonsense.  When the "dimnames" attribute is */
/* grabbed off an array it is always adjusted to be a vector. */

SEXP GetRowNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 0);
    else if (TYPEOF(dimnames) == LISTSXP)
	return CAR(dimnames);
    else
	return R_NilValue;
}

SEXP GetColNames(SEXP dimnames)
{
    if (TYPEOF(dimnames) == VECSXP)
	return VECTOR_ELT(dimnames, 1);
    else if (TYPEOF(dimnames) == LISTSXP)
	return CADR(dimnames);
    else
	return R_NilValue;
}

SEXP do_matrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals, snr, snc;
    int nr, nc, byrow, lendat;

    checkArity(op, args);
    vals = CAR(args);
    snr = CADR(args);
    snc = CADDR(args);
    byrow = asInteger(CADR(CDDR(args)));

    if (isVector(vals) || isList(vals)) {
	if (length(vals) < 0)
	    errorcall(call, "argument has length zero");
    }
    else errorcall(call, "invalid matrix element type");

    if (!isNumeric(snr) || !isNumeric(snc))
	error("non-numeric matrix extent");

    lendat = length(vals);
    nr = asInteger(snr);
    nc = asInteger(snc);

    if (lendat > 1 && (nr * nc) % lendat != 0) {
	if (((lendat > nr) && (lendat / nr) * nr != lendat) ||
	    ((lendat < nr) && (nr / lendat) * lendat != nr))
	    warning("Replacement length not a multiple of the elements to replace in matrix(...)");
	else if (((lendat > nc) && (lendat / nc) * nc != lendat) ||
		 ((lendat < nc) && (nc / lendat) * lendat != nc))
	    warning("Replacement length not a multiple of the elements to replace in matrix(...)");
    }
	else if ((lendat > 1) && (nr * nc == 0)){
	  warning("Replacement length not a multiple of the elements to replace in matrix(...)");
	}
	else if (lendat == 0 && nr * nc > 0){
	  error("No data to replace in matrix(...)");
	}

    PROTECT(snr = allocMatrix(TYPEOF(vals), nr, nc));
    if (isVector(vals))
	copyMatrix(snr, vals, byrow);
    else
	copyListMatrix(snr, vals, byrow);
    UNPROTECT(1);
    return snr;
}


SEXP allocMatrix(SEXPTYPE mode, int nrow, int ncol)
{
    SEXP s, t;
    int n;

    if (nrow < 0 || ncol < 0)
	error("negative extents to matrix");
    n = nrow * ncol;
    PROTECT(s = allocVector(mode, n));
    PROTECT(t = allocVector(INTSXP, 2));
    INTEGER(t)[0] = nrow;
    INTEGER(t)[1] = ncol;
    setAttrib(s, R_DimSymbol, t);
    UNPROTECT(2);
    return s;
}


SEXP allocArray(SEXPTYPE mode, SEXP dims)
{
    SEXP array;
    int i, n;

    n = 1;
    for (i = 0; i < LENGTH(dims); i++)
	n = n * INTEGER(dims)[i];

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
    SEXP q, dims, dimnames, newnames = R_NilValue;
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
	/* We have reduced to a vector result. */
	if (dimnames != R_NilValue) {
	    n = length(dims);
	    if (TYPEOF(dimnames) == VECSXP) {
		for (i = 0; i < n; i++) {
		    if (INTEGER(dims)[i] != 1) {
			newnames = VECTOR_ELT(dimnames, i);
			break;
		    }
		}
	    }
	    else {
		q = dimnames;
		for (i = 0; i < n; i++) {
		    if (INTEGER(dims)[i] != 1) {
			newnames = CAR(q);
			break;
		    }
		    q = CDR(q);
		}
	    }
	}
	PROTECT(newnames);
	setAttrib(x, R_DimNamesSymbol, R_NilValue);
	setAttrib(x, R_DimSymbol, R_NilValue);
	setAttrib(x, R_NamesSymbol, newnames);
	UNPROTECT(1);
    }
    else {
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

SEXP do_drop(SEXP call, SEXP op, SEXP args, SEXP rho)
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

SEXP do_length(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    if (length(args) != 1)
	error("incorrect number of args to length");
    ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = length(CAR(args));
    return ans;
}


SEXP do_rowscols(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int i, j, nr, nc;

    if (length(args) != 1)
	error("incorrect number of args to row/col");
    if (!isMatrix(CAR(args)))
	error("a matrix is required as arg to row/col");

    nr = nrows(CAR(args));
    nc = ncols(CAR(args));

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

/* FIXME - What about non-IEEE overflow ??? */
/* Does it really matter? */

static void matprod(double *x, int nrx, int ncx,
		    double *y, int nry, int ncy, double *z)
{
    int i, j, k;
    double xij, yjk, sum;

    for (i = 0; i < nrx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * nrx] = NA_REAL;
	    sum = 0.0;
	    for (j = 0; j < ncx; j++) {
		xij = x[i + j * nrx];
		yjk = y[j + k * nry];
#ifndef IEEE_754
		if (ISNAN(xij) || ISNAN(yjk))
		    goto next_ik;
#endif
		sum += xij * yjk;
	    }
	    z[i + k * nrx] = sum;
#ifndef IEEE_754
	next_ik:
	    ;
#endif
	}
}

static void cmatprod(Rcomplex *x, int nrx, int ncx,
		Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    int i, j, k;
    double xij_r, xij_i, yjk_r, yjk_i, sum_i, sum_r;

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
#ifndef IEEE_754
		if (ISNAN(xij_r) || ISNAN(xij_i)
		    || ISNAN(yjk_r) || ISNAN(yjk_i))
		    goto next_ik;
#endif
		sum_r += (xij_r * yjk_r - xij_i * yjk_i);
		sum_i += (xij_r * yjk_i + xij_i * yjk_r);
	    }
	    z[i + k * nrx].r = sum_r;
	    z[i + k * nrx].i = sum_i;
#ifndef IEEE_754
	next_ik:
	    ;
#endif
	}
}

static void crossprod(double *x, int nrx, int ncx,
		      double *y, int nry, int ncy, double *z)
{
    int i, j, k;
    double xji, yjk, sum;

    for (i = 0; i < ncx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * ncx] = NA_REAL;
	    sum = 0.0;
	    for (j = 0; j < nrx; j++) {
		xji = x[j + i * nrx];
		yjk = y[j + k * nry];
#ifndef IEEE_754
		if (ISNAN(xji) || ISNAN(yjk))
		    goto next_ik;
#endif
		sum += xji * yjk;
	    }
	    z[i + k * ncx] = sum;
#ifndef IEEE_754
	next_ik:
	    ;
#endif
	}
}

static void ccrossprod(Rcomplex *x, int nrx, int ncx,
		       Rcomplex *y, int nry, int ncy, Rcomplex *z)
{
    int i, j, k;
    double xji_r, xji_i, yjk_r, yjk_i, sum_r, sum_i;

    for (i = 0; i < ncx; i++)
	for (k = 0; k < ncy; k++) {
	    z[i + k * ncx].r = NA_REAL;
	    z[i + k * ncx].i = NA_REAL;
	    sum_r = 0.0;
	    sum_i = 0.0;
	    for (j = 0; j < nrx; j++) {
		xji_r = x[j + i * nrx].r;
		xji_i = x[j + i * nrx].i;
		yjk_r = y[j + k * nry].r;
		yjk_i = y[j + k * nry].i;
#ifndef IEEE_754
		if (ISNAN(xji_r) || ISNAN(xji_i)
		    || ISNAN(yjk_r) || ISNAN(yjk_i))
		    goto next_ik;
#endif
		sum_r += (xji_r * yjk_r - xji_i * yjk_i);
		sum_i += (xji_r * yjk_i + xji_i * yjk_r);
	    }
	    z[i + k * ncx].r = sum_r;
	    z[i + k * ncx].i = sum_i;
#ifndef IEEE_754
	next_ik:
	    ;
#endif
	}
}

SEXP do_matprod(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int ldx, ldy, nrx, ncx, nry, ncy, mode;
    SEXP x, y, xdims, ydims, ans;

    if (!(isNumeric(CAR(args)) || isComplex(CAR(args))) ||
	!(isNumeric(CADR(args)) || isComplex(CADR(args))))
	errorcall(call, "requires numeric matrix/vector arguments");

    x = CAR(args);
    y = CADR(args);
    xdims = getAttrib(x, R_DimSymbol);
    ydims = getAttrib(y, R_DimSymbol);
    ldx = length(xdims);
    ldy = length(ydims);

    if (ldx != 2 && ldy != 2) {
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
    else if (ldx != 2) {
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
	nrx = 0;
	ncx = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(x) == nry) { /* x as row vector */
		nrx = 1;
		ncx = LENGTH(x);
	    }
	    else if (nry == 1) { /* x as col vector */
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
	else {
	    if (LENGTH(x) == nry) {
		nrx = LENGTH(x);
		ncx = 1;
	    }
	}
    }
    else if (ldy != 2) {
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = 0;
	ncy = 0;
	if (PRIMVAL(op) == 0) {
	    if (LENGTH(y) == ncx) { /* y as col vector */
		nry = LENGTH(y);
		ncy = 1;
	    }
	    else if (ncx == 1){ /* y as row vector */
		nry = 1;
		ncy = LENGTH(y);
	    }
	}
	else {
	    if (LENGTH(y) == nrx) {
		nry = LENGTH(y);
		ncy = 1;
	    }
	}
    }
    else {
	nrx = INTEGER(xdims)[0];
	ncx = INTEGER(xdims)[1];
	nry = INTEGER(ydims)[0];
	ncy = INTEGER(ydims)[1];
    }

    if (PRIMVAL(op) == 0) {
	if (ncx != nry)
	    errorcall(call, "non-conformable arguments");
    }
    else {
	if (nrx != nry)
	    errorcall(call, "non-conformable arguments");
    }

    if (isComplex(CAR(args)) || isComplex(CADR(args)))
	mode = CPLXSXP;
    else
	mode = REALSXP;
    SETCAR(args, coerceVector(CAR(args), mode));
    SETCADR(args, coerceVector(CADR(args), mode));

    if (PRIMVAL(op) == 0) {
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
	    SEXP dimnames, dimnamesnames, dn;
	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		if (ldx == 2 || ncx ==1) {
		    dn = getAttrib(xdims, R_NamesSymbol);
		    SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 0));
		    if(!isNull(dn))
			SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dn, 0));
		}
	    }
	    if (ydims != R_NilValue) {
		if (ldy == 2 ){
		    dn = getAttrib(ydims, R_NamesSymbol);
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));
		    if(!isNull(dn))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dn, 1));
		} else if (nry == 1) {
		    dn = getAttrib(ydims, R_NamesSymbol);
		    SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 0));
		    if(!isNull(dn))
			SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dn, 0));
		}
	    }
	    setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(ans, R_DimNamesSymbol, dimnames);
	    UNPROTECT(2);
	}
    }
    else {
	PROTECT(ans = allocMatrix(mode, ncx, ncy));
	if (mode == CPLXSXP)
	    ccrossprod(COMPLEX(CAR(args)), nrx, ncx,
		       COMPLEX(CADR(args)), nry, ncy, COMPLEX(ans));
	else
	    crossprod(REAL(CAR(args)), nrx, ncx,
		      REAL(CADR(args)), nry, ncy, REAL(ans));
	PROTECT(xdims = getAttrib(CAR(args), R_DimNamesSymbol));
	PROTECT(ydims = getAttrib(CADR(args), R_DimNamesSymbol));
	if (xdims != R_NilValue || ydims != R_NilValue) {
	    SEXP dimnames, dimnamesnames, dnx=R_NilValue, dny=R_NilValue;
	    PROTECT(dimnames = allocVector(VECSXP, 2));
	    PROTECT(dimnamesnames = allocVector(STRSXP, 2));
	    if (xdims != R_NilValue) {
		dnx = getAttrib(xdims, R_NamesSymbol);
		SET_VECTOR_ELT(dimnames, 0, VECTOR_ELT(xdims, 1));
		if(!isNull(dnx))
		    SET_STRING_ELT(dimnamesnames, 0, STRING_ELT(dnx, 1));
	    }
	    if (ydims != R_NilValue) {
		dny = getAttrib(ydims, R_NamesSymbol);
		SET_VECTOR_ELT(dimnames, 1, VECTOR_ELT(ydims, 1));
		if(!isNull(dny))
		    SET_STRING_ELT(dimnamesnames, 1, STRING_ELT(dny, 1));
	    }
	    if (!isNull(dnx) || !isNull(dny))
		setAttrib(dimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(ans, R_DimNamesSymbol, dimnames);
	    UNPROTECT(2);
	}
    }
    UNPROTECT(3);
    return ans;
}

SEXP do_transpose(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, r, dims, dimnames, dimnamesnames=R_NilValue,
	ndimnamesnames, rnames, cnames;
    int i, len = 0, ncol=0, nrow=0;

    checkArity(op, args);
    a = CAR(args);

    if (isVector(a)) {
	dims = getAttrib(a, R_DimSymbol);
	rnames = R_NilValue;
	cnames = R_NilValue;
	switch(length(dims)) {
	case 0:
	    nrow = len = length(a);
	    ncol = 1;
	    rnames = getAttrib(a, R_NamesSymbol);
	    break;
	case 1:
	    nrow = len = length(a);
	    ncol = 1;
	    rnames = getAttrib(a, R_DimNamesSymbol);
	    if (rnames != R_NilValue)
		rnames = VECTOR_ELT(rnames, 0);
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
    switch (TYPEOF(a)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < len; i++)
	    INTEGER(r)[i] = INTEGER(a)[(i / ncol) + (i % ncol) * nrow];
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    REAL(r)[i] = REAL(a)[(i / ncol) + (i % ncol) * nrow];
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    COMPLEX(r)[i] = COMPLEX(a)[(i / ncol) + (i % ncol) * nrow];
	break;
    case STRSXP:
	for (i = 0; i < len; i++)
	    SET_STRING_ELT(r, i,
			   STRING_ELT(a, (i / ncol) + (i % ncol) * nrow));
	break;
    case VECSXP:
	for (i = 0; i < len; i++)
	    SET_VECTOR_ELT(r, i,
			   VECTOR_ELT(a, (i / ncol) + (i % ncol) * nrow));
	break;
    default:
	goto not_matrix;
    }
    PROTECT(dims = allocVector(INTSXP, 2));
    INTEGER(dims)[0] = ncol;
    INTEGER(dims)[1] = nrow;
    setAttrib(r, R_DimSymbol, dims);
    UNPROTECT(1);
    if(rnames != R_NilValue || cnames != R_NilValue) {
	PROTECT(dimnames = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnames, 0, cnames);
	SET_VECTOR_ELT(dimnames, 1, rnames);
	if(!isNull(dimnamesnames)) {
	    PROTECT(ndimnamesnames = allocVector(VECSXP, 2));
	    SET_STRING_ELT(ndimnamesnames, 1, STRING_ELT(dimnamesnames, 0));
	    SET_STRING_ELT(ndimnamesnames, 0, STRING_ELT(dimnamesnames, 1));
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
    errorcall(call, "argument is not a matrix");
    return call;/* never used; just for -Wall */
}


/* swap works by finding for a index i, the position */
/* in the array with dimensions dims1 in terms of */
/* (i, j, k, l, m, ...); i.e. component-wise position, */
/* then permute these to the order of the array with */
/* dimensions dims2 and work backwards to an integer */
/* offset in this array */

static int swap(int ival, SEXP dims1, SEXP dims2, SEXP perm,
		SEXP ind1, SEXP ind2)
{
    int len, t1, i;
    len = length(dims1);
    t1 = ival;
    for (i = 0; i < len; i++) {
	INTEGER(ind1)[i] = t1 % INTEGER(dims1)[i];
	t1 = t1 / INTEGER(dims1)[i];
    }

    for (i = 0; i < len; i++)
	INTEGER(ind2)[i] = INTEGER(ind1)[(INTEGER(perm)[i] - 1)];

    t1 = INTEGER(ind2)[(len - 1)];
    for (i = (len - 2); i >= 0; i--) {
	t1 *= INTEGER(dims2)[i];
	t1 += INTEGER(ind2)[i];
    }
    return t1;
}

SEXP do_aperm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, perm, r, dimsa, dimsr, ind1, ind2;
    int i, j, len;

    checkArity(op, args);

    a = CAR(args);
    PROTECT(dimsa = getAttrib(a, R_DimSymbol));
    if (dimsa == R_NilValue)
	error("aperm: invalid first argument, must be an array");

    PROTECT(perm = coerceVector(CADR(args), INTSXP));
    if (!isVector(perm) || (length(perm) != length(dimsa)))
	error("aperm: invalid second argument, must be a vector of the appropriate length");

    len = length(a);

    PROTECT(dimsr = allocVector(INTSXP, length(dimsa)));
    for (i = 0; i < length(dimsa); i++)
	INTEGER(dimsr)[i] = INTEGER(dimsa)[(INTEGER(perm)[i] - 1)];

    PROTECT(r = allocVector(TYPEOF(a), len));
    PROTECT(ind1 = allocVector(INTSXP, LENGTH(dimsa)));
    PROTECT(ind2 = allocVector(INTSXP, LENGTH(dimsa)));

    switch (TYPEOF(a)) {
    case INTSXP:
    case LGLSXP:
	for (i = 0; i < len; i++) {
	    j = swap(i, dimsa, dimsr, perm, ind1, ind2);
	    INTEGER(r)[j] = INTEGER(a)[i];
	}
	break;
    case REALSXP:
	for (i = 0; i < len; i++) {
	    j = swap(i, dimsa, dimsr, perm, ind1, ind2);
	    REAL(r)[j] = REAL(a)[i];
	}
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++) {
	    j = swap(i, dimsa, dimsr, perm, ind1, ind2);
	    COMPLEX(r)[j] = COMPLEX(a)[i];
	}
	break;
    case STRSXP:
	for (i = 0; i < len; i++) {
	    j = swap(i, dimsa, dimsr, perm, ind1, ind2);
	    SET_STRING_ELT(r, j, STRING_ELT(a, i));
	}
	break;
    case VECSXP:
	for (i = 0; i < len; i++) {
	    j = swap(i, dimsa, dimsr, perm, ind1, ind2);
	    SET_VECTOR_ELT(r, j, VECTOR_ELT(a, i));
	}
	break;
    default:
	errorcall(call, R_MSG_IA);
    }

    if (INTEGER(CAR(CDDR(args)))[0])
	setAttrib(r, R_DimSymbol, dimsr);
    else
	setAttrib(r, R_DimSymbol, dimsa);
    UNPROTECT(6);
    return r;
}
