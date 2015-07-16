/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001--2013  The R Core Team.
 *  Copyright (C) 2003--2010  The R Foundation
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

/* Interface routines, callable from R using .Internal, for Lapack code */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#include <ctype.h> /* for toupper */

#include "Lapack.h"

/* NB: the handling of dims is odd here.  Most are coerced to be
 * integers (which dimgets currently guarantees), but a couple were
 * used unchecked. 
 */

/* FIXME: MM would want to make these available to packages;
 * BUT:  1) cannot be in libRlapack {since that may be external}
 *       2) Pkgs cannot get it from the C-Lapack interface code {lapack.so}
 *          since that is R-internal
 */
static char La_norm_type(const char *typstr)
{
    char typup;

    if (strlen(typstr) != 1)
	error(
	    _("argument type[1]='%s' must be a character string of string length 1"),
	    typstr);
    typup = (char) toupper(*typstr);
    if (typup == '1')
	typup = 'O'; /* aliases */
    else if (typup == 'E')
	typup = 'F';
    else if (typup != 'M' && typup != 'O' && typup != 'I' && typup != 'F')
	error(_("argument type[1]='%s' must be one of 'M','1','O','I','F' or 'E'"),
	      typstr);
    return typup;
}

/* Lapack condition number approximation: currently only supports _1 or _Inf norm : */
static char La_rcond_type(const char *typstr)
{
    char typup;

    if (strlen(typstr) != 1)
	error(_("argument type[1]='%s' must be a character string of string length 1"),
	      typstr);
    typup = (char) toupper(*typstr);
    if (typup == '1')
	typup = 'O'; /* alias */
    else if (typup != 'O' && typup != 'I')
	error(_("argument type[1]='%s' must be one of '1','O', or 'I'"),
	      typstr);
    return typup; /* 'O' or 'I' */
}

/* La.svd, called from svd */
static SEXP La_svd(SEXP jobu, SEXP x, SEXP s, SEXP u, SEXP vt)
{
    int n, p, info = 0;

    if (!isString(jobu))
	error("'jobu' must be a character string");
    int *xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0]; p = xdims[1];

    /* work on a copy of x  */
    double *xvals;
    if (!isReal(x)) {
       x = coerceVector(x, REALSXP);
       xvals = REAL(x);
    } else {
	xvals = (double *) R_alloc(n * (size_t) p, sizeof(double));
	Memcpy(xvals, REAL(x), n * (size_t) p);
    }
    PROTECT(x);

    SEXP dims = getAttrib(u, R_DimSymbol);
    if (TYPEOF(dims) != INTSXP) error("non-integer dims");
    int ldu = INTEGER(dims)[0];
    dims = getAttrib(vt, R_DimSymbol);
    if (TYPEOF(dims) != INTSXP) error("non-integer dims");
    int ldvt = INTEGER(dims)[0];
    double tmp;
    /* min(n,p) large is implausible, but cast to be sure */
    int *iwork= (int *) R_alloc(8*(size_t)(n < p ? n : p), sizeof(int));

    /* ask for optimal size of work array */
    const char *ju = CHAR(STRING_ELT(jobu, 0));
    int lwork = -1;
    F77_CALL(dgesdd)(ju, &n, &p, xvals, &n, REAL(s),
		     REAL(u), &ldu, REAL(vt), &ldvt,
		     &tmp, &lwork, iwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgesdd");
    lwork = (int) tmp;
    double *work = (double *) R_alloc(lwork, sizeof(double));
    F77_CALL(dgesdd)(ju, &n, &p, xvals, &n, REAL(s),
		     REAL(u), &ldu, REAL(vt), &ldvt,
		     work, &lwork, iwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgesdd");

    SEXP val = PROTECT(allocVector(VECSXP, 3));
    SEXP nm = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(nm, 0, mkChar("d"));
    SET_STRING_ELT(nm, 1, mkChar("u"));
    SET_STRING_ELT(nm, 2, mkChar("vt"));
    setAttrib(val, R_NamesSymbol, nm);
    SET_VECTOR_ELT(val, 0, s);
    SET_VECTOR_ELT(val, 1, u);
    SET_VECTOR_ELT(val, 2, vt);
    UNPROTECT(3);
    return val;
}

/* Real, symmetric case of eigen */
static SEXP La_rs(SEXP x, SEXP only_values)
{
    int *xdims, n, lwork, info = 0, ov;
    char jobv[2] = "U", uplo[2] = "L", range[2] = "A";
    SEXP z = R_NilValue;
    double *work, *rx, *rvalues, tmp, *rz = NULL;
    int liwork, *iwork, itmp, m;
    double vl = 0.0, vu = 0.0, abstol = 0.0;
    /* valgrind seems to think vu should be set, but it is documented
       not to be used if range='a' */
    int il, iu, *isuppz;

    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1]) error(_("'x' must be a square numeric matrix"));
    ov = asLogical(only_values);
    if (ov == NA_LOGICAL) error(_("invalid '%s' argument"), "only.values");
    if (ov) jobv[0] = 'N'; else jobv[0] = 'V';

    /* work on a copy of x, since LAPACK trashes it */
    if (!isReal(x)) {
	x = coerceVector(x, REALSXP);
	rx = REAL(x);
    } else {
	rx = (double *) R_alloc(n * (size_t) n, sizeof(double));
	Memcpy(rx, REAL(x), (size_t) n * n);
    }
    PROTECT(x);
    SEXP values = PROTECT(allocVector(REALSXP, n));
    rvalues = REAL(values);

    if (!ov) {
	z = PROTECT(allocMatrix(REALSXP, n, n));
	rz = REAL(z);
    }
    isuppz = (int *) R_alloc(2*(size_t)n, sizeof(int));
    /* ask for optimal size of work arrays */
    lwork = -1; liwork = -1;
    F77_CALL(dsyevr)(jobv, range, uplo, &n, rx, &n,
		     &vl, &vu, &il, &iu, &abstol, &m, rvalues,
		     rz, &n, isuppz,
		     &tmp, &lwork, &itmp, &liwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dsyevr");
    lwork = (int) tmp;
    liwork = itmp;

    work = (double *) R_alloc(lwork, sizeof(double));
    iwork = (int *) R_alloc(liwork, sizeof(int));
    F77_CALL(dsyevr)(jobv, range, uplo, &n, rx, &n,
		     &vl, &vu, &il, &iu, &abstol, &m, rvalues,
		     rz, &n, isuppz,
		     work, &lwork, iwork, &liwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dsyevr");

    SEXP ret, nm;
    if (!ov) {
	ret = PROTECT(allocVector(VECSXP, 2));
	nm = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 1, mkChar("vectors"));
	SET_VECTOR_ELT(ret, 1, z);
	UNPROTECT_PTR(z);
    } else {
	ret = PROTECT(allocVector(VECSXP, 1));
	nm = PROTECT(allocVector(STRSXP, 1));
    }
    SET_STRING_ELT(nm, 0, mkChar("values"));
    setAttrib(ret, R_NamesSymbol, nm);
    SET_VECTOR_ELT(ret, 0, values);
    UNPROTECT(4);
    return ret;
}

static SEXP unscramble(const double* imaginary, int n,
		       const double* vecs)
{
    int i, j;
    SEXP s = allocMatrix(CPLXSXP, n, n);
    size_t N = n;
    for (j = 0; j < n; j++) {
	if (imaginary[j] != 0) {
	    int j1 = j + 1;
	    for (i = 0; i < n; i++) {
		COMPLEX(s)[i+N*j].r = COMPLEX(s)[i+N*j1].r = vecs[i + j * N];
		COMPLEX(s)[i+N*j1].i = -(COMPLEX(s)[i+N*j].i = vecs[i + j1 * N]);
	    }
	    j = j1;
	} else {
	    for (i = 0; i < n; i++) {
		COMPLEX(s)[i+N*j].r = vecs[i + j * N];
		COMPLEX(s)[i+N*j].i = 0.0;
	    }
	}
    }
    return s;
}

/* Real, general case of eigen */
static SEXP La_rg(SEXP x, SEXP only_values)
{
    Rboolean vectors, complexValues;
    int i, n, lwork, info, *xdims, ov;
    double *work, *wR, *wI, *left, *right, *xvals, tmp;
    char jobVL[2] = "N", jobVR[2] = "N";

    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1])
	error(_("'x' must be a square numeric matrix"));

    /* work on a copy of x */
    if (!isReal(x)) {
	x = coerceVector(x, REALSXP);
	xvals = REAL(x);
    } else {
	xvals = (double *) R_alloc(n * (size_t)n, sizeof(double));
	Memcpy(xvals, REAL(x), (size_t) n * n);
    }
    PROTECT(x);

    ov = asLogical(only_values);
    if (ov == NA_LOGICAL) error(_("invalid '%s' argument"), "only.values");
    vectors = !ov;
    left = right = (double *) 0;
    if (vectors) {
	jobVR[0] = 'V';
	right = (double *) R_alloc(n * (size_t)n, sizeof(double));
    }
    wR = (double *) R_alloc(n, sizeof(double));
    wI = (double *) R_alloc(n, sizeof(double));
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(dgeev)(jobVL, jobVR, &n, xvals, &n, wR, wI,
		    left, &n, right, &n, &tmp, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgeev");
    lwork = (int) tmp;
    work = (double *) R_alloc(lwork, sizeof(double));
    F77_CALL(dgeev)(jobVL, jobVR, &n, xvals, &n, wR, wI,
		    left, &n, right, &n, work, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgeev");

    complexValues = FALSE;
    for (i = 0; i < n; i++)
	/* This test used to be !=0 for R < 2.3.0.  This is OK for 0+0i */
	if (fabs(wI[i]) >  10 * R_AccuracyInfo.eps * fabs(wR[i])) {
	    complexValues = TRUE;
	    break;
	}
    SEXP ret = PROTECT(allocVector(VECSXP, 2));
    SEXP nm = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("values"));
    SET_STRING_ELT(nm, 1, mkChar("vectors"));
    setAttrib(ret, R_NamesSymbol, nm);
    SET_VECTOR_ELT(ret, 1, R_NilValue);
    if (complexValues) {
	SEXP val = allocVector(CPLXSXP, n);
	for (i = 0; i < n; i++) {
	    COMPLEX(val)[i].r = wR[i];
	    COMPLEX(val)[i].i = wI[i];
	}
	SET_VECTOR_ELT(ret, 0, val);
	if (vectors) SET_VECTOR_ELT(ret, 1, unscramble(wI, n, right));
    } else {
	SEXP val = allocVector(REALSXP, n);
	for (i = 0; i < n; i++) REAL(val)[i] = wR[i];
	SET_VECTOR_ELT(ret, 0, val);
	if(vectors) {
	    val = allocMatrix(REALSXP, n, n);
	    for (i = 0; i < (n * n); i++) REAL(val)[i] = right[i];
	    SET_VECTOR_ELT(ret, 1, val);
	}
    }
    UNPROTECT(3);
    return ret;
}

/* norm() except for 2-norm */
static SEXP La_dlange(SEXP A, SEXP type)
{
    int *xdims, m, n, nprot = 1;
    double *work = NULL;
    char typNorm[] = {'\0', '\0'};

    if (!isMatrix(A)) error(_("'A' must be a numeric matrix"));
    if (!isString(type))
	error(_("'type' must be a character string"));
    if (!isReal(A)) {
	A = PROTECT(coerceVector(A, REALSXP)); nprot++;
    }

    xdims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    m = xdims[0];
    n = xdims[1]; /* m x n  matrix {using Lapack naming convention} */

    typNorm[0] = La_norm_type(CHAR(asChar(type)));

    SEXP val = PROTECT(allocVector(REALSXP, 1));
    if(*typNorm == 'I') work = (double *) R_alloc(m, sizeof(double));
    REAL(val)[0] = F77_CALL(dlange)(typNorm, &m, &n, REAL(A), &m, work);

    UNPROTECT(nprot);
    return val;
}


/* ------------------------------------------------------------ */
/* Real case of rcond, for general matrices */
static SEXP La_dgecon(SEXP A, SEXP norm)
{
    int *xdims, m, n, info, *iwork;
    double anorm, *work;
    char typNorm[] = {'\0', '\0'};

    if (!isMatrix(A)) error(_("'A' must be a numeric matrix"));
    if (!isString(norm))
	error(_("'norm' must be a character string"));
    A = PROTECT(isReal(A) ? duplicate(A) : coerceVector(A, REALSXP));

    xdims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    m = xdims[0]; n = xdims[1];

    typNorm[0] = La_rcond_type(CHAR(asChar(norm)));

    SEXP val = PROTECT(allocVector(REALSXP, 1));
    work = (double *) R_alloc((*typNorm == 'I' && m > 4*(size_t)n) ? m : 4*(size_t)n,
			      sizeof(double));
    iwork = (int *) R_alloc(m, sizeof(int));

    anorm = F77_CALL(dlange)(typNorm, &m, &n, REAL(A), &m, work);

    /* Compute the LU-decomposition and overwrite 'A' with result :*/
    F77_CALL(dgetrf)(&m, &n, REAL(A), &m, iwork, &info);
    if (info) {
	if (info < 0) {
	    UNPROTECT(2);
	    error(_("error [%d] from Lapack 'dgetrf()'"), info);
	}
	else { /* i := info > 0:  LU decomp. is completed, but  U[i,i] = 0
		* <==> singularity */
#if 0
	    warning(_("exact singularity: U[%d,%d] = 0 in LU-decomposition {Lapack 'dgetrf()'}"),
		    info,info);
#endif
	    REAL(val)[0] = 0.; /* rcond = 0 <==> singularity */
	    UNPROTECT(2);
	    return val;
	}
    }
    F77_CALL(dgecon)(typNorm, &n, REAL(A), &n, &anorm,
		     /* rcond = */ REAL(val),
		     work, iwork, &info);
    UNPROTECT(2);
    if (info) error(_("error [%d] from Lapack 'dgecon()'"), info);
    return val;
}

/* Real case of kappa.tri (and also rcond for a triangular matrix) */
static SEXP La_dtrcon(SEXP A, SEXP norm)
{
    int *xdims, n, nprot = 0, info;
    char typNorm[] = {'\0', '\0'};

    if (!isMatrix(A)) error(_("'A' must be a numeric matrix"));
    if (!isString(norm)) error(_("'norm' must be a character string"));
    if (!isReal(A)) {
	nprot++;
	A = PROTECT(coerceVector(A, REALSXP));
    }
    xdims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    n = xdims[0];
    if(n != xdims[1]) {
	UNPROTECT(nprot);
	error(_("'A' must be a *square* matrix"));
    }

    typNorm[0] = La_rcond_type(CHAR(asChar(norm)));

    nprot++;
    SEXP val = PROTECT(allocVector(REALSXP, 1));

    F77_CALL(dtrcon)(typNorm, "U", "N", &n, REAL(A), &n,
		     REAL(val),
		     /* work : */ (double *) R_alloc(3*(size_t)n, sizeof(double)),
		     /* iwork: */ (int *)    R_alloc(n, sizeof(int)),
		     &info);
    UNPROTECT(nprot);
    if (info) error(_("error [%d] from Lapack 'dtrcon()'"), info);
    return val;
}

/* Complex case of rcond, for general matrices */
static SEXP La_zgecon(SEXP A, SEXP norm)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    Rcomplex *avals; /* copy of A, to be modified */
    int *dims, n, info;
    double anorm, *rwork;
    char typNorm[] = {'\0', '\0'};

    if (!isString(norm)) error(_("'norm' must be a character string"));
    if (!(isMatrix(A) && isComplex(A)))
	error(_("'A' must be a complex matrix"));
    dims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    n = dims[0];
    if(n != dims[1]) error(_("'A' must be a *square* matrix"));

    typNorm[0] = La_rcond_type(CHAR(asChar(norm)));

    SEXP val = PROTECT(allocVector(REALSXP, 1));

    rwork = (double *) R_alloc(2*(size_t)n, sizeof(Rcomplex));
    anorm = F77_CALL(zlange)(typNorm, &n, &n, COMPLEX(A), &n, rwork);

    /* Compute the LU-decomposition and overwrite 'x' with result;
     * working on a copy of A : */
    avals = (Rcomplex *) R_alloc((size_t)n * n, sizeof(Rcomplex));
    Memcpy(avals, COMPLEX(A), (size_t) n * n);
    F77_CALL(zgetrf)(&n, &n, avals, &n,
		     /* iwork: */(int *) R_alloc(n, sizeof(int)),
		     &info);
    if (info) {
	if (info < 0) {
	    UNPROTECT(1);
	    error(_("error [%d] from Lapack 'zgetrf()'"), info);
	} else {
	    REAL(val)[0] = 0.; /* rcond = 0 <==> singularity */
	    UNPROTECT(1);
	    return val;
	}
	UNPROTECT(1);
	error(_("error [%d] from Lapack 'zgetrf()'"), info);
    }

    F77_CALL(zgecon)(typNorm, &n, avals, &n, &anorm,
		     /* rcond = */ REAL(val),
		     /* work : */ (Rcomplex *) R_alloc(4*(size_t)n, sizeof(Rcomplex)),
		     rwork, &info);
    UNPROTECT(1);
    if (info) error(_("error [%d] from Lapack 'zgecon()'"), info);
    return val;

#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex case of kappa.tri (and also rcond for a triangular matrix) */
static SEXP La_ztrcon(SEXP A, SEXP norm)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    SEXP val;
    int *dims, n, info;
    char typNorm[] = {'\0', '\0'};

    if (!isString(norm))
	error(_("'norm' must be a character string"));
    if (!(isMatrix(A) && isComplex(A)))
	error(_("'A' must be a complex matrix"));
    dims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    n = dims[0];
    if(n != dims[1])
	error(_("'A' must be a *square* matrix"));

    typNorm[0] = La_rcond_type(CHAR(asChar(norm)));

    val = PROTECT(allocVector(REALSXP, 1));

    F77_CALL(ztrcon)(typNorm, "U", "N", &n, COMPLEX(A), &n,
		     REAL(val),
		     /* work : */ (Rcomplex *) R_alloc(2*(size_t)n, sizeof(Rcomplex)),
		     /* rwork: */ (double *)   R_alloc(n, sizeof(double)),
		     &info);
    UNPROTECT(1);
    if (info) error(_("error [%d] from Lapack 'ztrcon()'"), info);
    return val;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex case of solve.default: see the comments in La_solve */
static SEXP La_solve_cmplx(SEXP A, SEXP Bin)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int n, p, info, *ipiv, *Adims, *Bdims;
    Rcomplex *avals;
    SEXP B, Adn, Bdn;

    if (!isMatrix(A)) error(_("'a' must be a complex matrix"));
    Adims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    n = Adims[0];
    if(n == 0) error(_("'a' is 0-diml"));
    int n2 = Adims[1];
    if(n2 != n) error(_("'a' (%d x %d) must be square"), n, n2);
    Adn = getAttrib(A, R_DimNamesSymbol);

    if (isMatrix(Bin)) {
	Bdims = INTEGER(coerceVector(getAttrib(Bin, R_DimSymbol), INTSXP));
	p = Bdims[1];
	if(p == 0) error(_("no right-hand side in 'b'"));
	int p2 = Bdims[0];
	if(p2 != n)
	    error(_("'b' (%d x %d) must be compatible with 'a' (%d x %d)"),
		  p2, p, n, n);
	PROTECT(B = allocMatrix(CPLXSXP, n, p));
	SEXP Bindn =  getAttrib(Bin, R_DimNamesSymbol);
	if (!isNull(Adn) || !isNull(Bindn)) {
	    Bdn = allocVector(VECSXP, 2);
	    if (!isNull(Adn)) SET_VECTOR_ELT(Bdn, 0, VECTOR_ELT(Adn, 1));
	    if (!isNull(Bindn)) SET_VECTOR_ELT(Bdn, 1, VECTOR_ELT(Bindn, 1));
	    if (!isNull(VECTOR_ELT(Bdn, 0)) && !isNull(VECTOR_ELT(Bdn, 1)))
		setAttrib(B, R_DimNamesSymbol, Bdn);
	}
    } else {
	p = 1;
	if(length(Bin) != n)
	    error(_("'b' (%d x %d) must be compatible with 'a' (%d x %d)"),
		  length(Bin), p, n, n);	
	PROTECT(B = allocVector(CPLXSXP, n));
	if (!isNull(Adn)) setAttrib(B, R_NamesSymbol, VECTOR_ELT(Adn, 1));
    }
    Bin = PROTECT(coerceVector(Bin, CPLXSXP));
    Memcpy(COMPLEX(B), COMPLEX(Bin), (size_t)n * p);

    ipiv = (int *) R_alloc(n, sizeof(int));

    /* work on a copy of A */
    if(TYPEOF(A) != CPLXSXP) {
	A = coerceVector(A, CPLXSXP);
	avals = COMPLEX(A);
    } else {
	avals = (Rcomplex *) R_alloc((size_t)n * n, sizeof(Rcomplex));
	Memcpy(avals, COMPLEX(A), (size_t) n * n);
    }
    PROTECT(A);
    F77_CALL(zgesv)(&n, &p, avals, &n, ipiv, COMPLEX(B), &n, &info);
    if (info < 0)
	error(_("argument %d of Lapack routine %s had invalid value"),
	      -info, "zgesv");
    if (info > 0)
	error(("Lapack routine zgesv: system is exactly singular"));
    UNPROTECT(3);  /* B, Bin, A */
    return B;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex case of qr.default */
static SEXP La_qr_cmplx(SEXP Ain)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int i, m, n, *Adims, info, lwork;
    Rcomplex *work, tmp;
    double *rwork;

    if (!(isMatrix(Ain) && isComplex(Ain)))
	error(_("'a' must be a complex matrix"));
    SEXP Adn = getAttrib(Ain, R_DimNamesSymbol);
    Adims = INTEGER(coerceVector(getAttrib(Ain, R_DimSymbol), INTSXP));
    m = Adims[0]; n = Adims[1];
    SEXP A = PROTECT(allocMatrix(CPLXSXP, m, n));
    Memcpy(COMPLEX(A), COMPLEX(Ain), (size_t)m * n);
    rwork = (double *) R_alloc(2*(size_t)n, sizeof(double));

    SEXP jpvt = PROTECT(allocVector(INTSXP, n));
    for (i = 0; i < n; i++) INTEGER(jpvt)[i] = 0;
    SEXP tau = PROTECT(allocVector(CPLXSXP, m < n ? m : n));
    lwork = -1;
    F77_CALL(zgeqp3)(&m, &n, COMPLEX(A), &m, INTEGER(jpvt), COMPLEX(tau),
		     &tmp, &lwork, rwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgeqp3");
    lwork = (int) tmp.r;
    work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zgeqp3)(&m, &n, COMPLEX(A), &m, INTEGER(jpvt), COMPLEX(tau),
		     work, &lwork, rwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgeqp3");
    SEXP val = PROTECT(allocVector(VECSXP, 4));
    SEXP nm = PROTECT(allocVector(STRSXP, 4));
    SET_STRING_ELT(nm, 0, mkChar("qr"));
    SET_STRING_ELT(nm, 1, mkChar("rank"));
    SET_STRING_ELT(nm, 2, mkChar("qraux"));
    SET_STRING_ELT(nm, 3, mkChar("pivot"));
    setAttrib(val, R_NamesSymbol, nm);
    // Fix up dimnames(A)
    if(!isNull(Adn)) {
	SEXP Adn2 = duplicate(Adn);
	SEXP cn = VECTOR_ELT(Adn, 1), cn2 = VECTOR_ELT(Adn2, 1);
	if(!isNull(cn)) { // pivot them
	    for (int j = 0; j < n; j++)
		SET_STRING_ELT(cn2, j, STRING_ELT(cn, INTEGER(jpvt)[j]-1));
	}
	setAttrib(A, R_DimNamesSymbol, Adn2);
    }
    SET_VECTOR_ELT(val, 0, A);
    SET_VECTOR_ELT(val, 1, ScalarInteger(m < n ? m : n));
    SET_VECTOR_ELT(val, 2, tau);
    SET_VECTOR_ELT(val, 3, jpvt);
    UNPROTECT(5);
    return val;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex case of qr_coef */
static SEXP qr_coef_cmplx(SEXP Q, SEXP Bin)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int n, nrhs, lwork, info, k, *Bdims;
    SEXP B, qr = VECTOR_ELT(Q, 0), tau = VECTOR_ELT(Q, 2);
    Rcomplex *work, tmp;

    k = LENGTH(tau);
    if (!isMatrix(Bin)) error(_("'b' must be a complex matrix"));

    if (!isComplex(Bin)) B = PROTECT(coerceVector(Bin, CPLXSXP));
    else B = PROTECT(duplicate(Bin));

    n = INTEGER(coerceVector(getAttrib(qr, R_DimSymbol), INTSXP))[0];
    Bdims = INTEGER(coerceVector(getAttrib(Bin, R_DimSymbol), INTSXP));
    if(Bdims[0] != n)
	error(_("right-hand side should have %d not %d rows"), n, Bdims[0]);
    nrhs = Bdims[1];
    lwork = -1;
    F77_CALL(zunmqr)("L", "C", &n, &nrhs, &k,
		     COMPLEX(qr), &n, COMPLEX(tau), COMPLEX(B), &n,
		     &tmp, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zunmqr");
    lwork = (int) tmp.r;
    work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zunmqr)("L", "C", &n, &nrhs, &k,
		     COMPLEX(qr), &n, COMPLEX(tau), COMPLEX(B), &n,
		     work, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zunmqr");
    F77_CALL(ztrtrs)("U", "N", "N", &k, &nrhs,
		     COMPLEX(qr), &n, COMPLEX(B), &n, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "ztrtrs");
    UNPROTECT(1);
    return B;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex case of qr.qy and qr.qty */
static SEXP qr_qy_cmplx(SEXP Q, SEXP Bin, SEXP trans)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int n, nrhs, lwork, info, k, *Bdims, tr;
    SEXP B, qr = VECTOR_ELT(Q, 0), tau = VECTOR_ELT(Q, 2);
    Rcomplex *work, tmp;

    k = LENGTH(tau);
    if (!(isMatrix(Bin) && isComplex(Bin)))
	error(_("'b' must be a complex matrix"));
    tr = asLogical(trans);
    if(tr == NA_LOGICAL) error(_("invalid '%s' argument"), "trans");

    if (!isReal(Bin)) B = PROTECT(coerceVector(Bin, CPLXSXP));
    else B = PROTECT(duplicate(Bin));
    n = INTEGER(coerceVector(getAttrib(qr, R_DimSymbol), INTSXP))[0];
    Bdims = INTEGER(coerceVector(getAttrib(B, R_DimSymbol), INTSXP));
    if(Bdims[0] != n)
	error(_("right-hand side should have %d not %d rows"), n, Bdims[0]);
    nrhs = Bdims[1];
    lwork = -1;
    F77_CALL(zunmqr)("L", tr ? "C" : "N", &n, &nrhs, &k,
		     COMPLEX(qr), &n, COMPLEX(tau), COMPLEX(B), &n,
		     &tmp, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zunmqr");
    lwork = (int) tmp.r;
    work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zunmqr)("L", tr ? "C" : "N", &n, &nrhs, &k,
		     COMPLEX(qr), &n, COMPLEX(tau), COMPLEX(B), &n,
		     work, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zunmqr");
    UNPROTECT(1);
    return B;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

static SEXP La_svd_cmplx(SEXP jobu, SEXP x, SEXP s, SEXP u, SEXP v)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    if (!(isString(jobu)))
	error(_("'jobu' must be a character string"));
    int *xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    int n = xdims[0], p = xdims[1];
    const char *jz = CHAR(STRING_ELT(jobu, 0));

    /* The underlying LAPACK, specifically ZLARF, does not work with
     * long arrays */
    if ((double)n * (double)p > INT_MAX)
	error(_("matrices of 2^31 or more elements are not supported"));

    /* work on a copy of x */
    Rcomplex *xvals = (Rcomplex *) R_alloc(n * (size_t) p, sizeof(Rcomplex));
    Memcpy(xvals, COMPLEX(x), n * (size_t) p);

    int *iwork= (int *) R_alloc(8*(size_t)(n < p ? n : p), sizeof(int));
    size_t mn0 = (n < p ? n : p), mn1 = (n > p ? n : p), lrwork;
    if (strcmp(jz, "N")) {
	size_t f1 = 5 * mn1 + 7, f2 = 2 * mn1 + 2 * mn0 + 1;
	lrwork = (f1 > f2 ? f1 : f2) * mn0;
	// 7 replaces 5: bug 111 in http://www.netlib.org/lapack/Errata/index2.html
    } else lrwork = 7 * mn0;
    double *rwork  = (double *) R_alloc(lrwork, sizeof(double));
    /* ask for optimal size of lwork array */
    int lwork = -1, info;
    Rcomplex tmp;
    int ldu, ldv;
    SEXP dims = getAttrib(u, R_DimSymbol);
    if (TYPEOF(dims) != INTSXP) error("non-integer dims");
    ldu = INTEGER(dims)[0];
    dims = getAttrib(v, R_DimSymbol);
    if (TYPEOF(dims) != INTSXP) error("non-integer dims");
    ldv = INTEGER(dims)[0];
    F77_CALL(zgesdd)(jz, &n, &p, xvals, &n, REAL(s),
		     COMPLEX(u), &ldu, COMPLEX(v), &ldv,
		     &tmp, &lwork, rwork, iwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgesdd");
    lwork = (int) tmp.r;
    Rcomplex *work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zgesdd)(jz, &n, &p, xvals, &n, REAL(s),
		     COMPLEX(u), &ldu, COMPLEX(v), &ldv,
		     work, &lwork, rwork, iwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgesdd");

    SEXP val = PROTECT(allocVector(VECSXP, 3));
    SEXP nm = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(nm, 0, mkChar("d"));
    SET_STRING_ELT(nm, 1, mkChar("u"));
    SET_STRING_ELT(nm, 2, mkChar("vt"));
    setAttrib(val, R_NamesSymbol, nm);
    SET_VECTOR_ELT(val, 0, s);
    SET_VECTOR_ELT(val, 1, u);
    SET_VECTOR_ELT(val, 2, v);
    UNPROTECT(2);
    return val;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex, symmetric case of eigen */
static SEXP La_rs_cmplx(SEXP xin, SEXP only_values)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int *xdims, n, lwork, info, ov;
    char jobv[2] = "N", uplo[2] = "L";
    Rcomplex *work, *rx, tmp;
    double *rwork, *rvalues;

    xdims = INTEGER(coerceVector(getAttrib(xin, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1]) error(_("'x' must be a square complex matrix"));
    ov = asLogical(only_values);
    if (ov == NA_LOGICAL) error(_("invalid '%s' argument"), "only.values");
    if (ov) jobv[0] = 'N'; else jobv[0] = 'V';

    SEXP x = PROTECT(allocMatrix(CPLXSXP, n, n));
    rx = COMPLEX(x);
    Memcpy(rx, COMPLEX(xin), (size_t) n * n);
    SEXP values = PROTECT(allocVector(REALSXP, n));
    rvalues = REAL(values);

    rwork = (double *) R_alloc((3*(size_t)n-2) > 1 ? 3*(size_t)n-2 : 1, 
			       sizeof(double));
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(zheev)(jobv, uplo, &n, rx, &n, rvalues, &tmp, &lwork, rwork,
		    &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zheev");
    lwork = (int) tmp.r;
    work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zheev)(jobv, uplo, &n, rx, &n, rvalues, work, &lwork, rwork,
		    &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zheev");
    SEXP ret, nm;
    if (!ov) {
	ret = PROTECT(allocVector(VECSXP, 2));
	nm = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 1, mkChar("vectors"));
	SET_VECTOR_ELT(ret, 1, x);
    } else {
	ret = PROTECT(allocVector(VECSXP, 1));
	nm = PROTECT(allocVector(STRSXP, 1));
    }
    SET_STRING_ELT(nm, 0, mkChar("values"));
    setAttrib(ret, R_NamesSymbol, nm);
    SET_VECTOR_ELT(ret, 0, values);
    UNPROTECT(4);
    return ret;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* Complex, general case of eigen */
static SEXP La_rg_cmplx(SEXP x, SEXP only_values)
{
#ifdef HAVE_FORTRAN_DOUBLE_COMPLEX
    int  n, lwork, info, *xdims, ov;
    Rcomplex *work, *left, *right, *xvals, tmp;
    double *rwork;
    char jobVL[2] = "N", jobVR[2] = "N";
    SEXP ret, nm, values, val = R_NilValue;

    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1]) error(_("'x' must be a square numeric matrix"));

    /* work on a copy of x */
    xvals = (Rcomplex *) R_alloc((size_t)n * n, sizeof(Rcomplex));
    Memcpy(xvals, COMPLEX(x), (size_t) n * n);
    ov = asLogical(only_values);
    if (ov == NA_LOGICAL) error(_("invalid '%s' argument"), "only.values");
    left = right = (Rcomplex *) 0;
    if (!ov) {
	jobVR[0] = 'V';
	PROTECT(val = allocMatrix(CPLXSXP, n, n));
	right = COMPLEX(val);
    }
    PROTECT(values = allocVector(CPLXSXP, n));
    rwork = (double *) R_alloc(2*(size_t)n, sizeof(double));
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(zgeev)(jobVL, jobVR, &n, xvals, &n, COMPLEX(values),
		    left, &n, right, &n, &tmp, &lwork, rwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgeev");
    lwork = (int) tmp.r;
    work = (Rcomplex *) R_alloc(lwork, sizeof(Rcomplex));
    F77_CALL(zgeev)(jobVL, jobVR, &n, xvals, &n, COMPLEX(values),
		    left, &n, right, &n, work, &lwork, rwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "zgeev");

    if(!ov){
	ret = PROTECT(allocVector(VECSXP, 2));
	nm = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 1, mkChar("vectors"));
	SET_VECTOR_ELT(ret, 1, val);
    } else {
	ret = PROTECT(allocVector(VECSXP, 1));
	nm = PROTECT(allocVector(STRSXP, 1));
    }
    SET_STRING_ELT(nm, 0, mkChar("values"));
    SET_VECTOR_ELT(ret, 0, values);
    setAttrib(ret, R_NamesSymbol, nm);
    UNPROTECT(ov ? 3 : 4);
    return ret;
#else
    error(_("Fortran complex functions are not available on this platform"));
    return R_NilValue; /* -Wall */
#endif
}

/* ------------------------------------------------------------ */

static SEXP La_chol(SEXP A, SEXP pivot, SEXP stol)
{
    if (!isMatrix(A)) error(_("'a' must be a numeric matrix"));

    SEXP ans = PROTECT(isReal(A) ? duplicate(A): coerceVector(A, REALSXP));
    SEXP adims = getAttrib(A, R_DimSymbol);
    if (TYPEOF(adims) != INTSXP) error("non-integer dims");
    int m = INTEGER(adims)[0], n = INTEGER(adims)[1];

    if (m != n) error(_("'a' must be a square matrix"));
    if (m <= 0) error(_("'a' must have dims > 0"));
    size_t N = n;
    for (int j = 0; j < n; j++) 	/* zero the lower triangle */
	for (int i = j+1; i < n; i++) REAL(ans)[i + N * j] = 0.;

    int piv = asInteger(pivot);
    if (piv != 0 && piv != 1) error("invalid '%s' value", "pivot");
    if(!piv) {
	int info;
	F77_CALL(dpotrf)("Upper", &m, REAL(ans), &m, &info);
	if (info != 0) {
	    if (info > 0)
		error(_("the leading minor of order %d is not positive definite"),
		      info);
	    error(_("argument %d of Lapack routine %s had invalid value"),
		  -info, "dpotrf");
	}
    } else {
	double tol = asReal(stol);
	SEXP piv = PROTECT(allocVector(INTSXP, m));
	int *ip = INTEGER(piv);
	double *work = (double *) R_alloc(2 * (size_t)m, sizeof(double));
	int rank, info;
	F77_CALL(dpstrf)("U", &m, REAL(ans), &m, ip, &rank, &tol, work, &info);
	if (info != 0) {
	    if (info > 0)
		warning(_("the matrix is either rank-deficient or indefinite"));
	    else
		error(_("argument %d of Lapack routine %s had invalid value"),
		      -info, "dpstrf");
	}
	setAttrib(ans, install("pivot"), piv);
	setAttrib(ans, install("rank"), ScalarInteger(rank));
	SEXP cn, dn = getAttrib(ans, R_DimNamesSymbol);
	if (!isNull(dn) && !isNull(cn = VECTOR_ELT(dn, 1))) {
	    // need to pivot the colnames
	    SEXP dn2 = PROTECT(duplicate(dn));
	    SEXP cn2 = VECTOR_ELT(dn2, 1);
	    for(int i = 0; i < m; i++) 
		SET_STRING_ELT(cn2, i, STRING_ELT(cn, ip[i] - 1)); // base 1
	    setAttrib(ans, R_DimNamesSymbol, dn2);
	    UNPROTECT(1);
	}
	UNPROTECT(1); // piv
    }
    UNPROTECT(1); // ans
    return ans;
}

static SEXP La_chol2inv(SEXP A, SEXP size)
{
    int sz = asInteger(size);
    if (sz == NA_INTEGER || sz < 1) {
	error(_("'size' argument must be a positive integer"));
	return R_NilValue; /* -Wall */
    } else {
	SEXP ans, Amat = A; /* -Wall: we initialize here as for the 1x1 case */
	int m = 1, n = 1, nprot = 0;

	if (sz == 1 && !isMatrix(A) && isReal(A)) {
	    /* nothing to do; m = n = 1; ... */
	} else if (isMatrix(A)) {
	    SEXP adims = getAttrib(A, R_DimSymbol);
	    if (TYPEOF(adims) != INTSXP) error("non-integer dims");
	    Amat = PROTECT(coerceVector(A, REALSXP)); nprot++;
	    m = INTEGER(adims)[0]; n = INTEGER(adims)[1];
	} else error(_("'a' must be a numeric matrix"));

	if (sz > n) { UNPROTECT(nprot); error(_("'size' cannot exceed ncol(x) = %d"), n); }
	if (sz > m) { UNPROTECT(nprot); error(_("'size' cannot exceed nrow(x) = %d"), m); }
	ans = PROTECT(allocMatrix(REALSXP, sz, sz)); nprot++;
	size_t M = m, SZ = sz;
	for (int j = 0; j < sz; j++) {
	    for (int i = 0; i <= j; i++)
		REAL(ans)[i + j * SZ] = REAL(Amat)[i + j * M];
	}
	int info;
	F77_CALL(dpotri)("Upper", &sz, REAL(ans), &sz, &info);
	if (info != 0) {
	    UNPROTECT(nprot);
	    if (info > 0)
		error(_("element (%d, %d) is zero, so the inverse cannot be computed"),
		      info, info);
	    error(_("argument %d of Lapack routine %s had invalid value"),
		  -info, "dpotri");
	}
	for (int j = 0; j < sz; j++)
	    for (int i = j+1; i < sz; i++)
		REAL(ans)[i + j * SZ] = REAL(ans)[j + i * SZ];
	UNPROTECT(nprot);
	return ans;
    }
}

/* ------------------------------------------------------------ */

/* Real case of solve.default */
static SEXP La_solve(SEXP A, SEXP Bin, SEXP tolin)
{
    int n, p;
    double *avals, anorm, rcond, tol = asReal(tolin), *work;
    SEXP B, Adn, Bdn;

    if (!(isMatrix(A) && 
	  (TYPEOF(A) == REALSXP || TYPEOF(A) == INTSXP || TYPEOF(A) == LGLSXP)))
	error(_("'a' must be a numeric matrix"));
    int *Adims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    n = Adims[0];
    if(n == 0) error(_("'a' is 0-diml"));
    int n2 = Adims[1];
    if(n2 != n) error(_("'a' (%d x %d) must be square"), n, n2);
    Adn = getAttrib(A, R_DimNamesSymbol);

    if (isMatrix(Bin)) {
	int *Bdims = INTEGER(coerceVector(getAttrib(Bin, R_DimSymbol), INTSXP));
	p = Bdims[1];
	if(p == 0) error(_("no right-hand side in 'b'"));
	int p2 = Bdims[0];
	if(p2 != n)
	    error(_("'b' (%d x %d) must be compatible with 'a' (%d x %d)"),
		  p2, p, n, n);
	PROTECT(B = allocMatrix(REALSXP, n, p));
	SEXP Bindn =  getAttrib(Bin, R_DimNamesSymbol);
	// This is somewhat odd, but Matrix relies on dropping NULL dimnames
	if (!isNull(Adn) || !isNull(Bindn)) {
	    // rownames(ans) = colnames(A), colnames(ans) = colnames(Bin)
	    Bdn = allocVector(VECSXP, 2);
	    if (!isNull(Adn)) SET_VECTOR_ELT(Bdn, 0, VECTOR_ELT(Adn, 1));
	    if (!isNull(Bindn)) SET_VECTOR_ELT(Bdn, 1, VECTOR_ELT(Bindn, 1));
	    if (!isNull(VECTOR_ELT(Bdn, 0)) || !isNull(VECTOR_ELT(Bdn, 1)))
		setAttrib(B, R_DimNamesSymbol, Bdn);
	}
    } else {
	p = 1;
	if(length(Bin) != n)
	    error(_("'b' (%d x %d) must be compatible with 'a' (%d x %d)"),
		  length(Bin), p, n, n);	
	PROTECT(B = allocVector(REALSXP, n));
	if (!isNull(Adn)) setAttrib(B, R_NamesSymbol, VECTOR_ELT(Adn, 1));
    }
    PROTECT(Bin = coerceVector(Bin, REALSXP));
    Memcpy(REAL(B), REAL(Bin), (size_t)n * p);
    
    int *ipiv = (int *) R_alloc(n, sizeof(int));

    /* work on a copy of A */
    if (!isReal(A)) {
	A = coerceVector(A, REALSXP);
	avals = REAL(A);
    } else {
	avals = (double *) R_alloc((size_t)n * n, sizeof(double));
	Memcpy(avals, REAL(A), (size_t)n * n);
    }
    PROTECT(A);
    int info;
    F77_CALL(dgesv)(&n, &p, avals, &n, ipiv, REAL(B), &n, &info);
    if (info < 0)
	error(_("argument %d of Lapack routine %s had invalid value"),
	      -info, "dgesv");
    if (info > 0)
	error(_("Lapack routine %s: system is exactly singular: U[%d,%d] = 0"),
	      "dgesv", info, info);
    if(tol > 0) {
	char one[2] = "1";
	anorm = F77_CALL(dlange)(one, &n, &n, REAL(A), &n, (double*) NULL);
	work = (double *) R_alloc(4*(size_t)n, sizeof(double));
	F77_CALL(dgecon)(one, &n, avals, &n, &anorm, &rcond, work, ipiv, &info);
	if (rcond < tol)
	    error(_("system is computationally singular: reciprocal condition number = %g"),
		  rcond);
    }
    UNPROTECT(3); /* B, Bin, A */
    return B;
}

/* Real case of qr.default */
static SEXP La_qr(SEXP Ain)
{
    int m, n;

    if (!isMatrix(Ain)) error(_("'a' must be a numeric matrix"));
    SEXP Adn = getAttrib(Ain, R_DimNamesSymbol);
    int *Adims = INTEGER(coerceVector(getAttrib(Ain, R_DimSymbol), INTSXP));
    m = Adims[0]; n = Adims[1];
    SEXP A;
    if (!isReal(Ain)) {
	A = PROTECT(coerceVector(Ain, REALSXP));
    } else {
	A = PROTECT(allocMatrix(REALSXP, m, n));
	Memcpy(REAL(A), REAL(Ain), (size_t)m * n);
    }

    SEXP jpvt = PROTECT(allocVector(INTSXP, n));
    for (int i = 0; i < n; i++) INTEGER(jpvt)[i] = 0;
    SEXP tau = PROTECT(allocVector(REALSXP, m < n ? m : n));
    int info, lwork = -1;
    double tmp;
    F77_CALL(dgeqp3)(&m, &n, REAL(A), &m, INTEGER(jpvt), REAL(tau),
		     &tmp, &lwork, &info);
    if (info < 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgeqp3");
    lwork = (int) tmp;
    double *work = (double *) R_alloc(lwork, sizeof(double));
    F77_CALL(dgeqp3)(&m, &n, REAL(A), &m, INTEGER(jpvt), REAL(tau),
		     work, &lwork, &info);
    if (info < 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgeqp3");
    SEXP val = PROTECT(allocVector(VECSXP, 4));
    SEXP nm = PROTECT(allocVector(STRSXP, 4));
    SET_STRING_ELT(nm, 0, mkChar("qr"));
    SET_STRING_ELT(nm, 1, mkChar("rank"));
    SET_STRING_ELT(nm, 2, mkChar("qraux"));
    SET_STRING_ELT(nm, 3, mkChar("pivot"));
    setAttrib(val, R_NamesSymbol, nm);
    // Fix up dimnames(A)
    if(!isNull(Adn)) {
	SEXP Adn2 = duplicate(Adn);
	SEXP cn = VECTOR_ELT(Adn, 1), cn2 = VECTOR_ELT(Adn2, 1);
	if(!isNull(cn)) { // pivot them
	    for (int j = 0; j < n; j++)
		SET_STRING_ELT(cn2, j, STRING_ELT(cn, INTEGER(jpvt)[j]-1));
	}
	setAttrib(A, R_DimNamesSymbol, Adn2);
    }
    SET_VECTOR_ELT(val, 0, A);
    SET_VECTOR_ELT(val, 1, ScalarInteger(m < n ? m : n));
    SET_VECTOR_ELT(val, 2, tau);
    SET_VECTOR_ELT(val, 3, jpvt);
    UNPROTECT(5);
    return val;
}

/* Real case of qr.coef */
static SEXP qr_coef_real(SEXP Q, SEXP Bin)
{
    int n, nrhs, k, *Bdims;
    SEXP B, qr = VECTOR_ELT(Q, 0), tau = VECTOR_ELT(Q, 2);
    double *work, tmp;

    k = LENGTH(tau);
    if (!isMatrix(Bin)) error(_("'b' must be a numeric matrix"));
    
    B = PROTECT(isReal(Bin) ? duplicate(Bin) : coerceVector(Bin, REALSXP));

    n = INTEGER(coerceVector(getAttrib(qr, R_DimSymbol), INTSXP))[0];
    Bdims = INTEGER(coerceVector(getAttrib(B, R_DimSymbol), INTSXP));
    if(Bdims[0] != n)
	error(_("right-hand side should have %d not %d rows"), n, Bdims[0]);
    nrhs = Bdims[1];
    int lwork = -1, info;
    F77_CALL(dormqr)("L", "T", &n, &nrhs, &k,
		     REAL(qr), &n, REAL(tau), REAL(B), &n,
		     &tmp, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dormqr");
    lwork = (int) tmp;
    work = (double *) R_alloc(lwork, sizeof(double));
    F77_CALL(dormqr)("L", "T", &n, &nrhs, &k,
		     REAL(qr), &n, REAL(tau), REAL(B), &n,
		     work, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dormqr");
    F77_CALL(dtrtrs)("U", "N", "N", &k, &nrhs,
		     REAL(qr), &n, REAL(B), &n, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dtrtrs");
    UNPROTECT(1);
    return B;
}

/* Real case of qr.qy and qr.aty */
static SEXP qr_qy_real(SEXP Q, SEXP Bin, SEXP trans)
{
    int n, nrhs, k, *Bdims, tr;
    SEXP B, qr = VECTOR_ELT(Q, 0), tau = VECTOR_ELT(Q, 2);
    double *work, tmp;

    k = LENGTH(tau);
    if (!isMatrix(Bin)) error(_("'b' must be a numeric matrix"));
    tr = asLogical(trans);
    if(tr == NA_LOGICAL) error(_("invalid '%s' argument"), "trans");

    B = PROTECT(isReal(Bin) ? duplicate(Bin) : coerceVector(Bin, REALSXP));
    n = INTEGER(coerceVector(getAttrib(qr, R_DimSymbol), INTSXP))[0];
    Bdims = INTEGER(coerceVector(getAttrib(B, R_DimSymbol), INTSXP));
    if(Bdims[0] != n)
	error(_("right-hand side should have %d not %d rows"), n, Bdims[0]);
    nrhs = Bdims[1];
    int lwork = -1, info;
    F77_CALL(dormqr)("L", tr ? "T" : "N", &n, &nrhs, &k,
		     REAL(qr), &n, REAL(tau), REAL(B), &n,
		     &tmp, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dormqr");
    lwork = (int) tmp;
    work = (double *) R_alloc(lwork, sizeof(double));
    F77_CALL(dormqr)("L", tr ? "T" : "N", &n, &nrhs, &k,
		     REAL(qr), &n, REAL(tau), REAL(B), &n,
		     work, &lwork, &info);
    if (info != 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dormqr");
    UNPROTECT(1);
    return B;
}

/* TODO : add  a *complex* version, using  LAPACK ZGETRF() */
static SEXP det_ge_real(SEXP Ain, SEXP logarithm)
{
    int info, sign = 1, useLog = asLogical(logarithm);
    double modulus = 0.0; /* -Wall */

    if (!isMatrix(Ain)) error(_("'a' must be a numeric matrix"));
    if (useLog == NA_LOGICAL) error(_("argument 'logarithm' must be logical"));
    SEXP A = PROTECT(isReal(Ain) ? duplicate(Ain): coerceVector(Ain, REALSXP));
    int *Adims = INTEGER(coerceVector(getAttrib(Ain, R_DimSymbol), INTSXP));
    int n = Adims[0];
    if (Adims[1] != n) error(_("'a' must be a square matrix"));
    int *jpvt = (int *) R_alloc(n, sizeof(int));
    F77_CALL(dgetrf)(&n, &n, REAL(A), &n, jpvt, &info);
    if (info < 0)
	error(_("error code %d from Lapack routine '%s'"), info, "dgetrf");
    else if (info > 0) { /* Singular matrix:  U[i,i] (i := info) is 0 */
	/*warning("Lapack dgetrf(): singular matrix: U[%d,%d]=0", info,info);*/
	modulus =
#ifdef _not_quite_/* unfortunately does not work -- FIXME */
	    (ISNAN(REAL(A)[(info-1)*n + (info-1)])) /* pivot is NA/NaN */
	    ? R_NaN :
#endif
	    (useLog ? R_NegInf : 0.);
    }
    else {
	for (int i = 0; i < n; i++) if (jpvt[i] != (i + 1)) sign = -sign;
	if (useLog) {
	    modulus = 0.0;
	    size_t N1 = n+1;
	    for (int i = 0; i < n; i++) {
		double dii = REAL(A)[i * N1]; /* ith diagonal element */
		modulus += log(dii < 0 ? -dii : dii);
		if (dii < 0) sign = -sign;
	    }
	} else {
	    modulus = 1.0;
	    size_t N1 = n+1;
	    for (int i = 0; i < n; i++) modulus *= REAL(A)[i * N1];
	    if (modulus < 0) {
		modulus = -modulus;
		sign = -sign;
	    }
	}
    }
    SEXP val = PROTECT(allocVector(VECSXP, 2));
    SEXP nm = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("modulus"));
    SET_STRING_ELT(nm, 1, mkChar("sign"));
    setAttrib(val, R_NamesSymbol, nm);
    SET_VECTOR_ELT(val, 0, ScalarReal(modulus));
    setAttrib(VECTOR_ELT(val, 0), install("logarithm"), ScalarLogical(useLog));
    SET_VECTOR_ELT(val, 1, ScalarInteger(sign));
    setAttrib(val, R_ClassSymbol, ScalarString(mkChar("det")));
    UNPROTECT(3);
    return val;
}

static SEXP mod_do_lapack(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans = R_NilValue;

    switch(PRIMVAL(op)) {
    case 0: ans = La_qr_cmplx(CAR(args)); break;
    case 1: ans = La_rs(CAR(args), CADR(args)); break;
    case 2: ans = La_rs_cmplx(CAR(args), CADR(args)); break;
    case 3: ans = La_rg(CAR(args), CADR(args)); break;
    case 41: ans = La_rg_cmplx(CAR(args), CADR(args)); break;
    case 5: ans = La_rs(CAR(args), CADR(args)); break;
    case 51: ans = La_rs_cmplx(CAR(args), CADR(args)); break;
    case 6: ans = La_dlange(CAR(args), CADR(args)); break;
    case 7: ans = La_dgecon(CAR(args), CADR(args)); break;
    case 8: ans = La_dtrcon(CAR(args), CADR(args)); break;
    case 9: ans = La_zgecon(CAR(args), CADR(args)); break;
    case 10: ans = La_ztrcon(CAR(args), CADR(args)); break;
    case 11: ans = La_solve_cmplx(CAR(args), CADR(args)); break;

    case 100: ans = La_solve(CAR(args), CADR(args), CADDR(args)); break;
    case 101: ans = La_qr(CAR(args)); break;

    case 200: ans = La_chol(CAR(args), CADR(args), CADDR(args)); break;
    case 201: ans = La_chol2inv(CAR(args), CADR(args)); break;

    case 300: ans = qr_coef_real(CAR(args), CADR(args)); break;
    case 301: ans = qr_qy_real(CAR(args), CADR(args), CADDR(args)); break;
    case 302: ans = det_ge_real(CAR(args), CADR(args)); break;
    case 303: ans = qr_coef_cmplx(CAR(args), CADR(args)); break;
    case 304: ans = qr_qy_cmplx(CAR(args), CADR(args), CADDR(args)); break;

    case 400:
    {
	SEXP a1, a2, a3, a4;
	a1 = CAR(args); args = CDR(args);
	a2 = CAR(args); args = CDR(args);
	a3 = CAR(args); args = CDR(args);
	a4 = CAR(args); args = CDR(args);
	ans = La_svd(a1, a2, a3, a4, CAR(args));
	break;
    }
    case 401:
    {
	SEXP a1, a2, a3, a4;
	a1 = CAR(args); args = CDR(args);
	a2 = CAR(args); args = CDR(args);
	a3 = CAR(args); args = CDR(args);
	a4 = CAR(args); args = CDR(args);
	ans = La_svd_cmplx(a1, a2, a3, a4, CAR(args));
	break;
    }
    case 1000:
    {
	int major, minor, patch;
	char str[20];
	F77_CALL(ilaver)(&major, &minor, &patch);
	snprintf(str, 20, "%d.%d.%d", major, minor, patch);
	ans = mkString(str);
	break;
    }
    }

    return ans;
}


/* ------------------------------------------------------------ */

#include <Rmodules/Rlapack.h>
#include <R_ext/Rdynload.h>

void
R_init_lapack(DllInfo *info)
{
    R_LapackRoutines *tmp;
    tmp = Calloc(1, R_LapackRoutines);

    tmp->do_lapack = mod_do_lapack;
    R_setLapackRoutines(tmp);
}
