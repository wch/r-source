/* Interface routines, callable from R using .Call, for Lapack code */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "Lapack.h"

SEXP La_svd(SEXP jobu, SEXP jobv, SEXP x, SEXP s, SEXP u, SEXP v)
{
    int *xdims, n, p, lwork, info;
    double *work, tmp;
    SEXP val, nm;

    if (!(isString(jobu) && isString(jobv))) {
	error("jobu and jobv must be character objects");
	return R_NilValue;
    }
    if (!(isMatrix(x) && isMatrix(u) && isMatrix(v))) {
	error("x and u and v must be matrices");
	return R_NilValue;
    }
    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0]; p = xdims[1];
    if (!(isNumeric(s))) {
	error("s must be a numeric object");
	return R_NilValue;
    }
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(dgesvd)(CHAR(STRING_ELT(jobu, 0)), CHAR(STRING_ELT(jobv, 0)),
		     &n, &p, REAL(x), &n, REAL(s),
		     REAL(u), INTEGER(getAttrib(u, R_DimSymbol)),
		     REAL(v), INTEGER(getAttrib(v, R_DimSymbol)),
		     &tmp, &lwork, &info);
    lwork = (int) tmp;
    work = Calloc((size_t) lwork, double);
    F77_CALL(dgesvd)(CHAR(STRING_ELT(jobu, 0)), CHAR(STRING_ELT(jobv, 0)),
		     &n, &p, REAL(x), &n, REAL(s),
		     REAL(u), INTEGER(getAttrib(u, R_DimSymbol)),
		     REAL(v), INTEGER(getAttrib(v, R_DimSymbol)),
		     work, &lwork, &info);
    Free(work);
    if (info != 0) {
	error("error code %d from Lapack routine dgesvd", info);
	return R_NilValue;
    }
    val = PROTECT(allocVector(VECSXP, 3));
    nm = PROTECT(allocVector(STRSXP, 3));
    SET_STRING_ELT(nm, 0, mkChar("d"));
    SET_STRING_ELT(nm, 1, mkChar("u"));
    SET_STRING_ELT(nm, 2, mkChar("vt"));
    setAttrib(val, R_NamesSymbol, nm);
    SET_VECTOR_ELT(val, 0, s);
    SET_VECTOR_ELT(val, 1, u);
    SET_VECTOR_ELT(val, 2, v);
    UNPROTECT(2);
    return val;
}

SEXP La_rs(SEXP x, SEXP only_values)
{
    int *xdims, n, lwork, info, i, j;
    char jobv[1], uplo[1];
    SEXP values, ret, nm;
    double *work, *rx = REAL(x), *rvalues, tmp;

    uplo[0] = 'L';
    if (!(isMatrix(x) && isNumeric(x)))
	error("x must be a numeric matrix");
    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1])
	error("x must be a square numeric matrix");
    if (LENGTH(only_values) < 1)
	error("only.values cannot be of length 0");
    if (LOGICAL(coerceVector(only_values, LGLSXP))[0]) jobv[0] = 'N';
    else jobv[0] = 'V';

    values = allocVector(REALSXP, n);
    rvalues = REAL(values);
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(dsyev)(jobv, uplo, &n, rx, &n, rvalues, &tmp, &lwork, &info);
    lwork = (int) tmp;
    work = Calloc((size_t) lwork, double);
    F77_CALL(dsyev)(jobv, uplo, &n, rx, &n, rvalues, work, &lwork, &info);
    /* Map to eigenvalues in descending order */
    for (i = 0; i < n; i++) work[i] = rvalues[i];
    for (i = 0; i < n; i++) rvalues[i] = work[n - 1 - i];
    for (i = 0; i < n/2; i++) {
	for(j = 0; j < n; j++) work[j] = rx[j + n*i];
	for(j = 0; j < n; j++) rx[j + n*i] = rx[j + n*(n-1-i)];
	for(j = 0; j < n; j++) rx[j + n*(n-1-i)] = work[j];
    }
    Free(work);
    if (info != 0) {
	error("error code %d from Lapack routine dsyev", info);
	return R_NilValue;
    }
    if (jobv[0] == 'V') {
	ret = PROTECT(allocVector(VECSXP, 2));
	nm = PROTECT(allocVector(STRSXP, 2));
	SET_STRING_ELT(nm, 0, mkChar("values"));
	SET_STRING_ELT(nm, 1, mkChar("vectors"));
	setAttrib(ret, R_NamesSymbol, nm);
	SET_VECTOR_ELT(ret, 0, values);
	SET_VECTOR_ELT(ret, 1, x);
    }
    else {
	ret = PROTECT(allocVector(VECSXP, 1));
	nm = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(nm, 0, mkChar("values"));
	setAttrib(ret, R_NamesSymbol, nm);
	SET_VECTOR_ELT(ret, 0, values);
    }
    UNPROTECT(2);
    return ret;
}

static SEXP unscramble(const double* imaginary, int n,
		       const double* vecs)
{
    int i, j;
    SEXP s = allocMatrix(CPLXSXP, n, n);

    for (j = 0; j < n; j++) {
	if (imaginary[j] != 0) {
	    int j1 = j + 1;
	    for (i = 0; i < n; i++) {
		COMPLEX(s)[i+n*j].r = COMPLEX(s)[i+n*j1].r = vecs[i + j * n];
		COMPLEX(s)[i+n*j1].i = -(COMPLEX(s)[i+n*j].i = vecs[i + j1 * n]);
	    }
	    j = j1;
	} else {
	    for (i = 0; i < n; i++) {
		COMPLEX(s)[i+n*j].r = vecs[i + j * n];
		COMPLEX(s)[i+n*j].i = 0.0;
	    }
	}
    }
    return s;
}

SEXP La_rg(SEXP x, SEXP only_values)
{
    int i, n, lwork, info, vectors, complexValues, *xdims;
    double *work, *wR, *wI, *left, *right, *xvals, tmp;
    char jobVL[1], jobVR[1];
    SEXP ret, nm, val;

    if (!(isMatrix(x) && isNumeric(x))) {
	error("x must be a numeric matrix");
	return R_NilValue;
    }
    xdims = INTEGER(coerceVector(getAttrib(x, R_DimSymbol), INTSXP));
    n = xdims[0];
    if (n != xdims[1]) {
	error("x must be a square numeric matrix");
	return R_NilValue;
    }
    xvals = Calloc((size_t) (n * n), double); /* work on a copy of x */
    Memcpy(xvals, REAL(x), (size_t) (n * n));
    if (LENGTH(only_values) < 1)
	error("only.values cannot be of length 0");
    jobVL[0] = jobVR[0] = 'N';
    left = right = (double *) 0;
    vectors = 0;
    if (!LOGICAL(coerceVector(only_values, LGLSXP))[0]) {
	jobVR[0] = 'V';
	right = Calloc((size_t) n * n, double);
	vectors = 1;
    }
    wR = Calloc((size_t) n, double);
    wI = Calloc((size_t) n, double);
    /* ask for optimal size of work array */
    lwork = -1;
    F77_CALL(dgeev)(jobVL, jobVR, &n, xvals, &n, wR, wI,
		    left, &n, right, &n, &tmp, &lwork, &info);
    lwork = (int) tmp;
    work = Calloc((size_t) lwork, double);
    F77_CALL(dgeev)(jobVL, jobVR, &n, xvals, &n, wR, wI,
		    left, &n, right, &n, work, &lwork, &info);
    
    Free(work);
    if (info != 0) {
	Free(wR); Free(wI);
	if (vectors) {
	    Free(right);
	}
	error("error code %d from Lapack routine dgeev", info);
    }
    complexValues = 0;
    for (i = 0; i < n; i++) {
	if (wI[i] != 0.0) complexValues = 1;
    }
    ret = PROTECT(allocVector(VECSXP, 2));
    nm = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(nm, 0, mkChar("values"));
    SET_STRING_ELT(nm, 1, mkChar("vectors"));
    setAttrib(ret, R_NamesSymbol, nm);
    SET_VECTOR_ELT(ret, 1, R_NilValue);
    if (complexValues) {
	val = allocVector(CPLXSXP, n);
	for (i = 0; i < n; i++) {
	    COMPLEX(val)[i].r = wR[i];
	    COMPLEX(val)[i].i = wI[i];
	}
	SET_VECTOR_ELT(ret, 0, val);

	if (vectors) {
	    SET_VECTOR_ELT(ret, 1, unscramble(wI, n, right));
	}
    } else {
	/* FIXME Map to eigenvalues in descending order */
	val = allocVector(REALSXP, n);
	for (i = 0; i < n; i++) {
	    REAL(val)[i] = wR[i];
	}
	SET_VECTOR_ELT(ret, 0, val);
	val = allocMatrix(REALSXP, n, n);
	for (i = 0; i < (n * n); i++) {
	    REAL(val)[i] = right[i];
	}
	SET_VECTOR_ELT(ret, 1, val);
    }
    UNPROTECT(2);
    return ret;
}

#ifdef Unix
/* FIXME: until configure test is in place */
#define HAVE_DOUBLE_COMPLEX
#endif
SEXP La_zgesv(SEXP A, SEXP B)
{
#ifdef HAVE_DOUBLE_COMPLEX
    int n, p, info, *ipiv, *Adims, *Bdims;

    if (!(isMatrix(A) && isComplex(A))) {
	error("A must be a complex matrix");
    }
    if (!(isMatrix(B) && isComplex(B))) {
	error("A must be a complex matrix");
	}
    Adims = INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP));
    Bdims = INTEGER(coerceVector(getAttrib(B, R_DimSymbol), INTSXP));
    n = Adims[0];
    if(n == 0) error("A is 0-diml");
    p = Bdims[1];
    if(p == 0) error("no rhs in B");
    if(Adims[1] != n)
	error("A (%d x %d) must be square", n, Adims[1]);
    if(Bdims[0] != n)
	error("B (%d x %d) must be square", Bdims[0], p);
    ipiv = (int *) R_alloc(n, sizeof(int));

    F77_CALL(zgesv)(&n, &p, COMPLEX(A), &n, ipiv, COMPLEX(B), &n, &info);
    if (info != 0) {
	error("error code %d from Lapack routine zgesv", info);
    }
    return B;
#else
    error("Fortran complex functions are not available on this platform");
    return R_NilValue; /* -Wall */
#endif
}
