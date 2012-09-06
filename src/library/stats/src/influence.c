/* 
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2012 The R Core Team
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

#include <R.h>
#include <Rinternals.h>
#include "statsR.h"	

#include <R_ext/RS.h>
void F77_NAME(lminfl)(double *x, int *ldx, int *n, int *k, int *docoef,
		      double *qraux, double *resid, double *hat,
		      double *coef, double *sigma, double *tol);
					
SEXP influence(SEXP mqr, SEXP do_coef, SEXP e, SEXP stol)
{
    SEXP qr = getListElement(mqr, "qr"), qraux = getListElement(mqr, "qraux");
    int n = nrows(qr), k = asInteger(getListElement(mqr, "rank"));
    int docoef = asLogical(do_coef);
    double tol = asReal(stol);

    SEXP hat = PROTECT(allocVector(REALSXP, n));
    double *rh = REAL(hat);
    SEXP coefficients;
    if(docoef) coefficients = PROTECT(allocMatrix(REALSXP, n, k));
    else coefficients = PROTECT(allocVector(REALSXP, 0));
    SEXP sigma = PROTECT(allocVector(REALSXP, n));
    F77_CALL(lminfl)(REAL(qr), &n, &n, &k, &docoef, REAL(qraux),
		     REAL(e), rh, REAL(coefficients), REAL(sigma), &tol);

    for (int i = 0; i < n; i++) if (rh[i] > 1. - tol) rh[i] = 1.;
    SEXP ans = PROTECT(allocVector(VECSXP, docoef ? 4 : 3));
    SEXP nm = allocVector(STRSXP, docoef ? 4 : 3);
    setAttrib(ans, R_NamesSymbol, nm);
    int m = 0;
    SET_VECTOR_ELT(ans, m, hat);
    SET_STRING_ELT(nm, m++, mkChar("hat"));
    if (docoef) {
	SET_VECTOR_ELT(ans, m, coefficients);
	SET_STRING_ELT(nm, m++, mkChar("coefficients"));
    }
    SET_VECTOR_ELT(ans, m, sigma);
    SET_STRING_ELT(nm, m++, mkChar("sigma"));
    SET_VECTOR_ELT(ans, m, e);
    SET_STRING_ELT(nm, m, mkChar("wt.res"));
    UNPROTECT(4);
    return ans;
}
