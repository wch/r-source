/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 2012  The R Core Team
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
 *  http://www.r-project.org/Licenses/.
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>

#include "statsR.h"

/* A wrapper to replace

    z <- .Fortran("dqrls",
		  qr = x, n = n, p = p,
		  y = y, ny = ny,
		  tol = as.double(tol),
		  coefficients = mat.or.vec(p, ny),
		  residuals = y, effects = y, rank = integer(1L),
		  pivot = 1L:p, qraux = double(p), work = double(2*p),
                  PACKAGE="base")

    with less allocation.
*/


SEXP Cdqrls(SEXP x, SEXP y, SEXP tol)
{
    SEXP ans, ansnames;
    SEXP qr, coefficients, residuals, effects, pivot, qraux;
    int n, ny = 0, p, rank, nprotect = 4, pivoted = 0;
    double rtol = asReal(tol), *work;


    int *dims = INTEGER(getAttrib(x, R_DimSymbol));
    n = dims[0]; p = dims[1];
    if(n) ny = LENGTH(y)/n;  /* n x ny, or a vector */

    /* These lose attributes, so do after we have extracted dims */
    if (TYPEOF(x) != REALSXP) {
	PROTECT(x = coerceVector(x, REALSXP)); 
	nprotect++;
    }
    if (TYPEOF(y) != REALSXP) {
	PROTECT(y = coerceVector(y, REALSXP));
	nprotect++;
    }

    double *rptr = REAL(x);
    for (R_xlen_t i = 0 ; i < XLENGTH(x) ; i++)
	if(!R_FINITE(rptr[i])) error("NA/NaN/Inf in 'x'");
    
    rptr = REAL(y);
    for (R_xlen_t i = 0 ; i < XLENGTH(y) ; i++)
	if(!R_FINITE(rptr[i])) error("NA/NaN/Inf in 'y'");

    PROTECT(ans = allocVector(VECSXP, 9));
    ansnames = allocVector(STRSXP, 9);
    setAttrib(ans, R_NamesSymbol, ansnames);
    SET_STRING_ELT(ansnames, 0, mkChar("qr"));
    SET_STRING_ELT(ansnames, 1, mkChar("coefficients"));
    SET_STRING_ELT(ansnames, 2, mkChar("residuals"));
    SET_STRING_ELT(ansnames, 3, mkChar("effects"));
    SET_STRING_ELT(ansnames, 4, mkChar("rank"));
    SET_STRING_ELT(ansnames, 5, mkChar("pivot"));
    SET_STRING_ELT(ansnames, 6, mkChar("qraux"));
    SET_STRING_ELT(ansnames, 7, mkChar("tol"));
    SET_STRING_ELT(ansnames, 8, mkChar("pivoted"));
    SET_VECTOR_ELT(ans, 0, qr = duplicate(x));
    if (ny > 1) coefficients = allocMatrix(REALSXP, p, ny);
    else coefficients = allocVector(REALSXP, p);
    PROTECT(coefficients);
    SET_VECTOR_ELT(ans, 1, coefficients);
    SET_VECTOR_ELT(ans, 2, residuals = duplicate(y));
    SET_VECTOR_ELT(ans, 3, effects = duplicate(y));
    PROTECT(pivot = allocVector(INTSXP, p));
    int *ip = INTEGER(pivot);
    for(int i = 0; i < p; i++) ip[i] = i+1;
    SET_VECTOR_ELT(ans, 5, pivot);
    PROTECT(qraux = allocVector(REALSXP, p));
    SET_VECTOR_ELT(ans, 6, qraux);
    SET_VECTOR_ELT(ans, 7, tol);
   
    work = (double *) R_alloc(2 * p, sizeof(double));
    F77_CALL(dqrls)(REAL(qr), &n, &p, REAL(y), &ny, &rtol,
		    REAL(coefficients), REAL(residuals), REAL(effects),
		    &rank, INTEGER(pivot), REAL(qraux), work);
    SET_VECTOR_ELT(ans, 4, ScalarInteger(rank));
    for(int i = 0; i < p; i++)
	if(ip[i] != i+1) { pivoted = 1; break; }
    SET_VECTOR_ELT(ans, 8, ScalarLogical(pivoted));
    UNPROTECT(nprotect);
    
    return ans;
}
