/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2016	The R Foundation
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

#include <math.h>
#include <R.h>			/* for NA_REAL, includes math.h */
#include <Rinternals.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

static double dokern(double x, int kern)
{
    if(kern == 1) return(1.0);
    if(kern == 2) return(exp(-0.5*x*x));
    return(0.0); /* -Wall */
}

static void BDRksmooth(double *x, double *y, R_xlen_t n,
		       double *xp, double *yp, R_xlen_t np,
		       int kern, double bw)
{
    R_xlen_t imin = 0;
    double cutoff = 0.0, num, den, x0, w;

    /* bandwidth is in units of half inter-quartile range. */
    if(kern == 1) {bw *= 0.5; cutoff = bw;}
    if(kern == 2) {bw *= 0.3706506; cutoff = 4*bw;}
    while(x[imin] < xp[0] - cutoff && imin < n) imin++;
    for(R_xlen_t j = 0; j < np; j++) {
	num = den = 0.0;
	x0 = xp[j];
	for(R_xlen_t i = imin; i < n; i++) {
	    if(x[i] < x0 - cutoff) imin = i;
	    else {
		if(x[i] > x0 + cutoff) break;
		w = dokern(fabs(x[i] - x0)/bw, kern);
		num += w*y[i];
		den += w;
	    }
	}
	if(den > 0) yp[j] = num/den; else yp[j] = NA_REAL;
    }
}


void NORET F77_SUB(bdrsplerr)(void)
{
    error(_("only 2500 rows are allowed for sm.method=\"spline\""));
}

SEXP ksmooth(SEXP x, SEXP y, SEXP xp, SEXP skrn, SEXP sbw)
{
    int krn = asInteger(skrn);
    double bw = asReal(sbw);
    x = PROTECT(coerceVector(x, REALSXP));
    y = PROTECT(coerceVector(y, REALSXP));
    xp = PROTECT(coerceVector(xp, REALSXP));
    R_xlen_t nx = XLENGTH(x), np = XLENGTH(xp);
    SEXP yp = PROTECT(allocVector(REALSXP, np));

    BDRksmooth(REAL(x), REAL(y), nx, REAL(xp), REAL(yp), np, krn, bw);
    SEXP ans = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, xp);
    SET_VECTOR_ELT(ans, 1, yp);
    SEXP nm = allocVector(STRSXP, 2);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("x"));
    SET_STRING_ELT(nm, 1, mkChar("y"));
    UNPROTECT(5);
    return ans;
}
