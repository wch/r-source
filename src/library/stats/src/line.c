/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2012   The R Core Team.
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

#include <R_ext/Utils.h>	/* R_rsort() */
#include <math.h>

#include <Rinternals.h>
#include "statsR.h"

/* Speed up by `inlining' these (as macros) [since R version 1.2] : */
#if 1
#define il(n,x)	(int)floor((n - 1) * x)
#define iu(n,x)	(int) ceil((n - 1) * x)

#else
static int il(int n, double x)
{
    return (int)floor((n - 1) * x) ;
}

static int iu(int n, double x)
{
    return (int)ceil((n - 1) * x);
}
#endif

static void line(double *x, double *y, /* input (x[i],y[i])s */
		 double *z, double *w, /* work and output: resid. & fitted */
		 /* all the above of length */ int n,
		 double coef[2])
{
    int i, j, k;
    double xb, x1, x2, xt, yt, yb, tmp1, tmp2;
    double slope, yint;

    for(i = 0 ; i < n ; i++) {
	z[i] = x[i];
	w[i] = y[i];
    }
    R_rsort(z, n);/* z = ordered abscissae */

    tmp1 = z[il(n, 1./6.)];
    tmp2 = z[iu(n, 1./6.)];	xb = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 2./6.)];
    tmp2 = z[iu(n, 2./6.)];	x1 = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 4./6.)];
    tmp2 = z[iu(n, 4./6.)];	x2 = 0.5*(tmp1+tmp2);

    tmp1 = z[il(n, 5./6.)];
    tmp2 = z[iu(n, 5./6.)];	xt = 0.5*(tmp1+tmp2);

    slope = 0.;

    for(j = 1 ; j <= 1 ; j++) {
	/* yb := Median(y[i]; x[i] <= quantile(x, 1/3) */
	k = 0;
	for(i = 0 ; i < n ; i++)
	    if(x[i] <= x1)
		z[k++] = w[i];
	R_rsort(z, k);
	yb = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);

	/* yt := Median(y[i]; x[i] >= quantile(x, 2/3) */
	k = 0;
	for(i = 0 ; i < n ; i++)
	    if(x[i] >= x2)
		z[k++] = w[i];
	R_rsort(z,k);
	yt = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);

	slope += (yt - yb)/(xt - xb);
	for(i = 0 ; i < n ; i++) {
	    z[i] = y[i] - slope*x[i];
	    /* never used: w[i] = z[i]; */
	}
	R_rsort(z,n);
	yint = 0.5 * (z[il(n, 0.5)] + z[iu(n, 0.5)]);
    }
    for( i = 0 ; i < n ; i++ ) {
	w[i] = yint + slope*x[i];
	z[i] = y[i] - w[i];
    }
    coef[0] = yint;
    coef[1] = slope;
}

void tukeyline0(double *x, double *y, double *z, double *w, int *n,
	       double *coef)
{
    line(x, y, z, w, *n, coef);
}

SEXP tukeyline(SEXP x, SEXP y, SEXP call)
{
    int n = LENGTH(x);
    if (n < 2) error("insufficient observations");
    SEXP ans;
    ans = PROTECT(allocVector(VECSXP, 4));
    SEXP nm = allocVector(STRSXP, 4);
    setAttrib(ans, R_NamesSymbol, nm);
    SET_STRING_ELT(nm, 0, mkChar("call"));
    SET_STRING_ELT(nm, 1, mkChar("coefficients"));
    SET_STRING_ELT(nm, 2, mkChar("residuals"));
    SET_STRING_ELT(nm, 3, mkChar("fitted.values"));
    SET_VECTOR_ELT(ans, 0, call);
    SEXP coef = allocVector(REALSXP, 2);
    SET_VECTOR_ELT(ans, 1, coef);
    SEXP res = allocVector(REALSXP, n);
    SET_VECTOR_ELT(ans, 2, res);
    SEXP fit = allocVector(REALSXP, n);
    SET_VECTOR_ELT(ans, 3, fit);
    line(REAL(x), REAL(y), REAL(res), REAL(fit), n, REAL(coef));
    UNPROTECT(1);
    return ans;
}
