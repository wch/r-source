/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2017   The R Core Team.
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

#include <R_ext/Utils.h>	/* R_rsort() */
#include <math.h>

#include <Rinternals.h>
#include "statsR.h"

#ifdef DEBUG_tukeyline
# include <R_ext/Print.h>
#endif

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
		 int iter,
		 double coef[2])
{
    int i, j, k;
    double xb, x1, x2, xt, yt, yb, tmp1, tmp2;

    for(i = 0 ; i < n ; i++) {
	z[i] = x[i];
	w[i] = y[i];
    }
    R_rsort(z, n);/* z = ordered abscissae */

    // All x-space computations can (and hence should) happen outside the iterations

    /* NB: Using quantiles is *not* quite correct (not following the reference).
           Rather need 3 groups with group sizes depending on n and (n % 3)
    */

    // x1 := quantile(x, 1/3) :
    tmp1 = z[il(n, 1./3.)];
    tmp2 = z[iu(n, 1./3.)];	x1 = 0.5*(tmp1+tmp2);

    // x1 := quantile(x, 2/3) :
    tmp1 = z[il(n, 2./3.)];
    tmp2 = z[iu(n, 2./3.)];	x2 = 0.5*(tmp1+tmp2);

    // xb := x_L := Median(x[i]; x[i] <= quantile(x, 1/3))
    for(i = 0, k = 0; i < n; i++)
	if(x[i] <= x1)
	    z[k++] = x[i];
    R_rsort(z, k); xb = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);

    // xt := x_R := Median(x[i]; x[i] >= quantile(x, 2/3))
    for(i = 0, k = 0; i < n; i++)
	if(x[i] >= x2)
	    z[k++] = x[i];
    R_rsort(z, k); xt = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);


#ifdef DEBUG_tukeyline
    REprintf("tukey line(*, n = %d): xL=xb=%g, x_1=%g, x_2=%g, xR=xt=%g\n",
	     n, xb, x1, x2, xt);
#endif

    double slope = 0.;
    // "Polishing" iterations (incl first estimate) :
    for(j = 1; j <= iter; j++) { // w[] = "current y"

	/* yb := Median(y[i]; x[i] <= quantile(x, 1/3) */
	for(i = 0, k = 0; i < n; i++)
	    if(x[i] <= x1)
		z[k++] = w[i];
	R_rsort(z, k);
	yb = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);
#ifdef DEBUG_tukeyline
	REprintf("   Left 3rd: k=%d => (j1,j2)=(%d,%d) =>  yL=yb=%g\n",
		 k, il(k, 0.5), iu(k, 0.5), yb);
#endif

	/* yt := Median(y[i]; x[i] >= quantile(x, 2/3) */
	for(i = 0, k = 0; i < n; i++)
	    if(x[i] >= x2)
		z[k++] = w[i];
	R_rsort(z,k);
	yt = 0.5 * (z[il(k, 0.5)] + z[iu(k, 0.5)]);
#ifdef DEBUG_tukeyline
	REprintf("  Right 3rd: k=%d => (j1,j2)=(%d,%d) =>  yR=yt=%g\n",
		 k, il(k, 0.5), iu(k, 0.5), yt);
#endif

	slope += (yt - yb)/(xt - xb);
	for(i = 0 ; i < n ; i++)
	    w[i] = y[i] - slope*x[i];
    } // iterations

    // intercept := median{ "residuals" } (= mean of the 3 median residuals)
    R_rsort(w,n);
    double yint = 0.5 * (w[il(n, 0.5)] + w[iu(n, 0.5)]);

    for( i = 0 ; i < n ; i++ ) {
	w[i] = yint + slope*x[i];
	z[i] = y[i] - w[i];
    }
    coef[0] = yint;
    coef[1] = slope;
}

// where is this used?
void tukeyline0(double *x, double *y, double *z, double *w, int *n,
		double *coef)
{
    line(x, y, z, w, *n, 1, coef);
}

SEXP tukeyline(SEXP x, SEXP y, SEXP iter, SEXP call)
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
    line(REAL(x), REAL(y), REAL(res), REAL(fit), n, asInteger(iter), REAL(coef));
    UNPROTECT(1);
    return ans;
}
