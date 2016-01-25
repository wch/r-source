/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1999-2015   The R Core Team
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
 *  https://www.R-project.org/Licenses/.
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>
#include <math.h>

#include <R.h>
#include "ts.h"

#ifndef min
#define min(a, b) ((a < b)?(a):(b))
#define max(a, b) ((a < b)?(b):(a))
#endif

// currently ISNAN includes NAs
#define my_isok(x) (!ISNA(x) & !ISNAN(x))

SEXP cfilter(SEXP sx, SEXP sfilter, SEXP ssides, SEXP scircular)
{
   if (TYPEOF(sx) != REALSXP || TYPEOF(sfilter) != REALSXP)
       error("invalid input");
    R_xlen_t nx = XLENGTH(sx), nf = XLENGTH(sfilter);
    int sides = asInteger(ssides), circular = asLogical(scircular);
    if(sides == NA_INTEGER || circular == NA_LOGICAL)  error("invalid input");

    SEXP ans = allocVector(REALSXP, nx);

    R_xlen_t i, j, nshift;
    double z, tmp, *x = REAL(sx), *filter = REAL(sfilter), *out = REAL(ans);

    if(sides == 2) nshift = nf /2; else nshift = 0;
    if(!circular) {
	for(i = 0; i < nx; i++) {
	    z = 0;
	    if(i + nshift - (nf - 1) < 0 || i + nshift >= nx) {
		out[i] = NA_REAL;
		continue;
	    }
	    for(j = max(0, nshift + i - nx); j < min(nf, i + nshift + 1) ; j++) {
		tmp = x[i + nshift - j];
		if(my_isok(tmp)) z += filter[j] * tmp;
		else { out[i] = NA_REAL; goto bad; }
	    }
	    out[i] = z;
	bad:
	    continue;
	}
    } else { /* circular */
	for(i = 0; i < nx; i++)
	{
	    z = 0;
	    for(j = 0; j < nf; j++) {
		R_xlen_t ii = i + nshift - j;
		if(ii < 0) ii += nx;
		if(ii >= nx) ii -= nx;
		tmp = x[ii];
		if(my_isok(tmp)) z += filter[j] * tmp;
		else { out[i] = NA_REAL; goto bad2; }
	    }
	    out[i] = z;
	bad2:
	    continue;
	}
    }
    return ans;
}

/* recursive filtering */
SEXP rfilter(SEXP x, SEXP filter, SEXP out)
{
   if (TYPEOF(x) != REALSXP || TYPEOF(filter) != REALSXP
       || TYPEOF(out) != REALSXP) error("invalid input");
    R_xlen_t nx = XLENGTH(x), nf = XLENGTH(filter);
    double sum, tmp, *r = REAL(out), *rx = REAL(x), *rf = REAL(filter);

    for(R_xlen_t i = 0; i < nx; i++) {
	sum = rx[i];
	for (R_xlen_t j = 0; j < nf; j++) {
	    tmp = r[nf + i - j - 1];
	    if(my_isok(tmp)) sum += tmp * rf[j];
	    else { r[nf + i] = NA_REAL; goto bad3; }
	}
	r[nf + i] = sum;
    bad3:
	continue;
    }
    return out;
}

/* now allows missing values */
static void
acf0(double *x, int n, int ns, int nl, Rboolean correlation, double *acf)
{
    int d1 = nl+1, d2 = ns*d1;

    for(int u = 0; u < ns; u++)
	for(int v = 0; v < ns; v++)
	    for(int lag = 0; lag <= nl; lag++) {
		double sum = 0.0; int nu = 0;
		for(int i = 0; i < n-lag; i++)
		    if(!ISNAN(x[i + lag + n*u]) && !ISNAN(x[i + n*v])) {
			nu++;
			sum += x[i + lag + n*u] * x[i + n*v];
		    }
		acf[lag + d1*u + d2*v] = (nu > 0) ? sum/(nu + lag) : NA_REAL;
	    }
    if(correlation) {
	if(n == 1) {
	    for(int u = 0; u < ns; u++)
		acf[0 + d1*u + d2*u] = 1.0;
	} else {
	    double *se = (double *) R_alloc(ns, sizeof(double));
	    for(int u = 0; u < ns; u++)
		se[u] = sqrt(acf[0 + d1*u + d2*u]);
	    for(int u = 0; u < ns; u++)
		for(int v = 0; v < ns; v++)
		    for(int lag = 0; lag <= nl; lag++) { // ensure correlations remain in  [-1,1] :
			double a = acf[lag + d1*u + d2*v] / (se[u]*se[v]);
			acf[lag + d1*u + d2*v] = (a > 1.) ? 1. : ((a < -1.) ? -1. : a);
		    }
	}
    }
}

SEXP acf(SEXP x, SEXP lmax, SEXP sCor)
{
    int nx = nrows(x), ns = ncols(x), lagmax = asInteger(lmax),
	cor = asLogical(sCor);
    x = PROTECT(coerceVector(x, REALSXP));
    SEXP ans = PROTECT(allocVector(REALSXP, (lagmax + 1)*ns*ns));
    acf0(REAL(x), nx, ns, lagmax, cor, REAL(ans));
    SEXP d = PROTECT(allocVector(INTSXP, 3));
    INTEGER(d)[0] = lagmax + 1;
    INTEGER(d)[1] = INTEGER(d)[2] = ns;
    setAttrib(ans, R_DimSymbol, d);
    UNPROTECT(3);
    return ans;
}
