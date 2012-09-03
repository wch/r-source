/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2012   Robert Gentleman, Ross Ihaka and the
 *			      R Core Team
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

#include <R_ext/Arith.h>
#include <R_ext/Error.h>
#include <R_ext/Applic.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) gettext (String)
#else
#define _(String) (String)
#endif

/* Linear and Step Function Interpolation */

/* Assumes that ordinates are in ascending order
 * The right interval is found by bisection
 * Linear/constant interpolation then takes place on that interval
*/

/* NB:  R_interv(.) in ./interv.c  is conceptually a special case of
 *	this, where y = 1:n */

typedef struct {
    double ylow;
    double yhigh;
    double f1;
    double f2;
    int kind;
} appr_meth;

static double approx1(double v, double *x, double *y, int n,
		      appr_meth *Meth)
{
    /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */
    int i, j, ij;

    if(!n) return R_NaN;

    i = 0;
    j = n - 1;

    /* handle out-of-domain points */
    if(v < x[i]) return Meth->ylow;
    if(v > x[j]) return Meth->yhigh;

    /* find the correct interval by bisection */
    while(i < j - 1) { /* x[i] <= v <= x[j] */
	ij = (i + j)/2; /* i+1 <= ij <= j-1 */
	if(v < x[ij]) j = ij; else i = ij;
	/* still i < j */
    }
    /* provably have i == j-1 */

    /* interpolation */

    if(v == x[j]) return y[j];
    if(v == x[i]) return y[i];
    /* impossible: if(x[j] == x[i]) return y[i]; */

    if(Meth->kind == 1) /* linear */
	return y[i] + (y[j] - y[i]) * ((v - x[i])/(x[j] - x[i]));
    else /* 2 : constant */
	return y[i] * Meth->f1 + y[j] * Meth->f2;
}/* approx1() */


/* Testing done only once - in a separate function */
static void 
R_approxtest(double *x, double *y, int nxy, int method, double f)
{
    int i;

    switch(method) {
    case 1: /* linear */
      	break;
    case 2: /* constant */
	if(!R_FINITE(f) || f < 0 || f > 1)
	    error(_("approx(): invalid f value"));
	break;
    default:
	error(_("approx(): invalid interpolation method"));
	break;
    }
    /* check interpolation method */
    for(i = 0; i < nxy; i++)
	if(ISNA(x[i]) || ISNA(y[i]))
	    error(_("approx(): attempted to interpolate NA values"));
}

/* R Frontend for Linear and Constant Interpolation, no testing */

static void 
R_approxfun(double *x, double *y, int nxy, double *xout, double *yout,
	    int nout, int method, double yleft, double yright, double f)
{
    int i;
    appr_meth M = {0.0, 0.0, 0.0, 0.0, 0}; /* -Wall */

    M.f2 = f;
    M.f1 = 1 - f;
    M.kind = method;
    M.ylow = yleft;
    M.yhigh = yright;
    for(i = 0; i < nout; i++)
	if(!ISNA(xout[i])) yout[i] = approx1(xout[i], x, y, nxy, &M);
	else yout[i] = xout[i];
}

#include <Rinternals.h>
#include "statsR.h"
SEXP ApproxTest(SEXP x, SEXP y, SEXP method, SEXP sf)
{
    int nx = LENGTH(x), m = asInteger(method);
    double f = asReal(sf);
    R_approxtest(REAL(x), REAL(y), nx, m, f);
    return R_NilValue;
}

SEXP Approx(SEXP x, SEXP y, SEXP v, SEXP method, 
	    SEXP yleft, SEXP yright, SEXP sf)
{
    SEXP xout = PROTECT(coerceVector(v, REALSXP));
    int nx = LENGTH(x), nout = LENGTH(xout), m = asInteger(method);
    double yl = asReal(yleft), yr = asReal(yright), f = asReal(sf);
    SEXP yout = PROTECT(allocVector(REALSXP, nout));
    R_approxfun(REAL(x), REAL(y), nx, REAL(xout), REAL(yout), nout, 
		m, yl, yr, f);
    UNPROTECT(2);
    return yout;
}
