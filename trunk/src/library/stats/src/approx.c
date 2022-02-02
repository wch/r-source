/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2019   The R Core Team
 *  Copyright (C) 1995, 1996   Robert Gentleman and Ross Ihaka
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h>
#include <R_ext/Error.h>
#include <R_ext/Applic.h>
#include <Rinternals.h> // for R_xlen_t
#ifdef DEBUG_approx
# include <R_ext/Print.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

/* Linear and Step Function Interpolation */

/* Assumes that ordinates are in ascending order
 * The right interval is found by bisection
 * Linear/constant interpolation then takes place on that interval
*/

/* NB:  interv(.) in ../../../appl/interv.c
        ------    is conceptually a special case of this, where y = 1:n */

typedef struct {
    double ylow;
    double yhigh;
    double f1;
    double f2;
    int kind;
    int na_rm;
} appr_meth;

static double approx1(double v, double *x, double *y, R_xlen_t n,
		      appr_meth *Meth)
{
    /* Approximate  y(v),  given (x,y)[i], i = 0,..,n-1 */

#ifdef DEBUG_approx
    REprintf("approx1(*, n = %.0f, Meth:=(f1=%g,f2=%g, kind=%d)):\n",
	     (double)n, Meth->f1, Meth->f2, Meth->kind);
#endif

    if(!n) return R_NaN;

    R_xlen_t
	i = 0,
	j = n - 1;
    /* handle out-of-domain points */
    if(v < x[i]) return Meth->ylow;
    if(v > x[j]) return Meth->yhigh;


    /* find the correct interval by bisection */
    while(i < j - 1) { /* x[i] <= v <= x[j] */
	R_xlen_t ij = (i+j) / 2;
	/* i+1 <= ij <= j-1 */
	if(v < x[ij]) j = ij; else i = ij;
	/* still i < j */
#ifdef DEBUG_approx
	REprintf("  (i,j) = (%.0f,%.0f)\n", (double)i, (double)j);
#endif
    }
    /* provably have i == j-1 */

    /* interpolation */

    if(v == x[j]) return y[j];
    if(v == x[i]) return y[i];
    /* impossible: if(x[j] == x[i]) return y[i]; */

    if(Meth->kind == 1) /* linear */
	return y[i] + (y[j] - y[i]) * ((v - x[i])/(x[j] - x[i]));
    else /* 2 : constant */
	return (Meth->f1 != 0.0 ? y[i] * Meth->f1 : 0.0)
	     + (Meth->f2 != 0.0 ? y[j] * Meth->f2 : 0.0);
}/* approx1() */


/* Testing done only once - in a separate function */
static void
R_approxtest(double *x, double *y, R_xlen_t nxy, int method, double f, int na_rm)
{
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
    if(na_rm) { // (x,y) should not have any NA's anymore
	for(R_xlen_t i = 0; i < nxy; i++)
	    if(ISNAN(x[i]) || ISNAN(y[i]))
		error(_("approx(): attempted to interpolate NA values"));
    } else { // na.rm = FALSE ==> at least y may contain NA's
	for(R_xlen_t i = 0; i < nxy; i++)
	    if(ISNAN(x[i]))
		error(_("approx(x,y, .., na.rm=FALSE): NA values in x are not allowed"));
    }
}

/* R Frontend for Linear and Constant Interpolation, no testing */

static void
R_approxfun(double *x, double *y, R_xlen_t nxy, double *xout, double *yout,
	    R_xlen_t nout, int method, double yleft, double yright, double f, int na_rm)
{
    appr_meth M = {0.0, 0.0, 0.0, 0.0, 0}; /* -Wall */

    M.f2 = f;
    M.f1 = 1 - f;
    M.kind = method;
    M.ylow = yleft;
    M.yhigh = yright;
    M.na_rm = na_rm;
#ifdef DEBUG_approx
    REprintf("R_approxfun(x,y, nxy = %.0f, .., nout = %.0f, method = %d, ...)",
	     (double)nxy, (double)nout, Meth->kind);
#endif
    for(R_xlen_t i = 0; i < nout; i++)
	yout[i] = ISNAN(xout[i]) ? xout[i] : approx1(xout[i], x, y, nxy, &M);
}

#include <Rinternals.h>
#include "statsR.h"
SEXP ApproxTest(SEXP x, SEXP y, SEXP method, SEXP f, SEXP na_rm)
{
    R_xlen_t nx = XLENGTH(x);
    R_approxtest(REAL(x), REAL(y), nx,
		 asInteger(method), asReal(f), asLogical(na_rm));
    // if no error was signalled,
    return R_NilValue;
}

SEXP Approx(SEXP x, SEXP y, SEXP v, SEXP method,
	    SEXP yleft, SEXP yright, SEXP f, SEXP na_rm)
{
    SEXP xout = PROTECT(coerceVector(v, REALSXP));
    R_xlen_t nx = XLENGTH(x), nout = XLENGTH(xout);
    SEXP yout = PROTECT(allocVector(REALSXP, nout));
    R_approxfun(REAL(x), REAL(y), nx, REAL(xout), REAL(yout), nout,
		asInteger(method), asReal(yleft), asReal(yright), asReal(f), asLogical(na_rm));
    UNPROTECT(2);
    return yout;
}
