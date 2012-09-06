/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-10  The R Core Team
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
 *
 *  Quartz Quartz device module header file
 *
 */

#include <Rinternals.h>
#include <Rconfig.h>
#include <R_ext/Constants.h>
#include <float.h>
#include "stats.h"
#include "statsR.h"

static const double THRESH = 30.;
static const double MTHRESH = -30.;
static const double INVEPS = 1/DOUBLE_EPS;

/**
 * Evaluate x/(1 - x). An inline function is used so that x is
 * evaluated once only.
 *
 * @param x input in the range (0, 1)
 *
 * @return x/(1 - x)
 */
static R_INLINE double x_d_omx(double x) {
    if (x < 0 || x > 1)
	error(_("Value %g out of range (0, 1)"), x);
    return x/(1 - x);
}

/**
 * Evaluate x/(1 + x). An inline function is used so that x is
 * evaluated once only. [but inlining is optional!]
 *
 * @param x input
 *
 * @return x/(1 + x)
 */
static R_INLINE double x_d_opx(double x) {return x/(1 + x);}

SEXP logit_link(SEXP mu)
{
    int i, n = LENGTH(mu);
    SEXP ans = PROTECT(duplicate(mu));
    double *rans = REAL(ans), *rmu=REAL(mu);

    if (!n || !isReal(mu))
	error(_("Argument %s must be a nonempty numeric vector"), "mu");
    for (i = 0; i < n; i++)
	rans[i] = log(x_d_omx(rmu[i]));
    UNPROTECT(1);
    return ans;
}

SEXP logit_linkinv(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);
    double *rans = REAL(ans), *reta = REAL(eta);

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    for (i = 0; i < n; i++) {
	double etai = reta[i], tmp;
	tmp = (etai < MTHRESH) ? DOUBLE_EPS :
	    ((etai > THRESH) ? INVEPS : exp(etai));
	rans[i] = x_d_opx(tmp);
    }
    UNPROTECT(1);
    return ans;
}

SEXP logit_mu_eta(SEXP eta)
{
    SEXP ans = PROTECT(duplicate(eta));
    int i, n = LENGTH(eta);
    double *rans = REAL(ans), *reta = REAL(eta);

    if (!n || !isReal(eta))
	error(_("Argument %s must be a nonempty numeric vector"), "eta");
    for (i = 0; i < n; i++) {
	double etai = reta[i];
	double opexp = 1 + exp(etai);

	rans[i] = (etai > THRESH || etai < MTHRESH) ? DOUBLE_EPS :
	    exp(etai)/(opexp * opexp);
    }
    UNPROTECT(1);
    return ans;
}

static R_INLINE
double y_log_y(double y, double mu)
{
    return (y) ? (y * log(y/mu)) : 0;
}

SEXP binomial_dev_resids(SEXP y, SEXP mu, SEXP wt)
{
    int i, n = LENGTH(y), lmu = LENGTH(mu), lwt = LENGTH(wt), nprot = 1;
    SEXP ans;
    double mui, yi, *rmu, *ry, *rwt, *rans;

    if (!isReal(y)) {y = PROTECT(coerceVector(y, REALSXP)); nprot++;}
    ry = REAL(y);
    ans = PROTECT(duplicate(y));
    rans = REAL(ans);
    if (!isReal(mu)) {mu = PROTECT(coerceVector(mu, REALSXP)); nprot++;}
    if (!isReal(wt)) {wt = PROTECT(coerceVector(wt, REALSXP)); nprot++;}
    rmu = REAL(mu);
    rwt = REAL(wt);
    if (lmu != n && lmu != 1)
	error(_("argument %s must be a numeric vector of length 1 or length %d"),
	      "mu", n);
    if (lwt != n && lwt != 1)
	error(_("argument %s must be a numeric vector of length 1 or length %d"),
	      "wt", n);
    /* Written separately to avoid an optimization bug on Solaris cc */
    if(lmu > 1) {
	for (i = 0; i < n; i++) {
	    mui = rmu[i];
	    yi = ry[i];
	    rans[i] = 2 * rwt[lwt > 1 ? i : 0] *
		(y_log_y(yi, mui) + y_log_y(1 - yi, 1 - mui));
	}
    } else {
	mui = rmu[0];
	for (i = 0; i < n; i++) {
	    yi = ry[i];
	    rans[i] = 2 * rwt[lwt > 1 ? i : 0] *
		(y_log_y(yi, mui) + y_log_y(1 - yi, 1 - mui));
	}
    }

    UNPROTECT(nprot);
    return ans;
}
