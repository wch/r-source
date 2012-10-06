/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005-12   The R Core Team
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

#ifndef R_STATS_H
#define R_STATS_H

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

void R_approx(double *, double *, int *, double *, int *,
	      int *, double *, double *, double *);
void R_approxfun(double *, double *, int *, double *, int *,
	      int *, double *, double *, double *);
void R_approxtest(double *, double *, int *, int *, double *);
void band_ucv_bin(int *, int *, double *, int *, double *, double *);
void band_bcv_bin(int *, int *, double *, int *, double *, double *);
void band_phi4_bin(int *, int *, double *, int *, double *, double *);
void band_phi6_bin(int *, int *, double *, int *, double *, double *);
void band_den_bin(int *, int *, double *, int *, double *, double *);
void loglin(int *nvar, int *dim, int *ncon, int *config, int *ntab,
	    double *table, double *fit, int *locmar, int *nmar, double *marg,
	    int *nu, double *u, double *maxdev, int *maxit,
	    double *dev, int *nlast, int *ifault);
void lowess(double *x, double *y, int *n,
	    double *f, int *nsteps, double *delta,
	    double *ys, double *rw, double *res);
void spline_coef(int *method, int *n, double *x, double *y,
		 double *b, double *c, double *d, double *e);
void spline_eval(int *method, int *nu, double *u, double *v,
		 int *n, double *x, double *y,
		 double *b, double *c, double *d);

void F77_NAME(lminfl)(double *x, int *ldx, int *n, int *k, int *docoef,
		      double *qraux, double *resid, double *hat,
		      double *coef, double *sigma, double *tol);
#endif
