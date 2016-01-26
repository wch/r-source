/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2016   The R Core Team.
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

/* Kendall's rank correlation tau and its exact distribution in case of no ties
*/

#include <string.h>
#include <R.h>
#include <math.h> // for floor
#include <Rmath.h>

/*
   and the exact distribution of  T = (n * (n - 1) * tau + 1) / 4,
   which is -- if there are no ties -- the number of concordant ordered pairs
*/

static double
ckendall(int k, int n, double **w) 
{
    int i, u;
    double s;

    u =  (n * (n - 1) / 2);
    if ((k < 0) || (k > u)) return(0);
    if (w[n] == 0) {
	w[n] = (double *) R_alloc(u + 1, sizeof(double));
	memset(w[n], '\0', sizeof(double) * (u+1));
	for (i = 0; i <= u; i++) w[n][i] = -1;
    }
    if (w[n][k] < 0) {
	if (n == 1)
	    w[n][k] = (k == 0);
	else {
	    s = 0;
	    for (i = 0; i < n; i++)
		s += ckendall(k - i, n - 1, w);
	    w[n][k] = s;
	}
    }
    return(w[n][k]);
}

#if 0
void
dkendall(int *len, double *x, int *n) 
{
    int i;
    double **w;

    w = (double **) R_alloc(*n + 1, sizeof(double *));

    for (i = 0; i < *len; i++)
	if (fabs(x[i] - floor(x[i] + 0.5)) > 1e-7) {
	    x[i] = 0;
	} else {
	    x[i] = ckendall((int)x[i], *n) / gammafn(*n + 1, w);
	}
}
#endif

static void
pkendall(int len, double *Q, double *P, int n) 
{
    int i, j;
    double p, q;
    double **w;

    w = (double **) R_alloc(n + 1, sizeof(double *));
    memset(w, '\0', sizeof(double*) * (n+1));

    for (i = 0; i < len; i++) {
	q = floor(Q[i] + 1e-7);
	if (q < 0)
	    P[i] = 0;
	else if (q > (n * (n - 1) / 2))
	    P[i] = 1;
	else {
	    p = 0;
	    for (j = 0; j <= q; j++) p += ckendall(j, n, w);
	    P[i] = p / gammafn(n + 1);
	}
    }
}

#include <Rinternals.h>
SEXP pKendall(SEXP q, SEXP sn)
{
    q = PROTECT(coerceVector(q, REALSXP));
    int len = LENGTH(q), n = asInteger(sn);
    SEXP p = PROTECT(allocVector(REALSXP, len));
    pkendall(len, REAL(q), REAL(p), n);
    UNPROTECT(2);
    return p;
}
