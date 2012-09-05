/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-12   The R Core Team.
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

/* for mantelhaen.test */

#include <R.h>
#include <Rmath.h>

static void
int_d2x2xk(int K, double *m, double *n, double *t, double *d)
{
    int i, j, l, w, y, z;
    double u, **c;

    c = (double **) R_alloc(K + 1, sizeof(double *));
    l = y = z = 0;
    c[0] = (double *) R_alloc(1, sizeof(double));
    c[0][0] = 1;
    for(i = 0; i < K; i++) {
	y = imax2(0,  (int)(*t - *n));
	z = imin2((int)*m, (int)*t);
	c[i + 1] = (double *) R_alloc(l + z - y + 1, sizeof(double));
	for(j = 0; j <= l + z - y; j++) c[i + 1][j] = 0;
	for(j = 0; j <= z - y; j++) {
	    u = dhyper(j + y, *m, *n, *t, FALSE);
	    for(w = 0; w <= l; w++) c[i + 1][w + j] += c[i][w] * u;
	}
	l = l + z - y;
	m++; n++; t++;
    }

    u = 0;
    for(j = 0; j <= l; j++) u += c[K][j];
    for(j = 0; j <= l; j++) d[j] = c[K][j] / u;
}

#include <Rinternals.h>

SEXP d2x2xk(SEXP sK, SEXP m, SEXP n, SEXP t, SEXP srn)
{
    int K = asInteger(sK), rn = asInteger(srn);
    m = PROTECT(coerceVector(m, REALSXP));
    n = PROTECT(coerceVector(n, REALSXP));
    t = PROTECT(coerceVector(t, REALSXP));
    SEXP ans = PROTECT(allocVector(REALSXP, rn));
    int_d2x2xk(K, REAL(m), REAL(n), REAL(t), REAL(ans));
    UNPROTECT(4);
    return ans;
}
