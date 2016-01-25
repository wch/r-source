/* Copyright (C) 1997-1999  Adrian Trapletti
   Copyright (C) 2012 The R Core Team

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, a copy is available at
   https://www.R-project.org/Licenses/
*/

#include <string.h>
#include <R.h>

static double R_pp_sum (double *u, int n, int l)
{
    double tmp1, tmp2;

    tmp1 = 0.0;
    for (int i = 1; i <= l; i++) {
	tmp2 = 0.0;
	for (int j = i; j < n; j++) tmp2 += u[j] * u[j-i];
	tmp2 *= 1.0 - i/(l + 1.0);
	tmp1 += tmp2;
    }
    return 2.0 * tmp1 / n;
}

#include <Rinternals.h>

SEXP pp_sum(SEXP u, SEXP sl)
{
    u = PROTECT(coerceVector(u, REALSXP));
    int n = LENGTH(u), l = asInteger(sl);
    double trm = R_pp_sum(REAL(u), n, l);
    UNPROTECT(1);
    return ScalarReal(trm);
}

SEXP intgrt_vec(SEXP x, SEXP xi, SEXP slag)
{
    x = PROTECT(coerceVector(x, REALSXP));
    xi = PROTECT(coerceVector(xi, REALSXP));
    int n = LENGTH(x), lag = asInteger(slag);
    SEXP ans = PROTECT(allocVector(REALSXP, n + lag));
    double *rx = REAL(x), *y = REAL(ans);
    Memzero(y, n + lag); Memcpy(y, REAL(xi), lag);
    for (int i = lag; i < lag + n; i++) y[i] = rx[i - lag] + y[i - lag];
    UNPROTECT(3);
    return ans;
}
