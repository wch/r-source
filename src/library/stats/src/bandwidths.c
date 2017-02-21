/*
 *  R : A Computer Language for Statistical Data Analysis
 *  bandwidth.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-2001
 *  Copyright (C) 2012-2017  The R Core Team
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

#include <stdlib.h> //abs
#include <math.h>
#include <Rmath.h> // M_* constants
#include <Rinternals.h>

// or include "stats.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

#define DELMAX 1000
/* Avoid slow and possibly error-producing underflows by cutting off at
   plus/minus sqrt(DELMAX) std deviations */
/* Formulae (6.67) and (6.69) of Scott (1992), the latter corrected. */

SEXP bw_ucv(SEXP sn, SEXP sd, SEXP cnt, SEXP sh)
{
    double h = asReal(sh), d = asReal(sd), sum = 0.0, term, u;
    int n = asInteger(sn), nbin = LENGTH(cnt);
    double *x = REAL(cnt);
    for (int i = 0; i < nbin; i++) {
	double delta = i * d / h;
	delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 4.) - sqrt(8.0) * exp(-delta / 2.);
	sum += term * x[i];
    }
    u = (0.5 + sum/n) / (n * h * M_SQRT_PI);
    // = 1 / (2 * n * h * sqrt(PI)) + sum / (n * n * h * sqrt(PI));
    return ScalarReal(u);
}

SEXP bw_bcv(SEXP sn, SEXP sd, SEXP cnt, SEXP sh)
{
    double h = asReal(sh), d = asReal(sd), sum = 0.0, term, u;
    int n = asInteger(sn), nbin = LENGTH(cnt);
    double *x = REAL(cnt);

    sum = 0.0;
    for (int i = 0; i < nbin; i++) {
	double delta = i * d / h; delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 4) * (delta * delta - 12 * delta + 12);
	sum += term * x[i];
    }
    u = (1 + sum/(32.0*n)) / (2.0 * n * h * M_SQRT_PI);
    // = 1 / (2 * n * h * sqrt(PI)) + sum / (64 * n * n * h * sqrt(PI));
    return ScalarReal(u);
}

SEXP bw_phi4(SEXP sn, SEXP sd, SEXP cnt, SEXP sh)
{
    double h = asReal(sh), d = asReal(sd), sum = 0.0, term, u;
    int n = asInteger(sn), nbin = LENGTH(cnt);
    double *x = REAL(cnt);

    for (int i = 0; i < nbin; i++) {
	double delta = i * d / h; delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 2.) * (delta * delta - 6. * delta + 3.);
	sum += term * x[i];
    }
    sum = 2. * sum + n * 3.;	/* add in diagonal */
    u = sum / ((double)n * (n - 1) * pow(h, 5.0)) * M_1_SQRT_2PI;
    // = sum / (n * (n - 1) * pow(h, 5.0) * sqrt(2 * PI));
    return ScalarReal(u);
}

SEXP bw_phi6(SEXP sn, SEXP sd, SEXP cnt, SEXP sh)
{
    double h = asReal(sh), d = asReal(sd), sum = 0.0, term, u;
    int n = asInteger(sn), nbin = LENGTH(cnt);
    double *x = REAL(cnt);

    for (int i = 0; i < nbin; i++) {
	double delta = i * d / h; delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 2) *
	    (delta * delta * delta - 15 * delta * delta + 45 * delta - 15);
	sum += term * x[i];
    }
    sum = 2. * sum - 15. * n;	/* add in diagonal */
    u = sum / ((double)n * (n - 1) * pow(h, 7.0)) * M_1_SQRT_2PI;
    // = sum / (n * (n - 1) * pow(h, 7.0) * sqrt(2 * PI));
    return ScalarReal(u);
}

/*
   Use double cnt as from R 3.4.0, as counts can exceed INT_MAX for
   large n (65537 in the worse case but typically not at n = 1 million
   for a smooth distribution -- and this is by default no longer used
   for n > 500).
*/

SEXP bw_den(SEXP nbin, SEXP sx)
{
    int nb = asInteger(nbin), n = LENGTH(sx);
    double xmin, xmax, rang, dd, *x = REAL(sx);

    xmin = R_PosInf; xmax = R_NegInf;
    for (int i = 0; i < n; i++) {
	if(!R_FINITE(x[i]))
	    error(_("non-finite x[%d] in bandwidth calculation"), i+1);
	if(x[i] < xmin) xmin = x[i];
	if(x[i] > xmax) xmax = x[i];
    }
    rang = (xmax - xmin) * 1.01;
    dd = rang / nb;

    SEXP ans = PROTECT(allocVector(VECSXP, 2)),
	sc = SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, nb));
    SET_VECTOR_ELT(ans, 0, ScalarReal(dd));
    double *cnt = REAL(sc);
    for (int i = 0; i < nb; i++) cnt[i] = 0.0;

    for (int i = 1; i < n; i++) {
	int ii = (int)(x[i] / dd);
	for (int j = 0; j < i; j++) {
	    int jj = (int)(x[j] / dd);
	    cnt[abs(ii - jj)] += 1.0;
	}
    }

    UNPROTECT(1);
    return ans;
}

/* Input: counts for nb bins */
SEXP bw_den_binned(SEXP sx)
{
    int nb = LENGTH(sx);
    int *x = INTEGER(sx);

    SEXP ans = PROTECT(allocVector(REALSXP, nb));
    double *cnt = REAL(ans);
    for (int ib = 0; ib < nb; ib++) cnt[ib] = 0.0;

    for (int ii = 0; ii < nb; ii++) {
	int w = x[ii];
	cnt[0] += w*(w-1.); // don't count distances to self
	for (int jj = 0; jj < ii; jj++)
	    cnt[ii - jj] += w * x[jj];
    }
    cnt[0] *= 0.5; // counts in the same bin got double-counted

    UNPROTECT(1);
    return ans;
}
