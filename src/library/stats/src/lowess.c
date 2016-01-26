/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2016 The R Core Team
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
# include <config.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

#include <math.h>
#include <Rmath.h>		/* fmax2, imin2, imax2 */
#include <R_ext/Applic.h>	/* prototypes for lowess and clowess */
#include <R_ext/Boolean.h>
#include <R_ext/Utils.h>	/* rPsort() */
#ifdef DEBUG_lowess
# include <R_ext/Print.h>
#endif

static R_INLINE double fsquare(double x)
{
    return x * x;
}

static R_INLINE double fcube(double x)
{
    return x * x * x;
}

static void lowest(double *x, double *y, int n, double *xs, double *ys,
	int nleft, int nright, double *w,
	Rboolean userw, double *rw, Rboolean *ok)
{
    int nrt, j;
    double a, b, c, h, h1, h9, r, range;

    x--;
    y--;
    w--;
    rw--;

    range = x[n]-x[1];
    h = fmax2(*xs-x[nleft], x[nright]-*xs);
    h9 = 0.999*h;
    h1 = 0.001*h;

    /* sum of weights */

    a = 0.;
    j = nleft;
    while (j <= n) {

	/* compute weights */
	/* (pick up all ties on right) */

	w[j] = 0.;
	r = fabs(x[j] - *xs);
	if (r <= h9) {
	    if (r <= h1)
		w[j] = 1.;
	    else
		w[j] = fcube(1.-fcube(r/h));
	    if (userw)
		w[j] *= rw[j];
	    a += w[j];
	}
	else if (x[j] > *xs)
	    break;
	j = j+1;
    }

    /* rightmost pt (may be greater */
    /* than nright because of ties) */

    nrt = j-1;
    if (a <= 0.)
	*ok = FALSE;
    else {
	*ok = TRUE;

	/* weighted least squares */
	/* make sum of w[j] == 1 */

	for(j=nleft ; j<=nrt ; j++)
	    w[j] /= a;
	if (h > 0.) {
	    a = 0.;

	    /*  use linear fit */
	    /* weighted center of x values */

	    for(j=nleft ; j<=nrt ; j++)
		a += w[j] * x[j];
	    b = *xs - a;
	    c = 0.;
	    for(j=nleft ; j<=nrt ; j++)
		c += w[j]*fsquare(x[j]-a);
	    if (sqrt(c) > 0.001*range) {
		b /= c;

		/* points are spread out */
		/* enough to compute slope */

		for(j=nleft; j <= nrt; j++)
		    w[j] *= (b*(x[j]-a) + 1.);
	    }
	}
	*ys = 0.;
	for(j=nleft; j <= nrt; j++)
	    *ys += w[j] * y[j];
    }
}

static
void clowess(double *x, double *y, int n,
	     double f, int nsteps, double delta,
	     double *ys, double *rw, double *res)
{
    int i, iter, j, last, m1, m2, nleft, nright, ns;
    Rboolean ok;
    double alpha, c1, c9, cmad, cut, d1, d2, denom, r, sc;

    if (n < 2) {
	ys[0] = y[0]; return;
    }

    /* nleft, nright, last, etc. must all be shifted to get rid of these: */
    x--;
    y--;
    ys--;


    /* at least two, at most n points */
    ns = imax2(2, imin2(n, (int)(f*n + 1e-7)));
#ifdef DEBUG_lowess
    REprintf("lowess(): ns = %d\n", ns);
#endif

    /* robustness iterations */

    iter = 1;
    while (iter <= nsteps+1) {
	nleft = 1;
	nright = ns;
	last = 0;	/* index of prev estimated point */
	i = 1;		/* index of current point */

	for(;;) {
	    if (nright < n) {

		/* move nleft,  nright to right */
		/* if radius decreases */

		d1 = x[i] - x[nleft];
		d2 = x[nright+1] - x[i];

		/* if d1 <= d2 with */
		/* x[nright+1] == x[nright], */
		/* lowest fixes */

		if (d1 > d2) {

		    /* radius will not */
		    /* decrease by */
		    /* move right */

		    nleft++;
		    nright++;
		    continue;
		}
	    }

	    /* fitted value at x[i] */

	    lowest(&x[1], &y[1], n, &x[i], &ys[i],
		   nleft, nright, res, iter>1, rw, &ok);
	    if (!ok) ys[i] = y[i];

	    /* all weights zero */
	    /* copy over value (all rw==0) */

	    if (last < i-1) {
		denom = x[i]-x[last];

		/* skipped points -- interpolate */
		/* non-zero - proof? */

		for(j = last+1; j < i; j++) {
		    alpha = (x[j]-x[last])/denom;
		    ys[j] = alpha*ys[i] + (1.-alpha)*ys[last];
		}
	    }

	    /* last point actually estimated */
	    last = i;

	    /* x coord of close points */
	    cut = x[last]+delta;
	    for (i = last+1; i <= n; i++) {
		if (x[i] > cut)
		    break;
		if (x[i] == x[last]) {
		    ys[i] = ys[last];
		    last = i;
		}
	    }
	    i = imax2(last+1, i-1);
	    if (last >= n)
		break;
	}
	/* residuals */
	for(i = 0; i < n; i++)
	    res[i] = y[i+1] - ys[i+1];

	/* overall scale estimate */
	sc = 0.;
	for(i = 0; i < n; i++) sc += fabs(res[i]);
	sc /= n;

	/* compute robustness weights */
	/* except last time */

	if (iter > nsteps)
	    break;
	/* Note: The following code, biweight_{6 MAD|Ri|}
	   is also used in stl(), loess and several other places.
	   --> should provide API here (MM) */
	for(i = 0 ; i < n ; i++)
	    rw[i] = fabs(res[i]);

	/* Compute   cmad := 6 * median(rw[], n)  ---- */
	/* FIXME: We need C API in R for Median ! */
	m1 = n/2;
	/* partial sort, for m1 & m2 */
	rPsort(rw, n, m1);
	if(n % 2 == 0) {
	    m2 = n-m1-1;
	    rPsort(rw, n, m2);
	    cmad = 3.*(rw[m1]+rw[m2]);
	}
	else { /* n odd */
	    cmad = 6.*rw[m1];
	}
#ifdef DEBUG_lowess
	REprintf("   cmad = %12g\n", cmad);
#endif
	if(cmad < 1e-7 * sc) /* effectively zero */
	    break;
	c9 = 0.999*cmad;
	c1 = 0.001*cmad;
	for(i = 0 ; i < n ; i++) {
	    r = fabs(res[i]);
	    if (r <= c1)
		rw[i] = 1.;
	    else if (r <= c9)
		rw[i] = fsquare(1.-fsquare(r/cmad));
	    else
		rw[i] = 0.;
	}
	iter++;
    }
}

#include <Rinternals.h>
SEXP lowess(SEXP x, SEXP y, SEXP sf, SEXP siter, SEXP sdelta)
{
    if(TYPEOF(x) != REALSXP || TYPEOF(y) != REALSXP) error("invalid input");
    int nx = LENGTH(x);
    if (nx == NA_INTEGER || nx == 0) error("invalid input");
    double f = asReal(sf);
    if (!R_FINITE(f) || f <= 0) error(_("'f' must be finite and > 0"));
    int iter = asInteger(siter);
    if (iter == NA_INTEGER || iter < 0) 
	error(_("'iter' must be finite and >= 0"));
    double delta = asReal(sdelta), *rw, *res;
    if (!R_FINITE(delta) || delta < 0) 
	error(_("'delta' must be finite and > 0"));
    SEXP ans;
    PROTECT(ans = allocVector(REALSXP, nx));
    rw = (double *) R_alloc(nx, sizeof(double));
    res = (double *) R_alloc(nx, sizeof(double));
    clowess(REAL(x), REAL(y), nx, f, iter, delta, REAL(ans), rw, res);
    UNPROTECT(1);
    return ans;
}
