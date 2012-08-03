/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-8   The R Core Team.
 *
 *  Based on Applied Statistics algorithms AS181, R94
 *    (C) Royal Statistical Society 1982, 1995
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

/* swilk.f -- translated by f2c (version 19980913).
 * ------- and produced by f2c-clean,v 1.8 --- and hand polished: M.Maechler
 */
#include <Rmath.h>

#include "ctest.h"

#ifndef min
# define min(a, b)		((a) > (b) ? (b) : (a))
#endif

static double poly(const double *, int, double);

void
swilk(double *x, int *n, int *n1, double *w, double *pw, int *ifault)
{
    int nn2 = *n / 2;
    double a[nn2 + 1]; /* 1-based */

/*	ALGORITHM AS R94 APPL. STATIST. (1995) vol.44, no.4, 547-551.

	Calculates the Shapiro-Wilk W test and its significance level
*/

    double small = 1e-19;

    /* polynomial coefficients */
    double g[2] = { -2.273,.459 };
    double c1[6] = { 0.,.221157,-.147981,-2.07119, 4.434685, -2.706056 };
    double c2[6] = { 0.,.042981,-.293762,-1.752461,5.682633, -3.582633 };
    double c3[4] = { .544,-.39978,.025054,-6.714e-4 };
    double c4[4] = { 1.3822,-.77857,.062767,-.0020322 };
    double c5[4] = { -1.5861,-.31082,-.083751,.0038915 };
    double c6[3] = { -.4803,-.082676,.0030302 };
    double c7[2] = { .164,.533 };
    double c8[2] = { .1736,.315 };
    double c9[2] = { .256,-.00635 };

    /* System generated locals */
    double r__1;

/*
  Auxiliary routines : poly()  {below}
*/
    /* Local variables */
    int i, j, ncens, i1;

    double zbar, ssassx, summ2, ssumm2, gamma, delta, range;
    double a1, a2, an, bf, ld, m, s, sa, xi, sx, xx, y, w1;
    double fac, asa, an25, ssa, z90f, sax, zfm, z95f, zsd, z99f, rsn, ssx, xsx;

    *pw = 1.;
    if (*w >= 0.) *w = 1.;
    if (*n < 3) { *ifault = 1; return;}

    an = (double) (*n);
    if (*n1 < 3) {*ifault = 1; return;}
    ncens = *n - *n1;
    if (ncens < 0 || (ncens > 0 && *n < 20)) {*ifault = 4; return;}
    if (ncens > 0) {
	delta = (double) ncens / an;
	if (delta > 0.8) {*ifault = 5; return;}
    } /* just for -Wall:*/ else { delta = 0.; }

    if (*n == 3) {
	a[1] = 0.70710678;/* = sqrt(1/2) */
    } else {
	an25 = an + .25;
	summ2 = 0.;
	for (i = 1; i <= nn2; ++i) {
	    a[i] = qnorm((i - 0.375) / an25, 0., 1., 1, 0);
	    r__1 = a[i];
	    summ2 += r__1 * r__1;
	}
	summ2 *= 2.;
	ssumm2 = sqrt(summ2);
	rsn = 1. / sqrt(an);
	a1 = poly(c1, 6, rsn) - a[1] / ssumm2;

	/* Normalize a[] */
	if (*n > 5) {
	    i1 = 3;
	    a2 = -a[2] / ssumm2 + poly(c2, 6, rsn);
	    fac = sqrt((summ2 - 2. * (a[1] * a[1]) - 2. * (a[2] * a[2]))
		       / (1. - 2. * (a1 * a1) - 2. * (a2 * a2)));
	    a[2] = a2;
	} else {
	    i1 = 2;
	    fac = sqrt((summ2 - 2. * (a[1] * a[1])) /
		       ( 1.  - 2. * (a1 * a1)));
	}
	a[1] = a1;
	for (i = i1; i <= nn2; ++i) a[i] /= - fac;
    }

/*	If W is input as negative, calculate significance level of -W */

    if (*w < 0.) {
	w1 = 1. + *w;
	*ifault = 0;
	goto L70;
    }

/*	Check for zero range */

    range = x[*n1 - 1] - x[0];
    if (range < small) {*ifault = 6; return;}

/*	Check for correct sort order on range - scaled X */

    /* *ifault = 7; <-- a no-op, since it is changed below, in ANY CASE! */
    *ifault = 0;
    xx = x[0] / range;
    sx = xx;
    sa = -a[1];
    j = *n - 1;
    for (i = 1; i < *n1; --j) {
	xi = x[i] / range;
	if (xx - xi > small) {
	    /* Fortran had:	 print *, "ANYTHING"
	     * but do NOT; it *does* happen with sorted x (on Intel GNU/linux 32bit):
	     *  shapiro.test(c(-1.7, -1,-1,-.73,-.61,-.5,-.24, .45,.62,.81,1))
	     */
	    *ifault = 7;
	}
	sx += xi;
	++i;
	if (i != j)
	    sa += sign(i - j) * a[min(i,j)];
	xx = xi;
    }
    if (*n > 5000) *ifault = 2;

/*	Calculate W statistic as squared correlation
	between data and coefficients */

    sa /= *n1;
    sx /= *n1;
    ssa = ssx = sax = 0.;
    j = *n - 1;
    for (i = 0; i < *n1; ++i, --j) {
	if (i != j)
	    asa = sign(i - j) * a[1+min(i,j)] - sa;
	else
	    asa = -sa;
	xsx = x[i] / range - sx;
	ssa += asa * asa;
	ssx += xsx * xsx;
	sax += asa * xsx;
    }

/*	W1 equals (1-W) calculated to avoid excessive rounding error
	for W very near 1 (a potential problem in very large samples) */

    ssassx = sqrt(ssa * ssx);
    w1 = (ssassx - sax) * (ssassx + sax) / (ssa * ssx);
L70:
    *w = 1. - w1;

/*	Calculate significance level for W */

    if (*n == 3) {/* exact P value : */
	double pi6 = 1.90985931710274, /* = 6/pi */
	    stqr = 1.04719755119660; /* = asin(sqrt(3/4)) */
	*pw = pi6 * (asin(sqrt(*w)) - stqr);
	if(*pw < 0.) *pw = 0.;
	return;
    }
    y = log(w1);
    xx = log(an);
    if (*n <= 11) {
	gamma = poly(g, 2, an);
	if (y >= gamma) {
	    *pw = 1e-99;/* an "obvious" value, was 'small' which was 1e-19f */
	    return;
	}
	y = -log(gamma - y);
	m = poly(c3, 4, an);
	s = exp(poly(c4, 4, an));
    } else {/* n >= 12 */
	m = poly(c5, 4, xx);
	s = exp(poly(c6, 3, xx));
    }
    /*DBG printf("c(w1=%g, w=%g, y=%g, m=%g, s=%g)\n",w1,*w,y,m,s); */

    if (ncens > 0) {/* <==>  n > n1 --- not happening currently when called from R */

/*	Censoring by proportion NCENS/N.
	Calculate mean and sd of normal equivalent deviate of W. */

	double z90 = 1.2816, z95 = 1.6449, z99 = 2.3263, zm = 1.7509, 
	    zss = 0.56268, bf1 = 0.8378, xx90 = 0.556, xx95 = 0.622;

	ld = -log(delta);
	bf = 1. + xx * bf1;
	z90f = z90 + bf * pow(poly(c7, 2, pow(xx90, xx)), ld);
	z95f = z95 + bf * pow(poly(c8, 2, pow(xx95, xx)), ld);
	z99f = z99 + bf * pow(poly(c9, 2, xx), ld);

/*	Regress Z90F,...,Z99F on normal deviates Z90,...,Z99 to get
	pseudo-mean and pseudo-sd of z as the slope and intercept */

	zfm = (z90f + z95f + z99f) / 3.;
	zsd = (z90 * (z90f - zfm) +
	       z95 * (z95f - zfm) + z99 * (z99f - zfm)) / zss;
	zbar = zfm - zsd * zm;
	m += zbar * s;
	s *= zsd;
    }
    *pw = pnorm(y, m, s, 0/* upper tail */, 0);

    return;
} /* swilk */

static double poly(const double *cc, int nord, double x)
{
/* Algorithm AS 181.2	Appl. Statist.	(1982) Vol. 31, No. 2

	Calculates the algebraic polynomial of order nord-1 with
	array of coefficients cc.  Zero order coefficient is cc(1) = cc[0]
*/
    /* Local variables */
    int j;
    double p, ret_val;/* preserve precision! */

    ret_val = cc[0];
    if (nord > 1) {
	p = x * cc[nord-1];
	for (j = nord - 2; j > 0; j--) p = (p + cc[j]) * x;
	ret_val += p;
    }
    return ret_val;
} /* poly */
