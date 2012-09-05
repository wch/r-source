/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-1   The R Core Team.
 *
 *  Based on Applied Statistics algorithm AS177
 *    (C) Royal Statistical Society 1982
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


/* nscor.f -- translated by f2c (version 19980913).
 * ------- and produced by f2c-clean,v 1.8 --- and hand polished: M.Maechler
 */

#include <Rmath.h>

#define nstep 721	/* = nrow(work) */

void nscor1(double *s, int *n, int *n2,
	    double *work, int *ifault)
{
/*	Algorithm AS 177   Appl. Statist. (1982) Vol. 31, No. 2, 161-165

	Exact calculation of Normal Scores
*/

    /* Initialized data */

    const double hh = .025;

    /* Local variables */
    double scor, c, an, ai1, ani;
    int i, j, i1, ni;

    /* Parameter adjustments */
    --s;
    work -= 5;

    /* Function Body */

    if (*n2 != *n / 2) {
	*ifault = 3;	return;
    }
    if (*n <= 1) {
	*ifault = 1;	return;
    }
    *ifault = 0;
    if (*n > 2000) {
	*ifault = 2;/* non-fatal potential accuracy loss */
    }

    an = (double) (*n);
    c = log(an);

/*	Accumulate ordinates for calculation of integral for rankits */

    for (i = 1; i <= *n2; ++i) {
	i1 = i - 1;
	ni = *n - i;
	ai1 = (double) i1;
	ani = (double) ni;
	scor = 0.;
	for (j = 1; j <= nstep; ++j) {
	    scor += exp(      work[(j << 2) + 2] + ai1 * work[(j << 2) + 3] +
			ani * work[(j << 2) + 4] + c)  * work[(j << 2) + 1];
	}
	s[i] = scor * hh;
	c += log(ani / (double) i);
    }
    return;
} /* nscor1 */



void init(double *work)
{
/*	Initialize the work[]  table for nscor1(.).

	Algorithm AS 177.1   Appl. Statist. (1982) Vol. 31, No. 2

	Now calling R's pnorm() instead of
	external alnorm { = Appl.Stat. AS 66 }
*/

    const double xstart = -9.;
    const double hh = .025;
    const double pi2 = -.918938533;
    const double half = .5;

    int i;
    double xx;

    /*	Parameter adjustments */
    work -= 5;

    xx = xstart;

/*	Set up arrays for calculation of integral */

    for (i = 1; i <= nstep; ++i) {
	work[(i << 2) + 1] = xx;
	work[(i << 2) + 2] = pi2 - xx * xx * half;
	/* upper & lower tail */
	/* had  log(alnorm_(&xx, UPPER)) & log(alnorm_(&xx, LOWER)) :*/
	work[(i << 2) + 3] = pnorm(xx,0.,1., 0, /*log_p = */1);
	work[(i << 2) + 4] = pnorm(xx,0.,1., 1, /*log_p = */1);
	xx = xstart + (double) i * hh;
    }
    return;
} /* init */


static double correc(int, int);

void nscor2(float *s, int *n, int *n2, int *ier)
{

/*     algorithm as 177.3, applied statistics, v.31, 161-165, 1982.

     calculates approximate expected values of normal order statistics.
     claimed accuracy is 0.0001, though usually accurate to 5-6 dec.

 ***  N.B. This routine is NOT in double precision ***

     Arguments:

     s(n2)   = output, the first n2 expected values.
     n	     = input, the sample size.
     n2	     = input, the number of order statistics required; must
		      be <= n/2.
     ier     = output, error indicator
		   = 0 if no error detected
		   = 1 if n <= 1.
		   = 2 if n > 2000, in which case the order statistics
			  are still calculated, but may be inaccurate.
		   = 3 if n2 > n/2 (n.b. this differs from the
			  published algorithm which returns an error
			  if n2 is not equal to n/2.)

     Calls qnorm() [from R] which is an improvement of
     ppnd = applied statistics algorithm 111.
     An alternative is ppnd7 in algorithm AS 241.
*/

    /* Initialized data */

    const float
	eps[4] = { .419885f,.450536f, .456936f, .468488f },
	dl1[4] = { .112063f,.12177f,  .239299f, .215159f },
	dl2[4] = { .080122f,.111348f,-.211867f,-.115049f },
	gam[4] = { .474798f,.469051f, .208597f, .259784f },
	lam[4] = { .282765f,.304856f,.407708f,.414093f };
    const float bb = -.283833f;
    const float d  = -.106136f;
    const float b1 = .5641896f;


    /* Local variables */
    int i, k;
    float e1, e2, ai, an;

    /* input parameter checks. */

    if (*n2 > *n / 2) {
	*ier = 3;	return;
    }
    if (*n <= 1) {
	*ier = 1;	return;
    }
    *ier = 0;
    if (*n > 2000) {
	*ier = 2;
    }
    s[0] = b1;
    if (*n == 2) {
	return;
    }

/*	calculate normal tail areas for first 3 order statistics. */

    an = (float) (*n);
    k = 3;
    if (*n2 < k)
	k = *n2;
    /* k := min(3, *n2) */
    for (i = 0; i < k; ++i) {
	ai = (float) i+1;
	e1 = (ai - eps[i]) / (an + gam[i]);
	e2 = pow((double) e1, (double) lam[i]);
	s[i] = e1 + e2 * (dl1[i] + e2 * dl2[i]) / an - correc(i+1, *n);
    }
    if (*n2 > k) {

/*	calculate normal areas for other cases. */

	for (i = 4-1; i < *n2; ++i) {
	    ai = (float) i+1;
	    e1 = (ai - eps[3]) / (an + gam[3]);
	    e2 = pow((double) e1, (double) lam[3] + bb / (ai + d));
	    s[i] = e1 + e2 * (dl1[3] + e2 * dl2[3]) / an - correc(i+1, *n);
	}
    }
/*	convert tail areas to normal deviates. */

    for (i = 0; i < *n2; ++i)
	s[i] = - qnorm(s[i], 0., 1., 1, 0);

    return;
} /* nscor2 */


static double correc(int i, int n)
{
/*	calculates correction for tail area of the i-th largest of n
	order statistics. */

    const float
	c1[7] = { 9.5f,28.7f,1.9f,0.f,-7.f,-6.2f,-1.6f },
	c2[7] = { -6195.f,-9569.f,-6728.f,-17614.f,-8278.f,-3570.f, 1075.f },
	c3[7] = { 93380.f,175160.f,410400.f,2157600.f,2.376e6f,
		  2.065e6f,2.065e6f };
    const float mic = 1e-6f;
    const float c14 = 1.9e-5f;

    double an;

    if (i * n == 4)		return c14;
    if (i < 1 || i > 7)		return 0;
    if (i != 4 && n > 20)	return 0;
    if (i == 4 && n > 40)	return 0;
    /* else : */
    an = (double) n;
    an = 1. / (an * an);
    i--;
    return((c1[i] + an * (c2[i] + an * c3[i])) * mic);
} /* correc */
