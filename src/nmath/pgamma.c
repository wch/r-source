/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double pgamma(double x, double a, double scale);
 *
 *  DESCRIPTION
 *
 *    This function computes the distribution function for the
 *    gamma distribution with shape parameter a and scale parameter
 *    scale.  This is also known as the incomplete gamma function.
 *    See Abramowitz and Stegun (6.5.1) for example.
 *
 *  NOTES
 *
 *    This function is an adaptation of Algorithm 239 from the
 *    Applied Statistics Series.  The algorithm is faster than
 *    those by W. Fullerton in the FNLIB library and also the
 *    TOMS 542 alorithm of W. Gautschi.  It provides comparable
 *    accuracy to those algorithms and is considerably simpler.
 *
 *  REFERENCES
 *
 *    Algorithm AS 239, Incomplete Gamma Function
 *    Applied Statistics 37, 1988.
 */

#include "Mathlib.h"

static const double
    third = 1.0 / 3.0,
    zero = 0.0,
    one = 1.0,
    two = 2.0,
    three = 3.0,
    nine = 9.0,
    xbig = 1.0e+8,
    oflo = 1.0e+37,
    plimit = 1000.0e0,
    elimit = -88.0e0;

double pgamma(double x, double p, double scale)
{
    double pn1, pn2, pn3, pn4, pn5, pn6, arg, c, rn, a, b, an;
    double sum;

    /* check that we have valid values for x and p */

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(p) || ISNAN(scale))
	return x + p + scale;
#endif
    if(p <= zero || scale <= zero) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    x = x / scale;
    if (x <= zero)
	return 0.0;

    /* use a normal approximation if p > plimit */

    if (p > plimit) {
	pn1 = sqrt(p) * three * (pow(x/p, third) + one / (p * nine) - one);
	return pnorm(pn1, 0.0, 1.0);
    }

    /* if x is extremely large compared to p then return 1 */

    if (x > xbig)
	return one;

    if (x <= one || x < p) {

	/* use pearson's series expansion. */

	arg = p * log(x) - x - lgammafn(p + one);
	c = one;
	sum = one;
	a = p;
	do {
	    a = a + one;
	    c = c * x / a;
	    sum = sum + c;
	} while (c > DBL_EPSILON);
	arg = arg + log(sum);
	sum = zero;
	if (arg >= elimit)
	    sum = exp(arg);
    } else {

	/* use a continued fraction expansion */

	arg = p * log(x) - x - lgammafn(p);
	a = one - p;
	b = a + x + one;
	c = zero;
	pn1 = one;
	pn2 = x;
	pn3 = x + one;
	pn4 = x * b;
	sum = pn3 / pn4;
	for (;;) {
	    a = a + one;
	    b = b + two;
	    c = c + one;
	    an = a * c;
	    pn5 = b * pn3 - an * pn1;
	    pn6 = b * pn4 - an * pn2;
	    if (fabs(pn6) > zero) {
		rn = pn5 / pn6;
		if (fabs(sum - rn) <= fmin2(DBL_EPSILON, DBL_EPSILON * rn))
		    break;
		sum = rn;
	    }
	    pn1 = pn3;
	    pn2 = pn4;
	    pn3 = pn5;
	    pn4 = pn6;
	    if (fabs(pn5) >= oflo) {

                /* re-scale the terms in continued fraction */
		/* if they are large */

		pn1 = pn1 / oflo;
		pn2 = pn2 / oflo;
		pn3 = pn3 / oflo;
		pn4 = pn4 / oflo;
	    }
	}
	arg = arg + log(sum);
	sum = one;
	if (arg >= elimit)
	    sum = one - exp(arg);
    }
    return sum;
}
