/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#include "Mathlib.h"

static double
	zero = 0.0,
	one = 1.0,
	two = 2.0,
	oflo = 1.0e+37,
	three = 3.0,
	nine = 9.0,
	xbig = 1.0e+8,
	plimit = 1000.0e0,
	elimit = -88.0e0;

double pgamma(double x, double p, double scale)
{
	double pn1, pn2, pn3, pn4, pn5, pn6, arg, c, rn, a, b, an;
	double sum;

	/* check that we have valid values for x and p */

	if (p <= zero || scale <= zero)
		DOMAIN_ERROR;
	x = x / scale;
	if (x <= zero)
		return 0.0;

	/* use a normal approximation if p > plimit */

	if (p > plimit) {
		pn1 = three * sqrt(p) * pow((x / p), (one / three) + one / (nine * p) - one);
		return pnorm(pn1, 0.0, 1.0);
	}
	/* if x is extremely large compared to p then return 1 */
	if (x > xbig)
		return one;

	if (x <= one || x < p) {
		/* use pearson's series expansion. */
		arg = p * log(x) - x - lgamma(p + one);
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
		arg = p * log(x) - x - lgamma(p);
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

				/* re-scale terms in continued fraction if terms are large */

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
