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
 *
 *  Reference:
 *  Beasley, J. D. and S. G. Springer (1977).
 *  Algorithm AS 111: The percentage points of the normal distribution,
 *  Applied Statistics, 26, 118-121.
 *
 *  Polished with a final Newton step.
 */

#include "Mathlib.h"

static double a0 = 2.50662823884;
static double a1 = -18.61500062529;
static double a2 = 41.39119773534;
static double a3 = -25.44106049637;
static double b1 = -8.47351093090;
static double b2 = 23.08336743743;
static double b3 = -21.06224101826;
static double b4 = 3.13082909833;
static double c0 = -2.78718931138;
static double c1 = -2.29796479134;
static double c2 = 4.85014127135;
static double c3 = 2.32121276858;
static double d1 = 3.54388924762;
static double d2 = 1.63706781897;
static double zero = 0.0;
static double half = 0.5;
static double one = 1.0;
static double split = 0.42;

double qnorm(double p, double mean, double sd)
{
	double q, r, val;

	if (p <= 0.0 || p >= 1.0)
		DOMAIN_ERROR;
	q = p - half;
	if (fabs(q) <= split) {
		/* 0.08 < p < 0.92 */
		r = q * q;
		val = q * (((a3 * r + a2) * r + a1) * r + a0)
			/ ((((b4 * r + b3) * r + b2) * r + b1) * r + one);
	}
	else {
		/* p < 0.08 or p > 0.92, set r = min(p,1-p) */
		r = p;
		if (q > zero)
			r = one - p;
		r = sqrt(-log(r));
		val = (((c3 * r + c2) * r + c1) * r + c0)
			/ ((d2 * r + d1) * r + one);
		if (q < zero)
			val = -val;
	}
	val = val - (pnorm(val, 0.0, 1.0) - p) / dnorm(val, 0.0, 1.0);
	return mean + sd * val;
}
