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
 *    double pnorm(double p, double mu, double sigma);
 *
 *  DESCRIPTION
 *
 *    Compute the quantile function for the normal distribution.
 *
 *    For small to moderate probabilities, algorithm referenced
 *    below is used to obtain an initial approximation which is
 *    polished with a final Newton step.
 *
 *    For very large arguments, an algorithm of Wichura is used.
 *
 *  REFERENCE
 *
 *    Beasley, J. D. and S. G. Springer (1977).
 *    Algorithm AS 111: The percentage points of the normal distribution,
 *    Applied Statistics, 26, 118-121.
 */

#include "Mathlib.h"


double qnorm(double p, double mu, double sigma)
{
    double q, r, val;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma))
	return p + mu + sigma;
#endif
    if (p < 0.0 || p > 1.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }

    q = p - 0.5;

    if (fabs(q) <= 0.42) {

	/* 0.08 < p < 0.92 */

	r = q * q;
	val = q * (((-25.44106049637 * r + 41.39119773534) * r
		    - 18.61500062529) * r + 2.50662823884)
	    / ((((3.13082909833 * r - 21.06224101826) * r
		 + 23.08336743743) * r + -8.47351093090) * r + 1.0);
    }
    else {

	/* p < 0.08 or p > 0.92, set r = min(p, 1 - p) */

	r = p;
	if (q > 0.0)
	    r = 1.0 - p;

	if(r > DBL_EPSILON) {
	    r = sqrt(-log(r));
	    val = (((2.32121276858 * r + 4.85014127135) * r
		    - 2.29796479134) * r - 2.78718931138)
		/ ((1.63706781897 * r + 3.54388924762) * r + 1.0);
	    if (q < 0.0)
		val = -val;
	}
	else if(r > 1e-300) {		/* Assuming IEEE here? */
	    val = -2 * log(p);
	    r = log(6.283185307179586476925286766552 * val);
	    r = r/val + (2 - r)/(val * val)
		+ (-14 + 6 * r - r * r)/(2 * val * val * val);
	    val = sqrt(val * (1 - r));
	    if(q < 0.0)
		val = -val;
	    return val;
	}
	else {
	    ML_ERROR(ME_RANGE);
	    if(q < 0.0) {
		return ML_NEGINF;
	    }
	    else {
		return ML_POSINF;
	    }
	}
    }
    val = val - (pnorm(val, 0.0, 1.0) - p) / dnorm(val, 0.0, 1.0);
    return mu + sigma * val;
}
