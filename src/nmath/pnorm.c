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
 *    double pnorm(double x, double mu, double sigma);
 *
 *  DESCRIPTION
 *
 *    The main computation evaluates near-minimax approximations derived
 *    from those in "Rational Chebyshev approximations for the error
 *    function" by W. J. Cody, Math. Comp., 1969, 631-637.  This
 *    transportable program uses rational functions that theoretically
 *    approximate the normal distribution function to at least 18
 *    significant decimal digits.  The accuracy achieved depends on the
 *    arithmetic system, the compiler, the intrinsic functions, and
 *    proper selection of the machine-dependent constants.
 *
 *  REFERENCE
 *
 *    Cody, W. D. (1993).
 *    ALGORITHM 715: SPECFUN - A Portable FORTRAN Package of
 *    Special Function Routines and Test Drivers".
 *    ACM Transactions on Mathematical Software. 19, 22-32.
 */

#include "Mathlib.h"

/*  Mathematical Constants */

#define SIXTEN	1.6					/* Magic Cutoff */

double pnorm(double x, double mu, double sigma)
{
    static double c[9] = {
	0.39894151208813466764,
	8.8831497943883759412,
	93.506656132177855979,
	597.27027639480026226,
	2494.5375852903726711,
	6848.1904505362823326,
	11602.651437647350124,
	9842.7148383839780218,
	1.0765576773720192317e-8
    };

    static double d[8] = {
	22.266688044328115691,
	235.38790178262499861,
	1519.377599407554805,
	6485.558298266760755,
	18615.571640885098091,
	34900.952721145977266,
	38912.003286093271411,
	19685.429676859990727
    };

    static double p[6] = {
	0.21589853405795699,
	0.1274011611602473639,
	0.022235277870649807,
	0.001421619193227893466,
	2.9112874951168792e-5,
	0.02307344176494017303
    };

    static double q[5] = {
	1.28426009614491121,
	0.468238212480865118,
	0.0659881378689285515,
	0.00378239633202758244,
	7.29751555083966205e-5
    };

    static double a[5] = {
	2.2352520354606839287,
	161.02823106855587881,
	1067.6894854603709582,
	18154.981253343561249,
	0.065682337918207449113
    };

    static double b[4] = {
	47.20258190468824187,
	976.09855173777669322,
	10260.932208618978205,
	45507.789335026729956
    };

    double xden, temp, xnum, result, ccum;
    double del, min, eps, xsq;
    double y;
    int i;

    /* Note: The structure of these checks has been */
    /* carefully thought through.  For example, if x == mu */
    /* and sigma == 0, we still get the correct answer. */

#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(mu) || ISNAN(sigma))
	return x + mu + sigma;
#endif
    if (sigma < 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    x = (x - mu) / sigma;
#ifdef IEEE_754
    if(!finite(x)) {
	if(x < 0) return 0;
	else return 1;
    }
#endif

    eps = DBL_EPSILON * 0.5;
    min = DBL_MIN;
    y = fabs(x);
    if (y <= 0.66291) {
	xsq = 0.0;
	if (y > eps) {
	    xsq = x * x;
	}
	xnum = a[4] * xsq;
	xden = xsq;
	for (i = 1; i <= 3; ++i) {
	    xnum = (xnum + a[i - 1]) * xsq;
	    xden = (xden + b[i - 1]) * xsq;
	}
	result = x * (xnum + a[3]) / (xden + b[3]);
	temp = result;
	result = 0.5 + temp;
	ccum = 0.5 - temp;
    }
    else if (y <= M_SQRT_32) {

	/* Evaluate pnorm for 0.66291 <= |z| <= sqrt(32) */

	xnum = c[8] * y;
	xden = y;
	for (i = 1; i <= 7; ++i) {
	    xnum = (xnum + c[i - 1]) * y;
	    xden = (xden + d[i - 1]) * y;
	}
	result = (xnum + c[7]) / (xden + d[7]);
	xsq = floor(y * SIXTEN) / SIXTEN;
	del = (y - xsq) * (y + xsq);
	result = exp(-xsq * xsq * 0.5) * exp(-del * 0.5) * result;
	ccum = 1.0 - result;
	if (x > 0.0) {
	    temp = result;
	    result = ccum;
	    ccum = temp;
	}
    }
    else if(y < 50) {

	/* Evaluate pnorm for sqrt(32) < |z| < 50 */

	result = 0.0;
	xsq = 1.0 / (x * x);
	xnum = p[5] * xsq;
	xden = xsq;
	for (i = 1; i <= 4; ++i) {
	    xnum = (xnum + p[i - 1]) * xsq;
	    xden = (xden + q[i - 1]) * xsq;
	}
	result = xsq * (xnum + p[4]) / (xden + q[4]);
	result = (M_1_SQRT_2PI - result) / y;
	xsq = floor(x * SIXTEN) / SIXTEN;
	del = (x - xsq) * (x + xsq);
	result = exp(-xsq * xsq * 0.5) * exp(-del * 0.5) * result;
	ccum = 1.0 - result;
	if (x > 0.0) {
	    temp = result;
	    result = ccum;
	    ccum = temp;
	}
    }
    else {
	if(x > 0) {
	    result = 1.0;
	    ccum = 0.0;
	}
	else {
	    result = 0.0;
	    ccum = 1.0;
	}
    }
    if (result < min) {
	result = 0.0;
    }
    if (ccum < min) {
	ccum = 0.0;
    }
    return result;
}
