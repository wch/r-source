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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double dnbeta(double x, double a, double b, double lambda);
 *
 *  DESCRIPTION
 *
 *    Computes the density of the noncentral beta distribution with
 *    noncentrality parameter lambda.  The noncentral beta distribution
 *    has density:
 *
 *                     Inf
 *        f(x|a,b,d) = SUM p(i) * B(a+i,b) * x^(a+i-1) * (1-x)^(b-1)
 *                     i=0
 *
 *    where:
 *
 *              p(k) = exp(-lambda) lambda^k / k!
 *
 *            B(a,b) = Gamma(a+b) / (Gamma(a) * Gamma(b))
 *
 *
 *    This can be computed efficiently by using the recursions:
 *
 *            p(k+1) = (lambda/(k+1)) * p(k-1)
 *
 *        B(a+k+1,b) = ((a+b+k)/(a+k)) * B(a+k,b)
 *
 *    The summation of the series continues until
 *
 *              psum = p(0) + ... + p(k)
 *
 *    is close to 1.  Here we continue until 1 - psum < epsilon,
 *    with epsilon set close to the relative machine precision.
 */

#include "Mathlib.h"

double dnbeta(double x, double a, double b, double lambda)
{
	double k, lambda2, psum, sum, term, weight;
	static double eps = 1.e-14;
	static int maxiter = 200;

#ifdef IEEE_754
	if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(lambda))
		return x + a + b + lambda;
#endif

	if (lambda < 0 || a <= 0 || b <= 0) {
		ML_ERROR(ME_DOMAIN);
	}

#ifdef IEEE_754
	if (!R_FINITE(a) || !R_FINITE(b) || !R_FINITE(lambda)) {
		ML_ERROR(ME_DOMAIN);
		return ML_NAN;
	}
#endif

	if(x <= 0) return 0;

	term = dbeta(x, a, b);
	if(lambda == 0)
		return term;

	lambda2 = 0.5 * lambda;
	weight = exp(- lambda2);
	sum = weight * term;
	psum = weight;
	for(k=1 ; k<=maxiter ; k++) {
		weight = weight * lambda2 / k;
		term = term * x * (a + b) / a;
		sum = sum + weight * term;
		psum = psum + weight;
		a = a + 1;
		if(1 - psum < eps) break;
	}
	return sum;
}
