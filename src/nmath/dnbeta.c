/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double dnbeta(double x, double a, double b, double lambda, int give_log);
 *
 *  DESCRIPTION
 *
 *    Computes the density of the noncentral beta distribution with
 *    noncentrality parameter lambda.  The noncentral beta distribution
 *    has density:
 *
 *		       Inf
 *	  f(x|a,b,d) = SUM p(i) * B(a+i,b) * x^(a+i-1) * (1-x)^(b-1)
 *		       i=0
 *
 *    where:
 *
 *		p(k) = exp(-lambda) lambda^k / k!
 *
 *	      B(a,b) = Gamma(a+b) / (Gamma(a) * Gamma(b))
 *
 *
 *    This can be computed efficiently by using the recursions:
 *
 *	      p(k+1) = (lambda/(k+1)) * p(k-1)
 *
 *	  B(a+k+1,b) = ((a+b+k)/(a+k)) * B(a+k,b)
 *
 *    The summation of the series continues until
 *
 *		psum = p(0) + ... + p(k)
 *
 *    is close to 1.  Here we continue until 1 - psum < epsilon,
 *    with epsilon set close to the relative machine precision.
 */

#include "nmath.h"
#include "dpq.h"

double dnbeta(double x, double a, double b, double lambda, int give_log)
{
    const static double eps = 1.e-14;
    const int maxiter = 200;

    double k, lambda2, psum, sum, term, weight;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(lambda))
	return x + a + b + lambda;
#endif
    if (lambda < 0 || a <= 0 || b <= 0)
	ML_ERR_return_NAN;

    if (!R_FINITE(a) || !R_FINITE(b) || !R_FINITE(lambda))
	ML_ERR_return_NAN;

    if (x < 0 || x > 1) return(R_D__0);
    if(lambda == 0)
	return dbeta(x, a, b, give_log);

    term = dbeta(x, a, b, /* log = */ FALSE);
    if(!R_FINITE(term)) /* in particular, if term = +Inf */
	return R_D_val(term);
    lambda2 = 0.5 * lambda;
    weight = exp(- lambda2);
    sum	 = weight * term;
    psum = weight;
    for(k = 1; k <= maxiter; k++) {
	weight *= (lambda2 / k);
	term *= x * (a + b) / a;
	sum  += weight * term;
	psum += weight;
	a += 1;
	if(1 - psum < eps) break;
    }
    if(1 - psum >= eps) { /* not converged */
	ML_ERROR(ME_PRECISION, "dnbeta");
    }
    return R_D_val(sum);
}
