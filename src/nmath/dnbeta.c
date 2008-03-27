/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-8 The R Development Core Team
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
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double dnbeta(double x, double a, double b, double ncp, int give_log);
 *
 *  DESCRIPTION
 *
 *    Computes the density of the noncentral beta distribution with
 *    noncentrality parameter ncp.  The noncentral beta distribution
 *    has density:
 *
 *		       Inf
 *	f(x|a,b,ncp) = SUM  p(i) *  x^(a+i-1) * (1-x)^(b-1) / B(a+i,b)
 *		       i=0
 *
 *    where:
 *
 *		p(k) = exp(-ncp/2) (ncp/2)^k / k!
 *
 *	      B(a,b) = Gamma(a) * Gamma(b) / Gamma(a+b)
 *
 *
 *    This can be computed efficiently by using the recursions:
 *
 *	      p(k+1) = ncp/2 / (k+1) * p(k)
 *
 *      B(a+k+1,b)   = (a+k)/(a+b+k) * B(a+k,b)
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

double dnbeta(double x, double a, double b, double ncp, int give_log)
{
    const static double eps = 1.e-14;
    const int maxiter = 10000; /* was 200 */

    double k, ncp2;
    LDOUBLE psum, sum, term, weight;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(ncp))
	return x + a + b + ncp;
#endif
    if (ncp < 0 || a <= 0 || b <= 0)
	ML_ERR_return_NAN;

    if (!R_FINITE(a) || !R_FINITE(b) || !R_FINITE(ncp))
	ML_ERR_return_NAN;

    if (x < 0 || x > 1) return(R_D__0);
    if(ncp == 0)
	return dbeta(x, a, b, give_log);

    term = dbeta(x, a, b, /* log = */ TRUE);
    if(!R_FINITE(term)) /* in particular, if term = +Inf */
	return R_D_exp(term);
    ncp2 = 0.5 * ncp;
    /* FIXME: prevent underflow in term *and* weight-- probably should
     * work in log scale and *rescale* when needed ..*/
    term   = exp(term);
    weight = exp(- ncp2);
    sum = weight * term;
    if(sum == 0.) {
	if(term != 0.) /* (x = {0,1} gives true 0 for a,b>=1) */
	    ML_ERROR(ME_UNDERFLOW, "dnbeta");
    } else {
	psum = weight;
	for(k = 1; k <= maxiter; k++) {
	    double c1, c2, t;
	    weight *= (c1 = (ncp2 / k));
	    term   *= (c2 = x * (a + b) / a);
	    sum  += (t = weight * term);
	    psum += weight;
	    a += 1;
	    if(c1*c2 < 1 && psum + eps > 1 && t < eps * sum)
		break;
	    else if(k == maxiter) /* not converged */
		ML_ERROR(ME_NOCONV, "dnbeta");
	}
    }
    return R_D_val(sum);
}
