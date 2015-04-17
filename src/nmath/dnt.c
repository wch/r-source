/*
 *  AUTHOR
 *    Claus Ekstrøm, ekstrom@dina.kvl.dk
 *    July 15, 2003.
 *
 *  Merge in to R:
 *	Copyright (C) 2003-2015 The R Foundation
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
 *
 *  NOTE
 *
 *    Requires the following auxiliary routines:
 *
 *	lgammafn(x)	- log gamma function
 *	pnt(x, df, ncp) - the distribution function for
 *			  the non-central t distribution
 *
 *
 * DESCRIPTION
 *
 *    From Johnson, Kotz and Balakrishnan (1995) [2nd ed.; formula (31.15), p.516],
 *    the non-central t density is
 *
 *      f(x, df, ncp) =
 *
 *        exp(-.5*ncp^2) * gamma((df+1)/2) / (sqrt(pi*df)* gamma(df/2)) * (df/(df+x^2))^((df+1)/2) *
 *          sum_{j=0}^Inf  gamma((df+j+1)/2)/(factorial(j)* gamma((df+1)/2)) * (x*ncp*sqrt(2)/sqrt(df+x^2))^ j
 *
 *
 *    The functional relationship
 *
 *	   f(x, df, ncp) = df/x *
 *			      (F(sqrt((df+2)/df)*x, df+2, ncp) - F(x, df, ncp))
 *
 *    is used to evaluate the density at x != 0 and
 *
 *	   f(0, df, ncp) = exp(-.5*ncp^2) /
 *				(sqrt(pi)*sqrt(df)*gamma(df/2))*gamma((df+1)/2)
 *
 *    is used for x=0.
 *
 *    All calculations are done on log-scale to increase stability.
 *
 * FIXME: pnt() is known to be inaccurate in the (very) left tail and for ncp > 38
 *       ==> use a direct log-space summation formula in that case
 */

#include "nmath.h"
#include "dpq.h"

double dnt(double x, double df, double ncp, int give_log)
{
    double u;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df))
	return x + df;
#endif

    /* If non-positive df then error */
    if (df <= 0.0) ML_ERR_return_NAN;

    if(ncp == 0.0) return dt(x, df, give_log);

    /* If x is infinite then return 0 */
    if(!R_FINITE(x))
	return R_D__0;

    /* If infinite df then the density is identical to a
       normal distribution with mean = ncp.  However, the formula
       loses a lot of accuracy around df=1e9
    */
    if(!R_FINITE(df) || df > 1e8)
	return dnorm(x, ncp, 1., give_log);

    /* Do calculations on log scale to stabilize */

    /* Consider two cases: x ~= 0 or not */
    if (fabs(x) > sqrt(df * DBL_EPSILON)) {
	u = log(df) - log(fabs(x)) +
	    log(fabs(pnt(x*sqrt((df+2)/df), df+2, ncp, 1, 0) -
		     pnt(x, df, ncp, 1, 0)));
	/* FIXME: the above still suffers from cancellation (but not horribly) */
    }
    else {  /* x ~= 0 : -> same value as for  x = 0 */
	u = lgammafn((df+1)/2) - lgammafn(df/2)
	    - (M_LN_SQRT_PI + .5*(log(df) + ncp*ncp));
    }

    return (give_log ? u : exp(u));
}
