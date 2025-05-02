/*
 * AUTHOR
 *   Catherine Loader, catherine@research.bell-labs.com.
 *   October 23, 2000.
 *
 *  Merge in to R and further tweaks :
 *  notably using log1p() and pow1p(), thanks to Morten Welinder, PR#18642
 *
 *	Copyright (C) 2000-2025 The R Core Team
 *	Copyright (C) 2008 The R Foundation
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
 *  https://www.R-project.org/Licenses/
 *
 *
 * DESCRIPTION
 *
 *   To compute the binomial probability, call dbinom(x,n,p).
 *   This checks for argument validity, and calls dbinom_raw().
 *
 *   dbinom_raw() does the actual computation; note this is called by
 *   other functions in addition to dbinom().
 *     (1) dbinom_raw() has both p and q arguments, when one may be represented
 *         more accurately than the other (in particular, in df()).
 *     (2) dbinom_raw() does NOT check that inputs x and n are integers. This
 *         should be done in the calling function, where necessary.
 *         -- but is not the case at all when called e.g., from df() or dbeta() !
 *     (3) Also does not check for 0 <= p <= 1 and 0 <= q <= 1 or NaN's.
 *         Do this in the calling function.
 */

#include "nmath.h"
#include "dpq.h"

/* Compute  (1+x)^y  accurately also for |x| << 1  */
double pow1p(double x, double y)
{
    if(isnan(y))
	return (x == 0) ? 1. : y; // (0+1)^NaN := 1  by standards
    if(0 <= y && y == trunc(y) && y <= 4.) {
	switch((int)y) {
	case 0: return 1;
	case 1: return x + 1.;
	case 2: return x*(x + 2.) + 1.;
	case 3: return x*(x*(x + 3.) + 3.) + 1.;
	case 4: return x*(x*(x*(x + 4.) + 6.) + 4.) + 1.;
	}
    }
    /* naive algorithm in two cases: (1) when 1+x is exact (compiler should not over-optimize !),
     * and (2) when |x| > 1/2 and we have no better algorithm.
     */
    volatile double xp1 = x + 1., x_ = xp1 - 1.; // compiler should *not* optimize these
    if (x_ == x || fabs(x) > 0.5 || isnan(x)) {
	return pow(xp1, y);
    } else { /* not perfect, e.g., for small |x|, non-huge y, use
	    binom expansion 1 + y*x + y(y-1)/2 x^2 + .. */
	return exp(y * log1p(x));
    }
}

double dbinom_raw(double x, double n, double p, double q, int give_log)
{
    if (p == 0) return((x == 0) ? R_D__1 : R_D__0);
    if (q == 0) return((x == n) ? R_D__1 : R_D__0);

    // NB: The smaller of p and q is the most accurate
    if (x == 0) {
	if(n == 0) return R_D__1;
	if (p > q)
	    return give_log ? n * log(q)    : pow(q, n);
	else // 0 < p <= 1/2
	    return give_log ? n * log1p(-p) : pow1p(-p, n);
    }
    if (x == n) { // r = p^x = p^n  -- accurately
	if (p > q)
	    return give_log ? n * log1p(-q) : pow1p(-q, n);
	else
	    return give_log ? n * log (p)   : pow (p, n);
    }
    if (x < 0 || x > n) return( R_D__0 );

    if(!R_FINITE(n)) {
	if(R_FINITE(x)) return( R_D__0 ); /* finite x << n = Inf */
	else n = DBL_MAX; // helps ? extreme dnbinom() cases
    }

// TODO?  Improve accuracy in these cases:
#ifdef _NO_LOG_DBINOM_
    if(!give_log) { // more accurate *not* going via log when result is much much smaller than 1
	if (x <= M || n-x <= M) { /* use "recursive" direct formula with
				     k := min(x, n-x) multiplications */
	}
    }
#endif

    /* n*p or n*q can underflow to zero if n and p or q are small.  This
       used to occur in dbeta, and gives NaN as from R 2.3.0.  */
    double lc = stirlerr(n) - stirlerr(x) - stirlerr(n-x) - bd0(x,n*p) - bd0(n-x,n*q);

    /* f = (M_2PI*x*(n-x))/n; could overflow or underflow */
    /* Upto R 2.7.1:
     * lf = log(M_2PI) + log(x) + log(n-x) - log(n);
     * -- following is much better for  x << n : */
    double lf = M_LN_2PI + log(x) + log1p(- x/n);

    return R_D_exp(lc - 0.5*lf);
}

double dbinom(double x, double n, double p, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(n) || ISNAN(p)) return x + n + p;
#endif

    if (p < 0 || p > 1 || R_D_negInonint(n))
	ML_WARN_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;

    n = R_forceint(n);
    x = R_forceint(x);

    return dbinom_raw(x, n, p, 1-p, give_log);
}
