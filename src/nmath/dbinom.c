/*
 * AUTHOR
 *   Catherine Loader, catherine@research.bell-labs.com.
 *   October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
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
 *
 * DESCRIPTION
 *
 *   To compute the binomial probability, call dbinom(x,n,p).
 *   This checks for argument validity, and calls dbinom_raw().
 *
 *   dbinom_raw() does the actual computation; note this is called by
 *   other functions in addition to dbinom()).
 *     (1) dbinom_raw() has both p and q arguments, when one may be represented
 *         more accurately than the other (in particular, in df()).
 *     (2) dbinom_raw() does NOT check that inputs x and n are integers. This
 *         should be done in the calling function, where necessary.
 *     (3) Also does not check for 0 <= p <= 1 and 0 <= q <= 1 or NaN's.
 *         Do this in the calling function.
 */

#include "nmath.h"
#include "dpq.h"

double dbinom_raw(double x, double n, double p, double q, int give_log)
{
    double f, lc;

    if (p == 0) return((x == 0) ? R_D__1 : R_D__0);
    if (q == 0) return((x == n) ? R_D__1 : R_D__0);

    if (x == 0) {
	if(n == 0) return R_D__1;
	lc = (p < 0.1) ? -bd0(n,n*q) - n*p : n*log(q);
	return( R_D_exp(lc) );
    }
    if (x == n) {
	lc = (q < 0.1) ? -bd0(n,n*p) - n*q : n*log(p);
	return( R_D_exp(lc) );
    }
    if (x < 0 || x > n) return( R_D__0 );

    lc = stirlerr(n) - stirlerr(x) - stirlerr(n-x) - bd0(x,n*p) - bd0(n-x,n*q);
    f = (M_2PI*x*(n-x))/n;

    return R_D_fexp(f,lc);
}

double dbinom(double x, double n, double p, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(n) || ISNAN(p)) return x + n + p;
#endif

    if (p < 0 || p > 1 || R_D_negInonint(n))
	ML_ERR_return_NAN;
    R_D_nonint_check(x);

    n = R_D_forceint(n);
    x = R_D_forceint(x);

    return dbinom_raw(x,n,p,1-p,give_log);
}
