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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  DESCRIPTION
 *
 *    The density of the binomial distribution.
 *
 * Using the new algorithm of Clive Loader(1999) :
 *
 * The author of this software is Clive Loader, clive@bell-labs.com.
 * Copyright (c) 1999-2000 Lucent Technologies, Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, with the exceptions noted below,
 * and provided that this entire notice is included in all copies of any
 * software which is or includes a copy or modification of this software
 * and in all copies of the supporting documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR LUCENT TECHNOLOGIES
 * MAKE ANY REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE
 * MERCHANTABILITY OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 *
 * This code provides functions dbinom(x,n,p) and dpois(x,lb) for computing
 * binomial and Poisson probabilities, that attempt to be accurate for
 * a full range of parameter values (standard algorithms are often
 * inaccurate with large parameters).
 *
 * NOTE: Loader's original code is now split (and merged into R's extras)
 *       into  ./dbinom.c, ./dpois.c and ./stirlerr.c
 */

#include "nmath.h"
#include "dpq.h"

/* The "deviance part"
   M * D0(x/M) = M*[ x/M * log(x/M) + 1 - (x/M) ] =
   = x*log(x/M) + M - x
   where M = E[X] = n*p or = lambda
*/
double bd0(double x, double np)
{
    double ej, s, s1, v;
    int j;

    if (fabs(x-np) < 0.1*(x+np)) {
	v = (x-np)/(x+np);
	s = (x-np)*v;/* s using v -- change by MM */
	ej = 2*x*v;
	v = v*v;
	for (j=1; ; j++) { /* Taylor series */
	    ej *= v;
	    s1 = s+ej/((j<<1)+1);
	    if (s1==s) /* last term was effectively 0 */
		return(s1);
	    s = s1;
	}
    }
    /* else:  | x - np |  is not too small */
    return(x*log(x/np)+np-x);
}


double dbinom(double x, double n, double p, int give_log)
{
    double lc;
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(n) || ISNAN(p)) return x + n + p;
#endif
    n = floor(n + 0.5);
    if(n < 0 || p < 0 || p > 1) ML_ERR_return_NAN;

    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	MATHLIB_WARNING("non-integer x = %f", x);
	return R_D__0;
    }
    if (x < 0 || x > n)
	return R_D__0;
    if (x == 0)
	return (n == 0 || p == 0) ? R_D__1
	    : (p == 1) ? R_D__0 : R_D_exp(n*log1p(-p));
    /* x > 0 : */
    if (p == 0 || p == 1)
	return (x == n && p == 1) ? R_D__1 : R_D__0;
    /* 0 < p < 1 : */
    if (x == n)
	return give_log ? n*log(p) : pow(p,n);/* or R_pow_di() {w/o checks}*/
    /* else */
#ifndef OLD_dbinom
    lc = stirlerr(n) - stirlerr(x) - stirlerr(n-x)
	- bd0(x, n*p)
	- bd0(n-x, n*(1.-p));
    if (give_log)
	return lc - M_LN_SQRT_2PI + .5*log(n/(x*(n-x)));
    else
	return exp(lc) * sqrt(n/(2*M_PI*x*(n-x)));
#else
    return R_D_exp(lfastchoose(n, x) + log(p) * x + (n - x) * log1p(-p));
#endif
}
