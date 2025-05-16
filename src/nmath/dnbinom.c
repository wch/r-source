/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000 and Feb, 2001.
 *
 *    dnbinom_mu(): Martin Maechler, June 2008
 *
 *  Merge in to R and improvements notably for |x| << size :
 *	Copyright (C) 2000--2025, The R Core Team
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
 *   Computes the negative binomial distribution. For integer n,
 *   this is probability of x failures before the nth success in a
 *   sequence of Bernoulli trials. We do not enforce integer n, since
 *   the distribution is well defined for non-integers,
 *   and this can be useful for e.g. overdispersed discrete survival times.
 */

#include "nmath.h"
#include "dpq.h"

double dnbinom(double x, double size, double prob, int give_log)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(size) || ISNAN(prob))
        return x + size + prob;
#endif

    if (prob <= 0 || prob > 1 || size < 0) ML_WARN_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;
    x = R_forceint(x);
    if(x == 0) {
	/* limiting case as size approaches zero is point mass at zero */
	if(size == 0) return R_D__1;
	// size > 0:  P(x, ..) = pr^n :
	return(give_log ? size*log(prob) : pow(prob, size));
    }
    if(!R_FINITE(size)) size = DBL_MAX;

    if(x < 1e-10 * size) { // instead of dbinom_raw(), use 2 terms of Abramowitz & Stegun (6.1.47)
	double xx2s =  /* x(x-1)/(2*size)  robustly */
	    (x < sqrt(DBL_MAX)) ? ldexp(x*(x-1), -1)/size : x*(ldexp(x,-1)/size);
	return R_D_exp(size * log(prob) + x * (log(size) + log1p(-prob))
		       - lgamma1p(x) + log1p(xx2s));
    } else {
	/* log( size/(size+x) ) is much less accurate than log1p(- x/(size+x))
	   for |x| << size (and actually when x < size): */
	double p = give_log ? (x < size ? log1p(-x/(size+x)) : log(size/(size+x)))
	                    : size/(size+x),
	     ans = dbinom_raw(size, x+size, prob, 1-prob, give_log);
	return((give_log) ? p + ans : p * ans);
    }
}

double dnbinom_mu(double x, double size, double mu, int give_log)
{
    /* originally, just set  prob :=  size / (size + mu)  and called dbinom_raw(),
     * but that suffers from cancellation when   mu << size  */

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(size) || ISNAN(mu))
        return x + size + mu;
#endif

    if (mu < 0 || size < 0) ML_WARN_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;

    /* limiting case as size approaches zero is point mass at zero,
     * even if mu is kept constant. limit distribution does not
     * have mean mu, though.
     */
    if (x == 0 && size == 0) return R_D__1;
    x = R_forceint(x);
    // FIXME use also for size "almost" Inf because that gives NaN ???
    if(!R_FINITE(size)) // limit case: Poisson
	return(dpois_raw(x, mu, give_log));

    if(x == 0)/* be accurate, both for n << mu, and n >> mu :*/
	return R_D_exp(size * (size < mu ? log(size/(size+mu)) : log1p(- mu/(size+mu))));
    if(x < 1e-10 * size) { /* don't use dbinom_raw() but MM's formula: */
	/* FIXME --- 1e-8 shows problem; rather use algdiv() from ./toms708.c */
	double p = (size < mu ? log(size/(1 + size/mu)) : log(mu / (1 + mu/size)));
	double xx2s =  /* x(x-1)/(2*size)  robustly */
	    (x < sqrt(DBL_MAX)) ? ldexp(x*(x-1), -1)/size : x*(ldexp(x,-1)/size);
	return R_D_exp(x * p - mu - lgamma1p(x) + log1p(xx2s));
    } else {
	/* no unnecessary cancellation inside dbinom_raw, when
	  x_ = size and n_ = x+size are so close that n_ - x_ loses accuracy
	  but log( size/(size+x) ) is much less accurate than log1p(- x/(size+x))
	  for |x| << size (and actually when x < size): */
	double p = give_log ? (x < size ? log1p(-x/(size+x)) : log(size/(size+x)))
			    : size/(size+x),
	    ans = dbinom_raw(size, x+size, size/(size+mu), mu/(size+mu), give_log);
	return((give_log) ? p + ans : p * ans);
    }
}
