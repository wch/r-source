/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *  based on AS 111 (C) 1977 Royal Statistical Society
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
 *	double qnorm5(double p, double mu, double sigma,
 *		      int lower_tail, int log_p)
 *            {qnorm (..) is synonymous and preferred inside R}
 *
 *  DESCRIPTION
 *
 *	Compute the quantile function for the normal distribution.
 *
 *	For small to moderate probabilities, algorithm referenced
 *	below is used to obtain an initial approximation which is
 *	polished with a final Newton step.
 *
 *	For very large arguments, an algorithm of Wichura is used.
 *
 *  REFERENCE
 *
 *	Beasley, J. D. and S. G. Springer (1977).
 *	Algorithm AS 111: The percentage points of the normal distribution,
 *	Applied Statistics, 26, 118-121.
 */

#include "Mathlib.h"
#include "dpq.h"

#ifdef DEBUG_qnorm
# include "PrtUtil.h"
#endif

double qnorm5(double p, double mu, double sigma, int lower_tail, int log_p)
{
    double p_, q, r, val;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(mu) || ISNAN(sigma))
	return p + mu + sigma;
#endif
    R_Q_P01_check(p);
    if(sigma  < 0)	ML_ERR_return_NAN;
    if(sigma == 0)	return mu;

    p_ = R_DT_qIv(p);/* real lower_tail prob. p */
    q = p_ - 0.5;

#ifdef DEBUG_qnorm
    REprintf("qnorm(p=%10.7g, m=%7g, s=%7g, l.t.=%2d, log=%2d): q = %7g\n",
	     p,mu,sigma, lower_tail, log_p, q);
#endif

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

	if (q > 0)
	    r = R_DT_CIv(p);/* 1-p */
	else
	    r = p_;/* = R_DT_Iv(p) ^=  p */
#ifdef DEBUG_qnorm
	REprintf("\t 'middle p': r = %7g\n", r);
#endif

	if(r > DBL_EPSILON) {
	    r = sqrt(- ((log_p && ((lower_tail && q <= 0) || (!lower_tail && q > 0))) ?
			p : /* else */ log(r)));
#ifdef DEBUG_qnorm
	    REprintf(" new r = %7g ( =? sqrt(- log(r)) )\n", r);
#endif
	    val = (((2.32121276858 * r + 4.85014127135) * r
		    - 2.29796479134) * r - 2.78718931138)
		/ ((1.63706781897 * r + 3.54388924762) * r + 1.0);
	    if (q < 0)
		val = -val;
	}
	else if(r >= DBL_MIN) { /* r = p <= eps : Use Wichura */
	    val = -2 * (log_p ? R_D_Lval(p) : log(R_D_Lval(p)));
	    r = log(2 * M_PI * val);
	    p = val * val;
	    r = r/val + (2 - r)/p + (-14 + 6 * r - r * r)/(2 * p * val);
	    val = sqrt(val * (1 - r));
	    if(q < 0.0)
		val = -val;
	    return mu + sigma * val;
	}
	else {
	    ML_ERROR(ME_RANGE);
	    if(q < 0.0) return ML_NEGINF;
	    else	return ML_POSINF;
	}
    }
/* FIXME: This could be improved when log_p or !lower_tail ?
 *	  (using p, not p_ , and a different derivative )
 */

#ifdef DEBUG_qnorm
    REprintf("\t before final step: val = %7g\n", val);
#endif
    /* Final Newton step: */
    val = val -
	(pnorm(val, 0., 1., /*lower*/LTRUE, /*log*/LFALSE) - p_) /
	 dnorm(val, 0., 1., /*log*/LFALSE);
    return mu + sigma * val;
}



