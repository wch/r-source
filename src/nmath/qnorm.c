/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
 *  based on AS 111 (C) 1977 Royal Statistical Society
 *  and   on AS 241 (C) 1988 Royal Statistical Society
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
 *
 *      Wichura, M.J. (1988).
 *      Algorithm AS 241: The Percentage Points of the Normal Distribution.
 *      Applied Statistics, 37, 477-484.
 */

#include "nmath.h"
#include "dpq.h"

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
    REprintf("qnorm(p=%10.7g, m=%g, s=%g, l.t.= %d, log= %d): q = %g\n",
	     p,mu,sigma, lower_tail, log_p, q);
#endif



#ifdef OLD_qnorm
    /* --- use  AS 111 --- */
    if (fabs(q) <= 0.42) {

	/* 0.08 <= p <= 0.92 */

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
	    r = sqrt(- ((log_p &&
			 ((lower_tail && q <= 0) || (!lower_tail && q > 0))) ?
			p : /* else */ log(r)));
#ifdef DEBUG_qnorm
	    REprintf("\t new r = %7g ( =? sqrt(- log(r)) )\n", r);
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
#ifdef DEBUG_qnorm
	    REprintf("\t DBL_MIN <= r <= DBL_EPS: val = %g, new r = %g\n",
		     val, r);
#endif
	    p = val * val;
	    r = r/val + (2 - r)/p + (-14 + 6 * r - r * r)/(2 * p * val);
	    val = sqrt(val * (1 - r));
	    if(q < 0.0)
		val = -val;
	    return mu + sigma * val;
	}
	else {
#ifdef DEBUG_qnorm
	    REprintf("\t r < DBL_MIN : giving up (-> +- Inf \n");
#endif
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

#else
/*-- use AS 241 --- */
/* double ppnd16_(double *p, long *ifault)*/
/*      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3

        Produces the normal deviate Z corresponding to a given lower
        tail area of P; Z is accurate to about 1 part in 10**16.

        (original fortran code used PARAMETER(..) for the coefficients
         and provided hash codes for checking them...)
*/

    if (fabs(q) <= .425) {/* 0.075 <= p <= 0.925 */
        r = .180625 - q * q;
	val =
            q * (((((((r * 2509.0809287301226727 +
                       33430.575583588128105) * r + 67265.770927008700853) * r +
                     45921.953931549871457) * r + 13731.693765509461125) * r +
                   1971.5909503065514427) * r + 133.14166789178437745) * r +
                 3.387132872796366608)
            / (((((((r * 5226.495278852854561 +
                     28729.085735721942674) * r + 39307.89580009271061) * r +
                   21213.794301586595867) * r + 5394.1960214247511077) * r +
                 687.1870074920579083) * r + 42.313330701600911252) * r + 1.);
    }
    else { /* closer than 0.075 from {0,1} boundary */

	/* r = min(p, 1-p) < 0.075 */
	if (q > 0)
	    r = R_DT_CIv(p);/* 1-p */
	else
	    r = p_;/* = R_DT_Iv(p) ^=  p */

/*      if (r <= 0.) { /.* p = 0 or 1 {or outside}; should return - or + Inf *./
 *             *ifault = 1; return 0.;
 *      }
 */

	r = sqrt(- ((log_p &&
		     ((lower_tail && q <= 0) || (!lower_tail && q > 0))) ?
		    p : /* else */ log(r)));
        /* r = sqrt(-log(r))  <==>  min(p, 1-p) = exp( - r^2 ) */

        if (r <= 5.) { /* <==> min(p,1-p) >= exp(-25) ~= 1.3888e-11 */
            r += -1.6;
            val = (((((((r * 7.7454501427834140764e-4 +
                       .0227238449892691845833) * r + .24178072517745061177) *
                     r + 1.27045825245236838258) * r +
                    3.64784832476320460504) * r + 5.7694972214606914055) *
                  r + 4.6303378461565452959) * r +
                 1.42343711074968357734)
                / (((((((r *
                         1.05075007164441684324e-9 + 5.475938084995344946e-4) *
                        r + .0151986665636164571966) * r +
                       .14810397642748007459) * r + .68976733498510000455) *
                     r + 1.6763848301838038494) * r +
                    2.05319162663775882187) * r + 1.);
        }
        else { /* very close to  0 or 1 */
            r += -5.;
            val = (((((((r * 2.01033439929228813265e-7 +
                       2.71155556874348757815e-5) * r +
                      .0012426609473880784386) * r + .026532189526576123093) *
                    r + .29656057182850489123) * r +
                   1.7848265399172913358) * r + 5.4637849111641143699) *
                 r + 6.6579046435011037772)
                / (((((((r *
                         2.04426310338993978564e-15 + 1.4215117583164458887e-7)*
                        r + 1.8463183175100546818e-5) * r +
                       7.868691311456132591e-4) * r + .0148753612908506148525)
                     * r + .13692988092273580531) * r +
                    .59983220655588793769) * r + 1.);
        }

	if(q < 0.0)
	    val = -val;
        /* return (q >= 0.)? r : -r ;*/
    }

#endif
/*-- Switch of AS 111 <-> AS 241 --- */

    return mu + sigma * val;
}



