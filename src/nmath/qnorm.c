/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000--2023 The R Core Team
 *  Copyright (C) 1998       Ross Ihaka
 *  based on AS 241 (C) 1988 Royal Statistical Society
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
 *	The algorithm AS 241 of Wichura is used,
 *      and has been improved for the very extreme tail (and log_p=TRUE)
 *
 *  REFERENCE
 *
 *      Wichura, M.J. (1988).
 *      Algorithm AS 241: The Percentage Points of the Normal Distribution.
 *      Applied Statistics, 37, 477-484.
 *
 *      Maechler, M. (2022). Asymptotic tail formulas for gaussian quantiles;
 *      https://CRAN.R-project.org/package=DPQ/vignettes/qnorm-asymp.pdf
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
    R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

    if(sigma  < 0)	ML_WARN_return_NAN;
    if(sigma == 0)	return mu;

    p_ = R_DT_qIv(p);/* real lower_tail prob. p */
    q = p_ - 0.5;

#ifdef DEBUG_qnorm
    REprintf("qnorm(p=%10.7g, m=%g, s=%g, l.t.= %d, log= %d): q = %g\n",
	     p,mu,sigma, lower_tail, log_p, q);
#endif


/*-- use AS 241 --- */
/* double ppnd16_(double *p, long *ifault)*/
/*      ALGORITHM AS241  APPL. STATIST. (1988) VOL. 37, NO. 3

        Produces the normal deviate Z corresponding to a given lower
        tail area of P; Z is accurate to about 1 part in 10**16.

        (original fortran code used PARAMETER(..) for the coefficients
         and provided hash codes for checking them...)
*/
    if (fabs(q) <= .425) {/* |p~ - 0.5| <= .425  <==> 0.075 <= p~ <= 0.925 */
        r = .180625 - q * q; // = .425^2 - q^2  >= 0
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
    else { /* closer than 0.075 from {0,1} boundary :
	    *  r := log(p~);  p~ = min(p, 1-p) < 0.075 :  */
	double lp;
	if(log_p && ((lower_tail && q <= 0) || (!lower_tail && q > 0))) {
	    lp = p;
	} else {
	    lp = log( (q > 0) ? R_DT_CIv(p) /* 1-p */ : p_ /* = R_DT_Iv(p) ^=  p */);
	}
	// r = sqrt( - log(min(p,1-p)) )  <==>  min(p, 1-p) = exp( - r^2 ) :
        r = sqrt(-lp);
#ifdef DEBUG_qnorm
	REprintf("\t close to 0 or 1: r = %7g\n", r);
#endif
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
	else if(r <= 27) { /* p is very close to  0 or 1: r in (5, 27] :
		*  r >   5 <==> min(p,1-p)  < exp(-25) = 1.3888..e-11
		*  r <= 27 <==> min(p,1-p) >= exp(-27^2) = exp(-729) ~= 2.507972e-317
		* i.e., we are just barely in the range where min(p, 1-p) has not yet underflowed to zero.
		*/
	    // Wichura, p.478: minimax rational approx R_3(t) is for 5 <= t <= 27  (t :== r)
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
        else { // r > 27: p is *really* close to 0 or 1 .. practically only when log_p =TRUE
	    if(r >= 6.4e8) { // p is *very extremely* close to 0 or 1
		// Using the asymptotical formula ("0-th order"): qn = sqrt(2*s)
		val = r * M_SQRT2;
	    } else {
		double s2 = -ldexp(lp, 1), // = -2*lp = 2s
		    x2 = s2 - log(M_2PI * s2); // = xs_1
		// if(r >= 36000.)  # <==> s >= 36000^2   use x2 = xs_1  above
		if(r < 36000.) {
		    x2 = s2 - log(M_2PI * x2) - 2./(2. + x2); // == xs_2
		    if(r < 840.) { // 27 < r < 840
			x2 = s2 - log(M_2PI * x2) + 2*log1p(- (1 - 1/(4 + x2))/(2. + x2)); // == xs_3
			if(r < 109.) { // 27 < r < 109
			  x2 = s2 - log(M_2PI * x2) +
			      2*log1p(- (1 - (1 - 5/(6 + x2))/(4. + x2))/(2. + x2)); // == xs_4
			  if(r < 55.) { // 27 < r < 55
			    x2 = s2 - log(M_2PI * x2) +
			      2*log1p(- (1 - (1 - (5 - 9/(8. + x2))/(6. + x2))/(4. + x2))/(2. + x2)); // == xs_5
			  }
			}
		    }
		}
                val = sqrt(x2);
	    }
	}
	if(q < 0.0)
	    val = -val;
    }
    return mu + sigma * val;
}
