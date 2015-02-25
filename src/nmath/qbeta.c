/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2015  The R Core Team
 *  based on code (C) 1979 and later Royal Statistical Society
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

 * Reference:
 * Cran, G. W., K. J. Martin and G. E. Thomas (1977).
 *	Remark AS R19 and Algorithm AS 109,
 *	Applied Statistics, 26(1), 111-114.
 * Remark AS R83 (v.39, 309-310) and the correction (v.40(1) p.236)
 *	have been incorporated in this version.
 */


#include "nmath.h"
#include "dpq.h"

#ifdef DEBUG_qbeta
/* for REprintf */
# include <R_ext/Print.h>
#endif

/* set the exponent of accu to -2r-2 for r digits of accuracy */
/*---- NEW ---- -- still fails for p = 1e11, q=.5*/

#define fpu 3e-308
/* acu_min:  Minimal value for accuracy 'acu' which will depend on (a,p);
	     acu_min >= fpu ! */
#define acu_min 1e-300
#define lower fpu
#define upper 1-2.22e-16

#define const1 2.30753
#define const2 0.27061
#define const3 0.99229
#define const4 0.04481


double qbeta(double alpha, double p, double q, int lower_tail, int log_p)
{
    int swap_tail, i_pb, i_inn;
#ifdef DEBUG_qbeta
    Rboolean warned = FALSE;
#endif
    double a, la, adj, logbeta, g, h, pp, p_, prev, qq, r, s, t, tx, w, y, wprev;
    double acu;
    volatile double xinbta;

    /* test for admissibility of parameters */

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(q) || ISNAN(alpha))
	return p + q + alpha;
#endif
    if(p < 0. || q < 0.) ML_ERR_return_NAN;

    R_Q_P01_boundaries(alpha, 0, 1);

    p_ = R_DT_qIv(alpha);/* lower_tail prob (in any case) */
    // Conceptually,  0 < p_ < 1  (but can be 0 or 1 because of cancellation!)

    //  p==0, q==0, p = Inf, q = Inf  <==> treat as one- or two-point mass
    if(p == 0 || q == 0 || !R_FINITE(p) || !R_FINITE(q)) {
	// We know 0 < p_ < 1 : pbeta() is constant and trivial in {0, 1/2, 1}
#ifdef DEBUG_qbeta
	REprintf(
	    "qbeta(%g, %g, %g, lower_t=%d, log_p=%d): (p,q)-boundary: trivial\n",
	    alpha, p,q, lower_tail, log_p);
#endif
	if(p == 0 && q == 0) { // point mass 1/2 at each of {0,1} :
	    if(alpha < R_D_half) return 0.;
	    if(alpha > R_D_half) return 1.;
	    // else:  alpha == "1/2"
	    return 0.5;
	} else if (p == 0 || p/q == 0) { // point mass 1 at 0 - "flipped around"
	    return 0.;
	} else if (q == 0 || q/p == 0) { // point mass 1 at 0 - "flipped around"
	    return 1;
	}
	// else:  p = q = Inf : point mass 1 at 1/2
	return 0.5;
    }


    if(log_p && (p_ == 0. || p_ == 1.))
	return p_; /* better than NaN or infinite loop;
		      FIXME: suboptimal, since -Inf < alpha ! */

    /* initialize */
    logbeta = lbeta(p, q);

    /* change tail if necessary;  afterwards   0 < a <= 1/2	 */
    if (p_ <= 0.5) {
	a = p_;	pp = p; qq = q; swap_tail = 0;
	la = lower_tail ? R_D_log(alpha) : R_D_LExp(alpha);
    } else { /* change tail, swap  p <-> q :*/
	a = R_DT_CIv(alpha); // = 1 - p_ < 1/2
	/* la := log(a), but without numerical cancellation: */
	la = lower_tail ? R_D_LExp(alpha) : R_D_log(alpha);
	pp = q; qq = p; swap_tail = 1;
    }

#ifdef DEBUG_qbeta
    REprintf("qbeta(%g, %g, %g, lower_t=%d, log_p=%d):%s\n"
	     "  swap_tail=%d, la=%g: ",
	     alpha, p,q, lower_tail, log_p,
	     (log_p && (p_ == 0. || p_ == 1.)) ? (p_==0.?" p_=0":" p_=1") : "",
	     swap_tail, la);
#endif
    /* calculate the initial approximation */

    /* y := {fast approximation of} qnorm(1 - a) :*/
    r = sqrt(-2 * la);
    y = r - (const1 + const2 * r) / (1. + (const3 + const4 * r) * r);
    if (pp > 1 && qq > 1) { // use  Carter(1947), see AS 109, remark '5.'
	r = (y * y - 3.) / 6.;
	s = 1. / (pp + pp - 1.);
	t = 1. / (qq + qq - 1.);
	h = 2. / (s + t);
	w = y * sqrt(h + r) / h - (t - s) * (r + 5. / 6. - 2. / (3. * h));
#ifdef DEBUG_qbeta
	REprintf("p,q > 1 => w=%g", w);
#endif
	xinbta = pp / (pp + qq * exp(w + w));
    } else {
	r = qq + qq;
	t = 1. / (9. * qq);
	t = r * R_pow_di(1. - t + y * sqrt(t), 3);
#ifdef DEBUG_qbeta
	REprintf("min(p,q) <= 1: t=%g", t);
#endif
	if (t <= 0.) {
#ifdef DEBUG_qbeta
	    REprintf(" t <= 0: log1p(-a)=%.15g\n", log1p(-a));
#endif
	    xinbta = 1. - exp((log1p(-a)+ log(qq) + logbeta) / qq);
	} else {
	    t = (4. * pp + r - 2.) / t;
#ifdef DEBUG_qbeta
	    REprintf(" t >= 0:  new t = %g ( > 1 ?)\n", t);
#endif
	    if (t <= 1.)
		xinbta = exp((log(a * pp) + logbeta) / pp);
	    else
		xinbta = 1. - 2. / (t + 1.);
	}
    }

    /* Desired accuracy for Newton iterations (below) should depend on  (a,p)
     * This is from Remark .. on AS 109, adapted.
     * However, it's not clear if this is "optimal" for IEEE double prec.

     * acu = fmax2(acu_min, pow(10., -25. - 5./(pp * pp) - 1./(a * a)));

     * NEW: 'acu' accuracy NOT for squared adjustment, but simple;
     * ---- i.e.,  "new acu" = sqrt(old acu)
    */
    acu = fmax2(acu_min, pow(10.0, -13.0 - 2.5/(pp * pp) - 0.5/(a * a)));

#ifdef DEBUG_qbeta
    REprintf(" -> xinbta = %.16g (Newton acu=%g)\n", xinbta, acu);
#endif

    /* solve for x by a modified newton-raphson method, */
    /* using the function pbeta_raw */

    r = 1 - pp;
    t = 1 - qq;
    wprev = 0.;
    adj = 1;
    /* Sometimes the approximation is negative! */
    if (xinbta < lower)
	xinbta = 0.5;
    else if (xinbta > upper)
	xinbta = 0.5;

    tx = prev = 0.;	/* keep -Wall happy */
    for (i_pb=0; i_pb < 1000; i_pb++) {
	y = pbeta_raw(xinbta, pp, qq, /*lower_tail = */ TRUE, FALSE);
#ifdef IEEE_754
	if(!R_FINITE(y))
#else
	    if (errno)
#endif
		ML_ERR_return_NAN;

	w = (y - a) *
	    exp(logbeta + r * log(xinbta) + t * log1p(-xinbta));
	if (w * wprev <= 0.)
	    prev = fmax2(fabs(adj),fpu);
	g = 1;
#ifdef DEBUG_qbeta
	REprintf("N(i=%d): x0=%.15g, pb(x0)=%.15g, w=%.15g, prev=%g,", i_pb, xinbta, y, w, prev);
#endif
	for (i_inn=0; i_inn < 1000;i_inn++) {
	    adj = g * w;
	    if (fabs(adj) < prev) {
		tx = xinbta - adj; // x_{n+1} = x_n - g*w
		if (0. <= tx && tx <= 1.) {
		    if (prev <= acu || fabs(w) <= acu) {
#ifdef DEBUG_qbeta
			REprintf(" it{in}=%d, delta(x)=%g, %s <= acu  ==> convergence\n",
				 i_inn, -adj, (prev <= acu) ? "prev" : "|w|");
#endif
			goto L_converged;
		    }
		    if (tx != 0. && tx != 1)
			break;
		}
	    }
	    g /= 3;
	}
#ifdef DEBUG_qbeta
	REprintf(" it{in}=%d, delta(x)=%g\n", i_inn, -adj);
#endif
	if (fabs(tx - xinbta) <= 1e-15 * xinbta) // "<=" for xinbta == 0
	    goto L_converged;
	xinbta = tx;
	wprev = w;
    }
    /*-- NOT converged: Iteration count --*/
#ifdef DEBUG_qbeta
    warned = TRUE;
#endif
    ML_ERROR(ME_PRECISION, "qbeta");

L_converged:
#ifdef DEBUG_qbeta
    REprintf(" %s: Final delta(y) = %g\n",
	     warned ? "_NO_ convergence" : "converged", y - a);
#endif
    return swap_tail ? 1 - xinbta : xinbta;
}
