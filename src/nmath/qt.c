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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *	The "Student" t distribution quantile function.
 *
 *  NOTES
 *
 *	This is a C translation of the Fortran routine given in:
 *	Algorithm 396: Student's t-quantiles by
 *	G.W. Hill CACM 13(10), 619-620, October 1970
 */

#include "nmath.h"
#include "dpq.h"

double qt(double p, double ndf, int lower_tail, int log_p)
{
    const double eps = 1.e-12;

    double a, b, c, d, p_, P, q, x, y;
    Rboolean neg;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(ndf))
	return p + ndf;
#endif
    if (p == R_DT_0) return ML_NEGINF;
    if (p == R_DT_1) return ML_POSINF;
    R_Q_P01_check(p);

    if (ndf < 1) /* FIXME:  not yet treated here */
	ML_ERR_return_NAN;

    /* FIXME: This test should depend on  ndf  AND p  !!
     * -----  and in fact should be replaced by
     * something like Abramowitz & Stegun 26.7.5 (p.949)
     */
    if (ndf > 1e20) return qnorm(p, 0., 1., lower_tail, log_p);

    p_ = R_D_qIv(p); /* note: exp(p) may underflow to 0; fix later */

    if((lower_tail && p_ > 0.5) || (!lower_tail && p_ < 0.5)) {
	neg = FALSE; P = 2 * R_D_Cval(p_);
    } else {
	neg = TRUE;  P = 2 * R_D_Lval(p_);
    } /* 0 <= P <= 1  in all cases */

    if (fabs(ndf - 2) < eps) {	/* df ~= 2 */
	if(P > 0)
	    q = sqrt(2 / (P * (2 - P)) - 2);
	else { /* P = 0, but maybe = exp(p) ! */
	    if(log_p) q = M_SQRT2 * exp(- .5 * R_D_Lval(p));
	    else q = ML_POSINF;
	}
    }
    else if (ndf < 1 + eps) { /* df ~= 1  (df < 1 excluded above !) */
	if(P > 0)
	    q = - tan((P+1) * M_PI_2);

	else { /* P = 0, but maybe p_ = exp(p) ! */
	    if(log_p) q = M_1_PI * exp(-R_D_Lval(p));/* cot(e) ~ 1/e */
	    else q = ML_POSINF;
	}
    }
    else {		/*-- usual case;  including, e.g.,  df = 1.1 */
	a = 1 / (ndf - 0.5);
	b = 48 / (a * a);
	c = ((20700 * a / b - 98) * a - 16) * a + 96.36;
	d = ((94.5 / (b + c) - 3) / b + 1) * sqrt(a * M_PI_2) * ndf;
	if(P > 0 || !log_p)
	    y = pow(d * P, 2 / ndf);
	else /* P = 0 && log_p;  P = 2*exp(p*) */
	    y = exp(2 / ndf * (log(d) + M_LN2 + R_D_Lval(p)));

	if (y > 0.05 + a) {
	    /* Asymptotic inverse expansion about normal */
	    if(P > 0 || !log_p)
		x = qnorm(0.5 * P, 0., 1., /*lower_tail*/TRUE, /*log_p*/FALSE);
	    else /* P = 0 && log_p;  P = 2*exp(p') */
		x = qnorm( p,	   0., 1., lower_tail,	       /*log_p*/TRUE);

	    y = x * x;
	    if (ndf < 5)
		c += 0.3 * (ndf - 4.5) * (x + 0.6);
	    c = (((0.05 * d * x - 5) * x - 7) * x - 2) * x + b + c;
	    y = (((((0.4 * y + 6.3) * y + 36) * y + 94.5) / c
		  - y - 3) / b + 1) * x;
	    y = a * y * y;
	    /* FIXME: Following cutoff is machine-precision dependent
	       -----  Really, use stable impl. of expm1(y) == exp(y) - 1,
	              as it is in GNU's mathlib ..*/
	    if (y > 1e-6) /* was (y > 0.002) */
		y = exp(y) - 1;
	    else { /* Taylor of	 e^y -1 : */
		y = (0.5 * y + 1) * y;
	    }
	} else {
	    y = ((1 / (((ndf + 6) / (ndf * y) - 0.089 * d - 0.822)
		       * (ndf + 2) * 3) + 0.5 / (ndf + 4))
		 * y - 1) * (ndf + 1) / (ndf + 2) + 1 / y;
	}
	q = sqrt(ndf * y);
    }
    if(neg) q = -q;
    return q;
}
