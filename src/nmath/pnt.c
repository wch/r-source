/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka and the R Development Core Team
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
 */

/*  Algorithm AS 243  Lenth,R.V. (1989). Appl. Statist., Vol.38, 185-189.
 *  ----------------
 *  Cumulative probability at t of the non-central t-distribution
 *  with df degrees of freedom (may be fractional) and non-centrality
 *  parameter delta.
 *
 *  NOTE
 *
 *    Requires the following auxiliary routines:
 *
 *	lgammafn(x)	- log gamma function
 *	pbeta(x, a, b)	- incomplete beta function
 *	pnorm(x)	- normal distribution function
 *
 *  CONSTANTS
 *
 *    M_SQRT_2dPI  = 1/ {gamma(1.5) * sqrt(2)} = sqrt(2 / pi)
 *    M_LN_SQRT_PI = ln(sqrt(pi)) = ln(pi)/2
 */

#include "Mathlib.h"

/*----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_pnt -g -I/usr/local/include -I../include'

 * -- Feb.3, 1999; M.Maechler:
	- For 't > delta > 20' (or so)	the result is completely WRONG!
 */
#ifdef DEBUG_pnt
# include "PrtUtil.h"
#endif

double pnt(double t, double df, double delta)
{
    double a, albeta, b, del, errbd, geven, godd,
	lambda, p, q, rxb, s, tnc, tt, x, xeven, xodd;
    int it, negdel;

    /* note - itrmax and errmax may be changed to suit one's needs. */

    static int itrmax = 1000;
    static double errmax = 1.e-12;

    static double zero = 0.0;
    static double half = 0.5;
    static double one = 1.0;
    static double two = 2.0;

    tnc = zero;/* tnc will be the result */
    if (df <= zero) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    tt = t;
    del = delta;
    negdel = LFALSE;
    if (t < zero) {
	negdel = LTRUE;
	tt = -tt;
	del = -del;
    }

    if (df > 4e5 || del*del > 2*M_LN2*(-(DBL_MIN_EXP))) {
	/*-- 2nd part: if del > 37.62, then p=0 below
	  FIXME: test should depend on `df', `tt' AND `del' ! */
	/* Approx. from	 Abramowitz & Stegun 26.7.10 (p.949) */
	s = one/(4.*df);
	del = - (tt*(1. - s) - del)/sqrt(1. + tt*tt*2.*s);
	goto finis; /* pnorm(-del, 1,0) */
    }

    /* initialize twin series */
    /* Guenther, J. (1978). Statist. Computn. Simuln. vol.6, 199. */

    x = t * t;
    x = x / (x + df);/* in [0,1) */
#ifdef DEBUG_pnt
    REprintf("pnt(t=%7g, df=%7g, delta=%7g) ==> x= %10g:",t,df,delta, x);
#endif
    if (x > zero) {/* <==>  t != 0 */
	lambda = del * del;
	p = half * exp(-half * lambda);
#ifdef DEBUG_pnt
	REprintf("\t p=%10g\n",p);
#endif
	if(p == 0.) { /* underflow! */

	  /*========== really use an other algorithm for this case !!! */
	  ML_ERROR(ME_UNDERFLOW);
	  ML_ERROR(ME_RANGE); /* |delta| too large */
	  return zero;
	}
#ifdef DEBUG_pnt
	REprintf("it  1e5*(godd,  geven)       p	 q	    s	 "
	       /* 1.3 1..4..7.9 1..4..7.9  1..4..7.9 1..4..7.9 1..4..7.9_ */
		 "	  pnt(*)      errbd\n");
	       /* 1..4..7..0..3..6 1..4..7.9*/
#endif
	q = M_SQRT_2dPI * p * del;
	s = half - p;
	a = half;
	b = half * df;
	rxb = pow(one - x, b);
	albeta = M_LN_SQRT_PI + lgammafn(b) - lgammafn(half + b);
	xodd = pbeta(x, a, b);
	godd = two * rxb * exp(a * log(x) - albeta);
	xeven = one - rxb;
	geven = b * x * rxb;
	tnc = p * xodd + q * xeven;

	/* repeat until convergence or iteration limit */
	for(it = 1; it <= itrmax; it++) {
	    a += one;
	    xodd  -= godd;
	    xeven -= geven;
	    godd  *= x * (a + b - one) / a;
	    geven *= x * (a + b - half) / (a + half);
	    p *= lambda / (2 * it);
	    q *= lambda / (2 * it + 1);
	    tnc += p * xodd + q * xeven;
	    s -= p;
	    if(s <= 0.) { /* happens e.g. for (t,df,delta)=(40,10,38.5), after 799 it.*/
		ML_ERROR(ME_PRECISION);
#ifdef DEBUG_pnt
		REprintf("s = %#14.7g < 0 !!! ---> non-convergence!!\n", s);
#endif
		goto finis;
	    }
	    errbd = two * s * (xodd - godd);
#ifdef DEBUG_pnt
	    REprintf("%3d %#9.4g %#9.4g	 %#9.4g %#9.4g %#9.4g %#14.10g %#9.4g\n",
		     it, 1e5*godd, 1e5*geven, p,q, s, tnc, errbd);
#endif
	    if(errbd < errmax) goto finis;/*convergence*/
	}
	/* non-convergence:*/
	ML_ERROR(ME_PRECISION);
    }
 finis:
    tnc += pnorm(- del, zero, one);
    if (negdel)
	tnc = one - tnc;
    return tnc;
}
