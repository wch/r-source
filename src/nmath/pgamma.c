/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 1999 The R Core Development team
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
 *	#include "Mathlib.h"
 *	double pgamma(double x, double alph, double scale);
 *
 *  DESCRIPTION
 *
 *	This function computes the distribution function for the
 *	gamma distribution with shape parameter alph and scale parameter
 *	scale.	This is also known as the incomplete gamma function.
 *	See Abramowitz and Stegun (6.5.1) for example.
 *
 *  NOTES
 *
 *	This function is an adaptation of Algorithm 239 from the
 *	Applied Statistics Series.  The algorithm is faster than
 *	those by W. Fullerton in the FNLIB library and also the
 *	TOMS 542 alorithm of W. Gautschi.  It provides comparable
 *	accuracy to those algorithms and is considerably simpler.
 *
 *  REFERENCES
 *
 *	Algorithm AS 239, Incomplete Gamma Function
 *	Applied Statistics 37, 1988.
 */

#include "Mathlib.h"
/*----------- DEBUGGING -------------
 *	make CFLAGS='-DDEBUG_p -g -I/usr/local/include -I../include'
 */
#ifdef DEBUG_p
# include "PrtUtil.h"
#endif

static const double
    xbig = 1.0e+8,
    xlarge = 1.0e+37,

    alphlimit = 1000.0e0,/* normal approx. for alph > alphlimit */
    elimit = M_LN2*(DBL_MIN_EXP);/* will set exp(E) = 0 for E < elimit ! */
    /* was elimit = -88.0e0; */

double pgamma(double x, double alph, double scale)
{
    double pn1, pn2, pn3, pn4, pn5, pn6, arg, a, b, c, an, osum, sum;
    long n;

    /* check that we have valid values for x and alph */

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(alph) || ISNAN(scale))
	return x + alph + scale;
#endif
    if(alph <= 0. || scale <= 0.) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#ifdef DEBUG_p
    REprintf("pgamma(x=%4g, alph=%4g, scale=%4g): ",x,alph,scale);
#endif
    x /= scale;
#ifdef DEBUG_p
    REprintf("-> x=%4g; ",x);
#endif
    if (x <= 0.)
	return 0.0;

    /* use a normal approximation if alph > alphlimit */

    if (alph > alphlimit) {
	pn1 = sqrt(alph) * 3. * (pow(x/alph, 1./3.) + 1. / (9. * alph) - 1.);
	return pnorm(pn1, 0., 1.);
    }

    /* if x is extremely large __compared to alph__ then return 1 */

    if (x > xbig * alph)
	return 1.;

    if (x <= 1. || x < alph) {

	/* use pearson's series expansion. */

	arg = alph * log(x) - x - lgammafn(alph + 1.);
#ifdef DEBUG_p
	REprintf("Pearson  arg=%g ", arg);
#endif
	c = 1.;
	sum = 1.;
	a = alph;
	do {
	    a += 1.;
	    c *= x / a;
	    sum += c;
	} while (c > DBL_EPSILON);
	arg += log(sum);

#ifndef IEEE_754
	/* Underflow check :*/
	if (arg < elimit)
	    sum = 0.;
	else
#endif
	    sum = exp(arg);

    } else { /* x >= max( 1, alph)

	      * use a continued fraction expansion */

	arg = alph * log(x) - x - lgammafn(alph);
#ifdef DEBUG_p
	REprintf("Cont.Fract. arg=%g ", arg);
#endif
	a = 1. - alph;
	b = a + x + 1.;
	pn1 = 1.;
	pn2 = x;
	pn3 = x + 1.;
	pn4 = x * b;
	sum = pn3 / pn4;
	for (n = 1; ; n++) {
	    a += 1.;/* =   n+1 -alph */
	    b += 2.;/* = 2(n+1)-alph+x */
	    an = a * n;
	    pn5 = b * pn3 - an * pn1;
	    pn6 = b * pn4 - an * pn2;
	    if (fabs(pn6) > 0.) {
		osum = sum;
		sum = pn5 / pn6;
		if (fabs(osum - sum) <= DBL_EPSILON * fmin2(1., sum))
		    break;
	    }
	    pn1 = pn3;
	    pn2 = pn4;
	    pn3 = pn5;
	    pn4 = pn6;
	    if (fabs(pn5) >= xlarge) {

		/* re-scale the terms in continued fraction if they are large */
#ifdef DEBUG_p
		REprintf(" [r] ");
#endif
		pn1 /= xlarge;
		pn2 /= xlarge;
		pn3 /= xlarge;
		pn4 /= xlarge;
	    }
	}
	arg += log(sum);

#ifndef IEEE_754
	/* Underflow check :*/
	if (arg < elimit)
	    sum = 1.;
	else
#endif
	    sum = 1. - exp(arg);
    }
#ifdef DEBUG_p
    REprintf("--> arg=%12g (elimit=%g)\n", arg, elimit);
#endif
    return sum;
}
