/*
 *  Algorithm AS 243  Appl. Statist. (1989), Vol.38, No. 1.
 *
 *  Cumulative probability at t of the non-central t-distribution
 *  with df degrees of freedom (may be fractional) and non-centrality
 *  parameter delta.
 *
 *  NOTE
 *
 *    Requires the following auxiliary routines:
 *
 *        lgamma(x)       - log gamma function
 *        pbeta(x, a, b)  - incomplete beta function
 *        pnorm(x)        - normal distribution function
 *
 *  CONSTANTS
 *
 *    M_SQRT_2dPI  = 1/ {gamma(1.5) * sqrt(2)} = sqrt(2 / pi)
 *    M_LN_SQRT_PI = ln(sqrt(pi)) = ln(pi)/2
 */

#include "Mathlib.h"

#define TRUE	1
#define FALSE	0


double pnt(double t, double df, double delta)
{
    double a, albeta, b, del, en, errbd, geven, godd,
	lambda, p, q, rxb, s, tnc, tt, x, xeven, xodd;
    int negdel;

    /* note - itrmax and errmax may be changed to suit one's needs. */

    static double itrmax = 100.1;
    static double errmax = 1.e-12;

    static double zero = 0.0;
    static double half = 0.5;
    static double one = 1.0;
    static double two = 2.0;

    tnc = zero;
    if (df <= zero) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    tt = t;
    del = delta;
    negdel = FALSE;
    if (t < zero) {
	negdel = TRUE;
	tt = -tt;
	del = -del;
    }
    /* initialize twin series */
    /* (guenther, j. statist. computn. simuln.  vol.6, 199, 1978). */

    en = one;
    x = t * t / (t * t + df);
    if (x > zero) {
	lambda = del * del;
	p = half * exp(-half * lambda);
	q = M_SQRT_2dPI * p * del;
	s = half - p;
	a = half;
	b = half * df;
	rxb = pow(one - x, b);
	albeta = M_LN_SQRT_PI + lgamma(b) - lgamma(a + b);
	xodd = pbeta(x, a, b);
	godd = two * rxb * exp(a * log(x) - albeta);
	xeven = one - rxb;
	geven = b * x * rxb;
	tnc = p * xodd + q * xeven;

	/* repeat until convergence */

	do {
	    a = a + one;
	    xodd = xodd - godd;
	    xeven = xeven - geven;
	    godd = godd * x * (a + b - one) / a;
	    geven = geven * x * (a + b - half) / (a + half);
	    p = p * lambda / (two * en);
	    q = q * lambda / (two * en + one);
	    s = s - p;
	    en = en + one;
	    tnc = tnc + p * xodd + q * xeven;
	    errbd = two * s * (xodd - godd);
	}
	while (errbd > errmax && en <= itrmax);
    }
    if (en <= itrmax)
	ML_ERROR(ME_PRECISION);
    tnc = tnc + pnorm(- del, zero, one);
    if (negdel)
	tnc = one - tnc;
    return tnc;
}
