/*
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  Algorithm AS 226 Appl. Statist. (1987) Vol. 36, No. 2
 *  Incorporates modification AS R84 from AS Vol. 39, pp311-2, 1990
 *  original (C) Royal Statistical Society 1987, 1990
 *
 *  Returns the cumulative probability of x for the non-central
 *  beta distribution with parameters a, b and non-centrality lambda.
 *
 *  Auxiliary routines required:
 *	lgamma - log-gamma function
 *      pbeta  - incomplete-beta function
 */

#include "nmath.h"
#include "dpq.h"

double pnbeta(double x, double a, double b, double lambda,
	      int lower_tail, int log_p)
{

    /* change errmax and itrmax if desired */

    const double errmax = 1.0e-9;
    const int    itrmax = 100;

    double a0, ans, ax, lbeta, c, errbd, gx, q, sumq, temp, x0;
    int j;


#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(lambda))
	return x + a + b + lambda;
#endif

    if (lambda < 0. || a <= 0. || b <= 0.) ML_ERR_return_NAN;

    if(x <= 0.) return R_DT_0;
    if(x >= 1.) return R_DT_1;

    c = lambda / 2.;

	/* initialize the series */

    x0 = floor(fmax2(c - 7. * sqrt(c), 0.));
    a0 = a + x0;
    lbeta = lgammafn(a0) + lgammafn(b) - lgammafn(a0 + b);
    temp = pbeta_raw(x, a0, b, /* lower = */TRUE);
    gx = exp(a0 * log(x) + b * log1p(-x) - lbeta - log(a0));
    if (a0 > a)
	q = exp(-c + x0 * log(c) - lgammafn(x0 + 1.));
    else
	q = exp(-c);

    sumq = 1. - q;
    ans = ax = q * temp;

	/* recur over subsequent terms until convergence is achieved */
    j = x0; 
    do {
	j++;
	temp -= gx;
	gx *= x * (a + b + j - 1.) / (a + j);
	q *= c / j;
	sumq -= q;
	ax = temp * q;
	ans += ax;
	errbd = (temp - gx) * sumq;
    }
    while (errbd > errmax && j < itrmax + x0);

    if (errbd > errmax) {
	ML_ERROR(ME_PRECISION);
    }
    return R_DT_val(ans);
}

