/*
 *  Algorithm AS 226 Appl. Statist. (1987) Vol. 36, No. 2
 *  Incorporates modification AS R84 from AS Vol. 39, pp311-2, 1990
 *
 *  Returns the cumulative probability of x for the non-central
 *  beta distribution with parameters a, b and non-centrality lambda.
 *
 *  Auxiliary routines required:
 *    lgamma - log-gamma function
 *    pbeta  - incomplete-beta function
 */

#include "Mathlib.h"

double lgamma(double);
double pbeta(double, double, double);

double pnbeta(double x, double a, double b, double lambda)
{
    double a0, ans, ax, lbeta, c, errbd, gx, q, sumq, temp, x0;
    int j;

    static double zero = 0;
    static double one = 1;
    static double half = 0.5;

        /* change errmax and itrmax if desired */

    static double ualpha = 5.0;
    static double errmax = 1.0e-6;
    static int itrmax = 100;


#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(a) || ISNAN(b) || ISNAN(lambda))
	return x + a + b + lambda;
#endif

    if (lambda < zero || a <= zero || b <= zero) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }

    if (x <= zero) return 0;
    if(x >= one) return 1;

    c = lambda * half;

        /* initialize the series */

    x0 = floor(fmax2(c - ualpha * sqrt(c), zero));
    a0 = a + x0;
    lbeta = lgamma(a0) + lgamma(b) - lgamma(a0 + b);
    temp = pbeta(x, a0, b);
    gx = exp(a0 * log(x) + b * log(one - x) - lbeta - log(a0));
    if (a0 > a)
	q = exp(-c + x0 * log(c) - lgamma(x0 + one));
    else
	q = exp(-c);

    ax = q * temp;
    sumq = one - q;
    ans = ax;

        /* recur over subsequent terms */
        /* until convergence is achieved */
    j = 0;
    do {
	j++;
	temp += - gx;
	gx *= x * (a + b + j - one) / (a + j);
	q *= c / j;
	sumq += - q;
	ax = temp * q;
	ans += ax;
	errbd = (temp - gx) * sumq;
    }
    while (errbd > errmax && j < itrmax);

    if (errbd > errmax) {
	ML_ERROR(ME_PRECISION);
    }
    return ans;
}
