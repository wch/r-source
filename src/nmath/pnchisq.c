/*
 *  algorithm as 275 appl.statist. (1992), vol.41, no.2
 *  computes the noncentral chi-square distribution function with
 *  positive real degrees of freedom f and nonnegative noncentrality
 *  parameter theta
 */

#include "Mathlib.h"

#define TRUE	1
#define FALSE	0

double pnchisq(double x, double f, double theta)
{
    double ans, lam, n, u, v, x2, f2, t, term, bound;
    int flag;

    static double errmax = 1e-12;
    static double zero = 0;
    static double one = 1;
    static double two = 2;
    static int itrmax = 100;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(f) || ISNAN(theta))
	return x + f + theta;
    if (!FINITE(f) || !FINITE(theta)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif

    if (f < zero && theta < zero) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x <= zero)
	return 0;
#ifdef IEEE_754
    if(!FINITE(x))
	return 1;
#endif

    lam = theta / two;

    /* evaluate the first term */

    n = one;
    u = exp(-lam);
    v = u;
    x2 = x / two;
    f2 = f / two;
    t = pow(x2, f2) * exp(-x2) / exp(lgamma((f2 + one)));

    /* there is no need to test ifault si */
    /* already been checked */

    term = v * t;
    ans = term;

    /* check if (f+2n) is greater than x */

    flag = FALSE;
    for(;;) {
	if (f + two * n - x > zero) {

	    /* find the error bound and */
	    /* check for convergence */

	    flag = TRUE;
	    goto L10;
	}
	for(;;) {

	    /* evaluate the next term of the */
	    /* expansion and then the partial sum */

	    u = u * lam / n;
	    v = v + u;
	    t = t * x / (f + two * n);
	    term = v * t;
	    ans = ans + term;
	    n = n + one;
	    if (!flag)
		break;
	  L10:
	    bound = t * x / (f + two * n - x);
	    if (bound <= errmax || (int)n > itrmax)
		goto L20;
	}
    }
  L20:
    if (bound > errmax)
	ML_ERROR(ME_PRECISION);
    return ans;
}
