/*
 *  Algorithm AS 275 Appl.Statist. (1992), vol.41, no.2
 *  original  (C) 1992 Royal Statistical Society
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  Computes the noncentral chi-squared distribution function with
 *  positive real degrees of freedom f and nonnegative noncentrality
 *  parameter theta
 */

#include "nmath.h"
#include "dpq.h"

/*----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_pnch ....'

 * -- Feb.6, 2000 (R pre0.99); M.Maechler:  still have
 * bad precision & non-convergence in some cases (x ~= f, both LARGE)
 */

double pnchisq(double x, double f, double theta, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(f) || ISNAN(theta))
	return x + f + theta;
    if (!R_FINITE(f) || !R_FINITE(theta))
	ML_ERR_return_NAN;
#endif

    if (f < 0. || theta < 0.) ML_ERR_return_NAN;

    return (R_DT_val(pnchisq_raw(x, f, theta, 1e-12, 10000)));
}

double pnchisq_raw(double x, double f, double theta, 
		   double errmax, int itrmax)
{
    double ans, lam, u, v, x2, f2, t, term, bound, f_x_2n, f_2n;
    int n, flag;

    if (x <= 0.)	return 0.;
    if(!R_FINITE(x))	return 1.;

    lam = .5 * theta;
#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%12g, f=%12g, theta=%12g):\n",x,f,theta);
#endif

    /* evaluate the first term */

    v = u = exp(-lam);
    x2 = .5 * x;
    f2 = .5 * f;
    f_x_2n = f - x;
    if(f2 * DBL_EPSILON > 0.125 &&
       fabs(t = x2 - f2) < sqrt(DBL_EPSILON) * f2) {
	/* evade cancellation error */
	t = exp((1 - t)*(2 - t/(f2 + 1))) / sqrt(2*M_PI*(f2 + 1));
    }
    else {
	/* careful not to overflow .. : */
	t = exp(f2*log(x2) -x2 - lgammafn(f2 + 1));
    }

    term = v * t;
    ans = term;
#ifdef DEBUG_pnch
    REprintf("\t v=exp(-th/2)=%12g, x/2=%12g, f/2=%12g ==> t=%12g\n",v,x2,f2,t);
#endif

    /* check if (f+2n) is greater than x */

    flag = FALSE;
    n = 1;
    f_2n = f + 2.;/* = f + 2*n */
    f_x_2n += 2.;/* = f - x + 2*n */
    for(;;) {
#ifdef DEBUG_pnch
	REprintf(" _OL_: n=%d",n);
#endif
	if (f_x_2n > 0) {

	    /* find the error bound and check for convergence */
	    flag = TRUE;
	    goto L10;
	}
	for(;;) {
#ifdef DEBUG_pnch
	    REprintf(" il: n=%d",n);
#endif
	    /* evaluate the next term of the */
	    /* expansion and then the partial sum */

	    u *= lam / n;
	    v += u;
	    t *= x / f_2n;
	    term = v * t;
	    ans += term;
	    n++; f_2n += 2; f_x_2n += 2;
	    if (!flag && n <= itrmax)
		break;
	L10:
	    bound = t * x / f_x_2n;
#ifdef DEBUG_pnch
	    REprintf("\tL10: n=%d; term=%12g; bound=%12g\n",n,term,bound);
#endif
	    if (bound <= errmax || n > itrmax)
		goto L_End;
	}
    }
L_End:
    if (bound > errmax) { /* NOT converged */
	ML_ERROR(ME_PRECISION);
    }
#ifdef DEBUG_pnch
    REprintf("\tL_End: n=%d; term=%12g; bound=%12g\n",n,term,bound);
#endif
    return (ans);
}
