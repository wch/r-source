/*
 *  Algorithm AS 275 appl.statist. (1992), vol.41, no.2
 *  original  (C) 1992 Royal Statistical Society
 *  Copyright (C) 2000 The R Development Core Team
 *
 *  computes the noncentral chi-square distribution function with
 *  positive real degrees of freedom f and nonnegative noncentrality
 *  parameter theta
 */

#include "Mathlib.h"

/*----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_pnch -g -I/usr/local/include -I../include'

 * -- Feb.1, 1998 (R 0.62 alpha); M.Maechler:  still have
	- INFINITE loop \
	- bad precision / in some cases
 */
#ifdef DEBUG_pnch
# include "PrtUtil.h"
#endif

double pnchisq(double x, double f, double theta, int lower_tail, int log_p)
{
    double ans, lam, u, v, x2, f2, t, term, bound, twon;
    int n, flag;

    const static double errmax = 1e-12;
    const static double half = 0.5;
    const static int itrmax = 100;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(f) || ISNAN(theta))
	return x + f + theta;
    if (!R_FINITE(f) || !R_FINITE(theta))
	ML_ERR_return_NAN;
#endif

    if (f < 0. || theta < 0.) ML_ERR_return_NAN;

    if (x <= 0.)
	return R_DT_0;
#ifdef IEEE_754
    if(!R_FINITE(x))
	return R_DT_1;
#endif

    lam = theta * half;
#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%12g, f=%12g, theta=%12g):\n",x,f,theta);
#endif

    /* evaluate the first term */

    v = u = exp(-lam);
    x2 = x * half;
    f2 = f * half;
    /* careful not to overflow .. : */
    t = exp(f2*log(x2) -x2 - lgammafn(f2 + 1));

    /* there is no need to test ifault si */
    /* already been checked */ /*^^^^^^^^ ?????? */

    term = v * t;
    ans = term;
#ifdef DEBUG_pnch
    REprintf("\t v=exp(-th/2)=%12g, x/2=%12g, f/2=%12g ==> t=%12g\n",v,x2,f2,t);
#endif

    /* check if (f+2n) is greater than x */

    flag = LFALSE;
    n = 1; twon = n*2;
    for(;;) {
#ifdef DEBUG_pnch
	REprintf(" _OL_: n=%d",n);
#endif
	if (f + twon - x > 0.) {

	    /* find the error bound and check for convergence */
	    flag = LTRUE;
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
	    t *= x / (f + twon);
	    term = v * t;
	    ans += term;
	    n++; twon = n*2;
	    if (!flag)
		break;
	L10:
	    bound = t * x / (f + twon - x);
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
    return ans;
}
