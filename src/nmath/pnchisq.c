/*
 *  Algorithm AS 275 Appl.Statist. (1992), vol.41, no.2
 *  original  (C) 1992	     Royal Statistical Society
 *  Copyright (C) 2000--2002 The R Development Core Team
 *  Copyright (C) 2003	     The R Foundation
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

    static const double my_dbl_min_exp = M_LN2 * DBL_MIN_EXP;
    /*= -708.3964 for IEEE double precision */

    if (x <= 0.)	return 0.;
    if(!R_FINITE(x))	return 1.;

#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%g, f=%g, theta=%g): ",x,f,theta);
#endif
    lam = .5 * theta;
    if(-lam < my_dbl_min_exp)
	MATHLIB_ERROR("non centrality parameter (= %g) too large for current algorithm", theta)

    /* evaluate the first term */

    v = u = exp(-lam);
    x2 = .5 * x;
    f2 = .5 * f;
    f_x_2n = f - x;

#ifdef DEBUG_pnch
    REprintf("-- v=exp(-th/2)=%g, x/2= %g, f/2= %g\n",v,x2,f2);
#endif

    if(f2 * DBL_EPSILON > 0.125 &&
       fabs(t = x2 - f2) < sqrt(DBL_EPSILON) * f2) {
	/* evade cancellation error */
	t = exp((1 - t)*(2 - t/(f2 + 1))) / sqrt(2*M_PI*(f2 + 1));
#ifdef DEBUG_pnch
	REprintf(" (case I) ==> t= %g\n",t);
#endif
    }
    else {
	/* careful not to overflow .. : */
	t = f2*log(x2) -x2 - lgammafn(f2 + 1);
	if (t < my_dbl_min_exp &&
	    x > f + theta +  3* sqrt( 2*(f + 2*theta))) {
	    /* x > E[X] + 3* sigma(X) */
	    return 1.; /* better than 0 ! */
	} /* else */
	t = exp(t);
#ifdef DEBUG_pnch
	REprintf(" (case 2) ==> t= %g\n",t);
#endif
    }

    if(t <= 0)
	MATHLIB_WARNING3("too large x (= %g) or centrality parameter %g for current algorithm;\n\t%s", theta, x, "result is probably invalid!");

    term = v * t;
    ans = term;

    /* check if (f+2n) is greater than x */

    flag = FALSE;
    n = 1;
    f_2n = f + 2.;/* = f + 2*n */
    f_x_2n += 2.;/* = f - x + 2*n */
    for(;;) {
#ifdef DEBUG_pnch
	REprintf("\n _OL_: n=%d",n);
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
	    REprintf("\n L10: n=%d; term= %g; bound= %g",n,term,bound);
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
    REprintf("\n == L_End: n=%d; term= %g; bound=%g\n",n,term,bound);
#endif
    return (ans);
}
