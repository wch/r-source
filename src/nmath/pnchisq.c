/*
 *  Algorithm AS 275 Appl.Statist. (1992), vol.41, no.2
 *  original  (C) 1992	     Royal Statistical Society
 *
 *  Computes the noncentral chi-squared distribution function with
 *  positive real degrees of freedom df and nonnegative noncentrality
 *  parameter ncp.  pnchisq_raw is based on
 *
 *    Ding, C. G. (1992)
 *    Algorithm AS275: Computing the non-central chi-squared
 *    distribution function. Appl.Statist., 41, 478-482.

 *  Other parts
 *  Copyright (C) 2000-2019  The R Core Team
 *  Copyright (C) 2003-2015  The R Foundation
 */



#include "nmath.h"
#include "dpq.h"

/*----------- DEBUGGING -------------
 *
 *	make CFLAGS='-DDEBUG_pnch ....'
(cd `R-devel RHOME`/src/nmath; gcc -I. -I../../src/include -I../../../R/src/include -I/usr/local/include -DHAVE_CONFIG_H -fopenmp -g -O0 -pedantic -Wall --std=gnu99 -DDEBUG_pnch -DDEBUG_q -Wcast-align -Wclobbered  -c ../../../R/src/nmath/pnchisq.c -o pnchisq.o )

 * -- Feb.6, 2000 (R pre0.99); M.Maechler:  still have
 * bad precision & non-convergence in some cases (x ~= f, both LARGE)
 */

#ifdef HAVE_LONG_DOUBLE
# define EXP expl
# define FABS fabsl
# define LOG logl
#else
# define EXP exp
# define FABS fabs
# define LOG log
#endif

static const double _dbl_min_exp = M_LN2 * DBL_MIN_EXP;
/*= -708.3964 for IEEE double precision */


double pnchisq(double x, double df, double ncp, int lower_tail, int log_p)
{
    double ans;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df) || ISNAN(ncp))
	return x + df + ncp;
    if (!R_FINITE(df) || !R_FINITE(ncp))
	ML_ERR_return_NAN;
#endif

    if (df < 0. || ncp < 0.) ML_ERR_return_NAN;

    ans = pnchisq_raw(x, df, ncp, 1e-12, 8*DBL_EPSILON, 1000000, lower_tail, log_p);

    if (x <= 0. || x == ML_POSINF)
	return ans; // because it's perfect

    if(ncp >= 80) {
	if(lower_tail) {
	    ans = fmin2(ans, R_D__1);  /* e.g., pchisq(555, 1.01, ncp = 80) */
	} else { /* !lower_tail */
	    /* since we computed the other tail cancellation is likely */
	    // FIXME: There are cases where  ans == 0. if(!log_p) is perfect
	    if(ans < (log_p ? (-10. * M_LN10) : 1e-10)) ML_ERROR(ME_PRECISION, "pnchisq");
	    if(!log_p && ans < 0.) ans = 0.;  /* Precaution PR#7099 */
	}
    }
    /* MM: the following "hack" from c51179 (<--> PR#14216, by Jerry Lewis)
     * -- is "kind of ok" ... but potentially suboptimal: we do  log1p(- p(*, <other tail>, log=FALSE)),
     *    but that  p(*, log=FALSE) may already be an exp(.) or even expm1(..)
     *   <---> "in principle"  this check should happen there, not here  */
    if (!log_p || ans < -1e-8)
	return ans;
    else { // log_p  &&  ans >= -1e-8
	// prob. = exp(ans) is near one: we can do better using the other tail
#ifdef DEBUG_pnch
	REprintf("   pnchisq_raw(*, log_p): ans=%g => 2nd call, other tail\n", ans);
#endif
	ans = pnchisq_raw(x, df, ncp, 1e-12, 8*DBL_EPSILON, 1000000, !lower_tail, FALSE);
	return log1p(-ans);
    }
}

double attribute_hidden
pnchisq_raw(double x, double f, double theta /* = ncp */,
	    double errmax, double reltol, int itrmax,
	    Rboolean lower_tail, Rboolean log_p)
{
    double lam, x2, f2, term, bound, f_x_2n, f_2n;
    double l_lam = -1., l_x = -1.; /* initialized for -Wall */
    int n;
    Rboolean lamSml, tSml, is_r, is_b;
    LDOUBLE ans, u, v, t, lt, lu =-1;

    if (x <= 0.) {
	if(x == 0. && f == 0.) { // chi^2_0(.) has point mass at zero
#define _L  (-0.5 * theta) // = -lambda
	    return lower_tail ? R_D_exp(_L) : (log_p ? R_Log1_Exp(_L) : -expm1(_L));
	}
	/* x < 0  or {x==0, f > 0} */
	return R_DT_0;
    }
    if(!R_FINITE(x))	return R_DT_1;

    /* This is principally for use from qnchisq */
#ifndef MATHLIB_STANDALONE
    R_CheckUserInterrupt();
#endif

    if(theta < 80) { /* use 110 for Inf, as ppois(110, 80/2, lower.tail=FALSE) is 2e-20 */
	LDOUBLE ans;
	int i;
	// Have  pgamma(x,s) < x^s / Gamma(s+1) (< and ~= for small x)
	// ==> pchisq(x, f) = pgamma(x, f/2, 2) = pgamma(x/2, f/2)
	//                  <  (x/2)^(f/2) / Gamma(f/2+1) < eps
	// <==>  f/2 * log(x/2) - log(Gamma(f/2+1)) < log(eps) ( ~= -708.3964 )
	// <==>        log(x/2) < 2/f*(log(Gamma(f/2+1)) + log(eps))
	// <==> log(x) < log(2) + 2/f*(log(Gamma(f/2+1)) + log(eps))
	if(lower_tail && f > 0. &&
	   log(x) < M_LN2 + 2/f*(lgamma(f/2. + 1) + _dbl_min_exp)) {
	    // all  pchisq(x, f+2*i, lower_tail, FALSE), i=0,...,110 would underflow to 0.
	    // ==> work in log scale
	    double lambda = 0.5 * theta;
	    double sum, sum2, pr = -lambda;
	    sum = sum2 = ML_NEGINF;
	    /* we need to renormalize here: the result could be very close to 1 */
	    for(i = 0; i < 110;  pr += log(lambda) - log(++i)) {
		sum2 = logspace_add(sum2, pr);
		sum = logspace_add(sum, pr + pchisq(x, f+2*i, lower_tail, TRUE));
		if (sum2 >= -1e-15) /*<=> EXP(sum2) >= 1-1e-15 */ break;
	    }
	    ans = sum - sum2;
#ifdef DEBUG_pnch
	    REprintf("pnchisq(x=%g, f=%g, th.=%g); th. < 80, logspace: i=%d, ans=(sum=%g)-(sum2=%g)\n",
		     x,f,theta, i, (double)sum, (double)sum2);
#endif
	    return (double) (log_p ? ans : EXP(ans));
	}
	else {
	    LDOUBLE lambda = 0.5 * theta; // < 40
	    LDOUBLE sum = 0, sum2 = 0, pr = EXP(-lambda); // does this need a feature test?
	    /* we need to renormalize here: the result could be very close to 1 */
	    for(i = 0; i < 110;  pr *= lambda/++i) {
		// pr == exp(-lambda) lambda^i / i!  ==  dpois(i, lambda)
		sum2 += pr;
		// pchisq(*, i, *) is  strictly decreasing to 0 for lower_tail=TRUE
		//                 and strictly increasing to 1 for lower_tail=FALSE
		sum += pr * pchisq(x, f+2*i, lower_tail, FALSE);
		if (sum2 >= 1-1e-15) break;
	    }
	    ans = sum/sum2;
#ifdef DEBUG_pnch
	    REprintf("pnchisq(x=%g, f=%g, theta=%g); theta < 80: i=%d, sum=%g, sum2=%g\n",
		     x,f,theta, i, (double)sum, (double)sum2);
#endif
	    return (double) (log_p ? LOG(ans) : ans);
	}
    } // if(theta < 80)

    // else: theta == ncp >= 80 --------------------------------------------
#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%g, f=%g, theta=%g >= 80): ",x,f,theta);
#endif
    // Series expansion ------- FIXME: log_p=TRUE, lower_tail=FALSE only applied at end ==> underflow

    lam = .5 * theta; // = lambda = ncp/2
    lamSml = (-lam < _dbl_min_exp);
    if(lamSml) {
	/* MATHLIB_ERROR(
	   "non centrality parameter (= %g) too large for current algorithm",
p 	   theta) */
        u = 0;
        lu = -lam;/* == ln(u) */
        l_lam = log(lam);
    } else {
	u = exp(-lam);
    }

    /* evaluate the first term */
    v = u;
    x2 = .5 * x;
    f2 = .5 * f;
    f_x_2n = f - x;

#ifdef DEBUG_pnch
    REprintf("-- v=exp(-th/2)=%g, x/2= %g, f/2= %g\n",v,x2,f2);
#endif

    if(f2 * DBL_EPSILON > 0.125 && /* very large f and x ~= f: probably needs */
       FABS(t = x2 - f2) <         /* another algorithm anyway */
       sqrt(DBL_EPSILON) * f2) {
	/* evade cancellation error */
	/* t = exp((1 - t)*(2 - t/(f2 + 1))) / sqrt(2*M_PI*(f2 + 1));*/
        lt = (1 - t)*(2 - t/(f2 + 1)) - M_LN_SQRT_2PI - 0.5 * log(f2 + 1);
#ifdef DEBUG_pnch
	REprintf(" (case I) ==> ");
#endif
    }
    else {
	/* Usual case 2: careful not to overflow .. : */
	lt = f2*log(x2) -x2 - lgammafn(f2 + 1);
    }
#ifdef DEBUG_pnch
    REprintf(" lt= %g", lt);
#endif

    tSml = (lt < _dbl_min_exp);
    if(tSml) {
#ifdef DEBUG_pnch
	REprintf(" is very small\n");
#endif
	if (x > f + theta +  5* sqrt( 2*(f + 2*theta))) {
	    /* x > E[X] + 5* sigma(X) */
	    return R_DT_1; /* FIXME: could be more accurate than 0. */
	} /* else */
	l_x = log(x);
	ans = term = 0.; t = 0;
    }
    else {
	t = EXP(lt);
#ifdef DEBUG_pnch
 	REprintf(", t=exp(lt)= %g\n", t);
#endif
	ans = term = (double) (v * t);
    }

    for (n = 1, f_2n = f + 2., f_x_2n += 2.; n <= itrmax ; n++, f_2n += 2, f_x_2n += 2) {
#ifdef DEBUG_pnch_n
	REprintf("\n _OL_: n=%d",n);
#endif
#ifndef MATHLIB_STANDALONE
	if(n % 1000 == 0) R_CheckUserInterrupt();
#endif
	/* f_2n    === f + 2*n
	 * f_x_2n  === f - x + 2*n   > 0  <==> (f+2n)  >   x */
	if (f_x_2n > 0) {
	    /* find the error bound and check for convergence */

	    bound = (double) (t * x / f_x_2n);
#ifdef DEBUG_pnch_n
	    REprintf("\n L10: n=%d; term= %g; bound= %g",n,term,bound);
#endif
	    is_r = FALSE;
	    /* convergence only if BOTH absolute and relative error < 'bnd' */
	    if (((is_b = (bound <= errmax)) &&
                 (is_r = (term <= reltol * ans))))
            {
#ifdef DEBUG_pnch
                REprintf("BREAK out of for(n = 1 ..): n=%d; bound= %g %s, rel.err= %g %s\n",
			 n,
			 bound, (is_b ? "<= errmax" : ""),
			 term/ans, (is_r ? "<= reltol" : ""));
#endif
		break; /* out completely */
            }
	}

	/* evaluate the next term of the */
	/* expansion and then the partial sum */

        if(lamSml) {
            lu += l_lam - log(n); /* u = u* lam / n */
            if(lu >= _dbl_min_exp) {
		/* no underflow anymore ==> change regime */
#ifdef DEBUG_pnch_n
                REprintf(" n=%d; nomore underflow in u = exp(lu) ==> change\n",
			 n);
#endif
                v = u = EXP(lu); /* the first non-0 'u' */
                lamSml = FALSE;
            }
        } else {
	    u *= lam / n;
	    v += u;
	}
	if(tSml) {
            lt += l_x - log(f_2n);/* t <- t * (x / f2n) */
            if(lt >= _dbl_min_exp) {
		/* no underflow anymore ==> change regime */
#ifdef DEBUG_pnch
                REprintf("  n=%d; nomore underflow in t = exp(lt) ==> change\n", n);
#endif
                t = EXP(lt); /* the first non-0 't' */
                tSml = FALSE;
            }
        } else {
	    t *= x / f_2n;
	}
        if(!lamSml && !tSml) {
	    term = (double) (v * t);
	    ans += term;
	}

    } /* for(n ...) */

    if (n > itrmax) {
	MATHLIB_WARNING4(_("pnchisq(x=%g, f=%g, theta=%g, ..): not converged in %d iter."),
			 x, f, theta, itrmax);
    }
#ifdef DEBUG_pnch
    REprintf("\n == L_End: n=%d; term= %g; bound=%g: ans=%Lg\n",
	     n, term, bound, ans);
#endif
    double dans = (double) ans;
    return R_DT_val(dans);
}
