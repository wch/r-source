/*
 *  Algorithm AS 275 Appl.Statist. (1992), vol.41, no.2
 *  original  (C) 1992	     Royal Statistical Society
 *  Copyright (C) 2000--2002 The R Development Core Team
 *  Copyright (C) 2003--2004 The R Foundation
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

    return (R_DT_val(pnchisq_raw(x, f, theta, 1e-12, 8*DBL_EPSILON, 1000000)));
}

double pnchisq_raw(double x, double f, double theta,
		   double errmax, double reltol, int itrmax)
{
    double ans, lam, u, v, x2, f2, t, term, bound, f_x_2n, f_2n, lt;
    double lu = -1., l_lam = -1., l_x = -1.; /* initialized for -Wall */
    int n;
    Rboolean lamSml, tSml, is_r, is_b, is_it;

    static const double _dbl_min_exp = M_LN2 * DBL_MIN_EXP;
    /*= -708.3964 for IEEE double precision */

    if (x <= 0.)	return 0.;
    if(!R_FINITE(x))	return 1.;

#ifdef DEBUG_pnch
    REprintf("pnchisq(x=%g, f=%g, theta=%g): ",x,f,theta);
#endif
    lam = .5 * theta;
    lamSml = (-lam < _dbl_min_exp);
    if(lamSml) {
	/* MATHLIB_ERROR(
	   "non centrality parameter (= %g) too large for current algorithm",
	   theta) */
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
       fabs(t = x2 - f2) <         /* other algorithm anyway */
       sqrt(DBL_EPSILON) * f2) {
	/* evade cancellation error */
	/* t = exp((1 - t)*(2 - t/(f2 + 1))) / sqrt(2*M_PI*(f2 + 1));*/
        lt = (1 - t)*(2 - t/(f2 + 1)) - 0.5 * log(2*M_PI*(f2 + 1));
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
	if (x > f + theta +  5* sqrt( 2*(f + 2*theta))) {
	    /* x > E[X] + 5* sigma(X) */
	    return 1.; /* better than 0 --- but definitely "FIXME" */
	} /* else */
	l_x = log(x);
	ans = term = t = 0.;
    }
    else {
	t = exp(lt);
#ifdef DEBUG_pnch
 	REprintf(", t=exp(lt)= %g\n", t);
#endif
	ans = term = v * t;
    }

    for (n = 1, f_2n = f + 2., f_x_2n += 2.;  ; n++, f_2n += 2, f_x_2n += 2) {
#ifdef DEBUG_pnch
	REprintf("\n _OL_: n=%d",n);
#endif
	/* f_2n    === f + 2*n
	 * f_x_2n  === f - x + 2*n   > 0  <==> (f+2n)  >   x */
	if (f_x_2n > 0) {
	    /* find the error bound and check for convergence */

	    bound = t * x / f_x_2n;
#ifdef DEBUG_pnch
	    REprintf("\n L10: n=%d; term= %g; bound= %g",n,term,bound);
#endif
	    is_r = is_it = FALSE;
	    /* convergence only if BOTH absolute and relative error < 'bnd' */
	    if (((is_b = (bound <= errmax)) &&
                 (is_r = (term <= reltol * ans))) || (is_it = (n > itrmax)))
            {
#ifdef DEBUG_pnch
                REprintf("BREAK n=%d %s; bound= %g %s, rel.err= %g %s\n",
			 n, (is_it ? "> itrmax" : ""),
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
#ifdef DEBUG_pnch
                REprintf(" n=%d; nomore underflow in u = exp(lu) ==> change\n",
			 n);
#endif
                v = u = exp(lu); /* the first non-0 'u' */
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
                REprintf("  n=%d; nomore underflow in t = exp(lt) ==> change\n",
			 n);
#endif
                t = exp(lt); /* the first non-0 't' */
                tSml = FALSE;
            }
        } else {
	    t *= x / f_2n;
	}
        if(!lamSml && !tSml) {
	    term = v * t;
	    ans += term;
	}

    } /* for(n ...) */

    if (is_it) {
	MATHLIB_WARNING2("pnchisq(x=%g, ..): not converged in %d iter.",
			 x, itrmax);
    }
#ifdef DEBUG_pnch
    REprintf("\n == L_End: n=%d; term= %g; bound=%g\n",n,term,bound);
#endif
    return (ans);
}
