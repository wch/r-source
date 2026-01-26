/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2026 Ross Ihaka and the R Core team.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*  DESCRIPTION --> see below */


/* From http://www.netlib.org/specfun/rjbesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 * Additional code for nu == alpha < 0  MM
 */
#include "nmath.h"
#include "bessel.h"

#ifndef MATHLIB_STANDALONE
#include <R_ext/Memory.h>
#endif


#define min0(x, y) (((x) <= (y)) ? (x) : (y))

static void J_bessel(double *x, double *alpha, int *nb,
		     double *b, int *ncalc);

// unused now from R  -- rather R's besselJ() calls  bessel_j_ex()  below
double bessel_j(double x, double alpha)
{
    int nb, ncalc;
    double na, *bj;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_WARNING(ME_RANGE, "bessel_j");
	return ML_NAN;
    }
    // ==> x >= 0  from now on
    na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.1.2
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(((alpha - na == 0.5) ? 0 : bessel_j(x, -alpha) * cospi(alpha)) +
	       ((alpha      == na ) ? 0 : bessel_y(x, -alpha) * sinpi(alpha)));
    }
    else if (alpha > 1e7) {
	MATHLIB_WARNING(_("besselJ(x, nu): nu=%g too large for bessel_j() algorithm"),
			alpha);
	return ML_NAN;
    }
    nb = 1 + (int)na; /* nb-1 <= alpha < nb */
    alpha -= (double)(nb-1); // ==> alpha' in [0, 1)

#ifdef MATHLIB_STANDALONE
    bj = (double *) calloc(nb, sizeof(double));
    if (!bj) MATHLIB_ERROR("%s", _("bessel_j allocation error"));
#else
    const void *vmax;
    vmax = vmaxget();
    bj = (double *) R_alloc((size_t) nb, sizeof(double));
#endif
    J_bessel(&x, &alpha, &nb, bj, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4(_("bessel_j(%g): ncalc (=%d) != nb (=%d); alpha=%g. Arg. out of range?\n"),
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2(_("bessel_j(%g,nu=%g): precision lost in result\n"),
			 x, alpha+(double)nb-1);
    }
    x = bj[nb-1];
#ifdef MATHLIB_STANDALONE
    free(bj);
#else
    vmaxset(vmax);
#endif
    return x;
}

/* Called from R via math_2b() in ../main/arithmetic.c:
 * modified version of bessel_j(), accepting a work array instead of allocating one.*/
double bessel_j_ex(double x, double alpha, double *bj)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0) {
	ML_WARNING(ME_RANGE, "bessel_j");
	return ML_NAN;
    }
    // ==> x >= 0.  from now on
    double na = floor(alpha);
    if (alpha < 0) {
	/* Using Abramowitz & Stegun  9.1.2
	 * this may not be quite optimal (CPU and accuracy wise) */
	return(((alpha - na == 0.5) ? 0 : bessel_j_ex(x, -alpha, bj) * cospi(alpha)) +
	       ((alpha      == na ) ? 0 : bessel_y_ex(x, -alpha, bj) * sinpi(alpha)));
    }
    else if (alpha > 1e7) { // NB: same bound 'besselJY_max_nu' in math_2b() and ./bessel_y.c
	// FIXME: do better e.g., Bessel::BesselJ()
	MATHLIB_WARNING(_("besselJ(x, nu): nu=%g > 1e7; too large for bessel_j() algorithm"),
			alpha);
	return ML_NAN;
    }
    int ncalc, nb = 1 + (int)na; /* nb-1 <= alpha < nb */
    alpha -= (double)(nb-1); // ==> alpha' in [0, 1)
    J_bessel(&x, &alpha, &nb, bj, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4(_("bessel_j(%g): ncalc (=%d) != nb (=%d); alpha=%g. Arg. out of range?\n"),
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2(_("bessel_j(%g,nu=%g): precision lost in result\n"),
			 x, alpha+(double)nb-1);
    }
    x = bj[nb-1];
    return x;
}

static void J_bessel(double *x, double *alpha, int *nb,
		     double *b, int *ncalc)
{
/*
 Calculates Bessel functions J_{n+alpha} (x)
 for non-negative argument x, and non-negative order n+alpha, n = 0,1,..,nb-1.

  Explanation of variables in the calling sequence.

 X     - Non-negative argument for which J's are to be calculated.
 ALPHA - Fractional part of order for which
	 J's are to be calculated.  0 <= ALPHA < 1.
 NB    - Number of functions to be calculated, NB >= 1.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 B     - Output vector of length NB.  If RJBESL
	 terminates normally (NCALC=NB), the vector B contains the
	 functions J/ALPHA/(X) through J/NB-1+ALPHA/(X).
 NCALC - Output variable indicating possible errors.
	 Before using the vector B, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See the following

	 ****************************************************************

 Error return codes

    In case of an error,  NCALC != NB, and not all J's are
    calculated to the desired accuracy.

    NCALC < 0:	An argument is out of range. For example,
       NBES <= 0, ALPHA < 0 or > 1, or X is too large.
       In this case, b[1] is set to zero, the remainder of the
       B-vector is not calculated, and NCALC is set to
       MIN(NB,0)-1 so that NCALC != NB.

    NB > NCALC > 0: Not all requested function values could
       be calculated accurately.  This usually occurs because NB is
       much larger than ABS(X).	 In this case, b[N] is calculated
       to the desired accuracy for N <= NCALC, but precision
       is lost for NCALC < N <= NB.  If b[N] does not vanish
       for N > NCALC (because it is too small to be represented),
       and b[N]/b[NCALC] = 10^(-K), then only the first NSIG - K
       significant figures of b[N] can be trusted.


  Acknowledgement

	This program is based on a program written by David J. Sookne
	(2) that computes values of the Bessel functions J or I of float
	argument and long order.  Modifications include the restriction
	of the computation to the J Bessel function of non-negative float
	argument, the extension of the computation to arbitrary positive
	order, and the elimination of most underflow.

  References:

	Olver, F.W.J., and Sookne, D.J. (1972)
	"A Note on Backward Recurrence Algorithms";
	Math. Comp. 26, 941-947.

	Sookne, D.J. (1973)
	"Bessel Functions of Real Argument and Integer Order";
	NBS Jour. of Res. B. 77B, 125-132.

  Latest modification: March 19, 1990

  Author: W. J. Cody
	  Applied Mathematics Division
	  Argonne National Laboratory
	  Argonne, IL  60439
 *******************************************************************
 */

/* ---------------------------------------------------------------------
   Mathematical constants */

    const static double pi2 = .636619772367581343075535;           // pi2 = 2 / \pi

    const static double twopi1 = 6.28125;			// twopi1 = first few significant digits of 2\pi
    const static double twopi2 =  .001935307179586476925286767; /* twopi2 = (2*\pi - twopi1) to working precision, i.e.,
								 * twopi1 + twopi2 = 2 \pi to extra precision.
 --------------------------------------------------------------------- */
#define very_small_nu  0x1p-800 // 2^-800 = 1.4996968....e-241

    --b; /* so, we use  b[1] .. b[nb]  in the code below */

    double nu = *alpha, // in [0, 1)   {ensured by caller bessel_j*()}
	twonu = ldexp(nu,1); // = 2 nu = nu+nu

    /*-------------------------------------------------------------------
      Check for out of range arguments.
      -------------------------------------------------------------------*/
    if (*nb > 0 && *x >= 0. && 0. <= nu && nu < 1.) {

	int i, m, n;
	*ncalc = *nb;
	/* Initialize result array to zero. */
	for (i = 1; i <= *nb; ++i)
	    b[i] = 0.;
	if(*x > xlrg_BESS_IJ) {
	    ML_WARNING(ME_RANGE, "J_bessel");
	    /* indeed, the limit is 0; but cutoff may happen too early */
	    return;
	}
	int intx = (int) (*x);

	/*===================================================================
	  Branch into  3 cases :
	  1) use 2-term ascending series for small X
	  2) use asymptotic form for large X when NB is not too large
	  3) use recursion otherwise;
	   3b:  if 0 < |nu| = |alpha| < very_small_nu, use nu = very_small_nu
	  ===================================================================*/

	double alpem, alp2em, aa, bb, cc, p, s, en, sum, tover;

	if (*x < rtnsig_BESS) { // x < 1e-4  here
	  /* --------------------------------------------------------------- ============= branch 1)
	     Two-term ascending series for small X.
	     --------------------------------------------------------------- */

	    alpem = 1. + nu;
	    double halfx = (*x > enmten_BESS) ? .5 * *x :  0.;
	    aa	  = (nu != 0.) ? pow(halfx, nu) / (nu * Rf_gamma_cody(nu)) : 1.;
	    bb	  = (*x + 1. > 1.) ? -halfx * halfx : 0.;  // manual underflow (FIXME: unneeded for IEEE?)
	    b[1] = aa + aa * bb / alpem;
	    if (*x != 0. && b[1] == 0.)
		*ncalc = 0;

	    if (*nb != 1) {
		if (*x <= 0.) {
		    for (n = 2; n <= *nb; ++n)
			b[n] = 0.;
		}
		else {
		    /* ----------------------------------------------
		       Calculate higher order functions.
		       ---------------------------------------------- */
		    if (bb == 0.)
			tover = (enmten_BESS + enmten_BESS) / *x;
		    else
			tover = enmten_BESS / bb;
		    cc = halfx;
		    for (n = 2; n <= *nb; ++n) {
			aa /= alpem;
			alpem += 1.;
			aa *= cc;
			if (aa <= tover * alpem)
			    aa = 0.;

			b[n] = aa + aa * bb / alpem;
			if (b[n] == 0. && *ncalc > n)
			    *ncalc = n - 1;
		    }
		}
	    }
	} else if (*x > 25. && *nb <= intx + 1) {
	    /* ------------------------------------------------------------ ============= branch 2)
	       Asymptotic series for X > 25 (and not much larger nb)
	       ------------------------------------------------------------ */
	    // m := #{terms in asymptotic series} to be used
	    if (*x >= 130.)	m = 4;
	    else if (*x >= 35.) m = 8;
	    else		m = 11; // ==> k := 2m <= 22  <==> length(fact[]) >= 23

	    /*---------------------------------------------------------------------
	     *  Factorial(N)
	     *--------------------------------------------------------------------- */
	    const static double fact[25] =
		{ 1.,1.,2.,6.,24.,120.,720.,5040.,40320.,
		  362880.,3628800.,39916800.,479001600.,6227020800.,87178291200.,
		  1.307674368e12,2.0922789888e13,3.55687428096e14,6.402373705728e15,
		  1.21645100408832e17,2.43290200817664e18,5.109094217170944e19,
		  1.12400072777760768e21,2.585201673888497664e22,
		  6.2044840173323943936e23 };
	    double xc = sqrt(pi2 / *x),
		xin = 1 / (64 * *x * *x),
		xm = 4. * (double) m,
	    /* ------------------------------------------------
	       Argument reduction for SIN and COS routines.
	       ------------------------------------------------ */
		t = trunc(*x / (twopi1 + twopi2) + .5),
		z = (*x - t * twopi1) - t * twopi2 - (nu + .5) / pi2,
		vsin = sin(z),
		vcos = cos(z),
		gnu = twonu;
	    for (i = 1; i <= 2; ++i) {
		s = (xm - 1. - gnu) * (xm - 1. + gnu) * xin * .5;
		t = (gnu - (xm - 3.)) * (gnu + (xm - 3.));
		int k = m + m;
		double t1 = (gnu - (xm + 1.)) * (gnu + (xm + 1.)),
		    capp = s * t / fact[k],
		    capq = s * t1/ fact[k + 1],
		    xk = xm;
		for (; k >= 4; k -= 2) {/* k + 2(j-2) == 2m,  for j = 1,..,  */
		    xk -= 4.;
		    s = (xk - 1. - gnu) * (xk - 1. + gnu);
		    t1 = t;
		    t = (gnu - (xk - 3.)) * (gnu + (xk - 3.));
		    capp = (capp + 1. / fact[k - 2]) * s * t  * xin;
		    capq = (capq + 1. / fact[k - 1]) * s * t1 * xin;
		}
		capp += 1.;
		capq = (capq + 1.) * (gnu * gnu - 1.) * (.125 / *x);
		b[i] = xc * (capp * vcos - capq * vsin);
		if (*nb == 1)
		    return; // result:  b[i] = b[1]

		/* vsin <--> vcos */ t = vsin; vsin = -vcos; vcos = t;
		gnu += 2.;
	    } // end  for i = 1,2
	    /* -----------------------------------------------
	       If  NB > 2, compute J(X,ORDER+I)	for I = 2,.., NB-1
	       ----------------------------------------------- */
	    if (*nb > 2)
		for (gnu = twonu + 2., i = 3; i <= *nb; i++, gnu += 2.)
		    b[i] = gnu * b[i - 1] / *x - b[i - 2];
	}
	else {
	    /* rtnsig_BESS <= x && ( x <= 25 || intx+1 < *nb ) :
	       -------------------------------------------------------- ============= branch 3)
	       Use recurrence to generate results.
	       First initialize the calculation of P*S.
	       -------------------------------------------------------- */

	    if(nu != 0. && fabs(nu) < very_small_nu) {
		nu = (nu < 0.) ? -very_small_nu : very_small_nu; // in R <= 4.5.2  besselJ(2, 2e-16) gave 1.119e+15
		twonu = ldexp(nu, 1);
	    }

	    int nbmx = *nb - intx; // = nb - floor(x)
	    n = intx + 1;
	    en = (double)(n + n) + twonu;
	    p = en / *x;
	    /* ---------------------------------------------------
	       Calculate general significance test.
	       --------------------------------------------------- */
	    double plast = 1., pold,
		test = ensig_BESS + ensig_BESS;
	    if (nbmx >= 3) {
		/* ------------------------------------------------------------
		   Calculate P*S until N = NB-1.  Check for possible overflow.
		   ---------------------------------------------------------- */
		tover = enten_BESS / ensig_BESS;
		int nstart = intx + 2,
		    nend = *nb - 1;
		en = (double) (nstart + nstart) - 2. + twonu;
		for (int k = nstart; k <= nend; ++k) {
		    int n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x - pold;
		    if (p > tover) {
			/* -------------------------------------------
			   To avoid overflow, divide P*S by TOVER.
			   Calculate P*S until ABS(P) > 1.
			   -------------------------------------------*/
			tover = enten_BESS;
			p /= tover;
			plast /= tover;
			double psave = p,
			    psavel = plast;
			nstart = n + 1;
			do {
			    ++n;
			    en += 2.;
			    pold = plast;
			    plast = p;
			    p = en * plast / *x - pold;
			} while (p <= 1.);

			bb = en / *x;
			/* -----------------------------------------------
			   Calculate backward test and find NCALC,
			   the highest N such that the test is passed.
			   ----------------------------------------------- */
			test = pold * plast * (.5 - .5 / (bb * bb));
			test /= ensig_BESS;
			p = plast * tover;
			--n;
			en -= 2.;
			nend = min0(*nb,n);
			for (int i = nstart; i <= nend; ++i) {
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x - pold;
			    if (psave * psavel > test) {
				*ncalc = i - 1;
				goto L190;
			    }
			}
			*ncalc = nend;
			goto L190;
		    } // p > tover
		} // for(k ..)
		/* get here only if *never* (p > tover) above */
		n = nend;
		en = (double) (n + n) + twonu;
		/* -----------------------------------------------------
		   Calculate special significance test for NBMX > 2.
		   -----------------------------------------------------*/
		test = fmax2(test, sqrt(plast * ensig_BESS) * sqrt(p + p));
	    } // end if{ nbmx >= 3 }
	    /* ------------------------------------------------
	       Calculate P*S until significance test passes. */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x - pold;
	    } while (p < test);

L190:
	    /*---------------------------------------------------------------
	      Initialize the backward recursion and the normalization sum.
	      --------------------------------------------------------------- */
	    ++n;
	    en += 2.;
	    bb = 0.;
	    aa = 1. / p;
	    m = n / 2;
	    double em = (double)m;
	    m = (n << 1) - (m << 2);/* = 2 n - 4 (n/2)
				       = 0 for even, 2 for odd n */
	    if (m == 0)
		sum = 0.;
	    else {
		alpem = em - 1. + nu;
		alp2em = em + em + nu;
		sum = aa * alpem * alp2em / em;
	    }
	    int nend = n - *nb;
	    /* if (nend > 0) */
	    /* --------------------------------------------------------
	       Recur backward via difference equation, calculating
	       (but not storing) b[N], until N = NB.
	       -------------------------------------------------------- */
	    for (int i = 0; i < nend; ++i) {
		--n;
		en -= 2.;
		cc = bb;
		bb = aa;
		aa = en * bb / *x - cc;
		m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		if (m != 0) {
		    em -= 1.;
		    alp2em = em + em + nu;
		    if (n == 1)
			break;

		    alpem = em - 1. + nu;
		    if (alpem == 0.)
			alpem = 1.;
		    sum = (sum + aa * alp2em) * alpem / em;
		}
	    } //-> new n := n - (n - nb) = nb
	    /*--------------------------------------------------
	      Store b[NB].
	      --------------------------------------------------*/
	    b[n] = aa;
	    if (nend >= 0) {
		if (n <= 1) {
		    sum += b[1] * ((nu == 0.) ? 1. : nu); // as |nu| >=  very_small_nu
		    goto L250;
		}
		else {/*-- nb >= 2 : ---------------------------
			Calculate and store b[NB-1].
			----------------------------------------*/
		    --n; // => n = nb-1
		    en -= 2.;
		    b[n] = en * aa / *x - bb;
		    if (n == 1)
			goto L240;

		    m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		    if (m != 0) {
			em -= 1.;
			alp2em = em + em + nu;
			alpem = em - 1. + nu;
			if (alpem == 0.)
			    alpem = 1.;
			sum = (sum + b[n] * alp2em) * alpem / em;
		    }
		}
	    }

	    /* if (n - 2 != 0) */
	    /* --------------------------------------------------------
	       Calculate via difference equation and store b[N],
	       until N = 2.
	       -------------------------------------------------------- */
	    for (n = n-1; n >= 2; n--) {
		en -= 2.;
		b[n] = en * b[n + 1] / *x - b[n + 2];
		m = m ? 0 : 2; /* m = 2 - m failed on gcc4-20041019 */
		if (m != 0) {
		    em -= 1.;
		    alp2em = em + em + nu;
		    alpem = em - 1. + nu;
		    if (alpem == 0.)
			alpem = 1.;
		    sum = (sum + b[n] * alp2em) * alpem / em;
		}
	    }
	    /* ---------------------------------------
	       Calculate b[1].
	       -----------------------------------------*/
	    b[1] = 2. * (nu + 1.) * b[2] / *x - b[3];

L240:
	    em -= 1.;
	    alp2em = em + em + nu;
	    if (alp2em == 0.)
		alp2em = 1.;
	    sum += b[1] * alp2em;

L250:
	    /* ---------------------------------------------------
	       Normalize.  Divide all b[N] by sum.
	       ---------------------------------------------------*/
	    // NB. ensured above that |nu| >= very_small_nu
	    if(nu != 0.) { /* was if(fabs(nu) > very_small_nu) , was if(nu + 1. != 1.); then '> 1e-15' .. */
		sum *= (Rf_gamma_cody(nu) * pow(.5* *x, -nu));
	    }

#ifdef UNDERFLOW_NOT_GOOD_ENOUGH
	    aa = enmten_BESS; // 8.9e-308 (for R in ./bessel.h)
	    if (sum > 1.)
		aa *= sum;
#endif
	    for (n = 1; n <= *nb; ++n) {
#ifdef UNDERFLOW_NOT_GOOD_ENOUGH
		if (fabs(b[n]) < aa)
		    b[n] = 0.;
		else
#endif
		    b[n] /= sum;
	    }
	}

    }
    else {
      /* Error return -- X = *x, NB = *nb, or ALPHA = nu is out of range
	 -- should never happen when called from bessel_j_ex() above, called from R: */
	b[1] = 0.;
	*ncalc = min0(*nb,0) - 1; //  <=  -1
    }
}
