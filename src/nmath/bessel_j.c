/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2000 Ross Ihaka and the R Development Core team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/*  DESCRIPTION --> see below */


/* From http://www.netlib.org/specfun/rjbesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 */
#include "nmath.h"

double bessel_j(double x, double alpha)
{
    long nb, ncalc;
    double *bj;
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    if (x < 0 || alpha < 0) {
      ML_ERROR(ME_RANGE);
      return ML_NAN;
    }
    nb = 1+ (long)floor(alpha);/* nb-1 <= alpha < nb */
    alpha -= (nb-1);
    bj = (double *) calloc(nb, sizeof(double));
    J_bessel(&x, &alpha, &nb, bj, &ncalc);
    if(ncalc != nb) {/* error input */
      if(ncalc < 0)
	MATHLIB_WARNING4("bessel_j(%g): ncalc (=%ld) != nb (=%ld); alpha=%g. Arg. out of range?\n",
			 x, ncalc, nb, alpha);
      else
	MATHLIB_WARNING2("bessel_j(%g,nu=%g): precision lost in result\n",
			 x, alpha+nb-1);
    }
    x = bj[nb-1];
    free(bj);
    return x;
}

void J_bessel(double *x, double *alpha, long *nb,
	      double *b, long *ncalc)
{

/* ---------------------------------------------------------------------

 This routine calculates Bessel functions J_(N+ALPHA) (X)
 for non-negative argument X, and non-negative order N+ALPHA.


  Explanation of variables in the calling sequence.

 X     - Non-negative argument for which
	 J's are to be calculated.
 ALPHA - Fractional part of order for which
	 J's or exponentially scaled J'r (J*exp(X)) are
	 to be calculated.  0 <= ALPHA < 1.
 NB    - Number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 B     - Output vector of length NB.  If RJBESL
	 terminates normally (NCALC=NB), the vector B contains the
	 functions J/ALPHA/(X) through J/NB-1+ALPHA/(X), or the
	 corresponding exponentially scaled functions.
 NCALC - Output variable indicating possible errors.
	 Before using the vector B, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See Error Returns below.


 *******************************************************************

  Error returns

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
       and b[N]/B(NCALC) = 10**(-K), then only the first NSIG-K
       significant figures of b[N] can be trusted.


  Intrinsic and other functions required are:

     ABS, AINT, COS, DBLE, GAMMA (or DGAMMA), INT, MAX, MIN,

     REAL, SIN, SQRT


  Acknowledgement

   This program is based on a program written by David J. Sookne
   (2) that computes values of the Bessel functions J or I of float
   argument and long order.  Modifications include the restriction
   of the computation to the J Bessel function of non-negative float
   argument, the extension of the computation to arbitrary positive
   order, and the elimination of most underflow.

  References: "A Note on Backward Recurrence Algorithms," Olver,
	       F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
	       pp 941-947.

	      "Bessel Functions of Real Argument and Integer Order,"
	       Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
	       125-132.

  Latest modification: March 19, 1990

  Author: W. J. Cody
	  Applied Mathematics Division
	  Argonne National Laboratory
	  Argonne, IL  60439
 *******************************************************************
 */

/* ---------------------------------------------------------------------
  Mathematical constants

   PI2	  - 2 / PI
   TWOPI1 - first few significant digits of 2 * PI
   TWOPI2 - (2*PI - TWOPI1) to working precision, i.e.,
	    TWOPI1 + TWOPI2 = 2 * PI to extra precision.
 --------------------------------------------------------------------- */
    static double pi2 = .636619772367581343075535;
    static double twopi1 = 6.28125;
    static double twopi2 =  .001935307179586476925286767;


/********************************************************************

  Explanation of machine-dependent constants

   it	  = Number of bits in the mantissa of a working precision
	    variable
   NSIG	  = Decimal significance desired.  Should be set to
	    INT(LOG10(2)*it+1).	 Setting NSIG lower will result
	    in decreased accuracy while setting NSIG higher will
	    increase CPU time without increasing accuracy.  The
	    truncation error is limited to a relative error of
	    T=.5*10**(-NSIG).
   ENTEN  = 10.0 ** K, where K is the largest long such that
	    ENTEN is machine-representable in working precision
   ENSIG  = 10.0 ** NSIG
   RTNSIG = 10.0 ** (-K) for the smallest long K such that
	    K >= NSIG/4
   ENMTEN = Smallest ABS(X) such that X/4 does not underflow
   XLARGE = Upper limit on the magnitude of X.	If ABS(X)=N,
	    then at least N iterations of the backward recursion
	    will be executed.  The value of 10.0 ** 4 is used on
	    every machine.


     Approximate values for some important machines are:

			    it	  NSIG	  ENTEN	      ENSIG

   CRAY-1	 (S.P.)	    48	   15	 1.0E+2465   1.0E+15
   Cyber 180/855
     under NOS	 (S.P.)	    48	   15	 1.0E+322    1.0E+15
   IEEE (IBM/XT,
     SUN, etc.)	 (S.P.)	    24	    8	 1.0E+38     1.0E+8
   IEEE (IBM/XT,
     SUN, etc.)	 (D.P.)	    53	   16	 1.0D+308    1.0D+16
   IBM 3033	 (D.P.)	    14	    5	 1.0D+75     1.0D+5
   VAX		 (S.P.)	    24	    8	 1.0E+38     1.0E+8
   VAX D-Format	 (D.P.)	    56	   17	 1.0D+38     1.0D+17
   VAX G-Format	 (D.P.)	    53	   16	 1.0D+307    1.0D+16


			   RTNSIG      ENMTEN	   XLARGE

   CRAY-1	 (S.P.)	   1.0E-4    1.84E-2466	  1.0E+4
   Cyber 180/855
     under NOS	 (S.P.)	   1.0E-4    1.25E-293	  1.0E+4
   IEEE (IBM/XT,
     SUN, etc.)	 (S.P.)	   1.0E-2    4.70E-38	  1.0E+4
   IEEE (IBM/XT,
     SUN, etc.)	 (D.P.)	   1.0E-4    8.90D-308	  1.0D+4
   IBM 3033	 (D.P.)	   1.0E-2    2.16D-78	  1.0D+4
   VAX		 (S.P.)	   1.0E-2    1.17E-38	  1.0E+4
   VAX D-Format	 (D.P.)	   1.0E-5    1.17D-38	  1.0D+4
   VAX G-Format	 (D.P.)	   1.0E-4    2.22D-308	  1.0D+4

 *******************************************************************
*/
/*---------------------------------------------------------------------
  Machine-dependent parameters
 ---------------------------------------------------------------------*/
    static double enten = 1e308;	/*NETLIB had 1e38 */
    static double ensig = 1e16;		/*NETLIB had 1e17 */
    static double rtnsig = 1e-4;
    static double enmten = 8.9e-308;	/*NETLIB had 1.2e-37 */
    static double xlarge = 1e4;

/*---------------------------------------------------------------------
 *  Factorial(N)
 *--------------------------------------------------------------------- */
    static double fact[25] = { 1.,1.,2.,6.,24.,120.,720.,5040.,40320.,
	    362880.,3628800.,39916800.,479001600.,6227020800.,87178291200.,
	    1.307674368e12,2.0922789888e13,3.55687428096e14,6.402373705728e15,
	    1.21645100408832e17,2.43290200817664e18,5.109094217170944e19,
	    1.12400072777760768e21,2.585201673888497664e22,
	    6.2044840173323943936e23 };

    extern double gamma_cody(double);

    /* Local variables */
    long nend, intx, nbmx, i, j, k, l, m, n, nstart;

    double nu, twonu, capp, capq, pold, vcos, test, vsin;
    double p, s, t, z, alpem, halfx, aa, bb, cc, psave, plast;
    double tover, t1, alp2em, em, en, xc, xk, xm, psavel, gnu, xin, sum;


    /* Parameter adjustment */
    --b;

    nu = *alpha;
    twonu = nu + nu;
/*---------------------------------------------------------------------
 Check for out of range arguments.
 ---------------------------------------------------------------------*/
    if (*nb > 0 && *x >= 0. && 0. <= nu && nu < 1.) {

	*ncalc = *nb;
	if(*x > xlarge) {
	    ML_ERROR(ME_RANGE);
	    for(i=1; i <= *nb; i++)
		b[i] = ML_POSINF;
	    return;
	}
	intx = (long) (*x);
	/* ----------------------------------------------------------
	   Initialize result array to zero.
	   -------------------------------------------------------- */
	for (i = 1; i <= *nb; ++i)
	    b[i] = 0.;

/* ---------------------------------------------------------------------
 Branch to use 2-term ascending series for small X and asymptotic
 form for large X when NB is not too large.
 --------------------------------------------------------------------- */

	if (*x < rtnsig) {
/* ---------------------------------------------------------------------
 Two-term ascending series for small X.
 --------------------------------------------------------------------- */
	    aa = 1.;
	    alpem = 1. + nu;
	    halfx = 0.;
	    if (*x > enmten) {
		halfx = .5 * *x;
	    }
	    if (nu != 0.) {
		aa = pow(halfx, nu) / (nu * gamma_cody(nu));
	    }
	    bb = 0.;
	    if (*x + 1. > 1.) {
		bb = -halfx * halfx;
	    }
	    b[1] = aa + aa * bb / alpem;
	    if (*x != 0. && b[1] == 0.) {
		*ncalc = 0;
	    }
	    if (*nb != 1) {
		if (*x <= 0.) {
		    for (n = 2; n <= *nb; ++n) {
			b[n] = 0.;
		    }
		} else {
		    /* ----------------------------------------------
		       Calculate higher order functions.
		       ---------------------------------------------- */
		    cc = halfx;
		    tover = (enmten + enmten) / *x;
		    if (bb != 0.) {
			tover = enmten / bb;
		    }
		    for (n = 2; n <= *nb; ++n) {
			aa /= alpem;
			alpem += 1.;
			aa *= cc;
			if (aa <= tover * alpem) {
			    aa = 0.;
			}
			b[n] = aa + aa * bb / alpem;
			if (b[n] == 0. && *ncalc > n) {
			    *ncalc = n - 1;
			}
		    }
		}
	    }
	} else if (*x > 25. && *nb <= intx + 1) {
/* ---------------------------------------------------------------------
 Asymptotic series for X > 25
 --------------------------------------------------------------------- */
	    xc = sqrt(pi2 / *x);
	    xin = 1 / (64 * *x * *x);
	    if (*x >= 130.)	m = 4;
	    else if (*x >= 35.) m = 8;
	    else		m = 11;
	    xm = 4. * (double) m;
	    /* ------------------------------------------------------
	       Argument reduction for SIN and COS routines.
	       ------------------------------------------------------ */
	    t = ftrunc(*x / (twopi1 + twopi2) + .5);
	    z = *x - t * twopi1 - t * twopi2 - (nu + .5) / pi2;
	    vsin = sin(z);
	    vcos = cos(z);
	    gnu = twonu;
	    for (i = 1; i <= 2; ++i) {
		s = (xm - 1. - gnu) * (xm - 1. + gnu) * xin * .5;
		t = (gnu - (xm - 3.)) * (gnu + (xm - 3.));
		capp = s * t / fact[m * 2];
		t1 = (gnu - (xm + 1.)) * (gnu + (xm + 1.));
		capq = s * t1 / fact[(m << 1) + 1];
		xk = xm;
		k = m + m;
		t1 = t;
		for (j = 2; j <= m; ++j) {
		    xk -= 4.;
		    s = (xk - 1. - gnu) * (xk - 1. + gnu);
		    t = (gnu - (xk - 3.)) * (gnu + (xk - 3.));
		    capp = (capp + 1. / fact[k - 2]) * s * t * xin;
		    capq = (capq + 1. / fact[k - 1]) * s * t1 * xin;
		    k += -2;
		    t1 = t;
		}
		capp += 1.;
		capq = (capq + 1.) * (gnu * gnu - 1.) * (.125 / *x);
		b[i] = xc * (capp * vcos - capq * vsin);
		if (*nb == 1) {
		    return;
		}
		t = vsin;
		vsin = -vcos;
		vcos = t;
		gnu += 2.;
	    }
	    /* -----------------------------------------------
	       If  NB > 2, compute J(X,ORDER+I)	 I = 2, NB-1
	       ----------------------------------------------- */
	    if (*nb > 2) {
		gnu = twonu + 2.;
		for (j = 3; j <= *nb; ++j) {
		    b[j] = gnu * b[j - 1] / *x - b[j - 2];
		    gnu += 2.;
		}
	    }
	} else {
/* ---------------------------------------------------------------------------
 rtnsig <= x <= 25 :	Use recurrence to generate results.
			First initialize the calculation of P*S.
 --------------------------------------------------------------------------- */
	    nbmx = *nb - intx;
	    n = intx + 1;
	    en = (double)(n + n) + twonu;
	    plast = 1.;
	    p = en / *x;
	    /* ---------------------------------------------------
	       Calculate general significance test.
	       --------------------------------------------------- */
	    test = ensig + ensig;
	    if (nbmx >= 3) {
		/* ------------------------------------------------------------
		   Calculate P*S until N = NB-1.  Check for possible overflow.
		   ---------------------------------------------------------- */
		tover = enten / ensig;
		nstart = intx + 2;
		nend = *nb - 1;
		en = (double) (nstart + nstart) - 2. + twonu;
		for (k = nstart; k <= nend; ++k) {
		    n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x - pold;
		    if (p > tover) {
			/* -------------------------------------------
			   To avoid overflow, divide P*S by TOVER.
			   Calculate P*S until ABS(P) > 1.
			   -------------------------------------------*/
			tover = enten;
			p /= tover;
			plast /= tover;
			psave = p;
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
			test /= ensig;
			p = plast * tover;
			--n;
			en -= 2.;
			nend = imin2(*nb,n);
			for (l = nstart; l <= nend; ++l) {
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x - pold;
			    if (psave * psavel > test) {
				*ncalc = l - 1;
				goto L190;
			    }
			}
			*ncalc = nend;
			goto L190;
		    }
		}
		n = nend;
		en = (double) (n + n) + twonu;
		/* -----------------------------------------------------
		   Calculate special significance test for NBMX > 2.
		   -----------------------------------------------------*/
		test = fmax2(test, sqrt(plast * ensig) * sqrt(p + p));
	    }
	    /* ------------------------------------------------
	       Calculate P*S until significance test passes. */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x - pold;
	    } while (p < test);

/* ---------------------------------------------------------------------
 Initialize the backward recursion and the normalization sum.
 --------------------------------------------------------------------- */
L190:
	    ++n;
	    en += 2.;
	    bb = 0.;
	    aa = 1. / p;
	    m = (n << 1) - (n / 2 << 2);
	    sum = 0.;
	    em = floor((double)n / 2);/* integer division*/
	    alpem = em - 1. + nu;
	    alp2em = em + em + nu;
	    if (m != 0) {
		sum = aa * alpem * alp2em / em;
	    }
	    nend = n - *nb;
	    if (nend > 0) {
		/* --------------------------------------------------------
		   Recur backward via difference equation, calculating
		   (but not storing) b[N], until N = NB.
		   -------------------------------------------------------- */
		for (l = 1; l <= nend; ++l, --n) {
		    en -= 2.;
		    cc = bb;
		    bb = aa;
		    aa = en * bb / *x - cc;
		    m = 2 - m;
		    if (m != 0) {
			em -= 1.;
			alp2em = em + em + nu;
			if (n == 1) {
			    break;
			}
			alpem = em - 1. + nu;
			if (alpem == 0.)
			    alpem = 1.;
			sum = (sum + aa * alp2em) * alpem / em;
		    }
		}
	    }
	    /*--------------------------------------------------
	      Store b[NB].
	      --------------------------------------------------*/
	    b[n] = aa;
	    if (nend >= 0) {
		if (*nb <= 1) {
		    alp2em = nu;
		    if (nu + 1. == 1.) {
			alp2em = 1.;
		    }
		    sum += b[1] * alp2em;
		    goto L250;
		} else {
		    /* -----------------------------------------
		       Calculate and store b[NB-1].
		       -----------------------------------------*/
		    --n;
		    en -= 2.;
		    b[n] = en * aa / *x - bb;
		    if (n == 1) {
			goto L240;
		    }
		    m = 2 - m;
		    if (m != 0) {
			em -= 1.;
			alp2em = em + em + nu;
			alpem = em - 1. + nu;
			if (alpem == 0.) {
			    alpem = 1.;
			}
			sum = (sum + b[n] * alp2em) * alpem / em;
		    }
		}
	    }
	    nend = n - 2;
	    if (nend != 0) {
/* ---------------------------------------------------------------------
 Calculate via difference equation and store b[N], until N = 2.
 --------------------------------------------------------------------- */
		for (l = 1; l <= nend; ++l) {
		    --n;
		    en -= 2.;
		    b[n] = en * b[n + 1] / *x - b[n + 2];
		    m = 2 - m;
		    if (m != 0) {
			em -= 1.;
			alp2em = em + em + nu;
			alpem = em - 1. + nu;
			if (alpem == 0.) {
			    alpem = 1.;
			}
			sum = (sum + b[n] * alp2em) * alpem / em;
		    }
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
	    if (nu + 1. != 1.)
		sum *= (gamma_cody(nu) * pow(.5* *x, -nu));

	    aa = enmten;
	    if (sum > 1.)
		aa *= sum;
	    for (n = 1; n <= *nb; ++n) {
		if (fabs(b[n]) < aa)
		    b[n] = 0.;
		else
		    b[n] /= sum;
	    }
	}

    } else {
	/* -----------------------------------------------------------------
	   Error return -- X, NB, or ALPHA is out of range.
	   -----------------------------------------------------------------*/
	b[1] = 0.;
	*ncalc = imin2(*nb,0) - 1;
    }
}
