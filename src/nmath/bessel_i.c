/* ribesl.f -- translated by f2c (version 19960514).
 */

#include "Mathlib.h"
#include "Error.h"

static double exparg = 709.;/* maximal x for UNscaled answer, see below */

double bessel_i(double x, double alpha, double expo) {
    long nb=1, ncalc=1, ize;
    double b[1];
    ize = (long)expo;
    I_bessel(&x, &alpha, &nb, &ize, b, &ncalc);
    if(ncalc != nb) {/* error input */

    }
    return b[0];
}

void I_bessel(double *x, double *alpha, long *nb,
	      long *ize, double *b, long *ncalc)
{
/* -------------------------------------------------------------------

  This routine calculates Bessel functions I SUB(N+ALPHA) (X)
  for non-negative argument X, and non-negative order N+ALPHA,
  with or without exponential scaling.


 Explanation of variables in the calling sequence

 X     - Working precision non-negative float argument for which
	 I's or exponentially scaled I's (I*EXP(-X))
	 are to be calculated.	If I's are to be calculated,
	 X must be less than EXPARG (see below).
 ALPHA - Working precision fractional part of order for which
	 I's or exponentially scaled I's (I*EXP(-X)) are
	 to be calculated.  0 <= ALPHA < 1.0.
 NB    - Integer number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 IZE   - Integer type.	IZE = 1 if unscaled I's are to calculated,
	 and 2 if exponentially scaled I's are to be calculated.
 B     - Working precision output vector of length NB.	If the routine
	 terminates normally (NCALC=NB), the vector B contains the
	 functions I(ALPHA,X) through I(NB-1+ALPHA,X), or the
	 corresponding exponentially scaled functions.
 NCALC - Integer output variable indicating possible errors.
	 Before using the vector B, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See error returns below.


 *******************************************************************
 *******************************************************************

 Error returns

  In case of an error,	NCALC != NB, and not all I's are
  calculated to the desired accuracy.

  NCALC < 0:  An argument is out of range. For example,
     NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >= EXPARG.
     In this case, the B-vector is not calculated, and NCALC is
     set to MIN0(NB,0)-1 so that NCALC != NB.

  NB > NCALC > 0: Not all requested function values could
     be calculated accurately.	This usually occurs because NB is
     much larger than ABS(X).  In this case, B(N) is calculated
     to the desired accuracy for N <= NCALC, but precision
     is lost for NCALC < N <= NB.  If B(N) does not vanish
     for N > NCALC (because it is too small to be represented),
     and B(N)/B(NCALC) = 10**(-K), then only the first NSIG-K
     significant figures of B(N) can be trusted.


 Intrinsic functions required are:

     DBLE, EXP, gamma_cody, INT, MAX, MIN, REAL, SQRT


 Acknowledgement

  This program is based on a program written by David J.
  Sookne (2) that computes values of the Bessel functions J or
  I of float argument and long order.  Modifications include
  the restriction of the computation to the I Bessel function
  of non-negative float argument, the extension of the computation
  to arbitrary positive order, the inclusion of optional
  exponential scaling, and the elimination of most underflow.
  An earlier version was published in (3).

 References: "A Note on Backward Recurrence Algorithms," Olver,
	      F. W. J., and Sookne, D. J., Math. Comp. 26, 1972,
	      pp 941-947.

	     "Bessel Functions of Real Argument and Integer Order,"
	      Sookne, D. J., NBS Jour. of Res. B. 77B, 1973, pp
	      125-132.

	     "ALGORITHM 597, Sequence of Modified Bessel Functions
	      of the First Kind," Cody, W. J., Trans. Math. Soft.,
	      1983, pp. 242-245.

  Latest modification: May 30, 1989

  Modified by: W. J. Cody and L. Stoltz
	       Applied Mathematics Division
	       Argonne National Laboratory
	       Argonne, IL  60439
*/

    /*-------------------------------------------------------------------
      Mathematical constants
      -------------------------------------------------------------------*/
    static double const__ = 1.585;

/* *******************************************************************

 Explanation of machine-dependent constants

   beta	  = Radix for the floating-point system
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows
   it	  = Number of bits in the mantissa of a working precision variable
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
   XLARGE = Upper limit on the magnitude of X when IZE=2.  Bear
	    in mind that if ABS(X)=N, then at least N iterations
	    of the backward recursion will be executed.	 The value
	    of 10.0 ** 4 is used on every machine.
   EXPARG = Largest working precision argument that the library
	    EXP routine can handle and upper limit on the
	    magnitude of X when IZE=1; approximately
	    LOG(beta**maxexp)


     Approximate values for some important machines are:

			beta	   minexp      maxexp	    it

  CRAY-1	(S.P.)	  2	   -8193	8191	    48
  Cyber 180/855
    under NOS	(S.P.)	  2	    -975	1070	    48
  IEEE (IBM/XT,
    SUN, etc.)	(S.P.)	  2	    -126	 128	    24
  IEEE (IBM/XT,
    SUN, etc.)	(D.P.)	  2	   -1022	1024	    53
  IBM 3033	(D.P.)	 16	     -65	  63	    14
  VAX		(S.P.)	  2	    -128	 127	    24
  VAX D-Format	(D.P.)	  2	    -128	 127	    56
  VAX G-Format	(D.P.)	  2	   -1024	1023	    53


			NSIG	   ENTEN       ENSIG	  RTNSIG

 CRAY-1	       (S.P.)	 15	  1.0E+2465   1.0E+15	  1.0E-4
 Cyber 180/855
   under NOS   (S.P.)	 15	  1.0E+322    1.0E+15	  1.0E-4
 IEEE (IBM/XT,
   SUN, etc.)  (S.P.)	  8	  1.0E+38     1.0E+8	  1.0E-2
 IEEE (IBM/XT,
   SUN, etc.)  (D.P.)	 16	  1.0D+308    1.0D+16	  1.0D-4
 IBM 3033      (D.P.)	  5	  1.0D+75     1.0D+5	  1.0D-2
 VAX	       (S.P.)	  8	  1.0E+38     1.0E+8	  1.0E-2
 VAX D-Format  (D.P.)	 17	  1.0D+38     1.0D+17	  1.0D-5
 VAX G-Format  (D.P.)	 16	  1.0D+307    1.0D+16	  1.0D-4


			 ENMTEN	     XLARGE   EXPARG

 CRAY-1	       (S.P.)	1.84E-2466   1.0E+4    5677
 Cyber 180/855
   under NOS   (S.P.)	1.25E-293    1.0E+4	741
 IEEE (IBM/XT,
   SUN, etc.)  (S.P.)	4.70E-38     1.0E+4	 88
 IEEE (IBM/XT,
   SUN, etc.)  (D.P.)	8.90D-308    1.0D+4	709
 IBM 3033      (D.P.)	2.16D-78     1.0D+4	174
 VAX	       (S.P.)	1.17E-38     1.0E+4	 88
 VAX D-Format  (D.P.)	1.17D-38     1.0D+4	 88
 VAX G-Format  (D.P.)	2.22D-308    1.0D+4	709

 *******************************************************************
 -------------------------------------------------------------------
  Machine-dependent parameters
 -------------------------------------------------------------------
*/
    static double ensig = 1e16;
    static double rtnsig = 1e-4;
    static double enmten = 8.9e-308;
    static long nsig = 16;
    static double xlarge = 1e4;
    static double enten = 1e308;

    extern double gamma_cody(double);/*--> ./gamma.c */

    /* Builtin functions */
    double pow_di(double *, long *);

    /* Local variables */
    long nend, magx, nbmx, k, l, n, nstart;
    double pold, test,	p, em, en, empal, emp2al, halfx,
	tempa, tempb, tempc, psave, plast, tover, psavel, sum;

    /*Parameter adjustments */
    --b;

    /*-------------------------------------------------------------------
      Check for X, NB, OR IZE out of range.
      ------------------------------------------------------------------- */
    if (*nb > 0 && *x >= 0. &&	0. <= *alpha && *alpha < 1. &&
	((*ize == 1 && *x <= exparg) ||
	 (*ize == 2 && *x <= xlarge))) {
	/* ----------------------------------------------------------
	   Use 2-term ascending series for small X
	   ----------------------------------------------------------*/
	*ncalc = *nb;
	magx = (long) (*x);
	if (*x >= rtnsig) {
/* -------------------------------------------------------------------
   Initialize the forward sweep, the P-sequence of Olver
   ------------------------------------------------------------------- */
	    nbmx = *nb - magx;
	    n = magx + 1;
	    en = (double) (n + n) + (*alpha + *alpha);
	    plast = 1.;
	    p = en / *x;
/* -------------------------------------------------------------------
 Calculate general significance test
 ------------------------------------------------------------------- */
	    test = ensig + ensig;
	    if (magx << 1 > nsig * 5) {
		test = sqrt(test * p);
	    } else {
		test /= pow_di(&const__, &magx);
	    }
	    if (nbmx >= 3) {
/* -------------------------------------------------------------------
 Calculate P-sequence until N = NB-1.  Check for possible overflow.
 ------------------------------------------------------------------- */
		tover = enten / ensig;
		nstart = magx + 2;
		nend = *nb - 1;
		for (k = nstart; k <= nend; ++k) {
		    n = k;
		    en += 2.;
		    pold = plast;
		    plast = p;
		    p = en * plast / *x + pold;
		    if (p > tover) {
/* -------------------------------------------------------------------
 To avoid overflow, divide P-sequence by TOVER.	 Calculate
 P-sequence until ABS(P) > 1.
 ------------------------------------------------------------------- */
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
			    p = en * plast / *x + pold;
			}
			while (p <= 1.);

			tempb = en / *x;
/* -------------------------------------------------------------------
 Calculate backward test, and find NCALC, the highest N
 such that the test is passed.
 ------------------------------------------------------------------- */
			test = pold * plast / ensig;
			test *= .5 - .5 / (tempb * tempb);
			p = plast * tover;
			--n;
			en -= 2.;
			nend = imin2(*nb,n);
			for (l = nstart; l <= nend; ++l) {
			    *ncalc = l;
			    pold = psavel;
			    psavel = psave;
			    psave = en * psavel / *x + pold;
			    if (psave * psavel > test) {
				goto L90;
			    }
			}
			*ncalc = nend + 1;
L90:
			--(*ncalc);
			goto L120;
		    }
		}
		n = nend;
		en = (double)(n + n) + (*alpha + *alpha);
		/*---------------------------------------------------
		  Calculate special significance test for NBMX > 2.
		  --------------------------------------------------- */
		test = fmax2(test,sqrt(plast * ensig) * sqrt(p + p));
	    }
	    /* --------------------------------------------------------
	       Calculate P-sequence until significance test passed.
	       -------------------------------------------------------- */
	    do {
		++n;
		en += 2.;
		pold = plast;
		plast = p;
		p = en * plast / *x + pold;
	    } while (p < test);

/* -------------------------------------------------------------------
 Initialize the backward recursion and the normalization sum.
 ------------------------------------------------------------------- */
L120:
	    ++n;
	    en += 2.;
	    tempb = 0.;
	    tempa = 1. / p;
	    em = (double) n - 1.;
	    empal = em + *alpha;
	    emp2al = em - 1. + (*alpha + *alpha);
	    sum = tempa * empal * emp2al / em;
	    nend = n - *nb;
	    if (nend < 0) {
/* -------------------------------------------------------------------
 N < NB, so store B(N) and set higher orders to 0..
 ------------------------------------------------------------------- */
		b[n] = tempa;
		nend = -nend;
		for (l = 1; l <= nend; ++l) {
		    b[n + l] = 0.;
		}
	    } else {
		if (nend > 0) {
/* -------------------------------------------------------------------
 Recur backward via difference equation, calculating (but
 not storing) B(N), until N = NB.
 ------------------------------------------------------------------- */
		    for (l = 1; l <= nend; ++l) {
			--n;
			en -= 2.;
			tempc = tempb;
			tempb = tempa;
			tempa = en * tempb / *x + tempc;
			em -= 1.;
			emp2al -= 1.;
			if (n == 1) {
			    break;
			}
			if (n == 2) {
			    emp2al = 1.;
			}
			empal -= 1.;
			sum = (sum + tempa * empal) * emp2al / em;
		    }
		}
		/* ---------------------------------------------------
		   Store B(NB)
		   --------------------------------------------------- */
		b[n] = tempa;
		if (*nb <= 1) {
		    sum = sum + sum + tempa;
		    goto L230;
		}
		/* -------------------------------------------------
		   Calculate and Store B(NB-1)
		   ------------------------------------------------- */
		--n;
		en -= 2.;
		b[n] = en * tempa / *x + tempb;
		if (n == 1) {
		    goto L220;
		}
		em -= 1.;
		emp2al -= 1.;
		if (n == 2) {
		    emp2al = 1.;
		}
		empal -= 1.;
		sum = (sum + b[n] * empal) * emp2al / em;
	    }
	    nend = n - 2;
	    if (nend > 0) {
/* -------------------------------------------------------------------
 Calculate via difference equation and store B(N), until N = 2.
 ------------------------------------------------------------------- */
		for (l = 1; l <= nend; ++l) {
		    --n;
		    en -= 2.;
		    b[n] = en * b[n + 1] / *x + b[n + 2];
		    em -= 1.;
		    emp2al -= 1.;
		    if (n == 2) {
			emp2al = 1.;
		    }
		    empal -= 1.;
		    sum = (sum + b[n] * empal) * emp2al / em;
		}
	    }
/* -------------------------------------------------------------------
 Calculate B(1)
 ------------------------------------------------------------------- */
	    b[1] = 2. * empal * b[2] / *x + b[3];
L220:
	    sum = sum + sum + b[1];
/* -------------------------------------------------------------------
 Normalize.  Divide all B(N) by sum.
 ------------------------------------------------------------------- */
L230:
	    if (*alpha != 0.) {
		sum *= (gamma_cody(1. + *alpha) * pow(*x * .5, -(*alpha)));
	    }
	    if (*ize == 1) {
		sum *= exp(-(*x));
	    }
	    tempa = enmten;
	    if (sum > 1.) {
		tempa *= sum;
	    }
	    for (n = 1; n <= *nb; ++n) {
		if (b[n] < tempa)
		    b[n] = 0.;
		else
		    b[n] /= sum;
	    }
	    return;
	} else {
	    /* -----------------------------------------------------------
	       Two-term ascending series for small X.
	       -----------------------------------------------------------*/
	    tempa = 1.;
	    empal = 1. + *alpha;
	    halfx = 0.;
	    if (*x > enmten)
		halfx = .5 * *x;
	    if (*alpha != 0.)
		tempa = pow(halfx, *alpha) / gamma_cody(empal);
	    if (*ize == 2)
		tempa *= exp(-(*x));
	    tempb = 0.;
	    if (*x + 1. > 1.)
		tempb = halfx * halfx;
	    b[1] = tempa + tempa * tempb / empal;
	    if (*x != 0. && b[1] == 0.)
		*ncalc = 0;
	    if (*nb > 1) {
		if (*x == 0.) {
		    for (n = 2; n <= *nb; ++n) {
			b[n] = 0.;
		    }
		} else {
		    /* -------------------------------------------------
		       Calculate higher-order functions.
		       ------------------------------------------------- */
		    tempc = halfx;
		    tover = (enmten + enmten) / *x;
		    if (tempb != 0.)
			tover = enmten / tempb;
		    for (n = 2; n <= *nb; ++n) {
			tempa /= empal;
			empal += 1.;
			tempa *= tempc;
			if (tempa <= tover * empal)
			    tempa = 0.;
			b[n] = tempa + tempa * tempb / empal;
			if (b[n] == 0. && *ncalc > n)
			    *ncalc = n - 1;
		    }
		}
	    }
	}
    } else {
	*ncalc = imin2(*nb,0) - 1;
    }
}
