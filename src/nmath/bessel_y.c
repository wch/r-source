/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka and the R Development Core team.
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


/* From http://www.netlib.org/specfun/rybesl	Fortran translated by f2c,...
 *	------------------------------=#----	Martin Maechler, ETH Zurich
 */
#include "Mathlib.h"

double bessel_y(double x, double alpha)
{
    long nb, ncalc;
    double *by;
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(alpha)) return x + alpha;
#endif
    nb = 1+ (long)floor(alpha);/* nb-1 <= alpha < nb */
    alpha -= (nb-1);
    by = (double *) calloc(nb, sizeof(double));
    Y_bessel(&x, &alpha, &nb, by, &ncalc);
    if(ncalc != nb) {/* error input */
	if(ncalc == -1)
	    return ML_POSINF;
	else if(ncalc < -1)
	    MATHLIB_WARNING4("bessel_y(%g): ncalc (=%d) != nb (=%d); alpha=%g. Arg. out of range?\n",
			     x, ncalc, nb, alpha);
	else /* ncalc >= 0 */
	    MATHLIB_WARNING2("bessel_y(%g,nu=%g): precision lost in result\n",
			     x, alpha+nb-1);
    }
    x = by[nb-1];
    free(by);
    return x;
}

void Y_bessel(double *x, double *alpha, long *nb,
	      double *by, long *ncalc)
{
/* ----------------------------------------------------------------------

 This routine calculates Bessel functions Y_(N+ALPHA) (X)
 for non-negative argument X, and non-negative order N+ALPHA.


 Explanation of variables in the calling sequence

 X     - Non-negative argument for which
	 Y's are to be calculated.
 ALPHA - Fractional part of order for which
	 Y's are to be calculated.  0 <= ALPHA < 1.0.
 NB    - Number of functions to be calculated, NB > 0.
	 The first function calculated is of order ALPHA, and the
	 last is of order (NB - 1 + ALPHA).
 BY    - Output vector of length NB.	If the
	 routine terminates normally (NCALC=NB), the vector BY
	 contains the functions Y(ALPHA,X), ... , Y(NB-1+ALPHA,X),
	 If (0 < NCALC < NB), BY(I) contains correct function
	 values for I <= NCALC, and contains the ratios
	 Y(ALPHA+I-1,X)/Y(ALPHA+I-2,X) for the rest of the array.
 NCALC - Output variable indicating possible errors.
	 Before using the vector BY, the user should check that
	 NCALC=NB, i.e., all orders have been calculated to
	 the desired accuracy.	See error returns below.


 *******************************************************************

 Error returns

  In case of an error, NCALC != NB, and not all Y's are
  calculated to the desired accuracy.

  NCALC < -1:  An argument is out of range. For example,
	NB <= 0, IZE is not 1 or 2, or IZE=1 and ABS(X) >=
	XMAX.  In this case, BY[0] = 0.0, the remainder of the
	BY-vector is not calculated, and NCALC is set to
	MIN0(NB,0)-2  so that NCALC != NB.
  NCALC = -1:  Y(ALPHA,X) >= XINF.  The requested function
	values are set to 0.0.
  1 < NCALC < NB: Not all requested function values could
	be calculated accurately.  BY(I) contains correct function
	values for I <= NCALC, and and the remaining NB-NCALC
	array elements contain 0.0.


 Intrinsic functions required are:

     DBLE, EXP, INT, MAX, MIN, REAL, SQRT


 Acknowledgement

	This program draws heavily on Temme's Algol program for Y(a,x)
	and Y(a+1,x) and on Campbell's programs for Y_nu(x).	Temme's
	scheme is used for  x < THRESH, and Campbell's scheme is used
	in the asymptotic region.  Segments of code from both sources
	have been translated into Fortran 77, merged, and heavily modified.
	Modifications include parameterization of machine dependencies,
	use of a new approximation for ln(gamma(x)), and built-in
	protection against over/underflow.

 References: "Bessel functions J_nu(x) and Y_nu(x) of float
	      order and float argument," Campbell, J. B.,
	      Comp. Phy. Comm. 18, 1979, pp. 133-142.

	     "On the numerical evaluation of the ordinary
	      Bessel function of the second kind," Temme,
	      N. M., J. Comput. Phys. 21, 1976, pp. 343-350.

  Latest modification: March 19, 1990

  Modified by: W. J. Cody
	       Applied Mathematics Division
	       Argonne National Laboratory
	       Argonne, IL  60439
 ----------------------------------------------------------------------*/


/* ----------------------------------------------------------------------
  Mathematical constants
    FIVPI = 5*PI
    PIM5 = 5*PI - 15
 ----------------------------------------------------------------------*/
    const static double fivpi = 15.707963267948966192;
    const static double pim5	=   .70796326794896619231;


/* *******************************************************************

 Explanation of machine-dependent constants

   beta	  = Radix for the floating-point system
   p	  = Number of significant base-beta digits in the
	    significand of a floating-point number
   minexp = Smallest representable power of beta
   maxexp = Smallest power of beta that overflows
   EPS	  = beta ** (-p)  == DBL_EPSILON
   DEL	  = Machine number below which sin(x)/x = 1; approximately SQRT(EPS).
   XMIN	  = Smallest acceptable argument for RBESY; approximately
	    max(2*beta**minexp,2/XINF), rounded up
   XINF	  = Largest positive machine number; approximately beta**maxexp
	    == DBL_MAX (defined in  #include <float.h>)
   THRESH = Lower bound for use of the asymptotic form;
	    approximately AINT(-LOG10(EPS/2.0))+1.0
   XLARGE = Upper bound on X;
	    approximately 1/DEL, because the sine and cosine functions
	    have lost about half of their precision at that point.

     Approximate values for some important machines are:

			beta	p     minexp	  maxexp      EPS

  CRAY-1	(S.P.)	  2    48     -8193	   8191	   3.55E-15
  Cyber 180/185
    under NOS	(S.P.)	  2    48      -975	   1070	   3.55E-15
  IEEE (IBM/XT,
    SUN, etc.)	(S.P.)	  2    24      -126	    128	   5.96E-8
  IEEE (IBM/XT,
    SUN, etc.)	(D.P.)	  2    53     -1022	   1024	   1.11D-16
  IBM 3033	(D.P.)	 16    14	-65	     63	   1.39D-17
  VAX		(S.P.)	  2    24      -128	    127	   5.96E-8
  VAX D-Format	(D.P.)	  2    56      -128	    127	   1.39D-17
  VAX G-Format	(D.P.)	  2    53     -1024	   1023	   1.11D-16


			 DEL	  XMIN	    XINF     THRESH  XLARGE

 CRAY-1	       (S.P.)  5.0E-8  3.67E-2466 5.45E+2465  15.0E0  2.0E7
 Cyber 180/855
   under NOS   (S.P.)  5.0E-8  6.28E-294  1.26E+322   15.0E0  2.0E7
 IEEE (IBM/XT,
   SUN, etc.)  (S.P.)  1.0E-4  2.36E-38	  3.40E+38     8.0E0  1.0E4
 IEEE (IBM/XT,
   SUN, etc.)  (D.P.)  1.0D-8  4.46D-308  1.79D+308   16.0D0  1.0D8
 IBM 3033      (D.P.)  1.0D-8  2.77D-76	  7.23D+75    17.0D0  1.0D8
 VAX	       (S.P.)  1.0E-4  1.18E-38	  1.70E+38     8.0E0  1.0E4
 VAX D-Format  (D.P.)  1.0D-9  1.18D-38	  1.70D+38    17.0D0  1.0D9
 VAX G-Format  (D.P.)  1.0D-8  2.23D-308  8.98D+307   16.0D0  1.0D8

 *******************************************************************

 ----------------------------------------------------------------------
  Machine-dependent constants
 ----------------------------------------------------------------------*/

/*    static double xmin = 4.46e-308;
 *    static double xinf = 1.79e308;
 */
    const static double del = 2.1491193328908e-8;/* x < del  <==>  sin(x)/x ~= 1 */
    const static double thresh = 16.;
    const static double xlarge = 1e8;

    /*----------------------------------------------------------------------
      Coefficients for Chebyshev polynomial expansion of
      1/gamma(1-x), abs(x) <= .5
      ----------------------------------------------------------------------*/
    const static double ch[21] = { -6.7735241822398840964e-24,
	    -6.1455180116049879894e-23,2.9017595056104745456e-21,
	    1.3639417919073099464e-19,2.3826220476859635824e-18,
	    -9.0642907957550702534e-18,-1.4943667065169001769e-15,
	    -3.3919078305362211264e-14,-1.7023776642512729175e-13,
	    9.1609750938768647911e-12,2.4230957900482704055e-10,
	    1.7451364971382984243e-9,-3.3126119768180852711e-8,
	    -8.6592079961391259661e-7,-4.9717367041957398581e-6,
	    7.6309597585908126618e-5,.0012719271366545622927,
	    .0017063050710955562222,-.07685284084478667369,
	    -.28387654227602353814,.92187029365045265648 };

    /* Local variables */
    long i, k, na;

    double alfa, div, ddiv, even, gamma, term, cosmu, sinmu,
	b, c, d, e, f, g, h, p, q, r, s, d1, d2, q0, pa,pa1, qa,qa1,
	en, en1, nu, ex,  ya,ya1, twobyx, den, odd, aye, dmu, x2, xna;

    en1 = ya = ya1 = 0;		/* -Wall */

    ex = *x;
    nu = *alpha;
    if (*nb > 0 && 0. <= nu && nu < 1.) {
	if(ex < DBL_MIN || ex > xlarge) {
	    ML_ERROR(ME_RANGE);
	    *ncalc = *nb;
	    if(ex > xlarge)  by[0]=ML_POSINF;
	    if(ex < DBL_MIN) by[0]=ML_NEGINF;
	    for(i=0; i < *nb; i++)
		by[i] = by[0];
	    return;
	}
	xna = ftrunc(nu + .5);
	na = (long) xna;
	if (na == 1) {/* <==>  .5 <= *alpha < 1	 <==>  -5. <= nu < 0 */
	    nu -= xna;
	}
	if (nu == -.5) {
	    p = M_SQRT_2dPI / sqrt(ex);
	    ya = p * sin(ex);
	    ya1 = -p * cos(ex);
	} else if (ex < 3.) {
	    /* -------------------------------------------------------------
	       Use Temme's scheme for small X
	       ------------------------------------------------------------- */
	    b = ex * .5;
	    d = -log(b);
	    f = nu * d;
	    e = pow(b, -nu);
	    if (fabs(nu) < del)
		c = M_1_PI;
	    else
		c = nu / sin(nu * M_PI);

	    /* ------------------------------------------------------------
	       Computation of sinh(f)/f
	       ------------------------------------------------------------ */
	    if (fabs(f) < 1.) {
		x2 = f * f;
		en = 19.;
		s = 1.;
		for (i = 1; i <= 9; ++i) {
		    s = s * x2 / en / (en - 1.) + 1.;
		    en -= 2.;
		}
	    } else {
		s = (e - 1. / e) * .5 / f;
	    }
	    /* --------------------------------------------------------
	       Computation of 1/gamma(1-a) using Chebyshev polynomials */
	    x2 = nu * nu * 8.;
	    aye = ch[0];
	    even = 0.;
	    alfa = ch[1];
	    odd = 0.;
	    for (i = 3; i <= 19; i += 2) {
		even = -(aye + aye + even);
		aye = -even * x2 - aye + ch[i - 1];
		odd = -(alfa + alfa + odd);
		alfa = -odd * x2 - alfa + ch[i];
	    }
	    even = (even * .5 + aye) * x2 - aye + ch[20];
	    odd = (odd + alfa) * 2.;
	    gamma = odd * nu + even;
	    /* End of computation of 1/gamma(1-a)
	       ----------------------------------------------------------- */
	    g = e * gamma;
	    e = (e + 1. / e) * .5;
	    f = 2. * c * (odd * e + even * s * d);
	    e = nu * nu;
	    p = g * c;
	    q = M_1_PI / g;
	    c = nu * M_PI_2;
	    if (fabs(c) < del)
		r = 1.;
	    else
		r = sin(c) / c;

	    r = M_PI * c * r * r;
	    c = 1.;
	    d = -b * b;
	    h = 0.;
	    ya = f + r * q;
	    ya1 = p;
	    en = 1.;

	    while (fabs(g / (1. + fabs(ya))) +
		   fabs(h / (1. + fabs(ya1))) > DBL_EPSILON) {
		f = (f * en + p + q) / (en * en - e);
		c *= (d / en);
		p /= en - nu;
		q /= en + nu;
		g = c * (f + r * q);
		h = c * p - en * g;
		ya += g;
		ya1+= h;
		en += 1.;
	    }
	    ya = -ya;
	    ya1 = -ya1 / b;
	} else if (ex < thresh) {
	    /* --------------------------------------------------------------
	       Use Temme's scheme for moderate X :  3 <= x < 16
	       -------------------------------------------------------------- */
	    c = (.5 - nu) * (.5 + nu);
	    b = ex + ex;
	    e = ex * M_1_PI * cos(nu * M_PI) / DBL_EPSILON;
	    e *= e;
	    p = 1.;
	    q = -ex;
	    r = 1. + ex * ex;
	    s = r;
	    en = 2.;
	    while (r * en * en < e) {
		en1 = en + 1.;
		d = (en - 1. + c / en) / s;
		p = (en + en - p * d) / en1;
		q = (-b + q * d) / en1;
		s = p * p + q * q;
		r *= s;
		en = en1;
	    }
	    f = p / s;
	    p = f;
	    g = -q / s;
	    q = g;
L220:
	    en -= 1.;
	    if (en > 0.) {
		r = en1 * (2. - p) - 2.;
		s = b + en1 * q;
		d = (en - 1. + c / en) / (r * r + s * s);
		p = d * r;
		q = d * s;
		e = f + 1.;
		f = p * e - g * q;
		g = q * e + p * g;
		en1 = en;
		goto L220;
	    }
	    f = 1. + f;
	    d = f * f + g * g;
	    pa = f / d;
	    qa = -g / d;
	    d = nu + .5 - p;
	    q += ex;
	    pa1 = (pa * q - qa * d) / ex;
	    qa1 = (qa * q + pa * d) / ex;
	    b = ex - M_PI_2 * (nu + .5);
	    c = cos(b);
	    s = sin(b);
	    d = M_SQRT_2dPI / sqrt(ex);
	    ya = d * (pa * s + qa * c);
	    ya1 = d * (qa1 * s - pa1 * c);
	} else { /* x > thresh */
	    /* ----------------------------------------------------------
	       Use Campbell's asymptotic scheme.
	       ---------------------------------------------------------- */
	    na = 0;
	    d1 = ftrunc(ex / fivpi);
	    i = (long) d1;
	    dmu = ex - 15. * d1 - d1 * pim5 - (*alpha + .5) * M_PI_2;
	    if (i - (i / 2 << 1) == 0) {
		cosmu = cos(dmu);
		sinmu = sin(dmu);
	    } else {
		cosmu = -cos(dmu);
		sinmu = -sin(dmu);
	    }
	    ddiv = 8. * ex;
	    dmu = *alpha;
	    den = sqrt(ex);
	    for (k = 1; k <= 2; ++k) {
		p = cosmu;
		cosmu = sinmu;
		sinmu = -p;
		d1 = (2. * dmu - 1.) * (2. * dmu + 1.);
		d2 = 0.;
		div = ddiv;
		p = 0.;
		q = 0.;
		q0 = d1 / div;
		term = q0;
		for (i = 2; i <= 20; ++i) {
		    d2 += 8.;
		    d1 -= d2;
		    div += ddiv;
		    term = -term * d1 / div;
		    p += term;
		    d2 += 8.;
		    d1 -= d2;
		    div += ddiv;
		    term *= (d1 / div);
		    q += term;
		    if (fabs(term) <= DBL_EPSILON) {
			break;
		    }
		}
		p += 1.;
		q += q0;
		if (k == 1)
		    ya = M_SQRT_2dPI * (p * cosmu - q * sinmu) / den;
		else
		    ya1 = M_SQRT_2dPI * (p * cosmu - q * sinmu) / den;
		dmu += 1.;
	    }
	}
	if (na == 1) {
	    h = 2. * (nu + 1.) / ex;
	    if (h > 1.) {
		if (fabs(ya1) > DBL_MAX / h) {
		    h = 0.;
		    ya = 0.;
		}
	    }
	    h = h * ya1 - ya;
	    ya = ya1;
	    ya1 = h;
	}

	/* ---------------------------------------------------------------
	   Now have first one or two Y's
	   --------------------------------------------------------------- */
	by[0] = ya;
	*ncalc = 1;
	if(*nb > 1) {
	    by[1] = ya1;
	    if (ya1 != 0.) {
		aye = 1. + *alpha;
		twobyx = 2. / ex;
		*ncalc = 2;
		for (i = 2; i < *nb; ++i) {
		    if (twobyx < 1.) {
			if (fabs(by[i - 1]) * twobyx >= DBL_MAX / aye)
			    goto L450;
		    } else {
			if (fabs(by[i - 1]) >= DBL_MAX / aye / twobyx)
			    goto L450;
		    }
		    by[i] = twobyx * aye * by[i - 1] - by[i - 2];
		    aye += 1.;
		    ++(*ncalc);
		}
	    }
	}
L450:
	for (i = *ncalc; i < *nb; ++i)
	    by[i] = ML_NEGINF;/* was 0 */

    } else {
	by[0] = 0.;
	*ncalc = imin2(*nb,0) - 1;
    }
}

