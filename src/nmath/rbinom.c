/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double rbinom(double nin, double pp)
 *    
 *  DESCRIPTION
 *
 *    Random variates from the binomial distribution.
 *
 *  REFERENCE
 *
 *    Kachitvichyanukul, V. and Schmeiser, B. W. (1988).
 *    Binomial random variate generation.
 *    Communications of the ACM 31, p216.
 *    (Algorithm BTPEC).
 */

#include "Mathlib.h"
#include <stdlib.h>

#define repeat for(;;)

double rbinom(double nin, double pp)
{
    static double al, alv, amaxp, c, f, f1, f2, ffm, fm, g;
    static double p1, p2, p3, p4, qn, r, u, v, w, w2;
    static double x, x1, x2, xl, xll, xlr, xm, xnp, xnpq, xr, ynorm, z, z2;
    static int i, ix, ix1, k, m, mp, n;
    static double p, q;
    static double psave = -1.0;
    static int nsave = -1;

    n = floor(nin + 0.5);
    /* n=0, p=0, p=1 are not errors <TSL>*/
    if (
#ifdef IEEE_754
	!finite(n) || !finite(pp) ||
#endif
	n < 0.0 || pp < 0.0 || pp > 1.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (n==0.0 || pp==0) return 0;
    if (pp==1.0) return n;

    /* setup, perform only when parameters change */

    if (pp != psave) {
	psave = pp;
	p = fmin2(psave, 1.0 - psave);
	q = 1.0 - p;
    } else if (n == nsave) {
	if (xnp < 30.0)
	    goto L20;
	goto L10;
    }
    xnp = n * p;
    nsave = n;
    if (xnp < 30.0) {
	/* inverse cdf logic for mean less than 30 */
	qn = pow(q, (double) n);
	r = p / q;
	g = r * (n + 1);
	goto L20;
    } else {
	ffm = xnp + p;
	m = ffm;
	fm = m;
	xnpq = xnp * q;
	p1 = (int)(2.195 * sqrt(xnpq) - 4.6 * q) + 0.5;
	xm = fm + 0.5;
	xl = xm - p1;
	xr = xm + p1;
	c = 0.134 + 20.5 / (15.3 + fm);
	al = (ffm - xl) / (ffm - xl * p);
	xll = al * (1.0 + 0.5 * al);
	al = (xr - ffm) / (xr * q);
	xlr = al * (1.0 + 0.5 * al);
	p2 = p1 * (1.0 + c + c);
	p3 = p2 + c / xll;
	p4 = p3 + c / xlr;
    }
  L10:repeat {
      u = sunif() * p4;
      v = sunif();
      /* triangular region */
      if (u <= p1) {
	  ix = xm - p1 * v + u;
	  goto L30;
      }
      /* parallelogram region */
      if (u <= p2) {
	  x = xl + (u - p1) / c;
	  v = v * c + 1.0 - fabs(xm - x) / p1;
	  if (v > 1.0 || v <= 0.)
	      continue;
	  ix = x;
      } else {
	  if (u > p3) {	/* right tail */
	      ix = xr - log(v) / xlr;
	      if (ix > n)
		  continue;
	      v = v * (u - p3) * xlr;
	  } else {/* left tail */
	      ix = xl + log(v) / xll;
	      if (ix < 0)
		  continue;
	      v = v * (u - p2) * xll;
	  }
      }
      /* determine appropriate way to perform accept/reject test */
      k = abs(ix - m);
      if (k <= 20 || k >= xnpq / 2 - 1) {
	  /* explicit evaluation */
	  f = 1.0;
	  r = p / q;
	  g = (n + 1) * r;
	  if (m < ix) {
	      mp = m + 1;
	      for (i = mp; i <= ix; i++)
		  f = f * (g / i - r);
	  } else if (m != ix) {
	      ix1 = ix + 1;
	      for (i = ix1; i <= m; i++)
		  f = f / (g / i - r);
	  }
	  if (v <= f)
	      goto L30;
      } else {
	  /* squeezing using upper and lower bounds */
	  /* on log(f(x)) */
	  amaxp = (k / xnpq) * ((k * (k / 3.0 + 0.625) + 0.1666666666666) / xnpq + 0.5);
	  ynorm = -k * k / (2.0 * xnpq);
	  alv = log(v);
	  if (alv < ynorm - amaxp)
	      goto L30;
	  if (alv <= ynorm + amaxp) {
				/* stirling's formula to machine accuracy */
				/* for the final acceptance/rejection test */
	      x1 = ix + 1;
	      f1 = fm + 1.0;
	      z = n + 1 - fm;
	      w = n - ix + 1.0;
	      z2 = z * z;
	      x2 = x1 * x1;
	      f2 = f1 * f1;
	      w2 = w * w;
	      if (alv <= xm * log(f1 / x1) + (n - m + 0.5) * log(z / w) + (ix - m) * log(w * p / x1 * q) + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / f2) / f2) / f2) / f2) / f1 / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / z2) / z2) / z2) / z2) / z / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / x2) / x2) / x2) / x2) / x1 / 166320.0 + (13860.0 - (462.0 - (132.0 - (99.0 - 140.0 / w2) / w2) / w2) / w2) / w / 166320.)
		  goto L30;
	  }
      }
  }
  L20:repeat {
      ix = 0;
      f = qn;
      u = sunif();
      repeat {
	  if (u < f)
	      goto L30;
	  if (ix > 110)
	      break;
	  u = u - f;
	  ix = ix + 1;
	  f = f * (g / ix - r);
      }
  }
  L30:if (psave > 0.5)
	 ix = n - ix;
  return (double)ix;
}
