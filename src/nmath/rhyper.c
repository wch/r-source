/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-2015 The R Core Team
 *  Copyright (C) 2005-2015 The R Foundation
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
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double rhyper(double NR, double NB, double n);
 *
 *  DESCRIPTION
 *
 *    Random variates from the hypergeometric distribution.
 *    Returns the number of white balls drawn when kk balls
 *    are drawn at random from an urn containing nn1 white
 *    and nn2 black balls.
 *
 *  REFERENCE
 *
 *    V. Kachitvichyanukul and B. Schmeiser (1985).
 *    ``Computer generation of hypergeometric random variates,''
 *    Journal of Statistical Computation and Simulation 22, 127-145.
 *
 *    The original algorithm had a bug -- R bug report PR#7314 --
 *    giving numbers slightly too small in case III h2pe
 *    where (m < 100 || ix <= 50) , see below.
 */

#include "nmath.h"
#include "dpq.h"
#include <limits.h>

// afc(i) :=  ln( i! )	[logarithm of the factorial i]
static double afc(int i)
{
    // If (i > 7), use Stirling's approximation, otherwise use table lookup.
    const static double al[8] =
    {
	0.0,/*ln(0!)=ln(1)*/
	0.0,/*ln(1!)=ln(1)*/
	0.69314718055994530941723212145817,/*ln(2) */
	1.79175946922805500081247735838070,/*ln(6) */
	3.17805383034794561964694160129705,/*ln(24)*/
	4.78749174278204599424770093452324,
	6.57925121201010099506017829290394,
	8.52516136106541430016553103634712
	/* 10.60460290274525022841722740072165, approx. value below =
	   10.6046028788027; rel.error = 2.26 10^{-9}

	  FIXME: Use constants and if(n > ..) decisions from ./stirlerr.c
	  -----  will be even *faster* for n > 500 (or so)
	*/
    };

    if (i < 0) {
	MATHLIB_WARNING(("rhyper.c: afc(i), i=%d < 0 -- SHOULD NOT HAPPEN!\n"), i);
	return -1; // unreached
    }
    if (i <= 7)
	return al[i];
    // else i >= 8 :
    double di = i, i2 = di*di;
    return (di + 0.5) * log(di) - di + M_LN_SQRT_2PI +
	(0.0833333333333333 - 0.00277777777777778 / i2) / di;
}

//     rhyper(NR, NB, n) -- NR 'red', NB 'blue', n drawn, how many are 'red'
double rhyper(double nn1in, double nn2in, double kkin)
{
    /* extern double afc(int); */

    int nn1, nn2, kk;
    int ix; // return value (coerced to double at the very end)
    Rboolean setup1, setup2;

    /* These should become 'thread_local globals' : */
    static int ks = -1, n1s = -1, n2s = -1;
    static int m, minjx, maxjx;
    static int k, n1, n2; // <- not allowing larger integer par
    static double tn;

    // II :
    static double w;
    // III:
    static double a, d, s, xl, xr, kl, kr, lamdl, lamdr, p1, p2, p3;

    /* check parameter validity */

    if(!R_FINITE(nn1in) || !R_FINITE(nn2in) || !R_FINITE(kkin))
	ML_ERR_return_NAN;

    nn1in = R_forceint(nn1in);
    nn2in = R_forceint(nn2in);
    kkin  = R_forceint(kkin);

    if (nn1in < 0 || nn2in < 0 || kkin < 0 || kkin > nn1in + nn2in)
	ML_ERR_return_NAN;
    if (nn1in >= INT_MAX || nn2in >= INT_MAX || kkin >= INT_MAX) {
	/* large n -- evade integer overflow (and inappropriate algorithms)
	   -------- */
        // FIXME: Much faster to give rbinom() approx when appropriate; -> see Kuensch(1989)
	// Johnson, Kotz,.. p.258 (top) mention the *four* different binomial approximations
	if(kkin == 1.) { // Bernoulli
	    return rbinom(kkin, nn1in / (nn1in + nn2in));
	}
	// Slow, but safe: return  F^{-1}(U)  where F(.) = phyper(.) and  U ~ U[0,1]
	return qhyper(unif_rand(), nn1in, nn2in, kkin, FALSE, FALSE);
    }
    nn1 = (int)nn1in;
    nn2 = (int)nn2in;
    kk  = (int)kkin;

    /* if new parameter values, initialize */
    if (nn1 != n1s || nn2 != n2s) {
	setup1 = TRUE;	setup2 = TRUE;
    } else if (kk != ks) {
	setup1 = FALSE;	setup2 = TRUE;
    } else {
	setup1 = FALSE;	setup2 = FALSE;
    }
    if (setup1) {
	n1s = nn1;
	n2s = nn2;
	tn = nn1 + nn2;
	if (nn1 <= nn2) {
	    n1 = nn1;
	    n2 = nn2;
	} else {
	    n1 = nn2;
	    n2 = nn1;
	}
    }
    if (setup2) {
	ks = kk;
	if (kk + kk >= tn) {
	    k = (int)(tn - kk);
	} else {
	    k = kk;
	}
    }
    if (setup1 || setup2) {
	m = (int) ((k + 1.) * (n1 + 1.) / (tn + 2.));
	minjx = imax2(0, k - n2);
	maxjx = imin2(n1, k);
#ifdef DEBUG_rhyper
	REprintf("rhyper(nn1=%d, nn2=%d, kk=%d), setup: floor(mean)= m=%d, jx in (%d..%d)\n",
		 nn1, nn2, kk, m, minjx, maxjx);
#endif
    }
    /* generate random variate --- Three basic cases */

    if (minjx == maxjx) { /* I: degenerate distribution ---------------- */
#ifdef DEBUG_rhyper
	REprintf("rhyper(), branch I (degenerate)\n");
#endif
	ix = maxjx;
	goto L_finis; // return appropriate variate

    } else if (m - minjx < 10) { // II: (Scaled) algorithm HIN (inverse transformation) ----
	const static double scale = 1e25; // scaling factor against (early) underflow
	const static double con = 57.5646273248511421;
					  // 25*log(10) = log(scale) { <==> exp(con) == scale }
	if (setup1 || setup2) {
	    double lw; // log(w);  w = exp(lw) * scale = exp(lw + log(scale)) = exp(lw + con)
	    if (k < n2) {
		lw = afc(n2) + afc(n1 + n2 - k) - afc(n2 - k) - afc(n1 + n2);
	    } else {
		lw = afc(n1) + afc(     k     ) - afc(k - n2) - afc(n1 + n2);
	    }
	    w = exp(lw + con);
	}
	double p, u;
#ifdef DEBUG_rhyper
	REprintf("rhyper(), branch II; w = %g > 0\n", w);
#endif
      L10:
	p = w;
	ix = minjx;
	u = unif_rand() * scale;
#ifdef DEBUG_rhyper
	REprintf("  _new_ u = %g\n", u);
#endif
	while (u > p) {
	    u -= p;
	    p *= ((double) n1 - ix) * (k - ix);
	    ix++;
	    p = p / ix / (n2 - k + ix);
#ifdef DEBUG_rhyper
	    REprintf("       ix=%3d, u=%11g, p=%20.14g (u-p=%g)\n", ix, u, p, u-p);
#endif
	    if (ix > maxjx)
		goto L10;
	    // FIXME  if(p == 0.)  we also "have lost"  => goto L10
	}
    } else { /* III : H2PE Algorithm --------------------------------------- */

	double u,v;

	if (setup1 || setup2) {
	    s = sqrt((tn - k) * k * n1 * n2 / (tn - 1) / tn / tn);

	    /* remark: d is defined in reference without int. */
	    /* the truncation centers the cell boundaries at 0.5 */

	    d = (int) (1.5 * s) + .5;
	    xl = m - d + .5;
	    xr = m + d + .5;
	    a = afc(m) + afc(n1 - m) + afc(k - m) + afc(n2 - k + m);
	    kl = exp(a - afc((int) (xl)) - afc((int) (n1 - xl))
		     - afc((int) (k - xl))
		     - afc((int) (n2 - k + xl)));
	    kr = exp(a - afc((int) (xr - 1))
		     - afc((int) (n1 - xr + 1))
		     - afc((int) (k - xr + 1))
		     - afc((int) (n2 - k + xr - 1)));
	    lamdl = -log(xl * (n2 - k + xl) / (n1 - xl + 1) / (k - xl + 1));
	    lamdr = -log((n1 - xr + 1) * (k - xr + 1) / xr / (n2 - k + xr));
	    p1 = d + d;
	    p2 = p1 + kl / lamdl;
	    p3 = p2 + kr / lamdr;
	}
#ifdef DEBUG_rhyper
	REprintf("rhyper(), branch III {accept/reject}: (xl,xr)= (%g,%g); (lamdl,lamdr)= (%g,%g)\n",
		 xl, xr, lamdl,lamdr);
	REprintf("-------- p123= c(%g,%g,%g)\n", p1,p2, p3);
#endif
	int n_uv = 0;
      L30:
	u = unif_rand() * p3;
	v = unif_rand();
	n_uv++;
	if(n_uv >= 10000) {
	    REprintf("rhyper() branch III: giving up after %d rejections", n_uv);
	    ML_ERR_return_NAN;
        }
#ifdef DEBUG_rhyper
	REprintf(" ... L30: new (u=%g, v ~ U[0,1])[%d]\n", u, n_uv);
#endif

	if (u < p1) {		/* rectangular region */
	    ix = (int) (xl + u);
	} else if (u <= p2) {	/* left tail */
	    ix = (int) (xl + log(v) / lamdl);
	    if (ix < minjx)
		goto L30;
	    v = v * (u - p1) * lamdl;
	} else {		/* right tail */
	    ix = (int) (xr - log(v) / lamdr);
	    if (ix > maxjx)
		goto L30;
	    v = v * (u - p2) * lamdr;
	}

	/* acceptance/rejection test */
	Rboolean reject = TRUE;

	if (m < 100 || ix <= 50) {
	    /* explicit evaluation */
	    /* The original algorithm (and TOMS 668) have
		   f = f * i * (n2 - k + i) / (n1 - i) / (k - i);
	       in the (m > ix) case, but the definition of the
	       recurrence relation on p134 shows that the +1 is
	       needed. */
	    int i;
	    double f = 1.0;
	    if (m < ix) {
		for (i = m + 1; i <= ix; i++)
		    f = f * (n1 - i + 1) * (k - i + 1) / (n2 - k + i) / i;
	    } else if (m > ix) {
		for (i = ix + 1; i <= m; i++)
		    f = f * i * (n2 - k + i) / (n1 - i + 1) / (k - i + 1);
	    }
	    if (v <= f) {
		reject = FALSE;
	    }
	} else {

	    const static double deltal = 0.0078;
	    const static double deltau = 0.0034;

	    double e, g, r, t, y;
	    double de, dg, dr, ds, dt, gl, gu, nk, nm, ub;
	    double xk, xm, xn, y1, ym, yn, yk, alv;

#ifdef DEBUG_rhyper
	    REprintf(" ... accept/reject 'large' case v=%g\n", v);
#endif
	    /* squeeze using upper and lower bounds */
	    y = ix;
	    y1 = y + 1.0;
	    ym = y - m;
	    yn = n1 - y + 1.0;
	    yk = k - y + 1.0;
	    nk = n2 - k + y1;
	    r = -ym / y1;
	    s = ym / yn;
	    t = ym / yk;
	    e = -ym / nk;
	    g = yn * yk / (y1 * nk) - 1.0;
	    dg = 1.0;
	    if (g < 0.0)
		dg = 1.0 + g;
	    gu = g * (1.0 + g * (-0.5 + g / 3.0));
	    gl = gu - .25 * (g * g * g * g) / dg;
	    xm = m + 0.5;
	    xn = n1 - m + 0.5;
	    xk = k - m + 0.5;
	    nm = n2 - k + xm;
	    ub = y * gu - m * gl + deltau
		+ xm * r * (1. + r * (-0.5 + r / 3.0))
		+ xn * s * (1. + s * (-0.5 + s / 3.0))
		+ xk * t * (1. + t * (-0.5 + t / 3.0))
		+ nm * e * (1. + e * (-0.5 + e / 3.0));
	    /* test against upper bound */
	    alv = log(v);
	    if (alv > ub) {
		reject = TRUE;
	    } else {
				/* test against lower bound */
		dr = xm * (r * r * r * r);
		if (r < 0.0)
		    dr /= (1.0 + r);
		ds = xn * (s * s * s * s);
		if (s < 0.0)
		    ds /= (1.0 + s);
		dt = xk * (t * t * t * t);
		if (t < 0.0)
		    dt /= (1.0 + t);
		de = nm * (e * e * e * e);
		if (e < 0.0)
		    de /= (1.0 + e);
		if (alv < ub - 0.25 * (dr + ds + dt + de)
		    + (y + m) * (gl - gu) - deltal) {
		    reject = FALSE;
		}
		else {
		    /* * Stirling's formula to machine accuracy
		     */
		    if (alv <= (a - afc(ix) - afc(n1 - ix)
				- afc(k - ix) - afc(n2 - k + ix))) {
			reject = FALSE;
		    } else {
			reject = TRUE;
		    }
		}
	    }
	} // else
	if (reject)
	    goto L30;
    }


L_finis:
    /* return appropriate variate */

    if (kk + kk >= tn) {
	if (nn1 > nn2) {
	    ix = kk - nn2 + ix;
	} else {
	    ix = nn1 - ix;
	}
    } else {
	if (nn1 > nn2)
	    ix = kk - ix;
    }
    return ix;
}
