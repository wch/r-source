/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 */

/* Reference:
 * R. C. H. Cheng (1978).
 * Generating beta variates with nonintegral shape parameters.
 * Communications of the ACM 21, 317-322.
 * (Algorithms BB and BC)
 */

#include "Mathlib.h"

/* static double expmax = 0.0; */

#define repeat for(;;)

#define expmax	(DBL_MAX_EXP * M_LN2)/* = log(DBL_MAX) */

double rbeta(double aa, double bb)
{
    /* FIXME:  Keep Globals (properly) for threading */

    /*--- MM: I have a first fix, quite nice, but w/ a bug (20/1/2000) ----*/

    /* Uses these GLOBALS to save time when many rv's are generated : */
    static double a, b, delta, r, s, t, u1, u2, v, w, y, z;
    static double alpha, beta, gamma, k1, k2;
    static double olda = -1.0;
    static double oldb = -1.0;
    int qsame;

    if (aa <= 0. || bb <= 0.)
	ML_ERR_return_NAN;

    a = fmin2(aa, bb);
    b = fmax2(aa, bb); /* a <= b */
    alpha = a + b;

    qsame = (olda == aa) && (oldb == bb);
    if (!qsame) {
      olda = aa;
      oldb = bb;
    }

    if (a <= 1.0) {	/* Algorithm BC */

	if (!qsame) { /* initialize */
	    beta = 1.0 / a;
	    delta = 1.0 + b - a;
	    k1 = delta * (0.0138889 + 0.0416667 * a) /
		(b * beta - 0.777778);
	    k2 = 0.25 + (0.5 + 0.25 / delta) * a;
	}
	repeat {
	    u1 = sunif();
	    u2 = sunif();
	    if (u1 < 0.5) {
		y = u1 * u2;
		z = u1 * y;
		if (0.25 * u2 + z - y >= k1)
		    continue;
	    } else {
		z = u1 * u1 * u2;
		if (z <= 0.25)
		    break;
		if (z >= k2)
		    continue;
	    }
	    v = beta * log(u1 / (1.0 - u1));
	    if (v <= expmax)
		w = b * exp(v);
	    else
		w = DBL_MAX;
	    if (alpha * (log(alpha / (a + w)) + v) - 1.3862944 >= log(z))
		goto deliver;
	}
	v = beta * log(u1 / (1.0 - u1));
	if (v <= expmax)
	    w = b * exp(v);
	else
	    w = DBL_MAX;

    deliver:
	return (aa == a) ? a / (a + w) : w / (a + w);

    }
    else {		/* Algorithm BB */

	if (!qsame) { /* initialize */
	    beta = sqrt((alpha - 2.0) / (2.0 * a * b - alpha));
	    gamma = a + 1.0 / beta;
	}
	do {
	    u1 = sunif();
	    u2 = sunif();
	    v = beta * log(u1 / (1.0 - u1));
	    if (v <= expmax)
		w = a * exp(v);
	    else
		w = DBL_MAX;
	    z = u1 * u1 * u2;
	    r = gamma * v - 1.3862944;
	    s = a + r - w;
	    if (s + 2.609438 >= 5.0 * z)
		break;
	    t = log(z);
	    if (s > t)
		break;
	}
	while (r + alpha * log(alpha / (b + w)) < t);

	return (aa != a) ? b / (b + w) : w / (b + w);
    }
}
