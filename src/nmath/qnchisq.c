/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

#include "Mathlib.h"

double qnchisq(double p, double n, double lambda)
{
    double ux, lx, nx;
    double acu = 1.0e-12;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n) || ISNAN(lambda))
	return p + n + lambda;
    if (!R_FINITE(n)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    n = floor(n + 0.5);
    if (p < 0 || p >= 1 || n < 1 || lambda < 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (p == 0)
	return 0;
    /* Invert pnchisq(.) finding an upper and lower bound;
       then interval halfing : */

    /* FIXME: Use less precision here (see pnchisq() !) */
    for (ux = 1.0; pnchisq(ux, n, lambda) < p; ux *= 2);
    for (lx = ux; pnchisq(lx, n, lambda) > p; lx *= 0.5);
    do {
	nx = 0.5 * (lx + ux);
	if (pnchisq(nx, n, lambda) > p)
	    ux = nx;
	else
	    lx = nx;
    }
    while ((ux - lx) / nx > acu);
    return 0.5 * (ux + lx);
}
