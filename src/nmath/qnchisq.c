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

#include "Mathlib.h"

double qnchisq(double p, double n, double lambda, int lower_tail, int log_p)
{
    static const double acu = 1.0e-12;

    double ux, lx, nx;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n) || ISNAN(lambda))
	return p + n + lambda;
#endif
    if (!R_FINITE(n)) ML_ERR_return_NAN;

    n = floor(n + 0.5);
    if (n < 1 || lambda < 0) ML_ERR_return_NAN;

    R_Q_P01_check(p);

    if (p == R_DT_0)
	return 0;

    /* Invert pnchisq(.) finding an upper and lower bound;
       then interval halfing : */

    /* FIXME: Use less precision here (see pnchisq() !) */
    for (ux = 1.0; pnchisq(ux, n, lambda, lower_tail, log_p) < p; ux *= 2);
    for (lx = ux;  pnchisq(lx, n, lambda, lower_tail, log_p) > p; lx *= 0.5);
    do {
	nx = 0.5 * (lx + ux);
	if (pnchisq(nx, n, lambda, lower_tail, log_p) > p)
	    ux = nx;
	else
	    lx = nx;
    }
    while ((ux - lx) / nx > acu);
    return 0.5 * (ux + lx);
}
