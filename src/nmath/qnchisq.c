/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2000, 2001 The R Development Core Team
 *  Copyright (C) 2004	     The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc., 59 Temple Place,
 *  Suite 330, Boston, MA  02111-1307  USA.
 */

#include "nmath.h"
#include "dpq.h"

double qnchisq(double p, double n, double lambda, int lower_tail, int log_p)
{
    const double acu = 1e-12;
    const double Eps = 1e-6; /* must be > acu */

    double ux, lx, nx, pp;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n) || ISNAN(lambda))
	return p + n + lambda;
#endif
    if (!R_FINITE(n)) ML_ERR_return_NAN;

    n = floor(n + 0.5);
    if (n < 1 || lambda < 0) ML_ERR_return_NAN;

    R_Q_P01_check(p);

    if (p == R_DT_0) return 0;
    if (p == R_DT_1) return ML_POSINF;

    p = R_D_qIv(p);

    /* Invert pnchisq(.) :
     * 1. finding an upper and lower bound */
    if(lower_tail) {
	pp = p * (1 + Eps);/*not good when p ~= 1; caught via DBL_MAX */
        for(ux = 1.;
	    ux < DBL_MAX && pnchisq_raw(ux, n, lambda, Eps, 128) < pp;
	    ux *= 2);
	pp = p * (1 - Eps);
        for(lx = fmin2(ux, DBL_MAX);
	    lx > DBL_MIN && pnchisq_raw(lx, n, lambda, Eps, 128) > pp;
	    lx *= 0.5);
    }
    else {
	pp = (p > Eps) ? 1 + Eps : 1;
        for(ux = 1.;
	    ux < DBL_MAX && pnchisq_raw(ux, n, lambda, Eps, 128) + p < pp;
	    ux *= 2);
	pp = 1 - Eps;
        for(lx = fmin2(ux, DBL_MAX);
	    lx > DBL_MIN && pnchisq_raw(lx, n, lambda, Eps, 128) + p > pp;
	    lx *= 0.5);
    }

    p = R_D_Lval(p);

    /* 2. interval (lx,ux)  halving : */
    do {
	nx = 0.5 * (lx + ux);
	if (pnchisq_raw(nx, n, lambda, acu, 1000) > p)
	    ux = nx;
	else
	    lx = nx;
    }
    while ((ux - lx) / nx > acu);
    return 0.5 * (ux + lx);
}


