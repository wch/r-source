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

double qnchisq(double p, double df, double lambda, int lower_tail, int log_p)
{
    const double accu = 1e-13;
    const double racc = 4*DBL_EPSILON;
    /* these two are for the "search" loops, can have less accuracy: */
    const double Eps = 1e-11; /* must be > accu */
    const double rEps= 1e-10; /* relative tolerance ... */

    double ux, lx, nx, pp;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(df) || ISNAN(lambda))
	return p + df + lambda;
#endif
    if (!R_FINITE(df)) ML_ERR_return_NAN;

    /* Was
     * df = floor(df + 0.5);
     * if (df < 1 || lambda < 0) ML_ERR_return_NAN;
     */
    if (df < 0 || lambda < 0) ML_ERR_return_NAN;

    R_Q_P01_check(p);

    if (p == R_DT_0) return 0;
    if (p == R_DT_1) return ML_POSINF;

    p = R_D_qIv(p);

    /* Invert pnchisq(.) :
     * 1. finding an upper and lower bound */
    if(lower_tail) {
	pp = p * (1 + Eps);/*not good when p ~= 1; caught via DBL_MAX */
        for(ux = 1.;
	    ux < DBL_MAX && pnchisq_raw(ux, df, lambda, Eps, rEps, 10000) < pp;
	    ux *= 2);
	pp = p * (1 - Eps);
        for(lx = fmin2(ux, DBL_MAX);
	    lx > DBL_MIN && pnchisq_raw(lx, df, lambda, Eps, rEps, 10000) > pp;
	    lx *= 0.5);
    }
    else {
	pp = (p > Eps) ? 1 + Eps : 1;
        for(ux = 1.;
	    ux < DBL_MAX && pnchisq_raw(ux, df, lambda, Eps, rEps, 10000) + p < pp;
	    ux *= 2);
	pp = 1 - Eps;
        for(lx = fmin2(ux, DBL_MAX);
	    lx > DBL_MIN && pnchisq_raw(lx, df, lambda, Eps, rEps, 10000) + p > pp;
	    lx *= 0.5);
    }

    p = R_D_Lval(p);

    /* 2. interval (lx,ux)  halving : */
    do {
	nx = 0.5 * (lx + ux);
	if (pnchisq_raw(nx, df, lambda, accu, racc, 100000) > p)
	    ux = nx;
	else
	    lx = nx;
    }
    while ((ux - lx) / nx > accu);
    return 0.5 * (ux + lx);
}


