/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *
 *  DESCRIPTION
 *
 *    The density of the noncentral chisquare distribution with
 *    "df" degrees of freedom and noncentrality parameter "lambda".
 */

#include "nmath.h"
#include "dpq.h"

double dnchisq(double x, double df, double lambda, int give_log)
{
    const static int maxiter = 100;
    const static double eps = 1.e-14;

    double dens, i, lambda2, psum, sum, weight;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df) || ISNAN(lambda))
	return x + df + lambda;
#endif
    if (lambda < 0 || df <= 0) ML_ERR_return_NAN;

    if (!R_FINITE(df) || !R_FINITE(lambda))
	ML_ERR_return_NAN;

    if(x <= 0) return R_D__0;


    if(lambda == 0)
	return dchisq(x, df, give_log);

    dens = dchisq(x, df,  /* log = */ LFALSE);
    lambda2 = 0.5 * lambda;
    weight = exp(-lambda2);
    sum	 = weight * dens;
    psum = weight;
    for(i=1; i < maxiter; i++) {
	dens *= (x/df);
	df += 2;
	weight *= (lambda2 / i);
	sum  += weight * dens;
	psum += weight;
	if (1 - psum < eps) break;
    }
    if(1 - psum >= eps) { /* not converged */
	ML_ERROR(ME_PRECISION);
    }
    return R_D_val(sum);
}
