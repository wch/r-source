/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000-1 The R Development Core Team
 *  Copyright (C) 2004   The R Foundation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but
 *  WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 *  USA.
 *
 *  DESCRIPTION
 *
 *    The density of the noncentral chi-squared distribution with "df"
 *    degrees of freedom and noncentrality parameter "lambda".
 */

#include "nmath.h"
#include "dpq.h"

double dnchisq(double x, double df, double lambda, int give_log)
{
    const static double eps = 5e-15;

    double i, lambda2, term, sum, q, mid, dfmid, imax, errorbound;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(df) || ISNAN(lambda))
	return x + df + lambda;
#endif
    if (lambda < 0 || df <= 0) ML_ERR_return_NAN;

    if (!R_FINITE(df) || !R_FINITE(lambda))
	ML_ERR_return_NAN;

    if(x < 0) return R_D__0;
    if(x == 0 && df < 2.)
	return ML_POSINF;
    if(lambda == 0)
	return dchisq(x, df, give_log);

    lambda2 = 0.5 * lambda;

    /* find max element of sum */
    imax = ceil((-(2+df) +sqrt((2-df) * (2-df) + 4 * lambda * x))/4);
    if (imax < 0) imax = 0;
    dfmid = df + 2 * imax;
    mid = dpois_raw(imax, lambda2, FALSE) * dchisq(x, dfmid, FALSE);

    sum = mid;
    /* upper tail */
    term = mid;
    i = imax;
    df = dfmid;
    do {
	i++;
	q = x * lambda2 / i / df;
	df += 2;
	term = q * term;
	sum += term;
	errorbound = term * q / (1-q);
    } while (errorbound > eps || q >= 1);
    /* lower tail */
    term = mid;
    df = dfmid;
    i = imax;
    while ( i ){
	df -= 2;
	q = i * df / x / lambda2;
	i--;
	term = q * term;
	sum += term;
	errorbound = term * q / (1-q);
	if (errorbound <= eps && q < 1) break;
    }
    return R_D_val(sum);
}
