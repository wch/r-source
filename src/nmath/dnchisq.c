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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double dnchisq(x, df, lambda);
 *
 *  DESCRIPTION
 *
 *    The density of the noncentral chisquare distribution with
 *    "df" degrees of freedom and noncentrality parameter "lambda".
 *
 */

#include "Mathlib.h"

double dnchisq(double x, double df, double lambda)
{
	double dens, i, lambda2, psum, sum, weight;
	static int maxiter = 100;
	static double eps = 1.e-14;

#ifdef IEEE_754
	if (ISNAN(x) || ISNAN(df) || ISNAN(lambda))
		return x + df + lambda;
#endif

	if (lambda < 0 || df <= 0) {
		ML_ERROR(ME_DOMAIN);
	}

#ifdef IEEE_754
	if (!R_FINITE(df) || !R_FINITE(lambda)) {
		ML_ERROR(ME_DOMAIN);
		return ML_NAN;
	}
#endif

	if(x <= 0) return 0;

	dens = dchisq(x, df);
	if(lambda == 0)
		return dens;

	lambda2 = 0.5 * lambda;
	weight = exp(-lambda2);
	sum = weight * dens;
	psum = weight;
	for(i=1 ; i<maxiter ; i++) {
		dens = (x/df) * dens;
		df = df + 2;
		weight = weight * lambda2 / i;
		sum = sum + dens * weight;
		psum = psum + weight;
		if (1 - psum < eps) break;
	}
	return sum;
}
