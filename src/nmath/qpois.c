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
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double qpois(double x, double lambda)
 *
 *  DESCRIPTION
 *
 *    The quantile function of the Poisson distribution.
 *
 *  METHOD
 *
 *    Uses the Cornish-Fisher Expansion to include a skewness
 *    correction to a normal approximation.  This gives an
 *    initial value which never seems to be off by more than
 *    1 or 2.  A search is then conducted of values close to
 *    this initial start point.
 */

#include "Mathlib.h"

double qpois(double x, double lambda, int lower_tail, int log_p)
{
    double mu, sigma, gamma, z, y;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(lambda))
	return x + lambda;
    if(!R_FINITE(lambda)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    if(lambda <= 0 ||
       (log_p  && x > 0) ||
       (!log_p && (x < 0 || x > 1)) ) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x == R_D__0) return 0;
#ifdef IEEE_754
    if (x == R_D__1) return ML_POSINF;
#endif
    mu = lambda;
    sigma = sqrt(lambda);
    gamma = sigma;
#ifdef DPQ_NEW_NORM
    z = qnorm(x, 0., 1., lower_tail, log_p);
#else
    z = qnorm(R_DT_qIv(x), 0., 1.);
#endif
    y = floor(mu + sigma * (z + gamma * (z * z - 1) / 6) + 0.5);
    z = ppois(y, lambda, lower_tail, log_p);

    if(z >= x) {

	/* search to the left */

	for(;;) {
	    if((z = ppois(y - 1, lambda, lower_tail, log_p)) < x)
		return y;
	    y = y - 1;
	}
    }
    else {

	/* search to the right */

	for(;;) {
	    y = y + 1;
	    if((z = ppois(y, lambda, lower_tail, log_p)) >= x)
		return y;
	}
    }
}
