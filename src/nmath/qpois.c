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

double qpois(double p, double lambda, int lower_tail, int log_p)
{
    double mu, sigma, gamma, z, y;
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(lambda))
	return p + lambda;
    if(!R_FINITE(lambda)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    R_Q_P01_check(p);
    if(lambda <= 0) { ML_ERROR(ME_DOMAIN); return ML_NAN; }

    if (p == R_DT_0) return 0;
#ifdef IEEE_754
    if (p == R_DT_1) return ML_POSINF;
#endif
    mu = lambda;
    sigma = sqrt(lambda);
    gamma = sigma;
    z = qnorm(p, 0., 1., lower_tail, log_p);

    y = floor(mu + sigma * (z + gamma * (z * z - 1) / 6) + 0.5);
    z = ppois(y, lambda, lower_tail, log_p);

    if(z >= p) {	/* search to the left */
	for(;;) {
	    if((z = ppois(y - 1, lambda, lower_tail, log_p)) < p)
		return y;
	    y = y - 1;
	}
    }
    else {		/* search to the right */
	for(;;) {
	    y = y + 1;
	    if((z = ppois(y, lambda, lower_tail, log_p)) >= p)
		return y;
	}
    }
}
