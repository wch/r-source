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
 *    double qbinom(double x, double n, double p);
 *
 *  DESCRIPTION
 *
 *    The quantile function of the binomial distribution.
 *
 *  NOTES
 *
 *    The function uses the Cornish-Fisher Expansion to include
 *    a skewness correction to a normal approximation.  This gives
 *    an initial value which never seems to be off by more than
 *    1 or 2.  A search is then conducted of values close to
 *    this initial start point.
 */

#include "Mathlib.h"

double qbinom(double x, double n, double p)
{
    double q, mu, sigma, gamma, z, y;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n) || ISNAN(p))
	return x + n + p;
    if(!R_FINITE(x) || !R_FINITE(n) || !R_FINITE(p)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif

    n = floor(n + 0.5);
    if (x < 0 || x > 1 || p <= 0 || p >= 1 || n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x == 0) return 0.0;
    if (x == 1) return n;
    q = 1 - p;
    mu = n * p;
    sigma = sqrt(n * p * q);
    gamma = (q-p)/sigma;
    z = qnorm(x, 0.0, 1.0);
    y = floor(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);

    z = pbinom(y, n, p);
    if(z >= x) {

	/* search to the left */

	for(;;) {
	    if((z = pbinom(y - 1, n, p)) < x)
		return y;
	    y = y - 1;
	}
    }
    else {

	/* search to the right */

	for(;;) {
	    if((z = pbinom(y + 1, n, p)) >= x)
		return y + 1;
	    y = y + 1;
	}
    }
}
