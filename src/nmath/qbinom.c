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
 *	The quantile function of the binomial distribution.
 *
 *  NOTES
 *
 *	The function uses the Cornish-Fisher Expansion to include
 *	a skewness correction to a normal approximation.  This gives
 *	an initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of valdues close to
 *	this initial start point.
 */
#include "Mathlib.h"

double qbinom(double p, double n, double pr, int lower_tail, int log_p)
{
    double q, mu, sigma, gamma, z, y;

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n) || ISNAN(pr))
	return p + n + pr;
#endif
    if(!R_FINITE(p) || !R_FINITE(n) || !R_FINITE(pr))
	ML_ERR_return_NAN;
    R_Q_P01_check(p);

    n = floor(n + 0.5);
    if (pr <= 0 || pr >= 1 || n <= 0)
	ML_ERR_return_NAN;

    if (p == R_DT_0) return 0.;
    if (p == R_DT_1) return n;

    /* FIXME */
    if(!lower_tail || log_p) {
	warning("lower_tail & log_p not yet implemented in ptukey()");
	return ML_NAN;
    }

    q = 1 - pr;
    mu = n * pr;
    sigma = sqrt(n * pr * q);
    gamma = (q - pr) / sigma;

    /* in all the following, z is "as p" : lower/upper tail; log or non-log :*/
    z = qnorm(p, 0.0, 1.0, lower_tail, log_p);
    y = floor(mu + sigma * (z + gamma * (z*z - 1) / 6) + 0.5);

    z = pbinom(y, n, pr, lower_tail, log_p);

    if(z >= p) {	/* search to the left */

	for(;;) {
	    if((z = pbinom(y - 1, n, pr, lower_tail, log_p)) < p)
		return y;
	    y = y - 1;
	}
    }
    else { /* z < p :	search to the right */

	for(;;) {
	    y = y + 1;
	    if((z = pbinom(y, n, pr, lower_tail, log_p)) >= p)
		return y;
	}
    }
}
