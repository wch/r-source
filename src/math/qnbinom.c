/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

/*
 * Method:
 *
 * Uses the Cornish-Fisher Expansion to include a skewness
 * correction to a normal approximation.  This gives an
 * initial value which never seems to be off by more than
 * 1 or 2.  A search is then conducted of values close to
 * this initial start point.
 */

#include "Mathlib.h"

double qnbinom(double x, double n, double p)
{
	double P, Q, mu, sigma, gamma, z, y;

	n = floor(n + 0.5);
	if (x < 0.0 || x > 1.0 || p <= 0.0 || p >= 1.0 || n <= 0.0)
		DOMAIN_ERROR;
	if (x == 0.0) return 0.0;

	Q = 1.0 / p;
	P = (1.0 - p) * Q;
	mu = n * P;
	sigma = sqrt(n * P * Q);
	gamma = (Q + P)/sigma;
	z = qnorm(x, 0.0, 1.0);
	y = floor(mu + sigma * (z + gamma * (z*z - 1.0) / 6.0) + 0.5);

	z = pnbinom(y, n, p);
	if(z >= x) {
		/* search to the left */
		for(;;) {
			if((z = pnbinom(y - 1, n, p)) < x)
				return y;
			y = y - 1;
		}
	}
	else {
		/* search to the right */
		for(;;) {
			if((z = pnbinom(y + 1, n, p)) >= x)
				return y + 1;
			y = y + 1;
		}
	}
}
