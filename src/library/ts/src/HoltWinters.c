/*  R : A Computer Language for Statistical Data Analysis
 *
 *  Copyright (C) 2002	The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA.
 */

#include <R.h>
#include "ts.h"
#include <stdlib.h>

void HoltWinters (double *x,
		  int    *xl,
		  double *alpha,
		  double *beta,
		  double *gamma,
		  int    *start_time,
		  int    *seasonal,
		  int    *period,
		  double *a,
		  double *b,
		  double *s,
		  double *SSE,
		  double *xhat)

{
    double *st = NULL;
    double season, a_new, res, trend, b_new = 0;
    int i, i0;

    if (*gamma > 0) {
	st = (double *) malloc(*xl * sizeof(double));
	memcpy (st, s, *period * sizeof(double));
    }

    for (i = *start_time - 1; i < *xl; i++) {
	i0 = i - *start_time + 1;
        /* forecast *for* period i */
	trend  = *beta  > 0 ? *b : 0;
	season = *gamma > 0 ? st[i - *period] : 0;

	xhat[i0] = *a + trend;
	if (*seasonal == 1)
	    xhat[i0] += season;
	else
	    xhat[i0] *= season;

	/* Sum of Squared Errors */
	res   = x[i] - xhat[i0];
	*SSE += res * res;

	/* estimate of level *in* period i */
	if (*seasonal == 1)
	    a_new = *alpha  * (x[i] - season) + (1 - *alpha) * (*a + trend);
	else
	    a_new = *alpha  * (x[i] / season) + (1 - *alpha) * (*a + trend);

	/* estimate of trend *in* period i */
	if (*beta > 0)
	    b_new = *beta   * (a_new - *a)    + (1 - *beta)  * trend;

	/* estimate of seasonal component *in* period i */
	if (*gamma > 0) {
	    if (*seasonal == 1)
		st[i] = *gamma  * (x[i] - a_new)  + (1 - *gamma) * season;
	    else
		st[i] = *gamma  * (x[i] / a_new)  + (1 - *gamma) * season;
	}
	/* move one step ahead */
	*a = a_new;
	if (*beta > 0) *b = b_new;
    }

    if (*gamma > 0) {
	memcpy (s, st - *period + *xl, *period * sizeof(double));
	free (st);
    }
}


