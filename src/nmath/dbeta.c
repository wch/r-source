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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  DESCRIPTION
 *
 *    The density of the beta distribution.
 */

#include "nmath.h"
#include "dpq.h"

double dbeta(double x, double a, double b, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(a) || ISNAN(b)) return x + a + b;

# define xmax 171.61447887182298/* (fixme) -->> ./gammalims.c */

#else
    static double xmax = 0; double xmin;
    if (xmax == 0)
	gammalims(&xmin, &xmax);
#endif

    if (a <= 0 || b <= 0) ML_ERR_return_NAN;

    if (x < 0 || x > 1)
	return R_D__0;

#define R_LOG_DBETA log(x)*(a - 1) + log(1 - x)*(b - 1) - lbeta(a, b)

    if(give_log)
	return R_LOG_DBETA;
    else if (a + b >= xmax) /* beta(a,b) might be = 0 numerically */
	return exp(R_LOG_DBETA);
    else {
	double y;
	y = beta(a, b);
	a = pow(x, a - 1);
	b = pow(1 - x, b - 1);
#ifndef IEEE_754
	if(errno) return ML_NAN;
#endif
	return (a * b / y);
    }
}
