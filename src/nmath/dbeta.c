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

#include "Mathlib.h"

double dbeta(double x, double a, double b, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(a) || ISNAN(b)) return x + a + b;
#endif
    if (a <= 0 || b <= 0) ML_ERR_return_NAN;

    if (x < 0 || x > 1)
	return R_D__0;

    if(give_log) {
	return log(x)*(a - 1) + log(1 - x)*(b - 1) - lbeta(a, b);
    }
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
