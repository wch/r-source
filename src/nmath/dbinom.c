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
 *    The density of the binomial distribution.
 */

#include "Mathlib.h"
#include "dpq.h"

double dbinom(double x, double n, double p, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(n) || ISNAN(p)) return x + n + p;
#endif
    n = floor(n + 0.5);
    if(n <= 0 || p < 0 || p > 1) ML_ERR_return_NAN;

    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	warning("non-integer x = %f", x);
	return R_D__0;
    }
    if (x < 0 || x > n)
	return R_D__0;
    if (p == 0)
	return (x == 0) ? R_D__1 : R_D__0;
    if (p == 1)
	return (x == n) ? R_D__1 : R_D__0;
    return R_D_exp(lfastchoose(n, x) + log(p) * x + (n - x) * log1p(-p));
}
