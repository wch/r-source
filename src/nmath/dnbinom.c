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
 *    The density function of the negative binomial distribution.
 *
 *  NOTE
 *
 *    x = the number of failures before the n-th success
 */

#include "nmath.h"
#include "dpq.h"

double dnbinom(double x, double n, double p, int give_log)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n) || ISNAN(p))
	return x + n + p;
#endif
    if (n <= 0 || p <= 0 || p >= 1)
	ML_ERR_return_NAN;

    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	MATHLIB_WARNING("non-integer x = %f", x);
	return R_D__0;
    }
    if (x < 0)
	return R_D__0;
    if (!R_FINITE(x))
	return R_D__0;
    return R_D_exp(lfastchoose(x + n - 1, x)
		   + n * log(p) + x * log(1 - p));
}
