/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000, 2005 The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *  DESCRIPTION
 *
 *    The distribution function of the F distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pf(double x, double n1, double n2, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2))
	return x + n2 + n1;
#endif
    if (n1 <= 0. || n2 <= 0.) ML_ERR_return_NAN;

    R_P_bounds_01(x, 0., ML_POSINF);

    /* move to pchisq for very large values - was 'n1 > 4e5' in 2.0.x,
       now only needed for n1 = Inf or n2 = Inf {since pbeta(0,*)=0} : */
    if (n2 == ML_POSINF) {
	if (n1 == ML_POSINF) {
	    if(x <  1.) return R_DT_0;
	    if(x == 1.) return (log_p ? -M_LN2 : 0.5);
	    if(x >  1.) return R_DT_1;
	}

	return pchisq(x * n1, n1, lower_tail, log_p);
    }

    if (n1 == ML_POSINF)/* was "fudge"	'n1 > 4e5' in 2.0.x */
	return pchisq(n2 / x , n2, !lower_tail, log_p);

    /* Avoid squeezing pbeta's first parameter against 1 :  */
    if (n1 * x > n2)
	x = pbeta(n2 / (n2 + n1 * x),	  n2 / 2., n1 / 2., !lower_tail, log_p);
    else
	x = pbeta(n1 * x / (n2 + n1 * x), n1 / 2., n2 / 2.,  lower_tail, log_p);

    return ML_VALID(x) ? x : ML_NAN;
}
