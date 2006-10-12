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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *  DESCRIPTION
 *
 *    The density of the lognormal distribution.
 */

#include "nmath.h"
#include "dpq.h"

double dlnorm(double x, double logmean, double logsd, int give_log)
{
    double y;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(logmean) || ISNAN(logsd))
	return x + logmean + logsd;
#endif
    if(logsd <= 0) ML_ERR_return_NAN;

    if(x <= 0) return R_D__0;

    y = (log(x) - logmean) / logsd;
    return (give_log ?
	    -(M_LN_SQRT_2PI   + 0.5 * y * y + log(x * logsd)) :
	    M_1_SQRT_2PI * exp(-0.5 * y * y)  /	 (x * logsd));
    /* M_1_SQRT_2PI = 1 / sqrt(2 * pi) */

}
