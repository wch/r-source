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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    double dlnorm(double x, double logmean, double logsd);
 *
 *  DESCRIPTION
 *
 *    The density of the lognormal distribution.
 *
 * 	M_1_SQRT_2PI = 1 / sqrt(2 * pi)
 */

#include "Mathlib.h"

double dlnorm(double x, double logmean, double logsd)
{
    double y;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(logmean) || ISNAN(logsd))
	return x + logmean + logsd;
#endif
    if(logsd <= 0) {
        ML_ERROR(ME_DOMAIN);
        return ML_NAN;
    }
    if(x == 0) return 0;
    y = (log(x) - logmean) / logsd;
    return M_1_SQRT_2PI * exp(-0.5 * y * y) / (x * logsd);
}
