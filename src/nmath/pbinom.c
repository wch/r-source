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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double pbinom(double x, double n, double p)
 *
 *  DESCRIPTION
 *
 *    The distribution function of the binomial distribution.
 */

#include "Mathlib.h"

double pbinom(double x, double n, double p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n) || ISNAN(p))
	return x + n + p;
    if (!R_FINITE(n) || !R_FINITE(p)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    n = floor(n + 0.5);
    if(n <= 0 || p < 0 || p > 1) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    x = floor(x);
    if (x < 0.0) return 0;
    if (n <= x) return 1;
    return pbeta(1.0 - p, n - x, x + 1);
}
