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
 *    #include "Mathlib.h"
 *    double pnbinom(double x, double n, double p);
 *
 *  DESCRIPTION
 *
 *    The distribution function of the negative binomial distribution.
 *
 *  NOTES
 *
 *    x = the number of failures before the n-th success
 */

#include "Mathlib.h"

double pnbinom(double x, double n, double p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n) || ISNAN(p))
	return x + n + p;
    if(!finite(n) || !finite(p)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    x = floor(x + 0.5);
    n = floor(n + 0.5);
    if (n < 1 || p <= 0 || p >= 1) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x < 0) return 0;
#ifdef IEEE_754
    if (!finite(x))
	return 1;
#endif
    return pbeta(p, n, x + 1);
}
