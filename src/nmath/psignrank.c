/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 R Core Team
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
 *    double psignrank(double x, double n)
 *
 *  DESCRIPTION
 *
 *    The distribution function of the Wilcoxon Signed Rank distribution.
 */

#include "Mathlib.h"

double psignrank(double x, double n) {
    int i;
    double p = 0.0;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n))
    return x + n;
    if (!FINITE(n)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    n = floor(n + 0.5);
    if (n <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    } else if (n >= SIGNRANK_NMAX) {
	MATHLIB_WARNING("n should be less than %d\n", SIGNRANK_NMAX);
	return ML_NAN;
    }
    x = floor(x + 0.5);
    if (x < 0.0)
	return 0;
    if (x >= n * (n + 1) / 2)
	return 1;
    for (i = 0; i <= x; i++)
	p += dsignrank(i, n);
    return(p);
}
