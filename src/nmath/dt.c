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
 *    double dt(double x, double n);
 *
 *  DESCRIPTION
 *
 *    The density of the "Student" t distribution.
 */

#include "Mathlib.h"

double dt(double x, double n)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n))
	return x + n;
#endif
    if (n <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#ifdef IEEE_754
    if(!finite(x))
	return 0;
    if(!finite(n))
	return dnorm(x, 0.0, 1.0);
#endif
    return pow(1.0 + x * x / n, -0.5 * (n + 1.0))
	/ (sqrt(n) * beta(0.5, 0.5 * n));
}
