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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double dbeta(double x, double a, double b);
 *
 *  DESCRIPTION
 *
 *    The density of the beta distribution.
 */

#include "Mathlib.h"

double dbeta(double x, double a, double b)
{
    double y;
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(a) || ISNAN(b)) return x + a + b;
#endif
    if (a <= 0.0 || b <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x < 0)
	return 0;
    if (x > 1)
	return 0;
    y = beta(a, b);
    a = pow(x, a - 1);
    b = pow(1.0 - x, b - 1.0);
#ifndef IEEE_754
    if(errno) return ML_NAN;
#endif
    return a * b / y;
}
