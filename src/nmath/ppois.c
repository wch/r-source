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
 *    double ppois(double x, double lambda)
 *
 *  DESCRIPTION
 *
 *    The distribution function of the Poisson distribution.
 */

#include "Mathlib.h"

double ppois(double x, double lambda)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(lambda))
	return x + lambda;
#endif
    if(lambda <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    x = floor(x + 1e-7);
    if (x < 0)
	return 0;
#ifdef IEEE_754
    if (!R_FINITE(x))
	return 1;
#endif
    return  1 - pgamma(lambda, x + 1, 1.0);
}
