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
 *    double dpois(double x, double lambda)
 *
 *  DESCRIPTION
 *
 *    The density function of the Poisson distribution.
 */

#include "Mathlib.h"

double dpois(double x, double lambda)
{
#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(lambda))
	return x + lambda;
#endif
    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	warning("non-integer x = %f\n", x);
	return 0;
    }
    if(lambda <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x < 0)
	return 0;
#ifdef IEEE_754
    if(!finite(x))
	return 0;
#endif
    return exp(x * log(lambda) - lambda - lgammafn(x + 1));
}
