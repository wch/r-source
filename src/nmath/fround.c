/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2019 The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *  SYNOPSIS
 *
 *    #include <Rmath.h>
 *    double fround(double x, double digits);
 *
 *  DESCRIPTION
 *
 *    Rounds "x" to "digits" decimal digits.
 *
 */

#include <config.h> /* needed for HAVE_* */
#include "nmath.h"

double fround(double x, double digits) {
#define MAX_DIGITS DBL_MAX_10_EXP
    /* = 308 (IEEE); was till R 0.99: (DBL_DIG - 1) */
    /* Note that large digits make sense for very small numbers */

    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if(digits == ML_POSINF) return x;
    else if(digits == ML_NEGINF) return 0.0;

    if (digits > MAX_DIGITS) digits = MAX_DIGITS;

    int dig = (int)floor(digits + 0.5);
    LDOUBLE sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    }
    if (dig == 0) {
	return (double)(sgn * nearbyint(x));
    } else if (dig > 0) {
	LDOUBLE pow10 = R_pow_di(10., dig)
#ifdef R_3_AND_OLDER
	    , intx = floor(x);
	return (double)(sgn * (intx + nearbyint((double)((x-intx) * pow10)) / pow10));
#else
	;
	return (double)(sgn * (nearbyint((double)(x * pow10)) / pow10));
#endif
    } else {
	LDOUBLE pow10 = R_pow_di(10., -dig);
        return (double)(sgn * nearbyint((double)(x/pow10)) * pow10);
    }
}
