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
#define MAX_DIGITS (DBL_MAX_10_EXP + DBL_DIG)
    /* was DBL_MAX_10_EXP (= 308, IEEE) till R 3.6.x; before,
       was (DBL_DIG - 1)  till R 0.99  */
    const static int max10e = (int) DBL_MAX_10_EXP; // == 308 ("IEEE")

    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits == ML_NEGINF) return 0.0;

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    if (dig == 0) {
	return sgn * nearbyint(x);
    } else if (dig > 0) {
	double l10x = log10(x);
	if(l10x + dig > DBL_DIG) { // rounding to so many digits that no rounding is needed
	    return sgn * x;
	} else if (dig <= DBL_MAX_10_EXP) { // both pow10 := 10^d and (x * pow10) do *not* overflow
	    LDOUBLE pow10 = R_pow_di(10., dig);
	    return sgn *  (double)(nearbyint((double)(x * pow10)) / pow10);
	} else { // DBL_MAX_10_EXP < dig <= DBL_DIG - log10(x) : case of |x| << 1; ~ 10^-305
	    int e10 = dig - max10e; // > 0
	    LDOUBLE p10 = R_pow_di(10., e10),
		  pow10 = R_pow_di(10., max10e);
	    return  sgn * (double) (nearbyint((double)((x*pow10)*p10))/pow10/p10);
	}
    } else {
	LDOUBLE pow10 = R_pow_di(10., -dig);
        return sgn *  (double) (nearbyint((double)(x/pow10)) * pow10);
    }
}
