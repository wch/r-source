/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2020 The R Core Team
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
#define MAX_DIGITS (DBL_MAX_10_EXP + DBL_DIG) /* typically = 308+15 = 323
    * was DBL_MAX_10_EXP (= 308, IEEE) till R 3.6.x; before,
    * was (DBL_DIG - 1)  till R 0.99  */
    const static int max10e = (int) DBL_MAX_10_EXP; // == 308 ("IEEE")

    /* Note that large digits make sense for very small numbers */
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!R_FINITE(x)) return x;

    if (digits > MAX_DIGITS || x == 0.)
	return x;
    else if(digits < -max10e) // includes -Inf {aka ML_NEGINF}
	return 0.;
    else if (digits == 0.) // common
	return nearbyint(x);

    int dig = (int)floor(digits + 0.5);
    double sgn = +1.;
    if(x < 0.) {
	sgn = -1.;
	x = -x;
    } // now  x > 0
    double l10x = M_LOG10_2*(0.5 + logb(x)); // ~= log10(x), but cheaper (presumably)
    if(l10x + dig > DBL_DIG) // rounding to so many digits that no rounding is needed
	return sgn * x;
    else {
	double pow10, x10, i10,
	    xd, xu; // x, rounded _d_own or _u_p
	if (dig <= max10e) { // both pow10 := 10^d and x10 := x * pow10 do *not* overflow
	    pow10 = R_pow_di(10., dig);
	    x10 = x * pow10;
	    i10 = floor(x10);
	    xd =    i10     / pow10;
	    xu = ceil (x10) / pow10;
	} else { // DBL_MAX_10_EXP =: max10e < dig <= DBL_DIG - l10x: case of |x| << 1; ~ 10^-305
	    int e10 = dig - max10e; // > 0
	    double
		p10 = R_pow_di(10., e10);
	    pow10   = R_pow_di(10., max10e);
	    x10 = (x * pow10) * p10;
	    i10 = floor(x10);
	    xd =    i10     / pow10 / p10;
	    xu = ceil (x10) / pow10 / p10;
	}
	double
	    du = xu - x,
	    dd = x  - xd;
	//  D =  du - dd
	//  return sgn * ((D < 0 || (is_odd_i10 && D == 0)) ? xu : xd);
	return sgn * ((du < dd || (fmod(i10, 2.) == 1 && du == dd)) ? xu : xd);
    }
}
