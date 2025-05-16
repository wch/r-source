/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2025 The R Core Team
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
 *    double fprec(double x, double digits);
 *
 *  DESCRIPTION
 *
 *    Returns the value of x rounded to "digits" significant
 *    decimal digits.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 *    Some modifications have been made so that the routines
 *    conform to the IEEE 754 standard.
 */

#include <config.h>
#include "nmath.h"

/* Improvements by Martin Maechler, May 1997;
   further ones, Feb.2000:
   Replace  pow(x, (double)i) by  R_pow_di(x, i) {and use  int dig} */

#define MAX_DIGITS 22
/* was till R 0.99: DBL_DIG := digits of precision of a double, usually 15 */
/* FIXME: Hmm, have quite a host of these:

       1) ./fround.c   uses much more (sensibly!) ``instead''
       2) ../main/coerce.c   & ../main/deparse.c have  DBL_DIG	directly
       3) ../main/options.c has	  #define MAX_DIGITS 22	 for options(digits)

       Really should decide on a (config.h dependent?) global MAX_DIGITS.
       --MM--
     */

// R's  signif(x, digits)   via   Math2(args, fprec) in  ../main/arithmetic.c :
double fprec(double x, double digits)
{
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if (!R_FINITE(x)) return x;
    if (!R_FINITE(digits)) {
	if(digits > 0.) return x;
	else digits = 1.;
    }
    if(x == 0) return x;

    int dig = (int)round(digits);
    if (dig > MAX_DIGITS) {
	return x;
    } else if (dig < 1)
	dig = 1;

    double sgn = 1.;
    if(x < 0.0) {
	sgn = -sgn;
	x = -x;
    }
    double l10 = log10(x);
    int e10 = dig-1 - (int)floor(l10);
    // Max.expon. of 10 (w/o denormalizing or overflow; = R's trunc( log10(.Machine$double.xmax) ):
    const static int max10e = (int) DBL_MAX_10_EXP; // == 308 ("IEEE")
    if(fabs(l10) < max10e - 2) {
	double p10 = 1.;
	if(e10 > max10e) { /* numbers less than 10^(dig-1) * 1e-308 */
	    p10 =  R_pow_di(10., e10-max10e);
	    e10 = max10e;
	}
	double pow10;
	if(e10 > 0) { /* Try always to have pow >= 1
			 and so exactly representable */
	    pow10 = R_pow_di(10., e10);
	    return(sgn*(nearbyint((x*pow10)*p10)/pow10)/p10);
	} else {
	    pow10 = R_pow_di(10., -e10);
	    return(sgn*(nearbyint((x/pow10))*pow10));
	}
    } else { /* -- LARGE or small -- */
	bool do_round = log10(DBL_MAX) - l10 >= R_pow_di(10., -dig); /* e.g. signif(1.09e308, 2) */
	int e2 = dig + ((e10 > 0)? 1 : -1) * MAX_DIGITS;
	double
	    p10 = R_pow_di(10., e2),
	    P10 = R_pow_di(10., e10-e2);
	x *= p10;
	x *= P10;
	/*-- p10 * P10 = 10 ^ e10 */
	if(do_round) x += 0.5;
	x = floor(x) / p10;
	return(sgn*x/P10);
    }
}
