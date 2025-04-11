/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998-2025 The R Core Team
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
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
# undef fprintf
#endif
#include "nmath.h"

#ifdef MATHLIB_STANDALONE
/*
 *  based on code in ../main/arithmetic.c
 *  used only in standalone Rmath lib.
 */

int R_finite(double x)
{
#ifdef HAVE_WORKING_ISFINITE
    return isfinite(x);
# else
    return (!isnan(x) & (x != ML_POSINF) & (x != ML_NEGINF));
#endif
}

/* C++ math header undefines any isnan macro. This file
   doesn't get C++ headers and so is safe. */
int R_isnancpp(double x)
{
    return (isnan(x) != 0);
}

static double myfmod(double x1, double x2)
{
    double q = x1 / x2;
    return x1 - floor(q) * x2;
}

double R_pow(double x, double y) /* = x ^ y */
{
    if(x == 1. || y == 0.)
	return(1.);
    if(x == 0.) {
	if(y > 0.) return(0.);
	else if(y < 0) return(ML_POSINF);
	else return(y); /* y is NA or NaN, we assert */
    }
    if (R_FINITE(x) && R_FINITE(y))
	return(pow(x,y));
    if (ISNAN(x) || ISNAN(y)) {
#ifdef IEEE_754
	return(x + y);
#else
	return(ML_NAN);
#endif
    }
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return((y < 0.)? 0. : ML_POSINF);
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return (y < 0.) ? 0. : (myfmod(y,2.) != 0 ? x  : -x);
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return((x >= 1)? ML_POSINF : 0.);
	    else		/* y == -Inf */
		return((x < 1) ? ML_POSINF : 0.);
	}
    }
    return(ML_NAN);		/* all other cases: (-Inf)^{+-Inf,
				   non-int}; (neg)^{+-Inf} */
}

double R_pow_di(double x, int n)
{
    double pow = 1.0;

    if (ISNAN(x)) return x;
    if (n != 0) {
	if (!R_FINITE(x)) return R_pow(x, (double)n);
	if (n < 0) { n = -n; x = 1/x; }
	for(;;) {
	    if(n & 01) pow *= x;
	    if(n >>= 1) x *= x; else break;
	}
    }
    return pow;
}

/* It is not clear why these are being defined in standalone nmath:
 * but that they are is stated in the R-admin manual.
 *
 * In R NA_REAL is a specific NaN computed during initialization.
 */
#if defined(__clang__) && defined(NAN)
// C99 (optionally) has NAN, which is a float but will coerce to double.
double NA_REAL = NAN;
#else
// ML_NAN is defined as (0.0/0.0) in nmath.h
// Fails to compile in Intel ics 2025.0, Apple clang 17, LLVM clang 20
double NA_REAL = ML_NAN;
#endif

double R_PosInf = ML_POSINF, R_NegInf = ML_NEGINF;

#include <stdio.h>
#include <stdarg.h>
attribute_hidden void REprintf(const char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    fprintf(stderr, format, ap);
    va_end(ap);
}

#endif /* MATHLIB_STANDALONE */
