/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2013-2014 The R Core Team
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
 */

#include "nmath.h"

/* HAVE_COSPI etc will not be defined in standalone-use: the
   intention is to make the versions here available in that case.
*/

#ifndef HAVE_COSPI
// cos(pi * x)  -- exact when x = k/2  for all integer k
double cospi(double x) {
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x)) return x;
#endif
    if(!R_FINITE(x)) ML_ERR_return_NAN;

    x = fmod(fabs(x), 2.);// cos() symmetric; cos(pi(x + 2k)) == cos(pi x) for all integer k
    if(fmod(x, 1.) == 0.5) return 0.;
    if( x == 1.)	return -1.;
    if( x == 0.)	return  1.;
    // otherwise
    return cos(M_PI * x);
}
#endif

#ifndef HAVE_SINPI
// sin(pi * x)  -- exact when x = k/2  for all integer k
double sinpi(double x) {
#ifdef IEEE_754
    if (ISNAN(x)) return x;
#endif
    if(!R_FINITE(x)) ML_ERR_return_NAN;

    x = fmod(x, 2.); // sin(pi(x + 2k)) == sin(pi x)  for all integer k
    // map (-2,2) --> (-1,1] :
    if(x <= -1) x += 2.; else if (x > 1.) x -= 2.;
    if(x == 0. || x == 1.) return 0.;
    if(x ==  0.5)	return  1.;
    if(x == -0.5)	return -1.;
    // otherwise
    return sin(M_PI * x);
}
#endif

// tan(pi * x)  -- exact when x = k/2  for all integer k
#ifndef HAVE_TANPI
double tanpi(double x)
#else
// for use in arithmetic.c, half-values documented to give NaN
double Rtanpi(double x)
#endif
{
#ifdef IEEE_754
    if (ISNAN(x)) return x;
#endif
    if(!R_FINITE(x)) ML_ERR_return_NAN;

    x = fmod(x, 1.); // tan(pi(x + k)) == tan(pi x)  for all integer k
    // map (-1,1) --> (-1/2, 1/2] :
    if(x <= -0.5) x++; else if(x > 0.5) x--;
    return (x == 0.) ? 0. : ((x == 0.5) ? ML_NAN : tan(M_PI * x));
}
