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
 *    extern int signgam;
 *    double lgamma(double x);
 *
 *  DESCRIPTION
 *
 *    This function computes log|gamma(x)|.  At the same time
 *    the variable "signgam" is set to the sign of the gamma
 *    function.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 *
 *    The accuracy of this routine compares (very) favourably
 *    with those of the Sun Microsystems portable mathematical
 *    library.                                                  
 */

#include "Mathlib.h"

int signgam;

double lgamma(double x)
{
    /* sq2pil = alog(sqrt(2*pi)) */
    /* sqpi2l = alog(sqrt(pi/2)) */
    static double sq2pil = .91893853320467274178032973640562;
    static double sqpi2l = .225791352644727432363097614947441;
    static double pi = 3.1415926535897932384626433832795;
    static double xmax = 0.;
    static double dxrel = 0.;
    double ans, y, sinpiy;

    if (xmax == 0) {
	xmax = d1mach(2)/log(d1mach(2));
	dxrel = sqrt (d1mach(4));
    }

    signgam = 1;

#ifdef IEEE_754
    if(ISNAN(x)) return x;
#endif

    y = fabs(x);

    if (y <= 10) {
	return log(fabs(gamma(x)));
    }
    else {

	if (y > xmax) {
	    ML_ERROR(ME_RANGE);
	    return ML_POSINF;
	}

	if (x > 0)
	    return sq2pil + (x - 0.5) * log(x) - x + lgammacor(y);

	sinpiy = fabs(sin(pi * y));

	if (sinpiy == 0) {			/* Negative integer argument */
	    ML_ERROR(ME_DOMAIN);
	    return ML_NAN;
	}

	ans = sqpi2l + (x - 0.5) * log(y) - x - log(sinpiy) - lgammacor(y);

	if(fabs((x - (int)(x - 0.5)) * ans / x) < dxrel) {

	    /* The answer is less than half precision because */
	    /* the argument is too near a negative integer. */

	    ML_ERROR(ME_PRECISION);
	}

    if (x > 0)
        return;

    if (((int)(-x))%2 == 0)
        signgam = -1; 
	return ans;
    }
}
