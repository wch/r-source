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
 *    double beta(double a, double b);
 *
 *  DESCRIPTION
 *
 *    This function returns the value of the beta function
 *    evaluated with arguments a and b.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 *    Some modifications have been made so that the routines
 *    conform to the IEEE 754 standard.
 */

#include "Mathlib.h"

double beta(double a, double b)
{
    static double xmax = 0;
    static double alnsml = 0;
    double val, xmin;

    if (xmax == 0) {
	    gammalims(&xmin, &xmax);
	    alnsml = log(d1mach(1));
    }

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if(ISNAN(a) || ISNAN(b)) return a + b;
#endif

    if (a < 0 || b < 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    else if (a == 0 || b == 0) {
	return ML_POSINF;
    }
#ifdef IEEE_754
    else if (!FINITE(a) || !FINITE(b)) {
	return 0;
    }
#endif

    if (a + b < xmax)
	return gammafn(a) * gammafn(b) / gammafn(a+b);

    val = lbeta(a, b);
    if (val < alnsml) {
	/* a and/or b so big that beta underflows */
	ML_ERROR(ME_UNDERFLOW);
	return ML_UNDERFLOW;
    }
    return exp(val);
}
