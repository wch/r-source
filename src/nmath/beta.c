/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
 *  Copyright (C) 2000 The R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  SYNOPSIS
 *
 *    #include "Rmath.h"
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

#include "nmath.h"

double beta(double a, double b)
{
    static double xmax = 0;  /*-> typically = 171.61447887 for IEEE */
    static double alnsml = 0;/*-> typically = -708.3964185 */
    double val, xmin;

    if (xmax == 0) {
	    gammalims(&xmin, &xmax);
	    alnsml = log(d1mach(1));
    }

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if(ISNAN(a) || ISNAN(b)) return a + b;
#endif

    if (a < 0 || b < 0)
	ML_ERR_return_NAN
    else if (a == 0 || b == 0) {
	return ML_POSINF;
    }
    else if (!R_FINITE(a) || !R_FINITE(b)) {
	return 0;
    }

    if (a + b < xmax) /* ~= 171.61 for IEEE */
	return gammafn(a) * gammafn(b) / gammafn(a+b);

    val = lbeta(a, b);
    if (val < alnsml) {
	/* a and/or b so big that beta underflows */
	ML_ERROR(ME_UNDERFLOW);
	return ML_UNDERFLOW;
    }
    return exp(val);
}
