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
#include "Errormsg.h"

int signgam;

double lgamma(double x)
{
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

    if (x <= 0 && x == (int)x) { /* Negative integer argument */
	ML_ERROR(ME_RANGE);
	return ML_POSINF;/* +Inf, since lgamma(x) = log|gamma(x)| */
    }

    y = fabs(x);

    if (y <= 10) {
	return log(fabs(gamma(x)));
    }
    else { /* y = |x| > 10  */

	if (y > xmax) {
	    ML_ERROR(ME_RANGE);
	    return ML_POSINF;
	}

	if (x > 0)
	  return M_LN_SQRT_2PI + (x - 0.5) * log(x) - x + lgammacor(y);

	/* else: x < -10 */
	sinpiy = fabs(sin(M_PI * y));

	if (sinpiy == 0) { /* Negative integer argument ===
			      Now UNNECESSARY: caught above */
	    warning(" **this  should NEVER happen! *** [lgamma.c: Neg.int]\n");
	    ML_ERROR(ME_DOMAIN);
	    return ML_NAN;
	}

	ans = M_LN_SQRT_PId2 + (x - 0.5) * log(y) - x
	      - log(sinpiy) - lgammacor(y);

	if(fabs((x - (int)(x - 0.5)) * ans / x) < dxrel) {

	    /* The answer is less than half precision because */
	    /* the argument is too near a negative integer. */

	    ML_ERROR(ME_PRECISION);
	}

	if (x > 0)
	  return ans;
	else if (((int)(-x))%2 == 0)
	  signgam = -1;
	return ans;
    }
}
