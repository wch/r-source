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
 *    double lbeta(double a, double b);
 *
 *  DESCRIPTION
 *
 *    This function returns the value of the log beta function.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 */

#include "Mathlib.h"

double lbeta(double a, double b)
{
    static double sq2pil = .91893853320467274178032973640562;
    static double corr, p, q;

    p = q = a;
    if(b < p) p = b;
    if(b > q) q = b;

#ifdef IEEE_754
    if(ISNAN(a) || ISNAN(b))
	return a + b;
#endif

    /* both arguments must be > 0 */

    if (p <= 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }

    if (p >= 10) {
	/* p and q are big. */
	corr = lgammacor(p) + lgammacor(q) - lgammacor(p + q);
	return log(q) * -0.5 + sq2pil + corr
		+ (p - 0.5) * log(p / (p + q)) + q * logrelerr(-p / (p + q));
    }
    else if (q >= 10) {
	/* p is small, but q is big. */
	corr = lgammacor(q) - lgammacor(p + q);
	return lgamma(p) + corr + p - p * log(p + q)
		+ (q - 0.5) * logrelerr(-p / (p + q));
    }
    else
	/* p and q are small. */
	return log(gamma(p) * (gamma(q) / gamma(p + q)));
}
