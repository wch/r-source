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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double df(double x, double n1, double n2);
 *
 *  DESCRIPTION
 *
 *    The density function of the F distribution.
 */

#include "Mathlib.h"

double df(double x, double n1, double n2)
{
	double a;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2))
	return x + n1 + n2;
#endif
	if (n1 <= 0 || n2 <= 0) {
	    ML_ERROR(ME_DOMAIN);
	    return ML_NAN;
	}
	if (x <= 0.0)
		return 0.0;
	a = (n1 / n2) * x;
	return pow(a, 0.5 * n1) * pow(1.0 + a, -0.5 * (n1 + n2))
			/ (x * beta(0.5 * n1, 0.5 * n2));
}
