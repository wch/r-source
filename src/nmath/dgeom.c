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
 *    double dgeom(double x, double p);
 *
 *  DESCRIPTION
 *
 *    The density of the geometric distribution.
 */

#include "Mathlib.h"

double dgeom(double x, double p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(p)) return x + p;
#endif
    if (p <= 0 || p >= 1) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	warning("non-integer x = %f\n", x);
	return 0;
    }
    if (x < 0)
	return 0;
#ifdef IEEE_754
    if(!R_FINITE(x)) return 1;
#endif
    return p * pow(1 - p, x);
}
