/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 */

#include "Mathlib.h"

double pt(double x, double n)
{
    double val;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n))
	return x + n;
#endif
    if (n <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#ifdef IEEE_754
    if(!finite(x))
	return (x < 0) ? 0 : 1;
    if(!finite(n))
	return pnorm(x, 0.0, 1.0);
#endif
    val = 0.5 * pbeta(n / (n + x * x), n / 2.0, 0.5);
    return (x > 0.0) ? 1 - val : val;
}
