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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "mathlib.h"
 *    double rf(double dfn, double dfd);
 *
 *  DESCRIPTION
 *
 *    Pseudo-random variates from an F distribution.
 *
 *  NOTES
 *
 *    This function calls rchisq to do the real work
 */

#include "Mathlib.h"

double rf(double n1, double n2)
{
    double v1, v2;
    if (
#ifdef IEEE_754
	isnan(n1) || isnan(n2) ||
#endif
	n1 <= 0.0 || n2 <= 0.0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    v1 = R_FINITE(n1) ? (rchisq(n1) / n1) : snorm();
    v2 = R_FINITE(n2) ? (rchisq(n2) / n2) : snorm();
    return v1 / v2;
}
