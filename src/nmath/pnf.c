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
 *    #include "Mathlib.h"
 *    double pnf(double x, double n1, double n2, double ncp);
 *
 *  DESCRIPTION
 *
 *    The distribution function of the non-central F distribution.
 */

#include "Mathlib.h"

double pnf(double x, double n1, double n2, double ncp)
{
    double y;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2) || ISNAN(ncp))
	return x + n2 + n1 + ncp;
#endif
    if (n1 <= 0.0 || n2 <= 0.0 || ncp < 0) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    if (x <= 0.0)
	return 0.0;
    y = (n1 / n2) * x;
    return pnbeta(y/(1 + y), n1 / 2.0, n2 / 2.0, ncp);
}
