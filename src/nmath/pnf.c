/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998	Ross Ihaka
 *  Copyright (C) 2000, 2005 The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *  DESCRIPTION
 *
 *	The distribution function of the non-central F distribution.
 */

#include "nmath.h"
#include "dpq.h"

double pnf(double x, double n1, double n2, double ncp,
	   int lower_tail, int log_p)

{
    double y;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2) || ISNAN(ncp))
	return x + n2 + n1 + ncp;
#endif
    if (n1 <= 0. || n2 <= 0. || ncp < 0) ML_ERR_return_NAN;
    if (!R_FINITE(ncp)) ML_ERR_return_NAN;
    if (!R_FINITE(n1) && !R_FINITE(n2)) /* both +Inf */
	ML_ERR_return_NAN;

    R_P_bounds_01(x, 0., ML_POSINF);

    if (n2 > 1e8) /* avoid problems with +Inf and loss of accuracy */
	return pnchisq(x * n1, n1, ncp, lower_tail, log_p);

    y = (n1 / n2) * x;
    return pnbeta(y/(1 + y), n1 / 2., n2 / 2., ncp, lower_tail, log_p);
}
