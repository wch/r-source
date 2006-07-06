/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2006 The R Development Core Team
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "nmath.h"
#include "dpq.h"

double qnf(double p, double n1, double n2, double ncp, int lower_tail, 
	   int log_p)
{
    double y;
    
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n1) || ISNAN(n2) || ISNAN(ncp))
	return p + n1 + n2 + ncp;
#endif
    if (n1 <= 0. || n2 <= 0. || ncp < 0) ML_ERR_return_NAN;
    if (!R_FINITE(ncp)) ML_ERR_return_NAN;
    if (!R_FINITE(n1) && !R_FINITE(n2)) ML_ERR_return_NAN;
    R_Q_P01_boundaries(p, 0, ML_POSINF);

    if (n2 > 1e8) /* avoid problems with +Inf and loss of accuracy */
	return qnchisq(p, n1, ncp, lower_tail, log_p)/n1;

    y = qnbeta(p, n1 / 2., n2 / 2., ncp, lower_tail, log_p);
    return y/(1-y) * (n2/n1);
}
