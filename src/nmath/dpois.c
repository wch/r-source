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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  DESCRIPTION
 *
 *    The density function of the Poisson distribution.
 */

#include "Mathlib.h"
#include "dpq.h"

double dpois(double x, double lambda, int give_log)
{

#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(lambda))
	return x + lambda;
#endif
    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	warning("non-integer x = %f", x);
	return R_D__0;
    }
    if(lambda < 0.0) ML_ERR_return_NAN;

    if (x < 0)
	return R_D__0;
    if(!R_FINITE(x))
	return R_D__0;

    if (lambda == 0.0)
	    return (x > 0) ? R_D__0 : R_D__1 ;

    return R_D_exp(x * log(lambda) - lambda - lgammafn(x + 1));
}
