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
 *    The density of the "Student" t distribution.
 */

#include "nmath.h"
#include "dpq.h"

double dt(double x, double n, int give_log)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n))
	return x + n;
#endif
    if (n <= 0.) ML_ERR_return_NAN;

    if(!R_FINITE(x))
	return R_D__0;
    if(!R_FINITE(n))
	return dnorm(x, 0.0, 1.0, give_log);
    return give_log ?
	log(1. + x * x / n)*(-0.5 * (n + 1.)) -.5*log(n) - lbeta(.5, .5 * n) :
	pow(1. + x * x / n , -0.5 * (n + 1.)) / (sqrt(n) *  beta(.5, .5 * n));
}






