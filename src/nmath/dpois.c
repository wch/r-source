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
 *
 * Using the new algorithm of Clive Loader(1999) :
 *
 * The author of this software is Clive Loader, clive@bell-labs.com.
 * Copyright (c) 1999-2000 Lucent Technologies, Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, with the exceptions noted below,
 * and provided that this entire notice is included in all copies of any
 * software which is or includes a copy or modification of this software
 * and in all copies of the supporting documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHOR NOR LUCENT TECHNOLOGIES
 * MAKE ANY REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE
 * MERCHANTABILITY OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 *
 * This code provides functions dbinom(x,n,p) and dpois(x,lb) for computing
 * binomial and Poisson probabilities, that attempt to be accurate for
 * a full range of parameter values (standard algorithms are often
 * inaccurate with large parameters).
 *
 * NOTE: Loader's original code is now split (and merged into R's extras)
 *       into  ./dbinom.c, ./dpois.c and ./stirlerr.c
 */

#include "nmath.h"
#include "dpq.h"

double dpois(double x, double lambda, int give_log)
{

#ifdef IEEE_754
    if(ISNAN(x) || ISNAN(lambda))
	return x + lambda;
#endif
    if(fabs(x - floor(x + 0.5)) > 1e-7) {
	MATHLIB_WARNING("non-integer x = %f", x);
	return R_D__0;
    }
    if(lambda < 0.0) ML_ERR_return_NAN;

    if (x < 0 || !R_FINITE(x))
	return R_D__0;
    if (lambda == 0)
	return (x > 0) ? R_D__0 : R_D__1 ;
    if (x == 0)
	return R_D_exp(-lambda);
    /* else, normal case : */
#ifndef OLD_dpois
    if (give_log)
	return - (stirlerr(x) + bd0(x,lambda) + .5 * log(x) + M_LN_SQRT_2PI);
    else
	return exp(-stirlerr(x) - bd0(x,lambda)) / sqrt(2*M_PI*x);
#else
    return R_D_exp(x * log(lambda) - lambda - lgammafn(x + 1));
#endif
}

