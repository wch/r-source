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
 *  SYNOPSIS
 *
 *	double dnorm4(double x, double mu, double sigma, int give_log)
 *	      {dnorm (..) is synonymous and preferred inside R}
 *
 *  DESCRIPTION
 *
 *	Compute the density of the normal distribution.
 */

#include "Mathlib.h"

double dnorm4(double x, double mu, double sigma, int give_log)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma))
	return x + mu + sigma;
#endif
    if (sigma <= 0) ML_ERR_return_NAN;

    x = (x - mu) / sigma;

    return (give_log ?
	    -(M_LN_SQRT_2PI  +	0.5 * x * x + log(sigma)) :
	    M_1_SQRT_2PI * exp(-0.5 * x * x)  /	  sigma);
    /* M_1_SQRT_2PI = 1 / sqrt(2 * pi) */
}

#ifndef NEW_NORM_ONLY
/* These are defined in Mathlib.h */
#undef dnorm
#undef pnorm
#undef qnorm
double dnorm(double x, double mu, double sigma)
{
    return dnorm4(x, mu, sigma, 0);
}
double pnorm(double x, double mu, double sigma)
{
    return pnorm5(x, mu, sigma, 1, 0);
}
double qnorm(double x, double mu, double sigma)
{
    return qnorm5(x, mu, sigma, 1, 0);    
}
#endif
