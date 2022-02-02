/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1999-2021 The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *  DESCRIPTION
 *
 *	The quantile function of the Poisson distribution.
 *
 *  METHOD
 *
 *	Uses the Cornish-Fisher Expansion to include a skewness
 *	correction to a normal approximation.  This gives an
 *	initial value which never seems to be off by more than
 *	1 or 2.	 A search is then conducted of values close to
 *	this initial start point.
 */

#include "nmath.h"
#include "dpq.h"

#ifdef DEBUG_qpois
# define R_DBG_printf(...) REprintf(__VA_ARGS__)
#else
# define R_DBG_printf(...)
#endif


#define _thisDIST_ pois
#define _dist_PARS_DECL_ double lambda
#define _dist_PARS_      lambda

#include "qDiscrete_search.h"
//        ------------------>  do_search() and all called by q_DISCRETE_*() below

double qpois(double p, double lambda, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(lambda))
	return p + lambda;
#endif
    if(!R_FINITE(lambda))
	ML_WARN_return_NAN;
    if(lambda < 0) ML_WARN_return_NAN;
    R_Q_P01_check(p);
    if(lambda == 0) return 0;
    if(p == R_DT_0) return 0;
    if(p == R_DT_1) return ML_POSINF;

    double
	mu = lambda,
	sigma = sqrt(lambda),
	// had gamma = sigma; PR#8058 should be kurtosis which is mu^-0.5 = 1/sigma
	gamma = 1.0/sigma;

     R_DBG_printf("qpois(p=%.12g, lambda=%.15g, l.t.=%d, log=%d):"
		  " mu=%g, sigma=%g, gamma=%g;\n",
		  p, lambda, lower_tail, log_p, mu, sigma, gamma);

     // never "needed" here (FIXME?):   q_DISCRETE_01_CHECKS();
     q_DISCRETE_BODY();
}
