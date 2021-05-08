/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 2000-2021 The R Core Team
 *  Copyright (C) 2005-2021 The R Foundation
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
 *  SYNOPSIS
 *
 *	#include <Rmath.h>
 *      double qnbinom_mu(double p, double size, double mu,
 *                     int lower_tail, int log_p)
 *
 *  DESCRIPTION
 *
 *	The quantile function of the negative binomial distribution,
 *      for the (size, mu) parametrizations
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

#ifdef DEBUG_qnbinom
# define R_DBG_printf(...) REprintf(__VA_ARGS__)
#else
# define R_DBG_printf(...)
#endif

#define _thisDIST_ nbinom_mu
#define _dist_PARS_DECL_ double size, double mu
#define _dist_PARS_      size, mu

#include "qDiscrete_search.h"
//        ------------------>  do_search() and all called by q_DISCRETE_*() below

double qnbinom_mu(double p, double size, double mu, int lower_tail, int log_p)
{
    if (size == ML_POSINF) // limit case: Poisson
	return(qpois(p, mu, lower_tail, log_p));

#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(size) || ISNAN(mu))
	return p + size + mu;
#endif

    if (mu == 0 || size == 0) return 0;
    if (mu <  0 || size <  0) ML_WARN_return_NAN;

    R_Q_P01_boundaries(p, 0, ML_POSINF);

    double
	Q = 1 + mu/size, // (size+mu)/size = 1 / prob
	P = mu/size,     // = (1 - prob) * Q = (1 - prob) / prob  =  Q - 1
	sigma = sqrt(size * P * Q),
	gamma = (Q + P)/sigma;

    R_DBG_printf("qnbinom_mu(p=%.12g, size=%.15g, mu=%g, l.t.=%d, log=%d):"
		 " mu=%g, sigma=%g, gamma=%g;\n",
		 p, size, mu, lower_tail, log_p, mu, sigma, gamma);

    q_DISCRETE_01_CHECKS();
    q_DISCRETE_BODY();
}
