/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1999-2021 The R Core Team
 *  Copyright (C) 2003-2021 The R Foundation
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
 *	The quantile function of the binomial distribution.
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

#ifdef DEBUG_qbinom
# define R_DBG_printf(...) REprintf(__VA_ARGS__)
#else
# define R_DBG_printf(...)
#endif


#define _thisDIST_ binom
#define _dist_PARS_DECL_ double n, double pr
#define _dist_PARS_      n, pr
#define _dist_MAX_y  n
//                  ===  Binomial  Y <= n

#include "qDiscrete_search.h"
//        ------------------>  do_search() and all called by q_DISCRETE_*() below

double qbinom(double p, double n, double pr, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(n) || ISNAN(pr))
	return p + n + pr;
#endif
    if(!R_FINITE(n) || !R_FINITE(pr))
	ML_WARN_return_NAN;
    /* if log_p is true, p = -Inf is a legitimate value */
    if(!R_FINITE(p) && !log_p)
	ML_WARN_return_NAN;

    n = R_forceint(n);

    if (pr < 0 || pr > 1 || n < 0)
	ML_WARN_return_NAN;

    R_Q_P01_boundaries(p, 0, n);

    if (pr == 0. || n == 0) return 0.;
    if (pr == 1.)           return n; /* covers the full range of the distribution */

    // (NB: unavoidable cancellation for pr ~= 1)
    double
	q = 1 - pr,
	mu = n * pr,
	sigma = sqrt(n * pr * q),
	gamma = (q - pr) / sigma;

    R_DBG_printf("qbinom(p=%.12g, n=%.15g, pr=%.7g, l.t.=%d, log=%d): sigma=%g, gamma=%g;\n",
		 p, n,pr, lower_tail, log_p, sigma, gamma);

    q_DISCRETE_01_CHECKS();
    q_DISCRETE_BODY();
}
