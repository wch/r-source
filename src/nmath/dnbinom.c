/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000 and Feb, 2001.
 *
 *  Merge in to R:
 *	Copyright (C) 2000--2008, The R Core Development Team
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
 *  http://www.r-project.org/Licenses/
 *
 *
 * DESCRIPTION
 *
 *   Computes the negative binomial distribution. For integer n,
 *   this is probability of x failures before the nth success in a
 *   sequence of Bernoulli trials. We do not enforce integer n, since
 *   the distribution is well defined for non-integers,
 *   and this can be useful for e.g. overdispersed discrete survival times.
 */

#include "nmath.h"
#include "dpq.h"

double dnbinom(double x, double size, double prob, int give_log)
{ 
    double ans, p;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(size) || ISNAN(prob))
        return x + size + prob;
#endif

    if (prob <= 0 || prob > 1 || size <= 0) ML_ERR_return_NAN;
    R_D_nonint_check(x);
    if (x < 0 || !R_FINITE(x)) return R_D__0;
    x = R_D_forceint(x);

    ans = dbinom_raw(size, x+size, prob, 1-prob, give_log);
    p = ((double)size)/(size+x);
    return((give_log) ? log(p) + ans : p * ans);
}
