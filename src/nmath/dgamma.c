/*
 *  AUTHOR
 *    Catherine Loader, catherine@research.bell-labs.com.
 *    October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000, The R Core Development Team
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
 *
 * DESCRIPTION
 *
 *   Gamma density,
 *                    lb^r x^{r-1} exp(-lb*x)
 *        p(x;r,lb) = -----------------------
 *                            (r-1)!
 *
 *   If USE_SCALE is defined below, the lb argument will be interpreted
 *   as a scale parameter (i.e. replace lb by 1/lb above).
 *   Otherwise, it is interpreted as a rate parameter, as above.
 */

#include "nmath.h"
#include "dpq.h"

#define USE_SCALE

double dgamma(double x, double r, double lambda, int give_log)
{ 
    double pr;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(r) || ISNAN(lambda))
        return x + r + lambda;
#endif

    if (r <= 0 || lambda < 0) ML_ERR_return_NAN;
    if (x <= 0) return(R_D__0);

#ifdef USE_SCALE
    lambda = 1.0/lambda;
#endif

    if (r < 1) { 
	pr = dpois_raw(r,lambda*x,give_log);
	return( (give_log) ?  pr + log(r/x) : pr*r/x );
    }
    /* else  r >= 1 */
    pr = dpois_raw(r-1,lambda*x,give_log);
    return( (give_log) ? pr + log(lambda) : lambda*pr);
}
