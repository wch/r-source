/*
 *  AUTHOR
 *    Peter Ruckdeschel, peter.ruckdeschel@uni-bayreuth.de.
 *    April 13, 2006.
 *
 *  Merge in to R:
 *	Copyright (C) 2006 The R Core Development Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *
 *  DESCRIPTION
 *
 *	The density function of the non-central F distribution ---
 *  obtained by differentiating the corresp. cumulative distribution function
 *  using dnbeta.
 *  For n1 < 2, since the F density has a singularity as x -> Inf.
 */

#include "nmath.h"
#include "dpq.h"

double dnf(double x, double n1, double n2, double ncp, int give_log)
{
    double y, z, f;

#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2) || ISNAN(ncp))
	return x + n2 + n1 + ncp;
#endif

    /* want to compare dnf(ncp=0) behavior with df() one, hence *NOT* :
     * if (ncp == 0)
     *   return df(x, n1, n2, give_log); */

    if (n1 <= 0. || n2 <= 0. || ncp < 0) ML_ERR_return_NAN;
    if (x < 0.)	 return(R_D__0);
    if (!R_FINITE(ncp)) /* ncp = +Inf -- FIXME?: in some cases, limit exists */
	ML_ERR_return_NAN;

    /* This is not correct for  n1 == 2, ncp > 0 - and seems unneeded:
     *  if (x == 0.) return(n1 > 2 ? R_D__0 : (n1 == 2 ? R_D__1 : ML_POSINF));
     */
    if (!R_FINITE(n1) && !R_FINITE(n2)) { /* both +Inf */
	/* PR: not sure about this (taken from  ncp==0)  -- FIXME ? */
	if(x == 1.) return ML_POSINF;
	/* else */  return R_D__0;
    }
    if (!R_FINITE(n2)) /* i.e.  = +Inf */
	return n1* dnchisq(x*n1, n1, ncp, give_log);
    /*	 ==  dngamma(x, n1/2, 2./n1, ncp, give_log)  -- but that does not exist */
    if (n1 > 1e14 && ncp < 1e7) {
	/* includes n1 == +Inf: code below is inaccurate there */
	f = 1 + ncp/n1; /* assumes  ncp << n1 [ignores 2*ncp^(1/2)/n1*x term] */
	z = dgamma(1./x/f, n2/2, 2./n2, give_log);
	return give_log ? z - 2*log(x) - log(f) : z / (x*x) / f;
    }

    y = (n1 / n2) * x;
    z = dnbeta(y/(1 + y), n1 / 2., n2 / 2., ncp, give_log);
    return  give_log ?
	z + log(n1) - log(n2) - 2 * log1p(y) :
	z * (n1 / n2) /(1 + y) / (1 + y);
}



