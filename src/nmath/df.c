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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  DESCRIPTION
 *
 *    The density function of the F distribution.
 */

#include "Mathlib.h"

double df(double x, double n1, double n2, int give_log)
{
    double a;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(n1) || ISNAN(n2))
	return x + n1 + n2;
#endif
    if (n1 <= 0 || n2 <= 0) ML_ERR_return_NAN;

    if (x <= 0.0)
	return R_D__0;
    a = (n1 / n2) * x;
    n1 /= 2;
    n2 /= 2;
    return give_log ?
	log(a) * n1  + log(1. + a)*(- (n1 + n2)) - log(x) - lbeta(n1, n2) :
	pow(a  , n1) * pow(1. + a , - (n1 + n2)) /  (  x  *  beta(n1, n2));
}
