/*
 *  Mathlib : A C Library of Special Functions
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double dhyper(double x, double NR, double NB, double n);
 *
 *  DESCRIPTION
 *
 *    The quantile function of the hypergeometric distribution.
 */

#include "Mathlib.h"

double qhyper(double x, double NR, double NB, double n)
{
/* This is basically the same code as  ./phyper.c -- keep in sync! */
    double N, xstart, xend, xr, xb, sum, term;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(NR) || ISNAN(NB) || ISNAN(n))
	return x + NR + NB + n;
    if(!R_FINITE(x) || !R_FINITE(NR) || !R_FINITE(NB) || !R_FINITE(n)) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
#endif
    NR = floor(NR + 0.5);
    NB = floor(NB + 0.5);
    N = NR + NB;
    n = floor(n + 0.5);
    if (x < 0 || x > 1 || NR < 0 || NR < 0 || n < 0 || n > N) {
	ML_ERROR(ME_DOMAIN);
	return ML_NAN;
    }
    xstart = fmax2(0, n - NB);
    xend = fmin2(n, NR);
    if(x <= 0) return xstart;
    if(x >= 1) return xend;
    xr = xstart;
    xb = n - xr;
    term = exp(lfastchoose(NR, xr) + lfastchoose(NB, xb)
	       - lfastchoose(N, n));
    NR = NR - xr;
    NB = NB - xb;
    sum = term;
    while(sum < x && xr < xend) {
	xr++;
	NB++;
	term *= (NR / xr) * (xb / NB);
	sum += term;
	xb--;
	NR--;
    }
    return xr;
}
