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
 *    The quantile function of the hypergeometric distribution.
 */

#include "Mathlib.h"
#include "dpq.h"

double qhyper(double p, double NR, double NB, double n,
	      int lower_tail, int log_p)
{
/* This is basically the same code as  ./phyper.c -- keep in sync! */
    double N, xstart, xend, xr, xb, sum, term;
#ifdef IEEE_754
    if (ISNAN(p) || ISNAN(NR) || ISNAN(NB) || ISNAN(n))
	return p + NR + NB + n;
#endif
    if(!R_FINITE(p) || !R_FINITE(NR) || !R_FINITE(NB) || !R_FINITE(n))
	ML_ERR_return_NAN;
    R_Q_P01_check(p);

    NR = floor(NR + 0.5);
    NB = floor(NB + 0.5);
    N = NR + NB;
    n = floor(n + 0.5);
    if (NR < 0 || NR < 0 || n < 0 || n > N)
	ML_ERR_return_NAN;

    xstart = fmax2(0, n - NB);
    xend = fmin2(n, NR);
    if(p == R_DT_0) return xstart;
    if(p == R_DT_1) return xend;

    xr = xstart;
    xb = n - xr;

    term = exp(lfastchoose(NR, xr) + lfastchoose(NB, xb) - lfastchoose(N, n));
    NR = NR - xr;
    NB = NB - xb;
    sum = term;
    if(!lower_tail || log_p) {
	p = R_DT_qIv(p);
    }
    p *= 1 - 64*DBL_EPSILON;
    while(sum < p && xr < xend) {
	xr++;
	NB++;
	term *= (NR / xr) * (xb / NB);
	sum += term;
	xb--;
	NR--;
    }
    return xr;
}
