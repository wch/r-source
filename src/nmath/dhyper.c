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
 *    The density of the hypergeometric distribution.
 */

#include "Mathlib.h"
#include "dpq.h"

double dhyper(double x, double NR, double NB, double n, int give_log)
{
    double N;
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(NR) || ISNAN(NB) || ISNAN(n))
	return x + NR + NB + n;
#endif
    x = floor(x + 0.5);
    NR = floor(NR + 0.5);
    NB = floor(NB + 0.5);
    N = NR + NB;
    n = floor(n + 0.5);
    if (NR < 0 || NB < 0 || n < 0 || n > N) {
	ML_ERR_return_NAN;
    }
    if (x < fmax2(0, n - NB) || x > fmin2(n, NR))
	return R_D__0;
    return R_D_exp(lfastchoose(NR, x) + lfastchoose(NB, n - x)
		   - lfastchoose(N, n));
}
