/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Mathlib.h"

double dhyper(double x, double NR, double NB, double n)
{
	double N;
	x = floor(x + 0.5);
	NR = floor(NR + 0.5);
	NB = floor(NB + 0.5);
	N = NR + NB;
	n = floor(n + 0.5);
	if (NR < 0 || NB < 0 || n <= 0 || n > N)
		DOMAIN_ERROR;
	if (x < fmax2(0, n - NB) || x > fmin2(n, NR))
		return 0.0;
	return exp(lfastchoose(NR, x) + lfastchoose(NB, n - x)
		    - lfastchoose(N, n));
}
