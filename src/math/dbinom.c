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

double dbinom(double x, double n, double p)
{
	x = floor(x + 0.5);
	n = floor(n + 0.5);
	if (n <= 0 || p < 0 || p > 1)
		DOMAIN_ERROR;
	if (x < 0 || x > n)
		return 0;
	if (p == 0)
	        return (x == 0) ? 1 : 0;
	if (p == 1)
	        return (x == n) ? 1 : 0;
	return exp(lfastchoose(n, x) + log(p) * x + (n - x) * log(1 - p));
}
