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

double df(double x, double n1, double n2)
{
	double a;
	n1 = floor(n1 + 0.5);
	n2 = floor(n2 + 0.5);
	if (n1 <= 0.0 || n2 <= 0.0)
		DOMAIN_ERROR;
	if (x <= 0.0)
		return 0.0;
	a = (n1 / n2) * x;
	return pow(a, 0.5 * n1) * pow(1.0 + a, -0.5 * (n1 + n2))
			/ (x * beta(0.5 * n1, 0.5 * n2));
}
