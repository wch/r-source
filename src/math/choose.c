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

double lfastchoose(double n, double k)
{
	return lgamma(n + 1.0) - lgamma(k + 1.0) - lgamma(n - k + 1.0);
}

double fastchoose(double n, double k)
{
	return exp(lfastchoose(n, k));
}

double lchoose(double n, double k)
{
	n = floor(n + 0.5);
	k = floor(k + 0.5);
	if (k < 0 || n < k)
		DOMAIN_ERROR;
	return lfastchoose(n, k);
}

double choose(double n, double k)
{
	n = floor(n + 0.5);
	k = floor(k + 0.5);
	if (k < 0 || n < k)
		DOMAIN_ERROR;
	return floor(exp(lfastchoose(n, k)) + 0.5);
}
