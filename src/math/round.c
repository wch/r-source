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

#ifndef HAVE_RINT
#define USE_BUILTIN_RINT
#endif

#ifdef USE_BUILTIN_RINT
#define RINT builtin_rint

	/* The largest integer which can be represented */
	/* exactly in floating point form. */

#define BIGGEST 4503599627370496.0E0		/* 2^52 for IEEE */

double builtin_rint(double x)
{
	double tmp;

	if (x != x) return x;			/* NaN */

	if (fabs(x) >= BIGGEST)			/* Already an integer */
		return x;

	if(x >= 0) {
		tmp = x + BIGGEST;
		return tmp - BIGGEST;
	}
	else {
		tmp = x - BIGGEST;
		return tmp + BIGGEST;
	}
}

#else
#define RINT rint
#endif

#define MAXPLACES 14

double rround(double x, double ndp)
{
	double pow10, sgn, intx;

	ndp = floor(ndp+0.5);
	if (ndp > MAXPLACES)
		ndp = MAXPLACES;
	pow10 = pow(10.0, ndp);
	sgn = 1.0;
	if(x < 0.0) {
		sgn = -sgn;
		x = -x;
	}
	if (ndp > 0.0) {
		intx = floor(x);
		x = x - intx;
	} else {
		intx = 0.0;
	}
	return sgn * (intx + RINT(x*pow10)/pow10);
}


double trunc(double x)
{
	if(x >= 0) return floor(x);
	else return ceil(x);
}
