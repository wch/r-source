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

	/* These should always defined in ANSI C */

#ifndef DBL_DIG
#define	DBL_DIG		15
#endif

#ifndef DBL_MAX_EXP
#define	DBL_MAX_EXP	1024
#endif

/* Improvements by Martin Maechler, May 1977 */
/* Note that the code could be further improved by using */
/* pow_di(x, i)  instead of  pow(x, (double)i) */

#define MAXPLACES DBL_DIG

double prec(double x, double digits)
{
	double l10, pow10, sgn, p10, P10,
	  max10e = DBL_MAX_EXP * log10(2);	/* Max.expon. of 10 (=308.2547) */
	int e10, e2, do_round;
	
	if(x == 0.0) return x;
	digits = floor(digits+0.5);
	if (digits > MAXPLACES) {
        	return x;
	} else if (digits < 1)
		digits = 1;

	sgn = 1.0;
	if(x < 0.0) {
		sgn = -sgn;
		x = -x;
	}
	l10 = log10(x);
	e10 = (int)(digits-1-floor(l10));
	if(fabs(l10) < max10e - 2) {
		pow10 = pow(10.0, (double)e10);
		return(sgn*floor(x*pow10+0.5)/pow10);
	} else { /* -- LARGE or small -- */
		do_round = max10e - l10	 >= pow(10.0, -digits);
		e2 = (e10>0)? 16 : -16;
		p10 = pow(10.0, (double)e2);		x *= p10;
		P10 = pow(10.0, (double)e10-e2);	x *= P10;
		/*-- p10 * P10 = 10 ^ e10 */
		if(do_round) x += 0.5;
		x = floor(x) / p10;
		return(sgn*x/P10);
	}
}
