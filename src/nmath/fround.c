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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double fround(double x, double digits);
 *
 *  DESCRIPTION
 *
 *    Rounds "x" to "digits" decimal digits.
 */

#include "Mathlib.h"

#ifndef HAVE_RINT
#define USE_BUILTIN_RINT
#endif

#ifdef USE_BUILTIN_RINT
#define R_rint private_rint

	/* The largest integer which can be represented */
	/* exactly in floating point form. */

#define BIGGEST 4503599627370496.0E0		/* 2^52 for IEEE */

static double private_rint(double x)
{
    static double biggest = BIGGEST;
    double tmp;

    if (x != x) return x;			/* NaN */

    if (fabs(x) >= biggest)			/* Already integer */
	return x;

    if(x >= 0) {
	tmp = x + biggest;
	return tmp - biggest;
    }
    else {
	tmp = x - biggest;
	return tmp + biggest;
    }
}

#else
#define R_rint rint
#endif

double fround(double x, double digits)
{
    double pow10, sgn, intx;
    static double maxdigits = DBL_DIG - 1;
    /* FIXME: Hmm, have quite a host of these:
       
       1) ./fprec.c   uses  MAXPLACES = DLB_DIG  ``instead''
       2) ../main/coerce.c   & ../main/deparse.c have  DBL_DIG  directly
       3) ../main/options.c has   #define MAX_DIGITS 22  for options(digits)

       Really should decide on a (Platform.h dependent ?) global MAX_DIGITS.
       --MM--
     */
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(digits))
	return x + digits;
    if(!finite(x)) return x;
#endif

    digits = floor(digits + 0.5);
    if (digits > maxdigits)
	digits = maxdigits;
    pow10 = pow(10.0, digits);
    sgn = 1.0;
    if(x < 0.0) {
	sgn = -sgn;
	x = -x;
    }
    if (digits > 0.0) {
	intx = floor(x);
	x = x - intx;
    } else {
	intx = 0.0;
    }
    return sgn * (intx + R_rint(x * pow10) / pow10);
}
