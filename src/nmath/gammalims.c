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
 *    void gammalims(double *xmin, double *xmax);
 *
 *  DESCRIPTION
 *
 *    This function alculates the minimum and maximum legal bounds
 *    for x in gamma(x).  These are not the only bounds, but they
 *    are the only non-trivial ones to calculate.
 *
 *  NOTES
 *
 *    This routine is a translation into C of a Fortran subroutine
 *    by W. Fullerton of Los Alamos Scientific Laboratory.
 */

#include "Mathlib.h"

/* FIXME: We need an ifdef'ed version of this which gives  */
/* the exact values when we are using IEEE 754 arithmetic. */

void gammalims(double *xmin, double *xmax)
{
    double alnbig, alnsml, xln, xold;
    int i;

    alnsml = log(d1mach(1));
    *xmin = -alnsml;
    for (i=1; i<=10; ++i) {
	xold = *xmin;
	xln = log(*xmin);
	*xmin -= *xmin * ((*xmin + .5) * xln - *xmin - .2258 + alnsml) /
		(*xmin * xln + .5);
	if (fabs(*xmin - xold) < .005) {
	    *xmin = -(*xmin) + .01;
	    goto find_xmax;
	}
    }

    /* unable to find xmin */

    ML_ERROR(ME_NOCONV);
    *xmin = *xmax = ML_NAN;

find_xmax:

    alnbig = log(d1mach(2));
    *xmax = alnbig;
    for (i=1; i<=10; ++i) {
	xold = *xmax;
	xln = log(*xmax);
	*xmax -= *xmax * ((*xmax - .5) * xln - *xmax + .9189 - alnbig) /
		(*xmax * xln - .5);
	if (fabs(*xmax - xold) < .005) {
	    *xmax += -.01;
	    goto done;
	}
    }

    /* unable to find xmax */

    ML_ERROR(ME_NOCONV);
    *xmin = *xmax = ML_NAN;

done:
    *xmin = fmax2(*xmin, -(*xmax) + 1);
}
