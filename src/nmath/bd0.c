/*
 *  AUTHOR
 *	Catherine Loader, catherine@research.bell-labs.com.
 *	October 23, 2000.
 *
 *  Merge in to R:
 *	Copyright (C) 2000-2021 The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 *
 *
 *  DESCRIPTION
 *	Evaluates the "deviance part"
 *	bd0(x,M) :=  M * D0(x/M) = M*[ x/M * log(x/M) + 1 - (x/M) ] =
 *		  =  x * log(x/M) + M - x
 *	where M = E[X] = n*p (or = lambda), for	  x, M > 0
 *
 *	in a manner that should be stable (with small relative error)
 *	for all x and M=np. In particular for x/np close to 1, direct
 *	evaluation fails, and evaluation is based on the Taylor series
 *	of log((1+v)/(1-v)) with v = (x-M)/(x+M) = (x-np)/(x+np).
 *
 * Martyn Plummer had the nice idea to use log1p() and Martin Maechler
 * emphasized the extra need to control cancellation.
 *
 * MP:   t := (x-M)/M  ( <==> 1+t = x/M  ==>
 *
 * bd0 = M*[ x/M * log(x/M) + 1 - (x/M) ] = M*[ (1+t)*log1p(t) + 1 - (1+t) ]
 *     = M*[ (1+t)*log1p(t) - t ] =: M * p1log1pm(t) =: M * p1l1(t)
 * MM: The above is very nice, as the "simple" p1l1() function would be useful
 *    to have available in a fast numerical stable way more generally.
 */
#include "nmath.h"

double attribute_hidden bd0(double x, double np)
{
    if(!R_FINITE(x) || !R_FINITE(np) || np == 0.0) ML_WARN_return_NAN;

    if (fabs(x-np) < 0.1*(x+np)) {
	double
	    v = (x-np)/(x+np),  // might underflow to 0
	    s = (x-np)*v;
	if(fabs(s) < DBL_MIN) return s;
	double ej = 2*x*v;
	v *= v; // "v = v^2"
	for (int j = 1; j < 1000; j++) { /* Taylor series; 1000: no infinite loop
					as |v| < .1,  v^2000 is "zero" */
	    ej *= v;// = 2 x v^(2j+1)
	    double s_ = s;
	    s += ej/((j<<1)+1);
	    if (s == s_) { /* last term was effectively 0 */
#ifdef DEBUG_bd0
		REprintf("bd0(%g, %g): T.series w/ %d terms -> bd0=%g\n", x, np, j, s);
#endif
		return s;
	    }
	}
	MATHLIB_WARNING4("bd0(%g, %g): T.series failed to converge in 1000 it.; s=%g, ej/(2j+1)=%g\n",
			 x, np, s, ej/((2*1000)+1));
    }
    /* else:  | x - np |  is not too small */
    return(x*log(x/np)+np-x);
}


//- NOTA BENE (R Bugzilla PR#15628) :  Morten Welinder proposed  ebd0()  -- in file ./ebd0.c   as more accurate --------
