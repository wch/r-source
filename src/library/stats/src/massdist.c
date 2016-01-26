/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1996-2012	The R Core Team
 *  Copyright (C) 2005		The R Foundation

 *  "HACKED" to allow weights by Adrian Baddeley
 *  Changes indicated by 'AB'
 * -------
 *  FIXME   Does he want 'COPYRIGHT' ?
 * -------
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h> // includes math.h
#include <Rinternals.h>

/* NB: this only works in the lower half of y, but pads with zeros. */
SEXP BinDist(SEXP sx, SEXP sw, SEXP slo, SEXP shi, SEXP sn)
{
    PROTECT(sx = coerceVector(sx, REALSXP)); 
    PROTECT(sw = coerceVector(sw, REALSXP));
    int n = asInteger(sn);
    if (n == NA_INTEGER || n <= 0) error("invalid '%s' argument", "n");
    SEXP ans = allocVector(REALSXP, 2*n);
    PROTECT(ans);
    double xlo = asReal(slo), xhi = asReal(shi);
    double *x = REAL(sx), *w = REAL(sw), *y = REAL(ans);

    int ixmin = 0, ixmax = n - 2;
    double xdelta = (xhi - xlo) / (n - 1);

    for(int i = 0; i < 2*n ; i++) y[i] = 0;

    for(R_xlen_t i = 0; i < XLENGTH(sx) ; i++) {
	if(R_FINITE(x[i])) {
	    double xpos = (x[i] - xlo) / xdelta;
	    int ix = (int) floor(xpos);
	    double fx = xpos - ix;
	    double wi = w[i];
	    if(ixmin <= ix && ix <= ixmax) {
		y[ix] += (1 - fx) * wi;
		y[ix + 1] += fx * wi;
	    }
	    else if(ix == -1) y[0] += fx * wi;
	    else if(ix == ixmax + 1) y[ix] += (1 - fx) * wi;
	}
    }
    UNPROTECT(3);
    return ans;
}
