/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2014  The R Core Team
 *  Copyright (C) 2002--2009  The R Foundation
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
# include <config.h>
#endif

#include <Defn.h>
#include <float.h>  /* for DBL_MAX */
#include <Graphics.h>
#include <Print.h>
#include <Rmath.h> // for Rexp10, imax2

/* used in graphics and grid */
SEXP CreateAtVector(double *axp, double *usr, int nint, Rboolean logflag)
{
/*	Create an  'at = ...' vector for  axis(.)
 *	i.e., the vector of tick mark locations,
 *	when none has been specified (= default).
 *
 *	axp[0:2] = (x1, x2, nInt), where x1..x2 are the extreme tick marks
 *		   {unless in log case, where nInt \in {1,2,3 ; -1,-2,....}
 *		    and the `nint' argument is used *instead*.}

 *	The resulting REAL vector must have length >= 1, ideally >= 2
 */
    SEXP at = R_NilValue;/* -Wall*/
    double umin, umax, dn, rng, small;
    int i, n, ne;
    if (!logflag || axp[2] < 0) { /* --- linear axis --- Only use axp[] arg. */
	n = (int)(fabs(axp[2]) + 0.25);/* >= 0 */
	dn = imax2(1, n);
	rng = axp[1] - axp[0];
	small = fabs(rng)/(100.*dn);
	at = allocVector(REALSXP, n + 1);
	for (i = 0; i <= n; i++) {
	    REAL(at)[i] = axp[0] + (i / dn) * rng;
	    if (fabs(REAL(at)[i]) < small)
		REAL(at)[i] = 0;
	}
    }
    else { /* ------ log axis ----- */
	Rboolean reversed = FALSE;

	n = (int)(axp[2] + 0.5);
	/* {xy}axp[2] for 'log': GLpretty() [./graphics.c] sets
	   n < 0: very small scale ==> linear axis, above, or
	   n = 1,2,3.  see switch() below */
	umin = usr[0];
	umax = usr[1];
	if (umin > umax) {
	    reversed = (axp[0] > axp[1]);
	    if (reversed) {
		/* have *reversed* log axis -- whereas
		 * the switch(n) { .. } below assumes *increasing* values
		 * --> reverse axis direction here, and reverse back at end */
		umin = usr[1];
		umax = usr[0];
		dn = axp[0]; axp[0] = axp[1]; axp[1] = dn;
	    }
	    else {
		/* can the following still happen... ? */
		warning("CreateAtVector \"log\"(from axis()): "
			"usr[0] = %g > %g = usr[1] !", umin, umax);
	    }
	}
	/* allow a fuzz since we will do things like 0.2*dn >= umin */
	umin *= 1 - 1e-12;
	umax *= 1 + 1e-12;

	dn = axp[0];
	if (dn < DBL_MIN) {/* was 1e-300; now seems too cautious */
	    warning("CreateAtVector \"log\"(from axis()): axp[0] = %g !", dn);
	    if (dn <= 0) /* real trouble (once for Solaris) later on */
		error("CreateAtVector [log-axis()]: axp[0] = %g < 0!", dn);
	}

	/* You get the 3 cases below by
	 *  for (y in 1e-5*c(1,2,8))  plot(y, log = "y")
	 */
	switch(n) {
	case 1: /* large range:	1	 * 10^k */
	    i = (int)(floor(log10(axp[1])) - ceil(log10(axp[0])) + 0.25);
	    ne = i / nint + 1;
#ifdef DEBUG_axis
	    REprintf("CreateAtVector [log-axis(), case 1]: (nint, ne) = (%d,%d)\n",
		     nint, ne);
#endif
	    if (ne < 1)
		error("log - axis(), 'at' creation, _LARGE_ range: "
		      "ne = %d <= 0 !!\n"
		      "\t axp[0:1]=(%g,%g) ==> i = %d;	nint = %d",
		      ne, axp[0],axp[1], i, nint);
	    rng = Rexp10((double)ne); /* >= 10 */
	    n = 0;
	    while (dn < umax) {
		n++;
		dn *= rng;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _LARGE_ range: "
		      "invalid {xy}axp or par; nint=%d\n"
		      "	 axp[0:1]=(%g,%g), usr[0:1]=(%g,%g); i=%d, ni=%d",
		      nint, axp[0],axp[1], umin,umax, i,ne);
	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    while (dn < umax) {
		REAL(at)[n++] = dn;
		dn *= rng;
	    }
	    break;

	case 2: /* medium range:  1, 5	  * 10^k */
	    n = 0;
	    if (0.5 * dn >= umin) n++;
	    for (;;) {
		if (dn > umax) break;
		n++;
		if (5 * dn > umax) break;
		n++;
		dn *= 10;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _MEDIUM_ range: "
		      "invalid {xy}axp or par;\n"
		      "	 axp[0]= %g, usr[0:1]=(%g,%g)",
		      axp[0], umin,umax);

	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    if (0.5 * dn >= umin) REAL(at)[n++] = 0.5 * dn;
	    for (;;) {
		if (dn > umax) break;		REAL(at)[n++] = dn;
		if (5 * dn > umax) break;	REAL(at)[n++] = 5 * dn;
		dn *= 10;
	    }
	    break;

	case 3: /* small range:	 1,2,5,10 * 10^k */
	    n = 0;
	    if (0.2 * dn >= umin) n++;
	    if (0.5 * dn >= umin) n++;
	    for (;;) {
		if (dn > umax) break;
		n++;
		if (2 * dn > umax) break;
		n++;
		if (5 * dn > umax) break;
		n++;
		dn *= 10;
	    }
	    if (!n)
		error("log - axis(), 'at' creation, _SMALL_ range: "
		      "invalid {xy}axp or par;\n"
		      "	 axp[0]= %g, usr[0:1]=(%g,%g)",
		      axp[0], umin,umax);
	    at = allocVector(REALSXP, n);
	    dn = axp[0];
	    n = 0;
	    if (0.2 * dn >= umin) REAL(at)[n++] = 0.2 * dn;
	    if (0.5 * dn >= umin) REAL(at)[n++] = 0.5 * dn;
	    for (;;) {
		if (dn > umax) break;		REAL(at)[n++] = dn;
		if (2 * dn > umax) break;	REAL(at)[n++] = 2 * dn;
		if (5 * dn > umax) break;	REAL(at)[n++] = 5 * dn;
		dn *= 10;
	    }
	    break;
	default:
	    error("log - axis(), 'at' creation: INVALID {xy}axp[3] = %g",
		  axp[2]);
	}

	if (reversed) {/* reverse back again - last assignment was at[n++]= . */
	    for (i = 0; i < n/2; i++) { /* swap( at[i], at[n-i-1] ) : */
		dn = REAL(at)[i];
		REAL(at)[i] = REAL(at)[n-i-1];
		REAL(at)[n-i-1] = dn;
	    }
	}
    } /* linear / log */
    return at;
}

