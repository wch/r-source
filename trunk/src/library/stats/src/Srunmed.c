/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2002 Martin Maechler <maechler@stat.math.ethz.ch>
 *  Copyright (C) 2003       The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
 */
/* --- $Id: Srunmed.c,v 1.1 2003/12/09 18:21:36 ripley Exp $ */

#include "modreg.h"
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

void Srunmed(double* y, double* smo, int* n, Sint* band,
	     Sint* end_rule, Sint* debug)
{
/*
 *  Computes "Running Median" smoother with medians of 'band'

 *  Input:
 *	y(n)	- responses in order of increasing predictor values
 *	n	- number of observations
 *	band	- span of running medians (MUST be ODD !!)
 *	end_rule -- 0: Keep original data at ends {j; j < b2 | j > n-b2}
 *		 -- 1: Constant ends = median(y[1],..,y[bw]) "robust"
 *  Output:
 *	smo(n)	- smoothed responses

 * NOTE:  The 'end' values are just copied !! this is fast but not too nice !
 */

/* Local variables */
    double rmed, rmin, temp, rnew, yout, yi;
    double rbe, rtb, rse, yin, rts;
    int imin, ismo, i, j, first, last, band2, kminus, kplus;

    int bw = (int)*band;

    double *scrat = (double *) R_alloc(bw, sizeof(double));
    /*was  malloc( (unsigned) bw * sizeof(double));*/

    if(bw > *n)
	error(_("bandwidth/span of running medians is larger than n"));

/* 1. Compute  'rmed' := Median of the first 'band' values
   ======================================================== */

    for (i = 0; i < bw; ++i)
	scrat[i] = y[i];

    /* find minimal value  rmin = scrat[imin] <= scrat[j] */
    rmin = scrat[0]; imin = 0;
    for (i = 1; i < bw; ++i)
	if (scrat[i] < rmin) {
	    rmin = scrat[i]; imin = i;
	}
    /* swap scrat[0] <-> scrat[imin] */
    temp = scrat[0]; scrat[0] = rmin; scrat[imin] = temp;
    /* sort the rest of of scrat[] by bubble (?) sort -- */
    for (i = 2; i < bw; ++i) {
	if (scrat[i] < scrat[i - 1]) {/* find the proper place for scrat[i] */
	    temp = scrat[i];
	    j = i;
	    do {
		scrat[j] = scrat[j - 1];
		--j;
	    } while (scrat[j - 1] > temp); /* now:  scrat[j-1] <= temp */
	    scrat[j] = temp;
	}
    }

    band2 = bw / 2;
    rmed = scrat[band2];/* == Median( y[(1:band2)-1] ) */
    /* "malloc" had  free( (char*) scrat);*/ /*-- release scratch memory --*/

    if(*end_rule == 0) { /*-- keep DATA at end values */
	for (i = 0; i < band2; ++i)
	    smo[i] = y[i];
    }
    else { /* if(*end_rule == 1)  copy median to CONSTANT end values */
	for (i = 0; i < band2; ++i)
	    smo[i] = rmed;
    }
    smo[band2] = rmed;
    band2++; /* = bw / 2 + 1*/;

    if(*debug) REprintf("(bw,b2)= (%d,%d)\n", bw, band2);

/* Big	FOR Loop: RUNNING median, update the median 'rmed'
   ======================================================= */
    for (first = 1, last = bw, ismo = band2;
	 last < *n;
	 ++first, ++last, ++ismo) {

	yin = y[last];
	yout = y[first - 1];

	if(*debug) REprintf(" is=%d, y(in/out)= %10g, %10g", ismo ,yin, yout);

	rnew = rmed; /* New median = old one   in all the simple cases --*/

	if (yin < rmed) {
	    if (yout >= rmed) {
		kminus = 0;
		if (yout > rmed) {/*	--- yin < rmed < yout --- */
		    if(*debug) REprintf(": yin < rmed < yout ");
		    rnew = yin;/* was -rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi < rmed) {
			    ++kminus;
			    if (yi > rnew)	rnew = yi;
			}
		    }
		    if (kminus < band2)		rnew = rmed;
		}
		else {/*		--- yin < rmed = yout --- */
		    if(*debug) REprintf(": yin < rmed == yout ");
		    rse = rts = yin;/* was -rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi <= rmed) {
			    if (yi < rmed) {
				++kminus;
				if (yi > rts)	rts = yi;
				if (yi > rse)	rse = yi;
			    } else		rse = yi;

			}
		    }
		    rnew = (kminus == band2) ? rts : rse ;
		    if(*debug) REprintf("k- : %d,", kminus);
		}
	    } /* else: both  yin, yout < rmed -- nothing to do .... */
	}
	else if (yin != rmed) { /* yin > rmed */
	    if (yout <= rmed) {
		kplus = 0;
		if (yout < rmed) {/* -- yout < rmed < yin --- */
		    if(*debug) REprintf(": yout < rmed < yin ");
		    rnew = yin; /* was rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi > rmed) {
			    ++kplus;
			    if (yi < rnew)	rnew = yi;
			}
		    }
		    if (kplus < band2)	rnew = rmed;

		} else { /* -- yout = rmed < yin --- */
		    if(*debug) REprintf(": yout == rmed < yin ");
		    rbe = rtb = yin; /* was rinf */
		    for (i = first; i <= last; ++i) {
			yi = y[i];
			if (yi >= rmed) {
			    if (yi > rmed) {
				++kplus;
				if (yi < rtb)	rtb = yi;
				if (yi < rbe)	rbe = yi;
			    } else		rbe = yi;
			}
		    }
		    rnew = (kplus == band2) ? rtb : rbe;
		    if(*debug) REprintf("k+ : %d,", kplus);
		}
	    } /* else: both  yin, yout > rmed --> nothing to do */
	} /* else: yin == rmed -- nothing to do .... */
	if(*debug) REprintf("=> %12g, %12g\n", rmed, rnew);
	rmed = rnew;
	smo[ismo] = rmed;
    } /*     END FOR ------------ big Loop -------------------- */

    if(*end_rule == 0) { /*-- keep DATA at end values */
	for (i = ismo; i < *n; ++i)
	    smo[i] = y[i];
    }
    else { /* if(*end_rule == 1)  copy median to CONSTANT end values */
	for (i = ismo; i < *n; ++i)
	    smo[i] = rmed;
    }
} /* Srunmed */
