/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2003	The R Foundation
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

#include <R.h>			/* for NA_REAL, includes math.h */
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

static double dokern(double x, int kern)
{
    if(kern == 1) return(1.0);
    if(kern == 2) return(exp(-0.5*x*x));
    return(0.0); /* -Wall */
}

void BDRksmooth(double *x, double *y, int *n,
		double *xp, double *yp, int *np,
		int *kern, double *bandwidth)
{
    int i, imin=0, j;
    double cutoff=0.0, num, den, x0, w, bw=*bandwidth;

    /* bandwidth is in units of half inter-quartile range. */
    if(*kern == 1) {bw *= 0.5; cutoff = bw;}
    if(*kern == 2) {bw *= 0.3706506; cutoff = 4*bw;}
    while(x[imin] < xp[0] - cutoff && imin < *n) imin++;
    for(j = 0; j < *np; j++) {
	num = den = 0.0;
	x0 = xp[j];
	for(i = imin; i < *n; i++) {
	    if(x[i] < x0 - cutoff) imin = i;
	    else {
		if(x[i] > x0 + cutoff) break;
		w = dokern(fabs(x[i] - x0)/bw, *kern);
		num += w*y[i];
		den += w;
	    }
	}
	if(den > 0) yp[j] = num/den; else yp[j] = NA_REAL;
    }
}


void F77_SUB(bdrsplerr)(void)
{
    error(_("only 2500 rows are allowed for sm.method=\"spline\""));
}
