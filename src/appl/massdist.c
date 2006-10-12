/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1996-2004	Robert Gentleman and Ross Ihaka and the
 *				R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h>
#include <R_ext/Applic.h>

void massdist(double *x,
	      double *xmass, /* AB: new variable */
	      int *nx,
	      double *xlow, double *xhigh,
	      double *y, int *ny)
{
    double fx, xdelta, xmi, xpos;   /* AB */
    int i, ix, ixmax, ixmin;

    ixmin = 0;
    ixmax = *ny - 2;
    /* AB: line deleted */
    xdelta = (*xhigh - *xlow) / (*ny - 1);

    for(i=0; i < *ny ; i++)
	y[i] = 0;

    for(i=0; i < *nx ; i++) {
	if(R_FINITE(x[i])) {
	    xpos = (x[i] - *xlow) / xdelta;
	    ix = floor(xpos);
	    fx = xpos - ix;
	    xmi = xmass[i];   /* AB: new line  */
	    if(ixmin <= ix && ix <= ixmax) {
		y[ix] += (1 - fx) * xmi;   /* AB */
		y[ix + 1] += fx * xmi; /* AB */
	    }
	    else if(ix == -1) {
		y[0] += fx * xmi;  /* AB */
	    }
	    else if(ix == ixmax + 1) {
		y[ix] += (1 - fx) * xmi;  /* AB */
	    }
	}
    }

    /* AB: lines deleted */
}
