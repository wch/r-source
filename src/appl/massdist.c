/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1996	Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Mathlib.h"

void massdist(double *x, int *nx, double *xlow, double *xhigh, 
	      double *y, int *ny)
{
    double fx, xdelta, xmass, xpos;
    int i, ix, ixmax, ixmin;

    ixmin = 0;
    ixmax = *ny - 2;
    xmass = 1.0 / *nx;
    xdelta = (*xhigh - *xlow) / (*ny - 1.0);

    for(i=0 ; i<*ny ; i++)
	y[i] = 0;

    for(i=0 ; i<*nx ; i++) {
	xpos = (x[i] - *xlow) / xdelta;
	ix = floor(xpos);
	fx = xpos - ix;
	if(ixmin <= ix && ix <= ixmax) {
	    y[ix] += (1.0 - fx);
	    y[ix + 1] += fx;
	}
	else if(ix == -1) {
	    y[0] += fx;
	}
	else if(ix == ixmax + 1) {
	    y[ixmax + 1] += (1.0 - fx);
	}
    }

    for(i=0 ; i<*ny ; i++)
	y[i] *= xmass;
}
