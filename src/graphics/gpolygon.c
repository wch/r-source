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

#include "Graphics.h"
#include "Errormsg.h"

	/* Draw a polygon. */
	/* Filled with color bg */
	/* Outlined with color fg */
	/* These may both be NA_INTEGER */

void GPolygon(int n, double *x, double *y, int bg, int fg, int usrcoords, double *work)
{
	int i;
	double *dx, *dy;
	double xx, yy;
	dx = &work[0];
	dy = &work[n];

	for(i=0 ; i<n ; i++) {
		xx = x[i];
		yy = y[i];
		if(!FINITE(xx) || !FINITE(yy))
			error("NA encountered in polygon\n");
		if(usrcoords) {
			xx = XMAP(xx);
			yy = YMAP(yy);
		}
		dx[i] = XFMAP(xx);
		dy[i] = YFMAP(yy);
	}

	/* If device can't clip we should use something */
	/* like Sutherland-Hodgman here */

	if(DP->canClip) GClip();
	DevPolygon(n, dx, dy, bg, fg);
}
