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

void GMetricInfo(int c, double *ascent, double *descent, double *width, int units)
{
	double a, b;

	if(DevMetricInfo)
		DevMetricInfo(c, ascent, descent, width);
	else
		error("detailed character metric information unavailable\n");

	switch(units) {
		case 1: /* user == world */
			a = 1.0 / (GP->fig2dev.bx * GP->win2fig.bx);
			b = 1.0 / (GP->fig2dev.by * GP->win2fig.by);
			break;
		case 2:	/* figure */
			a = 1.0 / GP->fig2dev.bx;
			b = 1.0 / GP->fig2dev.by;
			break;
		case 3: /* inches */
			a = 1.0 * GP->ipr[0];
			b = 1.0 * GP->ipr[1];
			break;
		case 4: /* rasters */
			a = 1.0;
			b = 1.0;
			break;
	}
	*ascent = b * *ascent;
	*descent = b * *descent;
	*width = a * *width;
}
