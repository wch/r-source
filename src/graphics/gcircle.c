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

	/* radius is given in points */

void GCircle(double x, double y, double radius, int col, int border)
{
	double ix, iy, ir;
	ix = XFMAP(x);
	iy = YFMAP(y);
#ifdef POINTS
	ir = radius/(72.0 * GP->ipr[0]);
#endif
	ir = radius/GP->ipr[0];
	DevCircle(ix, iy, ir, col, border);
}
