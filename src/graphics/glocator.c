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

int GLocator(double *x, double *y, int usercoords)
{
	double ix, iy;
	if(!DevLocator)
		error("no locator capability in device driver\n");
	if(DevLocator(&ix, &iy)) {
		*x = (ix - GP->fig2dev.ax) / GP->fig2dev.bx;
		*y = (iy - GP->fig2dev.ay) / GP->fig2dev.by;
		if(usercoords) {
			*x = (*x - GP->win2fig.ax) / GP->win2fig.bx;
			*y = (*y - GP->win2fig.ay) / GP->win2fig.by;
		}
		return 1;
	}
	else
		return 0;
}
