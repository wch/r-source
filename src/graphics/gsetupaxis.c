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

void GSetupAxis(int axis)
{
	double xmin, xmax;
	int n;

	if(axis == 1 || axis == 3) {
		n = GP->lab[0];
		xmin = GP->usr[0];
		xmax = GP->usr[1];
	}
	else {
		n = GP->lab[1];
		xmin = GP->usr[2];
		xmax = GP->usr[3];
	}

	GPretty(&xmin, &xmax, &n);

	if(axis == 1 || axis == 3) {
		GP->xaxp[0] = DP->xaxp[0] = xmin;
		GP->xaxp[1] = DP->xaxp[1] = xmax;
		GP->xaxp[2] = DP->xaxp[2] = n;
	}
	else {
		GP->yaxp[0] = DP->yaxp[0] = xmin;
		GP->yaxp[1] = DP->yaxp[1] = xmax;
		GP->yaxp[2] = DP->yaxp[2] = n;
	}
}
