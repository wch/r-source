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

static double deg2rad = 0.01745329251994329576;


void GText(double x, double y, char *str, double xc, double yc, double rot)
{
	double ix, iy, xtest, ytest;
	
	ix = XFMAP(x);
	iy = YFMAP(y);
	if(str && *str)
		if(DP->canClip) {
			GClip();
			DevText(ix, iy, str, xc, yc, rot);
		}
		else {
			if(!DP->xpd) {
				xtest = (ix-XFMAP(GP->plt[0]))*(XFMAP(GP->plt[1])-ix);
				ytest = (iy-YFMAP(GP->plt[2]))*(YFMAP(GP->plt[3])-iy);
				if(xtest < 0 || ytest < 0) return;
			}
			DevText(ix, iy, str, xc, yc, rot);
		}
}
