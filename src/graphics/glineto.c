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

static void CSclip(double x1, double y1, double x2, double y2);

void GLineTo(double x, double y)
{
	if(GP->lty != NA_INTEGER) {
		if(DP->canClip) {
			GClip();
			DevLineTo(XFMAP(x),YFMAP(y));
		}
		else {
			if(GP->xpd) DevLineTo(XFMAP(x),YFMAP(y));
			else CSclip(DP->xlast, DP->ylast, x, y);
		}
	}
	DP->xlast = x;
	DP->ylast = y;
}

void GStartPath()
{
	if(GP->lty != NA_INTEGER)
		DevStartPath();
}

void GEndPath()
{
	if(GP->lty != NA_INTEGER)
		DevEndPath();
}

/* Draw Line Segments, Clipping to the Viewport */
/* Cohen-Sutherland Algorithm */
/* Unneeded if the device can do the clipping */

#define	BOTTOM	001
#define	LEFT	002
#define	TOP	004
#define	RIGHT	010

/* Parameters for clipping to the plot region in figure coordinates */

#define Clipxl (GP->plt[0])
#define Clipxr (GP->plt[1])
#define Clipyb (GP->plt[2])
#define Clipyt (GP->plt[3])

static int clipcode(double x, double y)
{
	int c;

	c = 0;
	if(x < Clipxl)
		c |= LEFT;
	else if(x > Clipxr)
		c |= RIGHT;
	if(y < Clipyb)
		c |= BOTTOM;
	else if(y > Clipyt)
		c |= TOP;
	return c;
}


static void CSclip(double x1, double y1, double x2, double y2)
{
	int c, c1, c2;
	double x, y, xstart, ystart;

	xstart = x1;
	ystart = y1;
	
	c1 = clipcode(x1, y1);
	c2 = clipcode(x2, y2);
	while( c1 || c2 ) {
		if(c1 & c2) goto skip;
		if( c1 ) c = c1;
		else c = c2;
		if( c & LEFT ) {
			y = y1+(y2-y1)*(Clipxl-x1)/(x2-x1);
			x = Clipxl;
		}
		else if( c & RIGHT ) {
			y = y1+(y2-y1)*(Clipxr-x1)/(x2-x1);
			x = Clipxr;
		}
		else if( c & BOTTOM ) {
			x = x1+(x2-x1)*(Clipyb-y1)/(y2-y1);
			y = Clipyb;
		}
		else if( c & TOP ) {
			x = x1+(x2-x1)*(Clipyt-y1)/(y2-y1);
			y = Clipyt;
		}

		if( c==c1 ) {
			x1 = x;
			y1 = y;
			c1 = clipcode(x,y);
		}
		else {
			x2 = x;
			y2 = y;
			c2 = clipcode(x,y);
		}
	}
	if(x1 != xstart || y1 != ystart)
		DevMoveTo(XFMAP(x1),YFMAP(y1));
	DevLineTo(XFMAP(x2),YFMAP(y2));
skip:
	;
}
