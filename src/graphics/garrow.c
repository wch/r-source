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

#define	DEG2RAD	0.01745329251994329576

#include "Graphics.h"

void GArrow(double xfrom, double yfrom, double xto, double yto, 
	double length, double angle, int code)
{
	double rot, x1, x2, xc,  y1, y2, yc;

	GStartPath();
	GMoveTo(xfrom, yfrom);
	GLineTo(xto, yto);
	GEndPath();

	if(code & 1) {
		xc = xFigtoInch(xto - xfrom);
		yc = yFigtoInch(yto - yfrom);
		rot= atan2(yc, xc);
		x1 = xfrom + xInchtoFig(length * cos(rot+angle*DEG2RAD));
		y1 = yfrom + xInchtoFig(length * sin(rot+angle*DEG2RAD));
		x2 = xfrom + xInchtoFig(length * cos(rot-angle*DEG2RAD));
		y2 = yfrom + xInchtoFig(length * sin(rot-angle*DEG2RAD));
		GStartPath();
		GMoveTo(x1, y1);
		GLineTo(xfrom, yfrom);
		GLineTo(x2, y2);
		GEndPath();
	}
	if(code & 2) {
		xc = xFigtoInch(xfrom - xto);
		yc = yFigtoInch(yfrom - yto);
		rot= atan2(yc, xc);
		x1 = xto + xInchtoFig(length * cos(rot+angle*DEG2RAD));
		y1 = yto + xInchtoFig(length * sin(rot+angle*DEG2RAD));
		x2 = xto + xInchtoFig(length * cos(rot-angle*DEG2RAD));
		y2 = yto + xInchtoFig(length * sin(rot-angle*DEG2RAD));
		GStartPath();
		GMoveTo(x1, y1);
		GLineTo(xto, yto);
		GLineTo(x2, y2);
		GEndPath();
	}
}
