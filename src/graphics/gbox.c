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

#define XINVFMAP(x) ((x - GP->fig2dev.ax)/GP->fig2dev.bx)
#define YINVFMAP(y) ((y - GP->fig2dev.ay)/GP->fig2dev.by)

void GBox(int which)
{
	double x0, x1, y0, y1;
	GClip();
	switch(which) {
	case 1:
		GStartPath();
		switch(GP->bty) {
		case 'o':
		case 'O':
			GMoveTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[2]);
			break;
		case 'l':
		case 'L':
			GMoveTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[1], GP->plt[2]);
			break;
		case '7':
			GMoveTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[2]);
			break;
		case 'c':
		case 'C':
			GMoveTo(GP->plt[1], GP->plt[2]);
			GLineTo(GP->plt[0], GP->plt[2]);
			GLineTo(GP->plt[0], GP->plt[3]);
			GLineTo(GP->plt[1], GP->plt[3]);
			break;
		default:
			break;
		}
		GEndPath();
		break;
	case 2:
		GStartPath();
		GMoveTo(0.0, 0.0);
		GLineTo(1.0, 0.0);
		GLineTo(1.0, 1.0);
		GLineTo(0.0, 1.0);
		GLineTo(0.0, 0.0);
		GEndPath();
		break;
	case 4:
		x0 = XINVFMAP(DP->ndc2dev.ax);
		y0 = YINVFMAP(DP->ndc2dev.ay);
		x1 = XINVFMAP(DP->ndc2dev.ax + DP->ndc2dev.bx);
		y1 = YINVFMAP(DP->ndc2dev.ay + DP->ndc2dev.by);
		GStartPath();
		GMoveTo(x0, y0);
		GLineTo(x1, y0);
		GLineTo(x1, y1);
		GLineTo(x0, y1);
		GLineTo(x0, y0);
		GEndPath();
		break;
	default:
		error("invalid GBox argument\n");
	}
}
