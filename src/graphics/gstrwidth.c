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

double GStrWidth(char *str, int units)
{
	double w = DevStrWidth(str) / GP->fig2dev.bx;
	switch(units) {
		case 1: /* user == world */
			w = (DevStrWidth(str) / GP->fig2dev.bx) / GP->win2fig.bx;
			break;
		case 2:	/* figure */
			w = DevStrWidth(str) / GP->fig2dev.bx;
			break;
		case 3: /* inches */
			w = DevStrWidth(str) * GP->ipr[0];
			break;
		case 4: /* rasters */
			w = DevStrWidth(str);
			break;
	}
	return w;
}
