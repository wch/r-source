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
#include <stdlib.h>

static double deg2rad = 0.01745329251994329576;


	/* Based on HSV_TO_RGB from Foley and Van Dam First Ed. Page 616 */
	/* See Alvy Ray Smith, Color Gamut Transform Pairs, SIGGRAPH '78 */

void hsv2rgb(double h, double s, double v, double *r, double *g, double *b)
{
	double	f, p, q, t;
	double	modf();
	int i;

	h = 6 * modf(h, &f);
	i = floor(h);
	f = h - i;
	p = v * (1 - s);
	q = v * (1 - s * f);
	t = v * (1 - (s * (1 - f)));
	switch (i) {
	case 0: 
		*r = v; 
		*g = t; 
		*b = p; 
		break;
	case 1: 
		*r = q; 
		*g = v; 
		*b = p; 
		break;
	case 2: 
		*r = p; 
		*g = v; 
		*b = t; 
		break;
	case 3: 
		*r = p; 
		*g = q; 
		*b = v; 
		break;
	case 4: 
		*r = t; 
		*g = p; 
		*b = v; 
		break;
	case 5: 
		*r = v; 
		*g = p; 
		*b = q; 
		break;
	default: 
		error("bad hsv to rgb color conversion\n");
	}
}
