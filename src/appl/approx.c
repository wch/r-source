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

#include "Mathlib.h"
#include <stdio.h>

/* Linear Interpolation */
/* Assumes that ordinates are in ascending order */
/* The right interval is found by bisection */
/* Linear interpolation then takes place on that interval*/

static double approx1(double v, double *x, double *y, int n, double ylow, double yhigh)
{
	int i, j, ij;
	
	i = 0;
	j = n-1;
	
	/* Handle out-of-domain points */
	if(v < x[i]) return ylow;
	if(v > x[j]) return yhigh;
	
	/* Find the right interval by bisection */
	while(i < j-1) {
		ij = (i+j)/2;
		if(v < x[ij])
			j = ij;
		else
			i = ij;
	}
	
	/* Linear interpolation */
	return (x[i] == x[j]) ? y[i] : y[i]+(y[j]-y[i])*((v-x[i])/(x[j]-x[i]));
}

/* R Frontend for Linear Interpolation */
int approx(double *x, double *y, int *nxy, double *xout, int *nout, double *low, double *high)
{
	int i;

	for(i=0 ; i<*nout ; i++)
		xout[i] = approx1(xout[i], x, y, *nxy, *low, *high);
	return 0;
}
