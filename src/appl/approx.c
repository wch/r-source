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
#include "Errormsg.h"
#include <stdio.h>

/* Linear and Step Function Interpolation */
/* Assumes that ordinates are in ascending order */
/* The right interval is found by bisection */
/* Linear/constant interpolation then takes place on that interval*/

extern double R_NaReal;
static double ylow;
static double yhigh;
double f1;
double f2;

static double approx1(double v, double *x, double *y, int n, int method)
{
	int i, j, ij;
	
	i = 0;
	j = n - 1;
	
		/* handle out-of-domain points */

	if(v < x[i]) return ylow;
	if(v > x[j]) return yhigh;
	
		/* find the correct interval by bisection */

	while(i < j - 1) {
		ij = (i + j)/2;
		if(v < x[ij]) j = ij;
		else i = ij;
	}
	
		/* interpolation */

	if(v == x[i]) return y[i];
	if(v == x[j]) return y[j];

	if(method == 1) {
		return (x[i] == x[j]) ?
			y[i] :
			y[i] + (y[j] - y[i]) * ((v - x[i])/(x[j] - x[i]));
	}
	else {
		return (x[i] == x[j]) ?
			y[i] :
			y[i] * f1 + y[j] * f2;
	}
}

	/* R Frontend for Linear and Constant Interpolation */

int approx(double *x, double *y, int *nxy, double *xout, int *nout, int *method, double *yleft, double *yright, double *f)
{
	int i;

		/* check interpolation method */

	switch(*method) {
	    case 1:
		break;
	    case 2:
		if(!FINITE(*f) || *f < 0 || *f > 1)
			error("invalid f value in approx\n");
		f2 = *f;
		f1 = 1 - *f;
		break;
	    default:
		error("invalid interpolation method in approx\n");
		break;
	}

	for(i=0 ; i<*nxy ; i++)
		if(x[i] == R_NaReal || y[i] == R_NaReal)
			error("attempt interpolate NA values in approx\n");

	ylow = *yleft;
	yhigh = *yright;

	for(i=0 ; i<*nout ; i++) {
		if(xout[i] !=  R_NaReal)
			xout[i] = approx1(xout[i], x, y, *nxy, *method);
	}
	return 0;
}
