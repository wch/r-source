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

	/* Note: 1 2 5 10 looks good on logarithmic scales */

void GLPretty(double*, double*, int*);
void GPretty(double*, double*, int*);
void GLScale(double, double, int);
void GScale(double, double, int);


void GLPretty(double *xmin, double *xmax, int *n)
{
	double u1, u2, v1, v2, p1, p2;
	
	if((*xmax) <= 7.8651*(*xmin))
		GPretty(xmin, xmax, n);
	else {
		p1 = ceil(*xmin-0.0001);
		p2 = floor(*xmax+0.0001);
		u1 = pow(10.0, p1);
		v1 = pow(10.0, p2);
		*n = p2 - p1;

		if(*n <= 1) {
			*n = 2*(*n);
			u2 = 0.5*u1;
			if(u2 <= *xmin) {
				u2 = u1;
			}
			else (*n)++;
	
			v2 = 5.0*v1;
			if(v2 >= *xmax) {
				v2 = v1;
			}
			else (*n)++;

			*xmin = u2;
			*xmax = v2;
		}
		else {
			*xmin = u1;
			*xmax = v1;
		}
	}
}

void GPretty(double *s, double *u, int *ndiv)
{
	double base, cell, unit, tmp;
	int ns, nu;

	if( *s == R_PosInf || *u == R_PosInf || *s == R_NegInf || *u == R_NegInf || *ndiv == 0 )
		error("infinite axis extents\n");

	cell = FLT_EPSILON + (*u-*s) / *ndiv;
	base = pow(10.0, floor(log10(cell)));
	unit = base;
	if(fabs((tmp = 2.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	if(fabs((tmp = 5.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	if(fabs((tmp = 10.0*base)-cell) < fabs(unit-cell)) unit = tmp;
	
	ns = floor(*s/unit);
	while(ns*unit > *s - FLT_EPSILON) ns--;
	ns++;
	*s = unit*ns;
	
	nu = ceil(*u/unit);
	while(nu*unit < *u + FLT_EPSILON) nu++;
	nu--;
	*u = unit*nu;
	
	*ndiv = (*u-*s)/unit+0.5;
}

void GScale(double xmin, double xmax, int axis)
{
	int log, n, style, swap;
	double temp;

	log = 0;

	if(axis == 1 || axis == 3) {
		n = GP->lab[0];
		style = GP->xaxs;
		if(GP->xlog) {
			log = 1;
			xmin = log10(xmin);
			xmax = log10(xmax);
		}
	}
	else {
		n = GP->lab[1];
		style = GP->yaxs;
		if(GP->ylog) {
			log = 1;
			xmin = log10(xmin);
			xmax = log10(xmax);
		}
	}

	if(xmin == xmax) {
		if(xmin == 0) {
			xmin = -1;
			xmax =  1;
		}
		else {
			xmin = 0.6 * xmin;
			xmax = 1.4 * xmax;
		}
	}

	switch(style) {
		case 'r':
			temp = 0.04 * (xmax-xmin);
			xmin = xmin - temp;
			xmax = xmax + temp;
		case 'i':
			break;
		case 's':
		case 'e':
		default:
			error("axis style \"%c\" unimplemented\n", style);
	}

	if(axis == 1 || axis == 3) {
		GP->usr[0] = DP->usr[0] = xmin;
		GP->usr[1] = DP->usr[1] = xmax;
	}
	else {
		GP->usr[2] = DP->usr[2] = xmin;
		GP->usr[3] = DP->usr[3] = xmax;
	}

	if(xmin > xmax) {
		swap = 1;
		temp = xmin;
		xmin = xmax;
		xmax = temp;
	}
	else swap = 0;

	if(log) {
		xmin = pow(10.0,xmin);
		xmax = pow(10.0,xmax);
	}
	GPretty(&xmin, &xmax, &n);

	if(swap) {
		temp = xmin;
		xmin = xmax;
		xmax = temp;
	}

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
