/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1996  Robert Gentleman and Ross Ihaka
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

/* Includes code from the Venables and Ripley MASS library */
/* with modifications by Ross Ihaka */


#include "Mathlib.h"

int massdist(double *x, int *nx, double *xlow, double *xhigh, double *y, int *ny)
{
	double fx, xdelta, xmass, xpos;
	int i, ix, ixmax, ixmin;

	ixmin = 0;
	ixmax = *ny - 2;
	xmass = 1.0 / *nx;
	xdelta = (*xhigh - *xlow) / (*ny - 1.0);

	for(i=0 ; i<*ny ; i++)
		y[i] = 0;

	for(i=0 ; i<*nx ; i++) {
		xpos = (x[i] - *xlow) / xdelta;
		ix = floor(xpos);
		fx = xpos - ix;
		if(ixmin <= ix && ix <= ixmax) {
			y[ix] += (1.0 - fx);
			y[ix + 1] += fx;
		}
		else if(ix == -1) {
			y[0] += fx;
		}
		else if(ix == ixmax + 1) {
			y[ixmax + 1] += (1.0 - fx);
		}
	}

	for(i=0 ; i<*ny ; i++)
		y[i] *= xmass;
	return 0;
}

/* The following code is from the Venables and Ripley MASS library */
/* Modifications by Ross Ihaka */

#define DELMAX 100

/* Avoid slow and possibly error-producing undeflows by cutting off at
   plus/minus DELMAX^2 std deviations */
/* Formulae (6.67) and (6.69) of Scott (1992), the latter corrected. */

void ucv(int *n, double *x, double *h, double *u)
{
	double delta, hh, sum;
	int i, j, nn;
	nn = *n;
	hh = (*h) / 4;
	sum = 0.0;
	for (i = 0; i < nn - 1; i++)
		for (j = i + 1; j < nn; j++) {
			delta = (x[i] - x[j]) / hh;
			delta = delta * delta;
			if (delta < DELMAX)
				sum += exp(-delta / 4) - sqrt(8.0) * exp(-delta / 2);
	}
	*u = 1 / (2 * nn * hh * M_SQRT_PI) + sum / (nn * nn * hh * M_SQRT_PI);
}

void bcv(int *n, double *x, double *h, double *u)
{
	double delta, hh, sum;
	int i, j, nn;
	nn = *n;
	hh = (*h) / 4;
	sum = 0.0;
	for (i = 0; i < nn - 1; i++)
		for (j = i + 1; j < nn; j++) {
			delta = (x[i] - x[j]) / hh;
			delta = delta * delta;
			if (delta < DELMAX)
				sum += exp(-delta / 4) * (delta * delta - 12 * delta + 12);
		}
	*u = 1 / (2 * nn * hh * M_SQRT_PI) + sum / (64 * nn * nn * hh * M_SQRT_PI);
}

void phi4(int *n, double *x, double *h, double *u)
{
	double delta, hh, sum;
	int i, j, nn;
	nn = *n;
	hh = (*h);
	sum = 0.0;
	for (i = 0; i < nn - 1; i++)
		for (j = i + 1; j < nn; j++) {
			delta = (x[i] - x[j]) / hh;
			delta = delta * delta;
			if (delta < DELMAX)
				sum += exp(-delta / 2) * (delta * delta - 6 * delta + 3);
		}
	sum = 2 * sum + nn * 3;	/* add in diagonal */
	*u = sum * M_1_SQRT_2PI / (nn * (nn - 1) * pow(hh, 5.0));
}

void phi6(int *n, double *x, double *h, double *u)
{
	double delta, hh, sum;
	int i, j, nn;
	nn = *n;
	hh = (*h);
	sum = 0.0;
	for (i = 0; i < nn - 1; i++)
		for (j = i + 1; j < nn; j++) {
			delta = (x[i] - x[j]) / hh;
			delta = delta * delta;
			if (delta < DELMAX)
				sum += exp(-delta / 2) *
					(delta * delta * delta - 15 * delta * delta + 45 * delta - 15);
		}
	sum = 2 * sum - nn * 15;	/* add in diagonal */
	*u = sum * M_1_SQRT_2PI / (nn * (nn - 1) * pow(hh, 7.0));
}
