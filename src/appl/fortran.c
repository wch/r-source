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
 *
 *  Support for Fortran Intrinsics
 *  Loosely based on f2c Libraries
 */

#include "Fortran.h"
#include "Errormsg.h"


double DABS(double *a)
{
	return (*a >= 0.0) ? *a : -*a;
}

double DSIGN(double *a, double *b)
{
	double x;
	x = (*a >= 0 ? *a : - *a);
	return (*b >= 0 ? x : -x);
}

double POW_DI(double *ap, int *bp)
{
	double pow, x;
	int n;

	pow = 1;
	x = *ap;
	n = *bp;

	if(n != 0) {
		if(n < 0) {
			n = -n;
			x = 1/x;
		}
		for(;;) {
			if(n & 01)
				pow *= x;
			if(n >>= 1)
				x *= x;
			else
				break;
		}
	}
	return pow;
}

double POW_DD(double *ap, double *bp)
{
	return pow(*ap, *bp);
}


#define log10e 0.43429448190325182765


double DLOG10(double *x)
{
	return log10e * log(*x);
}


	/* Complex Arithmetic */

void ZDIV(complex *c, complex *a, complex *b)
{
	double ratio, den;
	double abr, abi;

	if( (abr = b->r) < 0.)
		abr = - abr;
	if( (abi = b->i) < 0.)
		abi = - abi;
	if( abr <= abi ) {
		if(abi == 0)
			error("complex division by zero\n");
		ratio = b->r / b->i ;
		den = b->i * (1 + ratio*ratio);
		c->r = (a->r*ratio + a->i) / den;
		c->i = (a->i*ratio - a->r) / den;
	}
	else {
		ratio = b->i / b->r ;
		den = b->r * (1 + ratio*ratio);
		c->r = (a->r + a->i*ratio) / den;
		c->i = (a->i - a->r*ratio) / den;
        }
}

double ZABS(complex *z)
{
	return hypot(z->r, z->i);
}

double ZIMAG(complex *z)
{
	return z->i;
}

double ZREAL(complex *z)
{
	return z->r;
}

void ZCNJG(complex *r, complex *z)
{
	r->r = z->r;
	r->i = - z->i;
}
