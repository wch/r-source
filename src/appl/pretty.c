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

/* Pretty Intervals */
/* Constructs "pretty" values which cover the given interval */

#include "Mathlib.h"

int pretty(double *s, double *u, int *ndiv)
{
	double cell, unit[4], dif[4];
	int	i, k, ns, nu;

	cell = FLT_EPSILON+(*u-*s) / *ndiv;
	unit[0] = pow(10.0,((int)(99.0+log10(cell))-99.0));
	unit[1] = 2*unit[0];
	unit[2] = 5*unit[0];
	unit[3] = 10*unit[0];
	for( i=0 ; i<4 ; i++ )
		dif[i] = fabs(cell-unit[i]);
	k = 0;
	if( dif[1]<dif[0] ) k=1;
	if( dif[2]<dif[k] ) k=2;
	if( dif[3]<dif[k] ) k=3;
	ns = ((int)(99+*s/unit[k])-99);
	while( ns*unit[k] > *s )
		ns--;
	*s = unit[k]*ns;
	nu = (99-(int)(99-*u/unit[k]));
	while( nu*unit[k] < *u )
		nu++;
	*u = unit[k]*nu;
	*ndiv = (*u-*s)/unit[k]+0.5;
	return 0;
}
