/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1999        The R Development Core Team
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

#include "S.h"

/* cor is the autocorrelations starting from 0 lag*/
void uni_pacf(double *cor, double *p, int *pn, int *pnlag)
{
    int n = *pn, nlag = *pnlag;
    int i, ll;
    double a, b, c, *v, *w;
    
    v = (double*) R_alloc(nlag, sizeof(double));
    w = (double*) R_alloc(nlag, sizeof(double));
    w[0] = p[0] = cor[1];
    for(ll = 1; ll < nlag; ll++) {
	a = cor[ll+1];
	b = 1.0;
	for(i = 0; i < ll; i++) {
	    a -= w[i] * cor[ll - i];
	    b -= w[i] * cor[i + 1];
	}
	p[ll] = c = a/b;
	if(ll+1 == nlag) break;
	w[ll] = c;
	for(i = 0; i < ll; i++)
	    v[ll-i-1] = w[i];
	for(i = 0; i < ll; i++)
	    w[i] -= c*v[i];
    }
}
