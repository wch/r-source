/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1999, 2001  The R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
 */

#include <R.h>
#include "ts.h"

void
burg(int *pn, double*x, int *pp, double *coefs, double *var1, double *var2)
{
    int i, j, n=*pn, p, pmax=*pp, t;
    double d, phii, *u, *v, *u0, sum;
    
    u = (double *) R_alloc(n, sizeof(double));
    v = (double *) R_alloc(n, sizeof(double));
    u0 = (double *) R_alloc(n, sizeof(double));

    for(i = 0; i < pmax*pmax; i++) 
	coefs[i] = 0.0;
    sum = 0.0;
    for(t = 0; t < n; t++) {
	u[t] = v[t] = x[n - 1 - t];
	sum += x[t] * x[t];
    }
    var1[0] = var2[0] = sum/n;
    for(p = 1; p <= pmax; p++) { /* do AR(p) */
	sum = 0.0;
	d = 0;
	for(t = p; t < n; t++) {
	    sum += v[t]*u[t-1];
	    d += v[t]*v[t] + u[t-1]*u[t-1];
	}
	phii = 2*sum/d;
	coefs[pmax*(p-1) + (p-1)] = phii;
	if(p > 1) 
	    for(j = 1; j < p; j++) 
		coefs[p-1 + pmax*(j-1)] = 
		    coefs[p-2 + pmax*(j-1)] - phii* coefs[p-2 + pmax*(p-j-1)];
	/* update u and v */
	for(t = 0; t < n; t++)
	    u0[t] = u[t];
	for(t = p; t < n; t++) {
	    u[t] = u0[t-1] - phii * v[t];
	    v[t] = v[t] - phii * u0[t-1];
	}
	var1[p] = var1[p-1] * (1 - phii * phii);
	d = 0.0;
	for(t = p; t < n; t++) {
	    d += v[t]*v[t] + u[t]*u[t];
	    }
	var2[p] = d/(2.0*(n-p));
    }
}
