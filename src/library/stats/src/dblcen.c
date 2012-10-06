/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2012   The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

#include "Rinternals.h"

/* NB: this does not duplicate A */
SEXP DoubleCentre(SEXP A)
{
    int i, j, n = nrows(A);
    double *a = REAL(A), sum;

    for(i = 0 ; i < n ; i++) {
	sum = 0;
	for(j = 0 ; j < n ; j++) sum += a[i+j*n];
	sum /= n;
	for(j = 0 ; j < n ; j++) a[i+j*n] -= sum;
    }
    for(j = 0 ; j < n ; j++) {
	sum = 0;
	for(i = 0 ; i < n ; i++) sum += a[i+j*n];
	sum /= n;
	for(i = 0 ; i < n ; i++) a[i+j*n] -= sum;
    }
    return A;
}
