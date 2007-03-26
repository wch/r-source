/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1999   Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Applic.h>

void R_cumsum(double *x, int *n, double *na_value, double *ans)
{
    double sum;
    int i;

    for(i=0 ; i<*n ; i++)
	ans[i] = *na_value;

    sum = 0.0;
    for( i=0 ; i<*n ; i++) {
	if(x[i] == *na_value)
	    break;
	sum += x[i];
	ans[i] = sum;
    }
}
