/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C)       1999  Robert Gentleman, Ross Ihaka and the
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

/* NOTE: This is no longer the code used by rowsum(), which can be
   found as Rrowsum_matrix and Rrowsum_df in src/main/unique.c.  This
   old code is preserved in case it is needed by C/Fortran programs */

/* However, it is still used by Hmisc as an entry point in PACKAGE=base */ 

/*
**  SCCS @(#)rowsum.c	4.2 06/30/93
**
** Add up data along rows
**
** Input
**	dim:   integer vector, the #rows and #columns of the matrix
**	na_x:  the value that marks NA's in the X matrix
**	x  :   matrix of data (remember, S uses column major order!)
**	group: the group to which each row belongs
**
** Output:
**	dd[0]: the number of unique groups found
**	x    : rows 1 to dd[0] contain the sums.
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <R_ext/Applic.h>

void R_rowsum(int *dim, double *na_x, double *x, double *group)
{
    register int i,j, k;
    int	    nrow, ncol, newrow, isna;
    double  tgrp, sum,	dummy, na;

    nrow = dim[0];
    ncol = dim[1];
    na	 = *na_x;

    dummy =0;
    for (i=0; i<nrow; i++) if (group[i] < dummy) dummy = group[i];
    dummy = (dummy/2) -1;    /*no group uses this number */

    newrow =0;
    for (i=0; i<nrow; i++) {
	if (group[i] > dummy) {
	    tgrp = group[i];
	    for (j=0; j<ncol; j++) {
		sum =0;
		isna=0;
		for (k=i; k<nrow; k++)
		    if (group[k] == tgrp) {
			if (x[k + j*nrow] == na)  isna=1;
			else			  sum += x[k + j*nrow];
			}
		if (isna==1) x[newrow + j*nrow] = na;
		else	     x[newrow + j*nrow] = sum;
		}
	    for (k=i; k<nrow; k++)
		if (group[k] == tgrp) group[k] = dummy;
	    newrow++;
	    }
	}
    dim[0] = newrow;
}
