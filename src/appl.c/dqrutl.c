/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1996, 1997  Robert Gentleman and Ross Ihaka
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

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1000 = 1000;
static int c__10000 = 10000;
static int c__100 = 100;
static int c__10 = 10;
static int c__1 = 1;

int 
F77_SYMBOL(dqrqty) (double *x, int *n, int *k,
		    double *qraux, double *y, int *ny, double *qty)
{
	int x_dim1, x_offset, y_dim1, y_offset, qty_dim1, qty_offset, i__1;
	int info, j;
/*
	extern int F77_SYMBOL(dqrsl) ();
*/
	double dummy;

	--qraux;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	qty_dim1 = *n;
	qty_offset = qty_dim1 + 1;
	qty -= qty_offset;
	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;

	i__1 = *ny;
	for (j = 1; j <= i__1; ++j) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[j * y_dim1 + 1], &dummy, &
		  qty[j * qty_dim1 + 1], &dummy, &dummy, &dummy, &c__1000, &
				   info);
	}
	return 0;
}


int 
F77_SYMBOL(dqrqy) (double *x, int *n, int *k, double
		   *qraux, double *y, int *ny, double *qy)
{
	int x_dim1, x_offset, y_dim1, y_offset, qy_dim1, qy_offset, i__1;
	int info, j;
	extern int F77_SYMBOL(dqrsl) ();
	double dummy;

	--qraux;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	qy_dim1 = *n;
	qy_offset = qy_dim1 + 1;
	qy -= qy_offset;
	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;

	i__1 = *ny;
	for (j = 1; j <= i__1; ++j) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[j * y_dim1 + 1], &qy[j *
		  qy_dim1 + 1], &dummy, &dummy, &dummy, &dummy, &c__10000, &
				   info);
	}
	return 0;
}


int 
F77_SYMBOL(dqrcf) (double *x, int *n, int *k, double
		   *qraux, double *y, int *ny, double *b, int *info)
{
	int x_dim1, x_offset, y_dim1, y_offset, b_dim1, b_offset, i__1;
	int j;
	extern int F77_SYMBOL(dqrsl) ();
	double dummy;

	--qraux;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	b_dim1 = *k;
	b_offset = b_dim1 + 1;
	b -= b_offset;
	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;

	i__1 = *ny;
	for (j = 1; j <= i__1; ++j) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[j * y_dim1 + 1], &dummy, &
		    y[j * y_dim1 + 1], &b[j * b_dim1 + 1], &dummy, &dummy, &
				   c__100, info);
	}
	return 0;
}


int 
F77_SYMBOL(dqrrsd) (double *x, int *n, int *k,
		    double *qraux, double *y, int *ny, double *rsd)
{
	int x_dim1, x_offset, y_dim1, y_offset, rsd_dim1, rsd_offset, i__1;
	int info, j;
	extern int F77_SYMBOL(dqrsl) ();
	double dummy;

	--qraux;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	rsd_dim1 = *n;
	rsd_offset = rsd_dim1 + 1;
	rsd -= rsd_offset;
	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;

	i__1 = *ny;
	for (j = 1; j <= i__1; ++j) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[j * y_dim1 + 1], &dummy, &
		y[j * y_dim1 + 1], &dummy, &rsd[j * rsd_dim1 + 1], &dummy, &
				   c__10, &info);
	}
	return 0;
}


int 
F77_SYMBOL(dqrxb) (double *x, int *n, int *k, double
		   *qraux, double *y, int *ny, double *xb)
{
	int x_dim1, x_offset, y_dim1, y_offset, xb_dim1, xb_offset, i__1;
	int info, j;
	extern int F77_SYMBOL(dqrsl) ();
	double dummy;

	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;
	--qraux;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	xb_dim1 = *n;
	xb_offset = xb_dim1 + 1;
	xb -= xb_offset;

	i__1 = *ny;
	for (j = 1; j <= i__1; ++j) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[j * y_dim1 + 1], &dummy, &
		  y[j * y_dim1 + 1], &dummy, &dummy, &xb[j * xb_dim1 + 1], &
				   c__1, &info);
	}
	return 0;
}
