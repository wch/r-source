/* -----------------------------------------------------------------------
 *
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (c) 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  The Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to The Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * -----------------------------------------------------------------------
 *
 *     ch2inv computes the inverse of a positive-definite symmetric
 *     matrix from its choleski factorization.  this can be used (for
 *     example) to compute the dispersion matrix for the estimated
 *     parameters in a regression analysis.
 *
 *     on entry
 *
 *         x         double precision(ldx,k)
 *                   the choleski decomposition or the
 *                   qr decomposition as computed by dqrdc
 *                   or dqrdc2
 *
 *         ldx       int
 *                   the leading dimension of the array x
 *
 *         n         int
 *                   the number of rows of the matrix x
 *
 *         k         int
 *                   the number of columns in the matrix k
 *
 *     on return
 *
 *         v         double precision(k,k)
 *                   the value of inverse(x'x)
 *
 *     This version dated Aug 24, 1996.
 *     Ross Ihaka, University of Auckland.
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;

int F77_SYMBOL(ch2inv)(double *x, int *ldx, int *n, double *v, int *info)
{
	int x_dim1, x_offset, v_dim1, v_offset, i__1, i__2;
	double d;
	int i, j;
	/* extern int F77_SYMBOL(dpodi) (); */
	int im1;

	v_dim1 = *n;
	v_offset = v_dim1 + 1;
	v -= v_offset;
	x_dim1 = *ldx;
	x_offset = x_dim1 + 1;
	x -= x_offset;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		if (x[i + i * x_dim1] == 0.) {
			*info = i;
			return 0;
		}
		i__2 = *n;
		for (j = i; j <= i__2; ++j) {
			v[i + j * v_dim1] = x[i + j * x_dim1];
		}
	}
	F77_SYMBOL(dpodi) (&v[v_offset], n, n, &d, &c__1);
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		im1 = i - 1;
		i__2 = im1;
		for (j = 1; j <= i__2; ++j) {
			v[i + j * v_dim1] = v[j + i * v_dim1];
		}
	}
	return 0;
}
