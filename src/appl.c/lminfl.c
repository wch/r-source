/* -----------------------------------------------------------------------
 *
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
 *
 * -----------------------------------------------------------------------
 *
 *     lminfl computes basic quantities useful for computing
 *     regression diagnostics.
 *
 *     on entry
 *
 *         x         double precision(ldx,k)
 *                   the qr decomposition as computed by dqrdc or dqrdc2.
 *
 *         ldx       int
 *                   the leading dimension of the array x.
 *
 *         n         int
 *                   the number of rows of the matrix x.
 *
 *         k         int
 *                   the number of columns in the matrix k.
 *
 *         qraux     double precision(k)
 *                   auxiliary information about the qr decomposition.
 *
 *         b         double precision(k)
 *                   the least-squares parameter estimates.
 *
 *         resid     double precision(k)
 *                   the residuals from the regression.
 *
 *     on return
 *
 *         hat       double precision(n)
 *                   the diagonal of the hat matrix.
 *
 *         coef      double precision(n,p)
 *                   a matrix which has as i-th row contains the estimated
 *                   regression coefficients when the i-th case is omitted
 *                   from the regression.
 *
 *         sigma     double precision(n)
 *                   the i-th element of sigma contains an estimate
 *                   of the residual standard deviation for the model with
 *                   the i-th case omitted.
 *
 *     This version dated Aug 24, 1996.
 *     Ross Ihaka, University of Auckland.
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__10000 = 10000;
static int c__1000 = 1000;
static int c__1 = 1;

int 
lminfl_(double *x, int *ldx, int *n, int
	*k, double *qraux, double *b, double *resid, double *
	hat, double *coef, double *sigma)
{
	double denom;
	double dummy, sum;
	double sqrt();
/*
	extern int F77_SYMBOL(dqrsl) (), F77_SYMBOL(dtrsl) ();
*/
	int info, i, j;
	int x_dim1, x_offset, coef_dim1, coef_offset, i__1, i__2;

	--sigma;
	--hat;
	--resid;
	coef_dim1 = *n;
	coef_offset = coef_dim1 + 1;
	coef -= coef_offset;
	--b;
	--qraux;
	x_dim1 = *ldx;
	x_offset = x_dim1 + 1;
	x -= x_offset;

	/* hat matrix diagonal */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		hat[i] = 0.;
	}
	i__1 = *k;
	for (j = 1; j <= i__1; ++j) {
		i__2 = *n;
		for (i = 1; i <= i__2; ++i) {
			sigma[i] = 0.;
		}
		sigma[j] = 1.;
		F77_SYMBOL(dqrsl) (&x[x_offset], ldx, n, k, &qraux[1], &sigma[1], &sigma[1], &
			   dummy, &dummy, &dummy, &dummy, &c__10000, &info);
		i__2 = *n;
		for (i = 1; i <= i__2; ++i) {
			hat[i] += sigma[i] * sigma[i];
		}
	}

	/* changes in the estimated coefficients */

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
			sigma[j] = 0.;
		}
		sigma[i] = resid[i] / (1. - hat[i]);
		F77_SYMBOL(dqrsl) (&x[x_offset], ldx, n, k, &qraux[1], &sigma[1], &dummy, &sigma[
			       1], &dummy, &dummy, &dummy, &c__1000, &info);
		F77_SYMBOL(dtrsl) (&x[x_offset], ldx, k, &sigma[1], &c__1, &info);
		i__2 = *k;
		for (j = 1; j <= i__2; ++j) {
			coef[i + j * coef_dim1] = sigma[j];
		}
	}

	/* estimated residual standard deviation */

	denom = (double) (*n - *k - 1);
	sum = 0.;
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sum += resid[i] * resid[i];
	}
	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		sigma[i] = sqrt((sum - resid[i] * resid[i] / (1. - hat[i])) / denom);
	}
	return 0;
}
