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
 *     dqrfit is a subroutine to compute least squares solutions
 *     to the system
 *
 *     (1)               x * b = y
 *
 *     which may be either under-determined or over-determined.
 *     the user must supply a tolerance to limit the columns of
 *     x used in computing the solution.  in effect, a set of
 *     columns with a condition number approximately bounded by
 *     1/tol is used, the other components of b being set to zero.
 *
 *     on entry
 *
 *        x      double precision(n,p).
 *               x contains n-by-p coefficient matrix of
 *               the system (1), x is destroyed by dqrfit.
 *
 *        n      the number of rows of the matrix x.
 *
 *        p      the number of columns of the matrix x.
 *
 *        y      double precision(n,ny)
 *               y contains the right hand side(s) of the system (1).
 *
 *        ny     the number of right hand sides of the system (1).
 *
 *        tol    double precision
 *               tol is the nonnegative tolerance used to
 *               determine the subset of columns of x included
 *               in the solution.  columns are pivoted out of
 *               decomposition if
 *
 *        jpvt   int(p)
 *               the values in jpvt are permuted in the same
 *               way as the columns of x.  this can be useful
 *               in unscrambling coefficients etc.
 *
 *        work   double precision(2*p)
 *               work is an array used by dqrdc2 and dqrsl.
 *
 *     on return
 *
 *        x      contains the output array from dqrdc2.
 *               namely the qr decomposition of x stored in
 *               compact form.
 *
 *        b      double precision(p,ny)
 *               b contains the solution vectors with rows permuted
 *               in the same way as the columns of x.  components
 *               corresponding to columns not used are set to zero.
 *
 *        rsd    double precision(n,ny)
 *               rsd contains the residual vectors y-x*b.
 *
 *        qty    double precision(n,ny)     t
 *               qty contains the vectors  q y.   note that
 *               the initial p elements of this vector are
 *               permuted in the same way as the columns of x.
 *
 *        k      int
 *               k contains the number of columns used in the
 *               solution.
 *
 *        jpvt   has its contents permuted as described above.
 *
 *        qraux  double precision(p)
 *               qraux contains auxiliary information on the
 *               qr decomposition of x.
 *
 *
 *     on return the arrays x, jpvt and qraux contain the
 *     usual output from dqrdc, so that the qr decomposition
 *     of x with pivoting is fully available to the user.
 *     in particular, columns jpvt(1), jpvt(2),...,jpvt(k)
 *     were used in the solution, and the condition number
 *     associated with those columns is estimated by
 *     abs(x(1,1)/x(k,k)).
 *
 *     dqrfit uses the linpack routines dqrdc and dqrsl.
*/

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1110 = 1110;

int 
F77_SYMBOL(dqrls) (double *x, int *n, int *p, double
		   *y, int *ny, double *tol, double *b, double *rsd,
		   double *qty, int *k, int *jpvt, double *qraux,
		   double *work)
{
	int x_dim1, x_offset, y_dim1, y_offset, b_dim1, b_offset, rsd_dim1, rsd_offset,
	 qty_dim1, qty_offset, i__1, i__2;
	int info, j;
	extern int F77_SYMBOL(dqrdc2)();
	int jj, kk;

	--work;
	--qraux;
	--jpvt;
	x_dim1 = *n;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	qty_dim1 = *n;
	qty_offset = qty_dim1 + 1;
	qty -= qty_offset;
	rsd_dim1 = *n;
	rsd_offset = rsd_dim1 + 1;
	rsd -= rsd_offset;
	b_dim1 = *p;
	b_offset = b_dim1 + 1;
	b -= b_offset;
	y_dim1 = *n;
	y_offset = y_dim1 + 1;
	y -= y_offset;

	/* reduce x. */

	F77_SYMBOL(dqrdc2) (&x[x_offset], n, n, p, tol, k, &qraux[1], &jpvt[1], &work[1]);

	/* solve the truncated least squares problem for each rhs. */

	if (*k == 0) {
		goto L30;
	}
	i__1 = *ny;
	for (jj = 1; jj <= i__1; ++jj) {
		F77_SYMBOL(dqrsl) (&x[x_offset], n, n, k, &qraux[1], &y[jj * y_dim1 + 1], &rsd[jj
											       * rsd_dim1 + 1], &qty[jj * qty_dim1 + 1], &b[jj * b_dim1 + 1],
		&rsd[jj * rsd_dim1 + 1], &rsd[jj * rsd_dim1 + 1], &c__1110, &
				   info);
	}
L30:

	/* set the unused components of b to zero. */

	kk = *k + 1;
	i__1 = *p;
	for (j = kk; j <= i__1; ++j) {
		i__2 = *ny;
		for (jj = 1; jj <= i__2; ++jj) {
			b[j + jj * b_dim1] = 0.;
		}
	}
	return 0;
}
