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
 *     chol performs the choleski decomposition of a symmetric
 *     positive-definite matrix.  this is just a wrapper for the
 *     linpack routine dpofa.
 *
 *     on entry
 *
 *         a         double precision(lda,n)
 *                   the upper triangle of the matrix to be factorized
 *                   is contained in the upper triangle of a.
 *
 *         lda       integer
 *                   the leading dimension of a.
 *
 *         n         integer
 *                   the number or rows and columns of the matrix
 *                   to be factorized.
 *
 *     on return
 *
 *         v         double precision(n,n)
 *                   the square-root (choleski) factor.
 *
 *         info      integer
 *                   the error indicator from dpofa.  this will be
 *                   zero unless the matrix being factorized is
 *                   not positive definite.
 *
 *     This version dated Aug 25, 1996.
 *     Ross Ihaka, University of Auckland.
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

int F77_SYMBOL(chol) (double *a, int *lda, int *n, double *v, int *info)
{
	int a_dim1, a_offset, v_dim1, v_offset, i__1, i__2;
	int i, j;
	/* extern int F77_SYMBOL(dpofa) (); */

	v_dim1 = *n;
	v_offset = v_dim1 + 1;
	v -= v_offset;
	a_dim1 = *lda;
	a_offset = a_dim1 + 1;
	a -= a_offset;

	i__1 = *n;
	for (i = 1; i <= i__1; ++i) {
		i__2 = *n;
		for (j = 1; j <= i__2; ++j) {
			if (i > j) {
				v[i + j * v_dim1] = 0.;
			}
			else {
				v[i + j * v_dim1] = a[i + j * a_dim1];
			}
		}
	}
	F77_SYMBOL(dpofa) (&v[v_offset], n, n, info);
	return 0;
}
