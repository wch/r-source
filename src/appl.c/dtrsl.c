/*
 *     dtrsl solves systems of the form
 *
 *                   t * x = b
 *     or
 *                   trans(t) * x = b
 *
 *     where t is a triangular matrix of order n. here trans(t)
 *     denotes the transpose of the matrix t.
 *
 *     on entry
 *
 *         t         double precision(ldt,n)
 *                   t contains the matrix of the system. the zero
 *                   elements of the matrix are not referenced, and
 *                   the corresponding elements of the array can be
 *                   used to store other information.
 *
 *         ldt       int
 *                   ldt is the leading dimension of the array t.
 *
 *         n         int
 *                   n is the order of the system.
 *
 *         b         double precision(n).
 *                   b contains the right hand side of the system.
 *
 *         job       int
 *                   job specifies what kind of system is to be solved.
 *                   if job is
 *
 *                        00   solve t*x=b, t lower triangular,
 *                        01   solve t*x=b, t upper triangular,
 *                        10   solve trans(t)*x=b, t lower triangular,
 *                        11   solve trans(t)*x=b, t upper triangular.
 *
 *     on return
 *
 *         b         b contains the solution, if info .eq. 0.
 *                   otherwise b is unaltered.
 *
 *         info      int
 *                   info contains zero if the system is nonsingular.
 *                   otherwise info contains the index of
 *                   the first zero diagonal element of t.
 *
 *     linpack. this version dated 08/14/78 .
 *     g. w. stewart, university of maryland, argonne national lab.
 *
 *     subroutines and functions
 *
 *     blas daxpy, ddot
 *     fortran mod
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;

int 
F77_SYMBOL(dtrsl) (double *t, int *ldt, int *n,
		   double *b, int *job, int *info)
{
	double temp;
/*
	extern double F77_SYMBOL(ddot) ();
	extern int F77_SYMBOL(daxpy) ();
*/
	int case_;
	int j;
	int jj;
	int t_dim1, t_offset, i__1, i__2;

	t_dim1 = *ldt;
	t_offset = t_dim1 + 1;
	t -= t_offset;
	--b;

	/* begin block permitting ...exits to 150 */
	/* for zero diagonal elements. */

	i__1 = *n;
	for (*info = 1; *info <= i__1; ++(*info)) {

		/* ......exit */

		if (t[*info + *info * t_dim1] == 0.) {
			goto L150;
		}
	}
	*info = 0;

	/* determine the task and go to it. */

	case_ = 1;
	if (*job % 10 != 0) {
		case_ = 2;
	}
	if (*job % 100 / 10 != 0) {
		case_ += 2;
	}
	switch (case_) {
	case 1:
		goto L20;
	case 2:
		goto L50;
	case 3:
		goto L80;
	case 4:
		goto L110;
	}

	/* solve t*x=b for t lower triangular */

L20:
	b[1] /= t[t_dim1 + 1];
	if (*n < 2) {
		goto L40;
	}
	i__1 = *n;
	for (j = 2; j <= i__1; ++j) {
		temp = -b[j - 1];
		i__2 = *n - j + 1;
		F77_SYMBOL(daxpy) (&i__2, &temp, &t[j + (j - 1) * t_dim1], &c__1, &b[j], &c__1);
		b[j] /= t[j + j * t_dim1];
	}
L40:
	goto L140;

	/* solve t*x=b for t upper triangular. */

L50:
	b[*n] /= t[*n + *n * t_dim1];
	if (*n < 2) {
		goto L70;
	}
	i__1 = *n;
	for (jj = 2; jj <= i__1; ++jj) {
		j = *n - jj + 1;
		temp = -b[j + 1];
		F77_SYMBOL(daxpy) (&j, &temp, &t[(j + 1) * t_dim1 + 1], &c__1, &b[1], &c__1);
		b[j] /= t[j + j * t_dim1];
	}
L70:
	goto L140;

	/* solve trans(t)*x=b for t lower triangular. */

L80:
	b[*n] /= t[*n + *n * t_dim1];
	if (*n < 2) {
		goto L100;
	}
	i__1 = *n;
	for (jj = 2; jj <= i__1; ++jj) {
		j = *n - jj + 1;
		i__2 = jj - 1;
		b[j] -= F77_SYMBOL(ddot) (&i__2, &t[j + 1 + j * t_dim1], &c__1, &b[j + 1], &c__1);
		b[j] /= t[j + j * t_dim1];
	}
L100:
	goto L140;

	/* solve trans(t)*x=b for t upper triangular. */

L110:
	b[1] /= t[t_dim1 + 1];
	if (*n < 2) {
		goto L130;
	}
	i__1 = *n;
	for (j = 2; j <= i__1; ++j) {
		i__2 = j - 1;
		b[j] -= F77_SYMBOL(ddot) (&i__2, &t[j * t_dim1 + 1], &c__1, &b[1], &c__1);
		b[j] /= t[j + j * t_dim1];
	}
L130:
L140:
L150:
	return 0;
}
