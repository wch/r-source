/*     dposl solves the double precision symmetric positive definite
 *     system a * x = b
 *     using the factors computed by dpoco or dpofa.
 *
 *     on entry
 *
 *        a       double precision(lda, n)
 *                the output from dpoco or dpofa.
 *
 *        lda     int
 *                the leading dimension of the array  a .
 *
 *        n       int
 *                the order of the matrix  a .
 *
 *        b       double precision(n)
 *                the right hand side vector.
 *
 *     on return
 *
 *        b       the solution vector  x .
 *
 *     error condition
 *
 *        a division by zero will occur if the input factor contains
 *        a zero on the diagonal.  technically this indicates
 *        singularity but it is usually caused by improper subroutine
 *        arguments.  it will not occur if the subroutines are called
 *        correctly and  info .eq. 0 .
 *
 *     to compute  inverse(a) * c  where  c  is a matrix
 *     with  p  columns
 *           call dpoco(a,lda,n,rcond,z,info)
 *           if (rcond is too small .or. info .ne. 0) go to ...
 *           do 10 j = 1, p
 *              call dposl(a,lda,n,c(1,j))
 *        10 continue
 *
 *     linpack.  this version dated 08/14/78 .
 *     cleve moler, university of new mexico, argonne national lab.
 *
 *     subroutines and functions
 *
 *     blas daxpy,ddot
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;

int 
F77_SYMBOL(dposl) (double *a, int *lda, int *n,
		   double *b)
{
	double t;
/*
	extern double F77_SYMBOL(ddot) ();
	extern int F77_SYMBOL(daxpy) ();
*/
	int a_dim1, a_offset, i__1, i__2;
	int k;
	int kb;

	a_dim1 = *lda;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--b;

	/* solve trans(r)*y = b */

	i__1 = *n;
	for (k = 1; k <= i__1; ++k) {
		i__2 = k - 1;
		t = F77_SYMBOL(ddot) (&i__2, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
		b[k] = (b[k] - t) / a[k + k * a_dim1];
	}

	/* solve r*x = y */

	i__1 = *n;
	for (kb = 1; kb <= i__1; ++kb) {
		k = *n + 1 - kb;
		b[k] /= a[k + k * a_dim1];
		t = -b[k];
		i__2 = k - 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
	}
	return 0;
}
