/*     dpofa factors a double precision symmetric positive definite
 *     matrix.
 *
 *     dpofa is usually called by dpoco, but it can be called
 *     directly with a saving in time if  rcond  is not needed.
 *     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
 *
 *     on entry
 *
 *        a       double precision(lda, n)
 *                the symmetric matrix to be factored.  only the
 *                diagonal and upper triangle are used.
 *
 *        lda     int
 *                the leading dimension of the array  a .
 *
 *        n       int
 *                the order of the matrix  a .
 *
 *     on return
 *
 *        a       an upper triangular matrix  r  so that  a = trans(r)*r
 *                where  trans(r)  is the transpose.
 *                the strict lower triangle is unaltered.
 *                if  info .ne. 0 , the factorization is not complete.
 *
 *        info    int
 *                = 0  for normal return.
 *                = k  signals an error condition.  the leading minor
 *                     of order  k  is not positive definite.
 *
 *     linpack.  this version dated 08/14/78 .
 *     cleve moler, university of new mexico, argonne national lab.
 *
 *     subroutines and functions
 *
 *     blas ddot
 *     fortran dsqrt
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;

int 
F77_SYMBOL(dpofa) (double *a, int *lda, int *n, int *
		   info)
{
	int a_dim1, a_offset, i__1, i__2, i__3;
	/* double sqrt(); */
	/* extern double F77_SYMBOL(ddot) (); */
	int j, k;
	double s, t;
	int jm1;

	a_dim1 = *lda;
	a_offset = a_dim1 + 1;
	a -= a_offset;

	/* begin block with ...exits to 40 */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		*info = j;
		s = 0.;
		jm1 = j - 1;
		if (jm1 < 1) {
			goto L20;
		}
		i__2 = jm1;
		for (k = 1; k <= i__2; ++k) {
			i__3 = k - 1;
			t = a[k + j * a_dim1] - F77_SYMBOL(ddot) (&i__3, &a[k * a_dim1 + 1], &c__1, &
						  a[j * a_dim1 + 1], &c__1);
			t /= a[k + k * a_dim1];
			a[k + j * a_dim1] = t;
			s += t * t;
		}
L20:
		s = a[j + j * a_dim1] - s;
		if (s <= 0.) {
			goto L40;	/* ......exit */
		}
		a[j + j * a_dim1] = sqrt(s);
	}
	*info = 0;
L40:
	return 0;
}
