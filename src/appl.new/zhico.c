/*
 *     zhico factors a complex*16 hermitian matrix by elimination with
 *     symmetric pivoting and estimates the condition of the matrix.
 *
 *     if  rcond  is not needed, zhifa is slightly faster.
 *     to solve  a*x = b , follow zhico by zhisl.
 *     to compute  inverse(a)*c , follow zhico by zhisl.
 *     to compute  inverse(a) , follow zhico by zhidi.
 *     to compute  determinant(a) , follow zhico by zhidi.
 *     to compute  inertia(a), follow zhico by zhidi.
 *
 *     on entry
 *
 *        a       complex*16(lda, n)
 *                the hermitian matrix to be factored.
 *                only the diagonal and upper triangle are used.
 *
 *        lda     int
 *                the leading dimension of the array  a .
 *
 *        n       int
 *                the order of the matrix  a .
 *
 *     output
 *
 *        a       a block diagonal matrix and the multipliers which
 *                were used to obtain it.
 *                the factorization can be written  a = u*d*ctrans(u)
 *                where  u  is a product of permutation and unit
 *                upper triangular matrices , ctrans(u) is the
 *                conjugate transpose of  u , and  d  is block diagonal
 *                with 1 by 1 and 2 by 2 blocks.
 *
 *        kpvt    int(n)
 *                an int vector of pivot indices.
 *
 *        rcond   double precision
 *                an estimate of the reciprocal condition of  a .
 *                for the system  a*x = b , relative perturbations
 *                in  a  and  b  of size  epsilon  may cause
 *                relative perturbations in  x  of size  epsilon/rcond .
 *                if  rcond  is so small that the int expression
 *                           1.0 + rcond .eq. 1.0
 *                is true, then  a  may be singular to working
 *                precision.  in particular,  rcond  is zero  if
 *                exact singularity is detected or the estimate
 *                underflows.
 *
 *        z       complex*16(n)
 *                a work vector whose contents are usually unimportant.
 *                if  a  is close to a singular matrix, then  z  is
 *                an approximate null vector in the sense that
 *                norm(a*z) = rcond*norm(a)*norm(z) .
 *
 *     linpack. this version dated 08/14/78 .
 *     cleve moler, university of new mexico, argonne national lab.
 *
 *     subroutines and functions
 *
 *     linpack zhifa
 *     blas zaxpy,zdotc,zdscal,dzasum
 *     fortran dabs,dmax1,dcmplx,dconjg,iabs
 */

#include "F77.h"

static int c__1 = 1;

int 
F77_SYMBOL(zhico) (complex * a, int *lda, int *n,
		   int *kpvt, double *rcond, complex * z)
{
	int a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5, i__6;
	double d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
	complex z__1, z__2, z__3, z__4, z__5;
	int info, i, j, k;
	double s;
	complex t;
	extern int F77_SYMBOL(zhifa) ();
	complex denom;
	double anorm;
	extern /* Double Complex */ void F77_SYMBOL(zdotc) ();
	double ynorm;
	extern int F77_SYMBOL(zaxpy) ();
	complex ak, bk, ek;
	int kp, ks;
	extern int F77_SYMBOL(zdscal) ();
	extern double F77_SYMBOL(dzasum) ();
	int jm1, kps;
	complex akm1, bkm1;

	a_dim1 = *lda;
	a_offset = a_dim1 + 1;
	a -= a_offset;
	--kpvt;
	--z;

	/* find norm of a using only upper half */

	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = j;
		d__1 = F77_SYMBOL(dzasum) (&j, &a[j * a_dim1 + 1], &c__1);
		z__1.r = d__1;
		z__1.i = 0.;
		z[i__2].r = z__1.r;
		z[i__2].i = z__1.i;
		jm1 = j - 1;
		if (jm1 < 1) {
			goto L20;
		}
		i__2 = jm1;
		for (i = 1; i <= i__2; ++i) {
			i__3 = i;
			i__4 = i;
			i__5 = i + j * a_dim1;
			i__6 = i + j * a_dim1;
			z__2.r = a[i__6].r * 0. - a[i__6].i * -1.;
			z__2.i = a[i__6].r * -1. + a[i__6].i * 0.;
			d__3 = z[i__4].r + ((d__1 = a[i__5].r, abs(d__1)) + (d__2 = z__2.r, abs(d__2)));
			z__1.r = d__3;
			z__1.i = 0.;
			z[i__3].r = z__1.r;
			z[i__3].i = z__1.i;
		}
L20:
		;
	}
	anorm = 0.;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = j;
		d__1 = anorm;
		d__2 = z[i__2].r;
		anorm = max(d__1, d__2);
	}

	/* factor */

	F77_SYMBOL(zhifa) (&a[a_offset], lda, n, &kpvt[1], &info);

	/* rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
	/* estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e . */
	/* the components of  e  are chosen to cause maximum local */
	/* growth in the elements of w  where  u*d*w = e . */
	/* the vectors are frequently rescaled to avoid overflow. */

	/* solve u*d*w = e */

	ek.r = 1.;
	ek.i = 0.;
	i__1 = *n;
	for (j = 1; j <= i__1; ++j) {
		i__2 = j;
		z[i__2].r = 0.;
		z[i__2].i = 0.;
	}
	k = *n;
L60:
	if (k == 0) {
		goto L120;
	}
	ks = 1;
	if (kpvt[k] < 0) {
		ks = 2;
	}
	kp = (i__1 = kpvt[k], abs(i__1));
	kps = k + 1 - ks;
	if (kp == kps) {
		goto L70;
	}
	i__1 = kps;
	t.r = z[i__1].r;
	t.i = z[i__1].i;
	i__1 = kps;
	i__2 = kp;
	z[i__1].r = z[i__2].r;
	z[i__1].i = z[i__2].i;
	i__1 = kp;
	z[i__1].r = t.r;
	z[i__1].i = t.i;
L70:
	i__1 = k;
	i__2 = k;
	z__1.r = z[i__2].r * 0. - z[i__2].i * -1.;
	z__1.i = z[i__2].r * -1. + z[i__2].i * 0.;
	if ((d__1 = z[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
		z__3.r = ek.r * 0. - ek.i * -1.;
		z__3.i = ek.r * -1. + ek.i * 0.;
		d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = z__3.r, abs(d__4));
		i__3 = k;
		i__4 = k;
		i__5 = k;
		z__5.r = z[i__5].r * 0. - z[i__5].i * -1.;
		z__5.i = z[i__5].r * -1. + z[i__5].i * 0.;
		d__8 = (d__5 = z[i__4].r, abs(d__5)) + (d__6 = z__5.r, abs(d__6));
		z__4.r = z[i__3].r / d__8;
		z__4.i = z[i__3].i / d__8;
		z__2.r = d__7 * z__4.r;
		z__2.i = d__7 * z__4.i;
		ek.r = z__2.r;
		ek.i = z__2.i;
	}
	i__1 = k;
	i__2 = k;
	z__1.r = z[i__2].r + ek.r;
	z__1.i = z[i__2].i + ek.i;
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	i__1 = k - ks;
	F77_SYMBOL(zaxpy) (&i__1, &z[k], &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
	if (ks == 1) {
		goto L80;
	}
	i__1 = k - 1;
	i__2 = k - 1;
	z__1.r = z[i__2].r * 0. - z[i__2].i * -1.;
	z__1.i = z[i__2].r * -1. + z[i__2].i * 0.;
	if ((d__1 = z[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
		z__3.r = ek.r * 0. - ek.i * -1.;
		z__3.i = ek.r * -1. + ek.i * 0.;
		d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = z__3.r, abs(d__4));
		i__3 = k - 1;
		i__4 = k - 1;
		i__5 = k - 1;
		z__5.r = z[i__5].r * 0. - z[i__5].i * -1.;
		z__5.i = z[i__5].r * -1. + z[i__5].i * 0.;
		d__8 = (d__5 = z[i__4].r, abs(d__5)) + (d__6 = z__5.r, abs(d__6));
		z__4.r = z[i__3].r / d__8;
		z__4.i = z[i__3].i / d__8;
		z__2.r = d__7 * z__4.r;
		z__2.i = d__7 * z__4.i;
		ek.r = z__2.r;
		ek.i = z__2.i;
	}
	i__1 = k - 1;
	i__2 = k - 1;
	z__1.r = z[i__2].r + ek.r;
	z__1.i = z[i__2].i + ek.i;
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	i__1 = k - ks;
	F77_SYMBOL(zaxpy) (&i__1, &z[k - 1], &a[(k - 1) * a_dim1 + 1], &c__1, &z[1], &c__1);
L80:
	if (ks == 2) {
		goto L100;
	}
	i__1 = k;
	i__2 = k;
	z__1.r = z[i__2].r * 0. - z[i__2].i * -1.;
	z__1.i = z[i__2].r * -1. + z[i__2].i * 0.;
	i__3 = k + k * a_dim1;
	i__4 = k + k * a_dim1;
	z__2.r = a[i__4].r * 0. - a[i__4].i * -1.;
	z__2.i = a[i__4].r * -1. + a[i__4].i * 0.;
	if ((d__1 = z[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= (d__3 =
		       a[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
		goto L90;
	}
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	i__3 = k;
	i__4 = k;
	z__2.r = z[i__4].r * 0. - z[i__4].i * -1.;
	z__2.i = z[i__4].r * -1. + z[i__4].i * 0.;
	s = ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))) / ((d__3
		      = z[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4)));
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);
	z__2.r = s;
	z__2.i = 0.;
	z__1.r = z__2.r * ek.r - z__2.i * ek.i;
	z__1.i = z__2.r * ek.i + z__2.i * ek.r;
	ek.r = z__1.r;
	ek.i = z__1.i;
L90:
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
		i__3 = k;
		ZDIV(&z__2, &z[k], &a[k + k * a_dim1]);
		z[i__3].r = z__2.r;
		z[i__3].i = z__2.i;
	}
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
		i__3 = k;
		z[i__3].r = 1.;
		z[i__3].i = 0.;
	}
	goto L110;
L100:
	ZCNJG(&z__2, &a[k - 1 + k * a_dim1]);
	ZDIV(&z__1, &a[k + k * a_dim1], &z__2);
	ak.r = z__1.r;
	ak.i = z__1.i;
	ZDIV(&z__1, &a[k - 1 + (k - 1) * a_dim1], &a[k - 1 + k * a_dim1]);
	akm1.r = z__1.r;
	akm1.i = z__1.i;
	ZCNJG(&z__2, &a[k - 1 + k * a_dim1]);
	ZDIV(&z__1, &z[k], &z__2);
	bk.r = z__1.r;
	bk.i = z__1.i;
	ZDIV(&z__1, &z[k - 1], &a[k - 1 + k * a_dim1]);
	bkm1.r = z__1.r;
	bkm1.i = z__1.i;
	z__2.r = ak.r * akm1.r - ak.i * akm1.i;
	z__2.i = ak.r * akm1.i + ak.i * akm1.r;
	z__1.r = z__2.r - 1.;
	z__1.i = z__2.i;
	denom.r = z__1.r;
	denom.i = z__1.i;
	i__1 = k;
	z__3.r = akm1.r * bk.r - akm1.i * bk.i;
	z__3.i = akm1.r * bk.i + akm1.i * bk.r;
	z__2.r = z__3.r - bkm1.r;
	z__2.i = z__3.i - bkm1.i;
	ZDIV(&z__1, &z__2, &denom);
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	i__1 = k - 1;
	z__3.r = ak.r * bkm1.r - ak.i * bkm1.i;
	z__3.i = ak.r * bkm1.i + ak.i * bkm1.r;
	z__2.r = z__3.r - bk.r;
	z__2.i = z__3.i - bk.i;
	ZDIV(&z__1, &z__2, &denom);
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
L110:
	k -= ks;
	goto L60;
L120:
	s = 1. / F77_SYMBOL(dzasum) (n, &z[1], &c__1);
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);

	/* solve ctrans(u)*y = w */

	k = 1;
L130:
	if (k > *n) {
		goto L160;
	}
	ks = 1;
	if (kpvt[k] < 0) {
		ks = 2;
	}
	if (k == 1) {
		goto L150;
	}
	i__1 = k;
	i__2 = k;
	i__3 = k - 1;
	F77_SYMBOL(zdotc) (&z__2, &i__3, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
	z__1.r = z[i__2].r + z__2.r;
	z__1.i = z[i__2].i + z__2.i;
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	if (ks == 2) {
		i__1 = k + 1;
		i__2 = k + 1;
		i__3 = k - 1;
		F77_SYMBOL(zdotc) (&z__2, &i__3, &a[(k + 1) * a_dim1 + 1], &c__1, &z[1], &c__1);
		z__1.r = z[i__2].r + z__2.r;
		z__1.i = z[i__2].i + z__2.i;
		z[i__1].r = z__1.r;
		z[i__1].i = z__1.i;
	}
	kp = (i__1 = kpvt[k], abs(i__1));
	if (kp == k) {
		goto L140;
	}
	i__1 = k;
	t.r = z[i__1].r;
	t.i = z[i__1].i;
	i__1 = k;
	i__2 = kp;
	z[i__1].r = z[i__2].r;
	z[i__1].i = z[i__2].i;
	i__1 = kp;
	z[i__1].r = t.r;
	z[i__1].i = t.i;
L140:
L150:
	k += ks;
	goto L130;
L160:
	s = 1. / F77_SYMBOL(dzasum) (n, &z[1], &c__1);
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);

	ynorm = 1.;

	/* solve u*d*v = y */

	k = *n;
L170:
	if (k == 0) {
		goto L230;
	}
	ks = 1;
	if (kpvt[k] < 0) {
		ks = 2;
	}
	if (k == ks) {
		goto L190;
	}
	kp = (i__1 = kpvt[k], abs(i__1));
	kps = k + 1 - ks;
	if (kp == kps) {
		goto L180;
	}
	i__1 = kps;
	t.r = z[i__1].r;
	t.i = z[i__1].i;
	i__1 = kps;
	i__2 = kp;
	z[i__1].r = z[i__2].r;
	z[i__1].i = z[i__2].i;
	i__1 = kp;
	z[i__1].r = t.r;
	z[i__1].i = t.i;
L180:
	i__1 = k - ks;
	F77_SYMBOL(zaxpy) (&i__1, &z[k], &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
	if (ks == 2) {
		i__1 = k - ks;
		F77_SYMBOL(zaxpy) (&i__1, &z[k - 1], &a[(k - 1) * a_dim1 + 1], &c__1, &z[1], &
				   c__1);
	}
L190:
	if (ks == 2) {
		goto L210;
	}
	i__1 = k;
	i__2 = k;
	z__1.r = z[i__2].r * 0. - z[i__2].i * -1.;
	z__1.i = z[i__2].r * -1. + z[i__2].i * 0.;
	i__3 = k + k * a_dim1;
	i__4 = k + k * a_dim1;
	z__2.r = a[i__4].r * 0. - a[i__4].i * -1.;
	z__2.i = a[i__4].r * -1. + a[i__4].i * 0.;
	if ((d__1 = z[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= (d__3 =
		       a[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
		goto L200;
	}
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	i__3 = k;
	i__4 = k;
	z__2.r = z[i__4].r * 0. - z[i__4].i * -1.;
	z__2.i = z[i__4].r * -1. + z[i__4].i * 0.;
	s = ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))) / ((d__3
		      = z[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4)));
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);
	ynorm = s * ynorm;
L200:
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
		i__3 = k;
		ZDIV(&z__2, &z[k], &a[k + k * a_dim1]);
		z[i__3].r = z__2.r;
		z[i__3].i = z__2.i;
	}
	i__1 = k + k * a_dim1;
	i__2 = k + k * a_dim1;
	z__1.r = a[i__2].r * 0. - a[i__2].i * -1.;
	z__1.i = a[i__2].r * -1. + a[i__2].i * 0.;
	if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
		i__3 = k;
		z[i__3].r = 1.;
		z[i__3].i = 0.;
	}
	goto L220;
L210:
	ZCNJG(&z__2, &a[k - 1 + k * a_dim1]);
	ZDIV(&z__1, &a[k + k * a_dim1], &z__2);
	ak.r = z__1.r;
	ak.i = z__1.i;
	ZDIV(&z__1, &a[k - 1 + (k - 1) * a_dim1], &a[k - 1 + k * a_dim1]);
	akm1.r = z__1.r;
	akm1.i = z__1.i;
	ZCNJG(&z__2, &a[k - 1 + k * a_dim1]);
	ZDIV(&z__1, &z[k], &z__2);
	bk.r = z__1.r;
	bk.i = z__1.i;
	ZDIV(&z__1, &z[k - 1], &a[k - 1 + k * a_dim1]);
	bkm1.r = z__1.r;
	bkm1.i = z__1.i;
	z__2.r = ak.r * akm1.r - ak.i * akm1.i;
	z__2.i = ak.r * akm1.i + ak.i * akm1.r;
	z__1.r = z__2.r - 1.;
	z__1.i = z__2.i;
	denom.r = z__1.r;
	denom.i = z__1.i;
	i__1 = k;
	z__3.r = akm1.r * bk.r - akm1.i * bk.i;
	z__3.i = akm1.r * bk.i + akm1.i * bk.r;
	z__2.r = z__3.r - bkm1.r;
	z__2.i = z__3.i - bkm1.i;
	ZDIV(&z__1, &z__2, &denom);
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	i__1 = k - 1;
	z__3.r = ak.r * bkm1.r - ak.i * bkm1.i;
	z__3.i = ak.r * bkm1.i + ak.i * bkm1.r;
	z__2.r = z__3.r - bk.r;
	z__2.i = z__3.i - bk.i;
	ZDIV(&z__1, &z__2, &denom);
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
L220:
	k -= ks;
	goto L170;
L230:
	s = 1. / F77_SYMBOL(dzasum) (n, &z[1], &c__1);
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);
	ynorm = s * ynorm;

	/* solve ctrans(u)*z = v */

	k = 1;
L240:
	if (k > *n) {
		goto L270;
	}
	ks = 1;
	if (kpvt[k] < 0) {
		ks = 2;
	}
	if (k == 1) {
		goto L260;
	}
	i__1 = k;
	i__2 = k;
	i__3 = k - 1;
	F77_SYMBOL(zdotc) (&z__2, &i__3, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
	z__1.r = z[i__2].r + z__2.r;
	z__1.i = z[i__2].i + z__2.i;
	z[i__1].r = z__1.r;
	z[i__1].i = z__1.i;
	if (ks == 2) {
		i__1 = k + 1;
		i__2 = k + 1;
		i__3 = k - 1;
		F77_SYMBOL(zdotc) (&z__2, &i__3, &a[(k + 1) * a_dim1 + 1], &c__1, &z[1], &c__1);
		z__1.r = z[i__2].r + z__2.r;
		z__1.i = z[i__2].i + z__2.i;
		z[i__1].r = z__1.r;
		z[i__1].i = z__1.i;
	}
	kp = (i__1 = kpvt[k], abs(i__1));
	if (kp == k) {
		goto L250;
	}
	i__1 = k;
	t.r = z[i__1].r;
	t.i = z[i__1].i;
	i__1 = k;
	i__2 = kp;
	z[i__1].r = z[i__2].r;
	z[i__1].i = z[i__2].i;
	i__1 = kp;
	z[i__1].r = t.r;
	z[i__1].i = t.i;
L250:
L260:
	k += ks;
	goto L240;
L270:
	/* make znorm = 1.0 */
	s = 1. / F77_SYMBOL(dzasum) (n, &z[1], &c__1);
	F77_SYMBOL(zdscal) (n, &s, &z[1], &c__1);
	ynorm = s * ynorm;

	if (anorm != 0.) {
		*rcond = ynorm / anorm;
	}
	if (anorm == 0.) {
		*rcond = 0.;
	}
	return 0;
}
