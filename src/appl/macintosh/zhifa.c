/* zhifa.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/*     zhifa factors a double complex hermitian matrix by elimination */
/*     with symmetric pivoting. */

/*     to solve  a*x = b , follow zhifa by zhisl. */
/*     to compute  inverse(a)*c , follow zhifa by zhisl. */
/*     to compute  determinant(a) , follow zhifa by zhidi. */
/*     to compute  inertia(a) , follow zhifa by zhidi. */
/*     to compute  inverse(a) , follow zhifa by zhidi. */

/*     on entry */

/*        a       double complex(lda,n) */
/*                the hermitian matrix to be factored. */
/*                only the diagonal and upper triangle are used. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*     on return */

/*        a       a block diagonal matrix and the multipliers which */
/*                were used to obtain it. */
/*                the factorization can be written  a = u*d*ctrans(u) */
/*                where  u  is a product of permutation and unit */
/*                upper triangular matrices , ctrans(u) is the */
/*                conjugate transpose of  u , and  d  is block diagonal */
/*                with 1 by 1 and 2 by 2 blocks. */

/*        kpvt    integer(n) */
/*                an integer vector of pivot indices. */

/*        info    integer */
/*                = 0  normal value. */
/*                = k  if the k-th pivot block is singular. this is */
/*                     not an error condition for this subroutine, */
/*                     but it does indicate that zhisl or zhidi may */
/*                     divide by zero if called. */

/*     linpack. this version dated 08/14/78 . */
/*     james bunch, univ. calif. san diego, argonne nat. lab. */

/*     subroutines and functions */

/*     blas zaxpy,zswap,izamax */
/*     fortran dabs,dmax1,dcmplx,dconjg,dsqrt */

/* Subroutine */ int zhifa(doublecomplex *a, integer *lda, integer *n, 
	integer *kpvt, integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;
    doublereal d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double sqrt(doublereal);
    void d_cnjg(doublecomplex *, doublecomplex *), z_div(doublecomplex *, 
	    doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer imax, jmax;
    static doublecomplex mulk;
    static logical swap;
    static integer j, k;
    static doublecomplex t;
    static doublereal alpha;
    static doublecomplex denom;
    static integer kstep;
    extern /* Subroutine */ int zswap(integer *, doublecomplex *, integer *, 
	    doublecomplex *, integer *), zaxpy(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static integer imaxp1;
    static doublecomplex mulkm1, ak, bk;
    static integer jj;
    static doublereal absakk, colmax;
    extern integer izamax(integer *, doublecomplex *, integer *);
    static integer km1, km2;
    static doublereal rowmax;
    static doublecomplex akm1, bkm1;


/*     internal variables */



/*     initialize */

/*     alpha is used in choosing pivot block size. */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --kpvt;

    /* Function Body */
    alpha = (sqrt(17.) + 1.) / 8.;

    *info = 0;

/*     main loop on k, which goes from n to 1. */

    k = *n;
L10:

/*        leave the loop if k=0 or k=1. */

/*     ...exit */
    if (k == 0) {
	goto L200;
    }
    if (k > 1) {
	goto L20;
    }
    kpvt[1] = 1;
    i__1 = a_dim1 + 1;
    i__2 = a_dim1 + 1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
	*info = 1;
    }
/*     ......exit */
    goto L200;
L20:

/*        this section of code determines the kind of */
/*        elimination to be performed.  when it is completed, */
/*        kstep will be set to the size of the pivot block, and */
/*        swap will be set to .true. if an interchange is */
/*        required. */

    km1 = k - 1;
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    absakk = (d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2));

/*        determine the largest off-diagonal element in */
/*        column k. */

    i__1 = k - 1;
    imax = izamax(&i__1, &a[k * a_dim1 + 1], &c__1);
    i__1 = imax + k * a_dim1;
    i__2 = imax + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    colmax = (d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2));
    if (absakk < alpha * colmax) {
	goto L30;
    }
    kstep = 1;
    swap = FALSE_;
    goto L90;
L30:

/*           determine the largest off-diagonal element in */
/*           row imax. */

    rowmax = 0.;
    imaxp1 = imax + 1;
    i__1 = k;
    for (j = imaxp1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = imax + j * a_dim1;
	i__3 = imax + j * a_dim1;
	z__1.r = a[i__3].r * 0. - a[i__3].i * -1., z__1.i = a[i__3].r * -1. + 
		a[i__3].i * 0.;
	d__3 = rowmax, d__4 = (d__1 = a[i__2].r, abs(d__1)) + (d__2 = z__1.r, 
		abs(d__2));
	rowmax = max(d__3,d__4);
/* L40: */
    }
    if (imax == 1) {
	goto L50;
    }
    i__1 = imax - 1;
    jmax = izamax(&i__1, &a[imax * a_dim1 + 1], &c__1);
/* Computing MAX */
    i__1 = jmax + imax * a_dim1;
    i__2 = jmax + imax * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    d__3 = rowmax, d__4 = (d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(
	    d__2));
    rowmax = max(d__3,d__4);
L50:
    i__1 = imax + imax * a_dim1;
    i__2 = imax + imax * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) < alpha * 
	    rowmax) {
	goto L60;
    }
    kstep = 1;
    swap = TRUE_;
    goto L80;
L60:
    if (absakk < alpha * colmax * (colmax / rowmax)) {
	goto L70;
    }
    kstep = 1;
    swap = FALSE_;
    goto L80;
L70:
    kstep = 2;
    swap = imax != km1;
L80:
L90:
    if (max(absakk,colmax) != 0.) {
	goto L100;
    }

/*           column k is zero.  set info and iterate the loop. */

    kpvt[k] = k;
    *info = k;
    goto L190;
L100:
    if (kstep == 2) {
	goto L140;
    }

/*           1 x 1 pivot block. */

    if (! swap) {
	goto L120;
    }

/*              perform an interchange. */

    zswap(&imax, &a[imax * a_dim1 + 1], &c__1, &a[k * a_dim1 + 1], &c__1);
    i__1 = k;
    for (jj = imax; jj <= i__1; ++jj) {
	j = k + imax - jj;
	d_cnjg(&z__1, &a[j + k * a_dim1]);
	t.r = z__1.r, t.i = z__1.i;
	i__2 = j + k * a_dim1;
	d_cnjg(&z__1, &a[imax + j * a_dim1]);
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	i__2 = imax + j * a_dim1;
	a[i__2].r = t.r, a[i__2].i = t.i;
/* L110: */
    }
L120:

/*           perform the elimination. */

    i__1 = km1;
    for (jj = 1; jj <= i__1; ++jj) {
	j = k - jj;
	i__2 = j + k * a_dim1;
	z__2.r = -a[i__2].r, z__2.i = -a[i__2].i;
	z_div(&z__1, &z__2, &a[k + k * a_dim1]);
	mulk.r = z__1.r, mulk.i = z__1.i;
	d_cnjg(&z__1, &mulk);
	t.r = z__1.r, t.i = z__1.i;
	zaxpy(&j, &t, &a[k * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1], &c__1);
	i__2 = j + j * a_dim1;
	i__3 = j + j * a_dim1;
	d__1 = a[i__3].r;
	z__1.r = d__1, z__1.i = 0.;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	i__2 = j + k * a_dim1;
	a[i__2].r = mulk.r, a[i__2].i = mulk.i;
/* L130: */
    }

/*           set the pivot array. */

    kpvt[k] = k;
    if (swap) {
	kpvt[k] = imax;
    }
    goto L190;
L140:

/*           2 x 2 pivot block. */

    if (! swap) {
	goto L160;
    }

/*              perform an interchange. */

    zswap(&imax, &a[imax * a_dim1 + 1], &c__1, &a[(k - 1) * a_dim1 + 1], &
	    c__1);
    i__1 = km1;
    for (jj = imax; jj <= i__1; ++jj) {
	j = km1 + imax - jj;
	d_cnjg(&z__1, &a[j + (k - 1) * a_dim1]);
	t.r = z__1.r, t.i = z__1.i;
	i__2 = j + (k - 1) * a_dim1;
	d_cnjg(&z__1, &a[imax + j * a_dim1]);
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
	i__2 = imax + j * a_dim1;
	a[i__2].r = t.r, a[i__2].i = t.i;
/* L150: */
    }
    i__1 = k - 1 + k * a_dim1;
    t.r = a[i__1].r, t.i = a[i__1].i;
    i__1 = k - 1 + k * a_dim1;
    i__2 = imax + k * a_dim1;
    a[i__1].r = a[i__2].r, a[i__1].i = a[i__2].i;
    i__1 = imax + k * a_dim1;
    a[i__1].r = t.r, a[i__1].i = t.i;
L160:

/*           perform the elimination. */

    km2 = k - 2;
    if (km2 == 0) {
	goto L180;
    }
    z_div(&z__1, &a[k + k * a_dim1], &a[k - 1 + k * a_dim1]);
    ak.r = z__1.r, ak.i = z__1.i;
    d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
    z_div(&z__1, &a[k - 1 + (k - 1) * a_dim1], &z__2);
    akm1.r = z__1.r, akm1.i = z__1.i;
    z__2.r = ak.r * akm1.r - ak.i * akm1.i, z__2.i = ak.r * akm1.i + ak.i * 
	    akm1.r;
    z__1.r = 1. - z__2.r, z__1.i = -z__2.i;
    denom.r = z__1.r, denom.i = z__1.i;
    i__1 = km2;
    for (jj = 1; jj <= i__1; ++jj) {
	j = km1 - jj;
	z_div(&z__1, &a[j + k * a_dim1], &a[k - 1 + k * a_dim1]);
	bk.r = z__1.r, bk.i = z__1.i;
	d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
	z_div(&z__1, &a[j + (k - 1) * a_dim1], &z__2);
	bkm1.r = z__1.r, bkm1.i = z__1.i;
	z__3.r = akm1.r * bk.r - akm1.i * bk.i, z__3.i = akm1.r * bk.i + 
		akm1.i * bk.r;
	z__2.r = z__3.r - bkm1.r, z__2.i = z__3.i - bkm1.i;
	z_div(&z__1, &z__2, &denom);
	mulk.r = z__1.r, mulk.i = z__1.i;
	z__3.r = ak.r * bkm1.r - ak.i * bkm1.i, z__3.i = ak.r * bkm1.i + ak.i 
		* bkm1.r;
	z__2.r = z__3.r - bk.r, z__2.i = z__3.i - bk.i;
	z_div(&z__1, &z__2, &denom);
	mulkm1.r = z__1.r, mulkm1.i = z__1.i;
	d_cnjg(&z__1, &mulk);
	t.r = z__1.r, t.i = z__1.i;
	zaxpy(&j, &t, &a[k * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1], &c__1);
	d_cnjg(&z__1, &mulkm1);
	t.r = z__1.r, t.i = z__1.i;
	zaxpy(&j, &t, &a[(k - 1) * a_dim1 + 1], &c__1, &a[j * a_dim1 + 1], &
		c__1);
	i__2 = j + k * a_dim1;
	a[i__2].r = mulk.r, a[i__2].i = mulk.i;
	i__2 = j + (k - 1) * a_dim1;
	a[i__2].r = mulkm1.r, a[i__2].i = mulkm1.i;
	i__2 = j + j * a_dim1;
	i__3 = j + j * a_dim1;
	d__1 = a[i__3].r;
	z__1.r = d__1, z__1.i = 0.;
	a[i__2].r = z__1.r, a[i__2].i = z__1.i;
/* L170: */
    }
L180:

/*           set the pivot array. */

    kpvt[k] = 1 - k;
    if (swap) {
	kpvt[k] = -imax;
    }
    kpvt[k - 1] = kpvt[k];
L190:
    k -= kstep;
    goto L10;
L200:
    return 0;
} /* zhifa_ */
