/* zhico.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


/*     zhico factors a double complex hermitian matrix by elimination with */
/*     symmetric pivoting and estimates the condition of the matrix. */

/*     if  rcond  is not needed, zhifa is slightly faster. */
/*     to solve  a*x = b , follow zhico by zhisl. */
/*     to compute  inverse(a)*c , follow zhico by zhisl. */
/*     to compute  inverse(a) , follow zhico by zhidi. */
/*     to compute  determinant(a) , follow zhico by zhidi. */
/*     to compute  inertia(a), follow zhico by zhidi. */

/*     on entry */

/*        a       double complex(lda, n) */
/*                the hermitian matrix to be factored. */
/*                only the diagonal and upper triangle are used. */

/*        lda     integer */
/*                the leading dimension of the array  a . */

/*        n       integer */
/*                the order of the matrix  a . */

/*     output */

/*        a       a block diagonal matrix and the multipliers which */
/*                were used to obtain it. */
/*                the factorization can be written  a = u*d*ctrans(u) */
/*                where  u  is a product of permutation and unit */
/*                upper triangular matrices , ctrans(u) is the */
/*                conjugate transpose of  u , and  d  is block diagonal */
/*                with 1 by 1 and 2 by 2 blocks. */

/*        kpvt    integer(n) */
/*                an integer vector of pivot indices. */

/*        rcond   double precision */
/*                an estimate of the reciprocal condition of  a . */
/*                for the system  a*x = b , relative perturbations */
/*                in  a  and  b  of size  epsilon  may cause */
/*                relative perturbations in  x  of size  epsilon/rcond . */
/*                if  rcond  is so small that the logical expression */
/*                           1.0 + rcond .eq. 1.0 */
/*                is true, then  a  may be singular to working */
/*                precision.  in particular,  rcond  is zero  if */
/*                exact singularity is detected or the estimate */
/*                underflows. */

/*        z       double complex(n) */
/*                a work vector whose contents are usually unimportant. */
/*                if  a  is close to a singular matrix, then  z  is */
/*                an approximate null vector in the sense that */
/*                norm(a*z) = rcond*norm(a)*norm(z) . */

/*     linpack. this version dated 08/14/78 . */
/*     cleve moler, university of new mexico, argonne national lab. */

/*     subroutines and functions */

/*     linpack zhifa */
/*     blas zaxpy,zdotc,zdscal,dzasum */
/*     fortran dabs,dmax1,dcmplx,dconjg,iabs */

/* Subroutine */ int zhico(doublecomplex *a, integer *lda, integer *n, 
	integer *kpvt, doublereal *rcond, doublecomplex *z__)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
    doublecomplex z__1, z__2, z__3, z__4, z__5;

    /* Builtin functions */
    void z_div(doublecomplex *, doublecomplex *, doublecomplex *), d_cnjg(
	    doublecomplex *, doublecomplex *);

    /* Local variables */
    static integer info, i__, j, k;
    static doublereal s;
    static doublecomplex t;
    extern /* Subroutine */ int zhifa(doublecomplex *, integer *, integer *, 
	    integer *, integer *);
    static doublecomplex denom;
    static doublereal anorm;
    extern /* Double Complex */ VOID zdotc(doublecomplex *, integer *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static doublereal ynorm;
    extern /* Subroutine */ int zaxpy(integer *, doublecomplex *, 
	    doublecomplex *, integer *, doublecomplex *, integer *);
    static doublecomplex ak, bk, ek;
    static integer kp, ks;
    extern /* Subroutine */ int zdscal(integer *, doublereal *, 
	    doublecomplex *, integer *);
    extern doublereal dzasum(integer *, doublecomplex *, integer *);
    static integer jm1, kps;
    static doublecomplex akm1, bkm1;


/*     internal variables */



/*     find norm of a using only upper half */

    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;
    --kpvt;
    --z__;

    /* Function Body */
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	d__1 = dzasum(&j, &a[j * a_dim1 + 1], &c__1);
	z__1.r = d__1, z__1.i = 0.;
	z__[i__2].r = z__1.r, z__[i__2].i = z__1.i;
	jm1 = j - 1;
	if (jm1 < 1) {
	    goto L20;
	}
	i__2 = jm1;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    i__3 = i__;
	    i__4 = i__;
	    i__5 = i__ + j * a_dim1;
	    i__6 = i__ + j * a_dim1;
	    z__2.r = a[i__6].r * 0. - a[i__6].i * -1., z__2.i = a[i__6].r * 
		    -1. + a[i__6].i * 0.;
	    d__3 = z__[i__4].r + ((d__1 = a[i__5].r, abs(d__1)) + (d__2 = 
		    z__2.r, abs(d__2)));
	    z__1.r = d__3, z__1.i = 0.;
	    z__[i__3].r = z__1.r, z__[i__3].i = z__1.i;
/* L10: */
	}
L20:
/* L30: */
	;
    }
    anorm = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	i__2 = j;
	d__1 = anorm, d__2 = z__[i__2].r;
	anorm = max(d__1,d__2);
/* L40: */
    }

/*     factor */

    zhifa(&a[a_offset], lda, n, &kpvt[1], &info);

/*     rcond = 1/(norm(a)*(estimate of norm(inverse(a)))) . */
/*     estimate = norm(z)/norm(y) where  a*z = y  and  a*y = e . */
/*     the components of  e  are chosen to cause maximum local */
/*     growth in the elements of w  where  u*d*w = e . */
/*     the vectors are frequently rescaled to avoid overflow. */

/*     solve u*d*w = e */

    ek.r = 1., ek.i = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	z__[i__2].r = 0., z__[i__2].i = 0.;
/* L50: */
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
    t.r = z__[i__1].r, t.i = z__[i__1].i;
    i__1 = kps;
    i__2 = kp;
    z__[i__1].r = z__[i__2].r, z__[i__1].i = z__[i__2].i;
    i__1 = kp;
    z__[i__1].r = t.r, z__[i__1].i = t.i;
L70:
    i__1 = k;
    i__2 = k;
    z__1.r = z__[i__2].r * 0. - z__[i__2].i * -1., z__1.i = z__[i__2].r * -1. 
	    + z__[i__2].i * 0.;
    if ((d__1 = z__[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
	z__3.r = ek.r * 0. - ek.i * -1., z__3.i = ek.r * -1. + ek.i * 0.;
	d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = z__3.r, abs(d__4));
	i__3 = k;
	i__4 = k;
	i__5 = k;
	z__5.r = z__[i__5].r * 0. - z__[i__5].i * -1., z__5.i = z__[i__5].r * 
		-1. + z__[i__5].i * 0.;
	d__8 = (d__5 = z__[i__4].r, abs(d__5)) + (d__6 = z__5.r, abs(d__6));
	z__4.r = z__[i__3].r / d__8, z__4.i = z__[i__3].i / d__8;
	z__2.r = d__7 * z__4.r, z__2.i = d__7 * z__4.i;
	ek.r = z__2.r, ek.i = z__2.i;
    }
    i__1 = k;
    i__2 = k;
    z__1.r = z__[i__2].r + ek.r, z__1.i = z__[i__2].i + ek.i;
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    i__1 = k - ks;
    zaxpy(&i__1, &z__[k], &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
    if (ks == 1) {
	goto L80;
    }
    i__1 = k - 1;
    i__2 = k - 1;
    z__1.r = z__[i__2].r * 0. - z__[i__2].i * -1., z__1.i = z__[i__2].r * -1. 
	    + z__[i__2].i * 0.;
    if ((d__1 = z__[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
	z__3.r = ek.r * 0. - ek.i * -1., z__3.i = ek.r * -1. + ek.i * 0.;
	d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = z__3.r, abs(d__4));
	i__3 = k - 1;
	i__4 = k - 1;
	i__5 = k - 1;
	z__5.r = z__[i__5].r * 0. - z__[i__5].i * -1., z__5.i = z__[i__5].r * 
		-1. + z__[i__5].i * 0.;
	d__8 = (d__5 = z__[i__4].r, abs(d__5)) + (d__6 = z__5.r, abs(d__6));
	z__4.r = z__[i__3].r / d__8, z__4.i = z__[i__3].i / d__8;
	z__2.r = d__7 * z__4.r, z__2.i = d__7 * z__4.i;
	ek.r = z__2.r, ek.i = z__2.i;
    }
    i__1 = k - 1;
    i__2 = k - 1;
    z__1.r = z__[i__2].r + ek.r, z__1.i = z__[i__2].i + ek.i;
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    i__1 = k - ks;
    zaxpy(&i__1, &z__[k - 1], &a[(k - 1) * a_dim1 + 1], &c__1, &z__[1], &
	    c__1);
L80:
    if (ks == 2) {
	goto L100;
    }
    i__1 = k;
    i__2 = k;
    z__1.r = z__[i__2].r * 0. - z__[i__2].i * -1., z__1.i = z__[i__2].r * -1. 
	    + z__[i__2].i * 0.;
    i__3 = k + k * a_dim1;
    i__4 = k + k * a_dim1;
    z__2.r = a[i__4].r * 0. - a[i__4].i * -1., z__2.i = a[i__4].r * -1. + a[
	    i__4].i * 0.;
    if ((d__1 = z__[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= (d__3 
	    = a[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
	goto L90;
    }
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    i__3 = k;
    i__4 = k;
    z__2.r = z__[i__4].r * 0. - z__[i__4].i * -1., z__2.i = z__[i__4].r * -1. 
	    + z__[i__4].i * 0.;
    s = ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))) / ((d__3 
	    = z__[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4)));
    zdscal(n, &s, &z__[1], &c__1);
    z__2.r = s, z__2.i = 0.;
    z__1.r = z__2.r * ek.r - z__2.i * ek.i, z__1.i = z__2.r * ek.i + z__2.i * 
	    ek.r;
    ek.r = z__1.r, ek.i = z__1.i;
L90:
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
	i__3 = k;
	z_div(&z__2, &z__[k], &a[k + k * a_dim1]);
	z__[i__3].r = z__2.r, z__[i__3].i = z__2.i;
    }
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
	i__3 = k;
	z__[i__3].r = 1., z__[i__3].i = 0.;
    }
    goto L110;
L100:
    d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
    z_div(&z__1, &a[k + k * a_dim1], &z__2);
    ak.r = z__1.r, ak.i = z__1.i;
    z_div(&z__1, &a[k - 1 + (k - 1) * a_dim1], &a[k - 1 + k * a_dim1]);
    akm1.r = z__1.r, akm1.i = z__1.i;
    d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
    z_div(&z__1, &z__[k], &z__2);
    bk.r = z__1.r, bk.i = z__1.i;
    z_div(&z__1, &z__[k - 1], &a[k - 1 + k * a_dim1]);
    bkm1.r = z__1.r, bkm1.i = z__1.i;
    z__2.r = ak.r * akm1.r - ak.i * akm1.i, z__2.i = ak.r * akm1.i + ak.i * 
	    akm1.r;
    z__1.r = z__2.r - 1., z__1.i = z__2.i;
    denom.r = z__1.r, denom.i = z__1.i;
    i__1 = k;
    z__3.r = akm1.r * bk.r - akm1.i * bk.i, z__3.i = akm1.r * bk.i + akm1.i * 
	    bk.r;
    z__2.r = z__3.r - bkm1.r, z__2.i = z__3.i - bkm1.i;
    z_div(&z__1, &z__2, &denom);
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    i__1 = k - 1;
    z__3.r = ak.r * bkm1.r - ak.i * bkm1.i, z__3.i = ak.r * bkm1.i + ak.i * 
	    bkm1.r;
    z__2.r = z__3.r - bk.r, z__2.i = z__3.i - bk.i;
    z_div(&z__1, &z__2, &denom);
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
L110:
    k -= ks;
    goto L60;
L120:
    s = 1. / dzasum(n, &z__[1], &c__1);
    zdscal(n, &s, &z__[1], &c__1);

/*     solve ctrans(u)*y = w */

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
    zdotc(&z__2, &i__3, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
    z__1.r = z__[i__2].r + z__2.r, z__1.i = z__[i__2].i + z__2.i;
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    if (ks == 2) {
	i__1 = k + 1;
	i__2 = k + 1;
	i__3 = k - 1;
	zdotc(&z__2, &i__3, &a[(k + 1) * a_dim1 + 1], &c__1, &z__[1], &c__1);
	z__1.r = z__[i__2].r + z__2.r, z__1.i = z__[i__2].i + z__2.i;
	z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    }
    kp = (i__1 = kpvt[k], abs(i__1));
    if (kp == k) {
	goto L140;
    }
    i__1 = k;
    t.r = z__[i__1].r, t.i = z__[i__1].i;
    i__1 = k;
    i__2 = kp;
    z__[i__1].r = z__[i__2].r, z__[i__1].i = z__[i__2].i;
    i__1 = kp;
    z__[i__1].r = t.r, z__[i__1].i = t.i;
L140:
L150:
    k += ks;
    goto L130;
L160:
    s = 1. / dzasum(n, &z__[1], &c__1);
    zdscal(n, &s, &z__[1], &c__1);

    ynorm = 1.;

/*     solve u*d*v = y */

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
    t.r = z__[i__1].r, t.i = z__[i__1].i;
    i__1 = kps;
    i__2 = kp;
    z__[i__1].r = z__[i__2].r, z__[i__1].i = z__[i__2].i;
    i__1 = kp;
    z__[i__1].r = t.r, z__[i__1].i = t.i;
L180:
    i__1 = k - ks;
    zaxpy(&i__1, &z__[k], &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
    if (ks == 2) {
	i__1 = k - ks;
	zaxpy(&i__1, &z__[k - 1], &a[(k - 1) * a_dim1 + 1], &c__1, &z__[1], &
		c__1);
    }
L190:
    if (ks == 2) {
	goto L210;
    }
    i__1 = k;
    i__2 = k;
    z__1.r = z__[i__2].r * 0. - z__[i__2].i * -1., z__1.i = z__[i__2].r * -1. 
	    + z__[i__2].i * 0.;
    i__3 = k + k * a_dim1;
    i__4 = k + k * a_dim1;
    z__2.r = a[i__4].r * 0. - a[i__4].i * -1., z__2.i = a[i__4].r * -1. + a[
	    i__4].i * 0.;
    if ((d__1 = z__[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) <= (d__3 
	    = a[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4))) {
	goto L200;
    }
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    i__3 = k;
    i__4 = k;
    z__2.r = z__[i__4].r * 0. - z__[i__4].i * -1., z__2.i = z__[i__4].r * -1. 
	    + z__[i__4].i * 0.;
    s = ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2))) / ((d__3 
	    = z__[i__3].r, abs(d__3)) + (d__4 = z__2.r, abs(d__4)));
    zdscal(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;
L200:
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) != 0.) {
	i__3 = k;
	z_div(&z__2, &z__[k], &a[k + k * a_dim1]);
	z__[i__3].r = z__2.r, z__[i__3].i = z__2.i;
    }
    i__1 = k + k * a_dim1;
    i__2 = k + k * a_dim1;
    z__1.r = a[i__2].r * 0. - a[i__2].i * -1., z__1.i = a[i__2].r * -1. + a[
	    i__2].i * 0.;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = z__1.r, abs(d__2)) == 0.) {
	i__3 = k;
	z__[i__3].r = 1., z__[i__3].i = 0.;
    }
    goto L220;
L210:
    d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
    z_div(&z__1, &a[k + k * a_dim1], &z__2);
    ak.r = z__1.r, ak.i = z__1.i;
    z_div(&z__1, &a[k - 1 + (k - 1) * a_dim1], &a[k - 1 + k * a_dim1]);
    akm1.r = z__1.r, akm1.i = z__1.i;
    d_cnjg(&z__2, &a[k - 1 + k * a_dim1]);
    z_div(&z__1, &z__[k], &z__2);
    bk.r = z__1.r, bk.i = z__1.i;
    z_div(&z__1, &z__[k - 1], &a[k - 1 + k * a_dim1]);
    bkm1.r = z__1.r, bkm1.i = z__1.i;
    z__2.r = ak.r * akm1.r - ak.i * akm1.i, z__2.i = ak.r * akm1.i + ak.i * 
	    akm1.r;
    z__1.r = z__2.r - 1., z__1.i = z__2.i;
    denom.r = z__1.r, denom.i = z__1.i;
    i__1 = k;
    z__3.r = akm1.r * bk.r - akm1.i * bk.i, z__3.i = akm1.r * bk.i + akm1.i * 
	    bk.r;
    z__2.r = z__3.r - bkm1.r, z__2.i = z__3.i - bkm1.i;
    z_div(&z__1, &z__2, &denom);
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    i__1 = k - 1;
    z__3.r = ak.r * bkm1.r - ak.i * bkm1.i, z__3.i = ak.r * bkm1.i + ak.i * 
	    bkm1.r;
    z__2.r = z__3.r - bk.r, z__2.i = z__3.i - bk.i;
    z_div(&z__1, &z__2, &denom);
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
L220:
    k -= ks;
    goto L170;
L230:
    s = 1. / dzasum(n, &z__[1], &c__1);
    zdscal(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;

/*     solve ctrans(u)*z = v */

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
    zdotc(&z__2, &i__3, &a[k * a_dim1 + 1], &c__1, &z__[1], &c__1);
    z__1.r = z__[i__2].r + z__2.r, z__1.i = z__[i__2].i + z__2.i;
    z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    if (ks == 2) {
	i__1 = k + 1;
	i__2 = k + 1;
	i__3 = k - 1;
	zdotc(&z__2, &i__3, &a[(k + 1) * a_dim1 + 1], &c__1, &z__[1], &c__1);
	z__1.r = z__[i__2].r + z__2.r, z__1.i = z__[i__2].i + z__2.i;
	z__[i__1].r = z__1.r, z__[i__1].i = z__1.i;
    }
    kp = (i__1 = kpvt[k], abs(i__1));
    if (kp == k) {
	goto L250;
    }
    i__1 = k;
    t.r = z__[i__1].r, t.i = z__[i__1].i;
    i__1 = k;
    i__2 = kp;
    z__[i__1].r = z__[i__2].r, z__[i__1].i = z__[i__2].i;
    i__1 = kp;
    z__[i__1].r = t.r, z__[i__1].i = t.i;
L250:
L260:
    k += ks;
    goto L240;
L270:
/*     make znorm = 1.0 */
    s = 1. / dzasum(n, &z__[1], &c__1);
    zdscal(n, &s, &z__[1], &c__1);
    ynorm = s * ynorm;

    if (anorm != 0.) {
	*rcond = ynorm / anorm;
    }
    if (anorm == 0.) {
	*rcond = 0.;
    }
    return 0;
} /* zhico_ */
