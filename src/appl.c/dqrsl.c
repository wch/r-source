/*
 *     dqrsl applies the output of dqrdc to compute coordinate
 *     transformations, projections, and least squares solutions.
 *     for k .le. min(n,p), let xk be the matrix
 *
 *            xk = (x(jpvt(1)),x(jpvt(2)), ... ,x(jpvt(k)))
 *
 *     formed from columnns jpvt(1), ... ,jpvt(k) of the original
 *     n x p matrix x that was input to dqrdc (if no pivoting was
 *     done, xk consists of the first k columns of x in their
 *     original order).  dqrdc produces a factored orthogonal matrix q
 *     and an upper triangular matrix r such that
 *
 *              xk = q * (r)
 *                       (0)
 *
 *     this information is contained in coded form in the arrays
 *     x and qraux.
 *
 *     on entry
 *
 *        x      double precision(ldx,p).
 *               x contains the output of dqrdc.
 *
 *        ldx    int.
 *               ldx is the leading dimension of the array x.
 *
 *        n      int.
 *               n is the number of rows of the matrix xk.  it must
 *               have the same value as n in dqrdc.
 *
 *        k      int.
 *               k is the number of columns of the matrix xk.  k
 *               must nnot be greater than min(n,p), where p is the
 *               same as in the calling sequence to dqrdc.
 *
 *        qraux  double precision(p).
 *               qraux contains the auxiliary output from dqrdc.
 *
 *        y      double precision(n)
 *               y contains an n-vector that is to be manipulated
 *               by dqrsl.
 *
 *        job    int.
 *               job specifies what is to be computed.  job has
 *               the decimal expansion abcde, with the following
 *               meaning.
 *
 *                    if a.ne.0, compute qy.
 *                    if b,c,d, or e .ne. 0, compute qty.
 *                    if c.ne.0, compute b.
 *                    if d.ne.0, compute rsd.
 *                    if e.ne.0, compute xb.
 *
 *               note that a request to compute b, rsd, or xb
 *               automatically triggers the computation of qty, for
 *               which an array must be provided in the calling
 *               sequence.
 *
 *     on return
 *
 *        qy     double precision(n).
 *               qy conntains q*y, if its computation has been
 *               requested.
 *
 *        qty    double precision(n).
 *               qty contains trans(q)*y, if its computation has
 *               been requested.  here trans(q) is the
 *               transpose of the matrix q.
 *
 *        b      double precision(k)
 *               b contains the solution of the least squares problem
 *
 *                    minimize norm2(y - xk*b),
 *
 *               if its computation has been requested.  (note that
 *               if pivoting was requested in dqrdc, the j-th
 *               component of b will be associated with column jpvt(j)
 *               of the original matrix x that was input into dqrdc.)
 *
 *        rsd    double precision(n).
 *               rsd contains the least squares residual y - xk*b,
 *               if its computation has been requested.  rsd is
 *               also the orthogonal projection of y onto the
 *               orthogonal complement of the column space of xk.
 *
 *        xb     double precision(n).
 *               xb contains the least squares approximation xk*b,
 *               if its computation has been requested.  xb is also
 *               the orthogonal projection of y onto the column space
 *               of x.
 *
 *        info   int.
 *               info is zero unless the computation of b has
 *               been requested and r is exactly singular.  in
 *               this case, info is the index of the first zero
 *               diagonal element of r and b is left unaltered.
 *
 *     the parameters qy, qty, b, rsd, and xb are not referenced
 *     if their computation is not requested and in this case
 *     can be replaced by dummy variables in the calling program.
 *     to save storage, the user may in some cases use the same
 *     array for different parameters in the calling sequence.  a
 *     frequently occuring example is when one wishes to compute
 *     any of b, rsd, or xb and does not need y or qty.  in this
 *     case one may identify y, qty, and one of b, rsd, or xb, while
 *     providing separate arrays for anything else that is to be
 *     computed.  thus the calling sequence
 *
 *          call dqrsl(x,ldx,n,k,qraux,y,dum,y,b,y,dum,110,info)
 *
 *     will result in the computation of b and rsd, with rsd
 *     overwriting y.  more generally, each item in the following
 *     list contains groups of permissible identifications for
 *     a single callinng sequence.
 *
 *          1. (y,qty,b) (rsd) (xb) (qy)
 *
 *          2. (y,qty,rsd) (b) (xb) (qy)
 *
 *          3. (y,qty,xb) (b) (rsd) (qy)
 *
 *          4. (y,qy) (qty,b) (rsd) (xb)
 *
 *          5. (y,qy) (qty,rsd) (b) (xb)
 *
 *          6. (y,qy) (qty,xb) (b) (rsd)
 *
 *     in any group the value returned in the array allocated to
 *     the group corresponds to the last member of the group.
 *
 *     linpack. this version dated 08/14/78 .
 *     g.w. stewart, university of maryland, argonne national lab.
 *
 *     dqrsl uses the following functions and subprograms.
 *
 *     blas daxpy,dcopy,ddot
 *     fortran dabs,min0,mod
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;

int 
F77_SYMBOL(dqrsl) (double *x, int *ldx, int *n, int *
		   k, double *qraux, double *y, double *qy, double *qty,
		   double *b, double *rsd, double *xb, int *job, int
		   *info)
{
	double t;
	double temp;
/*
	extern double F77_SYMBOL(ddot) ();
	extern int F77_SYMBOL(dcopy) (), F77_SYMBOL(daxpy) ();
*/
	int cb;
	int cqty;
	int cr;
	int cxb, cqy;
	int i, j;
	int jj;
	int ju, kp1;
	int x_dim1, x_offset, i__1, i__2;

	x_dim1 = *ldx;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	--qraux;
	--y;
	--qy;
	--qty;
	--b;
	--rsd;
	--xb;

	/* set info flag. */

	*info = 0;

	/* determine what is to be computed. */

	cqy = *job / 10000 != 0;
	cqty = *job % 10000 != 0;
	cb = *job % 1000 / 100 != 0;
	cr = *job % 100 / 10 != 0;
	cxb = *job % 10 != 0;
	i__1 = *k, i__2 = *n - 1;
	ju = min(i__1, i__2);

	/* special action when n=1. */

	if (ju != 0) {
		goto L40;
	}
	if (cqy) {
		qy[1] = y[1];
	}
	if (cqty) {
		qty[1] = y[1];
	}
	if (cxb) {
		xb[1] = y[1];
	}
	if (!cb) {
		goto L30;
	}
	if (x[x_dim1 + 1] != 0.) {
		goto L10;
	}
	*info = 1;
	goto L20;
L10:
	b[1] = y[1] / x[x_dim1 + 1];
L20:
L30:
	if (cr) {
		rsd[1] = 0.;
	}
	goto L250;
L40:

	/* set up to compute qy or qty. */

	if (cqy) {
		F77_SYMBOL(dcopy) (n, &y[1], &c__1, &qy[1], &c__1);
	}
	if (cqty) {
		F77_SYMBOL(dcopy) (n, &y[1], &c__1, &qty[1], &c__1);
	}
	if (!cqy) {
		goto L70;
	}
	/* compute qy. */

	i__1 = ju;
	for (jj = 1; jj <= i__1; ++jj) {
		j = ju - jj + 1;
		if (qraux[j] == 0.) {
			goto L50;
		}
		temp = x[j + j * x_dim1];
		x[j + j * x_dim1] = qraux[j];
		i__2 = *n - j + 1;
		t = -F77_SYMBOL(ddot) (&i__2, &x[j + j * x_dim1], &c__1, &qy[j], &c__1) / x[j + j
								  * x_dim1];
		i__2 = *n - j + 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &x[j + j * x_dim1], &c__1, &qy[j], &c__1);
		x[j + j * x_dim1] = temp;
L50:
		;
	}
L70:
	if (!cqty) {
		goto L100;
	}
	/* compute trans(q)*y. */

	i__1 = ju;
	for (j = 1; j <= i__1; ++j) {
		if (qraux[j] == 0.) {
			goto L80;
		}
		temp = x[j + j * x_dim1];
		x[j + j * x_dim1] = qraux[j];
		i__2 = *n - j + 1;
		t = -F77_SYMBOL(ddot) (&i__2, &x[j + j * x_dim1], &c__1, &qty[j], &c__1) / x[j +
								j * x_dim1];
		i__2 = *n - j + 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &x[j + j * x_dim1], &c__1, &qty[j], &c__1);
		x[j + j * x_dim1] = temp;
L80:
		;
	}
L100:

	/* set up to compute b, rsd, or xb. */

	if (cb) {
		F77_SYMBOL(dcopy) (k, &qty[1], &c__1, &b[1], &c__1);
	}
	kp1 = *k + 1;
	if (cxb) {
		F77_SYMBOL(dcopy) (k, &qty[1], &c__1, &xb[1], &c__1);
	}
	if (cr && *k < *n) {
		i__1 = *n - *k;
		F77_SYMBOL(dcopy) (&i__1, &qty[kp1], &c__1, &rsd[kp1], &c__1);
	}
	if (!cxb || kp1 > *n) {
		goto L120;
	}
	i__1 = *n;
	for (i = kp1; i <= i__1; ++i) {
		xb[i] = 0.;
	}
L120:
	if (!cr) {
		goto L140;
	}
	i__1 = *k;
	for (i = 1; i <= i__1; ++i) {
		rsd[i] = 0.;
	}
L140:
	if (!cb) {
		goto L190;
	}
	/* compute b. */

	i__1 = *k;
	for (jj = 1; jj <= i__1; ++jj) {
		j = *k - jj + 1;
		if (x[j + j * x_dim1] != 0.) {
			goto L150;
		}
		*info = j;
		/* ......exit */
		goto L180;
L150:
		b[j] /= x[j + j * x_dim1];
		if (j == 1) {
			goto L160;
		}
		t = -b[j];
		i__2 = j - 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &x[j * x_dim1 + 1], &c__1, &b[1], &c__1);
L160:
		;
	}
L180:
L190:
	if (!cr && !cxb) {
		goto L240;
	}
	/* compute rsd or xb as required. */

	i__1 = ju;
	for (jj = 1; jj <= i__1; ++jj) {
		j = ju - jj + 1;
		if (qraux[j] == 0.) {
			goto L220;
		}
		temp = x[j + j * x_dim1];
		x[j + j * x_dim1] = qraux[j];
		if (!cr) {
			goto L200;
		}
		i__2 = *n - j + 1;
		t = -F77_SYMBOL(ddot) (&i__2, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1) / x[j +
								j * x_dim1];
		i__2 = *n - j + 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &x[j + j * x_dim1], &c__1, &rsd[j], &c__1);
L200:
		if (!cxb) {
			goto L210;
		}
		i__2 = *n - j + 1;
		t = -F77_SYMBOL(ddot) (&i__2, &x[j + j * x_dim1], &c__1, &xb[j], &c__1) / x[j + j
								  * x_dim1];
		i__2 = *n - j + 1;
		F77_SYMBOL(daxpy) (&i__2, &t, &x[j + j * x_dim1], &c__1, &xb[j], &c__1);
L210:
		x[j + j * x_dim1] = temp;
L220:
		;
	}
L240:
L250:
	return 0;
}
