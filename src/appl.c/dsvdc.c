/*
 *     dsvdc is a subroutine to reduce a double precision nxp matrix x
 *     by orthogonal transformations u and v to diagonal form.  the
 *     diagonal elements s(i) are the singular values of x.  the
 *     columns of u are the corresponding left singular vectors,
 *     and the columns of v the right singular vectors.
 *
 *     on entry
 *
 *         x         double precision(ldx,p), where ldx.ge.n.
 *                   x contains the matrix whose singular value
 *                   decomposition is to be computed.  x is
 *                   destroyed by dsvdc.
 *
 *         ldx       int.
 *                   ldx is the leading dimension of the array x.
 *
 *         n         int.
 *                   n is the number of rows of the matrix x.
 *
 *         p         int.
 *                   p is the number of columns of the matrix x.
 *
 *         ldu       int.
 *                   ldu is the leading dimension of the array u.
 *                   (see below).
 *
 *         ldv       int.
 *                   ldv is the leading dimension of the array v.
 *                   (see below).
 *
 *         work      double precision(n).
 *                   work is a scratch array.
 *
 *         job       int.
 *                   job controls the computation of the singular
 *                   vectors.  it has the decimal expansion ab
 *                   with the following meaning
 *
 *                        a.eq.0    do not compute the left singular
 *                                  vectors.
 *                        a.eq.1    return the n left singular vectors
 *                                  in u.
 *                        a.ge.2    return the first min(n,p) singular
 *                                  vectors in u.
 *                        b.eq.0    do not compute the right singular
 *                                  vectors.
 *                        b.eq.1    return the right singular vectors
 *                                  in v.
 *
 *     on return
 *
 *         s         double precision(mm), where mm=min(n+1,p).
 *                   the first min(n,p) entries of s contain the
 *                   singular values of x arranged in descending
 *                   order of magnitude.
 *
 *         e         double precision(p),
 *                   e ordinarily contains zeros.  however see the
 *                   discussion of info for exceptions.
 *
 *         u         double precision(ldu,k), where ldu.ge.n.  if
 *                                   joba.eq.1 then k.eq.n, if joba.ge.2
 *                                   then k.eq.min(n,p).
 *                   u contains the matrix of left singular vectors.
 *                   u is not referenced if joba.eq.0.  if n.le.p
 *                   or if joba.eq.2, then u may be identified with x
 *                   in the subroutine call.
 *
 *         v         double precision(ldv,p), where ldv.ge.p.
 *                   v contains the matrix of right singular vectors.
 *                   v is not referenced if job.eq.0.  if p.le.n,
 *                   then v may be identified with x in the
 *                   subroutine call.
 *
 *         info      int.
 *                   the singular values (and their corresponding
 *                   singular vectors) s(info+1),s(info+2),...,s(m)
 *                   are correct (here m=min(n,p)).  thus if
 *                   info.eq.0, all the singular values and their
 *                   vectors are correct.  in any event, the matrix
 *                   b = trans(u)*x*v is the bidiagonal matrix
 *                   with the elements of s on its diagonal and the
 *                   elements of e on its super-diagonal (trans(u)
 *                   is the transpose of u).  thus the singular
 *                   values of x and b are the same.
 *
 *     linpack. this version dated 08/14/78 .
 *              correction made to shift 2/84.
 *     g.w. stewart, university of maryland, argonne national lab.
 *
 *     dsvdc uses the following functions and subprograms.
 *
 *     external drot
 *     blas daxpy, ddot, dscal, dswap, dnrm2, drotg
 *     fortran dabs, dmax1, max0, min0, mod, dsqrt
 */

#include "Fortran.h"
#include "Blas.h"
#include "Linpack.h"

static int c__1 = 1;
static double c_b44 = -1.;

int 
F77_SYMBOL(dsvdc) (double *x, int *ldx, int *n, int *
		   p, double *s, double *e, double *u, int *ldu,
		   double *v, int *ldv, double *work, int *job, int *
		   info)
{
	double b, c;
	double cs;
	double d__1, d__2, d__3, d__4, d__5, d__6, d__7;
	double emm1, smm1;
	double f, g;
	double shift;
	double sl;
	double sm, sn;
	double t, scale;
	double t1, ztest, el;
	double test;
/*
	extern double F77_SYMBOL(ddot) ();
	extern double F77_SYMBOL(dnrm2) ();
	extern int F77_SYMBOL(daxpy) ();
	extern int F77_SYMBOL(drot) ();
	extern int F77_SYMBOL(dscal) ();
	extern int F77_SYMBOL(dswap) ();
	extern int F77_SYMBOL(drotg) ();
*/
	int i, j, k, l, m;
	int jobu, iter;
	int kase;
	int kk;
	int ll, mm, ls;
	int lm1, mm1, lp1, mp1, nct, ncu, lls, nrt;
	int lu;
	int maxit;
	int nctp1;
	int nrtp1;
	int wantu, wantv;
	int x_dim1, x_offset, u_dim1, u_offset, v_dim1, v_offset, i__1,
	 i__2, i__3;

	x_dim1 = *ldx;
	x_offset = x_dim1 + 1;
	x -= x_offset;
	--s;
	--e;
	u_dim1 = *ldu;
	u_offset = u_dim1 + 1;
	u -= u_offset;
	v_dim1 = *ldv;
	v_offset = v_dim1 + 1;
	v -= v_offset;
	--work;

	/* set the maximum number of iterations. */

	maxit = 30;

	/* determine what is to be computed. */

	wantu = FALSE;
	wantv = FALSE;
	jobu = *job % 100 / 10;
	ncu = *n;
	if (jobu > 1) {
		ncu = min(*n, *p);
	}
	if (jobu != 0) {
		wantu = TRUE;
	}
	if (*job % 10 != 0) {
		wantv = TRUE;
	}
	/* reduce x to bidiagonal form, storing the diagonal elements */
	/* in s and the super-diagonal elements in e. */

	*info = 0;
	i__1 = *n - 1;
	nct = min(i__1, *p);
	i__3 = *p - 2;
	i__1 = 0, i__2 = min(i__3, *n);
	nrt = max(i__1, i__2);
	lu = max(nct, nrt);
	if (lu < 1) {
		goto L170;
	}
	i__1 = lu;
	for (l = 1; l <= i__1; ++l) {
		lp1 = l + 1;
		if (l > nct) {
			goto L20;
		}
		/* compute the transformation for the l-th column and */
		/* place the l-th diagonal in s(l). */

		i__2 = *n - l + 1;
		s[l] = F77_SYMBOL(dnrm2) (&i__2, &x[l + l * x_dim1], &c__1);
		if (s[l] == 0.) {
			goto L10;
		}
		if (x[l + l * x_dim1] != 0.) {
			s[l] = DSIGN(&s[l], &x[l + l * x_dim1]);
		}
		i__2 = *n - l + 1;
		d__1 = 1. / s[l];
		F77_SYMBOL(dscal) (&i__2, &d__1, &x[l + l * x_dim1], &c__1);
		x[l + l * x_dim1] += 1.;
L10:
		s[l] = -s[l];
L20:
		if (*p < lp1) {
			goto L50;
		}
		i__2 = *p;
		for (j = lp1; j <= i__2; ++j) {
			if (l > nct) {
				goto L30;
			}
			if (s[l] == 0.) {
				goto L30;
			}
			/* apply the transformation. */

			i__3 = *n - l + 1;
			t = -F77_SYMBOL(ddot) (&i__3, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
					       c__1) / x[l + l * x_dim1];
			i__3 = *n - l + 1;
			F77_SYMBOL(daxpy) (&i__3, &t, &x[l + l * x_dim1], &c__1, &x[l + j * x_dim1], &
					   c__1);
	L30:

			/* place the l-th row of x into  e for the */
			/* subsequent calculation of the row transformation. */

			e[j] = x[l + j * x_dim1];
		}
L50:
		if (!wantu || l > nct) {
			goto L70;
		}
		/* place the transformation in u for subsequent back */
		/* multiplication. */

		i__2 = *n;
		for (i = l; i <= i__2; ++i) {
			u[i + l * u_dim1] = x[i + l * x_dim1];
		}
L70:
		if (l > nrt) {
			goto L150;
		}
		/* compute the l-th row transformation and place the */
		/* l-th super-diagonal in e(l). */

		i__2 = *p - l;
		e[l] = F77_SYMBOL(dnrm2) (&i__2, &e[lp1], &c__1);
		if (e[l] == 0.) {
			goto L80;
		}
		if (e[lp1] != 0.) {
			e[l] = DSIGN(&e[l], &e[lp1]);
		}
		i__2 = *p - l;
		d__1 = 1. / e[l];
		F77_SYMBOL(dscal) (&i__2, &d__1, &e[lp1], &c__1);
		e[lp1] += 1.;
L80:
		e[l] = -e[l];
		if (lp1 > *n || e[l] == 0.) {
			goto L120;
		}
		/* apply the transformation. */

		i__2 = *n;
		for (i = lp1; i <= i__2; ++i) {
			work[i] = 0.;
		}
		i__2 = *p;
		for (j = lp1; j <= i__2; ++j) {
			i__3 = *n - l;
			F77_SYMBOL(daxpy) (&i__3, &e[j], &x[lp1 + j * x_dim1], &c__1, &work[lp1], &
					   c__1);
		}
		i__2 = *p;
		for (j = lp1; j <= i__2; ++j) {
			i__3 = *n - l;
			d__1 = -e[j] / e[lp1];
			F77_SYMBOL(daxpy) (&i__3, &d__1, &work[lp1], &c__1, &x[lp1 + j * x_dim1], &
					   c__1);
		}
L120:
		if (!wantv) {
			goto L140;
		}
		/* place the transformation in v for subsequent */
		/* back multiplication. */

		i__2 = *p;
		for (i = lp1; i <= i__2; ++i) {
			v[i + l * v_dim1] = e[i];
		}
L140:
L150:
		;
	}
L170:

	/* set up the final bidiagonal matrix or order m. */

	i__1 = *p, i__2 = *n + 1;
	m = min(i__1, i__2);
	nctp1 = nct + 1;
	nrtp1 = nrt + 1;
	if (nct < *p) {
		s[nctp1] = x[nctp1 + nctp1 * x_dim1];
	}
	if (*n < m) {
		s[m] = 0.;
	}
	if (nrtp1 < m) {
		e[nrtp1] = x[nrtp1 + m * x_dim1];
	}
	e[m] = 0.;

	/* if required, generate u. */

	if (!wantu) {
		goto L300;
	}
	if (ncu < nctp1) {
		goto L200;
	}
	i__1 = ncu;
	for (j = nctp1; j <= i__1; ++j) {
		i__2 = *n;
		for (i = 1; i <= i__2; ++i) {
			u[i + j * u_dim1] = 0.;
		}
		u[j + j * u_dim1] = 1.;
	}
L200:
	if (nct < 1) {
		goto L290;
	}
	i__1 = nct;
	for (ll = 1; ll <= i__1; ++ll) {
		l = nct - ll + 1;
		if (s[l] == 0.) {
			goto L250;
		}
		lp1 = l + 1;
		if (ncu < lp1) {
			goto L220;
		}
		i__2 = ncu;
		for (j = lp1; j <= i__2; ++j) {
			i__3 = *n - l + 1;
			t = -F77_SYMBOL(ddot) (&i__3, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
					       c__1) / u[l + l * u_dim1];
			i__3 = *n - l + 1;
			F77_SYMBOL(daxpy) (&i__3, &t, &u[l + l * u_dim1], &c__1, &u[l + j * u_dim1], &
					   c__1);
		}
L220:
		i__2 = *n - l + 1;
		F77_SYMBOL(dscal) (&i__2, &c_b44, &u[l + l * u_dim1], &c__1);
		u[l + l * u_dim1] += 1.;
		lm1 = l - 1;
		if (lm1 < 1) {
			goto L240;
		}
		i__2 = lm1;
		for (i = 1; i <= i__2; ++i) {
			u[i + l * u_dim1] = 0.;
		}
L240:
		goto L270;
L250:
		i__2 = *n;
		for (i = 1; i <= i__2; ++i) {
			u[i + l * u_dim1] = 0.;
		}
		u[l + l * u_dim1] = 1.;
L270:
		;
	}
L290:
L300:

	/* if it is required, generate v. */

	if (!wantv) {
		goto L350;
	}
	i__1 = *p;
	for (ll = 1; ll <= i__1; ++ll) {
		l = *p - ll + 1;
		lp1 = l + 1;
		if (l > nrt) {
			goto L320;
		}
		if (e[l] == 0.) {
			goto L320;
		}
		i__2 = *p;
		for (j = lp1; j <= i__2; ++j) {
			i__3 = *p - l;
			t = -F77_SYMBOL(ddot) (&i__3, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j *
				      v_dim1], &c__1) / v[lp1 + l * v_dim1];
			i__3 = *p - l;
			F77_SYMBOL(daxpy) (&i__3, &t, &v[lp1 + l * v_dim1], &c__1, &v[lp1 + j *
							    v_dim1], &c__1);
		}
L320:
		i__2 = *p;
		for (i = 1; i <= i__2; ++i) {
			v[i + l * v_dim1] = 0.;
		}
		v[l + l * v_dim1] = 1.;
	}
L350:

	/* main iteration loop for the singular values. */

	mm = m;
	iter = 0;
L360:

	/* quit if all the singular values have been found. */

	/* ...exit */

	if (m == 0) {
		goto L620;
	}
	/* if too many iterations have been performed, set */
	/* flag and return. */

	if (iter < maxit) {
		goto L370;
	}
	*info = m;

	/* ......exit */

	goto L620;
L370:

	/* this section of the program inspects for */
	/* negligible elements in the s and e arrays.  on */
	/* completion the variables kase and l are set as follows. */

	/* kase = 1     if s(m) and e(l-1) are negligible and l.lt.m */
	/* kase = 2     if s(l) is negligible and l.lt.m */
	/* kase = 3     if e(l-1) is negligible, l.lt.m, and */
	/* s(l), ..., s(m) are not negligible (qr step). */
	/* kase = 4     if e(m-1) is negligible (convergence). */

	i__1 = m;
	for (ll = 1; ll <= i__1; ++ll) {
		l = m - ll;
		/* ...exit */
		if (l == 0) {
			goto L400;
		}
		test = (d__1 = s[l], abs(d__1)) + (d__2 = s[l + 1], abs(d__2));
		ztest = test + (d__1 = e[l], abs(d__1));
		if (ztest != test) {
			goto L380;
		}
		e[l] = 0.;
		/* ......exit */
		goto L400;
L380:
		;
	}
L400:
	if (l != m - 1) {
		goto L410;
	}
	kase = 4;
	goto L480;
L410:
	lp1 = l + 1;
	mp1 = m + 1;
	i__1 = mp1;
	for (lls = lp1; lls <= i__1; ++lls) {
		ls = m - lls + lp1;
		/* ...exit */
		if (ls == l) {
			goto L440;
		}
		test = 0.;
		if (ls != m) {
			test += (d__1 = e[ls], abs(d__1));
		}
		if (ls != l + 1) {
			test += (d__1 = e[ls - 1], abs(d__1));
		}
		ztest = test + (d__1 = s[ls], abs(d__1));
		if (ztest != test) {
			goto L420;
		}
		s[ls] = 0.;
		/* ......exit */
		goto L440;
L420:
		;
	}
L440:
	if (ls != l) {
		goto L450;
	}
	kase = 3;
	goto L470;
L450:
	if (ls != m) {
		goto L460;
	}
	kase = 1;
	goto L470;
L460:
	kase = 2;
	l = ls;
L470:
L480:
	++l;

	/* perform the task indicated by kase. */

	switch (kase) {
	case 1:
		goto L490;
	case 2:
		goto L520;
	case 3:
		goto L540;
	case 4:
		goto L570;
	}

	/* deflate negligible s(m). */

L490:
	mm1 = m - 1;
	f = e[m - 1];
	e[m - 1] = 0.;
	i__1 = mm1;
	for (kk = l; kk <= i__1; ++kk) {
		k = mm1 - kk + l;
		t1 = s[k];
		F77_SYMBOL(drotg) (&t1, &f, &cs, &sn);
		s[k] = t1;
		if (k == l) {
			goto L500;
		}
		f = -sn * e[k - 1];
		e[k - 1] = cs * e[k - 1];
L500:
		if (wantv) {
			F77_SYMBOL(drot) (p, &v[k * v_dim1 + 1], &c__1, &v[m * v_dim1 + 1], &c__1, &
					  cs, &sn);
		}
	}
	goto L610;

	/* split at negligible s(l). */

L520:
	f = e[l - 1];
	e[l - 1] = 0.;
	i__1 = m;
	for (k = l; k <= i__1; ++k) {
		t1 = s[k];
		F77_SYMBOL(drotg) (&t1, &f, &cs, &sn);
		s[k] = t1;
		f = -sn * e[k];
		e[k] = cs * e[k];
		if (wantu) {
			F77_SYMBOL(drot) (n, &u[k * u_dim1 + 1], &c__1, &u[(l - 1) * u_dim1 + 1], &
					  c__1, &cs, &sn);
		}
	}
	goto L610;

	/* perform one qr step. */

L540:

	/* calculate the shift. */

	d__6 = (d__1 = s[m], abs(d__1)), d__7 = (d__2 = s[m - 1], abs(d__2)),
		d__6 = max(d__6, d__7), d__7 = (d__3 = e[m - 1], abs(d__3)), d__6 =
		max(d__6, d__7), d__7 = (d__4 = s[l], abs(d__4)), d__6 = max(d__6,
				     d__7), d__7 = (d__5 = e[l], abs(d__5));
	scale = max(d__6, d__7);
	sm = s[m] / scale;
	smm1 = s[m - 1] / scale;
	emm1 = e[m - 1] / scale;
	sl = s[l] / scale;
	el = e[l] / scale;
	d__1 = emm1;
	b = ((smm1 + sm) * (smm1 - sm) + d__1 * d__1) / 2.;
	d__1 = sm * emm1;
	c = d__1 * d__1;
	shift = 0.;
	if (b == 0. && c == 0.) {
		goto L550;
	}
	d__1 = b;
	shift = sqrt(d__1 * d__1 + c);
	if (b < 0.) {
		shift = -shift;
	}
	shift = c / (b + shift);
L550:
	f = (sl + sm) * (sl - sm) + shift;
	g = sl * el;

	/* chase zeros. */

	mm1 = m - 1;
	i__1 = mm1;
	for (k = l; k <= i__1; ++k) {
		F77_SYMBOL(drotg) (&f, &g, &cs, &sn);
		if (k != l) {
			e[k - 1] = f;
		}
		f = cs * s[k] + sn * e[k];
		e[k] = cs * e[k] - sn * s[k];
		g = sn * s[k + 1];
		s[k + 1] = cs * s[k + 1];
		if (wantv) {
			F77_SYMBOL(drot) (p, &v[k * v_dim1 + 1], &c__1, &v[(k + 1) * v_dim1 + 1], &
					  c__1, &cs, &sn);
		}
		F77_SYMBOL(drotg) (&f, &g, &cs, &sn);
		s[k] = f;
		f = cs * e[k] + sn * s[k + 1];
		s[k + 1] = -sn * e[k] + cs * s[k + 1];
		g = sn * e[k + 1];
		e[k + 1] = cs * e[k + 1];
		if (wantu && k < *n) {
			F77_SYMBOL(drot) (n, &u[k * u_dim1 + 1], &c__1, &u[(k + 1) * u_dim1 + 1], &
					  c__1, &cs, &sn);
		}
	}
	e[m - 1] = f;
	++iter;
	goto L610;

	/* convergence. */

L570:

	/* make the singular value  positive. */

	if (s[l] >= 0.) {
		goto L580;
	}
	s[l] = -s[l];
	if (wantv) {
		F77_SYMBOL(dscal) (p, &c_b44, &v[l * v_dim1 + 1], &c__1);
	}
L580:

	/* order the singular value. */

L590:
	if (l == mm) {
		goto L600;
	}
	/* ...exit */
	if (s[l] >= s[l + 1]) {
		goto L600;
	}
	t = s[l];
	s[l] = s[l + 1];
	s[l + 1] = t;
	if (wantv && l < *p) {
		F77_SYMBOL(dswap) (p, &v[l * v_dim1 + 1], &c__1, &v[(l + 1) * v_dim1 + 1], &c__1);
	}
	if (wantu && l < *n) {
		F77_SYMBOL(dswap) (n, &u[l * u_dim1 + 1], &c__1, &u[(l + 1) * u_dim1 + 1], &c__1);
	}
	++l;
	goto L590;
L600:
	iter = 0;
	--m;
L610:
	goto L360;
L620:
	return 0;
}
