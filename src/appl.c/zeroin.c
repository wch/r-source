/*
 *      a zero of the function  f(x)  is computed in the interval ax,bx .
 *
 *  input..
 *
 *  ax     left endpoint of initial interval
 *  bx     right endpoint of initial interval
 *  f      function subprogram which evaluates f(x) for any x in
 *         the interval  ax,bx
 *  tol    desired length of the interval of uncertainty of the
 *         final result ( .ge. 0.0d0)
 *
 *
 *  output..
 *
 *  zeroin abcissa approximating a zero of  f  in the interval ax,bx
 *
 *
 *      it is assumed  that   f(ax)   and   f(bx)   have  opposite  signs
 *  without  a  check.  zeroin  returns a zero  x  in the given interval
 *  ax,bx  to within a tolerance  4*macheps*abs(x) + tol, where macheps
 *  is the relative machine precision.
 *      this function subprogram is a slightly  modified  translation  of
 *  the algol 60 procedure  zero  given in  richard brent, algorithms for
 *  minimization without derivatives, prentice - hall, inc. (1973).
 */


#include "Fortran.h"

double F77_SYMBOL(zeroin) (double *ax, double *bx, double (*f) (),
				   double *tol) {
	double ret_val, d__1;
	double a, b, c, d, e, p, q, r, s, fa, fb, fc, xm, eps, tol1;

	/* compute eps, the relative machine precision */

	eps = 1.;
L10:
	eps /= 2.;
	tol1 = eps + 1.;
	if (tol1 > 1.) {
		goto L10;
	}
	/* initialization */

	a = *ax;
	b = *bx;
	fa = (*f) (&a);
	fb = (*f) (&b);

	/* begin step */

L20:
	c = a;
	fc = fa;
	d = b - a;
	e = d;
L30:
	if (abs(fc) >= abs(fb)) {
		goto L40;
	}
	a = b;
	b = c;
	c = a;
	fa = fb;
	fb = fc;
	fc = fa;

	/* convergence test */

L40:
	tol1 = eps * 2. * abs(b) + *tol * .5;
	xm = (c - b) * .5;
	if (abs(xm) <= tol1) {
		goto L90;
	}
	if (fb == 0.) {
		goto L90;
	}
	/* is bisection necessary */

	if (abs(e) < tol1) {
		goto L70;
	}
	if (abs(fa) <= abs(fb)) {
		goto L70;
	}
	/* is quadratic interpolation possible */

	if (a != c) {
		goto L50;
	}
	/* linear interpolation */

	s = fb / fa;
	p = xm * 2. * s;
	q = 1. - s;
	goto L60;

	/* inverse quadratic interpolation */

L50:
	q = fa / fc;
	r = fb / fc;
	s = fb / fa;
	p = s * (xm * 2. * q * (q - r) - (b - a) * (r - 1.));
	q = (q - 1.) * (r - 1.) * (s - 1.);

	/* adjust signs */

L60:
	if (p > 0.) {
		q = -q;
	}
	p = abs(p);

	/* is interpolation acceptable */

	if (p * 2. >= xm * 3. * q - (d__1 = tol1 * q, abs(d__1))) {
		goto L70;
	}
	if (p >= (d__1 = e * .5 * q, abs(d__1))) {
		goto L70;
	}
	e = d;
	d = p / q;
	goto L80;

	/* bisection */

L70:
	d = xm;
	e = d;

	/* complete step */

L80:
	a = b;
	fa = fb;
	if (abs(d) > tol1) {
		b += d;
	}
	if (abs(d) <= tol1) {
		b += DSIGN(&tol1, &xm);
	}
	fb = (*f) (&b);
	if (fb * (fc / abs(fc)) > 0.) {
		goto L20;
	}
	goto L30;

	/* done */

L90:
	ret_val = b;
	return ret_val;
}

int 
F77_SYMBOL(root1d) (double *ax, double *bx, double (*f) (),
	double *tol, double *root)
{
	extern double F77_SYMBOL(zeroin) ();

	if (*ax * *bx < 0.) {
		*root = F77_SYMBOL(zeroin)(ax, bx, f, tol);
	}
	return 0;
}
