/*
 *  an approximation x to the point where f attains a minimum  on
 *  the interval  (ax,bx)  is determined.
 *
 *  input..
 *
 *  ax    left endpoint of initial interval
 *  bx    right endpoint of initial interval
 *  f     function subprogram which evaluates  f(x)  for any  x
 *        in the interval  (ax,bx)
 *  tol   desired length of the interval of uncertainty of the final
 *        result (.ge.0.)
 *
 *  output..
 *
 *  fmin  abcissa approximating the point where  f  attains a
 *        minimum
 *
 *      the method used is a combination of  golden  section  search  and
 *  successive parabolic interpolation.  convergence is never much slower
 *  than  that  for  a  fibonacci search.  if  f  has a continuous second
 *  derivative which is positive at the minimum (which is not  at  ax  or
 *  bx),  then  convergence  is  superlinear, and usually of the order of
 *  about  1.324....
 *      the function  f  is never evaluated at two points closer together
 *  than  eps*abs(fmin)+(tol/3), where eps is  approximately  the  square
 *  root  of  the  relative  machine  precision.   if   f   is a unimodal
 *  function and the computed values of   f   are  always  unimodal  when
 *  separated  by  at least  eps*abs(x)+(tol/3), then  fmin  approximates
 *  the abcissa of the global minimum of  f  on the interval  ax,bx  with
 *  an error less than  3*eps*abs(fmin)+tol.  if   f   is  not  unimodal,
 *  then fmin may approximate a local, but perhaps non-global, minimum to
 *  the same accuracy.
 *      this function subprogram is a slightly modified  version  of  the
 *  algol  60 procedure  localmin  given in richard brent, algorithms for
 *  minimization without derivatives, prentice-hall, inc. (1973).
 */

#include "Fortran.h"

static int c__4 = 4;

double F77_SYMBOL(fmin) (double *ax, double *bx, double (*f) (),
	      double *tol) {
	double a, b, c, d, e, p, q, r, u, v, w, x;
	double ret_val, d__1;
	double sqrt();
	double t2, fu, fv, fw, fx, xm, eps, tol1, tol3;
	extern double F77_SYMBOL(d1mach) ();


	/* c is the squared inverse of the golden ratio */

	c = (3. - sqrt(5.)) * .5;

	/* eps is approximately the square root of the relative machine */
	/* precision. */

	eps = F77_SYMBOL(d1mach) (&c__4);
	tol1 = eps + 1.;
	eps = sqrt(eps);

	a = *ax;
	b = *bx;
	v = a + c * (b - a);
	w = v;
	x = v;
	e = 0.;
	fx = (*f) (&x);
	fv = fx;
	fw = fx;
	tol3 = *tol / 3.;

	/* main loop starts here */

L20:
	xm = (a + b) * .5;
	tol1 = eps * abs(x) + tol3;
	t2 = tol1 * 2.;

	/* check stopping criterion */

	if ((d__1 = x - xm, abs(d__1)) <= t2 - (b - a) * .5) {
		goto L190;
	}
	p = 0.;
	q = 0.;
	r = 0.;
	if (abs(e) <= tol1) {
		goto L50;
	}
	/* fit parabola */

	r = (x - w) * (fx - fv);
	q = (x - v) * (fx - fw);
	p = (x - v) * q - (x - w) * r;
	q = (q - r) * 2.;
	if (q <= 0.) {
		goto L30;
	}
	p = -p;
	goto L40;
L30:
	q = -q;
L40:
	r = e;
	e = d;
L50:
	if (abs(p) >= (d__1 = q * .5 * r, abs(d__1)) || p <= q * (a - x) || p >=
	    q * (b - x)) {
		goto L60;
	}
	/* a parabolic-interpolation step */

	d = p / q;
	u = x + d;

	/* f must not be evaluated too close to ax or bx */

	if (u - a >= t2 && b - u >= t2) {
		goto L90;
	}
	d = tol1;
	if (x >= xm) {
		d = -d;
	}
	goto L90;

	/* a golden-section step */

L60:
	if (x >= xm) {
		goto L70;
	}
	e = b - x;
	goto L80;
L70:
	e = a - x;
L80:
	d = c * e;

	/* f must not be evaluated too close to x */

L90:
	if (abs(d) < tol1) {
		goto L100;
	}
	u = x + d;
	goto L120;
L100:
	if (d <= 0.) {
		goto L110;
	}
	u = x + tol1;
	goto L120;
L110:
	u = x - tol1;
L120:
	fu = (*f) (&u);

	/* update  a, b, v, w, and x */

	if (fx > fu) {
		goto L140;
	}
	if (u >= x) {
		goto L130;
	}
	a = u;
	goto L140;
L130:
	b = u;
L140:
	if (fu > fx) {
		goto L170;
	}
	if (u >= x) {
		goto L150;
	}
	b = x;
	goto L160;
L150:
	a = x;
L160:
	v = w;
	fv = fw;
	w = x;
	fw = fx;
	x = u;
	fx = fu;
	goto L20;
L170:
	if (fu > fw && w != x) {
		goto L180;
	}
	v = w;
	fv = fw;
	w = u;
	fw = fu;
	goto L20;
L180:
	if (fu > fv && v != x && v != w) {
		goto L20;
	}
	v = u;
	fv = fu;
	goto L20;

	/* end of main loop */

L190:
	ret_val = x;
	return ret_val;
}
