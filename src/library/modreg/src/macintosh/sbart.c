/* sbart.f -- translated by f2c (version 19971204).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

Extern struct {
    doublereal q;
} xxxsbart_;

#define xxxsbart_1 xxxsbart_

/*  sbart() : The cubic spline smoother */
/*  ------- */
/* Calls	 sgram	(sg0,sg1,sg2,sg3,knot,nk) */
/* 	 stxwx	(xs,ys,ws,n,knot,nk,xwy,hs0,hs1,hs2,hs3) */
/* 	 sslvrg (penalt,dofoff,xs,ys,ws,ssw,n,knot,nk,  coef,sz,lev,crit,icrit, */
/* 		 spar,ratio, xwy, hs0,hs1,hs2,hs3, sg0,sg1,sg2,sg3, */
/* 		 abd,p1ip,p2ip,ld4,ldnk,ier) */

/* is itself called from  qsbart() [./qsbart.f]   which has only one work array */

/* Subroutine */ int sbart(doublereal *penalt, doublereal *dofoff, 
	doublereal *xs, doublereal *ys, doublereal *ws, doublereal *ssw, 
	integer *n, doublereal *knot, integer *nk, doublereal *coef, 
	doublereal *sz, doublereal *lev, doublereal *crit, integer *icrit, 
	doublereal *spar, integer *ispar, doublereal *lspar, doublereal *
	uspar, doublereal *tol, integer *isetup, doublereal *xwy, doublereal *
	hs0, doublereal *hs1, doublereal *hs2, doublereal *hs3, doublereal *
	sg0, doublereal *sg1, doublereal *sg2, doublereal *sg3, doublereal *
	abd, doublereal *p1ip, doublereal *p2ip, integer *ld4, integer *ldnk, 
	integer *ier)
{
    /* System generated locals */
    integer abd_dim1, abd_offset, p1ip_dim1, p1ip_offset, p2ip_dim1, 
	    p2ip_offset, i__1;
    doublereal d__1;

    /* Builtin functions */
    double sqrt(doublereal), d_sign(doublereal *, doublereal *);

    /* Local variables */
    static doublereal a, b, c__, d__, e;
    static integer i__;
    static doublereal p, r__, u, v, w, x;
    extern /* Subroutine */ int sgram(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, integer *);
    static doublereal ratio, t1, t2;
    extern /* Subroutine */ int stxwx(doublereal *, doublereal *, doublereal 
	    *, integer *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *);
    static doublereal ax, fu, fv, fw, fx, bx, xm;
    extern /* Subroutine */ int sslvrg(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, integer *, doublereal *, doublereal *, doublereal *
	    , doublereal *, integer *, doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *);
    static doublereal eps, tol1, tol2;

/*     ------------- */
/* A Cubic B-spline Smoothing routine. */

/*  The algorithm minimises: */

/*      (1/n) * sum ws(i)**2 * (ys(i)-sz(i))**2 + lambda* int ( sz"(xs) )**2 dxs */

/*  lambda is a function of the spar which is assumed to be between 0 and 1 */

/* INPUT */
/* ----- */
/* penalt	A penalty > 1 to be used in the gcv criterion */
/*   n		number of data points */
/*  ys(n)	vector of length n containing the observations */
/*  ws(n)	vector containing the weights given to each data point */
/*  xs(n)	vector containing the ordinates of the observations */
/*  nk		number of b-spline coefficients to be estimated */
/* 		nk <= n+2 */
/*  knot(nk+4)	vector of knot points defining the cubic b-spline basis. */
/* 		To obtain full cubic smoothing splines one might */
/* 		have (provided the xs-values are strictly increasing) */
/*  spar		penalised likelihood smoothing parameter */
/*  ispar	indicator saying if spar is supplied or to be estimated */
/*  lspar, uspar lower and upper values for spar;  0.,1. are good values */
/*  tol		used in Golden Search routine */
/*  isetup	setup indicator */
/*  icrit	indicator saying which cross validation score */
/* 		is to be computed */
/*  ld4		the leading dimension of abd (ie ld4=4) */
/*  ldnk		the leading dimension of p2ip (not referenced) */
/* OUTPUT */
/* ------ */
/*   coef(nk)	vector of spline coefficients */
/*   sz(n)	vector of smoothed z-values */
/*   lev(n)	vector of leverages */
/*   crit	either ordinary of generalized CV score */
/*   ier		error indicator */
/* 		ier = 0 ___  everything fine */
/* 		ier = 1 ___  spar too small or too big */
/* 				 problem in cholesky decomposition */
/* Working arrays/matrix */
/*   xwy			X'Wy */
/*   hs0,hs1,hs2,hs3	the diagonals of the X'WX matrix */
/*   sg0,sg1,sg2,sg3	the diagonals of the Gram matrix */
/*   abd(ld4,nk)		[ X'WX + lambda*SIGMA ] in diagonal form */
/*   p1ip(ld4,nk)	inner products between columns of L inverse */
/*   p2ip(ldnk,nk)	all inner products between columns of L inverse */
/* 			where  L'L = [X'WX + lambda*SIGMA]  NOT REFERENCED */
/* Local variables */

/*     unnecessary initialization of d u to keep g77 -Wall happy */

    /* Parameter adjustments */
    --lev;
    --sz;
    --ws;
    --ys;
    --xs;
    --sg3;
    --sg2;
    --sg1;
    --sg0;
    --hs3;
    --hs2;
    --hs1;
    --hs0;
    --xwy;
    --coef;
    --knot;
    p1ip_dim1 = *ld4;
    p1ip_offset = p1ip_dim1 + 1;
    p1ip -= p1ip_offset;
    abd_dim1 = *ld4;
    abd_offset = abd_dim1 + 1;
    abd -= abd_offset;
    p2ip_dim1 = *ldnk;
    p2ip_offset = p2ip_dim1 + 1;
    p2ip -= p2ip_offset;

    /* Function Body */
    d__ = 0.;
    u = 0.;
/*  Compute SIGMA, X' W**2 X, X' W**2 z, trace ratio, s0, s1. */
/*     		SIGMA     -> sg0,sg1,sg2,sg3 */
/*     		X' W**2 X -> hs0,hs1,hs2,hs3 */
/*     		X' W**2 Z -> xwy */
/* trevor fixed this 4/19/88 */
/* Note: sbart uses the square of the weights */
/* the following rectifies that */
    if (*n >= 1) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (ws[i__] > 0.) {
		ws[i__] = sqrt(ws[i__]);
	    }
/* L5: */
	}
    }
    if (*isetup == 0) {
	sgram(&sg0[1], &sg1[1], &sg2[1], &sg3[1], &knot[1], nk);
	stxwx(&xs[1], &ys[1], &ws[1], n, &knot[1], nk, &xwy[1], &hs0[1], &
		hs1[1], &hs2[1], &hs3[1]);
	t1 = 0.f;
	t2 = 0.f;
	i__1 = *nk - 3;
	for (i__ = 3; i__ <= i__1; ++i__) {
	    t1 += hs0[i__];
	    t2 += sg0[i__];
/* L7: */
	}
	ratio = t1 / t2;
	*isetup = 1;
/*     Compute estimate */
    }
    if (*ispar == 1) {
/*     Value of spar supplied */
	sslvrg(penalt, dofoff, &xs[1], &ys[1], &ws[1], ssw, n, &knot[1], nk, 
		&coef[1], &sz[1], &lev[1], crit, icrit, spar, &ratio, &xwy[1],
		 &hs0[1], &hs1[1], &hs2[1], &hs3[1], &sg0[1], &sg1[1], &sg2[1]
		, &sg3[1], &abd[abd_offset], &p1ip[p1ip_offset], &p2ip[
		p2ip_offset], ld4, ldnk, ier);
/*     check 2 */
/* print 2999;2999 format(" got through check 2") */
    } else {
/*     ---- spar not supplied --> compute it ! ------------------------- */
/*     Use Forsythe Malcom and Moler routine to minimise criterion */
	ax = *lspar;
/*     f denotes the value of the criterion */
	bx = *uspar;

/*     an approximation	x  to the point where	f  attains a minimum  on */
/*     the interval  (ax,bx)  is determined. */


/*  input.. */

/*  ax	 left endpoint of initial interval */
/*  bx	 right endpoint of initial interval */
/*  f	 function subprogram which evaluates  f(x)  for any  x */
/* 	 in the interval  (ax,bx) */
/*  tol	 desired length of the interval of uncertainty of the final */
/* 	 result ( .ge. 0.0) */


/*  output.. */

/*  fmin	 abcissa approximating the point where	f  attains a minimum */


/*      the method used is a combination of  golden  section  search  and */
/*  successive parabolic interpolation.	convergence is never much slower */
/*  than	 that  for  a  fibonacci search.  if  f	 has a continuous second */
/*  derivative which is positive at the minimum (which is not  at  ax  or */
/*  bx),	 then  convergence  is	superlinear, and usually of the order of */
/*  about  1.324.... */
/*      the function  f	is never evaluated at two points closer together */
/*  than	 eps*abs(fmin) + (tol/3), where eps is	approximately the square */
/*  root	 of  the  relative  machine  precision.	  if   f   is a unimodal */
/*  function and the computed values of	 f   are  always  unimodal  when */
/*  separated by at least  eps*abs(x) + (tol/3), then  fmin  approximates */
/*  the abcissa of the global minimum of	 f  on the interval  ax,bx  with */
/*  an error less than  3*eps*abs(fmin) + tol.  if   f	is not unimodal, */
/*  then fmin may approximate a local, but perhaps non-global, minimum to */
/*  the same accuracy. */
/*      this function subprogram is a slightly modified	version	 of  the */
/*  algol  60 procedure	localmin  given in richard brent, algorithms for */
/*  minimization without derivatives, prentice - hall, inc. (1973). */


/*      DOUBLE precision	 a,b,c,d,e,eps,xm,p,q,r,tol1,tol2,u,v,w */
/*      DOUBLE precision	 fu,fv,fw,fx,x */

/*  c is the squared inverse of the golden ratio */

	c__ = (3.f - sqrt(5.)) * .5f;

/*  eps is approximately the square root of the relative machine */
/*  precision. */

/* -       eps = 1e0 */
/* - 10    eps = eps/2e0 */
/* -       tol1 = 1e0 + eps */
/* -       if (tol1 .gt. 1e0) go to 10 */
/* -       eps = sqrt(eps) */
	eps = 2.44e-4f;

/*  initialization */

	a = ax;
	b = bx;
	v = a + c__ * (b - a);
	w = v;
	x = v;
	e = 0.f;
	*spar = x;
	sslvrg(penalt, dofoff, &xs[1], &ys[1], &ws[1], ssw, n, &knot[1], nk, 
		&coef[1], &sz[1], &lev[1], crit, icrit, spar, &ratio, &xwy[1],
		 &hs0[1], &hs1[1], &hs2[1], &hs3[1], &sg0[1], &sg1[1], &sg2[1]
		, &sg3[1], &abd[abd_offset], &p1ip[p1ip_offset], &p2ip[
		p2ip_offset], ld4, ldnk, ier);
	fx = *crit;
	fv = fx;
	fw = fx;

/*  main loop starts here */

L20:
	xm = (a + b) * .5f;
	tol1 = eps * abs(x) + *tol / 3.;
	tol2 = tol1 * 2.;

/*  check stopping criterion */

	if ((d__1 = x - xm, abs(d__1)) <= tol2 - (b - a) * .5f) {
	    goto L990;
	}

/* is golden-section necessary */

	if (abs(e) <= tol1) {
	    goto L40;
	}

/*  fit parabola */

	r__ = (x - w) * (fx - fv);
	xxxsbart_1.q = (x - v) * (fx - fw);
	p = (x - v) * xxxsbart_1.q - (x - w) * r__;
	xxxsbart_1.q = (xxxsbart_1.q - r__) * 2.f;
	if (xxxsbart_1.q > 0.) {
	    p = -p;
	}
	xxxsbart_1.q = abs(xxxsbart_1.q);
	r__ = e;
	e = d__;

/*  is parabola acceptable?  Otherwise do golden-section */

	if (abs(p) >= (d__1 = xxxsbart_1.q * .5f * r__, abs(d__1))) {
	    goto L40;
	}
	if (xxxsbart_1.q == 0.) {
	    goto L40;
	}
/*     above line added by BDR; [the abs(.) >= abs() = 0 should have branched..] */
/*     COMMON above ensures q is NOT a register variable */
	if (p <= xxxsbart_1.q * (a - x)) {
	    goto L40;
	}
	if (p >= xxxsbart_1.q * (b - x)) {
	    goto L40;
	}

/*  a parabolic interpolation step */

	d__ = p / xxxsbart_1.q;
	u = x + d__;

/*  f must not be evaluated too close to ax or bx */

	if (u - a < tol2) {
	    d__1 = xm - x;
	    d__ = d_sign(&tol1, &d__1);
	}
	if (b - u < tol2) {
	    d__1 = xm - x;
	    d__ = d_sign(&tol1, &d__1);
	}
	goto L50;
/*     -------- */

/*  a golden-section step */

L40:
	if (x >= xm) {
	    e = a - x;
	}
	if (x < xm) {
	    e = b - x;
	}
	d__ = c__ * e;

/*  f must not be evaluated too close to x */

L50:
	if (abs(d__) >= tol1) {
	    u = x + d__;
	}
	if (abs(d__) < tol1) {
	    u = x + d_sign(&tol1, &d__);
	}
	*spar = u;
	sslvrg(penalt, dofoff, &xs[1], &ys[1], &ws[1], ssw, n, &knot[1], nk, 
		&coef[1], &sz[1], &lev[1], crit, icrit, spar, &ratio, &xwy[1],
		 &hs0[1], &hs1[1], &hs2[1], &hs3[1], &sg0[1], &sg1[1], &sg2[1]
		, &sg3[1], &abd[abd_offset], &p1ip[p1ip_offset], &p2ip[
		p2ip_offset], ld4, ldnk, ier);
	fu = *crit;

/*  update  a, b, v, w, and x */

	if (fu > fx) {
	    goto L60;
	}
	if (u >= x) {
	    a = x;
	}
	if (u < x) {
	    b = x;
	}
	v = w;
	fv = fw;
	w = x;
	fw = fx;
	x = u;
	fx = fu;
	goto L20;
L60:
	if (u < x) {
	    a = u;
	} else {
	    b = u;
	}
	if (fu <= fw) {
	    goto L70;
	}
	if (w == x) {
	    goto L70;
	}
	if (fu <= fv) {
	    goto L80;
	}
	if (v == x) {
	    goto L80;
	}
	if (v == w) {
	    goto L80;
	}
	goto L20;
L70:
	v = w;
	fv = fw;
	w = u;
	fw = fu;
	goto L20;
L80:
	v = u;
	fv = fu;
	goto L20;

/*  end of main loop */

L990:
	*spar = x;
	*crit = fx;
    }
    return 0;
} /* sbart_ */
