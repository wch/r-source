/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-1998 Ross Ihaka
 *  Copyright (C) 1999-2001 R Development Core Team
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *
 *	cpoly finds the zeros of a complex polynomial.
 *
 *	On Entry
 *
 *	opr, opi      -	 double precision vectors of real and
 *			 imaginary parts of the coefficients in
 *			 order of decreasing powers.
 *
 *	degree	      -	 int degree of polynomial.
 *
 *
 *	On Return
 *
 *	zeror, zeroi  -	 output double precision vectors of
 *			 real and imaginary parts of the zeros.
 *
 *	fail	      -	 output int parameter,	true  only if
 *			 leading coefficient is zero or if cpoly
 *			 has found fewer than degree zeros.
 *
 *	The program has been written to reduce the chance of overflow
 *	occurring. If it does occur, there is still a possibility that
 *	the zerofinder will work provided the overflowed quantity is
 *	replaced by a large number.
 *
 *	This is a C translation of the following.
 *
 *	TOMS Algorithm 419
 *	Jenkins and Traub.
 *	Comm. ACM 15 (1972) 97-99.
 *
 *	Ross Ihaka
 *	February 1997
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h> /* for declaration of hypot */

#include <float.h> /* for FLT_RADIX */

#include <Rmath.h> /* for R_pow_di */
#include <R_ext/Applic.h>


#ifndef HAVE_HYPOT
# define hypot pythag
#endif

static void calct(Rboolean *);
static Rboolean fxshft(int, double *, double *);
static Rboolean vrshft(int, double *, double *);
static void nexth(Rboolean);
static void noshft(int);

/* Consider exporting these (via Applic.h): */
static void polyev(int, double, double,
		   double *, double *, double *, double *, double *, double *);
static double errev(int, double *, double *, double, double, double, double);
static double cpoly_cauchy(int, double *, double *);
static double cpoly_scale(int, double *, double, double, double, double);
static void cdivid(double, double, double, double, double *, double *);

/* Global Variables (too many!) */

#define NMAX 50

static int nn;
static double pr[NMAX];
static double pi[NMAX];
static double hr[NMAX];
static double hi[NMAX];
static double qpr[NMAX];
static double qpi[NMAX];
static double qhr[NMAX];
static double qhi[NMAX];
static double shr[NMAX];
static double shi[NMAX];
static double sr, si;
static double tr, ti;
static double pvr, pvi;

static const double eta =  DBL_EPSILON;
static const double are = /* eta = */DBL_EPSILON;
static const double mre = 2. * M_SQRT2 * /* eta, i.e. */DBL_EPSILON;
static const double infin = DBL_MAX;

void R_cpolyroot(double *opr, double *opi, int *degree,
		 double *zeror, double *zeroi, Rboolean *fail)
{
    static const double smalno = DBL_MIN;
    static const double base = (double)FLT_RADIX;
    static int d_n, i, i1, i2;
    static double zi, zr, xx, yy;
    static double bnd, xxx;
    Rboolean conv;
    int d1;
    static const double cosr =/* cos 94 */ -0.06975647374412529990;
    static const double sinr =/* sin 94 */  0.99756405025982424767;
    xx = M_SQRT1_2;/* 1/sqrt(2) = 0.707.... */

    yy = -xx;
    *fail = FALSE;

    nn = *degree;
    d1 = nn - 1;

    /* algorithm fails if the leading coefficient is zero. */

    if (opr[0] == 0. && opi[0] == 0.) {
	*fail = TRUE;
	return;
    }

    /* remove the zeros at the origin if any. */

    while (opr[nn] == 0. && opi[nn] == 0.) {
	d_n = d1-nn+1;
	zeror[d_n] = 0.;
	zeroi[d_n] = 0.;
	nn--;
    }
    nn++;
    /*-- Now, global var.  nn := #{coefficients} = (relevant degree)+1 */

    if (nn == 1) return;

    /* make a copy of the coefficients and shr[] = | p[] | */
    for (i = 0; i < nn; i++) {
	pr[i] = opr[i];
	pi[i] = opi[i];
	shr[i] = hypot(pr[i], pi[i]);
    }

    /* scale the polynomial with factor 'bnd'. */
    bnd = cpoly_scale(nn, shr, eta, infin, smalno, base);
    if (bnd != 1.) {
	for (i=0; i < nn; i++) {
	    pr[i] *= bnd;
	    pi[i] *= bnd;
	}
    }

    /* start the algorithm for one zero */

    while (nn > 2) {

	/* calculate bnd, a lower bound on the modulus of the zeros. */

	for (i=0 ; i < nn ; i++)
	    shr[i] = hypot(pr[i], pi[i]);
	bnd = cpoly_cauchy(nn, shr, shi);

	/* outer loop to control 2 major passes */
	/* with different sequences of shifts */

	for (i1 = 1; i1 <= 2; i1++) {

	    /* first stage calculation, no shift */

	    noshft(5);

	    /*	inner loop to select a shift */
	    for (i2 = 1; i2 <= 9; i2++) {

		/* shift is chosen with modulus bnd */
		/* and amplitude rotated by 94 degrees */
		/* from the previous shift */

		xxx= cosr * xx - sinr * yy;
		yy = sinr * xx + cosr * yy;
		xx = xxx;
		sr = bnd * xx;
		si = bnd * yy;

		/*  second stage calculation, fixed shift */

		conv = fxshft(i2 * 10, &zr, &zi);
		if (conv)
		    goto L10;
	    }
	}

	/* the zerofinder has failed on two major passes */
	/* return empty handed */

	*fail = TRUE;
	return;

	/* the second stage jumps directly to the third stage iteration.
	 * if successful, the zero is stored and the polynomial deflated.
	 */
    L10:
	d_n = d1+2 - nn;
	zeror[d_n] = zr;
	zeroi[d_n] = zi;
	--nn;
	for (i=0; i < nn ; i++) {
	    pr[i] = qpr[i];
	    pi[i] = qpi[i];
	}
    }/*while*/

    /*	calculate the final zero and return */
    cdivid(-pr[1], -pi[1], pr[0], pi[0], &zeror[d1], &zeroi[d1]);
    return;
}


/*  Computes the derivative polynomial as the initial
 *  polynomial and computes l1 no-shift h polynomials.	*/

static void noshft(int l1)
{
    int i, j, jj, n = nn - 1, nm1 = n - 1;

    double t1, t2, xni;

    for (i=0; i < n; i++) {
	xni = (double)(nn - i - 1);
	hr[i] = xni * pr[i] / n;
	hi[i] = xni * pi[i] / n;
    }

    for (jj = 1; jj <= l1; jj++) {

	if (hypot(hr[n-1], hi[n-1]) <=
	    eta * 10.0 * hypot(pr[n-1], pi[n-1])) {
	    /*	If the constant term is essentially zero, */
	    /*	shift h coefficients. */

	    for (i = 1; i <= nm1; i++) {
		j = nn - i;
		hr[j-1] = hr[j-2];
		hi[j-1] = hi[j-2];
	    }
	    hr[0] = 0.;
	    hi[0] = 0.;
	}
	else {
	    cdivid(-pr[nn-1], -pi[nn-1], hr[n-1], hi[n-1], &tr, &ti);
	    for (i = 1; i <= nm1; i++) {
		j = nn - i;
		t1 = hr[j-2];
		t2 = hi[j-2];
		hr[j-1] = tr * t1 - ti * t2 + pr[j-1];
		hi[j-1] = tr * t2 + ti * t1 + pi[j-1];
	    }
	    hr[0] = pr[0];
	    hi[0] = pi[0];
	}
    }
}


/*  Computes l2 fixed-shift h polynomials and tests for convergence.
 *  initiates a variable-shift iteration and returns with the
 *  approximate zero if successful.
 */
static Rboolean fxshft(int l2, double *zr, double *zi)
{
/*  l2	  - limit of fixed shift steps
 *  zr,zi - approximate zero if convergence (result TRUE)
 *
 * Return value indicates convergence of stage 3 iteration
 *
 * Uses global (sr,si), nn, pr[], pi[], .. (all args of polyev() !)
*/

    Rboolean pasd, bool, test;
    static double svsi, svsr;
    static int i, j, n;
    static double oti, otr;

    n = nn - 1;

    /* evaluate p at s. */

    polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);

    test = TRUE;
    pasd = FALSE;

    /* calculate first t = -p(s)/h(s). */

    calct(&bool);

    /* main loop for one second stage step. */

    for (j=1; j<=l2; j++) {

	otr = tr;
	oti = ti;

	/* compute next h polynomial and new t. */

	nexth(bool);
	calct(&bool);
	*zr = sr + tr;
	*zi = si + ti;

	/* test for convergence unless stage 3 has */
	/* failed once or this is the last h polynomial. */

	if (!bool && test && j != l2) {
	    if (hypot(tr - otr, ti - oti) >= hypot(*zr, *zi) * 0.5) {
		pasd = FALSE;
	    }
	    else if (! pasd) {
		pasd = TRUE;
	    }
	    else {

		/* the weak convergence test has been */
		/* passed twice, start the third stage */
		/* iteration, after saving the current */
		/* h polynomial and shift. */

		for (i = 0; i < n; i++) {
		    shr[i] = hr[i];
		    shi[i] = hi[i];
		}
		svsr = sr;
		svsi = si;
		if (vrshft(10, zr, zi)) {
		    return TRUE;
		}

		/* the iteration failed to converge. */
		/* turn off testing and restore */
		/* h, s, pv and t. */

		test = FALSE;
		for (i=1 ; i<=n ; i++) {
		    hr[i-1] = shr[i-1];
		    hi[i-1] = shi[i-1];
		}
		sr = svsr;
		si = svsi;
		polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);
		calct(&bool);
	    }
	}
    }

    /* attempt an iteration with final h polynomial */
    /* from second stage. */

    return(vrshft(10, zr, zi));
}


/* carries out the third stage iteration.
 */
static Rboolean vrshft(int l3, double *zr, double *zi)
{
/*  l3	    - limit of steps in stage 3.
 *  zr,zi   - on entry contains the initial iterate;
 *	      if the iteration converges it contains
 *	      the final iterate on exit.
 * Returns TRUE if iteration converges
 *
 * Assign and uses  GLOBAL sr, si
*/
    Rboolean bool, b;
    static int i, j;
    static double r1, r2, mp, ms, tp, relstp;
    static double omp;

    b = FALSE;
    sr = *zr;
    si = *zi;

    /* main loop for stage three */

    for (i = 1; i <= l3; i++) {

	/* evaluate p at s and test for convergence. */
	polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);

	mp = hypot(pvr, pvi);
	ms = hypot(sr, si);
	if (mp <=  20. * errev(nn, qpr, qpi, ms, mp, /*are=*/eta, mre)) {
	    goto L_conv;
	}

	/* polynomial value is smaller in value than */
	/* a bound on the error in evaluating p, */
	/* terminate the iteration. */

	if (i != 1) {

	    if (!b && mp >= omp && relstp < .05) {

		/* iteration has stalled. probably a */
		/* cluster of zeros. do 5 fixed shift */
		/* steps into the cluster to force */
		/* one zero to dominate. */

		tp = relstp;
		b = TRUE;
		if (relstp < eta)
		    tp = eta;
		r1 = sqrt(tp);
		r2 = sr * (r1 + 1.) - si * r1;
		si = sr * r1 + si * (r1 + 1.);
		sr = r2;
		polyev(nn, sr, si, pr, pi, qpr, qpi, &pvr, &pvi);
		for (j = 1; j <= 5; ++j) {
		    calct(&bool);
		    nexth(bool);
		}
		omp = infin;
		goto L10;
	    }
	    else {

		/* exit if polynomial value */
		/* increases significantly. */

		if (mp * .1 > omp)
		    return FALSE;
	    }
	}
	omp = mp;

	/* calculate next iterate. */

    L10:
	calct(&bool);
	nexth(bool);
	calct(&bool);
	if (!bool) {
	    relstp = hypot(tr, ti) / hypot(sr, si);
	    sr += tr;
	    si += ti;
	}
    }
    return FALSE;

L_conv:
    *zr = sr;
    *zi = si;
    return TRUE;
}

static void calct(Rboolean *bool)
{
    /* computes	 t = -p(s)/h(s).
     * bool   - logical, set true if h(s) is essentially zero.	*/

    int n = nn - 1;
    double hvi, hvr;

    /* evaluate h(s). */
    polyev(n, sr, si, hr, hi,
	   qhr, qhi, &hvr, &hvi);

    *bool = hypot(hvr, hvi) <= are * 10. * hypot(hr[n-1], hi[n-1]);
    if (!*bool) {
	cdivid(-pvr, -pvi, hvr, hvi, &tr, &ti);
    }
    else {
	tr = 0.;
	ti = 0.;
    }
}

static void nexth(Rboolean bool)
{
    /* calculates the next shifted h polynomial.
     * bool :	if TRUE  h(s) is essentially zero
     */
    int j, n = nn - 1;
    double t1, t2;

    if (!bool) {
	for (j=1; j < n; j++) {
	    t1 = qhr[j - 1];
	    t2 = qhi[j - 1];
	    hr[j] = tr * t1 - ti * t2 + qpr[j];
	    hi[j] = tr * t2 + ti * t1 + qpi[j];
	}
	hr[0] = qpr[0];
	hi[0] = qpi[0];
    }
    else {
	/* if h(s) is zero replace h with qh. */

	for (j=1; j < n; j++) {
	    hr[j] = qhr[j-1];
	    hi[j] = qhi[j-1];
	}
	hr[0] = 0.;
	hi[0] = 0.;
    }
}

/*--------------------- Independent Complex Polynomial Utilities ----------*/

static
void polyev(int n,
	    double s_r, double s_i,
	    double *p_r, double *p_i,
	    double *q_r, double *q_i,
	    double *v_r, double *v_i)
{
    /* evaluates a polynomial  p  at  s	 by the horner recurrence
     * placing the partial sums in q and the computed value in v_.
     */
    int i;
    double t;

    q_r[0] = p_r[0];
    q_i[0] = p_i[0];
    *v_r = q_r[0];
    *v_i = q_i[0];
    for (i = 1; i < n; i++) {
	t = *v_r * s_r - *v_i * s_i + p_r[i];
	q_i[i] = *v_i = *v_r * s_i + *v_i * s_r + p_i[i];
	q_r[i] = *v_r = t;
    }
}

static
double errev(int n, double *qr, double *qi,
	     double ms, double mp, double a_re, double m_re)
{
    /*	bounds the error in evaluating the polynomial by the horner
     *	recurrence.
     *
     *	qr,qi	 - the partial sum vectors
     *	ms	 - modulus of the point
     *	mp	 - modulus of polynomial value
     * a_re,m_re - error bounds on complex addition and multiplication
     */
    double e;
    int i;

    e = hypot(qr[0], qi[0]) * m_re / (a_re + m_re);
    for (i=0; i < n; i++)
	e = e*ms + hypot(qr[i], qi[i]);

    return e * (a_re + m_re) - mp * m_re;
}


static
double cpoly_cauchy(int n, double *pot, double *q)
{
    /* Computes a lower bound on the moduli of the zeros of a polynomial
     * pot[1:nn] is the modulus of the coefficients.
     */
    double f, x, delf, dx, xm;
    int i, n1 = n - 1;

    pot[n1] = -pot[n1];

    /* compute upper estimate of bound. */

    x = exp((log(-pot[n1]) - log(pot[0])) / (double) n1);

    /* if newton step at the origin is better, use it. */

    if (pot[n1-1] != 0.) {
	xm = -pot[n1] / pot[n1-1];
	if (xm < x)
	    x = xm;
    }

    /* chop the interval (0,x) unitl f le 0. */

    for(;;) {
	xm = x * 0.1;
	f = pot[0];
	for (i = 1; i < n; i++)
	    f = f * xm + pot[i];
	if (f <= 0.0) {
	    break;
	}
	x = xm;
    }

    dx = x;

    /* do Newton iteration until x converges to two decimal places. */

    while (fabs(dx / x) > 0.005) {
	q[0] = pot[0];
	for(i = 1; i < n; i++)
	    q[i] = q[i-1] * x + pot[i];
	f = q[n1];
	delf = q[0];
	for(i = 1; i < n1; i++)
	    delf = delf * x + q[i];
	dx = f / delf;
	x -= dx;
    }
    return x;
}

static
double cpoly_scale(int n, double *pot,
		   double eps, double BIG, double small, double base)
{
    /* Returns a scale factor to multiply the coefficients of the polynomial.
     * The scaling is done to avoid overflow and to avoid
     *	undetected underflow interfering with the convergence criterion.
     * The factor is a power of the base.

     * pot[1:n] : modulus of coefficients of p
     * eps,BIG,
     * small,base - constants describing the floating point arithmetic.
     */

    int i, ell;
    double x, high, sc, lo, min_, max_;

    /* find largest and smallest moduli of coefficients. */
    high = sqrt(BIG);
    lo = small / eps;
    max_ = 0.;
    min_ = BIG;
    for (i = 0; i < n; i++) {
	x = pot[i];
	if (x > max_) max_ = x;
	if (x != 0. && x < min_)
	    min_ = x;
    }

    /* scale only if there are very large or very small components. */

    if (min_ < lo || max_ > high) {
	x = lo / min_;
	if (x <= 1.)
	    sc = 1. / (sqrt(max_) * sqrt(min_));
	else {
	    sc = x;
	    if (BIG / sc > max_)
		sc = 1.0;
	}
	ell = (int) (log(sc) / log(base) + 0.5);
	return R_pow_di(base, ell);
    }
    else return 1.0;
}


static
void cdivid(double ar, double ai, double br, double bi,
	    double *cr, double *ci)
{
/* complex division c = a/b, i.e., (cr +i*ci) = (ar +i*ai) / (br +i*bi),
   avoiding overflow. */

    double d, r;

    if (br == 0. && bi == 0.) {
	/* division by zero, c = infinity. */
	*cr = *ci = R_PosInf;
    }
    else if (fabs(br) >= fabs(bi)) {
	r = bi / br;
	d = br + r * bi;
	*cr = (ar + ai * r) / d;
	*ci = (ai - ar * r) / d;
    }
    else {
	r = br / bi;
	d = bi + r * br;
	*cr = (ar * r + ai) / d;
	*ci = (ai * r - ar) / d;
    }
}

/* static double cpoly_cmod(double *r, double *i)
 * --> replaced by hypot() everywhere
*/
