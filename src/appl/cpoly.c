/*
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

#include <float.h>
#include "Fortran.h"
#include "Arith.h"
#include "Applic.h"

static void calct(int *);
static void fxshft(int *, double *, double *, int *);
static void vrshft(int, double *, double *, int *);
static void nexth(int *);
static void noshft(int);

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
static double are;
static double mre;
static double eta;
static double infin;


int F77_SYMBOL(cpoly)(double *opr, double *opi, int *degree,
		      double *zeror, double *zeroi, int *fail)
{
    int i__1;
    double d__1, d__2;
    static double base = (double)FLT_RADIX;
    static double cosr, sinr;
    static int conv, d_n, i, i1, i2;
    static double zi, zr, xx, yy, smalno;
    static double bnd, xxx;
    int d1;

    eta = DBL_EPSILON;
    infin = DBL_MAX;
    smalno = DBL_MIN;

    are = eta;
    mre = sqrt(2.0) * 2.0 * eta;

    /* We use the originals so we get exact */
    /* agreement with the original, but ... */
    /*	cos 94 =   -0.06975647374412529990   */
    /*	sin 94 =    0.99756405025982424767   */
    /*	1/sqrt(2) = 0.70710678118654752440   */

    cosr = (float)-.060756474;
    sinr = (float).99756405;
    xx = (float).70710678;
    yy = -xx;
    *fail = FALSE;

    nn = *degree;
    d1 = nn - 1;

    /* algorithm fails if the leading coefficient is zero. */

    if (opr[0] == 0.0 && opi[0] == 0.0) {
	*fail = TRUE;
	return 0;
    }

    /* remove the zeros at the origin if any. */

    while (opr[nn] == 0.0 && opi[nn] == 0.0) {
	d_n = d1-nn+1;
	zeror[d_n] = 0.0;
	zeroi[d_n] = 0.0;
	nn--;
    }
    nn++;
    /*-- Now, global var.  nn := #{coefficients} = (relevant degree)+1 */

    /* make a copy of the coefficients and shr[] = | p[] | */
    for (i = 0; i<nn ; i++) {
	pr[i] = opr[i];
	pi[i] = opi[i];
	shr[i] = hypot(pr[i], pi[i]);
    }

    /* scale the polynomial with factor 'bnd'. */

    scale(&nn, shr,
	  &eta, &infin, &smalno, &base,
	  &bnd);

    if (bnd != 1.0) {
	for (i=0; i<nn; i++) {
	    pr[i] *= bnd;
	    pi[i] *= bnd;
	}
    }

    /* start the algorithm for one zero */

    while (nn > 2) {

	/* calculate bnd, a lower bound on the modulus of the zeros. */

	for (i=0 ; i < nn ; i++)
	    shr[i] = cmod(&pr[i], &pi[i]);

	bnd = cauchy(&nn, shr, shi);

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

		i__1 = i2 * 10;
		fxshft(&i__1, &zr, &zi, &conv);

		if (conv)
		    goto L10;
	    }
	}

	/* the zerofinder has failed on two major passes */
	/* return empty handed */

	*fail = TRUE;
	return 0;

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

    d__1 = -pr[1];
    d__2 = -pi[1];
    cdivid(&d__1, &d__2, pr, pi, &zeror[d1], &zeroi[d1]);
    return 0;
}


/*  Computes the derivative polynomial as the initial
 *  polynomial and computes l1 no-shift h polynomials.	*/

static void noshft(int l1)
{
    double d__1, d__2;
    static int i, j, n;
    static double t1, t2;
    static int jj;
    static int nm1;
    static double xni;

    n = nn - 1;
    nm1 = n - 1;

    for (i=1; i<=n; i++) {
	xni = (double)(nn - i);
	hr[i-1] = xni * pr[i-1] / (double) n;
	hi[i-1] = xni * pi[i-1] / (double) n;
    }

    for (jj = 1; jj <= l1; jj++) {

	if (cmod(&hr[n-1], &hi[n-1]) <= eta * 10.0 * cmod(&pr[n-1], &pi[n-1])) {

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
	    d__1 = -pr[nn-1];
	    d__2 = -pi[nn-1];
	    cdivid(&d__1, &d__2, &hr[n-1], &hi[n-1], &tr, &ti);
	    for (i=1; i<=nm1; i++) {
		j = nn - i;
		t1 = hr[j-2];
		t2 = hi[j-2];
		hr[j-1] = tr * t1 - ti * t2 +
		    pr[j-1];
		hi[j-1] = tr * t2 + ti * t1 +
		    pi[j-1];
	    }
	    hr[0] = pr[0];
	    hi[0] = pi[0];
	}
    }
}


/*  Computes l2 fixed-shift h polynomials and tests for convergence.
 *  initiates a variable-shift iteration and returns with the
 *  approximate zero if successful.
 *
 *  l2	  - limit of fixed shift steps
 *  zr,zi - approximate zero if conv is .true.
 *  conv  - int indicating convergence of stage 3 iteration  */

static void fxshft(int *l2, double *zr, double *zi, int *conv)
{
    double d__1, d__2;
    static int pasd, bool, test;
    static double svsi, svsr;
    static int i, j, n;
    static double oti, otr;

    n = nn - 1;

    /* evaluate p at s. */

    polyev(&nn, &sr, &si,
	   pr, pi, qpr, qpi, &pvr, &pvi);

    test = TRUE;
    pasd = FALSE;

    /* calculate first t = -p(s)/h(s). */

    calct(&bool);

    /* main loop for one second stage step. */

    for (j=1; j<=*l2; j++) {

	otr = tr;
	oti = ti;

	/* compute next h polynomial and new t. */

	nexth(&bool);
	calct(&bool);
	*zr = sr + tr;
	*zi = si + ti;

	/* test for convergence unless stage 3 has */
	/* failed once or this is the last h polynomial. */

	if (!bool && test && j != *l2) {
	    d__1 = tr - otr;
	    d__2 = ti - oti;
	    if (cmod(&d__1, &d__2) >= cmod(zr, zi) * 0.5) {
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

		for (i=1; i<=n ; i++) {
		    shr[i-1] = hr[i-1];
		    shi[i-1] = hi[i-1];
		}
		svsr = sr;
		svsi = si;
		vrshft(10, zr, zi, conv);
		if (*conv) {
		    return;
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
		polyev(&nn, &sr, &si, pr, pi, qpr, qpi, &pvr, &pvi);
		calct(&bool);
	    }
	}
    }

    /* attempt an iteration with final h polynomial */
    /* from second stage. */

    vrshft(10, zr, zi, conv);
}


/*  carries out the third stage iteration.
 *  l3	    - limit of steps in stage 3.
 *  zr,zi   - on entry contains the initial iterate, if the
 *	      iteration converges it contains the final iterate
 *	      on exit.
 *  conv    - .true. if iteration converges  */

static void vrshft(int l3, double *zr, double *zi, int *conv)
{
    static int bool, b;
    static int i, j;
    static double r1, r2, mp, ms, tp, relstp;
    static double omp;

    *conv = FALSE;
    b = FALSE;
    sr = *zr;
    si = *zi;

    /* main loop for stage three */

    for (i=1; i<=l3 ; i++) {

	/* evaluate p at s and test for convergence. */

	polyev(&nn, &sr, &si,
	       pr, pi, qpr, qpi,
	       &pvr, &pvi);
	mp = cmod(&pvr, &pvi);
	ms = cmod(&sr, &si);
	if (mp <=  20. * errev(&nn, qpr, qpi, &ms, &mp, &are, &mre)) {
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
		polyev(&nn, &sr, &si,
		       pr, pi, qpr, qpi,
		       &pvr, &pvi);
		for (j = 1; j <= 5; ++j) {
		    calct(&bool);
		    nexth(&bool);
		}
		omp = infin;
		goto L10;
	    }
	    else {

		/* exit if polynomial value */
		/* increases significantly. */

		if (mp * .1 > omp)
		    return;
	    }
	}
	omp = mp;

	/* calculate next iterate. */

    L10:
	calct(&bool);
	nexth(&bool);
	calct(&bool);
	if (!bool) {
	    relstp = cmod(&tr, &ti) / cmod(&sr, &si);
	    sr += tr;
	    si += ti;
	}
    }
    return;

L_conv:
    *conv = TRUE;
    *zr = sr;
    *zi = si;
}

static void calct(int *bool)
{
    /* computes	 t = -p(s)/h(s).
     * bool   - logical, set true if h(s) is essentially zero.	*/

    double d__1, d__2;
    static int n;
    static double hvi, hvr;

    n = nn - 1;

    /* evaluate h(s). */

    polyev(&n, &sr, &si, hr, hi,
	   qhr, qhi, &hvr, &hvi);
    *bool = cmod(&hvr, &hvi) <= are * 10. * cmod(&hr[n-1], &hi[n-1]);
    if (!*bool) {
	d__1 = -pvr;
	d__2 = -pvi;
	cdivid(&d__1, &d__2, &hvr, &hvi, &tr, &ti);
    }
    else {
	tr = 0.;
	ti = 0.;
    }
}



static void nexth(int *bool)
{
    /* calculates the next shifted h polynomial.
     * bool   -	 logical, if .true. h(s) is essentially zero
     */
    static int j, n;
    static int nm1;
    static double t1, t2;

    n = nn - 1;
    nm1 = n - 1;
    if (!*bool) {
	for (j=2 ; j<=n ; j++) {
	    t1 = qhr[j - 2];
	    t2 = qhi[j - 2];
	    hr[j-1] = tr * t1 - ti * t2 +
		qpr[j-1];
	    hi[j-1] = tr * t2 + ti * t1 +
		qpi[j-1];
	}
	hr[0] = qpr[0];
	hi[0] = qpi[0];
    }
    else {

	/* if h(s) is zero replace h with qh. */

	for (j=2 ; j<=n ; j++) {
	    hr[j-1] = qhr[j-2];
	    hi[j-1] = qhi[j-2];
	}
	hr[0] = 0.;
	hi[0] = 0.;
    }
}

/*--------------------- Independent Complex Polynomial Utilities ----------*/

void polyev(int *nn,
	    double *sr, double *si,
	    double *pr, double *pi,
	    double *qr, double *qi,
	    double *pvr, double *pvi)
{
    /* evaluates a polynomial  p  at  s	 by the horner recurrence
     * placing the partial sums in q and the computed value in pv.
     */
    int i;
    double t;

    qr[0] = pr[0];
    qi[0] = pi[0];
    *pvr = qr[0];
    *pvi = qi[0];
    for (i=1; i < *nn; i++) {
	t = *pvr * *sr - *pvi * *si + pr[i];
	qi[i] = *pvi = *pvr * *si + *pvi * *sr + pi[i];
	qr[i] = *pvr = t;
    }
}


double errev(int *nn, double *qr, double *qi, double *ms,
	     double *mp, double *are, double *mre)
{
    /*	bounds the error in evaluating the polynomial by the horner
     *	recurrence.
     *
     *	qr,qi	 - the partial sums
     *	ms	 - modulus of the point
     *	mp	 - modulus of polynomial value
     *	are, mre - error bounds on complex addition and multiplication
     */
    double e;
    int i;

    e = cmod(&qr[0], &qi[0]) * *mre / (*are + *mre);
    for (i=0 ; i < *nn ; i++) {
	e *= (*ms + cmod(&qr[i], &qi[i]));
    }
    return e * (*are + *mre) - *mp * *mre;
}


double cauchy(int *nn, double *pt, double *q)
{
    /* Computes a lower bound on the moduli of the zeros of a polynomial
     * pt[1:nn] is the modulus of the coefficients.
     */
    double f, x, df, dx, xm;
    int i, n;

    n = *nn - 1;
    pt[n] = -pt[n];

    /* compute upper estimate of bound. */

    x = exp((log(-pt[n]) - log(pt[0])) / (double) n);

    /* if newton step at the origin is better, use it. */

    if (pt[n-1] != 0.) {
	xm = -pt[n] / pt[n-1];
	if (xm < x)
	    x = xm;
    }

    /* chop the interval (0,x) unitl f le 0. */

    for(;;) {
	xm = x * 0.1;
	f = pt[0];
	for (i=1 ; i < *nn ; i++)
	    f = f * xm + pt[i];
	if (f <= 0.0) {
	    break;
	}
	x = xm;
    }

    dx = x;

    /* do newton iteration until x converges to two decimal places. */

    while (fabs(dx / x) > 0.005) {
	q[0] = pt[0];
	for (i=1 ; i < *nn; i++) {
	    q[i] = q[i-1] * x + pt[i];
	}
	f = q[n];
	df = q[0];
	for (i=1 ; i<n ; i++) {
	    df = df * x + q[i];
	}
	dx = f / df;
	x -= dx;
    }
    return x;
}


void scale(int *nn, double *pt,
	   double *eta, double *infin, double *smalno, double *base,
	   double *fact)
{
    /* Returns a scale factor to multiply the coefficients of the polynomial.
     * The scaling is done to avoid overflow and to avoid
     *	undetected underflow interfering with the convergence criterion.
     * The factor is a power of the base.

     * pt [1:nn] : modulus of coefficients of p
     * eta,infin,
     * smalno,base - constants describing the floating point arithmetic.
     * fact : scale factor
     */

    int i, ell;
    double x, hi, sc, lo, min_, max_;

    /* find largest and smallest moduli of coefficients. */
    hi = sqrt(*infin);
    lo = *smalno / *eta;
    max_ = 0.;
    min_ = *infin;
    for (i=0 ; i < *nn ; i++) {
	x = pt[i];
	if (x > max_)
	    max_ = x;
	if (x != 0. && x < min_)
	    min_ = x;
    }

    /* scale only if there are very large or very small components. */

    if (min_ < lo || max_ > hi) {
	x = lo / min_;
	if (x <= 1.)
	    sc = 1. / (sqrt(max_) * sqrt(min_));
	else {
	    sc = x;
	    if (*infin / sc > max_)
		sc = 1.0;
	}
	ell = (int) (log(sc) / log(*base) + 0.5);
	*fact = POW_DI(base, &ell);
    }
    else *fact = 1.0;
}



void cdivid(double *ar, double *ai, double *br, double *bi,
	    double *cr, double *ci)
{
/* complex division c = a/b, i.e., (cr +i*ci) = (ar +i*ai) / (br +i*bi),
   avoiding overflow. */

    static double d, r;

    if (*br == 0. && *bi == 0.) {
	/* division by zero, c = infinity. */
	*cr = *ci = R_PosInf;
    }
    else if (fabs(*br) >= fabs(*bi)) {
	r = *bi / *br;
	d = *br + r * *bi;
	*cr = (*ar + *ai * r) / d;
	*ci = (*ai - *ar * r) / d;
    }
    else {
	r = *br / *bi;
	d = *bi + r * *br;
	*cr = (*ar * r + *ai) / d;
	*ci = (*ai * r - *ar) / d;
    }
}


double cmod(double *r, double *i)
{
    /* modulus of a complex number avoiding overflow. */
    double ai, ar, d1;

    ar = fabs(*r);
    ai = fabs(*i);
    if (ar < ai) {
	d1 = ar / ai;
	return ai * sqrt(d1 * d1 + 1.0);
    }
    else if (ar <= ai) {
	return ar * sqrt(2.0);
    }
    else {
	d1 = ai / ar;
	return ar * sqrt(d1 * d1 + 1.0);
    }
}
