/*
 *      cpoly finds the zeros of a complex polynomial.
 *
 *      On Entry
 *
 *      opr, opi      -  double precision vectors of real and
 *                       imaginary parts of the coefficients in
 *                       order of decreasing powers.
 *
 *      degree        -  int degree of polynomial.
 *
 *
 *      On Return
 *
 *      zeror, zeroi  -  output double precision vectors of
 *                       real and imaginary parts of the zeros.
 *
 *      fail          -  output int parameter,  true  only if
 *                       leading coefficient is zero or if cpoly
 *                       has found fewer than degree zeros.
 *
 *      The program has been written to reduce the chance of overflow
 *      occurring. If it does occur, there is still a possibility that
 *      the zerofinder will work provided the overflowed quantity is
 *      replaced by a large number.
 *
 *      This is a C translation of the following.
 *
 *      TOMS Algorithm 419
 *      Jenkins and Traub.
 *      Comm. ACM 15 (1972) 97-99.
 *
 *      Ross Ihaka
 *      February 1997
 */

#include "Fortran.h"

static double cauchy_();
static double cmod_(double *, double *);
static double errev_(int *, double *, double *, double *, double *, double *, double *);
static double scale_();
static int calct_(int *);
static int cdivid_(double *, double *,double *, double *, double *, double *);
static int fxshft_();
static int mcon_(double *, double *, double *, double *);
static int nexth_(int *);
static int noshft_(int);
static int polyev_(int *, double *, double *, double *, double *, double *, double *, double *, double *);
static int vrshft_(int, double *, double *, int *);

/* Global Variables */

#define NMAX 50

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
static double sr;
static double si;
static double tr;
static double ti;
static double pvr;
static double pvi;
static double are;
static double mre;
static double eta;
static double infin;
static int nn;


int F77_SYMBOL(cpoly)(double *opr, double *opi, int *degree,
	double *zeror, double *zeroi, int *fail)
{
	int i__1;
	double d__1, d__2;
	static double base;
	static int conv;
	static double cosr, sinr;
	static int idnn2, i;
	static double zi;
	static double zr;
	static double xx, yy, smalno;
	static double bnd, xxx;
	static int cnt1, cnt2;
	--zeroi;
	--zeror;
	--opi;
	--opr;

	mcon_(&eta, &infin, &smalno, &base);
	are = eta;
	mre = sqrt(2.0) * 2.0 * eta;

	/* We use the originals so we get exact */
	/* agreement with the original, but ... */
	/*  cos 94 =   -0.06975647374412529990   */
	/*  sin 94 =    0.99756405025982424767   */
        /*  1/sqrt(2) = 0.70710678118654752440   */

	xx = (float).70710678;
	yy = -xx;
	cosr = (float)-.060756474;
	sinr = (float).99756405;
	*fail = FALSE;
	nn = *degree + 1;

	/* algorithm fails if the leading coefficient is zero. */

	if (opr[1] == 0.0 && opi[1] == 0.0) {
		*fail = TRUE;
		return 0;
	}

	/* remove the zeros at the origin if any. */

	while (opr[nn] == 0.0 && opi[nn] == 0.0) {
		idnn2 = *degree-nn+2;
		zeror[idnn2] = 0.0;
		zeroi[idnn2] = 0.0;
		nn = nn-1;
	}

	/* make a copy of the coefficients. */

	for (i = 1; i<=nn ; i++) {
		pr[i-1] = opr[i];
		pi[i-1] = opi[i];
		shr[i-1] = hypot(pr[i-1], pi[i-1]);
	}

	/* scale the polynomial. */

	bnd = scale_(&nn, shr, &eta,
		&infin, &smalno, &base);

	if (bnd != 1.0) {
		for (i=1; i<=nn; i++) {
			pr[i-1] = bnd * pr[i-1];
			pi[i-1] = bnd * pi[i-1];
		}
	}

	/* start the algorithm for one zero */

	while (nn > 2) {

		/* calculate bnd, a lower bound on the modulus of the zeros. */

		for (i=1 ; i<=nn ; i++)
			shr[i-1] = cmod_(&pr[i-1], &pi[i-1]);

		bnd = cauchy_(&nn, shr, shi);

		/* outer loop to control 2 major passes */
		/* with different sequences of shifts */

		for (cnt1=1; cnt1<=2; cnt1++) {

			/* first stage calculation, no shift */

			noshft_(5);

			/*  inner loop to select a shift */
			for (cnt2=1 ; cnt2<=9 ; cnt2++) {

				/* shift is chosen with modulus bnd */
				/* and amplitude rotated by 94 degrees */
				/* from the previous shift */

				xxx = cosr * xx - sinr * yy;
				yy = sinr * xx + cosr * yy;
				xx = xxx;
				sr = bnd * xx;
				si = bnd * yy;

				/*  second stage calculation, fixed shift */

				i__1 = cnt2 * 10;
				fxshft_(&i__1, &zr, &zi, &conv);

				if (conv)
					goto L10;
			}
		}

		/* the zerofinder has failed on two major passes */
		/* return empty handed */

	   	*fail = TRUE;
		return 0;

		/* the second stage jumps directly to the third */
		/* stage iteration.  if successful the zero is */
		/* stored and the polynomial deflated. */

	   L10:	idnn2 = *degree - nn + 2;
		zeror[idnn2] = zr;
		zeroi[idnn2] = zi;
		--nn;
		for (i=1 ; i<=nn ; i++) {
			pr[i-1] = qpr[i-1];
			pi[i-1] = qpi[i-1];
		}
	}

	/*  calculate the final zero and return */

	d__1 = -pr[1];
	d__2 = -pi[1];
	cdivid_(&d__1, &d__2, pr, pi, &zeror[*degree], &zeroi[*degree]);
	return 0;

}


/*  Computes the derivative polynomial as the initial
 *  polynomial and computes l1 no-shift h polynomials.  */

static int noshft_(int l1)
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

		if (cmod_(&hr[n-1], &hi[n-1]) <= eta * 10.0 * cmod_(&pr[n-1], &pi[n-1])) {

			/*  If the constant term is essentially zero, */
			/*  shift h coefficients. */

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
			cdivid_(&d__1, &d__2, &hr[n-1],
				&hi[n-1], &tr, &ti);
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
	return 0;
}


/*  Computes l2 fixed-shift h polynomials and tests for convergence.
 *  initiates a variable-shift iteration and returns with the
 *  approximate zero if successful.
 *
 *  l2    - limit of fixed shift steps
 *  zr,zi - approximate zero if conv is .true.
 *  conv  - int indicating convergence of stage 3 iteration  */

static int fxshft_(int *l2, double *zr, double *zi, int *conv)
{
	double d__1, d__2;
	static int pasd, bool, test;
	static double svsi, svsr;
	static int i, j, n;
	static double oti, otr;

	n = nn - 1;

	/* evaluate p at s. */

	polyev_(&nn, &sr, &si, pr, 
		pi, qpr, qpi, &pvr,
		&pvi);

	test = TRUE;
	pasd = FALSE;

	/* calculate first t = -p(s)/h(s). */

	calct_(&bool);

	/* main loop for one second stage step. */

	for (j=1; j<=*l2; j++) {

		otr = tr;
		oti = ti;

		/* compute next h polynomial and new t. */

		nexth_(&bool);
		calct_(&bool);
		*zr = sr + tr;
		*zi = si + ti;

		/* test for convergence unless stage 3 has */
		/* failed once or this is the last h polynomial. */

		if (!bool && test && j != *l2) {
			d__1 = tr - otr;
			d__2 = ti - oti;
			if (cmod_(&d__1, &d__2) >= cmod_(zr, zi) * 0.5) {
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
				vrshft_(10, zr, zi, conv);
				if (*conv) {
					return 0;
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
				polyev_(&nn, &sr, &si, pr, 
					pi, qpr, qpi, &pvr, &
					pvi);
				calct_(&bool);
			}
		}
	}

	/* attempt an iteration with final h polynomial */
	/* from second stage. */

	vrshft_(10, zr, zi, conv);
	return 0;
}


/*  carries out the third stage iteration.
 *  l3      - limit of steps in stage 3.
 *  zr,zi   - on entry contains the initial iterate, if the
 *            iteration converges it contains the final iterate
 *            on exit.
 *  conv    - .true. if iteration converges  */

static int vrshft_(int l3, double *zr, double *zi, int *conv)
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

		polyev_(&nn, &sr, &si,
			pr, pi, qpr, qpi,
			&pvr, &pvi);
		mp = cmod_(&pvr, &pvi);
		ms = cmod_(&sr, &si);
		if (mp <= errev_(&nn, qpr, qpi, &ms, &mp, &are, &mre) * 20.0) {
			goto L20;
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
				polyev_(&nn, &sr,
					&si, pr, 
					pi, qpr,
					qpi, &pvr,
					&pvi);
				for (j = 1; j <= 5; ++j) {
					calct_(&bool);
					nexth_(&bool);
				}
				omp = infin;
				goto L10;
			}
			else {

				/* exit if polynomial value */
				/* increases significantly. */

				if (mp * .1 > omp)
					return 0;
			}
		}
		omp = mp;

		/* calculate next iterate. */

	L10:
		calct_(&bool);
		nexth_(&bool);
		calct_(&bool);
		if (!bool) {
			relstp = cmod_(&tr, &ti) / cmod_(&sr, &si);
			sr += tr;
			si += ti;
		}
	}
	return 0;
L20:
	*conv = TRUE;
	*zr = sr;
	*zi = si;
	return 0;
}

/* computes  t = -p(s)/h(s).
 * bool   - logical, set true if h(s) is essentially zero.  */

static int calct_(int *bool)
{
	double d__1, d__2;
	static int n;
	static double hvi, hvr;

	n = nn - 1;

	/* evaluate h(s). */

	polyev_(&n, &sr, &si, hr, hi, 
		qhr, qhi, &hvr, &hvi);
	*bool = cmod_(&hvr, &hvi) <= are * 10. * cmod_(&hr[n-1], &hi[n-1]);
	if (!*bool) {
		d__1 = -pvr;
		d__2 = -pvi;
		cdivid_(&d__1, &d__2, &hvr, &hvi, &tr, &ti);
	}
	else {
		tr = 0.;
		ti = 0.;
	}
	return 0;
}


/* calculates the next shifted h polynomial. */
/* bool   -  logical, if .true. h(s) is essentially zero */

static int nexth_(int *bool)
{
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
	return 0;
}

/* evaluates a polynomial  p  at  s  by the horner recurrence */
/* placing the partial sums in q and the computed value in pv. */

static int polyev_(int *nn, double *sr, double *si, double *pr, double *pi,
	double *qr, double *qi, double *pvr, double *pvi)
{
	static int i;
	static double t;

	/* Parameter adjustments */
	--qi;
	--qr;
	--pi;
	--pr;

	/* Function Body */
	qr[1] = pr[1];
	qi[1] = pi[1];
	*pvr = qr[1];
	*pvi = qi[1];
	for (i=2; i<=*nn; i++) {
		t = *pvr * *sr - *pvi * *si + pr[i];
		*pvi = *pvr * *si + *pvi * *sr + pi[i];
		*pvr = t;
		qr[i] = *pvr;
		qi[i] = *pvi;
	}
	return 0;
}


/*  bounds the error in evaluating the polynomial by the horner
 *  recurrence.
 *
 *  qr,qi    - the partial sums
 *  ms       - modulus of the point
 *  mp       - modulus of polynomial value
 *  are, mre - error bounds on complex addition and multiplication  */

static double errev_(int *nn, double *qr, double *qi, double *ms,
	 double *mp, double *are, double *mre)
{
    static double e;
    static int i;

    /* Parameter adjustments */
    --qi;
    --qr;

    /* Function Body */
    e = cmod_(&qr[1], &qi[1]) * *mre / (*are + *mre);
    for (i=1 ; i<=*nn ; i++) {
	e = e * *ms + cmod_(&qr[i], &qi[i]);
    }
    return e * (*are + *mre) - *mp * *mre;
}


/* cauchy computes a lower bound on the moduli of the zeros of a */
/* polynomial - pt is the modulus of the coefficients. */

static double cauchy_(int *nn, double *pt, double *q)
{
	static double f;
	static int i, n;
	static double x, df, dx, xm;

	/* Parameter adjustments */
	--q;
	--pt;

	/* Function Body */
	pt[*nn] = -pt[*nn];

	/* compute upper estimate of bound. */

	n = *nn - 1;
	x = exp((log(-pt[*nn]) - log(pt[1])) / (double) n);

	/* if newton step at the origin is better, use it. */

	if (pt[n] != 0.) {
		xm = -pt[*nn] / pt[n];
		if (xm < x)
			x = xm;
	}

	/* chop the interval (0,x) unitl f le 0. */

	for(;;) {
		xm = x * 0.1;
		f = pt[1];
		for (i=2 ; i<=*nn ; i++)
			f = f * xm + pt[i];
		if (f <= 0.0) {
			break;
		}
		x = xm;
	}

	dx = x;

	/* do newton iteration until x converges to two decimal places. */

	while (fabs(dx / x) > 0.005) {
		q[1] = pt[1];
		for (i=2 ; i<=*nn; i++) {
			q[i] = q[i-1] * x + pt[i];
		}
		f = q[*nn];
		df = q[1];
		for (i=2 ; i<=n ; i++) {
			df = df * x + q[i];
		}
		dx = f / df;
		x -= dx;
	}
	return x;
}


/* returns a scale factor to multiply the coefficients of the */
/* polynomial. the scaling is done to avoid overflow and to avoid */
/* undetected underflow interfering with the convergence */
/* criterion.  the factor is a power of the base. */
/* pt - modulus of coefficients of p */
/* eta,infin,smalno,base - constants describing the */
/* floating point arithmetic. */

static double scale_(int *nn, double *pt, double *eta, double *infin,
	double *smalno, double *base)
{
	/* Local variables */
	static int i, l;
	static double x, hi, sc, lo, min_, max_;

	/* Parameter adjustments */
	--pt;

	/* Function Body */
	/* find largest and smallest moduli of coefficients. */

	hi = sqrt(*infin);
	lo = *smalno / *eta;
	max_ = 0.;
	min_ = *infin;
	for (i=1 ; i<=*nn ; i++) {
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
			if (*infin / sc > max_) {
				sc = 1.0;
			}
		}
		l = (int) (log(sc) / log(*base) + 0.5);
		return POW_DI(base, &l);
	}
	else return 1.0;
}


	/* complex division c = a/b, avoiding overflow. */

static int cdivid_(double *ar, double *ai, double *br, 
	double *bi, double *cr, double *ci)
{
	static double d, r, t, infin;

	if (*br == 0. && *bi == 0.) {

		/* division by zero, c = infinity. */

		mcon_(&t, &infin, &t, &t);
		*cr = infin;
		*ci = infin;
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
	return 0;
}


/* modulus of a complex number avoiding overflow. */

static double cmod_(double *r, double *i)
{
	static double ai, ar, d1;

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

/*  mcon provides machine constants used in various parts of the
 *  program. the user may either set them directly or use the
 *  statements below to compute them. the meaning of the four
 *  constants are -
 *
 *  eta       the maximum relative representation error
 *            which can be described as the smallest positive
 *            floating-point number such that 1.0d0 + eta is
 *            greater than 1.0d0.
 *  infiny    the largest floating-point number
 *  smalno    the smallest positive floating-point number
 *  base      the base of the floating-point number system used
 *
 *  let t be the number of base-digits in each floating-point
 *  number(double precision). then eta is either .5*b**(1-t)
 *  or b**(1-t) depending on whether rounding or truncation
 *  is used.
 *
 *  let m be the largest exponent and n the smallest exponent
 *  in the number system. then infiny is (1-base**(-t))*base**m
 *  and smalno is base**n.
 *
 *  the values for base,t,m,n below correspond to the ibm/360.  */

#ifdef OLD
static int mcon_(double *eta, double *infiny, double *smalno, double *base)
{
	/* System generated locals */
	int i__1, i__2;
	double d__1, d__2;

	/* Local variables */
	static int m, n, t;

	*base = 16.;
	t = 14;
	m = 63;
	n = -65;
	i__1 = 1 - t;
	*eta = pow_di(base, &i__1);
	i__1 = -t;
	i__2 = m - 1;
	*infiny = *base * (1. - pow_di(base, &i__1)) * pow_di(base, &i__2);
	i__1 = n + 3;

	/* Computing 3rd power */

	d__1 = *base, d__2 = d__1;
	*smalno = pow_di(base, &i__1) / (d__2 * (d__1 * d__1));
	return 0;
}
#else
#include <float.h>

static int mcon_(double *eta, double *infiny, double *smalno, double *base)
{
	*base = (double)FLT_RADIX;
	*eta = DBL_EPSILON;
	*infiny = DBL_MAX;
	*smalno = DBL_MIN;
	return 0;
}
#endif
