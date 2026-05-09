/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2026   The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/

 * stl.c  = stl.f -- translated by f2c (version 20200916).
   Run through MM's f2c-clean.
   Heavily hand-edited by MM.

 * stl.f :
   From netlib/a/stl: no authorship nor copyright claim in the source;
   presumably by the authors of

     R.B. Cleveland, W.S.Cleveland, J.E. McRae, and I. Terpenning,
     STL: A Seasonal-Trend Decomposition Procedure Based on Loess,
     Statistics Research Report, AT&T Bell Laboratories.

   Converted to double precision by B.D. Ripley 1999.
   Indented, goto labels renamed, some goto's replaced,
   more comments;  by M.Maechler 2026.
*/

// for imax2() ..
#include <Rmath.h>
// R_alloc() ..
#include <R_ext/Memory.h>

#include "ts.h" // for F77_NAME(stl)

static
void stlstp(double *y, int n, int np, int ns, int nt, int nl,
	     int isdeg, int itdeg, int ildeg, int nsjump, int ntjump, int nljump,
	     int niter,
	     bool use_rw, double *rw,
	     double *season, double *trend, double *work);

static
void stlrwt(double *, int n, double *, double *);


/* Called from R -- for now via .Fortran(C_stl, ...)
 *                 "for now": to switch between *.c and *.f */
void F77_NAME(stl)(double *y, int *n, // n = length(y)
	  int *np, /* period */
	  int *ns, /* s.window  \    */
	  int *nt, /* t.window   > = spans for 's', 't' and 'l' smoother */
	  int *nl, /* l.window  /    */
	  // {s,t,l}.degree: local degree for {s,t,l} smoother:
	  int *isdeg,  int *itdeg,  int *ildeg,
	  // {s,t,l}.jump  : "jump"       for {s,t,l} smoother:
	  int *nsjump, int *ntjump, int *nljump,
	  int *ni, // inner iterations
	  int *no, // outer robustness iterations
	  // result :
	  double *rw, // weights
	  double *season, // seasonal
	  double *trend)  // trend
// now allocated below: double *work) // of size  5*(n + 2*period)
{
    bool use_rw = false;

    for (int i = 0; i < *n; ++i) {
	trend[i] = 0.;
    }
    /* the three spans must be at least three and odd: */
    int newns = imax2(3,*ns),
	newnt = imax2(3,*nt),
	newnl = imax2(3,*nl);
    if (newns % 2 == 0) ++newns;
    if (newnt % 2 == 0) ++newnt;
    if (newnl % 2 == 0) ++newnl;

    /* periodicity at least 2: */
    int nperiod = imax2(2,*np),
	niter = *ni;

    // This is not large so could simply use automatic allocation.
    double *work = (double *) R_alloc(5*(*n + 2*nperiod), sizeof(double));

    /* --- outer loop -- do *no robustness iterations --------------------- */
    for(int k = 0; /* do stlstp_() in *any* case */; ) { /* L100: */
	stlstp(y, *n, nperiod, newns, newnt, newnl,
		*isdeg, *itdeg, *ildeg,
		*nsjump, *ntjump, *nljump,
		niter, use_rw, rw,
		season, trend, work);
	++k;
	if (k > *no) {
	    break /* goto L10*/;
	}
	for (int i = 0; i < *n; ++i) {
	    work[i] = trend[i] + season[i];
	}
	stlrwt(y, *n, work, rw);
	use_rw = true;
    } /* --- end Loop -- L10: ------------------ */

    /* robustness weights when there were no robustness iterations: */
    if (*no <= 0) {
	for (int i = 0; i < *n; ++i)
	    rw[i] = 1.;
    }
    return;
} /* stl_ */


static
bool stlest(const double y[], int n, int len, int ideg,
	     double xs, double *ys,
	     int nleft, int nright,
	     double *w,
	     bool use_rw, const double rw[]);

static
void stless(double *y, int n, int len, int ideg, int njump,
	     bool use_rw, double *rw, double *ys, double *res)
{
    if (n < 2) {
	ys[0] = y[0];
	return;
    }

    bool ok;
    int newnj = imin2(njump, n - 1);
    int nleft, nright; // 1-indexing still

    if (len >= n) {
	nleft = 1;
	nright = n;
	// Fortran:   do i = 1,n,newnj
	for (int i = 0; i < n; i += newnj) {
	    ok = stlest(y, n, len, ideg, (double) (i+1), &ys[i],
			 nleft, nright,
			 res, use_rw, rw);
	    if (! ok) {
		ys[i] = y[i];
	    }
	}
    } else {
	int nsh = (len + 1) / 2;
	if (newnj == 1) {
	    nleft = 1;
	    nright = len;
	    for (int i = 0; i < n; ++i) {
		if (i+1 > nsh && nright != n) {
		    ++nleft;
		    ++nright;
		}
		ok = stlest(y, n, len, ideg, (double) (i+1), &ys[i],
			     nleft, nright,
			     res, use_rw, rw);
		if (! ok) {
		    ys[i] = y[i];
		}
	    }
	} else { /* newnj != 1 */
	    /* -Wall: */ nleft = 1; nright = len;
	    // Fortran:  do i = 1,n,newnj
	    for (int i = 0; i < n; i += newnj) {
		if (i+1 < nsh) {
		    nleft = 1;
		    nright = len;
		} else if (i >= n - nsh) {
		    nleft = n - len + 1;
		    nright = n;
		} else {
		    nleft = i+1 - nsh + 1;
		    nright = len + i+1 - nsh;
		}
		ok = stlest(y, n, len, ideg, (double) (i+1), &ys[i],
			     nleft, nright,
			     res, use_rw, rw);
		if (! ok) {
		    ys[i] = y[i];
		}
	    }
	}
    }
    if (newnj != 1) {
	// Fortran:  do i = 1,n-newnj,newnj
	for (int i = 0; i < n-newnj; i += newnj) {
	    double delta = (ys[i + newnj] - ys[i]) / (double) newnj;
	    for (int j = i + 1; j <= (i + newnj - 1); ++j) {
		ys[j] = ys[i] + delta * (double) (j - i);
	    }
	}
	int k = (n - 1) / newnj * newnj; // now 0-based
	if (k != n - 1) {
	    ok = stlest(y, n, len, ideg, (double) n, &ys[n-1],
			 nleft, nright,
			 res, use_rw, rw);
	    if (! ok) {
		ys[n-1] = y[n-1];
	    }
	    if (k != n - 1) {
		double delta = (ys[n-1] - ys[k]) / (double) (n-1 - k);
		for (int j = k + 1; j < (n - 1); ++j) {
		    ys[j] = ys[k] + delta * (double) (j - k);
		}
	    }
	}
    }
    return;
} // stless_

static
bool stlest(const double y[], int n, int len, int ideg,
	     double xs, double *ys,
	     int nleft, int nright,
	     double *w,
	     bool use_rw, const double rw[])
{
    double range = (double) (n - 1),
	h = fmax2(xs - (double) nleft, (double) nright - xs);
    if (len > n) {
	h += (double) ((len - n) / 2);
    }
    double
	h9 = h * .999,
	h1 = h * .001,
	a = 0.;
    for (int j = nleft-1; j < nright; ++j) {
	double r = fabs((double)(j+1) - xs);
	if (r <= h9) {
	    if (r <= h1) {
		w[j] = 1.;
	    } else {
		//ftn:   w(j) = (1.d0 - (r/h)**3)**3
		w[j] = R_pow_di(1. - R_pow_di(r / h, 3), 3);
	    }
	    if (use_rw) {
		w[j] = rw[j] * w[j];
	    }
	    a += w[j];
	} else {
	    w[j] = 0.;
	}
    }

    bool ok; /* ----> the result */
    if (a <= 0.) {
	ok = false;
    } else {
	ok = true;
	int j;
	for (j = nleft-1; j < nright; ++j) {
	    w[j] /= a;
	}
	if (h > 0. && ideg > 0) {
	    a = 0.;
	    for (j = nleft-1; j < nright; ++j) {
		a += w[j] * (double) (j+1);
	    }
	    double b = xs - a,
		c = 0.;
	    for (j = nleft-1; j < nright; ++j) {
		double d = (double) (j+1) - a;
		c += w[j] * (d * d);
	    }
	    if (sqrt(c) > range * .001) {
		b /= c;
		for (j = nleft-1; j < nright; ++j) {
		    w[j] *= b * ((double)(j+1) - a) + 1.;
		}
	    }
	}
	*ys = 0.;
	for (j = nleft-1; j < nright; ++j) {
	    *ys += w[j] * y[j];
	}
    }
    return ok;
} /* stlest_ */


static
void stlma(double *x, int n, int len, double *ave)
{
/* Moving Average (aka "running mean")
 ave(i) := mean(x{j}, j = max(1,i-k),..., min(n, i+k))
           for i = 1,2,..,n
*/
    double flen = (double) (len),
	v = 0.;
    for (int i = 0; i < len; ++i)
	v += x[i];
    ave[0] = v / flen;
    int newn = n - len + 1;
    if (newn > 1) {
	int k = len,
	    m = 0;
	for (int j = 1; j < newn; ++j, ++k, ++m) {
	    v += x[k] - x[m];
	    ave[j] = v / flen;
	}
    }
    return;
} /* stlma_ */

void stlfts(double *x, int n, int np,
	     double *trend, double *work)
{
    stlma(x,     n,                  np, trend);
    stlma(trend, n - np + 1,         np, work);
    stlma(work , n - (np << 1) + 2,   3, trend);
    return;
}

static
void stlss(const double y[], int n, int np, int ns, int isdeg, int nsjump,
	    bool use_rw, const double rw[],
	    /* ---> result: */ double *season, /* [1:(n+2*np)] */
	    double *work1, double *work2, double *work3, double *work4);

static
void stlstp(double *y, int n, int np, int ns, int nt, int nl,
	     int isdeg, int itdeg, int ildeg, int nsjump, int ntjump, int nljump,
	     int niter,
	     bool use_rw, double *rw,
	     double *season, double *trend, double *work)
{
    int i, n2p = n + (np << 1); /* = n + 2 * np */
    double
	*work2 = work + n2p,
	*work3 = work2+ n2p,
	*work4 = work3+ n2p,
	*work5 = work4+ n2p;

    for (int j = 1; j <= niter; ++j) { /* Do  niter  "inner" iterations : -------------*/
	for (i = 0; i < n; ++i)
	    work[i] = y[i] - trend[i];
	stlss(work, n, np, ns, isdeg, nsjump, use_rw, rw,
	       work2, work3, work4, work5, season);
	stlfts(work2, n2p, np, work3, work);
	stless(work3, n, nl, ildeg, nljump, /* use_rw = */ false,
		work4, work, work5);
	for (i = 0; i < n; ++i)
	    season[i] = work2[np + i] - work[i];
	for (i = 0; i < n; ++i)
	    work[i] = y[i] - season[i];
	stless(work, n, nt, itdeg, ntjump, use_rw, rw, trend, work3);
    }
    return;
} // stlstp_

static void psort(double *a, int n, int *ind, int ni);

static
void stlrwt(double *y, int n, double *fit, double *rw)
{
/* Robustness Weights
       rw_i := B( |y_i - fit_i| / (6 M) ),   i = 1,2,...,n
               where B(u) = (1 - u^2)^2  * 1[|u| < 1]   {Tukey's biweight}
               and   M := median{ |y_i - fit_i| }
*/
    for (int i = 0; i < n; ++i) {
	rw[i] = fabs(y[i] - fit[i]);
    }
    int mid[2];
    mid[0] = n / 2 + 1; // 1-indexing also in psort()
    mid[1] = n - mid[0] + 1;
    psort(rw, n, mid, 2);
    double
	cmad = (rw[mid[0]-1] + rw[mid[1]-1]) * 3., /* = 6 * MAD */
	c9 = cmad * .999,
	c1 = cmad * .001;
    for (int i = 0; i < n; ++i) {
	double r = fabs(y[i] - fit[i]);
	if (r <= c1) {
	    rw[i] = 1.;
	} else if (r <= c9) {
	    double d2 = r / cmad,
		 I_d2 = 1. - d2 * d2;
	    rw[i] = I_d2 * I_d2;
	} else {
	    rw[i] = 0.;
	}
    }
    return;
} // stlrwt_


/* called by stlstp() at the beginning of each (inner) iteration */
static
void stlss(const double y[], int n, int np, int ns, int isdeg, int nsjump,
	    bool use_rw, const double rw[],
	    /* ---> result: */ double *season, /* [1:(n+2*np)] */
	    double *work1, double *work2, double *work3, double *work4)
{
    for (int j = 0; j < np; ++j) {
	int k = (n - (j + 1)) / np + 1;
	for (int i = 0; i < k; ++i) {
	    work1[i] = y[i * np + j];
	}
	if (use_rw) {
	    for (int i = 0; i < k; ++i) {
		work3[i] = rw[i * np + j];
	    }
	}
	stless(work1, k, ns, isdeg, nsjump, use_rw, work3, &work2[1], work4);
	int nleft, nright = imin2(ns,k);
	bool ok = stlest(work1, k, ns, isdeg, 0., work2,
			  1, nright,
			  work4, use_rw, work3);
	if (! ok) {
	    work2[0] = work2[1];
	}
	nleft = imax2(1, k - ns + 1);
	ok = stlest(work1, k, ns, isdeg, (double) (k + 1), &work2[k + 1],
		     nleft, k,
		     work4, use_rw, work3);
	if (! ok) {
	    work2[k + 1] = work2[k];
	}
	for (int m = 0; m < k + 2; ++m) {
	    season[m * np + j] = work2[m];
	}
    }
    return;
} // stlss_


/* Partial Sorting ; used for Median (MAD) computation only */
static
void psort(double *a, int n, int *ind, int ni)
{
    if (n < 0 || ni < 0) {
	return;
    }
    if (n < 2 || ni == 0) {
	return;
    }
    int il[16], iu[16];
    int indl[16], indu[16];

    /* Parameter adjustments */
    --a;
    --ind;

    int jl = 1,
	ju = ni,
	i = 1,
	j = n,
	m = 0;
    indl[0] = 1;
    indu[0] = ni;

LoopOuter: // L 161
    if (i < j) {
	goto L10;
    }
/*  _Loop_ */
Loop: // L 166
    --m;
    if (m < 0) {
	return;
    }
    i = il[m];
    j = iu[m];
    jl = indl[m];
    ju = indu[m];
    if (! (jl <= ju)) {
	goto Loop;
    }
/*     while (j - i > 10) */
L173:
    if (! (j - i > 10)) {
	goto L174;
    }
L10:
    int k = i, l = j,
	ij = (i + j) / 2;
    double t = a[ij];
    if (a[i] > t) { // swap a[i] <--> a[ij]
	a[ij] = a[i]; a[i] = t; t = a[ij];
    }
    if (a[j] < t) { // swap a[j] ..
	a[ij] = a[j]; a[j] = t; t = a[ij];
	if (a[i] > t) { // swap
	    a[ij] = a[i]; a[i] = t; t = a[ij];
	}
    }
    do { // L181:
	--l;
	if (a[l] <= t) {
	    double tt = a[l];
	    do
		++k;
	    while (a[k] < t);
	    if (k > l)
		break;
	    a[l] = a[k];
	    a[k] = tt;
	}
    } while(1);

    indl[m] = jl;
    indu[m] = ju;
    int p1 = m;
    ++m;
    if (l - i <= j - k) {
	il[p1] = k;
	iu[p1] = j;
	j = l;
    L_1:
	if (jl > ju) {
	    goto Loop;
	}
	if (ind[ju] > j) {
	    --ju;
	    goto L_1;
	}
	indl[p1] = ju + 1;
    } else {
	il[p1] = i;
	iu[p1] = l;
	i = k;
    L_2:
	if (jl > ju) {
	    goto Loop;
	}
	if (ind[jl] < i) {
	    ++jl;
	    goto L_2;
	}
	indu[p1] = jl - 1;
    }
    goto L173;
/*     end while */
L174:

    if (i != 1) {
	--i;
	do { // L209:
	    ++i;
	    if (i == j) {
		goto Loop;
	    }
	    t = a[i + 1];
	    if (a[i] > t) {
		k = i;
		do {
		    a[k + 1] = a[k];
		    --k;
		} while (!(t >= a[k]));
		/* now  t >= a(k) */
		a[k + 1] = t;
	    }
	} while(1);
    }
    goto LoopOuter;

} /* psort_ */

