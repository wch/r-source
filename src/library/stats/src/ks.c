/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1999-2023   The R Core Team.
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
 */

/* ks.c
   Compute the asymptotic distribution of the one- and two-sample
   two-sided Kolmogorov-Smirnov statistics, and the exact distributions
   in the two-sided one-sample and two-sample cases.
*/

#include <math.h>
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>		/* constants */

#include "stats.h"		// for rcont2

static double K2l(double x, double tol);

static int psmirnov_exact_test_one(double q, double r, double s);
static int psmirnov_exact_test_two(double q, double r, double s);
static double psmirnov_exact_uniq_lower(double q, int m, int n, int two);
static double psmirnov_exact_uniq_upper(double q, int m, int n, int two);
static double psmirnov_exact_ties_lower(double q, int m, int n, int *z, int two);
static double psmirnov_exact_ties_upper(double q, int m, int n, int *z, int two);

static double K2x(int n, double d);
static void m_multiply(double *A, double *B, double *C, int m);
static void m_power(double *A, int eA, double *V, int *eV, int m, int n);

static void
Smirnov_sim_wrk(int nrow, int ncol,
		const int nrowt[], const int ncolt[],
		int n, int B, int *observed, int twosided,
		double *fact, int *jwork, double *results);

/* Two-sample two-sided asymptotic distribution */

SEXP pkolmogorov_two_limit(SEXP sq, SEXP stol)
{
    int i;
    double tol = asReal(stol);
    SEXP ans;

    PROTECT(ans = allocVector(REALSXP, LENGTH(sq)));
    for(i = 0; i < LENGTH(sq); i++) {
	REAL(ans)[i] = K2l(REAL(sq)[i], tol);
    }
    UNPROTECT(1);

    return ans;
}

static double
K2l(double x, double tol)
{
/* Compute
 *   \sum_{k=-\infty}^\infty (-1)^k e^{-2 k^2 x^2}
 *   = 1 + 2 \sum_{k=1}^\infty (-1)^k e^{-2 k^2 x^2}
 *   = \frac{\sqrt{2\pi}}{x} \sum_{k=1}^\infty \exp(-(2k-1)^2\pi^2/(8x^2))
 *
 * See e.g. J. Durbin (1973), Distribution Theory for Tests Based on the
 * Sample Distribution Function.  SIAM.
 *
 * The 'standard' series expansion obviously cannot be used close to 0;
 * we use the alternative series for x < 1, and a rather crude estimate
 * of the series remainder term in this case, in particular using that
 * ue^(-lu^2) \le e^(-lu^2 + u) \le e^(-(l-1)u^2 - u^2+u) \le e^(-(l-1))
 * provided that u and l are >= 1.
 *
 * (But note that for reasonable tolerances, one could simply take 0 as
 * the value for x < 0.2, and use the standard expansion otherwise.)
 *
 */
    double new, old, s, w, z, p;
    int k, k_max;

    k_max = (int) sqrt(2 - log(tol));

    /* Note that for x = 0.1 we get 6.609305e-53 ... */
    if(x <= 0.)
	p = 0.;
    else if(x < 1.) {
	z = - (M_PI_2 * M_PI_4) / (x * x);
	w = log(x);
	s = 0;
	for(k = 1; k < k_max; k += 2) {
	    s += exp(k * k * z - w);
	}
	p = s / M_1_SQRT_2PI;
    }
    else {
	z = -2 * x * x;
	s = -1;
	k = 1;
	old = 0;
	new = 1;
	while(fabs(old - new) > tol) {
	    old = new;
	    new += 2 * s * exp(z * k * k);
	    s *= -1;
	    k++;
	}
	p = new;
    }

    return p;
}

/* Two-sample exact distributions.

   See 

     Gunar Schröer and Dietrich Trenkler (1995),
     Exact and Randomization Distributions of Kolmogorov-Smirnov Tests
     for Two or Three Samples,
     Computational Statistics & Data Analysis, 20, 185--202

   and

     Thomas Viehmann (2021),
     Numerically more stable computation of the p-values for the
     two-sample Kolmogorov-Smirnov test,
     <https://arxiv.org/abs/2102.08037>.

   For the lower tail probabilities p = P(D < q), we have

     p = A_{m,n} / choose(m + n, m)

   where for the case of no ties, the A_{i,j} can be computed via the
   basic recursion
   
      A_{i,j} = D_{i,j} (A_{i-1,j} + A_{i,j-1})

   with 

      D_{i,j} = 0 if FUN(i/m - j/n) >= q
                1 otherwise

   where FUN = abs in the two-sided case and identity otherwise.

   In case of ties, the test needs to be changed to

      FUN(i/m - j/n) >= q && z_{(i+j)} == z_{(i+j+1)}

   for i + j < m + n.

   We actually recusively compute

     p_{i,j} = A_{i,j} / choose(i + n, i)

  (inner loop over j, outer loop over i).

  For the upper tail probabilities one has

    p = P(D >= q) = 1 - A_{m,n} / choose(m + n, m)

  one can compute

     C_{i,j} = 1 - A_{i,j} / choose(i + j, i)

  via the recursion

     C_{i,j} = 1 if D_{i,j} = 0
               w * C_{i-1,j} + (1 - w) C_{i,j-1},  w = i / (i + j)

  given in Viehmann (2021).
*/

SEXP psmirnov_exact(SEXP sq, SEXP sm, SEXP sn, SEXP sz,
		    SEXP stwo, SEXP slower) {
    double md, nd, *p, q;
    int i, m, n, *z, two, lower, ties;
    SEXP ans;

    m = asInteger(sm);
    n = asInteger(sn);
    two = asInteger(stwo);
    lower = asInteger(slower);
    ties = (sz != R_NilValue);
    if(ties)
	z = INTEGER(sz);

    md = (double) m;
    nd = (double) n;

    PROTECT(ans = allocVector(REALSXP, LENGTH(sq)));
    p = REAL(ans);
    for(i = 0; i < LENGTH(sq); i++) {
	q = REAL(sq)[i];
	/*
	  q has 0.5/mn added to ensure that rounding error doesn't
	  turn an equality into an inequality, eg abs(1/2-4/5)>3/10 
	*/
	q = (0.5 + floor(q * md * nd - 1e-7)) / (md * nd);
	if(ties) {
	    if(lower)
		p[i] = psmirnov_exact_ties_lower(q, m, n, z, two);
	    else
		p[i] = psmirnov_exact_ties_upper(q, m, n, z, two);
	} else {
	    if(lower)
		p[i] = psmirnov_exact_uniq_lower(q, m, n, two);
	    else
		p[i] = psmirnov_exact_uniq_upper(q, m, n, two);
	}
    }
    UNPROTECT(1);

    return(ans);
}

static int
psmirnov_exact_test_one(double q, double r, double s) {
    return ((r - s) >= q);
}

static int
psmirnov_exact_test_two(double q, double r, double s) {
    return (fabs(r - s) >= q);
}

static double
psmirnov_exact_uniq_lower(double q, int m, int n, int two) {
    double md, nd, *u, w;
    int i, j;
    int (*test)(double, double, double);

    md = (double) m;
    nd = (double) n;
    if(two)
	test = psmirnov_exact_test_two;
    else
	test = psmirnov_exact_test_one;

    u = (double *) R_alloc(n + 1, sizeof(double));

    u[0] = 1.;
    for(j = 1; j <= n; j++) {
        if(test(q, 0., j / nd))
            u[j] = 0.;
        else
            u[j] = u[j - 1];
    }
    for(i = 1; i <= m; i++) {
        w = (double)(i) / ((double)(i + n));
        if(test(q, i / md, 0.))
            u[0] = 0.;
        else
            u[0] = w * u[0];
        for(j = 1; j <= n; j++) {
            if(test(q, i / md, j / nd))
                u[j] = 0.;
            else
                u[j] = w * u[j] + u[j - 1];
        }
    }
    return u[n];
}

static double
psmirnov_exact_uniq_upper(double q, int m, int n, int two) {
    double md, nd, *u, v, w;
    int i, j;
    int (*test)(double, double, double);

    md = (double) m;
    nd = (double) n;
    if(two)
	test = psmirnov_exact_test_two;
    else
	test = psmirnov_exact_test_one;

    u = (double *) R_alloc(n + 1, sizeof(double));

    u[0] = 0.;
    for(j = 1; j <= n; j++) {
        if(test(q, 0., j / nd))
            u[j] = 1.;
        else
            u[j] = u[j - 1];
    }
    for(i = 1; i <= m; i++) {
        if(test(q, i / md, 0.))
            u[0] = 1.;
        for(j = 1; j <= n; j++) {
            if(test(q, i / md, j / nd))
                u[j] = 1.;
            else {
                v = (double)(i) / (double)(i + j);
                w = (double)(j) / (double)(i + j); /* 1 - v */
                u[j] = v * u[j] + w * u[j - 1];
            }
        }
    }
    return u[n];
}

static double
psmirnov_exact_ties_lower(double q, int m, int n, int *z, int two) {
    double md, nd, *u, w;
    int i, j;
    int (*test)(double, double, double);

    md = (double) m;
    nd = (double) n;
    if(two)
	test = psmirnov_exact_test_two;
    else
	test = psmirnov_exact_test_one;

    u = (double *) R_alloc(n + 1, sizeof(double));

    u[0] = 1.;
    for(j = 1; j <= n; j++) {
        if(test(q, 0., j / nd) && z[j])
            u[j] = 0.;
        else
            u[j] = u[j - 1];
    }
    for(i = 1; i <= m; i++) {
        w = (double)(i) / ((double)(i + n));
        if(test(q, i / md, 0.) && z[i])
            u[0] = 0.;
        else
            u[0] = w * u[0];
        for(j = 1; j <= n; j++) {
            if(test(q, i / md, j / nd) && z[i + j])
                u[j] = 0.;
            else
                u[j] = w * u[j] + u[j - 1];
        }
    }
    return u[n];
}

static double
psmirnov_exact_ties_upper(double q, int m, int n, int *z, int two) {
    double md, nd, *u, v, w;
    int i, j;
    int (*test)(double, double, double);

    md = (double) m;
    nd = (double) n;
    if(two)
	test = psmirnov_exact_test_two;
    else
	test = psmirnov_exact_test_one;

    u = (double *) R_alloc(n + 1, sizeof(double));

    u[0] = 0.;
    for(j = 1; j <= n; j++) {
        if(test(q, 0., j / nd) && z[j])
            u[j] = 1.;
        else
            u[j] = u[j - 1];
    }
    for(i = 1; i <= m; i++) {
        if(test(q, i / md, 0.) && z[i])
            u[0] = 1.;
        for(j = 1; j <= n; j++) {
            if(test(q, i / md, j / nd) && z[i + j])
                u[j] = 1.;
            else {
                v = (double)(i) / (double)(i + j);
                w = (double)(j) / (double)(i + j); /* 1 - v */
                u[j] = v * u[j] + w * u[j - 1];
            }
        }
    }
    return u[n];
}

/* One-sample two-sided exact distribution */
SEXP pkolmogorov_two_exact(SEXP sq, SEXP sn)
{
    int n = asInteger(sn), i;
    SEXP ans;

    PROTECT(ans = allocVector(REALSXP, LENGTH(sq)));
    for(i = 0; i < LENGTH(sq); i++) {
	REAL(ans)[i] = K2x(n, REAL(sq)[i]);
    }
    UNPROTECT(1);

    return ans;
}

static double
K2x(int n, double d)
{
    /* Compute Kolmogorov's distribution.
       Code published in
	 George Marsaglia and Wai Wan Tsang and Jingbo Wang (2003),
	 "Evaluating Kolmogorov's distribution".
	 Journal of Statistical Software, Volume 8, 2003, Issue 18.
	 URL: http://www.jstatsoft.org/v08/i18/.
    */

   int k, m, i, j, g, eH, eQ;
   double h, s, *H, *Q;

   /* 
      The faster right-tail approximation is omitted here.
      s = d*d*n; 
      if(s > 7.24 || (s > 3.76 && n > 99)) 
          return 1-2*exp(-(2.000071+.331/sqrt(n)+1.409/n)*s);
   */
   k = (int) (n * d) + 1;
   m = 2 * k - 1;
   h = k - n * d;
   H = (double*) R_Calloc(m * m, double);
   Q = (double*) R_Calloc(m * m, double);
   for(i = 0; i < m; i++)
       for(j = 0; j < m; j++)
	   if(i - j + 1 < 0)
	       H[i * m + j] = 0;
	   else
	       H[i * m + j] = 1;
   for(i = 0; i < m; i++) {
       H[i * m] -= R_pow_di(h, i + 1);
       H[(m - 1) * m + i] -= R_pow_di(h, (m - i));
   }
   H[(m - 1) * m] += ((2 * h - 1 > 0) ? R_pow_di(2 * h - 1, m) : 0);
   for(i = 0; i < m; i++)
       for(j = 0; j < m; j++)
	   if(i - j + 1 > 0)
	       for(g = 1; g <= i - j + 1; g++)
		   H[i * m + j] /= g;
   eH = 0;
   m_power(H, eH, Q, &eQ, m, n);
   s = Q[(k - 1) * m + k - 1];
   for(i = 1; i <= n; i++) {
       s = s * i / n;
       if(s < 1e-140) {
	   s *= 1e140;
	   eQ -= 140;
       }
   }
   s *= R_pow_di(10.0, eQ);
   R_Free(H);
   R_Free(Q);
   return(s);
}

static void
m_multiply(double *A, double *B, double *C, int m)
{
    /* Auxiliary routine used by K2x().
       Matrix multiplication.
    */
    int i, j, k;
    double s;
    for(i = 0; i < m; i++)
	for(j = 0; j < m; j++) {
	    s = 0.;
	    for(k = 0; k < m; k++)
		s+= A[i * m + k] * B[k * m + j];
	    C[i * m + j] = s;
	}
}

static void
m_power(double *A, int eA, double *V, int *eV, int m, int n)
{
    /* Auxiliary routine used by K2x().
       Matrix power.
    */
    double *B;
    int eB, i;

    if(n == 1) {
	for(i = 0; i < m * m; i++)
	    V[i] = A[i];
	*eV = eA;
	return;
    }
    m_power(A, eA, V, eV, m, n / 2);
    B = (double*) R_Calloc(m * m, double);
    m_multiply(V, V, B, m);
    eB = 2 * (*eV);
    if((n % 2) == 0) {
	for(i = 0; i < m * m; i++)
	    V[i] = B[i];
	*eV = eB;
    }
    else {
	m_multiply(A, B, V, m);
	*eV = eA + eB;
    }
    if(V[(m / 2) * m + (m / 2)] > 1e140) {
	for(i = 0; i < m * m; i++)
	    V[i] = V[i] * 1e-140;
	*eV += 140;
    }
    R_Free(B);
}

/* Generation from the Smirnov distribution. */

SEXP Smirnov_sim(SEXP sr, SEXP sc, SEXP sB, SEXP twosided)
{
    sr = PROTECT(coerceVector(sr, INTSXP));
    sc = PROTECT(coerceVector(sc, INTSXP));
    int nr = LENGTH(sr), nc = LENGTH(sc), B = asInteger(sB);
    if (nc != 2)
        error("Smirnov statistic only defined for two groups"); 
    int n = 0, *isr = INTEGER(sr);
    for (int i = 0; i < nr; i++) {
        /* avoid integer overflow */
        if (n > INT_MAX - isr[i]) 
            error("Sample size too large");
        n += isr[i];
    }
    int *observed = (int *) R_alloc(nr * nc, sizeof(int));
    double *fact = (double *) R_alloc(n+1, sizeof(double));
    int *jwork = (int *) R_alloc(nc, sizeof(int));
    SEXP ans = PROTECT(allocVector(REALSXP, B));
    Smirnov_sim_wrk(nr, nc, isr, INTEGER(sc), n, B, observed, 
		    INTEGER(twosided)[0], fact, jwork, REAL(ans));
    UNPROTECT(3);
    return ans;
}

static void
Smirnov_sim_wrk(int nrow, int ncol,
		const int nrowt[], const int ncolt[],
		int n, int B, int *observed, int twosided,
		double *fact, int *jwork, double *results)
{
    /* Calculate log-factorials.  fact[i] = lgamma(i+1) */
    fact[0] = fact[1] = 0.;
    for(int i = 2; i <= n; i++)
        fact[i] = fact[i - 1] + log(i);

    GetRNGstate();

    for(int iter = 0; iter < B; ++iter) {
        rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
        double S = 0., diff = 0.;
        int cs0 = 0, cs1 = 0;
        for (int j = 0; j < nrow; j++) {
            cs0 += observed[j];
            cs1 += observed[nrow + j];
            diff = ((double) cs0) / ncolt[0] - ((double) cs1) / ncolt[1];
            if (twosided) diff = fabs(diff);
            if (diff > S) S = diff;
        }
        results[iter] = S;
    }

    PutRNGstate();

    return;
}
