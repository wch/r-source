/*
  Mathlib : A C Library of Special Functions
  Copyright (C) 1999-2024  The R Core Team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at
  your option) any later version.

  This program is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, a copy is available at
  https://www.R-project.org/Licenses/

  SYNOPSIS

    #include <Rmath.h>
    double dwilcox(double x, double m, double n, int give_log)
    double pwilcox(double x, double m, double n, int lower_tail, int log_p)
    double qwilcox(double x, double m, double n, int lower_tail, int log_p);
    double rwilcox(double m, double n)

  DESCRIPTION

    dwilcox	The density of the Wilcoxon distribution.
    pwilcox	The distribution function of the Wilcoxon distribution.
    qwilcox	The quantile function of the Wilcoxon distribution.
    rwilcox	Random variates from the Wilcoxon distribution.

 */

/*
   Note: the checks here for R_CheckUserInterrupt also do stack checking.

   calloc/free are remapped for use in R, so allocation checks are done there.
   freeing is completed by an on.exit action in the R wrappers.

   The Wilcoxon distribution is calculated using work from Andreas Loeffler
   https://upload.wikimedia.org/wikipedia/commons/f/f5/LoefflerWilcoxonMannWhitneyTest.pdf
   https://upload.wikimedia.org/wikipedia/de/1/19/MannWhitney_151102.pdf
*/

#include <limits.h>

#include "nmath.h"
#include "dpq.h"

#ifndef MATHLIB_STANDALONE
#include <R_ext/Utils.h>
#endif

static double *w; /* to store (one half of) the Wilcoxon distribution */
static int *sigma;
static int allocated_m, allocated_n, max_k;

static int
cwilcox_sigma(int k, int m, int n) {
    /* this is used in w_fill_to_k to calculate w */
    int s=0, d, iter1, iter2;

    /* the factors of k must be at most k */
    iter1 = m < k ? m : k;
    iter2 = m+n < k ? m+n : k;
    for (d = 1; d <= iter1; d++) s += (k%d==0)*d;
    for (d = n+1; d <= iter2; d++) s -= (k%d==0)*d;
    return s;
}

static void
w_fill_to_k(int m, int n, int new_k)
{
    /*
     * lazily fill in the distribution up to new_k
     * store the last final index evaluated globally
     */
    if(new_k < max_k) return;

    int i, k;
    double s;

    /* fill in the values for sigma */
    for(i=max_k+1; i<=new_k; i++)
        sigma[i] = cwilcox_sigma(i, m, n);

    /* fill in the values for the distribution */
    for (k=max_k+1; k<=new_k; k++) {
        if (k==0){
            w[0]=1; /* by definition 0 has only 1 partition */
        } else {
            s = 0;
            for (i = 0; i<k; i++){
                /* recursion formula */
                s += w[i]*sigma[k-i];
            }
            w[k] = s/k;
        }
    }
    max_k = new_k;
    return;
}

static void
w_init_maybe(int m, int n)
{
    /* if the size is 0 just return, no need to recall cwilcox_sigma */
    if (m==0 || n==0) return;

    /* if we need to resize, first free the existing array */
    if ((w || sigma) && (m > allocated_m || n > allocated_n))
        wilcox_free();

    if (!w || !sigma) { /* initialize w[] */
        w = (double *) calloc(((size_t) m*n)/2+1, sizeof(double));
        sigma = (int *) calloc(((size_t) m*n)/2+1, sizeof(int));

#ifdef MATHLIB_STANDALONE
        if (!w || !sigma) MATHLIB_ERROR(_("wilcox allocation error %d"), 1);
#endif

	allocated_m = m; allocated_n = n;
        max_k = -1;
    }
}

#ifndef MATHLIB_STANDALONE
static int ic = 99999;
#endif
/* This counts the number of choices with statistic = k */
static double
cwilcox(int k, int m, int n)
{
    int c, i, j,
	u = m * n;
#ifndef MATHLIB_STANDALONE
    if (!ic--) {
	R_CheckUserInterrupt();
	ic = 99999;
    }
#endif
    if (k < 0 || k > u)
	return(0);
    c = (int)(u / 2);
    if (k > c)
	k = u - k; /* hence  k < floor(u / 2) */
    if (m < n) {
	i = m; j = n;
    } else {
	i = n; j = m;
    } /* hence  i <= j */

    /* if any of these values are 0 we return k==0 */
    if (i == 0 || j == 0 || k == 0) return (k == 0);

    /*
     * previous iterations called cwilcox(k, i, k) here if j>0 and k<j
     * however, I'm intentionally not doing that since `sigma` and `w` are
     * cached for fixed values of m,n. Swapping m,n will force a realloc
     * and lots of additional w_fill_to_k() calls.
    */

    /* initialize space for the distribution if it doesn't yet exist */
    if(!w || !sigma || i != allocated_m || j != allocated_n)
        w_init_maybe(i,j);

    /* fill in values up to k (caching results) */
    w_fill_to_k(i,j,k);

    return w[k];
}

double dwilcox(double x, double m, double n, int give_log)
{
#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
#endif
    m = R_forceint(m);
    n = R_forceint(n);
    if (m <= 0 || n <= 0)
	ML_WARN_return_NAN;

    if (R_nonint(x))
	return(R_D__0);
    x = R_forceint(x);
    if ((x < 0) || (x > m * n))
	return(R_D__0);

    if (m > INT_MAX)
	MATHLIB_ERROR("m(%g) > INT_MAX", m);
    if (n > INT_MAX)
	MATHLIB_ERROR("n(%g) > INT_MAX", n);
    if (x > INT_MAX)
	MATHLIB_ERROR("x(%g) > INT_MAX", x);

    int mm = (int) m, nn = (int) n, xx = (int) x;
    double d = give_log ?
	log(cwilcox(xx, mm, nn)) - lchoose(m + n, n) :
	    cwilcox(xx, mm, nn)  /  choose(m + n, n);

    return(d);
}

/* args have the same meaning as R function pwilcox */
double pwilcox(double q, double m, double n, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(q) || ISNAN(m) || ISNAN(n))
	return(q + m + n);
#endif
    if (!R_FINITE(m) || !R_FINITE(n))
	ML_WARN_return_NAN;
    m = R_forceint(m);
    n = R_forceint(n);
    if (m <= 0 || n <= 0)
	ML_WARN_return_NAN;

    q = floor(q + 1e-7);

    if (q < 0.0)
	return(R_DT_0);
    if (q >= m * n)
	return(R_DT_1);

    if (m > INT_MAX)
	MATHLIB_ERROR("m(%g) > INT_MAX", m);
    if (n > INT_MAX)
	MATHLIB_ERROR("n(%g) > INT_MAX", n);
    int mm = (int) m, nn = (int) n;
    double c = choose(m + n, n),
	p = 0;
    /* Use summation of probs over the shorter range */
    if (q <= (m * n / 2)) {
	for (int i = 0; i <= q; i++)
	    p += cwilcox(i, mm, nn) / c;
    }
    else {
	q = m * n - q;
	for (int i = 0; i < q; i++)
	    p += cwilcox(i, mm, nn) / c;
	lower_tail = !lower_tail; /* p = 1 - p; */
    }

    return(R_DT_val(p));
} /* pwilcox */

/* x is 'p' in R function qwilcox */

double qwilcox(double x, double m, double n, int lower_tail, int log_p)
{
#ifdef IEEE_754
    if (ISNAN(x) || ISNAN(m) || ISNAN(n))
	return(x + m + n);
#endif
    if(!R_FINITE(x) || !R_FINITE(m) || !R_FINITE(n))
	ML_WARN_return_NAN;
    R_Q_P01_check(x);

    m = R_forceint(m);
    n = R_forceint(n);
    if (m <= 0 || n <= 0)
	ML_WARN_return_NAN;

    if (x == R_DT_0)
	return(0);
    if (x == R_DT_1)
	return(m * n);

    if(log_p || !lower_tail)
	x = R_DT_qIv(x); /* lower_tail,non-log "p" */

    if (m > INT_MAX)
	MATHLIB_ERROR("m(%g) > INT_MAX", m);
    if (n > INT_MAX)
	MATHLIB_ERROR("n(%g) > INT_MAX", n);
    int mm = (int) m, nn = (int) n;
    double c = choose(m + n, n),
	p = 0.;
    int q = 0;
    if (x <= 0.5) {
	x = x - 10 * DBL_EPSILON;
	for (;;) {
	    p += cwilcox(q, mm, nn) / c;
	    if (p >= x)
		break;
	    q++;
	}
    }
    else {
	x = 1 - x + 10 * DBL_EPSILON;
	for (;;) {
	    p += cwilcox(q, mm, nn) / c;
	    if (p > x) {
		q = (int) (m * n - q);
		break;
	    }
	    q++;
	}
    }

    return(q);
}

double rwilcox(double m, double n)
{
    int i, j, k, *x;
    double r;

#ifdef IEEE_754
    /* NaNs propagated correctly */
    if (ISNAN(m) || ISNAN(n))
	return(m + n);
#endif
    m = R_forceint(m);
    n = R_forceint(n);
    if ((m < 0) || (n < 0))
	ML_WARN_return_NAN;

    if ((m == 0) || (n == 0))
	return(0);

    r = 0.0;
    if ((m + n) > INT_MAX)
	MATHLIB_ERROR("m+n(%g) > INT_MAX", m + n);
    k = (int) (m + n);
    x = (int *) calloc((size_t) k, sizeof(int));
#ifdef MATHLIB_STANDALONE
    if (!x) MATHLIB_ERROR(_("wilcox allocation error %d"), 4);
#endif
    for (i = 0; i < k; i++)
	x[i] = i;
    for (i = 0; i < n; i++) {
	j = (int) R_unif_index(k);
	r += x[j];
	x[j] = x[--k];
    }
    free(x);
    return(r - n * (n - 1) / 2);
}

void wilcox_free(void)
{
    free(w);
    free(sigma);
    w = NULL; sigma = NULL;
    allocated_m = allocated_n = 0;
    max_k = -1;
}
