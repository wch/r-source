/* ks.c
   Compute the asymptotic distribution of the one- and two-sample
   two-sided Kolmogorov-Smirnov statistics, and the exact distribution
   in the two-sided two-sample case.
   */

#include <R.h>

#include "ctest.h"

void
pkstwo(Sint *n, double *x, double *tol)
{
/* x[1:n] is input and output
 *
 * res_i := \sum_{k = -\infty}^\infty  (-1)^k e^{-2 k^2 {x_i}^2}
 *        = 1 + 2 \sum_{k = 1}^\infty  (-1)^k e^{-2 k^2 {x_i}^2}
 */
    double new, old, s, z;
    Sint i, k;

    for(i = 0; i < *n; i++) {
	z = -2 * x[i] * x[i];
	s = -1;
	k = 1;
	old = 0;
	new = 1;
	while(fabs(old - new) > *tol) {
	    old = new;
	    new += 2 * s * exp(z * k * k);
	    s *= -1;
	    k++;
	}
	x[i] = new;
    }
}

void
psmirnov2x(double *x, Sint *m, Sint *n)
{
    double md, nd, q, *u, w;
    Sint i, j;

    if(*m > *n) {
	i = *n; *n = *m; *m = i;
    }
    md = (double) (*m);
    nd = (double) (*n);
    q = floor(*x * md * nd - 1e-7) / (md * nd);
    u = (double *) R_alloc(*n + 1, sizeof(double));

    /* not needed, as R_alloc does not return if it fails 
    if(!u) error("allocation error in psmirnov2x()."); */

    for(j = 0; j <= *n; j++) {
	u[j] = ((j / nd) > q) ? 0 : 1;
    }
    for(i = 1; i <= *m; i++) {
	w = (double)(i) / ((double)(i + *n));
	if((i / md) > q)
	    u[0] = 0;
	else
	    u[0] = w * u[0];
	for(j = 1; j <= *n; j++) {
	    if(fabs(i / md - j / nd) > q)
		u[j] = 0;
	    else
		u[j] = w * u[j] + u[j - 1];
	}
    }
    *x = u[*n];
}
