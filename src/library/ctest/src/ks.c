/* ks.c
   Compute the asymptotic distribution of the one- and two-sample
   two-sided Kolmogorov-Smirnov statistics, and the exact distribution
   in the two-sided two-sample case.
   */

#include <R.h>


#include "ctest.h"

static void
errmsg(char *s) {
    PROBLEM "%s", s RECOVER(NULL_ENTRY);
}

void
pkstwo(Sint *n, double *x, double *tol) {
    double new, old, s, z;
    Sint i, k;

    for(i = 0; i < *n; i++) {
	z = 2 * *x * *x;
	s = -1;
	k = 1;
	old = 0;
	new = 1;
	while(fabs(old - new) > *tol) {
	    old = new;
	    new += 2 * s / exp(z * k * k);
	    s *= -1;
	    k += 1;
	}
	x[i] = new;
    }
}

void
psmirnov2x(double *x, Sint *m, Sint *n) {
    double md, nd, *u, w;
    Sint i, j;

    if(*m > *n) {
	i = *n; *n = *m; *m = i;
    }
    md = (double) (*m);
    nd = (double) (*n);
    u = Calloc(*n + 1, double);
    if(!u)
	errmsg("allocation error in psmirnov2x().");
    for(j = 0; j <= *n; j++) {
	u[j] = ((j / nd) > *x) ? 0 : 1;
    }
    for(i = 1; i <= *m; i++) {
	w = (double)(i) / ((double)(i + *n));
	if((i / md) > *x)
	    u[0] = 0;
	else
	    u[0] = w * u[0];
	for(j = 1; j <= *n; j++) {
	    if(fabs(i / md - j / nd) > *x)
		u[j] = 0;
	    else
		u[j] = w * u[j] + u[j - 1];
	}
    }
    *x = u[*n];
}
