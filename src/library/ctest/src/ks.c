/* ks.c
   Compute the asymptotic and eventually hopefully also the exact
   distribution(s) of the Kolmogorov-Smirnov statistics.
   */

#include <R.h>

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
