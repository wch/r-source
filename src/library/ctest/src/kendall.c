/* kendall.c
   Compute Kendall's rank correlation tau and the exact distribution of
   T = (n * (n - 1) * tau + 1) / 4, the number of concordant ordered
   pairs.
   */

#include <R.h>
#include <Rmath.h>

#include "ctest.h"

void
kendall_tau(Sint *n, double *x, double *y, double *tau) {
    double c = 0, vx = 0, vy = 0, sx, sy;
    int i, j;

    for(i = 0; i < *n; i++) {
	for(j = 0; j < i; j++) {
	    sx = sign(x[i] - x[j]);
	    sy = sign(y[i] - y[j]);
	    vx += sx * sx;
	    vy += sy * sy;
	    c += sx * sy;
	}
    }

    *tau = c / (sqrt(vx) * sqrt(vy));
}

static double
ckendall(int k, int n, double **w) {
    int i, u;
    double s;

    u = (Sint) (n * (n - 1) / 2);
    if ((k < 0) || (k > u))
	return(0);
    if (w[n] == 0) {
	w[n] = (double *) R_alloc(u + 1, sizeof(double));
	if(!w[n])
	    error("allocation error in ckendall().");
	memset(w[n], '\0', sizeof(double) * (u+1));
	for (i = 0; i <= u; i++)
	    w[n][i] = -1;
    }
    if (w[n][k] < 0) {
	if (n == 1)
	    w[n][k] = (k == 0);
	else {
	    s = 0;
	    for (i = 0; i < n; i++)
		s += ckendall(k - i, n - 1, w);
	    w[n][k] = s;
	}
    }
    return(w[n][k]);
}

#if 0
void
dkendall(Sint *len, double *x, Sint *n) {
    Sint i;
    double **w;

    w = (double **) R_alloc(*n + 1, sizeof(double *));
    if (!w)
	error("allocation error in dkendall().");

    for (i = 0; i < *len; i++)
	if (fabs(x[i] - floor(x[i] + 0.5)) > 1e-7) {
	    x[i] = 0;
	} else {
	    x[i] = ckendall((Sint)x[i], (Sint)*n) / gammafn(*n + 1, w);
	}
}
#endif

void
pkendall(Sint *len, double *x, Sint *n) {
    Sint i, j;
    double p, q;
    double **w;

    w = (double **) R_alloc(*n + 1, sizeof(double *));
    if(!w)
	error("allocation error in pkendall().");
    memset(w, '\0', sizeof(double*) * (*n+1));

    for (i = 0; i < *len; i++) {
	q = floor(x[i] + 1e-7);
	if (q < 0)
	    x[i] = 0;
	else if (q > (*n * (*n - 1) / 2))
	    x[i] = 1;
	else {
	    p = 0;
	    for (j = 0; j <= q; j++) {
		p += ckendall((Sint)j, (Sint)*n, w);
	    }
	    x[i] = p / gammafn(*n + 1);
	}
    }
}
