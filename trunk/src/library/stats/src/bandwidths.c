/*
 *  bandwidth.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-2001
 */

#include <math.h>

#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#  define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#define abs9(a) (a > 0 ? a:-a)


#if !defined(PI)
#  define PI 3.14159265
#endif

#define DELMAX 1000
/* Avoid slow and possibly error-producing underflows by cutting off at
   plus/minus sqrt(DELMAX) std deviations */
/* Formulae (6.67) and (6.69) of Scott (1992), the latter corrected. */

void
band_ucv_bin(int *n, int *nb, double *d, int *x, double *h, double *u)
{
    int   i, nn = *n, nbin = *nb;
    double delta, hh = (*h), sum, term;

    sum = 0.0;
    for (i = 0; i < nbin; i++) {
	delta = i * (*d) / hh;
	delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 4) - sqrt(8.0) * exp(-delta / 2);
	sum += term * x[i];
    }
    *u = 1 / (2 * nn * hh * sqrt(PI)) + sum / (nn * nn * hh * sqrt(PI));
}

void
band_bcv_bin(int *n, int *nb, double *d, int *x, double *h, double *u)
{
    int   i, nn = *n, nbin = *nb;
    double delta, hh = (*h), sum, term;

    sum = 0.0;
    for (i = 0; i < nbin; i++) {
	delta = i * (*d) / hh;
	delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 4) * (delta * delta - 12 * delta + 12);
	sum += term * x[i];
    }
    *u = 1 / (2 * nn * hh * sqrt(PI)) + sum / (64 * nn * nn * hh * sqrt(PI));
}


void
band_phi4_bin(int *n, int *nb, double *d, int *x, double *h, double *u)
{
    int   i, nn = *n, nbin = *nb;
    double delta, sum, term;

    sum = 0.0;
    for (i = 0; i < nbin; i++) {
	delta = i * (*d) / (*h);
	delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 2) * (delta * delta - 6 * delta + 3);
	sum += term * x[i];
    }
    sum = 2 * sum + nn * 3;	/* add in diagonal */
    *u = sum / (nn * (nn - 1) * pow(*h, 5.0) * sqrt(2 * PI));
}

void
band_phi6_bin(int *n, int *nb, double *d, int *x, double *h, double *u)
{
    int   i, nn = *n, nbin = *nb;
    double delta, sum, term;

    sum = 0.0;
    for (i = 0; i < nbin; i++) {
	delta = i * (*d) / (*h);
	delta *= delta;
	if (delta >= DELMAX) break;
	term = exp(-delta / 2) *
	    (delta * delta * delta - 15 * delta * delta + 45 * delta - 15);
	sum += term * x[i];
    }
    sum = 2 * sum - 15 * nn;	/* add in diagonal */
    *u = sum / (nn * (nn - 1) * pow(*h, 7.0) * sqrt(2 * PI));
}

void
band_den_bin(int *n, int *nb, double *d, double *x, int *cnt)
{
    int   i, j, ii, jj, iij, nn = *n;
    double xmin, xmax, rang, dd;

    for (i = 0; i < *nb; i++) cnt[i] = 0;
    xmin = xmax = x[0];
    for (i = 1; i < nn; i++) {
	xmin = min(xmin, x[i]);
	xmax = max(xmax, x[i]);
    }
    rang = (xmax - xmin) * 1.01;
    *d = dd = rang / (*nb);
    for (i = 1; i < nn; i++) {
	ii = x[i] / dd;
	for (j = 0; j < i; j++) {
	    jj = x[j] / dd;
	    iij = abs9((ii - jj));
	    cnt[iij]++;
	}
    }
}
