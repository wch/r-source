/*
 *  modreg/src/ksmooth.c by B. D. Ripley  Copyright (C) 1998
 */

#include <R.h>			/* for NA_REAL, includes math.h */

static double dokern(double x, int kern)
{
    if(kern == 1) return(1.0);
    if(kern == 2) return(exp(-0.5*x*x));
    return(0.0); /* -Wall */
}

void BDRksmooth(double *x, double *y, int *n,
		double *xp, double *yp, int *np,
		int *kern, double *bandwidth)
{
    int i, imin=0, j;
    double cutoff=0.0, num, den, x0, w, bw=*bandwidth;

    /* bandwidth is in units of half inter-quartile range. */
    if(*kern == 1) {bw *= 0.5; cutoff = bw;}
    if(*kern == 2) {bw *= 0.3706506; cutoff = 4*bw;}
    while(x[imin] < xp[0] - cutoff && imin < *n) imin++;
    for(j = 0; j < *np; j++) {
	num = den = 0.0;
	x0 = xp[j];
	for(i = imin; i < *n; i++) {
	    if(x[i] < x0 - cutoff) imin = i;
	    else {
		if(x[i] > x0 + cutoff) break;
		w = dokern(fabs(x[i] - x0)/bw, *kern);
		num += w*y[i];
		den += w;
	    }
	}
	if(den > 0) yp[j] = num/den; else yp[j] = NA_REAL;
    }
}


void F77_SUB(bdrsplerr)(void)
{
    error("only 2500 rows are allowed for sm.method=\"spline\"");
}
