/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-9	B. D. Ripley
 *  Copyright (C) 1999          R Development Core Team
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 * Exports
 *	lqs_fitlots(...)
 *	mve_fitlots(...)
 *
 * to be called as  .C(.)  in ../R/lqs.R
 */

/* in R.h
#include <math.h>
#include <limits.h> */

#include <R.h>
#include "R_ext/Applic.h"	/* for the QR	  routines */
#include "R_ext/Utils.h"	/* for the *sort() routines */
#define BIG DBL_MAX

#ifdef WIN32
extern void R_ProcessEvents(void);
#endif

/* GLOBAL Variables, explicitly allocated and freed: */
static double *coef, *qraux, *work, *res, *yr, *xr, *means, *d2, *d2copy;
static int *pivot, *which, *which2;
static int *ind;

static void lqs_setup(int *n, int *p, int *nwhich)
{
    coef = (double *) R_alloc(*p, sizeof(double));
    qraux = (double *) R_alloc(*p, sizeof(double));
    work = (double *) R_alloc(2*(*p), sizeof(double));
    res = (double *) R_alloc(*n, sizeof(double));
    yr = (double *) R_alloc(*n, sizeof(double));
    xr = (double *) R_alloc((*n)*(*p), sizeof(double));
    pivot = (int *) R_alloc(*p, sizeof(int));
    ind = (int *) R_alloc(*n, sizeof(int));
    which = (int *) R_alloc(*nwhich, sizeof(int));
    /*bestone = R_alloc(*nwhich, int);*/
}


/*
   Sampling k from 0:n-1 without replacement.
 */
static void sample_noreplace(int *x, int n, int k)
{
    int i, j, nn=n;

    for (i = 0; i < n; i++) ind[i] = i;
    for (i = 0; i < k; i++) {
	j = nn * unif_rand();
	x[i] = ind[j];
	ind[j] = ind[--nn];
    }
}

/*
   Find all subsets of size k in order: this gets a new one each call
 */
static void next_set(int *x, int n, int k)
{
    int i, j, tmp;

    j = k - 1;
    tmp = x[j]++;
    while(j > 0 && x[j] >= n - (k - 1 -j)) tmp = ++x[--j];
    for(i = j+1; i < k; i++)  x[i] =  ++tmp;
}


/*
   Adjust the constant for an LMS fit. This is the midpoint of the
   qn contiguous observations of shortest length.
 */
static double lmsadj(double *x, int n, int qn, double *ssbest)
{
    int i, k = qn - 1;
    double len, best, adj;

    best = x[k] - x[0];
    adj = 0.5*(x[k] + x[0]);
    for(i = 1; i < n-k; i++){
	len = x[i+k] - x[i];
	if(len < best) {
	    best = len;
	    adj = 0.5*(x[i+k] + x[i]);
	}
    }
    *ssbest = 0.25*best*best;
    return(adj);
}

/*
   Adjust the constant for an LTS fit. This is the mean of the
   qn contiguous observations of smallest variance.
 */
static double ltsadj(double *x, int n, int qn, double *ssbest)
{
    int i, k = qn - 1;
    double ss, best, m1, m2, adj;

    /*printf("qn = %d\n", qn);*/
    m1 = m2 = 0.0;
    for(i=0; i < qn; i++) {
	m1 += x[i];
	m2 += x[i]*x[i];
    }
    adj = m1/qn;
    best = m2 - m1*m1/qn;

    for(i = 1; i < n-k; i++){
	m1 += x[i+k] - x[i-1];
	m2 += x[i+k]*x[i+k] - x[i-1]*x[i-1];
	ss = m2 - m1*m1/qn;
	if(ss < best) {
	    best = ss;
	    adj = m1/qn;
	}
    }
    *ssbest = best;
    return(adj);
}

/* the chi function for the S estimator: the integral of biweight */
static double chi(double x, double a)
{
    x /= a; x *= x;
    if(x > 1) return(1.0);
    else return(x*(3 + x*(-3 + x)));
}

/*
   For lots of subsets of size *nwhich, compute the exact fit to those
   data points and the residuals from all the data points.
 */
void
lqs_fitlots(double *x, double *y, int *n, int *p, int *qn,
	    int *lts, int *adj, int *sample, int *nwhich,
	    int *ntrials, double *crit, int *sing, int *bestone,
	    double *bestcoef, double *pk0, double *beta)
{
    int nnew = *nwhich, pp = *p;
    int i, iter, j, k,  nn = *n, this, trial;
    int rank, info, n100 = 100;
    int firsttrial = 1;
    double a = 0.0, tol = 1.0e-7, sum, thiscrit, best = BIG, target,
	old, new, dummy, k0 = *pk0;

    lqs_setup(n, p, nwhich);

    *sing = 0;
    target = (nn - pp)* (*beta);

    if(!(*sample)) {
	for(i = 0; i < nnew; i++) which[i] = i;
    } else GetRNGstate();

    for(trial = 0; trial < *ntrials; trial++) {

	/* check for a user interrupt */
#ifdef Macintosh
	if(trial % 10) isintrpt();
#endif
#ifdef WIN32
	if(trial % 10) R_ProcessEvents();
#endif

	if(!(*sample)) {if(trial > 0) next_set(which, nn, nnew);}
	else sample_noreplace(which, nn, nnew);

	for(j = 0; j < nnew; j++) {
	    this = which[j];
	    yr[j] = y[this];
	    for(k = 0; k < pp; k++) xr[j + nnew*k] = x[this + nn*k];
	}

	/* compute fit, find residuals */
	F77_CALL(dqrdc2)(xr, &nnew, &nnew, &pp, &tol, &rank,
			 qraux, pivot, work);
	if(rank < pp) { (*sing)++; continue; }
	F77_CALL(dqrsl)(xr, &nnew, &nnew, &rank, qraux, yr, &dummy, yr,
			coef, &dummy, &dummy, &n100, &info);

	for(i = 0; i < nn; i++) {
	    sum = y[i];
	    for(j = 0; j < rank; j++) sum -= coef[j] * x[i + nn*j];
	    res[i] = sum;
	}

	if(*lts < 2) {/* lqs or lts estimation */
	    /* find the constant subtracted from the residuals that minimizes
	       the criterion. As this is a univariate problem, has an exact
	       solution.  */
	    if(*adj) {
		R_rsort(res, nn);
		if(*lts) a = ltsadj(res, nn, *qn, &thiscrit);
		else a = lmsadj(res, nn, *qn, &thiscrit);
	    } else {
		for(i = 0; i < nn; i++) {
		    sum = res[i] - a;
		    res[i] = sum*sum;
		}
		rPsort(res, nn, *qn-1); /* partial sort */
		if(!(*lts)) thiscrit = res[*qn-1];
		else {
		    sum = 0.0;
		    for(i = 0; i < *qn; i++) sum += res[i];
		    thiscrit = sum;
		}
	    }
	} else { /* S estimation */
	    if(firsttrial) {
		for(i = 0; i < nn; i ++) res[i] = fabs(res[i]);
		rPsort(res, nn, nn/2);
		old = res[nn/2]/0.6745;	 /* MAD provides the initial scale */
		firsttrial = 0;
	    } else {
		/* only find optimal scale if it will be better than
		   existing best solution */
		sum = 0.0;
		for(i = 0; i < nn; i ++) sum += chi(res[i], k0 * best);
		if(sum > target) continue;
		old = best;
	    } /* now solve for scale S by re-substitution */
	    for(iter = 0; iter < 30; iter++) {
		/*printf("iter %d, s = %f sum = %f %f\n", iter, old, sum, target);*/
		sum = 0.0;
		for(i = 0; i < nn; i ++) sum += chi(res[i], k0 * old);
		new = sqrt(sum/target) * old;
		if(fabs(sum/target - 1.) < 1e-4) break;
		old = new;
	    }
	    thiscrit = new;
	}

	if(thiscrit < best) {  /* first trial might be singular, so use fence */
	    sum = 0.0;
	    for(i = 0; i < nn; i ++) sum += chi(res[i], k0 * best);
	    best = thiscrit;
	    /* printf("trial %d, best = %f sum = %f %f\n", trial, best, sum, target);*/
	    for(i = 0; i < nnew; i++) bestone[i] = which[i] + 1;
	    for(i = 0; i < pp; i++) bestcoef[i] = coef[i];
	    bestcoef[0] += a;
	}
    }/* for(trial in 0:ntrials) */

    *crit = (best < 0.0) ? 0.0 : best;
    if(*sample) PutRNGstate();
    /* lqs_free(); */
}


static void mve_setup(int *n, int *p, int *ps)
{
    xr = (double *) R_alloc((*ps)*(*p), sizeof(double));
    qraux = (double *) R_alloc(*p, sizeof(double));
    pivot = (int *) R_alloc(*p, sizeof(int));
    work = (double *) R_alloc(2*(*p), sizeof(double));
    d2 = (double *) R_alloc(*n, sizeof(double));
    d2copy = (double *) R_alloc(*n, sizeof(double));
    means = (double *) R_alloc((*p), sizeof(double));
    ind = (int *) R_alloc(*n, sizeof(int));
    which = (int *) R_alloc(*ps, sizeof(int));
    which2 = (int *) R_alloc(*ps, sizeof(int));
}


/* find the squared Mahalanobis distance to x via QR decomposition in xr. */
static double mah(double *xr, int nnew, int p, double *x)
{
    int i, j;
    double s, ss = 0.0;

    for(j = 0; j < p; j++) {
	s = x[j];
	if(j > 0) for(i = 0; i < j; i++) s -= work[i] * xr[i + nnew*j];
	work[j] = s / xr[j + nnew*j];
	ss += work[j] * work[j];
    }
    return(ss*(nnew-1));
}

/*
   Compute the squared Mahalanobis distances, in d2, to all points in x
   from the mean of the subset in which using the covariance of that
   subset.
*/
static int do_one(double *x, int *which, int n, int nnew, int p,
       double *det, double *d2)
{
    int i, j, k;
    int rank;
    double sum, tol = 1.0e-7;

    for(j = 0; j < nnew; j++)
	for(k = 0; k < p; k++) xr[j + nnew*k] = x[which[j] + n*k];
    for(k = 0; k < p; k++) {
	sum = 0.0;
	for(j = 0; j < nnew; j++) sum += xr[j + nnew*k];
	sum /= nnew;
	means[k] = sum;
	for(j = 0; j < nnew; j++) xr[j + nnew*k] -= sum;
    }

    F77_CALL(dqrdc2)(xr, &nnew, &nnew, &p, &tol, &rank, qraux, pivot, work);
    if(rank < p) return(1);

    sum = 0.0;
    for(k = 0; k < p; k++)
	sum += log(fabs(xr[k + nnew*k]));
    *det = sum;

    /* now solve R^T b = (x[i, ] - means) and find squared length of b */
    for(i = 0; i < n; i++) {
	for(j = 0; j < p; j++) qraux[j] = x[i + n*j] - means[j];
	d2[i] = mah(xr, nnew, p, qraux);
    }
    return(0);
}


void
mve_fitlots(double *x, int *n, int *p, int *qn, int *mcd,
	    int *sample, int *nwhich, int *ntrials,
	    double *crit, int *sing, int *bestone)
{
    int i, iter, j, nn = *n, quan = *qn, trial, this_sing;
    int nnew = *nwhich;
    double det, best = BIG, thiscrit, lim;

    if(*mcd != 1)
	mve_setup(n, p, nwhich);
    else
	mve_setup(n, p, n); /* could get ties */

    *sing = 0;
    if(!*sample) {
	for(i = 0; i < nnew; i++) which[i] = i;
    } else GetRNGstate();

    thiscrit = 0.0;		/* -Wall */

    for(trial = 0; trial < *ntrials; trial++) {

	/* check for a user interrupt */
#ifdef Macintosh
	if(trial % 10) isintrpt();
#endif
#ifdef WIN32
	if(trial % 10) R_ProcessEvents();
#endif

	if(!(*sample)) {if(trial > 0) next_set(which, nn, nnew);}
	else sample_noreplace(which, nn, nnew);

	/* for(i = 0; i < nnew; i++) printf("%d ", which[i]); printf("\n");
	   fflush(stdout);*/


	/* Find the mean and covariance matrix of the sample. Check if singular.
	   Compute Mahalanobis distances of all points to the means using
	   this covariance matrix V, and find quantile largest. Volume is
	   then monotone in determinant(V * dist2). */

	this_sing = do_one(x, which, nn, nnew, *p, &det, d2);
	if(this_sing)  {(*sing)++; continue;}

	/*for(i = 0; i < nnew; i++) printf(" %d", which[i]); printf("\n");*/

	for(i = 0; i < nn; i++) d2copy[i] = d2[i];
	rPsort(d2copy, nn, quan-1);
	lim = d2copy[*qn-1];
	if(!*mcd) thiscrit = (*p) * log(lim) + 2*det;
	else {
	    for(iter = 0; iter < 4; iter++) {
		/*for(i = 0; i < nn; i++) printf(" %f", d2[i]); printf("\n");*/
		if(iter > 0) {
		    for(i = 0; i < nn; i++) d2copy[i] = d2[i];
		    rPsort(d2copy, nn, quan-1);
		    lim = d2copy[*qn-1];
		}
		j = 0;
		for(i = 0; i < nn; i++)
		    if(d2[i] <= lim) which2[j++] = i;
		/* note: we take all points that meet this limit:
		   there could be more than quan. */
		(void) do_one(x, which2, nn, quan, *p, &det, d2);
		if(iter > 0 && 2 * det >= 0.999*thiscrit) break;
		thiscrit = 2 * det;
		/* printf("iter %d %f", iter, thiscrit);
		   for(i = 0; i < quan; i++) printf(" %d", which2[i]);
		   printf("\n"); fflush(stdout);*/
	    }

	}
	/*   printf("this %f\n", thiscrit);*/


	if(thiscrit < best) { /* warning: first might be singular */
	    best = thiscrit;
	    for(i = 0; i < nn; i++) bestone[i] = (d2[i] <= lim);
	}
    }
    *crit = best;
    if(*sample) PutRNGstate();
    /* mve_free(); */
}

#include "Rinternals.h"
#include "R_ext/Rdynload.h"

static R_CMethodDef R_CDef[] = {
   {"lqs_fitlots", (DL_FUNC)&lqs_fitlots, 16},
   {"mve_fitlots", (DL_FUNC)&mve_fitlots, 11},
   {NULL, NULL, 0},
};

void
R_init_lqs(DllInfo *info)
{
    R_registerRoutines(info, R_CDef, NULL, NULL, NULL);
}

