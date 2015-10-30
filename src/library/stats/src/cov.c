/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2015	The R Core Team
 *  Copyright (C) 2003		The R Foundation
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_LONG_DOUBLE
# define SQRTL sqrtl
#else
# define SQRTL sqrt
#endif

#include <Defn.h>
#include <Rmath.h>

#include "statsR.h"
#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

static SEXP corcov(SEXP x, SEXP y, SEXP na_method, SEXP kendall, Rboolean cor);


SEXP cor(SEXP x, SEXP y, SEXP na_method, SEXP kendall)
{
    return corcov(x, y, na_method, kendall, TRUE);
}
SEXP cov(SEXP x, SEXP y, SEXP na_method, SEXP kendall)
{
    return corcov(x, y, na_method, kendall, FALSE);
}



#define COV_SUM_UPDATE				\
		    sum += xm * ym;		\
		    if(cor) {			\
			xsd += xm * xm;		\
			ysd += ym * ym;		\
		    }

#define ANS(I,J)  ans[I + J * ncx]

/* Note that "if (kendall)" and	 "if (cor)" are used inside a double for() loop;
   which makes the code better readable -- and is hopefully dealt with
   by a smartly optimizing compiler
*/

/** Compute   Cov(xx[], yy[])  or  Cor(.,.)  with n = length(xx)
 */
#define COV_PAIRWISE_BODY						\
	LDOUBLE sum, xmean = 0., ymean = 0., xsd, ysd, xm, ym;	\
        int k, nobs, n1 = -1;	/* -Wall initializing */		\
									\
	    nobs = 0;							\
	    if(!kendall) {						\
		xmean = ymean = 0.;					\
		for (k = 0 ; k < n ; k++) {				\
		    if(!(ISNAN(xx[k]) || ISNAN(yy[k]))) {		\
			nobs ++;					\
			xmean += xx[k];					\
			ymean += yy[k];					\
		    }							\
		}							\
	    } else /*kendall*/						\
		for (k = 0 ; k < n ; k++)				\
		    if(!(ISNAN(xx[k]) || ISNAN(yy[k])))			\
			nobs ++;					\
									\
	    if (nobs >= 2) {						\
		xsd = ysd = sum = 0.;					\
		if(!kendall) {						\
		    xmean /= nobs;					\
		    ymean /= nobs;					\
		    n1 = nobs-1;					\
		}							\
		for(k=0; k < n; k++) {					\
		    if(!(ISNAN(xx[k]) || ISNAN(yy[k]))) {		\
			if(!kendall) {					\
			    xm = xx[k] - xmean;				\
			    ym = yy[k] - ymean;				\
									\
			    COV_SUM_UPDATE				\
			}						\
			else { /* Kendall's tau */			\
			    for(n1=0 ; n1 < k ; n1++)			\
				if(!(ISNAN(xx[n1]) || ISNAN(yy[n1]))) {	\
				    xm = sign(xx[k] - xx[n1]);		\
				    ym = sign(yy[k] - yy[n1]);		\
									\
				    COV_SUM_UPDATE			\
				}					\
			}						\
		    }							\
		}							\
		if (cor) {						\
		    if(xsd == 0. || ysd == 0.) {			\
			*sd_0 = TRUE;					\
			sum = NA_REAL;					\
		    }							\
		    else {						\
			if(!kendall) {					\
			    xsd /= n1;					\
			    ysd /= n1;					\
			    sum /= n1;					\
			}						\
			sum /= (SQRTL(xsd) * SQRTL(ysd));	       	\
			if(sum > 1.) sum = 1.;				\
		    }							\
		}							\
		else if(!kendall)					\
		    sum /= n1;						\
									\
		ANS(i,j) = (double) sum;       				\
	    }								\
	    else							\
		ANS(i,j) = NA_REAL


static void cov_pairwise1(int n, int ncx, double *x,
			  double *ans, Rboolean *sd_0, Rboolean cor,
			  Rboolean kendall)
{
    for (int i = 0 ; i < ncx ; i++) {
	double *xx = &x[i * n];
	for (int j = 0 ; j <= i ; j++) {
	    double *yy = &x[j * n];

	    COV_PAIRWISE_BODY;

	    ANS(j,i) = ANS(i,j);
	}
    }
}

static void cov_pairwise2(int n, int ncx, int ncy, double *x, double *y,
			  double *ans, Rboolean *sd_0, Rboolean cor,
			  Rboolean kendall)
{
    for (int i = 0 ; i < ncx ; i++) {
	double *xx = &x[i * n];
	for (int j = 0 ; j < ncy ; j++) {
	    double *yy = &y[j * n];

	    COV_PAIRWISE_BODY;
	}
    }
}
#undef COV_PAIRWISE_BODY


/* method = "complete" or "all.obs" (only difference: na_fail):
 *           --------      -------
*/
#define COV_ini_0				\
    LDOUBLE sum, tmp, xxm, yym;			\
    double *xx, *yy;				\
    int i, j, k, n1=-1/* -Wall */

#define COV_n_le_1(_n_,_k_)			\
    if (_n_ <= 1) {/* too many missing */	\
	for (i = 0 ; i < ncx ; i++)		\
	    for (j = 0 ; j < _k_ ; j++)		\
		ANS(i,j) = NA_REAL;		\
	return;					\
    }

#define COV_init(_ny_)				\
    COV_ini_0; int nobs;			\
						\
    /* total number of complete observations */	\
    nobs = 0;					\
    for(k = 0 ; k < n ; k++) {			\
	if (ind[k] != 0) nobs++;		\
    }						\
    COV_n_le_1(nobs, _ny_)

#define COV_ini_na(_ny_)			\
    COV_ini_0; 					\
    COV_n_le_1(n, _ny_)


/* This uses two passes for better accuracy */
#define MEAN(_X_)				\
    /* variable means */			\
    for (i = 0 ; i < nc##_X_ ; i++) {		\
	xx = &_X_[i * n];			\
	sum = 0.;				\
	for (k = 0 ; k < n ; k++)		\
	    if(ind[k] != 0)			\
		sum += xx[k];			\
	tmp = sum / nobs;			\
	if(R_FINITE((double)tmp)) {		\
	    sum = 0.;				\
	    for (k = 0 ; k < n ; k++)		\
		if(ind[k] != 0)			\
		    sum += (xx[k] - tmp);	\
	     tmp = tmp + sum / nobs;		\
	}					\
	_X_##m [i] = (double)tmp;		\
    }

/* This uses two passes for better accuracy */
#define MEAN_(_X_,_HAS_NA_)			\
    /* variable means (has_na) */		\
    for (i = 0 ; i < nc##_X_ ; i++) {		\
	if(_HAS_NA_[i])				\
	    tmp = NA_REAL;			\
	else {					\
	    xx = &_X_[i * n];			\
	    sum = 0.;				\
	    for (k = 0 ; k < n ; k++)		\
		sum += xx[k];			\
	    tmp = sum / n;			\
	    if(R_FINITE((double)tmp)) {		\
		sum = 0.;			\
		for (k = 0 ; k < n ; k++)	\
		    sum += (xx[k] - tmp);	\
		tmp = tmp + sum / n;		\
	    }					\
	}					\
	_X_##m [i] = (double)tmp;		\
    }


static void
cov_complete1(int n, int ncx, double *x, double *xm,
	      int *ind, double *ans, Rboolean *sd_0, Rboolean cor,
	      Rboolean kendall)
{
    COV_init(ncx);

    if(!kendall) {
	MEAN(x);/* -> xm[] */
	n1 = nobs - 1;
    }
    for (i = 0 ; i < ncx ; i++) {
	xx = &x[i * n];

	if(!kendall) {
	    xxm = xm[i];
	    for (j = 0 ; j <= i ; j++) {
		yy = &x[j * n];
		yym = xm[j];
		sum = 0.;
		for (k = 0 ; k < n ; k++)
		    if (ind[k] != 0)
			sum += (xx[k] - xxm) * (yy[k] - yym);
		ANS(j,i) = ANS(i,j) = (double)(sum / n1);
	    }
	}
	else { /* Kendall's tau */
	    for (j = 0 ; j <= i ; j++) {
		yy = &x[j * n];
		sum = 0.;
		for (k = 0 ; k < n ; k++)
		    if (ind[k] != 0)
			for (n1 = 0 ; n1 < n ; n1++)
			    if (ind[n1] != 0)
				sum += sign(xx[k] - xx[n1])
				     * sign(yy[k] - yy[n1]);
		ANS(j,i) = ANS(i,j) = (double)sum;
	    }
	}
    }

    if (cor) {
	for (i = 0 ; i < ncx ; i++)
	    xm[i] = sqrt(ANS(i,i));
	for (i = 0 ; i < ncx ; i++) {
	    for (j = 0 ; j < i ; j++) {
		if (xm[i] == 0 || xm[j] == 0) {
		    *sd_0 = TRUE;
		    ANS(j,i) = ANS(i,j) = NA_REAL;
		}
		else {
		    sum = ANS(i,j) / (xm[i] * xm[j]);
		    if(sum > 1.) sum = 1.;
		    ANS(j,i) = ANS(i,j) = (double)sum;
		}
	    }
	    ANS(i,i) = 1.0;
	}
    }
} /* cov_complete1 */

static void
cov_na_1(int n, int ncx, double *x, double *xm,
	 int *has_na, double *ans, Rboolean *sd_0, Rboolean cor,
	 Rboolean kendall)
{

    COV_ini_na(ncx);

    if(!kendall) {
	MEAN_(x, has_na);/* -> xm[] */
	n1 = n - 1;
    }
    for (i = 0 ; i < ncx ; i++) {
	if(has_na[i]) {
	    for (j = 0 ; j <= i ; j++)
		ANS(j,i) = ANS(i,j) = NA_REAL;
	}
	else {
	    xx = &x[i * n];

	    if(!kendall) {
		xxm = xm[i];
		for (j = 0 ; j <= i ; j++)
		    if(has_na[j]) {
			ANS(j,i) = ANS(i,j) = NA_REAL;
		    } else {
			yy = &x[j * n];
			yym = xm[j];
			sum = 0.;
			for (k = 0 ; k < n ; k++)
			    sum += (xx[k] - xxm) * (yy[k] - yym);
			ANS(j,i) = ANS(i,j) = (double)(sum / n1);
		    }
	    }
	    else { /* Kendall's tau */
		for (j = 0 ; j <= i ; j++)
		    if(has_na[j]) {
			ANS(j,i) = ANS(i,j) = NA_REAL;
		    } else {
			yy = &x[j * n];
			sum = 0.;
			for (k = 0 ; k < n ; k++)
			    for (n1 = 0 ; n1 < n ; n1++)
				sum += sign(xx[k] - xx[n1]) * sign(yy[k] - yy[n1]);
			ANS(j,i) = ANS(i,j) = (double)sum;
		    }
	    }
	}
    }

    if (cor) {
	for (i = 0 ; i < ncx ; i++)
	    if(!has_na[i]) xm[i] = sqrt(ANS(i,i));
	for (i = 0 ; i < ncx ; i++) {
	    if(!has_na[i]) for (j = 0 ; j < i ; j++) {
		if (xm[i] == 0 || xm[j] == 0) {
		    *sd_0 = TRUE;
		    ANS(j,i) = ANS(i,j) = NA_REAL;
		}
		else {
		    sum = ANS(i,j) / (xm[i] * xm[j]);
		    if(sum > 1.) sum = 1.;
		    ANS(j,i) = ANS(i,j) = (double)sum;
		}
	    }
	    ANS(i,i) = 1.0;
	}
    }
} /* cov_na_1() */

static void
cov_complete2(int n, int ncx, int ncy, double *x, double *y,
	      double *xm, double *ym, int *ind,
	      double *ans, Rboolean *sd_0, Rboolean cor, Rboolean kendall)
{
    COV_init(ncy);

    if(!kendall) {
	MEAN(x);/* -> xm[] */
	MEAN(y);/* -> ym[] */
	n1 = nobs - 1;
    }
    for (i = 0 ; i < ncx ; i++) {
	xx = &x[i * n];
	if(!kendall) {
	    xxm = xm[i];
	    for (j = 0 ; j < ncy ; j++) {
		yy = &y[j * n];
		yym = ym[j];
		sum = 0.;
		for (k = 0 ; k < n ; k++)
		    if (ind[k] != 0)
			sum += (xx[k] - xxm) * (yy[k] - yym);
		ANS(i,j) = (double)(sum / n1);
	    }
	}
	else { /* Kendall's tau */
	    for (j = 0 ; j < ncy ; j++) {
		yy = &y[j * n];
		sum = 0.;
		for (k = 0 ; k < n ; k++)
		    if (ind[k] != 0)
			for (n1 = 0 ; n1 < n ; n1++)
			    if (ind[n1] != 0)
				sum += sign(xx[k] - xx[n1])
 				    * sign(yy[k] - yy[n1]);
		ANS(i,j) = (double)sum;
	    }
	}
    }

    if (cor) {

#define COV_SDEV(_X_)							\
	for (i = 0 ; i < nc##_X_ ; i++) { /* Var(X[i]) */		\
	    xx = &_X_[i * n];						\
	    sum = 0.;							\
	    if(!kendall) {						\
		xxm = _X_##m [i];					\
		for (k = 0 ; k < n ; k++)				\
		    if (ind[k] != 0)					\
			sum += (xx[k] - xxm) * (xx[k] - xxm);		\
		sum /= n1;						\
	    }								\
	    else { /* Kendall's tau */					\
		for (k = 0 ; k < n ; k++)				\
		    if (ind[k] != 0)					\
			for (n1 = 0 ; n1 < n ; n1++)			\
			    if (ind[n1] != 0 &&	 xx[k] != xx[n1])	\
				sum ++; /* = sign(. - .)^2 */		\
	    }								\
	    _X_##m [i] = (double)SQRTL(sum);				\
	}

	COV_SDEV(x); /* -> xm[.] */
	COV_SDEV(y); /* -> ym[.] */

	for (i = 0 ; i < ncx ; i++)
	    for (j = 0 ; j < ncy ; j++)
		if (xm[i] == 0. || ym[j] == 0.) {
		    *sd_0 = TRUE;
		    ANS(i,j) = NA_REAL;
		}
		else {
		    ANS(i,j) /= (xm[i] * ym[j]);
		    if(ANS(i,j) > 1.) ANS(i,j) = 1.;
		}
    }/* cor */

}/* cov_complete2 */
#undef COV_SDEV

static void
cov_na_2(int n, int ncx, int ncy, double *x, double *y,
	 double *xm, double *ym, int *has_na_x, int *has_na_y,
	 double *ans, Rboolean *sd_0, Rboolean cor, Rboolean kendall)
{
    COV_ini_na(ncy);

    if(!kendall) {
	MEAN_(x, has_na_x);/* -> xm[] */
	MEAN_(y, has_na_y);/* -> ym[] */
	n1 = n - 1;
    }
    for (i = 0 ; i < ncx ; i++) {
	if(has_na_x[i]) {
	    for (j = 0 ; j < ncy; j++)
		ANS(i,j) = NA_REAL;
	}
	else {
	    xx = &x[i * n];
	    if(!kendall) {
		xxm = xm[i];
		for (j = 0 ; j < ncy ; j++)
		    if(has_na_y[j]) {
			ANS(i,j) = NA_REAL;
		    } else {
			yy = &y[j * n];
			yym = ym[j];
			sum = 0.;
			for (k = 0 ; k < n ; k++)
			    sum += (xx[k] - xxm) * (yy[k] - yym);
			ANS(i,j) = (double)(sum / n1);
		    }
	    }
	    else { /* Kendall's tau */
		for (j = 0 ; j < ncy ; j++)
		    if(has_na_y[j]) {
			ANS(i,j) = NA_REAL;
		    } else {
			yy = &y[j * n];
			sum = 0.;
			for (k = 0 ; k < n ; k++)
			    for (n1 = 0 ; n1 < n ; n1++)
				sum += sign(xx[k] - xx[n1]) * sign(yy[k] - yy[n1]);
			ANS(i,j) = (double)sum;
		    }
	    }
	}
    }

    if (cor) {

#define COV_SDEV(_X_)							\
	for (i = 0 ; i < nc##_X_ ; i++) 				\
	    if(!has_na_##_X_[i]) { /* Var(X[j]) */			\
		xx = &_X_[i * n];					\
		sum = 0.;						\
		if(!kendall) {						\
		    xxm = _X_##m [i];					\
		    for (k = 0 ; k < n ; k++)				\
			sum += (xx[k] - xxm) * (xx[k] - xxm);		\
		    sum /= n1;						\
		}							\
		else { /* Kendall's tau */				\
		    for (k = 0 ; k < n ; k++)				\
			for (n1 = 0 ; n1 < n ; n1++)			\
			    if (xx[k] != xx[n1])			\
				sum ++; /* = sign(. - .)^2 */		\
		}							\
		_X_##m [i] = (double) SQRTL(sum);			\
	    }

	COV_SDEV(x); /* -> xm[.] */
	COV_SDEV(y); /* -> ym[.] */

	for (i = 0 ; i < ncx ; i++)
	    if(!has_na_x[i]) {
		for (j = 0 ; j < ncy ; j++)
		    if(!has_na_y[j]) {
			if (xm[i] == 0. || ym[j] == 0.) {
			    *sd_0 = TRUE;
			    ANS(i,j) = NA_REAL;
			}
			else {
			    ANS(i,j) /= (xm[i] * ym[j]);
			    if(ANS(i,j) > 1.) ANS(i,j) = 1.;
			}
		    }
	    }
    }/* cor */

}/* cov_na_2 */

#undef ANS
#undef COV_init
#undef MEAN
#undef MEAN_
#undef COV_SDEV

/* complete[12]() returns indicator vector ind[] of complete.cases(), or
 * -------------- if(na_fail) signals error if any NA/NaN is encountered
 */

/* This might look slightly inefficient, but it is designed to
 * optimise paging in virtual memory systems ...
 * (or at least that's my story, and I'm sticking to it.)
*/
#define NA_LOOP								\
	for (i = 0 ; i < n ; i++)					\
	    if (ISNAN(z[i])) {						\
		if (na_fail) error(_("missing observations in cov/cor"));\
		else ind[i] = 0;					\
	    }

#define COMPLETE_1				\
    double *z;					\
    int i, j;					\
    for (i = 0 ; i < n ; i++)			\
	ind[i] = 1;				\
    for (j = 0 ; j < ncx ; j++) {		\
	z = &x[j * n];				\
	NA_LOOP					\
    }

static void complete1(int n, int ncx, double *x, int *ind, Rboolean na_fail)
{
    COMPLETE_1
}

static void
complete2(int n, int ncx, int ncy, double *x, double *y, int *ind, Rboolean na_fail)
{
    COMPLETE_1

    for(j = 0 ; j < ncy ; j++) {
	z = &y[j * n];
	NA_LOOP
    }
}

#define HAS_NA_1(_X_,_HAS_NA_)			\
    for (j = 0 ; j < nc##_X_ ; j++) {		\
	z = &_X_[j * n];			\
        _HAS_NA_[j] = 0;			\
	for (i = 0 ; i < n ; i++)		\
	    if (ISNAN(z[i])) {			\
		_HAS_NA_[j] = 1; break;		\
	    }					\
    }


static void find_na_1(int n, int ncx, double *x, int *has_na)
{
    double *z;
    int i, j;
    HAS_NA_1(x, has_na)
}

static void
find_na_2(int n, int ncx, int ncy, double *x, double *y, int *has_na_x, int *has_na_y)
{
    double *z;
    int i, j;
    HAS_NA_1(x, has_na_x)
    HAS_NA_1(y, has_na_y)
}

#undef NA_LOOP
#undef COMPLETE_1
#undef HAS_NA_1

/* co[vr](x, y, use =
	{ 1,		2,		3,		   4,		5  }
  "all.obs", "complete.obs", "pairwise.complete", "everything", "na.or.complete"
	  kendall = TRUE/FALSE)
*/
static SEXP corcov(SEXP x, SEXP y, SEXP na_method, SEXP skendall, Rboolean cor)
{
    SEXP ans, xm, ym, ind;
    Rboolean ansmat, kendall, pair, na_fail, everything, sd_0, empty_err;
    int i, method, n, ncx, ncy, nprotect = 2;

    /* Arg.1: x */
    if(isNull(x)) /* never allowed */
	error(_("'x' is NULL"));
    if(isFactor(x)) error(_("'x' is a factor"));
    /* length check of x -- only if(empty_err) --> below */
    x = PROTECT(coerceVector(x, REALSXP));
    if ((ansmat = isMatrix(x))) {
	n = nrows(x);
	ncx = ncols(x);
    }
    else {
	n = length(x);
	ncx = 1;
    }
    /* Arg.2: y */
    if (isNull(y)) {/* y = x  : var() */
	ncy = ncx;
    } else {
	if(isFactor(y)) error(_("'y' is a factor"));
	y = PROTECT(coerceVector(y, REALSXP));
	nprotect++;
	if (isMatrix(y)) {
	    if (nrows(y) != n)
		error(_("incompatible dimensions"));
	    ncy = ncols(y);
	    ansmat = TRUE;
	}
	else {
	    if (length(y) != n)
		error(_("incompatible dimensions"));
	    ncy = 1;
	}
    }
    /* Arg.3:  method */
    method = asInteger(na_method);

    /* Arg.4:  kendall */
    kendall = asLogical(skendall);

    /* "default: complete" (easier for -Wall) */
    na_fail = FALSE; everything = FALSE; empty_err = TRUE;
    pair = FALSE;
    switch(method) {
    case 1:		/* use all :  no NAs */
	na_fail = TRUE;
	break;
    case 2:		/* complete */
	/* did na.omit in R */
	if (!LENGTH(x)) error(_("no complete element pairs"));
	break;
    case 3:		/* pairwise.complete */
	pair = TRUE;
	break;
    case 4:		/* "everything": NAs are propagated */
	everything = TRUE;
	empty_err = FALSE;
	break;
    case 5:		/* "na.or.complete": NAs are propagated */
	empty_err = FALSE;
	break;
    default:
	error(_("invalid 'use' (computational method)"));
    }
    if (empty_err && !LENGTH(x))
	error(_("'x' is empty"));

    if (ansmat) PROTECT(ans = allocMatrix(REALSXP, ncx, ncy));
    else PROTECT(ans = allocVector(REALSXP, ncx * ncy));
    sd_0 = FALSE;
    if (isNull(y)) {
	if (everything) { /* NA's are propagated */
	    PROTECT(xm = allocVector(REALSXP, ncx));
	    PROTECT(ind = allocVector(LGLSXP, ncx));
	    find_na_1(n, ncx, REAL(x), /* --> has_na[] = */ LOGICAL(ind));
	    cov_na_1 (n, ncx, REAL(x), REAL(xm), LOGICAL(ind), REAL(ans), &sd_0, cor, kendall);

	    UNPROTECT(2);
	}
	else if (!pair) { /* all | complete "var" */
	    PROTECT(xm = allocVector(REALSXP, ncx));
	    PROTECT(ind = allocVector(INTSXP, n));
	    complete1(n, ncx, REAL(x), INTEGER(ind), na_fail);
	    cov_complete1(n, ncx, REAL(x), REAL(xm),
			  INTEGER(ind), REAL(ans), &sd_0, cor, kendall);
	    if(empty_err) {
		Rboolean indany = FALSE;
		for(i = 0; i < n; i++) {
		    if(INTEGER(ind)[i] == 1) { indany = TRUE; break; }
		}
		if(!indany) error(_("no complete element pairs"));
	    }
	    UNPROTECT(2);
	}
	else {		/* pairwise "var" */
	    cov_pairwise1(n, ncx, REAL(x), REAL(ans), &sd_0, cor, kendall);
	}
    }
    else { /* Co[vr] (x, y) */
	if (everything) {
	    SEXP has_na_y;
	    PROTECT(xm = allocVector(REALSXP, ncx));
	    PROTECT(ym = allocVector(REALSXP, ncy));
	    PROTECT(ind      = allocVector(LGLSXP, ncx));
	    PROTECT(has_na_y = allocVector(LGLSXP, ncy));

	    find_na_2(n, ncx, ncy, REAL(x), REAL(y), INTEGER(ind), INTEGER(has_na_y));
	    cov_na_2 (n, ncx, ncy, REAL(x), REAL(y), REAL(xm), REAL(ym),
		      INTEGER(ind), INTEGER(has_na_y), REAL(ans), &sd_0, cor, kendall);
	    UNPROTECT(4);
	}
	else if (!pair) { /* all | complete */
	    PROTECT(xm = allocVector(REALSXP, ncx));
	    PROTECT(ym = allocVector(REALSXP, ncy));
	    PROTECT(ind = allocVector(INTSXP, n));
	    complete2(n, ncx, ncy, REAL(x), REAL(y), INTEGER(ind), na_fail);
	    cov_complete2(n, ncx, ncy, REAL(x), REAL(y), REAL(xm), REAL(ym),
			  INTEGER(ind), REAL(ans), &sd_0, cor, kendall);
	    if(empty_err) {
		Rboolean indany = FALSE;
		for(i = 0; i < n; i++) {
		    if(INTEGER(ind)[i] == 1) { indany = TRUE; break; }
		}
		if(!indany) error(_("no complete element pairs"));
	    }
	    UNPROTECT(3);
	}
	else {		/* pairwise */
	    cov_pairwise2(n, ncx, ncy, REAL(x), REAL(y), REAL(ans),
			  &sd_0, cor, kendall);
	}
    }
    if (ansmat) { /* set dimnames() when applicable */
	if (isNull(y)) {
	    x = getAttrib(x, R_DimNamesSymbol);
	    if (!isNull(x) && !isNull(VECTOR_ELT(x, 1))) {
		PROTECT(ind = allocVector(VECSXP, 2));
		SET_VECTOR_ELT(ind, 0, duplicate(VECTOR_ELT(x, 1)));
		SET_VECTOR_ELT(ind, 1, duplicate(VECTOR_ELT(x, 1)));
		setAttrib(ans, R_DimNamesSymbol, ind);
		UNPROTECT(1);
	    }
	}
	else {
	    x = getAttrib(x, R_DimNamesSymbol);
	    y = getAttrib(y, R_DimNamesSymbol);
	    if ((length(x) >= 2 && !isNull(VECTOR_ELT(x, 1))) ||
		(length(y) >= 2 && !isNull(VECTOR_ELT(y, 1)))) {
		PROTECT(ind = allocVector(VECSXP, 2));
		if (length(x) >= 2 && !isNull(VECTOR_ELT(x, 1)))
		    SET_VECTOR_ELT(ind, 0, duplicate(VECTOR_ELT(x, 1)));
		if (length(y) >= 2 && !isNull(VECTOR_ELT(y, 1)))
		    SET_VECTOR_ELT(ind, 1, duplicate(VECTOR_ELT(y, 1)));
		setAttrib(ans, R_DimNamesSymbol, ind);
		UNPROTECT(1);
	    }
	}
    }
    if(sd_0)/* only in cor() */
	warning(_("the standard deviation is zero"));
    UNPROTECT(nprotect);
    return ans;
}
