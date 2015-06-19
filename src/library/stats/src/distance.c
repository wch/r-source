/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2013  The R Core Team
 *  Copyright (C) 2002, 2004  The R Foundation
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
 *  http://www.r-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
/* do this first to get the right options for math.h */
#include <R_ext/Arith.h>

#include <R.h>
#include <Rmath.h>
#include <float.h>
#include "stats.h"
#ifdef _OPENMP
# include <R_ext/MathThreads.h>
#endif

#define both_FINITE(a,b) (R_FINITE(a) && R_FINITE(b))
#ifdef R_160_and_older
#define both_non_NA both_FINITE
#else
#define both_non_NA(a,b) (!ISNAN(a) && !ISNAN(b))
#endif

static double R_euclidean(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int count, j;

    count= 0;
    dist = 0;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    dev = (x[i1] - x[i2]);
	    if(!ISNAN(dev)) {
		dist += dev * dev;
		count++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return sqrt(dist);
}

static double R_maximum(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int count, j;

    count = 0;
    dist = -DBL_MAX;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    dev = fabs(x[i1] - x[i2]);
	    if(!ISNAN(dev)) {
		if(dev > dist)
		    dist = dev;
		count++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    return dist;
}

static double R_manhattan(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist;
    int count, j;

    count = 0;
    dist = 0;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    dev = fabs(x[i1] - x[i2]);
	    if(!ISNAN(dev)) {
		dist += dev;
		count++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return dist;
}

static double R_canberra(double *x, int nr, int nc, int i1, int i2)
{
    double dev, dist, sum, diff;
    int count, j;

    count = 0;
    dist = 0;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    sum = fabs(x[i1] + x[i2]);
	    diff = fabs(x[i1] - x[i2]);
	    if (sum > DBL_MIN || diff > DBL_MIN) {
		dev = diff/sum;
		if(!ISNAN(dev) ||
		   (!R_FINITE(diff) && diff == sum &&
		    /* use Inf = lim x -> oo */ (dev = 1.))) {
		    dist += dev;
		    count++;
		}
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return dist;
}

static double R_dist_binary(double *x, int nr, int nc, int i1, int i2)
{
    int total, count, dist;
    int j;

    total = 0;
    count = 0;
    dist = 0;

    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    if(!both_FINITE(x[i1], x[i2])) {
		warning(_("treating non-finite values as NA"));
	    }
	    else {
		if(x[i1] || x[i2]) {
		    count++;
		    if( ! (x[i1] && x[i2]) ) dist++;
		}
		total++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }

    if(total == 0) return NA_REAL;
    if(count == 0) return 0;
    return (double) dist / count;
}

static double R_minkowski(double *x, int nr, int nc, int i1, int i2, double p)
{
    double dev, dist;
    int count, j;

    count= 0;
    dist = 0;
    for(j = 0 ; j < nc ; j++) {
	if(both_non_NA(x[i1], x[i2])) {
	    dev = (x[i1] - x[i2]);
	    if(!ISNAN(dev)) {
		dist += R_pow(fabs(dev), p);
		count++;
	    }
	}
	i1 += nr;
	i2 += nr;
    }
    if(count == 0) return NA_REAL;
    if(count != nc) dist /= ((double)count/nc);
    return R_pow(dist, 1.0/p);
}

enum { EUCLIDEAN=1, MAXIMUM, MANHATTAN, CANBERRA, BINARY, MINKOWSKI };
/* == 1,2,..., defined by order in the R function dist */

void R_distance(double *x, int *nr, int *nc, double *d, int *diag,
		int *method, double *p)
{
    int dc, i, j;
    size_t  ij;  /* can exceed 2^31 - 1 */
    double (*distfun)(double*, int, int, int, int) = NULL;
#ifdef _OPENMP
    int nthreads;
#endif

    switch(*method) {
    case EUCLIDEAN:
	distfun = R_euclidean;
	break;
    case MAXIMUM:
	distfun = R_maximum;
	break;
    case MANHATTAN:
	distfun = R_manhattan;
	break;
    case CANBERRA:
	distfun = R_canberra;
	break;
    case BINARY:
	distfun = R_dist_binary;
	break;
    case MINKOWSKI:
	if(!R_FINITE(*p) || *p <= 0)
	    error(_("distance(): invalid p"));
	break;
    default:
	error(_("distance(): invalid distance"));
    }
    dc = (*diag) ? 0 : 1; /* diag=1:  we do the diagonal */
#ifdef _OPENMP
    if (R_num_math_threads > 0)
	nthreads = R_num_math_threads;
    else
	nthreads = 1; /* for now */
    if (nthreads == 1) {
	/* do the nthreads == 1 case without any OMP overhead to see
	   if it matters on some platforms */
	ij = 0;
	for(j = 0 ; j <= *nr ; j++)
	    for(i = j+dc ; i < *nr ; i++)
		d[ij++] = (*method != MINKOWSKI) ?
		    distfun(x, *nr, *nc, i, j) :
		    R_minkowski(x, *nr, *nc, i, j, *p);
    }
    else
	/* This produces uneven thread workloads since the outer loop
	   is over the subdiagonal portions of columns.  An
	   alternative would be to use a loop on ij and to compute the
	   i and j values from ij. */
#pragma omp parallel for num_threads(nthreads) default(none)	\
    private(i, j, ij)						\
    firstprivate(nr, dc, d, method, distfun, nc, x, p)
	for(j = 0 ; j <= *nr ; j++) {
	    ij = j * (*nr - dc) + j - ((1 + j) * j) / 2;
	    for(i = j+dc ; i < *nr ; i++)
		d[ij++] = (*method != MINKOWSKI) ?
		    distfun(x, *nr, *nc, i, j) :
		    R_minkowski(x, *nr, *nc, i, j, *p);
	}
#else
    ij = 0;
    for(j = 0 ; j <= *nr ; j++)
	for(i = j+dc ; i < *nr ; i++)
	    d[ij++] = (*method != MINKOWSKI) ?
		distfun(x, *nr, *nc, i, j) : R_minkowski(x, *nr, *nc, i, j, *p);
#endif
}

#include <Rinternals.h>

SEXP Cdist(SEXP x, SEXP smethod, SEXP attrs, SEXP p)
{
    SEXP ans;
    int nr = nrows(x), nc = ncols(x), method = asInteger(smethod);
    int diag = 0;
    R_xlen_t N;
    double rp = asReal(p);
    N = (R_xlen_t)nr * (nr-1)/2; /* avoid int overflow for N ~ 50,000 */
    PROTECT(ans = allocVector(REALSXP, N));
    if(TYPEOF(x) != REALSXP) x = coerceVector(x, REALSXP);
    PROTECT(x);
    R_distance(REAL(x), &nr, &nc, REAL(ans), &diag, &method, &rp);
    /* tack on attributes */
    SEXP names = getAttrib(attrs, R_NamesSymbol);
    for (int i = 0; i < LENGTH(attrs); i++)
	setAttrib(ans, install(translateChar(STRING_ELT(names, i))),
		  VECTOR_ELT(attrs, i));
    UNPROTECT(2);
    return ans;
}
