/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"/* => Utils.h with the protos from here */
#include "Mathlib.h"
			/*--- Part I: Comparison Utilities ---*/

static int icmp(int x, int y)
{
    if (x == NA_INTEGER)return 1;
    if (y == NA_INTEGER)return -1;
    if (x < y)		return -1;
    if (x > y)		return 1;
    return 0;
}

static int rcmp(double x, double y)
{
    if (ISNAN(x))	return 1;
    if (ISNAN(y))	return -1;
    if (x < y)		return -1;
    if (x > y)		return 1;
    return 0;
}

static int ccmp(complex x, complex y)
{
				/* compare real parts */
    if (ISNAN(x.r))	return 1;
    if (ISNAN(y.r))	return -1;
    if (x.r < y.r)	return -1;
    if (x.r > y.r)	return 1;
				/* compare complex parts */
    if (ISNAN(x.i))	return 1;
    if (ISNAN(y.i))	return -1;
    if (x.i < y.i)	return -1;
    if (x.i > y.i)	return 1;

    return 0;		/* equal */
}

static int scmp(SEXP x, SEXP y)
{
#ifdef HAVE_STRCOLL
    return strcoll(CHAR(x), CHAR(y));
#else
    return strcmp(CHAR(x), CHAR(y));
#endif
}

			/*--- Part II: Complete (non-partial) Sorting ---*/


/* SHELLsort -- corrected from R. Sedgewick `Algorithms in C' 
 *		(version of BDR's lqs():*/
#define sort_body \
    int i, j, h;\
\
    for (h = 1; h <= n / 9; h = 3 * h + 1);\
    for (; h > 0; h /= 3)\
	for (i = h; i < n; i++) {\
	    v = x[i];\
	    j = i;\
	    while (j >= h && TYPE_CMP(x[j - h], v) > 0)\
		 { x[j] = x[j - h]; j -= h; }\
	    x[j] = v;\
	}

void isort(int *x, int n)
{
    int v;
#define TYPE_CMP icmp
    sort_body
#undef TYPE_CMP
}

void rsort(double *x, int n)
{
    double v;
#define TYPE_CMP rcmp
    sort_body
#undef TYPE_CMP
}

void csort(complex *x, int n)
{
    complex v;
#define TYPE_CMP ccmp
    sort_body
#undef TYPE_CMP
}


void ssort(SEXP *x, int n)
{
    SEXP v;
#define TYPE_CMP scmp
    sort_body
#undef TYPE_CMP
}

void revsort(double *a, int *ib, int n)
{
/* Sort a[] into descending order by "heapsort";
 * sort ib[] alongside;
 * if initially, ib[] = 1...n, it will contain the permutation finally
 */

    int l, j, ir, i;
    double ra;
    int ii;

    a--; ib--;

    l = (n >> 1) + 1;
    ir = n;

    for (;;) {
        if (l > 1) {
	    l = l - 1;
	    ra = a[l];
	    ii = ib[l];
        }
        else {
	    ra = a[ir];
	    ii = ib[ir];
	    a[ir] = a[1];
	    ib[ir] = ib[1];
	    if (--ir == 1) {
		a[1] = ra;
		ib[1] = ii;
		return;
	    }
        }
        i = l;
        j = l << 1;
        while (j <= ir) {
	    if (j < ir && a[j] > a[j + 1]) ++j;
	    if (ra > a[j]) {
		a[i] = a[j];
		ib[i] = ib[j];
		j += (i = j);
	    }
	    else
		j = ir + 1;
        }
        a[i] = ra;
        ib[i] = ii;
    }
}

void sortVector(SEXP s)
{
    int n;

    n = LENGTH(s);
    if (n >= 2)
	switch (TYPEOF(s)) {
	case LGLSXP:
	case INTSXP:
	    isort(INTEGER(s), n);
	    break;
	case REALSXP:
	    rsort(REAL(s), n);
	    break;
	case CPLXSXP:
	    csort(COMPLEX(s), n);
	    break;
	case STRSXP:
	    ssort(STRING(s), n);
	    break;
	}
}

SEXP do_sort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity(op, args);

    if (!isVector(CAR(args)))
	errorcall(call, "only vectors can be sorted\n");
    ans = duplicate(CAR(args));
    sortVector(ans);
    return ans;
}

			/*--- Part III: Partial Sorting ---*/

/* 
   Partial sort so that x[k] is in the correct place, smaller to left,
   larger to right
 */
#define psort_body \
    int L, R, i, j;\
\
    for (L = 0, R = n - 1; L < R; ) {\
	v = x[k];\
	for(i = L, j = R; i <= j;) {\
	    while (TYPE_CMP(x[i], v) < 0) i++;\
	    while (TYPE_CMP(v, x[j]) < 0) j--;\
	    if (i <= j) { w = x[i]; x[i++] = x[j]; x[j--] = w; }\
	}\
	if (j < k) L = i;\
	if (k < i) R = j;\
    }


void iPsort(int *x, int n, int k)
{
    int v, w;
#define TYPE_CMP icmp
    psort_body
#undef TYPE_CMP
}

void rPsort(double *x, int n, int k)
{
    double v, w;
#define TYPE_CMP rcmp
    psort_body
#undef TYPE_CMP
}

void cPsort(complex *x, int n, int k)
{
    complex v, w;
#define TYPE_CMP ccmp
    psort_body
#undef TYPE_CMP
}


void sPsort(SEXP *x, int n, int k)
{
    SEXP v, w;
#define TYPE_CMP scmp
    psort_body
#undef TYPE_CMP
}

void Psort(SEXP x, int k)
{
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	iPsort(INTEGER(x), LENGTH(x), k);
	break;
    case REALSXP:
	rPsort(REAL(x), LENGTH(x), k);
	break;
    case CPLXSXP:
	cPsort(COMPLEX(x), LENGTH(x), k);
	break;
    case STRSXP:
	sPsort(STRING(x), LENGTH(x), k);
	break;
    }
}

/* FUNCTION psort(x, indices) */
SEXP do_psort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, k, n;
    int *l;
    checkArity(op, args);

    if (!isVector(CAR(args)))
	errorcall(call,"only vectors can be sorted\n");
    n = LENGTH(CAR(args));
    CADR(args) = coerceVector(CADR(args), INTSXP);
    l = INTEGER(CADR(args));
    k = LENGTH(CADR(args));
    for (i = 0; i < k; i++) {
	if (l[i] == NA_INTEGER)
	    errorcall(call,"NA index\n");
	if (l[i] < 1 || l[i] > n)
	    errorcall(call,"index %d outside bounds\n", l[i]);
    }
    CAR(args) = duplicate(CAR(args));
    for (i = 0; i < k; i++)
	Psort(CAR(args), l[i] - 1);
    return CAR(args);
}


			/*--- Part IV : Rank & Order ---*/

static int equal(int i, int j, SEXP x)
{
    int c=-1;

    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	c = icmp(INTEGER(x)[i], INTEGER(x)[j]);
	break;
    case REALSXP:
	c = rcmp(REAL(x)[i], REAL(x)[j]);
	break;
    case CPLXSXP:
	c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j]);
	break;
    case STRSXP:
	c = scmp(STRING(x)[i], STRING(x)[j]);
	break;
    }
    if (c == 0)
	return 1;
    return 0;
}

static int greater(int i, int j, SEXP x)
{
    int c=-1;

    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	c = icmp(INTEGER(x)[i], INTEGER(x)[j]);
	break;
    case REALSXP:
	c = rcmp(REAL(x)[i], REAL(x)[j]);
	break;
    case CPLXSXP:
	c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j]);
	break;
    case STRSXP:
	c = scmp(STRING(x)[i], STRING(x)[j]);
	break;
    }
    if (c > 0)
	return 1;
    return 0;
}

static int listgreater(int i, int j, SEXP key)
{
    SEXP x;
    int c=-1;

    while (key != R_NilValue) {
	x = CAR(key);
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    c = icmp(INTEGER(x)[i], INTEGER(x)[j]);
	    break;
	case REALSXP:
	    c = rcmp(REAL(x)[i], REAL(x)[j]);
	    break;
	case CPLXSXP:
	    c = ccmp(COMPLEX(x)[i], COMPLEX(x)[j]);
	    break;
	case STRSXP:
	    c = scmp(STRING(x)[i], STRING(x)[j]);
	    break;
	}
	if (c > 0)
	    return 1;
	if (c < 0)
	    return 0;
	key = CDR(key);
    }
    if (c==0 && i<j)
	return 0;
    return 1;
}

void orderVector(int *index, int n, SEXP key, int greater())
{
    int i, j, h;
    int itmp;

    h = 1;
    do {
	h = 3 * h + 1;
    } while (h <= n);

    do {
	h = h / 3;
	for (i = h; i < n; i++) {
	    itmp = index[i];
	    j = i;
	    while (greater(index[j - h], itmp, key)) {
		index[j] = index[j - h];
		j = j - h;
		if (j < h)
		    goto next_h;
	    }
	next_h:	index[j] = itmp;
	}
    }
    while (h != 1);
}

/* FUNCTION order(...) */
SEXP do_order(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ap, ans;
    int i, n, narg = 0;

    if (args == R_NilValue)
	return R_NilValue;

    if (isVector(CAR(args)))
	n = LENGTH(CAR(args));
    else    n = -1; /* for -Wall;  will have error below */
    for (ap = args; ap != R_NilValue; ap = CDR(ap)) {
	if (!isVector(CAR(ap)))
	    errorcall(call, "Argument %d is not a vector\n", ++narg);
	if (LENGTH(CAR(ap)) != n)
	    errorcall(call, "Argument lengths differ\n");
    }
    ans = allocVector(INTSXP, n);
    if (n != 0) {
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = i;
	orderVector(INTEGER(ans), n, args, listgreater);
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] += 1;
    }
    return ans;
}

/* FUNCTION: rank(x) */
SEXP do_rank(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rank, index, x;
    int *in;
    double *rk;
    int i, j, k, n;

    checkArity(op, args);
    if (args == R_NilValue)
	return R_NilValue;
    x = CAR(args);
    if (!isVector(x))
	errorcall(call, "Argument is not a vector\n");
    n = LENGTH(x);
    PROTECT(index = allocVector(INTSXP, n));
    PROTECT(rank = allocVector(REALSXP, n));
    UNPROTECT(2);
    if (n > 0) {
	in = INTEGER(index);
	rk = REAL(rank);
	for (i = 0; i < n; i++)
	    in[i] = i;
	orderVector(in, n, x, greater);
	i = 0;
	while (i < n) {
	    j = i;
	    while ((j < n - 1) && equal(in[j], in[j + 1], x))
		j++;
	    if (i != j) {
		for (k = i; k <= j; k++)
		    rk[in[k]] = (i + j + 2) / 2.0;
	    }
	    else
		rk[in[i]] = i + 1;
	    i = j + 1;
	}
    }
    return rank;
}
