/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2000   The R Development Core Team.
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h> /* => Utils.h with the protos from here */
#include <Rmath.h>
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

static int ccmp(Rcomplex x, Rcomplex y)
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

Rboolean isUnsorted(SEXP x)
{
    int n, i;

    if (!isVectorAtomic(x))
	error("only atomic vectors can be tested to be sorted");
    n = LENGTH(x);
    if(n >= 2) 
	switch (TYPEOF(x)) { 
	    
	    /* NOTE: x must have no NAs {is.na(.) in R}; 
	       hence be faster than `rcmp()', `icmp()' for these two cases */

	case LGLSXP:
	case INTSXP:
	    for(i = 0; i+1 < n ; i++) 
		if(INTEGER(x)[i] > INTEGER(x)[i+1])
		    return TRUE;
	    break;
	case REALSXP:
	    for(i = 0; i+1 < n ; i++) 
		if(REAL(x)[i] > REAL(x)[i+1])
		    return TRUE;
	    break;
	case CPLXSXP:
	    for(i = 0; i+1 < n ; i++) 
		if(ccmp(COMPLEX(x)[i], COMPLEX(x)[i+1]) > 0)
		    return TRUE;
	    break;
	case STRSXP:
	    for(i = 0; i+1 < n ; i++) 
		if(scmp(STRING_ELT(x, i ), 
			STRING_ELT(x,i+1)) > 0)
		    return TRUE;
	    break;
	default:
	    error("unknown atomic type in isUnsorted() -- should not happen");
	}
    return FALSE;/* sorted */
}

SEXP do_isunsorted(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity(op, args);
    ans = allocVector(LGLSXP, 1);
    LOGICAL(ans)[0] = isUnsorted(CAR(args));
    return ans;
}


			/*--- Part II: Complete (non-partial) Sorting ---*/


/* SHELLsort -- corrected from R. Sedgewick `Algorithms in C' 
 *		(version of BDR's lqs():*/
#define sort_body					\
    int i, j, h;					\
							\
    for (h = 1; h <= n / 9; h = 3 * h + 1);		\
    for (; h > 0; h /= 3)				\
	for (i = h; i < n; i++) {			\
	    v = x[i];					\
	    j = i;					\
	    while (j >= h && TYPE_CMP(x[j - h], v) > 0)	\
		 { x[j] = x[j - h]; j -= h; }		\
	    x[j] = v;					\
	}

void R_isort(int *x, int n)
{
    int v;
#define TYPE_CMP icmp
    sort_body
#undef TYPE_CMP
}

void R_rsort(double *x, int n)
{
    double v;
#define TYPE_CMP rcmp
    sort_body
#undef TYPE_CMP
}

void R_csort(Rcomplex *x, int n)
{
    Rcomplex v;
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

void rsort_with_index(double *x, int *indx, int n)
{
    double v;
    int i, j, h, iv;

    for (h = 1; h <= n / 9; h = 3 * h + 1);
    for (; h > 0; h /= 3)
	for (i = h; i < n; i++) {
	    v = x[i]; iv = indx[i];
	    j = i;
	    while (j >= h && rcmp(x[j - h], v) > 0)
		 { x[j] = x[j - h]; indx[j] = indx[j-h]; j -= h; }
	    x[j] = v; indx[j] = iv;
	}
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

    if (n <= 1) return;

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
    int n = LENGTH(s);
    if (n >= 2 && isUnsorted(s))
	switch (TYPEOF(s)) {
	case LGLSXP:
	case INTSXP:
	    R_isort(INTEGER(s), n);
	    break;
	case REALSXP:
	    R_rsort(REAL(s), n);
	    break;
	case CPLXSXP:
	    R_csort(COMPLEX(s), n);
	    break;
	case STRSXP:
	    ssort(STRING_PTR(s), n);
	    break;
	}
}

SEXP do_sort(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    checkArity(op, args);

    if(CAR(args) == R_NilValue) return R_NilValue;
    if(!isVectorAtomic(CAR(args)))
	errorcall(call, "only atomic vectors can be sorted");
    if (isUnsorted(CAR(args))) { /* do not duplicate if sorted */
	ans = duplicate(CAR(args));
	sortVector(ans);
	return(ans);
    } 
    else return(CAR(args));
}

			/*--- Part III: Partial Sorting ---*/

/* 
   Partial sort so that x[k] is in the correct place, smaller to left,
   larger to right

   NOTA BENE:  k < n  required, and *not* checked here but in do_psort();
               -----  infinite loop possible otherwise!
 */
#define psort_body						\
    int L, R, i, j;						\
								\
    for (L = 0, R = n - 1; L < R; ) {				\
	v = x[k];						\
	for(i = L, j = R; i <= j;) {				\
	    while (TYPE_CMP(x[i], v) < 0) i++;			\
	    while (TYPE_CMP(v, x[j]) < 0) j--;			\
	    if (i <= j) { w = x[i]; x[i++] = x[j]; x[j--] = w; }\
	}							\
	if (j < k) L = i;					\
	if (k < i) R = j;					\
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

void cPsort(Rcomplex *x, int n, int k)
{
    Rcomplex v, w;
#define TYPE_CMP ccmp
    psort_body
#undef TYPE_CMP
}


static void sPsort(SEXP *x, int n, int k)
{
    SEXP v, w;
#define TYPE_CMP scmp
    psort_body
#undef TYPE_CMP
}

static void Psort(SEXP x, int k)
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
	sPsort(STRING_PTR(x), LENGTH(x), k);
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
	errorcall(call,"only vectors can be sorted");
    n = LENGTH(CAR(args));
    SETCADR(args, coerceVector(CADR(args), INTSXP));
    l = INTEGER(CADR(args));
    k = LENGTH(CADR(args));
    for (i = 0; i < k; i++) {
	if (l[i] == NA_INTEGER)
	    errorcall(call,"NA index");
	if (l[i] < 1 || l[i] > n)
	    errorcall(call,"index %d outside bounds", l[i]);
    }
    SETCAR(args, duplicate(CAR(args)));
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
	c = scmp(STRING_ELT(x, i), STRING_ELT(x, j));
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
	c = scmp(STRING_ELT(x, i), STRING_ELT(x, j));
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
	    c = scmp(STRING_ELT(x, i), STRING_ELT(x, j));
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

static void orderVector(int *indx, int n, SEXP key, int greater_sub())
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
	    itmp = indx[i];
	    j = i;
	    while (greater_sub(indx[j - h], itmp, key)) {
		indx[j] = indx[j - h];
		j = j - h;
		if (j < h)
		    goto next_h;
	    }
	next_h:	indx[j] = itmp;
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
	    errorcall(call, "Argument %d is not a vector", ++narg);
	if (LENGTH(CAR(ap)) != n)
	    errorcall(call, "Argument lengths differ");
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
    SEXP rank, indx, x;
    int *in;
    double *rk;
    int i, j, k, n;

    checkArity(op, args);
    if (args == R_NilValue)
	return R_NilValue;
    x = CAR(args);
    if (!isVector(x))
	errorcall(call, "Argument is not a vector");
    n = LENGTH(x);
    PROTECT(indx = allocVector(INTSXP, n));
    PROTECT(rank = allocVector(REALSXP, n));
    UNPROTECT(2);
    if (n > 0) {
	in = INTEGER(indx);
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
