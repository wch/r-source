/*
 *  R : A Computer Langage for Statistical Data Analysis
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

#include "Defn.h"
#include "Mathlib.h"

static int icmp(int x, int y)
{
	if (x == NA_INTEGER)
		return 1;
	if (y == NA_INTEGER)
		return -1;
	if (x < y)
		return -1;
	if (x > y)
		return 1;
	return 0;
}

static int rcmp(double x, double y)
{
	if (ISNAN(x))
		return 1;
	if (ISNAN(y))
		return -1;
	if (x < y)
		return -1;
	if (x > y)
		return 1;
	return 0;
}

static int ccmp(complex x, complex y)
{
	if (ISNAN(x.r))		/* compare real parts */
		return 1;
	if (ISNAN(y.r))
		return -1;
	if (x.r < y.r)
		return -1;
	if (x.r > y.r)
		return 1;

	if (ISNAN(x.i))		/* compare complex parts */
		return 1;
	if (ISNAN(y.i))
		return -1;
	if (x.i < y.i)
		return -1;
	if (x.i > y.i)
		return 1;

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

void isort(int * x, int n)
{
	int i, j, h;
	int xtmp;

	h = 1;
	do {
		h = 3 * h + 1;
	}
	while (h <= n);

	do {
		h = h / 3;
		for (i = h; i < n; i++) {
			xtmp = x[i];
			j = i;
			while (icmp(x[j - h], xtmp) > 0) {
				x[j] = x[j - h];
				j = j - h;
				if (j < h)
					goto end;
			}
		end:	x[j] = xtmp;
		}
	} while (h != 1);
}

void rsort(double *x, int n)
{
	int i, j, h;
	double xtmp;

	h = 1;
	do {
		h = 3 * h + 1;
	}
	while (h <= n);

	do {
		h = h / 3;
		for (i = h; i < n; i++) {
			xtmp = x[i];
			j = i;
			while (rcmp(x[j - h], xtmp) > 0) {
				x[j] = x[j - h];
				j = j - h;
				if (j < h)
					goto end;
			}
		end:	x[j] = xtmp;
		}
	} while (h != 1);
}

void csort(complex *x, int n)
{
	int i, j, h;
	complex xtmp;

	h = 1;
	do {
		h = 3 * h + 1;
	}
	while (h <= n);

	do {
		h = h / 3;
		for (i = h; i < n; i++) {
			xtmp = x[i];
			j = i;
			while (ccmp(x[j - h], xtmp) > 0) {
				x[j] = x[j - h];
				j = j - h;
				if (j < h)
					goto end;
			}
		end:    x[j] = xtmp;
		}
	} while (h != 1);
}


void ssort(SEXP *x, int n)
{
	int i, j, h;
	SEXP xtmp;

	h = 1;
	do {
		h = 3 * h + 1;
	}
	while (h <= n);

	do {
		h = h / 3;
		for (i = h; i < n; i++) {
			xtmp = x[i];
			j = i;
			while (scmp(x[j - h], xtmp) > 0) {
				x[j] = x[j - h];
				j = j - h;
				if (j < h)
					goto end;
			}
		end:	x[j] = xtmp;
		}
	} while (h != 1);
}

void sortVector(SEXP s)
{
	int n;

	if (!isVector(s))
		error("only vectors can be sorted\n");

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

static void iFind(int * x, int n, int k)
{
	int L, R, i, j;
	int v, w;

	L = 0;
	R = n - 1;
	while (L < R) {
		v = x[k];
		i = L;
		j = R;
		do {
			while (icmp(x[i], v) < 0)
				i++;
			while (icmp(v, x[j]) < 0)
				j--;
			if (i <= j) {
				w = x[i];
				x[i] = x[j];
				x[j] = w;
				i++;
				j--;
			}
		} while (i <= j);
		if (j < k)
			L = i;
		if (k < i)
			R = j;
	}
}

void rFind(double * x, int n, int k)
{
	int L, R, i, j;
	double v, w;

	L = 0;
	R = n - 1;
	while (L < R) {
		v = x[k];
		i = L;
		j = R;
		do {
			while (rcmp(x[i], v) < 0)
				i++;
			while (rcmp(v, x[j]) < 0)
				j--;
			if (i <= j) {
				w = x[i];
				x[i] = x[j];
				x[j] = w;
				i++;
				j--;
			}
		} while (i <= j);
		if (j < k)
			L = i;
		if (k < i)
			R = j;
	}
}

void cFind(complex *x, int n, int k)
{
	int L, R, i, j;
	complex v, w;

	L = 0;
	R = n - 1;
	while (L < R) {
		v = x[k];
		i = L;
		j = R;
		do {
			while (ccmp(x[i], v) < 0)
				i++;
			while (ccmp(v, x[j]) < 0)
				j--;
			if (i <= j) {
				w = x[i];
				x[i] = x[j];
				x[j] = w;
				i++;
				j--;
			}
		} while (i <= j);
		if (j < k)
			L = i;
		if (k < i)
			R = j;
	}
}


void sFind(SEXP * x, int n, int k)
{
	int L, R, i, j;
	SEXP v, w;

	L = 0;
	R = n - 1;
	while (L < R) {
		v = x[k];
		i = L;
		j = R;
		do {
			while (scmp(x[i], v) < 0)
				i++;
			while (scmp(v, x[j]) < 0)
				j--;
			if (i <= j) {
				w = x[i];
				x[i] = x[j];
				x[j] = w;
				i++;
				j--;
			}
		} while (i <= j);
		if (j < k)
			L = i;
		if (k < i)
			R = j;
	}
}

void find(SEXP x, int k)
{
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
		iFind(INTEGER(x), LENGTH(x), k);
		break;
	case REALSXP:
		rFind(REAL(x), LENGTH(x), k);
		break;
	case CPLXSXP:
		cFind(COMPLEX(x), LENGTH(x), k);
		break;
	case STRSXP:
		sFind(STRING(x), LENGTH(x), k);
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
		error("only vectors can be sorted\n");
	CADR(args) = coerceVector(CADR(args), INTSXP);
	n = LENGTH(CAR(args));
	k = LENGTH(CADR(args));
	l = INTEGER(CADR(args));
	for (i = 0; i < k; i++) {
		if (l[i] == NA_INTEGER)
			error("NA index in find\n");
		if (l[i] < 1 || l[i] > n)
			error("index %d outside bounds in find\n", l[i]);
	}
	CAR(args) = duplicate(CAR(args));
	l = INTEGER(CADR(args));
	for (i = 0; i < k; i++)
		find(CAR(args), l[i] - 1);
	return CAR(args);
}

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

void orderVector(int * index, int n, SEXP key, int greater())
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
