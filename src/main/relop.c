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

static SEXP integer_relop(int code, SEXP s1, SEXP s2);
static SEXP real_relop(int code, SEXP s1, SEXP s2);
static SEXP complex_relop(int code, SEXP s1, SEXP s2);
static SEXP string_relop(int code, SEXP s1, SEXP s2);

static SEXP rcall;

static void CheckFactorsConform(SEXP x, SEXP y)
{
	SEXP levels1, levels2;
	int i;

	if(TYPEOF(x) == TYPEOF(y) && LEVELS(x) == LEVELS(y)) {
		levels1 = getAttrib(x, R_LevelsSymbol);
		levels2 = getAttrib(x, R_LevelsSymbol);
		if(levels1 == R_NilValue && levels2 == R_NilValue)
			return;
		if(levels1 != R_NilValue && levels2 != R_NilValue) {
			for(i=0 ; i<(int)LEVELS(x) ; i++)
				if(strcmp(CHAR(STRING(levels1)[i]), CHAR(STRING(levels2)[i])))
					goto invalid;
		}
		return;
	}
invalid:
	errorcall(rcall, "non-comparable factors\n");
}

/* Coerce x to be a factor like y */
/* Actually, match the contents of x with the levels of y */
static SEXP FactorCoerce(SEXP x, SEXP y)
{
	SEXP labels, ans;
	int i, j, found;

	x = coerceVector(x, STRSXP);
	labels = getAttrib(y, install("levels"));
	if(TYPEOF(labels) != STRSXP) goto invalid;
	ans = allocVector(INTSXP, LENGTH(x));
	for(i=0 ; i<LENGTH(x) ; i++) {
		INTEGER(ans)[i] = NA_INTEGER;
		if(STRING(x)[i] != NA_STRING) {
			found = 0;
			for(j=0 ; j<LENGTH(labels) ; j++) {
				if(!strcmp(CHAR(STRING(x)[i]), CHAR(STRING(labels)[j]))) {
					INTEGER(ans)[i] = j+1;
					found = 1;
					break;
				}
			}
			if(!found) goto invalid;
		}
	}
	return ans;
invalid:
	errorcall(rcall, "invalid factor level in comparison\n");
	/*NOTREACHED*/
}

SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP x, y, class, dims, tsp, xnames, ynames, ans;
	int mismatch, nx, ny, xarray, yarray, xts, yts;

	if( DispatchGroup("Ops", call, op, args, env, &ans) )
		return ans;

	x = CAR(args);
	y = CADR(args);

	if(isSymbol(x)) {
		PROTECT(x);
		x = allocVector(STRSXP, 1);
		STRING(x)[0] = PRINTNAME(CAR(args));
		CAR(args) = x;
		UNPROTECT(1);
	}
	if(isSymbol(y)) {
		PROTECT(y);
		y = allocVector(STRSXP, 1);
		STRING(y)[0] = PRINTNAME(CADR(args));
		CADR(args) = y;
		UNPROTECT(1);
	}

	if (!isVector(x) || !isVector(y))
		error("comparison is possible only for vector types\n");

	if(LENGTH(x) <= 0 || LENGTH(y) <= 0)
		return allocVector(LGLSXP,0);

	rcall = call;
	mismatch = 0;
	xarray = isArray(x);
	yarray = isArray(y);
	xts = isTs(x);
	yts = isTs(y);
	if (xarray || yarray) {
		if (xarray && yarray) {
			if (!conformable(x, y))
				errorcall(call, "non-conformable arrays\n");
			PROTECT(dims = getAttrib(x, R_DimSymbol));
		}
		else if (xarray) {
			PROTECT(dims = getAttrib(x, R_DimSymbol));
		}
		else if (yarray) {
			PROTECT(dims = getAttrib(y, R_DimSymbol));
		}
		PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
		PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
	}
	else {
		nx = length(x);
		ny = length(y);
		if(nx > 0 && ny > 0) {
			if(nx > ny) mismatch = nx % ny;
			else mismatch = ny % nx;
		}
		PROTECT(dims = R_NilValue);
		PROTECT(xnames = getAttrib(x, R_NamesSymbol));
		PROTECT(ynames = getAttrib(y, R_NamesSymbol));
	}
	if (xts || yts) {
		if (xts && yts) {
			if (!tsConform(x, y))
				errorcall(call, "Non-conformable time-series\n");
			PROTECT(tsp = getAttrib(x, R_TspSymbol));
			PROTECT(class = getAttrib(x, R_ClassSymbol));
		}
		else if (xts) {
			if (length(x) < length(y))
				ErrorMessage(call, ERROR_TSVEC_MISMATCH);
			PROTECT(tsp = getAttrib(x, R_TspSymbol));
			PROTECT(class = getAttrib(x, R_ClassSymbol));
		}
		else if (yts) {
			if (length(y) < length(x))
				ErrorMessage(call, ERROR_TSVEC_MISMATCH);
			PROTECT(tsp = getAttrib(y, R_TspSymbol));
			PROTECT(class = getAttrib(y, R_ClassSymbol));
		}
	}
	if(mismatch) warningcall(call, "longer object length\n\tis not a multiple of shorter object length\n");

	if (isFactor(x) || isFactor(y)) {
		if(isFactor(x) && isFactor(y)) {
			CheckFactorsConform(x, y);
		}
		else if(isFactor(y)) {
			if(isUnordered(y) && (PRIMVAL(op) != EQOP) && (PRIMVAL(op) != NEOP))
				errorcall(call, "comparison not possible for factors\n");
			x = CAR(args) = FactorCoerce(x, y);
		}
		else {
			if(isUnordered(x) && (PRIMVAL(op) != EQOP) && (PRIMVAL(op) != NEOP))
				errorcall(call, "comparison not possible for factors\n");
			y = CADR(args) = FactorCoerce(y, x);
		}
		x = integer_relop(PRIMVAL(op), x, y);
	}
	else if (isString(x) || isString(y)) {
		x = CAR(args) = coerceVector(x, STRSXP);
		y = CADR(args) = coerceVector(y, STRSXP);
		x = string_relop(PRIMVAL(op), x, y);
	}
#ifdef COMPLEX_DATA
	else if (isComplex(x) || isComplex(y)) {
		x = CAR(args) = coerceVector(x, CPLXSXP);
		y = CADR(args) = coerceVector(y, CPLXSXP);
		x = complex_relop(PRIMVAL(op), x, y);
	}
#endif
	else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
		x = CAR(args) = coerceVector(x, REALSXP);
		y = CADR(args) = coerceVector(y, REALSXP);
		x = real_relop(PRIMVAL(op), x, y);
	}
	else {
		x = integer_relop(PRIMVAL(op), x, y);
	}

	PROTECT(x);
	if (xts || yts) {
		setAttrib(x, R_TspSymbol, tsp);
		setAttrib(x, R_ClassSymbol, class);
		UNPROTECT(2);
	}
	if (dims != R_NilValue) {
		setAttrib(x, R_DimSymbol, dims);
		if(xnames != R_NilValue)
			setAttrib(x, R_DimNamesSymbol, xnames);
		else if(ynames != R_NilValue)
			setAttrib(x, R_DimNamesSymbol, ynames);
	}
	else {
		if(length(x) == length(xnames))
			setAttrib(x, R_NamesSymbol, xnames);
		else if(length(x) == length(ynames))
			setAttrib(x, R_NamesSymbol, ynames);
	}

	UNPROTECT(4);
	return x;

}

static SEXP integer_relop(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
	int x1, x2;
	SEXP ans;

	n1 = LENGTH(s1);
	n2 = LENGTH(s2);
	n = (n1 > n2) ? n1 : n2;
	PROTECT(s1);
	PROTECT(s2);
	ans = allocVector(LGLSXP, n);

	switch (code) {
	case EQOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 == x2);
		}
		break;
	case NEOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 != x2);
		}
		break;
	case LTOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 < x2);
		}
		break;
	case GTOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 > x2);
		}
		break;
	case LEOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 <= x2);
		}
		break;
	case GEOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				LOGICAL(ans)[i] = NA_LOGICAL;
			else
				LOGICAL(ans)[i] = (x1 >= x2);
		}
		break;
	}
	UNPROTECT(2);
	return ans;
}

static SEXP real_relop(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
	double x1, x2;
	SEXP ans;

	n1 = LENGTH(s1);
	n2 = LENGTH(s2);
	n = (n1 > n2) ? n1 : n2;
	PROTECT(s1);
	PROTECT(s2);
	ans = allocVector(LGLSXP, n);

	switch (code) {
	case EQOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 == x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case NEOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 != x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case LTOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 < x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case GTOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 > x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case LEOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 <= x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case GEOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				LOGICAL(ans)[i] = (x1 >= x2);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	}
	UNPROTECT(2);
	return ans;
}

#ifdef COMPLEX_DATA
static SEXP complex_relop(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
	complex x1, x2;
	SEXP ans;

	if(code != EQOP && code != NEOP) {
		errorcall(rcall, "illegal comparison with complex values\n");
	}

	n1 = LENGTH(s1);
	n2 = LENGTH(s2);
	n = (n1 > n2) ? n1 : n2;
	PROTECT(s1);
	PROTECT(s2);
	ans = allocVector(LGLSXP, n);

	switch (code) {
	case EQOP:
		for (i = 0; i < n; i++) {
			x1 = COMPLEX(s1)[i % n1];
			x2 = COMPLEX(s2)[i % n2];
			if (FINITE(x1.r) && FINITE(x1.i) && FINITE(x2.r) && FINITE(x2.i))
				LOGICAL(ans)[i] = (x1.r == x2.r && x1.i == x2.i);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	case NEOP:
		for (i = 0; i < n; i++) {
			x1 = COMPLEX(s1)[i % n1];
			x2 = COMPLEX(s2)[i % n2];
			if (FINITE(x1.r) && FINITE(x1.i) && FINITE(x2.r) && FINITE(x2.i))
				LOGICAL(ans)[i] = (x1.r != x2.r || x1.i != x2.i);
			else
				LOGICAL(ans)[i] = NA_LOGICAL;
		}
		break;
	}
	UNPROTECT(2);
	return ans;
}
#endif

static SEXP string_relop(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
	SEXP ans;

	n1 = LENGTH(s1);
	n2 = LENGTH(s2);
	n = (n1 > n2) ? n1 : n2;
	PROTECT(s1);
	PROTECT(s2);
	ans = allocVector(LGLSXP, n);

	switch (code) {
	case EQOP:
		for (i = 0; i < n; i++) {
			if (strcmp(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) == 0)
				LOGICAL(ans)[i] = 1;
			else
				LOGICAL(ans)[i] = 0;
		}
		break;
	case NEOP:
		for (i = 0; i < n; i++) {
			if (streql(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) != 0)
				LOGICAL(ans)[i] = 0;
			else
				LOGICAL(ans)[i] = 1;
		}
		break;
	case LTOP:
		for (i = 0; i < n; i++) {
			if (strcmp(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) < 0)
				LOGICAL(ans)[i] = 1;
			else
				LOGICAL(ans)[i] = 0;
		}
		break;
	case GTOP:
		for (i = 0; i < n; i++) {
			if (strcmp(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) > 0)
				LOGICAL(ans)[i] = 1;
			else
				LOGICAL(ans)[i] = 0;
		}
		break;
	case LEOP:
		for (i = 0; i < n; i++) {
			if (strcmp(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) <= 0)
				LOGICAL(ans)[i] = 1;
			else
				LOGICAL(ans)[i] = 0;
		}
		break;
	case GEOP:
		for (i = 0; i < n; i++) {
			if (strcmp(CHAR(STRING(s1)[i % n1]), CHAR(STRING(s2)[i % n2])) >= 0)
				LOGICAL(ans)[i] = 1;
			else
				LOGICAL(ans)[i] = 0;
		}
		break;
	}
	UNPROTECT(2);
	return ans;
}
