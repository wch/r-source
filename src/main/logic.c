/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2003   The R Development Core Team.
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

#include "Defn.h"


static SEXP lunary(SEXP, SEXP, SEXP);
static SEXP lbinary(SEXP, SEXP, SEXP);
static SEXP binaryLogic(int code, SEXP s1, SEXP s2);


SEXP do_logic(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops",call, op, args, env, &ans))
	return ans;
    switch (length(args)) {
    case 1:
	return lunary(call, op, CAR(args));
    case 2:
	return lbinary(call, op, args);
    default:
	error("binary operations require two arguments");
	return R_NilValue;/* for -Wall */
    }
}

static SEXP lbinary(SEXP call, SEXP op, SEXP args)
{
/* logical binary : "&" or "|" */ 
    SEXP x, y, dims, tsp, class, xnames, ynames;
    int mismatch, nx, ny, xarray, yarray, xts, yts;
    mismatch = 0;
    x = CAR(args);
    y = CADR(args);
    if (!isNumeric(x) || !isNumeric(y))
	errorcall(call, "operations are possible only for numeric or logical types");
    tsp = R_NilValue;		/* -Wall */
    class = R_NilValue;		/* -Wall */
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		error("binary operation non-conformable arrays");
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else /*(yarray)*/ {
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
		errorcall(call, "Non-conformable time-series");
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(class = getAttrib(y, R_ClassSymbol));
	}
    }
    if(mismatch)
	warningcall(call, "longer object length\n\tis not a multiple of shorter object length");

    x = SETCAR(args, coerceVector(x, LGLSXP));
    y = SETCADR(args, coerceVector(y, LGLSXP));
    PROTECT(x = binaryLogic(PRIMVAL(op), x, y));

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

    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, class);
	UNPROTECT(2);
    }
    UNPROTECT(4);
    return x;
}

static SEXP lunary(SEXP call, SEXP op, SEXP arg)
{
    SEXP x, dim, dimnames, names;
    int i, len;

    if (!isLogical(arg) && !isNumeric(arg))
	errorcall(call, "invalid argument type");
    len = LENGTH(arg);
    PROTECT(names = getAttrib(arg, R_NamesSymbol));
    PROTECT(dim = getAttrib(arg, R_DimSymbol));
    PROTECT(dimnames = getAttrib(arg, R_DimNamesSymbol));
    PROTECT(x = allocVector(LGLSXP, len));
    switch(TYPEOF(arg)) {
    case LGLSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (LOGICAL(arg)[i] == NA_LOGICAL) ?
		NA_LOGICAL : LOGICAL(arg)[i] == 0;
	break;
    case INTSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = (INTEGER(arg)[i] == NA_INTEGER) ?
		NA_LOGICAL : INTEGER(arg)[i] == 0;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    LOGICAL(x)[i] = ISNAN(REAL(arg)[i]) ?
		NA_LOGICAL : REAL(arg)[i] == 0;
	break;
    }
    if(names != R_NilValue) setAttrib(x, R_NamesSymbol, names);
    if(dim != R_NilValue) setAttrib(x, R_DimSymbol, dim);
    if(dimnames != R_NilValue) setAttrib(x, R_DimNamesSymbol, dimnames);
    UNPROTECT(4);
    return x;
}

SEXP do_logic2(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*  &&	and  ||	 */
    SEXP s1, s2;
    int x1, x2;
    SEXP ans;

    if (length(args) != 2)
	error("`%s' operator requires 2 arguments",
	      PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = CAR(args);
    s2 = CADR(args);
    PROTECT(ans = allocVector(LGLSXP, 1));
    s1 = eval(s1, env);
    if (!isNumeric(s1))
	errorcall(call, "invalid `x' type in `x %s y'",
		  PRIMVAL(op) == 1 ? "&&" : "||"); 
    x1 = asLogical(s1);

#define get_2nd							\
	s2 = eval(s2, env);					\
	if (!isNumeric(s2))					\
	    errorcall(call, "invalid `y' type in `x %s y'",	\
		      PRIMVAL(op) == 1 ? "&&" : "||");		\
	x2 = asLogical(s2);

    switch (PRIMVAL(op)) {
    case 1: /* && */
	if (x1 == FALSE)
	    LOGICAL(ans)[0] = FALSE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL) 
		LOGICAL(ans)[0] = (x2 == NA_LOGICAL || x2) ? NA_LOGICAL : x2;
	    else /* x1 == TRUE */
		LOGICAL(ans)[0] = x2;
	}
	break;
    case 2: /* || */
	if (x1 == TRUE)
	    LOGICAL(ans)[0] = TRUE;
	else {
	    get_2nd;
	    if (x1 == NA_LOGICAL)
		LOGICAL(ans)[0] = (x2 == NA_LOGICAL || !x2) ? NA_LOGICAL : x2;
	    else /* x1 == FALSE */
		LOGICAL(ans)[0] = x2;
	}
    }
    UNPROTECT(1);
    return ans;
}

static SEXP binaryLogic(int code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2;
    int x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(LGLSXP, 0);
	return ans;
    }
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case 1:		/* & : AND */
	for (i = 0; i < n; i++) {
	    x1 = LOGICAL(s1)[i % n1];
	    x2 = LOGICAL(s2)[i % n2];
	    if (x1 == 0 || x2 == 0)
		LOGICAL(ans)[i] = 0;
	    else if (x1 == NA_LOGICAL || x2 == NA_LOGICAL)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    case 2:		/* | : OR */
	for (i = 0; i < n; i++) {
	    x1 = LOGICAL(s1)[i % n1];
	    x2 = LOGICAL(s2)[i % n2];
	    if ((x1 != NA_LOGICAL && x1) || (x2 != NA_LOGICAL && x2))
		LOGICAL(ans)[i] = 1;
	    else if (x1 == 0 && x2 == 0)
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = NA_LOGICAL;
	}
	break;
    }
    return ans;
}

static void checkValues(int * x, int n, Rboolean *haveFalse, Rboolean *haveTrue, Rboolean *haveNA);

SEXP do_logic3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t;
    int narm;
    Rboolean haveTrue;
    Rboolean haveFalse;
    Rboolean haveNA;

    if(DispatchGroup("Summary", call, op, args, env, &s))
	return s;

    ans = matchArg(R_NaRmSymbol, &args);
    narm = asLogical(ans);
    haveTrue = FALSE;
    haveFalse = FALSE;
    haveNA = FALSE;

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	if (LGLSXP <= TYPEOF(t) && TYPEOF(t) <= CPLXSXP) {
	    /* coerceVector protects its argument so this actually works
	       just fine */
	    t = coerceVector(t, LGLSXP);
	    checkValues(LOGICAL(t), LENGTH(t), &haveFalse, &haveTrue, &haveNA);
	}
	else if(!isNull(t))
	    errorcall(call, "incorrect argument type");
    }
    if (narm)
	haveNA = FALSE;

    s = allocVector(LGLSXP, 1L);
    if (PRIMVAL(op) == 1) {	/* ALL */
	LOGICAL(s)[0] = haveNA ? (haveFalse ? FALSE : NA_LOGICAL) : !haveFalse;
    } else {			/* ANY */
	LOGICAL(s)[0] = haveNA ? (haveTrue  ? TRUE  : NA_LOGICAL) : haveTrue;
    }
    return s;
}

static void checkValues(int * x, int n, Rboolean *haveFalse, Rboolean *haveTrue, Rboolean *haveNA)
{
    int i;
    for (i = 0; i < n; i++) {
	if (x[i] == NA_LOGICAL)
	    *haveNA = TRUE;
	else if (x[i])
	    *haveTrue = TRUE;
	else
	    *haveFalse = TRUE;
    }
}
