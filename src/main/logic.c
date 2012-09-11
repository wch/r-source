/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999--2012  The R Core Team.
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
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000


static SEXP lunary(SEXP, SEXP, SEXP);
static SEXP lbinary(SEXP, SEXP, SEXP);
static SEXP binaryLogic(int code, SEXP s1, SEXP s2);
static SEXP binaryLogic2(int code, SEXP s1, SEXP s2);


/* & | ! */
SEXP attribute_hidden do_logic(SEXP call, SEXP op, SEXP args, SEXP env)
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
	error(_("binary operations require two arguments"));
	return R_NilValue;	/* for -Wall */
    }
}

#define isRaw(x) (TYPEOF(x) == RAWSXP)
static SEXP lbinary(SEXP call, SEXP op, SEXP args)
{
/* logical binary : "&" or "|" */
    SEXP x, y, dims, tsp, klass, xnames, ynames;
    R_xlen_t mismatch, nx, ny;
    int xarray, yarray, xts, yts;
    mismatch = 0;
    x = CAR(args);
    y = CADR(args);
    if (isRaw(x) && isRaw(y)) {
    }
    else if (!isNumber(x) || !isNumber(y))
    	errorcall(call,
    		  _("operations are possible only for numeric, logical or complex types"));
    tsp = R_NilValue;		/* -Wall */
    klass = R_NilValue;		/* -Wall */
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		error(_("binary operation on non-conformable arrays"));
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
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }
    nx = XLENGTH(x);
    ny = XLENGTH(y);
    if(nx > 0 && ny > 0) {
	if(nx > ny) mismatch = nx % ny;
	else mismatch = ny % nx;
    }
    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, _("non-conformable time series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (XLENGTH(x) < XLENGTH(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (XLENGTH(y) < XLENGTH(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }
    if(mismatch)
	warningcall(call,
		    _("longer object length is not a multiple of shorter object length"));

    if (isRaw(x) && isRaw(y)) {
	PROTECT(x = binaryLogic2(PRIMVAL(op), x, y));
    } else {
	if (!isNumber(x) || !isNumber(y))
	    errorcall(call,
		      _("operations are possible only for numeric, logical or complex types"));
	x = SETCAR(args, coerceVector(x, LGLSXP));
	y = SETCADR(args, coerceVector(y, LGLSXP));
	PROTECT(x = binaryLogic(PRIMVAL(op), x, y));
    }


    if (dims != R_NilValue) {
	setAttrib(x, R_DimSymbol, dims);
	if(xnames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, xnames);
	else if(ynames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, ynames);
    }
    else {
	if(XLENGTH(x) == XLENGTH(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if(XLENGTH(x) == XLENGTH(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }

    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, klass);
	UNPROTECT(2);
    }
    UNPROTECT(4);
    return x;
}

static SEXP lunary(SEXP call, SEXP op, SEXP arg)
{
    SEXP x, dim, dimnames, names;
    R_xlen_t i, len;

    len = XLENGTH(arg);
    if (!isLogical(arg) && !isNumber(arg) && !isRaw(arg)) {
	/* For back-compatibility */
	if (!len) return allocVector(LGLSXP, 0);
	errorcall(call, _("invalid argument type"));
    }
    PROTECT(names = getAttrib(arg, R_NamesSymbol));
    PROTECT(dim = getAttrib(arg, R_DimSymbol));
    PROTECT(dimnames = getAttrib(arg, R_DimNamesSymbol));
    PROTECT(x = allocVector(isRaw(arg) ? RAWSXP : LGLSXP, len));
    switch(TYPEOF(arg)) {
    case LGLSXP:
	for (i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(x)[i] = (LOGICAL(arg)[i] == NA_LOGICAL) ?
		NA_LOGICAL : LOGICAL(arg)[i] == 0;
	}
	break;
    case INTSXP:
	for (i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(x)[i] = (INTEGER(arg)[i] == NA_INTEGER) ?
		NA_LOGICAL : INTEGER(arg)[i] == 0;
	}
	break;
    case REALSXP:
	for (i = 0; i < len; i++){
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(x)[i] = ISNAN(REAL(arg)[i]) ?
		NA_LOGICAL : REAL(arg)[i] == 0;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    LOGICAL(x)[i] = (ISNAN(COMPLEX(arg)[i].r) || ISNAN(COMPLEX(arg)[i].i))
		? NA_LOGICAL : (COMPLEX(arg)[i].r == 0. && COMPLEX(arg)[i].i == 0.);
	}
	break;
    case RAWSXP:
	for (i = 0; i < len; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    RAW(x)[i] = 0xFF ^ RAW(arg)[i];
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("lunary", arg);
    }
    if(names != R_NilValue) setAttrib(x, R_NamesSymbol, names);
    if(dim != R_NilValue) setAttrib(x, R_DimSymbol, dim);
    if(dimnames != R_NilValue) setAttrib(x, R_DimNamesSymbol, dimnames);
    UNPROTECT(4);
    return x;
}

/* && || */
SEXP attribute_hidden do_logic2(SEXP call, SEXP op, SEXP args, SEXP env)
{
/*  &&	and  ||	 */
    SEXP s1, s2;
    int x1, x2;
    SEXP ans;

    if (length(args) != 2)
	error(_("'%s' operator requires 2 arguments"),
	      PRIMVAL(op) == 1 ? "&&" : "||");

    s1 = CAR(args);
    s2 = CADR(args);
    PROTECT(ans = allocVector(LGLSXP, 1));
    s1 = eval(s1, env);
    if (!isNumber(s1))
	errorcall(call, _("invalid 'x' type in 'x %s y'"),
		  PRIMVAL(op) == 1 ? "&&" : "||");
    x1 = asLogical(s1);

#define get_2nd							\
	s2 = eval(s2, env);					\
	if (!isNumber(s2))					\
	    errorcall(call, _("invalid 'y' type in 'x %s y'"),	\
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
    R_xlen_t i, n, n1, n2;
    int x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(LGLSXP, 0);
	return ans;
    }
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case 1:		/* & : AND */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
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
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
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
    case 3:
	error(_("Unary operator `!' called with two arguments"));
	break;
    }
    return ans;
}

static SEXP binaryLogic2(int code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    if (n1 == 0 || n2 == 0) {
	ans = allocVector(RAWSXP, 0);
	return ans;
    }
    ans = allocVector(RAWSXP, n);

    switch (code) {
    case 1:		/* & : AND */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i % n1];
	    x2 = RAW(s2)[i % n2];
	    RAW(ans)[i] = x1 & x2;
	}
	break;
    case 2:		/* | : OR */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = RAW(s1)[i % n1];
	    x2 = RAW(s2)[i % n2];
	    RAW(ans)[i] = x1 | x2;
	}
	break;
    }
    return ans;
}

#define _OP_ALL 1
#define _OP_ANY 2

static int checkValues(int op, int na_rm, int *x, R_xlen_t n)
{
    R_xlen_t i;
    int has_na = 0;
    for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
        if (!na_rm && x[i] == NA_LOGICAL) has_na = 1;
        else {
            if (x[i] == TRUE && op == _OP_ANY) return TRUE;
            if (x[i] == FALSE && op == _OP_ALL) return FALSE;
        }
    }
    switch (op) {
    case _OP_ANY:
        return has_na ? NA_LOGICAL : FALSE;
    case _OP_ALL:
        return has_na ? NA_LOGICAL : TRUE;
    default:
        error("bad op value for do_logic3");
    }
    return NA_LOGICAL; /* -Wall */
}

extern SEXP fixup_NaRm(SEXP args); /* summary.c */

/* all, any */
SEXP attribute_hidden do_logic3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, s, t, call2;
    int narm, has_na = 0;
    /* initialize for behavior on empty vector
       all(logical(0)) -> TRUE
       any(logical(0)) -> FALSE
     */
    Rboolean val = PRIMVAL(op) == _OP_ALL ? TRUE : FALSE;

    PROTECT(args = fixup_NaRm(args));
    PROTECT(call2 = duplicate(call));
    SETCDR(call2, args);

    if (DispatchGroup("Summary", call2, op, args, env, &ans)) {
	UNPROTECT(2);
	return(ans);
    }

    ans = matchArgExact(R_NaRmSymbol, &args);
    narm = asLogical(ans);

    for (s = args; s != R_NilValue; s = CDR(s)) {
	t = CAR(s);
	/* Avoid memory waste from coercing empty inputs, and also
	   avoid warnings with empty lists coming from sapply */
	if(xlength(t) == 0) continue;
	/* coerceVector protects its argument so this actually works
	   just fine */
	if (TYPEOF(t) != LGLSXP) {
	    /* Coercion of integers seems reasonably safe, but for
	       other types it is more often than not an error.
	       One exception is perhaps the result of lapply, but
	       then sapply was often what was intended. */
	    if(TYPEOF(t) != INTSXP)
		warningcall(call,
			    _("coercing argument of type '%s' to logical"),
			    type2char(TYPEOF(t)));
	    t = coerceVector(t, LGLSXP);
	}
	val = checkValues(PRIMVAL(op), narm, LOGICAL(t), XLENGTH(t));
        if (val != NA_LOGICAL) {
            if ((PRIMVAL(op) == _OP_ANY && val)
                || (PRIMVAL(op) == _OP_ALL && !val)) {
                has_na = 0;
                break;
            }
        } else has_na = 1;
    }
    UNPROTECT(2);
    return has_na ? ScalarLogical(NA_LOGICAL) : ScalarLogical(val);
}
#undef _OP_ALL
#undef _OP_ANY
