/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-1999	    The R Development Core Team.
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
#include <Rconfig.h>
#endif

#include "Defn.h"		/*-> Arith.h */
#include "Mathlib.h"
#include "Applic.h"		/* machar */
#include "arithmetic.h"

/* Error Handling for Floating Point Errors */

#ifndef IEEE_754
#include <signal.h>

static RETSIGTYPE handle_fperror(int dummy)
{
    errno = ERANGE;
#ifdef Unix
    signal(SIGFPE, handle_fperror);
#endif
}
#endif

#ifdef HAVE_MATHERR

/* Override the SVID matherr function */

int matherr(struct exception *exc)
{
    switch (exc->type) {
    case DOMAIN:
    case SING:
	errno = EDOM;
	break;
    case OVERFLOW:
	errno = ERANGE;
	break;
    case UNDERFLOW:
	exc->retval = 0.0;
	break;
    }
    return 1;
}
#endif

#ifdef IEEE_754
double R_Zero_Hack = 0.0;	/* Silence the Sun compiler */

typedef union
{
  double value;
  unsigned int word[2];
} ieee_double;

static int little_endian;
static int hw;
static int lw;

static void establish_endianness()
{
    ieee_double x;
    x.value = 1;
    if (x.word[0] == 0x3ff00000) {
	little_endian = 0;
	hw = 0;
	lw = 1;
    }
    else if (x.word[1] == 0x3ff00000) {
	little_endian = 1;
	hw = 1;
	lw = 0;
    }
    else R_Suicide("couldn't determine endianness for IEEE 754!\n");
}

static double R_ValueOfNA(void)
{
    ieee_double x;
    x.word[hw] = 0x7ff00000;
    x.word[lw] = 1954;
    return x.value;
}

int R_IsNA(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] == 1954);
    }
    return 0;
}

int R_IsNaN(double x)
{
    if (isnan(x)) {
	ieee_double y;
	y.value = x;
	return (y.word[lw] != 1954);
    }
    return 0;
}
#endif

/* Arithmetic Initialization */

void InitArithmetic()
{
    R_NaInt = INT_MIN;

#ifdef IEEE_754
    establish_endianness();
    R_NaN = 0.0/R_Zero_Hack;
    R_NaReal = R_ValueOfNA();
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
#else
    R_NaN = -DBL_MAX;
    R_NaReal = R_NaN;
    R_PosInf = -DBL_MAX;
    R_NegInf = DBL_MAX;
#ifdef Unix
    signal(SIGFPE, handle_fperror);
#endif
#endif
}


/* Machine Constants */

SEXP do_Machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
    SEXP ans, nms;

    checkArity(op, args);

    machar(&ibeta, &it, &irnd, &ngrd, &machep, &negep, &iexp,
	   &minexp, &maxexp, &eps, &epsneg, &xmin, &xmax);

    PROTECT(ans = allocVector(VECSXP, 14));
    PROTECT(nms = allocVector(STRSXP, 14));
    STRING(nms)[0] = mkChar("double.eps");
    VECTOR(ans)[0] = ScalarReal(eps);

    STRING(nms)[1] = mkChar("double.neg.eps");
    VECTOR(ans)[1] = ScalarReal(epsneg);

    STRING(nms)[2] = mkChar("double.xmin");
    VECTOR(ans)[2] = ScalarReal(xmin);

    STRING(nms)[3] = mkChar("double.xmax");
    VECTOR(ans)[3] = ScalarReal(xmax);

    STRING(nms)[4] = mkChar("double.base");
    VECTOR(ans)[4] = ScalarInteger(ibeta);

    STRING(nms)[5] = mkChar("double.digits");
    VECTOR(ans)[5] = ScalarInteger(it);

    STRING(nms)[6] = mkChar("double.rounding");
    VECTOR(ans)[6] = ScalarInteger(irnd);

    STRING(nms)[7] = mkChar("double.guard");
    VECTOR(ans)[7] = ScalarInteger(ngrd);

    STRING(nms)[8] = mkChar("double.ulp.digits");
    VECTOR(ans)[8] = ScalarInteger(machep);

    STRING(nms)[9] = mkChar("double.neg.ulp.digits");
    VECTOR(ans)[9] = ScalarInteger(negep);

    STRING(nms)[10] = mkChar("double.exponent");
    VECTOR(ans)[10] = ScalarInteger(iexp);

    STRING(nms)[11] = mkChar("double.min.exp");
    VECTOR(ans)[11] = ScalarInteger(minexp);

    STRING(nms)[12] = mkChar("double.max.exp");
    VECTOR(ans)[12] = ScalarInteger(maxexp);

    STRING(nms)[13] = mkChar("integer.max");
    VECTOR(ans)[13] = ScalarInteger(INT_MAX);
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

static double myfmod(double x1, double x2)
{
    double q = x1 / x2;
    return x1 - floor(q) * x2;
}


#ifdef LOG_BROKEN
double R_log(double x) { return(x > 0 ? log(x) : x < 0 ? R_NaN : R_NegInf); }
#else
# define R_log	log
#endif

#ifdef POW_DIRTY

# define R_pow	pow

#else
double R_pow(double x, double y) /* = x ^ y */
{
    if(x == 1. || y == 0.)
	return(1.);
    if (R_FINITE(x) && R_FINITE(y))
	return(pow(x,y));
    if (ISNAN(x) || ISNAN(y)) {
#ifdef IEEE_754
	return(x + y);
#else
	return(NA_REAL);
#endif
    }
    if(!R_FINITE(x)) {
	if(x > 0)		/* Inf ^ y */
	    return((y < 0.)? 0. : R_PosInf);
	else {			/* (-Inf) ^ y */
	    if(R_FINITE(y) && y == floor(y)) /* (-Inf) ^ n */
		return((y < 0.) ? 0. : (myfmod(y,2.) ? x  : -x));
	}
    }
    if(!R_FINITE(y)) {
	if(x >= 0) {
	    if(y > 0)		/* y == +Inf */
		return((x >= 1)? R_PosInf : 0.);
	    else		/* y == -Inf */
		return((x < 1) ? R_PosInf : 0.);
	}
    }
    return(R_NaN);		/* all other cases: (-Inf)^{+-Inf,
				   non-int}; (neg)^{+-Inf} */
}
#endif

/* General Base Logarithms */

double logbase(double x, double base)
{
    return R_log(x) / log(base);
}

static SEXP unary(SEXP, SEXP);
static SEXP binary(SEXP, SEXP);
static SEXP integer_unary(int, SEXP);
static SEXP real_unary(int, SEXP);
static SEXP real_binary(int, SEXP, SEXP);
static SEXP integer_binary(int, SEXP, SEXP);

static int naflag;
static SEXP lcall;


/* Unary and Binary Operators */

SEXP do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    lcall = call;
    switch (length(args)) {
    case 1:
	return unary(op, CAR(args));
    case 2:
	return binary(op, args);
    default:
	error("operator with more than two arguments");
    }
    return ans;			/* never used; to keep -Wall happy */
}


static SEXP binary(SEXP op, SEXP args)
{
    SEXP x, y, class, dims, tsp, xnames, ynames;
    int mismatch, nx, ny, xarray, yarray, xts, yts;

    x = CAR(args);
    y = CADR(args);

    /* fix up NULL */
    if (isNull(x))
	x = CAR(args) = allocVector(REALSXP,0);
    if (isNull(y))
	y = CADR(args) = allocVector(REALSXP,0);

    if (!(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y))) {
	errorcall(lcall, "non-numeric argument to binary operator");
	return R_NilValue;	/* -Wall */
    }

    mismatch = 0;
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);

    /* If either x or y is a matrix with length 1 and the other is a
       vector, we want to coerce the matrix to be a vector. */

    /* FIXME: Danger Will Robinson.
     * -----  We might be trashing arguments here.
     * If we have NAMED(x) or NAMED(y) we should duplicate!
     */
    if (xarray != yarray) {
	if (xarray && length(x)==1 && length(y)!=1) {
	    x = CAR(args) = duplicate(x);
	    setAttrib(x, R_DimSymbol, R_NilValue);
	}
	if (yarray && length(y)==1 && length(x)!=1) {
	    y = CADR(args) = duplicate(y);
	    setAttrib(y, R_DimSymbol, R_NilValue);
	}
    }

    if (xarray || yarray) {
	nx = length(x);
	ny = length(y);
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(lcall, "non-conformable arrays");
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else {			/* (yarray) */
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	nx = length(x);
	ny = length(y);
	if (nx > 0 && ny > 0) {
	    if (nx > ny) mismatch = nx % ny;
	    else mismatch = ny % nx;
	}
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }

    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(lcall, "Non-conformable time-series");
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else {			/* (yts) */ 
	    if (length(y) < length(x))
		ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(class = getAttrib(y, R_ClassSymbol));
	}
    } else {
	class = tsp = R_NilValue; /* -Wall */
    }

    if (mismatch)
	warningcall(lcall, "longer object length\n\tis not a multiple of shorter object length");

    if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
	x = CAR(args) = coerceVector(x, CPLXSXP);
	y = CADR(args) = coerceVector(y, CPLXSXP);
	x = complex_binary(PRIMVAL(op), x, y);
    }
    else
	if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	    x = CAR(args) = coerceVector(x, REALSXP);
	    y = CADR(args) = coerceVector(y, REALSXP);
	    x = real_binary(PRIMVAL(op), x, y);
	}
	else {
	    x = integer_binary(PRIMVAL(op), x, y);
	}

    PROTECT(x);
    /* Don't set the dims if one argument is an array of size 0 and the
       other isn't of size zero, cos they're wrong */ 
    if (dims != R_NilValue) {
	if (!((xarray && (nx == 0) && (ny != 0)) ||
	      (yarray && (ny == 0) && (nx != 0)))){
	    setAttrib(x, R_DimSymbol, dims);
	    if (xnames != R_NilValue)
		setAttrib(x, R_DimNamesSymbol, xnames);
	    else if (ynames != R_NilValue)
		setAttrib(x, R_DimNamesSymbol, ynames);
	}
    }
    else {
	if (length(x) == length(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if (length(x) == length(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }

    if (xts || yts) {		/* must set *after* dims! */
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, class);
	UNPROTECT(2);
    }

    UNPROTECT(4);
    return x;
}

static SEXP unary(SEXP op, SEXP s1)
{
    switch (TYPEOF(s1)) {
    case LGLSXP:
    case INTSXP:
	return integer_unary(PRIMVAL(op), s1);
    case REALSXP:
	return real_unary(PRIMVAL(op), s1);
    case CPLXSXP:
	return complex_unary(PRIMVAL(op), s1);
    default:
	errorcall(lcall, "Invalid argument to unary operator");
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP integer_unary(int code, SEXP s1)
{
    int i, n, x;
    SEXP ans;

    switch (code) {
    case PLUSOP:
	return s1;
    case MINUSOP:
	ans = duplicate(s1);
	n = LENGTH(s1);
	for (i = 0; i < n; i++) {
	    x = INTEGER(s1)[i];
	    INTEGER(ans)[i] = (x == NA_INTEGER) ?
		NA_INTEGER : ((x == 0.0) ? 0 : -x);
	}
	return ans;
    default:
	error("illegal unary operator");
    }
    return s1;			/* never used; to keep -Wall happy */
}

static SEXP real_unary(int code, SEXP s1)
{
    int i, n;
    SEXP ans;

    switch (code) {
    case PLUSOP: return s1;
    case MINUSOP:
	ans = duplicate(s1);
	n = LENGTH(s1);
	for (i = 0; i < n; i++) {
#ifdef IEEE_754
	    REAL(ans)[i] = -REAL(s1)[i];
#else
	    double x;
	    x = REAL(s1)[i];
	    REAL(ans)[i] = ISNA(x) ? NA_REAL :
		((x == 0.0) ? 0.0 : -x);
#endif
	}
	return ans;
    default:
	errorcall(lcall, "illegal unary operator");
    }
    return s1;			/* never used; to keep -Wall happy */
}

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
(++i1, (i1==n1)&&(i1=0), ++i2, (i2==n2)&&(i2=0), ++i))

static SEXP integer_binary(int code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    int x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;

    if (code == DIVOP || code == POWOP)
	ans = allocVector(REALSXP, n);
    else
	ans = allocVector(INTSXP, n);

    if (n1 < 1 || n2 < 1) {
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = NA_INTEGER;
	return ans;
    }

    switch (code) {
    case PLUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else
		INTEGER(ans)[i] = x1 + x2;
	}
	break;
    case MINUSOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else
		INTEGER(ans)[i] = x1 - x2;
	}
	break;
    case TIMESOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else
		INTEGER(ans)[i] = x1 * x2;
	}
	break;
    case DIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
#ifdef IEEE_754
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
#else
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
#endif
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = (double) x1 / (double) x2;
	}
	break;
    case POWOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		REAL(ans)[i] = NA_REAL;
	    else {
		REAL(ans)[i] = MATH_CHECK(R_pow((double) x1, (double) x2));
	    }
	}
	break;
    case MODOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
		INTEGER(ans)[i] = NA_INTEGER;
	    else {
		INTEGER(ans)[i] = /* till 0.63.2:	x1 % x2 */
		    (x1 >= 0 && x2 > 0) ? x1 % x2 :
		    (int)myfmod((double)x1,(double)x2);
	    }
	}
	break;
    case IDIVOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		INTEGER(ans)[i] = NA_INTEGER;
	    else if (x2 == 0)
		INTEGER(ans)[i] = 0;
	    else
		INTEGER(ans)[i] = floor((double)x1 / (double)x2);
	}
	break;
    }

    /* Copy attributes from longest argument. */

    if (n1 > n2)
	copyMostAttrib(s1, ans);
    else if (n1 == n2) {
	copyMostAttrib(s2, ans);
	copyMostAttrib(s1, ans);
    }
    else
	copyMostAttrib(s2, ans);

    return ans;
}

static SEXP real_binary(int code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    SEXP ans;
#ifndef IEEE_754
    double x1, x2;
#endif

    /* Note: "s1" and "s2" are protected above. */
    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    ans = allocVector(REALSXP, n);

    if (n1 < 1 || n2 < 1) {
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = NA_REAL;
	return ans;
    }

    switch (code) {
    case PLUSOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = REAL(s1)[i1] + REAL(s2)[i2];
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2))
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(x1 + x2);
#endif
	}
	break;
    case MINUSOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = REAL(s1)[i1] - REAL(s2)[i2];
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2))
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(x1 - x2);
#endif
	}
	break;
    case TIMESOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = REAL(s1)[i1] * REAL(s2)[i2];
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) && ISNA(x2))
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(x1 * x2);
#endif
	}
	break;
    case DIVOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = REAL(s1)[i1] / REAL(s2)[i2];
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2) || x2 == 0)
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(x1 / x2);
#endif
	}
	break;
    case POWOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = R_pow(REAL(s1)[i1], REAL(s2)[i2]);
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2))
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(R_pow(x1, x2));
#endif
	}
	break;
    case MODOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = myfmod(REAL(s1)[i1], REAL(s2)[i2]);
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2) || x2 == 0)
		REAL(ans)[i] = NA_REAL;
	    else
		REAL(ans)[i] = MATH_CHECK(myfmod(x1, x2));
#endif
	}
	break;
    case IDIVOP:
	mod_iterate(n1, n2, i1, i2) {
#ifdef IEEE_754
	    REAL(ans)[i] = floor(REAL(s1)[i1] / REAL(s2)[i2]);
#else
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNA(x1) || ISNA(x2))
		REAL(ans)[i] = NA_REAL;
	    else {
		if (x2 == 0)
		REAL(ans)[i] = 0;
		else
		    REAL(ans)[i] = MATH_CHECK(floor(x1 / x2));
	    }
#endif
	}
	break;
    }

    /* Copy attributes from longest argument. */

    if (n1 > n2)
	copyMostAttrib(s1, ans);
    else if (n1 == n2) {
	copyMostAttrib(s2, ans);
	copyMostAttrib(s1, ans);
    }
    else
	copyMostAttrib(s2, ans);

    return ans;
}

/* Mathematical Functions of One Argument */

#if !defined(HAVE_ASINH) || !defined(HAVE_ACOSH) || !defined(HAVE_ATANH)
static double unavailable(double x)
{
    errorcall(lcall, "function unavailable in this R");
    return 0.;			/* to keep -Wall happy */
}
#ifndef HAVE_ASINH
#define asinh unavailable
#endif
#ifndef HAVE_ACOSH
#define acosh unavailable
#endif
#ifndef HAVE_ATANH
#define atanh unavailable
#endif
#endif

static SEXP math1(SEXP op, SEXP sa, double(*f)())
{
    SEXP sy;
    double *y, *a;
    int i, n;

    if (isNumeric(sa)) {
	n = length(sa);
	PROTECT(sa = coerceVector(sa, REALSXP));
	PROTECT(sy = allocVector(REALSXP, n));
	a = REAL(sa);
	y = REAL(sy);
	naflag = 0;
	for (i = 0; i < n; i++) {
	    if (ISNAN(a[i]))
		y[i] = a[i];
	    else {
		y[i] = MATH_CHECK(f(a[i]));
		if (ISNAN(y[i])) {
#ifdef OLD
		    y[i] = NA_REAL;
#endif
		    naflag = 1;
		}
	    }
	}
	if (naflag)
#ifdef IEEE_754
	    warningcall(lcall, "NaNs produced");
#else
	    warningcall(lcall, "NAs produced");
#endif
	ATTRIB(sy) = duplicate(ATTRIB(sa));
	OBJECT(sy) = OBJECT(sa);
	UNPROTECT(2);
	return sy;
    }
    else errorcall(lcall, "Non-numeric argument to mathematical function");
    return sa;			/* never used; to keep -Wall happy */
}

SEXP do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;

    checkArity(op, args);

    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;

    if (isComplex(CAR(args)))
	return complex_math1(call, op, args, env);
    lcall = call;

    switch (PRIMVAL(op)) {
    case 1: return math1(op, CAR(args), floor);
    case 2: return math1(op, CAR(args), ceil);
    case 3: return math1(op, CAR(args), sqrt);
    case 4: return math1(op, CAR(args), sign);
    case 5: return math1(op, CAR(args), trunc);

    case 10: return math1(op, CAR(args), exp);
    case 20: return math1(op, CAR(args), cos);
    case 21: return math1(op, CAR(args), sin);
    case 22: return math1(op, CAR(args), tan);
    case 23: return math1(op, CAR(args), acos);
    case 24: return math1(op, CAR(args), asin);

    case 30: return math1(op, CAR(args), cosh);
    case 31: return math1(op, CAR(args), sinh);
    case 32: return math1(op, CAR(args), tanh);
    case 33: return math1(op, CAR(args), acosh);
    case 34: return math1(op, CAR(args), asinh);
    case 35: return math1(op, CAR(args), atanh);

    case 40: return math1(op, CAR(args), lgammafn);
    case 41: return math1(op, CAR(args), gammafn);

    case 42: return math1(op, CAR(args), digamma);
    case 43: return math1(op, CAR(args), trigamma);
    case 44: return math1(op, CAR(args), tetragamma);
    case 45: return math1(op, CAR(args), pentagamma);

    case 46: return math1(op, CAR(args), gamma_cody);

    default:
	errorcall(call, "unimplemented real function");
    }
    return s;			/* never used; to keep -Wall happy */
}

static SEXP math2(SEXP op, SEXP sa, SEXP sb, double (*f)())
{
    SEXP sy;
    int i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, "Non-numeric argument to mathematical function");

    na = LENGTH(sa);
    nb = LENGTH(sb);
    if ((na == 0) || (nb == 0))
	return(allocVector(REALSXP, 0));
    n = (na < nb) ? nb : na;
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sb = coerceVector(sb, REALSXP));
    PROTECT(sy = allocVector(REALSXP, n));
    a = REAL(sa);
    b = REAL(sb);
    y = REAL(sy);
    naflag = 0;
    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if (ISNAN(ai) || ISNAN(bi)) {
#ifdef IEEE_754
	    y[i] = ai + bi;
#else
	    y[i] = NA_REAL;
#endif
	}
	else {
	    y[i] = MATH_CHECK(f(ai, bi));
	    if (ISNAN(y[i])) {
#ifdef OLD
		y[i] = NA_REAL;
#endif
		naflag = 1;
	    }
	}
    }
    if (naflag)
#ifdef IEEE_754
	warningcall(lcall, "NaNs produced");
#else
	warningcall(lcall, "NAs produced");
#endif
    if (n == na) {
	ATTRIB(sy) = duplicate(ATTRIB(sa));
	OBJECT(sy) = OBJECT(sa);
    }
    else if (n == nb) {
	ATTRIB(sy) = duplicate(ATTRIB(sb));
	OBJECT(sy) = OBJECT(sb);
    }
    UNPROTECT(3);
    return sy;
}

/* Mathematical Functions of Two Arguments */

SEXP do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if (isComplex(CAR(args)))
	return complex_math2(call, op, args, env);
    lcall = call;

    switch (PRIMVAL(op)) {
    case  0: return math2(op, CAR(args), CADR(args), atan2);
    /* case 1: return math2(op, CAR(args), CADR(args), prec); */

    case  2: return math2(op, CAR(args), CADR(args), lbeta);
    case  3: return math2(op, CAR(args), CADR(args), beta);
    case  4: return math2(op, CAR(args), CADR(args), lchoose);
    case  5: return math2(op, CAR(args), CADR(args), choose);

    case  6: return math2(op, CAR(args), CADR(args), dchisq);
    case  7: return math2(op, CAR(args), CADR(args), pchisq);
    case  8: return math2(op, CAR(args), CADR(args), qchisq);

    case  9: return math2(op, CAR(args), CADR(args), dexp);
    case 10: return math2(op, CAR(args), CADR(args), pexp);
    case 11: return math2(op, CAR(args), CADR(args), qexp);

    case 12: return math2(op, CAR(args), CADR(args), dgeom);
    case 13: return math2(op, CAR(args), CADR(args), pgeom);
    case 14: return math2(op, CAR(args), CADR(args), qgeom);

    case 15: return math2(op, CAR(args), CADR(args), dpois);
    case 16: return math2(op, CAR(args), CADR(args), ppois);
    case 17: return math2(op, CAR(args), CADR(args), qpois);

    case 18: return math2(op, CAR(args), CADR(args), dt);
    case 19: return math2(op, CAR(args), CADR(args), pt);
    case 20: return math2(op, CAR(args), CADR(args), qt);

    case 21: return math2(op, CAR(args), CADR(args), dsignrank);
    case 22: return math2(op, CAR(args), CADR(args), psignrank);
    case 23: return math2(op, CAR(args), CADR(args), qsignrank);

    case 24: return math2(op, CAR(args), CADR(args), bessel_j);
    case 25: return math2(op, CAR(args), CADR(args), bessel_y);

    default:
	errorcall(call, "unimplemented real function");
    }
    return op;			/* never used; to keep -Wall happy */
}

SEXP do_atan(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;
    lcall = call;
    switch (n = length(args)) {
    case 1:
	if (isComplex(CAR(args)))
	    return complex_math1(call, op, args, env);
	else
	    return math1(op, CAR(args), atan);
    case 2:
	if (isComplex(CAR(args)) || isComplex(CDR(args)))
	    return complex_math2(call, op, args, env);
	else
	    return math2(op, CAR(args), CADR(args), atan2);
    default:
	error("%d arguments passed to \"atan\" which requires 1 or 2", n);
    }
    return s;			/* never used; to keep -Wall happy */
}

SEXP do_round(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a, b;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &a))
	return a;
    b = R_NilValue;		/* -Wall */
    lcall = call;
    switch (n = length(args)) {
    case 1:
	PROTECT(a = CAR(args));
	PROTECT(b = allocVector(REALSXP, 1));
	REAL(b)[0] = 0;
	break;
    case 2:
	if (length(CADR(args)) == 0)
	    errorcall(call, "illegal 2nd arg of length 0");	    
	PROTECT(a = CAR(args));
	PROTECT(b = CADR(args));
	break;
    default:
	error("%d arguments passed to \"round\" which requires 1 or 2", n);
    }
    if (isComplex(CAR(args))) {
	args = list2(a, b);
	a = complex_math2(call, op, args, env);
    }
    else
	a = math2(op, a, b, rround);
    UNPROTECT(2);
    return a;
}

SEXP do_log(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &s))
	return s;
    lcall = call;
    switch (n = length(args)) {
    case 1:
	if (isComplex(CAR(args)))
	    return complex_math1(call, op, args, env);
	else
	    return math1(op, CAR(args), R_log);
    case 2:
	if (length(CADR(args)) == 0)
	    errorcall(call, "illegal 2nd arg of length 0");	    
	if (isComplex(CAR(args)) || isComplex(CDR(args)))
	    return complex_math2(call, op, args, env);
	else
	    return math2(op, CAR(args), CADR(args), logbase);
    default:
	error("%d arguments passed to \"log\" which requires 1 or 2", n);
    }
    return s;			/* never used; to keep -Wall happy */
}

SEXP do_signif(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP a, b;
    int n;
    if (DispatchGroup("Math", call, op, args, env, &a))
	return a;
    b = R_NilValue;		/* -Wall */
    lcall = call;
    switch (n = length(args)) {
    case 1:
	PROTECT(a = CAR(args));
	PROTECT(b = allocVector(REALSXP, 1));
	REAL(b)[0] = 6;
	break;
    case 2:
	if (length(CADR(args)) == 0)
	    errorcall(call, "illegal 2nd arg of length 0");	    
	PROTECT(a = CAR(args));
	PROTECT(b = CADR(args));
	break;
    default:
	error("%d arguments passed to \"signif\" which requires 1 or 2", n);
    }
    if (isComplex(CAR(args))) {
	args = list2(a, b);
	a = complex_math2(call, op, args, env);
    }
    else
	a = math2(op, a, b, prec);
    UNPROTECT(2);
    return a;
}

#define mod_iterate3(n1,n2,n3,i1,i2,i3) for (i=i1=i2=i3=0; i<n; \
	(++i1, (i1==n1)&&(i1=0),\
	 ++i2, (i2==n2)&&(i2=0),\
	 ++i3, (i3==n3)&&(i3=0), ++i))

static SEXP math3(SEXP op, SEXP sa, SEXP sb, SEXP sc, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;

    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))
	errorcall(lcall,
		  "Non-numeric argument to mathematical function");

    na = LENGTH(sa);
    nb = LENGTH(sb);
    nc = LENGTH(sc);
    if ((na == 0) || (nb == 0) || (nc == 0))
	return(allocVector(REALSXP, 0));
    n = na;
    if (n < nb) n = nb;
    if (n < nc) n = nc;
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sb = coerceVector(sb, REALSXP));
    PROTECT(sc = coerceVector(sc, REALSXP));
    PROTECT(sy = allocVector(REALSXP, n));
    a = REAL(sa);
    b = REAL(sb);
    c = REAL(sc);
    y = REAL(sy);
    naflag = 0;
    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if (ISNAN(ai) || ISNAN(bi) || ISNAN(ci)) {
#ifdef IEEE_754
	    y[i] = ai + bi + ci;
#else
	    y[i] = NA_REAL;
#endif
	}
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci));
	    if (ISNAN(y[i])) {
#ifdef OLD
		y[i] = NA_REAL;
#endif
		naflag = 1;
	    }
	}
    }
    if (naflag)
#ifdef IEEE_754
	warningcall(lcall, "NaNs produced");
#else
	warningcall(lcall, "NAs produced");
#endif
    if (n == na) {
	ATTRIB(sy) = duplicate(ATTRIB(sa));
	OBJECT(sy) = OBJECT(sa);
    }
    else if (n == nb) {
	ATTRIB(sy) = duplicate(ATTRIB(sb));
	OBJECT(sy) = OBJECT(sb);
    }
    else if (n == nc) {
	ATTRIB(sy) = duplicate(ATTRIB(sc));
	OBJECT(sy) = OBJECT(sc);
    }
    UNPROTECT(4);
    return sy;
}

/* Mathematical Functions of Three (Real) Arguments */

#define Math3(op, A, FUN) math3(op, CAR(A), CADR(A), CADDR(A), FUN);

SEXP do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

    case  1:  return Math3(op, args, dbeta);
    case  2:  return Math3(op, args, pbeta);
    case  3:  return Math3(op, args, qbeta);

    case  4:  return Math3(op, args, dbinom);
    case  5:  return Math3(op, args, pbinom);
    case  6:  return Math3(op, args, qbinom);

    case  7:  return Math3(op, args, dcauchy);
    case  8:  return Math3(op, args, pcauchy);
    case  9:  return Math3(op, args, qcauchy);

    case 10:  return Math3(op, args, df);
    case 11:  return Math3(op, args, pf);
    case 12:  return Math3(op, args, qf);

    case 13:  return Math3(op, args, dgamma);
    case 14:  return Math3(op, args, pgamma);
    case 15:  return Math3(op, args, qgamma);

    case 16:  return Math3(op, args, dlnorm);
    case 17:  return Math3(op, args, plnorm);
    case 18:  return Math3(op, args, qlnorm);

    case 19:  return Math3(op, args, dlogis);
    case 20:  return Math3(op, args, plogis);
    case 21:  return Math3(op, args, qlogis);

    case 22:  return Math3(op, args, dnbinom);
    case 23:  return Math3(op, args, pnbinom);
    case 24:  return Math3(op, args, qnbinom);

    case 25:  return Math3(op, args, dnorm);
    case 26:  return Math3(op, args, pnorm);
    case 27:  return Math3(op, args, qnorm);

    case 28:  return Math3(op, args, dunif);
    case 29:  return Math3(op, args, punif);
    case 30:  return Math3(op, args, qunif);

    case 31:  return Math3(op, args, dweibull);
    case 32:  return Math3(op, args, pweibull);
    case 33:  return Math3(op, args, qweibull);

    case 34:  return Math3(op, args, dnchisq);
    case 35:  return Math3(op, args, pnchisq);
#ifdef UNIMP
    case 36:  return Math3(op, args, qnchisq);
#endif

#ifdef UNIMP
    case 37:  return Math3(op, args, dnt);
#endif
    case 38:  return Math3(op, args, pnt);
#ifdef UNIMP
    case 39:  return Math3(op, args, qnt);
#endif

    case 40:  return Math3(op, args, dwilcox);
    case 41:  return Math3(op, args, pwilcox);
    case 42:  return Math3(op, args, qwilcox);

    case 43:  return Math3(op, args, bessel_i);
    case 44:  return Math3(op, args, bessel_k);


    default:
	errorcall(call, "unimplemented real function");
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

#define mod_iterate4(n1,n2,n3,n4,i1,i2,i3,i4) for (i=i1=i2=i3=i4=0; i<n; \
(++i1, (i1==n1)&&(i1=0), ++i2, (i2==n2)&&(i2=0),\
 ++i3, (i3==n3)&&(i3=0), ++i4, (i4==n4)&&(i4=0), ++i))

static SEXP math4(SEXP op, SEXP sa, SEXP sb, SEXP sc, SEXP sd, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;

    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) || !isNumeric(sd))
	errorcall(lcall, "Non-numeric argument to mathematical function");

    na = LENGTH(sa);
    nb = LENGTH(sb);
    nc = LENGTH(sc);
    nd = LENGTH(sd);
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))
	return(allocVector(REALSXP, 0));
    n = na;
    if (n < nb) n = nb;
    if (n < nc) n = nc;
    if (n < nd) n = nd;
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sb = coerceVector(sb, REALSXP));
    PROTECT(sc = coerceVector(sc, REALSXP));
    PROTECT(sd = coerceVector(sd, REALSXP));
    PROTECT(sy = allocVector(REALSXP, n));
    a = REAL(sa);
    b = REAL(sb);
    c = REAL(sc);
    d = REAL(sd);
    y = REAL(sy);
    naflag = 0;
    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if (ISNAN(ai) || ISNAN(bi) || ISNAN(ci) || ISNAN(di)) {
#ifdef IEEE_754
	    y[i] = ai + bi + ci + di;
#else
	    y[i] = NA_REAL;
#endif
	}
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di));
	    if (ISNAN(y[i])) {
#ifdef OLD
		y[i] = NA_REAL;
#endif
		naflag = 1;
	    }
	}
    }
    if (naflag)
#ifdef IEEE_754
	warningcall(lcall, "NaNs produced");
#else
	warningcall(lcall, "NAs produced");
#endif
    if (n == na) {
	ATTRIB(sy) = duplicate(ATTRIB(sa));
	OBJECT(sy) = OBJECT(sa);
    }
    else if (n == nb) {
	ATTRIB(sy) = duplicate(ATTRIB(sb));
	OBJECT(sy) = OBJECT(sb);
    }
    else if (n == nc) {
	ATTRIB(sy) = duplicate(ATTRIB(sc));
	OBJECT(sy) = OBJECT(sc);
    }
    else if (n == nd) {
	ATTRIB(sy) = duplicate(ATTRIB(sd));
	OBJECT(sy) = OBJECT(sd);
    }
    UNPROTECT(5);
    return sy;
} /* math4() */

/* Mathematical Functions of Four (Real) Arguments */

#define Math4(op, A, FUN) math4(op, CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN);

SEXP do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {
    case  1: return Math4(op, args, dhyper);
    case  2: return Math4(op, args, phyper);
    case  3: return Math4(op, args, qhyper);
    case  4: return Math4(op, args, dnbeta);
    case  5: return Math4(op, args, pnbeta);
#ifdef UNIMP
    case  6: return Math4(op, args, qnbeta);
#endif
#ifdef UNIMP
    case  7: return Math4(op, args, dnf);
#endif
    case  8: return Math4(op, args, pnf);
#ifdef UNIMP
    case  9: return Math4(op, args, qnf);
#endif
#ifdef UNIMP
    case 10: return Math4(op, args, dtukey);
#endif
    case 11: return Math4(op, args, ptukey);
    case 12: return Math4(op, args, qtukey);
    default:
	errorcall(call, "unimplemented real function");
    }
    return op;			/* never used; to keep -Wall happy */
}

#ifdef WHEN_MATH5_IS_THERE

#define mod_iterate5(n1,n2,n3,n4,n5, i1,i2,i3,i4,i5) \
 for (i=i1=i2=i3=i4=i5=0; i<n; \
(++i1, (i1==n1)&&(i1=0), ++i2, (i2==n2)&&(i2=0),\
 ++i3, (i3==n3)&&(i3=0), ++i4, (i4==n4)&&(i4=0),\
 ++i5, (i5==n5)&&(i5=0), ++i))

static SEXP math5(SEXP op, SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se,
		  double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||
	!isNumeric(sd) || !isNumeric(se))
	errorcall(lcall, "Non-numeric argument to mathematical function");

    na = LENGTH(sa);
    nb = LENGTH(sb);
    nc = LENGTH(sc);
    nd = LENGTH(sd);
    ne = LENGTH(se);
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))
	return(allocVector(REALSXP, 0));
    n = na;
    if (n < nb) n = nb;
    if (n < nc) n = nc;
    if (n < nd) n = nd;
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */
    PROTECT(sa = coerceVector(sa, REALSXP));
    PROTECT(sb = coerceVector(sb, REALSXP));
    PROTECT(sc = coerceVector(sc, REALSXP));
    PROTECT(sd = coerceVector(sd, REALSXP));
    PROTECT(se = coerceVector(se, REALSXP));
    PROTECT(sy = allocVector(REALSXP, n));
    a = REAL(sa);
    b = REAL(sb);
    c = REAL(sc);
    d = REAL(sd);
    e = REAL(se);
    y = REAL(sy);
    naflag = 0;
    mod_iterate5 (na, nb, nc, nd, ne,
		  ia, ib, ic, id, ie) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if (ISNAN(ai) || ISNAN(bi) || ISNAN(ci) || ISNAN(di) || ISNAN(ei)) {
#ifdef IEEE_754
	    y[i] = ai + bi + ci + di + ei;
#else
	    y[i] = NA_REAL;
#endif
	}
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di, ei));
	    if (ISNAN(y[i])) {
#ifdef OLD
		y[i] = NA_REAL;
#endif
		naflag = 1;
	    }
	}
    }
    if (naflag)
#ifdef IEEE_754
	warningcall(lcall, "NaNs produced");
#else
	warningcall(lcall, "NAs produced");
#endif
    if (n == na) {
	ATTRIB(sy) = duplicate(ATTRIB(sa));
	OBJECT(sy) = OBJECT(sa);
    }
    else if (n == nb) {
	ATTRIB(sy) = duplicate(ATTRIB(sb));
	OBJECT(sy) = OBJECT(sb);
    }
    else if (n == nc) {
	ATTRIB(sy) = duplicate(ATTRIB(sc));
	OBJECT(sy) = OBJECT(sc);
    }
    else if (n == nd) {
	ATTRIB(sy) = duplicate(ATTRIB(sd));
	OBJECT(sy) = OBJECT(sd);
    }
    else if (n == ne) {
	ATTRIB(sy) = duplicate(ATTRIB(se));
	OBJECT(sy) = OBJECT(se);
    }
    UNPROTECT(6);
    return sy;
} /* math5() */

/* Mathematical Functions of Five (Real) Arguments */

#define Math5(op, A, FUN) \
	math5(op, CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN);

SEXP do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {
    case  1: return Math5(op, args, dhyper);
    case  2: return Math5(op, args, phyper);
    case  3: return Math5(op, args, qhyper);
    case  4: return Math5(op, args, dnbeta);
    case  5: return Math5(op, args, pnbeta);
#ifdef UNIMP
    case  6: return Math5(op, args, qnbeta);
#endif
#ifdef UNIMP
    case  7: return Math5(op, args, dnf);
#endif
    case  8: return Math5(op, args, pnf);
#ifdef UNIMP
    case  9: return Math5(op, args, qnf);
#endif
#ifdef UNIMP
    case 10: return Math5(op, args, dtukey);
#endif
    case 11: return Math5(op, args, ptukey);
    case 12: return Math5(op, args, qtukey);
    default:
	errorcall(call, "unimplemented real function");
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */
