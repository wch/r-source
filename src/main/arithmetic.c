/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2001	    The R Development Core Team.
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

#include "Defn.h"		/*-> Arith.h */
#define MATHLIB_PRIVATE
#include <Rmath.h>
#undef MATHLIB_PRIVATE
#include "R_ext/Applic.h"		/* machar */
#include "arithmetic.h"

/* Error Handling for Floating Point Errors */

#ifndef IEEE_754
#ifdef Unix
#include <signal.h>

static RETSIGTYPE handle_fperror(int dummy)
{
    errno = ERANGE;
    signal(SIGFPE, handle_fperror);
}
#endif
#endif /* not IEEE_754 */

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

static int hw;
static int lw;

#ifdef OLD
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
#endif

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

int R_IsNaNorNA(double x)
{
/* True for *both* NA and NaN.
   NOTE: some systems do not return 1 for TRUE. */
    return (isnan(x) != 0);
}

/* Include the header file defining finite() */
#ifdef HAVE_IEEE754_H
# include <ieee754.h>		/* newer Linuxen */
#else
# ifdef HAVE_IEEEFP_H
#  include <ieeefp.h>		/* others [Solaris 2.5.x], .. */
# endif
#endif
#if defined(Win32) && defined(_MSC_VER)
#include <float.h>
#endif

int R_finite(double x)
{
#ifdef Macintosh
    return isfinite(x);
#endif
#ifndef FINITE_BROKEN
    return finite(x);
# else
#  ifdef _AIX
#   include <fp.h>
     return FINITE(x);
#  else
    return (!isnan(x) & (x != R_PosInf) & (x != R_NegInf));
#  endif
#endif
}

#else /* not IEEE_754 */

int R_IsNA(double x)
{
    return (x == R_NaReal);
}

/* NaN but not NA: never true */
int R_IsNaN(double x)
{
    return 0;
}

int R_IsNaNorNA(double x)
{
# ifndef HAVE_ISNAN
    return (x == R_NaReal);
# else
    return (isnan(x) != 0 || x == R_NaReal);
# endif
}

int R_finite(double x)
{
# ifndef HAVE_FINITE
    return (x != R_NaReal && x < R_PosInf && x > R_NegInf);
# else
    int finite(double);
    return finite(x);
# endif
}
#endif /* IEEE_754 */

/* Arithmetic Initialization */

void InitArithmetic()
{
    R_NaInt = INT_MIN;

#ifdef IEEE_754
    /* establish_endianness(); */
#  ifdef WORDS_BIGENDIAN
    hw = 0;
    lw = 1;
#  else
    hw = 1;
    lw = 0;
#  endif
    R_NaN = 0.0/R_Zero_Hack;
    R_NaReal = R_ValueOfNA();
    R_PosInf = 1.0/R_Zero_Hack;
    R_NegInf = -1.0/R_Zero_Hack;
#else
    R_NaN = -DBL_MAX*(1-1e-15);
    R_NaReal = R_NaN;
    R_PosInf = DBL_MAX;
    R_NegInf = -DBL_MAX;
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
    SET_STRING_ELT(nms, 0, mkChar("double.eps"));
    SET_VECTOR_ELT(ans, 0, ScalarReal(eps));

    SET_STRING_ELT(nms, 1, mkChar("double.neg.eps"));
    SET_VECTOR_ELT(ans, 1, ScalarReal(epsneg));

    SET_STRING_ELT(nms, 2, mkChar("double.xmin"));
    SET_VECTOR_ELT(ans, 2, ScalarReal(xmin));

    SET_STRING_ELT(nms, 3, mkChar("double.xmax"));
    SET_VECTOR_ELT(ans, 3, ScalarReal(xmax));

    SET_STRING_ELT(nms, 4, mkChar("double.base"));
    SET_VECTOR_ELT(ans, 4, ScalarInteger(ibeta));

    SET_STRING_ELT(nms, 5, mkChar("double.digits"));
    SET_VECTOR_ELT(ans, 5, ScalarInteger(it));

    SET_STRING_ELT(nms, 6, mkChar("double.rounding"));
    SET_VECTOR_ELT(ans, 6, ScalarInteger(irnd));

    SET_STRING_ELT(nms, 7, mkChar("double.guard"));
    SET_VECTOR_ELT(ans, 7, ScalarInteger(ngrd));

    SET_STRING_ELT(nms, 8, mkChar("double.ulp.digits"));
    SET_VECTOR_ELT(ans, 8, ScalarInteger(machep));

    SET_STRING_ELT(nms, 9, mkChar("double.neg.ulp.digits"));
    SET_VECTOR_ELT(ans, 9, ScalarInteger(negep));

    SET_STRING_ELT(nms, 10, mkChar("double.exponent"));
    SET_VECTOR_ELT(ans, 10, ScalarInteger(iexp));

    SET_STRING_ELT(nms, 11, mkChar("double.min.exp"));
    SET_VECTOR_ELT(ans, 11, ScalarInteger(minexp));

    SET_STRING_ELT(nms, 12, mkChar("double.max.exp"));
    SET_VECTOR_ELT(ans, 12, ScalarInteger(maxexp));

    SET_STRING_ELT(nms, 13, mkChar("integer.max"));
    SET_VECTOR_ELT(ans, 13, ScalarInteger(INT_MAX));
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
    if(x == 0.) {
	if(y > 0.) return(0.);
	/* y < 0 */return(R_PosInf);
    }
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

double R_pow_di(double x, int n)
{
    double xn = 1.0;

    if (ISNAN(x)) return x;
    if (n == NA_INTEGER) return NA_REAL;
    if (n != 0) {
	if (!R_FINITE(x)) return R_pow(x, (double)n);
	if (n < 0) { n = -n; x = 1/x; }
	for(;;) {
	    if(n & 01) xn *= x;
	    if(n >>= 1) x *= x; else break;
	}
    }
    return xn;
}


/* General Base Logarithms */

static double logbase(double x, double base)
{
    return R_log(x) / log(base);
}

SEXP R_unary(SEXP, SEXP, SEXP);
SEXP R_binary(SEXP, SEXP, SEXP, SEXP);
static SEXP integer_unary(ARITHOP_TYPE, SEXP);
static SEXP real_unary(ARITHOP_TYPE, SEXP);
static SEXP real_binary(ARITHOP_TYPE, SEXP, SEXP);
static SEXP integer_binary(ARITHOP_TYPE, SEXP, SEXP);

static int naflag;
static SEXP lcall;


/* Unary and Binary Operators */

SEXP do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    switch (length(args)) {
    case 1:
	return R_unary(call, op, CAR(args));
    case 2:
	return R_binary(call, op, CAR(args), CADR(args));
    default:
	error("operator needs one or two arguments");
    }
    return ans;			/* never used; to keep -Wall happy */
}


SEXP R_binary(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP class, dims, tsp, xnames, ynames;
    int mismatch, nx, ny, xarray, yarray, xts, yts;
    PROTECT_INDEX xpi, ypi;

    lcall = call;

    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    /* fix up NULL */
    if (isNull(x))
	REPROTECT(x = allocVector(REALSXP,0), xpi);
    if (isNull(y))
	REPROTECT(y = allocVector(REALSXP,0), ypi);

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
	    REPROTECT(x = duplicate(x), xpi);
	    setAttrib(x, R_DimSymbol, R_NilValue);
	}
	if (yarray && length(y)==1 && length(x)!=1) {
	    REPROTECT(y = duplicate(y), ypi);
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
	REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
	REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
	x = complex_binary(PRIMVAL(op), x, y);
    }
    else
	if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	    REPROTECT(x = coerceVector(x, REALSXP), xpi);
	    REPROTECT(y = coerceVector(y, REALSXP), ypi);
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

    UNPROTECT(6);
    return x;
}

SEXP R_unary(SEXP call, SEXP op, SEXP s1)
{
    lcall = call;
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

static SEXP integer_unary(ARITHOP_TYPE code, SEXP s1)
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

static SEXP real_unary(ARITHOP_TYPE code, SEXP s1)
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
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

static SEXP integer_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
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

static SEXP real_binary(ARITHOP_TYPE code, SEXP s1, SEXP s2)
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

static SEXP math1(SEXP sa, double(*f)())
{
    SEXP sy;
    double *y, *a;
    int i, n;

    if (!isNumeric(sa))
	errorcall(lcall, R_MSG_NONNUM_MATH);

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
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    if(naflag)
	warningcall(lcall, R_MSG_NA);

    SET_ATTRIB(sy, duplicate(ATTRIB(sa)));
    SET_OBJECT(sy, OBJECT(sa));
    UNPROTECT(2);
    return sy;
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
    case 1: return math1(CAR(args), floor);
    case 2: return math1(CAR(args), ceil);
    case 3: return math1(CAR(args), sqrt);
    case 4: return math1(CAR(args), sign);
    case 5: return math1(CAR(args), trunc);

    case 10: return math1(CAR(args), exp);
    case 12: return math1(CAR(args), log1p);
    case 20: return math1(CAR(args), cos);
    case 21: return math1(CAR(args), sin);
    case 22: return math1(CAR(args), tan);
    case 23: return math1(CAR(args), acos);
    case 24: return math1(CAR(args), asin);

    case 30: return math1(CAR(args), cosh);
    case 31: return math1(CAR(args), sinh);
    case 32: return math1(CAR(args), tanh);
    case 33: return math1(CAR(args), acosh);
    case 34: return math1(CAR(args), asinh);
    case 35: return math1(CAR(args), atanh);

    case 40: return math1(CAR(args), lgammafn);
    case 41: return math1(CAR(args), gammafn);

    case 42: return math1(CAR(args), digamma);
    case 43: return math1(CAR(args), trigamma);
    case 44: return math1(CAR(args), tetragamma);
    case 45: return math1(CAR(args), pentagamma);

    case 46: return math1(CAR(args), gamma_cody);

    default:
	errorcall(call, "unimplemented real function (of 1 arg.)");
    }
    return s;			/* never used; to keep -Wall happy */
}

/* Mathematical Functions of Two Numeric Arguments (plus 1 int) */

#define if_NA_Math2_set(y,a,b)				\
	if      (ISNA (a) || ISNA (b)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)) y = R_NaN;

static SEXP math2(SEXP sa, SEXP sb, double (*f)())
{
    SEXP sy;
    int i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

#define SETUP_Math2				\
    na = LENGTH(sa);				\
    nb = LENGTH(sb);				\
    if ((na == 0) || (nb == 0))			\
	return(allocVector(REALSXP, 0));	\
    n = (na < nb) ? nb : na;			\
    PROTECT(sa = coerceVector(sa, REALSXP));	\
    PROTECT(sb = coerceVector(sb, REALSXP));	\
    PROTECT(sy = allocVector(REALSXP, n));	\
    a = REAL(sa);				\
    b = REAL(sb);				\
    y = REAL(sy);				\
    naflag = 0

    SETUP_Math2;

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = MATH_CHECK(f(ai, bi));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math2				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, OBJECT(sa));		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, OBJECT(sb));		\
    }						\
    UNPROTECT(3)

    FINISH_Math2;

    return sy;
} /* math2() */

static SEXP math2_1(SEXP sa, SEXP sb, SEXP sI, double (*f)())
{
    SEXP sy;
    int i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int m_opt;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    m_opt = asInteger(sI);

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, m_opt));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_1() */

static SEXP math2_2(SEXP sa, SEXP sb, SEXP sI1, SEXP sI2, double (*f)())
{
    SEXP sy;
    int i, ia, ib, n, na, nb;
    double ai, bi, *a, *b, *y;
    int i_1, i_2;

    if (!isNumeric(sa) || !isNumeric(sb))
	errorcall(lcall, R_MSG_NONNUM_MATH);

    SETUP_Math2;
    i_1 = asInteger(sI1);
    i_2 = asInteger(sI2);

    mod_iterate(na, nb, ia, ib) {
	ai = a[ia];
	bi = b[ib];
	if_NA_Math2_set(y[i], ai, bi)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, i_1, i_2));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math2;
    return sy;
} /* math2_2() */

#define Math2(A, FUN)	  math2(CAR(A), CADR(A), FUN);
#define Math2_1(A, FUN)	math2_1(CAR(A), CADR(A), CADDR(A), FUN);
#define Math2_2(A, FUN) math2_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN)

SEXP do_math2(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);

    if (isComplex(CAR(args)))
	return complex_math2(call, op, args, env);
    lcall = call;

    switch (PRIMVAL(op)) {
    case  0: return Math2(args, atan2);
    /*case 1: return Math2(args, prec); */

    case  2: return Math2(args, lbeta);
    case  3: return Math2(args, beta);
    case  4: return Math2(args, lchoose);
    case  5: return Math2(args, choose);

    case  6: return Math2_1(args, dchisq);
    case  7: return Math2_2(args, pchisq);
    case  8: return Math2_2(args, qchisq);

    case  9: return Math2_1(args, dexp);
    case 10: return Math2_2(args, pexp);
    case 11: return Math2_2(args, qexp);

    case 12: return Math2_1(args, dgeom);
    case 13: return Math2_2(args, pgeom);
    case 14: return Math2_2(args, qgeom);

    case 15: return Math2_1(args, dpois);
    case 16: return Math2_2(args, ppois);
    case 17: return Math2_2(args, qpois);

    case 18: return Math2_1(args, dt);
    case 19: return Math2_2(args, pt);
    case 20: return Math2_2(args, qt);

    case 21: return Math2_1(args, dsignrank);
    case 22: return Math2_2(args, psignrank);
    case 23: return Math2_2(args, qsignrank);

    case 24: return Math2(args, bessel_j);
    case 25: return Math2(args, bessel_y);

    default:
	errorcall(call, "unimplemented real function of 2 numeric arg.s");
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
	    return math1(CAR(args), atan);
    case 2:
	if (isComplex(CAR(args)) || isComplex(CDR(args)))
	    return complex_math2(call, op, args, env);
	else
	    return math2(CAR(args), CADR(args), atan2);
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
    b = R_NilValue;	/* -Wall */
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
	a = math2(a, b, rround);
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
	    return math1(CAR(args), R_log);
    case 2:
	if (length(CADR(args)) == 0)
	    errorcall(call, "illegal 2nd arg of length 0");
	if (isComplex(CAR(args)) || isComplex(CDR(args)))
	    return complex_math2(call, op, args, env);
	else
	    return math2(CAR(args), CADR(args), logbase);
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
	a = math2(a, b, prec);
    UNPROTECT(2);
    return a;
}

/* Mathematical Functions of Three (Real) Arguments */

#define if_NA_Math3_set(y,a,b,c)			        \
	if      (ISNA (a) || ISNA (b)|| ISNA (c)) y = NA_REAL;	\
	else if (ISNAN(a) || ISNAN(b)|| ISNAN(c)) y = R_NaN;

#define mod_iterate3(n1,n2,n3,i1,i2,i3) for (i=i1=i2=i3=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,				\
	i2 = (++i2==n2) ? 0 : i2,				\
	i3 = (++i3==n3) ? 0 : i3,				\
	++i)

static SEXP math3(SEXP sa, SEXP sb, SEXP sc, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;

#define SETUP_Math3						\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))	\
	errorcall(lcall, R_MSG_NONNUM_MATH);			\
								\
    na = LENGTH(sa);						\
    nb = LENGTH(sb);						\
    nc = LENGTH(sc);						\
    if ((na == 0) || (nb == 0) || (nc == 0))			\
	return(allocVector(REALSXP, 0));			\
    n = na;							\
    if (n < nb) n = nb;						\
    if (n < nc) n = nc;						\
    PROTECT(sa = coerceVector(sa, REALSXP));			\
    PROTECT(sb = coerceVector(sb, REALSXP));			\
    PROTECT(sc = coerceVector(sc, REALSXP));			\
    PROTECT(sy = allocVector(REALSXP, n));			\
    a = REAL(sa);						\
    b = REAL(sb);						\
    c = REAL(sc);						\
    y = REAL(sy);						\
    naflag = 0

    SETUP_Math3;

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math3				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, OBJECT(sa));		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, OBJECT(sb));		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, OBJECT(sc));		\
    }						\
    UNPROTECT(4)

    FINISH_Math3;

    return sy;
} /* math3 */

static SEXP math3_1(SEXP sa, SEXP sb, SEXP sc, SEXP sI, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1;

    SETUP_Math3;
    i_1 = asInteger(sI);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, i_1));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_1 */

static SEXP math3_2(SEXP sa, SEXP sb, SEXP sc, SEXP sI, SEXP sJ, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, n, na, nb, nc;
    double ai, bi, ci, *a, *b, *c, *y;
    int i_1,i_2;

    SETUP_Math3;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate3 (na, nb, nc, ia, ib, ic) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	if_NA_Math3_set(y[i], ai,bi,ci)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, i_1, i_2));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

    FINISH_Math3;
    return sy;
} /* math3_2 */

#define Math3(A, FUN)   math3  (CAR(A), CADR(A), CADDR(A), FUN);
#define Math3_1(A, FUN)	math3_1(CAR(A), CADR(A), CADDR(A), CADDDR(A), FUN);
#define Math3_2(A, FUN) math3_2(CAR(A), CADR(A), CADDR(A), CADDDR(A), CAD4R(A), FUN)

SEXP do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

    case  1:  return Math3_1(args, dbeta);
    case  2:  return Math3_2(args, pbeta);
    case  3:  return Math3_2(args, qbeta);

    case  4:  return Math3_1(args, dbinom);
    case  5:  return Math3_2(args, pbinom);
    case  6:  return Math3_2(args, qbinom);

    case  7:  return Math3_1(args, dcauchy);
    case  8:  return Math3_2(args, pcauchy);
    case  9:  return Math3_2(args, qcauchy);

    case 10:  return Math3_1(args, df);
    case 11:  return Math3_2(args, pf);
    case 12:  return Math3_2(args, qf);

    case 13:  return Math3_1(args, dgamma);
    case 14:  return Math3_2(args, pgamma);
    case 15:  return Math3_2(args, qgamma);

    case 16:  return Math3_1(args, dlnorm);
    case 17:  return Math3_2(args, plnorm);
    case 18:  return Math3_2(args, qlnorm);

    case 19:  return Math3_1(args, dlogis);
    case 20:  return Math3_2(args, plogis);
    case 21:  return Math3_2(args, qlogis);

    case 22:  return Math3_1(args, dnbinom);
    case 23:  return Math3_2(args, pnbinom);
    case 24:  return Math3_2(args, qnbinom);

    case 25:  return Math3_1(args, dnorm);
    case 26:  return Math3_2(args, pnorm);
    case 27:  return Math3_2(args, qnorm);

    case 28:  return Math3_1(args, dunif);
    case 29:  return Math3_2(args, punif);
    case 30:  return Math3_2(args, qunif);

    case 31:  return Math3_1(args, dweibull);
    case 32:  return Math3_2(args, pweibull);
    case 33:  return Math3_2(args, qweibull);

    case 34:  return Math3_1(args, dnchisq);
    case 35:  return Math3_2(args, pnchisq);
/* #ifdef UNIMP */
/* This appears to be IMP already */
    case 36:  return Math3_2(args, qnchisq);
/* #endif */

#ifdef UNIMP
    case 37:  return Math3_1(args, dnt);
#endif
    case 38:  return Math3_2(args, pnt);
#ifdef UNIMP
    case 39:  return Math3_2(args, qnt);
#endif

    case 40:  return Math3_1(args, dwilcox);
    case 41:  return Math3_2(args, pwilcox);
    case 42:  return Math3_2(args, qwilcox);

    case 43:  return Math3(args, bessel_i);
    case 44:  return Math3(args, bessel_k);


    default:
	errorcall(call, "unimplemented real function of 3 numeric arg.s");
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math3() */

/* Mathematical Functions of Four (Real) Arguments */

#define if_NA_Math4_set(y,a,b,c,d)			        	\
	if      (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)) y = NA_REAL;\
	else if (ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)) y = R_NaN;

#define mod_iterate4(n1,n2,n3,n4,i1,i2,i3,i4) for (i=i1=i2=i3=i4=0; i<n; \
	i1 = (++i1==n1) ? 0 : i1,					\
	i2 = (++i2==n2) ? 0 : i2,					\
	i3 = (++i3==n3) ? 0 : i3,					\
	i4 = (++i4==n4) ? 0 : i4,					\
	++i)

static SEXP math4(SEXP sa, SEXP sb, SEXP sc, SEXP sd, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;

#define SETUP_Math4							\
    if(!isNumeric(sa)|| !isNumeric(sb)|| !isNumeric(sc)|| !isNumeric(sd))\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = LENGTH(sa);							\
    nb = LENGTH(sb);							\
    nc = LENGTH(sc);							\
    nd = LENGTH(sd);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0))		\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math4;

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math4				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, OBJECT(sa));		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, OBJECT(sb));		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, OBJECT(sc));		\
    }						\
    else if (n == nd) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sd)));	\
	SET_OBJECT(sy, OBJECT(sd));		\
    }						\
    UNPROTECT(5)

    FINISH_Math4;

    return sy;
} /* math4() */

static SEXP math4_1(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1;

    SETUP_Math4;
    i_1 = asInteger(sI);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di, i_1));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_1() */

static SEXP math4_2(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP sI, SEXP sJ,
		    double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, n, na, nb, nc, nd;
    double ai, bi, ci, di, *a, *b, *c, *d, *y;
    int i_1, i_2;

    SETUP_Math4;
    i_1 = asInteger(sI);
    i_2 = asInteger(sJ);

    mod_iterate4 (na, nb, nc, nd, ia, ib, ic, id) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	if_NA_Math4_set(y[i], ai,bi,ci,di)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di, i_1, i_2));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }
    FINISH_Math4;
    return sy;
} /* math4_2() */


#define CAD3R	CADDDR
/* This is not (yet) in Rinternals.h : */
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))

#define Math4(A, FUN)   math4  (CAR(A), CADR(A), CADDR(A), CAD3R(A), FUN)
#define Math4_1(A, FUN) math4_1(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				FUN)
#define Math4_2(A, FUN) math4_2(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), \
				CAD5R(A), FUN)


SEXP do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- math4() at all! : */
    case -99: return Math4(args, dhyper);

    case  1: return Math4_1(args, dhyper);
    case  2: return Math4_2(args, phyper);
    case  3: return Math4_2(args, qhyper);

    case  4: return Math4_1(args, dnbeta);
    case  5: return Math4_2(args, pnbeta);
#ifdef UNIMP
    case  6: return Math4_2(args, qnbeta);
#endif
#ifdef UNIMP
    case  7: return Math4_1(args, dnf);
#endif
    case  8: return Math4_2(args, pnf);
#ifdef UNIMP
    case  9: return Math4_2(args, qnf);
#endif
#ifdef UNIMP
    case 10: return Math4_1(args, dtukey);
#endif
    case 11: return Math4_2(args, ptukey);
    case 12: return Math4_2(args, qtukey);
    default:
	errorcall(call, "unimplemented real function of 4 numeric arg.s");
    }
    return op;			/* never used; to keep -Wall happy */
}


#ifdef WHEN_MATH5_IS_THERE/* as in ./arithmetic.h */

/* Mathematical Functions of Five (Real) Arguments */

#define if_NA_Math5_set(y,a,b,c,d,e)					\
	if     (ISNA (a)|| ISNA (b)|| ISNA (c)|| ISNA (d)|| ISNA (e)) 	\
    		y = NA_REAL;						\
	else if(ISNAN(a)|| ISNAN(b)|| ISNAN(c)|| ISNAN(d)|| ISNAN(e))	\
		y = R_NaN;

#define mod_iterate5(n1,n2,n3,n4,n5, i1,i2,i3,i4,i5)	\
 for (i=i1=i2=i3=i4=i5=0; i<n;				\
	i1 = (++i1==n1) ? 0 : i1,			\
	i2 = (++i2==n2) ? 0 : i2,			\
	i3 = (++i3==n3) ? 0 : i3,			\
	i4 = (++i4==n4) ? 0 : i4,			\
	i5 = (++i5==n5) ? 0 : i5,			\
	++i)

static SEXP math5(SEXP sa, SEXP sb, SEXP sc, SEXP sd, SEXP se, double (*f)())
{
    SEXP sy;
    int i, ia, ib, ic, id, ie, n, na, nb, nc, nd, ne;
    double ai, bi, ci, di, ei, *a, *b, *c, *d, *e, *y;

#define SETUP_Math5							\
    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) ||		\
	!isNumeric(sd) || !isNumeric(se))				\
	errorcall(lcall, R_MSG_NONNUM_MATH);				\
									\
    na = LENGTH(sa);							\
    nb = LENGTH(sb);							\
    nc = LENGTH(sc);							\
    nd = LENGTH(sd);							\
    ne = LENGTH(se);							\
    if ((na == 0) || (nb == 0) || (nc == 0) || (nd == 0) || (ne == 0))	\
	return(allocVector(REALSXP, 0));				\
    n = na;								\
    if (n < nb) n = nb;							\
    if (n < nc) n = nc;							\
    if (n < nd) n = nd;							\
    if (n < ne) n = ne;		/* n = max(na,nb,nc,nd,ne) */		\
    PROTECT(sa = coerceVector(sa, REALSXP));				\
    PROTECT(sb = coerceVector(sb, REALSXP));				\
    PROTECT(sc = coerceVector(sc, REALSXP));				\
    PROTECT(sd = coerceVector(sd, REALSXP));				\
    PROTECT(se = coerceVector(se, REALSXP));				\
    PROTECT(sy = allocVector(REALSXP, n));				\
    a = REAL(sa);							\
    b = REAL(sb);							\
    c = REAL(sc);							\
    d = REAL(sd);							\
    e = REAL(se);							\
    y = REAL(sy);							\
    naflag = 0

    SETUP_Math5;

    mod_iterate5 (na, nb, nc, nd, ne,
		  ia, ib, ic, id, ie) {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	di = d[id];
	ei = e[ie];
	if_NA_Math5_set(y[i], ai,bi,ci,di,ei)
	else {
	    y[i] = MATH_CHECK(f(ai, bi, ci, di, ei));
	    if (ISNAN(y[i])) naflag = 1;
	}
    }

#define FINISH_Math5				\
    if(naflag)					\
	warningcall(lcall, R_MSG_NA);		\
						\
    if (n == na) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sa)));	\
	SET_OBJECT(sy, OBJECT(sa));		\
    }						\
    else if (n == nb) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sb)));	\
	SET_OBJECT(sy, OBJECT(sb));		\
    }						\
    else if (n == nc) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sc)));	\
	SET_OBJECT(sy, OBJECT(sc));		\
    }						\
    else if (n == nd) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(sd)));	\
	SET_OBJECT(sy, OBJECT(sd));		\
    }						\
    else if (n == ne) {				\
	SET_ATTRIB(sy, duplicate(ATTRIB(se)));	\
	SET_OBJECT(sy, OBJECT(se));		\
    }						\
    UNPROTECT(6)

    FINISH_Math5;

    return sy;
} /* math5() */

#define Math5(A, FUN) \
	math5(CAR(A), CADR(A), CADDR(A), CAD3R(A), CAD4R(A), FUN);

SEXP do_math5(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    lcall = call;

    switch (PRIMVAL(op)) {

	/* Completely dummy for -Wall -- use math5() at all! : */
    case -99: return Math5(args, dhyper);
#ifdef UNIMP
    case  2: return Math5(args, p...);
    case  3: return Math5(args, q...);
#endif
    default:
	errorcall(call, "unimplemented real function of 5 numeric arg.s");
    }
    return op;			/* never used; to keep -Wall happy */
} /* do_math5() */

#endif /* Math5 is there */
