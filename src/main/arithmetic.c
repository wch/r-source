/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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


	/* Error Handling for Floating Point Errors */

#ifndef HAVE_ISNAN
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

	/* Arithmetic Initialization */

#ifdef HAVE_ISNAN
double R_Zero_Hack = 0.0;		/* silence the compiler */
#endif

void InitArithmetic()
{
	R_NaInt = INT_MIN;

#ifdef HAVE_ISNAN
	R_NaN = 0.0/R_Zero_Hack;
	R_NaReal = R_NaN;
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

void machar(int*, int*, int*, int*, int*, int*,
        int*, int*, int*, double*, double*, double*, double*);

SEXP do_Machine(SEXP call, SEXP op, SEXP args, SEXP env)
{
	int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
	double eps, epsneg, xmin, xmax;
	SEXP a, ans;

	checkArity(op, args);

	PROTECT(a = ans = allocList(14));

	machar(&ibeta, &it, &irnd, &ngrd, &machep, &negep, &iexp,
		&minexp, &maxexp, &eps, &epsneg, &xmin, &xmax);

	TAG(a) = install("double.eps");
	CAR(a) = allocVector(REALSXP, 1);
	REAL(CAR(a))[0] = eps;
	a = CDR(a);

	TAG(a) = install("double.neg.eps");
	CAR(a) = allocVector(REALSXP, 1);
	REAL(CAR(a))[0] = epsneg;
	a = CDR(a);

	TAG(a) = install("double.xmin");
	CAR(a) = allocVector(REALSXP, 1);
	REAL(CAR(a))[0] = xmin;
	a = CDR(a);

	TAG(a) = install("double.xmax");
	CAR(a) = allocVector(REALSXP, 1);
	REAL(CAR(a))[0] = xmax;
	a = CDR(a);

	TAG(a) = install("double.base");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = ibeta;
	a = CDR(a);

	TAG(a) = install("double.digits");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = it;
	a = CDR(a);

	TAG(a) = install("double.rounding");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = irnd;
	a = CDR(a);

	TAG(a) = install("double.guard");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = ngrd;
	a = CDR(a);

	TAG(a) = install("double.ulp.digits");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = machep;
	a = CDR(a);

	TAG(a) = install("double.neg.ulp.digits");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = negep;
	a = CDR(a);

	TAG(a) = install("double.exponent");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = iexp;
	a = CDR(a);

	TAG(a) = install("double.min.exp");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = minexp;
	a = CDR(a);

	TAG(a) = install("double.max.exp");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = maxexp;
	a = CDR(a);

	TAG(a) = install("integer.max");
	CAR(a) = allocVector(INTSXP, 1);
	INTEGER(CAR(a))[0] = INT_MAX;
	a = CDR(a);

	UNPROTECT(1);
	return ans;
}


	/* Base 2 Logarithms */

double log2(double x)
{
	return log(x) / 0.69314718055994530941;
}

double logbase(double x, double base)
{       
        return log(x) / log(base);
}

static SEXP unary(SEXP, SEXP);
static SEXP binary(SEXP, SEXP);
static SEXP integer_unary(int, SEXP);
static SEXP real_unary(int, SEXP);
static SEXP real_binary(int, SEXP, SEXP);
static SEXP integer_binary(int, SEXP, SEXP);

#ifdef COMPLEX_DATA
extern SEXP complex_unary(int, SEXP);
extern SEXP complex_binary(int, SEXP, SEXP);
extern SEXP complex_math1(SEXP, SEXP, SEXP, SEXP);
extern SEXP complex_math2(SEXP, SEXP, SEXP, SEXP);
#endif

static int naflag;
static SEXP lcall;


	/* Unary and Binary Operators */

SEXP do_arith(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP ans;

	if( DispatchGroup("Ops", call, op, args, env, &ans) )
		return ans;

	lcall = call;
	switch (length(args)) {
	case 1:
		return unary(op, CAR(args));
	case 2:
		return binary(op, args);
	default:
		error("operator with more than two arguments\n");
	}
}


static SEXP binary(SEXP op, SEXP args)
{
	SEXP x, y, class, dims, tsp, xnames, ynames;
	int mismatch, nx, ny, xarray, yarray, xts, yts;

	x = CAR(args);
	y = CADR(args);

	/* fix up NULL */
	if( isNull(x) )
		x = CAR(args) = allocVector(REALSXP,0);
	if( isNull(y) )
		y = CADR(args) = allocVector(REALSXP,0);

#ifdef COMPLEX_DATA
	if (!(isNumeric(x) || isComplex(x)) || !(isNumeric(y) || isComplex(y)))
#else
	if (!isNumeric(x) || !isNumeric(y))
#endif
		errorcall(lcall, "non-numeric argument to binary operator\n");

	mismatch = 0;
	xarray = isArray(x);
	yarray = isArray(y);
	xts = isTs(x);
	yts = isTs(y);
	if (xarray || yarray) {
		if (xarray && yarray) {
			if (!conformable(x, y))
				errorcall(lcall, "non-conformable arrays\n");
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
				errorcall(lcall, "Non-conformable time-series\n");
			PROTECT(tsp = getAttrib(x, R_TspSymbol));
			PROTECT(class = getAttrib(x, R_ClassSymbol));
		}
		else if (xts) {
			if (length(x) < length(y))
				ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
			PROTECT(tsp = getAttrib(x, R_TspSymbol));
			PROTECT(class = getAttrib(x, R_ClassSymbol));
		}
		else if (yts) {
			if (length(y) < length(x))
				ErrorMessage(lcall, ERROR_TSVEC_MISMATCH);
			PROTECT(tsp = getAttrib(y, R_TspSymbol));
			PROTECT(class = getAttrib(y, R_ClassSymbol));
		}
	}
	if(mismatch) warningcall(lcall, "longer object length\n\tis not a multiple of shorter object length\n");

#ifdef COMPLEX_DATA
	if (TYPEOF(x) == CPLXSXP || TYPEOF(y) == CPLXSXP) {
		x = CAR(args) = coerceVector(x, CPLXSXP);
		y = CADR(args) = coerceVector(y, CPLXSXP);
		x = complex_binary(PRIMVAL(op), x, y);
	}
	else
#endif
	if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
		x = CAR(args) = coerceVector(x, REALSXP);
		y = CADR(args) = coerceVector(y, REALSXP);
		x = real_binary(PRIMVAL(op), x, y);
	}
	else {
		x = integer_binary(PRIMVAL(op), x, y);
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

static SEXP unary(SEXP op, SEXP s1)
{
	switch (TYPEOF(s1)) {
	case LGLSXP:
	case INTSXP:
		return integer_unary(PRIMVAL(op), s1);
	case REALSXP:
		return real_unary(PRIMVAL(op), s1);
#ifdef COMPLEX_DATA
	case CPLXSXP:
		return complex_unary(PRIMVAL(op), s1);
#endif
	default:
		errorcall(lcall, "Invalid argument to unary operator\n");
	}
}

static SEXP integer_unary(int code, SEXP s1)
{
	int i, n, x;
	SEXP ans;

	switch(code) {
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
		error("illegal unary operator\n");
	}
}

static SEXP real_unary(int code, SEXP s1)
{
	int i, n;
	double x;
	SEXP ans;

	switch(code) {
	case PLUSOP: return s1;
	case MINUSOP:
		ans = duplicate(s1);
		n = LENGTH(s1);
		for (i = 0; i < n; i++) {
			x = REAL(s1)[i];
			REAL(ans)[i] = FINITE(x) ?
				((x == 0.0) ? 0.0 : -x) : NA_REAL;
		}
		return ans;
	default:
		errorcall(lcall, "illegal unary operator\n");
	}
}

static SEXP integer_binary(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
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
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				INTEGER(ans)[i] = NA_INTEGER;
			else
				INTEGER(ans)[i] = x1 + x2;
		}
		break;
	case MINUSOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				INTEGER(ans)[i] = NA_INTEGER;
			else
				INTEGER(ans)[i] = x1 - x2;
		}
		break;
	case TIMESOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				INTEGER(ans)[i] = NA_INTEGER;
			else
				INTEGER(ans)[i] = x1 * x2;
		}
		break;
	case DIVOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
				REAL(ans)[i] = NA_REAL;
			else
				REAL(ans)[i] = (double) x1 / (double) x2;
		}
		break;
	case POWOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				REAL(ans)[i] = NA_REAL;
			else {
				REAL(ans)[i] = MATH_CHECK(pow((double) x1, (double) x2));
			}
		}
		break;
	case MODOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER || x2 == 0)
				INTEGER(ans)[i] = NA_INTEGER;
			else {
				INTEGER(ans)[i] = x1 % x2;
			}
		}
		break;
	case IDIVOP:
		for (i = 0; i < n; i++) {
			x1 = INTEGER(s1)[i % n1];
			x2 = INTEGER(s2)[i % n2];
			if (x1 == NA_INTEGER || x2 == NA_INTEGER)
				INTEGER(ans)[i] = NA_INTEGER;
			else if(x2 == 0)
				INTEGER(ans)[i] = 0;
			else
				INTEGER(ans)[i] = floor((double)x1 / (double)x2);
		}
		break;
	}
	return ans;
}

static double myfmod(double x1, double x2)
{
	double q = x1 / x2;
	return x1 - ((x1 < 0.0) ?  ceil(q) : floor(q)) * x2;
}

static SEXP real_binary(int code, SEXP s1, SEXP s2)
{
	int i, n, n1, n2;
	double x1, x2;
	SEXP ans;

	n1 = LENGTH(s1);
	n2 = LENGTH(s2);
	n = (n1 > n2) ? n1 : n2;
	PROTECT(s1);
	PROTECT(s2);
	ans = allocVector(REALSXP, n);
	UNPROTECT(2);

	if (n1 < 1 || n2 < 1) {
		for (i = 0; i < n; i++)
			REAL(ans)[i] = NA_REAL;
		return ans;
	}

	switch (code) {
	case PLUSOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				REAL(ans)[i] = MATH_CHECK(x1 + x2);
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case MINUSOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				REAL(ans)[i] = MATH_CHECK(x1 - x2);
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case TIMESOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				REAL(ans)[i] = MATH_CHECK(x1 * x2);
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case DIVOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2) && x2 != 0.0)
				REAL(ans)[i] = MATH_CHECK(x1 / x2);
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case POWOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2))
				REAL(ans)[i] = MATH_CHECK(pow(x1, x2));
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case MODOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2) && x2 != 0.0)
				REAL(ans)[i] = MATH_CHECK(myfmod(x1, x2));
			else
				REAL(ans)[i] = NA_REAL;
		}
		break;
	case IDIVOP:
		for (i = 0; i < n; i++) {
			x1 = REAL(s1)[i % n1];
			x2 = REAL(s2)[i % n2];
			if (FINITE(x1) && FINITE(x2)) {
				if(x2 == 0.0)
					REAL(ans)[i] = 0.0;
				else
					REAL(ans)[i] = MATH_CHECK(floor(x1 / x2));
			}
			else REAL(ans)[i] = NA_REAL;
		}
		break;
	}
	return ans;
}

	/* Mathematical Functions of One Argument */

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
			if (FINITE(a[i])) {
				y[i] = MATH_CHECK(f(a[i]));
				if(!FINITE(y[i])) {
					y[i] = NA_REAL;
					naflag = 1;
				}
			}
			else y[i] = NA_REAL;
		}
		if (naflag)
			warning("NAs produced in function \"%s\"\n", PRIMNAME(op));
		ATTRIB(sy) = duplicate(ATTRIB(sa));
		OBJECT(sy) = OBJECT(sa);
		UNPROTECT(2);
		return sy;
	}
	else errorcall(lcall, "Non-numeric argument to mathematical function\n");
}

SEXP do_math1(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s;

	checkArity(op, args);

	if( DispatchGroup("Math", call, op, args, env, &s) )
		return s;

#ifdef COMPLEX_DATA
	if (isComplex(CAR(args)))
		return complex_math1(call, op, args, env);
#endif
	lcall = call;

	switch (PRIMVAL(op)) {
	case 0: return math1(op, CAR(args), fabs);
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

	case 40: return math1(op, CAR(args), lgamma);
	case 41: return math1(op, CAR(args), gamma);

	case 42: return math1(op, CAR(args), digamma);
	case 43: return math1(op, CAR(args), trigamma);
	case 44: return math1(op, CAR(args), tetragamma);
	case 45: return math1(op, CAR(args), pentagamma);

	default:
		errorcall(lcall, "unimplemented real function\n");
	}
}

static SEXP math2(SEXP op, SEXP sa, SEXP sb, double (*f)())
{
	SEXP sy;
	int i, n, na, nb;
	double ai, bi, *a, *b, *y;

	if (!isNumeric(sa) || !isNumeric(sb))
		errorcall(lcall, "Non-numeric argument to mathematical function\n");

	na = LENGTH(sa);
	nb = LENGTH(sb);
	n = (na < nb) ? nb : na;
	PROTECT(sa = coerceVector(sa, REALSXP));
	PROTECT(sb = coerceVector(sb, REALSXP));
	PROTECT(sy = allocVector(REALSXP, n));
	a = REAL(sa);
	b = REAL(sb);
	y = REAL(sy);
	if (na < 1 || nb < 1) {
		for (i = 0; i < n; i++)
			y[i] = NA_REAL;
	}
	else {
		naflag = 0;
		for (i = 0; i < n; i++) {
			ai = a[i % na];
			bi = b[i % nb];
			if(FINITE(ai) && FINITE(bi)) {
				y[i] = MATH_CHECK(f(ai, bi));
				if(!FINITE(y[i])) {
					y[i] = NA_REAL;
					naflag = 1;
				}
			}
			else y[i] = NA_REAL;
		}
	}
	if (naflag)
		warning("NAs produced in function \"%s\"\n", PRIMNAME(op));
	if(n == na) {
		ATTRIB(sy) = duplicate(ATTRIB(sa));
		OBJECT(sy) = OBJECT(sa);
	}
	else if(n == nb) {
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

#ifdef COMPLEX_DATA
	if (isComplex(CAR(args)))
		return complex_math2(call, op, args, env);
#endif

	switch (PRIMVAL(op)) {
	case 0: return math2(op, CAR(args), CADR(args), atan2);
	case 1: return math2(op, CAR(args), CADR(args), prec);

	case 2: return math2(op, CAR(args), CADR(args), lbeta);
	case 3: return math2(op, CAR(args), CADR(args), beta);
	case 4: return math2(op, CAR(args), CADR(args), lchoose);
	case 5: return math2(op, CAR(args), CADR(args), choose);

	case 6: return math2(op, CAR(args), CADR(args), dchisq);
	case 7: return math2(op, CAR(args), CADR(args), pchisq);
	case 8: return math2(op, CAR(args), CADR(args), qchisq);

	case 9: return math2(op, CAR(args), CADR(args), dexp);
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

	default:
		errorcall(lcall, "unimplemented real function\n");
	}
}

SEXP do_atan(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s;
	int n;
	if( DispatchGroup("Math", call, op, args, env, &s) )
		return s;
	switch(n = length(args)) {
	case 1:
#ifdef COMPLEX_DATA
		if (isComplex(CAR(args)))
			return complex_math1(call, op, args, env);
		else
#endif
		return math1(op, CAR(args), atan);
	case 2:
#ifdef COMPLEX_DATA
		if (isComplex(CAR(args)) || isComplex(CDR(args)))
			return complex_math2(call, op, args, env);
		else
#endif
		return math2(op, CAR(args), CADR(args), atan2);
	default:
		error("%d arguments passed to \"atan\" which requires 1 or 2\n", n);
	}
}

SEXP do_round(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP a, b;
	int n;
	if( DispatchGroup("Math", call, op, args, env, &a) )
		return a;
	lcall = call;
	switch(n = length(args)) {
	case 1: PROTECT(a = CAR(args));
		PROTECT(b = allocVector(REALSXP, 1));
		REAL(b)[0] = 0;
		break;
	case 2: PROTECT(a = CAR(args));
		PROTECT(b = CADR(args));
		break;
	default: error("%d arguments passed to \"round\" which requires 1 or 2\n", n);
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
	if( DispatchGroup("Math", call, op, args, env, &s) )
		return s;
	switch(n = length(args)) {
	case 1:
#ifdef COMPLEX_DATA
		if (isComplex(CAR(args)))
			return complex_math1(call, op, args, env);
		else
#endif
		return math1(op, CAR(args), log);
	case 2:
#ifdef COMPLEX_DATA
		if (isComplex(CAR(args)) || isComplex(CDR(args)))
			return complex_math2(call, op, args, env);
		else
#endif
		return math2(op, CAR(args), CADR(args), logbase);
	default:
		error("%d arguments passed to \"log\" which requires 1 or 2\n", n);
	}
}

static SEXP math3(SEXP op, SEXP sa, SEXP sb, SEXP sc, double (*f)())
{
	SEXP sy;
	int i, n, na, nb, nc;
	double ai, bi, ci, *a, *b, *c, *y;

	if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc))
		errorcall(lcall, "Non-numeric argument to mathematical function\n");
	
	na = LENGTH(sa);
	nb = LENGTH(sb); 
	nc = LENGTH(sc); 
	n = na;
	if(n < nb) n = nb;
	if(n < nc) n = nc;
	PROTECT(sa = coerceVector(sa, REALSXP));
	PROTECT(sb = coerceVector(sb, REALSXP));
	PROTECT(sc = coerceVector(sc, REALSXP));
	PROTECT(sy = allocVector(REALSXP, n));
	a = REAL(sa);
	b = REAL(sb);
	c = REAL(sc);
	y = REAL(sy);
	if (na < 1 || nb < 1 || nc < 1) {
		for (i = 0; i < n; i++)
			y[i] = NA_REAL;
	}
	else {
		naflag = 0;
		for (i = 0; i < n; i++) {
			ai = a[i % na];
			bi = b[i % nb];
			ci = c[i % nc];
			if(FINITE(ai) && FINITE(bi) && FINITE(ci)) {
				y[i] = MATH_CHECK(f(ai, bi, ci));
				if(!FINITE(y[i])) {
					y[i] = NA_REAL;
					naflag = 1;
				}
			}
			else y[i] = NA_REAL;
		}
	}
	if (naflag)
		warning("NAs produced in function \"%s\"\n", PRIMNAME(op));
	if(n == na) {
		ATTRIB(sy) = duplicate(ATTRIB(sa));
		OBJECT(sy) = OBJECT(sa);
	}
	else if(n == nb) {
		ATTRIB(sy) = duplicate(ATTRIB(sb));
		OBJECT(sy) = OBJECT(sb);
	}
	else if(n == nc) {
		ATTRIB(sy) = duplicate(ATTRIB(sc));
		OBJECT(sy) = OBJECT(sc);
	}
	UNPROTECT(4);
	return sy;
}

	/* Mathematical Functions of Three (Real) Arguments */

SEXP do_math3(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);

	switch (PRIMVAL(op)) {

	case 1:  return math3(op, CAR(args), CADR(args), CADDR(args), dbeta);
	case 2:  return math3(op, CAR(args), CADR(args), CADDR(args), pbeta);
	case 3:  return math3(op, CAR(args), CADR(args), CADDR(args), qbeta);

	case 4:  return math3(op, CAR(args), CADR(args), CADDR(args), dbinom);
	case 5:  return math3(op, CAR(args), CADR(args), CADDR(args), pbinom);
	case 6:  return math3(op, CAR(args), CADR(args), CADDR(args), qbinom);

	case 7:  return math3(op, CAR(args), CADR(args), CADDR(args), dcauchy);
	case 8:  return math3(op, CAR(args), CADR(args), CADDR(args), pcauchy);
	case 9:  return math3(op, CAR(args), CADR(args), CADDR(args), qcauchy);

	case 10:  return math3(op, CAR(args), CADR(args), CADDR(args), df);
	case 11:  return math3(op, CAR(args), CADR(args), CADDR(args), pf);
	case 12:  return math3(op, CAR(args), CADR(args), CADDR(args), qf);

	case 13:  return math3(op, CAR(args), CADR(args), CADDR(args), dgamma);
	case 14:  return math3(op, CAR(args), CADR(args), CADDR(args), pgamma);
	case 15:  return math3(op, CAR(args), CADR(args), CADDR(args), qgamma);

	case 16:  return math3(op, CAR(args), CADR(args), CADDR(args), dlnorm);
	case 17:  return math3(op, CAR(args), CADR(args), CADDR(args), plnorm);
	case 18:  return math3(op, CAR(args), CADR(args), CADDR(args), qlnorm);

	case 19:  return math3(op, CAR(args), CADR(args), CADDR(args), dlogis);
	case 20:  return math3(op, CAR(args), CADR(args), CADDR(args), plogis);
	case 21:  return math3(op, CAR(args), CADR(args), CADDR(args), qlogis);

	case 22:  return math3(op, CAR(args), CADR(args), CADDR(args), dnbinom);
	case 23:  return math3(op, CAR(args), CADR(args), CADDR(args), pnbinom);
	case 24:  return math3(op, CAR(args), CADR(args), CADDR(args), qnbinom);

	case 25:  return math3(op, CAR(args), CADR(args), CADDR(args), dnorm);
	case 26:  return math3(op, CAR(args), CADR(args), CADDR(args), pnorm);
	case 27:  return math3(op, CAR(args), CADR(args), CADDR(args), qnorm);

	case 28:  return math3(op, CAR(args), CADR(args), CADDR(args), dunif);
	case 29:  return math3(op, CAR(args), CADR(args), CADDR(args), punif);
	case 30:  return math3(op, CAR(args), CADR(args), CADDR(args), qunif);

	case 31:  return math3(op, CAR(args), CADR(args), CADDR(args), dweibull);
	case 32:  return math3(op, CAR(args), CADR(args), CADDR(args), pweibull);
	case 33:  return math3(op, CAR(args), CADR(args), CADDR(args), qweibull);

	/*
	case 34:  return math3(op, CAR(args), CADR(args), CADDR(args), dnchisq);
	*/
	case 35:  return math3(op, CAR(args), CADR(args), CADDR(args), pnchisq);
	case 36:  return math3(op, CAR(args), CADR(args), CADDR(args), qnchisq);

	default:
		 errorcall(lcall, "unimplemented real function\n");
	}
}

static SEXP math4(SEXP op, SEXP sa, SEXP sb, SEXP sc, SEXP sd, double (*f)())
{
	SEXP sy;
	int i, n, na, nb, nc, nd;
	double ai, bi, ci, di, *a, *b, *c, *d, *y;

	if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc) || !isNumeric(sd))
		errorcall(lcall, "Non-numeric argument to mathematical function\n");
	
	na = LENGTH(sa);
	nb = LENGTH(sb); 
	nc = LENGTH(sc); 
	nd = LENGTH(sd); 
	n = na;
	if(n < nb) n = nb;
	if(n < nc) n = nc;
	if(n < nd) n = nd;
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
	if (na < 1 || nb < 1 || nc < 1 || nd < 1) {
		for (i = 0; i < n; i++)
			y[i] = NA_REAL;
	}
	else {
		naflag = 0;
		for (i = 0; i < n; i++) {
			ai = a[i % na];
			bi = b[i % nb];
			ci = c[i % nc];
			di = d[i % nd];
			if(FINITE(ai) && FINITE(bi) && FINITE(ci) && FINITE(di)) {
				y[i] = MATH_CHECK(f(ai, bi, ci, di));
				if(!FINITE(y[i])) {
					y[i] = NA_REAL;
					naflag = 1;
				}
			}
			else y[i] = NA_REAL;
		}
	}
	if (naflag)
		warning("NAs produced in function \"%s\"\n", PRIMNAME(op));
	if(n == na) {
		ATTRIB(sy) = duplicate(ATTRIB(sa));
		OBJECT(sy) = OBJECT(sa);
	}
	else if(n == nb) {
		ATTRIB(sy) = duplicate(ATTRIB(sb));
		OBJECT(sy) = OBJECT(sb);
	}
	else if(n == nc) {
		ATTRIB(sy) = duplicate(ATTRIB(sc));
		OBJECT(sy) = OBJECT(sc);
	}
	else if(n == nd) {
		ATTRIB(sy) = duplicate(ATTRIB(sd));
		OBJECT(sy) = OBJECT(sd);
	}
	UNPROTECT(5);
	return sy;
}

	/* Mathematical Functions of Four (Real) Arguments */

SEXP do_math4(SEXP call, SEXP op, SEXP args, SEXP env)
{
	checkArity(op, args);

	switch (PRIMVAL(op)) {
	case 1: return math4(op, CAR(args), CADR(args), CADDR(args), CADDDR(args), dhyper);
	case 2: return math4(op, CAR(args), CADR(args), CADDR(args), CADDDR(args), phyper);
	case 3: return math4(op, CAR(args), CADR(args), CADDR(args), CADDDR(args), qhyper);
	default:
		 errorcall(lcall, "unimplemented real function\n");
	}
}
