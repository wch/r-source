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


/*	W A R N I N G
 *
 *	As things stand, these routines should not be called
 *	recursively because of the way global variables are used.
 *	This could be fixed by making saving and restoring these
 *	global variables.
 */



/*--------------------------------------------------------------------*/

	/* One Dimensional Minimization */
	/* This is just wrapper code for Brent's "fmin" */

static SEXP R_fcall1;
static SEXP R_env1;

static double F77_SYMBOL(fcn1)(double *x)
{
	SEXP s;
	REAL(CADR(R_fcall1))[0] = *x;
	s = eval(R_fcall1, R_env1);
	switch(TYPEOF(s)) {
	case INTSXP:
		if(length(s) != 1) goto badvalue;
		if(INTEGER(s)[0] == NA_INTEGER) {
			REprintf("warning: NA replaced by maximum positive value\n");
			return  DBL_MAX;
		}
		else return INTEGER(s)[0];
		break;
	case REALSXP:
		if(length(s) != 1) goto badvalue;
		if(!FINITE(REAL(s)[0])) {
			REprintf("warning: NA replaced by maximum positive value\n");
			return DBL_MAX;
		}
		else return REAL(s)[0];
		break;
	default:
		goto badvalue;
	}
badvalue:
	error("invalid function value in optimizer\n");
}

	/* fmin(f, xmin, xmax tol) */

extern double F77_SYMBOL(fmin)();

SEXP do_fmin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	double ans, xmin, xmax, tol;
	SEXP v;

	checkArity(op, args);
	PrintDefaults(rho);

		/* the function to be minimized */

	v = CAR(args);
	if(!isFunction(v))
		error("attempt to minimize non-function\n");
	args = CDR(args);

		/* xmin */

	xmin = asReal(CAR(args));
	if(!isFinite(xmin))
		errorcall(call, "invalid xmin value\n");
	args = CDR(args);

		/* xmax */

	xmax = asReal(CAR(args));
	if(!isFinite(xmax))
		errorcall(call, "invalid xmax value\n");
	if(xmin >= xmax)
		errorcall(call, "xmin not less than xmax\n");
	args = CDR(args);

		/* tol */

	tol = asReal(CAR(args));
	if(!isFinite(tol) || tol <= 0.0)
		errorcall(call, "invalid tol value\n");

	R_env1 = rho;
	PROTECT(R_fcall1 = lang2(v, R_NilValue));
	CADR(R_fcall1) = allocVector(REALSXP, 1);
	REAL(CADR(R_fcall1))[0] = F77_SYMBOL(fmin)(&xmin, &xmax, F77_SYMBOL(fcn1), &tol);
	UNPROTECT(1);
	return CADR(R_fcall1);
}


/*--------------------------------------------------------------------*/

	/* One Dimensional Root Finding */
	/* This is just wrapper code for Brent's "zeroin" */

static SEXP R_fcall2;
static SEXP R_env2;

static double F77_SYMBOL(fcn2)(double *x)
{
	SEXP s;
	REAL(CADR(R_fcall2))[0] = *x;
	s = eval(R_fcall2, R_env2);
	switch(TYPEOF(s)) {
	case INTSXP:
		if(length(s) != 1) goto badvalue;
		if(INTEGER(s)[0] == NA_INTEGER) {
			REprintf("warning: NA replaced by maximum positive value\n");
			return  DBL_MAX;
		}
		else return INTEGER(s)[0];
		break;
	case REALSXP:
		if(length(s) != 1) goto badvalue;
		if(!FINITE(REAL(s)[0])) {
			REprintf("warning: NA replaced by maximum positive value\n");
			return DBL_MAX;
		}
		else return REAL(s)[0];
		break;
	default:
		goto badvalue;
	}
badvalue:
	error("invalid function value in optimizer\n");
}

	/* zeroin(f, xmin, xmax tol) */

extern double F77_SYMBOL(zeroin)();

SEXP do_zeroin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	double ans, xmin, xmax, tol;
	SEXP v;

	checkArity(op, args);
	PrintDefaults(rho);

		/* the function to be minimized */

	v = CAR(args);
	if(!isFunction(v))
		error("attempt to minimize non-function\n");
	args = CDR(args);

		/* xmin */

	xmin = asReal(CAR(args));
	if(!isFinite(xmin))
		errorcall(call, "invalid xmin value\n");
	args = CDR(args);

		/* xmax */

	xmax = asReal(CAR(args));
	if(!isFinite(xmax))
		errorcall(call, "invalid xmax value\n");
	if(xmin >= xmax)
		errorcall(call, "xmin not less than xmax\n");
	args = CDR(args);

		/* tol */

	tol = asReal(CAR(args));
	if(!isFinite(tol) || tol <= 0.0)
		errorcall(call, "invalid tol value\n");

	R_env2 = rho;
	PROTECT(R_fcall2 = lang2(v, R_NilValue));
	CADR(R_fcall2) = allocVector(REALSXP, 1);
	REAL(CADR(R_fcall2))[0] = F77_SYMBOL(zeroin)(&xmin, &xmax, F77_SYMBOL(fcn2), &tol);
	UNPROTECT(1);
	return CADR(R_fcall2);
}


/*--------------------------------------------------------------------*/

	/* General Nonlinear Optimization */

	/* These are unevaluated calls to R functions supplied */
	/* by the user.  When the optimizer needs a value the */
	/* functions below insert the function argument and then */
	/* evaluate the call. */

static SEXP R_fcall;	/* function */
static SEXP R_gcall;	/* gradient */
static SEXP R_hcall;	/* hessian */
static SEXP R_env;	/* where to evaluate the calls */

static int have_gradient;
static int have_hessian;



	/* This how the optimizer sees them */

static int F77_SYMBOL(fcn)(int *n, double *x, double *f)
{
	SEXP s;
	int i;

	s = allocVector(REALSXP, *n);
	for(i=0 ; i<*n ; i++)
		REAL(s)[i] = x[i];
	CADR(R_fcall) = s;
	s = eval(R_fcall, R_env);
	switch(TYPEOF(s)) {
	case INTSXP:
		if(length(s) != 1) goto badvalue;
		if(INTEGER(s)[0] == NA_INTEGER) {
			REprintf("warning: NA replaced by maximum positive value\n");
			*f = DBL_MAX;
		}
		else *f = INTEGER(s)[0];
		break;
	case REALSXP:
		if(length(s) != 1) goto badvalue;
		if(!FINITE(REAL(s)[0])) {
			REprintf("warning: NA replaced by maximum positive value\n");
			*f = DBL_MAX;
		}
		else *f = REAL(s)[0];
		break;
	default:
		goto badvalue;
	}
	return 0;
badvalue:
	error("invalid function value in optimizer\n");
}


static int F77_SYMBOL(d1fcn)(int *n, double *x, double *g)
{
	error("optimization using analytic gradients not implemented (yet)\n");
}


static int F77_SYMBOL(d2fcn)(int *n, double *x, double *g)
{
	error("optimization using analytic Hessians not implemented (yet)\n");
}


static double *fixparam(SEXP p, int *n, SEXP call)
{
	double *x;
	int i;

	if(!isNumeric(p))
		errorcall(call, "numeric parameter expected\n");

	if(*n) {
		if(LENGTH(p) != *n)
			errorcall(call, "conflicting parameter lengths\n");
	}
	else {
		if(LENGTH(p) <= 0) 
			errorcall(call, "invalid parameter length\n");
		*n = LENGTH(p);
	}

	x = (double*)R_alloc(*n, sizeof(double));
	switch(TYPEOF(p)) {
		case LGLSXP:
		case INTSXP:
			for(i=0 ; i<*n ; i++) {
				if(INTEGER(p)[i] == NA_INTEGER)
					errorcall(call, "missing value in parameter\n");
				x[i] = INTEGER(p)[i];
			}
			break;
		case REALSXP:
			for(i=0 ; i<*n ; i++) {
				if(!FINITE(REAL(p)[i]))
					errorcall(call, "missing value in parameter\n");
				x[i] = REAL(p)[i];
			}
			break;
		default:
			errorcall(call, "invalid parameter type\n");
	}
	return x;
}


static void invalid_na(SEXP call)
{
	errorcall(call, "invalid NA value in parameter\n");
}


	/* Fatal errors - we don't deliver an answer */

static int opterror(int nerr)
{
	switch(nerr) {
	case -1:
		error("non-positive number of parameters in nlm\n");
	case -2:
		error("nlm is inefficient for 1-d problems\n");
	case -3:
		error("illegal gradient tolerance in nlm\n");
	case -4:
		error("illegal iteration limit in nlm\n");
	case -5:
		error("minimization function has no good digits in nlm\n");
	case -6:
		error("no analytic gradient to check in nlm!\n");
	case -7:
		error("no analytic Hessian to check in nlm!\n");
	case -21:
		error("probable coding error in analytic gradient\n");
	case -22:
		error("probable coding error in analytic Hessian\n");
	}
}


	/* Warnings - we return a value, but print a warning */

static optcode(int code)
{
	switch(code) {
	case 1:
		Rprintf("Relative gradient close to zero.\n");
		Rprintf("Current iterate is probably solution.\n");
		break;
	case 2:
		Rprintf("Successive iterates within tolerance.\n");
		Rprintf("Current iterate is probably solution.\n");
		break;
	case 3:
		Rprintf("Last global step failed to locate a point lower than x.\n");
		Rprintf("Either x is an approximate local minimum of the function,\n");
		Rprintf("the function is too non-linear for this algorithm,\n");
		Rprintf("or steptl is too large.\n");
		break;
	case 4:
		Rprintf("Iteration limit exceeded.  Algorithm failed.\n");
		break;
	case 5:
		Rprintf("Maximum step size exceeded 5 consecutive times.\n");
		Rprintf("Either the function is unbounded below,\n");
		Rprintf("becomes asymptotic to a finite value\n");
		Rprintf("from above in some direction,\n");
		Rprintf("or stepmx is too small.\n");
		break;
	}
	Rprintf("\n");
}


SEXP do_nlm(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP value, v;
	double *x, *typsiz, fscale, gradtl, stepmx, steptl,
		*xpls, *gpls, fpls, *a, *wrk;
	double dlt;
	int code, i, j, ipr, itnlim, method, iexp, omsg, msg, n, ndigit;
	int iagflg, iahflg;
	int want_hessian;
	char *vmax;

	checkArity(op, args);
	PrintDefaults(rho);
	vmax = vmaxget();

		/* the function to be minimized */

	R_env = rho;
	v = CAR(args);
	if(!isFunction(v))
		error("attempt to minimize non-function\n");
	PROTECT(R_fcall = lang2(v, R_NilValue));
	args = CDR(args);

		/* inital parameter value */

	n = 0;
	x = fixparam(CAR(args), &n, call);
	args = CDR(args);

		/* hessian required? */

	want_hessian = asLogical(CAR(args));
	if(want_hessian == NA_LOGICAL) want_hessian = 0;
	args = CDR(args);

		/* typical size of parameter elements */

	typsiz = fixparam(CAR(args), &n, call);
	args = CDR(args);
	
		/* expected function size */

	fscale = asReal(CAR(args));
	if(fscale == NA_REAL) invalid_na(call);
	args = CDR(args);

	omsg = msg = asInteger(CAR(args));
	if(msg == NA_INTEGER) invalid_na(call);
	args = CDR(args);

	ndigit = asInteger(CAR(args));
	if(ndigit == NA_INTEGER) invalid_na(call);
	args = CDR(args);

	gradtl = asReal(CAR(args));
	if(gradtl == NA_REAL) invalid_na(call);
	args = CDR(args);

	stepmx = asReal(CAR(args));
	if(stepmx == NA_REAL) invalid_na(call);
	args = CDR(args);

	steptl = asReal(CAR(args));
	if(steptl == NA_REAL) invalid_na(call);
	args = CDR(args);

	itnlim = asInteger(CAR(args));
	if(itnlim == NA_INTEGER) invalid_na(call);
	args = CDR(args);

		/* Plug in the call to the optimizer here */

	method = 1;	/* Line Search */
	iexp = 1;	/* Function calls are expensive */
	iagflg = 0;	/* No analytic gradient */
	iahflg = 0;	/* No analytic hessian */
	ipr = 6;
	dlt = 1.0;

	xpls = (double*)R_alloc(n, sizeof(double));
	gpls = (double*)R_alloc(n, sizeof(double));
	a = (double*)R_alloc(n*n, sizeof(double));
	wrk = (double*)R_alloc(8*n, sizeof(double));

/*
 *   Dennis + Schnabel Minimizer
 *
 *    SUBROUTINE OPTIF9(NR,N,X,FCN,D1FCN,D2FCN,TYPSIZ,FSCALE,
 *   +     METHOD,IEXP,MSG,NDIGIT,ITNLIM,IAGFLG,IAHFLG,IPR,
 *   +     DLT,GRADTL,STEPMX,STEPTL,
 *   +     XPLS,FPLS,GPLS,ITRMCD,A,WRK)
 */

		/* Note: I have figured out what msg does. */
		/* It is actually a sum of bit flags as follows */
		/*     1 = don't check/warn for 1-d problems */
		/*     2 = don't check analytic gradients */
		/*     4 = don't check analytic hessians */
		/*     8 = don't print start and end info */
		/*    16 = print at every iteration */
		/* Using msg=9 is absolutely minimal */
		/* I think we always check gradients and hessians */


	F77_SYMBOL(optif9)(&n, &n, x, F77_SYMBOL(fcn), F77_SYMBOL(d1fcn),
		F77_SYMBOL(d2fcn), typsiz, &fscale,
		&method, &iexp, &msg, &ndigit, &itnlim, &iagflg, &iahflg, &ipr,
		&dlt, &gradtl, &stepmx, &steptl,
		xpls, &fpls, gpls, &code, a, wrk);

	if(msg < 0) opterror(msg);
	if(code != 0 && (omsg&8) == 0) optcode(code);

	if(want_hessian) {
		PROTECT(v = value = allocList(5));
		F77_SYMBOL(fdhess)(&n, xpls, &fpls, F77_SYMBOL(fcn), a, &n,
			&wrk[0], &wrk[n], &ndigit, typsiz);
		for(i=0 ; i<n ; i++)
			for(j=0 ; j<i ; j++)
				a[i+j*n] = a[j+i*n];
	}
	else {
		PROTECT(v = value = allocList(4));
	}

	CAR(v) = allocVector(REALSXP, 1);
	REAL(CAR(v))[0] = fpls;
	TAG(v) = install("minimum");
	v = CDR(v);

	CAR(v) = allocVector(REALSXP, n);
	for(i=0 ; i<n ; i++)
		REAL(CAR(v))[i] = xpls[i];
	TAG(v) = install("estimate");
	v = CDR(v);

	CAR(v) = allocVector(REALSXP, n);
	for(i=0 ; i<n ; i++)
		REAL(CAR(v))[i] = gpls[i];
	TAG(v) = install("gradient");
	v = CDR(v);

	if(want_hessian) {
		CAR(v) = allocMatrix(REALSXP, n, n);
		for(i=0 ; i<n*n ; i++)
			REAL(CAR(v))[i] = a[i];
		TAG(v) = install("hessian");
		v = CDR(v);
	}

	CAR(v) = allocVector(INTSXP, 1);
	INTEGER(CAR(v))[0] = code;
	TAG(v) = install("code");
	v = CDR(v);

	vmaxset(vmax); 
	UNPROTECT(2);
	return value;
}

/*
 *	PURPOSE
 *
 *	Print information.  This code done in C to avoid the necessity
 *	of having the (vast) Fortran I/O library loaded.
 *
 *	PARAMETERS
 *
 *	nr	   --> row dimension of matrix
 *	n	    --> dimension of problem
 *	x(n)	 --> iterate x[k]
 *	f	    --> function value at x[k]
 *	g(n)	 --> gradient at x[k]
 *	a(n,n)       --> hessian at x[k]
 *	p(n)	 --> step taken
 *	itncnt       --> iteration number k
 *	iflg	 --> flag controlling info to print
 *	ipr	  --> device to which to send output
 */

int F77_SYMBOL(result)(int *nr, int *n, double *x, double *f, double *g,
	double *a, double *p, int *itncnt, int *iflg, int *ipr)
{
	static int i, j;

		/* Print iteration number */

	Rprintf("iteration = %d\n", *itncnt);

		/* Print step */

	if (*iflg != 0) {
		Rprintf("Step:\n");
		printRealVector(p, *n, 1);
	}

		/* Print current iterate */

	Rprintf("Parameter:\n");
	printRealVector(x, *n, 1);

		/* Print function value */

	Rprintf("Function Value\n");
	printRealVector(f, 1, 1);

		/* Print gradient */

	Rprintf("Gradient:\n");
	printRealVector(g, *n, 1);

#ifdef NEVER
		/* Print Hessian */
		/* We don't do this because the printRealMatrix */
		/* code takes a SEXP rather than a double*. */
		/* We could do something ugly like use fixed */
		/* e format but that would be UGLY */

	if (*iflg != 0) {
	}
#endif

	Rprintf("\n");
	return 0;
}
