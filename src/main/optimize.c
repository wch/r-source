/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--1999  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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

#include "Defn.h"
#include "Print.h"		/*for printRealVector()*/
#include "Mathlib.h"
#include "Applic.h"
#include "S.h"		/* for the Calloc and Free macros */

/* WARNING : As things stand, these routines should not be called
 *	     recursively because of the way global variables are used.
 *	     This could be fixed by saving and restoring these global variables.
 */


/* One Dimensional Minimization --- just wrapper code for Brent's "fmin" */

static SEXP R_fcall1;
static SEXP R_env1;

static double F77_SYMBOL(fcn1)(double *x)
{
    SEXP s;
    REAL(CADR(R_fcall1))[0] = *x;
    s = eval(R_fcall1, R_env1);
    switch(TYPEOF(s)) {
    case INTSXP:
	if (length(s) != 1) goto badvalue;
	if (INTEGER(s)[0] == NA_INTEGER) {
	    warning("NA replaced by maximum positive value");
	    return DBL_MAX;
	}
	else return INTEGER(s)[0];
	break;
    case REALSXP:
	if (length(s) != 1) goto badvalue;
	if (!R_FINITE(REAL(s)[0])) {
	    warning("NA/Inf replaced by maximum positive value");
	    return DBL_MAX;
	}
	else return REAL(s)[0];
	break;
    default:
	goto badvalue;
    }
 badvalue:
    error("invalid function value in 'fmin' optimizer");
    return 0;/* for -Wall */
}

/* fmin(f, xmin, xmax tol) */
SEXP do_fmin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double xmin, xmax, tol;
    SEXP v;

    checkArity(op, args);
    PrintDefaults(rho);

    /* the function to be minimized */

    v = CAR(args);
    if (!isFunction(v))
	errorcall(call, "attempt to minimize non-function");
    args = CDR(args);

    /* xmin */

    xmin = asReal(CAR(args));
    if (!R_FINITE(xmin))
	errorcall(call, "invalid xmin value");
    args = CDR(args);

    /* xmax */

    xmax = asReal(CAR(args));
    if (!R_FINITE(xmax))
	errorcall(call, "invalid xmax value");
    if (xmin >= xmax)
	errorcall(call, "xmin not less than xmax");
    args = CDR(args);

    /* tol */

    tol = asReal(CAR(args));
    if (!R_FINITE(tol) || tol <= 0.0)
	errorcall(call, "invalid tol value");

    R_env1 = rho;
    PROTECT(R_fcall1 = lang2(v, R_NilValue));
    CADR(R_fcall1) = allocVector(REALSXP, 1);
    REAL(CADR(R_fcall1))[0] = F77_SYMBOL(fmin)(&xmin, &xmax, F77_SYMBOL(fcn1), &tol);
    UNPROTECT(1);
    return CADR(R_fcall1);
}



/* One Dimensional Root Finding --  just wrapper code for Brent's "zeroin" */

struct callinfo {
  SEXP R_fcall2;
  SEXP R_env2;
} ;

double fcn2(double x, struct callinfo *info)
{
    SEXP s;
    REAL(CADR(info->R_fcall2))[0] = x;
    s = eval(info->R_fcall2, info->R_env2);
    switch(TYPEOF(s)) {
    case INTSXP:
	if (length(s) != 1) goto badvalue;
	if (INTEGER(s)[0] == NA_INTEGER) {
	    warning("NA replaced by maximum positive value");
	    return	DBL_MAX;
	}
	else return INTEGER(s)[0];
	break;
    case REALSXP:
	if (length(s) != 1) goto badvalue;
	if (!R_FINITE(REAL(s)[0])) {
	    warning("NA/Inf replaced by maximum positive value");
	    return DBL_MAX;
	}
	else return REAL(s)[0];
	break;
    default:
	goto badvalue;
    }
 badvalue:
    error("invalid function value in 'zeroin'");
    return 0;/* for -Wall */

}

/* zeroin(f, xmin, xmax, tol, maxiter) */
SEXP do_zeroin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    double xmin, xmax, tol;
    int iter;
    SEXP v, res;
    struct callinfo info;

    checkArity(op, args);
    PrintDefaults(rho);

    /* the function to be minimized */

    v = CAR(args);
    if (!isFunction(v))
	errorcall(call,"attempt to minimize non-function");
    args = CDR(args);

    /* xmin */

    xmin = asReal(CAR(args));
    if (!R_FINITE(xmin))
	errorcall(call, "invalid xmin value");
    args = CDR(args);

    /* xmax */

    xmax = asReal(CAR(args));
    if (!R_FINITE(xmax))
	errorcall(call, "invalid xmax value");
    if (xmin >= xmax)
	errorcall(call, "xmin not less than xmax");
    args = CDR(args);

    /* tol */

    tol = asReal(CAR(args));
    if (!R_FINITE(tol) || tol <= 0.0)
	errorcall(call, "invalid tol value");
    args = CDR(args);

    /* maxiter */
    iter = asInteger(CAR(args));
    if (iter <= 0)
	errorcall(call, "maxiter must be positive");

    info.R_env2 = rho;
    PROTECT(info.R_fcall2 = lang2(v, R_NilValue)); /* the info used in fcn2() */
    CADR(info.R_fcall2) = allocVector(REALSXP, 1);
    PROTECT(res = allocVector(REALSXP, 3));
    REAL(res)[0] =
	zeroin(xmin, xmax,   (double (*)(double, void*)) fcn2,
	       (void *) &info, &tol, &iter);
    REAL(res)[1] = (double)iter;
    REAL(res)[2] = tol;
    UNPROTECT(2);
    return res;
}



/* General Nonlinear Optimization */

/* These are unevaluated calls to R functions supplied by the user.
 * When the optimizer needs a value, the functions below insert
 * the function argument and then evaluate the call.
 */
static SEXP R_fcall;	/* function */
static SEXP R_env;	/* where to evaluate the calls */

#ifdef NOT_yet_used
static SEXP R_gcall;	/* gradient */
static SEXP R_hcall;	/* hessian */
#endif

static SEXP R_gradientSymbol;
static SEXP R_hessianSymbol;

static int have_gradient;
static int have_hessian;

#define FT_SIZE 5		/* size of table to store computed
				   function values */
static int FT_last;		/* Newest entry in the table */
/*static int xdim;		-* length of the parameter (x) vector */

static struct {
    double   fval;
    double  *x;
    double  *grad;
    double  *hess;
} Ftable[FT_SIZE];

/* Initialize the storage in the table of computed function values */

static void FT_init(int n)
{
    int i, j;

    for (i = 0; i < FT_SIZE; i++) {
	Ftable[i].x =  Calloc(n, double);
				/* initialize to unlikely parameter values */
	for (j = 0; j < n; j++) {
	    Ftable[i].x[j] = DBL_MAX;
	}
	if (have_gradient) {
	    Ftable[i].grad =  Calloc(n, double);
	    if (have_hessian) {
		Ftable[i].hess =  Calloc(n * n, double);
	    }
	}
    }
    FT_last = -1;
}

/* Free the storage from the table of computed function values */

static void FT_free()
{
    int i;
    for (i = 0; i < FT_SIZE; i++) {
	Free(Ftable[i].x);
	if (have_gradient) {
	    Free(Ftable[i].grad);
	    if (have_hessian) {
		Free(Ftable[i].hess);
	    }
	}
    }
    FT_last = -1;
}

/* Store an entry in the table of computed function values */

static void FT_store(int n, double f, double *x, double *grad, double *hess)
{
    int ind;

    ind = (++FT_last) % FT_SIZE;
    Ftable[ind].fval = f;
    Memcpy(Ftable[ind].x, x, n);
    if (grad) {
        Memcpy(Ftable[ind].grad, grad, n);
	if (hess) {
	    Memcpy(Ftable[ind].hess, hess, n * n);
	}
    }
}

/* Check for stored values in the table of computed function values.
   Returns the index in the table or -1 for failure */

static int FT_lookup(int n, double *x)
{
    double *ftx;
    int i, j, ind, matched;

    for (i = 0; i < FT_SIZE; i++) {
	ind = (FT_last - i) % FT_SIZE;
				/* why can't they define modulus correctly */
	if (ind < 0) ind += FT_SIZE;
	ftx = Ftable[ind].x;
	if (ftx) {
	    matched = 1;
	    for (j = 0; j < n; j++) {
	        if (x[j] != ftx[j]) {
	            matched = 0;
		    break;
	        }
	    }
	    if (matched) return ind;
        }
    }
    return -1;
}

/* This how the optimizer sees them */

static int F77_SYMBOL(fcn)(int *n, double *x, double *f)
{
    SEXP s;
    double *g = (double *) 0, *h = (double *) 0;
    int i;

    if ((i = FT_lookup(*n, x)) >= 0) {
	*f = Ftable[i].fval;
	return 0;
    }
				/* calculate for a new value of x */
    s = allocVector(REALSXP, *n);
    for (i = 0; i < *n; i++)
	REAL(s)[i] = x[i];
    CADR(R_fcall) = s;
    s = eval(R_fcall, R_env);
    switch(TYPEOF(s)) {
    case INTSXP:
	if (length(s) != 1) goto badvalue;
	if (INTEGER(s)[0] == NA_INTEGER) {
	    warning("NA replaced by maximum positive value");
	    *f = DBL_MAX;
	}
	else *f = INTEGER(s)[0];
	break;
    case REALSXP:
	if (length(s) != 1) goto badvalue;
	if (!R_FINITE(REAL(s)[0])) {
	    warning("NA/Inf replaced by maximum positive value");
	    *f = DBL_MAX;
	}
	else *f = REAL(s)[0];
	break;
    default:
	goto badvalue;
    }
    if (have_gradient) {
	g = REAL(coerceVector(getAttrib(s, R_gradientSymbol), REALSXP));
	if (have_hessian) {
	    h = REAL(coerceVector(getAttrib(s, R_hessianSymbol), REALSXP));
	}
    }
    FT_store(*n, *f, x, g, h);
    return 0;
 badvalue:
    error("invalid function value in 'nlm' optimizer");
    return 0;/* for -Wall */
}


static int F77_SYMBOL(Cd1fcn)(int *n, double *x, double *g)
{
    /* error("optimization using analytic gradients not implemented
       (yet)\n"); */
    int ind;

    if ((ind = FT_lookup(*n, x)) < 0) {	/* shouldn't happen */
	F77_SYMBOL(fcn)(n, x, g);
	if ((ind = FT_lookup(*n, x)) < 0) {
	    error("function value caching for optimization is seriously confused.\n");
	}
    }
    Memcpy(g, Ftable[ind].grad, *n);
    return 0;
}


static int F77_SYMBOL(Cd2fcn)(int *nr, int *n, double *x, double *h)
{
    /*  error("optimization using analytic Hessians not implemented
	(yet)\n"); */
    int j, ind;

    if ((ind = FT_lookup(*n, x)) < 0) {	/* shouldn't happen */
	F77_SYMBOL(fcn)(n, x, h);
	if ((ind = FT_lookup(*n, x)) < 0) {
	    error("function value caching for optimization is seriously confused.\n");
	}
    }
    for (j = 0; j < *n; j++) {  /* fill in lower triangle only */
        Memcpy( h + j*(*n + 1), Ftable[ind].hess + j*(*n + 1), *n - j);
    }
    return 0;
}


static double *fixparam(SEXP p, int *n, SEXP call)
{
    double *x;
    int i;

    if (!isNumeric(p))
	errorcall(call, "numeric parameter expected");

    if (*n) {
	if (LENGTH(p) != *n)
	    errorcall(call, "conflicting parameter lengths");
    }
    else {
	if (LENGTH(p) <= 0)
	    errorcall(call, "invalid parameter length");
	*n = LENGTH(p);
    }

    x = (double*)R_alloc(*n, sizeof(double));
    switch(TYPEOF(p)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < *n; i++) {
	    if (INTEGER(p)[i] == NA_INTEGER)
		errorcall(call, "missing value in parameter");
	    x[i] = INTEGER(p)[i];
	}
	break;
    case REALSXP:
	for (i = 0; i < *n; i++) {
	    if (!R_FINITE(REAL(p)[i]))
		errorcall(call, "missing value in parameter");
	    x[i] = REAL(p)[i];
	}
	break;
    default:
	errorcall(call, "invalid parameter type");
    }
    return x;
}


static void invalid_na(SEXP call)
{
    errorcall(call, "invalid NA value in parameter");
}


	/* Fatal errors - we don't deliver an answer */

static void opterror(int nerr)
{
    switch(nerr) {
    case -1:
	error("non-positive number of parameters in nlm");
    case -2:
	error("nlm is inefficient for 1-d problems");
    case -3:
	error("illegal gradient tolerance in nlm");
    case -4:
	error("illegal iteration limit in nlm");
    case -5:
	error("minimization function has no good digits in nlm");
    case -6:
	error("no analytic gradient to check in nlm!");
    case -7:
	error("no analytic Hessian to check in nlm!");
    case -21:
	error("probable coding error in analytic gradient");
    case -22:
	error("probable coding error in analytic Hessian");
    default:
	error("*** unknown error message (msg = %d) in nlm()\n*** should not happen!", nerr);
    }
}


	/* Warnings - we return a value, but print a warning */

static void optcode(int code)
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
	Rprintf("or steptol is too large.\n");
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
    SEXP value, names, v;

    double *x, *typsiz, fscale, gradtl, stepmx,
	steptol, *xpls, *gpls, fpls, *a, *wrk, dlt;

    int code, i, j, k, ipr, itnlim, method, iexp, omsg, msg,
	n, ndigit, iagflg, iahflg, want_hessian, itncnt;

    char *vmax;

    checkArity(op, args);
    PrintDefaults(rho);
    vmax = vmaxget();

    /* the function to be minimized */

    R_env = rho;
    v = CAR(args);
    if (!isFunction(v))
	error("attempt to minimize non-function");
    PROTECT(R_fcall = lang2(v, R_NilValue));
    args = CDR(args);

    /* inital parameter value */

    n = 0;
    x = fixparam(CAR(args), &n, call);
    args = CDR(args);

    /* hessian required? */

    want_hessian = asLogical(CAR(args));
    if (want_hessian == NA_LOGICAL) want_hessian = 0;
    args = CDR(args);

    /* typical size of parameter elements */

    typsiz = fixparam(CAR(args), &n, call);
    args = CDR(args);

    /* expected function size */

    fscale = asReal(CAR(args));
    if (ISNA(fscale)) invalid_na(call);
    args = CDR(args);

    omsg = msg = asInteger(CAR(args));
    if (msg == NA_INTEGER) invalid_na(call);
    args = CDR(args);

    ndigit = asInteger(CAR(args));
    if (ndigit == NA_INTEGER) invalid_na(call);
    args = CDR(args);

    gradtl = asReal(CAR(args));
    if (ISNA(gradtl)) invalid_na(call);
    args = CDR(args);

    stepmx = asReal(CAR(args));
    if (ISNA(stepmx)) invalid_na(call);
    args = CDR(args);

    steptol = asReal(CAR(args));
    if (ISNA(steptol)) invalid_na(call);
    args = CDR(args);

    itnlim = asInteger(CAR(args));
    if (itnlim == NA_INTEGER) invalid_na(call);
    args = CDR(args);

    /* force one evaluation to check for the gradient and hessian */
    iagflg = 0;			/* No analytic gradient */
    iahflg = 0;			/* No analytic hessian */
    have_gradient = 0;
    have_hessian = 0;
    R_gradientSymbol = install("gradient");
    R_hessianSymbol = install("hessian");

    v = allocVector(REALSXP, n);
    for (i = 0; i < n; i++) {
	REAL(v)[i] = x[i];
    }
    CADR(R_fcall) = v;
    value = eval(R_fcall, R_env);

    v = getAttrib(value, R_gradientSymbol);
    if (v != R_NilValue && LENGTH(v) == n && (isReal(v) || isInteger(v))) {
      iagflg = 1;
      have_gradient = 1;
       v = getAttrib(value, R_hessianSymbol);
       if (v != R_NilValue && LENGTH(v) == (n * n) &&
 	  (isReal(v) || isInteger(v))) {
 	iahflg = 1;
 	have_hessian = 1;
       }
    }
    if (((msg/4) % 2) && !iahflg) { /* don't check of analytic Hessian */
      msg -= 4;
    }
    if (((msg/2) % 2) && !iagflg) { /* don't check of analytic gradient */
      msg -= 2;
    }
    FT_init(n);
    /* Plug in the call to the optimizer here */

    method = 1;	/* Line Search */
    iexp = have_gradient ? 0 : 1; /* Function calls are expensive */
    ipr = 6;
    dlt = 1.0;

    xpls = (double*)R_alloc(n, sizeof(double));
    gpls = (double*)R_alloc(n, sizeof(double));
    a = (double*)R_alloc(n*n, sizeof(double));
    wrk = (double*)R_alloc(8*n, sizeof(double));

    /*
     *	 Dennis + Schnabel Minimizer
     *
     *	  SUBROUTINE OPTIF9(NR,N,X,FCN,D1FCN,D2FCN,TYPSIZ,FSCALE,
     *	 +	   METHOD,IEXP,MSG,NDIGIT,ITNLIM,IAGFLG,IAHFLG,IPR,
     *	 +	   DLT,GRADTL,STEPMX,STEPTOL,
     *	 +	   XPLS,FPLS,GPLS,ITRMCD,A,WRK)
     *
     *
     *	 Note: I have figured out what msg does.
     *	 It is actually a sum of bit flags as follows
     *	   1 = don't check/warn for 1-d problems
     *	   2 = don't check analytic gradients
     *	   4 = don't check analytic hessians
     *	   8 = don't print start and end info
     *	  16 = print at every iteration
     *	 Using msg=9 is absolutely minimal
     *	 I think we always check gradients and hessians
     */

    F77_SYMBOL(optif9)(&n, &n, x, F77_SYMBOL(fcn), F77_SYMBOL(Cd1fcn),
		       F77_SYMBOL(Cd2fcn), typsiz, &fscale,
		       &method, &iexp, &msg, &ndigit, &itnlim,
		       &iagflg, &iahflg, &ipr,
		       &dlt, &gradtl, &stepmx, &steptol,
		       xpls, &fpls, gpls, &code, a, wrk, &itncnt);

    if (msg < 0)
	opterror(msg);
    if (code != 0 && (omsg&8) == 0)
	optcode(code);

    if (want_hessian) {
	PROTECT(value = allocVector(VECSXP, 6));
	PROTECT(names = allocVector(STRSXP, 6));
	F77_SYMBOL(fdhess)(&n, xpls, &fpls, F77_SYMBOL(fcn), a, &n,
			   &wrk[0], &wrk[n], &ndigit, typsiz);
	for (i = 0; i < n; i++)
	    for (j = 0; j < i; j++)
		a[i + j * n] = a[j + i * n];
    }
    else {
	PROTECT(value = allocVector(VECSXP, 5));
	PROTECT(names = allocVector(STRSXP, 5));
    }
    k = 0;

    STRING(names)[k] = mkChar("minimum");
    VECTOR(value)[k] = allocVector(REALSXP, 1);
    REAL(VECTOR(value)[k])[0] = fpls;
    k++;

    STRING(names)[k] = mkChar("estimate");
    VECTOR(value)[k] = allocVector(REALSXP, n);
    for (i = 0; i < n; i++)
	REAL(VECTOR(value)[k])[i] = xpls[i];
    k++;

    STRING(names)[k] = mkChar("gradient");
    VECTOR(value)[k] = allocVector(REALSXP, n);
    for (i = 0; i < n; i++)
	REAL(VECTOR(value)[k])[i] = gpls[i];
    k++;

    if (want_hessian) {
	STRING(names)[k] = mkChar("hessian");
	VECTOR(value)[k] = allocMatrix(REALSXP, n, n);
	for (i = 0; i < n * n; i++)
	    REAL(VECTOR(value)[k])[i] = a[i];
	k++;
    }

    STRING(names)[k] = mkChar("code");
    VECTOR(value)[k] = allocVector(INTSXP, 1);
    INTEGER(VECTOR(value)[k])[0] = code;
    k++;

    /* added by Jim K Lindsey */
    STRING(names)[k] = mkChar("iterations");
    VECTOR(value)[k] = allocVector(INTSXP, 1);
    INTEGER(VECTOR(value)[k])[0] = itncnt;
    k++;

    setAttrib(value, R_NamesSymbol, names);
    FT_free();
    vmaxset(vmax);
    UNPROTECT(3);
    return value;
}

/*
 *  PURPOSE
 *
 *  Print information.	This code done in C to avoid the necessity
 *  of having the (vast) Fortran I/O library loaded.
 *
 *  PARAMETERS
 *
 *  nr	   --> row dimension of matrix
 *  n	   --> dimension of problem
 *  x(n)   --> iterate x[k]
 *  f	   --> function value at x[k]
 *  g(n)   --> gradient at x[k]
 *  a(n,n) --> hessian at x[k]
 *  p(n)   --> step taken
 *  itncnt --> iteration number k
 *  iflg   --> flag controlling info to print
 *  ipr	   --> device to which to send output [unused in C]
 */

int F77_SYMBOL(result)(int *nr, int *n, double *x, double *f, double *g,
		       double *a, double *p, int *itncnt, int *iflg, int *ipr)
{
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
