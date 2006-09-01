/*
     C code of the .Call/.External examples in `Writing R extensions'
     Compile with R CMD SHLIB R-exts.c
     The use the R code in R-exts.R
 */


/* ----- Calculating outer products example ----- */

#include <R.h>
#include <Rinternals.h>

SEXP out(SEXP x, SEXP y)
{
    int i, j, nx, ny;
    double tmp, *rx = REAL(x), *ry = REAL(y), *rans;
    SEXP ans, dim, dimnames, dd;

    nx = length(x); ny = length(y);
    PROTECT(ans = allocVector(REALSXP, nx*ny));
    rans = REAL(ans);
    for(i = 0; i < nx; i++) {
	tmp = rx[i];
	for(j = 0; j < ny; j++)
	    rans[i + nx*j] = tmp * ry[j];
    }
    PROTECT(dim = allocVector(INTSXP, 2));
    INTEGER(dim)[0] = nx; INTEGER(dim)[1] = ny;
    setAttrib(ans, R_DimSymbol, dim);

    PROTECT(dimnames = allocVector(VECSXP, 2));
    dd = getAttrib(x, R_NamesSymbol);
    SET_VECTOR_ELT(dimnames, 0, getAttrib(x, R_NamesSymbol));
    SET_VECTOR_ELT(dimnames, 1, getAttrib(y, R_NamesSymbol));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(3);
    return(ans);
}

/* get the list element named str, or return NULL */

SEXP getListElement(SEXP list, char *str)
{
    SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
    int i;

    for (i = 0; i < length(list); i++)
	if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	    elmt = VECTOR_ELT(list, i);
	    break;
	}
    return elmt;
}

SEXP getvar(SEXP name, SEXP rho)
{
    SEXP ans;

    if(!isString(name) || length(name) != 1)
	error("name is not a single string");
    if(!isEnvironment(rho))
	error("rho should be an environment");
    ans = findVar(install(CHAR(STRING_ELT(name, 0))), rho);
    printf("first value is %f\n", REAL(ans)[0]);
    return(R_NilValue);
}

/* ----- Convolution via .Call  ----- */

#include <Rdefines.h>
SEXP convolve2(SEXP a, SEXP b)
{
    int i, j, na, nb, nab;
    double *xa, *xb, *xab;
    SEXP ab;

    PROTECT(a = AS_NUMERIC(a));
    PROTECT(b = AS_NUMERIC(b));
    na = LENGTH(a); nb = LENGTH(b); nab = na + nb - 1;
    PROTECT(ab = NEW_NUMERIC(nab));
    xa = NUMERIC_POINTER(a); xb = NUMERIC_POINTER(b);
    xab = NUMERIC_POINTER(ab);
    for(i = 0; i < nab; i++) xab[i] = 0.0;
    for(i = 0; i < na; i++)
	for(j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return(ab);
}

SEXP convolve2b(SEXP a, SEXP b)
{
    int i, j, na, nb, nab;
    double *xa, *xb, *xab;
    SEXP ab;

    PROTECT(a = coerceVector(a, REALSXP));
    PROTECT(b = coerceVector(b, REALSXP));
    na = length(a); nb = length(b); nab = na + nb - 1;
    PROTECT(ab = allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b);
    xab = REAL(ab);
    for(i = 0; i < nab; i++) xab[i] = 0.0;
    for(i = 0; i < na; i++)
	for(j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return(ab);
}

/* ----- Convolution via .External  ----- */

SEXP convolveE(SEXP args)
{
    int i, j, na, nb, nab;
    double *xa, *xb, *xab;
    SEXP a, b, ab;

    PROTECT(a = coerceVector(CADR(args), REALSXP));
    PROTECT(b = coerceVector(CADDR(args), REALSXP));
    na = length(a); nb = length(b); nab = na + nb - 1;
    PROTECT(ab = allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b);
    xab = REAL(ab);
    for(i = 0; i < nab; i++) xab[i] = 0.0;
    for(i = 0; i < na; i++)
	for(j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return(ab);
}

/* ----- Show arguments  ----- */

#include <R_ext/PrtUtil.h>

SEXP showArgs(SEXP args)
{
    int i;
    Rcomplex cpl;
    char *name;
    SEXP el;

    args = CDR(args); /* skip 'name' */
    for(i = 0; args != R_NilValue; i++, args = CDR(args)) {
	name = CHAR(PRINTNAME(TAG(args)));
	el = CAR(args);
	switch(TYPEOF(el)) {
	case REALSXP:
	    Rprintf("[%d] '%s' %f\n", i+1, name, REAL(el)[0]);
	    break;
	case LGLSXP:
	case INTSXP:
	    Rprintf("[%d] '%s' %d\n", i+1, name, INTEGER(el)[0]);
	    break;
	case CPLXSXP:
	    cpl = COMPLEX(el)[0];
	    Rprintf("[%d] '%s' %f + %fi\n", i+1, name, cpl.r, cpl.i);
	    break;
	case STRSXP:
	    Rprintf("[%d] '%s' %s\n", i+1, name,
		    CHAR(STRING_ELT(el, 0)));
	    break;
	default:
	    Rprintf("[%d] '%s' R type\n", i+1, name);
	}
    }
    return(R_NilValue);
}

SEXP showArgs1(SEXP largs)
{
    int i, nargs = LENGTH(largs);
    Rcomplex cpl;
    SEXP el, names = getAttrib(largs, R_NamesSymbol);
    char *name;

    for(i = 0; i < nargs; i++) {
	el = VECTOR_ELT(largs, i);
	name = isNull(names) ? "" : CHAR(STRING_ELT(names, i));
	switch(TYPEOF(el)) {
	case REALSXP:
	    Rprintf("[%d] '%s' %f\n", i+1, name, REAL(el)[0]);
	    break;
	case LGLSXP:
	case INTSXP:
	    Rprintf("[%d] '%s' %d\n", i+1, name, INTEGER(el)[0]);
	    break;
	case CPLXSXP:
	    cpl = COMPLEX(el)[0];
	    Rprintf("[%d] '%s' %f + %fi\n", i+1, name, cpl.r, cpl.i);
	    break;
	case STRSXP:
	    Rprintf("[%d] '%s' %s\n", i+1, name,
		    CHAR(STRING_ELT(el, 0)));
	    break;
	default:
	    Rprintf("[%d] '%s' R type\n", i+1, name);
	}
    }
    return(R_NilValue);
}

/* ----- Skeleton lapply ----- */

SEXP lapply(SEXP list, SEXP expr, SEXP rho)
{
    int i, n = length(list);
    SEXP ans;

    if(!isNewList(list)) error("`list' must be a list");
    if(!isEnvironment(rho)) error("`rho' should be an environment");
    PROTECT(ans = allocVector(VECSXP, n));
    for(i = 0; i < n; i++) {
	defineVar(install("x"), VECTOR_ELT(list, i), rho);
	SET_VECTOR_ELT(ans, i, eval(expr, rho));
    }
    setAttrib(ans, R_NamesSymbol, getAttrib(list, R_NamesSymbol));
    UNPROTECT(1);
    return(ans);
}

SEXP lapply2(SEXP list, SEXP fn, SEXP rho)
{
    int i, n = length(list);
    SEXP R_fcall, ans;

    if(!isNewList(list)) error("`list' must be a list");
    if(!isFunction(fn)) error("`fn' must be a function");
    if(!isEnvironment(rho)) error("`rho' should be an environment");
    PROTECT(R_fcall = lang2(fn, R_NilValue));
    PROTECT(ans = allocVector(VECSXP, n));
    for(i = 0; i < n; i++) {
	SETCADR(R_fcall, VECTOR_ELT(list, i));
	SET_VECTOR_ELT(ans, i, eval(R_fcall, rho));
    }
    setAttrib(ans, R_NamesSymbol, getAttrib(list, R_NamesSymbol));
    UNPROTECT(2);
    return(ans);
}

/* ----- Zero-finding ----- */

SEXP mkans(double x)
{
    SEXP ans;
    PROTECT(ans = allocVector(REALSXP, 1));
    REAL(ans)[0] = x;
    UNPROTECT(1);
    return ans;
}

double feval(double x, SEXP f, SEXP rho)
{
    defineVar(install("x"), mkans(x), rho);
    return(REAL(eval(f, rho))[0]);
}

SEXP zero(SEXP f, SEXP guesses, SEXP stol, SEXP rho)
{
    double x0 = REAL(guesses)[0], x1 = REAL(guesses)[1],
           tol = REAL(stol)[0];
    double f0, f1, fc, xc;

    if(tol <= 0.0) error("non-positive tol value");
    f0 = feval(x0, f, rho); f1 = feval(x1, f, rho);
    if(f0 == 0.0) return mkans(x0);
    if(f1 == 0.0) return mkans(x1);
    if(f0*f1 > 0.0) error("x[0] and x[1] have the same sign");
    for(;;) {
        xc = 0.5*(x0+x1);
        if(fabs(x0-x1) < tol) return  mkans(xc);
        fc = feval(xc, f, rho);
        if(fc == 0) return  mkans(xc);
        if(f0*fc > 0.0) {
            x0 = xc; f0 = fc;
        } else {
            x1 = xc; f1 = fc;
        }
    }
}


/* ----- Calculating numerical derivatives example ----- */

#include <R.h> /* for DOUBLE_EPS */
#include <Rinternals.h>

SEXP numeric_deriv(SEXP args)
{
    SEXP theta, expr, rho, ans, ans1, gradient, par, dimnames;
    double tt, xx, delta, eps = sqrt(DOUBLE_EPS), *rgr, *rans;
    int start, i, j;

    expr = CADR(args);
    if(!isString(theta = CADDR(args)))
	error("theta should be of type character");
    if(!isEnvironment(rho = CADDDR(args)))
	error("rho should be an environment");

    PROTECT(ans = coerceVector(eval(expr, rho), REALSXP));
    PROTECT(gradient = allocMatrix(REALSXP, LENGTH(ans), LENGTH(theta)));
    rgr = REAL(gradient); rans = REAL(ans);

    for(i = 0, start = 0; i < LENGTH(theta); i++, start += LENGTH(ans)) {
	PROTECT(par = findVar(install(CHAR(STRING_ELT(theta, i))), rho));
	tt = REAL(par)[0];
	xx = fabs(tt);
	delta = (xx < 1) ? eps : xx*eps;
	REAL(par)[0] += delta;
	PROTECT(ans1 = coerceVector(eval(expr, rho), REALSXP));
	for(j = 0; j < LENGTH(ans); j++)
            rgr[j + start] = (REAL(ans1)[j] - rans[j])/delta;
	REAL(par)[0] = tt;
	UNPROTECT(2); /* par, ans1 */
    }

    PROTECT(dimnames = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 1,  theta);
    dimnamesgets(gradient, dimnames);
    setAttrib(ans, install("gradient"), gradient);
    UNPROTECT(3); /* ans  gradient  dimnames */
    return ans;
}
