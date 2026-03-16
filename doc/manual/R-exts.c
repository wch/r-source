/*
     C code of the .Call/.External examples in `Writing R extensions'
     Compile with R CMD SHLIB R-exts.c
     Then use the R code in R-exts.R
 */


/* ----- Calculating outer products example ----- */

#include <R.h>
#include <Rinternals.h>

/* second version */
SEXP out(SEXP x, SEXP y)
{
    int nx = Rf_length(x), ny = Rf_length(y);
    SEXP ans = PROTECT(Rf_allocMatrix(REALSXP, nx, ny));
    double *rx = REAL(x), *ry = REAL(y), *rans = REAL(ans);

    for(int i = 0; i < nx; i++) {
	double tmp = rx[i];
	for(int j = 0; j < ny; j++)
	    rans[i + nx*j] = tmp * ry[j];
    }

    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, Rf_getAttrib(x, R_NamesSymbol));
    SET_VECTOR_ELT(dimnames, 1, Rf_getAttrib(y, R_NamesSymbol));
    Rf_setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(2);
    return ans;
}

/* get the list element named str, or return NULL */

SEXP getListElement(SEXP list, const char *str)
{
    SEXP elmt = R_NilValue, names = Rf_getAttrib(list, R_NamesSymbol);

    for (int i = 0; i < Rf_length(list); i++)
	if(strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
	    /* ASCII only */
            elmt = VECTOR_ELT(list, i);
	    break;
	}
    return elmt;
}

SEXP getvar(SEXP name, SEXP rho)
{
    SEXP ans;

    if (!Rf_isString(name) || Rf_length(name) != 1)
        Rf_error("name is not a single string");
    if (!Rf_isEnvironment(rho))
        Rf_error("rho should be an environment");
    ans = R_getVar(Rf_installChar(STRING_ELT(name, 0)), rho, TRUE);
    if (TYPEOF(ans) != REALSXP || Rf_length(ans) == 0)
        Rf_error("value is not a numeric vector with at least one element");
    Rprintf("first value is %f\n", REAL(ans)[0]);
    return R_NilValue;
}

/* ----- Convolution via .Call  ----- */

#include <Rinternals.h>
SEXP convolve2(SEXP a, SEXP b)
{
    int na, nb, nab;
    double *xa, *xb, *xab;
    SEXP ab;

    a = PROTECT(Rf_coerceVector(a, REALSXP));
    b = PROTECT(Rf_coerceVector(b, REALSXP));
    na = Rf_length(a); nb = Rf_length(b); nab = na + nb - 1;
    ab = PROTECT(Rf_allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b); xab = REAL(ab);
    for(int i = 0; i < nab; i++) xab[i] = 0.0;
    for(int i = 0; i < na; i++)
        for(int j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return ab;
}

/* ----- Convolution via .External  ----- */

SEXP convolveE(SEXP args)
{
    int na, nb, nab;
    double *xa, *xb, *xab;
    SEXP a, b, ab;

    a = PROTECT(Rf_coerceVector(CADR(args), REALSXP));
    b = PROTECT(Rf_coerceVector(CADDR(args), REALSXP));
    na = Rf_length(a); nb = Rf_length(b); nab = na + nb - 1;
    ab = PROTECT(Rf_allocVector(REALSXP, nab));
    xa = REAL(a); xb = REAL(b); xab = REAL(ab);
    for(int i = 0; i < nab; i++) xab[i] = 0.0;
    for(int i = 0; i < na; i++)
	for(int j = 0; j < nb; j++) xab[i + j] += xa[i] * xb[j];
    UNPROTECT(3);
    return ab;
}

/* ----- Show arguments  ----- */

SEXP showArgs(SEXP args)
{
    args = CDR(args); /* skip 'name' */
    for(int i = 0; args != R_NilValue; i++, args = CDR(args)) {
        const char *name = 
            Rf_isNull(TAG(args)) ? "" : CHAR(PRINTNAME(TAG(args)));
	SEXP el = CAR(args);
	if (Rf_length(el) == 0) {
	    Rprintf("[%d] '%s' R type, length 0\n", i+1, name);
	    continue;
	}
	switch(TYPEOF(el)) {
	case REALSXP:
	    Rprintf("[%d] '%s' %f\n", i+1, name, REAL(el)[0]);
	    break;
	case LGLSXP:
	case INTSXP:
	    Rprintf("[%d] '%s' %d\n", i+1, name, INTEGER(el)[0]);
	    break;
	case CPLXSXP:
	{
	    Rcomplex cpl = COMPLEX(el)[0];
	    Rprintf("[%d] '%s' %f + %fi\n", i+1, name, cpl.r, cpl.i);
	}
	    break;
	case STRSXP:
	    Rprintf("[%d] '%s' %s\n", i+1, name,
		    CHAR(STRING_ELT(el, 0)));
	    break;
	default:
	    Rprintf("[%d] '%s' R type\n", i+1, name);
	}
    }
    return R_NilValue;
}

SEXP showArgs1(SEXP largs)
{
    int i, nargs = LENGTH(largs);
    Rcomplex cpl;
    SEXP el, names = Rf_getAttrib(largs, R_NamesSymbol);
    const char *name;

    for(i = 0; i < nargs; i++) {
	el = VECTOR_ELT(largs, i);
	name = Rf_isNull(names) ? "" : CHAR(STRING_ELT(names, i));
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
    return R_NilValue;
}

/* ----- Skeleton lapply ----- */

SEXP lapply(SEXP list, SEXP expr, SEXP rho)
{
    int n = Rf_length(list);
    SEXP ans;

    if(!Rf_isNewList(list)) Rf_error("'list' must be a list");
    if(!Rf_isEnvironment(rho)) Rf_error("'rho' should be an environment");
    ans = PROTECT(Rf_allocVector(VECSXP, n));
    for(int i = 0; i < n; i++) {
	Rf_defineVar(Rf_install("x"), VECTOR_ELT(list, i), rho);
	SET_VECTOR_ELT(ans, i, Rf_eval(expr, rho));
    }
    Rf_setAttrib(ans, R_NamesSymbol, Rf_getAttrib(list, R_NamesSymbol));
    UNPROTECT(1);
    return ans;
}

SEXP lapply2(SEXP list, SEXP fn, SEXP rho)
{
    int n = Rf_length(list);
    SEXP R_fcall, ans;

    if(!Rf_isNewList(list)) Rf_error("'list' must be a list");
    if(!Rf_isFunction(fn)) Rf_error("'fn' must be a function");
    if(!Rf_isEnvironment(rho)) Rf_error("'rho' should be an environment");
    R_fcall = PROTECT(Rf_lang2(fn, R_NilValue));
    ans = PROTECT(Rf_allocVector(VECSXP, n));
    for(int i = 0; i < n; i++) {
	SETCADR(R_fcall, VECTOR_ELT(list, i));
	SET_VECTOR_ELT(ans, i, Rf_eval(R_fcall, rho));
    }
    Rf_setAttrib(ans, R_NamesSymbol, Rf_getAttrib(list, R_NamesSymbol));
    UNPROTECT(2);
    return ans;
}

/* ----- Zero-finding ----- */

SEXP mkans(double x)
{
    SEXP ans;
    ans = PROTECT(Rf_allocVector(REALSXP, 1));
    REAL(ans)[0] = x;
    UNPROTECT(1);
    return ans;
}

double feval(double x, SEXP f, SEXP rho)
{
    Rf_defineVar(Rf_install("x"), mkans(x), rho);
    return REAL(Rf_eval(f, rho))[0];
}

SEXP zero(SEXP f, SEXP guesses, SEXP stol, SEXP rho)
{
    double x0 = REAL(guesses)[0], x1 = REAL(guesses)[1],
           tol = REAL(stol)[0];
    double f0, f1, fc, xc;

    if(tol <= 0.0) Rf_error("non-positive tol value");
    f0 = feval(x0, f, rho); f1 = feval(x1, f, rho);
    if(f0 == 0.0) return mkans(x0);
    if(f1 == 0.0) return mkans(x1);
    if(f0*f1 > 0.0) Rf_error("x[0] and x[1] have the same sign");
    for(;;) {
        xc = 0.5*(x0+x1);
        if(fabs(x0-x1) < tol) return mkans(xc);
        fc = feval(xc, f, rho);
        if(fc == 0) return mkans(xc);
        if(f0*fc > 0.0) {
            x0 = xc; f0 = fc;
        } else {
            x1 = xc; f1 = fc;
        }
    }
}


/* ----- Calculating numerical derivatives example ----- */

#include <R.h>
#include <Rinternals.h>
#include <float.h> /* for DBL_EPSILON */

SEXP numeric_deriv(SEXP args)
{
    SEXP theta, expr, rho, ans, ans1, gradient, par, dimnames;
    double tt, xx, delta, eps = sqrt(DBL_EPSILON), *rgr, *rans;
    int i, start;

    expr = CADR(args);
    if(!Rf_isString(theta = CADDR(args)))
	Rf_error("theta should be of type character");
    if(!Rf_isEnvironment(rho = CADDDR(args)))
	Rf_error("rho should be an environment");

    ans = PROTECT(Rf_coerceVector(Rf_eval(expr, rho), REALSXP));
    gradient = PROTECT(Rf_allocMatrix(REALSXP, LENGTH(ans), LENGTH(theta)));
    rgr = REAL(gradient); rans = REAL(ans);

    for(i = 0, start = 0; i < LENGTH(theta); i++, start += LENGTH(ans)) {
	par = PROTECT(R_getVar(Rf_installChar(STRING_ELT(theta, i)), rho, TRUE));
	tt = REAL(par)[0];
	xx = fabs(tt);
	delta = (xx < 1) ? eps : xx*eps;
	REAL(par)[0] += delta;
	ans1 = PROTECT(Rf_coerceVector(Rf_eval(expr, rho), REALSXP));
	for(int j = 0; j < LENGTH(ans); j++)
            rgr[j + start] = (REAL(ans1)[j] - rans[j])/delta;
	REAL(par)[0] = tt;
	UNPROTECT(2); /* par, ans1 */
    }

    dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 1,  theta);
    Rf_dimnamesgets(gradient, dimnames);
    Rf_setAttrib(ans, Rf_install("gradient"), gradient);
    UNPROTECT(3); /* ans  gradient  dimnames */
    return ans;
}
