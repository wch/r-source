/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2012  the R Core Team
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

#include <Rinternals.h>
#include <R_ext/Applic.h>

/* alled via .External(.) :*/
SEXP call_dqags(SEXP args);
SEXP call_dqagi(SEXP args);

typedef struct int_struct
{
    SEXP f;    /* function */
    SEXP env;  /* where to evaluate the calls */
} int_struct, *IntStruct;


/* This is *the* ``integr_fn f'' used when called from R : */
static void Rintfn(double *x, int n, void *ex)
{
    SEXP args, resultsxp, tmp;
    int i;
    IntStruct IS = (IntStruct) ex;

    PROTECT(args = allocVector(REALSXP, n));
    for(i = 0; i < n; i++) REAL(args)[i] = x[i];

    PROTECT(tmp = lang2(IS->f , args));
    PROTECT(resultsxp = eval(tmp, IS->env));

    if(length(resultsxp) != n)
	error("evaluation of function gave a result of wrong length");
    if(TYPEOF(resultsxp) == INTSXP) {
	resultsxp = coerceVector(resultsxp, REALSXP);
    } else if(TYPEOF(resultsxp) != REALSXP)
	error("evaluation of function gave a result of wrong type");
    for(i = 0; i < n; i++) {
	x[i] = REAL(resultsxp)[i];
	if(!R_FINITE(x[i]))
	    error("non-finite function value");
    }
    UNPROTECT(3);
    return;
}

SEXP call_dqags(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double lower, upper, epsabs, epsrel, result, abserr, *work;
    int neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    lower = asReal(CAR(args)); args = CDR(args);
    upper = asReal(CAR(args)); args = CDR(args);
    epsabs = asReal(CAR(args)); args = CDR(args);
    epsrel = asReal(CAR(args)); args = CDR(args);
    limit = asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc((size_t) limit, sizeof(int));
    work = (double *) R_alloc((size_t) lenw, sizeof(double));

    Rdqags(Rintfn, (void*)&is,
	   &lower, &upper, &epsabs, &epsrel, &result,
	   &abserr, &neval, &ier, &limit, &lenw, &last, iwork, work);

    PROTECT(ans = allocVector(VECSXP, 4));
    PROTECT(ansnames = allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, mkChar("value"));
    SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}

SEXP call_dqagi(SEXP args)
{
    int_struct is;
    SEXP ans, ansnames;
    double bound, epsabs, epsrel, result, abserr, *work;
    int inf, neval, ier, limit, lenw, last, *iwork;

    args = CDR(args);
    is.f = CAR(args); args = CDR(args);
    is.env = CAR(args); args = CDR(args);
    bound = asReal(CAR(args)); args = CDR(args);
    inf = asInteger(CAR(args)); args = CDR(args);
    epsabs = asReal(CAR(args)); args = CDR(args);
    epsrel = asReal(CAR(args)); args = CDR(args);
    limit = asInteger(CAR(args)); args = CDR(args);
    lenw = 4 * limit;
    iwork = (int *) R_alloc((size_t) limit, sizeof(int));
    work = (double *) R_alloc((size_t) lenw, sizeof(double));

    Rdqagi(Rintfn, (void*)&is, &bound,&inf,&epsabs,&epsrel,&result,
	   &abserr,&neval,&ier,&limit,&lenw,&last,iwork,work);

    PROTECT(ans = allocVector(VECSXP, 4));
    PROTECT(ansnames = allocVector(STRSXP, 4));
    SET_STRING_ELT(ansnames, 0, mkChar("value"));
    SET_VECTOR_ELT(ans, 0, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 0))[0] = result;
    SET_STRING_ELT(ansnames, 1, mkChar("abs.error"));
    SET_VECTOR_ELT(ans, 1, allocVector(REALSXP, 1));
    REAL(VECTOR_ELT(ans, 1))[0] = abserr;
    SET_STRING_ELT(ansnames, 2, mkChar("subdivisions"));
    SET_VECTOR_ELT(ans, 2, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 2))[0] = last;
    SET_STRING_ELT(ansnames, 3, mkChar("ierr"));
    SET_VECTOR_ELT(ans, 3, allocVector(INTSXP, 1));
    INTEGER(VECTOR_ELT(ans, 3))[0] = ier;
    setAttrib(ans, R_NamesSymbol, ansnames);
    UNPROTECT(2);
    return ans;
}
