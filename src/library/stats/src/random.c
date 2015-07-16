/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
 *  Copyright (C) 2003--2008  The R Foundation
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
# include <config.h>
#endif

#include <Defn.h>
#include <R_ext/Random.h>
#include <Rmath.h>		/* for lgammafn, rmultinom */
#include <errno.h>
#include "statsR.h"
#undef _
#include "stats.h" // for rcont2

/* interval at which to check interrupts */
#define NINTERRUPT 1000000

typedef double (*ran1) (double);
typedef double (*ran2) (double, double);
typedef double (*ran3) (double, double, double);

/* random sampling from 1 parameter families. */

SEXP Random1(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    SEXP x, a;
    R_xlen_t i, n, na;
    ran1 fn = NULL; /* -Wall */
    const char *dn = CHAR(STRING_ELT(getListElement(CAR(args), "name"), 0));
    SEXPTYPE type = REALSXP;

    if (streql(dn, "rchisq")) fn = &rchisq;
    else if (streql(dn, "rexp")) fn = &rexp;
    else if (streql(dn, "rgeom")) {
	type = INTSXP;
	fn = &rgeom;
    } else if (streql(dn, "rpois")) {
	type = INTSXP;
	fn = &rpois;
    }
    else if (streql(dn, "rt")) fn = &rt;
    else if (streql(dn, "rsignrank")) {
	type = INTSXP;
	fn = &rsignrank;
    }
    else error(_("invalid arguments"));

    args = CDR(args);
    if (!isVector(CAR(args)) || !isNumeric(CADR(args)))
	error(_("invalid arguments"));
    if (XLENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    error(_("invalid arguments"));
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    error(_("invalid arguments"));
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(type, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(CADR(args));
    if (na < 1) {
	if (type == INTSXP)
	    for (i = 0; i < n; i++) INTEGER(x)[i] = NA_INTEGER;
	else
	    for (i = 0; i < n; i++) REAL(x)[i] = NA_REAL;
	warning(_("NAs produced"));
    } else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	GetRNGstate();
	double *ra = REAL(a);
	errno = 0;
	if (type == INTSXP) {
	    double rx;
	    int *ix = INTEGER(x);
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx = fn(ra[i % na]);
		if (ISNAN(rx)) {
		    naflag = TRUE;
		    ix[i] = NA_INTEGER;
		} else ix[i] = (int) rx;
	    }
	} else {
	    double *rx = REAL(x);
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx[i] = fn(ra[i % na]);
		if (ISNAN(rx[i])) naflag = TRUE;
	    }
	}
	if (naflag) warning(_("NAs produced"));
	PutRNGstate();
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return x;
}

/* random sampling from 2 parameter families. */

SEXP Random2(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    SEXP x, a, b;
    R_xlen_t i, n, na, nb;
    ran2 fn = NULL; /* -Wall */
    const char *dn = CHAR(STRING_ELT(getListElement(CAR(args), "name"), 0));
    SEXPTYPE type = REALSXP;

    if (streql(dn, "rbeta")) fn = &rbeta;
    else if (streql(dn, "rbinom")) {
	type = INTSXP;
	fn = &rbinom;
    } else if (streql(dn, "rcauchy")) fn = &rcauchy;
    else if (streql(dn, "rf")) fn = &rf;
    else if (streql(dn, "rgamma")) fn = &rgamma;
    else if (streql(dn, "rlnorm")) fn = &rlnorm;
    else if (streql(dn, "rlogis")) fn = &rlogis;
    else if (streql(dn, "rnbinom")) {
	type = INTSXP;
	fn = &rnbinom;
    } else if (streql(dn, "rnorm")) fn = &rnorm;
    else if (streql(dn, "runif")) fn = &runif;
    else if (streql(dn, "rweibull")) fn = &rweibull;
    else if (streql(dn, "rwilcox")) {
	type = INTSXP;
	fn = &rwilcox;
    } else if (streql(dn, "rnchisq")) fn = &rnchisq;
    else if (streql(dn, "rnbinom_mu")) {
	fn = &rnbinom_mu;
    } else error(_("invalid arguments"));

    args = CDR(args);
    if (!isVector(CAR(args)) ||
	!isNumeric(CADR(args)) ||
	!isNumeric(CADDR(args)))
	error(_("invalid arguments"));
    if (XLENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    error(_("invalid arguments"));
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    error(_("invalid arguments"));
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(type, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(CADR(args));
    nb = XLENGTH(CADDR(args));
    if (na < 1 || nb < 1) {
	if (type == INTSXP)
	    for (i = 0; i < n; i++) INTEGER(x)[i] = NA_INTEGER;
	else
	    for (i = 0; i < n; i++) REAL(x)[i] = NA_REAL;
	for (i = 0; i < n; i++) REAL(x)[i] = NA_REAL;
	warning(_("NAs produced"));
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	PROTECT(b = coerceVector(CADDR(args), REALSXP));
	GetRNGstate();
	double *ra = REAL(a), *rb = REAL(b);
	if (type == INTSXP) {
	    int *ix = INTEGER(x); double rx;
	    errno = 0;
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx = fn(ra[i % na], rb[i % nb]);
		if (ISNAN(rx)) {
		    naflag = TRUE;
		    ix[i] = NA_INTEGER;
		} else ix[i] = (int) rx;
	    }
	} else {
	    double *rx = REAL(x);
	    errno = 0;
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx[i] = fn(ra[i % na], rb[i % nb]);
		if (ISNAN(rx[i])) naflag = TRUE;
	    }
	}
	if (naflag) warning(_("NAs produced"));
	PutRNGstate();
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return x;
}

/* random sampling from 3 parameter families. */

SEXP Random3(SEXP args)
{
    if (!isVectorList(CAR(args))) error("incorrect usage");
    SEXP x, a, b, c;
    R_xlen_t i, n, na, nb, nc;
    ran3 fn = rhyper;  /* the only current example */

    args = CDR(args);
    if (!isVector(CAR(args))) error(_("invalid arguments"));
    if (LENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    error(_("invalid arguments"));
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    error(_("invalid arguments"));
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(INTSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }

    args = CDR(args); a = CAR(args);
    args = CDR(args); b = CAR(args);
    args = CDR(args); c = CAR(args);
    if (!isNumeric(a) || !isNumeric(b) || !isNumeric(c))
	error(_("invalid arguments"));
    na = XLENGTH(a);
    nb = XLENGTH(b);
    nc = XLENGTH(c);
    if (na < 1 || nb < 1 || nc < 1) {
	for (i = 0; i < n; i++) INTEGER(x)[i] = NA_INTEGER;
	warning(_("NAs produced"));
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(a, REALSXP));
	PROTECT(b = coerceVector(b, REALSXP));
	PROTECT(c = coerceVector(c, REALSXP));
	GetRNGstate();
	double *ra = REAL(a), *rb = REAL(b), *rc = REAL(c), rx;
	int *ix = INTEGER(x);
	errno = 0;
	for (R_xlen_t i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	    rx = fn(ra[i % na], rb[i % nb], rc[i % nc]);
	    if (ISNAN(rx)) {
		naflag = TRUE;
		ix[i] = NA_INTEGER;
	    } else ix[i] = (int) rx;
	}
	if (naflag) warning(_("NAs produced"));

	PutRNGstate();
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return x;
}


SEXP Rmultinom(SEXP args)
{
    SEXP prob, ans, nms;
    int n, size, k, i, ik;
    
    args = CDR(args);
    n	 = asInteger(CAR(args)); args = CDR(args);/* n= #{samples} */
    size = asInteger(CAR(args)); args = CDR(args);/* X ~ Multi(size, prob) */
    if (n == NA_INTEGER || n < 0)
	error(_("invalid first argument 'n'"));
    if (size == NA_INTEGER || size < 0)
	error(_("invalid second argument 'size'"));
    prob = CAR(args);
    prob = coerceVector(prob, REALSXP);
    k = length(prob);/* k = #{components or classes} = X-vector length */
    if (MAYBE_REFERENCED(prob)) prob = duplicate(prob);/*as `do_sample' -- need this line? */
    PROTECT(prob);
    /* check and make sum = 1: */
    FixupProb(REAL(prob), k, /*require_k = */ 0, TRUE);
    GetRNGstate();
    PROTECT(ans = allocMatrix(INTSXP, k, n));/* k x n : natural for columnwise store */
    for(i=ik = 0; i < n; i++, ik += k) {
//	if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
	rmultinom(size, REAL(prob), k, &INTEGER(ans)[ik]);
    }
    PutRNGstate();
    if(!isNull(nms = getAttrib(prob, R_NamesSymbol))) {
	SEXP dimnms;
	PROTECT(nms);
	PROTECT(dimnms = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(dimnms, 0, nms);
	setAttrib(ans, R_DimNamesSymbol, dimnms);
	UNPROTECT(2);
    }
    UNPROTECT(2);
    return ans;
}

SEXP r2dtable(SEXP n, SEXP r, SEXP c)
{
    int nr, nc, *row_sums, *col_sums, i, *jwork;
    int n_of_samples, n_of_cases;
    double *fact;
    SEXP ans, tmp;
    const void *vmax = vmaxget();

    nr = length(r);
    nc = length(c);

    /* Note that the R code in r2dtable() also checks for missing and
       negative values.
       Should maybe do the same here ...
    */
    if(!isInteger(n) || (length(n) == 0) ||
       !isInteger(r) || (nr <= 1) ||
       !isInteger(c) || (nc <= 1))
	error(_("invalid arguments"));

    n_of_samples = INTEGER(n)[0];
    row_sums = INTEGER(r);
    col_sums = INTEGER(c);

    /* Compute total number of cases as the sum of the row sums.
       Note that the R code in r2dtable() also checks whether this is
       the same as the sum of the col sums.
       Should maybe do the same here ...
    */
    n_of_cases = 0;
    jwork = row_sums;
    for(i = 0; i < nr; i++)
	n_of_cases += *jwork++;

    /* Log-factorials from 0 to n_of_cases.
       (I.e., lgamma(1), ..., lgamma(n_of_cases + 1).)
    */
    fact = (double *) R_alloc(n_of_cases + 1, sizeof(double));
    fact[0] = 0.;
    for(i = 1; i <= n_of_cases; i++)
	fact[i] = lgammafn((double) (i + 1));

    jwork = (int *) R_alloc(nc, sizeof(int));

    PROTECT(ans = allocVector(VECSXP, n_of_samples));

    GetRNGstate();

    for(i = 0; i < n_of_samples; i++) {
	PROTECT(tmp = allocMatrix(INTSXP, nr, nc));
	rcont2(&nr, &nc, row_sums, col_sums, &n_of_cases, fact,
	       jwork, INTEGER(tmp));
	SET_VECTOR_ELT(ans, i, tmp);
	UNPROTECT(1);
    }

    PutRNGstate();

    UNPROTECT(1);
    vmaxset(vmax);

    return(ans);
}
