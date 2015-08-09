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
 *  https://www.R-project.org/Licenses/
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

static void fillWithNAs(SEXP x, R_xlen_t n, SEXPTYPE type) {
    R_xlen_t i;

    if (type == INTSXP) {
        for (i = 0; i < n; i++) {
            INTEGER(x)[i] = NA_INTEGER;
        }
    } else { /* REALSXP */
        for (i = 0; i < n; i++) {
            REAL(x)[i] = NA_REAL;
        }
    }
    warning(_("NAs produced"));
}

static R_INLINE R_xlen_t resultLength(SEXP lengthArgument) {
    R_xlen_t n;

    if (!isVector(lengthArgument)) {
	error(_("invalid arguments"));
    }
    if (XLENGTH(lengthArgument) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(lengthArgument);
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX) {
	    error(_("invalid arguments"));
        }
	n = (R_xlen_t) dn;
#else
	n = asInteger(lengthArgument);
	if (n == NA_INTEGER || n < 0) {
	    error(_("invalid arguments"));
        }
#endif
    } else {
        n = XLENGTH(lengthArgument);
    }

    return n;
}

/* random sampling from 1 parameter families. */

static R_INLINE SEXP random1(SEXP sn, SEXP sa, ran1 fn, SEXPTYPE type)
{
    SEXP x, a;
    R_xlen_t n, na;

    if (!isNumeric(sa)) {
	error(_("invalid arguments"));
    }
    n = resultLength(sn);
    PROTECT(x = allocVector(type, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(sa);
    if (na < 1) {
        fillWithNAs(x, n, type);
    } else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(sa, REALSXP));
	GetRNGstate();
	double *ra = REAL(a);
	errno = 0;
	if (type == INTSXP) {
	    double rx;
	    int *ix = INTEGER(x);
	    
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx = fn(ra[i % na]);
		if (ISNAN(rx) || rx > INT_MAX || rx <= INT_MIN ) {
		    naflag = TRUE;
		    ix[i] = NA_INTEGER;
		}
		else ix[i] = (int) rx;
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

#define DEFRAND1_REAL(name) \
    SEXP do_##name(SEXP sn, SEXP sa) { \
        return random1(sn, sa, name, REALSXP); \
    }

#define DEFRAND1_INT(name) \
    SEXP do_##name(SEXP sn, SEXP sa) { \
        return random1(sn, sa, name, INTSXP); \
    }

DEFRAND1_REAL(rchisq)
DEFRAND1_REAL(rexp)
DEFRAND1_INT(rgeom)
DEFRAND1_INT(rpois)
DEFRAND1_REAL(rt)
DEFRAND1_INT(rsignrank)

/* random sampling from 2 parameter families. */

static R_INLINE SEXP random2(SEXP sn, SEXP sa, SEXP sb, ran2 fn, SEXPTYPE type)
{
    SEXP x, a, b;
    R_xlen_t n, na, nb;

    if (!isNumeric(sa) || !isNumeric(sb)) {
	error(_("invalid arguments"));
    }
    n = resultLength(sn);
    PROTECT(x = allocVector(type, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(sa);
    nb = XLENGTH(sb);
    if (na < 1 || nb < 1) {
        fillWithNAs(x, n, type);
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(sa, REALSXP));
	PROTECT(b = coerceVector(sb, REALSXP));
	GetRNGstate();
	double *ra = REAL(a), *rb = REAL(b);
	if (type == INTSXP) {
	    int *ix = INTEGER(x); double rx;
	    errno = 0;
	    for (R_xlen_t i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx = fn(ra[i % na], rb[i % nb]);
		if (ISNAN(rx) || rx > INT_MAX || rx <= INT_MIN) {
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

#define DEFRAND2_REAL(name) \
    SEXP do_##name(SEXP sn, SEXP sa, SEXP sb) { \
        return random2(sn, sa, sb, name, REALSXP); \
    }

#define DEFRAND2_INT(name) \
    SEXP do_##name(SEXP sn, SEXP sa, SEXP sb) { \
        return random2(sn, sa, sb, name, INTSXP); \
    }

DEFRAND2_REAL(rbeta)
DEFRAND2_INT(rbinom)
DEFRAND2_REAL(rcauchy)
DEFRAND2_REAL(rf)
DEFRAND2_REAL(rgamma)
DEFRAND2_REAL(rlnorm)
DEFRAND2_REAL(rlogis)
DEFRAND2_INT(rnbinom)
DEFRAND2_REAL(rnorm)
DEFRAND2_REAL(runif)
DEFRAND2_REAL(rweibull)
DEFRAND2_INT(rwilcox)
DEFRAND2_REAL(rnchisq)
DEFRAND2_REAL(rnbinom_mu)

/* random sampling from 3 parameter families. */

static R_INLINE SEXP random3(SEXP sn, SEXP sa, SEXP sb, SEXP sc, ran3 fn,
			     SEXPTYPE type)
{
    SEXP x, a, b, c;
    R_xlen_t n, na, nb, nc;

    if (!isNumeric(sa) || !isNumeric(sb) || !isNumeric(sc)) {
	error(_("invalid arguments"));
    }
    n = resultLength(sn);
    PROTECT(x = allocVector(type, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(sa);
    nb = XLENGTH(sb);
    nc = XLENGTH(sc);
    if (na < 1 || nb < 1 || nc < 1) {
        fillWithNAs(x, n, type);
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(sa, REALSXP));
	PROTECT(b = coerceVector(sb, REALSXP));
	PROTECT(c = coerceVector(sc, REALSXP));
	GetRNGstate();
	double *ra = REAL(a), *rb = REAL(b), *rc = REAL(c);
	errno = 0;
	if (type == INTSXP) {
	    int *ix = INTEGER(x); double rx;
	    errno = 0;
	    for (R_xlen_t i = 0; i < n; i++) {
//	        if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx = fn(ra[i % na], rb[i % nb], rc[i % nc]);
		if (ISNAN(rx) || rx > INT_MAX || rx <= INT_MIN) {
		    naflag = TRUE;
		    ix[i] = NA_INTEGER;
		} else ix[i] = (int) rx;
	    }
	} else {
	    double *rx = REAL(x);
	    errno = 0;
	    for (R_xlen_t i = 0; i < n; i++) {
//	        if ((i+1) % NINTERRUPT) R_CheckUserInterrupt();
		rx[i] = fn(ra[i % na], rb[i % nb], rc[i % nc]);
		if (ISNAN(rx[i])) naflag = TRUE;
	    }
	}
	if (naflag) warning(_("NAs produced"));
	PutRNGstate();
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return x;
}

#define DEFRAND3_REAL(name) \
    SEXP do_##name(SEXP sn, SEXP sa, SEXP sb, SEXP sc) { \
        return random3(sn, sa, sb, sc, name, REALSXP); \
    }

#define DEFRAND3_INT(name) \
    SEXP do_##name(SEXP sn, SEXP sa, SEXP sb, SEXP sc) { \
        return random3(sn, sa, sb, sc, name, INTSXP); \
    }

DEFRAND3_INT(rhyper)

static void FixupProb(double *p, int n)
{
    double sum = 0.0;
    int npos = 0;
    for (int i = 0; i < n; i++) {
	if (!R_FINITE(p[i]))
	    error(_("NA in probability vector"));
	if (p[i] < 0.0)
	    error(_("negative probability"));
	if (p[i] > 0.0) {
	    npos++;
	    sum += p[i];
	}
    }
    if (npos == 0) error(_("no positive probabilities"));
    for (int i = 0; i < n; i++) p[i] /= sum;
}

SEXP do_rmultinom(SEXP sn, SEXP ssize, SEXP prob)
{
    SEXP ans, nms;
    int n, size, k, i, ik;
    
    n	 = asInteger(sn);/* n= #{samples} */
    size = asInteger(ssize);/* X ~ Multi(size, prob) */
    if (n == NA_INTEGER || n < 0)
	error(_("invalid first argument 'n'"));
    if (size == NA_INTEGER || size < 0)
	error(_("invalid second argument 'size'"));
    prob = coerceVector(prob, REALSXP);
    k = length(prob);/* k = #{components or classes} = X-vector length */
    if (MAYBE_REFERENCED(prob)) prob = duplicate(prob);/*as `do_sample' -- need this line? */
    PROTECT(prob);
    /* check and make sum = 1: */
    FixupProb(REAL(prob), k);
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
