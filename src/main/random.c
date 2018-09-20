/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2003--2018  The R Foundation
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

#include <R_ext/Itermacros.h>
#include <R_ext/Random.h>
#include <R_ext/RS.h>		/* for Calloc() */
#include <Rmath.h>		/* for rxxx functions */
#include <errno.h>

/* Code down to do_random3 (inclusive) can be removed once the byte
  compiler knows how to optimize to .External rather than .Internal */
#include <Internal.h>
static void NORET invalid(SEXP call)
{
    error(_("invalid arguments"));
}

static Rboolean
random1(double (*f) (double), double *a, R_xlen_t na, double *x, R_xlen_t n)
{
    Rboolean naflag = FALSE;
    double ai;
    R_xlen_t i, ia;
    errno = 0;
    MOD_ITERATE1(n, na, i, ia, {
	ai = a[ia];
	x[i] = f(ai);
	if (ISNAN(x[i])) naflag = TRUE;
    });
    return(naflag);
}

#define RAND1(num,name) \
	case num: \
		naflag = random1(name, REAL(a), na, REAL(x), n); \
		break


/* "do_random1" - random sampling from 1 parameter families. */
/* See switch below for distributions. */

SEXP attribute_hidden do_random1(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a;
    R_xlen_t i, n, na;
    checkArity(op, args);
    if (!isVector(CAR(args)) || !isNumeric(CADR(args)))
	invalid(call);
    if (XLENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    invalid(call);
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(REALSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(CADR(args));
    if (na < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
	warning(_("NAs produced"));
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	GetRNGstate();
	switch (PRIMVAL(op)) {
	    RAND1(0, rchisq);
	    RAND1(1, rexp);
	    RAND1(2, rgeom);
	    RAND1(3, rpois);
	    RAND1(4, rt);
	    RAND1(5, rsignrank);
	default:
	    error("internal error in do_random1");
	}
	if (naflag)
	    warning(_("NAs produced"));

	PutRNGstate();
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return x;
}

static Rboolean random2(double (*f) (double, double),
			double *a, R_xlen_t na, double *b, R_xlen_t nb,
			double *x, R_xlen_t n)
{
    double ai, bi;
    R_xlen_t i, ia, ib;
    Rboolean naflag = FALSE;
    errno = 0;
    MOD_ITERATE2(n, na, nb, i, ia, ib, {
	ai = a[ia];
	bi = b[ib];
	x[i] = f(ai, bi);
	if (ISNAN(x[i])) naflag = TRUE;
    });
    return(naflag);
}

#define RAND2(num,name) \
	case num: \
		naflag = random2(name, REAL(a), na, REAL(b), nb, REAL(x), n); \
		break

/* "do_random2" - random sampling from 2 parameter families. */
/* See switch below for distributions. */

SEXP attribute_hidden do_random2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a, b;
    R_xlen_t i, n, na, nb;
    checkArity(op, args);
    if (!isVector(CAR(args)) ||
	!isNumeric(CADR(args)) ||
	!isNumeric(CADDR(args)))
	invalid(call);
    if (XLENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    invalid(call);
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(REALSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = XLENGTH(CADR(args));
    nb = XLENGTH(CADDR(args));
    if (na < 1 || nb < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
	warning(_("NAs produced"));
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	PROTECT(b = coerceVector(CADDR(args), REALSXP));
	GetRNGstate();
	switch (PRIMVAL(op)) {
	    RAND2(0, rbeta);
	    RAND2(1, rbinom);
	    RAND2(2, rcauchy);
	    RAND2(3, rf);
	    RAND2(4, rgamma);
	    RAND2(5, rlnorm);
	    RAND2(6, rlogis);
	    RAND2(7, rnbinom);
	    RAND2(8, rnorm);
	    RAND2(9, runif);
	    RAND2(10, rweibull);
	    RAND2(11, rwilcox);
	    RAND2(12, rnchisq);
	    RAND2(13, rnbinom_mu);
	default:
	    error("internal error in do_random2");
	}
	if (naflag)
	    warning(_("NAs produced"));

	PutRNGstate();
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return x;
}

static Rboolean
random3(double (*f) (double, double, double), double *a,
	R_xlen_t na, double *b, R_xlen_t nb, double *c, R_xlen_t nc,
	double *x, R_xlen_t n)
{
    double ai, bi, ci;
    R_xlen_t i, ia, ib, ic;
    Rboolean naflag = FALSE;
    errno = 0;
    MOD_ITERATE3(n, na, nb, nc, i, ia, ib, ic, {
	ai = a[ia];
	bi = b[ib];
	ci = c[ic];
	x[i] = f(ai, bi, ci);
	if (ISNAN(x[i])) naflag = TRUE;
    });
    return(naflag);
}

#define RAND3(num,name) \
	case num: \
		naflag = random3(name, REAL(a), na, REAL(b), nb, REAL(c), nc, REAL(x), n); \
		break


/* "do_random3" - random sampling from 3 parameter families. */
/* See switch below for distributions. */

SEXP attribute_hidden do_random3(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a, b, c;
    R_xlen_t i, n, na, nb, nc;
    checkArity(op, args);
    if (!isVector(CAR(args))) invalid(call);
    if (LENGTH(CAR(args)) == 1) {
#ifdef LONG_VECTOR_SUPPORT
	double dn = asReal(CAR(args));
	if (ISNAN(dn) || dn < 0 || dn > R_XLEN_T_MAX)
	    invalid(call);
	n = (R_xlen_t) dn;
#else
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
#endif
    }
    else n = XLENGTH(CAR(args));
    PROTECT(x = allocVector(REALSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }

    args = CDR(args); a = CAR(args);
    args = CDR(args); b = CAR(args);
    args = CDR(args); c = CAR(args);
    if (!isNumeric(a) || !isNumeric(b) || !isNumeric(c))
	invalid(call);
    na = XLENGTH(a);
    nb = XLENGTH(b);
    nc = XLENGTH(c);
    if (na < 1 || nb < 1 || nc < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
	warning(_("NAs produced"));
    }
    else {
	Rboolean naflag = FALSE;
	PROTECT(a = coerceVector(a, REALSXP));
	PROTECT(b = coerceVector(b, REALSXP));
	PROTECT(c = coerceVector(c, REALSXP));
	GetRNGstate();
	switch (PRIMVAL(op)) {
	    RAND3(0, rhyper);
	default:
	    error("internal error in do_random3");
	}
	if (naflag)
	    warning(_("NAs produced"));

	PutRNGstate();
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return x;
}


/*
 *  Unequal Probability Sampling.
 *
 *  Modelled after Fortran code provided by:
 *    E. S. Venkatraman <venkat@biosta.mskcc.org>
 *  but with significant modifications in the
 *  "with replacement" case.
 */

/* Unequal probability sampling; with-replacement case */

static void ProbSampleReplace(int n, double *p, int *perm, int nans, int *ans)
{
    double rU;
    int i, j;
    int nm1 = n - 1;

    /* record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* sort the probabilities into descending order */
    revsort(p, perm, n);

    /* compute cumulative probabilities */
    for (i = 1 ; i < n; i++)
	p[i] += p[i - 1];

    /* compute the sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand();
	for (j = 0; j < nm1; j++) {
	    if (rU <= p[j])
		break;
	}
	ans[i] = perm[j];
    }
}

/* A  version using Walker's alias method, based on Alg 3.13B in
   Ripley (1987).
 */

#define SMALL 10000
static void
walker_ProbSampleReplace(int n, double *p, int *a, int nans, int *ans)
{
    double *q, rU;
    int i, j, k;
    int *HL, *H, *L;

    /* Create the alias tables.
       The idea is that for HL[0] ... L-1 label the entries with q < 1
       and L ... H[n-1] label those >= 1.
       By rounding error we could have q[i] < 1. or > 1. for all entries.
     */
    if(n <= SMALL) {
	R_CheckStack2(n *(sizeof(int) + sizeof(double)));
	/* might do this repeatedly, so speed matters */
	HL = (int *) alloca(n * sizeof(int));
	q = (double *) alloca(n * sizeof(double));
    } else {
	/* Slow enough anyway not to risk overflow */
	HL = Calloc(n, int);
	q = Calloc(n, double);
    }
    H = HL - 1; L = HL + n;
    for (i = 0; i < n; i++) {
	q[i] = p[i] * n;
	if (q[i] < 1.) *++H = i; else *--L = i;
    }
    if (H >= HL && L < HL + n) { /* So some q[i] are >= 1 and some < 1 */
	for (k = 0; k < n - 1; k++) {
	    i = HL[k];
	    j = *L;
	    a[i] = j;
	    q[j] += q[i] - 1;
	    if (q[j] < 1.) L++;
	    if(L >= HL + n) break; /* now all are >= 1 */
	}
    }
    for (i = 0; i < n; i++) q[i] += i;

    /* generate sample */
    for (i = 0; i < nans; i++) {
	rU = unif_rand() * n;
	k = (int) rU;
	ans[i] = (rU < q[k]) ? k+1 : a[k]+1;
    }
    if(n > SMALL) {
	Free(HL);
	Free(q);
    }
}


/* Unequal probability sampling; without-replacement case */

static void ProbSampleNoReplace(int n, double *p, int *perm,
				int nans, int *ans)
{
    double rT, mass, totalmass;
    int i, j, k, n1;

    /* Record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* Sort probabilities into descending order */
    /* Order element identities in parallel */
    revsort(p, perm, n);

    /* Compute the sample */
    totalmass = 1;
    for (i = 0, n1 = n-1; i < nans; i++, n1--) {
	rT = totalmass * unif_rand();
	mass = 0;
	for (j = 0; j < n1; j++) {
	    mass += p[j];
	    if (rT <= mass)
		break;
	}
	ans[i] = perm[j];
	totalmass -= p[j];
	for(k = j; k < n1; k++) {
	    p[k] = p[k + 1];
	    perm[k] = perm[k + 1];
	}
    }
}

static void FixupProb(double *p, int n, int require_k, Rboolean replace)
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
    if (npos == 0 || (!replace && require_k > npos))
	error(_("too few positive probabilities"));
    for (int i = 0; i < n; i++) p[i] /= sum;
}

/* do_sample - probability sampling with/without replacement.
   .Internal(sample(n, size, replace, prob))
*/
SEXP attribute_hidden do_sample(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, y, sn, sk, prob, sreplace;

    checkArity(op, args);
    sn = CAR(args); args = CDR(args);
    sk = CAR(args); args = CDR(args); /* size */
    sreplace = CAR(args); args = CDR(args);
    if(length(sreplace) != 1)
	 error(_("invalid '%s' argument"), "replace");
    int replace = asLogical(sreplace);
    prob = CAR(args);
    if (replace == NA_LOGICAL)
	error(_("invalid '%s' argument"), "replace");
    GetRNGstate();
    if (!isNull(prob)) {
	int n = asInteger(sn), k = asInteger(sk);
	if (n == NA_INTEGER || n < 0 || (k > 0 && n == 0))
	    error(_("invalid first argument"));
	if (k == NA_INTEGER || k < 0)
	    error(_("invalid '%s' argument"), "size");
	if (!replace && k > n)
	    error(_("cannot take a sample larger than the population when 'replace = FALSE'"));
	PROTECT(y = allocVector(INTSXP, k));
	prob = coerceVector(prob, REALSXP);
	if (MAYBE_REFERENCED(prob)) prob = duplicate(prob);
	PROTECT(prob);
	double *p = REAL(prob);
	if (length(prob) != n)
	    error(_("incorrect number of probabilities"));
	FixupProb(p, n, k, (Rboolean) replace);
	PROTECT(x = allocVector(INTSXP, n));
	if (replace) {
	    int i, nc = 0;
	    for (i = 0; i < n; i++) if(n * p[i] > 0.1) nc++;
	    if (nc > 200)
		walker_ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	    else
		ProbSampleReplace(n, p, INTEGER(x), k, INTEGER(y));
	} else
	    ProbSampleNoReplace(n, p, INTEGER(x), k, INTEGER(y));
	UNPROTECT(2);
    }
    else {  // uniform sampling
	double dn = asReal(sn);
	R_xlen_t k = asVecSize(sk);
	if (!R_FINITE(dn) || dn < 0 || dn > 4.5e15 || (k > 0 && dn == 0))
	    error(_("invalid first argument"));
	if (k < 0) error(_("invalid '%s' argument"), "size");
	if (!replace && k > dn)
	    error(_("cannot take a sample larger than the population when 'replace = FALSE'"));
	if (dn > INT_MAX || k > INT_MAX) {
	    PROTECT(y = allocVector(REALSXP, k));
	    if (replace) {
		double *ry = REAL(y);
		for (R_xlen_t i = 0; i < k; i++) ry[i] = R_unif_index(dn) + 1;
	    } else {
#ifdef LONG_VECTOR_SUPPORT
		R_xlen_t n = (R_xlen_t) dn;
		double *x = (double *)R_alloc(n, sizeof(double));
		double *ry = REAL(y);
		for (R_xlen_t i = 0; i < n; i++) x[i] = (double) i;
		for (R_xlen_t i = 0; i < k; i++) {
		    R_xlen_t j = (R_xlen_t) R_unif_index(n);
		    ry[i] = x[j] + 1;
		    x[j] = x[--n];
		}
#else
		error(_("n >= 2^31, replace = FALSE is only supported on 64-bit platforms"));
#endif
	    }
	} else {
	    int n = (int) dn;
	    PROTECT(y = allocVector(INTSXP, k));
	    int *iy = INTEGER(y);
	    /* avoid allocation for a single sample */
	    if (replace || k < 2) {
		for (int i = 0; i < k; i++) iy[i] = (int)(R_unif_index(n) + 1);
	    } else {
		int *x = (int *)R_alloc(n, sizeof(int));
		for (int i = 0; i < n; i++) x[i] = i;
		for (int i = 0; i < k; i++) {
		    int j = (int)(R_unif_index(n));
		    iy[i] = x[j] + 1;
		    x[j] = x[--n];
		}
	    }
	}
    }
    PutRNGstate();
    UNPROTECT(1);
    return y;
}
