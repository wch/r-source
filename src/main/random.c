/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the
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
#include "Random.h"
#include "Mathlib.h"

/* .Random.seed == (RNGkind, i1_seed, i_seed[0],i_seed[1],..,i_seed[n_seed-2])
 *		                      i2_seed   i3_seed
 * or           == (RNGkind)  [--> Randomize that one !]
 */

static void GetRNGstate()
{
  /* Get  .Random.seed  into proper variables */
    int len_seed, j, seed_off = 0;
    SEXP seeds;

    seeds = findVar(R_SeedsSymbol, R_GlobalEnv);
    if (seeds == R_UnboundValue) {
	Randomize(RNG_kind);
    }
    else {
	seeds = coerceVector(seeds, INTSXP);
	if (seeds == R_MissingArg)
	    error(".Random.seed is a missing argument with no default\n");
	if (!isVector(seeds))
	    error(".Random.seed is not a vector\n");
	RNG_kind = INTEGER(seeds)[0];
	if (RNG_kind > MERSENNE_TWISTER || RNG_kind < 0) 
		RNG_kind = WICHMANN_HILL; 
	len_seed = RNG_Table[RNG_kind].n_seed;
	if(LENGTH(seeds) > 1 && LENGTH(seeds) < len_seed + 1) {
	    if(LENGTH(seeds) == RNG_Table[WICHMANN_HILL].n_seed) {
		/* BACKWARDS COMPATIBILITY: */
		seed_off = 1;
		warning("Wrong length .Random.seed; forgot initial RNGkind? set to Wichmann-Hill");
		/* compatibility mode */
		RNG_kind = WICHMANN_HILL;
	    } else {
		error(".Random.seed has wrong length.\n");
	    }
	}

 	switch(RNG_kind) {
 	case WICHMANN_HILL:
 	case MARSAGLIA_MULTICARRY:
 	case SUPER_DUPER:
 	case RAND:
	    break;
 	case MERSENNE_TWISTER:
	    error("'Mersenne-Twister' not yet implemented\n"); break;
	default:
	    error(".Random.seed[1] is NOT a valid RNG kind (code)\n");
	}
	if(LENGTH(seeds) == 1)
	    Randomize(RNG_kind);
	else {
	    RNG_Table[RNG_kind].i1_seed = INTEGER(seeds)[1 - seed_off];
	    for(j = 2; j <= len_seed; j++)
		RNG_Table[RNG_kind].i_seed[j - 2] = INTEGER(seeds)[j - seed_off];
	    FixupSeeds(RNG_kind);
	}
    }
}

static void PutRNGstate()
{
    int len_seed, j;
    SEXP seeds;
    len_seed = RNG_Table[RNG_kind].n_seed;

    PROTECT(seeds = allocVector(INTSXP, len_seed + 1));

    INTEGER(seeds)[0] = RNG_kind;
    INTEGER(seeds)[1] = RNG_Table[RNG_kind].i1_seed;
    for(j = 2; j <= len_seed; j++)
	INTEGER(seeds)[j] = RNG_Table[RNG_kind].i_seed[j-2];

    setVar(R_SeedsSymbol, seeds, R_GlobalEnv);
    UNPROTECT(1);
}

static void RNGkind(RNGtype newkind)
{
/* Choose a new kind of RNG.
 * Initialize its seed by calling the old RNG's sunif()
 */
    GetRNGstate();

    RNG_Init(newkind, sunif() * UINT_MAX);

    switch(newkind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
      break;
    case RAND:
	error("RNGkind: \"Rand\" not yet available (BUG)!\n");
	srand((unsigned int)sunif()*UINT_MAX);
      break;
    case MERSENNE_TWISTER:
	/* ... */
	error("RNGkind: \"Mersenne-Twister\" not yet available!\n");
      break;
    default:
      error("RNGkind: unimplemented RNG kind %d\n", newkind);
    }
    RNG_kind = newkind;

    PutRNGstate();
}

SEXP do_RNGkind (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP r;
    RNGtype kind, oldkind;

    checkArity(op,args);
    oldkind = RNG_kind;
    r = CAR(args);
    if(length(r)) { /* set a new RNG kind */
      kind = asInteger(r);
      RNGkind(kind);
    }
    r = allocVector(INTSXP, 1);
    INTEGER(r)[0] = oldkind;
    return r;
}

/*------ Part without RNGkind dependency ------------------------*/

static int naflag = 0;

static void invalid(SEXP call)
{
    errorcall(call, "invalid arguments\n");
}

static void random1(double (*f) (), double *a, int na, double *x, int n)
{
    double ai;
    int i;
    errno = 0;
    for (i = 0; i < n; i++) {
	ai = a[i % na];
	if (R_FINITE(ai)) {
	    x[i] = MATH_CHECK(f(ai));
	    if (!R_FINITE(x[i])) naflag = 1;
	}
	else x[i] = NA_REAL;
    }
}

#define RAND1(num,name) \
	case num: \
		random1(name, REAL(a), na, REAL(x), n); \
		break


/* "do_random1" - random sampling from 1 parameter families. */
/* See switch below for distributions. */

SEXP do_random1(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a;
    int i, n, na;
    checkArity(op, args);
    if (!isVector(CAR(args)) || !isNumeric(CADR(args)))
	invalid(call);
    if (LENGTH(CAR(args)) == 1) {
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
    }
    else n = LENGTH(CAR(args));
    PROTECT(x = allocVector(REALSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = LENGTH(CADR(args));
    if (na < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
    }
    else {
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	naflag = 0;
	GetRNGstate();
	switch (PRIMVAL(op)) {
	    RAND1(0, rchisq);
	    RAND1(1, rexp);
	    RAND1(2, rgeom);
	    RAND1(3, rpois);
	    RAND1(4, rt);
	    RAND1(5, rsignrank);
	default:
	    error("internal error in do_random1\n");
	}
	if (naflag)
	    warningcall(call, "NAs produced");

	PutRNGstate();
	UNPROTECT(1);
    }
    UNPROTECT(1);
    return x;
}

static void random2(double (*f) (), double *a, int na, double *b, int nb,
		    double *x, int n)
{
    double ai, bi; int i;
    errno = 0;
    for (i = 0; i < n; i++) {
	ai = a[i % na];
	bi = b[i % nb];
	if (R_FINITE(ai) && R_FINITE(bi)) {
	    x[i] = MATH_CHECK(f(ai, bi));
	    if (!R_FINITE(x[i])) naflag = 1;
	}
	else x[i] = NA_REAL;
    }
}

#define RAND2(num,name) \
	case num: \
		random2(name, REAL(a), na, REAL(b), nb, REAL(x), n); \
		break

/* "do_random2" - random sampling from 2 parameter families. */
/* See switch below for distributions. */

SEXP do_random2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a, b;
    int i, n, na, nb;
    checkArity(op, args);
    if (!isVector(CAR(args)) ||
	!isNumeric(CADR(args)) ||
	!isNumeric(CADDR(args)))
	invalid(call);
    if (LENGTH(CAR(args)) == 1) {
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
    }
    else n = LENGTH(CAR(args));
    PROTECT(x = allocVector(REALSXP, n));
    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }
    na = LENGTH(CADR(args));
    nb = LENGTH(CADDR(args));
    if (na < 1 || nb < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
    }
    else {
	PROTECT(a = coerceVector(CADR(args), REALSXP));
	PROTECT(b = coerceVector(CADDR(args), REALSXP));
	naflag = 0;
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
	default:
	    error("internal error in do_random2\n");
	}
	if (naflag)
	    warningcall(call,"NAs produced");

	PutRNGstate();
	UNPROTECT(2);
    }
    UNPROTECT(1);
    return x;
}

static void random3(double (*f) (), double *a, int na, double *b, int nb,
		    double *c, int nc, double *x, int n)
{
    double ai, bi, ci;
    int i;
    errno = 0;
    for (i = 0; i < n; i++) {
	ai = a[i % na];
	bi = b[i % nb];
	ci = c[i % nc];
	if (R_FINITE(ai) && R_FINITE(bi) && R_FINITE(ci)) {
	    x[i] = MATH_CHECK(f(ai, bi, ci));
	    if (!R_FINITE(x[i])) naflag = 1;
	}
	else x[i] = NA_REAL;
    }
}

#define RAND3(num,name) \
	case num: \
		random3(name, REAL(a), na, REAL(b), nb, REAL(c), nc, REAL(x), n); \
		break


/* "do_random3" - random sampling from 3 parameter families. */
/* See switch below for distributions. */

SEXP do_random3(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, a, b, c;
    int i, n, na, nb, nc;
    checkArity(op, args);
    if (!isVector(CAR(args))) invalid(call);
    if (LENGTH(CAR(args)) == 1) {
	n = asInteger(CAR(args));
	if (n == NA_INTEGER || n < 0)
	    invalid(call);
    }
    else n = LENGTH(CAR(args));
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
    na = LENGTH(a);
    nb = LENGTH(b);
    nc = LENGTH(c);
    if (na < 1 || nb < 1 || nc < 1) {
	for (i = 0; i < n; i++)
	    REAL(x)[i] = NA_REAL;
    }
    else {
	PROTECT(a = coerceVector(a, REALSXP));
	PROTECT(b = coerceVector(b, REALSXP));
	PROTECT(c = coerceVector(c, REALSXP));
	naflag = 0;
	GetRNGstate();
	switch (PRIMVAL(op)) {
	    RAND3(0, rhyper);
	default:
	    error("internal error in do_random2\n");
	}
	if (naflag)
	    warningcall(call,"NAs produced");

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
    double random;
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
	random = sunif();
	for (j = 0; j < nm1; j++) {
	    if (random <= p[j])
	        break;
        }
	ans[i] = perm[j];
    }
}

/* Unequal probability sampling; without-replacement case */

static void ProbSampleNoReplace(int n, double *p, int *perm,
				int nans, int *ans)
{
    double random, mass, totalmass;
    int i, j, k, nm1;

    /* Record element identities */
    for (i = 0; i < n; i++)
	perm[i] = i + 1;

    /* Sort probabilities into descending order */
    /* Order element identities in parallel */
    revsort(p, perm, n);

    /* Compute the sample */
    totalmass = 1;
    nm1 = n - 1;
    for (i = 0; i < nans; i++) {
	random = totalmass * sunif();
	mass = 0;
	for (j = 0; j < nm1; j++) {
	    mass += p[j];
	    if (random <= mass)
	        break;
        }
        ans[i] = perm[j];
	totalmass -= p[j];
	for(k = j; k < nm1; k++) {
	    p[k] = p[k + 1];
	    perm[k] = perm[k + 1];
	}
	nm1 = nm1 - 1;
    }
}

/* Equal probability sampling; with-replacement case */

static void SampleReplace(int k, int n, int *y)
{
    int i;
    for (i = 0; i < k; i++)
	y[i] = n * sunif() + 1;
}

/* Equal probability sampling; without-replacement case */

static void SampleNoReplace(int k, int n, int *y, int *x)
{
    int i, j;
    for (i = 0; i < n; i++)
	x[i] = i;
    for (i = 0; i < k; i++) {
	j = n * sunif();
	y[i] = x[j] + 1;
	x[j] = x[--n];
    }
}

/* do_sample - equal probability sampling with/without replacement. */
/* Implements sample(n, k, r) - choose k elements from 1 to n */
/* with/without replacement according to r. */
static void FixupProb(SEXP call, double *p, int n, int k, int replace)
{
    double sum;
    int i, npos;
    npos = 0;
    sum = 0.;
    for (i = 0; i < n; i++) {
	if (!R_FINITE(p[i]))
	    errorcall(call, "NA in probability vector\n");
	if (p[i] < 0)
	    errorcall(call, "non-positive probability\n");
	if (p[i] > 0)
	    npos++;
	sum += p[i];
    }
    if (npos == 0 || (!replace && k > npos))
	errorcall(call, "insufficient positive probabilities\n");
    for (i = 0; i < n; i++)
	p[i] = p[i] / sum;
}

SEXP do_sample(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, y, prob;
    int k, n, replace;
    checkArity(op, args);
    n = asInteger(CAR(args)); args = CDR(args);
    k = asInteger(CAR(args)); args = CDR(args);
    replace = asLogical(CAR(args)); args = CDR(args);
    prob = CAR(args);
    if (replace == NA_LOGICAL)
	errorcall(call, "invalid third argument\n");
    if (n == NA_INTEGER || n < 1)
	errorcall(call, "invalid first argument\n");
    if (k == NA_INTEGER || k < 0)
	errorcall(call, "invalid second argument\n");
    if (!replace && k > n)
	errorcall(call, "can't take a sample larger than the population\n when replace = FALSE\n");
    GetRNGstate();
    PROTECT(y = allocVector(INTSXP, k));
    if (!isNull(prob)) {
	prob = coerceVector(prob, REALSXP);
	if (NAMED(prob)) prob = duplicate(prob);
	PROTECT(prob);
	if (length(prob) != n)
	    errorcall(call, "incorrect number of probabilities\n");
	FixupProb(call, REAL(prob), n, k, replace);
	PROTECT(x = allocVector(INTSXP, n));
	if (replace)
	    ProbSampleReplace(n, REAL(prob), INTEGER(x), k, INTEGER(y));
	else
	    ProbSampleNoReplace(n, REAL(prob), INTEGER(x), k, INTEGER(y));
	UNPROTECT(2);
    }
    else {
	if (replace) SampleReplace(k, n, INTEGER(y));
	else {
	    x = allocVector(INTSXP, n);
	    SampleNoReplace(k, n, INTEGER(y), INTEGER(x));
	}
    }
    PutRNGstate();
    UNPROTECT(1);
    return y;
}


/* S COMPATIBILITY */

/* The following entry points provide compatibility with S. */
/* These entry points should not be used by new R code. */

void seed_in(long *ignored)
{
    GetRNGstate();
}

void seed_out(long *ignored)
{
    PutRNGstate();
}
/* unif_rand == sunif , norm_rand == snorm   via 'define' in Mathlib.h */
