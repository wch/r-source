/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998  Robert Gentleman, Ross Ihaka and the R Core team.
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

extern int ix_seed;
extern int iy_seed;
extern int iz_seed;

static int naflag = 0;

static void Randomize()
{
	srand((int)time(NULL));
	ix_seed = abs(rand() % 30269);
	iy_seed = abs(rand() % 30307);
	iz_seed = abs(rand() % 30323);
}

static void GetSeeds()
{
	SEXP seeds;
	seeds = findVar(R_SeedsSymbol, R_GlobalEnv);
	if (seeds == R_UnboundValue) {
		Randomize();
	}
	else {
		if (seeds == R_MissingArg)
			error(".Random.seed is a missing argument with no default\n");
		if (!isVector(seeds) || LENGTH(seeds) < 3)
			error("missing or invalid random number seeds\n");
		seeds = coerceVector(seeds, INTSXP);
		ix_seed = INTEGER(seeds)[0]; if(!ix_seed) ix_seed++;
		iy_seed = INTEGER(seeds)[1]; if(!iy_seed) iy_seed++;
		iz_seed = INTEGER(seeds)[2]; if(!iz_seed) iz_seed++;
	}
}

static void PutSeeds()
{
	SEXP seeds;
	PROTECT(seeds = allocVector(INTSXP, 3));
	INTEGER(seeds)[0] = ix_seed;
	INTEGER(seeds)[1] = iy_seed;
	INTEGER(seeds)[2] = iz_seed;
	setVar(R_SeedsSymbol, seeds, R_GlobalEnv);
	UNPROTECT(1);
}

static void invalid(SEXP call)
{
	errorcall(call, "invalid arguments\n");
}

static void random1(double (*f) (), double * a, int na, double * x, int n)
{
	double ai;
	int i;

	errno = 0;
	for (i = 0; i < n; i++) {
		ai = a[i % na];
		if (FINITE(ai)) {
			x[i] = MATH_CHECK(f(ai));
			if(!FINITE(x[i])) naflag = 1;
		}
		else x[i] = NA_REAL;
	}
}

#define RAND1(num,name) \
	case num: \
		random1(name, REAL(a), na, REAL(x), n); \
		break

/**********************************************************/
/* do_random1 - random sampling from 1 parameter families */
/* see switch below for distributions.			  */
/**********************************************************/

SEXP do_random1(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, a;
	int i, n, na;

	checkArity(op, args);

	if(!isVector(CAR(args)) || !isNumeric(CADR(args)))
		invalid(call);

	if(LENGTH(CAR(args)) == 1) {
		n = asInteger(CAR(args));
		if (n == NA_INTEGER || n < 0)
			invalid(call);
	}
	else n = LENGTH(CAR(args));

	PROTECT(x = allocVector(REALSXP, n));

	if( n == 0 ) {
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
		GetSeeds();
		switch (PRIMVAL(op)) {
			RAND1(0, rchisq);
			RAND1(1, rexp);
			RAND1(2, rgeom);
			RAND1(3, rpois);
			RAND1(4, rt);
		default:
			error("internal error in do_random1\n");
		}
		if (naflag)
			warning("NAs produced in function \"%s\"\n", PRIMNAME(op));

		PutSeeds();
		UNPROTECT(1);
	}
	UNPROTECT(1);
	return x;
}

static void random2(double (*f) (), double * a, int na, double * b, int nb, double * x, int n)
{
	double ai, bi; int i;

	errno = 0;
	for (i = 0; i < n; i++) {
		ai = a[i % na];
		bi = b[i % nb];
		if (FINITE(ai) && FINITE(bi)) {
			x[i] = MATH_CHECK(f(ai, bi));
			if(!FINITE(x[i])) naflag = 1;
		}
		else x[i] = NA_REAL;
	}
}

#define RAND2(num,name) \
	case num: \
		random2(name, REAL(a), na, REAL(b), nb, REAL(x), n); \
		break

/**********************************************************/
/* do_random2 - random sampling from 2 parameter families */
/* see switch below for distributions.			  */
/**********************************************************/

SEXP do_random2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, a, b;
	int i, n, na, nb;

	checkArity(op, args);

	if (!isVector(CAR(args)) ||
	    !isNumeric(CADR(args)) ||
	    !isNumeric(CADDR(args)))
		invalid(call);

	if(LENGTH(CAR(args)) == 1) {
		n = asInteger(CAR(args));
		if (n == NA_INTEGER || n < 0)
			invalid(call);
	}
	else n = LENGTH(CAR(args));

	PROTECT(x = allocVector(REALSXP, n));

	if( n == 0 ) {
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
		GetSeeds();
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
			warning("NAs produced in function \"%s\"\n", PRIMNAME(op));

		PutSeeds();
		UNPROTECT(2);
	}
	UNPROTECT(1);
	return x;
}

static void random3(double (*f) (), double * a, int na, double * b, int nb, double * c, int nc, double * x, int n)
{
	double ai, bi, ci;
	int i;

	errno = 0;
	for (i = 0; i < n; i++) {
		ai = a[i % na];
		bi = b[i % nb];
		ci = c[i % nc];	 /* not i % nb */
		if (FINITE(ai) && FINITE(bi) && FINITE(ci)) {
			x[i] = MATH_CHECK(f(ai, bi, ci));
			if(!FINITE(x[i])) naflag = 1;
		}
		else x[i] = NA_REAL;
	}
}

#define RAND3(num,name) \
	case num: \
		random3(name, REAL(a), na, REAL(b), nb, REAL(c), nc, REAL(x), n); \
		break

/**********************************************************/
/* do_random3 - random sampling from 3 parameter families */
/* see switch below for distributions.			  */
/**********************************************************/

SEXP do_random3(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, a, b, c;
	int i, n, na, nb, nc;

	checkArity(op, args);

	if (!isVector(CAR(args))) invalid(call);

	if(LENGTH(CAR(args)) == 1) {
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
	if(!isNumeric(a) || !isNumeric(b) || !isNumeric(c))
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
		GetSeeds();
		switch (PRIMVAL(op)) {
			RAND3(0, rhyper);
		default:
			error("internal error in do_random2\n");
		}
		if (naflag)
			warning("NAs produced in function \"%s\"\n", PRIMNAME(op));

		PutSeeds();
		UNPROTECT(3);
	}
	UNPROTECT(1);
	return x;
}

/*******************************************************************/
/* do_sample - equal probability sampling with/without replacement */
/* Implements sample(n, k, r) - choose k elements from 1 to n	   */
/* with/without replacement according to r.			   */
/*******************************************************************/

SEXP do_sample(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, y;
	int i, j, k, n, r;
	checkArity(op, args);

	n = asInteger(CAR(args));
	k = asInteger(CADR(args));
	r = asLogical(CADDR(args));

	if (r == NA_LOGICAL)
		errorcall(call, "invalid third argument\n");
	if (n == NA_INTEGER || n < 1)
		errorcall(call, "invalid first argument\n");
	if (k == NA_INTEGER || k < 0)
		errorcall(call, "invalid second argument\n");
	if (!r && k > n)
		errorcall(call, "can't take a sample larger than the population\n when replace = FALSE\n");

	GetSeeds();
	PROTECT(y = allocVector(INTSXP, k));
	if (r) {
		for (i = 0; i < k; i++)
			INTEGER(y)[i] = n * sunif() + 1;
	}
	else {
		x = allocVector(INTSXP, n);
		for (i = 0; i < n; i++)
			INTEGER(x)[i] = i;

		for (i = 0; i < k; i++) {
			j = n * sunif();
			INTEGER(y)[i] = INTEGER(x)[j] + 1;
			INTEGER(x)[j] = INTEGER(x)[--n];
		}
	}
	PutSeeds();
	UNPROTECT(1);
	return y;
}


/* S COMPATIBILITY */

/* The following entry points provide compatibility with S. */
/* These entry points should not be used by new R code. */

void seed_in(long *ignored)
{
	GetSeeds();
}

void seed_out()
{
	PutSeeds();
}

/*
double unif_rand(void)
{
	sunif();
}

double norm_rand(void)
{
	snorm();
}
*/
