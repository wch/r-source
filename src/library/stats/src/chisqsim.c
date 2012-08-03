/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-5   The R Core Team.
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

#include <Rmath.h>
#include <R_ext/Random.h>
#include <R_ext/Applic.h>

#include "ctest.h"

/* Driver routine to call RCONT2 from R, B times.
   Calculates the Pearson chi-squared for each generated table.

   Mostly here for historical reasons now that we have r2dtable().
*/

void
chisqsim(int *nrow, int *ncol, int *nrowt, int *ncolt, int *n,
	 int *b, double *expected, int *observed, double *fact,
	 int *jwork, double *results)
{
    int i, j, ii, iter;
    double chisq, e, o;

    /* Calculate log-factorials.  fact[i] = lgamma(i+1) */
    fact[0] = fact[1] = 0.;
    for(i = 2; i <= *n; i++)
	fact[i] = fact[i - 1] + log(i);

    GetRNGstate();

    for(iter = 0; iter < *b; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate chi-squared value from the random table. */
	chisq = 0.;
	for (j = 0; j < *ncol; ++j) {
	    for (i = 0, ii = j * *nrow; i < *nrow;  i++, ii++) {
		e = expected[ii];
		o = observed[ii];
		chisq += (o - e) * (o - e) / e;
	    }
	}
	results[iter] = chisq;
    }

    PutRNGstate();

    return;
}

/* Driver routine to call RCONT2 from R, B times.
   Calculates the log probability for each generated table.

   Mostly here for historical reasons now that we have r2dtable().
*/

void
fisher_sim(int *nrow, int *ncol, int *nrowt, int *ncolt, int *n,
	   int *b, int *observed, double *fact,
	   int *jwork, double *results)
{
    int i, j, ii, iter;
    double ans;

    /* Calculate log-factorials.  fact[i] = lgamma(i+1) */
    fact[0] = fact[1] = 0.;
    for(i = 2; i <= *n; i++)
	fact[i] = fact[i - 1] + log(i);

    GetRNGstate();

    for(iter = 0; iter < *b; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate log-prob value from the random table. */
	ans = 0.;
	for (j = 0; j < *ncol; ++j) {
	    for (i = 0, ii = j * *nrow; i < *nrow;  i++, ii++) {
		ans -= fact[observed[ii]];
	    }
	}
	results[iter] = ans;
    }

    PutRNGstate();

    return;
}
