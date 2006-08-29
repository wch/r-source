/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003--2006  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Applic.h>	/* for rcont2() */
#include <Rmath.h>		/* for lgammafn */
#include <R_ext/Error.h>
#include "stats.h"

SEXP R_r2dtable(SEXP n, SEXP r, SEXP c)
{
    int nr, nc, *row_sums, *col_sums, i, *jwork;
    int n_of_samples, n_of_cases;
    double *fact;
    SEXP ans, tmp;

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

    return(ans);
}
