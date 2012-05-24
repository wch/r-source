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
#include <R_ext/Applic.h>	/* for rcont2() */
#include <Rmath.h>		/* for lgammafn, rmultinom */

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
    if (NAMED(prob)) prob = duplicate(prob);/*as `do_sample' -- need this line? */
    PROTECT(prob);
    /* check and make sum = 1: */
    FixupProb(REAL(prob), k, /*require_k = */ 0, TRUE);
    GetRNGstate();
    PROTECT(ans = allocMatrix(INTSXP, k, n));/* k x n : natural for columnwise store */
    for(i=ik = 0; i < n; i++, ik += k)
	rmultinom(size, REAL(prob), k, &INTEGER(ans)[ik]);
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
