/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
#include <config.h>
#endif

#include "Defn.h"

SEXP do_split(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, f, counts, vec;
    int i, j, k, nobs, nlevs, nfac;

    checkArity(op, args);

    x = CAR(args);
    f = CADR(args);
    if (!isVector(x))
	errorcall(call, "first argument must be a vector");
    if (!isFactor(f))
	errorcall(call, "second argument must be a factor");
    nlevs = nlevels(f);
    nfac = LENGTH(CADR(args));
    nobs = LENGTH(CAR(args));
    if (nobs <= 0)
	return R_NilValue;
    if (nfac <= 0)
	errorcall(call, "Group length is 0 but data length > 0");
    if (nobs != nfac)
	warningcall(call, "argument lengths differ");
    PROTECT(counts = allocVector(INTSXP, nlevs));
    for (i = 0; i < nlevs; i++)
	INTEGER(counts)[i] = 0;
    for (i = 0; i < nobs; i++) {
	j = INTEGER(f)[i % nfac];
	if (j != NA_INTEGER) {
	    INTEGER(counts)[j - 1] += 1;
	}
    }
    /* Allocate a generic vector to hold the results. */
    /* The i-th element will hold the split-out data */
    /* for the ith group. */
    PROTECT(vec = allocVector(VECSXP, nlevs));
    for (i = 0;  i< nlevs; i++) {
	SET_VECTOR_ELT(vec, i, allocVector(TYPEOF(x), INTEGER(counts)[i]));
	setAttrib(VECTOR_ELT(vec, i), R_LevelsSymbol,
		  getAttrib(x, R_LevelsSymbol));
    }
    for (i = 0; i < nlevs; i++)
	INTEGER(counts)[i] = 0;
    for (i = 0;  i< nobs; i++) {
	j = INTEGER(f)[i % nfac];
	if (j != NA_INTEGER) {
	    k = INTEGER(counts)[j - 1];
	    switch (TYPEOF(x)) {
	    case LGLSXP:
	    case INTSXP:
		INTEGER(VECTOR_ELT(vec, j - 1))[k] = INTEGER(x)[i];
		break;
	    case REALSXP:
		REAL(VECTOR_ELT(vec, j - 1))[k] = REAL(x)[i];
		break;
	    case CPLXSXP:
		COMPLEX(VECTOR_ELT(vec, j - 1))[k] = COMPLEX(x)[i];
		break;
	    case STRSXP:
		SET_STRING_ELT(VECTOR_ELT(vec, j - 1), k, STRING_ELT(x, i));
		break;
	    }
	    INTEGER(counts)[j - 1] += 1;
	}
    }
    /* Now transfer the results from the vector */
    /* into a dotted-pair list.  When structures */
    /* are full based on vectors this won't be needed. */
    setAttrib(vec, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
    UNPROTECT(2);
    return vec;
}
