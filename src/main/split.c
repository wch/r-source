/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copuright (C) 2006 The R Core Development Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

SEXP attribute_hidden do_split(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, f, counts, vec, nm, nmj;
    int i, j, k, nobs, nlevs, nfac;
    Rboolean have_names;

    checkArity(op, args);

    x = CAR(args);
    f = CADR(args);
    if (!isVector(x))
	error(_("first argument must be a vector"));
    if (!isFactor(f))
	error(_("second argument must be a factor"));
    nlevs = nlevels(f);
    nfac = LENGTH(CADR(args));
    nobs = LENGTH(CAR(args));
    if (nobs <= 0)
	return R_NilValue;
    if (nfac <= 0)
	error(_("Group length is 0 but data length > 0"));
    if (nobs % nfac != 0)
	warning(_("data length is not a multiple of split variable"));
    nm = getAttrib(x, R_NamesSymbol);
    have_names = nm != R_NilValue;
    PROTECT(counts = allocVector(INTSXP, nlevs));
    for (i = 0; i < nlevs; i++) INTEGER(counts)[i] = 0;
    for (i = 0; i < nobs; i++) {
	j = INTEGER(f)[i % nfac];
	if (j != NA_INTEGER) INTEGER(counts)[j - 1]++;
    }
    /* Allocate a generic vector to hold the results. */
    /* The i-th element will hold the split-out data */
    /* for the ith group. */
    PROTECT(vec = allocVector(VECSXP, nlevs));
    for (i = 0;  i < nlevs; i++) {
	SET_VECTOR_ELT(vec, i, allocVector(TYPEOF(x), INTEGER(counts)[i]));
	setAttrib(VECTOR_ELT(vec, i), R_LevelsSymbol,
		  getAttrib(x, R_LevelsSymbol));
	if(have_names)
	    setAttrib(VECTOR_ELT(vec, i), R_NamesSymbol,
		      allocVector(STRSXP, INTEGER(counts)[i]));
    }
    for (i = 0; i < nlevs; i++)
	INTEGER(counts)[i] = 0;
    for (i = 0;  i < nobs; i++) {
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
	    case VECSXP:
		SET_VECTOR_ELT(VECTOR_ELT(vec, j - 1), k, VECTOR_ELT(x, i));
		break;
	    case RAWSXP:
		RAW(VECTOR_ELT(vec, j - 1))[k] = RAW(x)[i];
		break;
	    default:
		UNIMPLEMENTED_TYPE("split", x);
	    }
	    if(have_names) {
		nmj = getAttrib(VECTOR_ELT(vec, j - 1), R_NamesSymbol);
		SET_STRING_ELT(nmj, k, STRING_ELT(nm, i));
	    }
	    INTEGER(counts)[j - 1] += 1;
	}
    }
    setAttrib(vec, R_NamesSymbol, getAttrib(f, R_LevelsSymbol));
    UNPROTECT(2);
    return vec;
}
