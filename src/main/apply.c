/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-4  the R Development Core Team
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

/* Code to handle lapply/apply */

/* .Internal(lapply(X, FUN)) */

SEXP do_lapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, ans, X, FUN, ind, tmp;
    int i, n;

    checkArity(op, args);
    X = CAR(args);
    FUN = CADR(args);
    if (!isSymbol(X) || !isSymbol(FUN))
	errorcall(call, "arguments must be symbolic");
    n = length(eval(X, rho));
    if (n == NA_INTEGER)
	errorcall(call, "invalid length");
    args = CDR(args);

    /* Build call: FUN(X[[<ind>]], ...) */

    /* Notice that it is OK to have one arg to LCONS do memory
       allocation and not PROTECT the result (LCONS does memory
       protection of its args internally), but not both of them,
       since the computation of one may destroy the other */


    ind = allocVector(INTSXP, 1);
    PROTECT(tmp = LCONS(R_Bracket2Symbol, LCONS(X, LCONS(ind, R_NilValue))));
    PROTECT(R_fcall = LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));

    PROTECT(ans = allocVector(VECSXP, n));
    for(i = 0; i < n; i++) {
	INTEGER(ind)[0] = i + 1;
	SET_VECTOR_ELT(ans, i, eval(R_fcall, rho));
    }
    UNPROTECT(3);
    return ans;
}

/* .Internal(apply(X, X1, FUN)) */
/* X is a matrix, and the last dimension is the one we want to 
   loop over */

SEXP do_apply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, X, Xd, X1, ans, FUN;
    int i, j, nr, nc, inr;

    checkArity(op, args);
    X = CAR(args); args = CDR(args);
    if(!isMatrix(X))
	errorcall(call, "First arg is not a matrix");
    Xd = getAttrib(X, R_DimSymbol);
    nr = INTEGER(Xd)[0];
    nc = INTEGER(Xd)[1];
    X1 = CAR(args); args = CDR(args);
    FUN = CAR(args);

    PROTECT(R_fcall = LCONS(FUN, LCONS(X1, LCONS(R_DotsSymbol, R_NilValue))));
    PROTECT(ans = allocVector(VECSXP, nc));
    PROTECT(X1 = allocVector(TYPEOF(X), nr));
    SETCADR(R_fcall, X1);
    for(i = 0; i < nc; i++) {
	switch(TYPEOF(X)) {
	case REALSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		REAL(X1)[j] = REAL(X)[j + inr];
	    break;
	case INTSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		INTEGER(X1)[j] = INTEGER(X)[j + inr];
	    break;
	case LGLSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		LOGICAL(X1)[j] = LOGICAL(X)[j + inr];
	    break;
	case CPLXSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		COMPLEX(X1)[j] = COMPLEX(X)[j + inr];
	    break;
	case STRSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		SET_STRING_ELT(X1, j, STRING_ELT(X, j + inr));
	    break;
	case RAWSXP:
	    for (j = 0, inr = i*nr; j < nr; j++)
		RAW(X1)[j] = RAW(X)[j + inr];
	    break;
	default:
	    error("unsupported type of array in apply");
	}
	/* careful: we have altered X1 and might have FUN = function(x) x */
	SET_VECTOR_ELT(ans, i, duplicate(eval(R_fcall, rho)));
    }
    UNPROTECT(3);
    return ans;
}
