/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000  the R Development Core Team
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

/* .Internal(lapply(i, FUN)) */

SEXP do_lapply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, ans, FUN, ind;
    int i, n;

    checkArity(op, args);
    n = asInteger(CAR(args));
    if (n == NA_INTEGER)
	errorcall(call, "invalid length");
    args = CDR(args);
    FUN = CAR(args);
    if(!isFunction(FUN))
	errorcall(call, "argument FUN is not a function");
    PROTECT(R_fcall = LCONS(FUN, args));
    PROTECT(ind = allocVector(INTSXP, 1));
    CADR(R_fcall) = ind;
    PROTECT(ans = allocVector(VECSXP, n));
    for(i = 0; i < n; i++) {
	INTEGER(ind)[0] = i + 1;
	VECTOR(ans)[i] = eval(R_fcall, rho);
    }
    UNPROTECT(3);
    return ans;
}

/* .Internal(lapply(X, FUN)) */
/* X is a matrix, and the last dimension is the one we want to 
   loop over */

SEXP do_apply(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP R_fcall, X, Xd, X1, ans, FUN;
    int i, nr, nc;

    checkArity(op, args);
    X = CAR(args);
    if(!isMatrix(X))
	errorcall(call, "First arg is not a matrix");
    Xd = getAttrib(X, R_DimSymbol);
    nr = INTEGER(Xd)[0];
    nc = INTEGER(Xd)[1];
    args = CDR(args);
    X1 = CAR(args);
    args = CDR(args);
    FUN = CAR(args);
    if(!isFunction(FUN))
	errorcall(call, "argument FUN is not a function");
    PROTECT(R_fcall = LCONS(FUN, args));
    PROTECT(ans = allocVector(VECSXP, nc));
    CADR(R_fcall) = X1;
    for(i = 0; i < nc; i++) {
	switch(TYPEOF(X)) {
	case REALSXP:
	    REAL(X1) = REAL(X) + i*nr;
	    break;
	case INTSXP:
	    INTEGER(X1) = INTEGER(X) + i*nr;
	    break;
	case LGLSXP:
	    LOGICAL(X1) = LOGICAL(X) + i*nr;
	    break;
	case CPLXSXP:
	    COMPLEX(X1) = COMPLEX(X) + i*nr;
	    break;
	case STRSXP:
	    STRING(X1) = STRING(X) + i*nr;
	    break;
	default:
	    error("unsupported type of array in apply");
	}
	/* careful: we have altered X1 and might have FUN = function(x) x */
	VECTOR(ans)[i] = duplicate(eval(R_fcall, rho));
    }
    UNPROTECT(2);
    return ans;
}
