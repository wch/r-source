/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998  Robert Gentleman and Ross Ihaka
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

/* The `` x:y ''  primitive calls do_seq(); do_seq() calls cross() if
   both arguments are factors and seq() otherwise.
   */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Mathlib.h"

static SEXP
cross(SEXP s, SEXP t)
{
    SEXP a, la, ls, lt;
    int i, j, k, n, nls, nlt, vs, vt;

    n = length(s);
    nls = nlevels(s);
    nlt = nlevels(t);
    PROTECT(a = allocVector(INTSXP, n));
    for (i = 0; i < n; i++) {
	vs = INTEGER(s)[i];
	vt = INTEGER(t)[i];
	if ((vs == NA_INTEGER) || (vt == NA_INTEGER))
	    INTEGER(a)[i] = NA_INTEGER;
	else
	    INTEGER(a)[i] = vt + (vs - 1) * nlt;
    }
    ls = getAttrib(s, R_LevelsSymbol);
    lt = getAttrib(t, R_LevelsSymbol);
    if (!isNull(ls) && !isNull(lt)) {
	PROTECT(la = allocVector(STRSXP, nls * nlt));
	k = 0;
	for (i = 0; i < nls; i++) {
	    vs = strlen(CHAR(STRING(ls)[i]));
	    for (j = 0; j < nlt; j++) {
		vt = strlen(CHAR(STRING(lt)[j]));
		STRING(la)[k] = allocString(vs + vt + 1);
		sprintf(CHAR(STRING(la)[k]), "%s:%s",
			CHAR(STRING(ls)[i]), CHAR(STRING(lt)[j]));
		k++;
	    }
	}
	setAttrib(a, R_LevelsSymbol, la);
	UNPROTECT(1);
    }
    PROTECT(la = allocVector(STRSXP, 1));
    STRING(la)[0] = mkChar("factor");
    setAttrib(a, R_ClassSymbol, la);
    UNPROTECT(2);
    return(a);
}

static SEXP
seq(SEXP call, SEXP s1, SEXP s2)
{
    int i, n, in1;
    double n1, n2;
    SEXP ans;

    n1 = length(s1);
    if( n1 > 1 )
	warningcall(call, "Numerical expression has %d elements: only the first used", (int) n1);
    n2 = length(s2);
    if( n2 > 1 )
	warningcall(call, "Numerical expression has %d elements: only the first used", (int) n2);
    n1 = asReal(s1);
    n2 = asReal(s2);
    if (ISNAN(n1) || ISNAN(n2))
	errorcall(call, "NA/NaN argument");

    if (n1 <= INT_MIN || n2 <= INT_MIN || n1 > INT_MAX || n2 > INT_MAX
	|| fabs(n2 - n1) >= INT_MAX)
	errorcall(call, "argument too large in magnitude");

    n = fabs(n2 - n1) + 1 + FLT_EPSILON;
    if (n1 == (in1 = (int)(n1))) {
	ans = allocVector(INTSXP, n);
	if (n1 <= n2)
	    for (i = 0; i < n; i++) INTEGER(ans)[i] = in1 + i;
	else
	    for (i = 0; i < n; i++) INTEGER(ans)[i] = in1 - i;
    } else {
	ans = allocVector(REALSXP, n);
	if (n1 <= n2)
	    for (i = 0; i < n; i++) REAL(ans)[i] = n1 + i;
	else
	    for (i = 0; i < n; i++) REAL(ans)[i] = n1 - i;
    }
    return ans;
}

SEXP do_seq(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (isFactor(CAR(args)) && isFactor(CADR(args))) {
	if (length(CAR(args)) != length(CADR(args)))
	    errorcall(call, "unequal factor lengths");
	return(cross(CAR(args), CADR(args)));
    }
    return seq(call, CAR(args), CADR(args));
}

/* It is assumed that type-checking has been done in rep */
static SEXP rep2(SEXP s, SEXP ncopy)
{
    int i, na, nc, n, j;
    SEXP a, t, u;

    t = coerceVector(ncopy, INTSXP);
    PROTECT(t);

    nc = length(ncopy);
    na = 0;
    for (i = 0; i < nc; i++) {
	if (INTEGER(t)[i] == NA_INTEGER)
	    error("invalid number of copies in \"rep\"");
	na += INTEGER(t)[i];
    }

    if (isVector(s))
	a = allocVector(TYPEOF(s), na);
    else
	a = allocList(na);
    PROTECT(a);
    n = 0;
    switch (TYPEOF(s)) {
    case LGLSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		LOGICAL(a)[n++] = LOGICAL(s)[i];
	break;
    case INTSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		INTEGER(a)[n++] = INTEGER(s)[i];
	break;
    case REALSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		REAL(a)[n++] = REAL(s)[i];
	break;
    case CPLXSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		COMPLEX(a)[n++] = COMPLEX(s)[i];
	break;
    case STRSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		STRING(a)[n++] = STRING(s)[i];
	break;
    case VECSXP:
    case EXPRSXP:
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++)
		VECTOR(a)[n++] = VECTOR(s)[i];
	break;
    case LISTSXP:
	u = a;
	for (i = 0; i < nc; i++)
	    for (j = 0; j < (INTEGER(t)[i]); j++) {
		CAR(u) = duplicate(CAR(nthcdr(s, i)));
		u = CDR(u);
	    }
	break;
    default:
	UNIMPLEMENTED("rep2");
    }
    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    STRING(tmp)[0] = mkChar("ordered");
	    STRING(tmp)[1] = mkChar("factor");
	}
	else {
	    PROTECT(tmp = allocVector(STRSXP, 1));
	    STRING(tmp)[0] = mkChar("factor");
	}
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(2);
    return a;
}

SEXP rep(SEXP s, SEXP ncopy)
{
    int i, ns, na, nc;
    SEXP a, t;

    if (!isVector(ncopy))
	error("\"rep\" incorrect type for second argument");

    if (!isVector(s) && (!isList(s)))
	error("attempt to replicate non-vector");

    if ((length(ncopy) == length(s)))
	return rep2(s, ncopy);

    if ((length(ncopy) != 1))
	error("invalid number of copies in \"rep\"");

    if ((nc = asInteger(ncopy)) == NA_INTEGER || nc <= 0)
	error("invalid number of copies in \"rep\"");

    ns = length(s);
    na = nc * ns;
    if (isVector(s))
	a = allocVector(TYPEOF(s), na);
    else
	a = allocList(na);
    PROTECT(a);

    switch (TYPEOF(s)) {
    case LGLSXP:
	for (i = 0; i < na; i++)
	    LOGICAL(a)[i] = LOGICAL(s)[i % ns];
	break;
    case INTSXP:
	for (i = 0; i < na; i++)
	    INTEGER(a)[i] = INTEGER(s)[i % ns];
	break;
    case REALSXP:
	for (i = 0; i < na; i++)
	    REAL(a)[i] = REAL(s)[i % ns];
	break;
    case CPLXSXP:
	for (i = 0; i < na; i++)
	    COMPLEX(a)[i] = COMPLEX(s)[i % ns];
	break;
    case STRSXP:
	for (i = 0; i < na; i++)
	    STRING(a)[i] = STRING(s)[i % ns];
	break;
    case LISTSXP:
	i = 0;
	for (t = a; t != R_NilValue; t = CDR(t), i++)
	    CAR(t) = duplicate(CAR(nthcdr(s, (i % ns))));
	break;
    default:
	UNIMPLEMENTED("rep");
    }
    if (inherits(s, "factor")) {
	SEXP tmp;
	if(inherits(s, "ordered")) {
	    PROTECT(tmp = allocVector(STRSXP, 2));
	    STRING(tmp)[0] = mkChar("ordered");
	    STRING(tmp)[1] = mkChar("factor");
	}
	else {
	    PROTECT(tmp = allocVector(STRSXP, 1));
	    STRING(tmp)[0] = mkChar("factor");
	}
	setAttrib(a, R_ClassSymbol, tmp);
	UNPROTECT(1);
	setAttrib(a, R_LevelsSymbol, getAttrib(s, R_LevelsSymbol));
    }
    UNPROTECT(1);
    return a;
}

SEXP do_rep(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return rep(CAR(args), CADR(args));
}
