/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  R Development Core Team
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

#include <Defn.h>
#include <Rmath.h>

static SEXP integer_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP real_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2);

static SEXP rcall;/* global, for error messages */

SEXP do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    return do_relop_dflt(call, op, CAR(args), CADR(args));
}

SEXP do_relop_dflt(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP class=R_NilValue, dims, tsp=R_NilValue, xnames, ynames;
    int nx, ny, xarray, yarray, xts, yts;
    Rboolean mismatch, iS;
    PROTECT_INDEX xpi, ypi;

    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    /* pre-test to handle the most common case quickly */
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue &&
	TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP &&
	LENGTH(x) > 0 && LENGTH(y) > 0) {
	SEXP ans = real_relop(PRIMVAL(op), x, y);
	UNPROTECT(2);
	return ans;
    }

    if ((iS = isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) : 
		       STRING_ELT(deparse1(x, 0), 0));
	REPROTECT(x = tmp, xpi);
	UNPROTECT(1);
    }
    if ((iS = isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) : 
		       STRING_ELT(deparse1(y, 0), 0));
	REPROTECT(y = tmp, ypi);
	UNPROTECT(1);
    }

    /* FIXME (?): S does
    if (!isVectorAtomic(x) || !isVectorAtomic(y)) { */
    if (!isVector(x) || !isVector(y)) {
	if (isNull(x) || isNull(y)) {
	    UNPROTECT(2);
	    return allocVector(LGLSXP,0);
	}
	errorcall(call,
		  "comparison (%d) is possible only for atomic types",
		  PRIMVAL(op));
    }

    /* ELSE :  x and y are both atomic */

    if (LENGTH(x) <= 0 || LENGTH(y) <= 0) {
	UNPROTECT(2);
	return allocVector(LGLSXP,0);
    }

    rcall = call;
    mismatch = FALSE;
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, "non-conformable arrays");
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else /*(yarray)*/ {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	}
	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	nx = length(x);
	ny = length(y);
	if (nx > 0 && ny > 0)
	    mismatch = ((nx > ny) ? nx % ny : ny % nx) != 0;
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }
    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, "Non-conformable time-series");
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(class = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(class = getAttrib(y, R_ClassSymbol));
	}
    }
    if (mismatch)
	warningcall(call, "longer object length\n"
		    "\tis not a multiple of shorter object length");

    if (isString(x) || isString(y)) {
	REPROTECT(x = coerceVector(x, STRSXP), xpi);
	REPROTECT(y = coerceVector(y, STRSXP), ypi);
	x = string_relop(PRIMVAL(op), x, y);
    }
    else if (isComplex(x) || isComplex(y)) {
	REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
	REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
	x = complex_relop(PRIMVAL(op), x, y);
    }
    else if (TYPEOF(x) == REALSXP || TYPEOF(y) == REALSXP) {
	REPROTECT(x = coerceVector(x, REALSXP), xpi);
	REPROTECT(y = coerceVector(y, REALSXP), ypi);
	x = real_relop(PRIMVAL(op), x, y);
    }
    else {
	x = integer_relop(PRIMVAL(op), x, y);
    }

    PROTECT(x);
    if (dims != R_NilValue) {
	setAttrib(x, R_DimSymbol, dims);
	if (xnames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, xnames);
	else if (ynames != R_NilValue)
	    setAttrib(x, R_DimNamesSymbol, ynames);
    }
    else {
	if (length(x) == length(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if (length(x) == length(ynames))
	    setAttrib(x, R_NamesSymbol, ynames);
    }
    if (xts || yts) {
	setAttrib(x, R_TspSymbol, tsp);
	setAttrib(x, R_ClassSymbol, class);
	UNPROTECT(2);
    }

    UNPROTECT(6);
    return x;
}

static SEXP integer_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2;
    int x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 == x2);
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 != x2);
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 < x2);
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 > x2);
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 <= x2);
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
	    x1 = INTEGER(s1)[i % n1];
	    x2 = INTEGER(s2)[i % n2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 >= x2);
	}
	break;
    }
    UNPROTECT(2);
    return ans;
}

static SEXP real_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2;
    double x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 == x2);
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 != x2);
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 < x2);
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 > x2);
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 <= x2);
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
	    x1 = REAL(s1)[i % n1];
	    x2 = REAL(s2)[i % n2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 >= x2);
	}
	break;
    }
    UNPROTECT(2);
    return ans;
}

static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2;
    Rcomplex x1, x2;
    SEXP ans;

    if (code != EQOP && code != NEOP) {
	errorcall(rcall, "illegal comparison with complex values");
    }

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
	    x1 = COMPLEX(s1)[i % n1];
	    x2 = COMPLEX(s2)[i % n2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1.r == x2.r && x1.i == x2.i);
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
	    x1 = COMPLEX(s1)[i % n1];
	    x2 = COMPLEX(s2)[i % n2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1.r != x2.r || x1.i != x2.i);
	}
	break;
    default:
	/* never happens (-Wall) */
	break;
    }
    UNPROTECT(2);
    return ans;
}
#ifdef HAVE_STRCOLL
#define STRCMP strcoll
#else
#define STRCMP strcmp
#endif

/* define NASTRING to have NA_STRING treated as missing */
#define NASTRING

static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (strcmp(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) == 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (streql(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) != 0)
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (STRCMP(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) < 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (STRCMP(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) > 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (STRCMP(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) <= 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
#ifdef NASTRING
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
#endif
	    if (STRCMP(CHAR(STRING_ELT(s1, i % n1)),
		       CHAR(STRING_ELT(s2, i % n2))) >= 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    }
    UNPROTECT(2);
    return ans;
}
