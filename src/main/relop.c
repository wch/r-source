/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  R Development Core Team
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

/* <UTF8> char here is handled as a whole.
   Collation is locale-specific if strcoll exists and works.
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Rmath.h>
#include <errno.h>

static SEXP integer_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP real_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call);
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP raw_relop(RELOP_TYPE code, SEXP s1, SEXP s2);

SEXP attribute_hidden do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;

    if (DispatchGroup("Ops", call, op, args, env, &ans))
	return ans;

    return do_relop_dflt(call, op, CAR(args), CADR(args));
}

SEXP attribute_hidden do_relop_dflt(SEXP call, SEXP op, SEXP x, SEXP y)
{
    SEXP klass = R_NilValue, dims, tsp=R_NilValue, xnames, ynames;
    int nx, ny, xarray, yarray, xts, yts;
    Rboolean mismatch = FALSE, iS;
    PROTECT_INDEX xpi, ypi;

    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);
    nx = length(x);
    ny = length(y);

    /* pre-test to handle the most common case quickly.
       Used to skip warning too ....
     */
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue &&
	TYPEOF(x) == REALSXP && TYPEOF(y) == REALSXP &&
	LENGTH(x) > 0 && LENGTH(y) > 0) {
	SEXP ans = real_relop((RELOP_TYPE) PRIMVAL(op), x, y);
	if (nx > 0 && ny > 0)
	    mismatch = ((nx > ny) ? nx % ny : ny % nx) != 0;
	if (mismatch)
	    warningcall(call, _("longer object length is not a multiple of shorter object length"));
	UNPROTECT(2);
	return ans;
    }

    /* That symbols and calls were allowwed was undocumented prior to
       R 2.5.0.  We deparse them as deparse() would, minus attributes */
    if ((iS = isSymbol(x)) || TYPEOF(x) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(x) :
		       STRING_ELT(deparse1(x, 0, DEFAULTDEPARSE), 0));
	REPROTECT(x = tmp, xpi);
	UNPROTECT(1);
    }
    if ((iS = isSymbol(y)) || TYPEOF(y) == LANGSXP) {
	SEXP tmp = allocVector(STRSXP, 1);
	PROTECT(tmp);
	SET_STRING_ELT(tmp, 0, (iS) ? PRINTNAME(y) :
		       STRING_ELT(deparse1(y, 0, DEFAULTDEPARSE), 0));
	REPROTECT(y = tmp, ypi);
	UNPROTECT(1);
    }

    if (!isVector(x) || !isVector(y)) {
	if (isNull(x) || isNull(y)) {
	    UNPROTECT(2);
	    return allocVector(LGLSXP,0);
	}
	errorcall(call,
		  _("comparison (%d) is possible only for atomic and list types"),
		  PRIMVAL(op));
    }

    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP)
	errorcall(call, _("comparison is not allowed for expressions"));

    /* ELSE :  x and y are both atomic or list */

    if (LENGTH(x) <= 0 || LENGTH(y) <= 0) {
	UNPROTECT(2);
	return allocVector(LGLSXP,0);
    }

    mismatch = FALSE;
    xarray = isArray(x);
    yarray = isArray(y);
    xts = isTs(x);
    yts = isTs(y);
    if (nx > 0 && ny > 0)
	mismatch = ((nx > ny) ? nx % ny : ny % nx) != 0;

    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, _("non-conformable arrays"));
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
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }
    if (xts || yts) {
	if (xts && yts) {
	    if (!tsConform(x, y))
		errorcall(call, _("non-conformable time series"));
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (length(x) < length(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (length(y) < length(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }
    if (mismatch)
	warningcall(call, _("longer object length is not a multiple of shorter object length"));

    if (isString(x) || isString(y)) {
	REPROTECT(x = coerceVector(x, STRSXP), xpi);
	REPROTECT(y = coerceVector(y, STRSXP), ypi);
	x = string_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isComplex(x) || isComplex(y)) {
	REPROTECT(x = coerceVector(x, CPLXSXP), xpi);
	REPROTECT(y = coerceVector(y, CPLXSXP), ypi);
	x = complex_relop((RELOP_TYPE) PRIMVAL(op), x, y, call);
    }
    else if (isReal(x) || isReal(y)) {
	REPROTECT(x = coerceVector(x, REALSXP), xpi);
	REPROTECT(y = coerceVector(y, REALSXP), ypi);
	x = real_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isInteger(x) || isInteger(y)) {
	REPROTECT(x = coerceVector(x, INTSXP), xpi);
	REPROTECT(y = coerceVector(y, INTSXP), ypi);
	x = integer_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isLogical(x) || isLogical(y)) {
	REPROTECT(x = coerceVector(x, LGLSXP), xpi);
	REPROTECT(y = coerceVector(y, LGLSXP), ypi);
	x = integer_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	REPROTECT(x = coerceVector(x, RAWSXP), xpi);
	REPROTECT(y = coerceVector(y, RAWSXP), ypi);
	x = raw_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    } else errorcall(call, _("comparison of these types is not implemented"));


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
	setAttrib(x, R_ClassSymbol, klass);
	UNPROTECT(2);
    }

    UNPROTECT(6);
    return x;
}

/* i1 = i % n1; i2 = i % n2;
 * this macro is quite a bit faster than having real modulo calls
 * in the loop (tested on Intel and Sparc)
 */
#define mod_iterate(n1,n2,i1,i2) for (i=i1=i2=0; i<n; \
	i1 = (++i1 == n1) ? 0 : i1,\
	i2 = (++i2 == n2) ? 0 : i2,\
	++i)

static SEXP integer_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
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
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 == x2);
	}
	break;
    case NEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 != x2);
	}
	break;
    case LTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 < x2);
	}
	break;
    case GTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 > x2);
	}
	break;
    case LEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
	    if (x1 == NA_INTEGER || x2 == NA_INTEGER)
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 <= x2);
	}
	break;
    case GEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = INTEGER(s1)[i1];
	    x2 = INTEGER(s2)[i2];
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
    int i, i1, i2, n, n1, n2;
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
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 == x2);
	}
	break;
    case NEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 != x2);
	}
	break;
    case LTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 < x2);
	}
	break;
    case GTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 > x2);
	}
	break;
    case LEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
	    if (ISNAN(x1) || ISNAN(x2))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1 <= x2);
	}
	break;
    case GEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = REAL(s1)[i1];
	    x2 = REAL(s2)[i2];
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

static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call)
{
    int i, i1, i2, n, n1, n2;
    Rcomplex x1, x2;
    SEXP ans;

    if (code != EQOP && code != NEOP) {
	errorcall(call, _("invalid comparison with complex values"));
    }

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = COMPLEX(s1)[i1];
	    x2 = COMPLEX(s2)[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		LOGICAL(ans)[i] = NA_LOGICAL;
	    else
		LOGICAL(ans)[i] = (x1.r == x2.r && x1.i == x2.i);
	}
	break;
    case NEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = COMPLEX(s1)[i1];
	    x2 = COMPLEX(s2)[i2];
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

#if defined(Win32) && defined(SUPPORT_UTF8)
#define STRCOLL Rstrcoll
#else

#ifdef HAVE_STRCOLL
#define STRCOLL strcoll
#else
#define STRCOLL strcmp
#endif

#endif


/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, n, n1, n2, res;
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
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
	    if (strcmp(translateChar(STRING_ELT(s1, i % n1)),
		       translateChar(STRING_ELT(s2, i % n2))) == 0)
		LOGICAL(ans)[i] = 1;
	    else
		LOGICAL(ans)[i] = 0;
	}
	break;
    case NEOP:
	for (i = 0; i < n; i++) {
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else
	    if (streql(translateChar(STRING_ELT(s1, i % n1)),
		       translateChar(STRING_ELT(s2, i % n2))) != 0)
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    case LTOP:
	for (i = 0; i < n; i++) {
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else {
		errno = 0;
		res = STRCOLL(translateChar(STRING_ELT(s1, i % n1)),
			      translateChar(STRING_ELT(s2, i % n2)));
		if(errno)  
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res < 0)?1:0;
	    }
	}
	break;
    case GTOP:
	for (i = 0; i < n; i++) {
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else {
		errno = 0;
		res = STRCOLL(translateChar(STRING_ELT(s1, i % n1)),
			      translateChar(STRING_ELT(s2, i % n2)));
		if(errno)  
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res > 0)?1:0;
	    }
	}
	break;
    case LEOP:
	for (i = 0; i < n; i++) {
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
 	    else {
		errno = 0;
		res = STRCOLL(translateChar(STRING_ELT(s1, i % n1)),
			      translateChar(STRING_ELT(s2, i % n2)));
		if(errno)  
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res <= 0)?1:0;
	    }
	}
	break;
    case GEOP:
	for (i = 0; i < n; i++) {
	    if ((STRING_ELT(s1, i % n1) == NA_STRING) ||
		(STRING_ELT(s2, i % n2) == NA_STRING))
		LOGICAL(ans)[i] = NA_LOGICAL;
  	    else {
		errno = 0;
		res = STRCOLL(translateChar(STRING_ELT(s1, i % n1)),
			      translateChar(STRING_ELT(s2, i % n2)));
		if(errno)  
		    LOGICAL(ans)[i] = NA_LOGICAL;
		else
		    LOGICAL(ans)[i] = (res >= 0)?1:0;
	    }
	}
	break;
    }
    UNPROTECT(2);
    return ans;
}

static SEXP raw_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    int i, i1, i2, n, n1, n2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = LENGTH(s1);
    n2 = LENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    switch (code) {
    case EQOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 == x2);
	}
	break;
    case NEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 != x2);
	}
	break;
    case LTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 < x2);
	}
	break;
    case GTOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 > x2);
	}
	break;
    case LEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 <= x2);
	}
	break;
    case GEOP:
	mod_iterate(n1, n2, i1, i2) {
	    x1 = RAW(s1)[i1];
	    x2 = RAW(s2)[i2];
	    LOGICAL(ans)[i] = (x1 >= x2);
	}
	break;
    }
    UNPROTECT(2);
    return ans;
}
