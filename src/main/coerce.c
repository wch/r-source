/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-2003  Robert Gentleman, Ross Ihaka and the
 *			     R Development Core Team
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

#include <Defn.h> /*-- Maybe modularize into own Coerce.h ..*/
#include <Rmath.h>
#include <Print.h>


/* This section of code handles type conversion for elements */
/* of data vectors.  Type coercion throughout R should use these */
/* routines to ensure consistency. */


const static char * const truenames[] = {
    "T",
    "True",
    "TRUE",
    "true",
    (char *) 0,
};

const static char * const falsenames[] = {
    "F",
    "False",
    "FALSE",
    "false",
    (char *) 0,
};

/* Coercion warnings will be OR'ed : */
#define WARN_NA	   1
#define WARN_INACC 2
#define WARN_IMAG  4

/* The following two macros copy or clear the attributes.  They also
   ensure that the object bit is properly set.  They avoid calling the
   assignment functions when possible, since the write barrier (and
   possibly cache behavior on some architectures) makes assigning more
   costly than dereferencing. */
#define DUPLICATE_ATTRIB(to, from) do {\
  SEXP __from__ = (from); \
  if (ATTRIB(__from__) != R_NilValue) { \
    SEXP __to__ = (to); \
    SET_ATTRIB(__to__, duplicate(ATTRIB(__from__))); \
    if (OBJECT(__from__)) SET_OBJECT(__to__, 1); \
  } \
} while (0)

#define CLEAR_ATTRIB(x) do {\
  SEXP __x__ = (x); \
  if (ATTRIB(__x__) != R_NilValue) { \
    SET_ATTRIB(__x__, R_NilValue); \
    if (OBJECT(__x__)) SET_OBJECT(__x__, 0); \
  } \
} while (0)

void CoercionWarning(int warn)
{
/* FIXME: Use
   =====
   WarningMessage(R_NilValue, WARNING_....);
*/
    if (warn & WARN_NA)
	warning("NAs introduced by coercion");
    if (warn & WARN_INACC)
	warning("inaccurate integer conversion in coercion");
    if (warn & WARN_IMAG)
	warning("imaginary parts discarded in coercion");
}

double R_strtod(const char *c, char **end)
{
    double x;

    if (strncmp(c, "NA", 2) == 0){
	x = NA_REAL; *end = (char *)c + 2; /* coercion for -Wall */
    }
    else if (strncmp(c, "NaN", 3) == 0) {
	x = R_NaN; *end = (char *)c + 3;
    }
    else if (strncmp(c, "Inf", 3) == 0) {
	x = R_PosInf; *end = (char *)c + 3;
    }
    else if (strncmp(c, "-Inf", 4) == 0) {
	x = R_NegInf; *end = (char *)c + 4;
    }
    else
        x = strtod(c, end);
    return x;
}

int LogicalFromInteger(int x, int *warn)
{
    return (x == NA_INTEGER) ?
	NA_LOGICAL : (x != 0);
}

int LogicalFromReal(double x, int *warn)
{
    return ISNAN(x) ?
	NA_LOGICAL : (x != 0);
}

int LogicalFromComplex(Rcomplex x, int *warn)
{
    return (ISNAN(x.r) || ISNAN(x.i)) ?
	NA_LOGICAL : (x.r != 0 || x.i != 0);
}

int LogicalFromString(SEXP x, int *warn)
{
    if (x != R_NaString) {
	int i;
	for (i = 0; truenames[i]; i++)
	    if (!strcmp(CHAR(x), truenames[i]))
		return 1;
	for (i = 0; falsenames[i]; i++)
	    if (!strcmp(CHAR(x), falsenames[i]))
		return 0;
    }
    return NA_LOGICAL;
}

int IntegerFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_INTEGER : x;
}

int IntegerFromReal(double x, int *warn)
{
    if (ISNAN(x))
	return NA_INTEGER;
    else if (x > INT_MAX || x <= INT_MIN ) {
	*warn |= WARN_NA;
	return NA_INTEGER;
    }
    return x;
}

int IntegerFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_INTEGER;
    else if (x.r > INT_MAX || x.r <= INT_MIN ) {
	*warn |= WARN_NA;
	return NA_INTEGER;;
    }
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return x.r;
}

int IntegerFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !isBlankString(CHAR(x))) {
	xdouble = strtod(CHAR(x), &endp);
	if (isBlankString(endp)) {
	    if (xdouble > INT_MAX) {
		*warn |= WARN_INACC;
		return INT_MAX;
	    }
	    else if(xdouble < INT_MIN+1) {
		*warn |= WARN_INACC;
		return INT_MIN;
	    }
	    else
		return xdouble;
	}
	else *warn |= WARN_NA;
    }
    return NA_INTEGER;
}

double RealFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_REAL : x;
}

double RealFromInteger(int x, int *warn)
{
    if (x == NA_INTEGER)
	return NA_REAL;
    else
	return x;
}

double RealFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_REAL;
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return x.r;
}

double RealFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !isBlankString(CHAR(x))) {
	xdouble = R_strtod(CHAR(x), &endp);
	if (isBlankString(endp))
	    return xdouble;
	else
	    *warn |= WARN_NA;
    }
    return NA_REAL;
}

Rcomplex ComplexFromLogical(int x, int *warn)
{
    Rcomplex z;
    if (x == NA_LOGICAL) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

Rcomplex ComplexFromInteger(int x, int *warn)
{
    Rcomplex z;
    if (x == NA_INTEGER) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

Rcomplex ComplexFromReal(double x, int *warn)
{
    Rcomplex z;
    if (ISNAN(x)) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
	z.r = x;
	z.i = 0;
    }
    return z;
}

Rcomplex ComplexFromString(SEXP x, int *warn)
{
    double xr, xi;
    Rcomplex z;
    char *endp = CHAR(x);;
    z.r = z.i = NA_REAL;
    if (x != R_NaString && !isBlankString(endp)) {
	xr = R_strtod(endp, &endp);
	if (isBlankString(endp)) {
	    z.r = xr;
	    z.i = 0.0;
	}
	else if (*endp == '+' || *endp == '-') {
	    xi = R_strtod(endp, &endp);
	    if (*endp++ == 'i' && isBlankString(endp)) {
		z.r = xr;
		z.i = xi;
	    }
	    else *warn |= WARN_NA;
	}
	else *warn |= WARN_NA;
    }
    return z;
}

SEXP StringFromLogical(int x, int *warn)
{
    int w;
    formatLogical(&x, 1, &w);
    if (x == NA_LOGICAL) return NA_STRING;
    else return mkChar(EncodeLogical(x, w));
}

SEXP StringFromInteger(int x, int *warn)
{
    int w;
    formatInteger(&x, 1, &w);
    if (x == NA_INTEGER) return NA_STRING;
    else return mkChar(EncodeInteger(x, w));
}

SEXP StringFromReal(double x, int *warn)
{
    int w, d, e;
    formatReal(&x, 1, &w, &d, &e, 0);
    if (ISNA(x)) return NA_STRING;
    else return mkChar(EncodeReal(x, w, d, e));
}

SEXP StringFromComplex(Rcomplex x, int *warn)
{
    int wr, dr, er, wi, di, ei;
    formatComplex(&x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
    if (ISNA(x.r) || ISNA(x.i)) return NA_STRING;
    else
	return mkChar(EncodeComplex(x, wr, dr, er, wi, di, ei));
}

/* Conversion between the two list types (LISTSXP and VECSXP). */

SEXP PairToVectorList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len = 0, named = 0;
    for (xptr = x ; xptr != R_NilValue ; xptr = CDR(xptr)) {
	named = named | (TAG(xptr) != R_NilValue);
	len++;
    }
    PROTECT(x);
    PROTECT(xnew = allocVector(VECSXP, len));
    for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr))
	SET_VECTOR_ELT(xnew, i, CAR(xptr));
    if (named) {
	PROTECT(xnames = allocVector(STRSXP, len));
	xptr = x;
	for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr)) {
	    if(TAG(xptr) == R_NilValue)
		SET_STRING_ELT(xnames, i, R_BlankString);
	    else
		SET_STRING_ELT(xnames, i, PRINTNAME(TAG(xptr)));
	}
	setAttrib(xnew, R_NamesSymbol, xnames);
	UNPROTECT(1);
    }
    copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

SEXP VectorToPairList(SEXP x)
{
    SEXP xptr, xnew, xnames;
    int i, len, named;
    len = length(x);
    PROTECT(x);
    PROTECT(xnew = allocList(len));
    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
    named = (xnames != R_NilValue);
    xptr = xnew;
    for (i = 0; i < len; i++) {
	SETCAR(xptr, VECTOR_ELT(x, i));
	if (named && CHAR(STRING_ELT(xnames, i))[0] != '\0')
	    SET_TAG(xptr, install(CHAR(STRING_ELT(xnames, i))));
	xptr = CDR(xptr);
    }
    copyMostAttrib(x, xnew);
    UNPROTECT(3);
    return xnew;
}

static SEXP coerceToSymbol(SEXP v)
{
    SEXP ans = R_NilValue;
    int warn = 0;
    if (length(v) <= 0)
	error("Invalid data of mode \"%s\" (too short)",
	      CHAR(type2str(TYPEOF(v))));
    PROTECT(v);
    switch(TYPEOF(v)) {
    case LGLSXP:
	ans = StringFromLogical(LOGICAL(v)[0], &warn);
	break;
    case INTSXP:
	ans = StringFromInteger(INTEGER(v)[0], &warn);
	break;
    case REALSXP:
	ans = StringFromReal(REAL(v)[0], &warn);
	break;
    case CPLXSXP:
	ans = StringFromComplex(COMPLEX(v)[0], &warn);
	break;
    case STRSXP:
	ans = STRING_ELT(v, 0);
	break;
    }
    if (warn) CoercionWarning(warn);/*2000/10/23*/
    ans = install(CHAR(ans));
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToLogical(SEXP v)
{
    SEXP ans;
    int i, n, warn = 0;
    PROTECT(ans = allocVector(LGLSXP, n = length(v)));
    DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = LogicalFromInteger(INTEGER(v)[i], &warn);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = LogicalFromReal(REAL(v)[i], &warn);
	    break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = LogicalFromComplex(COMPLEX(v)[i], &warn);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = LogicalFromString(STRING_ELT(v, i), &warn);
	break;
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToInteger(SEXP v)
{
    SEXP ans;
    int i, n, warn = 0;
    PROTECT(ans = allocVector(INTSXP, n = LENGTH(v)));
    DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = IntegerFromLogical(LOGICAL(v)[i], &warn);
	    break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = IntegerFromReal(REAL(v)[i], &warn);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = IntegerFromComplex(COMPLEX(v)[i], &warn);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    INTEGER(ans)[i] = IntegerFromString(STRING_ELT(v, i), &warn);
	break;
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToReal(SEXP v)
{
    SEXP ans;
    int i, n, warn = 0;
    PROTECT(ans = allocVector(REALSXP, n = LENGTH(v)));
    DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = RealFromLogical(LOGICAL(v)[i], &warn);
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = RealFromInteger(INTEGER(v)[i], &warn);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = RealFromComplex(COMPLEX(v)[i], &warn);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    REAL(ans)[i] = RealFromString(STRING_ELT(v, i), &warn);
	break;
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToComplex(SEXP v)
{
    SEXP ans;
    int i, n, warn = 0;
    PROTECT(ans = allocVector(CPLXSXP, n = LENGTH(v)));
    DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    COMPLEX(ans)[i] = ComplexFromLogical(LOGICAL(v)[i], &warn);
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    COMPLEX(ans)[i] = ComplexFromInteger(INTEGER(v)[i], &warn);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    COMPLEX(ans)[i] = ComplexFromReal(REAL(v)[i], &warn);
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    COMPLEX(ans)[i] = ComplexFromString(STRING_ELT(v, i), &warn);
	break;
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToString(SEXP v)
{
    SEXP ans;
    int i, n, savedigits, warn = 0;
    PROTECT(ans = allocVector(STRSXP, n = LENGTH(v)));
    DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(ans, i, StringFromLogical(LOGICAL(v)[i], &warn));
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(ans, i, StringFromInteger(INTEGER(v)[i], &warn));
	break;
    case REALSXP:
	PrintDefaults(R_NilValue);
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(ans, i, StringFromReal(REAL(v)[i], &warn));
	R_print.digits = savedigits;
	break;
    case CPLXSXP:
	PrintDefaults(R_NilValue);
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++)
	    SET_STRING_ELT(ans, i, StringFromComplex(COMPLEX(v)[i], &warn));
	R_print.digits = savedigits;
	break;
    }
    if (warn) CoercionWarning(warn);/*2000/10/23*/
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToExpression(SEXP v)
{
    SEXP ans;
    int i, n;
    if (isVectorAtomic(v)) {
	n = LENGTH(v);
	PROTECT(ans = allocVector(EXPRSXP, n));
	switch (TYPEOF(v)) {
	case LGLSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarLogical(LOGICAL(v)[i]));
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarInteger(INTEGER(v)[i]));
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarReal(REAL(v)[i]));
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarComplex(COMPLEX(v)[i]));
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarString(STRING_ELT(v, i)));
	    break;
	}
    }
    else {/* not used either */
	PROTECT(ans = allocVector(EXPRSXP, 1));
	SET_VECTOR_ELT(ans, 0, duplicate(v));
    }
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToVectorList(SEXP v)
{
    SEXP ans, tmp;
    int i, n;
    n = length(v);
    PROTECT(ans = allocVector(VECSXP, n));
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarLogical(LOGICAL(v)[i]));
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarInteger(INTEGER(v)[i]));
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarReal(REAL(v)[i]));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarComplex(COMPLEX(v)[i]));
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    SET_VECTOR_ELT(ans, i, ScalarString(STRING_ELT(v, i)));
	break;
    case LISTSXP:
    case LANGSXP:
	tmp = v;
	for (i = 0; i < n; i++) {
	    SET_VECTOR_ELT(ans, i, CAR(tmp));
	    tmp = CDR(tmp);
	}
	break;
    default:
	UNIMPLEMENTED("coerceToVectorList");
    }
    tmp = getAttrib(v, R_NamesSymbol);
    if (tmp != R_NilValue)
	setAttrib(ans, R_NamesSymbol, tmp);
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToPairList(SEXP v)
{
    SEXP ans, ansp;
    int i, n;
    n = LENGTH(v);
    PROTECT(ansp = ans = allocList(n));
    for (i = 0; i < n; i++) {
	switch (TYPEOF(v)) {
	case LGLSXP:
	    SETCAR(ansp, allocVector(LGLSXP, 1));
	    INTEGER(CAR(ansp))[0] = INTEGER(v)[i];
	    break;
	case INTSXP:
	    SETCAR(ansp, allocVector(INTSXP, 1));
	    INTEGER(CAR(ansp))[0] = INTEGER(v)[i];
	    break;
	case REALSXP:
	    SETCAR(ansp, allocVector(REALSXP, 1));
	    REAL(CAR(ansp))[0] = REAL(v)[i];
	    break;
	case CPLXSXP:
	    SETCAR(ansp, allocVector(CPLXSXP, 1));
	    COMPLEX(CAR(ansp))[0] = COMPLEX(v)[i];
	    break;
	case STRSXP:
	    SETCAR(ansp, allocVector(STRSXP, 1));
	    SET_STRING_ELT(CAR(ansp), 0, STRING_ELT(v, i));
	    break;
	case VECSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	case EXPRSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	default:
	    UNIMPLEMENTED("coerceToPairList");
	}
	ansp = CDR(ansp);
    }
    ansp = getAttrib(v, R_NamesSymbol);
    if (ansp != R_NilValue)
	setAttrib(ans, R_NamesSymbol, ansp);
    UNPROTECT(1);
    return (ans);
}

/* Coerce a list to the given type */
static SEXP coercePairList(SEXP v, SEXPTYPE type)
{
    int i, n=0;
    SEXP rval= R_NilValue, vp, names;

    if(type == LISTSXP) return v;/* IS pairlist */
    names = v;
    if (type == EXPRSXP) {
	PROTECT(rval = allocVector(type, 1));
	SET_VECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
	return rval;
    }
    else if (type == STRSXP) {
	n = length(v);
	PROTECT(rval = allocVector(type, n));
	for (vp = v, i = 0; vp != R_NilValue; vp = CDR(vp), i++) {
	    if (isString(CAR(vp)) && length(CAR(vp)) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(rval, i, STRING_ELT(deparse1line(CAR(vp), 0), 0));
	}
    }
    else if (type == VECSXP) {
	rval = PairToVectorList(v);
	return rval;
    }
    else if (isVectorizable(v)) {
	n = length(v);
	PROTECT(rval = allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		LOGICAL(rval)[i] = asLogical(CAR(vp));
	    break;
	case INTSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		INTEGER(rval)[i] = asInteger(CAR(vp));
	    break;
	case REALSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		REAL(rval)[i] = asReal(CAR(vp));
	    break;
	case CPLXSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		COMPLEX(rval)[i] = asComplex(CAR(vp));
	    break;
	default:
	    UNIMPLEMENTED("coercePairList");
	}
    }
    else
	error("pairlist object cannot be coerced to %s",
	      CHAR(type2str(type)));

    /* If any tags are non-null then we */
    /* need to add a names attribute. */
    for (vp = v, i = 0; vp != R_NilValue; vp = CDR(vp))
	if (TAG(vp) != R_NilValue)
	    i = 1;

    if (i) {
	i = 0;
	names = allocVector(STRSXP, n);
	for (vp = v; vp != R_NilValue; vp = CDR(vp), i++)
	    if (TAG(vp) != R_NilValue)
		SET_STRING_ELT(names, i, PRINTNAME(TAG(vp)));
	setAttrib(rval, R_NamesSymbol, names);
    }
    UNPROTECT(1);
    return rval;
}

static SEXP coerceVectorList(SEXP v, SEXPTYPE type)
{
    int i, n;
    SEXP rval, names;

    names = v;
    rval = R_NilValue;	/* -Wall */
    if (type == EXPRSXP) {
	PROTECT(rval = allocVector(type, 1));
	SET_VECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
	return rval;
    }
    else if (type == STRSXP) {
	n = length(v);
	PROTECT(rval = allocVector(type, n));
	for (i = 0; i < n;  i++) {
	    if (isString(VECTOR_ELT(v, i)) && length(VECTOR_ELT(v, i)) == 1)
		SET_STRING_ELT(rval, i, STRING_ELT(VECTOR_ELT(v, i), 0));
#if 0
	    /* this will make as.character(list(s)) not backquote
	     * non-syntactic name s. It is not entirely clear that
	     * that is really desirable though....
	     */
	    else if (isSymbol(VECTOR_ELT(v, i)))
	    	SET_STRING_ELT(rval, i, PRINTNAME(VECTOR_ELT(v, i)));
#endif
	    else
		SET_STRING_ELT(rval, i,
			       STRING_ELT(deparse1line(VECTOR_ELT(v, i), 0), 0));
	}
    }
    else if (type == LISTSXP) {
	rval = VectorToPairList(v);
	return rval;
    }
    else if (isVectorizable(v)) {
	n = length(v);
	PROTECT(rval = allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0; i < n; i++)
		LOGICAL(rval)[i] = asLogical(VECTOR_ELT(v, i));
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++)
		INTEGER(rval)[i] = asInteger(VECTOR_ELT(v, i));
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++)
		REAL(rval)[i] = asReal(VECTOR_ELT(v, i));
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++)
		COMPLEX(rval)[i] = asComplex(VECTOR_ELT(v, i));
	    break;
	default:
	    UNIMPLEMENTED("coerceVectorList");
	}
    }
    else
	error("(list) object cannot be coerced to %s",
	      CHAR(type2str(type)));

    names = getAttrib(v, R_NamesSymbol);
    if (names != R_NilValue)
	setAttrib(rval, R_NamesSymbol, names);
    UNPROTECT(1);
    return rval;
}

static SEXP coerceSymbol(SEXP v, SEXPTYPE type)
{
    SEXP rval = R_NilValue;
    if (type == EXPRSXP) {
	PROTECT(rval = allocVector(type, 1));
	SET_VECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
    }
    return rval;
}

SEXP coerceVector(SEXP v, SEXPTYPE type)
{
    SEXP op, vp, ans = R_NilValue;	/* -Wall */
    int i,n;

    if (TYPEOF(v) == type)
	return v;

    switch (TYPEOF(v)) {
#ifdef NOTYET
    case NILSXP:
	ans = coerceNull(v, type);
	break;
#endif
    case SYMSXP:
	ans = coerceSymbol(v, type);
	break;
    case NILSXP:
    case LISTSXP:
	ans = coercePairList(v, type);
	break;
    case LANGSXP:
	if (type != STRSXP) {
	    ans = coercePairList(v, type);
	    break;
	}

	/* This is mostly copied from coercePairList, but we need to
	 * special-case the first element so as not to get operators
	 * put in backticks. */
	n = length(v);
	PROTECT(ans = allocVector(type, n));
	if (n == 0) break; /* Can this actually happen? */
	i = 0;
	op = CAR(v);
	/* The case of practical relevance is "lhs ~ rhs", which
	 * people tend to split using as.character(), modify, and
	 * paste() back together. However, we might as well
	 * special-case all symbolic operators here. */
	if (TYPEOF(op) == SYMSXP) {
	    SET_STRING_ELT(ans, i, PRINTNAME(op));
	    i++;
	    v = CDR(v);
	}

	/* The distinction between strings and other elements was
	 * here "always", but is really dubious since it makes x <- a
	 * and x <- "a" come out identical. Won't fix just now. */
	for (vp = v;  vp != R_NilValue; vp = CDR(vp), i++) {
	    if (isString(CAR(vp)) && length(CAR(vp)) == 1)
		SET_STRING_ELT(ans, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(ans, i, STRING_ELT(deparse1line(CAR(vp), 0), 0));
	}
	UNPROTECT(1);
	break;
    case VECSXP:
    case EXPRSXP:
	ans = coerceVectorList(v, type);
	break;
    case ENVSXP:
	error("environments cannot be coerced to other types");
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:

#define COERCE_ERROR						\
	error("cannot coerce type %s to %s vector",		\
	      CHAR(type2str(TYPEOF(v))), CHAR(type2str(type)))

	switch (type) {
	case SYMSXP:
	    ans = coerceToSymbol(v);	    break;
	case LGLSXP:
	    ans = coerceToLogical(v);	    break;
	case INTSXP:
	    ans = coerceToInteger(v);	    break;
	case REALSXP:
	    ans = coerceToReal(v);	    break;
	case CPLXSXP:
	    ans = coerceToComplex(v);	    break;
	case STRSXP:
	    ans = coerceToString(v);	    break;
	case EXPRSXP:
	    ans = coerceToExpression(v);    break;
	case VECSXP:
	    ans = coerceToVectorList(v);    break;
	case LISTSXP:
	    ans = coerceToPairList(v);	    break;
	default:
	    COERCE_ERROR;
	}
	break;
    default:
	COERCE_ERROR;
    }
    return ans;
}
#undef COERCE_ERROR


SEXP CreateTag(SEXP x)
{
    if (isNull(x) || isSymbol(x))
	return x;
    if (isString(x)
	&& length(x) >= 1
	&& length(STRING_ELT(x, 0)) >= 1)
	x = install(CHAR(STRING_ELT(x, 0)));
    else
	x = install(CHAR(STRING_ELT(deparse1(x, 1), 0)));
    return x;
}

static SEXP asFunction(SEXP x)
{
    SEXP f, pf;
    int n;
    if (isFunction(x)) return x;
    PROTECT(f = allocSExp(CLOSXP));
    SET_CLOENV(f, R_GlobalEnv);
    if (NAMED(x)) PROTECT(x = duplicate(x));
    else PROTECT(x);

    if (isNull(x) || !isList(x)) {
	SET_FORMALS(f, R_NilValue);
	SET_BODY(f, x);
    }
    else {
	n = length(x);
	pf = allocList(n - 1);
	SET_FORMALS(f, pf);
	while(--n) {
	    if (TAG(x) == R_NilValue) {
		SET_TAG(pf, CreateTag(CAR(x)));
		SETCAR(pf, R_MissingArg);
	    }
	    else {
		SETCAR(pf, CAR(x));
		SET_TAG(pf, TAG(x));
	    }
	    pf = CDR(pf);
	    x = CDR(x);
	}
	SET_BODY(f, CAR(x));
    }
    UNPROTECT(2);
    return f;
}

SEXP ascommon(SEXP call, SEXP u, SEXPTYPE type)
{
    /* -> as.vector(..) or as.XXX(.) : coerce 'u' to 'type' : */
    SEXP v;
    if (type == CLOSXP) {
	return asFunction(u);
    }
    else if (isVector(u) || isList(u) || isLanguage(u)
	     || (isSymbol(u) && type == EXPRSXP)) {
	if (NAMED(u))
	    v = duplicate(u);
	else v = u;
	if (type != ANYSXP) {
	    PROTECT(v);
	    v = coerceVector(v, type);
	    UNPROTECT(1);
	}
	/* drop attributes() and class() in some cases: */
	if ((type == LISTSXP
	     /* already loses 'names' where it shouldn't:
		|| type == VECSXP) */
	    ) &&
	    !(TYPEOF(u) == LANGSXP || TYPEOF(u) == LISTSXP ||
	      TYPEOF(u) == EXPRSXP || TYPEOF(u) == VECSXP)) {
	    CLEAR_ATTRIB(v);
	}
	return v;
    }
    else if (isSymbol(u) && type == STRSXP) {
	v = allocVector(STRSXP, 1);
	SET_STRING_ELT(v, 0, PRINTNAME(u));
	return v;
    }
    else if (isSymbol(u) && type == SYMSXP)
	return u;
    else errorcall(call, "cannot coerce to vector");
    return u;/* -Wall */
}

SEXP do_ascharacter(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    if (DispatchOrEval(call, op, "as.character", args, rho, &ans, 1, 0))
	return(ans);

    /* Method dispatch has failed, we now just */
    /* run the generic internal code */

    PROTECT(args = ans);
    checkArity(op, args);

    ans = ascommon(call, CAR(args), STRSXP);
    CLEAR_ATTRIB(ans);
    UNPROTECT(1);
    return ans;
}


SEXP do_asvector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    int type;

    if (DispatchOrEval(call, op, "as.vector", args, rho, &ans, 1, 0))
	return(ans);

    /* Method dispatch has failed, we now just */
    /* run the generic internal code */

    PROTECT(args = ans);
    checkArity(op, args);

    if (!isString(CADR(args)) || LENGTH(CADR(args)) < 1)
	errorcall_return(call, R_MSG_mode);

    if (!strcmp("function", (CHAR(STRING_ELT(CADR(args), 0)))))
	type = CLOSXP;
    else
	type = str2type(CHAR(STRING_ELT(CADR(args), 0)));

    switch(type) {/* only those are valid : */
    case SYMSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP: /* list */
    case LISTSXP:/* pairlist */
    case CLOSXP: /* non-primitive function */
    case ANYSXP: /* any */
	break;
    default:
	errorcall_return(call, R_MSG_mode);
    }
    ans = ascommon(call, CAR(args), type);
    switch(TYPEOF(ans)) {/* keep attributes for these:*/
    case NILSXP:
    case VECSXP:
    case EXPRSXP:
    case LISTSXP:
    case LANGSXP:
	break;
    default:
	CLEAR_ATTRIB(ans);
	break;
    }
    UNPROTECT(1);
    return ans;
}


SEXP do_asfunction(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arglist, envir, names, pargs;
    int i, n;

    checkArity(op, args);

    /* Check the arguments; we need a list and environment. */

    arglist = CAR(args);
    if (!isNewList(arglist))
	errorcall(call, "list argument expected");

    envir = CADR(args);
    if (!isNull(envir) && !isEnvironment(envir))
	errorcall(call, "invalid environment");

    n = length(arglist);
    if (n < 1)
	errorcall(call, "argument must have length at least 1");
    names = getAttrib(arglist, R_NamesSymbol);
    PROTECT(pargs = args = allocList(n - 1));
    for (i = 0; i < n - 1; i++) {
	SETCAR(pargs, VECTOR_ELT(arglist, i));
	if (names != R_NilValue && *CHAR(STRING_ELT(names, i)) != '\0')
	    SET_TAG(pargs, install(CHAR(STRING_ELT(names, i))));
	else
	    SET_TAG(pargs, R_NilValue);
	pargs = CDR(pargs);
    }
    CheckFormals(args);
    args =  mkCLOSXP(args, VECTOR_ELT(arglist, n - 1), envir);
    UNPROTECT(1);
    return args;
}


SEXP do_ascall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ap, ans, names;
    int i, n;
    checkArity(op, args);
    args = CAR(args);
    switch (TYPEOF(args)) {
    case LANGSXP:
	ans = args;
	break;
    case VECSXP:
    case EXPRSXP:
	if(0 == (n = length(args)))
	    errorcall(call,"illegal length 0 argument");
	names = getAttrib(args, R_NamesSymbol);
	PROTECT(ap = ans = allocList(n));
	for (i = 0; i < n; i++) {
	    SETCAR(ap, VECTOR_ELT(args, i));
	    if (names != R_NilValue && !StringBlank(STRING_ELT(names, i)))
		SET_TAG(ap, install(CHAR(STRING_ELT(names, i))));
	    ap = CDR(ap);
	}
	UNPROTECT(1);
	break;
    case LISTSXP:
	ans = duplicate(args);
	break;
    default:
	errorcall(call, "invalid argument list");
	ans = R_NilValue;
    }
    SET_TYPEOF(ans, LANGSXP);
    SET_TAG(ans, R_NilValue);
    return ans;
}

/* return the type (= "detailed mode") of the SEXP */
SEXP do_typeof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    checkArity(op, args);
    PROTECT(ans = allocVector(STRSXP, 1));
    SET_STRING_ELT(ans,0, type2str(TYPEOF(CAR(args))));
    UNPROTECT(1);
    return ans;
}

/* Define many of the <primitive> "is.xxx" functions :
   Note that  isNull, isNumeric, etc are defined in ./util.c
*/
SEXP do_is(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    checkArity(op, args);

    if( isObject(CAR(args)) && DispatchOrEval(call, op,
	       CHAR(PRINTNAME(CAR(call))), args, rho, &ans, 0,1))
	return(ans);

    PROTECT(ans = allocVector(LGLSXP, 1));
    switch (PRIMVAL(op)) {
    case NILSXP:	/* is.null */
	LOGICAL(ans)[0] = isNull(CAR(args));
	break;
    case LGLSXP:	/* is.logical */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == LGLSXP);
	break;
    case INTSXP:	/* is.integer */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == INTSXP);
	break;
    case REALSXP:	/* is.double */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == REALSXP);
	break;
    case CPLXSXP:	/* is.complex */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == CPLXSXP);
	break;
    case STRSXP:	/* is.character */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == STRSXP);
	break;
    case SYMSXP:	/* is.symbol === is.name */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP);
	break;
    case ENVSXP:	/* is.environment */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == ENVSXP);
	break;
    case VECSXP:	/* is.list */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == VECSXP ||
			   TYPEOF(CAR(args)) == LISTSXP);
	break;
    case LISTSXP:	/* is.pairlist */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == LISTSXP ||
			   TYPEOF(CAR(args)) == NILSXP);/* pairlist() -> NULL */
	break;
    case EXPRSXP:	/* is.expression */
	LOGICAL(ans)[0] = TYPEOF(CAR(args)) == EXPRSXP;
	break;

    case 50:		/* is.object */
	LOGICAL(ans)[0] = OBJECT(CAR(args));
	break;
    case 80:
	LOGICAL(ans)[0] = isFrame(CAR(args));
	break;

    case 100:		/* is.numeric */
	LOGICAL(ans)[0] = (isNumeric(CAR(args)) &&
			   !isLogical(CAR(args)));
	break;
    case 101:		/* is.matrix */
	LOGICAL(ans)[0] = isMatrix(CAR(args));
	break;
    case 102:		/* is.array */
	LOGICAL(ans)[0] = isArray(CAR(args));
	break;

    case 200:		/* is.atomic */
	switch(TYPEOF(CAR(args))) {
	case NILSXP:
	    /* NULL is atomic (S compatibly), but not in isVectorAtomic(.) */
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	    LOGICAL(ans)[0] = 1;
	    break;
	default:
	    LOGICAL(ans)[0] = 0;
	    break;
	}
	break;
    case 201:		/* is.recursive */
	switch(TYPEOF(CAR(args))) {
	case VECSXP:
	case LISTSXP:
	case CLOSXP:
	case ENVSXP:
	case PROMSXP:
	case LANGSXP:
	case SPECIALSXP:
	case BUILTINSXP:
	case DOTSXP:
	case ANYSXP:
	case EXPRSXP:
	case EXTPTRSXP:
#ifdef BYTECODE
	case BCODESXP:
#endif
	case WEAKREFSXP:
	    LOGICAL(ans)[0] = 1;
	    break;
	default:
	    LOGICAL(ans)[0] = 0;
	    break;
	}
	break;

    case 300:		/* is.call */
	LOGICAL(ans)[0] = TYPEOF(CAR(args)) == LANGSXP;
	break;
    case 301:		/* is.language */
	LOGICAL(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP ||
			   TYPEOF(CAR(args)) == LANGSXP ||
			   TYPEOF(CAR(args)) == EXPRSXP);
	break;
    case 302:		/* is.function */
	LOGICAL(ans)[0] = isFunction(CAR(args));
	break;

    case 999:		/* is.single */
	errorcall(call, "type \"single\" unimplemented in R");
    default:
	errorcall(call, "unimplemented predicate");
    }
    UNPROTECT(1);
    return (ans);
}

/* What should is.vector do ?
 * In S, if an object has no attributes it is a vector, otherwise it isn't.
 * It seems to make more sense to check for a dim attribute.
 */

SEXP do_isvector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, a;
    checkArity(op, args);
    if (!isString(CADR(args)) || LENGTH(CADR(args)) <= 0)
	errorcall_return(call, R_MSG_mode);

    PROTECT(ans = allocVector(LGLSXP, 1));
    if (streql(CHAR(STRING_ELT(CADR(args), 0)), "any")) {
	LOGICAL(ans)[0] = isVector(CAR(args));/* from ./util.c */
    }
    else if (streql(CHAR(STRING_ELT(CADR(args), 0)), "numeric")) {
	LOGICAL(ans)[0] = (isNumeric(CAR(args)) &&
			   !isLogical(CAR(args)));
    }
    else if (streql(CHAR(STRING_ELT(CADR(args), 0)),
		    CHAR(type2str(TYPEOF(CAR(args)))))) {
	LOGICAL(ans)[0] = 1;
    }
    else
	LOGICAL(ans)[0] = 0;

    /* We allow a "names" attribute on any vector. */
    if (LOGICAL(ans)[0] && ATTRIB(CAR(args)) != R_NilValue) {
	a = ATTRIB(CAR(args));
	while(a != R_NilValue) {
	    if (TAG(a) != R_NamesSymbol) {
		LOGICAL(ans)[0] = 0;
		break;
	    }
	    a = CDR(a);
	}
    }
    UNPROTECT(1);
    return (ans);
}

SEXP do_isna(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, names, x;
    int i, n;

    if (DispatchOrEval(call, op, "is.na", args, rho, &ans, 1, 1))
	return(ans);
    PROTECT(args = ans);
    checkArity(op, args);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.na " R_MSG_list_vec);

#endif
    x = CAR(args);
    n = length(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    if (isVector(x)) {
	PROTECT(dims = getAttrib(x, R_DimSymbol));
	if (isArray(x))
	    PROTECT(names = getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = getAttrib(x, R_NamesSymbol));
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (INTEGER(x)[i] == NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = ISNAN(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (ISNAN(COMPLEX(x)[i].r) ||
			       ISNAN(COMPLEX(x)[i].i));
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (STRING_ELT(x, i) == NA_STRING);
	break;

/* Same code for LISTSXP and VECSXP : */
#define LIST_VEC_NA(s)							\
	if (!isVector(s) || length(s) != 1)				\
		LOGICAL(ans)[i] = 0;					\
	else {								\
		switch (TYPEOF(s)) {					\
		case LGLSXP:						\
		case INTSXP:						\
		    LOGICAL(ans)[i] = (INTEGER(s)[0] == NA_INTEGER);	\
		    break;						\
		case REALSXP:						\
		    LOGICAL(ans)[i] = ISNAN(REAL(s)[0]);		\
		    break;						\
		case STRSXP:						\
		    LOGICAL(ans)[i] = (STRING_ELT(s, 0) == NA_STRING);	\
		    break;						\
		case CPLXSXP:						\
		    LOGICAL(ans)[i] = (ISNAN(COMPLEX(s)[0].r)		\
				       || ISNAN(COMPLEX(s)[0].i));	\
		    break;						\
		}							\
	}

    case LISTSXP:
	for (i = 0; i < n; i++) {
	    LIST_VEC_NA(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    SEXP s = VECTOR_ELT(x, i);
	    LIST_VEC_NA(s);
	}
	break;
    default:
	warningcall(call, "is.na" R_MSG_list_vec2);
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
    }
    if (dims != R_NilValue)
	setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (isArray(x))
	    setAttrib(ans, R_DimNamesSymbol, names);
	else
	    setAttrib(ans, R_NamesSymbol, names);
    }
    if (isVector(x))
	UNPROTECT(2);
    UNPROTECT(1);
    UNPROTECT(1); /*ans*/
    return ans;
}

SEXP do_isnan(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, names, x;
    int i, n;

    if (DispatchOrEval(call, op, "is.nan", args, rho, &ans, 1, 1))
	return(ans);

    PROTECT(args = ans);
    checkArity(op, args);

#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.nan " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = length(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    if (isVector(x)) {
	PROTECT(dims = getAttrib(x, R_DimSymbol));
	if (isArray(x))
	    PROTECT(names = getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = getAttrib(x, R_NamesSymbol));
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case STRSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = R_IsNaN(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (R_IsNaN(COMPLEX(x)[i].r) ||
			       R_IsNaN(COMPLEX(x)[i].i));
	break;

/* Same code for LISTSXP and VECSXP : */

#define LIST_VEC_NAN(s)							\
	if (!isVector(s) || length(s) != 1)				\
		LOGICAL(ans)[i] = 0;					\
	else {								\
		switch (TYPEOF(s)) {					\
		case LGLSXP:						\
		case INTSXP:						\
		case STRSXP:						\
		    LOGICAL(ans)[i] = 0;				\
		    break;						\
		case REALSXP:						\
		    LOGICAL(ans)[i] = R_IsNaN(REAL(s)[0]);		\
		    break;						\
		case CPLXSXP:						\
		    LOGICAL(ans)[i] = (R_IsNaN(COMPLEX(s)[0].r) ||	\
				       R_IsNaN(COMPLEX(s)[0].i));	\
		    break;						\
		}							\
	}

    case LISTSXP:
	for (i = 0; i < n; i++) {
	    LIST_VEC_NAN(CAR(x));
	    x = CDR(x);
	}
	break;
    case VECSXP:
	for (i = 0; i < n; i++) {
	    SEXP s = VECTOR_ELT(x, i);
	    LIST_VEC_NAN(s);
	}
	break;
    default:
	warningcall(call, "is.nan" R_MSG_list_vec2);
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
    }
    if (dims != R_NilValue)
	setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (isArray(x))
	    setAttrib(ans, R_DimNamesSymbol, names);
	else
	    setAttrib(ans, R_NamesSymbol, names);
    }
    if (isVector(x))
	UNPROTECT(2);
    UNPROTECT(1);
    UNPROTECT(1); /*ans*/
    return ans;
}

SEXP do_isfinite(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, names, dims;
    int i, n;
    checkArity(op, args);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.finite " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = length(x);
    ans = allocVector(LGLSXP, n);
    if (isVector(x)) {
	dims = getAttrib(x, R_DimSymbol);
	if (isArray(x))
	    names = getAttrib(x, R_DimNamesSymbol);
	else
	    names = getAttrib(x, R_NamesSymbol);
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (INTEGER(x)[i] != NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = R_FINITE(REAL(x)[i]);
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = (R_FINITE(COMPLEX(x)[i].r) && R_FINITE(COMPLEX(x)[i].i));
	break;
    default:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
    }
    if (dims != R_NilValue)
	setAttrib(ans, R_DimSymbol, dims);
    if (names != R_NilValue) {
	if (isArray(x))
	    setAttrib(ans, R_DimNamesSymbol, names);
	else
	    setAttrib(ans, R_NamesSymbol, names);
    }
    return ans;
}

SEXP do_isinfinite(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, names, dims;
    double xr, xi;
    int i, n;
    checkArity(op, args);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.infinite " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = length(x);
    ans = allocVector(LGLSXP, n);
    if (isVector(x)) {
	dims = getAttrib(x, R_DimSymbol);
	if (isArray(x))
	    names = getAttrib(x, R_DimNamesSymbol);
	else
	    names = getAttrib(x, R_NamesSymbol);
    }
    else	dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case REALSXP:
	for (i = 0; i < n; i++) {
	    xr = REAL(x)[i];
	    if (ISNAN(xr) || R_FINITE(xr))
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    xr = COMPLEX(x)[i].r;
	    xi = COMPLEX(x)[i].i;
	    if ((ISNAN(xr) || R_FINITE(xr)) && (ISNAN(xi) || R_FINITE(xi)))
		LOGICAL(ans)[i] = 0;
	    else
		LOGICAL(ans)[i] = 1;
	}
	break;
    default:
	for (i = 0; i < n; i++)
	    LOGICAL(ans)[i] = 0;
    }
    if (!isNull(dims))
	setAttrib(ans, R_DimSymbol, dims);
    if (!isNull(names)) {
	if (isArray(x))
	    setAttrib(ans, R_DimNamesSymbol, names);
	else
	    setAttrib(ans, R_NamesSymbol, names);
    }
    return ans;
}

SEXP do_call(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rest, evargs, rfun;

    PROTECT(rfun = eval(CAR(args), rho));
    if (!isString(rfun) || length(rfun) <= 0 ||
	streql(CHAR(STRING_ELT(rfun, 0)), ""))
	errorcall_return(call, R_MSG_A1_char);
    PROTECT(rfun = install(CHAR(STRING_ELT(rfun, 0))));
    PROTECT(evargs = duplicate(CDR(args)));
    for (rest = evargs; rest != R_NilValue; rest = CDR(rest))
	SETCAR(rest, eval(CAR(rest), rho));
    rfun = LCONS(rfun, evargs);
    UNPROTECT(3);
    return (rfun);
}

SEXP do_docall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP c, fun, names;
    int i, n;
    RCNTXT *cptr;

    checkArity(op, args);

    fun = CAR(args);
    args = CADR(args);

    if (!isString(fun) || length(fun) <= 0 || CHAR(STRING_ELT(fun, 0)) == '\0')
	errorcall_return(call, R_MSG_A1_char);

    if (!isNull(args) && !isNewList(args))
	errorcall_return(call, R_MSG_A2_list);
    n = length(args);
    names = getAttrib(args, R_NamesSymbol);

    PROTECT(c = call = allocList(n + 1));
    SET_TYPEOF(c, LANGSXP);
    SETCAR(c, install(CHAR(STRING_ELT(fun, 0))));
    c = CDR(c);
    for (i = 0; i < n; i++) {
#ifndef NEW
	SETCAR(c, VECTOR_ELT(args, i));
#else
	SETCAR(c, mkPROMISE(VECTOR_ELT(args, i), rho));
	SET_PRVALUE(CAR(c), VECTOR_ELT(args, i));
#endif
	if (ItemName(names, i) != R_NilValue)
	    SET_TAG(c, install(CHAR(ItemName(names, i))));
	c = CDR(c);
    }
    cptr = R_GlobalContext;
    while (cptr->nextcontext != NULL) {
        if (cptr->callflag & CTXT_FUNCTION ) {
		if(cptr->cloenv == rho)
		   break;
	}
    }
    if( cptr->cloenv == rho )
    	call = eval(call, cptr->sysparent);
    else
        error("do.call: couldn't find parent environment");
    UNPROTECT(1);
    return call;
}


/* do_substitute has two arguments, an expression and an environment */
/* (optional).	Symbols found in the expression are substituted with their */
/* values as found in the environment.	There is no inheritance so only */
/* the supplied environment is searched. If no environment is specified */
/* the environment in which substitute was called is used.  If the */
/* specified environment is R_NilValue then R_GlobalEnv is used. */
/* Arguments to do_substitute should not be evaluated. */

SEXP substitute(SEXP lang, SEXP rho)
{
    SEXP t;
    switch (TYPEOF(lang)) {
    case PROMSXP:
	return substitute(PREXPR(lang), rho);
    case SYMSXP:
	t = findVarInFrame3( rho, lang, TRUE);
	if (t != R_UnboundValue) {
	    if (TYPEOF(t) == PROMSXP) {
		do {
		    t = PREXPR(t);
		}
		while(TYPEOF(t) == PROMSXP);
		return t;
	    }
	    else if (TYPEOF(t) == DOTSXP) {
		error("... used in an incorrect context");
	    }
	    if (rho != R_GlobalEnv)
		return t;
	}
	return (lang);
    case LANGSXP:
	return substituteList(lang, rho);
    default:
	return (lang);
    }
}


/* Work through a list doing substitute on the */
/* elements taking particular care to handle ... */

SEXP substituteList(SEXP el, SEXP rho)
{
    SEXP h, t;
    if (isNull(el))
	return el;
    if (CAR(el) == R_DotsSymbol) {
	h = findVarInFrame3(rho, CAR(el), TRUE);
	if (h == R_NilValue)
	    return substituteList(CDR(el), rho);
	if (TYPEOF(h) != DOTSXP) {
	    if (h == R_UnboundValue)
		return el;
	    if (h == R_MissingArg)
		return substituteList(CDR(el), rho);
	    error("... used in an incorrect context");
	}
	PROTECT(h = substituteList(h, R_NilValue));
	PROTECT(t = substituteList(CDR(el), rho));
	t = listAppend(h, t);
	UNPROTECT(2);
	return t;
    }
    else {
	/* This could involve deep recursion on long lists, so do tail
	 * first to avoid overflowing the protect stack */
	PROTECT(t = substituteList(CDR(el), rho));
	PROTECT(h = substitute(CAR(el), rho));
	if (isLanguage(el))
	    t = LCONS(h, t);
	else
	    t = CONS(h, t);
	SET_TAG(t, TAG(el));
	UNPROTECT(2);
	return t;
    }
}

SEXP do_substitute(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP env, s, t;
    /* set up the environment for substitution */
    if (length(args) == 1)
	env = rho;
    else
	env = eval(CADR(args), rho);
    if (env == R_NilValue)
	env = R_GlobalEnv;
    else if (TYPEOF(env) == VECSXP)
	env = NewEnvironment(R_NilValue, VectorToPairList(env), R_NilValue);
    else if (TYPEOF(env) == LISTSXP)
	env = NewEnvironment(R_NilValue, duplicate(env), R_NilValue);
    if (TYPEOF(env) != ENVSXP)
	errorcall(call, "invalid environment specified");

    PROTECT(env);
    PROTECT(t = duplicate(args));
    SETCDR(t, R_NilValue);
    s = substituteList(t, env);
    UNPROTECT(2);
    return CAR(s);
}

SEXP do_quote(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    return(CAR(args));
}

typedef struct {
    char *s;
    SEXPTYPE sexp;
    Rboolean canChange;
} classType;

static classType classTable[] = {
    { "logical",	LGLSXP,	   TRUE },
    { "integer",	INTSXP,	   TRUE },
    { "double",		REALSXP,   TRUE },
    { "complex",	CPLXSXP,   TRUE },
    { "character",	STRSXP,	   TRUE },
    { "expression",	EXPRSXP,   TRUE },
    { "list",		VECSXP,	   TRUE },
    { "environment",    ENVSXP,    FALSE },
    { "char",		CHARSXP,   TRUE },
    { "externalptr",	EXTPTRSXP,  FALSE },
    { "weakref",	WEAKREFSXP, FALSE },
    { "name",		SYMSXP,	   FALSE },

    { (char *)0,	(SEXPTYPE)-1, FALSE}
};

static int class2type(char *s)
{
    /* return the type if the class string is one of the basic types, else -1.
       Note that this is NOT str2type:  only certain types are defined to be basic
       classes; e.g., "language" is a type but many classes correspond to objects of
       this type.
    */
    int i; char *si;
    for(i=0; ; i++) {
	si = classTable[i].s;
	if(!si)
	    return -1;
	if(!strcmp(s, si))
	    return i;
    }
    return -1;
}

/* set the class to value, and return the modified object.  This is
   NOT a primitive assignment operator , because there is no code in R
   that changes type in place. See the definition of "class<-" in the methods
   package for the use of this code. */
SEXP R_set_class(SEXP obj, SEXP value, SEXP call)
{
    int nProtect = 0;
    if(isNull(value)) {
	setAttrib(obj, R_ClassSymbol, value);
	return obj;
    }
    if(TYPEOF(value) != STRSXP) {
	PROTECT(value = coerceVector(duplicate(value), STRSXP));
	nProtect++;
    }
    if(length(value) > 1)
	setAttrib(obj, R_ClassSymbol, value);
    else if(length(value) == 0) {
	UNPROTECT(nProtect); nProtect = 0;
	error("Invalid replacement object to be a class string");
    }
    else {
	char *valueString, *classString; int whichType;
	SEXP cur_class; SEXPTYPE valueType;
	valueString = CHAR(asChar(value));
	whichType = class2type(valueString);
	valueType = (whichType == -1) ? -1 : classTable[whichType].sexp;
	PROTECT(cur_class = R_data_class(obj, FALSE)); nProtect++;
	classString = CHAR(asChar(cur_class));
	/*  assigning type as a class deletes an explicit class attribute. */
	if(valueType != -1) {
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(classTable[whichType].canChange) {
		PROTECT(obj = ascommon(call, obj, valueType));
		nProtect++;
	    }
	    else if(valueType != TYPEOF(obj))
		error("\"%s\" can only be set as the class if the object has this type; found \"%s\"",
		      valueString, CHAR(type2str(TYPEOF(obj))));
	    /* else, leave alone */
	}
	else if(!strcmp("numeric", valueString)) {
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    switch(TYPEOF(obj)) {
	    case INTSXP: case REALSXP: break;
	    default: PROTECT(obj = coerceVector(obj, REALSXP));
		nProtect++;
	    }
	}
	/* the next 2 special cases mirror the special code in
	 * R_data_class */
	else if(!strcmp("matrix", valueString)) {
	    if(length(getAttrib(obj, R_DimSymbol)) != 2)
	        error("Invalid to set the class to matrix unless the dimension attribute is of length 2 (was %d)",
		 length(getAttrib(obj, R_DimSymbol)));
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	}
	else if(!strcmp("array", valueString)) {
	    if(length(getAttrib(obj, R_DimSymbol))<= 0)
	        error("Can't set class to \"array\" unless the dimension attribute has length > 0");
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	}
	else { /* set the class but don't do the coercion; that's
		  supposed to be done by an as() method */
	    setAttrib(obj, R_ClassSymbol, value);
	}
    }
    UNPROTECT(nProtect);
    return obj;
}
