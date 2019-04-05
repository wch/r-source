/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2019  The R Core Team
 *  Copyright (C) 2003-2019  The R Foundation
 *  Copyright (C) 1995,1996  Robert Gentleman, Ross Ihaka
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* interval at which to check interrupts */
#define NINTERRUPT 10000000

#include <Defn.h> /*-- Maybe modularize into own Coerce.h ..*/
#include <Internal.h>
#include <float.h> /* for DBL_DIG */
#define R_MSG_mode	_("invalid 'mode' argument")
#define R_MSG_list_vec	_("applies only to lists and vectors")
#include <Rmath.h>
#include <Print.h>


/* This section of code handles type conversion for elements */
/* of data vectors.  Type coercion throughout R should use these */
/* routines to ensure consistency. */

/* Coercion warnings will be OR'ed : */
#define WARN_NA	   1
#define WARN_INT_NA 2
#define WARN_IMAG  4
#define WARN_RAW  8

/* The following two macros copy or clear the attributes.  They also
   ensure that the object bit is properly set.  They avoid calling the
   assignment functions when possible, since the write barrier (and
   possibly cache behavior on some architectures) makes assigning more
   costly than dereferencing. */
#define SHALLOW_DUPLICATE_ATTRIB(to, from) do {\
  SEXP __from__ = (from); \
  if (ATTRIB(__from__) != R_NilValue) { \
    SEXP __to__ = (to); \
    (SHALLOW_DUPLICATE_ATTRIB)(__to__, __from__);	\
  } \
} while (0)

#define CLEAR_ATTRIB(x) do {\
  SEXP __x__ = (x); \
  if (ATTRIB(__x__) != R_NilValue) { \
    SET_ATTRIB(__x__, R_NilValue); \
    if (OBJECT(__x__)) SET_OBJECT(__x__, 0); \
    if (IS_S4_OBJECT(__x__)) UNSET_S4_OBJECT(__x__); \
  } \
} while (0)

void attribute_hidden CoercionWarning(int warn)
{
/* FIXME: Use
   =====
   WarningMessage(R_NilValue, WARNING_....);
*/
    if (warn & WARN_NA)
	warning(_("NAs introduced by coercion"));
    if (warn & WARN_INT_NA)
	warning(_("NAs introduced by coercion to integer range"));
    if (warn & WARN_IMAG)
	warning(_("imaginary parts discarded in coercion"));
    if (warn & WARN_RAW)
	warning(_("out-of-range values treated as 0 in coercion to raw"));
}

int attribute_hidden
LogicalFromInteger(int x, int *warn)
{
    return (x == NA_INTEGER) ?
	NA_LOGICAL : (x != 0);
}

int attribute_hidden
LogicalFromReal(double x, int *warn)
{
    return ISNAN(x) ?
	NA_LOGICAL : (x != 0);
}

int attribute_hidden
LogicalFromComplex(Rcomplex x, int *warn)
{
    return (ISNAN(x.r) || ISNAN(x.i)) ?
	NA_LOGICAL : (x.r != 0 || x.i != 0);
}

int attribute_hidden
LogicalFromString(SEXP x, int *warn)
{
    if (x != R_NaString) {
	if (StringTrue(CHAR(x))) return 1;
	if (StringFalse(CHAR(x))) return 0;
    }
    return NA_LOGICAL;
}

int attribute_hidden
IntegerFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_INTEGER : x;
}

int attribute_hidden
IntegerFromReal(double x, int *warn)
{
    if (ISNAN(x))
	return NA_INTEGER;
    else if (x >= INT_MAX+1. || x <= INT_MIN ) {
	*warn |= WARN_INT_NA;
	return NA_INTEGER;
    }
    return (int) x;
}

int attribute_hidden
IntegerFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_INTEGER;
    else if (x.r > INT_MAX+1. || x.r <= INT_MIN ) {
	*warn |= WARN_INT_NA;
	return NA_INTEGER;;
    }
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return (int) x.r;
}


int attribute_hidden
IntegerFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !isBlankString(CHAR(x))) { /* ASCII */
	xdouble = R_strtod(CHAR(x), &endp); /* ASCII */
	if (isBlankString(endp)) {
#ifdef _R_pre_Version_3_3_0
	    if (xdouble > INT_MAX) {
		*warn |= WARN_INT_NA;
		return INT_MAX;
	    }
	    else if(xdouble < INT_MIN+1) {
		*warn |= WARN_INT_NA;
		return INT_MIN;// <- "wrong" as INT_MIN == NA_INTEGER currently; should have used INT_MIN+1
	    }
#else
	    // behave the same as IntegerFromReal() etc:
	    if (xdouble >= INT_MAX+1. || xdouble <= INT_MIN ) {
		*warn |= WARN_INT_NA;
		return NA_INTEGER;
	    }
#endif
	    else
		return (int) xdouble;
	}
	else *warn |= WARN_NA;
    }
    return NA_INTEGER;
}

double attribute_hidden
RealFromLogical(int x, int *warn)
{
    return (x == NA_LOGICAL) ?
	NA_REAL : x;
}

double attribute_hidden
RealFromInteger(int x, int *warn)
{
    if (x == NA_INTEGER)
	return NA_REAL;
    else
	return x;
}

double attribute_hidden
RealFromComplex(Rcomplex x, int *warn)
{
    if (ISNAN(x.r) || ISNAN(x.i))
	return NA_REAL;
    if (ISNAN(x.r)) return x.r;
    if (ISNAN(x.i)) return NA_REAL;
    if (x.i != 0)
	*warn |= WARN_IMAG;
    return x.r;
}

double attribute_hidden
RealFromString(SEXP x, int *warn)
{
    double xdouble;
    char *endp;
    if (x != R_NaString && !isBlankString(CHAR(x))) { /* ASCII */
	xdouble = R_strtod(CHAR(x), &endp); /* ASCII */
	if (isBlankString(endp))
	    return xdouble;
	else
	    *warn |= WARN_NA;
    }
    return NA_REAL;
}

Rcomplex attribute_hidden
ComplexFromLogical(int x, int *warn)
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

Rcomplex attribute_hidden
ComplexFromInteger(int x, int *warn)
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

Rcomplex attribute_hidden
ComplexFromReal(double x, int *warn)
{
    Rcomplex z;
#ifdef PRE_R_3_3_0
    if (ISNAN(x)) {
	z.r = NA_REAL;
	z.i = NA_REAL;
    }
    else {
#endif
	z.r = x;
	z.i = 0;
#ifdef PRE_R_3_3_0
    }
#endif
    return z;
}

Rcomplex attribute_hidden
ComplexFromString(SEXP x, int *warn)
{
    double xr, xi;
    Rcomplex z;
    const char *xx = CHAR(x); /* ASCII */
    char *endp;

    z.r = z.i = NA_REAL;
    if (x != R_NaString && !isBlankString(xx)) {
	xr = R_strtod(xx, &endp);
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

SEXP attribute_hidden StringFromLogical(int x, int *warn)
{
    int w;
    formatLogical(&x, 1, &w);
    if (x == NA_LOGICAL) return NA_STRING;
    else return mkChar(EncodeLogical(x, w));
}

/* The conversions for small non-negative integers are saved in a chache. */
#define SFI_CACHE_SIZE 512
static SEXP sficache = NULL;

SEXP attribute_hidden StringFromInteger(int x, int *warn)
{
    if (x == NA_INTEGER) return NA_STRING;
    else if (x >= 0 && x < SFI_CACHE_SIZE) {
	if (sficache == NULL) {
	    sficache = allocVector(STRSXP, SFI_CACHE_SIZE);
	    R_PreserveObject(sficache);
	}
	SEXP cval = STRING_ELT(sficache, x);
	if (cval == R_BlankString) {
	    int w;
	    formatInteger(&x, 1, &w);
	    cval = mkChar(EncodeInteger(x, w));
	    SET_STRING_ELT(sficache, x, cval);
	}
	return cval;
    }
    else {
	int w;
	formatInteger(&x, 1, &w);
	return mkChar(EncodeInteger(x, w));
    }
}

// dropTrailing0 and StringFromReal moved to printutils.c

SEXP attribute_hidden StringFromComplex(Rcomplex x, int *warn)
{
    int wr, dr, er, wi, di, ei;
    formatComplex(&x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
    if (ISNA(x.r) || ISNA(x.i)) // "NA" if Re or Im is (but not if they're just NaN)
	return NA_STRING;
    else /* EncodeComplex has its own anti-trailing-0 care :*/
	return mkChar(EncodeComplex(x, wr, dr, er, wi, di, ei, OutDec));
}

static SEXP StringFromRaw(Rbyte x, int *warn)
{
    char buf[3];
    sprintf(buf, "%02x", x);
    return mkChar(buf);
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
    for (i = 0, xptr = x; i < len; i++, xptr = CDR(xptr)) {
	RAISE_NAMED(CAR(xptr), NAMED(x));
	SET_VECTOR_ELT(xnew, i, CAR(xptr));
    }
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
    PROTECT(xnew = allocList(len)); /* limited to int */
    PROTECT(xnames = getAttrib(x, R_NamesSymbol));
    named = (xnames != R_NilValue);
    xptr = xnew;
    for (i = 0; i < len; i++) {
	RAISE_NAMED(VECTOR_ELT(x, i), NAMED(x));
	SETCAR(xptr, VECTOR_ELT(x, i));
	if (named && CHAR(STRING_ELT(xnames, i))[0] != '\0') /* ASCII */
	    SET_TAG(xptr, installTrChar(STRING_ELT(xnames, i)));
	xptr = CDR(xptr);
    }
    if (len > 0)       /* can't set attributes on NULL */
	copyMostAttrib(x, xnew);
    UNPROTECT(3);
    return xnew;
}

static SEXP coerceToSymbol(SEXP v)
{
    SEXP ans = R_NilValue;
    int warn = 0;
    if (length(v) <= 0)
	error(_("invalid data of mode '%s' (too short)"),
	      type2char(TYPEOF(v)));
    PROTECT(v);
    switch(TYPEOF(v)) {
    case LGLSXP:
	ans = StringFromLogical(LOGICAL_ELT(v, 0), &warn);
	break;
    case INTSXP:
	ans = StringFromInteger(INTEGER_ELT(v, 0), &warn);
	break;
    case REALSXP:
	ans = StringFromReal(REAL_ELT(v, 0), &warn);
	break;
    case CPLXSXP:
	ans = StringFromComplex(COMPLEX_ELT(v, 0), &warn);
	break;
    case STRSXP:
	ans = STRING_ELT(v, 0);
	break;
    case RAWSXP:
	ans = StringFromRaw(RAW_ELT(v, 0), &warn);
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToSymbol", v);
    }
    PROTECT(ans);
    if (warn) CoercionWarning(warn);/*2000/10/23*/
    ans = installTrChar(ans);
    UNPROTECT(2); /* ans, v */
    return ans;
}

static SEXP coerceToLogical(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = allocVector(LGLSXP, n = XLENGTH(v)));
    int *pa = LOGICAL(ans);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = LogicalFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = LogicalFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = LogicalFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = LogicalFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = LogicalFromInteger((int)RAW_ELT(v, i), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToLogical", v);
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToInteger(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = allocVector(INTSXP, n = XLENGTH(v)));
    int *pa = INTEGER(ans);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = IntegerFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = IntegerFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = IntegerFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = IntegerFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = (int)RAW_ELT(v, i);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToInteger", v);
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToReal(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = allocVector(REALSXP, n = XLENGTH(v)));
    double *pa = REAL(ans);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = RealFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = RealFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = RealFromComplex(COMPLEX_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = RealFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = RealFromInteger((int)RAW_ELT(v, i), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToReal", v);
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToComplex(SEXP v)
{
    SEXP ans;
    int warn = 0;
    R_xlen_t i, n;
    PROTECT(ans = allocVector(CPLXSXP, n = XLENGTH(v)));
    Rcomplex *pa = COMPLEX(ans);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = ComplexFromLogical(LOGICAL_ELT(v, i), &warn);
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = ComplexFromInteger(INTEGER_ELT(v, i), &warn);
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = ComplexFromReal(REAL_ELT(v, i), &warn);
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = ComplexFromString(STRING_ELT(v, i), &warn);
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    pa[i] = ComplexFromInteger((int)RAW_ELT(v, i), &warn);
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToComplex", v);
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToRaw(SEXP v)
{
    SEXP ans;
    int warn = 0, tmp;
    R_xlen_t i, n;

    PROTECT(ans = allocVector(RAWSXP, n = XLENGTH(v)));
    Rbyte *pa = RAW(ans);
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = IntegerFromLogical(LOGICAL_ELT(v, i), &warn);
	    if(tmp == NA_INTEGER) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = (Rbyte) tmp;
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = INTEGER_ELT(v, i);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = (Rbyte) tmp;
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = IntegerFromReal(REAL_ELT(v, i), &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = (Rbyte) tmp;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = IntegerFromComplex(COMPLEX_ELT(v, i), &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = (Rbyte) tmp;
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    tmp = IntegerFromString(STRING_ELT(v, i), &warn);
	    if(tmp == NA_INTEGER || tmp < 0 || tmp > 255) {
		tmp = 0;
		warn |= WARN_RAW;
	    }
	    pa[i] = (Rbyte) tmp;
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToRaw", v);
    }
    if (warn) CoercionWarning(warn);
    UNPROTECT(1);
    return ans;
}

static SEXP coerceToString(SEXP v)
{
    SEXP ans;
    int savedigits, warn = 0;
    R_xlen_t i, n;

    PROTECT(ans = allocVector(STRSXP, n = XLENGTH(v)));
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    SHALLOW_DUPLICATE_ATTRIB(ans, v);
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromLogical(LOGICAL_ELT(v, i), &warn));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromInteger(INTEGER_ELT(v, i), &warn));
	}
	break;
    case REALSXP:
	PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromReal(REAL_ELT(v, i), &warn));
	}
	R_print.digits = savedigits;
	break;
    case CPLXSXP:
	PrintDefaults();
	savedigits = R_print.digits; R_print.digits = DBL_DIG;/* MAX precision */
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromComplex(COMPLEX_ELT(v, i), &warn));
	}
	R_print.digits = savedigits;
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_STRING_ELT(ans, i, StringFromRaw(RAW_ELT(v, i), &warn));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("coerceToString", v);
    }
    if (warn) CoercionWarning(warn);/*2000/10/23*/
    UNPROTECT(1);
    return (ans);
}

static SEXP coerceToExpression(SEXP v)
{
    SEXP ans;
    R_xlen_t i, n;
    if (isVectorAtomic(v)) {
	n = XLENGTH(v);
	PROTECT(ans = allocVector(EXPRSXP, n));
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
	switch (TYPEOF(v)) {
	case LGLSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarLogical(LOGICAL_ELT(v, i)));
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarInteger(INTEGER_ELT(v, i)));
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarReal(REAL_ELT(v, i)));
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarComplex(COMPLEX_ELT(v, i)));
	    break;
	case STRSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++)
		SET_VECTOR_ELT(ans, i, ScalarRaw(RAW_ELT(v, i)));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToExpression", v);
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
    R_xlen_t i, n;
    n = xlength(v);
    PROTECT(ans = allocVector(VECSXP, n));
#ifdef R_MEMORY_PROFILING
    if (RTRACE(v)){
       memtrace_report(v,ans);
       SET_RTRACE(ans,1);
    }
#endif
    switch (TYPEOF(v)) {
    case LGLSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarLogical(LOGICAL_ELT(v, i)));
	}
	break;
    case INTSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarInteger(INTEGER_ELT(v, i)));
	}
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarReal(REAL_ELT(v, i)));
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarComplex(COMPLEX_ELT(v, i)));
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarString(STRING_ELT(v, i)));
	}
	break;
    case RAWSXP:
	for (i = 0; i < n; i++) {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    SET_VECTOR_ELT(ans, i, ScalarRaw(RAW_ELT(v, i)));
	}
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
	UNIMPLEMENTED_TYPE("coerceToVectorList", v);
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
    n = LENGTH(v); /* limited to len */
    PROTECT(ansp = ans = allocList(n));
    for (i = 0; i < n; i++) {
	switch (TYPEOF(v)) {
	case LGLSXP:
	    SETCAR(ansp, allocVector(LGLSXP, 1));
	    LOGICAL0(CAR(ansp))[0] = LOGICAL_ELT(v, i);
	    break;
	case INTSXP:
	    SETCAR(ansp, allocVector(INTSXP, 1));
	    INTEGER0(CAR(ansp))[0] = INTEGER_ELT(v, i);
	    break;
	case REALSXP:
	    SETCAR(ansp, allocVector(REALSXP, 1));
	    REAL0(CAR(ansp))[0] = REAL_ELT(v, i);
	    break;
	case CPLXSXP:
	    SETCAR(ansp, allocVector(CPLXSXP, 1));
	    COMPLEX0(CAR(ansp))[0] = COMPLEX_ELT(v, i);
	    break;
	case STRSXP:
	    SETCAR(ansp, ScalarString(STRING_ELT(v, i)));
	    break;
	case RAWSXP:
	    SETCAR(ansp, allocVector(RAWSXP, 1));
	    RAW0(CAR(ansp))[0] = RAW_ELT(v, i);
	    break;
	case VECSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	case EXPRSXP:
	    SETCAR(ansp, VECTOR_ELT(v, i));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceToPairList", v);
	}
	ansp = CDR(ansp);
    }
    ansp = getAttrib(v, R_NamesSymbol);
    if (ansp != R_NilValue)
	setAttrib(ans, R_NamesSymbol, ansp);
    UNPROTECT(1);
    return (ans);
}

/* Coerce a pairlist to the given type */
static SEXP coercePairList(SEXP v, SEXPTYPE type)
{
    /* Hmm, this is also called to LANGSXP, and coerceVector already
       did the check of TYPEOF(v) == type */
    if(type == LISTSXP) return v;/* IS pairlist */

    int i;
    SEXP rval= R_NilValue, vp;
    if (type == EXPRSXP) {
	PROTECT(rval = allocVector(type, 1));
	SET_VECTOR_ELT(rval, 0, v);
	UNPROTECT(1);
	return rval;
    }
    else if (type == STRSXP) {
	int n = length(v);
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
	int n = length(v);
	PROTECT(rval = allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		LOGICAL0(rval)[i] = asLogical(CAR(vp));
	    break;
	case INTSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		INTEGER0(rval)[i] = asInteger(CAR(vp));
	    break;
	case REALSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		REAL0(rval)[i] = asReal(CAR(vp));
	    break;
	case CPLXSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		COMPLEX0(rval)[i] = asComplex(CAR(vp));
	    break;
	case RAWSXP:
	    for (i = 0, vp = v; i < n; i++, vp = CDR(vp))
		RAW0(rval)[i] = (Rbyte) asInteger(CAR(vp));
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coercePairList", v);
	}
    }
    else
	error(_("'pairlist' object cannot be coerced to type '%s'"),
	      type2char(type));

    /* If any tags are non-null then we */
    /* need to add a names attribute. */
    for (vp = v, i = 0; vp != R_NilValue; vp = CDR(vp))
	if (TAG(vp) != R_NilValue)
	    i = 1;

    if (i) {
	int n = length(v);
	SEXP names = allocVector(STRSXP, n);
	i = 0;
	for (vp = v; vp != R_NilValue; vp = CDR(vp), i++)
	    if (TAG(vp) != R_NilValue)
		SET_STRING_ELT(names, i, PRINTNAME(TAG(vp)));
	setAttrib(rval, R_NamesSymbol, names);
    }
    UNPROTECT(1);
    return rval;
}

/* Coerce a vector list to the given type */
static SEXP coerceVectorList(SEXP v, SEXPTYPE type)
{
    int warn = 0, tmp;
    R_xlen_t i, n;
    SEXP rval, names;

    names = v;
    rval = R_NilValue;	/* -Wall */

    /* expression -> list, new in R 2.4.0 */
    if (type == VECSXP && TYPEOF(v) == EXPRSXP) {
	/* This is sneaky but saves us rewriting a lot of the duplicate code */
	rval = MAYBE_REFERENCED(v) ? duplicate(v) : v;
	SET_TYPEOF(rval, VECSXP);
	return rval;
    }

    if (type == EXPRSXP && TYPEOF(v) == VECSXP) {
	rval = MAYBE_REFERENCED(v) ? duplicate(v) : v;
	SET_TYPEOF(rval, EXPRSXP);
	return rval;
    }

    if (type == STRSXP) {
	n = xlength(v);
	PROTECT(rval = allocVector(type, n));
#ifdef R_MEMORY_PROFILING
	if (RTRACE(v)){
	   memtrace_report(v, rval);
	   SET_RTRACE(rval,1);
	}
#endif
	for (i = 0; i < n;  i++) {
	    if (isString(VECTOR_ELT(v, i)) && xlength(VECTOR_ELT(v, i)) == 1)
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
			       STRING_ELT(deparse1line_(VECTOR_ELT(v, i), 0, NICE_NAMES),
					  0));
	}
    }
    else if (type == LISTSXP) {
	rval = VectorToPairList(v);
	return rval;
    }
    else if (isVectorizable(v)) {
	n = xlength(v);
	PROTECT(rval = allocVector(type, n));
	switch (type) {
	case LGLSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		LOGICAL0(rval)[i] = asLogical(VECTOR_ELT(v, i));
	    }
	    break;
	case INTSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		INTEGER0(rval)[i] = asInteger(VECTOR_ELT(v, i));
	    }
	    break;
	case REALSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		REAL0(rval)[i] = asReal(VECTOR_ELT(v, i));
	    }
	    break;
	case CPLXSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		COMPLEX0(rval)[i] = asComplex(VECTOR_ELT(v, i));
	    }
	    break;
	case RAWSXP:
	    for (i = 0; i < n; i++) {
//		if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
		tmp = asInteger(VECTOR_ELT(v, i));
		if (tmp < 0 || tmp > 255) { /* includes NA_INTEGER */
		    tmp = 0;
		    warn |= WARN_RAW;
		}
		RAW0(rval)[i] = (Rbyte) tmp;
	    }
	    break;
	default:
	    UNIMPLEMENTED_TYPE("coerceVectorList", v);
	}
    }
    else
	error(_("(list) object cannot be coerced to type '%s'"),
	      type2char(type));

    if (warn) CoercionWarning(warn);
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
    } else if (type == CHARSXP)
	rval = PRINTNAME(v);
    else if (type == STRSXP)
	rval = ScalarString(PRINTNAME(v));
    else
	warning(_("(symbol) object cannot be coerced to type '%s'"),
		type2char(type));
    return rval;
}

SEXP coerceVector(SEXP v, SEXPTYPE type)
{
    if (TYPEOF(v) == type)
	return v;

    SEXP ans = R_NilValue;	/* -Wall */
    if (ALTREP(v)) {
	ans = ALTREP_COERCE(v, type);
	if (ans) return ans;
    }

    /* code to allow classes to extend ENVSXP, SYMSXP, etc */
    if(IS_S4_OBJECT(v) && TYPEOF(v) == S4SXP) {
	SEXP vv = R_getS4DataSlot(v, ANYSXP);
	if(vv == R_NilValue)
	  error(_("no method for coercing this S4 class to a vector"));
	else if(TYPEOF(vv) == type)
	  return vv;
	v = vv;
    }

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
    case LANGSXP: {
	if (type != STRSXP) {
	    ans = coercePairList(v, type);
	    break;
	}

	/* This is mostly copied from coercePairList, but we need to
	 * special-case the first element so as not to get operators
	 * put in backticks. */
	int n = length(v);
	PROTECT(ans = allocVector(type, n));
	if (n == 0) {
	    /* Can this actually happen? */
	    UNPROTECT(1);
	    break;
	}
	int i = 0;
	SEXP op = CAR(v);
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
	for (SEXP vp = v;  vp != R_NilValue; vp = CDR(vp), i++) {
	    if (isString(CAR(vp)) && length(CAR(vp)) == 1)
		SET_STRING_ELT(ans, i, STRING_ELT(CAR(vp), 0));
	    else
		SET_STRING_ELT(ans, i, STRING_ELT(deparse1line(CAR(vp), 0), 0));
	}
	UNPROTECT(1);
	break;
    }
    case VECSXP:
    case EXPRSXP:
	ans = coerceVectorList(v, type);
	break;
    case ENVSXP:
	error(_("environments cannot be coerced to other types"));
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

#define COERCE_ERROR_STRING "cannot coerce type '%s' to vector of type '%s'"

#define COERCE_ERROR							\
	error(_(COERCE_ERROR_STRING), type2char(TYPEOF(v)), type2char(type))

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
	case RAWSXP:
	    ans = coerceToRaw(v);	    break;
	case STRSXP:
	    if (ATTRIB(v) == R_NilValue)
		switch(TYPEOF(v)) {
		case INTSXP:
		case REALSXP:
		    return R_deferred_coerceToString(v, NULL);
		}
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
	&& length(STRING_ELT(x, 0)) >= 1) {
	x = installTrChar(STRING_ELT(x, 0));
    } else
	x = installTrChar(STRING_ELT(deparse1(x, 1, SIMPLEDEPARSE), 0));
    return x;
}

static SEXP asFunction(SEXP x)
{
    SEXP f, pf;
    int n;
    if (isFunction(x)) return x;
    PROTECT(f = allocSExp(CLOSXP));
    SET_CLOENV(f, R_GlobalEnv);
    if (MAYBE_REFERENCED(x)) PROTECT(x = duplicate(x));
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

static SEXP ascommon(SEXP call, SEXP u, SEXPTYPE type)
{
    /* -> as.vector(..) or as.XXX(.) : coerce 'u' to 'type' : */
    /* code assumes u is protected */

    if (type == CLOSXP) {
	return asFunction(u);
    }
    else if (isVector(u) || isList(u) || isLanguage(u)
	     || (isSymbol(u) && type == EXPRSXP)) {
	SEXP v;
	if (type != ANYSXP && TYPEOF(u) != type) v = coerceVector(u, type);
	else v = u;

	/* drop attributes() and class() in some cases for as.pairlist:
	   But why?  (And who actually coerces to pairlists?)
	 */
	if ((type == LISTSXP) &&
	    !(TYPEOF(u) == LANGSXP || TYPEOF(u) == LISTSXP ||
	      TYPEOF(u) == EXPRSXP || TYPEOF(u) == VECSXP)) {
	    if (MAYBE_REFERENCED(v)) v = shallow_duplicate(v);
	    CLEAR_ATTRIB(v);
	}
	return v;
    }
    else if (isSymbol(u) && type == STRSXP)
	return ScalarString(PRINTNAME(u));
    else if (isSymbol(u) && type == SYMSXP)
	return u;
    else if (isSymbol(u) && type == VECSXP) {
	SEXP v = allocVector(VECSXP, 1);
	SET_VECTOR_ELT(v, 0, u);
	return v;
    }
    else errorcall(call, _(COERCE_ERROR_STRING),
		   type2char(TYPEOF(u)), type2char(type));
    return u;/* -Wall */
}

SEXP attribute_hidden do_asCharacterFactor(SEXP call, SEXP op, SEXP args,
                                           SEXP rho)
{
    SEXP x;
    checkArity(op, args);
    check1arg(args, call, "x");
    x = CAR(args);
    return asCharacterFactor(x);
}

/* used in attrib.c, eval.c and unique.c */
SEXP asCharacterFactor(SEXP x)
{
    SEXP ans;

    if( !inherits2(x, "factor") )
	error(_("attempting to coerce non-factor"));

    R_xlen_t i, n = XLENGTH(x);
    SEXP labels = getAttrib(x, R_LevelsSymbol);
    if (TYPEOF(labels) != STRSXP)
	error(_("malformed factor"));
    int nl = LENGTH(labels);
    PROTECT(ans = allocVector(STRSXP, n));
    for(i = 0; i < n; i++) {
      int ii = INTEGER_ELT(x, i);
      if (ii == NA_INTEGER)
	  SET_STRING_ELT(ans, i, NA_STRING);
      else if (ii >= 1 && ii <= nl)
	  SET_STRING_ELT(ans, i, STRING_ELT(labels, ii - 1));
      else
	  error(_("malformed factor"));
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_asatomic(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;

    int type = STRSXP, op0 = PRIMVAL(op);
    char *name = NULL /* -Wall */;

    check1arg(args, call, "x");
    switch(op0) {
    case 0:
	name = "as.character"; break;
    case 1:
	name = "as.integer"; type = INTSXP; break;
    case 2:
	name = "as.double"; type = REALSXP; break;
    case 3:
	name = "as.complex"; type = CPLXSXP; break;
    case 4:
	name = "as.logical"; type = LGLSXP; break;
    case 5:
	name = "as.raw"; type = RAWSXP; break;
    }
    if (DispatchOrEval(call, op, name, args, rho, &ans, 0, 1))
	return(ans);

    /* Method dispatch has failed, we now just */
    /* run the generic internal code */

    checkArity(op, args);
    x = CAR(args);
    if(TYPEOF(x) == type) {
	if(ATTRIB(x) == R_NilValue) return x;
	ans = MAYBE_REFERENCED(x) ? duplicate(x) : x;
	CLEAR_ATTRIB(ans);
	return ans;
    }
    ans = ascommon(call, CAR(args), type);
    CLEAR_ATTRIB(ans);
    return ans;
}

/* NB: as.vector is used for several other as.xxxx, including
   as.expression, as.list, as.pairlist, as.symbol, (as.single) */
SEXP attribute_hidden do_asvector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, ans;
    int type;

    if (DispatchOrEval(call, op, "as.vector", args, rho, &ans, 0, 1))
	return(ans);

    /* Method dispatch has failed, we now just */
    /* run the generic internal code */

    checkArity(op, args);
    x = CAR(args);

    if (!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error_return(R_MSG_mode);
    if (!strcmp("function", (CHAR(STRING_ELT(CADR(args), 0))))) /* ASCII */
	type = CLOSXP;
    else
	type = str2type(CHAR(STRING_ELT(CADR(args), 0))); /* ASCII */

    /* "any" case added in 2.13.0 */
    if(type == ANYSXP || TYPEOF(x) == type) {
	switch(TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	    if(ATTRIB(x) == R_NilValue) return x;
	    ans  = MAYBE_REFERENCED(x) ? duplicate(x) : x;
	    CLEAR_ATTRIB(ans);
	    return ans;
	case EXPRSXP:
	case VECSXP:
	    return x;
	default:
	    ;
	}
    }

    if(IS_S4_OBJECT(x) && TYPEOF(x) == S4SXP) {
	SEXP v = R_getS4DataSlot(x, ANYSXP);
	if(v == R_NilValue)
	    error(_("no method for coercing this S4 class to a vector"));
	x = v;
    }

    switch(type) {/* only those are valid : */
    case SYMSXP: /* for as.symbol */
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP: /* for as.expression */
    case VECSXP: /* list */
    case LISTSXP:/* for as.pairlist */
    case CLOSXP: /* non-primitive function */
    case RAWSXP:
    case ANYSXP: /* any */
	break;
    default:
	error_return(R_MSG_mode);
    }
    ans = ascommon(call, x, type);
    switch(TYPEOF(ans)) { /* keep attributes for these: */
    case NILSXP: /* doesn't have any */
    case LISTSXP: /* but ascommon fiddled */
    case LANGSXP:
    case VECSXP:
    case EXPRSXP:
	break;
    default:
	CLEAR_ATTRIB(ans);
	break;
    }
    return ans;
}


SEXP attribute_hidden do_asfunction(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP arglist, envir, names, pargs, body;
    int i, n;

    checkArity(op, args);

    /* Check the arguments; we need a list and environment. */

    arglist = CAR(args);
    if (!isNewList(arglist))
	error(_("list argument expected"));

    envir = CADR(args);
    if (isNull(envir)) {
	error(_("use of NULL environment is defunct"));
	envir = R_BaseEnv;
    } else
    if (!isEnvironment(envir))
	error(_("invalid environment"));

    n = length(arglist);
    if (n < 1)
	error(_("argument must have length at least 1"));
    PROTECT(names = getAttrib(arglist, R_NamesSymbol));
    PROTECT(pargs = args = allocList(n - 1));
    for (i = 0; i < n - 1; i++) {
	SETCAR(pargs, VECTOR_ELT(arglist, i));
	if (names != R_NilValue && *CHAR(STRING_ELT(names, i)) != '\0') /* ASCII */
	    SET_TAG(pargs, installTrChar(STRING_ELT(names, i)));
	else
	    SET_TAG(pargs, R_NilValue);
	pargs = CDR(pargs);
    }
    CheckFormals(args);
    PROTECT(body = VECTOR_ELT(arglist, n-1));
    /* the main (only?) thing to rule out is body being
       a function already. If we test here then
       mkCLOSXP can continue to overreact when its
       test fails (PR#1880, 7535, 7702) */
    if(isList(body) || isLanguage(body) || isSymbol(body)
       || isExpression(body) || isVector(body) || isByteCode(body)
       )
	    args =  mkCLOSXP(args, body, envir);
    else
	    error(_("invalid body for function"));
    UNPROTECT(3); /* body, pargs, names */
    return args;
}


/* primitive */
SEXP attribute_hidden do_ascall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ap, ans, names;
    int i, n;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "as.call", args, rho, &ans, 0, 1))
	return(ans);

    args = CAR(args);
    switch (TYPEOF(args)) {
    case LANGSXP:
	ans = args;
	break;
    case VECSXP:
    case EXPRSXP:
	if(0 == (n = length(args)))
	    errorcall(call, _("invalid length 0 argument"));
	PROTECT(names = getAttrib(args, R_NamesSymbol));
	PROTECT(ap = ans = allocList(n));
	for (i = 0; i < n; i++) {
	    SETCAR(ap, VECTOR_ELT(args, i));
	    if (names != R_NilValue && !StringBlank(STRING_ELT(names, i)))
		SET_TAG(ap, installTrChar(STRING_ELT(names, i)));
	    ap = CDR(ap);
	}
	UNPROTECT(2); /* ap, names */
	break;
    case LISTSXP:
	ans = duplicate(args);
	break;
    case STRSXP:
	errorcall(call, _("as.call(<character string>)  not yet implemented"));
	break;
    default:
	errorcall(call, _("invalid argument list"));
	ans = R_NilValue;
    }
    SET_TYPEOF(ans, LANGSXP);
    SET_TAG(ans, R_NilValue);
    return ans;
}


/* call and rho are only needed for _R_CHECK_LENGTH_1_LOGIC2_ checking
       and diagnostics; to be removed if length>1 value is turned to error */
/* return int, not Rboolean, for NA_LOGICAL : */
int asLogical2(SEXP x, int checking, SEXP call, SEXP rho)
{
    int warn = 0;

    if (isVectorAtomic(x)) {
	if (XLENGTH(x) < 1)
	    return NA_LOGICAL;
	if (checking && XLENGTH(x) > 1) {
	    char msg[128];
	    snprintf(msg, 128, _("'length(x) = %lld > 1' in coercion to '%s'"),
		    (long long) XLENGTH(x), "logical(1)");
	    R_BadValueInRCode(x, call, rho,
		"length > 1 in coercion to logical",
		msg,
		msg,
		"_R_CHECK_LENGTH_1_LOGIC2_",
		FALSE /* by default do nothing */);
	}
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return LOGICAL_ELT(x, 0);
	case INTSXP:
	    return LogicalFromInteger(INTEGER_ELT(x, 0), &warn);
	case REALSXP:
	    return LogicalFromReal(REAL_ELT(x, 0), &warn);
	case CPLXSXP:
	    return LogicalFromComplex(COMPLEX_ELT(x, 0), &warn);
	case STRSXP:
	    return LogicalFromString(STRING_ELT(x, 0), &warn);
	case RAWSXP:
	    return LogicalFromInteger((int)RAW_ELT(x, 0), &warn);
	default:
	    UNIMPLEMENTED_TYPE("asLogical", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	    return LogicalFromString(x, &warn);
    }
    return NA_LOGICAL;
}

int asLogical(SEXP x)
{
    return asLogical2(x, /* checking = */ 0, R_NilValue, R_NilValue);
}


int asInteger(SEXP x)
{
    int warn = 0, res;

    if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    return IntegerFromLogical(LOGICAL_ELT(x, 0), &warn);
	case INTSXP:
	    return INTEGER_ELT(x, 0);
	case REALSXP:
	    res = IntegerFromReal(REAL_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	case CPLXSXP:
	    res = IntegerFromComplex(COMPLEX_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = IntegerFromString(STRING_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asInteger", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = IntegerFromString(x, &warn);
	CoercionWarning(warn);
	return res;
    }
    return NA_INTEGER;
}

R_xlen_t asXLength(SEXP x)
{
    const R_xlen_t na = -999; /* any negative number should do */

    if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case INTSXP:
	{
	    int res = INTEGER_ELT(x, 0);
	    if (res == NA_INTEGER)
		return na;
	    else
		return (R_xlen_t) res;
	}
	case LGLSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	    break;
	default:
	    UNIMPLEMENTED_TYPE("asXLength", x);
	}
    } else if(TYPEOF(x) != CHARSXP)
	return na;

    double d = asReal(x);
    if (!R_FINITE(d) || d > R_XLEN_T_MAX || d < 0)
	return na;
    else
	return (R_xlen_t) d;
}

double asReal(SEXP x)
{
    int warn = 0;
    double res;

    if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    res = RealFromLogical(LOGICAL_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	case INTSXP:
	    res = RealFromInteger(INTEGER_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	case REALSXP:
	    return REAL_ELT(x, 0);
	case CPLXSXP:
	    res = RealFromComplex(COMPLEX_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	case STRSXP:
	    res = RealFromString(STRING_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return res;
	default:
	    UNIMPLEMENTED_TYPE("asReal", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	res = RealFromString(x, &warn);
	CoercionWarning(warn);
	return res;
    }
    return NA_REAL;
}

Rcomplex asComplex(SEXP x)
{
    int warn = 0;
    Rcomplex z;

    if (isVectorAtomic(x) && XLENGTH(x) >= 1) {
	switch (TYPEOF(x)) {
	case LGLSXP:
	    z = ComplexFromLogical(LOGICAL_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return z;
	case INTSXP:
	    z = ComplexFromInteger(INTEGER_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return z;
	case REALSXP:
	    z = ComplexFromReal(REAL_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return z;
	case CPLXSXP:
	    return COMPLEX_ELT(x, 0);
	case STRSXP:
	    z = ComplexFromString(STRING_ELT(x, 0), &warn);
	    CoercionWarning(warn);
	    return z;
	default:
	    UNIMPLEMENTED_TYPE("asComplex", x);
	}
    } else if(TYPEOF(x) == CHARSXP) {
	z = ComplexFromString(x, &warn);
	CoercionWarning(warn);
	return z;
    }
    z.r = NA_REAL;
    z.i = NA_REAL;
    return z;
}


/* return the type (= "detailed mode") of the SEXP */
SEXP attribute_hidden do_typeof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return type2rstr(TYPEOF(CAR(args)));
}

/* Define many of the <primitive> "is.xxx" functions :
   Note that  isNull, isNumeric, etc are defined in util.c or ../include/Rinlinedfuns.h
*/
SEXP attribute_hidden do_is(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    checkArity(op, args);
    check1arg(args, call, "x");

    /* These are all builtins, so we do not need to worry about
       evaluating arguments in DispatchOrEval */
    if(PRIMVAL(op) >= 100 && PRIMVAL(op) < 200 && isObject(CAR(args))) {
	/* This used CHAR(PRINTNAME(CAR(call))), but that is not
	   necessarily correct, e.g. when called from lapply() */
	const char *nm;
	switch(PRIMVAL(op)) {
	case 100: nm = "is.numeric"; break;
	case 101: nm = "is.matrix"; break;
	case 102: nm = "is.array"; break;
	default: nm = ""; /* -Wall */
	}
	if(DispatchOrEval(call, op, nm, args, rho, &ans, 0, 1))
	    return(ans);
    }

    PROTECT(ans = allocVector(LGLSXP, 1));

    switch (PRIMVAL(op)) {
    case NILSXP:	/* is.null */
	LOGICAL0(ans)[0] = isNull(CAR(args));
	break;
    case LGLSXP:	/* is.logical */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == LGLSXP);
	break;
    case INTSXP:	/* is.integer */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == INTSXP)
	    && !inherits(CAR(args), "factor");
	break;
    case REALSXP:	/* is.double */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == REALSXP);
	break;
    case CPLXSXP:	/* is.complex */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == CPLXSXP);
	break;
    case STRSXP:	/* is.character */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == STRSXP);
	break;
    case SYMSXP:	/* is.symbol === is.name */
	if(IS_S4_OBJECT(CAR(args)) && (TYPEOF(CAR(args)) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(CAR(args), SYMSXP);
	    LOGICAL0(ans)[0] = (TYPEOF(dot_xData) == SYMSXP);
	}
	else
	    LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP);
	break;
    case ENVSXP:	/* is.environment */
	if(IS_S4_OBJECT(CAR(args)) && (TYPEOF(CAR(args)) == S4SXP)) {
	    SEXP dot_xData = R_getS4DataSlot(CAR(args), ENVSXP);
	    LOGICAL0(ans)[0] = (TYPEOF(dot_xData) == ENVSXP);
	}
	else
	    LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == ENVSXP);
	break;
    case VECSXP:	/* is.list */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == VECSXP ||
			    TYPEOF(CAR(args)) == LISTSXP);
	break;
    case LISTSXP:	/* is.pairlist */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == LISTSXP ||
			    TYPEOF(CAR(args)) == NILSXP);/* pairlist() -> NULL */
	break;
    case EXPRSXP:	/* is.expression */
	LOGICAL0(ans)[0] = TYPEOF(CAR(args)) == EXPRSXP;
	break;
    case RAWSXP:	/* is.raw */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == RAWSXP);
	break;

    case 50:		/* is.object */
	LOGICAL0(ans)[0] = OBJECT(CAR(args));
	break;
    case 51:		/* isS4 */
	LOGICAL0(ans)[0] = IS_S4_OBJECT(CAR(args)) != 0;
	break;
/* no longer used: is.data.frame is R code
    case 80:
	LOGICAL0(ans)[0] = isFrame(CAR(args));
	break;
*/

    case 100:		/* is.numeric */
	LOGICAL0(ans)[0] = isNumeric(CAR(args)) &&
	    !isLogical(CAR(args));  /* isNumeric excludes factors */
	break;
    case 101:		/* is.matrix */
	LOGICAL0(ans)[0] = isMatrix(CAR(args));
	break;
    case 102:		/* is.array */
	LOGICAL0(ans)[0] = isArray(CAR(args));
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
	case RAWSXP:
	    LOGICAL0(ans)[0] = 1;
	    break;
	default:
	    LOGICAL0(ans)[0] = 0;
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
	// Not recursive, as long as not subsettable (on the R level)
	// case EXTPTRSXP:
	// case BCODESXP:
	// case WEAKREFSXP:
	    LOGICAL0(ans)[0] = 1;
	    break;
	default:
	    LOGICAL0(ans)[0] = 0;
	    break;
	}
	break;

    case 300:		/* is.call */
	LOGICAL0(ans)[0] = TYPEOF(CAR(args)) == LANGSXP;
	break;
    case 301:		/* is.language */
	LOGICAL0(ans)[0] = (TYPEOF(CAR(args)) == SYMSXP ||
			    TYPEOF(CAR(args)) == LANGSXP ||
			    TYPEOF(CAR(args)) == EXPRSXP);
	break;
    case 302:		/* is.function */
	LOGICAL0(ans)[0] = isFunction(CAR(args));
	break;

    case 999:		/* is.single */
	errorcall(call, _("type \"single\" unimplemented in R"));
    default:
	errorcall(call, _("unimplemented predicate"));
    }
    UNPROTECT(1);
    return (ans);
}

/* What should is.vector do ?
 * In S, if an object has no attributes it is a vector, otherwise it isn't.
 * It seems to make more sense to check for a dim attribute.
 */

// is.vector(x, mode) :
SEXP attribute_hidden do_isvector(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, a, x;
    const char *stype;

    checkArity(op, args);
    x = CAR(args);
    if (!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	error_return(R_MSG_mode);

    stype = CHAR(STRING_ELT(CADR(args), 0)); /* ASCII */

    /* "name" and "symbol" are synonymous */
    if (streql(stype, "name"))
      stype = "symbol";

    PROTECT(ans = allocVector(LGLSXP, 1));
    if (streql(stype, "any")) {
	/* isVector is inlined, means atomic or VECSXP or EXPRSXP */
	LOGICAL0(ans)[0] = isVector(x);
    }
    else if (streql(stype, "numeric")) {
	LOGICAL0(ans)[0] = (isNumeric(x) && !isLogical(x));
    }
    /* So this allows any type, including undocumented ones such as
       "closure", but not aliases such as "name" and "function". */
    else if (streql(stype, type2char(TYPEOF(x)))) {
	LOGICAL0(ans)[0] = 1;
    }
    else
	LOGICAL0(ans)[0] = 0;

    /* We allow a "names" attribute on any vector. */
    if (LOGICAL0(ans)[0] && ATTRIB(CAR(args)) != R_NilValue) {
	a = ATTRIB(CAR(args));
	while(a != R_NilValue) {
	    if (TAG(a) != R_NamesSymbol) {
		LOGICAL0(ans)[0] = 0;
		break;
	    }
	    a = CDR(a);
	}
    }
    UNPROTECT(1);
    return (ans);
}

static R_INLINE void copyDimAndNames(SEXP x, SEXP ans)
{
    if (isVector(x)) {
	/* PROTECT/UNPROTECT are probably not needed here */
	SEXP dims, names;
	PROTECT(dims = getAttrib(x, R_DimSymbol));
	if (dims != R_NilValue)
	    setAttrib(ans, R_DimSymbol, dims);
	UNPROTECT(1);
	if (isArray(x)) {
	    PROTECT(names = getAttrib(x, R_DimNamesSymbol));
	    if (names != R_NilValue)
		setAttrib(ans, R_DimNamesSymbol, names);
	    UNPROTECT(1);
	}
	else {
	    PROTECT(names = getAttrib(x, R_NamesSymbol));
	    if (names != R_NilValue)
		setAttrib(ans, R_NamesSymbol, names);
	    UNPROTECT(1);
	}
    }
}

SEXP attribute_hidden do_isna(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    R_xlen_t i, n;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "is.na", args, rho, &ans, 1, 1))
	return(ans);
    PROTECT(args = ans);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.na " R_MSG_list_vec);

#endif
    x = CAR(args);
    n = xlength(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    switch (TYPEOF(x)) {
    case LGLSXP:
       for (i = 0; i < n; i++)
	   pa[i] = (LOGICAL_ELT(x, i) == NA_LOGICAL);
	break;
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (INTEGER_ELT(x, i) == NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = ISNAN(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (ISNAN(v.r) || ISNAN(v.i));
	}
	break;
    case STRSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (STRING_ELT(x, i) == NA_STRING);
	break;

/* Same code for LISTSXP and VECSXP : */
#define LIST_VEC_NA(s)							\
	if (!isVector(s) || length(s) != 1)				\
	    pa[i] = 0;							\
	else {								\
		switch (TYPEOF(s)) {					\
		case LGLSXP:						\
		case INTSXP:						\
		    pa[i] = (INTEGER_ELT(s, 0) == NA_INTEGER);		\
		    break;						\
		case REALSXP:						\
		    pa[i] = ISNAN(REAL_ELT(s, 0));			\
		    break;						\
		case STRSXP:						\
		    pa[i] = (STRING_ELT(s, 0) == NA_STRING);		\
		    break;						\
		case CPLXSXP:						\
		    {							\
			Rcomplex v = COMPLEX_ELT(s, 0);			\
			pa[i] = (ISNAN(v.r) || ISNAN(v.i));		\
		    }							\
		    break;						\
		default:						\
		    pa[i] = 0;						\
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
    case RAWSXP:
	/* no such thing as a raw NA */
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case NILSXP: break;
    default:
	warningcall(call, _("%s() applied to non-(list or vector) of type '%s'"),
		    "is.na", type2char(TYPEOF(x)));
	for (i = 0; i < n; i++)
	    pa[i] = 0;
    }

    copyDimAndNames(x, ans);
    UNPROTECT(2); /* args, ans */
    return ans;
}

#include <R_ext/Itermacros.h>

// Check if x has missing values; the anyNA.default() method
static Rboolean anyNA(SEXP call, SEXP op, SEXP args, SEXP env)
/* Original code:
   Copyright 2012 Google Inc. All Rights Reserved.
   Author: Tim Hesterberg <rocket@google.com>
   Distributed under GPL 2 or later
*/
{
    SEXP x = CAR(args);
    SEXPTYPE xT = TYPEOF(x);
    Rboolean isList =  (xT == VECSXP || xT == LISTSXP), recursive = FALSE;

    if (isList && length(args) > 1) recursive = asLogical(CADR(args));
    if (OBJECT(x) || (isList && !recursive)) {
	SEXP e0 = PROTECT(lang2(install("is.na"), x));
	SEXP e = PROTECT(lang2(install("any"), e0));
	SEXP res = PROTECT(eval(e, env));
	int ans = asLogical(res);
	UNPROTECT(3);
	return ans == 1; // so NA answer is false.
    }

    R_xlen_t i, n = xlength(x);
    switch (xT) {
    case REALSXP:
    {
	if(REAL_NO_NA(x))
	    return FALSE;
	ITERATE_BY_REGION(x, xD, i, nbatch, double, REAL, {
		for (int k = 0; k < nbatch; k++)
		    if (ISNAN(xD[k]))
			return TRUE;
	    });
	break;
    }
    case INTSXP:
    {
	if(INTEGER_NO_NA(x))
	    return FALSE;
	ITERATE_BY_REGION(x, xI, i, nbatch, int, INTEGER, {
		for (int k = 0; k < nbatch; k++)
		    if (xI[k] == NA_INTEGER)
			return TRUE;
	    });
	break;
    }
    case LGLSXP:
    {
	for (i = 0; i < n; i++)
	    if (LOGICAL_ELT(x, i) == NA_LOGICAL) return TRUE;
	break;
    }
    case CPLXSXP:
    {
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    if (ISNAN(v.r) || ISNAN(v.i)) return TRUE;
	}
	break;
    }
    case STRSXP:
	for (i = 0; i < n; i++)
	    if (STRING_ELT(x, i) == NA_STRING) return TRUE;
	break;
    case RAWSXP: /* no such thing as a raw NA:  is.na(.) gives FALSE always */
	return FALSE;
    case NILSXP: // is.na() gives a warning..., but we do not.
	return FALSE;
    // The next two cases are only used if recursive = TRUE
    case LISTSXP:
    {
	SEXP call2, args2, ans;
	args2 = PROTECT(shallow_duplicate(args));
	call2 = PROTECT(shallow_duplicate(call));
	for (i = 0; i < n; i++, x = CDR(x)) {
	    SETCAR(args2, CAR(x)); SETCADR(call2, CAR(x));
	    if ((DispatchOrEval(call2, op, "anyNA", args2, env, &ans, 0, 1)
		 && asLogical(ans)) || anyNA(call2, op, args2, env)) {
		UNPROTECT(2);
		return TRUE;
	    }
	}
	UNPROTECT(2);
	break;
    }
    case VECSXP:
    {
	SEXP call2, args2, ans;
	args2 = PROTECT(shallow_duplicate(args));
	call2 = PROTECT(shallow_duplicate(call));
	for (i = 0; i < n; i++) {
	    SETCAR(args2, VECTOR_ELT(x, i)); SETCADR(call2, VECTOR_ELT(x, i));
	    if ((DispatchOrEval(call2, op, "anyNA", args2, env, &ans, 0, 1)
		 && asLogical(ans)) || anyNA(call2, op, args2, env)) {
		UNPROTECT(2);
		return TRUE;
	    }
	}
	UNPROTECT(2);
	break;
    }

    default:
	error("anyNA() applied to non-(list or vector) of type '%s'",
	      type2char(TYPEOF(x)));
    }
    return FALSE;
} // anyNA()

SEXP attribute_hidden do_anyNA(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;
    static SEXP do_anyNA_formals = NULL;

    if (length(args) < 1 || length(args) > 2)
	errorcall(call, "anyNA takes 1 or 2 arguments");

    if (DispatchOrEval(call, op, "anyNA", args, rho, &ans, 0, 1))
	return ans;

    if(length(args) == 1) {
	check1arg(args, call, "x");
	ans = ScalarLogical(anyNA(call, op, args, rho));
   } else {
	/* This is a primitive, so we manage argument matching ourselves.
	   But this takes a little time.
	 */
	if (do_anyNA_formals == NULL)
	    do_anyNA_formals = allocFormalsList2(install("x"),
						 R_RecursiveSymbol);
	PROTECT(args = matchArgs(do_anyNA_formals, args, call));
	if(CADR(args) ==  R_MissingArg) SETCADR(args, ScalarLogical(FALSE));
	ans = ScalarLogical(anyNA(call, op, args, rho));
	UNPROTECT(1);
    }
    return ans;
}


SEXP attribute_hidden do_isnan(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x;
    R_xlen_t i, n;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "is.nan", args, rho, &ans, 1, 1))
	return(ans);

    PROTECT(args = ans);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.nan " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = xlength(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = R_IsNaN(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (R_IsNaN(v.r) || R_IsNaN(v.i));
	}
	break;
    default:
	errorcall(call, _("default method not implemented for type '%s'"),
		  type2char(TYPEOF(x)));
    }
    copyDimAndNames(x, ans);
    UNPROTECT(2); /* args, ans*/
    return ans;
}

SEXP attribute_hidden do_isfinite(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, names, dims;
    R_xlen_t i, n;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "is.finite", args, rho, &ans, 0, 1))
	return(ans);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.finite " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = xlength(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    if (isVector(x)) {
	dims = getAttrib(x, R_DimSymbol);
	if (isArray(x))
	    PROTECT(names = getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = getAttrib(x, R_NamesSymbol));
    }
    else dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = (INTEGER_ELT(x, i) != NA_INTEGER);
	break;
    case REALSXP:
	for (i = 0; i < n; i++)
	    pa[i] = R_FINITE(REAL_ELT(x, i));
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    pa[i] = (R_FINITE(v.r) && R_FINITE(v.i));
	}
	break;
    default:
	errorcall(call, _("default method not implemented for type '%s'"),
		  type2char(TYPEOF(x)));
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
	UNPROTECT(1); /* names */
    UNPROTECT(1); /* ans */
    return ans;
}

SEXP attribute_hidden do_isinfinite(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, x, names, dims;
    double xr, xi;
    R_xlen_t i, n;

    checkArity(op, args);
    check1arg(args, call, "x");

    if (DispatchOrEval(call, op, "is.infinite", args, rho, &ans, 0, 1))
	return(ans);
#ifdef stringent_is
    if (!isList(CAR(args)) && !isVector(CAR(args)))
	errorcall_return(call, "is.infinite " R_MSG_list_vec);
#endif
    x = CAR(args);
    n = xlength(x);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);
    if (isVector(x)) {
	dims = getAttrib(x, R_DimSymbol);
	if (isArray(x))
	    PROTECT(names = getAttrib(x, R_DimNamesSymbol));
	else
	    PROTECT(names = getAttrib(x, R_NamesSymbol));
    }
    else	dims = names = R_NilValue;
    switch (TYPEOF(x)) {
    case STRSXP:
    case RAWSXP:
    case NILSXP:
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < n; i++)
	    pa[i] = 0;
	break;
    case REALSXP:
	for (i = 0; i < n; i++) {
	    xr = REAL_ELT(x, i);
	    if (ISNAN(xr) || R_FINITE(xr))
		pa[i] = 0;
	    else
		pa[i] = 1;
	}
	break;
    case CPLXSXP:
	for (i = 0; i < n; i++) {
	    Rcomplex v = COMPLEX_ELT(x, i);
	    xr = v.r;
	    xi = v.i;
	    if ((ISNAN(xr) || R_FINITE(xr)) && (ISNAN(xi) || R_FINITE(xi)))
		pa[i] = 0;
	    else
		pa[i] = 1;
	}
	break;
    default:
	errorcall(call, _("default method not implemented for type '%s'"),
		  type2char(TYPEOF(x)));
    }
    if (!isNull(dims))
	setAttrib(ans, R_DimSymbol, dims);
    if (!isNull(names)) {
	if (isArray(x))
	    setAttrib(ans, R_DimNamesSymbol, names);
	else
	    setAttrib(ans, R_NamesSymbol, names);
    }
    if (isVector(x))
	UNPROTECT(1); /* names */
    UNPROTECT(1); /* ans */
    return ans;
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_call(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rest, evargs, rfun, tmp;

    if (length(args) < 1) errorcall(call, _("'name' is missing"));
    check1arg(args, call, "name");
    PROTECT(rfun = eval(CAR(args), rho));
    /* zero-length string check used to be here but install gives
       better error message.
     */
    if (!isString(rfun) || length(rfun) != 1)
	errorcall_return(call, _("first argument must be a character string"));
    const char *str = translateChar(STRING_ELT(rfun, 0));
    if (streql(str, ".Internal")) error("illegal usage");
    PROTECT(rfun = install(str));
    PROTECT(evargs = shallow_duplicate(CDR(args)));
    for (rest = evargs; rest != R_NilValue; rest = CDR(rest)) {
	tmp = eval(CAR(rest), rho);
	if (NAMED(tmp)) MARK_NOT_MUTABLE(tmp);
	SETCAR(rest, tmp);
    }
    rfun = LCONS(rfun, evargs);
    UNPROTECT(3);
    return (rfun);
}

SEXP attribute_hidden do_docall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP c, fun, names, envir;
    int i, n;
    /* RCNTXT *cptr; */

    checkArity(op, args);

    fun = CAR(args);
    envir = CADDR(args);
    args = CADR(args);

    /* must be a string or a function:
       zero-length string check used to be here but install gives
       better error message.
     */
    if(!(isFunction(fun) || (isString(fun) && length(fun) == 1)))
	error(_("'what' must be a function or character string"));

#ifdef __maybe_in_the_future__
    if (!isNull(args) && !isVectorList(args))
	error(_("'%s' must be a list or expression"), "args");
#else
    if (!isNull(args) && !isNewList(args))
	error(_("'%s' must be a list"), "args");
#endif

    if (!isEnvironment(envir))
	error(_("'envir' must be an environment"));

    n = length(args);
    PROTECT(names = getAttrib(args, R_NamesSymbol));

    PROTECT(c = call = allocList(n + 1));
    SET_TYPEOF(c, LANGSXP);
    if( isString(fun) ) {
	const char *str = translateChar(STRING_ELT(fun, 0));
	if (streql(str, ".Internal")) error("illegal usage");
	SETCAR(c, install(str));
    } else {
	if(TYPEOF(fun) == SPECIALSXP && streql(PRIMNAME(fun), ".Internal"))
	    error("illegal usage");
	SETCAR(c, fun);
    }
    c = CDR(c);
    for (i = 0; i < n; i++) {
#ifndef NEW
	SETCAR(c, VECTOR_ELT(args, i));
#else
	SETCAR(c, mkPROMISE(VECTOR_ELT(args, i), rho));
	SET_PRVALUE(CAR(c), VECTOR_ELT(args, i));
#endif
	if (ItemName(names, (int)i) != R_NilValue)
	    SET_TAG(c, installTrChar(ItemName(names, i)));
	c = CDR(c);
    }
    call = eval(call, envir);

    UNPROTECT(2); /* c, names */
    return call;
}


/*
   do_substitute has two arguments, an expression and an environment
   (optional).	Symbols found in the expression are substituted with their
   values as found in the environment.	There is no inheritance so only
   the supplied environment is searched. If no environment is specified
   the environment in which substitute was called is used.  If the
   specified environment is R_GlobalEnv it is converted to R_NilValue, for
   historical reasons. In substitute(), R_NilValue signals that no
   substitution should be done, only extraction of promise expressions.
   Arguments to do_substitute should not be evaluated.
*/

SEXP substitute(SEXP lang, SEXP rho)
{
    SEXP t;
    switch (TYPEOF(lang)) {
    case PROMSXP:
	return substitute(PREXPR(lang), rho);
    case SYMSXP:
	if (rho != R_NilValue) {
	    t = findVarInFrame3( rho, lang, TRUE);
	    if (t != R_UnboundValue) {
		if (TYPEOF(t) == PROMSXP) {
		    do {
			t = PREXPR(t);
		    } while(TYPEOF(t) == PROMSXP);
		    /* make sure code will not be modified: */
		    ENSURE_NAMEDMAX(t);
		    return t;
		}
		else if (TYPEOF(t) == DOTSXP)
		    error(_("'...' used in an incorrect context"));
		if (rho != R_GlobalEnv)
		    return t;
	    }
	}
	return (lang);
    case LANGSXP:
	return substituteList(lang, rho);
    default:
	return (lang);
    }
}


/* Work through a list doing substitute on the
   elements taking particular care to handle '...' */

SEXP attribute_hidden substituteList(SEXP el, SEXP rho)
{
    SEXP h, p = R_NilValue, res = R_NilValue;

    if (isNull(el)) return el;

    while (el != R_NilValue) {
	/* walk along the pairlist, substituting elements.
	   res is the result
	   p is the current last element
	   h is the element currently being processed
	 */
	if (CAR(el) == R_DotsSymbol) {
	    if (rho == R_NilValue)
		h = R_UnboundValue;	/* so there is no substitution below */
	    else
		h = findVarInFrame3(rho, CAR(el), TRUE);
	    if (h == R_UnboundValue)
		h = LCONS(R_DotsSymbol, R_NilValue);
	    else if (h == R_NilValue  || h == R_MissingArg)
		h = R_NilValue;
	    else if (TYPEOF(h) == DOTSXP) {
		PROTECT(h);
		h = substituteList(h, R_NilValue);
		UNPROTECT(1);
	    } else
		error(_("'...' used in an incorrect context"));
	} else {
	    h = substitute(CAR(el), rho);
	    if (isLanguage(el))
		h = LCONS(h, R_NilValue);
	    else
		h = CONS(h, R_NilValue);
	    SET_TAG(h, TAG(el));
	}
	if (h != R_NilValue) {
	    if (res == R_NilValue)
		PROTECT(res = h);
	    else
		SETCDR(p, h);
	    /* now set 'p': dots might have expanded to a list of length > 1 */
	    while (CDR(h) != R_NilValue) h = CDR(h);
	    p = h;
	}
	el = CDR(el);
    }
    if(res != R_NilValue) UNPROTECT(1);
    return res;
}


/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_substitute(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP argList, env, s, t;
    static SEXP do_substitute_formals = NULL;

    if (do_substitute_formals == NULL)
	do_substitute_formals = allocFormalsList2(install("expr"),
						  install("env"));

    /* argument matching */
    PROTECT(argList = matchArgs(do_substitute_formals, args, call));

    /* set up the environment for substitution */
    if (CADR(argList) == R_MissingArg)
	env = rho;
    else
	env = eval(CADR(argList), rho);
    if (env == R_GlobalEnv)	/* For historical reasons, don't substitute in R_GlobalEnv */
	env = R_NilValue;
    else if (TYPEOF(env) == VECSXP)
	env = NewEnvironment(R_NilValue, VectorToPairList(env), R_BaseEnv);
    else if (TYPEOF(env) == LISTSXP)
	env = NewEnvironment(R_NilValue, duplicate(env), R_BaseEnv);
    if (env != R_NilValue && TYPEOF(env) != ENVSXP)
	errorcall(call, _("invalid environment specified"));

    PROTECT(env);
    PROTECT(t = CONS(duplicate(CAR(argList)), R_NilValue));
    s = substituteList(t, env);
    UNPROTECT(3);
    return CAR(s);
}

/* This is a primitive SPECIALSXP */
SEXP attribute_hidden do_quote(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "expr");
    SEXP val = CAR(args);
    /* Make sure expression has NAMED == NAMEDMAX before being returning
       in order to avoid modification of source code */
    ENSURE_NAMEDMAX(val);
    return(val);
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
    { "raw",		RAWSXP,    TRUE },
    { "complex",	CPLXSXP,   TRUE },
    { "character",	STRSXP,	   TRUE },
    { "expression",	EXPRSXP,   TRUE },
    { "list",		VECSXP,	   TRUE },
    { "environment",    ENVSXP,    FALSE },
    { "char",		CHARSXP,   TRUE },
    { "externalptr",	EXTPTRSXP,  FALSE },
    { "weakref",	WEAKREFSXP, FALSE },
    { "name",		SYMSXP,	   FALSE },

    { (char *)NULL,	(SEXPTYPE)-1, FALSE}
};

static int class2type(const char *s)
{
    /* return the type if the class string is one of the basic types, else -1.
       Note that this is NOT str2type:  only certain types are defined to be basic
       classes; e.g., "language" is a type but many classes correspond to objects of
       this type.
    */
    int i; char *si;
    for(i = 0; ; i++) {
	si = classTable[i].s;
	if(!si)
	    return -1;
	if(!strcmp(s, si))
	    return i;
    }
    /* cannot get here return -1; */
}

static SEXP do_unsetS4(SEXP obj, SEXP newClass)
{
  if(isNull(newClass))  { /* NULL class is only valid for S3 objects */
    warning(_("Setting class(x) to NULL;   result will no longer be an S4 object"));
  }
  else if(length(newClass) > 1)
    warning(_("Setting class(x) to multiple strings (\"%s\", \"%s\", ...); result will no longer be an S4 object"),
	    translateChar(STRING_ELT(newClass, 0)),
	    translateChar(STRING_ELT(newClass, 1)));
  else
    warning(_("Setting class(x) to \"%s\" sets attribute to NULL; result will no longer be an S4 object"),
	    CHAR(asChar(newClass)));
  UNSET_S4_OBJECT(obj);
  return obj;
}

/* set the class to value, and return the modified object.  This is
   NOT a primitive assignment operator , because there is no code in R
   that changes type in place. */
static SEXP R_set_class(SEXP obj, SEXP value, SEXP call)
{
    int nProtect = 0;
    // use of zero-length vector used to be documented.
    if(!length(value)) { // usually NULL
	setAttrib(obj, R_ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	  do_unsetS4(obj, value);
	return obj;
    }
    if(TYPEOF(value) != STRSXP) {
	SEXP dup;
	/* assumes value is protected, which it is in R_do_set_class */
	PROTECT(dup = duplicate(value));
	PROTECT(value = coerceVector(dup, STRSXP));
	nProtect += 2;
    }
    if(length(value) > 1) {
	setAttrib(obj, R_ClassSymbol, value);
	if(IS_S4_OBJECT(obj)) /*  multiple strings only valid for S3 objects */
	  do_unsetS4(obj, value);
    }
    else if(length(value) == 0) {
	error(_("invalid replacement object to be a class string"));
    }
    else {
	const char *valueString = CHAR(asChar(value)); /* ASCII */
	int whichType = class2type(valueString);
	SEXPTYPE valueType = (whichType == -1) ? (SEXPTYPE) -1
	    : classTable[whichType].sexp;
	// SEXP cur_class = PROTECT(R_data_class(obj, FALSE)); nProtect++;
	/*  assigning type as a class deletes an explicit class attribute. */
	if(valueType != (SEXPTYPE)-1) {
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
	    if(classTable[whichType].canChange) {
		PROTECT(obj = ascommon(call, obj, valueType));
		nProtect++;
	    }
	    else if(valueType != TYPEOF(obj))
		error(_("\"%s\" can only be set as the class if the object has this type; found \"%s\""),
		      valueString, type2char(TYPEOF(obj)));
	    /* else, leave alone */
	}
	else if(!strcmp("numeric", valueString)) {
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      do_unsetS4(obj, value);
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
		error(_("invalid to set the class to matrix unless the dimension attribute is of length 2 (was %d)"),
		 length(getAttrib(obj, R_DimSymbol)));
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj))
	      do_unsetS4(obj, value);
	}
	else if(!strcmp("array", valueString)) {
	    if(length(getAttrib(obj, R_DimSymbol)) <= 0)
		error(_("cannot set class to \"array\" unless the dimension attribute has length > 0"));
	    setAttrib(obj, R_ClassSymbol, R_NilValue);
	    if(IS_S4_OBJECT(obj)) /* NULL class is only valid for S3 objects */
	      UNSET_S4_OBJECT(obj);
	}
	else { /* set the class but don't do the coercion; that's
		  supposed to be done by an as() method */
	    setAttrib(obj, R_ClassSymbol, value);
	}
    }
    UNPROTECT(nProtect);
    return obj;
}

SEXP attribute_hidden R_do_set_class(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    checkArity(op, args);
    check1arg(args, call, "x");

    if (MAYBE_SHARED(CAR(args))) SETCAR(args, shallow_duplicate(CAR(args)));
    ans = R_set_class(CAR(args), CADR(args), call);
    SETTER_CLEAR_NAMED(CAR(args));
    return ans;
}

/* primitive */
SEXP attribute_hidden do_storage_mode(SEXP call, SEXP op, SEXP args, SEXP env)
{
/* storage.mode(obj) <- value */
    SEXP obj, value, ans;
    SEXPTYPE type;

    checkArity(op, args);
    check1arg(args, call, "x");

    obj = CAR(args);

    value = CADR(args);
    if (!isValidString(value) || STRING_ELT(value, 0) == NA_STRING)
	error(_("'value' must be non-null character string"));
    type = str2type(CHAR(STRING_ELT(value, 0)));
    if(type == (SEXPTYPE) -1) {
	/* For backwards compatibility we allow "real" and "single" */
	if(streql(CHAR(STRING_ELT(value, 0)), "real")) {
	    error("use of 'real' is defunct: use 'double' instead");
	} else if(streql(CHAR(STRING_ELT(value, 0)), "single")) {
	    error("use of 'single' is defunct: use mode<- instead");
	} else
	    error(_("invalid value"));
    }
    if(TYPEOF(obj) == type) return obj;
    if(isFactor(obj))
	error(_("invalid to change the storage mode of a factor"));
    PROTECT(ans = coerceVector(obj, type));
    SHALLOW_DUPLICATE_ATTRIB(ans, obj);
    UNPROTECT(1);
    return ans;
}
