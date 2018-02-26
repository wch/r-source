/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <Rmath.h>
#include <errno.h>
#include <R_ext/Itermacros.h>

/* interval at which to check interrupts, a guess */
#define NINTERRUPT 10000000

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call);
static SEXP string_relop (RELOP_TYPE code, SEXP s1, SEXP s2);
static SEXP raw_relop    (RELOP_TYPE code, SEXP s1, SEXP s2);

#define DO_SCALAR_RELOP(oper, x, y) do {		\
	switch (oper) {					\
	case EQOP: return ScalarLogical((x) == (y));	\
	case NEOP: return ScalarLogical((x) != (y));	\
	case LTOP: return ScalarLogical((x) < (y));	\
	case GTOP: return ScalarLogical((x) > (y));	\
	case LEOP: return ScalarLogical((x) <= (y));	\
	case GEOP: return ScalarLogical((x) >= (y));	\
	}						\
    } while (0)

SEXP attribute_hidden do_relop(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, arg1, arg2;
    int argc;

    if (args != R_NilValue &&
	CDR(args) != R_NilValue &&
	CDDR(args) == R_NilValue)
	argc = 2;
    else
	argc = length(args);
    arg1 = CAR(args);
    arg2 = CADR(args);

    if (ATTRIB(arg1) != R_NilValue || ATTRIB(arg2) != R_NilValue) {
	if (DispatchGroup("Ops", call, op, args, env, &ans))
	    return ans;
    }

    if (argc != 2)
	error("operator needs two arguments");

    return do_relop_dflt(call, op, arg1, arg2);
}

// also called from cmp_relop() in eval.c :
SEXP attribute_hidden do_relop_dflt(SEXP call, SEXP op, SEXP x, SEXP y)
{
    /* handle the REALSXP/INTSXP simple scalar case quickly */
    if (IS_SIMPLE_SCALAR(x, INTSXP)) {
	int ix = SCALAR_IVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ix == NA_INTEGER || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), ix, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ix == NA_INTEGER || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), ix, dy);
	}
    }
    else if (IS_SIMPLE_SCALAR(x, REALSXP)) {
	double dx = SCALAR_DVAL(x);
	if (IS_SIMPLE_SCALAR(y, INTSXP)) {
	    int iy = SCALAR_IVAL(y);
	    if (ISNAN(dx) || iy == NA_INTEGER)
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), dx, iy);
	}
	else if (IS_SIMPLE_SCALAR(y, REALSXP)) {
	    double dy = SCALAR_DVAL(y);
	    if (ISNAN(dx) || ISNAN(dy))
		return ScalarLogical(NA_LOGICAL);
	    DO_SCALAR_RELOP(PRIMVAL(op), dx, dy);
	}
    }

    R_xlen_t
	nx = xlength(x),
	ny = xlength(y);
    SEXPTYPE
	typex = TYPEOF(x),
	typey = TYPEOF(y);

    /* handle the REALSXP/INTSXP simple vector/scalar case quickly. */
    if (ATTRIB(x) == R_NilValue && ATTRIB(y) == R_NilValue &&
	(typex == REALSXP || typex == INTSXP) &&
	(typey == REALSXP || typey == INTSXP) &&
	nx > 0 && ny > 0 && (nx == 1 || ny == 1)) {

	PROTECT(x);
	PROTECT(y);
	SEXP ans;
	ans = numeric_relop(PRIMVAL(op), x, y);
	UNPROTECT(2);
	return ans;
    }

    /* handle the general case */
    PROTECT_INDEX xpi, ypi;
    PROTECT_WITH_INDEX(x, &xpi);
    PROTECT_WITH_INDEX(y, &ypi);

    Rboolean iS;
    /* That symbols and calls were allowed was undocumented prior to
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

    if (isNull(x)) REPROTECT(x = allocVector(INTSXP,0), xpi);
    if (isNull(y)) REPROTECT(y = allocVector(INTSXP,0), ypi);
    if (!isVector(x) || !isVector(y))
	errorcall(call,
		  _("comparison (%d) is possible only for atomic and list types"),
		  PRIMVAL(op));

    if (TYPEOF(x) == EXPRSXP || TYPEOF(y) == EXPRSXP)
	errorcall(call, _("comparison is not allowed for expressions"));

    /* ELSE :  x and y are both atomic or list */

    Rboolean
	xarray = isArray(x),
	yarray = isArray(y),
	xts = isTs(x),
	yts = isTs(y);
    SEXP dims, xnames, ynames;
    if (xarray || yarray) {
	if (xarray && yarray) {
	    if (!conformable(x, y))
		errorcall(call, _("non-conformable arrays"));
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (xarray && (ny != 0 || nx == 0)) {
	    PROTECT(dims = getAttrib(x, R_DimSymbol));
	}
	else if (yarray && (nx != 0 || ny == 0)) {
	    PROTECT(dims = getAttrib(y, R_DimSymbol));
	} else
	    PROTECT(dims = R_NilValue);

	PROTECT(xnames = getAttrib(x, R_DimNamesSymbol));
	PROTECT(ynames = getAttrib(y, R_DimNamesSymbol));
    }
    else {
	PROTECT(dims = R_NilValue);
	PROTECT(xnames = getAttrib(x, R_NamesSymbol));
	PROTECT(ynames = getAttrib(y, R_NamesSymbol));
    }

    SEXP klass = NULL, tsp = NULL; // -Wall
    if (xts || yts) {
	if (xts && yts) {
	    /* could check ts conformance here */
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else if (xts) {
	    if (xlength(x) < xlength(y))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(x, R_TspSymbol));
	    PROTECT(klass = getAttrib(x, R_ClassSymbol));
	}
	else /*(yts)*/ {
	    if (xlength(y) < xlength(x))
		ErrorMessage(call, ERROR_TSVEC_MISMATCH);
	    PROTECT(tsp = getAttrib(y, R_TspSymbol));
	    PROTECT(klass = getAttrib(y, R_ClassSymbol));
	}
    }

  if (nx > 0 && ny > 0) {
	if(((nx > ny) ? nx % ny : ny % nx) != 0) // mismatch
            warningcall(call, _(
		"longer object length is not a multiple of shorter object length"));

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
    else if ((isNumeric(x) || isLogical(x)) && (isNumeric(y) || isLogical(y))) {
        x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    } // rest of cases only apply when 'x' or 'y' is raw
    else if (isReal(x) || isReal(y)) {
	REPROTECT(x = coerceVector(x, REALSXP), xpi);
	REPROTECT(y = coerceVector(y, REALSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isInteger(x) || isInteger(y)) {
	REPROTECT(x = coerceVector(x, INTSXP), xpi);
	REPROTECT(y = coerceVector(y, INTSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (isLogical(x) || isLogical(y)) {
	REPROTECT(x = coerceVector(x, LGLSXP), xpi);
	REPROTECT(y = coerceVector(y, LGLSXP), ypi);
	x = numeric_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    }
    else if (TYPEOF(x) == RAWSXP || TYPEOF(y) == RAWSXP) {
	REPROTECT(x = coerceVector(x, RAWSXP), xpi);
	REPROTECT(y = coerceVector(y, RAWSXP), ypi);
	x = raw_relop((RELOP_TYPE) PRIMVAL(op), x, y);
    } else errorcall(call, _("comparison of these types is not implemented"));
  } else { // nx == 0 || ny == 0
	x = allocVector(LGLSXP, 0);
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
	if (xnames != R_NilValue && xlength(x) == xlength(xnames))
	    setAttrib(x, R_NamesSymbol, xnames);
	else if (ynames != R_NilValue && xlength(x) == xlength(ynames))
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

#define ISNA_INT(x) x == NA_INTEGER

#define NR_HELPER(OP, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
	type1 x1, *px1 = ACCESSOR1(s1);					\
	type2 x2, *px2 = ACCESSOR2(s2);					\
	int *pa = LOGICAL(ans);						\
        MOD_ITERATE2(n, n1, n2, i, i1, i2, {                            \
	    x1 = px1[i1];						\
	    x2 = px2[i2];						\
            if (ISNA1(x1) || ISNA2(x2))                                 \
                pa[i] = NA_LOGICAL;					\
            else                                                        \
                pa[i] = (x1 OP x2);					\
        });                                                             \
    } while (0)

#define NUMERIC_RELOP(type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2) do { \
    switch (code) {                                                     \
    case EQOP:                                                          \
	NR_HELPER(==, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case NEOP:                                                          \
	NR_HELPER(!=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LTOP:                                                          \
	NR_HELPER(<, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GTOP:                                                          \
	NR_HELPER(>, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case LEOP:                                                          \
	NR_HELPER(<=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    case GEOP:                                                          \
	NR_HELPER(>=, type1, ACCESSOR1, ISNA1, type2, ACCESSOR2, ISNA2); \
        break;                                                          \
    }                                                                   \
} while(0)

static SEXP numeric_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    if (isInteger(s1) || isLogical(s1)) {
        if (isInteger(s2) || isLogical(s2)) {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, int, INTEGER, ISNA_INT);
        } else {
            NUMERIC_RELOP(int, INTEGER, ISNA_INT, double, REAL, ISNAN);
        }
    } else if (isInteger(s2) || isLogical(s2)) {
        NUMERIC_RELOP(double, REAL, ISNAN, int, INTEGER, ISNA_INT);
    } else {
        NUMERIC_RELOP(double, REAL, ISNAN, double, REAL, ISNAN);
    }

    UNPROTECT(2);
    return ans;
}

static SEXP complex_relop(RELOP_TYPE code, SEXP s1, SEXP s2, SEXP call)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rcomplex x1, x2;
    SEXP ans;

    if (code != EQOP && code != NEOP) {
	errorcall(call, _("invalid comparison with complex values"));
    }

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rcomplex *px1 = COMPLEX_RO(s1);
    const Rcomplex *px2 = COMPLEX_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r == x2.r && x1.i == x2.i);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    if (ISNAN(x1.r) || ISNAN(x1.i) ||
		ISNAN(x2.r) || ISNAN(x2.i))
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = (x1.r != x2.r || x1.i != x2.i);
	});
	break;
    default:
	/* never happens (-Wall) */
	break;
    }
    UNPROTECT(2);
    return ans;
}


/* POSIX allows EINVAL when one of the strings contains characters
   outside the collation domain. */
static SEXP string_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, n, n1, n2, res, i1, i2;
    SEXP ans, c1, c2;
    const void *vmax = vmaxget(); // for Scollate

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    PROTECT(ans = allocVector(LGLSXP, n));
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 1 : 0;
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else
		pa[i] = Seql(c1, c2) ? 0 : 1;
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res < 0) ? 1 : 0;
	    }
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i ,i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 0;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res > 0) ? 1 : 0;
	    }
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res <= 0) ? 1 : 0;
	    }
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    c1 = STRING_ELT(s1, i1);
	    c2 = STRING_ELT(s2, i2);
	    if (c1 == NA_STRING || c2 == NA_STRING)
		pa[i] = NA_LOGICAL;
	    else if (c1 == c2)
		pa[i] = 1;
	    else {
		errno = 0;
		res = Scollate(c1, c2);
		if(errno)
		    pa[i] = NA_LOGICAL;
		else
		    pa[i] = (res >= 0) ? 1 : 0;
	    }
	});
	break;
    }
    UNPROTECT(3);
    vmaxset(vmax);
    return ans;
}

static SEXP raw_relop(RELOP_TYPE code, SEXP s1, SEXP s2)
{
    R_xlen_t i, i1, i2, n, n1, n2;
    Rbyte x1, x2;
    SEXP ans;

    n1 = XLENGTH(s1);
    n2 = XLENGTH(s2);
    n = (n1 > n2) ? n1 : n2;
    PROTECT(s1);
    PROTECT(s2);
    ans = allocVector(LGLSXP, n);

    const Rbyte *px1 = RAW_RO(s1);
    const Rbyte *px2 = RAW_RO(s2);
    int *pa = LOGICAL(ans);

    switch (code) {
    case EQOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 == x2);
	});
	break;
    case NEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 != x2);
	});
	break;
    case LTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 < x2);
	});
	break;
    case GTOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 > x2);
	});
	break;
    case LEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 <= x2);
	});
	break;
    case GEOP:
	MOD_ITERATE2(n, n1, n2, i, i1, i2, {
//	    if ((i+1) % NINTERRUPT == 0) R_CheckUserInterrupt();
	    x1 = px1[i1];
	    x2 = px2[i2];
	    pa[i] = (x1 >= x2);
	});
	break;
    }
    UNPROTECT(2);
    return ans;
}


static SEXP bitwiseNot(SEXP a)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t m = XLENGTH(a);
	    ans = allocVector(INTSXP, m);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a);
	    for(R_xlen_t i = 0; i < m; i++) {
		int aa = pa[i];
		pans[i] = (aa == NA_INTEGER) ? aa : ~aa;
	    }
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitwNot", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

#define mymax(x, y) ((x >= y) ? x : y)

#define BIT(op, name)							\
    SEXP ans;								\
    int np = 0;								\
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}		\
    if(isReal(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}		\
    if (TYPEOF(a) != TYPEOF(b))						\
	error(_("'a' and 'b' must have the same type"));		\
    switch(TYPEOF(a)) {							\
    case INTSXP:							\
	{								\
	    R_xlen_t i, ia, ib;						\
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),			\
		mn = (m && n) ? mymax(m, n) : 0;			\
	    ans = allocVector(INTSXP, mn);				\
	    int *pans = INTEGER(ans);					\
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);		\
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {				\
		    int aa = pa[ia]; int bb = pb[ib];			\
		    pans[i] = (aa == NA_INTEGER || bb == NA_INTEGER) ?	\
			NA_INTEGER : aa op bb;				\
		});							\
	}								\
	break;								\
    default:								\
	UNIMPLEMENTED_TYPE(name, a);					\
    }									\
    if(np) UNPROTECT(np);						\
    return ans

static SEXP bitwiseAnd(SEXP a, SEXP b)
{
    BIT(&, "bitwAnd");
}

static SEXP bitwiseOr(SEXP a, SEXP b)
{
    BIT(|, "bitwOr");
}

static SEXP bitwiseXor(SEXP a, SEXP b)
{
    BIT(^, "bitwXor");
}

static SEXP bitwiseShiftL(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? mymax(m, n) : 0;
	    ans = allocVector(INTSXP, mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa << bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftL", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

static SEXP bitwiseShiftR(SEXP a, SEXP b)
{
    SEXP ans;
    int np = 0;
    if(isReal(a)) {a = PROTECT(coerceVector(a, INTSXP)); np++;}
    if(!isInteger(b)) {b = PROTECT(coerceVector(b, INTSXP)); np++;}
    if (TYPEOF(a) != TYPEOF(b))
	error(_("'a' and 'b' must have the same type"));

    switch(TYPEOF(a)) {
    case INTSXP:
	{
	    R_xlen_t i, ia, ib;
	    R_xlen_t m = XLENGTH(a), n = XLENGTH(b),
		mn = (m && n) ? mymax(m, n) : 0;
	    ans = allocVector(TYPEOF(a), mn);
	    int *pans = INTEGER(ans);
	    const int *pa = INTEGER_RO(a), *pb = INTEGER_RO(b);
	    MOD_ITERATE2(mn, m, n, i, ia, ib, {
		    int aa = pa[ia]; int bb = pb[ib];
		    pans[i] =
			(aa == NA_INTEGER || bb == NA_INTEGER ||
			 bb < 0 || bb > 31) ?
			NA_INTEGER : ((unsigned int)aa >> bb);
		});
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("bitShiftR", a);
    }
    if(np) UNPROTECT(np);
    return ans;
}

SEXP attribute_hidden do_bitwise(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP ans = R_NilValue; /* -Wall */
    switch(PRIMVAL(op)) {
    case 1: ans = bitwiseAnd(CAR(args), CADR(args)); break;
    case 2: ans = bitwiseNot(CAR(args)); break;
    case 3: ans = bitwiseOr(CAR(args), CADR(args)); break;
    case 4: ans = bitwiseXor(CAR(args), CADR(args)); break;
    case 5: ans = bitwiseShiftL(CAR(args), CADR(args)); break;
    case 6: ans = bitwiseShiftR(CAR(args), CADR(args)); break;
    }
    return ans;
}
