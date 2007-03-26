/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2006   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in util.c) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   We pro tem only use the GCC one.  See
   http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

#ifndef COMPILING_R /* defined only in util.c */
/* The following was added in gcc 4.1.3.  It is defined if
   GCC is following C99 inline semantics by default: we
   switch R's usage to the older GNU semantics via attributes. */
# ifdef __GNUC_STDC_INLINE__
#  define INLINE_FUN extern __attribute__((gnu_inline)) inline
# else
#  define INLINE_FUN extern R_INLINE
# endif
#else
# define INLINE_FUN
#endif

/* define inline-able functions */


/* from dstruct.c */

/* mkChar - make a character (CHARSXP) variable */

INLINE_FUN SEXP mkChar(const char *name)
{
    SEXP c = allocString(strlen(name));
    strcpy(CHAR(c), name);
    return c;
}

/*  length - length of objects  */

int Rf_envlength(SEXP rho);

INLINE_FUN R_len_t length(SEXP s)
{
    int i;
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
	return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    case ENVSXP:
	return Rf_envlength(s);
    default:
	return 1;
    }
}


/* from list.c */
/* Return a dotted pair with the given CAR and CDR. */
/* The (R) TAG slot on the cell is set to NULL. */


/* Get the i-th element of a list */
INLINE_FUN SEXP elt(SEXP list, int i)
{
    int j;
    SEXP result = list;

    if ((i < 0) || (i > length(list)))
	return R_NilValue;
    else
	for (j = 0; j < i; j++)
	    result = CDR(result);

    return CAR(result);
}


/* Return the last element of a list */
INLINE_FUN SEXP lastElt(SEXP list)
{
    SEXP result = R_NilValue;
    while (list != R_NilValue) {
	result = list;
	list = CDR(list);
    }
    return result;
}


/* Shorthands for creating small lists */

INLINE_FUN SEXP list1(SEXP s)
{
    return CONS(s, R_NilValue);
}


INLINE_FUN SEXP list2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = CONS(s, list1(t));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP list3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = CONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}


INLINE_FUN SEXP list4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = CONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}


/* Destructive list append : See also ``append'' */

INLINE_FUN SEXP listAppend(SEXP s, SEXP t)
{
    SEXP r;
    if (s == R_NilValue)
	return t;
    r = s;
    while (CDR(r) != R_NilValue)
	r = CDR(r);
    SETCDR(r, t);
    return s;
}


/* Language based list constructs.  These are identical to the list */
/* constructs, but the results can be evaluated. */

/* Return a (language) dotted pair with the given car and cdr */

INLINE_FUN SEXP lcons(SEXP car, SEXP cdr)
{
    SEXP e = cons(car, cdr);
    SET_TYPEOF(e, LANGSXP);
    return e;
}

INLINE_FUN SEXP lang1(SEXP s)
{
    return LCONS(s, R_NilValue);
}

INLINE_FUN SEXP lang2(SEXP s, SEXP t)
{
    PROTECT(s);
    s = LCONS(s, list1(t));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang3(SEXP s, SEXP t, SEXP u)
{
    PROTECT(s);
    s = LCONS(s, list2(t, u));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang4(SEXP s, SEXP t, SEXP u, SEXP v)
{
    PROTECT(s);
    s = LCONS(s, list3(t, u, v));
    UNPROTECT(1);
    return s;
}

/* from util.c */

/* Check to see if the arrays "x" and "y" have the identical extents */

INLINE_FUN Rboolean conformable(SEXP x, SEXP y)
{
    int i, n;
    PROTECT(x = getAttrib(x, R_DimSymbol));
    y = getAttrib(y, R_DimSymbol);
    UNPROTECT(1);
    if ((n = length(x)) != length(y))
	return FALSE;
    for (i = 0; i < n; i++)
	if (INTEGER(x)[i] != INTEGER(y)[i])
	    return FALSE;
    return TRUE;
}

INLINE_FUN Rboolean isString(SEXP s)
{
    return (TYPEOF(s) == STRSXP);
}

INLINE_FUN Rboolean isNull(SEXP s)
{
    return (s == R_NilValue);
}

INLINE_FUN Rboolean isObject(SEXP s)
{
    return OBJECT(s);/* really '1-bit unsigned int' */
}

/*  The following should work but as of 06/09/04 it generates warnings about 
    undeclared IS_S4_OBJECT, in spite of being apparently identical to the handling 
    of isObject() above ------
INLINE_FUN Rboolean isS4(SEXP s)
{
  return IS_S4_OBJECT(s);
}

INLINE_FUN SEXP asS4(SEXP s, Rboolean flag)
{
    if(flag == IS_S4_OBJECT(s))
        return s;
    if(NAMED(s) == 2)
        s = duplicate(s);
    if(flag) SET_S4_OBJECT(s);
    else UNSET_S4_OBJECT(s);
    return s;
}

*/

INLINE_FUN Rboolean inherits(SEXP s, char *name)
{
    SEXP klass;
    int i, nclass;
    if (isObject(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	nclass = length(klass);
	for (i = 0; i < nclass; i++) {
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), name))
		return TRUE;
	}
    }
    return FALSE;
}

INLINE_FUN Rboolean isValidString(SEXP x)
{
    return isString(x) && LENGTH(x) > 0 && !isNull(STRING_ELT(x, 0));
}

/* non-empty ("") valid string :*/
INLINE_FUN Rboolean isValidStringF(SEXP x)
{
    return isValidString(x) && CHAR(STRING_ELT(x, 0))[0];
}

INLINE_FUN Rboolean isSymbol(SEXP s)
{
    return TYPEOF(s) == SYMSXP;
}

INLINE_FUN Rboolean isUserBinop(SEXP s)
{
    if (isSymbol(s)) {
	char *str = CHAR(PRINTNAME(s));
	if (strlen(str) >= 2 && str[0] == '%' && str[strlen(str)-1] == '%')
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isFunction(SEXP s)
{
    return (TYPEOF(s) == CLOSXP ||
	    TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isPrimitive(SEXP s)
{
    return (TYPEOF(s) == BUILTINSXP ||
	    TYPEOF(s) == SPECIALSXP);
}

INLINE_FUN Rboolean isList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LISTSXP);
}


INLINE_FUN Rboolean isNewList(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == VECSXP);
}

INLINE_FUN Rboolean isPairList(SEXP s)
{
    switch (TYPEOF(s)) {
    case NILSXP:
    case LISTSXP:
    case LANGSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isVectorList(SEXP s)
{
    switch (TYPEOF(s)) {
    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isVectorAtomic(SEXP s)
{
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:
	return TRUE;
    default: /* including NULL */
	return FALSE;
    }
}

INLINE_FUN Rboolean isVector(SEXP s)/* === isVectorList() or isVectorAtomic() */
{
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case RAWSXP:

    case VECSXP:
    case EXPRSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

INLINE_FUN Rboolean isFrame(SEXP s)
{
    SEXP klass;
    int i;
    if (isObject(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	for (i = 0; i < length(klass); i++)
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), "data.frame")) return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isExpression(SEXP s)
{
    return TYPEOF(s) == EXPRSXP;
}

INLINE_FUN Rboolean isLanguage(SEXP s)
{
    return (s == R_NilValue || TYPEOF(s) == LANGSXP);
}

INLINE_FUN Rboolean isMatrix(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a non-integer dim,
	   although this might be possible by misuse of ATTRIB. */
	if (TYPEOF(t) == INTSXP && LENGTH(t) == 2)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isArray(SEXP s)
{
    SEXP t;
    if (isVector(s)) {
	t = getAttrib(s, R_DimSymbol);
	/* You are not supposed to be able to assign a 0-length dim,
	 nor a non-integer dim */
	if (TYPEOF(t) == INTSXP && LENGTH(t) > 0)
	    return TRUE;
    }
    return FALSE;
}

INLINE_FUN Rboolean isTs(SEXP s)
{
    return (isVector(s) && getAttrib(s, R_TspSymbol) != R_NilValue);
}


INLINE_FUN Rboolean isLogical(SEXP s)
{
    return (TYPEOF(s) == LGLSXP);
}

INLINE_FUN Rboolean isInteger(SEXP s)
{
    return (TYPEOF(s) == INTSXP && !inherits(s, "factor"));
}

INLINE_FUN Rboolean isReal(SEXP s)
{
    return (TYPEOF(s) == REALSXP);
}

INLINE_FUN Rboolean isComplex(SEXP s)
{
    return (TYPEOF(s) == CPLXSXP);
}

INLINE_FUN Rboolean isUnordered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && !inherits(s, "ordered"));
}

INLINE_FUN Rboolean isOrdered(SEXP s)
{
    return (TYPEOF(s) == INTSXP
	    && inherits(s, "factor")
	    && inherits(s, "ordered"));
}

INLINE_FUN Rboolean isFactor(SEXP s)
{
    return (TYPEOF(s) == INTSXP  && inherits(s, "factor"));
}

INLINE_FUN Rboolean isEnvironment(SEXP s)
{
    return (TYPEOF(s) == ENVSXP);
}

INLINE_FUN int nlevels(SEXP f)
{
    if (!isFactor(f))
	return 0;
    return LENGTH(getAttrib(f, R_LevelsSymbol));
}


/* Is an object of numeric type. */
/* FIXME:  the LGLSXP case should be excluded here
 * (really? in many places we affirm they are treated like INTs)*/

INLINE_FUN Rboolean isNumeric(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inherits(s,"factor")) return FALSE;
    case LGLSXP:
    case REALSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/* As from R 2.4.0 we check that the value is allowed. */
INLINE_FUN SEXP ScalarLogical(int x)
{
    SEXP ans = allocVector(LGLSXP, 1);
    if (x == NA_LOGICAL) LOGICAL(ans)[0] = NA_LOGICAL;
    else LOGICAL(ans)[0] = (x != 0);
    return ans;
}

INLINE_FUN SEXP ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, 1);
    INTEGER(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, 1);
    REAL(ans)[0] = x;
    return ans;
}


INLINE_FUN SEXP ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, 1);
    COMPLEX(ans)[0] = x;
    return ans;
}

INLINE_FUN SEXP ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, 1);
    SET_STRING_ELT(ans, 0, x);
    UNPROTECT(1);
    return ans;
}

INLINE_FUN SEXP ScalarRaw(Rbyte x)
{
    SEXP ans = allocVector(RAWSXP, 1);
    RAW(ans)[0] = x;
    return ans;
}

/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */
/* BUT it does not exclude 0! */

INLINE_FUN Rboolean isVectorizable(SEXP s)
{
    if (isNull(s)) return TRUE;
    else if (isNewList(s)) {
	int i, n;

	n = LENGTH(s);
	for (i = 0 ; i < n; i++)
	    if (!isVector(VECTOR_ELT(s, i)) || LENGTH(VECTOR_ELT(s, i)) > 1)
		return FALSE;
	return TRUE;
    }
    else if (isList(s)) {
	for ( ; s != R_NilValue; s = CDR(s))
	    if (!isVector(CAR(s)) || LENGTH(CAR(s)) > 1) return FALSE;
	return TRUE;
    }
    else return FALSE;
}


/* from gram.y */

INLINE_FUN SEXP mkString(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkChar(s));
    UNPROTECT(1);
    return t;
}

/* from Rmath */

#define Rf_fmin2(x, y) fmin2_int(x, y)
#define Rf_fmax2(x, y) fmax2_int(x, y)
#define Rf_imin2(x, y) imin2_int(x, y)
#define Rf_imax2(x, y) imax2_int(x, y)
#define Rf_fsign(x, y) fsign_int(x, y)

INLINE_FUN double fmin2_int(double x, double y)
{
	if (ISNAN(x) || ISNAN(y))
		return x + y;
	return (x < y) ? x : y;
}

INLINE_FUN double fmax2_int(double x, double y)
{
	if (ISNAN(x) || ISNAN(y))
		return x + y;
	return (x < y) ? y : x;
}

INLINE_FUN int imin2_int(int x, int y)
{
    return (x < y) ? x : y;
}

INLINE_FUN int imax2_int(int x, int y)
{
    return (x < y) ? y : x;
}

INLINE_FUN double fsign_int(double x, double y)
{
    if (ISNAN(x) || ISNAN(y))
	return x + y;
    return ((y >= 0) ? fabs(x) : -fabs(x));
}


#endif /* R_INLINES_H_ */
