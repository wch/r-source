/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2017  The R Core Team.
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

/* Internal header, not installed */

/* this header is always to be included from others.
   It is only called if COMPILING_R is defined (in util.c) or
   from GNU C systems.

   There are different conventions for inlining across compilation units.
   See http://www.greenend.org.uk/rjk/2003/03/inline.html
 */
#ifndef R_INLINES_H_
#define R_INLINES_H_

/* Probably not able to use C99 semantics in gcc < 4.3.0 */
#if __GNUC__ == 4 && __GNUC_MINOR__ >= 3 && defined(__GNUC_STDC_INLINE__) && !defined(C99_INLINE_SEMANTICS)
#define C99_INLINE_SEMANTICS 1
#endif

/* Apple's gcc build >5400 (since Xcode 3.0) doesn't support GNU inline in C99 mode */
#if __APPLE_CC__ > 5400 && !defined(C99_INLINE_SEMANTICS) && __STDC_VERSION__ >= 199901L
#define C99_INLINE_SEMANTICS 1
#endif

#ifdef COMPILING_R
/* defined only in inlined.c: this emits standalone code there */
# define INLINE_FUN
#else
/* This section is normally only used for versions of gcc which do not
   support C99 semantics.  __GNUC_STDC_INLINE__ is defined if
   GCC is following C99 inline semantics by default: we
   switch R's usage to the older GNU semantics via attributes.
   Do this even for __GNUC_GNUC_INLINE__ to shut up warnings in 4.2.x.
   __GNUC_STDC_INLINE__ and __GNUC_GNU_INLINE__ were added in gcc 4.2.0.
*/
# if defined(__GNUC_STDC_INLINE__) || defined(__GNUC_GNU_INLINE__)
#  define INLINE_FUN extern __attribute__((gnu_inline)) inline
# else
#  define INLINE_FUN extern R_INLINE
# endif
#endif /* ifdef COMPILING_R */

#if C99_INLINE_SEMANTICS
# undef INLINE_FUN
# ifdef COMPILING_R
/* force exported copy */
#  define INLINE_FUN extern inline
# else
/* either inline or link to extern version at compiler's choice */
#  define INLINE_FUN inline
# endif /* ifdef COMPILING_R */
#endif /* C99_INLINE_SEMANTICS */


#include <string.h> /* for strlen, strcmp */

/* define inline-able functions */
#ifdef TESTING_WRITE_BARRIER
# define STRICT_TYPECHECK
# define CATCH_ZERO_LENGTH_ACCESS
#endif

#ifdef STRICT_TYPECHECK
INLINE_FUN void CHKVEC(SEXP x) {
    switch (TYPEOF(x)) {
    case CHARSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case VECSXP:
    case EXPRSXP:
    case RAWSXP:
    case WEAKREFSXP:
	break;
    default:
	error("cannot get data pointer of '%s' objects", type2char(TYPEOF(x)));
    }
}
#else
# define CHKVEC(x) do {} while(0)
#endif

INLINE_FUN void *DATAPTR(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return ALTVEC_DATAPTR(x);
#ifdef CATCH_ZERO_LENGTH_ACCESS
    /* Attempts to read or write elements of a zero length vector will
       result in a segfault, rather than read and write random memory.
       Returning NULL would be more natural, but Matrix seems to assume
       that even zero-length vectors have non-NULL data pointers, so
       return (void *) 1 instead. Zero-length CHARSXP objects still
       have a trailing zero byte so they are not handled. */
    else if (STDVEC_LENGTH(x) == 0 && TYPEOF(x) != CHARSXP)
	return (void *) 1;
#endif
    else
	return STDVEC_DATAPTR(x);
}

INLINE_FUN const void *DATAPTR_RO(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return ALTVEC_DATAPTR_RO(x);
    else
	return STDVEC_DATAPTR(x);
}

INLINE_FUN const void *DATAPTR_OR_NULL(SEXP x) {
    CHKVEC(x);
    if (ALTREP(x))
	return ALTVEC_DATAPTR_OR_NULL(x);
    else
	return STDVEC_DATAPTR(x);
}

#ifdef STRICT_TYPECHECK
# define CHECK_VECTOR_LGL(x) do {				\
	if (TYPEOF(x) != LGLSXP) error("bad LGLSXP vector");	\
    } while (0)
# define CHECK_VECTOR_INT(x) do {				\
	if (! (TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP))	\
	    error("bad INTSXP vector");				\
    } while (0)
# define CHECK_VECTOR_REAL(x) do {				\
	if (TYPEOF(x) != REALSXP) error("bad REALSXP vector");	\
    } while (0)
# define CHECK_VECTOR_CPLX(x) do {				\
	if (TYPEOF(x) != CPLXSXP) error("bad CPLXSXP vector");	\
    } while (0)
# define CHECK_VECTOR_RAW(x) do {				\
	if (TYPEOF(x) != RAWSXP) error("bad RAWSXP vector");	\
    } while (0)
#else
# define CHECK_VECTOR_LGL(x) do { } while(0)
# define CHECK_VECTOR_INT(x) do { } while(0)
# define CHECK_VECTOR_REAL(x) do { } while(0)
# define CHECK_VECTOR_CPLX(x) do { } while(0)
# define CHECK_VECTOR_RAW(x) do { } while(0)
#endif

INLINE_FUN const int *LOGICAL_OR_NULL(SEXP x) {
    CHECK_VECTOR_LGL(x);
    return ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x);
}

INLINE_FUN const int *INTEGER_OR_NULL(SEXP x) {
    CHECK_VECTOR_INT(x);
    return ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x);
}

INLINE_FUN const double *REAL_OR_NULL(SEXP x) {
    CHECK_VECTOR_REAL(x);
    return ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x);
}

INLINE_FUN const double *COMPLEX_OR_NULL(SEXP x) {
    CHECK_VECTOR_CPLX(x);
    return ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x);
}

INLINE_FUN const double *RAW_OR_NULL(SEXP x) {
    CHECK_VECTOR_RAW(x);
    return ALTREP(x) ? ALTVEC_DATAPTR_OR_NULL(x) : STDVEC_DATAPTR(x);
}

INLINE_FUN R_xlen_t XLENGTH_EX(SEXP x)
{
    return ALTREP(x) ? ALTREP_LENGTH(x) : STDVEC_LENGTH(x);
}

INLINE_FUN R_xlen_t XTRUELENGTH(SEXP x)
{
    return ALTREP(x) ? ALTREP_TRUELENGTH(x) : STDVEC_TRUELENGTH(x);
}

INLINE_FUN int LENGTH_EX(SEXP x, const char *file, int line)
{
    if (x == R_NilValue) return 0;
    R_xlen_t len = XLENGTH(x);
#ifdef LONG_VECTOR_SUPPORT
    if (len > R_SHORT_LEN_MAX)
	R_BadLongVector(x, file, line);
#endif
    return (int) len;
}

#ifdef STRICT_TYPECHECK
# define CHECK_STDVEC_LGL(x) do {				\
	CHECK_VECTOR_LGL(x);					\
	if (ALTREP(x)) error("bad standard LGLSXP vector");	\
    } while (0)
# define CHECK_STDVEC_INT(x) do {				\
	CHECK_VECTOR_INT(x);					\
	if (ALTREP(x)) error("bad standard INTSXP vector");	\
    } while (0)
# define CHECK_STDVEC_REAL(x) do {				\
	CHECK_VECTOR_REAL(x);					\
	if (ALTREP(x)) error("bad standard REALSXP vector");	\
    } while (0)
# define CHECK_STDVEC_CPLX(x) do {				\
	CHECK_VECTOR_CPLX(x);					\
	if (ALTREP(x)) error("bad standard CPLXSXP vector");	\
    } while (0)
# define CHECK_STDVEC_RAW(x) do {				\
	CHECK_VECTOR_RAW(x);					\
	if (ALTREP(x)) error("bad standard RAWSXP vector");	\
    } while (0)

# define CHECK_SCALAR_LGL(x) do {				\
	CHECK_STDVEC_LGL(x);					\
	if (XLENGTH(x) != 1) error("bad LGLSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_INT(x) do {				\
	CHECK_STDVEC_INT(x);					\
	if (XLENGTH(x) != 1) error("bad INTSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_REAL(x) do {				\
	CHECK_STDVEC_REAL(x);					\
	if (XLENGTH(x) != 1) error("bad REALSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_CPLX(x) do {				\
	CHECK_STDVEC_CPLX(x);					\
	if (XLENGTH(x) != 1) error("bad CPLXSXP scalar");	\
    } while (0)
# define CHECK_SCALAR_RAW(x) do {				\
	CHECK_STDVEC_RAW(x);					\
	if (XLENGTH(x) != 1) error("bad RAWSXP scalar");	\
    } while (0)

# define CHECK_BOUNDS_ELT(x, i) do {			\
	if (i < 0 || i > XLENGTH(x))			\
	    error("subscript out of bounds");		\
    } while (0)

# define CHECK_VECTOR_LGL_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_LGL(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_INT_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_INT(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_REAL_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_REAL(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_CPLX_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_CPLX(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
# define CHECK_VECTOR_RAW_ELT(x, i) do {	\
	SEXP ce__x__ = (x);			\
	R_xlen_t ce__i__ = (i);			\
	CHECK_VECTOR_RAW(ce__x__);		\
	CHECK_BOUNDS_ELT(ce__x__, ce__i__);	\
} while (0)
#else
# define CHECK_STDVEC_LGL(x) do { } while(0)
# define CHECK_STDVEC_INT(x) do { } while(0)
# define CHECK_STDVEC_REAL(x) do { } while(0)
# define CHECK_STDVEC_CPLX(x) do { } while(0)
# define CHECK_STDVEC_RAW(x) do { } while(0)

# define CHECK_SCALAR_LGL(x) do { } while(0)
# define CHECK_SCALAR_INT(x) do { } while(0)
# define CHECK_SCALAR_REAL(x) do { } while(0)
# define CHECK_SCALAR_CPLX(x) do { } while(0)
# define CHECK_SCALAR_RAW(x) do { } while(0)

# define CHECK_VECTOR_LGL_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_INT_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_REAL_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_CPLX_ELT(x, i) do { } while(0)
# define CHECK_VECTOR_RAW_ELT(x, i) do { } while(0)
#endif

INLINE_FUN int *LOGICAL0(SEXP x) {
    CHECK_STDVEC_LGL(x);
    return (int *) STDVEC_DATAPTR(x);
}
INLINE_FUN Rboolean SCALAR_LVAL(SEXP x) {
    CHECK_SCALAR_LGL(x);
    return LOGICAL0(x)[0];
}
INLINE_FUN void SET_SCALAR_LVAL(SEXP x, Rboolean v) {
    CHECK_SCALAR_LGL(x);
    LOGICAL0(x)[0] = v;
}

INLINE_FUN int *INTEGER0(SEXP x) {
    CHECK_STDVEC_INT(x);
    return (int *) STDVEC_DATAPTR(x);
}
INLINE_FUN int SCALAR_IVAL(SEXP x) {
    CHECK_SCALAR_INT(x);
    return INTEGER0(x)[0];
}
INLINE_FUN void SET_SCALAR_IVAL(SEXP x, int v) {
    CHECK_SCALAR_INT(x);
    INTEGER0(x)[0] = v;
}

INLINE_FUN double *REAL0(SEXP x) {
    CHECK_STDVEC_REAL(x);
    return (double *) STDVEC_DATAPTR(x);
}
INLINE_FUN double SCALAR_DVAL(SEXP x) {
    CHECK_SCALAR_REAL(x);
    return REAL0(x)[0];
}
INLINE_FUN void SET_SCALAR_DVAL(SEXP x, double v) {
    CHECK_SCALAR_REAL(x);
    REAL0(x)[0] = v;
}

INLINE_FUN Rcomplex *COMPLEX0(SEXP x) {
    CHECK_STDVEC_CPLX(x);
    return (Rcomplex *) STDVEC_DATAPTR(x);
}
INLINE_FUN Rcomplex SCALAR_CVAL(SEXP x) {
    CHECK_SCALAR_CPLX(x);
    return COMPLEX0(x)[0];
}
INLINE_FUN void SET_SCALAR_CVAL(SEXP x, Rcomplex v) {
    CHECK_SCALAR_CPLX(x);
    COMPLEX0(x)[0] = v;
}

INLINE_FUN Rbyte *RAW0(SEXP x) {
    CHECK_STDVEC_RAW(x);
    return (Rbyte *) STDVEC_DATAPTR(x);
}
INLINE_FUN Rbyte SCALAR_BVAL(SEXP x) {
    CHECK_SCALAR_RAW(x);
    return RAW0(x)[0];
}
INLINE_FUN void SET_SCALAR_BVAL(SEXP x, Rbyte v) {
    CHECK_SCALAR_RAW(x);
    RAW0(x)[0] = v;
}

INLINE_FUN SEXP ALTREP_CLASS(SEXP x) { return TAG(x); }

INLINE_FUN SEXP R_altrep_data1(SEXP x) { return CAR(x); }
INLINE_FUN SEXP R_altrep_data2(SEXP x) { return CDR(x); }
INLINE_FUN void R_set_altrep_data1(SEXP x, SEXP v) { SETCAR(x, v); }
INLINE_FUN void R_set_altrep_data2(SEXP x, SEXP v) { SETCDR(x, v); }

INLINE_FUN int INTEGER_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_INT_ELT(x, i);
    return ALTREP(x) ? ALTINTEGER_ELT(x, i) : INTEGER0(x)[i];
}

INLINE_FUN void SET_INTEGER_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_INT_ELT(x, i);
    if (ALTREP(x)) ALTINTEGER_SET_ELT(x, i, v);
    else INTEGER0(x)[i] = v;
}

INLINE_FUN int LOGICAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    return ALTREP(x) ? ALTLOGICAL_ELT(x, i) : LOGICAL0(x)[i];
}

INLINE_FUN void SET_LOGICAL_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    if (ALTREP(x)) ALTLOGICAL_SET_ELT(x, i, v);
    else LOGICAL0(x)[i] = v;
}

INLINE_FUN double REAL_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    return ALTREP(x) ? ALTREAL_ELT(x, i) : REAL0(x)[i];
}

INLINE_FUN void SET_REAL_ELT(SEXP x, R_xlen_t i, double v)
{
    CHECK_VECTOR_REAL_ELT(x, i);
    if (ALTREP(x)) ALTREAL_SET_ELT(x, i, v);
    else REAL0(x)[i] = v;
}

INLINE_FUN Rcomplex COMPLEX_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    return ALTREP(x) ? ALTCOMPLEX_ELT(x, i) : COMPLEX0(x)[i];
}

INLINE_FUN void SET_COMPLEX_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    CHECK_VECTOR_CPLX_ELT(x, i);
    if (ALTREP(x)) ALTCOMPLEX_SET_ELT(x, i, v);
    else COMPLEX0(x)[i] = v;
}

INLINE_FUN Rbyte RAW_ELT(SEXP x, R_xlen_t i)
{
    CHECK_VECTOR_RAW_ELT(x, i);
    return ALTREP(x) ? ALTRAW_ELT(x, i) : RAW0(x)[i];
}

INLINE_FUN void SET_RAW_ELT(SEXP x, R_xlen_t i, int v)
{
    CHECK_VECTOR_LGL_ELT(x, i);
    if (ALTREP(x)) ALTLOGICAL_SET_ELT(x, i, v);
    else RAW0(x)[i] = (Rbyte) v;
}

#if !defined(COMPILING_R) && !defined(COMPILING_MEMORY_C) &&	\
    !defined(TESTING_WRITE_BARRIER)
/* if not inlining use version in memory.c with more error checking */
INLINE_FUN SEXP STRING_ELT(SEXP x, R_xlen_t i) {
    if (ALTREP(x))
	return ALTSTRING_ELT(x, i);
    else {
	SEXP *ps = STDVEC_DATAPTR(x);
	return ps[i];
    }
}
#else
SEXP STRING_ELT(SEXP x, R_xlen_t i);
#endif

#ifdef INLINE_PROTECT
extern int R_PPStackSize;
extern int R_PPStackTop;
extern SEXP* R_PPStack;

INLINE_FUN SEXP protect(SEXP s)
{
    if (R_PPStackTop < R_PPStackSize)
	R_PPStack[R_PPStackTop++] = s;
    else R_signal_protect_error();
    return s;
}

INLINE_FUN void unprotect(int l)
{
#ifdef PROTECT_PARANOID
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else R_signal_unprotect_error();
#else
    R_PPStackTop -= l;
#endif
}

INLINE_FUN void R_ProtectWithIndex(SEXP s, PROTECT_INDEX *pi)
{
    protect(s);
    *pi = R_PPStackTop - 1;
}

INLINE_FUN void R_Reprotect(SEXP s, PROTECT_INDEX i)
{
    if (i >= R_PPStackTop || i < 0)
	R_signal_reprotect_error(i);
    R_PPStack[i] = s;
}
#endif /* INLINE_PROTECT */

/* from dstruct.c */

/*  length - length of objects  */

int Rf_envlength(SEXP rho);

/* TODO: a  Length(.) {say} which is  length() + dispatch (S3 + S4) if needed
         for one approach, see do_seq_along() in ../main/seq.c
*/
INLINE_FUN R_len_t length(SEXP s)
{
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
    {
	int i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return Rf_envlength(s);
    default:
	return 1;
    }
}

R_xlen_t Rf_envxlength(SEXP rho);

INLINE_FUN R_xlen_t xlength(SEXP s)
{
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
	return XLENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
    {
	// it is implausible this would be >= 2^31 elements, but allow it
	R_xlen_t i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    }
    case ENVSXP:
	return Rf_envxlength(s);
    default:
	return 1;
    }
}

/* regular allocVector() as a special case of allocVector3() with no custom allocator */
INLINE_FUN SEXP allocVector(SEXPTYPE type, R_xlen_t length)
{
    return allocVector3(type, length, NULL);
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

INLINE_FUN SEXP list5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = CONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP list6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = CONS(s, list5(t, u, v, w, x));
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

INLINE_FUN SEXP lang5(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w)
{
    PROTECT(s);
    s = LCONS(s, list4(t, u, v, w));
    UNPROTECT(1);
    return s;
}

INLINE_FUN SEXP lang6(SEXP s, SEXP t, SEXP u, SEXP v, SEXP w, SEXP x)
{
    PROTECT(s);
    s = LCONS(s, list5(t, u, v, w, x));
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

/* NOTE: R's inherits() is based on inherits3() in ../main/objects.c
 * Here, use char / CHAR() instead of the slower more general translateChar()
 */
INLINE_FUN Rboolean inherits(SEXP s, const char *name)
{
    SEXP klass;
    int i, nclass;
    if (OBJECT(s)) {
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
    return TYPEOF(x) == STRSXP && LENGTH(x) > 0 && TYPEOF(STRING_ELT(x, 0)) != NILSXP;
}

/* non-empty ("") valid string :*/
INLINE_FUN Rboolean isValidStringF(SEXP x)
{
    return isValidString(x) && CHAR(STRING_ELT(x, 0))[0];
}

INLINE_FUN Rboolean isUserBinop(SEXP s)
{
    if (TYPEOF(s) == SYMSXP) {
	const char *str = CHAR(PRINTNAME(s));
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
    case DOTSXP:
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
    if (OBJECT(s)) {
	klass = getAttrib(s, R_ClassSymbol);
	for (i = 0; i < length(klass); i++)
	    if (!strcmp(CHAR(STRING_ELT(klass, i)), "data.frame")) return TRUE;
    }
    return FALSE;
}

/* DIFFERENT than R's  is.language(.) in ../main/coerce.c [do_is(), case 301:]
 *                                    which is   <=>  SYMSXP || LANGSXP || EXPRSXP */
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


INLINE_FUN Rboolean isInteger(SEXP s)
{
    return (TYPEOF(s) == INTSXP && !inherits(s, "factor"));
}

INLINE_FUN Rboolean isFactor(SEXP s)
{
    return (TYPEOF(s) == INTSXP  && inherits(s, "factor"));
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

/** Is an object "Numeric" or  complex */
INLINE_FUN Rboolean isNumber(SEXP s)
{
    switch(TYPEOF(s)) {
    case INTSXP:
	if (inherits(s,"factor")) return FALSE;
    case LGLSXP:
    case REALSXP:
    case CPLXSXP:
	return TRUE;
    default:
	return FALSE;
    }
}

/* As from R 2.4.0 we check that the value is allowed. */
INLINE_FUN SEXP ScalarLogical(int x)
{
    extern SEXP R_LogicalNAValue, R_TrueValue, R_FalseValue;
    if (x == NA_LOGICAL) return R_LogicalNAValue;
    else if (x != 0) return R_TrueValue;
    else return R_FalseValue;
}

INLINE_FUN SEXP ScalarInteger(int x)
{
    SEXP ans = allocVector(INTSXP, 1);
    SET_SCALAR_IVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP ScalarReal(double x)
{
    SEXP ans = allocVector(REALSXP, 1);
    SET_SCALAR_DVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP ScalarComplex(Rcomplex x)
{
    SEXP ans = allocVector(CPLXSXP, 1);
    SET_SCALAR_CVAL(ans, x);
    return ans;
}

INLINE_FUN SEXP ScalarString(SEXP x)
{
    SEXP ans;
    PROTECT(x);
    ans = allocVector(STRSXP, (R_xlen_t)1);
    SET_STRING_ELT(ans, (R_xlen_t)0, x);
    UNPROTECT(1);
    return ans;
}

INLINE_FUN SEXP ScalarRaw(Rbyte x)
{
    SEXP ans = allocVector(RAWSXP, 1);
    SET_SCALAR_BVAL(ans, x);
    return ans;
}

/* Check to see if a list can be made into a vector. */
/* it must have every element being a vector of length 1. */
/* BUT it does not exclude 0! */

INLINE_FUN Rboolean isVectorizable(SEXP s)
{
    if (s == R_NilValue) return TRUE;
    else if (isNewList(s)) {
	R_xlen_t i, n;

	n = XLENGTH(s);
	for (i = 0 ; i < n; i++)
	    if (!isVector(VECTOR_ELT(s, i)) || XLENGTH(VECTOR_ELT(s, i)) > 1)
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


/**
 * Create a named vector of type TYP
 *
 * @example const char *nms[] = {"xi", "yi", "zi", ""};
 *          mkNamed(VECSXP, nms);  =~= R  list(xi=, yi=, zi=)
 *
 * @param TYP a vector SEXP type (e.g. REALSXP)
 * @param names names of list elements with null string appended
 *
 * @return (pointer to a) named vector of type TYP
 */
INLINE_FUN SEXP mkNamed(SEXPTYPE TYP, const char **names)
{
    SEXP ans, nms;
    R_xlen_t i, n;

    for (n = 0; strlen(names[n]) > 0; n++) {}
    ans = PROTECT(allocVector(TYP, n));
    nms = PROTECT(allocVector(STRSXP, n));
    for (i = 0; i < n; i++)
	SET_STRING_ELT(nms, i, mkChar(names[i]));
    setAttrib(ans, R_NamesSymbol, nms);
    UNPROTECT(2);
    return ans;
}

/* from gram.y */

/* short cut for  ScalarString(mkChar(s)) : */
INLINE_FUN SEXP mkString(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, (R_xlen_t)1));
    SET_STRING_ELT(t, (R_xlen_t)0, mkChar(s));
    UNPROTECT(1);
    return t;
}

/* index of a given C string in (translated) R string vector  */
INLINE_FUN int
stringPositionTr(SEXP string, const char *translatedElement) {

    int slen = LENGTH(string);
    int i;

    const void *vmax = vmaxget();
    for (i = 0 ; i < slen; i++) {
	Rboolean found = ! strcmp(translateChar(STRING_ELT(string, i)),
				  translatedElement);
	vmaxset(vmax);
        if (found)
            return i;
    }
    return -1; /* not found */
}

/* duplicate RHS value of complex assignment if necessary to prevent cycles */
INLINE_FUN SEXP R_FixupRHS(SEXP x, SEXP y)
{
    if( y != R_NilValue && MAYBE_REFERENCED(y) ) {
	if (R_cycle_detected(x, y)) {
#ifdef WARNING_ON_CYCLE_DETECT
	    warning("cycle detected");
	    R_cycle_detected(x, y);
#endif
	    y = duplicate(y);
	}
	else ENSURE_NAMEDMAX(y);
    }
    return y;
}
#endif /* R_INLINES_H_ */
