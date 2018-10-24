/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016--2017   The R Core Team
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
#include <R_ext/Altrep.h>
#include <float.h> /* for DBL_DIG */
#include <Print.h> /* for R_print */
#include <R_ext/Itermacros.h>


/***
 *** ALTREP Concrete Class Implementations
 ***/

/**
 ** Compact Integer Sequences
 **/

/*
 * Methods
 */

#define COMPACT_SEQ_INFO(x) R_altrep_data1(x)
#define COMPACT_SEQ_EXPANDED(x) R_altrep_data2(x)
#define SET_COMPACT_SEQ_EXPANDED(x, v) R_set_altrep_data2(x, v)

/* needed for now for objects serialized with INTSXP state */
#define COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[0] : (R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[1] : (int) REAL0(info)[1])
#define COMPACT_INTSEQ_SERIALIZED_STATE_INCR(info) \
    (TYPEOF(info) == INTSXP ? INTEGER0(info)[2] : (int) REAL0(info)[2])

/* info is stored as REALSXP to allow for long vector length */
#define COMPACT_INTSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_INTSEQ_INFO_FIRST(info) ((int) REAL0(info)[1])
#define COMPACT_INTSEQ_INFO_INCR(info) ((int) REAL0(info)[2])

/* By default, compact integer sequences are marked as not mutable at
   creation time. Thus even when expanded the expanded data will
   correspond to the original integer sequence (unless it runs into
   mis-behaving C code). If COMPACT_INTSEQ_MUTABLE is defined, then
   the sequence is not marked as not mutable. Once the DATAPTR has
   been requested and releases, the expanded data might be modified by
   an assignment and no longer correspond to the original sequence. */
//#define COMPACT_INTSEQ_MUTABLE

static SEXP compact_intseq_Serialized_state(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard serialization for expanded
       compact vectors */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return NULL;
#endif
    return COMPACT_SEQ_INFO(x);
}

static SEXP new_compact_intseq(R_xlen_t, int, int);
static SEXP new_compact_realseq(R_xlen_t, double, double);

static SEXP compact_intseq_Unserialize(SEXP class, SEXP state)
{
    R_xlen_t n = COMPACT_INTSEQ_SERIALIZED_STATE_LENGTH(state);
    int n1 = COMPACT_INTSEQ_SERIALIZED_STATE_FIRST(state);
    int inc = COMPACT_INTSEQ_SERIALIZED_STATE_INCR(state);

    if (inc == 1)
	return new_compact_intseq(n, n1,  1);
    else if (inc == -1)
	return new_compact_intseq(n, n1,  -1);
    else
	error("compact sequences with increment %d not supported yet", inc);
}
 
static SEXP compact_intseq_Coerce(SEXP x, int type)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* This drops through to standard coercion for expanded compact
       vectors */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return NULL;
#endif
    if (type == REALSXP) {
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return new_compact_realseq(n, n1, inc);
    }
    else return NULL;
}

static SEXP compact_intseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = allocVector(INTSXP, n);
    INTEGER_GET_REGION(x, 0, n, INTEGER0(val));
    return val;
}

static
Rboolean compact_intseq_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

#ifdef COMPACT_INTSEQ_MUTABLE
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) {
	Rprintf("  <expanded compact integer sequence>\n");
	inspect_subtree(COMPACT_SEQ_EXPANDED(x), pre, deep, pvec);
	return TRUE;
    }
#endif

    int n = LENGTH(x);
    int n1 = INTEGER_ELT(x, 0);
    int n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %d : %d (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_intseq_Length(SEXP x)
{
    SEXP info = COMPACT_SEQ_INFO(x);
    return COMPACT_INTSEQ_INFO_LENGTH(info);
}

static void *compact_intseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	/* no need to re-run if expanded data exists */
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	SEXP val = allocVector(INTSXP, n);
	int *data = INTEGER(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (int i = 0; i < n; i++)
		data[i] = n1 + i;
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (int i = 0; i < n; i++)
		data[i] = n1 - i;
	}
	else
	    error("compact sequences with increment %d not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_intseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static int compact_intseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != R_NilValue)
	return INTEGER0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return (int) (n1 + inc * i);
    }
}

#define CHECK_NOT_EXPANDED(x)					\
    if (DATAPTR_OR_NULL(x) != NULL)				\
	error("method should only handle unexpanded vectors")

static R_xlen_t
compact_intseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 + k + i);
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = (int) (n1 - k - i);
	return ncopy;
    }
    else
	error("compact sequences with increment %d not supported yet", inc);
}

static int compact_intseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_intseq_No_NA(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return FALSE;
#endif
    return TRUE;
}

/* XXX this also appears in summary.c. move to header file?*/
#define R_INT_MIN (1 + INT_MIN)

static SEXP compact_intseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) 
	return NULL;
#endif
    double tmp;
    SEXP info = COMPACT_SEQ_INFO(x);
    R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
    R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);
    tmp = (size / 2.0) * (n1 + n1 + inc * (size - 1));
    if(tmp > INT_MAX || tmp < R_INT_MIN)
	/**** check for overflow of exact integer range? */
	return ScalarReal(tmp);
    else
	return ScalarInteger((int) tmp);
}


/*
 * Class Objects and Method Tables
 */

R_altrep_class_t R_compact_intseq_class;

static void InitCompactIntegerClass()
{
    R_altrep_class_t cls = R_make_altinteger_class("compact_intseq", "base",
						   NULL);
    R_compact_intseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_intseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_intseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_intseq_Duplicate);
    R_set_altrep_Coerce_method(cls, compact_intseq_Coerce);
    R_set_altrep_Inspect_method(cls, compact_intseq_Inspect);
    R_set_altrep_Length_method(cls, compact_intseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_intseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_intseq_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, compact_intseq_Elt);
    R_set_altinteger_Get_region_method(cls, compact_intseq_Get_region);
    R_set_altinteger_Is_sorted_method(cls, compact_intseq_Is_sorted);
    R_set_altinteger_No_NA_method(cls, compact_intseq_No_NA);
    R_set_altinteger_Sum_method(cls, compact_intseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_intseq(R_xlen_t n, int n1, int inc)
{
    if (n == 1) return ScalarInteger(n1);

    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

    /* info used REALSXP to allow for long vectors */
    SEXP info = allocVector(REALSXP, 3);
    REAL0(info)[0] = (double) n;
    REAL0(info)[1] = (double) n1;
    REAL0(info)[2] = (double) inc;

    SEXP ans = R_new_altrep(R_compact_intseq_class, info, R_NilValue);
#ifndef COMPACT_INTSEQ_MUTABLE
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */
#endif

    return ans;
}


/**
 ** Compact Real Sequences
 **/

/*
 * Methods
 */

#define COMPACT_REALSEQ_INFO_LENGTH(info) ((R_xlen_t) REAL0(info)[0])
#define COMPACT_REALSEQ_INFO_FIRST(info) REAL0(info)[1]
#define COMPACT_REALSEQ_INFO_INCR(info) REAL0(info)[2]

static SEXP compact_realseq_Serialized_state(SEXP x)
{
    return COMPACT_SEQ_INFO(x);
}

static SEXP compact_realseq_Unserialize(SEXP class, SEXP state)
{
    double inc = COMPACT_REALSEQ_INFO_INCR(state);
    R_xlen_t len = COMPACT_REALSEQ_INFO_LENGTH(state);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(state);

    if (inc == 1)
	return new_compact_realseq(len, n1,  1);
    else if (inc == -1)
	return new_compact_realseq(len, n1, -1);
    else
	error("compact sequences with increment %f not supported yet", inc);
}

static SEXP compact_realseq_Duplicate(SEXP x, Rboolean deep)
{
    R_xlen_t n = XLENGTH(x);
    SEXP val = allocVector(REALSXP, n);
    REAL_GET_REGION(x, 0, n, REAL0(val));
    return val;
}

static
Rboolean compact_realseq_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %f not supported yet", inc);

    R_xlen_t n = XLENGTH(x);
    R_xlen_t n1 = (R_xlen_t) REAL_ELT(x, 0);
    R_xlen_t n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %ld : %ld (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_realseq_Length(SEXP x)
{
    return (R_xlen_t) REAL0(COMPACT_SEQ_INFO(x))[0];
}

static void *compact_realseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	R_xlen_t n = COMPACT_REALSEQ_INFO_LENGTH(info);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	
	SEXP val = allocVector(REALSXP, (R_xlen_t) n);
	double *data = REAL(val);

	if (inc == 1) {
	    /* compact sequences n1 : n2 with n1 <= n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 + i;
	}
	else if (inc == -1) {
	    /* compact sequences n1 : n2 with n1 > n2 */
	    for (R_xlen_t i = 0; i < n; i++)
		data[i] = n1 - i;
	}
	else
	    error("compact sequences with increment %f not supported yet", inc);

	SET_COMPACT_SEQ_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(COMPACT_SEQ_EXPANDED(x));
}

static const void *compact_realseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static double compact_realseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = COMPACT_SEQ_EXPANDED(x);
    if (ex != R_NilValue)
	return REAL0(ex)[i];
    else {
	SEXP info = COMPACT_SEQ_INFO(x);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	return n1 + inc * i;
    }
}

static R_xlen_t
compact_realseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = COMPACT_SEQ_INFO(sx);
    R_xlen_t size = COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);

    R_xlen_t ncopy = size - i > n ? n : size - i;
    if (inc == 1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 + k + i;
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 - k - i;
	return ncopy;
    }
    else
	error("compact sequences with increment %f not supported yet", inc);
}
    
static int compact_realseq_Is_sorted(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    double inc = COMPACT_REALSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? SORTED_DECR : SORTED_INCR;
}

static int compact_realseq_No_NA(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue)
	return FALSE;
#endif
    return TRUE;
}

static SEXP compact_realseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (COMPACT_SEQ_EXPANDED(x) != R_NilValue) 
	return NULL;
#endif
    SEXP info = COMPACT_SEQ_INFO(x);
    double size = (double) COMPACT_REALSEQ_INFO_LENGTH(info);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);
    return ScalarReal((size / 2.0) *(n1 + n1 + inc * (size - 1)));
}


/*
 * Class Objects and Method Tables
 */


R_altrep_class_t R_compact_realseq_class;

static void InitCompactRealClass()
{
    R_altrep_class_t cls = R_make_altreal_class("compact_realseq", "base",
						NULL);
    R_compact_realseq_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, compact_realseq_Unserialize);
    R_set_altrep_Serialized_state_method(cls, compact_realseq_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_realseq_Duplicate);
    R_set_altrep_Inspect_method(cls, compact_realseq_Inspect);
    R_set_altrep_Length_method(cls, compact_realseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_realseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_realseq_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, compact_realseq_Elt);
    R_set_altreal_Get_region_method(cls, compact_realseq_Get_region);
    R_set_altreal_Is_sorted_method(cls, compact_realseq_Is_sorted);
    R_set_altreal_No_NA_method(cls, compact_realseq_No_NA);
    R_set_altreal_Sum_method(cls, compact_realseq_Sum);
}


/*
 * Constructor
 */

static SEXP new_compact_realseq(R_xlen_t n, double n1, double inc)
{
    if (n == 1) return ScalarReal(n1);

    if (inc != 1 && inc != -1)
	error("compact sequences with increment %f not supported yet", inc);

    SEXP info = allocVector(REALSXP, 3);
    REAL(info)[0] = n;
    REAL(info)[1] = n1;
    REAL(info)[2] = inc;

    SEXP ans = R_new_altrep(R_compact_realseq_class, info, R_NilValue);
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */

    return ans;
}


/**
 ** Compact Integer/Real Sequences
 **/

SEXP attribute_hidden R_compact_intrange(R_xlen_t n1, R_xlen_t n2)
{
    R_xlen_t n = n1 <= n2 ? n2 - n1 + 1 : n1 - n2 + 1;

    if (n >= R_XLEN_T_MAX)
	error("result would be too long a vector");

    if (n1 <= INT_MIN || n1 > INT_MAX || n2 <= INT_MIN || n2 > INT_MAX)
	return new_compact_realseq(n, n1, n1 <= n2 ? 1 : -1);
    else
	return new_compact_intseq(n, (int) n1, n1 <= n2 ? 1 : -1);
}


/**
 ** Deferred String Coercions
 **/

/*
 * Methods
 */

#define DEFERRED_STRING_STATE(x) R_altrep_data1(x)
#define	CLEAR_DEFERRED_STRING_STATE(x) R_set_altrep_data1(x, R_NilValue)
#define DEFERRED_STRING_EXPANDED(x) R_altrep_data2(x)
#define SET_DEFERRED_STRING_EXPANDED(x, v) R_set_altrep_data2(x, v)

#define MAKE_DEFERRED_STRING_STATE(v, sp) CONS(v, sp)
#define DEFERRED_STRING_STATE_ARG(s) CAR(s)
#define DEFERRED_STRING_STATE_INFO(s) CDR(s)

#define DEFERRED_STRING_ARG(x) \
    DEFERRED_STRING_STATE_ARG(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_INFO(x) \
    DEFERRED_STRING_STATE_INFO(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_SCIPEN(x) \
    INTEGER0(DEFERRED_STRING_STATE_INFO(DEFERRED_STRING_STATE(x)))[0]

static SEXP R_OutDecSym = NULL;

static R_INLINE const char *DEFERRED_STRING_OUTDEC(SEXP x)
{
    /* The default value of OutDec at startup is ".". If it is
       something different at the time the deferred string conversion
       is created then the current value is stored as an attribute. */
    if (R_OutDecSym == NULL)
	R_OutDecSym = install("OutDec");
    SEXP info = DEFERRED_STRING_INFO(x);
    if (ATTRIB(info) != R_NilValue) {
	SEXP outdecattr = getAttrib(info, R_OutDecSym);
	if (TYPEOF(outdecattr) == STRSXP && XLENGTH(outdecattr) == 1)
	    return CHAR(STRING_ELT(outdecattr, 0));
    }
    return ".";
}

static SEXP deferred_string_Serialized_state(SEXP x)
{
    /* This drops through to standard serialization for fully expanded
       deferred string conversions. Partial expansions are OK since
       they still correspond to the original data. An assignment to
       the object will access the DATAPTR and force a full expansion
       and dropping the original data. */
    SEXP state = DEFERRED_STRING_STATE(x);
    return state != R_NilValue ? state : NULL;
}

static SEXP deferred_string_Unserialize(SEXP class, SEXP state)
{
    SEXP arg = DEFERRED_STRING_STATE_ARG(state);
    SEXP info = DEFERRED_STRING_STATE_INFO(state);
    return R_deferred_coerceToString(arg, info);
}

static
Rboolean deferred_string_Inspect(SEXP x, int pre, int deep, int pvec,
				 void (*inspect_subtree)(SEXP, int, int, int))
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != R_NilValue) {
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	Rprintf("  <deferred string conversion>\n");
	inspect_subtree(arg, pre, deep, pvec);
    }
    else {
	Rprintf("  <expanded string conversion>\n");
	inspect_subtree(DEFERRED_STRING_EXPANDED(x), pre, deep, pvec);
    }
    return TRUE;
}

static R_INLINE R_xlen_t deferred_string_Length(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state == R_NilValue ?
	XLENGTH(DEFERRED_STRING_EXPANDED(x)) :
	XLENGTH(DEFERRED_STRING_STATE_ARG(state));
}

static R_INLINE SEXP ExpandDeferredStringElt(SEXP x, R_xlen_t i)
{
    /* make sure the STRSXP for the expanded string is allocated */
    /* not yet expanded strings are NULL in the STRSXP */
    SEXP val = DEFERRED_STRING_EXPANDED(x);
    if (val == R_NilValue) {
	R_xlen_t n = XLENGTH(x);
	val = allocVector(STRSXP, n);
	memset(STDVEC_DATAPTR(val), 0, n * sizeof(SEXP));
	SET_DEFERRED_STRING_EXPANDED(x, val);
    }

    SEXP elt = STRING_ELT(val, i);
    if (elt == NULL) {
	int warn; /* not used by the coercion functions */
	int savedigits, savescipen;
	SEXP data = DEFERRED_STRING_ARG(x);
	switch(TYPEOF(data)) {
	case INTSXP:
	    elt = StringFromInteger(INTEGER_ELT(data, i), &warn);
	    break;
	case REALSXP:
	    savedigits = R_print.digits;
	    savescipen = R_print.scipen;
	    R_print.digits = DBL_DIG;/* MAX precision */
	    R_print.scipen = DEFERRED_STRING_SCIPEN(x);
	    const char *myoutdec = DEFERRED_STRING_OUTDEC(x);
	    if (strcmp(OutDec, myoutdec)) {
		/* The current and saved OutDec values differ. The
		   value to use is put in a static buffer and OutDec
		   temporarily points to this buffer while
		   StringFromReal is called and then reset. The buffer
		   originally pointed to by OutDec cannot be used as
		   it wil not be writable if the default "." has not
		   been changed. */
		static char buf[10];
		strncpy(buf, myoutdec, sizeof buf);
		char *savedOutDec = OutDec;
		OutDec = buf;
		elt = StringFromReal(REAL_ELT(data, i), &warn);
		OutDec = savedOutDec;
	    }
	    else
		elt = StringFromReal(REAL_ELT(data, i), &warn);
	    R_print.digits = savedigits;
	    R_print.scipen = savescipen;
	    break;
	default:
	    error("unsupported type for deferred string coercion");
	}
	SET_STRING_ELT(val, i, elt);
    }
    return elt;
}

static R_INLINE void expand_deferred_string(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state != R_NilValue) {
	/* expanded data may be incomplete until original data is removed */
	PROTECT(x);
	R_xlen_t n = XLENGTH(x), i;
	if (n == 0)
	    SET_DEFERRED_STRING_EXPANDED(x, allocVector(STRSXP, 0));
	else
	    for (i = 0; i < n; i++)
		ExpandDeferredStringElt(x, i);
	CLEAR_DEFERRED_STRING_STATE(x); /* allow arg to be reclaimed */
	UNPROTECT(1);
    }
}

static void *deferred_string_Dataptr(SEXP x, Rboolean writeable)
{
    expand_deferred_string(x);
    return DATAPTR(DEFERRED_STRING_EXPANDED(x));
}

static const void *deferred_string_Dataptr_or_null(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    return state != R_NilValue ? NULL : DATAPTR(DEFERRED_STRING_EXPANDED(x));
}

static SEXP deferred_string_Elt(SEXP x, R_xlen_t i)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == R_NilValue)
	/* string is fully expanded */
	return STRING_ELT(DEFERRED_STRING_EXPANDED(x), i);
    else {
	/* expand only the requested element */
	PROTECT(x);
	SEXP elt = ExpandDeferredStringElt(x, i);
	UNPROTECT(1);
	return elt;
    }
}

static void deferred_string_Set_elt(SEXP x, R_xlen_t i, SEXP v)
{
    expand_deferred_string(x);
    SET_STRING_ELT(DEFERRED_STRING_EXPANDED(x), i, v);
}

static int deferred_string_Is_sorted(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == R_NilValue)
	/* string is fully expanded and may have been modified. */
	return UNKNOWN_SORTEDNESS;
    else {
	/* defer to the argument */
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	switch(TYPEOF(arg)) {
	case INTSXP: return INTEGER_IS_SORTED(arg);
	case REALSXP: return REAL_IS_SORTED(arg);
	default: return UNKNOWN_SORTEDNESS;
	}
    }
}

static int deferred_string_No_NA(SEXP x)
{
    SEXP state = DEFERRED_STRING_STATE(x);
    if (state == R_NilValue)
	/* string is fully expanded and may have been modified. */
	return FALSE;
    else {
	/* defer to the argument */
	SEXP arg = DEFERRED_STRING_STATE_ARG(state);
	switch(TYPEOF(arg)) {
	case INTSXP: return INTEGER_NO_NA(arg);
	case REALSXP: return REAL_NO_NA(arg);
	default: return FALSE;
	}
    }
}

static SEXP deferred_string_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
    SEXP result = NULL;

    if (! OBJECT(x) && ATTRIB(x) == R_NilValue &&
	DEFERRED_STRING_STATE(x) != R_NilValue) {
	/* For deferred string coercions, create a new conversion
	   using the subset of the argument.  Could try to
	   preserve/share coercions already done, if there are any. */
	SEXP data = DEFERRED_STRING_ARG(x);
	SEXP info = DEFERRED_STRING_INFO(x);
	PROTECT(result = ExtractSubset(data, indx, call));
	result = R_deferred_coerceToString(result, info);
	UNPROTECT(1);
	return result;
    }

    return result;
}


/*
 * Class Object and Method Table
 */

static R_altrep_class_t R_deferred_string_class;

static void InitDefferredStringClass()
{
    R_altrep_class_t cls = R_make_altstring_class("deferred_string", "base",
						  NULL);
    R_deferred_string_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, deferred_string_Unserialize);
    R_set_altrep_Serialized_state_method(cls, deferred_string_Serialized_state);
    R_set_altrep_Inspect_method(cls, deferred_string_Inspect);
    R_set_altrep_Length_method(cls, deferred_string_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, deferred_string_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, deferred_string_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, deferred_string_Extract_subset);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, deferred_string_Elt);
    R_set_altstring_Set_elt_method(cls, deferred_string_Set_elt);
    R_set_altstring_Is_sorted_method(cls, deferred_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, deferred_string_No_NA);
}


/*
 * Constructor
 */

SEXP attribute_hidden R_deferred_coerceToString(SEXP v, SEXP info)
{
    SEXP ans = R_NilValue;
    switch (TYPEOF(v)) {
    case INTSXP:
    case REALSXP:
	PROTECT(v); /* may not be needed, but to be safe ... */
	if (info == NULL) {
	    PrintDefaults(); /* to set R_print from options */
	    info = ScalarInteger(R_print.scipen);
	    if (strcmp(OutDec, ".")) {
		/* non-default OutDec setting -- attach as an attribute */
		PROTECT(info);
		if (R_OutDecSym == NULL)
		    R_OutDecSym = install("OutDec");
		setAttrib(info, R_OutDecSym, GetOption1(R_OutDecSym));
		UNPROTECT(1); /* info */
	    }
	}
	MARK_NOT_MUTABLE(v); /* make sure it can't change once captured */
	ans = PROTECT(MAKE_DEFERRED_STRING_STATE(v, info));
	ans = R_new_altrep(R_deferred_string_class, ans, R_NilValue);
	UNPROTECT(2); /* ans, v */
	break;
    default:
	error("unsupported type for deferred string coercion");
    }
    return ans;
}


/**
 ** Memory Mapped Vectors
 **/

/* For now, this code is designed to work both in base R and in a
   package. Some simplifications would be possible if it was only to
   be used in base. in particular, the issue of finalizing objects
   before unloading the library would not need to be addressed, and
   ordinary finalizers in the external pointers could be used instead
   of maintaining a weak reference list of the live mmap objects. */

/*
 * MMAP Object State
 */

/* State is held in a LISTSXP of length 3, and includes
   
       file
       size and length in a REALSXP
       type, ptrOK, wrtOK, serOK in an INTSXP

   These are used by the methods, and also represent the serialized
   state object.
 */

static SEXP make_mmap_state(SEXP file, size_t size, int type,
			    Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP sizes = PROTECT(allocVector(REALSXP, 2));
    double *dsizes = REAL(sizes);
    dsizes[0] = size;
    switch(type) {
    case INTSXP: dsizes[1] = size / sizeof(int); break;
    case REALSXP: dsizes[1] = size / sizeof(double); break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP info = PROTECT(allocVector(INTSXP, 4));
    INTEGER(info)[0] = type;
    INTEGER(info)[1] = ptrOK;
    INTEGER(info)[2] = wrtOK;
    INTEGER(info)[3] = serOK;

    SEXP state = list3(file, sizes, info);

    UNPROTECT(2);
    return state;
}
			    
#define MMAP_STATE_FILE(x) CAR(x)
#define MMAP_STATE_SIZE(x) ((size_t) REAL_ELT(CADR(x), 0))
#define MMAP_STATE_LENGTH(x) ((size_t) REAL_ELT(CADR(x), 1))
#define MMAP_STATE_TYPE(x) INTEGER(CADDR(x))[0]
#define MMAP_STATE_PTROK(x) INTEGER(CADDR(x))[1]
#define MMAP_STATE_WRTOK(x) INTEGER(CADDR(x))[2]
#define MMAP_STATE_SEROK(x) INTEGER(CADDR(x))[3]


/*
 * MMAP Classes and Objects
 */

static R_altrep_class_t mmap_integer_class;
static R_altrep_class_t mmap_real_class;

/* MMAP objects are ALTREP objects with data fields

       data1: an external pointer to the mmaped address
       data2: the MMAP object's state

   The state is also stored in the Protected field of the external
   pointer for use by the finalizer.
*/

static void register_mmap_eptr(SEXP eptr);
static SEXP make_mmap(void *p, SEXP file, size_t size, int type,
		      Rboolean ptrOK, Rboolean wrtOK, Rboolean serOK)
{
    SEXP state = PROTECT(make_mmap_state(file, size,
					 type, ptrOK, wrtOK, serOK));
    SEXP eptr = PROTECT(R_MakeExternalPtr(p, R_NilValue, state));
    register_mmap_eptr(eptr);

    R_altrep_class_t class;
    switch(type) {
    case INTSXP:
	class = mmap_integer_class;
	break;
    case REALSXP:
	class = mmap_real_class;
	break;
    default: error("mmap for %s not supported yet", type2char(type));
    }

    SEXP ans = R_new_altrep(class, eptr, state);
    if (ptrOK && ! wrtOK)
	MARK_NOT_MUTABLE(ans);

    UNPROTECT(2); /* state, eptr */
    return ans;
}

#define MMAP_EPTR(x) R_altrep_data1(x)
#define MMAP_STATE(x) R_altrep_data2(x)
#define MMAP_LENGTH(x) MMAP_STATE_LENGTH(MMAP_STATE(x))
#define MMAP_PTROK(x) MMAP_STATE_PTROK(MMAP_STATE(x))
#define MMAP_WRTOK(x) MMAP_STATE_WRTOK(MMAP_STATE(x))
#define MMAP_SEROK(x) MMAP_STATE_SEROK(MMAP_STATE(x))

#define MMAP_EPTR_STATE(x) R_ExternalPtrProtected(x)

static R_INLINE void *MMAP_ADDR(SEXP x)
{
    SEXP eptr = MMAP_EPTR(x);
    void *addr = R_ExternalPtrAddr(eptr);

    if (addr == NULL)
	error("object has been unmapped");
    return addr;
}

/* We need to maintain a list of weak references to the external
   pointers of memory-mapped objects so a request to unload the shared
   library can finalize them before unloading; otherwise, attempting
   to run a finalizer after unloading would result in an illegal
   instruction. */

static SEXP mmap_list = NULL;

#define MAXCOUNT 10

static void mmap_finalize(SEXP eptr);
static void register_mmap_eptr(SEXP eptr)
{
    if (mmap_list == NULL) {
	mmap_list = CONS(R_NilValue, R_NilValue);
	R_PreserveObject(mmap_list);
    }
    
    /* clean out the weak list every MAXCOUNT calls*/
    static int cleancount = MAXCOUNT;
    if (--cleancount <= 0) {
	cleancount = MAXCOUNT;
	for (SEXP last = mmap_list, next = CDR(mmap_list);
	     next != R_NilValue;
	     next = CDR(next))
	    if (R_WeakRefKey(CAR(next)) == R_NilValue)
		SETCDR(last, CDR(next));
	    else
		last = next;
    }

    /* add a weak reference with a finalizer to the list */
    SETCDR(mmap_list, 
	   CONS(R_MakeWeakRefC(eptr, R_NilValue, mmap_finalize, TRUE),
		CDR(mmap_list)));

    /* store the weak reference in the external pointer for do_munmap_file */
    R_SetExternalPtrTag(eptr, CAR(CDR(mmap_list)));
}

#ifdef SIMPLEMMAP
static void finalize_mmap_objects()
{
    if (mmap_list == NULL)
	return;
    
    /* finalize any remaining mmap objects before unloading */
    for (SEXP next = CDR(mmap_list); next != R_NilValue; next = CDR(next))
	R_RunWeakRefFinalizer(CAR(next));
    R_ReleaseObject(mmap_list);
}
#endif


/*
 * ALTREP Methods
 */

static SEXP mmap_Serialized_state(SEXP x)
{
    /* If serOK is FALSE then serialize as a regular typed vector. If
       serOK is true, then serialize information to allow the mmap to
       be reconstructed. The original file name is serialized; it will
       be expanded again when unserializing, in a context where the
       result may be different. */
    if (MMAP_SEROK(x))
	return MMAP_STATE(x);
    else
	return NULL;
}

static SEXP mmap_file(SEXP, int, Rboolean, Rboolean, Rboolean, Rboolean);

static SEXP mmap_Unserialize(SEXP class, SEXP state)
{
    SEXP file = MMAP_STATE_FILE(state);
    int type = MMAP_STATE_TYPE(state);
    Rboolean ptrOK = MMAP_STATE_PTROK(state);
    Rboolean wrtOK = MMAP_STATE_WRTOK(state);
    Rboolean serOK = MMAP_STATE_SEROK(state);

    SEXP val = mmap_file(file, type, ptrOK, wrtOK, serOK, TRUE);
    if (val == NULL) {
	/**** The attempt to memory map failed. Eventually it would be
	      good to have a mechanism to allow the user to try to
	      resolve this.  For now, return a length zero vector with
	      another warning. */
	warning("memory mapping failed; returning vector of length zero");
	return allocVector(type, 0);
    }
    return val;
}

Rboolean mmap_Inspect(SEXP x, int pre, int deep, int pvec,
		      void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean ptrOK = MMAP_PTROK(x);
    Rboolean wrtOK = MMAP_WRTOK(x);
    Rboolean serOK = MMAP_SEROK(x);
    Rprintf(" mmaped %s", type2char(TYPEOF(x)));
    Rprintf(" [ptr=%d,wrt=%d,ser=%d]\n", ptrOK, wrtOK, serOK);
    return TRUE;
}


/*
 * ALTVEC Methods
 */

static R_xlen_t mmap_Length(SEXP x)
{
    return MMAP_LENGTH(x);
}

static void *mmap_Dataptr(SEXP x, Rboolean writeable)
{
    /* get addr first to get error if the object has been unmapped */
    void *addr = MMAP_ADDR(x);

    if (MMAP_PTROK(x))
	return addr;
    else
	error("cannot access data pointer for this mmaped vector");
}

static const void *mmap_Dataptr_or_null(SEXP x)
{
    return MMAP_PTROK(x) ? MMAP_ADDR(x) : NULL;
}


/*
 * ALTINTEGER Methods
 */

static int mmap_integer_Elt(SEXP x, R_xlen_t i)
{
    int *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_integer_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    int *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(int));
    return ncopy;
}


/*
 * ALTREAL Methods
 */

static double mmap_real_Elt(SEXP x, R_xlen_t i)
{
    double *p = MMAP_ADDR(x);
    return p[i];
}

static
R_xlen_t mmap_real_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    double *x = MMAP_ADDR(sx);
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = x[k + i];
    //memcpy(buf, x + i, ncopy * sizeof(double));
    return ncopy;
}


/*
 * Class Objects and Method Tables
 */

#ifdef SIMPLEMMAP
# define MMAPPKG "simplemmap"
#else
# define MMAPPKG "base"
#endif

static void InitMmapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("mmap_integer", MMAPPKG, dll);
    mmap_integer_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, mmap_integer_Elt);
    R_set_altinteger_Get_region_method(cls, mmap_integer_Get_region);
}

static void InitMmapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("mmap_real", MMAPPKG, dll);
    mmap_real_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, mmap_Unserialize);
    R_set_altrep_Serialized_state_method(cls, mmap_Serialized_state);
    R_set_altrep_Inspect_method(cls, mmap_Inspect);
    R_set_altrep_Length_method(cls, mmap_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, mmap_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, mmap_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, mmap_real_Elt);
    R_set_altreal_Get_region_method(cls, mmap_real_Get_region);
}


/*
 * Constructor
 */

#ifdef Win32
static void mmap_finalize(SEXP eptr)
{
    error("mmop objects not supported on Windows yet");
}

static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    error("mmop objects not supported on Windows yet");
}
#else
/* derived from the example in
  https://www.safaribooksonline.com/library/view/linux-system-programming/0596009585/ch04s03.html */

#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>

//#define DEBUG_PRINT(x) REprintf(x);
#define DEBUG_PRINT(x) do { } while (0)

static void mmap_finalize(SEXP eptr)
{
    DEBUG_PRINT("finalizing ... ");
    void *p = R_ExternalPtrAddr(eptr);
    size_t size = MMAP_STATE_SIZE(MMAP_EPTR_STATE(eptr));
    R_SetExternalPtrAddr(eptr, NULL);

    if (p != NULL) {
	munmap(p, size); /* don't check for errors */
	R_SetExternalPtrAddr(eptr, NULL);
    }
    DEBUG_PRINT("done\n");
}

#define MMAP_FILE_WARNING_OR_ERROR(str, ...) do {	\
	if (warn) {					\
	    warning(str, __VA_ARGS__);			\
	    return NULL;				\
	}						\
	else error(str, __VA_ARGS__);			\
    } while (0)
	    
static SEXP mmap_file(SEXP file, int type, Rboolean ptrOK, Rboolean wrtOK,
		      Rboolean serOK, Rboolean warn)
{
    const char *efn = R_ExpandFileName(translateChar(STRING_ELT(file, 0)));
    struct stat sb;

    /* Target not link */
    if (stat(efn, &sb) != 0)
	MMAP_FILE_WARNING_OR_ERROR("stat: %s", strerror(errno));

    if (! S_ISREG(sb.st_mode))
	MMAP_FILE_WARNING_OR_ERROR("%s is not a regular file", efn);

    int oflags = wrtOK ? O_RDWR : O_RDONLY;
    int fd = open(efn, oflags);
    if (fd == -1)
	MMAP_FILE_WARNING_OR_ERROR("open: %s", strerror(errno));

    int pflags = wrtOK ? PROT_READ | PROT_WRITE : PROT_READ;
    void *p = mmap(0, sb.st_size, pflags, MAP_SHARED, fd, 0);
    close(fd); /* don't care if this fails */
    if (p == MAP_FAILED)
	MMAP_FILE_WARNING_OR_ERROR("mmap: %s", strerror(errno));

    return make_mmap(p, file, sb.st_size, type, ptrOK, wrtOK, serOK);
}
#endif

static Rboolean asLogicalNA(SEXP x, Rboolean dflt)
{
    Rboolean val = asLogical(x);
    return val == NA_LOGICAL ? dflt : val;
}

#ifdef SIMPLEMMAP
SEXP do_mmap_file(SEXP args)
{
    args = CDR(args);
#else
SEXP attribute_hidden do_mmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP file = CAR(args);
    SEXP stype = CADR(args);
    SEXP sptrOK = CADDR(args);
    SEXP swrtOK = CADDDR(args);
    SEXP sserOK = CADDDR(CDR(args));

    int type = REALSXP;
    if (stype != R_NilValue) {
	const char *typestr = CHAR(asChar(stype));
	if (strcmp(typestr, "double") == 0)
	    type = REALSXP;
	else if (strcmp(typestr, "integer") == 0 ||
		 strcmp(typestr, "int") == 0)
	    type = INTSXP;
	else
	    error("type '%s' is not supported", typestr);
    }    

    Rboolean ptrOK = sptrOK == R_NilValue ? TRUE : asLogicalNA(sptrOK, FALSE);
    Rboolean wrtOK = swrtOK == R_NilValue ? FALSE : asLogicalNA(swrtOK, FALSE);
    Rboolean serOK = sserOK == R_NilValue ? FALSE : asLogicalNA(sserOK, FALSE);

    if (TYPEOF(file) != STRSXP || LENGTH(file) != 1 || file == NA_STRING)
	error("invalud 'file' argument");

    return mmap_file(file, type, ptrOK, wrtOK, serOK, FALSE);
}

#ifdef SIMPLEMMAP
static SEXP do_munmap_file(SEXP args)
{
    args = CDR(args);
#else
SEXP attribute_hidden do_munmap_file(SEXP call, SEXP op, SEXP args, SEXP env)
{
#endif
    SEXP x = CAR(args);

    /**** would be useful to have R_mmap_class virtual class as parent here */
    if (! (R_altrep_inherits(x, mmap_integer_class) ||
	   R_altrep_inherits(x, mmap_real_class)))
	error("not a memory-mapped object");

    /* using the finalizer is a cheat to avoid yet another #ifdef Windows */
    SEXP eptr = MMAP_EPTR(x);
    errno = 0;
    R_RunWeakRefFinalizer(R_ExternalPtrTag(eptr));
    if (errno)
	error("munmap: %s", strerror(errno));
    return R_NilValue;
}


/**
 ** Attribute and Meta Data Wrappers
 **/

/*
 * Wrapper Classes and Objects
 */

#define NMETA 2

static R_altrep_class_t wrap_integer_class;
static R_altrep_class_t wrap_real_class;
static R_altrep_class_t wrap_string_class;

/* Wrapper objects are ALTREP objects designed to hold the attributes
   of a potentially large object and/or meta data for the object. */

#define WRAPPER_WRAPPED(x) R_altrep_data1(x)
#define WRAPPER_SET_WRAPPED(x, v) R_set_altrep_data1(x, v)
#define WRAPPER_METADATA(x) R_altrep_data2(x)
#define WRAPPER_SET_METADATA(x, v) R_set_altrep_data2(x, v)

#define WRAPPER_SORTED(x) INTEGER(WRAPPER_METADATA(x))[0]
#define WRAPPER_NO_NA(x) INTEGER(WRAPPER_METADATA(x))[1]


/*
 * ALTREP Methods
 */

static SEXP wrapper_Serialized_state(SEXP x)
{
    return CONS(WRAPPER_WRAPPED(x), WRAPPER_METADATA(x));
}

static SEXP make_wrapper(SEXP, SEXP);

static SEXP wrapper_Unserialize(SEXP class, SEXP state)
{
    return make_wrapper(CAR(state), CDR(state));
}

static SEXP wrapper_Duplicate(SEXP x, Rboolean deep)
{
    SEXP data = WRAPPER_WRAPPED(x);

    /* For a deep copy, duplicate the data. */
    /* For a shallow copy, mark as immutable in the NAMED world; with
       reference counting the reference count will be incremented when
       the data is installed in the new wrapper object. */
    if (deep)
	data = duplicate(data);
#ifndef SWITCH_TO_REFCNT
    else
	/* not needed with reference counting */
	MARK_NOT_MUTABLE(data);
#endif
    PROTECT(data);

    /* always duplicate the meta data */
    SEXP meta = PROTECT(duplicate(WRAPPER_METADATA(x)));

    SEXP ans = make_wrapper(data, meta);

    UNPROTECT(2); /* data, meta */
    return ans;
}

Rboolean wrapper_Inspect(SEXP x, int pre, int deep, int pvec,
			 void (*inspect_subtree)(SEXP, int, int, int))
{
    Rboolean srt = WRAPPER_SORTED(x);
    Rboolean no_na = WRAPPER_NO_NA(x);
    Rprintf(" wrapper [srt=%d,no_na=%d]\n", srt, no_na);
    inspect_subtree(WRAPPER_WRAPPED(x), pre, deep, pvec);
    return TRUE;
}

static R_xlen_t wrapper_Length(SEXP x)
{
    return XLENGTH(WRAPPER_WRAPPED(x));
}


/*
 * ALTVEC Methods
 */

static void clear_meta_data(SEXP x)
{
    SEXP meta = WRAPPER_METADATA(x);
    INTEGER(meta)[0] = UNKNOWN_SORTEDNESS;
    for (int i = 1; i < NMETA; i++)
	INTEGER(meta)[i] = 0;
}

static void *wrapper_Dataptr(SEXP x, Rboolean writeable)
{
    SEXP data = WRAPPER_WRAPPED(x);

    /* If the data might be shared and a writeable pointer is
       requested, then the data needs to be duplicated now. */
    if (writeable && MAYBE_SHARED(data)) {
	PROTECT(x);
	WRAPPER_SET_WRAPPED(x, shallow_duplicate(data));
	UNPROTECT(1);
    }

    if (writeable) {
	/* If a writeable pointer is requested then the meta-data needs
	   to be cleared as it may no longer be valid after a write. */
	clear_meta_data(x);
	return DATAPTR(WRAPPER_WRAPPED(x));
    }
    else
	/**** avoid the cast by having separate methods */
	return (void *) DATAPTR_RO(WRAPPER_WRAPPED(x));
}

static const void *wrapper_Dataptr_or_null(SEXP x)
{
    return DATAPTR_OR_NULL(WRAPPER_WRAPPED(x));
}


/*
 * ALTINTEGER Methods
 */

static int wrapper_integer_Elt(SEXP x, R_xlen_t i)
{
    return INTEGER_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_integer_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, int *buf)
{
    return INTEGER_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_integer_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_integer_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return INTEGER_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTREAL Methods
 */

static double wrapper_real_Elt(SEXP x, R_xlen_t i)
{
    return REAL_ELT(WRAPPER_WRAPPED(x), i);
}

static
R_xlen_t wrapper_real_Get_region(SEXP x, R_xlen_t i, R_xlen_t n, double *buf)
{
    return REAL_GET_REGION(WRAPPER_WRAPPED(x), i, n, buf);
}

static int wrapper_real_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_real_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return REAL_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * ALTSTRING Methods
 */

static SEXP wrapper_string_Elt(SEXP x, R_xlen_t i)
{
    return STRING_ELT(WRAPPER_WRAPPED(x), i);
}

static int wrapper_string_Is_sorted(SEXP x)
{
    if (WRAPPER_SORTED(x) != UNKNOWN_SORTEDNESS)
	return WRAPPER_SORTED(x);
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_IS_SORTED(WRAPPER_WRAPPED(x));
}

static int wrapper_string_no_NA(SEXP x)
{
    if (WRAPPER_NO_NA(x))
	return TRUE;
    else
	/* If the  meta data bit is not set, defer to the wrapped object. */
	return STRING_NO_NA(WRAPPER_WRAPPED(x));
}


/*
 * Class Objects and Method Tables
 */

#define WRAPPKG "base"

static void InitWrapIntegerClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altinteger_class("wrap_integer", WRAPPKG, dll);
    wrap_integer_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, wrapper_integer_Elt);
    R_set_altinteger_Get_region_method(cls, wrapper_integer_Get_region);
    R_set_altinteger_Is_sorted_method(cls, wrapper_integer_Is_sorted);
    R_set_altinteger_No_NA_method(cls, wrapper_integer_no_NA);
}

static void InitWrapRealClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altreal_class("wrap_real", WRAPPKG, dll);
    wrap_real_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, wrapper_real_Elt);
    R_set_altreal_Get_region_method(cls, wrapper_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, wrapper_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, wrapper_real_no_NA);
}

static void InitWrapStringClass(DllInfo *dll)
{
    R_altrep_class_t cls =
	R_make_altstring_class("wrap_string", WRAPPKG, dll);
    wrap_string_class = cls;
 
    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, wrapper_Unserialize);
    R_set_altrep_Serialized_state_method(cls, wrapper_Serialized_state);
    R_set_altrep_Duplicate_method(cls, wrapper_Duplicate);
    R_set_altrep_Inspect_method(cls, wrapper_Inspect);
    R_set_altrep_Length_method(cls, wrapper_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, wrapper_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, wrapper_Dataptr_or_null);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, wrapper_string_Elt);
    R_set_altstring_Is_sorted_method(cls, wrapper_string_Is_sorted);
    R_set_altstring_No_NA_method(cls, wrapper_string_no_NA);
}


/*
 * Constructor
 */

static SEXP make_wrapper(SEXP x, SEXP meta)
{
    /* If x is itself a wrapper it might be a good idea to fuse */
    R_altrep_class_t cls;
    switch(TYPEOF(x)) {
    case INTSXP: cls = wrap_integer_class; break;
    case REALSXP: cls = wrap_real_class; break;
    case STRSXP: cls = wrap_string_class; break;
    default: error("unsupported type");
    }

    SEXP ans = R_new_altrep(cls, x, meta);

#define WRAPATTRIB
#ifdef WRAPATTRIB
    if (ATTRIB(x) != R_NilValue) {
	/* could just move attributes if there are no references to x */
	PROTECT(ans);
	SET_ATTRIB(ans, shallow_duplicate(ATTRIB(x)));
	SET_OBJECT(ans, OBJECT(x));
	IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	UNPROTECT(1); /* ans */
    }
#endif

#ifndef SWITCH_TO_REFCNT
    if (MAYBE_REFERENCED(x))
	/* make sure no mutation can happen through another reference */
	MARK_NOT_MUTABLE(x);
#endif
    
    return ans;
}

static R_INLINE int is_wrapper(SEXP x)
{
    if (ALTREP(x))
	switch(TYPEOF(x)) {
	case INTSXP: return R_altrep_inherits(x, wrap_integer_class);
	case REALSXP: R_altrep_inherits(x, wrap_real_class);
	default: return FALSE;
	}
    else return FALSE;
}

static SEXP wrap_meta(SEXP x, int srt, int no_na)
{
    switch(TYPEOF(x)) {
    case INTSXP:
    case REALSXP:
    case STRSXP: break;
    default: return x;
    }

#ifndef WRAPATTRIB
    if (ATTRIB(x) != R_NilValue)
	/* For objects without references we could move the attributes
	   to the wrapper. For objects with references the attributes
	   would have to be shallow duplicated at least. The object/S4
	   bits would need to be moved as well.	*/
	/* For now, just return the original object. */
	return x;
#endif

    if (!KNOWN_SORTED(srt) && srt != KNOWN_UNSORTED &&
	srt != UNKNOWN_SORTEDNESS)
	error("srt must be -2, -1, 0, or +1, +2, or NA");
    
    if (no_na < 0 || no_na > 1)
	error("no_na must be 0 or +1");

    SEXP meta = allocVector(INTSXP, NMETA);
    INTEGER(meta)[0] = srt;
    INTEGER(meta)[1] = no_na;

    return make_wrapper(x, meta);
}

SEXP attribute_hidden do_wrap_meta(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x = CAR(args);
    int srt = asInteger(CADR(args));
    int no_na = asInteger(CADDR(args));
    return wrap_meta(x, srt, no_na);
}


/**
 ** Initialize ALTREP Classes
 **/

void attribute_hidden R_init_altrep()
{
    InitCompactIntegerClass();
    InitCompactRealClass();
    InitDefferredStringClass();
    InitMmapIntegerClass(NULL);
    InitMmapRealClass(NULL);
    InitWrapIntegerClass(NULL);
    InitWrapRealClass(NULL);
    InitWrapStringClass(NULL);
}
