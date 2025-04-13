/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2025  The R Core Team.
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

/* Internal header, not installed, usied in some standard packages */

#ifndef DEFN_H_
#define DEFN_H_

/* seems unused */
#define COUNTING

#define BYTECODE

#if ( SIZEOF_SIZE_T < SIZEOF_DOUBLE )
# define BOXED_BINDING_CELLS 1
#else
# define BOXED_BINDING_CELLS 0
# define IMMEDIATE_PROMISE_VALUES
#endif

/* probably no longer needed */
#define NEW_CONDITION_HANDLING

/* To test the write barrier used by the generational collector,
   define TESTING_WRITE_BARRIER.  This makes the internal structure of
   SEXPRECs visible only inside of files that explicitly define
   USE_RINTERNALS, and all uses of SEXPREC fields that do not go
   through the appropriate functions or macros will become compilation
   errors.  Since this does impose a small but noticable performance
   penalty, code that includes Defn.h (or code that explicitly defines
   USE_RINTERNALS) can access a SEXPREC's fields directly. */

#ifndef TESTING_WRITE_BARRIER
# define USE_RINTERNALS
#endif

// should really include R_ext/Visibility.h
#ifdef HAVE_VISIBILITY_ATTRIBUTE
# define attribute_visible __attribute__ ((visibility ("default")))
# define attribute_hidden __attribute__ ((visibility ("hidden")))
#else
# define attribute_visible
# define attribute_hidden
#endif

#ifdef __MAIN__
# define extern0 attribute_hidden
#else
# define extern0 extern
#endif

#define attribute_no_sanitizer_instrumentation
#ifdef __has_attribute
# if __has_attribute(disable_sanitizer_instrumentation)
#  undef attribute_no_sanitizer_instrumentation
#  define attribute_no_sanitizer_instrumentation \
     __attribute__((disable_sanitizer_instrumentation))
# elif __has_attribute(no_sanitize)
#  undef attribute_no_sanitizer_instrumentation
#  define attribute_no_sanitizer_instrumentation \
     __attribute__ ((no_sanitize ("address", "thread", "leak", "undefined")))
# endif
#endif

#define MAXELTSIZE 8192 /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */

#include <R_ext/Complex.h>
#include <R_ext/Print.h>

void Rf_CoercionWarning(int);/* warning code */
int Rf_LogicalFromInteger(int, int*);
int Rf_LogicalFromReal(double, int*);
int Rf_LogicalFromComplex(Rcomplex, int*);
int Rf_IntegerFromLogical(int, int*);
int Rf_IntegerFromReal(double, int*);
int Rf_IntegerFromComplex(Rcomplex, int*);
double Rf_RealFromLogical(int, int*);
double Rf_RealFromInteger(int, int*);
double Rf_RealFromComplex(Rcomplex, int*);
Rcomplex Rf_ComplexFromLogical(int, int*);
Rcomplex Rf_ComplexFromInteger(int, int*);
Rcomplex Rf_ComplexFromReal(double, int*);

#define CALLED_FROM_DEFN_H 1
#include <Rinternals.h>		/*-> Arith.h, Boolean.h, Complex.h, Error.h,
				  Memory.h, PrtUtil.h, Utils.h */
#undef CALLED_FROM_DEFN_H

Rboolean Rf_asRbool(SEXP x,SEXP call);
bool Rf_asBool2(SEXP x,SEXP call);


/* UUID identifying the internals version -- packages using compiled
   code should be re-installed when this changes */
#define R_INTERNALS_UUID "2fdf6c18-697a-4ba7-b8ef-11c0d92f1327"

// ======================= USE_RINTERNALS section
#ifdef USE_RINTERNALS
/* This is intended for use only within R itself.
 * It defines internal structures that are otherwise only accessible
 * via SEXP, and macros to replace many (but not all) of accessor functions
 * (which are always defined).
 */

#define NAMED_BITS 16

/* Flags */


struct sxpinfo_struct {
    SEXPTYPE type      :  TYPE_BITS;
                            /* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int scalar:  1;
    unsigned int obj   :  1;
    unsigned int alt   :  1;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;  /* functions and memory tracing */
    unsigned int spare :  1;  /* used on closures and when REFCNT is defined */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
    unsigned int named : NAMED_BITS;
    unsigned int extra : 32 - NAMED_BITS; /* used for immediate bindings */
}; /*		    Tot: 64 */

struct vecsxp_struct {
    R_xlen_t	length;
    R_xlen_t	truelength;
};

struct primsxp_struct {
    int offset;
};

struct symsxp_struct {
    struct SEXPREC *pname;
    struct SEXPREC *value;
    struct SEXPREC *internal;
};

struct listsxp_struct {
    struct SEXPREC *carval;
    struct SEXPREC *cdrval;
    struct SEXPREC *tagval;
};

struct envsxp_struct {
    struct SEXPREC *frame;
    struct SEXPREC *enclos;
    struct SEXPREC *hashtab;
};

struct closxp_struct {
    struct SEXPREC *formals;
    struct SEXPREC *body;
    struct SEXPREC *env;
};

struct promsxp_struct {
    struct SEXPREC *value;
    struct SEXPREC *expr;
    struct SEXPREC *env;
};

/* Every node must start with a set of sxpinfo flags and an attribute
   field. Under the generational collector these are followed by the
   fields used to maintain the collector's linked list structures. */

#ifdef SWITCH_TO_REFCNT
# define REFCNTMAX ((1 << NAMED_BITS) - 1)
#endif

#define SEXPREC_HEADER \
    struct sxpinfo_struct sxpinfo; \
    struct SEXPREC *attrib; \
    struct SEXPREC *gengc_next_node, *gengc_prev_node

/* The standard node structure consists of a header followed by the
   node data. */
typedef struct SEXPREC {
    SEXPREC_HEADER;
    union {
	struct primsxp_struct primsxp;
	struct symsxp_struct symsxp;
	struct listsxp_struct listsxp;
	struct envsxp_struct envsxp;
	struct closxp_struct closxp;
	struct promsxp_struct promsxp;
    } u;
} SEXPREC;

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition. The standard SEXPREC takes up 7 words
   and the reduced version takes 6 words on most 64-bit systems. On most
   32-bit systems, SEXPREC takes 8 words and the reduced version 7 words. */
typedef struct VECTOR_SEXPREC {
    SEXPREC_HEADER;
    struct vecsxp_struct vecsxp;
} VECTOR_SEXPREC, *VECSEXP;

typedef union { VECTOR_SEXPREC s; double align; } SEXPREC_ALIGN;

/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define NAMED(x)	((x)->sxpinfo.named)
#define RTRACE(x)	((x)->sxpinfo.trace)
#define LEVELS(x)	((x)->sxpinfo.gp)
#define SET_OBJECT(x,v)	(((x)->sxpinfo.obj)=(v))
#define SET_TYPEOF(x,v)	(((x)->sxpinfo.type)=(v))
#define SET_NAMED(x,v)	(((x)->sxpinfo.named)=(v))
#define SET_RTRACE(x,v)	(((x)->sxpinfo.trace)=(v))
#define SETLEVELS(x,v)	(((x)->sxpinfo.gp)=((unsigned short)v))
#define ALTREP(x)       ((x)->sxpinfo.alt)
#define SETALTREP(x, v) (((x)->sxpinfo.alt) = (v))
#define SETSCALAR(x, v) (((x)->sxpinfo.scalar) = (v))
#define ANY_ATTRIB(x) (ATTRIB(x) != R_NilValue)

#if defined(COMPUTE_REFCNT_VALUES)
# define REFCNT(x) ((x)->sxpinfo.named)
# define TRACKREFS(x) (TYPEOF(x) == CLOSXP ? TRUE : ! (x)->sxpinfo.spare)
#else
# define REFCNT(x) 0
# define TRACKREFS(x) FALSE
#endif

#if defined(COMPUTE_REFCNT_VALUES)
# define SET_REFCNT(x,v) (REFCNT(x) = (v))
# if defined(EXTRA_REFCNT_FIELDS)
#  define SET_TRACKREFS(x,v) (TRACKREFS(x) = (v))
# else
#  define SET_TRACKREFS(x,v) ((x)->sxpinfo.spare = ! (v))
# endif
# define DECREMENT_REFCNT(x) do {					\
	SEXP drc__x__ = (x);						\
	if (REFCNT(drc__x__) > 0 && REFCNT(drc__x__) < REFCNTMAX)	\
	    SET_REFCNT(drc__x__, REFCNT(drc__x__) - 1);			\
    } while (0)
# define INCREMENT_REFCNT(x) do {			      \
	SEXP irc__x__ = (x);				      \
	if (REFCNT(irc__x__) < REFCNTMAX)		      \
	    SET_REFCNT(irc__x__, REFCNT(irc__x__) + 1);	      \
    } while (0)
#else
# define SET_REFCNT(x,v) do {} while(0)
# define SET_TRACKREFS(x,v) do {} while(0)
# define DECREMENT_REFCNT(x) do {} while(0)
# define INCREMENT_REFCNT(x) do {} while(0)
#endif

#define ENABLE_REFCNT(x) SET_TRACKREFS(x, TRUE)
#define DISABLE_REFCNT(x) SET_TRACKREFS(x, FALSE)

#ifdef SWITCH_TO_REFCNT
# define MARK_NOT_MUTABLE(x) SET_REFCNT(x, REFCNTMAX)
#else
# define MARK_NOT_MUTABLE(x) SET_NAMED(x, NAMEDMAX)
#endif

/* To make complex assignments a bit safer, in particular with
   reference counting, a bit is set on the LHS binding cell or symbol
   at the beginning of the complex assignment process and unset at the
   end.

   - When the assignment bit is set and a new value is assigned to the
     binding then the reference count on the old value is not
     decremented. This prevents moving a single binding from the LHS
     variable of the assignment to another variable during the
     assignment process.

  - If a complex assignment tries to update a binding that already has
    its bit set then, the value of the binding is shallow-duplicated
    before proceeding. This ensures that the structure involved in the
    original complex assignment will not be mutated by further R level
    assignments during the original assignment process.

  For now, no attempt is made to unset the bit if the end of an
  assignment is not reached because of a jump. This may result in some
  unnecessary duplications. This could be prevented by maintaining a
  stack of pending assignments to resent the bits on jump, but that
  seems like overkill.

  It might also be useful to use this bit to communicate to functions
  when they are used in a getter/setter context.

  The bit used is bit 11 in the 'gp' field. An alternative would be to
  take a bit from the 'extra' field.

  LT
*/

#define ASSIGNMENT_PENDING_MASK (1 << 11)
#define ASSIGNMENT_PENDING(x) ((x)->sxpinfo.gp & ASSIGNMENT_PENDING_MASK)
#define SET_ASSIGNMENT_PENDING(x, v) do {			\
	if (v) (((x)->sxpinfo.gp) |= ASSIGNMENT_PENDING_MASK);	\
	else (((x)->sxpinfo.gp) &= ~ASSIGNMENT_PENDING_MASK);	\
    } while (0)

/* The same bit can be used to mark calls used in complex assignments
   to allow replacement functions to determine when they are being
   called in an assignment context and can modify an object with one
   reference */
#define MARK_ASSIGNMENT_CALL(call) SET_ASSIGNMENT_PENDING(call, TRUE)
#define IS_ASSIGNMENT_CALL(call) ASSIGNMENT_PENDING(call)

#ifdef SWITCH_TO_REFCNT
# undef NAMED
# undef SET_NAMED
# define NAMED(x) REFCNT(x)
/* no definition for SET_NAMED; any calls will use the one in memory.c */
# define ENSURE_NAMEDMAX(v) do { } while (0)
# define ENSURE_NAMED(v) do { } while (0)
#else
# define ENSURE_NAMEDMAX(v) do {		\
	SEXP __enm_v__ = (v);			\
	if (NAMED(__enm_v__) < NAMEDMAX)	\
	    SET_NAMED( __enm_v__, NAMEDMAX);	\
    } while (0)
# define ENSURE_NAMED(v) do { if (NAMED(v) == 0) SET_NAMED(v, 1); } while (0)
#endif

#ifdef SWITCH_TO_REFCNT
# define SETTER_CLEAR_NAMED(x) do { } while (0)
# define RAISE_NAMED(x, n) do { } while (0)
#else
# define SETTER_CLEAR_NAMED(x) do {			\
	SEXP __x__ = (x);				\
	if (NAMED(__x__) == 1) SET_NAMED(__x__, 0);	\
    } while (0)
# define RAISE_NAMED(x, n) do {			\
	SEXP __x__ = (x);			\
	int __n__ = (n);			\
	if (NAMED(__x__) < __n__)		\
	    SET_NAMED(__x__, __n__);		\
    } while (0)
#endif

/* S4 object bit, set by R_do_new_object for all new() calls */
#define S4_OBJECT_MASK ((unsigned short)(1<<4))
#define IS_S4_OBJECT(x) ((x)->sxpinfo.gp & S4_OBJECT_MASK)
#define SET_S4_OBJECT(x) (((x)->sxpinfo.gp) |= S4_OBJECT_MASK)
#define UNSET_S4_OBJECT(x) (((x)->sxpinfo.gp) &= ~S4_OBJECT_MASK)

/* JIT optimization support */
#define NOJIT_MASK ((unsigned short)(1<<5))
#define NOJIT(x) ((x)->sxpinfo.gp & NOJIT_MASK)
#define SET_NOJIT(x) (((x)->sxpinfo.gp) |= NOJIT_MASK)
#define MAYBEJIT_MASK ((unsigned short)(1<<6))
#define MAYBEJIT(x) ((x)->sxpinfo.gp & MAYBEJIT_MASK)
#define SET_MAYBEJIT(x) (((x)->sxpinfo.gp) |= MAYBEJIT_MASK)
#define UNSET_MAYBEJIT(x) (((x)->sxpinfo.gp) &= ~MAYBEJIT_MASK)

/* Growable vector support */
#define GROWABLE_MASK ((unsigned short)(1<<5))
#define GROWABLE_BIT_SET(x) ((x)->sxpinfo.gp & GROWABLE_MASK)
#define SET_GROWABLE_BIT(x) (((x)->sxpinfo.gp) |= GROWABLE_MASK)
#define IS_GROWABLE(x) (GROWABLE_BIT_SET(x) && XLENGTH(x) < XTRUELENGTH(x))

/* Vector Access Macros */
#ifdef LONG_VECTOR_SUPPORT
# define IS_LONG_VEC(x) (XLENGTH(x) > R_SHORT_LEN_MAX)
#else
# define IS_LONG_VEC(x) 0
#endif
#define STDVEC_LENGTH(x) (((VECSEXP) (x))->vecsxp.length)
#define STDVEC_TRUELENGTH(x) (((VECSEXP) (x))->vecsxp.truelength)
#define SET_STDVEC_TRUELENGTH(x, v) (STDVEC_TRUELENGTH(x)=(v))
#define SET_TRUELENGTH(x,v) do {				\
	SEXP sl__x__ = (x);					\
	R_xlen_t sl__v__ = (v);					\
	if (ALTREP(x)) error("can't set ALTREP truelength");	\
	SET_STDVEC_TRUELENGTH(sl__x__, sl__v__);	\
    } while (0)

#define IS_SCALAR(x, t) (((x)->sxpinfo.type == (t)) && (x)->sxpinfo.scalar)
#define LENGTH(x) LENGTH_EX(x, __FILE__, __LINE__)
#define TRUELENGTH(x) XTRUELENGTH(x)

/* defined as a macro since fastmatch packages tests for it */
#define XLENGTH(x) XLENGTH_EX(x)

/* THIS ABSOLUTELY MUST NOT BE USED IN PACKAGES !!! */
#define SET_STDVEC_LENGTH(x,v) do {		\
	SEXP __x__ = (x);			\
	R_xlen_t __v__ = (v);			\
	STDVEC_LENGTH(__x__) = __v__;		\
	SETSCALAR(__x__, __v__ == 1 ? 1 : 0);	\
    } while (0)

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known offset from the node SEXP. */
#define STDVEC_DATAPTR(x) ((void *) (((SEXPREC_ALIGN *) (x)) + 1))
#undef CHAR
#define CHAR(x)		((const char *) STDVEC_DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))
#define LOGICAL_RO(x)	((const int *) DATAPTR_RO(x))
#define INTEGER_RO(x)	((const int *) DATAPTR_RO(x))
#define RAW_RO(x)	((const Rbyte *) DATAPTR_RO(x))
#define COMPLEX_RO(x)	((const Rcomplex *) DATAPTR_RO(x))
#define REAL_RO(x)	((const double *) DATAPTR_RO(x))
#define STRING_PTR_RO(x)((const SEXP *) DATAPTR_RO(x))
#define VECTOR_PTR_RO(x)((const SEXP *) DATAPTR_RO(x))

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		((e)->u.listsxp.tagval)
#define CAR0(e)		((e)->u.listsxp.carval)
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)
#define EXTPTR_PTR(e)	((e)->u.listsxp.carval)
#define CDR(e)		((e)->u.listsxp.cdrval)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CDDDR(e)	CDR(CDR(CDR(e)))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define CAD5R(e)	CAR(CDR(CDR(CDR(CDR(CDR(e))))))
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)
#define BNDCELL_TAG(e)	((e)->sxpinfo.extra)
#define SET_BNDCELL_TAG(e, v) ((e)->sxpinfo.extra = (v))

#if BOXED_BINDING_CELLS
/* Use allocated scalars to hold immediate binding values. A little
   less efficient but does not change memory layout or use. These
   allocated scalars must not escape their bindings. */
#define BNDCELL_DVAL(v) SCALAR_DVAL(CAR0(v))
#define BNDCELL_IVAL(v) SCALAR_IVAL(CAR0(v))
#define BNDCELL_LVAL(v) SCALAR_LVAL(CAR0(v))

#define SET_BNDCELL_DVAL(cell, dval) SET_SCALAR_DVAL(CAR0(cell), dval)
#define SET_BNDCELL_IVAL(cell, ival) SET_SCALAR_IVAL(CAR0(cell), ival)
#define SET_BNDCELL_LVAL(cell, lval) SET_SCALAR_LVAL(CAR0(cell), lval)

#define INIT_BNDCELL(cell, type) do {		\
	SEXP val = allocVector(type, 1);	\
	SETCAR(cell, val);			\
	INCREMENT_NAMED(val);			\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)
#else
/* Use a union in the CAR field to represent an SEXP or an immediate
   value.  More efficient, but changes the menory layout on 32 bit
   platforms since the size of the union is larger than the size of a
   pointer. The layout should not change on 64 bit platforms. */
typedef union {
    SEXP sxpval;
    double dval;
    int ival;
} R_bndval_t;

#define BNDCELL_DVAL(v) ((R_bndval_t *) &CAR0(v))->dval
#define BNDCELL_IVAL(v) ((R_bndval_t *) &CAR0(v))->ival
#define BNDCELL_LVAL(v) ((R_bndval_t *) &CAR0(v))->ival

#define SET_BNDCELL_DVAL(cell, dval) (BNDCELL_DVAL(cell) = (dval))
#define SET_BNDCELL_IVAL(cell, ival) (BNDCELL_IVAL(cell) = (ival))
#define SET_BNDCELL_LVAL(cell, lval) (BNDCELL_LVAL(cell) = (lval))

#define INIT_BNDCELL(cell, type) do {		\
	if (BNDCELL_TAG(cell) == 0)		\
	    SETCAR(cell, R_NilValue);		\
	SET_BNDCELL_TAG(cell, type);		\
	SET_MISSING(cell, 0);			\
    } while (0)
#endif

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define RDEBUG(x)	((x)->sxpinfo.debug)
#define SET_RDEBUG(x,v)	(((x)->sxpinfo.debug)=(v))
#define RSTEP(x)	((x)->sxpinfo.spare)
#define SET_RSTEP(x,v)	(((x)->sxpinfo.spare)=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))

#endif /* USE_RINTERNALS */

#define INCREMENT_LINKS(x) do {			\
	SEXP il__x__ = (x);			\
	INCREMENT_NAMED(il__x__);		\
	INCREMENT_REFCNT(il__x__);		\
    } while (0)
#define DECREMENT_LINKS(x) do {			\
	SEXP dl__x__ = (x);			\
	DECREMENT_NAMED(dl__x__);		\
	DECREMENT_REFCNT(dl__x__);		\
    } while (0)

/* Complex assignment support */
/* temporary definition that will need to be refined to distinguish
   getter from setter calls */
#define IS_GETTER_CALL(call) (CADR(call) == R_TmpvalSymbol)

#ifdef LONG_VECTOR_SUPPORT
    NORET R_len_t R_BadLongVector(SEXP, const char *, int);
#endif

/* checking for mis-use of multi-threading */
#ifdef TESTING_WRITE_BARRIER
# define THREADCHECK
#endif
#ifdef THREADCHECK
void R_check_thread(const char *s);
# define R_CHECK_THREAD R_check_thread(__func__)
#else
# define R_CHECK_THREAD do {} while (0)
#endif

/* Accessor functions.  Many are declared using () to avoid the macro
   definitions in the internal headers.
   The function STRING_ELT is used as an argument to arrayAssign even
   if the macro version is in use.
*/

/* General Cons Cell Attributes */
int  (TRACKREFS)(SEXP x);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, int v);
void (SET_NAMED)(SEXP x, int v);
void (ENSURE_NAMEDMAX)(SEXP x);
void (ENSURE_NAMED)(SEXP x);
void (SETTER_CLEAR_NAMED)(SEXP x);
void (RAISE_NAMED)(SEXP x, int n);
void (DECREMENT_REFCNT)(SEXP x);
void (INCREMENT_REFCNT)(SEXP x);
void (DISABLE_REFCNT)(SEXP x);
void (ENABLE_REFCNT)(SEXP x);

/* S4 object setting */
void (SET_S4_OBJECT)(SEXP x);
void (UNSET_S4_OBJECT)(SEXP x);

int (ASSIGNMENT_PENDING)(SEXP x);
void (SET_ASSIGNMENT_PENDING)(SEXP x, int v);
int (IS_ASSIGNMENT_CALL)(SEXP x);
void (MARK_ASSIGNMENT_CALL)(SEXP x);

/* JIT optimization support */
int (NOJIT)(SEXP x);
int (MAYBEJIT)(SEXP x);
void (SET_NOJIT)(SEXP x);
void (SET_MAYBEJIT)(SEXP x);
void (UNSET_MAYBEJIT)(SEXP x);

/* Growable vector support */
int (IS_GROWABLE)(SEXP x);
void (SET_GROWABLE_BIT)(SEXP x);

/* Vector Access Functions */
void (SETLENGTH)(SEXP x, R_xlen_t v);
void (SET_TRUELENGTH)(SEXP x, R_xlen_t v);
int  (SETLEVELS)(SEXP x, int v);
#ifdef TESTING_WRITE_BARRIER
R_xlen_t (STDVEC_LENGTH)(SEXP);
R_xlen_t (STDVEC_TRUELENGTH)(SEXP);
void (SETALTREP)(SEXP, int);
#endif

/* Binding Cell Access Functions */
int (BNDCELL_TAG)(SEXP e);
void (SET_BNDCELL_TAG)(SEXP e, int v);
double (BNDCELL_DVAL)(SEXP cell);
int (BNDCELL_IVAL)(SEXP cell);
int (BNDCELL_LVAL)(SEXP cell);
void (SET_BNDCELL_DVAL)(SEXP cell, double v);
void (SET_BNDCELL_IVAL)(SEXP cell, int v);
void (SET_BNDCELL_LVAL)(SEXP cell, int v);
void (INIT_BNDCELL)(SEXP cell, int type);
void SET_BNDCELL(SEXP cell, SEXP val);
int (PROMISE_TAG)(SEXP e);
void (SET_PROMISE_TAG)(SEXP e, int v);

/* List Access Functions */
SEXP (CAR0)(SEXP e);
void (SET_MISSING)(SEXP x, int v);
SEXP CONS_NR(SEXP a, SEXP b);

/* Symbol Access Functions */
void (SET_DDVAL)(SEXP x, int v);
void SET_PRINTNAME(SEXP x, SEXP v);
void SET_SYMVALUE(SEXP x, SEXP v);
void SET_INTERNAL(SEXP x, SEXP v);

/* Environment Access Functions */
void (SET_ENVFLAGS)(SEXP x, int v);
void SET_FRAME(SEXP x, SEXP v);
void SET_ENCLOS(SEXP x, SEXP v);
void SET_HASHTAB(SEXP x, SEXP v);

/* Promise Access Functions */
void (SET_PRSEEN)(SEXP x, int v);
void SET_PRENV(SEXP x, SEXP v);
void SET_PRVALUE(SEXP x, SEXP v);
void SET_PRCODE(SEXP x, SEXP v);
void IF_PROMSXP_SET_PRVALUE(SEXP x, SEXP v);
int  (PROMISE_IS_EVALUATED)(SEXP x);

/* Hashing Functions */
int  (HASHASH)(SEXP x);
int  (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);

/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
//#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)

/* ALTREP internal support */
int (IS_SCALAR)(SEXP x, int type);
SEXP ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep);
SEXP ALTREP_COERCE(SEXP x, int type);
Rboolean ALTREP_INSPECT(SEXP, int, int, int, void (*)(SEXP, int, int, int));
SEXP ALTREP_SERIALIZED_CLASS(SEXP);
SEXP ALTREP_SERIALIZED_STATE(SEXP);
SEXP ALTREP_UNSERIALIZE_EX(SEXP, SEXP, SEXP, int, int);
R_xlen_t ALTREP_LENGTH(SEXP x);
R_xlen_t ALTREP_TRUELENGTH(SEXP x);
void *ALTVEC_DATAPTR(SEXP x);
const void *ALTVEC_DATAPTR_RO(SEXP x);
const void *ALTVEC_DATAPTR_OR_NULL(SEXP x);
SEXP ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call);

/* data access */
int ALTINTEGER_ELT(SEXP x, R_xlen_t i);
void ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v);
int ALTLOGICAL_ELT(SEXP x, R_xlen_t i);
void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v);
double ALTREAL_ELT(SEXP x, R_xlen_t i);
void ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v);
SEXP ALTSTRING_ELT(SEXP, R_xlen_t);
void ALTSTRING_SET_ELT(SEXP, R_xlen_t, SEXP);
Rcomplex ALTCOMPLEX_ELT(SEXP x, R_xlen_t i);
void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v);
Rbyte ALTRAW_ELT(SEXP x, R_xlen_t i);
void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, Rbyte v);
SEXP ALTLIST_ELT(SEXP, R_xlen_t);
void ALTLIST_SET_ELT(SEXP, R_xlen_t, SEXP);

/* invoking ALTREP class methods */
SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MIN(SEXP x, Rboolean narm);
SEXP ALTINTEGER_MAX(SEXP x, Rboolean narm);
SEXP INTEGER_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
SEXP INTEGER_IS_NA(SEXP x);
SEXP ALTREAL_SUM(SEXP x, Rboolean narm);
SEXP ALTREAL_MIN(SEXP x, Rboolean narm);
SEXP ALTREAL_MAX(SEXP x, Rboolean narm);
SEXP REAL_MATCH(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
SEXP REAL_IS_NA(SEXP x);
SEXP ALTLOGICAL_SUM(SEXP x, Rboolean narm);

/* constructors for internal ALTREP classes */
SEXP R_compact_intrange(R_xlen_t n1, R_xlen_t n2);
SEXP R_deferred_coerceToString(SEXP v, SEXP info);
SEXP R_virtrep_vec(SEXP, SEXP);
SEXP R_tryWrap(SEXP);
SEXP R_tryUnwrap(SEXP);

Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);
Rboolean Rf_psmatch(const char *, const char *, Rboolean);
void Rf_printwhere(void);
void Rf_readS3VarsFromFrame(SEXP, SEXP*, SEXP*, SEXP*, SEXP*, SEXP*, SEXP*);

NORET void R_signal_protect_error(void);
NORET void R_signal_unprotect_error(void);
NORET void R_signal_reprotect_error(PROTECT_INDEX i);

const char *R_curErrorBuf(void);
Rboolean R_cycle_detected(SEXP s, SEXP child);

void R_init_altrep(void);
void R_reinit_altrep_classes(DllInfo *);

/* Defining NO_RINLINEDFUNS disables use to simulate platforms where
   this is not available */
#if !defined(__MAIN__) && (defined(COMPILING_R) || ( __GNUC__ && !defined(__INTEL_COMPILER) )) && (defined(COMPILING_R) || !defined(NO_RINLINEDFUNS))
#include "Rinlinedfuns.h"
#else
/* need remapped names here for use with R_NO_REMAP */

/*
   These are the private inlinable functions that are provided in
   Rinlinedfuns.h It is *essential* that these do not appear in any
   other header file, with or without the Rf_ prefix.
*/

SEXP R_FixupRHS(SEXP x, SEXP y);
double SCALAR_DVAL(SEXP x);
int SCALAR_LVAL(SEXP x);
int SCALAR_IVAL(SEXP x);
void SET_SCALAR_DVAL(SEXP x, double v);
void SET_SCALAR_LVAL(SEXP x, int v);
void SET_SCALAR_IVAL(SEXP x, int v);
void SET_SCALAR_CVAL(SEXP x, Rcomplex v);
void SET_SCALAR_BVAL(SEXP x, Rbyte v);
#endif

#ifdef USE_RINTERNALS

/* Test macros with function versions above */
#undef isNull
#define isNull(s)	(TYPEOF(s) == NILSXP)
#undef isSymbol
#define isSymbol(s)	(TYPEOF(s) == SYMSXP)
#undef isLogical
#define isLogical(s)	(TYPEOF(s) == LGLSXP)
#undef isReal
#define isReal(s)	(TYPEOF(s) == REALSXP)
#undef isComplex
#define isComplex(s)	(TYPEOF(s) == CPLXSXP)
#undef isExpression
#define isExpression(s) (TYPEOF(s) == EXPRSXP)
#undef isEnvironment
#define isEnvironment(s) (TYPEOF(s) == ENVSXP)
#undef isString
#define isString(s)	(TYPEOF(s) == STRSXP)
#undef isObject
#define isObject(s)	(OBJECT(s) != 0)

/* macro version of R_CheckStack */
#define R_CheckStack() do {						\
	NORET void R_SignalCStackOverflow(intptr_t);				\
	int dummy;							\
	intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy); \
	if(R_CStackLimit != (uintptr_t)(-1) && usage > ((intptr_t) R_CStackLimit)) \
	    R_SignalCStackOverflow(usage);				\
    } while (FALSE)

#ifdef __has_feature
# if __has_feature(address_sanitizer)
#  undef R_CheckStack
# endif
#endif

#ifdef R_CheckStack
# if defined(__SANITIZE_ADDRESS__) || defined(__SANITIZE_THREAD__)
#  undef R_CheckStack
# endif
#endif

#endif /* USE_RINTERNALS */

const char * Rf_translateCharFP(SEXP);
const char * Rf_translateCharFP2(SEXP);
const char * Rf_trCharUTF8(SEXP);
const char * Rf_trCharUTF82(SEXP);
const wchar_t * Rf_wtransChar2(SEXP);

extern0 SEXP	R_CommentSymbol;    /* "comment" */
extern0 SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern0 SEXP	R_ExactSymbol;	    /* "exact" */
extern0 SEXP	R_RecursiveSymbol;  /* "recursive" */
extern0 SEXP	R_WholeSrcrefSymbol;   /* "wholeSrcref" */
extern0 SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern0 SEXP	R_UseNamesSymbol;   /* "use.names" */
extern0 SEXP	R_ColonSymbol;         /* ":" */
//extern0 SEXP	R_DoubleColonSymbol;   /* "::" */
//extern0 SEXP	R_TripleColonSymbol;   /* ":::" */
extern0 SEXP    R_ConnIdSymbol;  /* "conn_id" */
extern0 SEXP    R_DevicesSymbol;  /* ".Devices" */

extern0 SEXP    R_dot_Methods;  /* ".Methods" */
extern0 SEXP    R_dot_Group;  /* ".Group" */
extern0 SEXP    R_dot_Class;  /* ".Class" */
extern0 SEXP    R_dot_GenericCallEnv;  /* ".GenericCallEnv" */
extern0 SEXP    R_dot_GenericDefEnv;  /* ".GenericDefEnv" */

extern0 SEXP	R_StringHash;       /* Global hash of CHARSXPs */


 /* writable char access for R internal use only */
#define CHAR_RW(x)	((char *) CHAR(x))

/* CHARSXP charset bits */
#define BYTES_MASK (1<<1)
#define LATIN1_MASK (1<<2)
#define UTF8_MASK (1<<3)
/* (1<<4) is taken by S4_OBJECT_MASK */
#define CACHED_MASK (1<<5)
#define ASCII_MASK (1<<6)
#define HASHASH_MASK 1
/**** HASHASH uses the first bit -- see HASHASH_MASK defined below */

#ifdef USE_RINTERNALS
# define IS_BYTES(x) ((x)->sxpinfo.gp & BYTES_MASK)
# define SET_BYTES(x) (((x)->sxpinfo.gp) |= BYTES_MASK)
# define IS_LATIN1(x) ((x)->sxpinfo.gp & LATIN1_MASK)
# define SET_LATIN1(x) (((x)->sxpinfo.gp) |= LATIN1_MASK)
# define IS_ASCII(x) ((x)->sxpinfo.gp & ASCII_MASK)
# define SET_ASCII(x) (((x)->sxpinfo.gp) |= ASCII_MASK)
# define IS_UTF8(x) ((x)->sxpinfo.gp & UTF8_MASK)
# define SET_UTF8(x) (((x)->sxpinfo.gp) |= UTF8_MASK)
# define ENC_KNOWN(x) ((x)->sxpinfo.gp & (LATIN1_MASK | UTF8_MASK))
# define SET_CACHED(x) (((x)->sxpinfo.gp) |= CACHED_MASK)
# define IS_CACHED(x) (((x)->sxpinfo.gp) & CACHED_MASK)
#else /* USE_RINTERNALS */
/* Needed only for write-barrier testing */
int IS_BYTES(SEXP x);
void SET_BYTES(SEXP x);
int IS_LATIN1(SEXP x);
void SET_LATIN1(SEXP x);
int IS_ASCII(SEXP x);
void SET_ASCII(SEXP x);
int IS_UTF8(SEXP x);
void SET_UTF8(SEXP x);
int ENC_KNOWN(SEXP x);
int SET_CACHED(SEXP x);
int IS_CACHED(SEXP x);
#endif /* USE_RINTERNALS */

/* macros and declarations for managing CHARSXP cache */
# define CXHEAD(x) (x)
# define CXTAIL(x) ATTRIB(x)
SEXP (SET_CXTAIL)(SEXP x, SEXP y);

#include "Errormsg.h"

extern void R_ProcessEvents(void);
#ifdef Win32
extern void R_WaitEvent(void);
#endif

#ifdef R_USE_SIGNALS
#ifdef Win32
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif
#endif

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

/* F77_SYMBOL was a minimal version of F77_SUB from RS.h, 
   used in main/util.c and main/registration.c
   F77_QSYMBOL is unused
#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif
*/

/*  Heap and Pointer Protection Stack Sizes.  */

/* These headers are all required by C99.
   However, we use types below such as uintptr_t which are optional in C11.
   And on some older systems they were in inttypes.h but not stdint.h.

   Up to 2.11.1 (r52035, May 2010) we had

#if !defined(HAVE_INTPTR_T) && !defined(intptr_t)
 typedef long intptr_t;
#endif
#if !defined(HAVE_UINTPTR_T) && !defined(uintptr_t)
 typedef unsigned long uintptr_t;
#endif
    but size_t might be better.

 */
#ifdef HAVE_INTTYPES_H
# include <inttypes.h>
#endif
/* According to POSIX inttypes.h should include stdint.h,
   but let's be sure. */
#ifdef HAVE_STDINT_H
# include <stdint.h>
#endif
#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif

#if defined HAVE_DECL_SIZE_MAX && HAVE_DECL_SIZE_MAX
  typedef size_t R_size_t;
# define R_SIZE_T_MAX SIZE_MAX
#else
# error SIZE_MAX is required for C99
#endif


#define Mega 1048576.    /* Misnomer !! 1 MiB (mebibyte) := 2^20 (= 1048576) Bytes */
#define Giga 1073741824. /* Misnomer !! 1 GiB (gibibyte) := 2^30 = (2^10)^3 Bytes */

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in ../main/startup.c */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	50000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#ifndef R_VSIZE
#define	R_VSIZE		67108864L
#endif

/* some commonly needed headers */
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* declare substitutions */
#if !defined(strdup) && defined(HAVE_DECL_STRDUP) && !HAVE_DECL_STRDUP
extern char *strdup(const char *s1);
#endif
#if !defined(strncascmp) && defined(HAVE_DECL_STRNCASECMP) && !HAVE_DECL_STRNCASECMP
extern int strncasecmp(const char *s1, const char *s2, size_t n);
#endif

/* safer alternative */
extern char *Rstrdup(const char *s);


/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif

/* PATH_MAX has historically been understood to be the maximal length in
   bytes of an entire path name that may exist.  In current POSIX, when
   defined, it is the maximum number of bytes that will be stored to a
   user-supplied buffer by file-system operations which do not allow the
   caller to provide actual size of the buffer (and such calls are rare).
   If the system limited path names to a certain value, it shall not be less
   than this value, if defined.

   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris, macOS, *BSD have 1024, Linux glibc has 4192 (but glibc does not
   enforce the limit).  File names are limited to FILENAME_MAX bytes
   (usually the same as PATH_MAX) or NAME_MAX (often 255/256).

   POSIX requires PATH_MAX to be defined in limits.h (included above) if
   independent of the file path (file system).  However, if it can vary by
   filepath, it is required to be undefined there. */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* MinGW-W64 defines PATH_MAX to MAX_PATH, which is 260 in Windows. It used to
   be the maximal length of an entire path name (depending on the API used,
   in bytes or in UCS-2 units, including the terminator).  This is no longer
   the case and like PATH_MAX on Unix, MAX_PATH is only used as the limit in
   several old API calls for the user buffer when the size cannot be passed
   by user.  Some additional old components of Windows and old APIs seem not
   to support longer paths.

   But longer paths can and do exist, they can be created using some API,
   usually UCS-2, sometimes using the extended syntax
   (\\?\D:\very_long_path) but sometimes using the common syntax.
   Applications that haven't opted for "long paths" (and those also have to
   be enabled system-wide) are shielded from seeing the long paths in some
   API calls to prevent buffer-overflow and other errors in user code that
   did not check the lengths.

   Like on Unix, user/library code should not depend on that there is a
   maximum to the path length.  The actual real maximum on Windows is not
   even precisely specified.
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
#endif

/* R_PATH_MAX is an R-defined limit on the length of entire path name,
   including the terminator.  It may be used in some older code that has not
   yet been rewritten to support arbitrary path lengths.  Such code still
   has to detect longer paths, but may not support them, e.g.  may throw an
   error. R_PATH_MAX shall be at least PATH_MAX, when PATH_MAX is defined. */
#ifdef Unix
# define R_PATH_MAX PATH_MAX
#else
  /* On Windows, 260 is too limiting */
# define R_PATH_MAX 5000
#endif

#ifdef R_USE_SIGNALS
#ifdef HAVE_POSIX_SETJMP
# define SIGJMP_BUF sigjmp_buf
# define SIGSETJMP(x,s) sigsetjmp(x,s)
# define SIGLONGJMP(x,i) siglongjmp(x,i)
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,0)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define SIGJMP_BUF jmp_buf
# define SIGSETJMP(x,s) setjmp(x)
# define SIGLONGJMP(x,i) longjmp(x,i)
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif
#endif

#define HSIZE	  49157	/* The size of the hash table for symbols */
#define MAXIDSIZE 10000	/* Largest symbol size,
			   in bytes excluding terminator.
			   Was 256 prior to 2.13.0, now just a sanity check.
			*/

/* The type of the do_xxxx functions. */
/* These are the built-in R functions. */
typedef SEXP (*CCODE)(SEXP, SEXP, SEXP, SEXP);

/* Information for Deparsing Expressions */
typedef enum {
    PP_INVALID  =  0,
    PP_ASSIGN   =  1,
    PP_ASSIGN2  =  2,
    PP_BINARY   =  3,
    PP_BINARY2  =  4,
    PP_BREAK    =  5,
    PP_CURLY    =  6,
    PP_FOR      =  7,
    PP_FUNCALL  =  8,
    PP_FUNCTION =  9,
    PP_IF 	= 10,
    PP_NEXT 	= 11,
    PP_PAREN    = 12,
    PP_RETURN   = 13,
    PP_SUBASS   = 14,
    PP_SUBSET   = 15,
    PP_WHILE 	= 16,
    PP_UNARY 	= 17,
    PP_DOLLAR 	= 18,
    PP_FOREIGN 	= 19,
    PP_REPEAT 	= 20
} PPkind;

typedef enum {
    PREC_FN	 = 0,
    PREC_EQ	 = 1,
    PREC_LEFT    = 2,
    PREC_RIGHT	 = 3,
    PREC_TILDE	 = 4,
    PREC_OR	 = 5,
    PREC_AND	 = 6,
    PREC_NOT	 = 7,
    PREC_COMPARE = 8,
    PREC_SUM	 = 9,
    PREC_PROD	 = 10,
    PREC_PERCENT = 11,
    PREC_COLON	 = 12,
    PREC_SIGN	 = 13,
    PREC_POWER	 = 14,
    PREC_SUBSET  = 15,
    PREC_DOLLAR	 = 16,
    PREC_NS	 = 17
} PPprec;

typedef struct {
	PPkind kind; 	 /* deparse kind */
	PPprec precedence; /* operator precedence */
	unsigned int rightassoc;  /* right associative? */
} PPinfo;

/* The type definitions for the table of built-in functions. */
/* This table can be found in ../main/names.c */
typedef struct {
    char   *name;    /* print name */
    CCODE  cfun;     /* c-code address */
    int	   code;     /* offset within c-code */
    int	   eval;     /* evaluate args? */
    int	   arity;    /* function arity */
    PPinfo gram;     /* pretty-print info */
} FUNTAB;

#ifdef USE_RINTERNALS
/* There is much more in Rinternals.h, including function versions
 * of the Promise and Hashing groups.
 */

/* Primitive Access Macros */
#define PRIMOFFSET(x)	((x)->u.primsxp.offset)
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.offset)=(v))
#define PRIMFUN(x)	(R_FunTab[(x)->u.primsxp.offset].cfun)
#define PRIMNAME(x)	(R_FunTab[(x)->u.primsxp.offset].name)
#define PRIMVAL(x)	(R_FunTab[(x)->u.primsxp.offset].code)
#define PRIMARITY(x)	(R_FunTab[(x)->u.primsxp.offset].arity)
#define PPINFO(x)	(R_FunTab[(x)->u.primsxp.offset].gram)
#define PRIMPRINT(x)	(((R_FunTab[(x)->u.primsxp.offset].eval)/100)%10)
#define PRIMINTERNAL(x)	(((R_FunTab[(x)->u.primsxp.offset].eval)%100)/10)

/* Promise Access Macros */
#define PRCODE(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))
#ifdef IMMEDIATE_PROMISE_VALUES
# define PRVALUE0(x) ((x)->u.promsxp.value)
# define PRVALUE(x) \
    (PROMISE_TAG(x) ? R_expand_promise_value(x) : PRVALUE0(x))
# define PROMISE_IS_EVALUATED(x) \
    (PROMISE_TAG(x) || PRVALUE0(x) != R_UnboundValue)
# define PROMISE_TAG(x)  BNDCELL_TAG(x)
# define SET_PROMISE_TAG(x, v) SET_BNDCELL_TAG(x, v)
#else
# define PRVALUE0(x) ((x)->u.promsxp.value)
# define PRVALUE(x) PRVALUE0(x)
# define PROMISE_IS_EVALUATED(x) (PRVALUE(x) != R_UnboundValue)
# define PROMISE_TAG(x) 0
#endif

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp & HASHASH_MASK)
#define HASHVALUE(x)    ((int) TRUELENGTH(x))
#define SET_HASHASH(x,v) ((v) ? (((x)->sxpinfo.gp) |= HASHASH_MASK) : \
			  (((x)->sxpinfo.gp) &= (~HASHASH_MASK)))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, ((int) (v)))

/* Vector Heap Structure */
typedef struct {
	union {
		SEXP		backpointer;
		double		align;
	} u;
} VECREC, *VECP;

/* Vector Heap Macros */
#define BACKPOINTER(v)	((v).u.backpointer)
#define BYTE2VEC(n)	(((n)>0)?(((n)-1)/sizeof(VECREC)+1):0)
#define INT2VEC(n)	(((n)>0)?(((n)*sizeof(int)-1)/sizeof(VECREC)+1):0)
#define FLOAT2VEC(n)	(((n)>0)?(((n)*sizeof(double)-1)/sizeof(VECREC)+1):0)
#define COMPLEX2VEC(n)	(((n)>0)?(((n)*sizeof(Rcomplex)-1)/sizeof(VECREC)+1):0)
#define PTR2VEC(n)	(((n)>0)?(((n)*sizeof(SEXP)-1)/sizeof(VECREC)+1):0)

/* Bindings */
/* use the same bits (15 and 14) in symbols and bindings */
#define ACTIVE_BINDING_MASK (1<<15)
#define BINDING_LOCK_MASK (1<<14)
#define SPECIAL_BINDING_MASK (ACTIVE_BINDING_MASK | BINDING_LOCK_MASK)
#define IS_ACTIVE_BINDING(b) ((b)->sxpinfo.gp & ACTIVE_BINDING_MASK)
#define BINDING_IS_LOCKED(b) ((b)->sxpinfo.gp & BINDING_LOCK_MASK)
#define SET_ACTIVE_BINDING_BIT(b) ((b)->sxpinfo.gp |= ACTIVE_BINDING_MASK)
#define LOCK_BINDING(b) do {						\
	SEXP lb__b__ = b;						\
	if (! IS_ACTIVE_BINDING(lb__b__)) {				\
	    if (TYPEOF(lb__b__) == SYMSXP)				\
		MARK_NOT_MUTABLE(SYMVALUE(lb__b__));			\
	    else							\
		MARK_NOT_MUTABLE(CAR(lb__b__));				\
	}								\
	((lb__b__))->sxpinfo.gp |= BINDING_LOCK_MASK;			\
    } while (0)
#define UNLOCK_BINDING(b) ((b)->sxpinfo.gp &= (~BINDING_LOCK_MASK))

#define BASE_SYM_CACHED_MASK (1<<13)
#define SET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp |= BASE_SYM_CACHED_MASK)
#define UNSET_BASE_SYM_CACHED(b) ((b)->sxpinfo.gp &= (~BASE_SYM_CACHED_MASK))
#define BASE_SYM_CACHED(b) ((b)->sxpinfo.gp & BASE_SYM_CACHED_MASK)

#define SPECIAL_SYMBOL_MASK (1<<12)
#define SET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define IS_SPECIAL_SYMBOL(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)
#define SET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp |= SPECIAL_SYMBOL_MASK)
#define UNSET_NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp &= (~SPECIAL_SYMBOL_MASK))
#define NO_SPECIAL_SYMBOLS(b) ((b)->sxpinfo.gp & SPECIAL_SYMBOL_MASK)

#else /* USE_RINTERNALS */

typedef struct VECREC *VECP;
int (PRIMOFFSET)(SEXP x);
void (SET_PRIMOFFSET)(SEXP x, int v);

#define PRIMFUN(x)	(R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x)	(R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x)	(R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x)	(R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x)	(R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x)	(((R_FunTab[PRIMOFFSET(x)].eval)/100)%10)
#define PRIMINTERNAL(x) (((R_FunTab[PRIMOFFSET(x)].eval)%100)/10)


Rboolean (IS_ACTIVE_BINDING)(SEXP b);
Rboolean (BINDING_IS_LOCKED)(SEXP b);
void (SET_ACTIVE_BINDING_BIT)(SEXP b);
void (LOCK_BINDING)(SEXP b);
void (UNLOCK_BINDING)(SEXP b);

void (SET_BASE_SYM_CACHED)(SEXP b);
void (UNSET_BASE_SYM_CACHED)(SEXP b);
Rboolean (BASE_SYM_CACHED)(SEXP b);

void (SET_SPECIAL_SYMBOL)(SEXP b);
void (UNSET_SPECIAL_SYMBOL)(SEXP b);
Rboolean (IS_SPECIAL_SYMBOL)(SEXP b);
void (SET_NO_SPECIAL_SYMBOLS)(SEXP b);
void (UNSET_NO_SPECIAL_SYMBOLS)(SEXP b);
Rboolean (NO_SPECIAL_SYMBOLS)(SEXP b);

#endif /* USE_RINTERNALS */

/* The byte code engine uses a typed stack. The typed stack's entries
   consist of a tag and a union. An entry can represent a standard
   SEXP value (tag = 0) or an unboxed scalar value.  For now real,
   integer, and logical values are supported. It would in principle be
   possible to support complex scalars and short scalar strings, but
   it isn't clear if this is worth while.

   In addition to unboxed values the typed stack can hold partially
   evaluated or incomplete allocated values. For now this is only used
   for holding a short representation of an integer sequence as produce
   by the colon operator, seq_len, or seq_along, and as consumed by
   compiled 'for' loops. This could be used more extensively in the
   future, though the ALTREP framework may be a better choice.

   Allocating memory on the stack is also supported; this is currently
   used for jump buffers.
*/
typedef struct {
    int tag;
    int flags;
    union {
	int ival;
	double dval;
	SEXP sxpval;
    } u;
} R_bcstack_t;
# define PARTIALSXP_MASK (~255)
# define IS_PARTIAL_SXP_TAG(x) ((x) & PARTIALSXP_MASK)
# define RAWMEM_TAG 254
# define CACHESZ_TAG 253

/* saved bcEval() state for implementing recursion using goto */
typedef struct R_bcFrame R_bcFrame_type;

#ifdef R_USE_SIGNALS
/* Stack entry for pending promises */
typedef struct RPRSTACK {
    SEXP promise;
    struct RPRSTACK *next;
} RPRSTACK;

/* Evaluation Context Structure */
typedef struct RCNTXT {
    struct RCNTXT *nextcontext;	/* The next context up the chain */
    int callflag;		/* The context "type" */
    JMP_BUF cjmpbuf;		/* C stack and register information */
    int cstacktop;		/* Top of the pointer protection stack */
    int evaldepth;	        /* evaluation depth at inception */
    SEXP promargs;		/* Promises supplied to closure */
    SEXP callfun;		/* The closure called */
    SEXP sysparent;		/* environment the closure was called from */
    SEXP call;			/* The call that effected this context*/
    SEXP cloenv;		/* The environment */
    SEXP conexit;		/* Interpreted "on.exit" code */
    void (*cend)(void *);	/* C "on.exit" thunk */
    void *cenddata;		/* data for C "on.exit" thunk */
    void *vmax;		        /* top of R_alloc stack */
    int intsusp;                /* interrupts are suspended */
    int gcenabled;		/* R_GCEnabled value */
    int bcintactive;            /* R_BCIntActive value */
    SEXP bcbody;                /* R_BCbody value */
    void* bcpc;                 /* R_BCpc value */
    ptrdiff_t relpc;            /* pc offset when begincontext is called */
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK *prstack;   /* stack of pending promises */
    R_bcstack_t *nodestack;
    R_bcstack_t *bcprottop;
    R_bcFrame_type *bcframe;
    SEXP srcref;	        /* The source line in effect */
    int browserfinish;          /* should browser finish this context without
                                   stopping */
    R_bcstack_t returnValue;    /* only set during on.exit calls */
    struct RCNTXT *jumptarget;	/* target for a continuing jump */
    int jumpmask;               /* associated LONGJMP argument */
} RCNTXT, *context;

/* The Various Context Types.

 * In general the type is a bitwise OR of the values below.
 * Note that CTXT_LOOP is already the or of CTXT_NEXT and CTXT_BREAK.
 * Only functions should have the third bit turned on;
 * this allows us to move up the context stack easily
 * with either RETURN's or GENERIC's or RESTART's.
 * If you add a new context type for functions make sure
 *   CTXT_NEWTYPE & CTXT_FUNCTION > 0
 */
enum {
    CTXT_TOPLEVEL = 0,
    CTXT_NEXT	  = 1,
    CTXT_BREAK	  = 2,
    CTXT_LOOP	  = 3,	/* break OR next target */
    CTXT_FUNCTION = 4,
    CTXT_CCODE	  = 8,
    CTXT_RETURN	  = 12,
    CTXT_BROWSER  = 16,
    CTXT_GENERIC  = 20,
    CTXT_RESTART  = 32,
    CTXT_BUILTIN  = 64, /* used in profiling */
    CTXT_UNWIND   = 128
};

extern0 RCNTXT *getLexicalContext(SEXP);
extern0 SEXP getLexicalCall(SEXP);

/*
TOP   0 0 0 0 0 0  = 0
NEX   1 0 0 0 0 0  = 1
BRE   0 1 0 0 0 0  = 2
LOO   1 1 0 0 0 0  = 3
FUN   0 0 1 0 0 0  = 4
CCO   0 0 0 1 0 0  = 8
BRO   0 0 0 0 1 0  = 16
RET   0 0 1 1 0 0  = 12
GEN   0 0 1 0 1 0  = 20
RES   0 0 0 0 0 0 1 = 32
BUI   0 0 0 0 0 0 0 1 = 64
*/

#define IS_RESTART_BIT_SET(flags) ((flags) & CTXT_RESTART)
#define SET_RESTART_BIT_ON(flags) (flags |= CTXT_RESTART)
#define SET_RESTART_BIT_OFF(flags) (flags &= ~CTXT_RESTART)
#endif

/* Miscellaneous Definitions */
#define streql(s, t)	(!strcmp((s), (t)))

/* Arithmetic and Relation Operators */
typedef enum {
    PLUSOP = 1,
    MINUSOP,
    TIMESOP,
    DIVOP,
    POWOP,
    MODOP,
    IDIVOP
} ARITHOP_TYPE;

typedef enum {
    EQOP = 1,
    NEOP,
    LTOP,
    LEOP,
    GEOP,
    GTOP
} RELOP_TYPE;

typedef enum {
    MATPROD_DEFAULT = 1,
    MATPROD_INTERNAL,
    MATPROD_BLAS,
    MATPROD_DEFAULT_SIMD  /* experimental */
} MATPROD_TYPE;

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1


/*--- Global Variables ---------------------------------------------------- */

/* Defined and initialized in names.c (not main.c) :*/
#ifndef __R_Names__
extern
#endif
FUNTAB	R_FunTab[];	    /* Built in functions */


#include <R_ext/libextern.h>

#ifdef __MAIN__
# define INI_as(v) = v
#define extern0 attribute_hidden
#else
# define INI_as(v)
#define extern0 extern
#endif

LibExtern SEXP  R_SrcfileSymbol;    /* "srcfile" */
LibExtern SEXP  R_SrcrefSymbol;     /* "srcref" */


LibExtern Rboolean R_interrupts_suspended INI_as(FALSE);
LibExtern int R_interrupts_pending INI_as(0);

/* R Home Directory */
LibExtern char *R_Home;		    /* Root of the R tree */

/* Memory Management */
extern0 R_size_t R_NSize  INI_as(R_NSIZE);/* Size of cons cell heap */
extern0 R_size_t R_VSize  INI_as(R_VSIZE);/* Size of the vector heap */
extern0 int	R_GCEnabled INI_as(1);
extern0 int	R_in_gc INI_as(0);
extern0 int	R_BCIntActive INI_as(0); /* bcEval called more recently than
                                            eval */
extern0 void*	R_BCpc INI_as(NULL);/* current byte code instruction */
extern0 SEXP	R_BCbody INI_as(NULL); /* current byte code object */
extern0 R_bcFrame_type *R_BCFrame INI_as(NULL); /* bcEval() frame */
extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern0 R_size_t R_Collected;	    /* Number of free cons cells (after gc) */
extern0 int	R_Is_Running;	    /* for Windows memory manager */

/* The Pointer Protection Stack */
LibExtern int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
LibExtern int	R_PPStackTop;	    /* The top of the stack */
LibExtern SEXP*	R_PPStack;	    /* The pointer protection stack */

void R_ReleaseMSet(SEXP mset, int keepSize);

/* Evaluation Environment */
extern0 SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
extern0 SEXP	R_ReturnedValue;    /* Slot for return-ing values */
extern0 SEXP*	R_SymbolTable;	    /* The symbol table */
#ifdef R_USE_SIGNALS
extern0 RCNTXT R_Toplevel;	      /* Storage for the toplevel context */
extern0 RCNTXT* R_ToplevelContext;  /* The toplevel context */
LibExtern RCNTXT* R_GlobalContext;    /* The global context */
extern0 RCNTXT* R_SessionContext;   /* The session toplevel context */
extern0 RCNTXT* R_ExitContext;      /* The active context for on.exit processing */
#endif
extern Rboolean R_Visible;	    /* Value visibility flag */
extern0 int	R_EvalDepth	INI_as(0);	/* Evaluation recursion depth */
extern0 int	R_BrowseLines	INI_as(0);	/* lines/per call in browser :
						 * options(deparse.max.lines) */
extern0 int	R_Expressions	INI_as(5000);	/* options(expressions) */
extern0 int	R_Expressions_keep INI_as(5000);/* options(expressions) */
extern0 Rboolean R_KeepSource	INI_as(FALSE);	/* options(keep.source) */
extern0 Rboolean R_CBoundsCheck	INI_as(FALSE);	/* options(CBoundsCheck) */
extern0 MATPROD_TYPE R_Matprod	INI_as(MATPROD_DEFAULT);  /* options(matprod) */
extern0 int	R_WarnLength	INI_as(1000);	/* Error/warning max length */
extern0 int	R_nwarnings	INI_as(50);

/* C stack checking */
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_OldCStackLimit INI_as((uintptr_t)0); /* Old value while
							   handling overflow */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
/* Default here is for Windows: set from configure in src/unix/system.c */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

#ifdef R_USE_SIGNALS
extern0 struct RPRSTACK *R_PendingPromises INI_as(NULL); /* Pending promise stack */
#endif

/* File Input/Output */
extern0 bool R_Quiet	INI_as(false);	/* Be as quiet as possible */
extern0 bool R_Verbose	INI_as(false);	/* Be verbose */
// Next two are duplicated in Rinterface.h
// R_Interactive is accessed in parallel's fork.c and on Windows in util's stubs.c
LibExtern Rboolean R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/
extern Rboolean  R_NoEcho	INI_as(FALSE);	/* do not echo R code */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
/* R_Consolefile is used in the internet module */
extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */
extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */
extern0 int	R_ErrorCon	INI_as(2);	/* Error connection */
LibExtern char *R_TempDir	INI_as(NULL);	/* Name of per-session dir */
extern0 char   *Sys_TempDir	INI_as(NULL);	/* Name of per-session dir
						   if set by R itself */
extern0 char	R_StdinEnc[31]  INI_as("");	/* Encoding assumed for stdin */

/* Objects Used In Parsing  */
LibExtern int	R_ParseError	INI_as(0); /* Line where parse error occurred */
extern0 int	R_ParseErrorCol;    /* Column of start of token where parse error occurred */
extern0 SEXP	R_ParseErrorFile;   /* Source file where parse error was seen.  Either a
				       STRSXP or (when keeping srcrefs) a SrcFile ENVSXP */
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
LibExtern char	R_ParseErrorMsg[PARSE_ERROR_SIZE] INI_as("");
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */
LibExtern char	R_ParseContext[PARSE_CONTEXT_SIZE] INI_as("");
LibExtern int	R_ParseContextLast INI_as(0); /* last character in context buffer */
LibExtern int	R_ParseContextLine; /* Line in file of the above */

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */

/* History */
LibExtern char *R_HistoryFile;	/* Name of the history file */
LibExtern int	R_HistorySize;	/* Size of the history file */
LibExtern int	R_RestoreHistory;	/* restore the history file? */
extern void 	R_setupHistory(void);

/* Warnings/Errors */
extern0 int	R_CollectWarnings INI_as(0);	/* the number of warnings */
extern0 SEXP	R_Warnings;	    /* the warnings and their calls */
extern0 int	R_ShowErrorMessages INI_as(1);	/* show error messages? */
extern0 SEXP	R_HandlerStack;	/* Condition handler stack */
extern0 SEXP	R_RestartStack;	/* Stack of available restarts */
extern0 Rboolean R_warn_partial_match_args   INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_dollar INI_as(FALSE);
extern0 Rboolean R_warn_partial_match_attr INI_as(FALSE);
extern0 Rboolean R_ShowWarnCalls INI_as(FALSE);
extern0 Rboolean R_ShowErrorCalls INI_as(FALSE);
extern0 int	R_NShowCalls INI_as(50);

LibExtern Rboolean utf8locale  INI_as(FALSE);  /* is this a UTF-8 locale? */
LibExtern Rboolean mbcslocale  INI_as(FALSE);  /* is this a MBCS locale? */
extern0   Rboolean latin1locale INI_as(FALSE); /* is this a Latin-1 locale? */
LibExtern int      R_MB_CUR_MAX INI_as(FALSE); /* corrected variant of MB_CUR_MAX */
#ifdef Win32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */
LibExtern unsigned int systemCP  INI_as(437);  /* the ANSI codepage, GetACP */
extern0   Rboolean WinUTF8out  INI_as(FALSE);  /* Use UTF-8 for output */
extern0   void WinCheckUTF8(void);
#endif

extern char* OutDec	INI_as(".");  /* decimal point used for output */
extern0 Rboolean R_DisableNLinBrowser	INI_as(FALSE);
extern0 char R_BrowserLastCommand	INI_as('n');

/* Initialization of the R environment when it is embedded */
extern int Rf_initEmbeddedR(int argc, char **argv);

/* GUI type */

extern char	*R_GUIType	INI_as("unknown");
extern Rboolean R_isForkedChild		INI_as(FALSE); /* was this forked? */

extern0 double cpuLimit			INI_as(-1.0);
extern0 double cpuLimit2	       	INI_as(-1.0);
extern0 double cpuLimitValue		INI_as(-1.0);
extern0 double elapsedLimit		INI_as(-1.0);
extern0 double elapsedLimit2		INI_as(-1.0);
extern0 double elapsedLimitValue       	INI_as(-1.0);

void resetTimeLimits(void);
void R_CheckTimeLimits(void);

#define R_BCNODESTACKSIZE 300000
LibExtern R_bcstack_t *R_BCNodeStackTop, *R_BCNodeStackEnd;
extern0 R_bcstack_t *R_BCNodeStackBase;
extern0 R_bcstack_t *R_BCProtTop;
extern0 int R_jit_enabled INI_as(0); /* has to be 0 during R startup */
extern0 int R_compile_pkgs INI_as(0);
extern0 int R_check_constants INI_as(0);
extern0 int R_disable_bytecode INI_as(0);
extern SEXP R_cmpfun1(SEXP); /* unconditional fresh compilation */
extern void R_init_jit_enabled(void);
extern void R_initEvalSymbols(void);
#ifdef R_USE_SIGNALS
extern SEXP R_findBCInterpreterSrcref(RCNTXT*);
#endif
extern SEXP R_getCurrentSrcref(void);
extern SEXP R_getBCInterpreterExpression(void);
extern ptrdiff_t R_BCRelPC(SEXP, void *);

void R_BCProtReset(R_bcstack_t *);

LibExtern int R_num_math_threads INI_as(1);
LibExtern int R_max_num_math_threads INI_as(1);

/* Pointer  type and utilities for dispatch in the methods package */
typedef SEXP (*R_stdGen_ptr_t)(SEXP, SEXP, SEXP); /* typedef */
//R_stdGen_ptr_t R_get_standardGeneric_ptr(void); /* get method */
R_stdGen_ptr_t R_set_standardGeneric_ptr(R_stdGen_ptr_t, SEXP); /* set method */
LibExtern SEXP R_MethodsNamespace;
SEXP R_deferred_default_method(void);
SEXP R_set_prim_method(SEXP fname, SEXP op, SEXP code_vec, SEXP fundef,
		       SEXP mlist);
SEXP do_set_prim_method(SEXP op, const char *code_string, SEXP fundef,
			SEXP mlist);
void R_set_quick_method_check(R_stdGen_ptr_t);
SEXP R_primitive_methods(SEXP op);
SEXP R_primitive_generic(SEXP op);

/* smallest decimal exponent, needed in format.c, set in Init_R_Machine */
extern0 int R_dec_min_exponent		INI_as(-308);

/* structure for caching machine accuracy values */
typedef struct {
    int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
    double eps, epsneg, xmin, xmax;
} AccuracyInfo;

LibExtern AccuracyInfo R_AccuracyInfo;

extern unsigned int max_contour_segments INI_as(25000);

/* used in package utils */
extern Rboolean known_to_be_latin1 INI_as(FALSE);
extern0 Rboolean known_to_be_utf8 INI_as(FALSE);

/* pre-allocated boolean values */
LibExtern SEXP R_TrueValue INI_as(NULL);
LibExtern SEXP R_FalseValue INI_as(NULL);
LibExtern SEXP R_LogicalNAValue INI_as(NULL);

/* for PCRE as from R 3.4.0 */
extern0 Rboolean R_PCRE_use_JIT INI_as(TRUE);
#ifdef HAVE_PCRE2
extern0 int R_PCRE_study INI_as(-2);
#else
extern0 int R_PCRE_study INI_as(10);
#endif
extern0 int R_PCRE_limit_recursion;


#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

#define checkArity(a,b) Rf_checkArityCall(a,b,call)

/*--- FUNCTIONS ------------------------------------------------------ */

/* Internal type coercions */
int Rf_asLogical2(SEXP x, int checking, SEXP call);


typedef enum { iSILENT, iWARN, iERROR } warn_type;

/* Other Internally Used Functions, excluding those which are inline-able*/
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP, Rboolean);
void Rf_addMissingVarsToNewEnv(SEXP, SEXP);
SEXP Rf_allocFormalsList2(SEXP sym1, SEXP sym2);
SEXP Rf_allocFormalsList3(SEXP sym1, SEXP sym2, SEXP sym3);
SEXP Rf_allocFormalsList4(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4);
SEXP Rf_allocFormalsList5(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5);
SEXP Rf_allocFormalsList6(SEXP sym1, SEXP sym2, SEXP sym3, SEXP sym4, SEXP sym5, SEXP sym6);
SEXP R_allocObject(void);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP Rf_fixSubset3Args(SEXP, SEXP, SEXP, SEXP*);
int Rf_countContexts(int, int);
SEXP Rf_CreateTag(SEXP);
SEXP Rf_DropDims(SEXP);
Rboolean R_envHasNoSpecialSymbols(SEXP);
SEXP Rf_ExtractSubset(SEXP, SEXP, SEXP);
SEXP Rf_findFun3(SEXP, SEXP, SEXP);
void Rf_findFunctionForBody(SEXP);
int Rf_FixupDigits(SEXP, warn_type);
int Rf_FixupWidth (SEXP, warn_type);
SEXP Rf_installDDVAL(int i);
SEXP Rf_installS3Signature(const char *, const char *);
bool Rf_isFree(SEXP);
Rboolean Rf_isUnmodifiedSpecSym(SEXP sym, SEXP env);
SEXP Rf_matchE(SEXP, SEXP, int, SEXP);
// void Rf_setSVector(SEXP*, int, SEXP);
SEXP Rf_stringSuffix(SEXP, int);
const char * Rf_translateChar0(SEXP);

void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP);
SEXP R_bcDecode(SEXP);
void R_registerBC(SEXP, SEXP);
Rboolean R_checkConstants(Rboolean);
Rboolean R_BCVersionOK(SEXP);
int R_NaN_is_R_NA(double);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);

# define allocCharsxp		Rf_allocCharsxp
# define asBool2	       	Rf_asBool2
# define asRbool		Rf_asRbool
# define asVecSize		Rf_asVecSize
# define asXLength		Rf_asXLength
# define begincontext		Rf_begincontext
# define BindDomain		Rf_BindDomain
# define check_stack_balance	Rf_check_stack_balance
# define check1arg		Rf_check1arg
# define CheckFormals		Rf_CheckFormals
# define CleanEd		Rf_CleanEd
# define CoercionWarning       	Rf_CoercionWarning
# define ComplexFromInteger	Rf_ComplexFromInteger
# define ComplexFromLogical	Rf_ComplexFromLogical
# define ComplexFromReal	Rf_ComplexFromReal
# define ComplexFromString	Rf_ComplexFromString
# define copyMostAttribNoTs	Rf_copyMostAttribNoTs
# define createS3Vars		Rf_createS3Vars
# define currentTime		Rf_currentTime
# define CustomPrintValue	Rf_CustomPrintValue
# define DataFrameClass		Rf_DataFrameClass
# define ddfindVar		Rf_ddfindVar
# define deparse1		Rf_deparse1
# define deparse1m		Rf_deparse1m
# define deparse1w		Rf_deparse1w
# define deparse1line		Rf_deparse1line
# define deparse1s		Rf_deparse1s
# define DispatchGroup		Rf_DispatchGroup
# define DispatchOrEval		Rf_DispatchOrEval
# define DispatchAnyOrEval      Rf_DispatchAnyOrEval
# define dynamicfindVar		Rf_dynamicfindVar
# define EncodeChar             Rf_EncodeChar
# define EncodeRaw              Rf_EncodeRaw
# define EncodeReal2            Rf_EncodeReal2
# define EncodeString           Rf_EncodeString
# define EnsureString 		Rf_EnsureString
# define endcontext		Rf_endcontext
# define errorcall_cpy		Rf_errorcall_cpy
# define ErrorMessage		Rf_ErrorMessage
# define evalList		Rf_evalList
# define evalListKeepMissing	Rf_evalListKeepMissing
# define factorsConform		Rf_factorsConform
# define findcontext		Rf_findcontext
# define findVar1		Rf_findVar1
# define FrameClassFix		Rf_FrameClassFix
# define framedepth		Rf_framedepth
# define frameSubscript		Rf_frameSubscript
# define get1index		Rf_get1index
# define GetOptionCutoff       	Rf_GetOptionCutoff
# define getVar			Rf_getVar
# define getVarInFrame		Rf_getVarInFrame
# define InitArithmetic		Rf_InitArithmetic
# define InitConnections	Rf_InitConnections
# define InitEd			Rf_InitEd
# define InitFunctionHashing	Rf_InitFunctionHashing
# define InitBaseEnv		Rf_InitBaseEnv
# define InitGlobalEnv		Rf_InitGlobalEnv
# define InitGraphics		Rf_InitGraphics
# define InitMemory		Rf_InitMemory
# define InitNames		Rf_InitNames
# define InitOptions		Rf_InitOptions
# define InitStringHash		Rf_InitStringHash
# define InitS3DefaultTypes	Rf_InitS3DefaultTypes
# define InitTempDir		Rf_InitTempDir
# define InitTypeTables		Rf_InitTypeTables
# define initStack		Rf_initStack
# define IntegerFromComplex	Rf_IntegerFromComplex
# define IntegerFromLogical	Rf_IntegerFromLogical
# define IntegerFromReal	Rf_IntegerFromReal
# define IntegerFromString	Rf_IntegerFromString
# define internalTypeCheck	Rf_internalTypeCheck
# define isValidName		Rf_isValidName
//# define installTrChar		Rf_installTrChar
# define ItemName		Rf_ItemName
# define jump_to_toplevel	Rf_jump_to_toplevel
# define KillAllDevices		Rf_KillAllDevices
# define levelsgets		Rf_levelsgets
# define LogicalFromComplex	Rf_LogicalFromComplex
# define LogicalFromInteger	Rf_LogicalFromInteger
# define LogicalFromReal	Rf_LogicalFromReal
# define LogicalFromString	Rf_LogicalFromString
# define mainloop		Rf_mainloop
# define makeSubscript		Rf_makeSubscript
# define markKnown		Rf_markKnown
# define mat2indsub		Rf_mat2indsub
# define matchArg		Rf_matchArg
# define matchArgExact		Rf_matchArgExact
# define matchArgs_NR		Rf_matchArgs_NR
# define matchArgs_RC		Rf_matchArgs_RC
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mbcsToUcs2		Rf_mbcsToUcs2
# define memtrace_report	Rf_memtrace_report
# define mkCharWUTF8		Rf_mkCharWUTF8
# define mkCLOSXP		Rf_mkCLOSXP
# define mkFalse		Rf_mkFalse
# define mkPROMISE		Rf_mkPROMISE
# define mkQUOTE		Rf_mkQUOTE
# define mkSYMSXP		Rf_mkSYMSXP
# define mkTrue			Rf_mkTrue
# define NewEnvironment		Rf_NewEnvironment
# define OneIndex		Rf_OneIndex
# define onintr			Rf_onintr
# define onintrNoResume		Rf_onintrNoResume
# define onsigusr1              Rf_onsigusr1
# define onsigusr2              Rf_onsigusr2
# define parse			Rf_parse
# define patchArgsByActuals	Rf_patchArgsByActuals
# define PrintInit              Rf_PrintInit
# define PrintDefaults		Rf_PrintDefaults
# define PrintGreeting		Rf_PrintGreeting
# define PrintValueEnv		Rf_PrintValueEnv
# define PrintValueRec		Rf_PrintValueRec
# define PrintVersion		Rf_PrintVersion
# define PrintVersion_part_1	Rf_PrintVersion_part_1
# define PrintVersionString    	Rf_PrintVersionString
# define PrintWarnings		Rf_PrintWarnings
# define promiseArgs		Rf_promiseArgs
# define RealFromComplex	Rf_RealFromComplex
# define RealFromInteger	Rf_RealFromInteger
# define RealFromLogical	Rf_RealFromLogical
# define RealFromString		Rf_RealFromString
# define Seql			Rf_Seql
# define sexptype2char		Rf_sexptype2char
# define Scollate		Rf_Scollate
# define sortVector		Rf_sortVector
# define SrcrefPrompt		Rf_SrcrefPrompt
# define ssort			Rf_ssort
# define StringFromComplex	Rf_StringFromComplex
# define StringFromInteger	Rf_StringFromInteger
# define StringFromLogical	Rf_StringFromLogical
# define StringFromReal		Rf_StringFromReal
# define strIsASCII		Rf_strIsASCII
# define StrToInternal		Rf_StrToInternal
# define strmat2intmat		Rf_strmat2intmat
# define substituteList		Rf_substituteList
# define TimeToSeed		Rf_TimeToSeed
# define translateCharFP	Rf_translateCharFP
# define translateCharFP2	Rf_translateCharFP2
# define trCharUTF8      	Rf_trCharUTF8
# define trCharUTF82      	Rf_trCharUTF82
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
# define wtransChar2		Rf_wtransChar2
# define yychar			Rf_yychar
# define yylval			Rf_yylval
# define yynerrs		Rf_yynerrs
# define yyparse		Rf_yyparse

/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

/* The maximum length of input line which will be asked for,
   in bytes, including the terminator */
#define CONSOLE_BUFFER_SIZE 4096
int	R_ReadConsole(const char *, unsigned char *, int, int);
void	R_WriteConsole(const char *, int); /* equivalent to R_WriteConsoleEx(a, b, 0) */
void	R_WriteConsoleEx(const char *, int, int);
void	R_ResetConsole(void);
void	R_FlushConsole(void);
void	R_ClearerrConsole(void);
void	R_Busy(int);
int	R_ShowFiles(int, const char **, const char **, const char *,
		    Rboolean, const char *);
int     R_EditFiles(int, const char **, const char **, const char *);
int	R_ChooseFile(int, char *, int);
char	*R_HomeDir(void);
bool    R_FileExists(const char *);
bool    R_HiddenFile(const char *);
double	R_FileMtime(const char *);
int	R_GetFDLimit(void);
int	R_EnsureFDLimit(int);
Rboolean R_IsDirPath(const char *);

/* environment cell access */
typedef struct { SEXP cell; } R_varloc_t; /* use struct to prevent casting */
#define R_VARLOC_IS_NULL(loc) ((loc).cell == NULL)
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
R_varloc_t R_findVarLoc(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
Rboolean R_GetVarLocMISSING(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);
SEXP R_findVar(SEXP, SEXP);
SEXP R_findVarInFrame(SEXP, SEXP);

/* deparse option bits: change do_dump if more are added */

#define KEEPINTEGER 		1
#define QUOTEEXPRESSIONS 	2
#define SHOWATTRIBUTES 		4
#define USESOURCE 		8
#define WARNINCOMPLETE 		16
#define DELAYPROMISES 		32
#define KEEPNA			64
#define S_COMPAT       		128
#define HEXNUMERIC             	256
#define DIGITS17		512
#define NICE_NAMES             	1024
/* common combinations of the above */
#define SIMPLEDEPARSE		0
#define DEFAULTDEPARSE		1089 /* KEEPINTEGER | KEEPNA | NICE_NAMES, used for calls */
#define FORSOURCING		95 /* not DELAYPROMISES, used in edit.c */

/* Coercion functions */
int Rf_LogicalFromString(SEXP, int*);
int Rf_IntegerFromString(SEXP, int*);
double Rf_RealFromString(SEXP, int*);
Rcomplex Rf_ComplexFromString(SEXP, int*);
SEXP Rf_StringFromLogical(int, int*);
SEXP Rf_StringFromInteger(int, int*);
SEXP Rf_StringFromReal(double, int*);
SEXP Rf_StringFromComplex(Rcomplex, int*);
SEXP Rf_EnsureString(SEXP);

/* ../../main/print.c : */
typedef struct {
    int width;
    int na_width;
    int na_width_noquote;
    int digits;
    int scipen;
    int gap;
    int quote;
    int right;
    int max;
    SEXP na_string;
    SEXP na_string_noquote;
    int useSource;
    int cutoff; // for deparsed language objects
    SEXP env;
    SEXP callArgs;
} R_PrintData;

/* Dirent wrappers/implementation */

struct R_dirent {
    char *d_name; /* null-terminated filename */
};

typedef struct R_DIR_INTERNAL R_DIR;

R_DIR *R_opendir(const char *name);
struct R_dirent *R_readdir(R_DIR *rdir);
int R_closedir(R_DIR *rdir);

#ifdef Win32
struct R_wdirent {
    wchar_t *d_name; /* null-terminated filename */
};

typedef struct R_WDIR_INTERNAL R_WDIR;

R_WDIR *R_wopendir(const wchar_t *name);
struct R_wdirent *R_wreaddir(R_WDIR *rdir);
int R_wclosedir(R_WDIR *rdir);
#endif

/* Other Internally Used Functions */

SEXP Rf_allocCharsxp(R_len_t);
SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
R_xlen_t asVecSize(SEXP x);
R_xlen_t asXLength(SEXP x);
void check1arg(SEXP, SEXP, const char *);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
void CheckFormals(SEXP, const char*);
void R_check_locale(void);
void check_stack_balance(SEXP op, int save);
void CleanEd(void);
void copyMostAttribNoTs(SEXP, SEXP);
SEXP createS3Vars(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
void CustomPrintValue(SEXP, SEXP);
double currentTime(void);
void DataFrameClass(SEXP);
SEXP ddfindVar(SEXP, SEXP);
SEXP deparse1(SEXP,bool,int);
SEXP deparse1m(SEXP call, bool abbrev, int opts);
SEXP deparse1w(SEXP,bool,int);
SEXP deparse1line (SEXP, bool);
SEXP deparse1line_ex(SEXP, bool, int);
SEXP deparse1s(SEXP call);
int DispatchAnyOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int DispatchOrEval(SEXP, SEXP, const char *, SEXP, SEXP, SEXP*, int, int);
int DispatchGroup(const char *, SEXP,SEXP,SEXP,SEXP,SEXP*);
R_xlen_t dispatch_xlength(SEXP, SEXP, SEXP);
R_len_t dispatch_length(SEXP, SEXP, SEXP);
SEXP dispatch_subset2(SEXP, R_xlen_t, SEXP, SEXP);
SEXP duplicated(SEXP, Rboolean);
R_xlen_t any_duplicated(SEXP, Rboolean);
R_xlen_t any_duplicated3(SEXP, SEXP, Rboolean);
SEXP evalList(SEXP, SEXP, SEXP, int);
SEXP evalListKeepMissing(SEXP, SEXP);
int factorsConform(SEXP, SEXP);
NORET void findcontext(int, SEXP, SEXP);
SEXP findVar1(SEXP, SEXP, SEXPTYPE, int);
void FrameClassFix(SEXP);
SEXP frameSubscript(int, SEXP, SEXP);
R_xlen_t get1index(SEXP, SEXP, R_xlen_t, int, int, SEXP);
int GetOptionCutoff(void);
SEXP getVar(SEXP, SEXP);
SEXP getVarInFrame(SEXP, SEXP);
void InitArithmetic(void);
void InitConnections(void);
void InitEd(void);
void InitFunctionHashing(void);
void InitBaseEnv(void);
void InitGlobalEnv(void);
Rboolean R_current_trace_state(void);
Rboolean R_current_debug_state(void);
Rboolean R_has_methods(SEXP);
void R_InitialData(void);
SEXP R_possible_dispatch(SEXP, SEXP, SEXP, SEXP, Rboolean);
Rboolean inherits2(SEXP, const char *);
void InitGraphics(void);
void InitMemory(void);
void InitNames(void);
void InitOptions(void);
void InitStringHash(void);
void Init_R_Variables(SEXP);
void InitTempDir(void);
void R_reInitTempDir(int);
void InitTypeTables(void);
void initStack(void);
void InitS3DefaultTypes(void);
void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
Rboolean isMethodsDispatchOn(void);
int isValidName(const char *);
NORET void jump_to_toplevel(void);
void KillAllDevices(void);
SEXP levelsgets(SEXP, SEXP);
void mainloop(void);
SEXP makeSubscript(SEXP, SEXP, R_xlen_t *, SEXP);
SEXP markKnown(const char *, SEXP);
SEXP mat2indsub(SEXP, SEXP, SEXP, SEXP);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgExact(SEXP, SEXP*);
SEXP matchArgs_NR(SEXP, SEXP, SEXP);
SEXP matchArgs_RC(SEXP, SEXP, SEXP);
SEXP matchPar(const char *, SEXP*);
void memtrace_report(void *, void *);
SEXP mkCharWUTF8(const wchar_t *);
SEXP mkCLOSXP(SEXP, SEXP, SEXP);
SEXP mkFalse(void);
SEXP mkPRIMSXP (int, int);
SEXP mkPROMISE(SEXP, SEXP);
SEXP R_mkEVPROMISE(SEXP, SEXP);
SEXP R_mkEVPROMISE_NR(SEXP, SEXP);
SEXP mkQUOTE(SEXP);
SEXP mkSYMSXP(SEXP, SEXP);
SEXP mkTrue(void);
const char *R_nativeEncoding(void);
SEXP NewEnvironment(SEXP, SEXP, SEXP);
void onintr(void);
void onintrNoResume(void);
void onsigusr1(int);
void onsigusr2(int);
R_xlen_t OneIndex(SEXP, SEXP, R_xlen_t, int, SEXP*, int, SEXP);
SEXP parse(FILE*, int);
SEXP patchArgsByActuals(SEXP, SEXP, SEXP);
void PrintInit(R_PrintData *, SEXP);
void PrintDefaults(void);
void PrintGreeting(void);
void PrintValueEnv(SEXP, SEXP);
void PrintValueRec(SEXP, R_PrintData *);
void PrintVersion(char *, size_t len);
void PrintVersion_part_1(char *, size_t len);
void PrintVersionString(char *, size_t len);
void PrintWarnings(void);
void process_site_Renviron(void);
void process_system_Renviron(void);
void process_user_Renviron(void);
SEXP promiseArgs(SEXP, SEXP);
int Rcons_vprintf(const char *, va_list);
int REvprintf_internal(const char *, va_list);
SEXP R_data_class(SEXP , Rboolean);
SEXP R_data_class2(SEXP);
char *R_LibraryFileName(const char *, char *, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, int);
extern int R_Newhashpjw(const char *);
FILE* R_OpenLibraryFile(const char *);
SEXP R_Primitive(const char *);
void R_RestoreGlobalEnv(void);
void R_RestoreGlobalEnvFromFile(const char *, Rboolean);
void R_SaveGlobalEnv(void);
void R_SaveGlobalEnvToFile(const char *);
void R_SaveToFile(SEXP, FILE*, int);
void R_SaveToFileV(SEXP, FILE*, int, int);
Rboolean R_seemsOldStyleS4Object(SEXP object);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
SEXP R_SetOption(SEXP, SEXP);
void R_Suicide(const char *);
SEXP R_flexiblas_info(void);
void R_getProcTime(double *data);
Rboolean R_isMissing(SEXP symbol, SEXP rho);
Rboolean R_missing(SEXP symbol, SEXP rho);
const char *sexptype2char(SEXPTYPE type);
void sortVector(SEXP, bool);
void SrcrefPrompt(const char *, SEXP);
void ssort(SEXP*,int);
int StrToInternal(const char *);
SEXP strmat2intmat(SEXP, SEXP, SEXP, SEXP);
SEXP substituteList(SEXP, SEXP);
unsigned int TimeToSeed(void);
SEXP tspgets(SEXP, SEXP);
SEXP type2symbol(SEXPTYPE);
void unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
SEXP R_LookupMethod(SEXP, SEXP, SEXP, SEXP);
int usemethod(const char *, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP*);
SEXP vectorIndex(SEXP, SEXP, int, int, int, SEXP, Rboolean);

#ifdef R_USE_SIGNALS
void begincontext(RCNTXT*, int, SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP dynamicfindVar(SEXP, RCNTXT*);
void endcontext(RCNTXT*);
int framedepth(RCNTXT*);
void R_InsertRestartHandlers(RCNTXT *, const char *);
NORET void R_JumpToContext(RCNTXT *, int, SEXP);
SEXP R_syscall(int,RCNTXT*);
int R_sysparent(int,RCNTXT*);
SEXP R_sysframe(int,RCNTXT*);
SEXP R_sysfunction(int,RCNTXT*);
RCNTXT *R_findExecContext(RCNTXT *, SEXP);
RCNTXT *R_findParentContext(RCNTXT *, int);

void R_run_onexits(RCNTXT *);
NORET void R_jumpctxt(RCNTXT *, int, SEXP);
#endif

/* ../main/bind.c */
SEXP ItemName(SEXP, R_xlen_t);

/* ../main/errors.c : */
NORET void errorcall_cpy(SEXP, const char *, ...) R_PRINTF_FORMAT(2,3);
NORET void ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, R_WARNING, ...);
SEXP R_GetTraceback(int);    // including deparse()ing
SEXP R_GetTracebackOnly(int);// no        deparse()ing
NORET void R_signalErrorCondition(SEXP cond, SEXP call);
NORET void R_signalErrorConditionEx(SEXP cond, SEXP call, int exitOnly);
void R_signalWarningCondition(SEXP cond);
SEXP R_vmakeErrorCondition(SEXP call,
			   const char *classname, const char *subclassname,
			   int nextra, const char *format, va_list ap)
     R_PRINTF_FORMAT(5,0);

SEXP R_makeErrorCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
     R_PRINTF_FORMAT(5,0);
SEXP R_makeWarningCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
     R_PRINTF_FORMAT(5,0);

NORET void R_MissingArgError     (SEXP symbol,     SEXP call, const char* subclass);
NORET void R_MissingArgError_c   (const char *arg, SEXP call, const char* subclass);

SEXP R_makePartialMatchWarningCondition(SEXP call, SEXP argument, SEXP formal);

void R_setConditionField(SEXP cond, R_xlen_t idx, const char *name, SEXP val);
SEXP R_makeNotSubsettableError(SEXP x, SEXP call);
SEXP R_makeMissingSubscriptError(SEXP x, SEXP call);
SEXP R_makeMissingSubscriptError1(SEXP call);
SEXP R_makeOutOfBoundsError(SEXP x, int subscript, SEXP sindex,
			    SEXP call, const char *prefix);
SEXP R_makeCStackOverflowError(SEXP call, intptr_t usage);
SEXP R_getProtectStackOverflowError(void);
SEXP R_getExpressionStackOverflowError(void);
SEXP R_getNodeStackOverflowError(void);
void R_InitConditions(void);

R_size_t R_GetMaxVSize(void);
Rboolean R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
Rboolean R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char *p, int *ierr);
void R_SetPPSize(R_size_t);
void R_SetNconn(int);

void R_expand_binding_value(SEXP);
#ifdef IMMEDIATE_PROMISE_VALUES
SEXP R_expand_promise_value(SEXP);
#endif

void R_args_enable_refcnt(SEXP);
void R_try_clear_args_refcnt(SEXP);

/* ../main/devices.c, used in memory.c, gnuwin32/extra.c */
#define R_MaxDevices 64

/* gnuwin32/extra.c */
wchar_t *R_getFullPathNameW(const wchar_t *);
char *R_getFullPathName(const char *);

/* ../../main/printutils.c : */
typedef enum {
    Rprt_adj_left = 0,
    Rprt_adj_right = 1,
    Rprt_adj_centre = 2,
    Rprt_adj_none = 3
} Rprt_adj;

int	Rstrlen(SEXP, int);
const char *EncodeRaw(Rbyte, const char *);
const char *EncodeString(SEXP, int, int, Rprt_adj);
const char *EncodeReal2(double, int, int, int);
const char *EncodeChar(SEXP);

/* main/raw.c */
int mbrtoint(int *w, const char *s);

/* main/sort.c */
void orderVector1(int *indx, int n, SEXP key, bool nalast,
		  bool decreasing, SEXP rho);

/* main/subset.c */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);

/* main/subassign.c */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);

#include <wchar.h>

/* main/util.c */
NORET void UNIMPLEMENTED_TYPE(const char *s, SEXP x);
NORET void UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
Rboolean Rf_strIsASCII(const char *str);
int utf8clen(char c);
int Rf_AdobeSymbol2ucs2(int n);
double R_strtod5(const char *str, char **endptr, char dec,
		 Rboolean NA, int exact);
SEXP R_listCompact(SEXP s, bool keep_initial);

typedef unsigned short R_ucs2_t;
size_t mbcsToUcs2(const char *in, R_ucs2_t *out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(R_ucs2_t *in, char *out);
size_t ucs2Mblen(R_ucs2_t *in); */
size_t utf8toucs(wchar_t *wc, const char *s);
size_t utf8towcs(wchar_t *wc, const char *s, size_t n);
size_t ucstomb(char *s, const unsigned int wc);
size_t ucstoutf8(char *s, const unsigned int wc);
size_t mbtoucs(unsigned int *wc, const char *s, size_t n);
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n);

SEXP Rf_installTrChar(SEXP);

const wchar_t *wtransChar(SEXP x); /* from sysutils.c */
const char *Rf_reEnc3(const char *x, const char *fromcode, const char *tocode, int subst);

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
Rboolean mbcsValid(const char *str);
char *mbcsTruncateToValid(char *s);
Rboolean utf8Valid(const char *str);
char *Rf_strchr(const char *s, int c);
char *Rf_strrchr(const char *s, int c);
int Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
    R_PRINTF_FORMAT(3,0);

int Rsnprintf_mbcs(char *str, size_t size, const char *format, ...)
    R_PRINTF_FORMAT(3,4);

int Rasprintf_malloc(char **str, const char *fmt, ...)
    R_PRINTF_FORMAT(2,3);

SEXP fixup_NaRm(SEXP args); /* summary.c */
void invalidate_cached_recodings(void);  /* from sysutils.c */
void resetICUcollator(bool disable); /* from util.c */
void dt_invalidate_locale(void); /* from Rstrptime.h */
extern int R_OutputCon; /* from connections.c */

extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.c */
SEXP R_SerializeInfo(R_inpstream_t ips);

void get_current_mem(size_t *,size_t *,size_t *); /* from memory.c */
unsigned long get_duplicate_counter(void);  /* from duplicate.c */
void reset_duplicate_counter(void);  /* from duplicate.c */
void BindDomain(char *); /* from main.c */
extern bool LoadInitFile;  /* from startup.c, uses in sys-*.c */

// Unix and Windows versions
double R_getClockIncrement(void);
void R_getProcTime(double *data);
void InitDynload(void);
void R_CleanTempDir(void);

#ifdef Win32
void R_fixslash(char *s);
void R_fixbackslash(char *s);
void R_wfixbackslash(wchar_t *s);
void R_wfixslash(wchar_t *s);
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand);
#endif

FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand);
int Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA);
double R_strtod(const char *str, char **endptr);
double R_atof(const char *str);

/* unix/sys-std.c, main/options.c */
void set_rl_word_breaks(const char *str);

/* unix/sys-unix.c, main/connections.c */
FILE *R_popen_pg(const char *cmd, const char *type);
int R_pclose_pg(FILE *fp);

/* From localecharset.c */
extern const char *locale2charset(const char *);

/* Localization */

#ifndef NO_NLS
# ifdef ENABLE_NLS
// libintl.h might remap these to libintl_XXXX and normally does in GNU gettext
#  include <libintl.h>
#  ifdef Win32
// assumes Windows is using GNU gettext with remapping.
#   define _(String) libintl_gettext (String)
#   undef gettext /* needed for graphapp */
#  else
#   define _(String) gettext (String)
#  endif
#  define gettext_noop(String) String
#  define N_(String) gettext_noop (String)
# else /* not NLS */
#  define _(String) (String)
#  define N_(String) String
#  define ngettext(String, StringP, N) (N > 1 ? StringP: String)
# endif
#endif

/* Macros for suspending interrupts: also in GraphicsDevice.h */
#define BEGIN_SUSPEND_INTERRUPTS do { \
    Rboolean __oldsusp__ = R_interrupts_suspended; \
    R_interrupts_suspended = TRUE;
#define END_SUSPEND_INTERRUPTS R_interrupts_suspended = __oldsusp__; \
    if (R_interrupts_pending && ! R_interrupts_suspended) \
        onintr(); \
} while(0)


/*
   alloca is neither C99 nor POSIX.

   It might be better to try alloca.h first, see
   https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Particular-Functions.html
*/
#ifdef __GNUC__
// This covers GNU, Clang and Intel compilers
// The undef is needed in case some other header, e.g. malloc.h, already did this
# undef alloca
# define alloca(x) __builtin_alloca((x))
#else
# ifdef HAVE_ALLOCA_H
// Needed for native compilers on Solaris and AIX
#  include <alloca.h>
# endif
// it might have been defined via some other standard header, e.g. stdlib.h
# if !HAVE_DECL_ALLOCA
#  include <stddef.h> // for size_t
extern void *alloca(size_t);
# endif
#endif

/* Required by C99, but might be slow */
#ifdef HAVE_LONG_DOUBLE
# define LDOUBLE long double
#else
# define LDOUBLE double
#endif

/* int_fast64_t is required by C99/C11
   Alternative would be to use intmax_t.
   Used in summary.c
 */
#ifdef HAVE_INT64_T
# define LONG_INT int64_t
# define LONG_INT_MAX INT64_MAX
#elif defined(HAVE_INT_FAST64_T)
# define LONG_INT int_fast64_t
# define LONG_INT_MAX INT_FAST64_MAX
#endif

// for reproducibility for now: use exp10 or pown later if accurate enough.
#define Rexp10(x) pow(10.0, x)

// this produces an initialized structure as a _compound literal_
#define SEXP_TO_STACKVAL(x) ((R_bcstack_t) { .tag = 0, .u.sxpval = (x) })

#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
