/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016   The R Core Team
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


/**
 **  ALTREP Class Registry for Serialization
 **/

/* Use ATTRIB field to hold class info. OK since not visible outside. */
#define ALTREP_CLASS_SERIALIZED_CLASS(x) ATTRIB(x)
#define SET_ALTREP_CLASS_SERIALIZED_CLASS(x, csym, psym, stype) \
    SET_ATTRIB(x, list3(csym, psym, stype))
#define ALTREP_SERIALIZED_CLASS_CLSSYM(x) CAR(x)
#define ALTREP_SERIALIZED_CLASS_PKGSYM(x) CADR(x)
#define ALTREP_SERIALIZED_CLASS_TYPE(x) INTEGER0(CADDR(x))[0]

#define ALTREP_CLASS_BASE_TYPE(x) \
    ALTREP_SERIALIZED_CLASS_TYPE(ALTREP_CLASS_SERIALIZED_CLASS(x))

static SEXP Registry = NULL;

static SEXP LookupClassEntry(SEXP csym, SEXP psym)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain))
	if (TAG(CAR(chain)) == csym && CADR(CAR(chain)) == psym)
	    return CAR(chain);
    return NULL;
}

static void
RegisterClass(SEXP class, int type, const char *cname, const char *pname,
	      DllInfo *dll)
{
    PROTECT(class);
    if (Registry == NULL) {
	Registry = CONS(R_NilValue, R_NilValue);
	R_PreserveObject(Registry);
    }

    SEXP csym = install(cname);
    SEXP psym = install(pname);
    SEXP stype = PROTECT(ScalarInteger(type));
    SEXP iptr = R_MakeExternalPtr(dll, R_NilValue, R_NilValue);
    SEXP entry = LookupClassEntry(csym, psym);
    if (entry == NULL) {
	entry = list4(class, psym, stype, iptr);
	SET_TAG(entry, csym);
	SETCDR(Registry, CONS(entry, CDR(Registry)));
    }
    else {
	SETCAR(entry, class);
	SETCAR(CDR(CDR(entry)), stype);
	SETCAR(CDR(CDR(CDR(entry))), iptr);
    }
    SET_ALTREP_CLASS_SERIALIZED_CLASS(class, csym, psym, stype);
    UNPROTECT(2); /* class, stype */
}

static SEXP LookupClass(SEXP csym, SEXP psym)
{
    SEXP entry = LookupClassEntry(csym, psym);
    return entry != NULL ? CAR(entry) : NULL;
}

static void reinit_altrep_class(SEXP sclass);
void attribute_hidden R_reinit_altrep_classes(DllInfo *dll)
{
    for (SEXP chain = CDR(Registry); chain != R_NilValue; chain = CDR(chain)) {
	SEXP entry = CAR(chain);
	SEXP iptr = CAR(CDR(CDR(CDR(entry))));
	if (R_ExternalPtrAddr(iptr) == dll)
	    reinit_altrep_class(CAR(entry));
    }
}


/**
 **  ALTREP Method Tables and Class Objects
 **/

static void SET_ALTREP_CLASS(SEXP x, SEXP class)
{
    SET_TAG(x, class);
    SETALTREP(x, 1);
}

#define CLASS_METHODS_TABLE(class) RAWDATAPTR(class)
#define GENERIC_METHODS_TABLE(x, class) \
    ((class##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))

#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)
#define ALTINTEGER_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altinteger)
#define ALTREAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altreal)
#define ALTSTRING_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altstring)

#define ALTREP_METHODS						\
    R_altrep_Unserialize_method_t Unserialize;			\
    R_altrep_Unserialize_core_method_t Unserialize_core;	\
    R_altrep_Serialized_state_method_t Serialized_state;	\
    R_altrep_Duplicate_method_t Duplicate;			\
    R_altrep_Duplicate_method_t Duplicate_core;			\
    R_altrep_Coerce_method_t Coerce;				\
    R_altrep_Inspect_method_t Inspect;				\
    R_altrep_Length_method_t Length

#define ALTVEC_METHODS					\
    ALTREP_METHODS;					\
    R_altvec_Dataptr_method_t Dataptr;			\
    R_altvec_Dataptr_or_null_method_t Dataptr_or_null;	\
    R_altvec_Extract_subset_method_t Extract_subset

#define ALTINTEGER_METHODS				\
    ALTVEC_METHODS;					\
    R_altinteger_Elt_method_t Elt;			\
    R_altinteger_Get_region_method_t Get_region;	\
    R_altinteger_Is_sorted_method_t Is_sorted

#define ALTREAL_METHODS				\
    ALTVEC_METHODS;				\
    R_altreal_Elt_method_t Elt;			\
    R_altreal_Get_region_method_t Get_region

#define ALTSTRING_METHODS			\
    ALTVEC_METHODS;				\
    R_altstring_Elt_method_t Elt

typedef struct { ALTREP_METHODS; } altrep_methods_t;
typedef struct { ALTVEC_METHODS; } altvec_methods_t;
typedef struct { ALTINTEGER_METHODS; } altinteger_methods_t;
typedef struct { ALTREAL_METHODS; } altreal_methods_t;
typedef struct { ALTSTRING_METHODS; } altstring_methods_t;

#define ALTREP_DISPATCH(fun, x, ...) \
    ALTREP_METHODS_TABLE(x)->fun(x, __VA_ARGS__)
#define ALTREP_DISPATCH0(fun, x) ALTREP_METHODS_TABLE(x)->fun(x)

#define ALTVEC_DISPATCH(fun, x, ...) \
    ALTVEC_METHODS_TABLE(x)->fun(x, __VA_ARGS__)
#define ALTVEC_DISPATCH0(fun, x) ALTVEC_METHODS_TABLE(x)->fun(x)

#define ALTINTEGER_DISPATCH(fun, x, ...) \
    ALTINTEGER_METHODS_TABLE(x)->fun(x, __VA_ARGS__)
#define ALTINTEGER_DISPATCH0(fun, x) ALTINTEGER_METHODS_TABLE(x)->fun(x)

#define ALTREAL_DISPATCH(fun, x, ...) \
    ALTREAL_METHODS_TABLE(x)->fun(x, __VA_ARGS__)
#define ALTSTRING_DISPATCH(fun, x, ...) \
    ALTSTRING_METHODS_TABLE(x)->fun(x, __VA_ARGS__)


/*
 * Generic ALTREP support
 */

SEXP attribute_hidden ALTREP_COERCE(SEXP x, int type)
{
    return ALTREP_DISPATCH(Coerce, x, type);
}

static SEXP ALTREP_DUPLICATE_CORE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate_core, x, deep);
}

SEXP attribute_hidden ALTREP_DUPLICATE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate, x, deep);
}

Rboolean attribute_hidden
ALTREP_INSPECT(SEXP x, int pre, int deep, int pvec,
	       void (*inspect_subtree)(SEXP, int, int, int))
{
    return ALTREP_DISPATCH(Inspect, x, pre, deep, pvec, inspect_subtree);
}


SEXP attribute_hidden
ALTREP_SERIALIZED_STATE(SEXP x)
{
    return ALTREP_DISPATCH0(Serialized_state, x);
}

SEXP attribute_hidden
ALTREP_SERIALIZED_CLASS(SEXP x)
{
    SEXP val = ALTREP_CLASS_SERIALIZED_CLASS(ALTREP_CLASS(x));
    return val != R_NilValue ? val : NULL;
}

static SEXP find_namespace(void *data) { return R_FindNamespace((SEXP) data); }
static SEXP handle_namespace_error(SEXP cond, void *data) { return R_NilValue; }

static SEXP ALTREP_UNSERIALIZE_CLASS(SEXP info)
{
    if (TYPEOF(info) == LISTSXP) {
	SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
	SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
	SEXP class = LookupClass(csym, psym);
	if (class == NULL) {
	    SEXP pname = ScalarString(PRINTNAME(psym));
	    R_tryCatchError(find_namespace, pname,
			    handle_namespace_error, NULL);
	    class = LookupClass(csym, psym);
	}
	return class;
    }
    return NULL;
}

SEXP attribute_hidden
ALTREP_UNSERIALIZE(SEXP info, SEXP state, SEXP attr)
{
    SEXP csym = ALTREP_SERIALIZED_CLASS_CLSSYM(info);
    SEXP psym = ALTREP_SERIALIZED_CLASS_PKGSYM(info);
    int type = ALTREP_SERIALIZED_CLASS_TYPE(info);

    /* look up the class in the registry and handle failure */
    SEXP class = ALTREP_UNSERIALIZE_CLASS(info);
    if (class == NULL) {
	switch(type) {
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	case STRSXP:
	case RAWSXP:
	case VECSXP:
	case EXPRSXP:
	    warning("cannot unserialize ALTVEC object of class '%s' from "
		    "package '%s'; returning length zero vector",
		    CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)));
	    return allocVector(type, 0);
	default:
	    error("cannot unserialize this ALTREP object");
	}
    }

    /* check the registered and unserialized types match */
    int rtype = ALTREP_CLASS_BASE_TYPE(class);
    if (type != rtype)
	warning("serialized class '%s' from package '%s' has type %s; "
		"registered class has type %s",
		CHAR(PRINTNAME(csym)), CHAR(PRINTNAME(psym)),
		type2char(type), type2char(rtype));
    
    /* dispatch to a class method */
    altrep_methods_t *m = CLASS_METHODS_TABLE(class);
    SEXP val = m->Unserialize(class, state, attr);
    return val;
}

R_xlen_t /*attribute_hidden*/ ALTREP_LENGTH(SEXP x)
{
    return ALTREP_DISPATCH0(Length, x);
}

R_xlen_t /*attribute_hidden*/ ALTREP_TRUELENGTH(SEXP x) { return 0; }


/*
 * Generic ALTVEC support
 */

void /*attribute_hidden*/ *ALTVEC_DATAPTR(SEXP x)
{
    /**** move GC disabling into methods? */
    if (R_in_gc)
	error("cannot get ALTVEC DATAPTR during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    void *val = ALTVEC_DISPATCH0(Dataptr, x);

    R_GCEnabled = enabled;
    return val;
}

void /*attribute_hidden*/ *ALTVEC_DATAPTR_OR_NULL(SEXP x)
{
    return ALTVEC_DISPATCH0(Dataptr_or_null, x);
}

SEXP attribute_hidden ALTVEC_EXTRACT_SUBSET(SEXP x, SEXP indx, SEXP call)
{
    return ALTVEC_DISPATCH(Extract_subset, x, indx, call);
}


/*
 * Typed ALTVEC support
 */

#define CHECK_NOT_EXPANDED(x)					\
    if (DATAPTR_OR_NULL(x) != NULL)				\
	error("method should only handle unexpanded vectors")

int attribute_hidden ALTINTEGER_ELT(SEXP x, R_xlen_t i)
{
    return ALTINTEGER_DISPATCH(Elt, x, i);
}

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    int *x = DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(int));
	return ncopy;
    }
    else
	return ALTINTEGER_DISPATCH(Get_region, sx, i, n, buf);
}

static int ALTINTEGER_IS_SORTED(SEXP x)
{
    return ALTINTEGER_DISPATCH0(Is_sorted, x);
}

Rboolean INTEGER_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTINTEGER_IS_SORTED(x) : 0;
}

double attribute_hidden ALTREAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTREAL_DISPATCH(Elt, x, i);
}

R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    double *x = DATAPTR_OR_NULL(sx);
    if (x != NULL) {
	R_xlen_t size = XLENGTH(sx);
	R_xlen_t ncopy = size - i > n ? n : size - i;
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = x[k + i];
	//memcpy(buf, x + i, ncopy * sizeof(double));
	return ncopy;
    }
    else
	return ALTREAL_DISPATCH(Get_region, sx, i, n, buf);
}

SEXP attribute_hidden ALTSTRING_ELT(SEXP x, R_xlen_t i)
{
    SEXP val = NULL;

    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot get ALTSTRING_ELT during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    val = ALTSTRING_DISPATCH(Elt, x, i);

    R_GCEnabled = enabled;
    return val;
}


/*
 * Not yet implemented
 */

int attribute_hidden ALTLOGICAL_ELT(SEXP x, R_xlen_t i)
{
    error("unknown ALTLOGICAL type"); /* dispatch here */
}

Rcomplex attribute_hidden ALTCOMPLEX_ELT(SEXP x, R_xlen_t i)
{
    error("unknown ALTCOMPLEX type"); /* dispatch here */
}


/**
 ** ALTREP Default Methods
 **/

static SEXP altrep_Unserialize_default(SEXP class, SEXP state, SEXP attr)
{
    altrep_methods_t *m = CLASS_METHODS_TABLE(class);
    SEXP val = m->Unserialize_core(class, state);
    SET_ATTRIB(val, attr);
    return val;
}

static SEXP altrep_Serialized_state_default(SEXP x) { return NULL; }

static SEXP altrep_Unserialize_core_default(SEXP class, SEXP state)
{
    error("cannot unserialize this ALTREP object yet");
}

static SEXP altrep_Coerce_default(SEXP x, int type) { return NULL; }

static SEXP altrep_Duplicate_core_default(SEXP x, Rboolean deep)
{
    return NULL;
}

static SEXP ALTREP_DUPLICATE_CORE(SEXP, Rboolean);

static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep)
{
    SEXP ans = ALTREP_DUPLICATE_CORE(x, deep);

    if (ans != NULL &&
	ans != x) { /* leave attributes alone if returning original */
	/* handle attributes generically */
	SEXP attr = ATTRIB(x);
	if (attr != R_NilValue) {
	    PROTECT(ans);
	    SET_ATTRIB(ans, deep ? duplicate(attr) : shallow_duplicate(attr));
	    SET_OBJECT(ans, OBJECT(x));
	    IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	    UNPROTECT(1);
	}
    }
    return ans;
}

static
Rboolean altrep_Inspect_default(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    return FALSE;
}

static R_xlen_t altrep_Length_default(SEXP x)
{
    error("no Length method defined");
}

static void *altvec_Dataptr_default(SEXP x)
{
    /**** use class info for better error message? */
    error("cannot access data pointer for this ALTVEC object");
}

static void *altvec_Dataptr_or_null_default(SEXP x) { return NULL; }

static SEXP altvec_Extract_subset_default(SEXP x, SEXP indx, SEXP call)
{
    return NULL;
}

static int altinteger_Elt_default(SEXP x, R_xlen_t i) { return INTEGER(x)[i]; }

static R_xlen_t
altinteger_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = INTEGER_ELT(sx, k + i);
    return ncopy;
}

static int altinteger_Is_sorted_default(SEXP x) { return FALSE; }

static double altreal_Elt_default(SEXP x, R_xlen_t i) { return REAL(x)[i]; }

static R_xlen_t
altreal_Get_region_default(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    R_xlen_t size = XLENGTH(sx);
    R_xlen_t ncopy = size - i > n ? n : size - i;
    for (R_xlen_t k = 0; k < ncopy; k++)
	buf[k] = REAL_ELT(sx, k + i);
    return ncopy;
}

static SEXP altstring_Elt_default(SEXP x, R_xlen_t i)
{
    return STRING_PTR(x)[i];
}


/**
 ** ALTREP Initial Method Tables
 **/

static altinteger_methods_t altinteger_default_methods = {
    .Unserialize = altrep_Unserialize_default,
    .Unserialize_core = altrep_Unserialize_core_default,
    .Serialized_state = altrep_Serialized_state_default,
    .Duplicate = altrep_Duplicate_default,
    .Duplicate_core = altrep_Duplicate_core_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altinteger_Elt_default,
    .Get_region = altinteger_Get_region_default,
    .Is_sorted = altinteger_Is_sorted_default
};

static altreal_methods_t altreal_default_methods = {
    .Unserialize = altrep_Unserialize_default,
    .Unserialize_core = altrep_Unserialize_core_default,
    .Serialized_state = altrep_Serialized_state_default,
    .Duplicate = altrep_Duplicate_default,
    .Duplicate_core = altrep_Duplicate_core_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altreal_Elt_default,
    .Get_region = altreal_Get_region_default
};


static altstring_methods_t altstring_default_methods = {
    .Unserialize = altrep_Unserialize_default,
    .Unserialize_core = altrep_Unserialize_core_default,
    .Serialized_state = altrep_Serialized_state_default,
    .Duplicate = altrep_Duplicate_default,
    .Duplicate_core = altrep_Duplicate_core_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altstring_Elt_default
};


/**
 ** Class Constructors
 **/

#define INIT_CLASS(cls, type) do {				\
	*((type##_methods_t *) (CLASS_METHODS_TABLE(cls))) =	\
	    type##_default_methods;				\
    } while (FALSE)

#define MAKE_CLASS(var, type) do {				\
	var = allocVector(RAWSXP,sizeof(type##_methods_t));	\
	R_PreserveObject(var);					\
	INIT_CLASS(var, type);					\
    } while (FALSE)

static R_altrep_class_t
make_altrep_class(int type, const char *cname, const char *pname, DllInfo *dll)
{
    SEXP class;
    switch(type) {
    case INTSXP:  MAKE_CLASS(class, altinteger); break;
    case REALSXP: MAKE_CLASS(class, altreal);    break;
    case STRSXP:  MAKE_CLASS(class, altstring);  break;
    default: error("unsupported ALTREP class");
    }
    RegisterClass(class, type, cname, pname, dll);
    return R_cast_altrep_class(class);
}

/*  Using macros like this makes it easier to add new methods, but
    makes searching for source harder. Probably a good idea on
    balance though. */
#define DEFINE_CLASS_CONSTRUCTOR(cls, type) \
    R_altrep_class_t R_make_##cls##_class(const char *cname,	\
					  const char *pname,	\
					  DllInfo *dll)		\
    {								\
	return  make_altrep_class(type, cname, pname, dll);	\
    }

DEFINE_CLASS_CONSTRUCTOR(altstring, STRSXP)
DEFINE_CLASS_CONSTRUCTOR(altinteger, INTSXP)
DEFINE_CLASS_CONSTRUCTOR(altreal, REALSXP)

static void reinit_altrep_class(SEXP class)
{
    switch (ALTREP_CLASS_BASE_TYPE(class)) {
    case INTSXP: INIT_CLASS(class, altinteger); break;
    case REALSXP: INIT_CLASS(class, altreal); break;
    case STRSXP: INIT_CLASS(class, altstring); break;
    default: error("unsupported ALTREP class");
    }
}


/**
 ** ALTREP Method Setters
 **/

#define DEFINE_METHOD_SETTER(CNAME, MNAME)				\
    void R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
					  R_##CNAME##_##MNAME##_method_t fun) \
    {									\
	CNAME##_methods_t *m = CLASS_METHODS_TABLE(R_SEXP(cls));	\
	m->MNAME = fun;							\
    }

DEFINE_METHOD_SETTER(altrep, Unserialize)
DEFINE_METHOD_SETTER(altrep, Unserialize_core)
DEFINE_METHOD_SETTER(altrep, Serialized_state)
DEFINE_METHOD_SETTER(altrep, Duplicate)
DEFINE_METHOD_SETTER(altrep, Duplicate_core)
DEFINE_METHOD_SETTER(altrep, Coerce)
DEFINE_METHOD_SETTER(altrep, Inspect)
DEFINE_METHOD_SETTER(altrep, Length)

DEFINE_METHOD_SETTER(altvec, Dataptr)
DEFINE_METHOD_SETTER(altvec, Dataptr_or_null)
DEFINE_METHOD_SETTER(altvec, Extract_subset)

DEFINE_METHOD_SETTER(altinteger, Elt)
DEFINE_METHOD_SETTER(altinteger, Get_region)
DEFINE_METHOD_SETTER(altinteger, Is_sorted)

DEFINE_METHOD_SETTER(altreal, Elt)
DEFINE_METHOD_SETTER(altreal, Get_region)

DEFINE_METHOD_SETTER(altstring, Elt)


/**
 ** ALTREP Object Constructor and Utility Functions
 **/

SEXP R_new_altrep(R_altrep_class_t class, SEXP data1, SEXP data2)
{
    SEXP sclass = R_SEXP(class);
    int type = ALTREP_CLASS_BASE_TYPE(sclass);
    SEXP ans = CONS(data1, data2);
    SET_TYPEOF(ans, type);
    SET_ALTREP_CLASS(ans, sclass);
    return ans;
}

Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t class)
{
    return ALTREP(x) && ALTREP_CLASS(x) == R_SEXP(class);
}


/**
 ** Compact Integer Sequences
 **/

/*
 * Methods
 */

#define COMPACT_SEQ_INFO(x) R_altrep_data1(x)
#define COMPACT_SEQ_EXPANDED(x) R_altrep_data2(x)
#define SET_COMPACT_SEQ_EXPANDED(x, v) R_set_altrep_data2(x, v)
#define COMPACT_INTSEQ_INFO_LENGTH(info) INTEGER0(info)[0]
#define COMPACT_INTSEQ_INFO_FIRST(info) INTEGER0(info)[1]
#define COMPACT_INTSEQ_INFO_INCR(info) INTEGER0(info)[2]

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

static SEXP compact_intseq_Unserialize_core(SEXP class, SEXP state)
{
    int inc = COMPACT_INTSEQ_INFO_INCR(state);

    if (inc == 1)
	return new_compact_intseq(INTEGER_ELT(state, 0),
				  INTEGER_ELT(state, 1),  1);
    else if (inc == -1)
	return new_compact_intseq(INTEGER_ELT(state, 0),
				  INTEGER_ELT(state, 1),  -1);
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

static SEXP compact_intseq_Duplicate_core(SEXP x, Rboolean deep)
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

static void *compact_intseq_Dataptr(SEXP x)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	/* no need to re-run if expended data exists */
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	int n = COMPACT_INTSEQ_INFO_LENGTH(info);
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

static void *compact_intseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static int compact_intseq_Elt(SEXP x, R_xlen_t i)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(x);

    SEXP info = COMPACT_SEQ_INFO(x);
    int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);

    if (inc == 1)
	return n1 + i;
    else if (inc == -1)
	return n1 - i;
    else
	error("compact sequences with increment %d not supported yet", inc);
}

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
	    buf[k] = n1 + k + i;
	return ncopy;
    }
    else if (inc == -1) {
	for (R_xlen_t k = 0; k < ncopy; k++)
	    buf[k] = n1 - k - i;
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
	return 0;
#endif
    int inc = COMPACT_INTSEQ_INFO_INCR(COMPACT_SEQ_INFO(x));
    return inc < 0 ? -1 : 1;
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
    R_set_altrep_Unserialize_core_method(cls, compact_intseq_Unserialize_core);
    R_set_altrep_Serialized_state_method(cls, compact_intseq_Serialized_state);
    R_set_altrep_Duplicate_core_method(cls, compact_intseq_Duplicate_core);
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
}


/*
 * Constructor
 */

static SEXP new_compact_intseq(R_xlen_t n, int n1, int inc)
{
    if (n == 1) return ScalarInteger(n1);

    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

    SEXP info = allocVector(INTSXP, 3);
    INTEGER(info)[0] = n;
    INTEGER(info)[1] = n1;
    INTEGER(info)[2] = inc;

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

#define COMPACT_REALSEQ_INFO_LENGTH(info) REAL0(info)[0]
#define COMPACT_REALSEQ_INFO_FIRST(info) REAL0(info)[1]
#define COMPACT_REALSEQ_INFO_INCR(info) REAL0(info)[2]

static SEXP compact_realseq_Serialized_state(SEXP x)
{
    return COMPACT_SEQ_INFO(x);
}

static SEXP compact_realseq_Unserialize_core(SEXP class, SEXP state)
{
    int inc = COMPACT_INTSEQ_INFO_INCR(state);

    if (inc == 1)
	return new_compact_realseq(REAL_ELT(state, 0), REAL_ELT(state, 1),  1);
    else if (inc == -1)
	return new_compact_realseq(REAL_ELT(state, 0), REAL_ELT(state, 1), -1);
    else
	error("compact sequences with increment %f not supported yet", inc);
}

static SEXP compact_realseq_Duplicate_core(SEXP x, Rboolean deep)
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
    R_xlen_t n1 = REAL_ELT(x, 0);
    R_xlen_t n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %ld : %ld (%s)", n1, n2,
	    COMPACT_SEQ_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_realseq_Length(SEXP x)
{
    return REAL0(COMPACT_SEQ_INFO(x))[0];
}

static void *compact_realseq_Dataptr(SEXP x)
{
    if (COMPACT_SEQ_EXPANDED(x) == R_NilValue) {
	PROTECT(x);
	SEXP info = COMPACT_SEQ_INFO(x);
	double n = COMPACT_REALSEQ_INFO_LENGTH(info);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	
	SEXP val = allocVector(REALSXP, n);
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

static void *compact_realseq_Dataptr_or_null(SEXP x)
{
    SEXP val = COMPACT_SEQ_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static double compact_realseq_Elt(SEXP x, R_xlen_t i)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(x);

    SEXP info = COMPACT_SEQ_INFO(x);
    double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
    double inc = COMPACT_REALSEQ_INFO_INCR(info);
    
    if (inc == 1)
	return n1 + i;
    else if (inc == -1)
	return n1 - i;
    else
	error("compact sequences with increment %f not supported yet", inc);
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
    R_set_altrep_Unserialize_core_method(cls, compact_realseq_Unserialize_core);
    R_set_altrep_Serialized_state_method(cls, compact_realseq_Serialized_state);
    R_set_altrep_Duplicate_core_method(cls, compact_realseq_Duplicate_core);
    R_set_altrep_Inspect_method(cls, compact_realseq_Inspect);
    R_set_altrep_Length_method(cls, compact_realseq_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, compact_realseq_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, compact_realseq_Dataptr_or_null);

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, compact_realseq_Elt);
    R_set_altreal_Get_region_method(cls, compact_realseq_Get_region);
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

    if (n1 <= INT_MIN || n1 > INT_MAX || n2 <= INT_MIN || n2 > INT_MAX)
	return new_compact_realseq(n, n1, n1 <= n2 ? 1 : -1);
    else
	return new_compact_intseq(n, n1, n1 <= n2 ? 1 : -1);
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
#define DEFERRED_STRING_STATE_SCIPEN(s) CDR(s)

#define DEFERRED_STRING_ARG(x) \
    DEFERRED_STRING_STATE_ARG(DEFERRED_STRING_STATE(x))
#define DEFERRED_STRING_SCIPEN(x) \
    DEFERRED_STRING_STATE_SCIPEN(DEFERRED_STRING_STATE(x))

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

static SEXP deferred_string_Unserialize_core(SEXP class, SEXP state)
{
    SEXP arg = DEFERRED_STRING_STATE_ARG(state);
    SEXP sp = DEFERRED_STRING_STATE_SCIPEN(state);
    return R_deferred_coerceToString(arg, sp);
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
	memset(RAWDATAPTR(val), 0, n * sizeof(SEXP));
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
	    R_print.scipen = INTEGER0(DEFERRED_STRING_SCIPEN(x))[0];
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

static void *deferred_string_Dataptr(SEXP x)
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
    return DATAPTR(DEFERRED_STRING_EXPANDED(x));
}

static void *deferred_string_Dataptr_or_null(SEXP x)
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

static SEXP deferred_string_Extract_subset(SEXP x, SEXP indx, SEXP call)
{
    SEXP result = NULL;

    if (! OBJECT(x) && ATTRIB(x) == R_NilValue &&
	DEFERRED_STRING_STATE(x) != R_NilValue) {
	/* For deferred string coercions, create a new conversion
	   using the subset of the argument.  Could try to
	   preserve/share coercions already done, if there are any. */
	SEXP data = DEFERRED_STRING_ARG(x);
	SEXP sp = DEFERRED_STRING_SCIPEN(x);
	PROTECT(result = ExtractSubset(data, indx, call));
	result = R_deferred_coerceToString(result, sp);
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
    R_set_altrep_Unserialize_core_method(cls, deferred_string_Unserialize_core);
    R_set_altrep_Serialized_state_method(cls, deferred_string_Serialized_state);
    R_set_altrep_Inspect_method(cls, deferred_string_Inspect);
    R_set_altrep_Length_method(cls, deferred_string_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, deferred_string_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, deferred_string_Dataptr_or_null);
    R_set_altvec_Extract_subset_method(cls, deferred_string_Extract_subset);

    /* override ALTSTRING methods */
    R_set_altstring_Elt_method(cls, deferred_string_Elt);
}


/*
 * Constructor
 */

SEXP attribute_hidden R_deferred_coerceToString(SEXP v, SEXP sp)
{
    SEXP ans = R_NilValue;
    switch (TYPEOF(v)) {
    case INTSXP:
    case REALSXP:
	if (sp == NULL)
	    sp = ScalarInteger(R_print.scipen);
	ans = PROTECT(MAKE_DEFERRED_STRING_STATE(v, sp));
	ans = R_new_altrep(R_deferred_string_class, ans, R_NilValue);
	UNPROTECT(1); /* ans */
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
    /* If serOK is false then serialize as a regular typed vector. If
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

static SEXP mmap_Unserialize(SEXP class, SEXP state, SEXP attr)
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
    SET_ATTRIB(val, attr);
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

static void *mmap_Dataptr(SEXP x)
{
    /* get addr first to get error if the object has been unmapped */
    void *addr = MMAP_ADDR(x);

    if (MMAP_PTROK(x))
	return addr;
    else
	error("cannot access data pointer for this mmaped vector");
}

static void *mmap_Dataptr_or_null(SEXP x)
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
# error "I'm sure this needs adjusting for Windows, so punt for now."
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
 ** Initialize ALTREP Classes
 **/

void attribute_hidden R_init_altrep()
{
    InitCompactIntegerClass();
    InitCompactRealClass();
    InitDefferredStringClass();
    InitMmapIntegerClass(NULL);
    InitMmapRealClass(NULL);
}
