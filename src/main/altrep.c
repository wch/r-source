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

static SEXP make_wrapper(SEXP, SEXP);
#define NMETA 2

/* passed to matching macros in various places w/in this file, results in
   major speed benefit. ptr/qptr MUST already exist/be defined before
   these end up getting called */
#define fastgetptr(x,i) ptr[i]
#define fastgetqptr(x,i) qptr[i]

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
    SETALTREP(x, 1);
    SET_TAG(x, class);
}

#define CLASS_METHODS_TABLE(class) STDVEC_DATAPTR(class)
#define GENERIC_METHODS_TABLE(x, class) \
    ((class##_methods_t *) CLASS_METHODS_TABLE(ALTREP_CLASS(x)))

#define ALTREP_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altrep)
#define ALTVEC_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altvec)
#define ALTINTEGER_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altinteger)
#define ALTREAL_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altreal)
#define ALTSTRING_METHODS_TABLE(x) GENERIC_METHODS_TABLE(x, altstring)

#define ALTREP_METHODS						\
    R_altrep_UnserializeEX_method_t UnserializeEX;		\
    R_altrep_Unserialize_method_t Unserialize;			\
    R_altrep_Serialized_state_method_t Serialized_state;	\
    R_altrep_DuplicateEX_method_t DuplicateEX;			\
    R_altrep_Duplicate_method_t Duplicate;			\
    R_altrep_Coerce_method_t Coerce;				\
    R_altrep_Inspect_method_t Inspect;				\
    R_altrep_Length_method_t Length

#define ALTVEC_METHODS					\
    ALTREP_METHODS;					\
    R_altvec_Dataptr_method_t Dataptr;			\
    R_altvec_Dataptr_or_null_method_t Dataptr_or_null;	\
    R_altvec_Extract_subset_method_t Extract_subset

#define ALTINTEGER_METHODS						\
    ALTVEC_METHODS;							\
    R_altinteger_Elt_method_t Elt;					\
    R_altinteger_Set_elt_method_t Set_elt;				\
    R_altinteger_Get_region_method_t Get_region;			\
    R_altinteger_Is_sorted_method_t Is_sorted;				\
    R_altinteger_No_NA_method_t No_NA;					\
    R_altinteger_Sort_check_method_t Sort_check;			\
    R_altinteger_Is_NA_method_t Is_NA;					\
    R_altinteger_Sum_method_t Sum ;					\
    R_altinteger_Min_method_t Min;					\
    R_altinteger_Max_method_t Max;					\
    R_altinteger_Which_min_method_t Which_min;				\
    R_altinteger_Which_max_method_t Which_max;				\
    R_altinteger_Match_method_t Match;					\
    R_altinteger_Unique_method_t Unique;				\
    R_altinteger_Order_method_t Order;					\
    R_altinteger_As_subscripts_method_t As_subscripts;			\
    R_altinteger_Compression_ratio_method_t Compression_ratio
    

#define ALTREAL_METHODS							\
    ALTVEC_METHODS;							\
    R_altreal_Elt_method_t Elt;						\
    R_altreal_Set_elt_method_t Set_elt;					\
    R_altreal_Get_region_method_t Get_region;				\
    R_altreal_Is_sorted_method_t Is_sorted;				\
    R_altreal_No_NA_method_t No_NA;					\
    R_altreal_Sort_check_method_t Sort_check;				\
    R_altreal_Is_NA_method_t Is_NA;					\
    R_altreal_Sum_method_t Sum;						\
    R_altreal_Min_method_t Min;						\
    R_altreal_Max_method_t Max;						\
    R_altreal_Which_min_method_t Which_min;				\
    R_altreal_Which_max_method_t Which_max;				\
    R_altreal_Match_method_t Match;					\
    R_altreal_Unique_method_t Unique;					\
    R_altreal_Order_method_t Order;					\
    R_altreal_As_subscripts_method_t As_subscripts;			\
    R_altreal_Compression_ratio_method_t Compression_ratio

    
#define ALTSTRING_METHODS			\
    ALTVEC_METHODS;				\
    R_altstring_Elt_method_t Elt;		\
    R_altstring_Set_elt_method_t Set_elt;	\
    R_altstring_Is_sorted_method_t Is_sorted;	\
    R_altstring_No_NA_method_t No_NA

typedef struct { ALTREP_METHODS; } altrep_methods_t;
typedef struct { ALTVEC_METHODS; } altvec_methods_t;
typedef struct { ALTINTEGER_METHODS; } altinteger_methods_t;
typedef struct { ALTREAL_METHODS; } altreal_methods_t;
typedef struct { ALTSTRING_METHODS; } altstring_methods_t;

/* Macro to extract first element from ... macro argument.
   From Richard Hansen's answer in
   http://stackoverflow.com/questions/5588855/standard-alternative-to-gccs-va-args-trick 
*/
#define DISPATCH_TARGET(...) DISPATCH_TARGET_HELPER(__VA_ARGS__, dummy)
#define DISPATCH_TARGET_HELPER(x, ...) x

#define DO_DISPATCH(type, fun, ...)					\
    type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun(__VA_ARGS__)

#define DISPATCH_METHOD(type, fun, ...)					\
    type##_METHODS_TABLE(DISPATCH_TARGET(__VA_ARGS__))->fun

#define ALTREP_DISPATCH(fun, ...) DO_DISPATCH(ALTREP, fun, __VA_ARGS__)
#define ALTVEC_DISPATCH(fun, ...) DO_DISPATCH(ALTVEC, fun, __VA_ARGS__)
#define ALTINTEGER_DISPATCH(fun, ...) DO_DISPATCH(ALTINTEGER, fun, __VA_ARGS__)
#define ALTREAL_DISPATCH(fun, ...) DO_DISPATCH(ALTREAL, fun, __VA_ARGS__)
#define ALTSTRING_DISPATCH(fun, ...) DO_DISPATCH(ALTSTRING, fun, __VA_ARGS__)

/* is this TOO general? I don't think so..., possible it should be ALTVEC? */

#define ALTREP_INFO(x) R_altrep_data1(x)
#define ALTREP_EXPANDED(x) R_altrep_data2(x)
#define ALTREP_NONEXP(x) (ALTREP(x) && ALTREP_EXPANDED(x) == R_NilValue)

/*
 * Generic ALTREP support
 */

SEXP attribute_hidden ALTREP_COERCE(SEXP x, int type)
{
    /* is this safe at this level? */
    if(ALTREP_EXPANDED(x) != R_NilValue)
	return NULL;
    return ALTREP_DISPATCH(Coerce, x, type);
}

static SEXP ALTREP_DUPLICATE(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(Duplicate, x, deep);
}

SEXP attribute_hidden ALTREP_DUPLICATE_EX(SEXP x, Rboolean deep)
{
    return ALTREP_DISPATCH(DuplicateEX, x, deep);
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
    return ALTREP_DISPATCH(Serialized_state, x);
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
ALTREP_UNSERIALIZE_EX(SEXP info, SEXP state, SEXP attr, int objf, int levs)
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
    SEXP val = m->UnserializeEX(class, state, attr, objf, levs);
    return val;
}

R_xlen_t /*attribute_hidden*/ ALTREP_LENGTH(SEXP x)
{
    return ALTREP_DISPATCH(Length, x);
}

R_xlen_t /*attribute_hidden*/ ALTREP_TRUELENGTH(SEXP x) { return 0; }


/*
 * Generic ALTVEC support
 */

static R_INLINE void *ALTVEC_DATAPTR_EX(SEXP x, Rboolean writeable)
{
    /**** move GC disabling into methods? */
    if (R_in_gc)
	error("cannot get ALTVEC DATAPTR during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    void *val = ALTVEC_DISPATCH(Dataptr, x, writeable);

    R_GCEnabled = enabled;
    return val;
}

void /*attribute_hidden*/ *ALTVEC_DATAPTR(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, TRUE);
}

const void /*attribute_hidden*/ *ALTVEC_DATAPTR_RO(SEXP x)
{
    return ALTVEC_DATAPTR_EX(x, FALSE);
}

const void /*attribute_hidden*/ *ALTVEC_DATAPTR_OR_NULL(SEXP x)
{
    return ALTVEC_DISPATCH(Dataptr_or_null, x);
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

#define ALTINTEGER_EXPANDED(x) R_altrep_data2(x)
#define ALTREAL_EXPANDED(x) R_altrep_data2(x)
#define SET_ALTINTEGER_EXPANDED(x, v) R_set_altrep_data2(x, v)
#define SET_ALTREAL_EXPANDED(x, v) R_set_altrep_data2(x, v)

int attribute_hidden ALTINTEGER_ELT(SEXP x, R_xlen_t i)
{
    return ALTINTEGER_DISPATCH(Elt, x, i);
}

void attribute_hidden ALTINTEGER_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    ALTINTEGER_DISPATCH(Set_elt, x, i, v);
}

R_xlen_t INTEGER_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    const int *x = INTEGER_OR_NULL(sx);
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

int INTEGER_IS_SORTED(SEXP x)
{
#ifdef CHECK_FOR_NONEXP
    return ALTREP_NONEXP(x) ?
	ALTINTEGER_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
#else
    return ALTREP(x) ? ALTINTEGER_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
#endif
}

int INTEGER_NO_NA(SEXP x)
{
#ifdef CHECK_FOR_NONEXP
    return ALTREP_NONEXP(x) ? ALTINTEGER_DISPATCH(No_NA, x) : 0;
#else
    return ALTREP(x) ? ALTINTEGER_DISPATCH(No_NA, x) : 0;
#endif    
}

SEXP INTEGER_IS_NA(SEXP x)
{
    Rboolean noNA = INTEGER_NO_NA(x);
    if(noNA) {
	SEXP ans = PROTECT(allocVector(LGLSXP, XLENGTH(x)));
	int *ptr = LOGICAL(ans);
	memset(ptr, 0, sizeof(int) * XLENGTH(x));
	UNPROTECT(1);
	return ans;
    }
    return ALTREP_NONEXP(x) ? ALTINTEGER_DISPATCH(Is_NA, x) : NULL;
}	


SEXP REAL_IS_NA(SEXP x)
{
    Rboolean noNA = REAL_NO_NA(x);
    if(noNA) {
	SEXP ans = PROTECT(allocVector(LGLSXP, XLENGTH(x)));
	int *ptr = LOGICAL(ans);
	memset(ptr, 0, sizeof(int) * XLENGTH(x));
	UNPROTECT(1);
	return ans;
    }
    return ALTREP_NONEXP(x) ? ALTREAL_DISPATCH(Is_NA, x) : NULL;
}	

double attribute_hidden ALTREAL_ELT(SEXP x, R_xlen_t i)
{
    return ALTREAL_DISPATCH(Elt, x, i);
}

void attribute_hidden ALTREAL_SET_ELT(SEXP x, R_xlen_t i, double v)
{
    ALTREAL_DISPATCH(Set_elt, x, i, v);
}

R_xlen_t REAL_GET_REGION(SEXP sx, R_xlen_t i, R_xlen_t n, double *buf)
{
    const double *x = REAL_OR_NULL(sx);
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

int REAL_IS_SORTED(SEXP x)
{
#ifdef CHECK_FOR_NONEXP
    return ALTREP_NONEXP(x) ?
	ALTREAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
#else
    return ALTREP(x) ? ALTREAL_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
#endif
}

int REAL_NO_NA(SEXP x)
{
#ifdef CHECK_FOR_NONEXP
    return ALTREP_NONEXP(x) ? ALTREAL_DISPATCH(No_NA, x) : 0;
#else
    return ALTREP(x) ? ALTREAL_DISPATCH(No_NA, x) : 0;
#endif
}

SEXP /*attribute_hidden*/ ALTSTRING_ELT(SEXP x, R_xlen_t i)
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

void attribute_hidden ALTSTRING_SET_ELT(SEXP x, R_xlen_t i, SEXP v)
{
    /**** move GC disabling into method? */
    if (R_in_gc)
	error("cannot get ALTSTRING_ELT during GC");
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;

    ALTSTRING_DISPATCH(Set_elt, x, i, v);

    R_GCEnabled = enabled;
}

int STRING_IS_SORTED(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(Is_sorted, x) : UNKNOWN_SORTEDNESS;
}

int STRING_NO_NA(SEXP x)
{
    return ALTREP(x) ? ALTSTRING_DISPATCH(No_NA, x) : 0;
}

SEXP ALTINTEGER_SUM(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Sum, x, narm);
}

int ALTINTEGER_MIN(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Min, x, narm);
}

int ALTINTEGER_MAX(SEXP x, Rboolean narm)
{
    return ALTINTEGER_DISPATCH(Max, x, narm);

}

SEXP ALTINTEGER_MATCH(SEXP table, SEXP x, int nm, SEXP incomp, SEXP env,
		      Rboolean first) {
    /* TODO: be cleverer about this when they're factors?? */
    if(OBJECT(table) || OBJECT(x) || TYPEOF(x) != INTSXP) {
	return NULL;
    }
    return ALTINTEGER_DISPATCH(Match, table, x, nm, incomp, env, first);
}

SEXP ALTREAL_SUM(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Sum, x, narm);
}

double ALTREAL_MIN(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Min, x, narm);
}

double ALTREAL_MAX(SEXP x, Rboolean narm)
{
    return ALTREAL_DISPATCH(Max, x, narm);

}

SEXP ALTREAL_MATCH(SEXP table, SEXP x, int nm, SEXP incomp, SEXP env,
		      Rboolean first) {
    return ALTREAL_DISPATCH(Match, table, x, nm, incomp, env, first);
}



/*
 * Not yet implemented
 */

int attribute_hidden ALTLOGICAL_ELT(SEXP x, R_xlen_t i)
{
    return LOGICAL(x)[i]; /* dispatch here */
}

Rcomplex attribute_hidden ALTCOMPLEX_ELT(SEXP x, R_xlen_t i)
{
    return COMPLEX(x)[i]; /* dispatch here */
}

Rbyte attribute_hidden ALTRAW_ELT(SEXP x, R_xlen_t i)
{
    return RAW(x)[i]; /* dispatch here */
}

void ALTLOGICAL_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    LOGICAL(x)[i] = v; /* dispatch here */
}

void ALTCOMPLEX_SET_ELT(SEXP x, R_xlen_t i, Rcomplex v)
{
    COMPLEX(x)[i] = v; /* dispatch here */
}

void ALTRAW_SET_ELT(SEXP x, R_xlen_t i, int v)
{
    RAW(x)[i] = (Rbyte) v; /* dispatch here */
}


/**
 ** ALTREP Default Methods
 **/

static SEXP altrep_UnserializeEX_default(SEXP class, SEXP state, SEXP attr,
					 int objf, int levs)
{
    altrep_methods_t *m = CLASS_METHODS_TABLE(class);
    SEXP val = m->Unserialize(class, state);
    SET_ATTRIB(val, attr);
    SET_OBJECT(val, objf);
    SETLEVELS(val, levs);
    return val;
}

static SEXP altrep_Serialized_state_default(SEXP x) { return NULL; }

static SEXP altrep_Unserialize_default(SEXP class, SEXP state)
{
    error("cannot unserialize this ALTREP object yet");
}

static SEXP altrep_Coerce_default(SEXP x, int type) { return NULL; }

static SEXP altrep_Duplicate_default(SEXP x, Rboolean deep)
{
    return NULL;
}

static SEXP altrep_DuplicateEX_default(SEXP x, Rboolean deep)
{
    SEXP ans = ALTREP_DUPLICATE(x, deep);

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

static void *altvec_Dataptr_default(SEXP x, Rboolean writeable)
{
    /**** use class info for better error message? */
    error("cannot access data pointer for this ALTVEC object");
}

static const void *altvec_Dataptr_or_null_default(SEXP x)
{
    return NULL;
}

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

#define DECLARE_ATOMIC_VEC_DATAPTR_DEFAULT(type, altpref, TYPEPTR, SXPTYPE ) \
    static type *altpref##_Dataptr(SEXP x)				\
    {									\
	if(ALT##TYPEPTR##_EXPANDED(x) != R_NilValue) {			\
	    PROTECT(x);							\
	    R_xlen_t n = XLENGTH(x);					\
	    SEXP ans =  allocVector(SXPTYPE, n);			\
	    type *data = TYPEPTR(ans);					\
	    TYPEPTR##_GET_REGION(x, 0, n, data);			\
	    SET_ALT##TYPEPTR##_EXPANDED(x, ans);			\
	}								\
	return DATAPTR(ALT##TYPEPTR##_EXPANDED(x));			\
    }

//DECLARE_ATOMIC_VEC_DATAPTR_DEFAULT(int, altinteger, INTEGER, INTSXP)
//DECLARE_ATOMIC_VEC_DATAPTR_DEFAULT(double, altreal, REAL, REALSXP)
//DECLARE_ATOMIC_VEC_DATAPTR_DEFAULT(int, altlogical, LOGICAL, LGLSXP)




static int altinteger_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altinteger_No_NA_default(SEXP x) { return 0; }
/* purpose of this is to memoise sortedness as side-effect. 
   No point in default method unless we have default way of
   indicating sortedness on the object. */
static int altinteger_Sort_check_default(SEXP x) {
    error("ALTINTEGER classes must provide a Sort_check method");
}




#define INTVAL_ISNA(val) (val == NA_INTEGER)

#define ALT_ISNA_DEFAULT(x, ALTPREFIX, NACHK) do {			\
	/* *_IS_SORTED imples na.last */				\
	int sorted = ALTPREFIX##_IS_SORTED(x);				\
	SEXP ans;							\
	R_xlen_t i;							\
	if(KNOWN_SORTED(sorted)) {					\
	    R_xlen_t cnt = 0;						\
	    i = XLENGTH(x) - 1;						\
	    while(i >= 0 && NACHK(ALTPREFIX##_ELT(x, i))) {		\
		cnt++;							\
		i--;							\
	    }								\
	    								\
	    /* XXX this will be altlogical Rle once it exists*/		\
	    PROTECT(ans= allocVector(LGLSXP, XLENGTH(x)));		\
	int *ptr = LOGICAL(ans);					\
	if(XLENGTH(x) - cnt > 0) {					\
	    memset(ptr, 0, sizeof(int) *(XLENGTH(x)  - cnt));		\
	}								\
	if(cnt > 0) {							\
	    for(R_xlen_t j = 1; j <= cnt; j++)				\
		ptr[XLENGTH(x) - 1 - cnt + j] = TRUE;			\
	}								\
    } else {	/*not known sorted (unknown or known unsorted)	*/	\
	PROTECT(ans= allocVector(LGLSXP, XLENGTH(x)));			\
	int *ptr = LOGICAL(ans);					\
	if(TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP) {		\
	    for(i = 0; i < LENGTH(x); i++) {				\
		ptr[i] = NACHK(ALTPREFIX##_ELT(x, i));			\
	    }								\
	} else {							\
	    ans = NULL;							\
	}								\
									\
    }									\
    UNPROTECT(1); /*ans, PROTECTED in if and else block */		\
    return ans;								\
    } while(0);				


static SEXP altinteger_Is_NA_default(SEXP x) {
    ALT_ISNA_DEFAULT(x, INTEGER, INTVAL_ISNA);
}

static SEXP altinteger_Sum_default(SEXP x, Rboolean narm) { return NULL; }

#define ALT_MINMAX(x, TYPE, ALTPREFIX, COMP, DOMAX, NARM, WHICH, NACHK) do { \
	int sorted = ALTPREFIX##_IS_SORTED(x);				\
	R_xlen_t pos;							\
	TYPE val;							\
	if(sorted == KNOWN_INCR) {					\
	    if(DOMAX) {							\
		pos = XLENGTH(x) - 1;					\
		val = ALTPREFIX##_ELT(x, pos);				\
		while(NACHK(val) && pos > 0) {				\
		    pos--;						\
		    val = ALTPREFIX##_ELT(x, pos);			\
		}							\
	    } else {							\
		val = ALTPREFIX##_ELT(x, 0);				\
		pos = 0;						\
	    }								\
	    if(NARM && NACHK(val)) { val = R_NegInf; pos = -1;}		\
	    return WHICH ? pos : val;					\
	} else if(sorted == KNOWN_DECR) {			 	\
	    if(!DOMAX) {						\
		pos = XLENGTH(x) - 1;					\
		val = ALTPREFIX##_ELT(x, pos);				\
		while(NACHK(val) && pos > 0) {				\
		    pos--;						\
		    val = ALTPREFIX##_ELT(x, pos);			\
		}							\
	    } else {							\
		val = ALTPREFIX##_ELT(x, 0);				\
		pos = 0;						\
	    }								\
	    if(NARM && NACHK(val)) { val = R_NegInf; pos = -1;}		\
	    return WHICH ? pos : val;					\
	}								\
	Rboolean noNA = ALTPREFIX##_NO_NA(x);				\
	TYPE ret = ALTPREFIX##_ELT(x, 0);				\
	pos = NACHK(ret) ? -1 : 0;					\
	TYPE cur = (TYPE)0;						\
	if(noNA) {							\
	    for(R_xlen_t i = 1; i < LENGTH(x); i++) {			\
		cur = ALTPREFIX##_ELT(x, i);				\
		if(COMP(cur, ret)) {					\
		    ret = cur;						\
		    pos = i;						\
		}							\
	    }								\
	} else {							\
	    R_xlen_t j = 1;						\
	    while(NACHK(cur) && j < LENGTH(x)) {			\
		cur = ALTPREFIX##_ELT(x, j);				\
		j++;							\
	    }								\
	    ret = cur;							\
	    pos = j;							\
	    for( ; j < LENGTH(x); j++) {				\
		cur = ALTPREFIX##_ELT(x, j);				\
		if(!NACHK(cur) && cur < ret) {				\
		    ret = cur;						\
		    pos = j;						\
		}							\
	    }								\
	}								\
	return WHICH ? pos : ret;					\
    } while (0);

#define LT(x,y) x < y
#define GT(x,y) x > y

static int altinteger_Min_default(SEXP x, Rboolean narm) {
    ALT_MINMAX(x, int, INTEGER, LT, FALSE, narm, FALSE, INTVAL_ISNA)
}

static int altinteger_Max_default(SEXP x, Rboolean narm) {
    ALT_MINMAX(x, int, INTEGER, GT, TRUE, narm, FALSE, INTVAL_ISNA)
}


static R_xlen_t altinteger_Which_min_default(SEXP x){
    ALT_MINMAX(x, int, INTEGER, LT, FALSE, TRUE, TRUE, INTVAL_ISNA)
}

static R_xlen_t altinteger_Which_max_default(SEXP x) {
    ALT_MINMAX(x, int, INTEGER, GT, TRUE, TRUE, TRUE, INTVAL_ISNA)
}

static SEXP altinteger_Order_default(SEXP x) {
    return NULL;
}

static void altinteger_Set_elt_default(SEXP x, R_xlen_t i, int v) {
    error("altinteger classes must define a specific Set_elt method");
}

#ifdef DODO
static SEXP altinteger_As_subscripts_default(SEXP x) {
    return x;
}
#endif

/* right now this returns 1.0, ie we don't know of any compression benefit
   it could also throw an error and force all classes to implement a method */
static double altinteger_Compression_ratio_default(SEXP x) {
    //error("altinteger classes must define a specific Compression_Ratio method");
    return 1.0;
}

#ifdef DODO
static SEXP altinteger_order_default(SEXP x, Rboolean decr, int nalast) {
    
    R_xlen_t n = XLENGTH(x);
    int sorted = INTEGER_IS_SORTED(x);
    if(sorted == KNOWN_INCR) 
	return decr ? R_compact_intrange(n, 1) : R_compact_intrange(1, n);
    else if(sorted == KNOWN_DECR)
	return decr ? R_compact_intrange(1, n) : R_compact_intrange(n, 1);
    return NULL;
}
#endif

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

static int altreal_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altreal_No_NA_default(SEXP x) { return 0; }

static int altreal_Sort_check_default (SEXP x) {
    error("altreal calsses must provide Sort_check method");
}

static SEXP altreal_Order_default(SEXP x) {
    return NULL;
}

static SEXP altreal_Is_NA_default(SEXP x) {
    ALT_ISNA_DEFAULT(x, REAL, ISNAN)
}

static SEXP altreal_Sum_default(SEXP x, Rboolean narm) { return NULL; }
    
static double altreal_Min_default(SEXP x, Rboolean narm) {
    ALT_MINMAX(x, double, REAL, LT, FALSE, narm, FALSE, ISNAN)
}

static double altreal_Max_default(SEXP x, Rboolean narm) {
    ALT_MINMAX(x, double, REAL, GT, TRUE, narm, FALSE, ISNAN)
}


static R_xlen_t altreal_Which_min_default(SEXP x) {
    ALT_MINMAX(x, double, REAL, LT, FALSE, TRUE, TRUE, ISNAN)
}

static R_xlen_t altreal_Which_max_default(SEXP x) {
    ALT_MINMAX(x, double, REAL, GT, TRUE, TRUE, TRUE, ISNAN)
}



/* Currently this code doesn't deal with incomp, so it should never be
   hit when that is in play. check must happen outside of macro. */

/* There's room to uber-optimize things here by shuffling if's around
   but right now I'm just looking for it to work. Switch from O(n) to
   O(log n) should completely obliterate that kind of optimization
   anyway */

/* This keeps descending until it finds the *lowest* (frst==TRUE) or
   *highest* (frst==FALSE) index, it doesn't stop after the first
   match. */

/* When no match is found, pos will be position of the first position
   to the right of where qval would be inserted, regardless of
   sortedness, or XLENGTH(table) - 1 if  insertion point is after 
   the last element. */

/* Symbol meanings:
   tb = table (for lookup) ALT_MATCH_DEFAULT guarantees it's length >=2 before
        calling down.
   qval = individual value to lookup in tb
   pos = current position (variable) 
   cval = current value variable
   u = upper edge of window we're currently looking at
   l = lower edge of window we're currently looking at
   ust = starting value for uper window edge
   lst = startinv value for lower window edge
   sd = sort direction (-1 for descending, 1 for ascending)
   ALTPREF = prefix to _ELT (etc), (INTEGER or REAL)
   frst = are we looking for lowest matching index (true) or highest (false)
   nachk = the function/macro to check for NA values. Should be ISNA for reals 
           and INTVAL_ISNA for ints
*/


#define BINARY_FIND(tb, qval, pos, cval, u, l, ust, lst, sd, frst,	\
		    tbeltfun, nachk)					\
    do { 								\
	u = ust;							\
	l = lst;							\
	pos  = (u + l) / 2; /* (long) int division */			\
	cval = tbeltfun(tb, pos);					\
	while(u > l + 1) {						\
	    cval = tbeltfun(tb, pos);					\
	    if(nachk(cval) || (cval > qval && sd == KNOWN_INCR) ||	\
	       (cval < qval && sd == KNOWN_DECR) ||			\
	       (cval == qval && frst)) {				\
		/* walk to lower indices, sorted implies na.last */	\
		    u = pos;						\
	    } else if((cval < qval && sd == KNOWN_INCR )  ||		\
		      (cval > qval && sd == KNOWN_DECR ) ||		\
		      (cval == qval && !frst)) {			\
		/*walk to higher indices */				\
		l = pos;						\
	    }								\
	    pos =  (u + l) / 2; /* (long) int division */		\
	    cval = tbeltfun(tb, pos);					\
	}								\
	/*last checks */						\
	if(cval != qval &&						\
	   pos == l &&							\
	   ((tbeltfun(tb, u) < qval) != (tbeltfun(tb, l) < qval))) {	\
	    /* if we found a spot where t_i < qval < t_(i+1) return	\
	       t_(i+1) as indicated in comment block */			\
	    pos = u;							\
		cval = tbeltfun(tb, pos);				\
	} else if(frst && pos == u && tbeltfun(tb, l) == qval) {	\
	    pos = l;							\
	    cval = qval;						\
	} else if (!frst && pos == l && tbeltfun(tb, u) == qval) {	\
	    pos = u;							\
	    cval = qval;						\
	}								\
    } while(0);


/* helper macro which assumes found and nmatch exist and determines
   the appropriate return value when passed pos/pos2 populated by
   BINARY_FIND */
#define TOINDEX(x) (curval == qval ? x + 1 : nmatch)

/* This always makes an integer vector, this path currently won't be hit
   if table is long vector (ie found indices need to return as doubles).
   Will fix in future patch */

/* NB: %in% calls match with an nmatch value of 0, so its not safe to
   set pos to nmatch and assume we will be able to detect that
   later to see if we found anything. Always check curval against qval */

#define BINARY_MATCHING_OUTER(TYPE, ALTPREFIX, fonly, nm, TBELTFUN,	\
			      QELTFUN, NACHK) do {			\
	int *retptr = NULL;						\
	if(fonly) {							\
	    PROTECT(ret = allocVector(INTSXP, XLENGTH(q))); nprot++;	\
	    retptr = INTEGER(ret);					\
	} else {							\
	    PROTECT(ret = allocVector(VECSXP, XLENGTH(q))); nprot++;	\
	}								\
	long int pos, pos2, u, l; /* long to prevent u+l int overflow */ \
	TYPE curval, qval, curval2;					\
	int tlen = LENGTH(table);					\
	for (R_xlen_t i = 0; i < XLENGTH(q); i++) {			\
	    qval = QELTFUN(q,i);					\
	    BINARY_FIND(table, qval,					\
			pos, curval, u, l, tlen - 1,			\
			0,						\
			tsorted,					\
			TRUE, TBELTFUN, NACHK);				\
	    if(curval != qval)	{					\
		if(nm == NA_INTEGER)					\
		    numNA++;						\
		/* TOINDEX takes care of making sure nm is returned */	\
	    }								\
	    if(fonly) {							\
		retptr[i] = TOINDEX((int) pos);				\
	    } else if (curval == qval) {				\
		BINARY_FIND(table, qval, pos2, curval2, u, l,		\
			    tlen - 1, pos,				\
			    tsorted,					\
			    FALSE, TBELTFUN, NACHK);			\
		SET_VECTOR_ELT(ret, i,					\
			       R_compact_intrange(TOINDEX((int) pos),	\
						  TOINDEX((int) pos2))); \
	    } else {							\
		SET_VECTOR_ELT(ret, i, ScalarInteger(TOINDEX((int) pos))); \
	    }								\
	}								\
    } while(0);



#define ALT_MATCH_DEFAULT(TYPE, SXPTYPE, ALTPREFIX, FIRSTONLY, ELTFUN,	\
			  QELTFUNTYPE, NACHK)				\
    do {								\
	if(XLENGTH(table) < 2)						\
	    return NULL;						\
	SEXP ret;							\
	int nprot = 0;							\
	int numNA = 0;							\
	int tsorted = ALTPREFIX##_IS_SORTED(table), qsorted;		\
	if(TYPEOF(q) == SXPTYPE &&					\
	   KNOWN_SORTED(tsorted) &&					\
	   /* no long vector support for table atm, coming soon */	\
	   XLENGTH(table) <= INT_MAX &&					\
	   (incomp == NULL || incomp == R_NilValue)) {			\
	    if(!ALTREP(q) || ALTREP_EXPANDED(q) != R_NilValue) {	\
		TYPE *qptr = ALTPREFIX(q);				\
		BINARY_MATCHING_OUTER(TYPE, ALTPREFIX, FIRSTONLY, nmatch, \
				      ELTFUN, fastgetqptr, NACHK);	\
	    } else {							\
		QELTFUNTYPE qeltmethod = DISPATCH_METHOD(ALT##ALTPREFIX, \
							 Elt, q);	\
		BINARY_MATCHING_OUTER(TYPE, ALTPREFIX, FIRSTONLY, nmatch, \
				      ELTFUN, qeltmethod, NACHK);	\
	    }								\
	    qsorted = ALTPREFIX##_IS_SORTED(q);				\
	    if(KNOWN_SORTED(qsorted)) {					\
		SEXP info = PROTECT(allocVector(INTSXP, NMETA)); nprot++; \
		INTEGER(info)[0] = qsorted * tsorted;			\
		INTEGER(info)[1] = numNA == 0;				\
		/* make a wrapper, sorted = qsorted*tsorted, na = numNA ==0 */ \
		ret = PROTECT(make_wrapper(ret, info)); nprot++;	\
	    }								\
	} else if(FIRSTONLY) {						\
	    UNPROTECT(nprot);						\
	    return NULL;	/* return to normal codepath */		\
	}  else { /*not sorted not first only */			\
	    error("no method for non-sorted non-firstonly matching");	\
	} /*end if(right type and sorted) */				\
	/*fprintf(stderr, "\nALT_MATCH_DEFAULT: 4, nprot: %d", nprot);*/ \
	UNPROTECT(nprot);						\
	return ret;							\
    } while(0); 


static SEXP altreal_Match_default(SEXP table, SEXP q,
				  int nmatch, SEXP incomp,
				  SEXP env,
				  Rboolean firstonly) {
    ALT_MATCH_DEFAULT(double, REALSXP, REAL, firstonly, REAL_ELT,
		      R_altreal_Elt_method_t, ISNA);
}

static SEXP altinteger_Match_default(SEXP table, SEXP q,
				     int nmatch, SEXP incomp,
				     SEXP env,
				     Rboolean firstonly) {
    ALT_MATCH_DEFAULT(int, INTSXP, INTEGER, firstonly, INTEGER_ELT,
		      R_altinteger_Elt_method_t, INTVAL_ISNA);
}


static SEXP altreal_Unique_default(SEXP x) {
    SEXP duped;
    PROTECT(duped= duplicated(x, FALSE));

    R_xlen_t k = 0, i, j = 0;
    /* this should be a sum call to take advantage of logical rles that might
       eventually come out of duplicated when x is sorted, or an Rle itself*/
    for(i = 0; i < XLENGTH(x); i++) {
	if(LOGICAL_ELT(duped, i) == 0)
	    k++;
    }
    SEXP ans;
    PROTECT(ans = allocVector(TYPEOF(x), k));
    for(i = 0; i < XLENGTH(x); i++) {
	if(LOGICAL_ELT(duped, i) == 0)
	    SET_REAL_ELT(ans, j++, REAL_ELT(x, i));
    }
    UNPROTECT(2);
    return ans;
}

static SEXP altinteger_Unique_default(SEXP x) {
    SEXP duped;
    PROTECT(duped= duplicated(x, FALSE));

    R_xlen_t k = 0, i, j = 0;
    /* this should be a sum call to take advantage of logical rles that might
       eventually come out of duplicated, or an Rle itself*/
    for(i = 0; i < XLENGTH(x); i++) {
	if(LOGICAL_ELT(duped, i) == 0)
	    k++;
    }
    SEXP ans;
    PROTECT(ans = allocVector(TYPEOF(x), k));
    for(i = 0; i < XLENGTH(x); i++) {
	if(LOGICAL_ELT(duped, i) == 0)
	    SET_INTEGER_ELT(ans, j++, INTEGER_ELT(x, i));
    }
    UNPROTECT(2);
    return ans;
}

static void altreal_Set_elt_default(SEXP x, R_xlen_t i, double v) {
    error("altreal classes must define a specific Set_elt method");
}

#ifdef DODO
static SEXP altreal_As_subscripts_default(SEXP x) {
    return x;
}
#endif

static double altreal_Compression_ratio_default(SEXP x) {
    error("altreal classes must define a specific Compression_Ratio method");
}


static SEXP altstring_Elt_default(SEXP x, R_xlen_t i)
{
    error("ALTSTRING classes must provide an Elt method");
}

static void altstring_Set_elt_default(SEXP x, R_xlen_t i, SEXP v)
{
    error("ALTSTRING classes must provide a Set_elt method");
}


static int altstring_Is_sorted_default(SEXP x) { return UNKNOWN_SORTEDNESS; }
static int altstring_No_NA_default(SEXP x) { return 0; }


/**
 ** ALTREP Initial Method Tables
 **/

static altinteger_methods_t altinteger_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altinteger_Elt_default,
    .Set_elt = altinteger_Set_elt_default,
    .Get_region = altinteger_Get_region_default,
    .Is_sorted = altinteger_Is_sorted_default,
    .No_NA = altinteger_No_NA_default,
    .Sort_check = altinteger_Sort_check_default,
    .Order = altinteger_Order_default,
    .Is_NA = altinteger_Is_NA_default,
    .Sum = altinteger_Sum_default,
    .Min = altinteger_Min_default,
    .Max = altinteger_Max_default,
    .Which_min = altinteger_Which_min_default,
    .Which_max = altinteger_Which_max_default,
    .Match = altinteger_Match_default,
    .Unique = altinteger_Unique_default,
    .Compression_ratio = altinteger_Compression_ratio_default
};

static altreal_methods_t altreal_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altreal_Elt_default,
    .Set_elt = altreal_Set_elt_default,
    .Get_region = altreal_Get_region_default,
    .Is_sorted = altreal_Is_sorted_default,
    .No_NA = altreal_No_NA_default,
    .Sort_check = altreal_Sort_check_default,
    .Order = altreal_Order_default,
    .Is_NA = altreal_Is_NA_default,
    .Sum = altreal_Sum_default,
    .Min = altreal_Min_default,
    .Max = altreal_Max_default,
    .Which_min = altreal_Which_min_default,
    .Which_max = altreal_Which_max_default,
    .Match = altreal_Match_default,
    .Unique = altreal_Unique_default,
    .Compression_ratio = altreal_Compression_ratio_default

};


static altstring_methods_t altstring_default_methods = {
    .UnserializeEX = altrep_UnserializeEX_default,
    .Unserialize = altrep_Unserialize_default,
    .Serialized_state = altrep_Serialized_state_default,
    .DuplicateEX = altrep_DuplicateEX_default,
    .Duplicate = altrep_Duplicate_default,
    .Coerce = altrep_Coerce_default,
    .Inspect = altrep_Inspect_default,
    .Length = altrep_Length_default,
    .Dataptr = altvec_Dataptr_default,
    .Dataptr_or_null = altvec_Dataptr_or_null_default,
    .Extract_subset = altvec_Extract_subset_default,
    .Elt = altstring_Elt_default,
    .Set_elt = altstring_Set_elt_default,
    .Is_sorted = altstring_Is_sorted_default,
    .No_NA = altstring_No_NA_default
};


/**
 ** Class Constructors
 **/

#define INIT_CLASS(cls, type) do {				\
	*((type##_methods_t *) (CLASS_METHODS_TABLE(cls))) =	\
	    type##_default_methods;				\
    } while (FALSE)

#define MAKE_CLASS(var, type) do {				\
	var = allocVector(RAWSXP, sizeof(type##_methods_t));	\
	R_PreserveObject(var);					\
	INIT_CLASS(var, type);					\
    } while (FALSE)

static R_INLINE R_altrep_class_t R_cast_altrep_class(SEXP x)
{
    /**** some king of optional check? */
    R_altrep_class_t val = R_SUBTYPE_INIT(x);
    return val;
}

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
#define DEFINE_CLASS_CONSTRUCTOR(cls, type)			\
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

DEFINE_METHOD_SETTER(altrep, UnserializeEX)
DEFINE_METHOD_SETTER(altrep, Unserialize)
DEFINE_METHOD_SETTER(altrep, Serialized_state)
DEFINE_METHOD_SETTER(altrep, DuplicateEX)
DEFINE_METHOD_SETTER(altrep, Duplicate)
DEFINE_METHOD_SETTER(altrep, Coerce)
DEFINE_METHOD_SETTER(altrep, Inspect)
DEFINE_METHOD_SETTER(altrep, Length)

DEFINE_METHOD_SETTER(altvec, Dataptr)
DEFINE_METHOD_SETTER(altvec, Dataptr_or_null)
DEFINE_METHOD_SETTER(altvec, Extract_subset)

DEFINE_METHOD_SETTER(altinteger, Elt)
DEFINE_METHOD_SETTER(altinteger, Get_region)
DEFINE_METHOD_SETTER(altinteger, Is_sorted)
DEFINE_METHOD_SETTER(altinteger, No_NA)
DEFINE_METHOD_SETTER(altinteger, Sort_check)
DEFINE_METHOD_SETTER(altinteger, Order)
DEFINE_METHOD_SETTER(altinteger, Is_NA)
DEFINE_METHOD_SETTER(altinteger, Sum)
DEFINE_METHOD_SETTER(altinteger, Min)
DEFINE_METHOD_SETTER(altinteger, Max)
DEFINE_METHOD_SETTER(altinteger, Which_min)
DEFINE_METHOD_SETTER(altinteger, Which_max)
DEFINE_METHOD_SETTER(altinteger, Match)
DEFINE_METHOD_SETTER(altinteger, Unique)
DEFINE_METHOD_SETTER(altinteger, As_subscripts)
DEFINE_METHOD_SETTER(altinteger, Compression_ratio)



DEFINE_METHOD_SETTER(altreal, Elt)
DEFINE_METHOD_SETTER(altreal, Get_region)
DEFINE_METHOD_SETTER(altreal, Is_sorted)
DEFINE_METHOD_SETTER(altreal, No_NA)
DEFINE_METHOD_SETTER(altreal, Sort_check)
DEFINE_METHOD_SETTER(altreal, Order)
DEFINE_METHOD_SETTER(altreal, Is_NA)
DEFINE_METHOD_SETTER(altreal, Sum)
DEFINE_METHOD_SETTER(altreal, Min)
DEFINE_METHOD_SETTER(altreal, Max)
DEFINE_METHOD_SETTER(altreal, Which_min)
DEFINE_METHOD_SETTER(altreal, Which_max)
DEFINE_METHOD_SETTER(altreal, Match)
DEFINE_METHOD_SETTER(altreal, Unique)
DEFINE_METHOD_SETTER(altstring, Elt)
DEFINE_METHOD_SETTER(altstring, Set_elt)
DEFINE_METHOD_SETTER(altstring, Is_sorted)
DEFINE_METHOD_SETTER(altstring, No_NA)
DEFINE_METHOD_SETTER(altreal, As_subscripts)
DEFINE_METHOD_SETTER(altreal, Compression_ratio)



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

//#define COMPACT_SEQ_INFO(x) R_altrep_data1(x)
//#define COMPACT_SEQ_EXPANDED(x) R_altrep_data2(x)
//#define SET_COMPACT_SEQ_EXPANDED(x, v) R_set_altrep_data2(x, v)
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
    if (ALTINTEGER_EXPANDED(x) != R_NilValue)
	return NULL;
#endif
    return ALTREP_INFO(x);
}

static SEXP new_compact_intseq(R_xlen_t, int, int);
static SEXP new_compact_realseq(R_xlen_t, double, double);

static SEXP compact_intseq_Unserialize(SEXP class, SEXP state)
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
    if ((type == INTSXP && ALTINTEGER_EXPANDED(x) != R_NilValue) ||
	(type == REALSXP && ALTREAL_EXPANDED(x) != R_NilValue))
	return NULL;
#endif
    if (ALTREP_EXPANDED(x) == R_NilValue && type == REALSXP) {
	SEXP info = ALTREP_INFO(x);
	R_xlen_t n = COMPACT_INTSEQ_INFO_LENGTH(info);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	SEXP ans = new_compact_realseq(n, n1, inc);
	SEXP attr = ATTRIB(x);
	if (attr != R_NilValue) {
	    PROTECT(ans);
	    SET_ATTRIB(ans, shallow_duplicate(attr));
	    SET_OBJECT(ans, OBJECT(x));
	    IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	    UNPROTECT(1);
	}
	return ans;
    }
    else return NULL;
}

static SEXP compact_intseq_Duplicate(SEXP x, Rboolean deep)
{
#ifdef BROKEN_WHEN_EXPANDED
    /* If this is worth doing, it is worth doing for the real case
       also.  But it needs to punt if the sequence is expanded and
       possibly modified. */
    if(!deep) {
	SEXP info = ALTREP_INFO(x);
	return new_compact_intseq(COMPACT_INTSEQ_INFO_LENGTH(info),
				  COMPACT_INTSEQ_INFO_FIRST(info),
				  COMPACT_INTSEQ_INFO_INCR(info));
    }
#endif
    R_xlen_t n = XLENGTH(x);
    SEXP val = allocVector(INTSXP, n);
    INTEGER_GET_REGION(x, 0, n, INTEGER0(val));
    return val;
}

static
Rboolean compact_intseq_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    int inc = COMPACT_INTSEQ_INFO_INCR(ALTREP_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %d not supported yet", inc);

#ifdef COMPACT_INTSEQ_MUTABLE
    if (ALTINTEGER_EXPANDED(x) != R_NilValue) {
	Rprintf("  <expanded compact integer sequence>\n");
	inspect_subtree(ALTREP_EXPANDED(x), pre, deep, pvec);
	return TRUE;
    }
#endif

    int n = LENGTH(x);
    int n1 = INTEGER_ELT(x, 0);
    int n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %d : %d (%s)", n1, n2,
	    ALTINTEGER_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_intseq_Length(SEXP x)
{
    SEXP info = ALTREP_INFO(x);
    return COMPACT_INTSEQ_INFO_LENGTH(info);
}

static void *compact_intseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (ALTINTEGER_EXPANDED(x) == R_NilValue) {
	/* no need to re-run if expanded data exists */
	PROTECT(x);
	SEXP info = ALTREP_INFO(x);
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

	
	SET_ALTINTEGER_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(ALTINTEGER_EXPANDED(x));
}

static const void *compact_intseq_Dataptr_or_null(SEXP x)
{
    SEXP val = ALTINTEGER_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static int compact_intseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = ALTREP_EXPANDED(x);
    if (ex != R_NilValue)
	return INTEGER0(ex)[i];
    else {
	SEXP info = ALTREP_INFO(x);
	int n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	return n1 + inc * i;
    }
}

static R_xlen_t
compact_intseq_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    /* should not get here if x is already expanded */
    CHECK_NOT_EXPANDED(sx);

    SEXP info = ALTREP_INFO(sx);
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
    if (ALTINTEGER_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    int inc = COMPACT_INTSEQ_INFO_INCR(ALTREP_INFO(x));
    return inc < 0 ? KNOWN_DECR : KNOWN_INCR;
}

static int compact_intseq_No_NA(SEXP x)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (ALTINTEGER_EXPANDED(x) != R_NilValue)
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
    if (ALTINTEGER_EXPANDED(x) != R_NilValue) 
	return NULL;
    else {
#endif
    double tmp;
	SEXP info = ALTREP_INFO(x);
	R_xlen_t size = COMPACT_INTSEQ_INFO_LENGTH(info);
	R_xlen_t n1 = COMPACT_INTSEQ_INFO_FIRST(info);
	int inc = COMPACT_INTSEQ_INFO_INCR(info);
	tmp = (size / 2.0) * (n1 + n1 + inc * (size - 1));
	if(tmp > INT_MAX || tmp < R_INT_MIN)
	    /**** check for overfolw of exact integer range? */
	    return ScalarReal(tmp);
	else
	    return ScalarInteger((int) tmp);
#ifdef COMPACT_INTSEQ_MUTABLE
    }
#endif
}

static SEXP compact_intseq_Match(SEXP table, SEXP x, int nm, SEXP incomp,
				 SEXP env, Rboolean firstonly)
{
    /* we could be more forgiving here */
    if(TYPEOF(table) != INTSXP || TYPEOF(x) != INTSXP ||
       XLENGTH(x) == 0 || XLENGTH(table) == 0)
	return NULL;

    SEXP ans, info = ALTREP_INFO(table);
    int inc = COMPACT_INTSEQ_INFO_INCR(info);
    int nt = COMPACT_INTSEQ_INFO_LENGTH(info);
    int minval, maxval;
    R_xlen_t nx = XLENGTH(x);
    if(inc == 1) { 
	minval = COMPACT_INTSEQ_INFO_FIRST(info);
	maxval = minval + nt - 1;
    } else if(inc == -1) {
	maxval = COMPACT_INTSEQ_INFO_FIRST(info);
	minval = maxval - (nt - 1);
    } else
	error("compact sequences with increment %d not supported yet", inc);
    int qval;
    
    if(firstonly) {
	PROTECT(ans = allocVector(INTSXP, nx));
	if(inc == 1) {
	    for(R_xlen_t i = 0; i < nx; i++) {
		qval = INTEGER_ELT(x, i);
		if(qval <= maxval && qval >= minval)
		    SET_INTEGER_ELT(ans, i, qval - minval + 1);
		else
		    SET_INTEGER_ELT(ans, i, nm);
	    }
	} else if( inc == -1) {
	    for(R_xlen_t i = 0; i < nx; i++) {
		qval = INTEGER_ELT(x, i);
		if(qval <= maxval && qval >= minval)
		    SET_INTEGER_ELT(ans, i, maxval - qval + 1);
		else
		    SET_INTEGER_ELT(ans, i, nm);
	    }
	}
    } else { /* !firstonly */
	PROTECT(ans = allocVector(VECSXP, nx));
	if(inc ==1 ) {
	    for(R_xlen_t i = 0; i < nx; i++) {
		qval = INTEGER_ELT(x, i);
		if(qval <= maxval && qval >= minval)
		    SET_VECTOR_ELT(ans, i, ScalarInteger(qval - minval + 1));
		else
		    SET_VECTOR_ELT(ans, i, ScalarInteger(nm));
	    }
	} else if( inc == -1) {
	    for(R_xlen_t i = 0; i < nx; i++) {
		qval = INTEGER_ELT(x, i);
		if(qval <= maxval && qval >= minval)
		    SET_VECTOR_ELT(ans, i, ScalarInteger(maxval - qval  +1));
		else
		    SET_VECTOR_ELT(ans, i, ScalarInteger(nm));
	    }
	}
    } /* end if(firstonly) */
    UNPROTECT(1); /* ans, in both branches */
    return ans;
}


#ifdef DODO
static double compact_intseq_Compression_ratio(SEXP x) {
    double ret;
    if(ALTREP_EXPANDED(x) != R_NilValue)
	ret = 1.0;
    else {
	SEXP info = ALTREP_INFO(x);
	ret = (double) COMPACT_INTSEQ_INFO_LENGTH(info) / 2.0;
    }

    return ret;
}
#endif



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
    R_set_altinteger_Match_method(cls, compact_intseq_Match);
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
    INTEGER(info)[0] = (int) n;
    INTEGER(info)[1] = n1;
    INTEGER(info)[2] = inc;

    SEXP ans = R_new_altrep(R_compact_intseq_class, info, R_NilValue);
#ifndef COMPACT_INTSEQ_MUTABLE
    MARK_NOT_MUTABLE(ans); /* force duplicate on modify */
#endif

    return ans;
}





/* 
 * Virtually rep'ed integer vector
 */


/* This will be a list with the parent vector and the length  */
//#define VIRTREP_INFO(x) R_altrep_data1(x)
//#define VIRTREP_EXPANDED(x) R_altrep_data2(x)
//#define SET_VIRTREP_EXPANDED(x, v) R_set_altrep_data2(x, v)
#define VIRTREP_LENGTH(info) INTEGER_ELT( VECTOR_ELT(info, 1), 0)
#define VIRTREP_PARENT(info) VECTOR_ELT(info, 0)
#define VIRTREP_PARENT_LENGTH(info) LENGTH( VIRTREP_PARENT(info) )

/* len is *total length*, i.e length.out or times*length(parent) */
static SEXP new_virtrep_intvec(SEXP parent, R_xlen_t len);

SEXP attribute_hidden R_virtrep_vec(SEXP parent, SEXP Rlen) {
    fprintf(stderr, "I'm in R_virtrep_vec\n");
    R_xlen_t plen = LENGTH(parent), len;
    switch(TYPEOF(Rlen)) { 
    case INTSXP:
	len = (R_xlen_t) INTEGER_ELT(Rlen, 0);
	break;
    case REALSXP:
	len = (R_xlen_t) REAL_ELT(Rlen, 0);
	break;
    default:
	error("length of type %s not supported",
	      TYPEOF(Rlen));
    };

    if(len == plen)
	return parent;

    if(len < plen)
	error("virtual rep'ed length must be longer than original vector");
    SEXP ans;
    switch(TYPEOF(parent)) {
    case INTSXP:
	ans = new_virtrep_intvec(parent, len);
	break;
    default:
	error("Virtually repping vectors of type %s not yet supported",
	      TYPEOF(parent));
    };
    MARK_NOT_MUTABLE(parent);

    return ans;
}

static SEXP virtrep_intvec_Serialized_state(SEXP x) {
    return ALTREP_INFO(x);
}

static SEXP virtrep_intvec_Unserialize(SEXP class, SEXP state)
{
    R_xlen_t len;
    SEXP Rlen = VECTOR_ELT(state, 1);
    SEXP parent = VECTOR_ELT(state, 0);
    SEXP ans;
    switch(TYPEOF(Rlen)) {
    case INTSXP:
	len = (R_xlen_t) INTEGER_ELT(Rlen, 0);
	break;
    case REALSXP:
	len = (R_xlen_t) REAL_ELT(Rlen, 0);
	break;
    default:
	error("Rep'ed length of type %s not supported",
	      TYPEOF(Rlen));
    }

    switch(TYPEOF(parent)) {
    case INTSXP:
	ans = new_virtrep_intvec(parent, len);
	break;
    default:
	error("Virtually rep'ed vectors of type %s not yet supported",
	      TYPEOF(parent));
    }
    return ans;
}

static SEXP virtrep_intvec_Coerce(SEXP s, int type)
{
    if(ALTINTEGER_EXPANDED(s) != R_NilValue)
	return NULL;
    if(type == REALSXP) {
	SEXP newparent;
	SEXP info = ALTREP_INFO(s);
	SEXP parent = VIRTREP_PARENT(info);
	R_xlen_t len = VIRTREP_LENGTH(s);
	SEXP ans;
	PROTECT(newparent = coerceVector(parent, type));
	ans = R_virtrep_vec(newparent, ScalarReal(len));
	UNPROTECT(1);
	return ans;
    } else {
	return NULL;
    }
}

/* I think we can just use compact_intseq_Duplicate 
   provided we write get_region correctly ... */
//static SEXP virtrep_intvec_Duplicate(SEXP x, Rboolean deep)

static
Rboolean virtrep_intvec_Inspect(SEXP x, int pre, int deep, int pvec,
				void (*inspect_subtree)(SEXP, int, int, int))
{
    if (ALTINTEGER_EXPANDED(x) != R_NilValue) {
	Rprintf("  <expanded virtually rep'ed vector>\n");
	inspect_subtree(ALTINTEGER_EXPANDED(x), pre, deep, pvec);
	return TRUE;
    }

    SEXP info = ALTREP_INFO(x);
    R_xlen_t n = XLENGTH(x);
    Rprintf(" length %d rep'ed to length %d (%s)", VIRTREP_PARENT_LENGTH(info),
	    n,
	    ALTINTEGER_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t virtrep_intvec_Length(SEXP x)
{
    SEXP info = ALTREP_INFO(x);
    return VIRTREP_LENGTH(info);
    
}

static void *virtrep_intvec_Dataptr(SEXP x, Rboolean writeable)
{
    if (ALTINTEGER_EXPANDED(x) == R_NilValue) {
	/* no need to re-run if expended data exists */
	PROTECT(x);
	SEXP info = ALTREP_INFO(x);
	SEXP parent = VIRTREP_PARENT(info);
	R_xlen_t len = VIRTREP_LENGTH(info);
	R_xlen_t plen = VIRTREP_PARENT_LENGTH(info);
	SEXP val = allocVector(INTSXP, len);
	int *data = INTEGER(val);
	int *parentdata = INTEGER(parent);
	
	for(R_xlen_t i = 0; i < len; i++)
	    data[i] = parentdata[i % plen];
	
	SET_ALTINTEGER_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(ALTINTEGER_EXPANDED(x));
    
}

static const void *virtrep_intvec_Dataptr_or_null(SEXP x)
{
    SEXP val = ALTINTEGER_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}



static int virtrep_intvec_Elt(SEXP x, R_xlen_t i)
{
    CHECK_NOT_EXPANDED(x);

    SEXP info = ALTREP_INFO(x);
    SEXP parent = VIRTREP_PARENT(info);
    R_xlen_t len = VIRTREP_LENGTH(info),
	plen = VIRTREP_PARENT_LENGTH(info);

    // do we need this check or does it already occur upstream?
    if(i > len)
	error("i greater than total length in Elt call");

    if(i > plen)
	i = i % plen;
    return INTEGER_ELT(parent, i);
}

static R_xlen_t
virtrep_intvec_Get_region(SEXP sx, R_xlen_t i, R_xlen_t n, int *buf)
{
    CHECK_NOT_EXPANDED(sx);

    SEXP info = ALTREP_INFO(sx);
    SEXP parent = VIRTREP_PARENT(info);
    R_xlen_t len = VIRTREP_LENGTH(info),
	plen = VIRTREP_PARENT_LENGTH(info);

    
    R_xlen_t ncopy = len - i > n ? n : len - i;


    R_xlen_t ppos = i % plen;
    R_xlen_t k = 0;
    MOD_ITERATE1_CORE(ncopy, plen, k, ppos,
		      buf[k] = INTEGER_ELT(parent, ppos););
    
    return ncopy;
}

static int virtrep_intvec_Is_sorted(SEXP x)
{
    if(ALTINTEGER_EXPANDED(x) != R_NilValue)
	return 0;
    
    SEXP info = ALTREP_INFO(x);
    R_xlen_t plen = VIRTREP_PARENT_LENGTH(info);

    /* we could be cleverer here...
       ie check if only one unique value...*/
    return plen == 1 ? KNOWN_INCR : UNKNOWN_SORTEDNESS;
}

static int virtrep_intvec_No_NA(SEXP x)
{
    if(ALTINTEGER_EXPANDED(x) != R_NilValue)
	return 0;

    SEXP info = ALTREP_INFO(x);
    SEXP parent = VIRTREP_PARENT(info);
    return INTEGER_NO_NA(parent);
}

static SEXP virtrep_intvec_Sum(SEXP x, Rboolean narm)
{
    return NULL;
#ifdef DODO
    int ret;
    if(ALTINTEGER_EXPANDED(x) != R_NilValue)
	return NULL;
    
    SEXP info = ALTREP_INFO(x);
    SEXP parent = VIRTREP_PARENT(info);

    double tmp;
    int remainder;
    if(ALTREP(parent))
	ret = ALTINTEGER_SUM(parent, narm);
    else
	isum(parent, &ret, narm, R_NilValue);
    /* if summing the parent gave us NA, no reason to do anything more */
    if(INTVAL_ISNA(ret))
	return NA_INTEGER;
    
    int numreps = floor(VIRTREP_LENGTH(info) / VIRTREP_PARENT_LENGTH(info));
    tmp = (double) ret * numreps;
    remainder = VIRTREP_LENGTH(info) -  VIRTREP_PARENT_LENGTH(info) * numreps; 

    /* I'm not checking at every step here for overflow, so
       technically this can go beyond the limits then come
       back within them and not trigger that. It would get the
       right behavior but not have identical behavior in that
       corner case. */
    for(int i = 0; i < remainder; i++)
	tmp = tmp + INTEGER_ELT(parent, i);

    if(tmp > INT_MAX || tmp < R_INT_MIN) {
	warning("Integer overflow - use sum(as.numeric(.))");
	ret = NA_INTEGER;
    } else {
	ret = (int) tmp;
    }
    
    return ret;
#endif
}




/*
 * Class Objects and Method Tables
 */

R_altrep_class_t R_virtrep_intvec_class;

static void InitVirtRepIntegerClass()
{
    R_altrep_class_t cls = R_make_altinteger_class("virtrep_intvec", "base",
						   NULL);
    R_virtrep_intvec_class = cls;

    /* override ALTREP methods */
    R_set_altrep_Unserialize_method(cls, virtrep_intvec_Unserialize);
    R_set_altrep_Serialized_state_method(cls, virtrep_intvec_Serialized_state);
    R_set_altrep_Duplicate_method(cls, compact_intseq_Duplicate);
    R_set_altrep_Coerce_method(cls, virtrep_intvec_Coerce);
    R_set_altrep_Inspect_method(cls, virtrep_intvec_Inspect);
    R_set_altrep_Length_method(cls, virtrep_intvec_Length);

    /* override ALTVEC methods */
    R_set_altvec_Dataptr_method(cls, virtrep_intvec_Dataptr);
    R_set_altvec_Dataptr_or_null_method(cls, virtrep_intvec_Dataptr_or_null);

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, virtrep_intvec_Elt);
    R_set_altinteger_Get_region_method(cls, virtrep_intvec_Get_region);
    R_set_altinteger_Is_sorted_method(cls, virtrep_intvec_Is_sorted);
    R_set_altinteger_No_NA_method(cls, virtrep_intvec_No_NA);
    R_set_altinteger_Sum_method(cls, virtrep_intvec_Sum);
}


/*
 * Constructor
 */


static SEXP new_virtrep_intvec(SEXP parent, R_xlen_t len)
{
    R_xlen_t plen = LENGTH(parent);
    if(len == plen)
	return parent;

    if(len < plen)
	error("length of virtual repeated vector must be longer than vector being repeated");

    SEXP info;
    PROTECT(info = allocVector(VECSXP, 2));

    SET_VECTOR_ELT(info, 0, parent);
    //XXX TODO this should be ScalarReal because R_xlen_t is bigger than int, right?
    //XXX If so, need to change VIRTREP_LENGTH acessor too
    SET_VECTOR_ELT(info, 1, ScalarInteger(len));
    
    SEXP ans = R_new_altrep(R_virtrep_intvec_class, info, R_NilValue);
    UNPROTECT(1);
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
    return ALTREP_INFO(x);
}

static SEXP compact_realseq_Unserialize(SEXP class, SEXP state)
{
    double inc = COMPACT_REALSEQ_INFO_INCR(state);
    R_xlen_t len = (R_xlen_t) REAL_ELT(state, 0);
    double n1 = REAL_ELT(state, 1);

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
    double inc = COMPACT_REALSEQ_INFO_INCR(ALTREP_INFO(x));
    if (inc != 1 && inc != -1)
	error("compact sequences with increment %f not supported yet", inc);

    R_xlen_t n = XLENGTH(x);
    R_xlen_t n1 = REAL_ELT(x, 0);
    R_xlen_t n2 = inc == 1 ? n1 + n - 1 : n1 - n + 1;
    Rprintf(" %ld : %ld (%s)", n1, n2,
	    ALTREP_EXPANDED(x) == R_NilValue ? "compact" : "expanded");
    Rprintf("\n");
    return TRUE;
}

static R_INLINE R_xlen_t compact_realseq_Length(SEXP x)
{
    return (R_xlen_t) REAL0(ALTREP_INFO(x))[0];
}

static void *compact_realseq_Dataptr(SEXP x, Rboolean writeable)
{
    if (ALTREAL_EXPANDED(x) == R_NilValue) {
	PROTECT(x);
	SEXP info = ALTREP_INFO(x);
	double n = COMPACT_REALSEQ_INFO_LENGTH(info);
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

	SET_ALTREAL_EXPANDED(x, val);
	UNPROTECT(1);
    }
    return DATAPTR(ALTREP_EXPANDED(x));
}

static const void *compact_realseq_Dataptr_or_null(SEXP x)
{
    SEXP val = ALTREAL_EXPANDED(x);
    return val == R_NilValue ? NULL : DATAPTR(val);
}

static double compact_realseq_Elt(SEXP x, R_xlen_t i)
{
    SEXP ex = ALTREP_EXPANDED(x);
    if (ex != R_NilValue)
	return REAL0(ex)[i];
    else {
	SEXP info = ALTREP_INFO(x);
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

    SEXP info = ALTREP_INFO(sx);
    R_xlen_t size = (R_xlen_t) COMPACT_REALSEQ_INFO_LENGTH(info);
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
    if (ALTREAL_EXPANDED(x) != R_NilValue)
	return UNKNOWN_SORTEDNESS;
#endif
    double inc = COMPACT_REALSEQ_INFO_INCR(ALTREP_INFO(x));
    return inc < 0 ? KNOWN_DECR : KNOWN_INCR;
}

static int compact_realseq_No_NA(SEXP x)
{
#ifdef COMPACT_REALSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (ALTREAL_EXPANDED(x) != R_NilValue)
	return FALSE;
#endif
    return TRUE;
}

static SEXP compact_realseq_Sum(SEXP x, Rboolean narm)
{
#ifdef COMPACT_INTSEQ_MUTABLE
    /* If the vector has been expanded it may have been modified. */
    if (ALTREAL_EXPANDED(x) != R_NilValue) 
	return NULL;
    else {
#endif
	SEXP info = ALTREP_INFO(x);
	double size = COMPACT_REALSEQ_INFO_LENGTH(info);
	double n1 = COMPACT_REALSEQ_INFO_FIRST(info);
	double inc = COMPACT_REALSEQ_INFO_INCR(info);
	return ScalarReal((size / 2.0) *(n1 + n1 + inc * (size - 1)));
#ifdef COMPACT_INTSEQ_MUTABLE
    }
#endif
}


static double compact_realseq_Compression_ratio(SEXP x) {
    double ret;
    if(ALTREP_EXPANDED(x) != R_NilValue)
	ret = 1.0;
    else {
	SEXP info = ALTREP_INFO(x);
	ret = (double) COMPACT_REALSEQ_INFO_LENGTH(info) / 2.0;
    }

    return ret;
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
    R_set_altreal_Compression_ratio_method(cls, compact_realseq_Compression_ratio);
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
	return new_compact_intseq(n, (int) n1, n1 <= n2 ? 1 : -1);
}


#ifdef DODO
/**** needs revsions for integer sum changes */
/**
 ** In-place subsetted vectors
 **
 ** NB: permutations including rev'ing and sorting are just a special case of subsetting!
 **/

/*
 * Methods
 */


//#define SUBSETTED_VEC_INFO(x) R_altrep_data1(x)
#define SUBSETTED_VEC_EXPANDED(x) R_altrep_data2(x)
#define SET_SUBSETTED_VEC_EXPANDED(x, v) R_set_altrep_data2(x, v)

#define SUBSETTED_VEC_PARENT(info) VECTOR_ELT(info, 0)
#define SUBSETTED_VEC_INDICES(info) VECTOR_ELT(info, 1)
#define SUBSETTED_VEC_SORTED(info) INTEGER_ELT(VECTOR_ELT(info, 2), 0)
#define SUBSETTED_VEC_NO_NA(info) INTEGER_ELT(VECTOR_ELT(info, 3), 0)

#define DECLARE_SUBSETTED_ALTCLASS(type, ALTTYPE, sumfun, SXPTYPE, funprefix, \
				   initname, clsname, ltype)		\
    static SEXP new_##funprefix(SEXP parent, SEXP indices,		\
				int sorted, int  no_NA);		\
    									\
    static SEXP funprefix##_Serialized_state(SEXP x)			\
    {									\
	return ALTREP_INFO(x);						\
    }									\
    									\
    static SEXP funprefix##_Unserialize(SEXP class, SEXP state)		\
    {									\
	return new_##funprefix(SUBSETTED_VEC_PARENT(state),		\
			       SUBSETTED_VEC_INDICES(state),		\
			       SUBSETTED_VEC_SORTED(state),		\
			       SUBSETTED_VEC_NO_NA(state));		\
    }									\
    									\
    static SEXP funprefix##_Duplicate(SEXP x, Rboolean deep)		\
    {									\
	R_xlen_t n = XLENGTH(x);					\
	SEXP val = allocVector(SXPTYPE, n);				\
	ALTTYPE##_GET_REGION(x, 0, n, ALTTYPE##0(val));			\
	return val;							\
    }									\
    									\
    static								\
    Rboolean funprefix##_Inspect(SEXP x, int pre, int deep, int pvec,	\
				 void (*inspect_subtree)(SEXP, int, int, int)) \
    {									\
	R_xlen_t n1, n2;						\
	n1 = XLENGTH(x);						\
	SEXP info = ALTREP_INFO(x);					\
	SEXP inds = SUBSETTED_VEC_INDICES(info);			\
	if(TYPEOF(inds) == LGLSXP)					\
	    error("Deferred logical subsetting not yet supported. Soon!"); \
	n2 = XLENGTH(inds);						\
									\
	Rprintf(" virtually subset vector %d of %d (%s)", n1, n2,	\
		SUBSETTED_VEC_EXPANDED(x) == R_NilValue ? "compact" : "expanded"); \
	Rprintf("\n");							\
	return TRUE;							\
    }									\
    									\
    static R_INLINE R_xlen_t funprefix##_Length(SEXP x)			\
    {									\
	SEXP info = ALTREP_INFO(x);					\
	return XLENGTH(SUBSETTED_VEC_INDICES(info));			\
    }									\
    									\
    static void *funprefix##_Dataptr(SEXP x, Rboolean writeable)	\
    {									\
	if (ALTREP_EXPANDED(x) == R_NilValue) {				\
	    PROTECT(x);							\
	    double n = funprefix##_Length(x);				\
	    								\
	    SEXP val = allocVector(SXPTYPE, n);				\
	    type *data = ALTTYPE##0(val);				\
	    ALTTYPE##_GET_REGION(x, 0, n, data);			\
	    								\
	    SET_ALT##ALTTYPE##_EXPANDED(x, val);			\
	    UNPROTECT(1);						\
	}								\
	return DATAPTR(ALTREP_EXPANDED(x));				\
    }									\
    									\
    static const void *funprefix##_Dataptr_or_null(SEXP x)		\
    {									\
	SEXP val = ALTREP_EXPANDED(x);					\
	return val == R_NilValue ? NULL : DATAPTR(val);			\
    }									\
    									\
    static type funprefix##_Elt(SEXP x, R_xlen_t i)			\
    {									\
	/* should not get here if x is already expanded */		\
	CHECK_NOT_EXPANDED(x);						\
									\
	SEXP info = ALTREP_INFO(x);					\
	SEXP parent = SUBSETTED_VEC_PARENT(info);			\
	SEXP indices = SUBSETTED_VEC_INDICES(info);			\
	R_xlen_t i2;							\
	if(TYPEOF(indices) == REALSXP)					\
	    i2 = (R_xlen_t) REAL_ELT(indices, i);			\
	else								\
	    i2 = (R_xlen_t) INTEGER_ELT(indices, i);			\
	return ALTTYPE##_ELT(parent, i2);				\
									\
    }									\
    									\
/* we will use Get_region_default methods, redirction mapping		\
   already implemented in Elt methods which Get_region defaults		\
   are based on */							\
									\
    static int funprefix##_Is_sorted(SEXP x)				\
    {									\
	/* If the vector has been expanded it may have been modified. */ \
	if (ALTREP_EXPANDED(x) != R_NilValue)				\
	    return UNKNOWN_SORTEDNESS;					\
	SEXP info = ALTREP_INFO(x);					\
	return SUBSETTED_VEC_SORTED(info);				\
    }									\
									\
    static int funprefix##_No_NA(SEXP x)				\
    {									\
	/* If the vector has been expanded it may have been modified. */ \
	if (ALTREP_EXPANDED(x) != R_NilValue)				\
	    return FALSE;						\
	/* this can be smarter but for now it will do */		\
	SEXP info = ALTREP_INFO(x);					\
	SEXP parent = SUBSETTED_VEC_PARENT(info);			\
	return SUBSETTED_VEC_NO_NA(parent);				\
									\
    }									\
    									\
    static SEXP funprefix##_Sum(SEXP x, Rboolean narm)			\
    {									\
	/* If the vector has been expanded it may have been modified. */ \
	if (ALTREP_EXPANDED(x) != R_NilValue)				\
	    return NULL;						\
	else {								\
	    SEXP info = ALTREP_INFO(x);					\
	    SEXP parent = SUBSETTED_VEC_PARENT(info);			\
	    if(ALTREP(parent))						\
		return NULL;						\
	    else							\
		return sumfun(parent, narm, R_NilValue);		\
	}								\
    }									\
									\
/*									\
 * Class Objects and Method Tables					\
 */									\
									\
    R_altrep_class_t R_##funprefix##_class;				\
    									\
static void Init##initname##Class()					\
{									\
    R_altrep_class_t cls = R_make_alt##ltype##_class(clsname, "base",	\
						     NULL);		\
    R_##funprefix##_class = cls;					\
    									\
    /* override ALTREP methods */					\
    R_set_altrep_Unserialize_method(cls, funprefix##_Unserialize);	\
    R_set_altrep_Serialized_state_method(cls, funprefix##_Serialized_state); \
    R_set_altrep_Duplicate_method(cls, funprefix##_Duplicate);		\
    R_set_altrep_Inspect_method(cls, funprefix##_Inspect);		\
    R_set_altrep_Length_method(cls, funprefix##_Length);		\
    									\
    /* override ALTVEC methods */					\
    R_set_altvec_Dataptr_method(cls, funprefix##_Dataptr);		\
    R_set_altvec_Dataptr_or_null_method(cls, funprefix##_Dataptr_or_null); \
    									\
    /* override ALTREAL methods */					\
    R_set_alt##ltype##_Elt_method(cls, funprefix##_Elt);		\
    R_set_alt##ltype##_Is_sorted_method(cls, funprefix##_Is_sorted);	\
    R_set_alt##ltype##_No_NA_method(cls, funprefix##_No_NA);		\
    R_set_alt##ltype##_Sum_method(cls, funprefix##_Sum);		\
}									\
									\
									\
/*									\
 * Constructor								\
 */									\
									\
static SEXP new_##funprefix(SEXP parent, SEXP indices,			\
				  int sorted, int  no_NA)		\
{									\
    SEXP info = allocVector(VECSXP, 4);					\
    SET_VECTOR_ELT(info, 0, parent);					\
    SET_VECTOR_ELT(info, 1, indices);					\
    SET_VECTOR_ELT(info, 2, ScalarInteger(sorted));			\
    SET_VECTOR_ELT(info, 3, ScalarInteger(no_NA));			\
    									\
    SEXP ans = R_new_altrep(R_##funprefix##_class, info, R_NilValue);	\
    MARK_NOT_MUTABLE(parent); /* force duplicate on modify */		\
									\
    return ans;								\
									\
}									\


DECLARE_SUBSETTED_ALTCLASS(int, INTEGER, isum, INTSXP, subsetted_intvec,
			       SubsettedInteger, "subsetted_intvec",
			       integer)
static Rboolean rsumWrapper(SEXP sx, double *value, Rboolean narm, SEXP call) {
    return rsum(sx, value, narm);
}
DECLARE_SUBSETTED_ALTCLASS(double, REAL, rsumWrapper, REALSXP,
			   subsetted_realvec, SubsettedReal,
			   "subsetted_realvec",
			   real)
#endif


/**
 ** Run-length encoded (RLE) vectors
 **/


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

static SEXP deferred_string_Unserialize(SEXP class, SEXP state)
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

//static R_INLINE void expand_deferred_string(SEXP x)
static void expand_deferred_string(SEXP x)
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

SEXP attribute_hidden R_deferred_coerceToString(SEXP v, SEXP sp)
{
    SEXP ans = R_NilValue;
    switch (TYPEOF(v)) {
    case INTSXP:
    case REALSXP:
	if (sp == NULL)
	    sp = ScalarInteger(R_print.scipen);
	MARK_NOT_MUTABLE(v); /* make sure it can't change once captured */
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



static SEXP wrapper_integer_Match(SEXP table, SEXP q,
					    int nmatch, SEXP incomp,
					    SEXP env,
					    Rboolean firstonly) {
    //  fprintf(stderr, "Hit altinteger_Match_default");
    if (!ALTREP(WRAPPER_WRAPPED(table))) {
	int *ptr = INTEGER(WRAPPER_WRAPPED(table));
	ALT_MATCH_DEFAULT(int, INTSXP, INTEGER, firstonly, fastgetptr,
			  R_altinteger_Elt_method_t, INTVAL_ISNA);
    } else {
	ALT_MATCH_DEFAULT(int, INTSXP, INTEGER, firstonly, INTEGER_ELT,
			  R_altinteger_Elt_method_t, INTVAL_ISNA);
    }
    //fprintf(stderr, "Leaving altintger_Match_default");
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


static SEXP wrapper_real_Match(SEXP table, SEXP q,
					    int nmatch, SEXP incomp,
					    SEXP env,
					    Rboolean firstonly) {
    //  fprintf(stderr, "Hit altinteger_Match_default");
    if(!ALTREP(WRAPPER_WRAPPED(table))) {
	double *ptr = REAL(WRAPPER_WRAPPED(table));
	ALT_MATCH_DEFAULT(double, REALSXP, REAL, firstonly, fastgetptr,
			  R_altreal_Elt_method_t, ISNA);
    } else {
	ALT_MATCH_DEFAULT(double, REALSXP, REAL, firstonly, REAL_ELT,
			  R_altreal_Elt_method_t, ISNA);
    }
    //fprintf(stderr, "Leaving altintger_Match_default");
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
    /**** should think about overriding Extract_subset at least if
	  there are no attributes */

    /* override ALTINTEGER methods */
    R_set_altinteger_Elt_method(cls, wrapper_integer_Elt);
    R_set_altinteger_Get_region_method(cls, wrapper_integer_Get_region);
    R_set_altinteger_Is_sorted_method(cls, wrapper_integer_Is_sorted);
    R_set_altinteger_No_NA_method(cls, wrapper_integer_no_NA);
    R_set_altinteger_Match_method(cls, wrapper_integer_Match);
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
    /**** should think about overriding Extract_subset at least if
	  there are no attributes */

    /* override ALTREAL methods */
    R_set_altreal_Elt_method(cls, wrapper_real_Elt);
    R_set_altreal_Get_region_method(cls, wrapper_real_Get_region);
    R_set_altreal_Is_sorted_method(cls, wrapper_real_Is_sorted);
    R_set_altreal_No_NA_method(cls, wrapper_real_no_NA);
    R_set_altreal_Match_method(cls, wrapper_real_Match);
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

SEXP make_wrapper(SEXP x, SEXP meta)
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

#ifndef SWITCH_TO_REFCNT
    if (MAYBE_REFERENCED(x))
	/* make sure no mutation can happen through another reference */
	MARK_NOT_MUTABLE(x);
#endif
    
    return ans;
}

SEXP attribute_hidden do_wrap_meta(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x = CAR(args);
    int nprot = 0;
    switch(TYPEOF(x)) {
    case INTSXP:
    case REALSXP:
    case STRSXP: break;
    default:  return x; //error("only INTSXP, REALSXP, STRSXP vectors suppoted for now");
    }
    
    int srt = asInteger(CADR(args));
    if (srt != KNOWN_INCR && srt != KNOWN_DECR && srt != KNOWN_UNSORTED &&
	srt != UNKNOWN_SORTEDNESS)
	error("srt must be -1, 0, or +1, or NA");
    
    int no_na = asInteger(CADDR(args));
    if (no_na < 0 || no_na > 1)
	error("no_na must be 0 or +1");

    SEXP meta = allocVector(INTSXP, NMETA);
    INTEGER(meta)[0] = srt;
    INTEGER(meta)[1] = no_na;
    SEXP ans = PROTECT(make_wrapper(x, meta));nprot++;
    if (ATTRIB(x) != R_NilValue) {
	/* For objects without references we could move the attributes
	   to the wrapper. For objects with references the attributes
	   would have to be shallow duplicated at least. The object/S4
	   bits would need to be moved as well.	*/

	/*experimental implementation of what's described above */
	SEXP attr = PROTECT(ATTRIB(x)); nprot++;
	if(NAMED(x) <= 1) {
	    SET_ATTRIB(ans, attr);
	    SET_ATTRIB(x, R_NilValue);
	} else {
	    /* what should happen to the attributes on x??? 
	     do we need to duplicate x so we can clear them? */
	    PROTECT(attr = shallow_duplicate(attr));nprot++;
	    SET_ATTRIB(ans, attr); 
	}
	SET_OBJECT(ans, OBJECT(x));
	IS_S4_OBJECT(x) ? SET_S4_OBJECT(ans) : UNSET_S4_OBJECT(ans);
	//error("only vectors without attributes are supported for now");
    }
    UNPROTECT(nprot);
    return ans;
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
    InitVirtRepIntegerClass();
#ifdef DODO
    InitSubsettedRealClass();
    InitSubsettedIntegerClass();
#endif
}

/* which.min, which.max, prod?? */

/*
  m*log2(n) = m + 20n
  m*(log2(n) - 1) = 20n
  m*(log2(n) - 1) / 20 = n
  (2^log2(n))^m = 2^(m)*2^(20n)
  n^m = 2^m * 2^20n
  (n/2)^m = 2^20n
  m log2 (n/2)
*/

/* m=n case
   n * log2 n = 21n
   n(log2(n) - 21) = 0
   log2(n) = 21
   n = 2^21 */
