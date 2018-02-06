/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2016  The R Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifndef R_EXT_ALTREP_H_
#define R_EXT_ALTREP_H_

#define STRUCT_SUBTYPES
#ifdef STRUCT_SUBTYPES
# define R_SEXP(x) (x).ptr
# define R_SUBTYPE_INIT(x) { x }
  typedef struct { SEXP ptr; } R_altrep_class_t;
#else
# define R_SEXP(x) ((SEXP) (x))
# define R_SUBTYPE_INIT(x) (void *) (x)
  typedef struct R_altcls *R_altrep_class_t;
#endif

SEXP
R_new_altrep(R_altrep_class_t class, SEXP data1, SEXP data2);

R_altrep_class_t
R_make_altstring_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altinteger_class(const char *cname, const char *pname, DllInfo *info);
R_altrep_class_t
R_make_altreal_class(const char *cname, const char *pname, DllInfo *info);
Rboolean R_altrep_inherits(SEXP x, R_altrep_class_t);

typedef SEXP (*R_altrep_UnserializeEX_method_t)(SEXP, SEXP, SEXP, int, int);
typedef SEXP (*R_altrep_Unserialize_method_t)(SEXP, SEXP);
typedef SEXP (*R_altrep_Serialized_state_method_t)(SEXP);
typedef SEXP (*R_altrep_DuplicateEX_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altrep_Duplicate_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altrep_Coerce_method_t)(SEXP, int);
typedef Rboolean (*R_altrep_Inspect_method_t)(SEXP, int, int, int,
					      void (*)(SEXP, int, int, int));
typedef R_xlen_t (*R_altrep_Length_method_t)(SEXP);

typedef void *(*R_altvec_Dataptr_method_t)(SEXP, Rboolean);
typedef const void *(*R_altvec_Dataptr_or_null_method_t)(SEXP);
typedef SEXP (*R_altvec_Extract_subset_method_t)(SEXP, SEXP, SEXP);

typedef struct matchset{
    R_xlen_t *matchpos;
    R_xlen_t length;
} matchset_t;

typedef struct multmatchset{
    matchset_t *matchsets;
    R_xlen_t length;
} multmatchset_t;

/*abstraction so the internals of representing matchsets can change without
  causing problems*/
SEXP multmatchsetToSEXP(multmatchset_t matches, Rboolean firstonly);
#define MMSET_GET_OUTER(mmset, i, EXPR) i<mmset.length ? EXPR : \
    error("tried to get matchset beyond length of mmset");
#define MMSET_GET_MSET(mmset, i) MMSET_GET_OUTER(mmset, i, mmset.matchsets[i])
#define MMSET_GET_MATCHPOS(mmset, i, j)				\
    MMSET_GET_OUTER(mmset, i, MSET_GET_MATCHPOS(mmset.msets[i], j))
#define MSET_GET_MATCHPOS(mset, j) j < mset.length ? mset.matchpos[j] : \
    error("tried to get match position beyond length of mset");

typedef int (*R_altinteger_Elt_method_t)(SEXP, R_xlen_t);
typedef void (*R_altinteger_Set_elt_method_t)(SEXP, R_xlen_t, int);
typedef R_xlen_t
(*R_altinteger_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, int *);
typedef int (*R_altinteger_Is_sorted_method_t)(SEXP);
typedef int (*R_altinteger_No_NA_method_t)(SEXP);
typedef int (*R_altinteger_Sort_check_method_t)(SEXP);
typedef SEXP (*R_altintger_Sort_method_t)(SEXP, Rboolean);
typedef SEXP (*R_altinteger_Order_method_t)(SEXP);
typedef SEXP (*R_altinteger_Is_NA_method_t)(SEXP);
typedef SEXP (*R_altinteger_Sum_method_t)(SEXP, Rboolean); 
typedef int (*R_altinteger_Min_method_t)(SEXP, Rboolean);
typedef int (*R_altinteger_Max_method_t)(SEXP, Rboolean);
typedef R_xlen_t (*R_altinteger_Which_min_method_t)(SEXP);
typedef R_xlen_t (*R_altinteger_Which_max_method_t)(SEXP);
typedef SEXP (*R_altinteger_Match_method_t)(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
typedef SEXP (*R_altinteger_Unique_method_t)(SEXP);
typedef SEXP (*R_altinteger_Scalar_compare_mehtod_t)(SEXP, int, Rboolean);
typedef double (*R_altinteger_Compression_ratio_method_t)(SEXP);
typedef SEXP (*R_altinteger_As_subscripts_method_t)(SEXP);



typedef double (*R_altreal_Elt_method_t)(SEXP, R_xlen_t);
typedef void (*R_altreal_Set_elt_method_t)(SEXP, R_xlen_t, double);
typedef R_xlen_t
(*R_altreal_Get_region_method_t)(SEXP, R_xlen_t, R_xlen_t, double *);
typedef int (*R_altreal_Is_sorted_method_t)(SEXP);
typedef int (*R_altreal_No_NA_method_t)(SEXP);
typedef SEXP (*R_altreal_Sort_method_t)(SEXP, Rboolean);
typedef int (*R_altreal_Sort_check_method_t)(SEXP);
typedef SEXP (*R_altreal_Order_method_t)(SEXP);
typedef SEXP (*R_altreal_Is_NA_method_t)(SEXP);
typedef SEXP (*R_altreal_Sum_method_t)(SEXP, Rboolean); 
typedef double (*R_altreal_Min_method_t)(SEXP, Rboolean);
typedef double (*R_altreal_Max_method_t)(SEXP, Rboolean);
typedef R_xlen_t (*R_altreal_Which_min_method_t)(SEXP);
typedef R_xlen_t (*R_altreal_Which_max_method_t)(SEXP);
typedef SEXP (*R_altreal_Match_method_t)(SEXP, SEXP, int, SEXP, SEXP, Rboolean);
typedef SEXP (*R_altreal_Unique_method_t)(SEXP);
typedef SEXP (*R_altreal_Scalar_compare_method_t)(SEXP, double, Rboolean);
typedef double (*R_altreal_Compression_ratio_method_t)(SEXP);
typedef SEXP (*R_altreal_As_subscripts_method_t)(SEXP);

typedef SEXP (*R_altstring_Elt_method_t)(SEXP, R_xlen_t);
typedef void (*R_altstring_Set_elt_method_t)(SEXP, R_xlen_t, SEXP);
typedef int (*R_altstring_Is_sorted_method_t)(SEXP);
typedef int (*R_altstring_No_NA_method_t)(SEXP);

#define DECLARE_METHOD_SETTER(CNAME, MNAME)				\
    void								\
    R_set_##CNAME##_##MNAME##_method(R_altrep_class_t cls,		\
				     R_##CNAME##_##MNAME##_method_t fun);

DECLARE_METHOD_SETTER(altrep, UnserializeEX)
DECLARE_METHOD_SETTER(altrep, Unserialize)
DECLARE_METHOD_SETTER(altrep, Serialized_state)
DECLARE_METHOD_SETTER(altrep, DuplicateEX)
DECLARE_METHOD_SETTER(altrep, Duplicate)
DECLARE_METHOD_SETTER(altrep, Coerce)
DECLARE_METHOD_SETTER(altrep, Inspect)
DECLARE_METHOD_SETTER(altrep, Length)

DECLARE_METHOD_SETTER(altvec, Dataptr)
DECLARE_METHOD_SETTER(altvec, Dataptr_or_null)
DECLARE_METHOD_SETTER(altvec, Extract_subset)

DECLARE_METHOD_SETTER(altinteger, Elt)
DECLARE_METHOD_SETTER(altinteger, Set_elt)
DECLARE_METHOD_SETTER(altinteger, Get_region)
DECLARE_METHOD_SETTER(altinteger, Is_sorted)
DECLARE_METHOD_SETTER(altinteger, No_NA)
DECLARE_METHOD_SETTER(altinteger, Sort_check)
DECLARE_METHOD_SETTER(altinteger, Order)
DECLARE_METHOD_SETTER(altinteger, Is_NA)
DECLARE_METHOD_SETTER(altinteger, Sum)
DECLARE_METHOD_SETTER(altinteger, Min)
DECLARE_METHOD_SETTER(altinteger, Max)
DECLARE_METHOD_SETTER(altinteger, Which_min)
DECLARE_METHOD_SETTER(altinteger, Which_max)
DECLARE_METHOD_SETTER(altinteger, Match)
DECLARE_METHOD_SETTER(altinteger, Unique)
DECLARE_METHOD_SETTER(altinteger, Compression_ratio)
DECLARE_METHOD_SETTER(altinteger, As_subscripts)


DECLARE_METHOD_SETTER(altreal, Elt)
DECLARE_METHOD_SETTER(altreal, Set_elt)
DECLARE_METHOD_SETTER(altreal, Get_region)
DECLARE_METHOD_SETTER(altreal, Is_sorted)
DECLARE_METHOD_SETTER(altreal, No_NA)
DECLARE_METHOD_SETTER(altreal, Sort_check)
DECLARE_METHOD_SETTER(altreal, Order)
DECLARE_METHOD_SETTER(altreal, Is_NA)
DECLARE_METHOD_SETTER(altreal, Sum)
DECLARE_METHOD_SETTER(altreal, Min)
DECLARE_METHOD_SETTER(altreal, Max)
DECLARE_METHOD_SETTER(altreal, Which_min)
DECLARE_METHOD_SETTER(altreal, Which_max)
DECLARE_METHOD_SETTER(altreal, Match)
DECLARE_METHOD_SETTER(altreal, Unique)
DECLARE_METHOD_SETTER(altreal, Compression_ratio)
DECLARE_METHOD_SETTER(altreal, As_subscripts)

DECLARE_METHOD_SETTER(altstring, Elt)
DECLARE_METHOD_SETTER(altstring, Set_elt)
DECLARE_METHOD_SETTER(altstring, Is_sorted)
DECLARE_METHOD_SETTER(altstring, No_NA)


/*INT_MIN is NA_INTEGER! */
enum {KNOWN_DECR = -1,
      UNKNOWN_SORTEDNESS = INT_MIN,
      KNOWN_INCR = 1,
      KNOWN_UNSORTED = 0};
#define KNOWN_SORTED(sorted) (sorted == KNOWN_DECR || sorted == KNOWN_INCR)

/* is this TOO general? I don't think so..., possible it should be ALTVEC? */

#define ALTREP_INFO(x) R_altrep_data1(x)
#define ALTREP_EXPANDED(x) R_altrep_data2(x)
#define ALTREP_NONEXP(x) (ALTREP(x) && ALTREP_EXPANDED(x) == R_NilValue)

#endif /* R_EXT_ALTREP_H_ */
