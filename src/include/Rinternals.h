/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2001   The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _R_INTERNALS_H_
#define _R_INTERNALS_H_

#include "R_ext/Arith.h"
#include "R_ext/Boolean.h"
#include "R_ext/Complex.h"
#include "R_ext/Error.h"
#include "R_ext/Memory.h"
#include "R_ext/PrtUtil.h"
#include "R_ext/Utils.h"

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>

#ifdef __MAIN__
#define extern
#endif


/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.

 * Note that the gap of 11 and 12 below is because of
 * the withdrawal of native "factor" and "ordered" types.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exaxt numeric values are seldom used, but they are, e.g., in
 *  ../main/subassign.c
*/
#ifndef enum_SEXPTYPE
/* NOT YET using enum:
 *  1)	The SEXPREC struct below has 'SEXPTYPE type : 5'
 *	(making FUNSXP and CLOSXP equivalent in there),
 *	giving (-Wall only ?) warnings all over the place
 * 2)	Many switch(type) { case ... } statements need a final `default:'
 *	added in order to avoid warnings like [e.g. l.170 of ../main/util.c]
 *	  "enumeration value `FUNSXP' not handled in switch"
 */
typedef unsigned int SEXPTYPE;

#define NILSXP	     0	  /* nil = NULL */
#define SYMSXP	     1	  /* symbols */
#define LISTSXP	     2	  /* lists of dotted pairs */
#define CLOSXP	     3	  /* closures */
#define ENVSXP	     4	  /* environments */
#define PROMSXP	     5	  /* promises: [un]evaluated closure arguments */
#define LANGSXP	     6	  /* language constructs (special lists) */
#define SPECIALSXP   7	  /* special forms */
#define BUILTINSXP   8	  /* builtin non-special forms */
#define CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define LGLSXP	    10	  /* logical vectors */
#define INTSXP	    13	  /* integer vectors */
#define REALSXP	    14	  /* real variables */
#define CPLXSXP	    15	  /* complex variables */
#define STRSXP	    16	  /* string vectors */
#define DOTSXP	    17	  /* dot-dot-dot object */
#define ANYSXP	    18	  /* make "any" args work */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */

#define FUNSXP      99    /* Closure or Builtin */

#else /* NOT YET */
/*------ enum_SEXPTYPE ----- */
typedef enum {
    NILSXP	= 0,	/* nil = NULL */
    SYMSXP	= 1,	/* symbols */
    LISTSXP	= 2,	/* lists of dotted pairs */
    CLOSXP	= 3,	/* closures */
    ENVSXP	= 4,	/* environments */
    PROMSXP	= 5,	/* promises: [un]evaluated closure arguments */
    LANGSXP	= 6,	/* language constructs (special lists) */
    SPECIALSXP	= 7,	/* special forms */
    BUILTINSXP	= 8,	/* builtin non-special forms */
    CHARSXP	= 9,	/* "scalar" string type (internal only)*/
    LGLSXP	= 10,	/* logical vectors */
    INTSXP	= 13,	/* integer vectors */
    REALSXP	= 14,	/* real variables */
    CPLXSXP	= 15,	/* complex variables */
    STRSXP	= 16,	/* string vectors */
    DOTSXP	= 17,	/* dot-dot-dot object */
    ANYSXP	= 18,	/* make "any" args work */
    VECSXP	= 19,	/* generic vectors */
    EXPRSXP	= 20,	/* expressions vectors */
    BCODESXP    = 21,   /* byte code */
    EXTPTRSXP   = 22,   /* external pointer */

    FUNSXP	= 99	/* Closure or Builtin */
} SEXPTYPE;
#endif

#define USE_GENERATIONAL_GC

#ifdef USE_GENERATIONAL_GC
# define USE_WRITE_BARRIER
#endif

#ifdef USE_RINTERNALS
/* Flags */
struct sxpinfo_struct {
    SEXPTYPE type      :  5;/* ==> (FUNSXP == 99) %% 2^5 == 3 == CLOSXP
			     * -> warning: `type' is narrower than values
			     *              of its type
			     * when SEXPTYPE was an enum */
    unsigned int obj   :  1;
    unsigned int named :  2;
    unsigned int gp    : 16;
    unsigned int mark  :  1;
    unsigned int debug :  1;
    unsigned int trace :  1;
    unsigned int fin   :  1;  /* has finalizer installed */
    unsigned int gcgen :  1;  /* old generation number */
    unsigned int gccls :  3;  /* node class */
}; /*		    Tot: 32 */

struct vecsxp_struct {
    int	length;
    int	truelength;
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
} SEXPREC, *SEXP;

/* The generational collector uses a reduced version of SEXPREC as a
   header in vector nodes.  The layout MUST be kept consistent with
   the SEXPREC definition.  The standard SEXPREC takes up 7 words on
   most hardware; this reduced version should take up only 6 words.
   In addition to slightly reducing memory use, this can lead to more
   favorable data alignment on 32-bit architectures like the Intel
   Pentium III where odd word alignment of doubles is allowed but much
   less efficient than even word alignment. */
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
#define SET_OBJECT(x,v)	(((x)->sxpinfo.obj)=(v))
#define SET_TYPEOF(x,v)	(((x)->sxpinfo.type)=(v))
#define SET_NAMED(x,v)	(((x)->sxpinfo.named)=(v))


/* Vector Access Macros */
#define LENGTH(x)	(((VECSEXP) (x))->vecsxp.length)
#define TRUELENGTH(x)	(((VECSEXP) (x))->vecsxp.truelength)
#define SETLENGTH(x,v)		((((VECSEXP) (x))->vecsxp.length)=(v))
#define SET_TRUELENGTH(x,v)	((((VECSEXP) (x))->vecsxp.truelength)=(v))
#define LEVELS(x)	((x)->sxpinfo.gp)
#define SETLEVELS(x,v)	(((x)->sxpinfo.gp)=(v))

/* Under the generational allocator the data for vector nodes comes
   immediately after the node structure, so the data address is a
   known ofset from the node SEXP. */
#define DATAPTR(x)	(((SEXPREC_ALIGN *) (x)) + 1)
#define CHAR(x)		((char *) DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define REAL(x)		((double *) DATAPTR(x))
#define STRING_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define VECTOR_ELT(x,i)	((SEXP *) DATAPTR(x))[i]
#define STRING_PTR(x)	((SEXP *) DATAPTR(x))
#define VECTOR_PTR(x)	((SEXP *) DATAPTR(x))

#ifndef USE_WRITE_BARRIER
#define SET_STRING_ELT(x,i,v)	(((x)->u.vecsxp.type.s)[i]=(v))
#define SET_VECTOR_ELT(x,i,v)	(((x)->u.vecsxp.type.s)[i]=(v))
#endif

/* List Access Macros */
/* These also work for ... objects */
#define LISTVAL(x)	((x)->u.listsxp)
#define TAG(e)		((e)->u.listsxp.tagval)
#define CAR(e)		((e)->u.listsxp.carval)
#define CDR(e)		((e)->u.listsxp.cdrval)
#define CAAR(e)		CAR(CAR(e))
#define CDAR(e)		CDR(CAR(e))
#define CADR(e)		CAR(CDR(e))
#define CDDR(e)		CDR(CDR(e))
#define CADDR(e)	CAR(CDR(CDR(e)))
#define CADDDR(e)	CAR(CDR(CDR(CDR(e))))
#define CAD4R(e)	CAR(CDR(CDR(CDR(CDR(e)))))
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
#define MISSING(x)	((x)->sxpinfo.gp)	/* for closure calls */
#ifndef USE_WRITE_BARRIER
#define SETCAR(x,v)	(CAR(x)=(v))
#define SETCADR(x,v)	(CADR(x)=(v))
#define SETCADDR(x,v)	(CADDR(x)=(v))
#define SETCADDDR(x,v)	(CADDDR(x)=(v))
#define SETCAD4R(x,v)	(CAD4R(x)=(v))
#define SETCDR(x,y)	do {SEXP X=(x), Y=(y); if(X != R_NilValue) CDR(X)=Y; else error("bad value");} while (0)
#endif
#define SET_MISSING(x,v)	(((x)->sxpinfo.gp)=(v))

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define DEBUG(x)	((x)->sxpinfo.debug)
#define TRACE(x)	((x)->sxpinfo.trace)
#define SET_DEBUG(x,v)	(((x)->sxpinfo.debug)=(v))
#define SET_TRACE(x,v)	(((x)->sxpinfo.trace)=(v))

/* Primitive Access Macros */
#define PRIMOFFSET(x)	((x)->u.primsxp.offset)
#define SET_PRIMOFFSET(x,v)	(((x)->u.primsxp.offset)=(v))

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL(x)	((x)->sxpinfo.gp) /* for ..1, ..2 etc */
#ifndef USE_WRITE_BARRIER
#define SET_PRINTNAME(x,v)	(((x)->u.symsxp.pname)=(v))
#define SET_SYMVALUE(x,v)	(((x)->u.symsxp.value)=(v))
#define SET_INTERNAL(x,v)	(((x)->u.symsxp.internal)=(v))
#endif
#define SET_DDVAL(x,v)	(((x)->sxpinfo.gp)=(v)) /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#ifndef USE_WRITE_BARRIER
#define SET_FRAME(x,v)		(((x)->u.envsxp.frame)=(v))
#define SET_ENCLOS(x,v)		(((x)->u.envsxp.enclos)=(v))
#define SET_HASHTAB(x,v)	(((x)->u.envsxp.hashtab)=(v))
#endif
#define ENVFLAGS(x)	((x)->sxpinfo.gp)	/* for environments */
#define SET_ENVFLAGS(x,v)	(((x)->sxpinfo.gp)=(v))
#else
typedef struct SEXPREC *SEXP;
#define CONS(a, b)	cons((a), (b))		/* data lists */
#define LCONS(a, b)	lcons((a), (b))		/* language lists */
#define CHAR(x)		R_CHAR(x)
#endif /* USE_RINTERNALS */

/* External pointer access macros */
#define EXTPTR_PTR(x)	CAR(x)
#define EXTPTR_PROT(x)	CDR(x)
#define EXTPTR_TAG(x)	TAG(x)

/* Pointer Protection and Unprotection */
#define PROTECT(s)	protect(s)
#define UNPROTECT(n)	unprotect(n)
#define UNPROTECT_PTR(s)	unprotect_ptr(s)

/* We sometimes need to coerce a protected value and place the new
   coerced value under protection.  For these cases PROTECT_WITH_INDEX
   saves an index of the protection location that can be used to
   replace the protected value using REPROTECT. */
typedef int PROTECT_INDEX;
#define PROTECT_WITH_INDEX(x,i) R_ProtectWithIndex(x,i)
#define REPROTECT(x,i) R_Reprotect(x,i)

/* Evaluation Environment */
extern SEXP	R_GlobalEnv;	    /* The "global" environment */

/* Special Values */
extern SEXP	R_NilValue;	    /* The nil object */
extern SEXP	R_UnboundValue;	    /* Unbound marker */
extern SEXP	R_MissingArg;	    /* Missing argument marker */

/* Symbol Table Shortcuts */
extern SEXP	R_Bracket2Symbol;   /* "[[" */
extern SEXP	R_BracketSymbol;    /* "[" */
extern SEXP	R_BraceSymbol;      /* "{" */
extern SEXP	R_TmpvalSymbol;     /* "*tmp*" */
extern SEXP	R_ClassSymbol;	    /* "class" */
extern SEXP	R_DimNamesSymbol;   /* "dimnames" */
extern SEXP	R_DimSymbol;	    /* "dim" */
extern SEXP	R_DollarSymbol;	    /* "$" */
extern SEXP	R_DotsSymbol;	    /* "..." */
extern SEXP	R_DropSymbol;	    /* "drop" */
extern SEXP	R_LevelsSymbol;	    /* "levels" */
extern SEXP	R_ModeSymbol;	    /* "mode" */
extern SEXP	R_NamesSymbol;	    /* "names" */
extern SEXP	R_NaRmSymbol;	    /* "na.rm" */
extern SEXP	R_RowNamesSymbol;   /* "row.names" */
extern SEXP	R_SeedsSymbol;	    /* ".Random.seed" */
extern SEXP	R_TspSymbol;	    /* "tsp" */
extern SEXP	R_LastvalueSymbol;  /* ".Last.value" */
extern SEXP	R_CommentSymbol;    /* "comment" */
extern SEXP	R_SourceSymbol;     /* "source" */
extern SEXP	R_DotEnvSymbol;     /* ".Environment" */
extern SEXP	R_RecursiveSymbol;  /* "recursive" */
extern SEXP	R_UseNamesSymbol;  /* "use.names" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
extern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
extern SEXP	R_BlankString;	    /* "" as a CHARSXP */

/*--- FUNCTIONS ------------------------------------------------------ */

#ifndef R_NO_REMAP
#define allocArray		Rf_allocArray
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocSExp		Rf_allocSExp
#define allocString		Rf_allocString
#define allocVector		Rf_allocVector
#define append			Rf_append
#define applyClosure		Rf_applyClosure
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asReal			Rf_asReal
#define classgets		Rf_classgets
#define coerceList		Rf_coerceList
#define coerceVector		Rf_coerceVector
#define CoercionWarning		Rf_CoercionWarning
#define ComplexFromInteger	Rf_ComplexFromInteger
#define ComplexFromLogical	Rf_ComplexFromLogical
#define ComplexFromReal		Rf_ComplexFromReal
#define ComplexFromString	Rf_ComplexFromString
#define conformable		Rf_conformable
#define cons			Rf_cons
#define copyListMatrix		Rf_copyListMatrix
#define copyMatrix		Rf_copyMatrix
#define copyMostAttrib		Rf_copyMostAttrib
#define copyVector		Rf_copyVector
#define CreateTag		Rf_CreateTag
#define CustomPrintValue	Rf_CustomPrintValue
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define duplicate		Rf_duplicate
#define elt			Rf_elt
#define emptyEnv		Rf_emptyEnv
#define EnsureString		Rf_EnsureString
#define eval			Rf_eval
#define EvalArgs		Rf_EvalArgs
#define evalList		Rf_evalList
#define evalListKeepMissing	Rf_evalListKeepMissing
#define findFun			Rf_findFun
#define findVar			Rf_findVar
#define GetArrayDimnames	Rf_GetArrayDimnames
#define getAttrib		Rf_getAttrib
#define GetColNames		Rf_GetColNames
#define GetMatrixDimnames	Rf_GetMatrixDimnames
#define GetOption		Rf_GetOption
#define GetOptionDigits		Rf_GetOptionDigits
#define GetOptionWidth		Rf_GetOptionWidth
#define GetPar			Rf_GetPar
#define GetRowNames		Rf_GetRowNames
#define gsetVar			Rf_gsetVar
#define inherits		Rf_inherits
#define install			Rf_install
#define IntegerFromComplex	Rf_IntegerFromComplex
#define IntegerFromLogical	Rf_IntegerFromLogical
#define IntegerFromReal		Rf_IntegerFromReal
#define IntegerFromString	Rf_IntegerFromString
#define isArray			Rf_isArray
#define isComplex		Rf_isComplex
#define isEnvironment		Rf_isEnvironment
#define isExpression		Rf_isExpression
#define isExpressionObject	Rf_isExpressionObject
#define isFactor		Rf_isFactor
#define isFrame			Rf_isFrame
#define isFree			Rf_isFree
#define isFunction		Rf_isFunction
#define isInteger		Rf_isInteger
#define isLanguage		Rf_isLanguage
#define isList			Rf_isList
#define isLogical		Rf_isLogical
#define isMatrix		Rf_isMatrix
#define isNewList		Rf_isNewList
#define isNull			Rf_isNull
#define isNumeric		Rf_isNumeric
#define isObject		Rf_isObject
#define isOrdered		Rf_isOrdered
#define isPairList		Rf_isPairList
#define isReal			Rf_isReal
#define isString		Rf_isString
#define isSymbol		Rf_isSymbol
#define isTs			Rf_isTs
#define isUnordered		Rf_isUnordered
#define isUserBinop		Rf_isUserBinop
#define isValidString		Rf_isValidString
#define isValidStringF		Rf_isValidStringF
#define isVector		Rf_isVector
#define isVectorAtomic		Rf_isVectorAtomic
#define isVectorizable		Rf_isVectorizable
#define isVectorList		Rf_isVectorList
#define ItemName		Rf_ItemName
#define lang1			Rf_lang1
#define lang2			Rf_lang2
#define lang3			Rf_lang3
#define lang4			Rf_lang4
#define lastElt			Rf_lastElt
#define lcons			Rf_lcons
#define length(x)		Rf_length(x)
#define lengthgets		Rf_lengthgets
#define list1			Rf_list1
#define list2			Rf_list2
#define list3			Rf_list3
#define list4			Rf_list4
#define listAppend		Rf_listAppend
#define LogicalFromComplex	Rf_LogicalFromComplex
#define LogicalFromInteger	Rf_LogicalFromInteger
#define LogicalFromReal		Rf_LogicalFromReal
#define LogicalFromString	Rf_LogicalFromString
#define makeSubscript		Rf_makeSubscript
#define matchArg		Rf_matchArg
#define matchArgs		Rf_matchArgs
#define matchPar		Rf_matchPar
#define mkChar			Rf_mkChar
#define mkString		Rf_mkString
#define namesgets		Rf_namesgets
#define ncols			Rf_ncols
#define nlevels			Rf_nlevels
#define NonNullStringMatch	Rf_NonNullStringMatch
#define nrows			Rf_nrows
#define nthcdr			Rf_nthcdr
#define PairToVectorList	Rf_PairToVectorList
#define pmatch			Rf_pmatch
#define psmatch			Rf_psmatch
#define PrintDefaults		Rf_PrintDefaults
#define PrintValue		Rf_PrintValue
#define PrintValueEnv		Rf_PrintValueEnv
#define PrintValueRec		Rf_PrintValueRec
#define protect			Rf_protect
#define RealFromComplex		Rf_RealFromComplex
#define RealFromInteger		Rf_RealFromInteger
#define RealFromLogical		Rf_RealFromLogical
#define RealFromString		Rf_RealFromString
#define rownamesgets		Rf_rownamesgets
#define ScalarComplex		Rf_ScalarComplex
#define ScalarInteger		Rf_ScalarInteger
#define ScalarLogical		Rf_ScalarLogical
#define ScalarReal		Rf_ScalarReal
#define ScalarString		Rf_ScalarString
#define setAttrib		Rf_setAttrib
#define setSVector		Rf_setSVector
#define setVar			Rf_setVar
#define StringBlank		Rf_StringBlank
#define StringFromComplex	Rf_StringFromComplex
#define StringFromInteger	Rf_StringFromInteger
#define StringFromLogical	Rf_StringFromLogical
#define StringFromReal		Rf_StringFromReal
#define substitute		Rf_substitute
#define unprotect		Rf_unprotect
#define unprotect_ptr		Rf_unprotect_ptr
#define VectorToPairList	Rf_VectorToPairList
#endif

/* Type Coercions of all kinds */

SEXP coerceVector(SEXP, SEXPTYPE);
SEXP coerceList(SEXP, SEXPTYPE);
void CoercionWarning(int);/* warning code */
SEXP PairToVectorList(SEXP x);
SEXP VectorToPairList(SEXP x);

int LogicalFromInteger(int, int*);
int LogicalFromReal(double, int*);
int LogicalFromComplex(Rcomplex, int*);
int LogicalFromString(SEXP, int*);
int IntegerFromLogical(int, int*);
int IntegerFromReal(double, int*);
int IntegerFromComplex(Rcomplex, int*);
int IntegerFromString(SEXP, int*);
double RealFromLogical(int, int*);
double RealFromInteger(int, int*);
double RealFromComplex(Rcomplex, int*);
double RealFromString(SEXP, int*);
Rcomplex ComplexFromLogical(int, int*);
Rcomplex ComplexFromInteger(int, int*);
Rcomplex ComplexFromReal(double, int*);
Rcomplex ComplexFromString(SEXP, int*);
SEXP StringFromLogical(int, int*);
SEXP StringFromInteger(int, int*);
SEXP StringFromReal(double, int*);
SEXP StringFromComplex(Rcomplex, int*);
SEXP EnsureString(SEXP);


/* Other Internally Used Functions */

SEXP allocArray(SEXPTYPE, SEXP);
SEXP allocMatrix(SEXPTYPE, int, int);
SEXP allocSExp(SEXPTYPE);
SEXP allocString(int);
SEXP allocVector(SEXPTYPE, int);
SEXP allocList(int);
SEXP append(SEXP, SEXP);
SEXP applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP asChar(SEXP);
Rcomplex asComplex(SEXP);
int asInteger(SEXP);
int asLogical(SEXP);
double asReal(SEXP);
SEXP arraySubscript(int, SEXP, SEXP);
SEXP classgets(SEXP, SEXP);
Rboolean conformable(SEXP, SEXP);
SEXP cons(SEXP, SEXP);
void copyListMatrix(SEXP, SEXP, Rboolean);
void copyMatrix(SEXP, SEXP, Rboolean);
void copyMostAttrib(SEXP, SEXP);
void copyVector(SEXP, SEXP);
SEXP CreateTag(SEXP);
void CustomPrintValue(SEXP,SEXP);
void defineVar(SEXP, SEXP, SEXP);
SEXP dimgets(SEXP, SEXP);
SEXP dimnamesgets(SEXP, SEXP);
SEXP duplicate(SEXP);
SEXP elt(SEXP, int);
SEXP emptyEnv(void);
SEXP eval(SEXP, SEXP);
SEXP EvalArgs(SEXP, SEXP, int);
SEXP evalList(SEXP, SEXP);
SEXP evalListKeepMissing(SEXP, SEXP);
/* SEXP extendEnv(SEXP, SEXP, SEXP); */
SEXP findVar(SEXP, SEXP);
SEXP findFun(SEXP, SEXP);
SEXP getAttrib(SEXP, SEXP);
void GetMatrixDimnames(SEXP, SEXP*, SEXP*, char**, char**);
SEXP GetArrayDimnames(SEXP);
SEXP GetColNames(SEXP);
SEXP GetOption(SEXP, SEXP);
int GetOptionDigits(SEXP);
int GetOptionWidth(SEXP);
SEXP GetPar(char*, SEXP);
SEXP GetRowNames(SEXP);
void gsetVar(SEXP, SEXP, SEXP);
Rboolean inherits(SEXP, char*);
SEXP install(char*);
Rboolean isArray(SEXP);
Rboolean isComplex(SEXP);
Rboolean isEnvironment(SEXP);
Rboolean isExpression(SEXP);
Rboolean isExpressionObject(SEXP);
Rboolean isFactor(SEXP);
Rboolean isFrame(SEXP);
Rboolean isFree(SEXP);
Rboolean isFunction(SEXP);
Rboolean isInteger(SEXP);
Rboolean isLanguage(SEXP);
Rboolean isList(SEXP);
Rboolean isLogical(SEXP);
Rboolean isMatrix(SEXP);
Rboolean isNewList(SEXP);
Rboolean isNull(SEXP);
Rboolean isNumeric(SEXP);
Rboolean isObject(SEXP);
Rboolean isOrdered(SEXP);
Rboolean isPairList(SEXP);
Rboolean isReal(SEXP);
Rboolean isString(SEXP);
Rboolean isSymbol(SEXP);
Rboolean isTs(SEXP);
Rboolean isUnordered(SEXP);
Rboolean isUserBinop(SEXP);
Rboolean isValidString(SEXP);
Rboolean isValidStringF(SEXP);
Rboolean isVector(SEXP);
Rboolean isVectorizable(SEXP);
Rboolean isVectorAtomic(SEXP);
Rboolean isVectorList(SEXP);
SEXP ItemName(SEXP, int);
SEXP lang1(SEXP);
SEXP lang2(SEXP, SEXP);
SEXP lang3(SEXP, SEXP, SEXP);
SEXP lang4(SEXP, SEXP, SEXP, SEXP);
SEXP lastElt(SEXP);
SEXP lcons(SEXP, SEXP);
int length(SEXP);
SEXP lengthgets(SEXP, int);
SEXP list1(SEXP);
SEXP list2(SEXP, SEXP);
SEXP list3(SEXP, SEXP, SEXP);
SEXP list4(SEXP, SEXP, SEXP, SEXP);
SEXP listAppend(SEXP, SEXP);
SEXP makeSubscript(SEXP, SEXP, int *);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgs(SEXP, SEXP);
SEXP matchPar(char*, SEXP*);
SEXP mkChar(const char*);
SEXP mkString(const char*);
SEXP namesgets(SEXP, SEXP);
int ncols(SEXP);
int nrows(SEXP);
int nlevels(SEXP);
Rboolean NonNullStringMatch(SEXP, SEXP);
SEXP nthcdr(SEXP, int);
Rboolean psmatch(char *, char *, Rboolean);
Rboolean pmatch(SEXP, SEXP, Rboolean);
void PrintDefaults(SEXP);
void PrintValue(SEXP);
void PrintValueEnv(SEXP, SEXP);
void PrintValueRec(SEXP, SEXP);
SEXP protect(SEXP);
SEXP rownamesgets(SEXP,SEXP);
SEXP ScalarLogical(int);
SEXP ScalarInteger(int);
SEXP ScalarReal(double);
SEXP ScalarComplex(Rcomplex);
SEXP ScalarString(SEXP);
SEXP setAttrib(SEXP, SEXP, SEXP);
void setSVector(SEXP*, int, SEXP);
void setVar(SEXP, SEXP, SEXP);
Rboolean StringBlank(SEXP);
SEXP substitute(SEXP,SEXP);
void unprotect(int);
void unprotect_ptr(SEXP);
void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP R_subset3_dflt(SEXP, SEXP);

				/* return(.) NOT reached : for -Wall */
#define error_return(msg)	{ error(msg);		return R_NilValue; }
#define errorcall_return(cl,msg){ errorcall(cl, msg);	return R_NilValue; }

#ifdef __MAIN__
#undef extern
#endif

/* General Cons Cell Attributes */
SEXP (ATTRIB)(SEXP x);
int (OBJECT)(SEXP x);
int (MARK)(SEXP x);
int (TYPEOF)(SEXP x);
int (NAMED)(SEXP x);
void (SET_ATTRIB)(SEXP x, SEXP v);
void (SET_OBJECT)(SEXP x, int v);
void (SET_TYPEOF)(SEXP x, int v);
void (SET_NAMED)(SEXP x, int v);

/* Vector Access Macros */
int (LENGTH)(SEXP x);
int (TRUELENGTH)(SEXP x);
char *(R_CHAR)(SEXP x);
SEXP (STRING_ELT)(SEXP x, int i);
void (SETLENGTH)(SEXP x, int v);
void (SET_TRUELENGTH)(SEXP x, int v);
void (SET_STRING_ELT)(SEXP x, int i, SEXP v);
int (LEVELS)(SEXP x);
int (SETLEVELS)(SEXP x, int v);
SEXP (VECTOR_ELT)(SEXP x, int i);
SEXP (SET_VECTOR_ELT)(SEXP x, int i, SEXP v);
int *(LOGICAL)(SEXP x);
int *(INTEGER)(SEXP x);
double *(REAL)(SEXP x);
Rcomplex *(COMPLEX)(SEXP x);
SEXP *(STRING_PTR)(SEXP x);
SEXP *(VECTOR_PTR)(SEXP x);

/* List Access Macros */
/* These also work for ... objects */
/*#define LISTVAL(x)	((x)->u.listsxp)*/
SEXP (TAG)(SEXP e);
SEXP (CAR)(SEXP e);
SEXP (CDR)(SEXP e);
SEXP (CAAR)(SEXP e);
SEXP (CDAR)(SEXP e);
SEXP (CADR)(SEXP e);
SEXP (CDDR)(SEXP e);
SEXP (CADDR)(SEXP e);
SEXP (CADDDR)(SEXP e);
SEXP (CAD4R)(SEXP e);
int (MISSING)(SEXP x);
void (SET_TAG)(SEXP x, SEXP y);
SEXP (SETCAR)(SEXP x, SEXP y);
SEXP (SETCDR)(SEXP x, SEXP y);
SEXP (SETCADR)(SEXP x, SEXP y);
SEXP (SETCADDR)(SEXP x, SEXP y);
SEXP (SETCADDDR)(SEXP x, SEXP y);
SEXP (SETCAD4R)(SEXP e, SEXP y);
void (SET_MISSING)(SEXP x, int v);

/* Closure Access Macros */
SEXP (FORMALS)(SEXP x);
SEXP (BODY)(SEXP x);
SEXP (CLOENV)(SEXP x);
int (DEBUG)(SEXP x);
int (TRACE)(SEXP x);
void (SET_FORMALS)(SEXP x, SEXP v);
void (SET_BODY)(SEXP x, SEXP v);
void (SET_CLOENV)(SEXP x, SEXP v);
void (SET_DEBUG)(SEXP x, int v);
void (SET_TRACE)(SEXP x, int v);

/* Primitive Access Macros */
int (PRIMOFFSET)(SEXP x);
void (SET_PRIMOFFSET)(SEXP x, int v);


/* Symbol Access Macros */
SEXP (PRINTNAME)(SEXP x);
SEXP (SYMVALUE)(SEXP x);
SEXP (INTERNAL)(SEXP x);
int (DDVAL)(SEXP x);
void (SET_PRINTNAME)(SEXP x, SEXP v);
void (SET_SYMVALUE)(SEXP x, SEXP v);
void (SET_INTERNAL)(SEXP x, SEXP v);
void (SET_DDVAL)(SEXP x, int v);

/* Environment Access Macros */
SEXP (FRAME)(SEXP x);
SEXP (ENCLOS)(SEXP x);
SEXP (HASHTAB)(SEXP x);
int (ENVFLAGS)(SEXP x);
void (SET_FRAME)(SEXP x, SEXP v);
void (SET_ENCLOS)(SEXP x, SEXP v);
void (SET_HASHTAB)(SEXP x, SEXP v);
void (SET_ENVFLAGS)(SEXP x, int v);

/* Promise Access Macros */
SEXP (PREXPR)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int (PRSEEN)(SEXP x);
void (SET_PREXPR)(SEXP x, SEXP v);
void (SET_PRENV)(SEXP x, SEXP v);
void (SET_PRVALUE)(SEXP x, SEXP v);
void (SET_PRSEEN)(SEXP x, int v);

/* Hashing Macros */
int (HASHASH)(SEXP x);
int (HASHVALUE)(SEXP x);
void (SET_HASHASH)(SEXP x, int v);
void (SET_HASHVALUE)(SEXP x, int v);

/* External pointer interface */
SEXP R_MakeExternalPtr(void *p, SEXP tag, SEXP prot);
void *R_ExternalPtrAddr(SEXP s);
SEXP R_ExternalPtrTag(SEXP s);
SEXP R_ExternalPtrProtected(SEXP s);
void R_ClearExternalPtr(SEXP s);
void R_SetExternalPtrAddr(SEXP s, void *p);
void R_SetExternalPtrTag(SEXP s, SEXP tag);
void R_SetExternalPtrProtected(SEXP s, SEXP p);

/* Finalization interface */
typedef void (*R_CFinalizer_t)(SEXP);
void R_RegisterFinalizer(SEXP s, SEXP fun);
void R_RegisterCFinalizer(SEXP s, R_CFinalizer_t fun);

#endif /* _R_INTERNALS_H_ */
