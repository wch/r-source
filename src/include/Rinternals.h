/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999-2004   The R Development Core Team.
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

#ifdef __cplusplus
extern "C" {
#endif

#include <R_ext/Arith.h>
#include <R_ext/Boolean.h>
#include <R_ext/Complex.h>
#include <R_ext/Error.h>
#include <R_ext/Memory.h>
#include <R_ext/PrtUtil.h>
#include <R_ext/Utils.h>

#include <errno.h>
#include <stdio.h>
/* #include <fcntl.h> This is not ISO C */
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>

#include <R_ext/libextern.h>

typedef unsigned char Rbyte;

/* type for length of vectors etc */
typedef int R_len_t; /* will be long later, LONG64 or ssize_t on Win64 */
#define R_LEN_T_MAX INT_MAX

/* Fundamental Data Types:  These are largely Lisp
 * influenced structures, with the exception of LGLSXP,
 * INTSXP, REALSXP, CPLXSXP and STRSXP which are the
 * element types for S-like data objects.

 * Note that the gap of 11 and 12 below is because of
 * the withdrawal of native "factor" and "ordered" types.
 *
 *			--> TypeTable[] in ../main/util.c for  typeof()
 */

/*  These exact numeric values are seldom used, but they are, e.g., in
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
#define ANYSXP	    18	  /* make "any" args work.
			     Used in specifying types for symbol
			     registration to mean anything is okay  */
#define VECSXP	    19	  /* generic vectors */
#define EXPRSXP	    20	  /* expressions vectors */
#define BCODESXP    21    /* byte code */
#define EXTPTRSXP   22    /* external pointer */
#define WEAKREFSXP  23    /* weak reference */
#define RAWSXP      24    /* raw bytes */

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
    WEAKREFSXP  = 23,   /* weak reference */
    RAWSXP      = 24,   /* raw bytes */

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
    R_len_t	length;
    R_len_t	truelength;
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
   known offset from the node SEXP. */
#define DATAPTR(x)	(((SEXPREC_ALIGN *) (x)) + 1)
#define CHAR(x)		((char *) DATAPTR(x))
#define LOGICAL(x)	((int *) DATAPTR(x))
#define INTEGER(x)	((int *) DATAPTR(x))
#define RAW(x)		((Rbyte *) DATAPTR(x))
#define COMPLEX(x)	((Rcomplex *) DATAPTR(x))
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
#define MISSING_MASK	15 /* reserve 4 bits--only 2 uses now */
#define MISSING(x)	((x)->sxpinfo.gp & MISSING_MASK)/* for closure calls */
#ifndef USE_WRITE_BARRIER
#define SETCAR(x,v)	(CAR(x)=(v))
#define SETCADR(x,v)	(CADR(x)=(v))
#define SETCADDR(x,v)	(CADDR(x)=(v))
#define SETCADDDR(x,v)	(CADDDR(x)=(v))
#define SETCAD4R(x,v)	(CAD4R(x)=(v))
#define SETCDR(x,y)	do {SEXP X=(x), Y=(y); if(X != R_NilValue) CDR(X)=Y; else error("bad value");} while (0)
#endif
#define SET_MISSING(x,v) do { \
  SEXP __x__ = (x); \
  int __v__ = (v); \
  int __other_flags__ = __x__->sxpinfo.gp & ~MISSING_MASK; \
  __x__->sxpinfo.gp = __other_flags__ | __v__; \
} while (0)

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
#define DDVAL_MASK	1
#define DDVAL(x)	((x)->sxpinfo.gp & DDVAL_MASK) /* for ..1, ..2 etc */
#ifndef USE_WRITE_BARRIER
#define SET_PRINTNAME(x,v)	(((x)->u.symsxp.pname)=(v))
#define SET_SYMVALUE(x,v)	(((x)->u.symsxp.value)=(v))
#define SET_INTERNAL(x,v)	(((x)->u.symsxp.internal)=(v))
#endif
#define SET_DDVAL_BIT(x) (((x)->sxpinfo.gp) |= DDVAL_MASK)
#define UNSET_DDVAL_BIT(x) (((x)->sxpinfo.gp) &= ~DDVAL_MASK)
#define SET_DDVAL(x,v) ((v) ? SET_DDVAL_BIT(x) : UNSET_DDVAL_BIT(x)) /* for ..1, ..2 etc */

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

#ifdef BYTECODE
/* Bytecode access macros */
#define BCODE_CODE(x)	CAR(x)
#define BCODE_CONSTS(x) CDR(x)
#define BCODE_EXPR(x)	TAG(x)
#define isByteCode(x)	(TYPEOF(x)==BCODESXP)
#else
#define isByteCode(x)	FALSE
#endif

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
LibExtern SEXP	R_GlobalEnv;	    /* The "global" environment */

LibExtern SEXP	R_BaseNamespace;    /* The (fake) name space for base */
LibExtern SEXP	R_NamespaceRegistry;/* Registry for registerd name spaces */

/* Special Values */
LibExtern SEXP	R_NilValue;	    /* The nil object */
LibExtern SEXP	R_UnboundValue;	    /* Unbound marker */
LibExtern SEXP	R_MissingArg;	    /* Missing argument marker */
extern SEXP	R_RestartToken;     /* Marker for restarted function calls */

/* Symbol Table Shortcuts */
LibExtern SEXP	R_Bracket2Symbol;   /* "[[" */
LibExtern SEXP	R_BracketSymbol;    /* "[" */
LibExtern SEXP	R_BraceSymbol;      /* "{" */
LibExtern SEXP	R_TmpvalSymbol;     /* "*tmp*" */
LibExtern SEXP	R_ClassSymbol;	    /* "class" */
LibExtern SEXP	R_DimNamesSymbol;   /* "dimnames" */
LibExtern SEXP	R_DimSymbol;	    /* "dim" */
LibExtern SEXP	R_DollarSymbol;	    /* "$" */
LibExtern SEXP	R_DotsSymbol;	    /* "..." */
LibExtern SEXP	R_DropSymbol;	    /* "drop" */
LibExtern SEXP	R_LevelsSymbol;	    /* "levels" */
LibExtern SEXP	R_ModeSymbol;	    /* "mode" */
LibExtern SEXP	R_NamesSymbol;	    /* "names" */
LibExtern SEXP	R_NaRmSymbol;	    /* "na.rm" */
LibExtern SEXP	R_RowNamesSymbol;   /* "row.names" */
LibExtern SEXP	R_SeedsSymbol;	    /* ".Random.seed" */
LibExtern SEXP	R_TspSymbol;	    /* "tsp" */
LibExtern SEXP	R_LastvalueSymbol;  /* ".Last.value" */
LibExtern SEXP	R_CommentSymbol;    /* "comment" */
LibExtern SEXP	R_SourceSymbol;     /* "source" */
LibExtern SEXP	R_DotEnvSymbol;     /* ".Environment" */
LibExtern SEXP	R_RecursiveSymbol;  /* "recursive" */
LibExtern SEXP	R_UseNamesSymbol;   /* "use.names" */

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
LibExtern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
LibExtern SEXP	R_BlankString;	    /* "" as a CHARSXP */

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rf_ascommon(SEXP, SEXP, SEXPTYPE);
SEXP Rf_coerceVector(SEXP, SEXPTYPE);
SEXP Rf_coerceList(SEXP, SEXPTYPE);
void Rf_CoercionWarning(int);/* warning code */
SEXP Rf_PairToVectorList(SEXP x);
SEXP Rf_VectorToPairList(SEXP x);

int Rf_LogicalFromInteger(int, int*);
int Rf_LogicalFromReal(double, int*);
int Rf_LogicalFromComplex(Rcomplex, int*);
int Rf_LogicalFromString(SEXP, int*);
int Rf_IntegerFromLogical(int, int*);
int Rf_IntegerFromReal(double, int*);
int Rf_IntegerFromComplex(Rcomplex, int*);
int Rf_IntegerFromString(SEXP, int*);
double Rf_RealFromLogical(int, int*);
double Rf_RealFromInteger(int, int*);
double Rf_RealFromComplex(Rcomplex, int*);
double Rf_RealFromString(SEXP, int*);
Rcomplex Rf_ComplexFromLogical(int, int*);
Rcomplex Rf_ComplexFromInteger(int, int*);
Rcomplex Rf_ComplexFromReal(double, int*);
Rcomplex Rf_ComplexFromString(SEXP, int*);
SEXP Rf_StringFromLogical(int, int*);
SEXP Rf_StringFromInteger(int, int*);
SEXP Rf_StringFromReal(double, int*);
SEXP Rf_StringFromComplex(Rcomplex, int*);
SEXP Rf_EnsureString(SEXP);


/* Other Internally Used Functions */

SEXP Rf_allocArray(SEXPTYPE, SEXP);
SEXP Rf_allocMatrix(SEXPTYPE, int, int);
SEXP Rf_allocSExp(SEXPTYPE);
SEXP Rf_allocString(int);
SEXP Rf_allocVector(SEXPTYPE, R_len_t);
SEXP Rf_allocList(int);
SEXP Rf_applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rf_asChar(SEXP);
Rcomplex Rf_asComplex(SEXP);
int Rf_asInteger(SEXP);
int Rf_asLogical(SEXP);
double Rf_asReal(SEXP);
R_len_t Rf_asVecSize(SEXP);
SEXP Rf_arraySubscript(int, SEXP, SEXP, SEXP (*)(SEXP,SEXP),
                       SEXP (*)(SEXP, int), SEXP);
SEXP Rf_classgets(SEXP, SEXP);
Rboolean Rf_conformable(SEXP, SEXP);
SEXP Rf_cons(SEXP, SEXP);
void Rf_copyListMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMatrix(SEXP, SEXP, Rboolean);
void Rf_copyMostAttrib(SEXP, SEXP);
void Rf_copyMostAttribNoTs(SEXP, SEXP);
void Rf_copyVector(SEXP, SEXP);
SEXP Rf_CreateTag(SEXP);
void Rf_CustomPrintValue(SEXP,SEXP);
void Rf_defineVar(SEXP, SEXP, SEXP);
SEXP Rf_dimgets(SEXP, SEXP);
SEXP Rf_dimnamesgets(SEXP, SEXP);
SEXP Rf_DropDims(SEXP);
SEXP Rf_duplicate(SEXP);
SEXP Rf_elt(SEXP, int);
SEXP Rf_emptyEnv(void);
SEXP Rf_eval(SEXP, SEXP);
SEXP R_tryEval(SEXP e, SEXP env, int *ErrorOccurred);
SEXP Rf_EvalArgs(SEXP, SEXP, int);
SEXP Rf_evalList(SEXP, SEXP);
SEXP Rf_evalListKeepMissing(SEXP, SEXP);
/* SEXP extendEnv(SEXP, SEXP, SEXP); */
SEXP Rf_findVar(SEXP, SEXP);
SEXP Rf_findVarInFrame(SEXP, SEXP);
SEXP Rf_findVarInFrame3(SEXP, SEXP, Rboolean);
SEXP Rf_findFun(SEXP, SEXP);
SEXP Rf_getAttrib(SEXP, SEXP);
void Rf_GetMatrixDimnames(SEXP, SEXP*, SEXP*, char**, char**);
SEXP Rf_GetArrayDimnames(SEXP);
SEXP Rf_GetColNames(SEXP);
SEXP Rf_GetOption(SEXP, SEXP);
int Rf_GetOptionDigits(SEXP);
int Rf_GetOptionWidth(SEXP);
SEXP Rf_GetPar(char*, SEXP);
SEXP Rf_GetRowNames(SEXP);
void Rf_gsetVar(SEXP, SEXP, SEXP);
Rboolean Rf_inherits(SEXP, char*);
SEXP Rf_install(char const *);
Rboolean Rf_isArray(SEXP);
Rboolean Rf_isComplex(SEXP);
Rboolean Rf_isEnvironment(SEXP);
Rboolean Rf_isExpression(SEXP);
Rboolean Rf_isExpressionObject(SEXP);
Rboolean Rf_isFactor(SEXP);
Rboolean Rf_isFrame(SEXP);
Rboolean Rf_isFree(SEXP);
Rboolean Rf_isFunction(SEXP);
Rboolean Rf_isInteger(SEXP);
Rboolean Rf_isLanguage(SEXP);
Rboolean Rf_isList(SEXP);
Rboolean Rf_isLogical(SEXP);
Rboolean Rf_isMatrix(SEXP);
Rboolean Rf_isNewList(SEXP);
Rboolean Rf_isNull(SEXP);
Rboolean Rf_isNumeric(SEXP);
Rboolean Rf_isObject(SEXP);
Rboolean Rf_isOrdered(SEXP);
Rboolean Rf_isPairList(SEXP);
Rboolean Rf_isPrimitive(SEXP);
Rboolean Rf_isReal(SEXP);
Rboolean Rf_isString(SEXP);
Rboolean Rf_isSymbol(SEXP);
Rboolean Rf_isTs(SEXP);
Rboolean Rf_isUnordered(SEXP);
Rboolean Rf_isUnsorted(SEXP);
Rboolean Rf_isUserBinop(SEXP);
Rboolean Rf_isValidString(SEXP);
Rboolean Rf_isValidStringF(SEXP);
Rboolean Rf_isVector(SEXP);
Rboolean Rf_isVectorizable(SEXP);
Rboolean Rf_isVectorAtomic(SEXP);
Rboolean Rf_isVectorList(SEXP);
SEXP Rf_ItemName(SEXP, int);
SEXP Rf_lang1(SEXP);
SEXP Rf_lang2(SEXP, SEXP);
SEXP Rf_lang3(SEXP, SEXP, SEXP);
SEXP Rf_lang4(SEXP, SEXP, SEXP, SEXP);
SEXP Rf_lastElt(SEXP);
SEXP Rf_lcons(SEXP, SEXP);
R_len_t Rf_length(SEXP);
SEXP Rf_lengthgets(SEXP, R_len_t);
SEXP Rf_list1(SEXP);
SEXP Rf_list2(SEXP, SEXP);
SEXP Rf_list3(SEXP, SEXP, SEXP);
SEXP Rf_list4(SEXP, SEXP, SEXP, SEXP);
SEXP Rf_listAppend(SEXP, SEXP);
SEXP R_lsInternal(SEXP, Rboolean);
SEXP Rf_makeSubscript(SEXP, SEXP, int *);
SEXP Rf_matchArg(SEXP, SEXP*);
SEXP Rf_matchArgExact(SEXP, SEXP*);
SEXP Rf_matchArgs(SEXP, SEXP);
SEXP Rf_matchPar(char*, SEXP*);
SEXP Rf_mkChar(const char*);
SEXP Rf_mkString(const char*);
SEXP Rf_namesgets(SEXP, SEXP);
int Rf_ncols(SEXP);
int Rf_nrows(SEXP);
int Rf_nlevels(SEXP);
Rboolean Rf_NonNullStringMatch(SEXP, SEXP);
SEXP Rf_nthcdr(SEXP, int);
Rboolean Rf_psmatch(char *, char *, Rboolean);
Rboolean Rf_pmatch(SEXP, SEXP, Rboolean);
void Rf_PrintDefaults(SEXP);
void Rf_PrintValue(SEXP);
void Rf_PrintValueEnv(SEXP, SEXP);
void Rf_PrintValueRec(SEXP, SEXP);
SEXP Rf_protect(SEXP);
SEXP Rf_rownamesgets(SEXP,SEXP);
SEXP Rf_ScalarLogical(int);
SEXP Rf_ScalarInteger(int);
SEXP Rf_ScalarReal(double);
SEXP Rf_ScalarComplex(Rcomplex);
SEXP Rf_ScalarString(SEXP);
SEXP Rf_ScalarRaw(Rbyte);
SEXP Rf_setAttrib(SEXP, SEXP, SEXP);
void Rf_setSVector(SEXP*, int, SEXP);
void Rf_setVar(SEXP, SEXP, SEXP);
Rboolean Rf_StringBlank(SEXP);
SEXP Rf_substitute(SEXP,SEXP);
void Rf_unprotect(int);
void Rf_unprotect_ptr(SEXP);
SEXP Rf_vectorSubscript(int, SEXP, int*, SEXP (*)(SEXP,SEXP),
                        SEXP (*)(SEXP, int), SEXP);

void R_ProtectWithIndex(SEXP, PROTECT_INDEX *);
void R_Reprotect(SEXP, PROTECT_INDEX);
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);
SEXP R_subset3_dflt(SEXP, SEXP);

				/* return(.) NOT reached : for -Wall */
#define error_return(msg)	{ Rf_error(msg);	   return R_NilValue; }
#define errorcall_return(cl,msg){ Rf_errorcall(cl, msg);   return R_NilValue; }

#ifdef __MAIN__
#undef extern
#undef LibExtern
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
Rbyte *(RAW)(SEXP x);
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
SEXP (PRCODE)(SEXP x);
SEXP (PRENV)(SEXP x);
SEXP (PRVALUE)(SEXP x);
int (PRSEEN)(SEXP x);
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
void R_RegisterFinalizerEx(SEXP s, SEXP fun, Rboolean onexit);
void R_RegisterCFinalizerEx(SEXP s, R_CFinalizer_t fun, Rboolean onexit);

/* Weak reference interface */
SEXP R_MakeWeakRef(SEXP key, SEXP val, SEXP fin, Rboolean onexit);
SEXP R_MakeWeakRefC(SEXP key, SEXP val, R_CFinalizer_t fin, Rboolean onexit);
SEXP R_WeakRefKey(SEXP w);
SEXP R_WeakRefValue(SEXP w);
void R_RunWeakRefFinalizer(SEXP w);

#ifdef BYTECODE
SEXP R_PromiseExpr(SEXP);
SEXP R_ClosureExpr(SEXP);
void R_initialize_bcode(void);
SEXP R_bcEncode(SEXP);
SEXP R_bcDecode(SEXP);
#define PREXPR(e) R_PromiseExpr(e)
#define BODY_EXPR(e) R_ClosureExpr(e)
#else
#define PREXPR(e) PRCODE(e)
#define BODY_EXPR(e) BODY(e)
#endif

/* Protected evaluation */
Rboolean R_ToplevelExec(void (*fun)(void *), void *data);

/* Environment and Binding Features */
void R_RestoreHashCount(SEXP rho);
Rboolean R_IsPackageEnv(SEXP rho);
SEXP R_PackageEnvName(SEXP rho);
SEXP R_FindPackageEnv(SEXP info);
Rboolean R_IsNamespaceEnv(SEXP rho);
SEXP R_NamespaceEnvSpec(SEXP rho);
SEXP R_FindNamespace(SEXP info);
void R_LockEnvironment(SEXP env, Rboolean bindings);
Rboolean R_EnvironmentIsLocked(SEXP env);
void R_LockBinding(SEXP sym, SEXP env);
void R_MakeActiveBinding(SEXP sym, SEXP fun, SEXP env);
Rboolean R_BindingIsLocked(SEXP sym, SEXP env);
Rboolean R_BindingIsActive(SEXP sym, SEXP env);
Rboolean R_HasFancyBindings(SEXP rho);

/* Experimental Changes in Dispatching */
void R_SetUseNamespaceDispatch(Rboolean val);

/* Save/Load Interface */
#define R_XDR_DOUBLE_SIZE 8
#define R_XDR_INTEGER_SIZE 4

void R_XDREncodeDouble(double d, void *buf);
double R_XDRDecodeDouble(void *buf);
void R_XDREncodeInteger(int i, void *buf);
int R_XDRDecodeInteger(void *buf);

typedef void *R_pstream_data_t;

typedef enum {
    R_pstream_any_format,
    R_pstream_ascii_format,
    R_pstream_binary_format,
    R_pstream_xdr_format
} R_pstream_format_t;

typedef struct R_outpstream_st *R_outpstream_t;
struct R_outpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int version;
    void (*OutChar)(R_outpstream_t, int);
    void (*OutBytes)(R_outpstream_t, void *, int);
    SEXP (*OutPersistHookFunc)(SEXP, SEXP);
    SEXP OutPersistHookData;
};

typedef struct R_inpstream_st *R_inpstream_t;
struct R_inpstream_st {
    R_pstream_data_t data;
    R_pstream_format_t type;
    int (*InChar)(R_inpstream_t);
    void (*InBytes)(R_inpstream_t, void *, int);
    SEXP (*InPersistHookFunc)(SEXP, SEXP);
    SEXP InPersistHookData;
};

void R_InitInPStream(R_inpstream_t stream, R_pstream_data_t data,
		     R_pstream_format_t type,
		     int (*inchar)(R_inpstream_t),
		     void (*inbytes)(R_inpstream_t, void *, int),
		     SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitOutPStream(R_outpstream_t stream, R_pstream_data_t data,
		      R_pstream_format_t type, int version,
		      void (*outchar)(R_outpstream_t, int),
		      void (*outbytes)(R_outpstream_t, void *, int),
		      SEXP (*phook)(SEXP, SEXP), SEXP pdata);

void R_InitFileInPStream(R_inpstream_t stream, FILE *fp,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitFileOutPStream(R_outpstream_t stream, FILE *fp,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);

#ifdef NEED_CONNECTION_PSTREAMS
/* The connection interface is not yet available to packages.  To
   allow limited use of connection pointers this defines the opaque
   pointer type. */
#ifndef HAVE_RCONNECTION_TYPEDEF
typedef struct Rconn  *Rconnection;
#define HAVE_RCONNECTION_TYPEDEF
#endif
void R_InitConnOutPStream(R_outpstream_t stream, Rconnection con,
			  R_pstream_format_t type, int version,
			  SEXP (*phook)(SEXP, SEXP), SEXP pdata);
void R_InitConnInPStream(R_inpstream_t stream,  Rconnection con,
			 R_pstream_format_t type,
			 SEXP (*phook)(SEXP, SEXP), SEXP pdata);
#endif

void R_Serialize(SEXP s, R_outpstream_t ops);
SEXP R_Unserialize(R_inpstream_t ips);

/* slot management */
SEXP R_do_slot(SEXP obj, SEXP name);
SEXP R_do_slot_assign(SEXP obj, SEXP name, SEXP value);

/* class definition, new objects */
SEXP R_do_MAKE_CLASS(char *what);
SEXP R_do_new_object(SEXP class_def);

/* preserve objects across GCs */
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);

/* Shutdown actions */
void R_dot_Last(void);		/* in main.c */
void R_RunExitFinalizers(void);	/* in memory.c */

/* Recpalcments for popen and system */
#ifdef HAVE_POPEN
FILE *R_popen(char *, char *);
#endif
int R_system(char *);


#ifndef R_NO_REMAP
#define allocArray		Rf_allocArray
#define allocList		Rf_allocList
#define allocMatrix		Rf_allocMatrix
#define allocSExp		Rf_allocSExp
#define allocString		Rf_allocString
#define allocVector		Rf_allocVector
#define applyClosure		Rf_applyClosure
#define arraySubscript		Rf_arraySubscript
#define asChar			Rf_asChar
#define ascommon		Rf_ascommon
#define asComplex		Rf_asComplex
#define asInteger		Rf_asInteger
#define asLogical		Rf_asLogical
#define asReal			Rf_asReal
#define asVecSize		Rf_asVecSize
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
#define copyMostAttribNoTs	Rf_copyMostAttribNoTs
#define copyVector		Rf_copyVector
#define CreateTag		Rf_CreateTag
#define CustomPrintValue	Rf_CustomPrintValue
#define defineVar		Rf_defineVar
#define dimgets			Rf_dimgets
#define dimnamesgets		Rf_dimnamesgets
#define DropDims                Rf_DropDims
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
#define findVarInFrame		Rf_findVarInFrame
#define findVarInFrame3		Rf_findVarInFrame3
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
#define isPrimitive		Rf_isPrimitive
#define isReal			Rf_isReal
#define isString		Rf_isString
#define isSymbol		Rf_isSymbol
#define isTs			Rf_isTs
#define isUnordered		Rf_isUnordered
#define isUnsorted		Rf_isUnsorted
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
#define matchArgExact		Rf_matchArgExact
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
#define ScalarRaw		Rf_ScalarRaw
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
#define vectorSubscript         Rf_vectorSubscript
#endif

#ifdef __cplusplus
}
#endif

#endif /* _R_INTERNALS_H_ */
