/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1999, The R Development Core Team.
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _R_INTERNALS_H_
#define _R_INTERNALS_H_

#include "R_ext/Complex.h"
#include "R_ext/Errormsg.h"
#include "R_ext/Memory.h"

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#ifdef PSIGNAL
#include <psignal.h>
#else
#include <signal.h>
#include <setjmp.h>
#endif
#include <time.h>
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#ifdef __MAIN__
#define extern
#endif
#include "R_ext/Arith.h"
#define Rwrap(x) x

/*  Fundamental Data Types:  These are largely Lisp  */
/*  influenced structures, with the exception of LGLSXP,  */
/*  INTSXP, REALSXP, CPLXSXP and STRSXP which are the  */
/*  element types for S-like data objects.  */

/*  Note that the gap of 11 and 12 below is because of	*/
/*  the withdrawal of native "factor" and "ordered" types.  */

/*			--> TypeTable[] in ../main/util.c for  typeof() */

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

#define FUNSXP      99    /* Closure or Builtin */

typedef struct SEXPREC {

    /* Flags */
    struct {
	SEXPTYPE type	   :  5;
	unsigned int obj   :  1;
	unsigned int named :  2;
	unsigned int gp	   : 16;
	unsigned int mark  :  1;
	unsigned int debug :  1;
	unsigned int trace :  1;
	unsigned int	   :  5;
    } sxpinfo;

    /* Attributes */
    struct SEXPREC *attrib;

    /* Data */
    union {
	struct {
	    int	length;
	    union {
		char		*c;
		int		*i;
		double		*f;
		Rcomplex		*z;
		struct SEXPREC	**s;
	    } type;
	    int	truelength;
	} vecsxp;
	struct {
	    int		offset;
	} primsxp;
	struct {
	    struct SEXPREC *pname;
	    struct SEXPREC *value;
	    struct SEXPREC *internal;
	} symsxp;
	struct {
	    struct SEXPREC *carval;
	    struct SEXPREC *cdrval;
	    struct SEXPREC *tagval;
	} listsxp;
	struct {
	    struct SEXPREC *frame;
	    struct SEXPREC *enclos;
	    struct SEXPREC *hashtab;
	} envsxp;
	struct {
	    struct SEXPREC *formals;
	    struct SEXPREC *body;
	    struct SEXPREC *env;
	} closxp;
	struct {
	    struct SEXPREC *value;
	    struct SEXPREC *expr;
	    struct SEXPREC *env;
	} promsxp;
    } u;
} SEXPREC, *SEXP;

/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define NAMED(x)	((x)->sxpinfo.named)


/* Vector Access Macros */
#define LENGTH(x)	((x)->u.vecsxp.length)
#define TRUELENGTH(x)	((x)->u.vecsxp.truelength)
#define CHAR(x)		((x)->u.vecsxp.type.c)
#define STRING(x)	((x)->u.vecsxp.type.s)
#define LOGICAL(x)	((x)->u.vecsxp.type.i)
#define INTEGER(x)	((x)->u.vecsxp.type.i)
#define REAL(x)		((x)->u.vecsxp.type.f)
#define COMPLEX(x)	((x)->u.vecsxp.type.z)
#define LEVELS(x)	((x)->sxpinfo.gp)
#define VECTOR(x)	((x)->u.vecsxp.type.s)

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
#define SETCDR(x,y)	{SEXP X=(x), Y=(y); if(X != R_NilValue) CDR(X)=Y; else error("bad value");}

/* Closure Access Macros */
#define FORMALS(x)	((x)->u.closxp.formals)
#define BODY(x)		((x)->u.closxp.body)
#define CLOENV(x)	((x)->u.closxp.env)
#define DEBUG(x)	((x)->sxpinfo.debug)
#define TRACE(x)	((x)->sxpinfo.trace)

/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)
#define DDVAL(x)	((x)->sxpinfo.gp) /* for ..1, ..2 etc */

/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define HASHTAB(x)	((x)->u.envsxp.hashtab)
#define NARGS(x)	((x)->sxpinfo.gp)	/* for closure calls */

/* Pointer Protection and Unprotection */
#define PROTECT(s)	protect(s)
#define UNPROTECT(n)	unprotect(n)
#define UNPROTECT_PTR(s)	unprotect_ptr(s)

/* Evaluation Environment */
extern SEXP	R_GlobalEnv;	    /* The "global" environment */

/* Special Values */
extern SEXP	R_NilValue;	    /* The nil object */
extern SEXP	R_UnboundValue;	    /* Unbound marker */
extern SEXP	R_MissingArg;	    /* Missing argument marker */

/* Symbol Table Shortcuts */
extern SEXP	R_Bracket2Symbol;   /* "[[" */
extern SEXP	R_BracketSymbol;    /* "[" */
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

/* Missing Values - others from Arith.h */
#define NA_STRING	R_NaString
extern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
extern SEXP	R_BlankString;	    /* "" as a CHARSXP */

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP Rwrap(coerceVector)(SEXP, SEXPTYPE);
SEXP Rwrap(coerceList)(SEXP, SEXPTYPE);
void Rwrap(CoercionWarning)(int);/* warning code */
SEXP Rwrap(PairToVectorList)(SEXP x);
SEXP Rwrap(VectorToPairList)(SEXP x);

int Rwrap(LogicalFromInteger)(int, int*);
int Rwrap(LogicalFromReal)(double, int*);
int Rwrap(LogicalFromComplex)(Rcomplex, int*);
int Rwrap(LogicalFromString)(SEXP, int*);
int Rwrap(IntegerFromLogical)(int, int*);
int Rwrap(IntegerFromReal)(double, int*);
int Rwrap(IntegerFromComplex)(Rcomplex, int*);
int Rwrap(IntegerFromString)(SEXP, int*);
double Rwrap(RealFromLogical)(int, int*);
double Rwrap(RealFromInteger)(int, int*);
double Rwrap(RealFromComplex)(Rcomplex, int*);
double Rwrap(RealFromString)(SEXP, int*);
Rcomplex Rwrap(ComplexFromLogical)(int, int*);
Rcomplex Rwrap(ComplexFromInteger)(int, int*);
Rcomplex Rwrap(ComplexFromReal)(double, int*);
Rcomplex Rwrap(ComplexFromString)(SEXP, int*);
SEXP Rwrap(StringFromLogical)(int, int*);
SEXP Rwrap(StringFromInteger)(int, int*);
SEXP Rwrap(StringFromReal)(double, int*);
SEXP Rwrap(StringFromComplex)(Rcomplex, int*);
SEXP Rwrap(EnsureString)(SEXP);


/* Other Internally Used Functions */

SEXP Rwrap(allocArray)(SEXPTYPE, SEXP);
SEXP Rwrap(allocMatrix)(SEXPTYPE, int, int);
SEXP Rwrap(allocSExp)(SEXPTYPE);
SEXP Rwrap(allocString)(int);
SEXP Rwrap(allocVector)(SEXPTYPE, int);
SEXP Rwrap(allocList)(int);
SEXP Rwrap(append)(SEXP, SEXP);
SEXP Rwrap(applyClosure)(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP Rwrap(asChar)(SEXP);
Rcomplex Rwrap(asComplex)(SEXP);
int Rwrap(asInteger)(SEXP);
int Rwrap(asLogical)(SEXP);
double Rwrap(asReal)(SEXP);
SEXP Rwrap(arraySubscript)(int, SEXP, SEXP);
int Rwrap(conformable)(SEXP, SEXP);
SEXP Rwrap(cons)(SEXP, SEXP);
void Rwrap(copyListMatrix)(SEXP, SEXP, int);
void Rwrap(copyMatrix)(SEXP, SEXP, int);
void Rwrap(copyMostAttrib)(SEXP, SEXP);
void Rwrap(copyVector)(SEXP, SEXP);
SEXP Rwrap(CreateTag)(SEXP);
void Rwrap(CustomPrintValue)(SEXP,SEXP);
void Rwrap(defineVar)(SEXP, SEXP, SEXP);
SEXP Rwrap(dimgets)(SEXP, SEXP);
SEXP Rwrap(dimnamesgets)(SEXP, SEXP);
SEXP Rwrap(duplicate)(SEXP);
SEXP Rwrap(elt)(SEXP, int);
SEXP Rwrap(emptyEnv)(void);
void Rwrap(errorcall)(SEXP, char*, ...);
void Rwrap(ErrorMessage)(SEXP, int, ...);
SEXP Rwrap(eval)(SEXP, SEXP);
SEXP Rwrap(EvalArgs)(SEXP, SEXP, int);
SEXP Rwrap(evalList)(SEXP, SEXP);
SEXP Rwrap(evalListKeepMissing)(SEXP, SEXP);
/* SEXP extendEnv(SEXP, SEXP, SEXP); */
SEXP Rwrap(findVar)(SEXP, SEXP);
SEXP Rwrap(findFun)(SEXP, SEXP);
SEXP Rwrap(getAttrib)(SEXP, SEXP);
void Rwrap(GetMatrixDimnames)(SEXP, SEXP*, SEXP*, char**, char**);
SEXP Rwrap(GetArrayDimnames)(SEXP);
SEXP Rwrap(GetColNames)(SEXP);
SEXP Rwrap(GetOption)(SEXP, SEXP);
int Rwrap(GetOptionDigits)(SEXP);
int Rwrap(GetOptionWidth)(SEXP);
SEXP Rwrap(GetPar)(char*, SEXP);
SEXP Rwrap(GetRowNames)(SEXP);
void Rwrap(gsetVar)(SEXP, SEXP, SEXP);
int Rwrap(inherits)(SEXP, char*);
SEXP Rwrap(install)(char*);
int Rwrap(isArray)(SEXP);
int Rwrap(isComplex)(SEXP);
int Rwrap(isEnvironment)(SEXP);
int Rwrap(isExpression)(SEXP);
int Rwrap(isExpressionObject)(SEXP);
int Rwrap(isFactor)(SEXP);
int Rwrap(isFrame)(SEXP);
int Rwrap(isFunction)(SEXP);
int Rwrap(isInteger)(SEXP);
int Rwrap(isLanguage)(SEXP);
int Rwrap(isList)(SEXP);
int Rwrap(isLogical)(SEXP);
int Rwrap(isMatrix)(SEXP);
int Rwrap(isNewList)(SEXP);
int Rwrap(isNull)(SEXP);
int Rwrap(isNumeric)(SEXP);
int Rwrap(isObject)(SEXP);
int Rwrap(isOrdered)(SEXP);
int Rwrap(isPairList)(SEXP);
int Rwrap(isReal)(SEXP);
int Rwrap(isString)(SEXP);
int Rwrap(isSymbol)(SEXP);
int Rwrap(isTs)(SEXP);
int Rwrap(isUnordered)(SEXP);
int Rwrap(isUserBinop)(SEXP);
int Rwrap(isValidString)(SEXP);
int Rwrap(isValidStringF)(SEXP);
int Rwrap(isVector)(SEXP);
int Rwrap(isVectorizable)(SEXP);
int Rwrap(isVectorAtomic)(SEXP);
int Rwrap(isVectorList)(SEXP);
SEXP Rwrap(ItemName)(SEXP, int);
SEXP Rwrap(lang1)(SEXP);
SEXP Rwrap(lang2)(SEXP, SEXP);
SEXP Rwrap(lang3)(SEXP, SEXP, SEXP);
SEXP Rwrap(lang4)(SEXP, SEXP, SEXP, SEXP);
SEXP Rwrap(lastElt)(SEXP);
SEXP Rwrap(lcons)(SEXP, SEXP);
int Rwrap(length)(SEXP);
SEXP Rwrap(list1)(SEXP);
SEXP Rwrap(list2)(SEXP, SEXP);
SEXP Rwrap(list3)(SEXP, SEXP, SEXP);
SEXP Rwrap(list4)(SEXP, SEXP, SEXP, SEXP);
SEXP Rwrap(listAppend)(SEXP, SEXP);
SEXP Rwrap(makeSubscript)(SEXP, SEXP, int *);
SEXP Rwrap(matchArg)(SEXP, SEXP*);
SEXP Rwrap(matchArgs)(SEXP, SEXP);
SEXP Rwrap(matchPar)(char*, SEXP*);
SEXP Rwrap(mkChar)(const char*);
SEXP Rwrap(mkString)(const char*);
SEXP Rwrap(namesgets)(SEXP, SEXP);
int Rwrap(ncols)(SEXP);
int Rwrap(nrows)(SEXP);
int Rwrap(nlevels)(SEXP);
int Rwrap(NonNullStringMatch)(SEXP, SEXP);
SEXP Rwrap(nthcdr)(SEXP, int);
int Rwrap(pmatch)(SEXP, SEXP, int);
void Rwrap(PrintDefaults)(SEXP);
void Rwrap(PrintValue)(SEXP);
void Rwrap(PrintValueEnv)(SEXP, SEXP);
void Rwrap(PrintValueRec)(SEXP, SEXP);
SEXP Rwrap(protect)(SEXP);
SEXP Rwrap(rownamesgets)(SEXP,SEXP);
SEXP Rwrap(ScalarLogical)(int);
SEXP Rwrap(ScalarInteger)(int);
SEXP Rwrap(ScalarReal)(double);
SEXP Rwrap(ScalarComplex)(Rcomplex);
SEXP Rwrap(ScalarString)(SEXP);
SEXP Rwrap(setAttrib)(SEXP, SEXP, SEXP);
void Rwrap(setSVector)(SEXP*, int, SEXP);
void Rwrap(setVar)(SEXP, SEXP, SEXP);
int Rwrap(StringBlank)(SEXP);
SEXP Rwrap(substitute)(SEXP,SEXP);
void Rwrap(unprotect)(int);
void Rwrap(unprotect_ptr)(SEXP);

#ifdef __MAIN__
#undef extern
#endif

#endif
