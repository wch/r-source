/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifndef _R_INTERNALS_H_
#define _R_INTERNALS_H_

#include "Arith.h"		/*-> Rconfig.h */
#include "Complex.h"
#include "Errormsg.h"
#include "Memory.h"
#include "PrtUtil.h"
#include "Utils.h"

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
		complex		*z;
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
#define FACTOR(x)	((x)->u.vecsxp.type.i)
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
extern SEXP	R_NaString;	    /* NA_STRING as a CHARSXP */
extern SEXP	R_BlankString;	    /* "" as a CHARSXP */

/*--- FUNCTIONS ------------------------------------------------------ */

/* Type Coercions of all kinds */

SEXP coerceVector(SEXP, SEXPTYPE);
SEXP coerceList(SEXP, SEXPTYPE);
void CoercionWarning(int);/* warning code */
SEXP PairToVectorList(SEXP x);
SEXP VectorToPairList(SEXP x);

int LogicalFromInteger(int, int*);
int LogicalFromReal(double, int*);
int LogicalFromComplex(complex, int*);
int LogicalFromString(SEXP, int*);
int IntegerFromLogical(int, int*);
int IntegerFromReal(double, int*);
int IntegerFromComplex(complex, int*);
int IntegerFromString(SEXP, int*);
double RealFromLogical(int, int*);
double RealFromInteger(int, int*);
double RealFromComplex(complex, int*);
double RealFromString(SEXP, int*);
complex ComplexFromLogical(int, int*);
complex ComplexFromInteger(int, int*);
complex ComplexFromReal(double, int*);
complex ComplexFromString(SEXP, int*);
SEXP StringFromLogical(int, int*);
SEXP StringFromInteger(int, int*);
SEXP StringFromReal(double, int*);
SEXP StringFromComplex(complex, int*);
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
complex asComplex(SEXP);
int asInteger(SEXP);
int asLogical(SEXP);
double asReal(SEXP);
SEXP arraySubscript(int, SEXP, SEXP);
int conformable(SEXP, SEXP);
SEXP cons(SEXP, SEXP);
void copyListMatrix(SEXP, SEXP, int);
void copyMatrix(SEXP, SEXP, int);
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
void errorcall(SEXP, char*, ...);
void ErrorMessage(SEXP, int, ...);
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
int inherits(SEXP, char*);
SEXP install(char*);
int isArray(SEXP);
int isComplex(SEXP);
int isEnvironment(SEXP);
int isExpression(SEXP);
int isExpressionObject(SEXP);
int isFactor(SEXP);
int isFrame(SEXP);
int isFunction(SEXP);
int isInteger(SEXP);
int isLanguage(SEXP);
int isList(SEXP);
int isLogical(SEXP);
int isMatrix(SEXP);
int isNewList(SEXP);
int isNull(SEXP);
int isNumeric(SEXP);
int isObject(SEXP);
int isOrdered(SEXP);
int isPairList(SEXP);
int isReal(SEXP);
int isString(SEXP);
int isSymbol(SEXP);
int isTs(SEXP);
int isUnordered(SEXP);
int isUserBinop(SEXP);
int isVector(SEXP);
int isVectorizable(SEXP);
int isVectorList(SEXP);
int isVectorObject(SEXP);
SEXP ItemName(SEXP, int);
SEXP lang1(SEXP);
SEXP lang2(SEXP, SEXP);
SEXP lang3(SEXP, SEXP, SEXP);
SEXP lang4(SEXP, SEXP, SEXP, SEXP);
SEXP lastElt(SEXP);
SEXP lcons(SEXP, SEXP);
int length(SEXP);
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
int NonNullStringMatch(SEXP, SEXP);
SEXP nthcdr(SEXP, int);
int pmatch(SEXP, SEXP, int);
void PrintDefaults(SEXP);
void PrintValue(SEXP);
void PrintValueEnv(SEXP, SEXP);
void PrintValueRec(SEXP, SEXP);
SEXP protect(SEXP);
SEXP rownamesgets(SEXP,SEXP);
SEXP ScalarLogical(int);
SEXP ScalarInteger(int);
SEXP ScalarReal(double);
SEXP ScalarComplex(complex);
SEXP ScalarString(SEXP);
SEXP setAttrib(SEXP, SEXP, SEXP);
void setSVector(SEXP*, int, SEXP);
void setVar(SEXP, SEXP, SEXP);
int StringBlank(SEXP);
void unprotect(int);
void unprotect_ptr(SEXP);

#ifdef __MAIN__
#undef extern
#endif

#endif
