/*
 *  R : A Computer Langage for Statistical Data Analysis
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifndef DEFN_H_
#define DEFN_H_

#define COMPLEX_DATA
#define COUNTING

#include "Platform.h"
#include "Arith.h"
#include "Errormsg.h"

	/* Heap and Pointer Protection Stack Sizes. */
	/* These values are minima and can be */
	/* overriden in Platform.h */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	10000L		/* pointer protection stack size */
#endif
#ifndef R_NSIZE
#define	R_NSIZE		200000L		/* number of cons cells */
#endif

#ifndef R_VSIZE
#define	R_VSIZE		2000000L	/* vector heap size in bytes */
#endif

#ifdef Macintosh
#include <fp.h>
#define PosixArith
#define QUICKDRAW_GRAPHICS
#else
#include <math.h>
#endif

#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>
#include <time.h>
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif

#define HSIZE		211	/* The size of the hash table for symbols */
#define MAXELTSIZE	512	/* The largest string size */
#define MAXIDSIZE	512	/* Largest symbol size possible */

	/* Fundamental Data Types:  These are largely Lisp */
	/* influenced structures, with the exception of LGLSXP, */
	/* FACTSXP, ORDSXP, INTSXP, REALSXP and STRSXP which */
	/* are the element types for S-like data objects. */

typedef unsigned int SEXPTYPE;

#define NILSXP		0	/* nil */
#define SYMSXP		1	/* symbols */
#define LISTSXP		2	/* lists & dotted pairs */
#define CLOSXP		3	/* closures */
#define ENVSXP		4	/* environments */
#define PROMSXP		5	/* evaluated/unevaluated closure arguments */
#define LANGSXP		6	/* language constructs (special lists) */
#define SPECIALSXP	7	/* special forms */
#define BUILTINSXP	8	/* builtin non-special forms */
#define CHARSXP		9	/* "scalar" string type (internal only)*/
#define LGLSXP		10	/* logical vectors */
#define FACTSXP		11	/* unordered factors */
#define ORDSXP		12	/* ordered factors */
#define INTSXP		13	/* integer vectors */
#define REALSXP		14	/* real variables */
#define CPLXSXP		15	/* complex variables */
#define STRSXP		16	/* string vectors */
#define DOTSXP		17	/* dot-dot-dot object */
#define ANYSXP		18	/* make "any" args work */
#define VECSXP		19	/* generic vectors */
#define EXPRSXP		20	/* expressions vectors */

typedef struct {
	double r;
	double i;
} complex;

typedef struct SEXPREC {
	struct {
		SEXPTYPE type      :  5;
		unsigned int obj   :  1;
		unsigned int named :  2;
		unsigned int gp    : 16;
		unsigned int mark  :  1;
		unsigned int debug :  1;
		unsigned int trace :  1;
		unsigned int       :  5;
	} sxpinfo;
	struct SEXPREC *attrib; 	/* Attributes */
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


typedef SEXP (*CCODE)();

typedef struct {
	char	*name;		/* print name */
	CCODE	cfun;		/* c-code address */
	int	code;		/* offset within c-code */
	int	eval;		/* evaluate args? */
	int	arity;		/* function arity */
	int	gram;		/* pretty-print info */
	int	mark;		/* mark info for restore */
} FUNTAB;

	/* General Cons Cell Attributes */
#define ATTRIB(x)	((x)->attrib)
#define OBJECT(x)	((x)->sxpinfo.obj)
#define MARK(x)		((x)->sxpinfo.mark)
#define TYPEOF(x)	((x)->sxpinfo.type)
#define NAMED(x)	((x)->sxpinfo.named)

	/* Primitive Access Macros */
#define PRIMOFFSET(x)	((x)->u.primsxp.offset)
#define PRIMFUN(x)	(R_FunTab[(x)->u.primsxp.offset].cfun)
#define PRIMNAME(x)	(R_FunTab[(x)->u.primsxp.offset].name)
#define PRIMVAL(x)	(R_FunTab[(x)->u.primsxp.offset].code)
#define PRIMARITY(x)	(R_FunTab[(x)->u.primsxp.offset].arity)
#define PPINFO(x)	(R_FunTab[(x)->u.primsxp.offset].gram)
#define PRIMPRINT(x)	(((R_FunTab[(x)->u.primsxp.offset].eval)/100)%10)


	/* Symbol Access Macros */
#define PRINTNAME(x)	((x)->u.symsxp.pname)
#define SYMVALUE(x)	((x)->u.symsxp.value)
#define INTERNAL(x)	((x)->u.symsxp.internal)


	/* Vector Access Macros */
#define LENGTH(x)	((x)->u.vecsxp.length)
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

	/* Environment Access Macros */
#define FRAME(x)	((x)->u.envsxp.frame)
#define ENCLOS(x)	((x)->u.envsxp.enclos)
#define NARGS(x)	((x)->sxpinfo.gp)	/* for closure calls */

	/* Promise Access Macros */
#define PREXPR(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)

	/* Pointer Protection and Unprotection */
#define PROTECT(s)	protect(s)
#define UNPROTECT(n)	unprotect(n)


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
#define COMPLEX2VEC(n)	(((n)>0)?(((n)*sizeof(complex)-1)/sizeof(VECREC)+1):0)
#define PTR2VEC(n)	(((n)>0)?(((n)*sizeof(SEXP)-1)/sizeof(VECREC)+1):0)


	/* Evaluation Context Structure */
typedef struct RCNTXT {
	struct RCNTXT *nextcontext;	/* The next context up the chain */
	int callflag;			/* The context "type" */
	jmp_buf cjmpbuf;		/* C stack and register information */
	int cstacktop;			/* Top of the pointer protection stack */
	SEXP promargs;			/* Promises supplied to closure */
	SEXP sysparent;			/* environment the closure was called from*/
	SEXP call;			/* The call that effected this context*/
	SEXP cloenv;			/* The environment */
	SEXP conexit;			/* Interpreted "on.exit" code */
	void (*cend)();			/* C "on.exit" thunk */
} RCNTXT, *context;

	/* The Various Context Types */
enum {
	CTXT_TOPLEVEL = 0,
	CTXT_NEXT     = 1,
	CTXT_BREAK    = 2,
	CTXT_LOOP     = 3,	/* break OR next target */
	CTXT_RETURN   = 4,
	CTXT_CCODE    = 8,
	CTXT_BROWSER  = 12,
	CTXT_GENERIC  = 16
};


	/* Miscellaneous Definitions */
#define streql(s, t)	(!strcmp((s), (t)))

	/* Arithmetic and Relation Operators */
#define	PLUSOP	1
#define	MINUSOP	2
#define	TIMESOP	3
#define	DIVOP	4
#define	POWOP	5
#define	MODOP	6
#define IDIVOP	7

#define	EQOP	1
#define	NEOP	2
#define	LTOP	3
#define	LEOP	4
#define	GEOP	5
#define	GTOP	6

extern int		errno;

/* Global Variables */

		/* Memory Management */
extern int	R_NSize;		/* Size of cons cell heap */
extern int	R_VSize;		/* Size of the vector heap */
extern SEXP	R_NHeap;		/* Start of the cons cell heap */
extern SEXP	R_FreeSEXP;		/* Cons cell free list */
extern VECREC*	R_VHeap;		/* Base of the vector heap */
extern VECREC*	R_VTop;			/* Current top of the vector heap */
extern VECREC*	R_VMax;			/* bottom of R_alloc'ed heap */
extern long	R_Collected;		/* Number of free cons cells (after gc) */
		/* The Pointer Protection Stack */
extern int	R_PPStackSize;		/* The stack size (elements) */
extern int	R_PPStackTop;		/* The top of the stack */
extern SEXP*	R_PPStack;		/* The pointer protection stack */
		/* Evaluation Environment */
extern SEXP	R_GlobalEnv;		/* The "global" environment */
extern SEXP	R_CurrentExpr;		/* Currently evaluating expression */
extern SEXP	R_ReturnedValue;	/* Slot for return-ing values */
extern SEXP*	R_SymbolTable;		/* The symbol table */
extern RCNTXT	R_Toplevel;		/* Storage for the toplevel environment */
extern RCNTXT*	R_ToplevelContext;	/* The toplevel environment */
extern RCNTXT*	R_GlobalContext;	/* The global environment */
extern int	R_Visible;		/* Value visibility flag */
extern int	R_EvalDepth;		/* Evaluation recursion depth */
extern int	R_EvalCount;		/* Evaluation count */
extern FUNTAB	R_FunTab[];		/* Built in functions */
extern int	R_BrowseLevel;		/* how deep the browser is */
		/* File Input/Output */
extern int	R_Interactive;		/* Non-zero during interactive use */
extern int	R_Quiet;		/* Be as quiet as possible */
/* extern int	R_Console;		/* Console active flag */
extern FILE*	R_Inputfile;		/* Current input flag */
extern FILE*	R_Consolefile;		/* Console output file */
extern FILE*	R_Outputfile;		/* Output file */
extern FILE*	R_Sinkfile;		/* Sink file */
		/* Objects Used In Parsing  */
extern SEXP	R_CommentSxp;		/* Comments accumulate here */
extern SEXP	R_ParseText;		/* Text to be parsed */
extern int	R_ParseCnt;		/* Count of lines of text to be parsed */
extern int	R_ParseError;		/* Line where parse error occured */
		/* Special Values */
extern SEXP	R_NilValue;		/* The nil object */
extern SEXP	R_UnboundValue;		/* Unbound marker */
extern SEXP	R_MissingArg;		/* Missing argument marker */
		/* Symbol Table Shortcuts */
extern SEXP	R_Bracket2Symbol;	/* "[[" */
extern SEXP	R_BracketSymbol;	/* "[" */
extern SEXP	R_ClassSymbol;		/* "class" */
extern SEXP	R_DimNamesSymbol;	/* "dimnames" */
extern SEXP	R_DimSymbol;		/* "dim" */
extern SEXP	R_DollarSymbol;		/* "$" */
extern SEXP	R_DotsSymbol;		/* "..." */
extern SEXP	R_DropSymbol;		/* "drop" */
extern SEXP	R_LevelsSymbol;		/* "levels" */
extern SEXP	R_ModeSymbol;		/* "mode" */
extern SEXP	R_NamesSymbol;		/* "names" */
extern SEXP	R_NaRmSymbol;		/* "na.rm" */
extern SEXP	R_RowNamesSymbol;	/* "row.names" */
extern SEXP	R_SeedsSymbol;		/* ".Random.seed" */
extern SEXP	R_TspSymbol;		/* "tsp" */
extern SEXP	R_LastvalueSymbol;	/* ".Last.value" */
extern SEXP	R_CommentSymbol;	/* "comment" */
		/* Missing Values - others from Arith.h */
extern SEXP	R_NaString;		/* NA_STRING */
		/* Image Dump/Restore */
extern char	R_ImageName[256];	/* Default image name */
extern int	R_Unnamed;		/* Use default name? */
extern int	R_DirtyImage;		/* Current image dirty */
extern int	R_Init;			/* Do we have an image loaded */
extern FILE*	R_FileRef;		/* the environment file pointer  */

#define NA_LOGICAL	R_NaInt
#define NA_INTEGER	R_NaInt
#define NA_FACTOR	R_NaInt
#define NA_REAL		R_NaReal
#define NA_STRING	R_NaString

	/* File Handling */

#define R_EOF	65535
extern int	R_fgetc(FILE*);

	/* MAGIC Numbers for files */

#define R_MAGIC_BINARY 1975
#define R_MAGIC_ASCII  1976
#define R_MAGIC_XDR    1977

#define R_MAGIC_BINARY_VERSION16 1971
#define R_MAGIC_ASCII_VERSION16  1972

	/* Other Stuff */

void hsv2rgb(double h, double s, double v, double *r, double *g, double *b);
void GCircle(double x, double y, double radius, int col, int border);
void call_R(char *func, long nargs, void **arguments, char **modes, long *lengths, char **names, long nres, char **results);
void printRealVector(double * x, int n, int index);
int StringFalse(char *name);

		/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

int	R_ReadConsole(char*, char*, int, int);
void	R_WriteConsole(char*, int);
void	R_ResetConsole(void);
void	R_FlushConsole(void);
void	R_ClearerrConsole(void);
void	R_Busy(int);
void	R_CleanUp(int);
void	R_StartUp(void);

		/* Defined in main.c */

char	*R_PromptString(int, int);

		/* Internally Used Functions */

SEXP allocArray(SEXPTYPE, SEXP);
SEXP allocMatrix(SEXPTYPE, int, int);
SEXP allocSExp(SEXPTYPE);
SEXP allocString(int);
SEXP allocVector(SEXPTYPE, int);
SEXP allocList(int);
SEXP append(SEXP, SEXP);
SEXP applyClosure(SEXP, SEXP, SEXP, SEXP, SEXP);
SEXP applyRelOp(int, int, int);
SEXP asChar(SEXP);
int asInteger(SEXP);
int asLogical(SEXP);
double asReal(SEXP);
SEXP arraySubscript(int, SEXP, SEXP);
void begincontext(RCNTXT*, int, SEXP, SEXP, SEXP, SEXP);
void checkArity(SEXP, SEXP);
void CheckFormals(SEXP);
SEXP classgets(SEXP, SEXP);
#ifdef Macintosh
	void CleanUpMemory( void );
#endif
SEXP coerceVector(SEXP, SEXPTYPE);
SEXP coerceList(SEXP, SEXPTYPE);
void compactPhase(void);
int conformable(SEXP, SEXP);
SEXP cons(SEXP, SEXP);
void copyListMatrix(SEXP, SEXP, int);
void copyMatrix(SEXP, SEXP, int);
void copyVector(SEXP, SEXP);
SEXP CreateTag(SEXP);
void CustomPrintValue(SEXP);
void DataFrameClass(SEXP);
void defineVar(SEXP, SEXP, SEXP);
SEXP deparse1(SEXP,int);
SEXP dimgets(SEXP, SEXP);
SEXP dimnamesgets(SEXP, SEXP);
int DispatchOrEval(SEXP, SEXP, SEXP, SEXP, SEXP*, int);
int DispatchGroup(char*, SEXP,SEXP,SEXP,SEXP,SEXP*);
void dhsv2rgb(double,double,double,double*,double*,double*);
SEXP DropDims(SEXP);
SEXP duplicate(SEXP);
SEXP duplicated(SEXP);
SEXP dynamicfindVar(SEXP, RCNTXT*);
SEXP emptyEnv(void);
void endcontext(RCNTXT*);
void errorcall(SEXP, char*, ...);
void  ErrorMessage(SEXP, int, ...);
SEXP eval(SEXP, SEXP);
SEXP EvalArgs(SEXP, SEXP, int);
SEXP evalList(SEXP, SEXP);
SEXP evalListKeepMissing(SEXP, SEXP);
SEXP extendEnv(SEXP, SEXP, SEXP);
int factorsConform(SEXP, SEXP);
void findcontext(int, SEXP);
SEXP findVar(SEXP, SEXP);
SEXP findVar1(SEXP, SEXP, SEXPTYPE, int);
SEXP findVarInFrame(SEXP, SEXP);
SEXP findFun(SEXP, SEXP);
SEXP FixupCex(SEXP);
SEXP FixupCol(SEXP);
SEXP FixupFont(SEXP);
SEXP FixupLty(SEXP);
SEXP FixupPch(SEXP);
void FrameClassFix(SEXP);
int framedepth(RCNTXT*);
SEXP frameSubscript(int, SEXP, SEXP);
void gc(void);
SEXP getAttrib(SEXP, SEXP);
int get1index(SEXP,SEXP,int);
SEXP GetOption(SEXP, SEXP);
int GetOptionDigits(SEXP);
int GetOptionWidth(SEXP);
SEXP GetPar(char*, SEXP);
SEXP getVar(SEXP, SEXP);
SEXP getVarInFrame(SEXP, SEXP);
void gsetVar(SEXP, SEXP, SEXP);
int hashpjw(char*);
int IndexWidth(int);
int inherits(SEXP, char*);
void InitArithmetic(void);
void InitColors(void);
void InitEd(void);
void InitFunctionHashing(void);
void InitGlobalEnv(void);
void R_InitialData(void);
void InitMemory(void);
void InitNames(void);
void InitOptions(void);
void initStack(void);
SEXP install(char*);
void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
int isArray(SEXP);
int isComplex(SEXP);
char *R_ExpandFileName(char*);
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
int isNull(SEXP);
int isNumeric(SEXP);
int isObject(SEXP);
int isOrdered(SEXP);
void isort(int*, int);
int isReal(SEXP);
int isString(SEXP);
int isSymbol(SEXP);
int isTs(SEXP);
int isUnordered(SEXP);
int isUserBinop(SEXP);
int isVector(SEXP);
int isVectorizable(SEXP);
void jump_to_toplevel(void);
void KillDevice(void);
SEXP lang1(SEXP);
SEXP lang2(SEXP, SEXP);
SEXP lang3(SEXP, SEXP, SEXP);
SEXP lang4(SEXP, SEXP, SEXP, SEXP);
SEXP lcons(SEXP, SEXP);
int length(SEXP);
SEXP levelsgets(SEXP, SEXP);
SEXP list1(SEXP);
SEXP list2(SEXP, SEXP);
SEXP list3(SEXP, SEXP, SEXP);
SEXP list4(SEXP, SEXP, SEXP, SEXP);
SEXP listAppend(SEXP, SEXP);
unsigned int LTYpar(SEXP, int);
void mainloop(void);
SEXP makeSubscript(SEXP, SEXP, int *);
void markPhase(void);
void markSExp(SEXP);
SEXP mat2indsub(SEXP, SEXP);
SEXP match(SEXP, SEXP, int);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgs(SEXP, SEXP);
SEXP matchPar(char*, SEXP*);
SEXP mkCLOSXP(SEXP, SEXP, SEXP);
SEXP mkEnv(SEXP, SEXP, SEXP);
SEXP mkPRIMSXP (int, int);
SEXP mkPROMISE(SEXP, SEXP);
SEXP mkQUOTE(SEXP);
SEXP mkSYMSXP(SEXP, SEXP);
SEXP mkChar(char*);
SEXP mkFalse(void);
SEXP mkString(char*);
SEXP mkTrue(void);
SEXP namesgets(SEXP, SEXP);
int ncols(SEXP);
int nrows(SEXP);
SEXP nthcdr(SEXP, int);
void onintr();
FILE* R_OpenLibraryFile();
SEXP parse(FILE*, int);
int pmatch(SEXP, SEXP, int);
void PrintDefaults(SEXP);
void PrintGreeting(void);
void PrintValue(SEXP);
void PrintValueEnv(SEXP, SEXP);
SEXP promiseArgs(SEXP, SEXP);
void protect(SEXP);
char *R_alloc(long, int);
void REvprintf(const char*, va_list);
void REprintf(char*, ...);
void R_RestoreGlobalEnv(void);
int restore_image(char*);
unsigned int RGBpar(SEXP, int);
SEXP rownamesgets(SEXP,SEXP);
void Rprintf(char*, ...);
char *Rsprintf(char*, ...);
void Rvprintf(const char*, va_list);
void rsort(double *x, int);
int Rstrlen(char*);
SEXP R_LoadFromFile(FILE*);
void R_SaveGlobalEnv(void);
void R_SaveToFile(SEXP, FILE*, int);
void scanPhase(void);
SEXP setAttrib(SEXP, SEXP, SEXP);
void setIVector(int*, int, int);
void setRVector(double*, int, double);
void setSVector(SEXP*, int, SEXP);
void setVar(SEXP, SEXP, SEXP);
SEXP setVarInFrame(SEXP, SEXP, SEXP);
void sortVector(SEXP);
void ssort(SEXP*,int);
SEXPTYPE str2type(char*);
int StringTrue(char*);
int StrToInternal(char*);
void R_Suicide(char*);
void SymbolShortcuts(void);
SEXP syscall(int,RCNTXT*);
int sysparent(int,RCNTXT*);
SEXP sysframe(int,RCNTXT*);
SEXP sysfunction(int,RCNTXT*);
int tsConform(SEXP,SEXP);
SEXP tspgets(SEXP, SEXP);
SEXP type2str(SEXPTYPE);
void unbindVar(SEXP, SEXP);
void UNIMPLEMENTED(char *s);
void unmarkPhase(void);
void unprotect(int);
int usemethod(char*, SEXP, SEXP, SEXP, SEXP, SEXP*);
char *vmaxget(void);
void vmaxset(char*);
void WrongArgCount(char*);
void warningcall(SEXP, char*,...);
void WarningMessage(SEXP, int, ...);
void yyinit(void);
int yyparse(void);
void yyprompt(char *format, ...);
int yywrap(void);

#endif
