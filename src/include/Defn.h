/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2001  The R Development Core Team.
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

#ifndef DEFN_H_
#define DEFN_H_

#define COUNTING

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

#include "config.h"

#include <Rinternals.h>		/*-> Arith.h, Complex.h, Error.h, Memory.h
				  PrtUtil.h, Utils.h */
#include "Internal.h"		/* do_FOO */

#include "Errormsg.h"

/* SunOS 4 is famous for broken header files. */
#ifdef SunOS4
# ifndef NULL
#  define	NULL		0
# endif
#endif /* SunOS4 */

/* PSIGNAL may be defined on Win32 in config.h */
#ifdef PSIGNAL
# include <psignal.h>
#else
# include <signal.h>
# include <setjmp.h>
#endif

/*
#include <time.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
*/

#ifdef Unix
# define OSTYPE      "unix"
# define FILESEP     "/"
#endif /* Unix */

#ifdef Macintosh
# define OSTYPE      "mac"
# define FILESEP     ":"
#endif /* Macintosh */

#ifdef Win32
# define OSTYPE      "windows"
# define FILESEP     "/"
#endif /* Win32 */

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

/*  Heap and Pointer Protection Stack Sizes.  */

#define Mega 1048576. /* 1 Mega Byte := 2^20 (= 1048576) Bytes */

/*	R_PPSSIZE  The pointer protection stack size  */
/*	R_NSIZE	   The number of cons cells	 */
/*	R_VSIZE	   The vector heap size in bytes */
/*  These values are defaults and can be overridden in config.h
    The maxima and minima are in ../unix/sys-common.c */

#ifndef R_PPSSIZE
#define	R_PPSSIZE	10000L
#endif
#ifndef R_NSIZE
#define	R_NSIZE		350000L
#endif
#ifndef R_VSIZE
#define	R_VSIZE		6291456L
#endif

#ifdef Macintosh
#include <fp.h> 
#else
#include <math.h>
#endif

/* all these are in Rinternals.h
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <ctype.h>
*/

/* Getting the working directory */
#if defined(HAVE_GETCWD)
# define R_GETCWD(x, y) getcwd(x, y)
#elif defined(Win32)
# define R_GETCWD(x, y) GetCurrentDirectory(y, x)
#else
# undef R_GETCWD
#endif

/* Maximal length of an entire file name */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if defined(MAXPATHLEN) && !defined(PATH_MAX)
#  define PATH_MAX MAXPATHLEN
# elif defined(Win32)
#  define PATH_MAX 260
# else
#  define PATH_MAX 255
# endif
#endif

#ifdef HAVE_POSIX_SETJMP
# define JMP_BUF sigjmp_buf
# define SETJMP(x) sigsetjmp(x,1)
# define LONGJMP(x,i) siglongjmp(x,i)
#else
# define JMP_BUF jmp_buf
# define SETJMP(x) setjmp(x)
# define LONGJMP(x,i) longjmp(x,i)
#endif

#include "R_ext/Rdynload.h"

#define HSIZE	   4119	/* The size of the hash table for symbols */
#define MAXELTSIZE 8192 /* The largest string size */
#define MAXIDSIZE   256	/* Largest symbol size possible */

/* The type of the do_xxxx functions. */
/* These are the built-in R functions. */
typedef SEXP (*CCODE)();

/* Type definitions for the non-recursive evaluator */
typedef struct R_code_st *R_code_t;
struct R_code_st {
    int entry;
    R_code_t (*fun)(R_code_t);
};
typedef R_code_t (*CCODE_NR)(SEXP, SEXP, SEXP, SEXP, R_code_t);

/* Information for Deparsing Expressions */
typedef enum {
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
    CCODE_NR cfun_nr;  /* c-code address for non-recursive version */
} FUNTAB;

#ifdef USE_RINTERNALS
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
#define PRIMFUN_NR(x)	(R_FunTab[(x)->u.primsxp.offset].cfun_nr)

/* Promise Access Macros */
#define PREXPR(x)	((x)->u.promsxp.expr)
#define PRENV(x)	((x)->u.promsxp.env)
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#ifndef USE_WRITE_BARRIER
# define SET_PREXPR(x,v)  (((x)->u.promsxp.expr)=(v))
# define SET_PRENV(x,v)	  (((x)->u.promsxp.env)=(v))
# define SET_PRVALUE(x,v) (((x)->u.promsxp.value)=(v))
#endif
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

/* Hashing Macros */
#define HASHASH(x)      ((x)->sxpinfo.gp)
#define HASHVALUE(x)    TRUELENGTH(x)
#define SET_HASHASH(x,v) (((x)->sxpinfo.gp)=(v))
#define SET_HASHVALUE(x,v) SET_TRUELENGTH(x, v)

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
#else
typedef struct VECREC *VECP;
#define PRIMFUN(x)	(R_FunTab[PRIMOFFSET(x)].cfun)
#define PRIMNAME(x)	(R_FunTab[PRIMOFFSET(x)].name)
#define PRIMVAL(x)	(R_FunTab[PRIMOFFSET(x)].code)
#define PRIMARITY(x)	(R_FunTab[PRIMOFFSET(x)].arity)
#define PPINFO(x)	(R_FunTab[PRIMOFFSET(x)].gram)
#define PRIMPRINT(x)	(((R_FunTab[PRIMOFFSET(x)].eval)/100)%10)
#endif

/* Evaluation Context Structure */
typedef struct RDISPATCHER {
    JMP_BUF cjmpbuf;
    int level;
    struct RDISPATCHER *next;
} RDISPATCHER, *R_dispatcher_t;

typedef struct RCNTXT {
    struct RCNTXT *nextcontext;	/* The next context up the chain */
    int callflag;		/* The context "type" */
    R_dispatcher_t dispatcher;	/* C stack and register information */
    int cstacktop;		/* Top of the pointer protection stack */
    int evaldepth;	        /* evaluation depth at inception */
    SEXP promargs;		/* Promises supplied to closure */
    SEXP sysparent;		/* environment the closure was called from */
    SEXP call;			/* The call that effected this context*/
    SEXP cloenv;		/* The environment */
    SEXP conexit;		/* Interpreted "on.exit" code */
    void (*cend)(void *);	/* C "on.exit" thunk */
    void *cenddata;		/* data for C "on.exit" thunk */
    char *vmax;		        /* top of R_alloc stack */
    SEXP curenv;		/* the current environment */
    SEXP frame;			/* the furrent frame pointer */
    R_code_t contcode;		/* continuation code for loops and closures */
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
    CTXT_BUILTIN  = 64  /* used in profiling */
};

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

/* File Handling */
/*
#define R_EOF	65535
*/
#define R_EOF	-1

/* MAGIC Numbers for files */
#define R_MAGIC_BINARY 1975
#define R_MAGIC_ASCII  1976
#define R_MAGIC_XDR    1977

#define R_MAGIC_BINARY_VERSION16 1971
#define R_MAGIC_ASCII_VERSION16	 1972


/*--- Global Variables ---------------------------------------------------- */

/* Defined and initialized in names.c (not main.c) :*/
#ifndef __R_Names__
extern
#endif
FUNTAB	R_FunTab[];	    /* Built in functions */


#ifdef __MAIN__
#define extern
#define INI_as(v) = v
#else
#define INI_as(v)
#endif

/* Formerly in Arith.h */
#ifdef IEEE_754
# define MATH_CHECK(call)	(call)
#else
  extern double R_tmp;
# define MATH_CHECK(call)	(errno=0,R_tmp=call,(errno==0)?R_tmp:R_NaN)
#endif

/* extern int	errno; already have errno.h ! */
extern int	gc_inhibit_torture INI_as(1);

extern Rboolean R_interrupts_suspended INI_as(FALSE);
extern Rboolean R_interrupt_pending INI_as(FALSE);

/* R Home Directory */
extern char*	R_Home;		    /* Root of the R tree */

/* Memory Management */
extern int	R_NSize		INI_as(R_NSIZE);/* Size of cons cell heap */
extern int	R_VSize		INI_as(R_VSIZE);/* Size of the vector heap */
extern SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern long	R_Collected;	    /* Number of free cons cells (after gc) */
extern SEXP	R_PreciousList;	    /* List of Persistent Objects */

/* The Pointer Protection Stack */
extern int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
extern int	R_PPStackTop;	    /* The top of the stack */
extern SEXP*	R_PPStack;	    /* The pointer protection stack */

/* Evaluation Environment */
extern SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
#define R_ReturnedValue (R_ThreadContext->value)
extern SEXP*	R_SymbolTable;	    /* The symbol table */
#define R_GlobalContext (R_ThreadContext->global_context)
#define R_SetGlobalContext(c) (R_GlobalContext = (c))
extern RDISPATCHER* R_Dispatcher;   /* The current dispatcher */
#define R_Visible (R_ThreadContext->visible)
#define	R_EvalDepth (R_ThreadContext->eval_depth)
#define R_EvalCount (R_ThreadContext->eval_count)
#define R_BrowseLevel (R_ThreadContext->browse_level)

extern int	R_Expressions	INI_as(500);	/* options(expressions) */
extern Rboolean	R_KeepSource	INI_as(FALSE);	/* options(keep.source) */

#define R_CurrentEnv (R_ThreadContext->current_env)
#define R_EvalFrame (R_ThreadContext->eval_frame)

/* File Input/Output */
extern Rboolean	R_Interactive	INI_as(TRUE);	/* TRUE during interactive use*/
extern Rboolean	R_Quiet		INI_as(FALSE);	/* Be as quiet as possible */
extern Rboolean	R_Slave		INI_as(FALSE);	/* Run as a slave process */
extern Rboolean	R_Verbose	INI_as(FALSE);	/* Be verbose */
/* extern int	R_Console; */	    /* Console active flag */
/* IoBuffer R_ConsoleIob; : --> ./IOStuff.h */
extern FILE*	R_Consolefile	INI_as(NULL);	/* Console output file */
extern FILE*	R_Outputfile	INI_as(NULL);	/* Output file */
extern int	R_ErrorCon	INI_as(2);	/* Error connection */

/* Objects Used In Parsing  */
extern SEXP	R_CommentSxp;	    /* Comments accumulate here */
extern SEXP	R_ParseText;	    /* Text to be parsed */
extern int	R_ParseCnt;	    /* Count of lines of text to be parsed */
extern int	R_ParseError	INI_as(0); /* Line where parse error occured */

/* Image Dump/Restore */
extern int	R_DirtyImage	INI_as(0);	/* Current image dirty */

/* History */
extern char*	R_HistoryFile;	/* Name of the history file */
extern int	R_HistorySize;	/* Size of the history file */
extern int	R_RestoreHistory;	/* restore the history file? */

/* Warnings/Errors */
#define R_ERRBUFSIZE 8192
#define R_CollectWarnings (R_ThreadContext->collect_warnings)
#define R_Warnings (R_ThreadContext->warnings)
#define R_errbuf (R_ThreadContext->errbuf)
extern int	R_ShowErrorMessages INI_as(1);	/* show error messages? */

/* GUI type */

extern char*	R_GUIType	INI_as("unknown");

/* Threading */

extern Rboolean R_threads_enabled       INI_as(FALSE);
extern Rboolean R_preemptive_scheduling INI_as(FALSE);
extern SEXP     R_ThreadTag             INI_as(NULL);
extern SEXP     R_MutexTag              INI_as(NULL);
extern SEXP     R_CondvarTag            INI_as(NULL);

extern void     (*R_SigintHandler)(int) INI_as(SIG_DFL);

#ifdef __MAIN__
#undef extern
#endif
#undef INI_as


/*--- FUNCTIONS ------------------------------------------------------ */

#ifndef R_NO_REMAP
#define begincontext		Rf_begincontext
#define checkArity		Rf_checkArity
#define CheckFormals		Rf_CheckFormals
#define CleanEd			Rf_CleanEd
#define DataFrameClass		Rf_DataFrameClass
#define ddfindVar		Rf_ddfindVar
#define deparse1		Rf_deparse1
#define deparse1line		Rf_deparse1line
#define DispatchGroup		Rf_DispatchGroup
#define DispatchOrEval		Rf_DispatchOrEval
#define duplicated		Rf_duplicated
#define dynamicfindVar		Rf_dynamicfindVar
#define endcontext		Rf_endcontext
#define errorcall		Rf_errorcall
#define ErrorMessage		Rf_ErrorMessage
#define factorsConform		Rf_factorsConform
#define FetchMethod		Rf_FetchMethod
#define findcontext		Rf_findcontext
#define findVar1		Rf_findVar1
#define findVarLocInFrame	Rf_findVarLocInFrame
#define FrameClassFix		Rf_FrameClassFix
#define framedepth		Rf_framedepth
#define frameSubscript		Rf_frameSubscript
#define get1index		Rf_get1index
#define getVar			Rf_getVar
#define getVarInFrame		Rf_getVarInFrame
#define hashpjw			Rf_hashpjw
#define InheritsClass		Rf_InheritsClass
#define InitArithmetic		Rf_InitArithmetic
#define InitColors		Rf_InitColors
#define InitConnections		Rf_InitConnections
#define InitEd			Rf_InitEd
#define InitFunctionHashing	Rf_InitFunctionHashing
#define InitGlobalEnv		Rf_InitGlobalEnv
#define InitMemory		Rf_InitMemory
#define InitNames		Rf_InitNames
#define InitOptions		Rf_InitOptions
#define initStack		Rf_initStack
#define internalTypeCheck	Rf_internalTypeCheck
#define isValidName		Rf_isValidName
#define jump_to_toplevel	Rf_jump_to_toplevel
#define levelsgets		Rf_levelsgets
#define mainloop		Rf_mainloop
#define mat2indsub		Rf_mat2indsub
#define match			Rf_match
#define mkCLOSXP		Rf_mkCLOSXP
#define mkComplex              	Rf_mkComplex
#define mkFalse			Rf_mkFalse
#define mkFloat			Rf_mkFloat
#define mkNA			Rf_mkNA
#define mkPROMISE		Rf_mkPROMISE
#define mkQUOTE			Rf_mkQUOTE
#define mkSYMSXP		Rf_mkSYMSXP
#define mkTrue			Rf_mkTrue
#define NewEnvironment		Rf_NewEnvironment
#define OneIndex		Rf_OneIndex
#define onintr			Rf_onintr
#define onsigusr1               Rf_onsigusr1
#define onsigusr2               Rf_onsigusr2
#define parse			Rf_parse
#define PrintGreeting		Rf_PrintGreeting
#define PrintVersion		Rf_PrintVersion
#define PrintWarnings		Rf_PrintWarnings
#define promiseArgs		Rf_promiseArgs
#define RemoveClass		Rf_RemoveClass
#define setVarInFrame		Rf_setVarInFrame
#define sortVector		Rf_sortVector
#define ssort			Rf_ssort
#define str2type		Rf_str2type
#define StrToInternal		Rf_StrToInternal
#define substituteList		Rf_substituteList
#define tsConform		Rf_tsConform
#define tspgets			Rf_tspgets
#define type2str		Rf_type2str
#define unbindVar		Rf_unbindVar
#define usemethod		Rf_usemethod
#define warningcall		Rf_warningcall
#define WarningMessage		Rf_WarningMessage
#define yyerror			Rf_yyerror
#define yyinit			Rf_yyinit
#define yylex			Rf_yylex
#define yyparse			Rf_yyparse
#define yyprompt		Rf_yyprompt
#define yywrap			Rf_yywrap
#endif

/* Platform Dependent Gui Hooks */

#define	R_CONSOLE	1
#define	R_FILE		2
#define R_TEXT		3

int	R_ReadConsole(char*, unsigned char*, int, int);
void	R_WriteConsole(char*, int);
void	R_ResetConsole(void);
void	R_FlushConsole(void);
void	R_ClearerrConsole(void);
void	R_Busy(int);
int	R_ShowFile(char*, char*);
int	R_ShowFiles(int, char **, char **, char *, Rboolean, char *);
int	R_ChooseFile(int, char*, int);
char*	R_Date(void);
char*	R_HomeDir(void);
Rboolean R_FileExists(char*);
Rboolean R_HiddenFile(char*);

/* Other Internally Used Functions */

R_code_t R_applyMethod_nr(SEXP, SEXP, SEXP, SEXP, SEXP, Rboolean, R_code_t);
void begincontext(int, SEXP, SEXP, SEXP, SEXP);
void R_BeginDispatcher(R_dispatcher_t);
void checkArity(SEXP, SEXP);
void CheckFormals(SEXP);
void CleanEd(void);
void DataFrameClass(SEXP);
SEXP ddfindVar(SEXP, SEXP);
SEXP deparse1(SEXP,Rboolean);
SEXP deparse1line(SEXP,Rboolean);
int DispatchOrEval(SEXP, char*, SEXP, SEXP, SEXP*, int);
int DispatchGroup(char*, SEXP,SEXP,SEXP,SEXP,SEXP*);
SEXP duplicated(SEXP);
SEXP dynamicfindVar(SEXP, RCNTXT*);
void endcontext();
void R_EndDispatcher(R_dispatcher_t);
int factorsConform(SEXP, SEXP);
SEXP FetchMethod(char *, char *, SEXP);
void findcontext(int, SEXP, SEXP);
R_code_t R_findcontext_nr(int, SEXP, SEXP);
SEXP findVar1(SEXP, SEXP, SEXPTYPE, int);
SEXP findVarLocInFrame(SEXP, SEXP);
void FrameClassFix(SEXP);
int framedepth(RCNTXT*);
SEXP frameSubscript(int, SEXP, SEXP);
int get1index(SEXP, SEXP, int, Rboolean);
SEXP getVar(SEXP, SEXP);
SEXP getVarInFrame(SEXP, SEXP);
int hashpjw(char*);
Rboolean InheritsClass(SEXP, char*);
void InitArithmetic(void);
void InitColors(void);
void InitConnections(void);
void InitEd(void);
void InitFunctionHashing(void);
void InitGlobalEnv(void);
void R_InitialData(void);
void InitMemory(void);
void InitNames(void);
void InitOptions(void);
void initStack(void);
void internalTypeCheck(SEXP, SEXP, SEXPTYPE);
int isValidName(char *);
void jump_to_toplevel(void);
SEXP levelsgets(SEXP, SEXP);
void mainloop(void);
SEXP mat2indsub(SEXP, SEXP);
SEXP match(SEXP, SEXP, int);
SEXP mkCLOSXP(SEXP, SEXP, SEXP);
SEXP mkComplex(char *s);
/* SEXP mkEnv(SEXP, SEXP, SEXP); */
SEXP mkFalse(void);
SEXP mkFloat(char *s);
SEXP mkNA(void);
SEXP mkPRIMSXP (int, int);
SEXP mkPROMISE(SEXP, SEXP);
SEXP mkQUOTE(SEXP);
SEXP mkSYMSXP(SEXP, SEXP);
SEXP mkTrue(void);
SEXP NewEnvironment(SEXP, SEXP, SEXP);
void onintr(int);
void onsigusr1();
void onsigusr2();
int OneIndex(SEXP, SEXP, int, int, SEXP*);
RCNTXT *R_ParentContext(SEXP);
SEXP parse(FILE*, int);
void PrintGreeting(void);
void PrintVersion(char *);
void PrintWarnings(void);
void process_global_Renviron();
void process_users_Renviron();
SEXP promiseArgs(SEXP, SEXP);
void Rcons_vprintf(const char *, va_list);
void RemoveClass(SEXP, char *);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashTable(int, int);
extern int R_Newhashpjw(char*);
FILE* R_OpenLibraryFile(char *);
void R_PreserveObject(SEXP);
void R_ReleaseObject(SEXP);
void R_RestoreGlobalEnv(void);
void R_SaveGlobalEnv(void);
void R_SaveToFile(SEXP, FILE*, int);
int R_SetOptionWarn(int);
int R_SetOptionWidth(int);
void R_Suicide(char*);
SEXP setVarInFrame(SEXP, SEXP, SEXP);
void sortVector(SEXP);
void ssort(SEXP*,int);
SEXPTYPE str2type(char*);
int StrToInternal(char*);
SEXP substituteList(SEXP, SEXP);
SEXP R_syscall(int,RCNTXT*);
int R_sysparent(int,RCNTXT*);
SEXP R_sysframe(int,RCNTXT*);
SEXP R_sysfunction(int,RCNTXT*);
RCNTXT* R_ToplevelContext(void);
Rboolean tsConform(SEXP,SEXP);
SEXP tspgets(SEXP, SEXP);
SEXP type2str(SEXPTYPE);
void unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
int usemethod(char*, SEXP, SEXP, SEXP, SEXP, SEXP*);
/* ../main/errors.c : */
void errorcall(SEXP, const char*, ...);
void warningcall(SEXP, const char*,...);
void ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, R_WARNING, ...);

int R_GetMaxVSize(void);
void R_SetMaxVSize(int);
int R_GetMaxNSize(void);
void R_SetMaxNSize(int);

void R_run_onexits(RCNTXT *);
void R_restore_globals(RCNTXT *);


/* gram.y & gram.c : */
void yyerror(char *);
void yyinit(void);
int yylex();
int yyparse(void);
void yyprompt(char *format, ...);
int yywrap(void);

/* Macros for suspending interrupts */
#ifdef DODO
#ifdef HAVE_POSIX_SETJMP
#define BEGIN_SUSPEND_INTERRUPTS do { \
    sigset_t mask, omask; \
    sigemptyset(&mask); \
    sigaddset(&mask,SIGINT); \
    sigprocmask(SIG_BLOCK, &mask, &omask);
#define END_SUSPEND_INTERRUPTS sigprocmask(SIG_SETMASK, &omask, &mask); \
    } while(0)
#else
#define BEGIN_SUSPEND_INTERRUPTS do {
#define END_SUSPEND_INTERRUPTS } while (0)
#endif
#else
#define BEGIN_SUSPEND_INTERRUPTS do { \
    Rboolean __oldsusp__ = R_interrupts_suspended; \
    R_interrupts_suspended = TRUE;
#define END_SUSPEND_INTERRUPTS R_interrupts_suspended = __oldsusp__; \
    if (R_interrupt_pending && ! R_interrupts_suspended) { \
        R_interrupt_pending = FALSE; \
        raise(SIGINT); \
    } \
} while(0)
#endif


/* Non-recursive evaluation support */
R_code_t R_eval_nr(SEXP, R_code_t);
void R_run_dispatcher(R_code_t);

#define EVAL_FRAME_VAR0() EXTPTR_TAG(R_EvalFrame)  /* TAG of CONS */
#define SET_EVAL_FRAME_VAR0(v) SET_TAG(R_EvalFrame, v)
#define EVAL_FRAME_VAR1() EXTPTR_PROT(R_EvalFrame) /* CDR of CONS */
#define SET_EVAL_FRAME_VAR1(v) SETCDR(R_EvalFrame, v)
#define EVAL_FRAME_CODE() ((R_code_t) EXTPTR_PTR((R_EvalFrame)))
#define POP_EVAL_FRAME() (R_EvalFrame = ATTRIB(R_EvalFrame))
#define PUSH_EVAL_FRAME(c,v0,v1) do { \
    SEXP __v0__, __v1__; \
    SEXP __frame__; \
    PROTECT(__v0__ = (v0)); \
    PROTECT(__v1__ = (v1)); \
    __frame__ = R_MakeExternalPtr(c, __v0__, __v1__); \
    SET_ATTRIB(__frame__, R_EvalFrame); \
    R_EvalFrame = __frame__; \
    UNPROTECT(2); \
} while (0)

#define DECLARE_CONTINUATION(cont, func) \
    static R_code_t func(R_code_t); \
    static struct R_code_st cont##_struct = { 0, func }; \
    static R_code_t cont = &cont##_struct


/* Blocking IO functions and data types */
typedef struct R_fd_set_st *R_fd_set_t;
typedef int R_fd_t; /**** OK for UNIX--need something else for Windows */

void R_init_fd_set(R_fd_set_t);
void R_fd_set(R_fd_t, R_fd_set_t);

/* Readline interface for console readers */
#ifdef HAVE_LIBREADLINE
typedef enum {
    R_readline_eof,
    R_readline_gotaline,
    R_readline_incomplete
} R_readline_status_t;

void R_StartReadline(char *, unsigned char *, int, int);
R_readline_status_t R_ReadlineReadOneChar(void);
#endif

/* thread macros, data, and functions */
#define R_CNTXTSTACKSIZE 1000
#define R_DEFAULT_THREAD_QUANTUM 5000

#define R_THREAD_VALUE(t) VECTOR_ELT(EXTPTR_PROT(t), 1)
#define R_THREAD_NAME(t) VECTOR_ELT(EXTPTR_PROT(t), 2)
#define R_SET_THREAD_NAME(t, v) SET_VECTOR_ELT(EXTPTR_PROT(t), 2, v)
#define R_THREAD_CONTEXT(t) ((R_thread_context_t) R_ExternalPtrAddr(t))

typedef struct R_thread_context_st *R_thread_context_t;
struct R_thread_context_st {
    RCNTXT *global_context, context_stack[R_CNTXTSTACKSIZE];
    size_t context_stack_size;
    R_thread_state_t state;
    R_thread_context_t next_context, prev_context;
    SEXP ref, prot, value, current_env, eval_frame, warnings, iodata;
    int visible, eval_depth, eval_count, browse_level, collect_warnings;
    int quantum;
    double wakeup;
    Rboolean (*ioready)(SEXP);
    void (*ioprepsleep)(R_fd_set_t, SEXP);
    char errbuf[R_ERRBUFSIZE];
    SEXP wait_object;
    R_thread_context_t wait_next, join;
};

extern R_thread_context_t R_ThreadContext;    /* current thread context */

void R_InitThreads(void);
R_thread_context_t R_MakeThreadContext(R_thread_state_t, SEXP);
R_thread_context_t R_ActiveThreads(void);
void R_DeactivateThread(R_thread_context_t);

RCNTXT *R_PushThreadGlobalContext(R_thread_context_t);
void R_SetThreadGlobalContext(R_thread_context_t, RCNTXT *);
#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
