/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998--2019  The R Core Team.
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

/* Internal header, not installed */

#ifndef DEFN_H_
#define DEFN_H_

/* seems unused */
#define COUNTING

#define BYTECODE

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

#define MAXELTSIZE 8192 /* Used as a default for string buffer sizes,
			   and occasionally as a limit. */

#include <R_ext/Complex.h>
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
#else
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
#endif
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

#ifdef HAVE_F77_UNDERSCORE
# define F77_SYMBOL(x)	x ## _
# define F77_QSYMBOL(x)	#x "_"
#else
# define F77_SYMBOL(x)	x
# define F77_QSYMBOL(x) #x
#endif

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


#define Mega 1048576. /* 1 Mega Byte := 2^20 (= 1048576) Bytes */
#define Giga 1073741824. /* 1 Giga Byte := 2^30 Bytes */

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

/* Glibc manages to not define this in -pedantic -ansi */
#if defined(HAVE_PUTENV) && !defined(putenv) && defined(HAVE_DECL_PUTENV) && !HAVE_DECL_PUTENV
extern int putenv(char *string);
#endif


/* Maximal length in bytes of an entire path name.
   POSIX has required this to be at least 255/256, and X/Open at least 1024.
   Solaris has 1024, Linux glibc has 4192.
   File names are limited to FILENAME_MAX bytes (usually the same as PATH_MAX)
   or NAME_MAX (often 255/256).
 */
#if !defined(PATH_MAX)
# if defined(HAVE_SYS_PARAM_H)
#  include <sys/param.h>
# endif
# if !defined(PATH_MAX)
#  if defined(MAXPATHLEN)
/* Try BSD name */
#    define PATH_MAX MAXPATHLEN
#  elif defined(Win32)
/* seems this is now defined by MinGW to be 259, whereas FILENAME_MAX
   and MAX_PATH are 260.  It is not clear that this really is in bytes,
   but might be chars for the Unicode interfaces.

   260 is d:\ plus 256 chars plus nul.  Some but not all API calls
   allow filepaths of the form \\?\D:\very_long_path .
*/
#    define PATH_MAX 260
#  else
/* quite possibly unlimited, so we make this large, and test when used */
#    define PATH_MAX 5000
#  endif
# endif
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
#define PRVALUE(x)	((x)->u.promsxp.value)
#define PRSEEN(x)	((x)->sxpinfo.gp)
#define SET_PRSEEN(x,v)	(((x)->sxpinfo.gp)=(v))

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

   Allocating on the stack memory is also supported; this is currently
   used for jump buffers.
*/
typedef struct {
    int tag;
    union {
	int ival;
	double dval;
	SEXP sxpval;
    } u;
} R_bcstack_t;
# define PARTIALSXP_MASK (~255)
# define IS_PARTIAL_SXP_TAG(x) ((x) & PARTIALSXP_MASK)
# define RAWMEM_TAG 254

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
    SEXP handlerstack;          /* condition handler stack */
    SEXP restartstack;          /* stack of available restarts */
    struct RPRSTACK *prstack;   /* stack of pending promises */
    R_bcstack_t *nodestack;
    SEXP srcref;	        /* The source line in effect */
    int browserfinish;          /* should browser finish this context without
                                   stopping */
    SEXP returnValue;           /* only set during on.exit calls */
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
extern0 SEXP	R_NHeap;	    /* Start of the cons cell heap */
extern0 SEXP	R_FreeSEXP;	    /* Cons cell free list */
extern0 R_size_t R_Collected;	    /* Number of free cons cells (after gc) */
extern0 int	R_Is_Running;	    /* for Windows memory manager */

/* The Pointer Protection Stack */
LibExtern int	R_PPStackSize	INI_as(R_PPSSIZE); /* The stack size (elements) */
LibExtern int	R_PPStackTop;	    /* The top of the stack */
LibExtern SEXP*	R_PPStack;	    /* The pointer protection stack */

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
extern uintptr_t R_CStackLimit	INI_as((uintptr_t)-1);	/* C stack limit */
extern uintptr_t R_OldCStackLimit INI_as((uintptr_t)0); /* Old value while
							   handling overflow */
extern uintptr_t R_CStackStart	INI_as((uintptr_t)-1);	/* Initial stack address */
extern int	R_CStackDir	INI_as(1);	/* C stack direction */

#ifdef R_USE_SIGNALS
extern0 struct RPRSTACK *R_PendingPromises INI_as(NULL); /* Pending promise stack */
#endif

/* File Input/Output */
LibExtern Rboolean R_Interactive INI_as(TRUE);	/* TRUE during interactive use*/
extern0 Rboolean R_Quiet	INI_as(FALSE);	/* Be as quiet as possible */
extern Rboolean  R_Slave	INI_as(FALSE);	/* Run as a slave process */
extern0 Rboolean R_Verbose	INI_as(FALSE);	/* Be verbose */
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
#ifdef Win32
LibExtern unsigned int localeCP  INI_as(1252); /* the locale's codepage */
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

#define R_BCNODESTACKSIZE 200000
extern0 R_bcstack_t *R_BCNodeStackBase, *R_BCNodeStackTop, *R_BCNodeStackEnd;
extern0 int R_jit_enabled INI_as(0); /* has to be 0 during R startup */
extern0 int R_compile_pkgs INI_as(0);
extern0 int R_check_constants INI_as(0);
extern0 int R_disable_bytecode INI_as(0);
extern SEXP R_cmpfun1(SEXP); /* unconditional fresh compilation */
extern void R_init_jit_enabled(void);
extern void R_initAssignSymbols(void);
#ifdef R_USE_SIGNALS
extern SEXP R_findBCInterpreterSrcref(RCNTXT*);
#endif
extern SEXP R_getCurrentSrcref();
extern SEXP R_getBCInterpreterExpression();

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
extern0 int R_PCRE_study INI_as(10);
extern0 int R_PCRE_limit_recursion;


#ifdef __MAIN__
# undef extern
# undef extern0
# undef LibExtern
#endif
#undef INI_as

#define checkArity(a,b) Rf_checkArityCall(a,b,call)

/*--- FUNCTIONS ------------------------------------------------------ */

# define allocCharsxp		Rf_allocCharsxp
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
# define installTrChar		Rf_installTrChar
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
# define matchArgs		Rf_matchArgs
# define matchArgs_RC		Rf_matchArgs_RC
# define matchPar		Rf_matchPar
# define Mbrtowc		Rf_mbrtowc
# define mbtoucs		Rf_mbtoucs
# define mbcsToUcs2		Rf_mbcsToUcs2
# define memtrace_report	Rf_memtrace_report
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
# define tspgets		Rf_tspgets
# define type2symbol		Rf_type2symbol
# define unbindVar		Rf_unbindVar
# define usemethod		Rf_usemethod
# define ucstomb		Rf_ucstomb
# define ucstoutf8		Rf_ucstoutf8
#ifdef ADJUST_ENVIR_REFCNTS
# define unpromiseArgs		Rf_unpromiseArgs
#endif
# define utf8toucs		Rf_utf8toucs
# define utf8towcs		Rf_utf8towcs
# define vectorIndex		Rf_vectorIndex
# define warningcall		Rf_warningcall
# define WarningMessage		Rf_WarningMessage
# define wcstoutf8		Rf_wcstoutf8
# define wtransChar		Rf_wtransChar
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
Rboolean R_FileExists(const char *);
Rboolean R_HiddenFile(const char *);
double	R_FileMtime(const char *);
int	R_GetFDLimit();
int	R_EnsureFDLimit(int);

/* environment cell access */
typedef struct { SEXP cell; } R_varloc_t; /* use struct to prevent casting */
#define R_VARLOC_IS_NULL(loc) ((loc).cell == NULL)
R_varloc_t R_findVarLocInFrame(SEXP, SEXP);
R_varloc_t R_findVarLoc(SEXP, SEXP);
SEXP R_GetVarLocValue(R_varloc_t);
SEXP R_GetVarLocSymbol(R_varloc_t);
Rboolean R_GetVarLocMISSING(R_varloc_t);
void R_SetVarLocValue(R_varloc_t, SEXP);

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
#define DIGITS16             	512
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

/* Other Internally Used Functions */

SEXP Rf_allocCharsxp(R_len_t);
SEXP Rf_append(SEXP, SEXP); /* apparently unused now */
R_xlen_t asVecSize(SEXP x);
R_xlen_t asXLength(SEXP x);
void check1arg(SEXP, SEXP, const char *);
void Rf_checkArityCall(SEXP, SEXP, SEXP);
void CheckFormals(SEXP);
void R_check_locale(void);
void check_stack_balance(SEXP op, int save);
void CleanEd(void);
void copyMostAttribNoTs(SEXP, SEXP);
SEXP createS3Vars(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
void CustomPrintValue(SEXP, SEXP);
double currentTime(void);
void DataFrameClass(SEXP);
SEXP ddfindVar(SEXP, SEXP);
SEXP deparse1(SEXP,Rboolean,int);
SEXP deparse1m(SEXP call, Rboolean abbrev, int opts);
SEXP deparse1w(SEXP,Rboolean,int);
SEXP deparse1line (SEXP, Rboolean);
SEXP deparse1line_(SEXP, Rboolean, int);
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
void NORET findcontext(int, SEXP, SEXP);
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
void NORET jump_to_toplevel(void);
void KillAllDevices(void);
SEXP levelsgets(SEXP, SEXP);
void mainloop(void);
SEXP makeSubscript(SEXP, SEXP, R_xlen_t *, SEXP);
SEXP markKnown(const char *, SEXP);
SEXP mat2indsub(SEXP, SEXP, SEXP);
SEXP matchArg(SEXP, SEXP*);
SEXP matchArgExact(SEXP, SEXP*);
SEXP matchArgs(SEXP, SEXP, SEXP);
SEXP matchArgs_RC(SEXP, SEXP, SEXP);
SEXP matchPar(const char *, SEXP*);
void memtrace_report(void *, void *);
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
RETSIGTYPE onsigusr1(int);
RETSIGTYPE onsigusr2(int);
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
void Rcons_vprintf(const char *, va_list);
SEXP R_data_class(SEXP , Rboolean);
SEXP R_data_class2(SEXP);
char *R_LibraryFileName(const char *, char *, size_t);
SEXP R_LoadFromFile(FILE*, int);
SEXP R_NewHashedEnv(SEXP, SEXP);
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
void R_Suicide(const char *);
void R_getProcTime(double *data);
int R_isMissing(SEXP symbol, SEXP rho);
const char *sexptype2char(SEXPTYPE type);
void sortVector(SEXP, Rboolean);
void SrcrefPrompt(const char *, SEXP);
void ssort(SEXP*,int);
int StrToInternal(const char *);
SEXP strmat2intmat(SEXP, SEXP, SEXP);
SEXP substituteList(SEXP, SEXP);
unsigned int TimeToSeed(void);
SEXP tspgets(SEXP, SEXP);
SEXP type2symbol(SEXPTYPE);
void unbindVar(SEXP, SEXP);
#ifdef ALLOW_OLD_SAVE
void unmarkPhase(void);
#endif
#ifdef ADJUST_ENVIR_REFCNTS
void unpromiseArgs(SEXP);
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
void NORET R_JumpToContext(RCNTXT *, int, SEXP);
SEXP R_syscall(int,RCNTXT*);
int R_sysparent(int,RCNTXT*);
SEXP R_sysframe(int,RCNTXT*);
SEXP R_sysfunction(int,RCNTXT*);

void R_run_onexits(RCNTXT *);
void NORET R_jumpctxt(RCNTXT *, int, SEXP);
#endif

/* ../main/bind.c */
SEXP ItemName(SEXP, R_xlen_t);

/* ../main/errors.c : */
void NORET errorcall_cpy(SEXP, const char *, ...);
void NORET ErrorMessage(SEXP, int, ...);
void WarningMessage(SEXP, R_WARNING, ...);
SEXP R_GetTraceback(int);

R_size_t R_GetMaxVSize(void);
void R_SetMaxVSize(R_size_t);
R_size_t R_GetMaxNSize(void);
void R_SetMaxNSize(R_size_t);
R_size_t R_Decode2Long(char *p, int *ierr);
void R_SetPPSize(R_size_t);

/* ../main/devices.c, used in memory.c, gnuwin32/extra.c */
#define R_MaxDevices 64

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


/* main/sort.c */
void orderVector1(int *indx, int n, SEXP key, Rboolean nalast,
		  Rboolean decreasing, SEXP rho);

/* main/subset.c */
SEXP R_subset3_dflt(SEXP, SEXP, SEXP);

/* main/subassign.c */
SEXP R_subassign3_dflt(SEXP, SEXP, SEXP, SEXP);

#include <wchar.h>

/* main/util.c */
void NORET UNIMPLEMENTED_TYPE(const char *s, SEXP x);
void NORET UNIMPLEMENTED_TYPEt(const char *s, SEXPTYPE t);
Rboolean Rf_strIsASCII(const char *str);
int utf8clen(char c);
int Rf_AdobeSymbol2ucs2(int n);
double R_strtod5(const char *str, char **endptr, char dec,
		 Rboolean NA, int exact);

typedef unsigned short ucs2_t;
size_t mbcsToUcs2(const char *in, ucs2_t *out, int nout, int enc);
/* size_t mbcsMblen(char *in);
size_t ucs2ToMbcs(ucs2_t *in, char *out);
size_t ucs2Mblen(ucs2_t *in); */
size_t utf8toucs(wchar_t *wc, const char *s);
size_t utf8towcs(wchar_t *wc, const char *s, size_t n);
size_t ucstomb(char *s, const unsigned int wc);
size_t ucstoutf8(char *s, const unsigned int wc);
size_t mbtoucs(unsigned int *wc, const char *s, size_t n);
size_t wcstoutf8(char *s, const wchar_t *wc, size_t n);

SEXP Rf_installTrChar(SEXP);

const wchar_t *wtransChar(SEXP x); /* from sysutils.c */

#define mbs_init(x) memset(x, 0, sizeof(mbstate_t))
size_t Mbrtowc(wchar_t *wc, const char *s, size_t n, mbstate_t *ps);
Rboolean mbcsValid(const char *str);
char *mbcsTruncateToValid(char *s);
Rboolean utf8Valid(const char *str);
char *Rf_strchr(const char *s, int c);
char *Rf_strrchr(const char *s, int c);

SEXP fixup_NaRm(SEXP args); /* summary.c */
void invalidate_cached_recodings(void);  /* from sysutils.c */
void resetICUcollator(Rboolean disable); /* from util.c */
void dt_invalidate_locale(); /* from Rstrptime.h */
extern int R_OutputCon; /* from connections.c */
extern int R_InitReadItemDepth, R_ReadItemDepth; /* from serialize.c */
void get_current_mem(size_t *,size_t *,size_t *); /* from memory.c */
unsigned long get_duplicate_counter(void);  /* from duplicate.c */
void reset_duplicate_counter(void);  /* from duplicate.c */
void BindDomain(char *); /* from main.c */
extern Rboolean LoadInitFile;  /* from startup.c */

// Unix and Windows versions
double R_getClockIncrement(void);
void R_getProcTime(double *data);
void InitDynload(void);
void R_CleanTempDir(void);

#ifdef Win32
void R_fixslash(char *s);
void R_fixbackslash(char *s);
wchar_t *filenameToWchar(const SEXP fn, const Rboolean expand);

#if defined(SUPPORT_UTF8_WIN32)
#define mbrtowc(a,b,c,d) Rmbrtowc(a,b)
#define wcrtomb(a,b,c) Rwcrtomb(a,b)
#define mbstowcs(a,b,c) Rmbstowcs(a,b,c)
#define wcstombs(a,b,c) Rwcstombs(a,b,c)
size_t Rmbrtowc(wchar_t *wc, const char *s);
size_t Rwcrtomb(char *s, const wchar_t wc);
size_t Rmbstowcs(wchar_t *wc, const char *s, size_t n);
size_t Rwcstombs(char *s, const wchar_t *wc, size_t n);
#endif
#endif

FILE *RC_fopen(const SEXP fn, const char *mode, const Rboolean expand);
int Seql(SEXP a, SEXP b);
int Scollate(SEXP a, SEXP b);

double R_strtod4(const char *str, char **endptr, char dec, Rboolean NA);
double R_strtod(const char *str, char **endptr);
double R_atof(const char *str);

/* unix/sys-std.c, main/options.c */
void set_rl_word_breaks(const char *str);

/* From localecharset.c */
extern const char *locale2charset(const char *);

/* Localization */

#ifndef NO_NLS
# ifdef ENABLE_NLS
#  include <libintl.h>
#  ifdef Win32
#   define _(String) libintl_gettext (String)
#   undef gettext /* needed for graphapp */
#  else
#   define _(String) gettext (String)
#  endif
#  define gettext_noop(String) String
#  define N_(String) gettext_noop (String)
#  else /* not NLS */
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

#endif /* DEFN_H_ */
/*
 *- Local Variables:
 *- page-delimiter: "^/\\*---"
 *- End:
 */
