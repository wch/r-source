/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--2000  The R Development Core Team.
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
 *
 *
 *      MEMORY MANAGEMENT
 *
 *	Separate areas are maintained for fixed and variable sized
 *	objects.  The first of these is allocated as an array of
 *	SEXPRECs and the second as an array of VECRECs.  The fixed
 *	sized objects are assembled into a free list and cons cells
 *	are allocated from it.  When the list is exhausted, a
 *	mark-sweep garbarge collection takes place and the free list
 *	is rebuilt.  Variable size objects are allocated in the VECREC
 *	area.  During a garbage collection, these are compacted to the
 *	beginning of the VECREC array.
 *
 *	The top end of the VECREC array is also used by R_alloc to
 *	maintain a stack of non-relocatable memory blocks.  These are
 *	used in calls to .Fortran and .C (and for other temporary
 *	purposes).  They are freed using a stack discipline.
 *
 *	+---------------------------------------------------+
 *	| allocated vectors |	free	| R-alloc'ed blocks |
 *	+---------------------------------------------------+
 *	                    ^           ^
 *	                    |           |
 *	                    R_VTop      R_VMax
 *
 *	If a piece of code R-allocs some blocks, it is required to
 *	reset the R_VMax pointer back to its original value before it
 *	exits.  This can be done with the functions vmaxget and
 *	vmaxset.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

/* #include "Memory.h" included in Defn.h */
#include "Defn.h"
#include "Graphics.h"

static int gc_reporting = 0;
static int gc_count = 0;

#define GC_TORTURE
/*
*/

#ifdef GC_TORTURE
#define FORCE_GC !gc_inhibit_torture
#else
#define FORCE_GC 0
#endif

extern SEXP framenames;

#define GC_PROT(X) {int __t = gc_inhibit_torture; \
	gc_inhibit_torture = 1 ; X ; gc_inhibit_torture = __t;}

void installIntVector(SEXP, int, FILE *);


SEXP do_gctorture(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = allocVector(LGLSXP, 1);

    checkArity(op, args);
    i = asLogical(CAR(args));
    LOGICAL(old)[0] = gc_reporting;
    if (i != NA_LOGICAL)
	gc_inhibit_torture = !i;
    return old;
}

SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP old = allocVector(LGLSXP, 1);

    checkArity(op, args);
    i = asLogical(CAR(args));
    LOGICAL(old)[0] = gc_reporting;
    if (i != NA_LOGICAL)
	gc_reporting = i;
    return old;
}

SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP value;
    int ogc;
    checkArity(op, args);
    ogc = gc_reporting;
    gc_reporting = asLogical(CAR(args));
    R_gc();
    gc_reporting = ogc;
    /*- now return the [free , total ] for cells and heap */
    PROTECT(value = allocVector(INTSXP, 6));
    INTEGER(value)[0] = R_Collected;
    INTEGER(value)[1] = (int)(R_VSize - (R_VTop - R_VHeap));
    INTEGER(value)[2] = R_NSize;
    INTEGER(value)[3] = R_VSize;
    /* next two are in 0.1Mb, rounded up */
    INTEGER(value)[4] = 10.0 * R_NSize/1048576.0 * sizeof(SEXPREC) + 0.999;
    INTEGER(value)[5] = 10.0 * R_VSize/131072.0 + 0.999;
    UNPROTECT(1);
    return value;
}


void mem_err_heap(long size)
{
    errorcall(R_NilValue, "heap memory (%ld Kb) exhausted [needed %ld Kb more]\n       See \"help(Memory)\" on how to increase the heap size.",
	  (R_VSize * sizeof(VECREC))/1024,
  (size * sizeof(VECREC))/1024);
}


void mem_err_cons()
{
    errorcall(R_NilValue, "cons memory (%ld cells) exhausted\n       See \"help(Memory)\" on how to increase the number of cons cells.", R_NSize);
}

/* InitMemory : Initialise the memory to be used in R. */
/* This includes: stack space, node space and vector space */

void InitMemory()
{
    int i;

    gc_reporting = R_Verbose;
    if (!(R_PPStack = (SEXP *) malloc(R_PPStackSize * sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for pointer stack");
    R_PPStackTop = 0;

    if (!(R_NHeap = (SEXPREC *) malloc(R_NSize * sizeof(SEXPREC))))
	R_Suicide("couldn't allocate memory for node heap");

    R_VSize = (((R_VSize + 1)/ sizeof(VECREC)));

    if (!(R_VHeap = (VECREC *) malloc(R_VSize * sizeof(VECREC))))
	R_Suicide("couldn't allocate memory for vector heap");

    R_VTop = &R_VHeap[0];
    R_VMax = &R_VHeap[R_VSize - 1];

    for (i = 0; i < R_NSize - 1; i++)
	CDR(&R_NHeap[i]) = &R_NHeap[i + 1];
    CDR(&R_NHeap[R_NSize - 1]) = NULL;
    R_FreeSEXP = &R_NHeap[0];

/* setting framenames and  R_PreciousList moved to InitNames after
   R_NilValue is allocated  */

    /* unmark all nodes to preserve the invariant */
    /* not really needed as long as allocSExp unmarks on allocation */
    unmarkPhase();
}


char *vmaxget(void)
{
    return (char *) R_VMax;
}

void vmaxset(char *ovmax)
{
    if (ovmax) R_VMax = (VECREC *) ovmax;
    else R_VMax = &R_VHeap[R_VSize - 1];
}

char *R_alloc(long nelem, int eltsize)
{
    unsigned int size = BYTE2VEC(nelem * eltsize);
    if (size > 0) {
	if (FORCE_GC || R_VMax - R_VTop < size) {
	    R_gc();
	    if (R_VMax - R_VTop < size)
		mem_err_heap(size);
	}
	R_VMax -= size;
        return (char*) R_VMax;
    }
    else return NULL;
}

/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
    unsigned int i, size  = nelem * eltsize;
    char *p = R_alloc(nelem, eltsize);
    for(i=0 ; i<size; i++)
	p[i] = 0;
    return p;
}


char *S_realloc(char *p, long new, long old, int size)
{
    int i, nold;
    char *q;
    /* shrinking is a no-op */
    if(new <= old) return p;
    q = R_alloc(new, size);
    nold = old * size;
    for(i=0 ; i<nold ; i++)
	q[i] = p[i];
    for(i=nold; i < new*size; i++)
	q[i] = 0;
    return q;
}

/* "allocSExp" allocate a SEXPREC from free list */
/* call gc if necessary */

SEXP allocSExp(SEXPTYPE t)
{
    SEXP s;
    if (FORCE_GC || R_FreeSEXP == NULL) {
	R_gc();
	if (R_FreeSEXP == NULL)
	    mem_err_cons();
    }
    s = R_FreeSEXP;
    R_FreeSEXP = CDR(s);
    CAR(s) = R_NilValue;
    CDR(s) = R_NilValue;
    TAG(s) = R_NilValue;
    ATTRIB(s) = R_NilValue;
    *(int *) (&(s)->sxpinfo) = 0;
    TYPEOF(s) = t;
    return s;
}

/* "allocString" allocate a string on the (vector) heap. */
/* All vector objects  must be a multiple of sizeof(ALIGN) */
/* bytes so that alignment is preserved for all objects */

SEXP allocString(int length)
{
    SEXP s;
    long size;
    /* number of vector cells to allocate */
    size = 1 + BYTE2VEC(length + 1);
    /* we need to do the gc here so allocSExp doesn't! */
    if (FORCE_GC || R_FreeSEXP == NULL || R_VMax - R_VTop < size) {
	R_gc();
	if (R_FreeSEXP == NULL)
	    mem_err_cons();
	if (R_VMax - R_VTop < size)
	    mem_err_heap(size);
    }
    GC_PROT(s = allocSExp(CHARSXP));
    CHAR(s) = (char *) (R_VTop + 1);
    LENGTH(s) = length;
    BACKPOINTER(*R_VTop) = s;
    R_VTop += size;
    return s;
}


/* Allocate a vector object on the heap */

SEXP allocVector(SEXPTYPE type, int length)
{
    SEXP s;
    int i;
    long size=0;
    if (length < 0 )
	errorcall(R_GlobalContext->call,
		  "negative length vectors are not allowed");
    /* number of vector cells to allocate */
    switch (type) {
    case NILSXP:
	return R_NilValue;
    case CHARSXP:
	size = 1 + BYTE2VEC(length + 1);
	break;
    case LGLSXP:
    case INTSXP:
	if (length <= 0)
	    size = 0;
	else
	    size = 1 + INT2VEC(length);
	break;
    case REALSXP:
	if (length <= 0)
	    size = 0;
	else
	    size = 1 + FLOAT2VEC(length);
	break;
    case CPLXSXP:
	if (length <= 0)
	    size = 0;
	else
	    size = 1 + COMPLEX2VEC(length);
	break;
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	if (length <= 0)
	    size = 0;
	else
	    size = 1 + PTR2VEC(length);
	break;
    case LANGSXP:
	if(length == 0) return R_NilValue;
	s = allocList(length);
	TYPEOF(s) = LANGSXP;
	return s;
    case LISTSXP:
	return allocList(length);
    default:
	error("invalid type/length (%d/%d) in vector allocation", type, length);
    }
    /* we need to do the gc here so allocSExp doesn't! */
    if (FORCE_GC || R_FreeSEXP == NULL || R_VMax - R_VTop < size) {
	R_gc();
	if (R_FreeSEXP == NULL)
	    mem_err_cons();
	if (R_VMax - R_VTop < size)
	    mem_err_heap(size);
    }
    GC_PROT(s = allocSExp(type));

    LENGTH(s) = length;
    NAMED(s) = 0;
    ATTRIB(s) = R_NilValue;
    if (size > 0) {
	CHAR(s) = (char *) (R_VTop + 1);
	BACKPOINTER(*R_VTop) = s;
	R_VTop += size;
    }
    else
	CHAR(s) = (char*)0;
    /* The following prevents disaster in the case */
    /* that an uninitialised string vector is marked */
    if (type == EXPRSXP || type == VECSXP) {
	for (i = 0; i < length; i++)
	    STRING(s)[i] = R_NilValue;
    }
    else if(type == STRSXP) {
	for (i = 0; i < length; i++)
	    STRING(s)[i] = R_BlankString;
    }
    return s;
}

SEXP allocList(int n)
{
    int i;
    SEXP result;
    result = R_NilValue;
    for (i = 0; i < n; i++) {
	result = CONS(R_NilValue, result);
    }
    return result;
}

/* "gc" a mark-sweep garbage collector */

void R_gc(void)
{
    int vcells;
    double vfrac;

    gc_count++;
    if (gc_reporting)
	REprintf("Garbage collection [nr. %d]...", gc_count);

    BEGIN_SUSPEND_INTERRUPTS {
      /* unmarkPhase(); */ 
      markPhase();
      compactPhase();
      scanPhase();
    } END_SUSPEND_INTERRUPTS;

    if (gc_reporting) {
	REprintf("\n%ld cons cells free (%ld%%)\n",
		 R_Collected, (100 * R_Collected / R_NSize));
	vcells = R_VSize - (R_VTop - R_VHeap);
	vfrac = (100.0 * vcells) / R_VSize;
	REprintf("%ld Kbytes of heap free (%.0f%%)\n",
		 vcells * sizeof(VECREC) / 1024, vfrac);
    }
}


SEXP do_memoryprofile(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nms;
    int i;

    PROTECT(ans = allocVector(INTSXP, 21));
    PROTECT(nms = allocVector(STRSXP, 21));
    for (i = 0; i < 21; i++) {
        INTEGER(ans)[i] = 0;
        STRING(nms)[i] = R_BlankString;
    }
    STRING(nms)[NILSXP]     = mkChar("NILSXP");
    STRING(nms)[SYMSXP]     = mkChar("SYMSXP");
    STRING(nms)[LISTSXP]    = mkChar("LISTSXP");
    STRING(nms)[CLOSXP]     = mkChar("CLOSXP");
    STRING(nms)[ENVSXP]     = mkChar("ENVSXP");
    STRING(nms)[PROMSXP]    = mkChar("PROMSXP");
    STRING(nms)[LANGSXP]    = mkChar("LANGSXP");
    STRING(nms)[SPECIALSXP] = mkChar("SPECIALSXP");
    STRING(nms)[BUILTINSXP] = mkChar("BUILTINSXP");
    STRING(nms)[CHARSXP]    = mkChar("CHARSXP");
    STRING(nms)[LGLSXP]     = mkChar("LGLSXP");
    STRING(nms)[INTSXP]     = mkChar("INTSXP");
    STRING(nms)[REALSXP]    = mkChar("REALSXP");
    STRING(nms)[CPLXSXP]    = mkChar("CPLXSXP");
    STRING(nms)[STRSXP]     = mkChar("STRSXP");
    STRING(nms)[DOTSXP]     = mkChar("DOTSXP");
    STRING(nms)[ANYSXP]     = mkChar("ANYSXP");
    STRING(nms)[VECSXP]     = mkChar("VECSXP");
    STRING(nms)[EXPRSXP]    = mkChar("EXPRSXP");
    setAttrib(ans, R_NamesSymbol, nms);

    BEGIN_SUSPEND_INTERRUPTS {
    markPhase();
    for (i = 0; i < R_NSize; i++)
	if(MARK(&R_NHeap[i]))
            INTEGER(ans)[TYPEOF(&R_NHeap[i])] += 1;
    unmarkPhase(); /* could be done smarter */
    } END_SUSPEND_INTERRUPTS;
    UNPROTECT(2);
    return ans;
}

/* "unmarkPhase" reset mark in ALL cons cells */

void unmarkPhase(void)
{
    int i; SEXP p = R_NHeap;
    for (i = R_NSize; i-- ; )
	MARK(p++) = 0;
}


/* "markPhase" set mark in all accessible cons cells */

void markPhase(void)
{
    int i;
    DevDesc *dd;
    RCNTXT * ctxt;

    markSExp(R_NilValue);	           /* Builtin constants */
    markSExp(NA_STRING);
    markSExp(R_BlankString);
    markSExp(R_UnboundValue);
    markSExp(R_MissingArg);
    markSExp(R_CommentSxp);

    markSExp(R_GlobalEnv);	           /* Global environment */
    markSExp(R_Warnings);	           /* Warnings, if any */

    for (i = 0; i < HSIZE; i++)	           /* Symbol table */
	markSExp(R_SymbolTable[i]);

    if (R_CurrentExpr != NULL)	           /* Current expression */
	markSExp(R_CurrentExpr);

    for (i = 0; i < R_MaxDevices; i++) {   /* Device display lists */
	dd = GetDevice(i);
	if (dd)
	    markSExp(dd->displayList);
    }

    for (ctxt = R_GlobalContext ; ctxt != NULL ; ctxt = ctxt->nextcontext)
	markSExp(ctxt->conexit);           /* on.exit expressions */

    markSExp(framenames); 		   /* used for interprocedure
					    communication in model.c */

    markSExp(R_PreciousList);

    for (i = 0; i < R_PPStackTop; i++)	   /* Protected pointers */
	markSExp(R_PPStack[i]);
}


/* "markSExp" set mark in s and all cells accessible from it */

void markSExp(SEXP s)
{
    int i;
    
 tailcall_entry:
    if (s && !MARK(s)) {
	MARK(s) = 1;
	if (ATTRIB(s) != R_NilValue)
	    markSExp(ATTRIB(s));
	switch (TYPEOF(s)) {
	case NILSXP:
	case BUILTINSXP:
	case SPECIALSXP:
	case CHARSXP:
	case LGLSXP:
	case INTSXP:
	case REALSXP:
	case CPLXSXP:
	    break;
	case STRSXP:
	case EXPRSXP:
	case VECSXP:
	    for (i = 0; i < LENGTH(s); i++)
		markSExp(STRING(s)[i]);
	    break;
	case ENVSXP:
	    markSExp(FRAME(s));
	    markSExp(ENCLOS(s));
	    markSExp(HASHTAB(s));
	    break;
	case CLOSXP:
	case PROMSXP:
	case LISTSXP:
	case LANGSXP:
	case DOTSXP:
	case SYMSXP:
	    markSExp(TAG(s));
	    markSExp(CAR(s));
	    /* use parameterized jump (i.e. assignment+goto) instead of
	       recursive tail call markSExp(CDR(s)) to reduce stack use */
	    s = CDR(s);
	    goto tailcall_entry;
	default:
	    abort();
	}
    }
}


/* "compactPhase" compact the vector heap */

void compactPhase(void)
{
    VECREC *vto, *vfrom;
    SEXP s;
    int i, size;
    vto = vfrom = R_VHeap;
    while (vfrom < R_VTop) {
	s = BACKPOINTER(*vfrom);
	switch (TYPEOF(s)) {	/* get size in bytes */
	case CHARSXP:
	    size = LENGTH(s) + 1;
	    break;
	case LGLSXP:
	case INTSXP:
	    size = LENGTH(s) * sizeof(int);
	    break;
	case REALSXP:
	    size = LENGTH(s) * sizeof(double);
	    break;
	case CPLXSXP:
	    size = LENGTH(s) * sizeof(Rcomplex);
	    break;
	case STRSXP:
	case EXPRSXP:
	case VECSXP:
	    size = LENGTH(s) * sizeof(SEXP);
	    break;
	default:
	    abort();
	}
	size = 1 + BYTE2VEC(size);
	if (MARK(s)) {
#if 0 /* help debug heap problems */
	    if (CHAR(s) != (char *) (vfrom + 1))
		error("inconsistency during heap compaction");
#endif
	    if (vfrom != vto) {
		for (i = 0; i < size; i++)
		    vto[i] = vfrom[i];
	    }
	    CHAR(BACKPOINTER(*vto)) = (char *) (vto + 1);
	    vto += size;
	    vfrom += size;
	}
	else {
	    vfrom += size;
	}
    }
    R_VTop = vto;
}


/* "scanPhase" reconstruct free list from cells not marked */

void scanPhase(void)
{
    register int i;
    register SEXP p = R_NHeap, tmp = NULL;

    tmp = NULL;
    R_Collected = 0;
    for (i = R_NSize; i--; ) {
	if (!MARK(p)) {
	    /* Call Destructors Here */
	    CDR(p) = tmp;
	    tmp = p++;
	    R_Collected++;
	} else {
            MARK(p++) = 0;
        }
    }
    R_FreeSEXP = tmp;
}


/* "protect" push a single argument onto R_PPStack */

/* In handling a stack overflow we have to be careful not to 
   use PROTECT. error("protect(): stack overflow") would call
   deparse1, which uses PROTECT and segfaults */
   
SEXP protect(SEXP s)
{
    if (R_PPStackTop >= R_PPStackSize)
	errorcall(R_NilValue,"protect(): stack overflow");
    R_PPStack[R_PPStackTop++] = s;
    return s;
}


/* "unprotect" pop argument list from top of R_PPStack */

void unprotect(int l)
{
    if (R_PPStackTop >=  l)
	R_PPStackTop -= l;
    else
	error("unprotect(): stack imbalance");
}

/* "unprotect_ptr" remove pointer from somewhere in R_PPStack */

void unprotect_ptr(SEXP s)
{
    int i = R_PPStackTop;

    /* go look for  s  in  R_PPStack */
    /* (should be among the top few items) */
    do {
    	if (i == 0)
	    error("unprotect_ptr: pointer not found");
    } while ( R_PPStack[--i] != s );

    /* OK, got it, and  i  is indexing its location */
    /* Now drop stack above it */

    do {
    	R_PPStack[i] = R_PPStack[i + 1];
    } while ( i++ < R_PPStackTop );

    R_PPStackTop--;
}


/* "initStack" initialize environment stack */
void initStack(void)
{
    R_PPStackTop = 0;
}


/* Wrappers for malloc/alloc/free */
/* These allow automatic freeing of malloc-ed */
/* blocks during error recovery. */

#define MAXPOINTERS 100
static char *C_Pointers[MAXPOINTERS];

void Init_C_alloc()
{
    int i;
    for(i=0 ; i<MAXPOINTERS ; i++)
	C_Pointers[i] = NULL;
}

void Reset_C_alloc()
{
    int i;
    for(i=0 ; i<MAXPOINTERS ; i++) {
	if(C_Pointers[i] != NULL)
	    free(C_Pointers[i]);
	C_Pointers[i] = NULL;
    }
}

char *C_alloc(long nelem, int eltsize)
{
    int i;
    for(i=0 ; i<MAXPOINTERS ; i++) {
	if(C_Pointers[i] == NULL) {
	    C_Pointers[i] = malloc(nelem * eltsize);
	    if(C_Pointers[i] == NULL)
		error("C_alloc(): unable to malloc memory");
	    else return C_Pointers[i];
	}
    }
    error("C_alloc(): all pointers in use (sorry)");
    /*-Wall:*/return C_Pointers[0];
}

void C_free(char *p)
{
    int i;
    for(i=0 ; i<MAXPOINTERS ; i++) {
	if(C_Pointers[i] == p) {
	    free(p);
	    C_Pointers[i] = NULL;
	    return;
	}
    }
    error("C_free(): attempt to free pointer not allocated by C_alloc()");
}

/* S-like wrappers for calloc, realloc and free that check for error
   conditions */

void *R_chk_calloc(size_t nelem, size_t elsize)
{
    void *p;
#ifdef CALLOC_BROKEN
    if(nelem == 0)
	return(NULL);
#endif
    p = calloc(nelem, elsize);
    if(!p) error("Calloc could not allocate memory");
    return(p);
}
void *R_chk_realloc(void *ptr, size_t size)
{
    void *p;
    p = realloc(ptr, size);
    if(!p) error("Realloc could not re-allocate memory");
    return(p);
}
void R_chk_free(void *ptr)
{
    /* S-PLUS warns here, but there seems no reason to do so */
    /* if(!ptr) warning("attempt to free NULL pointer by Free"); */
    if(ptr) free(ptr); /* ANSI C says free has no effect on NULL, but
			  better to be safe here */
}

/* This code keeps a list of objects which are not assigned to variables
   but which are required to persist across garbage collections.  The
   objects are registered with R_PreserveObject and deregistered with
   R_UnpreserveObject.  This is experimental code, it would not be wise
   to rely on it at this point - ihaka */

void R_PreserveObject(SEXP object)
{
    R_PreciousList = CONS(object, R_PreciousList);
}

static SEXP RecursiveRelease(SEXP object, SEXP list)
{
    if (!isNull(list)) {
        if (object == CAR(list))
            return CDR(list);
        else
            CDR(list) = RecursiveRelease(object, CDR(list));
    }
    return list;
}

void R_ReleaseObject(SEXP object)
{
    R_PreciousList =  RecursiveRelease(object, R_PreciousList);
}
