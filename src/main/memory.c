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

#include "Defn.h"

/*      MEMORY MANAGEMENT
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
 *	exits.  This can be done with the functions getvmax and
 *	setvmax.
 */

static int gc_reporting = 0;

void installIntVector(SEXP, int, FILE *);

SEXP do_gcinfo(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int i;
	checkArity(op, args);
	i = asLogical(CAR(args));
	if (i != NA_LOGICAL)
		gc_reporting = i;
	return R_NilValue;
}

SEXP do_gc(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	checkArity(op, args);
	gc_reporting = 1;
	gc();
	gc_reporting = 0;
	return R_NilValue;
}


#ifdef Macintosh
Handle  gStackH;
Handle  gNHeapH;
Handle  gVHeapH;

/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	CleanUpRMemory

	This routine releases the memory that R has allocated.
	This is only needed for the Mac because the memory
	is in system memory so not naturally cleaned up at the
	end of the application execution.

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/
 
void CleanUpMemory( void )
{
	OSErr   result;

	if( gStackH != nil )
		TempDisposeHandle( gStackH, &result);
	if( gNHeapH != nil )
		TempDisposeHandle( gNHeapH, &result );
	if( gVHeapH != nil )
		TempDisposeHandle( gVHeapH, &result );
}

#endif


/*
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	InitMemory

	Initialise the memory to be used in R:

	- stack space
	- node space
	- vector space

	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/

void InitMemory()
{
	int i;

#ifdef Macintosh
	OSErr   result;

	gStackH = TempNewHandle( R_PPStackSize * sizeof(SEXP), &result );
	if( (gStackH == NULL) || (result != noErr) )
		suicide( "couldn't allocate system memory for pointer stack" );
	TempHLock( gStackH, &result );
	R_PPStack = (SEXP*)*gStackH;
#else  
	if (!(R_PPStack = (SEXP *) malloc(R_PPStackSize * sizeof(SEXP))))
		suicide("couldn't allocate memory for pointer stack");
#endif
     
	R_PPStackTop = 0;

#ifdef Macintosh
	gNHeapH = TempNewHandle( R_NSize * sizeof(SEXPREC), &result );
	if( (gNHeapH == NULL) || (result != noErr) )
		suicide( "couldn't allocate system memory for node heap" );
	TempHLock( gNHeapH, &result );
	R_NHeap = (SEXPREC *)*gNHeapH;
#else
	if (!(R_NHeap = (SEXPREC *) malloc(R_NSize * sizeof(SEXPREC))))
		suicide("couldn't allocate memory for node heap");
#endif
     
	R_VSize = (((R_VSize + 1)/ sizeof(VECREC)));

#ifdef Macintosh
	gVHeapH = TempNewHandle( R_VSize * sizeof(VECREC), &result );
	if( (gVHeapH == NULL) || (result != noErr) )
		suicide( "couldn't allocate system memory for vector heap" );
	TempHLock( gVHeapH, &result );
	R_VHeap = (VECREC *)*gVHeapH;
#else
#ifdef DEBUGGING
	printf("R_VSize = %d malloc-ed\n", R_VSize * sizeof(VECREC));
#endif
	if (!(R_VHeap = (VECREC *) malloc(R_VSize * sizeof(VECREC))))
		suicide("couldn't allocate memory for vector heap");
#endif
     
	R_VTop = &R_VHeap[0];
	R_VMax = &R_VHeap[R_VSize - 1];

	for (i = 0; i < R_NSize - 1; i++)
		CDR(&R_NHeap[i]) = &R_NHeap[i + 1];
	CDR(&R_NHeap[R_NSize - 1]) = NULL;
	R_FreeSEXP = &R_NHeap[0];
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
	unsigned int size = INT2VEC(nelem * eltsize);
	if (size != 0) {
		if (R_VMax - R_VTop < size) {
			gc();
			if (R_VMax - R_VTop < size)
				error("memory exhausted\n");
		}
		R_VMax -= size;
	}
	return (char*) R_VMax;
}

/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
	unsigned int i, size  = INT2VEC(nelem * eltsize);
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
	return q;
}

	/* allocSExp - get SEXPREC from free list; call gc if necessary */

SEXP allocSExp(SEXPTYPE t)
{
	SEXP s;

	if (R_FreeSEXP == NULL) {
		gc();
		if (R_FreeSEXP == NULL)
			error("memory exhausted\n");
	}
	s = R_FreeSEXP;
	R_FreeSEXP = CDR(s);
	CAR(s) = R_NilValue;
	CDR(s) = R_NilValue;
	TAG(s) = R_NilValue;
	ATTRIB(s) = R_NilValue;
#ifdef oldmem
	NAMED(s) = 0;
	OBJECT(s) = 0;
	LEVELS(s) = 0;
	DEBUG(s) = 0;
	TRACE(s) = 0;
#else
	*(int *) (&(s)->sxpinfo) = 0;
#endif
	TYPEOF(s) = t;
	return s;
}


	/* allocString - allocate a string on the (vector) heap */
	/* all vector objects must be a multiple of sizeof(ALIGN) */
	/* bytes so that alignment is preserved for all objects */

SEXP allocString(int length)
{
	SEXP s;
	long size;

	/* number of vector cells to allocate */
	size = 1 + BYTE2VEC(length + 1);

	/* we need to do the gc here so allocSExp doesn't! */
	if (R_FreeSEXP == NULL || R_VMax - R_VTop < size) {
		gc();
		if (R_FreeSEXP == NULL || R_VMax - R_VTop < size)
			error("memory exhausted\n");
	}

#ifdef oldmem
	s = R_FreeSEXP;
	R_FreeSEXP = CDR(s);
	TYPEOF(s) = CHARSXP;
	TAG(s) = R_NilValue;
	ATTRIB(s) = R_NilValue;
	NAMED(s) = 0;
	OBJECT(s) = 0;
#else
	s = allocSExp(CHARSXP);
#endif
	CHAR(s) = (char *) (R_VTop + 1);
	LENGTH(s) = length;
	BACKPOINTER(*R_VTop) = s;
	R_VTop += size;
	return s;
}


	/* allocVector - allocate a vector object on the heap */

SEXP allocVector(SEXPTYPE type, int length)
{
	SEXP s;
	int i;
	long size;

	if (length < 0 )
		errorcall(R_GlobalContext->call,
			"negative length vectors are not allowed\n");

	/* number of vector cells to allocate */
	switch (type) {
	case NILSXP:
		return R_NilValue;
	case CHARSXP:
		size = 1 + BYTE2VEC(length + 1);
		break;
	case LGLSXP:
	case INTSXP:
	case FACTSXP:
	case ORDSXP:
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
#ifdef COMPLEX_DATA
	case CPLXSXP:
		if (length <= 0)
			size = 0;
		else
			size = 1 + COMPLEX2VEC(length);
		break;
#endif
	case STRSXP:
	case EXPRSXP:
	case VECSXP:
		if (length <= 0)
			size = 0;
		else
			size = 1 + PTR2VEC(length);
		break;
	case LISTSXP:
		return allocList(length);
	default:
		error("invalid type/length (%d/%d) in vector allocation\n", type, length);
	}

	/* we need to do the gc here so allocSExp doesn't! */
	if (R_FreeSEXP == NULL || R_VMax - R_VTop < size) {
		gc();
		if (R_FreeSEXP == NULL || R_VMax - R_VTop < size)
			error("memory exhausted\n");
	}

#ifdef oldmem
	s = R_FreeSEXP;
	R_FreeSEXP = CDR(s);
	TYPEOF(s) = type;
#else
	s = allocSExp(type);
#endif
	LENGTH(s) = length;
	NAMED(s) = 0;
	OBJECT(s) = 0;
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

	if (type == STRSXP || type == EXPRSXP || type == VECSXP) {
		for (i = 0; i < length; i++)
			STRING(s)[i] = R_NilValue;
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
#ifdef oldmem
		TAG(result) = R_NilValue;
#endif
	}
	return result;
}




	/* gc - mark-sweep garbage collector */

void gc(void)
{
	int vcells, vfrac;
	if (gc_reporting)
		REprintf("Garbage collection ...");
	unmarkPhase();
	markPhase();
	compactPhase();
	scanPhase();
	if (gc_reporting) {
		REprintf("\n%ld cons cells free (%ld%%)\n",
			 R_Collected, (100 * R_Collected / R_NSize));
		vcells = R_VSize - (R_VTop - R_VHeap);
		vfrac = 100 * vcells / R_VSize;
		REprintf("%ldk bytes of heap free (%ld%%)\n",
			 vcells * sizeof(VECREC) / 1024, vfrac);
	}
}


	/* unmarkPhase - reset mark in ALL cons cells */

void unmarkPhase(void)
{
	int i;

	for (i = 0; i < R_NSize; i++)
		MARK(&R_NHeap[i]) = 0;
}


/* markPhase - set mark in all accessible cons cells */
void markPhase(void)
{
	int i;

	markSExp(R_NilValue);	/* Builtin constants */
	markSExp(NA_STRING);
	markSExp(R_UnboundValue);
	markSExp(R_MissingArg);
	markSExp(R_CommentSxp);

	markSExp(R_GlobalEnv);	/* Global environent */

	for (i = 0; i < HSIZE; i++)	/* Symbol table */
		markSExp(R_SymbolTable[i]);

	if (R_CurrentExpr != NULL)	/* Current expression */
		markSExp(R_CurrentExpr);

	for (i = 0; i < R_PPStackTop; i++)	/* protected pointers */
		markSExp(R_PPStack[i]);
}


/* markSExp - set mark in s and all cells accessible from it */
void markSExp(SEXP s)
{
	int i;

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
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
		case REALSXP:
#ifdef COMPLEX_DATA
		case CPLXSXP:
#endif
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
			break;
		case CLOSXP:
		case PROMSXP:
		case LISTSXP:
		case LANGSXP:
		case DOTSXP:
		case SYMSXP:
			markSExp(TAG(s));
			markSExp(CAR(s));
			markSExp(CDR(s));
			break;
		default:
			abort();
		}
	}
}


/* compactPhase - compact the vector heap */
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
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
			size = LENGTH(s) * sizeof(int);
			break;
		case REALSXP:
			size = LENGTH(s) * sizeof(double);
			break;
#ifdef COMPLEX_DATA
		case CPLXSXP:
			size = LENGTH(s) * sizeof(complex);
			break;
#endif
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


/* scanPhase - reconstruct free list from cells not marked */
void scanPhase(void)
{
	int i;

	R_FreeSEXP = NULL;
	R_Collected = 0;
	if(R_FreeSEXP == NULL)
		i=10;
	for (i = 0; i < R_NSize; i++) {
		if (!MARK(&R_NHeap[i])) {
			CDR(&R_NHeap[i]) = R_FreeSEXP;
			R_FreeSEXP = &R_NHeap[i];
#ifdef oldmem
			CAR(&R_NHeap[i]) = R_NilValue;
			TAG(&R_NHeap[i]) = R_NilValue;
			ATTRIB(&R_NHeap[i]) = R_NilValue;
#endif
			R_Collected = R_Collected + 1;
		}
	}
}


/* protect - push a single argument onto R_PPStack */
void protect(SEXP s)
{
	if (R_PPStackTop >= R_PPStackSize)
		error("stack overflow\n");
	R_PPStack[R_PPStackTop] = s;
	R_PPStackTop = R_PPStackTop + 1;
}


/* unprotect - pop argument list from top of R_PPStack */
void unprotect(int l)
{
	if (R_PPStackTop > 0)
		R_PPStackTop = R_PPStackTop - l;
	else
		error("stack imbalance in \"unprotect\"\n");
}


/* initStack - initialize environment stack */
void initStack(void)
{
	R_PPStackTop = 0;
}
