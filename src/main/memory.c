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
 *
 *  Modified by Heiner Schwarte to allow growth of vector heap
 *  Copyright (C) 1997, Heiner Schwarte
 */

#include "Defn.h"

static int gc_reporting = 0;


#define HS_NUMS 100
VECREC*HS_Stacks[HS_NUMS];
VECREC*HS_StackPtr;

VECREC*HS_Heaps[HS_NUMS];
VECREC*HS_HeapPtrs[HS_NUMS];
int HS_HeapActive;


static int HS_StackActive(VECREC *StackPtr)
{
	int i;
	for (i = 0; HS_Stacks[i] != NULL; i++)
		if (&HS_Stacks[i][0] <= HS_StackPtr &&
		    HS_StackPtr <= &HS_Stacks[i][R_VSize-1])
			return i;
	return - 1;
}


static void HS_expandStack()
{
	int i = 0;
	while (HS_Stacks[i] != NULL)
		i++;
	HS_Stacks[i] = (VECREC * )malloc(R_VSize * sizeof(VECREC));
	if (HS_Stacks[i] == NULL)
		if (i == 0)
			suicide("couldn't allocate memory for stack");
			else
			error(" couldn't expand stack");
	return;
}


void HS_expandHeap()
{
	int i = 0;
	while (HS_Heaps[i] != NULL)
		i++;
	HS_Heaps[i] = (VECREC * )malloc(R_VSize * sizeof(VECREC));
	if (HS_Heaps[i] == NULL)
		if (i == 0)
			suicide("couldn't allocate memory for stack");
			else
			error(" couldn't expand heap");
	HS_HeapPtrs[i] = &HS_Heaps[i][0];
	return;
}


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

	if (!(R_PPStack = (SEXP * ) malloc(R_PPStackSize * sizeof(SEXP))))
		suicide("couldn't allocate memory for pointer stack");

	R_PPStackTop = 0;

	if (!(R_NHeap = (SEXPREC * ) malloc(R_NSize * sizeof(SEXPREC))))
		suicide("couldn't allocate memory for node heap");

	R_VSize = (((R_VSize + 1) / sizeof(VECREC)));


	for (i = 0; i < HS_NUMS; i++)
		HS_Stacks[i] = NULL;
	HS_expandStack();
	HS_StackPtr = &HS_Stacks[0][0];

	for (i = 0; i < HS_NUMS; i++) {
		HS_Heaps[i] = NULL;
		HS_HeapPtrs[i] = NULL;
	}
	HS_expandHeap();
	HS_HeapActive = 0;


	for (i = 0; i < R_NSize - 1; i++)
		CDR(&R_NHeap[i]) = &R_NHeap[i + 1];
	CDR(&R_NHeap[R_NSize - 1]) = NULL;
	R_FreeSEXP = &R_NHeap[0];
}


char *vmaxget(void)
{
	return (char *) HS_StackPtr;
}


void vmaxset(char *ovmax)
{
	if (ovmax) 
		HS_StackPtr = (VECREC * ) ovmax;
	else 
		HS_StackPtr = &HS_Stacks[0][0];
}


char *R_alloc(long nelem, int eltsize)
{
	int stacknum;
	VECREC * res = HS_StackPtr;
	unsigned int size = INT2VEC(nelem *eltsize);
	if (size != 0) {
		stacknum = HS_StackActive(HS_StackPtr);
		if (&HS_Stacks[stacknum][R_VSize-1] - HS_StackPtr < size) {
			if (HS_Stacks[stacknum+1] == NULL) {
				HS_expandStack();
			}
			stacknum++;
			HS_StackPtr = &HS_Stacks[stacknum][0];
			if (&HS_Stacks[stacknum][R_VSize-1] - HS_StackPtr < size)
				error("memory exhausted in R_alloc");
			res = HS_StackPtr;
		}
		HS_StackPtr += size;
	}
	return (char * ) res;
}


/* S COMPATIBILITY */

char *S_alloc(long nelem, int eltsize)
{
	unsigned int i, size  = INT2VEC(nelem *eltsize);
	char *p = R_alloc(nelem, eltsize);
	for (i = 0 ; i < size; i++)
		p[i] = 0;
	return p;
}


char *S_realloc(char *p, long new, long old, int size)
{
	int i, nold;
	char *q;

	/* shrinking is a no-op */
	if (new <= old) 
		return p;

	q = R_alloc(new, size);
	nold = old * size;
	for (i = 0 ; i < nold ; i++)
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

	if (R_FreeSEXP == NULL || 
	    &HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size) {
		gc();
		if (R_FreeSEXP == NULL)
			error("memory exhausted\n");
		if (&HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size) {
			if (HS_Heaps[HS_HeapActive+1] == NULL)
				HS_expandHeap();
			HS_HeapActive++;
		}
		if (&HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size)
			error("could not allocate memory 1: size %d", size);
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
	CHAR(s) = (char *) (HS_HeapPtrs[HS_HeapActive] + 1);
	LENGTH(s) = length;
	BACKPOINTER(*HS_HeapPtrs[HS_HeapActive]) = s;
	HS_HeapPtrs[HS_HeapActive] += size;
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
		error("invalid type/length (%d/%d) in vector allocation\n",
			type, length);
	}

	/* we need to do the gc here so allocSExp doesn't! */

	if (R_FreeSEXP == NULL || 
	    &HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size) {
		gc();
		if (R_FreeSEXP == NULL)
			error("memory exhausted\n");
		if (&HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size) {
			if (HS_Heaps[HS_HeapActive+1] == NULL)
				HS_expandHeap();
			HS_HeapActive++;
		}
		if (&HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive] < size)
			error("could not allocate memory 2 size: %d", size);
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
		CHAR(s) = (char *) (HS_HeapPtrs[HS_HeapActive] + 1);
		BACKPOINTER(*HS_HeapPtrs[HS_HeapActive]) = s;
		HS_HeapPtrs[HS_HeapActive] += size;
	} else
		CHAR(s) = (char * )0;

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
	int heapchunks;
	int i;
	int freecells;
	if (gc_reporting)
		REprintf("Garbage collection ...");
	unmarkPhase();
	markPhase();
	compactPhase();
	scanPhase();
	if (1 | gc_reporting) {
		REprintf("\n%ld cons cells free (%ld%%)\n",
		    R_Collected, (100 * R_Collected / R_NSize));
		heapchunks = 0;
		for (i = 0; HS_Heaps[i] != NULL; i++)
			heapchunks++;
		freecells = &HS_Heaps[HS_HeapActive][R_VSize-1] - HS_HeapPtrs[HS_HeapActive];
		freecells = freecells + R_VSize * (heapchunks - 1 - HS_HeapActive);
		REprintf("%ldk bytes of heap free (%ld%%)\n",
		    freecells * sizeof(VECREC) / 1024, (100 * freecells) / (heapchunks * R_VSize));
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

	markSExp(R_NilValue);			/* Builtin constants */
	markSExp(NA_STRING);
	markSExp(R_UnboundValue);
	markSExp(R_MissingArg);
	markSExp(R_CommentSxp);

	markSExp(R_GlobalEnv);			/* Global environent */

	for (i = 0; i < HSIZE; i++)		/* Symbol table */
		markSExp(R_SymbolTable[i]);

	if (R_CurrentExpr != NULL)		/* Current expression */
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
	VECREC * vto, *vfrom;
	SEXP s;
	int i, size;
	int chunkfrom;
	int chunkto = 0;

	vto = &HS_Heaps[0][0];
	for (chunkfrom = 0; chunkfrom <= HS_HeapActive; chunkfrom++) {
		vfrom = &HS_Heaps[chunkfrom][0];
		while (vfrom < HS_HeapPtrs[chunkfrom]) {
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
					if (vto + size > &HS_Heaps[chunkto][R_VSize-1]) {
						HS_HeapPtrs[chunkto] = vto;
						chunkto++;
						vto = &HS_Heaps[chunkto][0];
					}
					for (i = 0; i < size; i++)
						vto[i] = vfrom[i];
				}
				CHAR(BACKPOINTER(*vto)) = (char *) (vto + 1);
				vto += size;
				vfrom += size;
			} else {
				vfrom += size;
			}
		}
	}
	HS_HeapPtrs[chunkto] = vto;
	HS_HeapActive = chunkto;
	for (i = HS_HeapActive + 1; HS_Heaps[i] != NULL; i++)
		HS_HeapPtrs[i] = &HS_Heaps[i][0];
}


/* scanPhase - reconstruct free list from cells not marked */
void scanPhase(void)
{
	int i;

	R_FreeSEXP = NULL;
	R_Collected = 0;
	if (R_FreeSEXP == NULL)
		i = 10;
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
