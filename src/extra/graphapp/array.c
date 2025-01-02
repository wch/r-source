/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: array.c -- memory allocation functions.
 * Platform: Neutral  Version: 2.35  Date: 1998/03/04
 *
 * Version: 2.30  Changes: Original version by Lachlan Patrick.
 * Version: 2.35  Changes: Join and append improved by Jim McDonald.
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

/* R modification: mheader union (instead of simply long size) to force
   alignment of allocated data. With C11, one could use max_align_t. 

   Copyright (C) 2025  The R Core Team
*/

#include <stdlib.h>
#include "internal.h"

#ifndef array

#define create(type)  ( (type*) memalloc(sizeof(type)) )
#define array(n,type) ( (type*) memalloc(n*sizeof(type)) )
#define len(a)        ( memlength((char*)(a))/sizeof((a)[0]) )
#define element(a,i)  ( (((i)<len(a)) && ((i)>=0)) ? (a)[i] : 0 )
#define append(a,e)   ( *(char**)&(a)=memexpand((char*)(a),sizeof((a)[0])), \
				(a)[len(a)-1]=(e) )
#define shrink(a,ns)  ( *(char**)&(a)=memrealloc((char*)(a),(ns)) )
#define join(a,b)     ( *(char**)&(a)=memjoin((char*)(a),(char*)(b)) )
#define discard(a)    ( memfree((char*)(a)), (a)=0 )

char *	memalloc(long size);
char *	memrealloc(char *a, long new_size);
void	memfree(char *a);
long	memlength(char *a);
char *	memexpand(char *a, long extra);
char *	memjoin(char *a, char *b);

#endif /* array defintions */

#define TRACEAR(a)

typedef union {
    long size;

    void *dummy_ptr;
    void (*dummy_funptr)(void);
    long long dummy_ll;
    double dummy_dbl;
} mheader;

char * memalloc(long size)
{
    char *block;
    char *a;
    long i, datasize;
    TRACEAR("alloc");
    datasize = (((size + 4) >> 2) << 2);
#ifdef COMPILER
#if (COMPILER <= 16)
    if ((sizeof(mheader)+datasize) >= (1<<16))
	return NULL;
#endif
#endif
    block = (char *) malloc(sizeof(mheader) + datasize);
    if (block == NULL)
	return NULL;
    ((mheader *)block)->size = size;
    a = block + sizeof(mheader);
    for (i=0; i<datasize; i++)
	a[i] = '\0';
    return a;
}

char * memrealloc(char *a, long new_size)
{
    char *block;
    long i, size, oldsize, newsize;
    TRACEAR("realloc");
    if (new_size <= 0) {
	memfree(a);
	return NULL;
    }

    if (a == NULL) {
	block = NULL;
	size = 0;
    }
    else {
	block = a - sizeof(mheader);
	size = ((mheader *)block)->size;
    }

    oldsize = size ? (((size + 4) >> 2) << 2) : 0;
    newsize = (((new_size + 4) >> 2) << 2);

    if ( newsize != oldsize ) {
#ifdef COMPILER
#if (COMPILER <= 16)
	if ((sizeof(mheader)+newsize) >= (1<<16))
	    return NULL;
#endif
#endif
	block = (char *) realloc(block, sizeof(mheader) + newsize);
	if (block == NULL)
	    return NULL;
	a = block + sizeof(mheader);
	for (i=oldsize; i<newsize; i++)
	    a[i] = '\0';
    }

    ((mheader *)block)->size = new_size;
    return a;
}

long memlength(char *a)
{
    return (a) ? ((mheader *)(a - sizeof(mheader)))->size : 0;
}

void memfree(char *a)
{
    if (a) free(a - sizeof(mheader));
}

char * memexpand(char *a, long extra)
{
    char *block;
    long i, size, oldsize, newsize;
    TRACEAR("exp");
    if (extra == 0)
	return a;

    if (a == NULL) {
	block = NULL;
	size = 0;
    }
    else {
	block = a - sizeof(mheader);
	size = ((mheader *)block)->size;
    }

    oldsize = size ? (((size + 4) >> 2) << 2) : 0;
    newsize = (((size + extra + 4) >> 2) << 2);

    if ( newsize != oldsize ) {
#ifdef COMPILER
#if (COMPILER <= 16)
	if ((sizeof(mheader)+newsize) >= (1<<16))
	    return NULL;
#endif
#endif
	block = (char *) realloc(block, sizeof(mheader) + newsize);
	if (block == NULL)
	    return NULL;
	a = block + sizeof(mheader);
	for (i=oldsize; i<newsize; i++)
	    a[i] = '\0';
    }

    ((mheader *)block)->size = size + extra;
    return a;
}

char * memjoin(char *a, char *b)
{
    long i, size, extra;

    size = memlength(a);
    extra = memlength(b);
    a = memexpand(a, extra);
    if (a) {
	for (i=0; i<extra; i++)
	    a[i+size] = b[i];
    }
    return a;
}

