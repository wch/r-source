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

char * memalloc(long size)
{
    long *block;
    char *a;
    long i, datasize;
    TRACEAR("alloc");
    datasize = (((size + 4) >> 2) << 2);
#ifdef COMPILER
#if (COMPILER <= 16)
    if ((sizeof(long)+datasize) >= (1<<16))
	return NULL;
#endif
#endif
    block = (long *) malloc(sizeof(long) + datasize);
    if (block == NULL)
	return NULL;
    block[0] = size;
    a = (char *) & block[1];
    for (i=0; i<datasize; i++)
	a[i] = '\0';
    return a;
}

char * memrealloc(char *a, long new_size)
{
    long *block;
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
	block = ((long*)a) - 1;
	size = block[0];
    }

    oldsize = size ? (((size + 4) >> 2) << 2) : 0;
    newsize = (((new_size + 4) >> 2) << 2);

    if ( newsize != oldsize ) {
#ifdef COMPILER
#if (COMPILER <= 16)
	if ((sizeof(long)+newsize) >= (1<<16))
	    return NULL;
#endif
#endif
	block = (long *) realloc(block, sizeof(long) + newsize);
	if (block == NULL)
	    return NULL;
	a = (char *) & block[1];
	for (i=oldsize; i<newsize; i++)
	    a[i] = '\0';
    }

    block[0] = new_size;
    return a;
}

long memlength(char *a)
{
    return (a) ? ((long*)(a)-1)[0] : 0;
}

void memfree(char *a)
{
    if (a) free((long*)(a)-1);
}

char * memexpand(char *a, long extra)
{
    long *block;
    long i, size, oldsize, newsize;
    TRACEAR("exp");
    if (extra == 0)
	return a;

    if (a == NULL) {
	block = NULL;
	size = 0;
    }
    else {
	block = ((long*)a) - 1;
	size = block[0];
    }

    oldsize = size ? (((size + 4) >> 2) << 2) : 0;
    newsize = (((size + extra + 4) >> 2) << 2);

    if ( newsize != oldsize ) {
#ifdef COMPILER
#if (COMPILER <= 16)
	if ((sizeof(long)+newsize) >= (1<<16))
	    return NULL;
#endif
#endif
	block = (long *) realloc(block, sizeof(long) + newsize);
	if (block == NULL)
	    return NULL;
	a = (char *) & block[1];
	for (i=oldsize; i<newsize; i++)
	    a[i] = '\0';
    }

    block[0] = size + extra;
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
