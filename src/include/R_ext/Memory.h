/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998--1999  The R Development Core Team.
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

#ifndef R_EXT_MEMORY_H_
#define R_EXT_MEMORY_H_
#include <stddef.h> /* for size_t */


/* R allocation on the heap */

char*	R_alloc(long, int);
char*	vmaxget(void);
void	vmaxset(char*);


/* S Like Memory Management */

extern char* S_alloc(long, int);
extern char* S_realloc(char*, long, long, int);

extern void *R_chk_calloc(size_t, size_t);
extern void *R_chk_realloc(void *, size_t);
extern void  R_chk_free(void *);

#define Calloc(n, t)   (t *) R_chk_calloc( (size_t) (n), sizeof(t) )
#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (size_t)((n) * sizeof(t)) )
/* S-PLUS 3.x but not 5.x NULLs the pointer in the following */
#define Free(p)        (R_chk_free( (void *)(p) ), (p) = NULL)
#include <string.h>
#define Memcpy(p,q,n)  memcpy( p, q, (size_t)( (n) * sizeof(*p) ) )

#endif
