/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--1998, The R Development Core Team.
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
 *  Much of this is from Doug Bates.
 */

#ifndef R_S_H
#define R_S_H

#ifndef USING_R
#define USING_R
#define longint int
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include "Error.h"
#include "Memory.h"
#include "Platform.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <float.h>
#ifdef Macintosh
#include <fp.h>
#else
#include <math.h>
#endif

#define call_S call_R

extern void seed_in(long *);
extern void seed_out(long *);
extern double unif_rand(void);
extern double norm_rand(void);

extern void *R_chk_calloc(size_t, size_t);
extern void *R_chk_realloc(void *, size_t);
extern void R_chk_free(void *);

#define R_PROBLEM_BUFSIZE 4096
char R_problem_buf[R_PROBLEM_BUFSIZE];

#define NULL_ENTRY
#define PROBLEM		sprintf(R_problem_buf,
#define RECOVER(x)	), error(R_problem_buf)
#define WARNING(x)	), warning(R_problem_buf)

#define Calloc(n, t)   (t *) R_chk_calloc( (size_t) (n), sizeof(t) )
#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (size_t)((n) * sizeof(t)) )
#define Free(p)        R_chk_free( (void *)(p) )
#define Memcpy(p,q,n)  memcpy( p, q, (size_t)( (n) * sizeof(*p) ) )

#define F77_CALL(x)    F77_SYMBOL(x)
#define F77_NAME(x)    F77_SYMBOL(x)

/* Can't be sure Mathlib.h or math.h is included */
#ifndef M_PI
#define M_PI 3.141592653589793238462643383279502884197169399375
#endif

#define PI             M_PI
#define SINGLE_EPS     FLT_EPSILON
#define SINGLE_BASE    FLT_RADIX
#define SINGLE_XMIN    FLT_MIN
#define SINGLE_XMAX    FLT_MAX
#define DOUBLE_DIGITS  DBL_MANT_DIG
#define DOUBLE_EPS     DBL_EPSILON
#define DOUBLE_XMAX    DBL_MAX
#define DOUBLE_XMIN    DBL_MIN

extern int F77_SYMBOL(dblepr) (char *label, int *nchar,
			       double *data, int *ndata);
extern int F77_SYMBOL(intpr) (char *label, int *nchar,
			      int *data, int *ndata);

#ifdef __cplusplus
}
#endif

#endif /* !R_S_H */
