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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
#include "Rconfig.h"

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

/* Macros for S/R Compatibility */

#include "Rdefines.h"

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
