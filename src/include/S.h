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
/* is this a good idea? - conflicts with many versions of f2c.h */
#define longint int
#endif

#ifdef __cplusplus
extern "C" {
#endif

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

#include "Rconfig.h"
#include "R_ext/Constants.h"
#include "R_ext/Memory.h" /* S_alloc */

/* Not quite full compatibility: beware! */
/* void	call_R(char*, long, void**, char**, long*, char**, long, char**);*/
#define call_S call_R

/* subset of those in Random.h */
extern void seed_in(long *);
extern void seed_out(long *);
extern double unif_rand(void);
extern double norm_rand(void);

/* Macros for S/R Compatibility */

#include "R_ext/RS.h"
/* for PROBLEM ... Calloc, Realloc, Free, Memcpy, F77_xxxx */

/* S's complex is different, and is a define to S_complex now */
typedef struct {
	double re;
	double im;
} S_complex;

#ifdef S_OLD_COMPLEX
#  define complex S_complex
#endif

/* Not quite full compatibility: beware! */
/* void	call_R(char*, long, void**, char**, long*, char**, long, char**);*/
#define call_S call_R


#ifdef __cplusplus
}
#endif

#endif /* !R_S_H */
