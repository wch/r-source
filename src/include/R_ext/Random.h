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

#ifndef R_EXT_RANDOM_H_
#define R_EXT_RANDOM_H_
/*#include "Random.h"  for RNG_Init(RNGtype kind, long seed) */

/*extern void RNGkind(RNGtype)
extern void GetRNGstate();
extern void PutRNGstate();

extern double sunif(void);
extern double snorm(void);
extern double sexp(void);
*/
/* S COMPATIBILITY */

extern void seed_in(long *ignored);
extern void seed_out(long *ignored);
extern double unif_rand(void);
extern double norm_rand(void);
/* next one is not actually in S */
extern double exp_rand(void);

#endif
