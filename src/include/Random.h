/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998, 1999   Robert Gentleman, Ross Ihaka 
 *                             and the R Development Core Team
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

/* only used by ../main/random.c  & ../nmath/{snorm,sunif}.c
 */
#ifndef RANDOM_H
#define RANDOM_H

typedef unsigned long Int32;/* how is this done on 64-bit archtictures? */

typedef enum {
    WICHMANN_HILL,
    MARSAGLIA_MULTICARRY,
    SUPER_DUPER,
    RAND,
    MERSENNE_TWISTER
} RNGtype;

/* Different kind of "N(0,1)" generators :*/
typedef enum {
    AHRENS_DIETER,
    KINDERMAN_RAMAGE
} N01type;

typedef struct {
    RNGtype kind; /* above enum: 0,1,2... */
    N01type Nkind;
    char *name; /* print name */
    int is_seeded; /* False(0), True(1) */
    int n_seed; /* length of seed vector */
    Int32 i1_seed;
    Int32 *i_seed;
} RNGTAB;
#define i2_seed i_seed[0]
#define i3_seed i_seed[1]

/* .Random.seed == (RNGkind, i1_seed, i_seed[0],i_seed[1],..,i_seed[n_seed-2])
 *		                      i2_seed   i3_seed
 */
void MaybeAllocSeeds(RNGtype);
void Randomize(RNGtype);
void FixupSeeds(RNGtype);
void RNG_Init(RNGtype kind, long seed);

extern RNGTAB  RNG_Table[];
extern RNGtype RNG_kind;

#endif
