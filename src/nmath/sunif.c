/*
 *  Mathlib : A C Library of Special Functions
 *  Copyright (C) 1998 Ross Ihaka
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
 *  SYNOPSIS
 *
 *    #include "Mathlib.h"
 *    double sunif(void);
 *
 *  DESCRIPTION
 *
 *     Random variates from the standard uniform distribution, U(0,1).
 *
 *  REFERENCE
 *
 *    Wichmann, B. A. and I. D. Hill (1982).
 *    Algorithm AS 183: An efficient and portable
 *    pseudo-random number generator,
 *    Applied Statistics, 31, 188.
 */

#include "Mathlib.h"/* >> "Random.h" */
#include <time.h>/* for Randomize() */

/* ----------------
 * New Scheme: Allow CHOICE of Random Number Generator [RNG]
 *
 * For R, the setup here must be compatible with
 * GetSeeds(), SetSeeds(), SetRNG()  from  ../main/random.c
 *
 */

Int32 dummy[3];

RNGTAB RNG_Table[] =
{
/* kind Nkind	  name	  is_seeded seed-length	i1_s, *seed-vec */
    { 0, 0, "Wichmann-Hill",	0,	3,	123, 	dummy},
    { 1, 0, "Marsaglia-MultiCarry",0,	2,	123, 	dummy},
    { 2, 0, "Super-Duper",	0,	2,	123, 	dummy},
    { 3, 0, "Mersenne-Twister",	0,  1+624,	123, 	dummy},
    { 4, 0, "Rand",		0,	2,	-1,  	dummy},
};

RNGtype RNG_kind = WICHMANN_HILL;

/* SEED vector:  Assume 32 __or more__ bits 

 * The first few are `unrolled' for speed
 * Here, use maximal seed length from above;
 *
 */

/*unsigned long int i1_seed, i2_seed, i_seed[1+624 - 2];
 */

#define d2_32	4294967296./* = (double) */
#define i2_32m1 2.328306437080797e-10/* = 1/(2^32 - 1) */

/* do32bits(): Zero bits higher than 32
 * ----------
 * & 037.. really does nothing when long=32bits, 
 * however does every compiler optimize this?  -- optimize ourselves!
 */
#ifdef LONG_32_BITS
# define do32bits(N) (N)
#else
# define do32bits(N) ((N) & 037777777777)
#endif

#define I1 RNG_Table[RNG_kind].i1_seed
#define I2 RNG_Table[RNG_kind].i2_seed
#define I3 RNG_Table[RNG_kind].i3_seed
#define ISd RNG_Table[RNG_kind].i_seed

double sunif(void)
{
    double value;

    switch(RNG_kind) {

    case WICHMANN_HILL:
	I1 = I1 * 171 % 30269;
	I2 = I2 * 172 % 30307;
	I3 = I3 * 170 % 30323;
	value =
	  I1 / 30269.0 +
	  I2 / 30307.0 +
	  I3 / 30323.0;
	return value - (int) value;/* in [0,1) */

    case MARSAGLIA_MULTICARRY:/* 0177777(octal) == 65535(decimal)*/
	/* The following also works when 'usigned long' is > 32 bits : */
	I1= 36969*(I1 & 0177777) + (I1>>16);
	I2= 18000*(I2 & 0177777) + (I2>>16);
	return (do32bits(I1 << 16)^(I2 & 0177777))
	    * i2_32m1;/* in [0,1) */

    case SUPER_DUPER:

	/* This is Reeds et al (1984) implementation; 
	 * modified using __unsigned__  seeds instead of signed ones
	 */
	I1 ^= ((I1 >> 15) & 0377777);/*  Tausworthe */
	I1 ^= do32bits(I1 << 17);
#ifdef LONG_32_BITS
	I2 *= 69069;		/* Congruential */
#else
	I2 = do32bits(69069 * I2);
#endif
	return (I1^I2) * i2_32m1;/* in [0,1) */

    case RAND:
	/* Use ANSI C_INTERNAL  (with which you can only SET a seed,
	   but not get the current)*/

	return rand()/(.1 + RAND_MAX);/* in [0,1) */

    case MERSENNE_TWISTER:

	return 0.5;/*PLACE HOLDER*/

    default:/* can never happen (enum type)*/ return -1.;
  }
}

/*--- This are called from ../main/random.c : ---------*/

void FixupSeeds(RNGtype kind)
{
/* Depending on RNG, set 0 values to non-0, etc. */

    int j; 
#ifdef OLD
    RNGtype tkind;
#endif

    /* Set 0 to 1 : */
    if(!RNG_Table[kind].i1_seed) RNG_Table[kind].i1_seed++;
    for(j = 0; j <= RNG_Table[kind].n_seed - 2; j++)
	if(!RNG_Table[kind].i_seed[j]) RNG_Table[kind].i_seed[j]++;

    switch(kind) {
    case WICHMANN_HILL:
	I1 = I1 % 30269; I2 = I2 % 30307; I3 = I3 % 30323;
	
	/* map values equal to 0 mod modulus to 1. */
	if(I1 == 0) I1 = 1;
	if(I2 == 0) I2 = 1;
	if(I3 == 0) I3 = 1;
#ifdef OLD
	if(RNG_Table[kind].i1_seed >= 30269 ||
	   RNG_Table[kind].i2_seed >= 30307 ||
	   RNG_Table[kind].i3_seed >= 30323 ) {
            /*.Random.seed was screwed up */
	    /* do 1 iteration */
	    tkind = RNG_kind; RNG_kind = WICHMANN_HILL;
	    sunif();
	    RNG_kind = tkind;
	}
#endif
	return;
    case MARSAGLIA_MULTICARRY: 
	return;
    case SUPER_DUPER:
	/* I2 = Congruential: must be ODD */
	RNG_Table[kind].i2_seed |= 1;
	break;

    case RAND:/* no read-access to seed */

    case MERSENNE_TWISTER:

	break;
    }
}

void MaybeAllocSeeds(RNGtype kind)
{
    if(! RNG_Table[kind].is_seeded) { /* allocate ! */
#ifdef DEBUG
	MATHLIB_WARNING2("Allocating seed (length %d) for RNG kind '%d'\n",
			 RNG_Table[kind].n_seed - 1, kind);
#endif
	RNG_Table[kind].i_seed = (Int32 *) 
	    calloc((size_t)RNG_Table[kind].n_seed - 1,
		   sizeof(Int32));
	RNG_Table[kind].is_seeded = 1;
    }
}

void RNG_Init(RNGtype kind, long seed)
{
    int j;

    RNG_Table[kind].i1_seed = seed;
    for(j=0; j < RNG_Table[RNG_kind].n_seed - 1; j++) {
	seed = (69069 * seed) & 0xffffffff;
	RNG_Table[kind].i_seed[j] = seed;
    }
    FixupSeeds(kind);
}

void Randomize(RNGtype kind)
{
/* Only called by  GetRNGstate(), when there's no .Random.seed */

    MaybeAllocSeeds(kind);

    srand((int)time(NULL));
    
    RNG_Init(kind, (long) rand() | 01/* odd */);
}

