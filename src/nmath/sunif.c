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

#include "Mathlib.h"/*-> ../include/Random.h */

/* ----------------
 * New Scheme: Allow CHOICE of Random Number Generator [RNG]
 *
 * For R, the setup here must be compatible with
 *   Randomize(), GetSeeds(), SetSeeds()  from  ../main/random.c
 * + SetRNG()
 *   ~~~~~~  [new]
 *
 */

RNGTAB RNG_Table[] =
{
  /* kind  seed-length	name*/
    { 0,	3,	"Wichmann-Hill"},
    { 1, 	2,	"Marsaglia-MultiCarry"},
    { 2, 	2,	"Super-Duper"},
    { 3,	624,	"Mersenne-Twister"},

    { 9, 	2,	"Rand"},

    {-1,-1 ,""}
};

RNGtype RNG_kind = WICHMANN_HILL;

/* SEED vector:  Assume 32 __or more__ bits 

 * The first few are `unrolled' for speed
 * Here, use maximal seed length from above;
 *
 */
unsigned long int i1_seed, i2_seed, i_seed[624 - 2];
#define i3_seed (i_seed[0])

#define d2_32	4294967296./* = (double) */
#define I2_32m1 2.328306437080797e-10/* = 1/(2^32 - 1) */

/* do32bits(): Drop higher than 32 bits */
#ifdef LONG_32_BITS
# define do32bits(N) (N)
#else
# define do32bits(N) ((N) & 037777777777)
#endif

double sunif(void)
{
    double value;

    switch(RNG_kind) {

    case WICHMANN_HILL:
	i1_seed = i1_seed * 171 % 30269;
	i2_seed = i2_seed * 172 % 30307;
	i3_seed = i3_seed * 170 % 30323;
	value =
	  i1_seed / 30269.0 +
	  i2_seed / 30307.0 +
	  i3_seed / 30323.0;
	return value - (int) value;/* in [0,1) */

    case MARSAGLIA_MULTICARRY:/* 0177777(octal) == 65535(decimal)*/
	/* The following also works when 'usigned long' is > 32 bits : */
	i1_seed= 36969*(i1_seed & 0177777) + (i1_seed>>16);
	i2_seed= 18000*(i2_seed & 0177777) + (i2_seed>>16);
	return (do32bits(i1_seed << 16)^(i2_seed & 0177777))
	    * I2_32m1;/* in [0,1) */

    case SUPER_DUPER:

	/* This is Reeds et al (1984) implementation; 
	 * modified using __unsigned__  seeds instead of signed ones
	 */
	i1_seed ^= ((i1_seed >> 15) & 0377777);
	i1_seed ^= do32bits(i1_seed << 17);
#ifdef LONG_32_BITS
	i2_seed *= 69069;
#else
	i2_seed = do32bits(69069 * i2_seed);
#endif
	return (i1_seed^i2_seed) * I2_32m1;

/*
    case MERSENNE_TWISTER:

	return ....;
*/
    case RAND:
	/* Use ANSI C_INTERNAL  (with which you can only SET a seed,
	   but not get the current)*/

	return rand()/(.1 + RAND_MAX);

    default:/* can never happen (enum type)*/ return -1.;
  }
}


