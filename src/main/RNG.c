/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Random.h"

typedef unsigned int Int32;/* how is this done on 64-bit architectures? */

#define i2_seed i_seed[0]
#define i3_seed i_seed[1]
typedef struct {
    RNGtype kind; /* above enum: 0,1,2... */
    N01type Nkind;
    char *name; /* print name */
    int is_seeded; /* False(0), True(1) */
    int n_seed; /* length of seed vector */
    Int32 i1_seed;
    Int32 *i_seed;
} RNGTAB;

/* .Random.seed == (RNGkind, i1_seed, i_seed[0],i_seed[1],..,i_seed[n_seed-2])
 *		                      i2_seed   i3_seed
 * or           == (RNGkind)  [--> Randomize that one !]
 */


static Int32 dummy[3];

static
RNGTAB RNG_Table[] =
{
/* kind Nkind	  name	  is_seeded seed-length	i1_s, *seed-vec */
    { 0, 0, "Wichmann-Hill",	0,	3,	123,	dummy},
    { 1, 0, "Marsaglia-MultiCarry",0,	2,	123,	dummy},
    { 2, 0, "Super-Duper",	0,	2,	123,	dummy},
    { 3, 0, "Mersenne-Twister",	0,  1+624,	123,	dummy},
    { 4, 0, "Rand",		0,	2,	-1,	dummy},
};

static
RNGtype RNG_kind = WICHMANN_HILL;

/* SEED vector:	 Assume 32 __or more__ bits 
   
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

#if (SIZEOF_LONG == 4)
# define do32bits(N) (N)
#else
# define do32bits(N) ((N) & 037777777777)
#endif

#define I1 RNG_Table[RNG_kind].i1_seed
#define I2 RNG_Table[RNG_kind].i2_seed
#define I3 RNG_Table[RNG_kind].i3_seed
#define ISd RNG_Table[RNG_kind].i_seed


extern N01type N01_kind; /* from .../nmath/snorm.c */

double unif_rand(void)
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
	 * modified using __unsigned__	seeds instead of signed ones
	 */
	I1 ^= ((I1 >> 15) & 0377777);/*	 Tausworthe */
	I1 ^= do32bits(I1 << 17);
#ifdef LONG_32_BITS
	I2 *= 69069;		/* Congruential */
#else
	I2 = do32bits(69069 * I2);
#endif
	return (I1^I2) * i2_32m1;/* in [0,1) */
	
    case RAND:
	/* Use ANSI C_INTERNAL	(with which you can only SET a seed,
	   but not get the current)*/
	
	return rand()/(.1 + RAND_MAX);/* in [0,1) */
	
    case MERSENNE_TWISTER:
	
	return 0.5;/*PLACE HOLDER*/
	
    default:/* can never happen (enum type)*/ return -1.;
    }
}

static void FixupSeeds(RNGtype kind)
{
/* Depending on RNG, set 0 values to non-0, etc. */
    
    int j; 

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

static void MaybeAllocSeeds(RNGtype kind)
{
    if(! RNG_Table[kind].is_seeded) { /* allocate ! */
#ifdef DEBUG_RAND
	MATHLIB_WARNING2("Allocating seed (length %d) for RNG kind '%d'\n",
			 RNG_Table[kind].n_seed - 1, kind);
#endif
	RNG_Table[kind].i_seed = (Int32 *) 
	    calloc((size_t)RNG_Table[kind].n_seed - 1,
		   sizeof(Int32));
	RNG_Table[kind].is_seeded = 1;
    }
}

static void RNG_Init(RNGtype kind, Int32 seed)
{
    int j;
    
    /* Initial scrambling */
    for(j = 0; j < 50; j++)
	seed = (69069 * seed + 1) & 0xffffffff;
    RNG_Table[kind].i1_seed = seed;
    for(j = 0; j < RNG_Table[RNG_kind].n_seed - 1; j++) {
	seed = (69069 * seed + 1) & 0xffffffff;
	RNG_Table[kind].i_seed[j] = seed;
    }
    FixupSeeds(kind);
}

#include <time.h>
static void Randomize(RNGtype kind)
{
/* Only called by  GetRNGstate(), when there's no .Random.seed */
    
    MaybeAllocSeeds(kind);
    
    RNG_Init(kind, (Int32) time(NULL));
}


void GetRNGstate()
{
  /* Get  .Random.seed  into proper variables */
    int len_seed, j, seed_off = 0;
    SEXP seeds;

    seeds = findVar(R_SeedsSymbol, R_GlobalEnv);
    if (seeds == R_UnboundValue) {
	Randomize(RNG_kind);
    }
    else {
	seeds = coerceVector(seeds, INTSXP);
	if (seeds == R_MissingArg)
	    error(".Random.seed is a missing argument with no default");
	if (!isVector(seeds))
	    error(".Random.seed is not a vector");
	RNG_kind = INTEGER(seeds)[0];
	if (RNG_kind > MERSENNE_TWISTER || RNG_kind < 0) 
		RNG_kind = WICHMANN_HILL; 
	len_seed = RNG_Table[RNG_kind].n_seed;
	if(LENGTH(seeds) > 1 && LENGTH(seeds) < len_seed + 1) {
	    if(LENGTH(seeds) == RNG_Table[WICHMANN_HILL].n_seed) {
		/* BACKWARDS COMPATIBILITY: */
		seed_off = 1;
		warning("Wrong length .Random.seed; forgot initial RNGkind? set to Wichmann-Hill");
		/* compatibility mode */
		RNG_kind = WICHMANN_HILL;
	    } else {
		error(".Random.seed has wrong length");
	    }
	}

 	switch(RNG_kind) {
 	case WICHMANN_HILL:
 	case MARSAGLIA_MULTICARRY:
 	case SUPER_DUPER:
 	case RAND:
	    break;
 	case MERSENNE_TWISTER:
	    error("'Mersenne-Twister' not yet implemented"); break;
	default:
	    error(".Random.seed[1] is NOT a valid RNG kind (code)");
	}
	if(LENGTH(seeds) == 1)
	    Randomize(RNG_kind);
	else {
	    RNG_Table[RNG_kind].i1_seed = INTEGER(seeds)[1 - seed_off];
	    for(j = 2; j <= len_seed; j++)
		RNG_Table[RNG_kind].i_seed[j - 2] = INTEGER(seeds)[j - seed_off];
	    FixupSeeds(RNG_kind);
	}
    }
}

void PutRNGstate()
{
    int len_seed, j;
    SEXP seeds;
    len_seed = RNG_Table[RNG_kind].n_seed;

    PROTECT(seeds = allocVector(INTSXP, len_seed + 1));

    INTEGER(seeds)[0] = RNG_kind;
    INTEGER(seeds)[1] = RNG_Table[RNG_kind].i1_seed;
    for(j = 2; j <= len_seed; j++)
	INTEGER(seeds)[j] = RNG_Table[RNG_kind].i_seed[j-2];

    setVar(R_SeedsSymbol, seeds, R_GlobalEnv);
    UNPROTECT(1);
}

static void RNGkind(RNGtype newkind)
{
/* Choose a new kind of RNG.
 * Initialize its seed by calling the old RNG's unif_rand()
 */
    GetRNGstate();

    RNG_Init(newkind, unif_rand() * UINT_MAX);

    switch(newkind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
      break;
    case RAND:
	error("RNGkind: \"Rand\" not yet available (BUG)!");
	/* srand((unsigned int)unif_rand()*UINT_MAX);*/
      break;
    case MERSENNE_TWISTER:
	/* ... */
	error("RNGkind: \"Mersenne-Twister\" not yet available!");
      break;
    default:
      error("RNGkind: unimplemented RNG kind %d", newkind);
    }
    RNG_kind = newkind;

    PutRNGstate();
}

/*------ .Internal interface ------------------------*/

SEXP do_RNGkind (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, rng, norm;

    checkArity(op,args);
    PROTECT(ans = allocVector(INTSXP, 2));
    INTEGER(ans)[0] = RNG_kind;
    INTEGER(ans)[1] = N01_kind;
    rng = CAR(args);
    norm = CADR(args);
    if(!isNull(rng)) { /* set a new RNG kind */
      RNGkind(asInteger(rng));
    }
    if(!isNull(norm)) { /* set a new normal kind */
      N01_kind = asInteger(norm);
    }
    UNPROTECT(1);
    return ans;
}


SEXP do_setseed (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP skind;
    int seed;
    RNGtype kind;

    checkArity(op,args);
    seed = asInteger(CAR(args));
    if (seed == NA_INTEGER)
	error("supplied seed is not a valid integer");
    skind = CADR(args);
    if (!isNull(skind)) {
	kind = asInteger(skind);
	RNGkind(kind);
    } else
	kind = RNG_kind;
    RNG_Init(kind, (Int32) seed);
    PutRNGstate();
    return R_NilValue;
}


/* S COMPATIBILITY */

/* The following entry points provide compatibility with S. */
/* These entry points should not be used by new R code. */

void seed_in(long *ignored)
{
    GetRNGstate();
}

void seed_out(long *ignored)
{
    PutRNGstate();
}
