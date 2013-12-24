/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include <Internal.h>
#include <R_ext/Random.h>

/* Normal generator is not actually set here but in nmath/snorm.c */
#define RNG_DEFAULT MERSENNE_TWISTER
#define N01_DEFAULT INVERSION


#include <R_ext/Rdynload.h>

static DL_FUNC User_unif_fun, User_unif_nseed,
	User_unif_seedloc;
typedef void (*UnifInitFun)(Int32);

UnifInitFun User_unif_init = NULL; /* some picky compilers */

DL_FUNC  User_norm_fun = NULL; /* also in ../nmath/snorm.c */


static RNGtype RNG_kind = RNG_DEFAULT;
extern N01type N01_kind; /* from ../nmath/snorm.c */

/* typedef unsigned int Int32; in Random.h */

/* .Random.seed == (RNGkind, i_seed[0],i_seed[1],..,i_seed[n_seed-1])
 * or           == (RNGkind) or missing  [--> Randomize]
 */

typedef struct {
    RNGtype kind;
    N01type Nkind;
    char *name; /* print name */
    int n_seed; /* length of seed vector */
    Int32 *i_seed;
} RNGTAB;


static Int32 dummy[625];
static
RNGTAB RNG_Table[] =
{
/* kind Nkind	  name	           n_seed      i_seed */
    { WICHMANN_HILL,        BUGGY_KINDERMAN_RAMAGE, "Wichmann-Hill",	     3,	dummy},
    { MARSAGLIA_MULTICARRY, BUGGY_KINDERMAN_RAMAGE, "Marsaglia-MultiCarry",  2,	dummy},
    { SUPER_DUPER,          BUGGY_KINDERMAN_RAMAGE, "Super-Duper",	     2,	dummy},
    { MERSENNE_TWISTER,     BUGGY_KINDERMAN_RAMAGE, "Mersenne-Twister",  1+624,	dummy},
    { KNUTH_TAOCP,          BUGGY_KINDERMAN_RAMAGE, "Knuth-TAOCP",       1+100,	dummy},
    { USER_UNIF,            BUGGY_KINDERMAN_RAMAGE, "User-supplied",         0,	dummy},
    { KNUTH_TAOCP2,         BUGGY_KINDERMAN_RAMAGE, "Knuth-TAOCP-2002",  1+100,	dummy},
    { LECUYER_CMRG,         BUGGY_KINDERMAN_RAMAGE, "L'Ecuyer-CMRG",         6,	dummy},
};


#define d2_32	4294967296./* = (double) */
#define i2_32m1 2.328306437080797e-10/* = 1/(2^32 - 1) */
#define KT      9.31322574615479e-10 /* = 2^-30 */

#define I1 (RNG_Table[RNG_kind].i_seed[0])
#define I2 (RNG_Table[RNG_kind].i_seed[1])
#define I3 (RNG_Table[RNG_kind].i_seed[2])

static void Randomize(RNGtype kind);
static double MT_genrand(void);
static Int32 KT_next(void);
static void RNG_Init_R_KT(Int32);
static void RNG_Init_KT2(Int32);
#define KT_pos (RNG_Table[KNUTH_TAOCP].i_seed[100])

static double fixup(double x)
{
    /* ensure 0 and 1 are never returned */
    if(x <= 0.0) return 0.5*i2_32m1;
    if((1.0 - x) <= 0.0) return 1.0 - 0.5*i2_32m1;
    return x;
}


double unif_rand(void)
{
    double value;

    switch(RNG_kind) {

    case WICHMANN_HILL:
	I1 = I1 * 171 % 30269;
	I2 = I2 * 172 % 30307;
	I3 = I3 * 170 % 30323;
	value = I1 / 30269.0 + I2 / 30307.0 + I3 / 30323.0;
	return fixup(value - (int) value);/* in [0,1) */

    case MARSAGLIA_MULTICARRY:/* 0177777(octal) == 65535(decimal)*/
	I1= 36969*(I1 & 0177777) + (I1>>16);
	I2= 18000*(I2 & 0177777) + (I2>>16);
	return fixup(((I1 << 16)^(I2 & 0177777)) * i2_32m1); /* in [0,1) */

    case SUPER_DUPER:
	/* This is Reeds et al (1984) implementation;
	 * modified using __unsigned__	seeds instead of signed ones
	 */
	I1 ^= ((I1 >> 15) & 0377777); /* Tausworthe */
	I1 ^= I1 << 17;
	I2 *= 69069;		/* Congruential */
	return fixup((I1^I2) * i2_32m1); /* in [0,1) */

    case MERSENNE_TWISTER:
	return fixup(MT_genrand());

    case KNUTH_TAOCP:
    case KNUTH_TAOCP2:
	return fixup(KT_next() * KT);

    case USER_UNIF:
	return *((double *) User_unif_fun());

    case LECUYER_CMRG:
    {
	/* Based loosely on the GPL-ed version of
	   http://www.iro.umontreal.ca/~lecuyer/myftp/streams00/c2010/RngStream.c
	   but using int_least64_t, which C99 guarantees.
	*/
	int k;
	int_least64_t p1, p2;

#define II(i) (RNG_Table[RNG_kind].i_seed[i])
#define m1    4294967087
#define m2    4294944443
#define normc  2.328306549295727688e-10
#define a12     (int_least64_t)1403580
#define a13n    (int_least64_t)810728
#define a21     (int_least64_t)527612
#define a23n    (int_least64_t)1370589

	p1 = a12 * (unsigned int)II(1) - a13n * (unsigned int)II(0);
	/* p1 % m1 would surely do */
	k = (int) (p1 / m1);
	p1 -= k * m1;
	if (p1 < 0.0) p1 += m1;
	II(0) = II(1); II(1) = II(2); II(2) = (int) p1;

	p2 = a21 * (unsigned int)II(5) - a23n * (unsigned int)II(3);
	k = (int) (p2 / m2);
	p2 -= k * m2;
	if (p2 < 0.0) p2 += m2;
	II(3) = II(4); II(4) = II(5); II(5) = (int) p2;

	return (double)((p1 > p2) ? (p1 - p2) : (p1 - p2 + m1)) * normc;
    }
    default:
	error(_("unif_rand: unimplemented RNG kind %d"), RNG_kind);
	return -1.;
    }
}

/* we must mask global variable here, as I1-I3 hide RNG_kind
   and we want the argument */
static void FixupSeeds(RNGtype RNG_kind, int initial)
{
/* Depending on RNG, set 0 values to non-0, etc. */

    int j, notallzero = 0;

    /* Set 0 to 1 :
       for(j = 0; j <= RNG_Table[RNG_kind].n_seed - 1; j++)
       if(!RNG_Table[RNG_kind].i_seed[j]) RNG_Table[RNG_kind].i_seed[j]++; */

    switch(RNG_kind) {
    case WICHMANN_HILL:
	I1 = I1 % 30269; I2 = I2 % 30307; I3 = I3 % 30323;

	/* map values equal to 0 mod modulus to 1. */
	if(I1 == 0) I1 = 1;
	if(I2 == 0) I2 = 1;
	if(I3 == 0) I3 = 1;
	return;

    case SUPER_DUPER:
	if(I1 == 0) I1 = 1;
	/* I2 = Congruential: must be ODD */
	I2 |= 1;
	break;

    case MARSAGLIA_MULTICARRY:
	if(I1 == 0) I1 = 1;
	if(I2 == 0) I2 = 1;
	break;

    case MERSENNE_TWISTER:
	if(initial) I1 = 624;
	 /* No action unless user has corrupted .Random.seed */
	if(I1 <= 0) I1 = 624;
	/* check for all zeroes */
	for (j = 1; j <= 624; j++)
	    if(RNG_Table[RNG_kind].i_seed[j] != 0) {
		notallzero = 1;
		break;
	    }
	if(!notallzero) Randomize(RNG_kind);
	break;

    case KNUTH_TAOCP:
    case KNUTH_TAOCP2:
	if(KT_pos <= 0) KT_pos = 100;
	/* check for all zeroes */
	for (j = 0; j < 100; j++)
	    if(RNG_Table[RNG_kind].i_seed[j] != 0) {
		notallzero = 1;
		break;
	    }
	if(!notallzero) Randomize(RNG_kind);
	break;
    case USER_UNIF:
	break;
    case LECUYER_CMRG:
	/* first set: not all zero, in [0, m1)
	   second set: not all zero, in [0, m2) */
    {
	unsigned int tmp;
	int allOK = 1;
	for (j = 0; j < 3; j++) {
	    tmp = RNG_Table[RNG_kind].i_seed[j];
	    if(tmp != 0) notallzero = 1;
	    if (tmp >= m1) allOK = 0;
	}
	if(!notallzero || !allOK) Randomize(RNG_kind);
	for (j = 3; j < 6; j++) {
	    tmp = RNG_Table[RNG_kind].i_seed[j];
	    if(tmp != 0) notallzero = 1;
	    if (tmp >= m2) allOK = 0;
	}
	if(!notallzero || !allOK) Randomize(RNG_kind);
    }
    break;
    default:
	error(_("FixupSeeds: unimplemented RNG kind %d"), RNG_kind);
    }
}

extern double BM_norm_keep; /* ../nmath/snorm.c */

static void RNG_Init(RNGtype kind, Int32 seed)
{
    int j;

    BM_norm_keep = 0.0; /* zap Box-Muller history */

    /* Initial scrambling */
    for(j = 0; j < 50; j++)
	seed = (69069 * seed + 1);
    switch(kind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
    case MERSENNE_TWISTER:
	/* i_seed[0] is mti, *but* this is needed for historical consistency */
	for(j = 0; j < RNG_Table[kind].n_seed; j++) {
	    seed = (69069 * seed + 1);
	    RNG_Table[kind].i_seed[j] = seed;
	}
	FixupSeeds(kind, 1);
	break;
    case KNUTH_TAOCP:
	RNG_Init_R_KT(seed);
	break;
    case KNUTH_TAOCP2:
	RNG_Init_KT2(seed);
	break;
    case LECUYER_CMRG:
	for(j = 0; j < RNG_Table[kind].n_seed; j++) {
	    seed = (69069 * seed + 1);
	    while(seed >= m2) seed = (69069 * seed + 1);
	    RNG_Table[kind].i_seed[j] = seed;
	}
	break;
    case USER_UNIF:
	User_unif_fun = R_FindSymbol("user_unif_rand", "", NULL);
	if (!User_unif_fun) error(_("'user_unif_rand' not in load table"));
	User_unif_init = (UnifInitFun) R_FindSymbol("user_unif_init", "", NULL);
	if (User_unif_init) (void) User_unif_init(seed);
	User_unif_nseed = R_FindSymbol("user_unif_nseed", "", NULL);
	User_unif_seedloc = R_FindSymbol("user_unif_seedloc", "",  NULL);
	if (User_unif_seedloc) {
	    int ns = 0;
	    if (!User_unif_nseed) {
		warning(_("cannot read seeds unless 'user_unif_nseed' is supplied"));
		break;
	    }
	    ns = *((int *) User_unif_nseed());
	    if (ns < 0 || ns > 625) {
		warning(_("seed length must be in 0...625; ignored"));
		break;
	    }
	    RNG_Table[kind].n_seed = ns;
	    RNG_Table[kind].i_seed = (Int32 *) User_unif_seedloc();
	}
	break;
    default:
	error(_("RNG_Init: unimplemented RNG kind %d"), kind);
    }
}

static SEXP GetSeedsFromVar(void)
{
    SEXP seeds = findVarInFrame(R_GlobalEnv, R_SeedsSymbol);
    if (TYPEOF(seeds) == PROMSXP)
	seeds = eval(R_SeedsSymbol, R_GlobalEnv);
    return seeds;
}

unsigned int TimeToSeed(void); /* datetime.c */

static void Randomize(RNGtype kind)
{
/* Only called by  GetRNGstate() when there is no .Random.seed */
    RNG_Init(kind, TimeToSeed());
}

static void GetRNGkind(SEXP seeds)
{
    /* Load RNG_kind, N01_kind from .Random.seed if present */
    int tmp, *is;
    RNGtype newRNG; N01type newN01;

    if (isNull(seeds))
	seeds = GetSeedsFromVar();
    if (seeds == R_UnboundValue) return;
    if (!isInteger(seeds)) {
	if (seeds == R_MissingArg) /* How can this happen? */
	    error(_("'.Random.seed' is a missing argument with no default"));
	warning(_("'.Random.seed' is not an integer vector but of type '%s', so ignored"),
		type2char(TYPEOF(seeds)));
	goto invalid;
    }
    is = INTEGER(seeds);
    tmp = is[0];
    /* avoid overflow here: max current value is 705 */
    if (tmp == NA_INTEGER || tmp < 0 || tmp > 1000) {
	warning(_("'.Random.seed[1]' is not a valid integer, so ignored"));
	goto invalid;
    }
    newRNG = (RNGtype) (tmp % 100);
    newN01 = (N01type) (tmp / 100);
    if (newN01 > KINDERMAN_RAMAGE) {
	warning(_("'.Random.seed[1]' is not a valid Normal type, so ignored"));
	goto invalid;
    }
    switch(newRNG) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
    case MERSENNE_TWISTER:
    case KNUTH_TAOCP:
    case KNUTH_TAOCP2:
    case LECUYER_CMRG:
	break;
    case USER_UNIF:
	if(!User_unif_fun) {
	    warning(_("'.Random.seed[1] = 5' but no user-supplied generator, so ignored"));
	    goto invalid;
	}
	break;
    default:
	warning(_("'.Random.seed[1]' is not a valid RNG kind so ignored"));
	goto invalid;
    }
    RNG_kind = newRNG; N01_kind = newN01;
    return;
invalid:
    RNG_kind = RNG_DEFAULT; N01_kind = N01_DEFAULT;
    Randomize(RNG_kind);
    return;
}


void GetRNGstate()
{
    /* Get  .Random.seed  into proper variables */
    int len_seed;
    SEXP seeds;

    /* look only in the workspace */
    seeds = GetSeedsFromVar();
    if (seeds == R_UnboundValue) {
	Randomize(RNG_kind);
    } else {
	GetRNGkind(seeds);
	len_seed = RNG_Table[RNG_kind].n_seed;
	/* Not sure whether this test is needed: wrong for USER_UNIF */
	if(LENGTH(seeds) > 1 && LENGTH(seeds) < len_seed + 1)
	    error(_("'.Random.seed' has wrong length"));
	if(LENGTH(seeds) == 1 && RNG_kind != USER_UNIF)
	    Randomize(RNG_kind);
	else {
	    int j, *is = INTEGER(seeds);
	    for(j = 1; j <= len_seed; j++)
		RNG_Table[RNG_kind].i_seed[j - 1] = is[j];
	    FixupSeeds(RNG_kind, 0);
	}
    }
}

void PutRNGstate()
{
    /* Copy out seeds to  .Random.seed  */
    int len_seed, j;
    SEXP seeds;

    if (RNG_kind > LECUYER_CMRG || N01_kind > KINDERMAN_RAMAGE) {
	warning("Internal .Random.seed is corrupt: not saving");
	return;
    }

    len_seed = RNG_Table[RNG_kind].n_seed;

    PROTECT(seeds = allocVector(INTSXP, len_seed + 1));

    INTEGER(seeds)[0] = RNG_kind + 100 * N01_kind;
    for(j = 0; j < len_seed; j++)
	INTEGER(seeds)[j+1] = RNG_Table[RNG_kind].i_seed[j];

    /* assign only in the workspace */
    defineVar(R_SeedsSymbol, seeds, R_GlobalEnv);
    UNPROTECT(1);
}

static void RNGkind(RNGtype newkind)
{
/* Choose a new kind of RNG.
 * Initialize its seed by calling the old RNG's unif_rand()
 */
    if (newkind == (RNGtype)-1) newkind = RNG_DEFAULT;
    switch(newkind) {
    case WICHMANN_HILL:
    case MARSAGLIA_MULTICARRY:
    case SUPER_DUPER:
    case MERSENNE_TWISTER:
    case KNUTH_TAOCP:
    case USER_UNIF:
    case KNUTH_TAOCP2:
    case LECUYER_CMRG:
	break;
    default:
	error(_("RNGkind: unimplemented RNG kind %d"), newkind);
    }
    GetRNGstate();
    // precaution against corruption as per package randtoolbox
    double u = unif_rand();
    if (u < 0.0 || u > 1.0) {
	warning("someone corrupted the random-number generator: re-initializing");
	RNG_Init(newkind, TimeToSeed());
    } else
	RNG_Init(newkind, (Int32) (u * UINT_MAX));
    RNG_kind = newkind;
    PutRNGstate();
}

static void Norm_kind(N01type kind)
{
    /* N01type is an enumeration type, so this will probably get
       mapped to an unsigned integer type. */
    if (kind == (N01type)-1) kind = N01_DEFAULT;
    if (kind > KINDERMAN_RAMAGE)
	error(_("invalid Normal type in 'RNGkind'"));
    if (kind == USER_NORM) {
	User_norm_fun = R_FindSymbol("user_norm_rand", "", NULL);
	if (!User_norm_fun) error(_("'user_norm_rand' not in load table"));
    }
    GetRNGstate(); /* might not be initialized */
    if (kind == BOX_MULLER)
	BM_norm_keep = 0.0; /* zap Box-Muller history */
    N01_kind = kind;
    PutRNGstate();
}


/*------ .Internal interface ------------------------*/

SEXP attribute_hidden do_RNGkind (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, rng, norm;

    checkArity(op,args);
    GetRNGstate(); /* might not be initialized */
    PROTECT(ans = allocVector(INTSXP, 2));
    INTEGER(ans)[0] = RNG_kind;
    INTEGER(ans)[1] = N01_kind;
    rng = CAR(args);
    norm = CADR(args);
    GetRNGkind(R_NilValue); /* pull from .Random.seed if present */
    if(!isNull(rng)) { /* set a new RNG kind */
	RNGkind((RNGtype) asInteger(rng));
    }
    if(!isNull(norm)) { /* set a new normal kind */
	Norm_kind((N01type) asInteger(norm));
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden do_setseed (SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP skind, nkind;
    int seed;

    checkArity(op, args);
    if(!isNull(CAR(args))) {
	seed = asInteger(CAR(args));
	if (seed == NA_INTEGER)
	    error(_("supplied seed is not a valid integer"));
    } else seed = TimeToSeed();
    skind = CADR(args);
    nkind = CADDR(args);
    GetRNGkind(R_NilValue); /* pull RNG_kind, N01_kind from 
			       .Random.seed if present */
    if (!isNull(skind)) RNGkind((RNGtype) asInteger(skind));
    if (!isNull(nkind)) Norm_kind((N01type) asInteger(nkind));
    RNG_Init(RNG_kind, (Int32) seed); /* zaps BM history */
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

/* ===================  Mersenne Twister ========================== */
/* From http://www.math.keio.ac.jp/~matumoto/emt.html */

/* A C-program for MT19937: Real number version([0,1)-interval)
   (1999/10/28)
     genrand() generates one pseudorandom real number (double)
   which is uniformly distributed on [0,1)-interval, for each
   call. sgenrand(seed) sets initial values to the working area
   of 624 words. Before genrand(), sgenrand(seed) must be
   called once. (seed is any 32-bit integer.)
   Integer generator is obtained by modifying two lines.
     Coded by Takuji Nishimura, considering the suggestions by
   Topher Cooper and Marc Rieffel in July-Aug. 1997.

   Copyright (C) 1997, 1999 Makoto Matsumoto and Takuji Nishimura.
   When you use this, send an email to: matumoto@math.keio.ac.jp
   with an appropriate reference to your work.

   REFERENCE
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.
*/

/* Period parameters */
#define N 624
#define M 397
#define MATRIX_A 0x9908b0df   /* constant vector a */
#define UPPER_MASK 0x80000000 /* most significant w-r bits */
#define LOWER_MASK 0x7fffffff /* least significant r bits */

/* Tempering parameters */
#define TEMPERING_MASK_B 0x9d2c5680
#define TEMPERING_MASK_C 0xefc60000
#define TEMPERING_SHIFT_U(y)  (y >> 11)
#define TEMPERING_SHIFT_S(y)  (y << 7)
#define TEMPERING_SHIFT_T(y)  (y << 15)
#define TEMPERING_SHIFT_L(y)  (y >> 18)

static Int32 *mt = dummy+1; /* the array for the state vector  */
static int mti=N+1; /* mti==N+1 means mt[N] is not initialized */

/* Initializing the array with a seed */
static void
MT_sgenrand(Int32 seed)
{
    int i;

    for (i = 0; i < N; i++) {
	mt[i] = seed & 0xffff0000;
	seed = 69069 * seed + 1;
	mt[i] |= (seed & 0xffff0000) >> 16;
	seed = 69069 * seed + 1;
    }
    mti = N;
}

/* Initialization by "sgenrand()" is an example. Theoretically,
   there are 2^19937-1 possible states as an intial state.
   Essential bits in "seed_array[]" is following 19937 bits:
    (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1].
   (seed_array[0]&LOWER_MASK) is discarded.
   Theoretically,
    (seed_array[0]&UPPER_MASK), seed_array[1], ..., seed_array[N-1]
   can take any values except all zeros.                             */

static double MT_genrand(void)
{
    Int32 y;
    static Int32 mag01[2]={0x0, MATRIX_A};
    /* mag01[x] = x * MATRIX_A  for x=0,1 */

    mti = dummy[0];

    if (mti >= N) { /* generate N words at one time */
	int kk;

	if (mti == N+1)   /* if sgenrand() has not been called, */
	    MT_sgenrand(4357); /* a default initial seed is used   */

	for (kk = 0; kk < N - M; kk++) {
	    y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
	    mt[kk] = mt[kk+M] ^ (y >> 1) ^ mag01[y & 0x1];
	}
	for (; kk < N - 1; kk++) {
	    y = (mt[kk] & UPPER_MASK) | (mt[kk+1] & LOWER_MASK);
	    mt[kk] = mt[kk+(M-N)] ^ (y >> 1) ^ mag01[y & 0x1];
	}
	y = (mt[N-1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
	mt[N-1] = mt[M-1] ^ (y >> 1) ^ mag01[y & 0x1];

	mti = 0;
    }

    y = mt[mti++];
    y ^= TEMPERING_SHIFT_U(y);
    y ^= TEMPERING_SHIFT_S(y) & TEMPERING_MASK_B;
    y ^= TEMPERING_SHIFT_T(y) & TEMPERING_MASK_C;
    y ^= TEMPERING_SHIFT_L(y);
    dummy[0] = mti;

    return ( (double)y * 2.3283064365386963e-10 ); /* reals: [0,1)-interval */
}

/*
   The following code was taken from earlier versions of
   http://www-cs-faculty.stanford.edu/~knuth/programs/rng.c-old
   http://www-cs-faculty.stanford.edu/~knuth/programs/rng.c
*/


#define long Int32
#define ran_arr_buf       R_KT_ran_arr_buf
#define ran_arr_cycle     R_KT_ran_arr_cycle
#define ran_arr_ptr       R_KT_ran_arr_ptr
#define ran_arr_sentinel  R_KT_ran_arr_sentinel
#define ran_x             dummy

#define KK 100                     /* the long lag */
#define LL  37                     /* the short lag */
#define MM (1L<<30)                 /* the modulus */
#define TT  70   /* guaranteed separation between streams */
#define mod_diff(x,y) (((x)-(y))&(MM-1)) /* subtraction mod MM */
#define is_odd(x)  ((x)&1)          /* units bit of x */
static void ran_array(long aa[],int n)    /* put n new random numbers in aa */
{
  register int i,j;
  for (j=0;j<KK;j++) aa[j]=ran_x[j];
  for (;j<n;j++) aa[j]=mod_diff(aa[j-KK],aa[j-LL]);
  for (i=0;i<LL;i++,j++) ran_x[i]=mod_diff(aa[j-KK],aa[j-LL]);
  for (;i<KK;i++,j++) ran_x[i]=mod_diff(aa[j-KK],ran_x[i-LL]);
}
#define QUALITY 1009 /* recommended quality level for high-res use */
static long ran_arr_buf[QUALITY];
static long ran_arr_sentinel=(long)-1;
static long *ran_arr_ptr=&ran_arr_sentinel; /* the next random number, or -1 */

static long ran_arr_cycle(void)
{
  ran_array(ran_arr_buf,QUALITY);
  ran_arr_buf[KK]=(long)(-1);
  ran_arr_ptr=ran_arr_buf+1;
  return ran_arr_buf[0];
}

/* ===================  Knuth TAOCP  2002 ========================== */

/*    This program by D E Knuth is in the public domain and freely copyable.
 *    It is explained in Seminumerical Algorithms, 3rd edition, Section 3.6
 *    (or in the errata to the 2nd edition --- see
 *        http://www-cs-faculty.stanford.edu/~knuth/taocp.html
 *    in the changes to Volume 2 on pages 171 and following).              */

/*    N.B. The MODIFICATIONS introduced in the 9th printing (2002) are
      included here; there's no backwards compatibility with the original. */


static void ran_start(long seed)
{
  register int t,j;
  long x[KK+KK-1];              /* the preparation buffer */
  register long ss=(seed+2)&(MM-2);
  for (j=0;j<KK;j++) {
    x[j]=ss;                      /* bootstrap the buffer */
    ss<<=1; if (ss>=MM) ss-=MM-2; /* cyclic shift 29 bits */
  }
  x[1]++;              /* make x[1] (and only x[1]) odd */
  for (ss=seed&(MM-1),t=TT-1; t; ) {
    for (j=KK-1;j>0;j--) x[j+j]=x[j], x[j+j-1]=0; /* "square" */
    for (j=KK+KK-2;j>=KK;j--)
      x[j-(KK-LL)]=mod_diff(x[j-(KK-LL)],x[j]),
      x[j-KK]=mod_diff(x[j-KK],x[j]);
    if (is_odd(ss)) {              /* "multiply by z" */
      for (j=KK;j>0;j--)  x[j]=x[j-1];
      x[0]=x[KK];            /* shift the buffer cyclically */
      x[LL]=mod_diff(x[LL],x[KK]);
    }
    if (ss) ss>>=1; else t--;
  }
  for (j=0;j<LL;j++) ran_x[j+KK-LL]=x[j];
  for (;j<KK;j++) ran_x[j-LL]=x[j];
  for (j=0;j<10;j++) ran_array(x,KK+KK-1); /* warm things up */
  ran_arr_ptr=&ran_arr_sentinel;
}
/* ===================== end of Knuth's code ====================== */

static void RNG_Init_KT2(Int32 seed)
{
    ran_start(seed % 1073741821);
    KT_pos = 100;
}

static Int32 KT_next(void)
{
    if(KT_pos >= 100) {
	ran_arr_cycle();
	KT_pos = 0;
    }
    return ran_x[(KT_pos)++];
}

static void RNG_Init_R_KT(Int32 seed)
{
    SEXP fun, sseed, call, ans;
    fun = findVar1(install(".TAOCP1997init"), R_BaseEnv, CLOSXP, FALSE);
    if(fun == R_UnboundValue)
	error("function '.TAOCP1997init' is missing");
    PROTECT(sseed = ScalarInteger((int)(seed % 1073741821)));
    PROTECT(call = lang2(fun, sseed));
    ans = eval(call, R_GlobalEnv);
    memcpy(dummy, INTEGER(ans), 100*sizeof(int));
    UNPROTECT(2);
    KT_pos = 100;
}
