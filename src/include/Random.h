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
    MERSENNE_TWISTER,
} RNGtype;

/* Different kind of "N(0,1)" generators :*/
typedef enum {
    AHRENS_DIETER,
    KINDERMAN_RAMAGE,
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
