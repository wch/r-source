/* Is basically only used by ../main/random.c  & ../nmath/sunif.c 
 */
#ifndef RANDOM_H
#define RANDOM_H

typedef enum {
    WICHMANN_HILL,
    MARSAGLIA_MULTICARRY,
    SUPER_DUPER,
    MERSENNE_TWISTER,

    RAND
} RNGtype;

typedef struct {
    RNGtype kind;
    int n_seed;
    char *name;
} RNGTAB;

extern RNGTAB  RNG_Table[];
extern RNGtype RNG_kind;

extern unsigned long int i1_seed, i2_seed, i_seed[624 - 2];
#define i3_seed (i_seed[0])

#endif
