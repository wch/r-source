extern char *S_alloc();  
extern void seed_in(long *);
extern void seed_out();
extern double unif_rand(void);
extern double norm_rand(void);

#include <stdio.h>
#define NULL_ENTRY
#define PROBLEM		fprintf(stderr,
#define RECOVER(x)	); fprintf(stderr, "\n")

#include <float.h>
#define DOUBLE_XMAX	DBL_MAX
