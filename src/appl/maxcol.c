/* Original (by permission) from
 * MASS/MASS.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-9

 * Find maximum column: designed for probabilities.
 * Uses reservoir sampling to break ties at random.
 */

#include "R_ext/Arith.h" /* NA handling */
#include <Rmath.h> /* fmax2 */
#include "R_ext/Random.h" /* ..RNGstate */

#include "R_ext/Applic.h" /* NA handling */

#define RELTOL 1e-5

void R_max_col(double *matrix, int *nr, int *nc, int *maxes)
{
    int	  r, c, m, ntie, n_r = *nr;
    double a, b, tol;

#define NA_check				\
	if (ISNAN(a)) { /* NA or NaN */		\
	    maxes[r] = NA_INTEGER;		\
	    continue;				\
	}

    GetRNGstate();
    for (r = 0; r < n_r; r++) {
	m = 0;
	ntie = 1;
	a = matrix[r];
	NA_check;
	for (c = 1; c < *nc; c++) {
	    b = matrix[r + c * n_r];
	    NA_check;
	    tol = RELTOL * fmax2(fabs(a), fabs(b));
	    if (b >= a + tol) {
		ntie = 1;
		a = b;
		m = c;
	    } else if (b >= a - tol) {
		ntie++;
		if (ntie * unif_rand() < 1.) m = c;
	    }
	}
	maxes[r] = m + 1;
    }
    PutRNGstate();
}
