/* Original (by permission) from
 * MASS/MASS.c by W. N. Venables and B. D. Ripley  Copyright (C) 1994-9

 * Find maximum column: designed for probabilities.
 * Uses reservoir sampling to break ties at random.
 */

#include <R_ext/Arith.h>	/* NA handling */
#include <Rmath.h>		/* fmax2 */
#include <R_ext/Random.h>	/* ..RNGstate */

#include <R_ext/Applic.h>	/* NA handling */

#define RELTOL 1e-5

void R_max_col(double *matrix, int *nr, int *nc, int *maxes)
{
    int	  r, c, m, ntie, n_r = *nr;
    double a, b, tol, large;
    Rboolean isna, used_random=FALSE;

    for (r = 0; r < n_r; r++) {
	/* first check row for any NAs and find the largest entry */
	large = 0.0;
	isna = FALSE;
	for (c = 0; c < *nc; c++) {
	    a = matrix[r + c * n_r];
	    if (ISNAN(a)) { isna = TRUE; break; }
	    large = fmax2(large, fabs(a));
	}
	if (isna) { maxes[r] = NA_INTEGER; continue; }
	tol = RELTOL * large;

	m = 0;
	ntie = 1;
	a = matrix[r];
	for (c = 1; c < *nc; c++) {
	    b = matrix[r + c * n_r];
	    if (b >= a + tol) {
		ntie = 1;
		a = b;
		m = c;
	    } else if (b >= a - tol) {
		ntie++;    
		if (!used_random) { GetRNGstate(); used_random = TRUE; }
		if (ntie * unif_rand() < 1.) m = c;
	    }
	}
	maxes[r] = m + 1;
    }
    if(used_random) PutRNGstate();
}
