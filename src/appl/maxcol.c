/*
 *  R : A Computer Language for Statistical Data Analysis

 *  Copyright (C) 1994-9 W. N. Venables and B. D. Ripley
 *  Copyright (C) 2007-12  The R Core Team
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
 *  https://www.R-project.org/Licenses/
 */

/* Original (by permission) from
 * MASS/MASS.c by W. N. Venables and B. D. Ripley

 * Find maximum column: designed for probabilities.
 * Uses reservoir sampling to break ties at random.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <R_ext/Arith.h>	/* NA handling */
#include <Rmath.h>		/* fmax2 */
#include <R_ext/Random.h>	/* ..RNGstate */

#include <R_ext/Applic.h>	/* NA handling */

#define RELTOL 1e-5

void R_max_col(double *matrix, int *nr, int *nc, int *maxes, int *ties_meth)
{
    int	  r, c, m;
    size_t  n_r = *nr; // for indexing like r + c * n_r
    double a, b, large;
    Rboolean isna, used_random = FALSE, do_rand = *ties_meth == 1;

    for (r = 0; r < n_r; r++) {
	/* first check row for any NAs and find the largest abs(entry) */
	large = 0.0;
	isna = FALSE;
	for (c = 0; c < *nc; c++) {
	    a = matrix[r + c * n_r];
	    if (ISNAN(a)) { isna = TRUE; break; }
	    if (!R_FINITE(a)) continue;
	    if (do_rand) large = fmax2(large, fabs(a));
	}
	if (isna) { maxes[r] = NA_INTEGER; continue; }

	m = 0;
	a = matrix[r];
	if (do_rand) {
	    double tol = RELTOL * large;
	    int ntie = 1;
	    for (c = 1; c < *nc; c++) {
		b = matrix[r + c * n_r];
		if (b > a + tol) { /* tol could be zero */
		    a = b; m = c;
		    ntie = 1;
		} else if (b >= a - tol) { /* b ~= current max. a */
		    ntie++;
		    if (!used_random) { GetRNGstate(); used_random = TRUE; }
		    if (ntie * unif_rand() < 1.) m = c;
		}
	    }
	} else {
	    if(*ties_meth == 2) /* return the *first* max if there are ties */
		for (c = 1; c < *nc; c++) {
		    b = matrix[r + c * n_r];
		    if (a < b) {
			a = b; m = c;
		    }
		}
	    else if(*ties_meth == 3) /* return the *last* max ... */
		for (c = 1; c < *nc; c++) {
		    b = matrix[r + c * n_r];
		    if (a <= b) {
			a = b; m = c;
		    }
		}
	    else error("invalid 'ties_meth' {should not happen}");
	}
	maxes[r] = m + 1;
    }
    if(used_random) PutRNGstate();
}
