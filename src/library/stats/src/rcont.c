/* Algorithm AS 159 Applied Statistics (1981), vol. 30, no. 1
   original (C) Royal Statistical Society 1981

   Generate random two-way table with given marginal totals.

   Heavily pretty edited by Martin Maechler, Dec 2003
   use double precision for integer multiplication (against overflow);
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

#include <math.h>

#include <R_ext/Random.h>
#include <R_ext/Applic.h>
#include <R_ext/Boolean.h>
#include <R_ext/Error.h>
#include <R_ext/Print.h>
#include <R_ext/Utils.h>
#ifdef DEBUG_rcont2
# include <limits.h>
#endif

#include "stats.h"

// NB: Exported via S_rcont() --> ../../../include/R_ext/stats_stubs.h & stats_package.h
void
rcont2(int nrow, int ncol,
       /* vectors of row and column totals, and their sum ntotal: */
       const int nrowt[], const int ncolt[], int ntotal,
       const double fact[],
       int *jwork, int *matrix)
{
    int nr_1 = nrow - 1,
	nc_1 = ncol - 1,
	ib = 0; /* -Wall */

    /* Construct random matrix */
    for (int j = 0; j < nc_1; ++j)
	jwork[j] = ncolt[j];

    int jc = ntotal;
    for (int l = 0; l < nr_1; ++l) { /* -----  matrix[ l, * ] ----- */
	int ia = nrowt[l],
	    ic = jc;
	jc -= ia;/* = n_tot - sum(nr[0:l]) */

	for (int m = 0; m < nc_1; ++m) {
	    int id = jwork[m],
		ie = ic, ii;
	    ib = ie - ia;
	    ii = ib - id;
	    ic -= id;

	    if (ie == 0) { /* Row [l,] is full, fill rest with zero entries */
		for (int j = m; j < nc_1; ++j)
		    matrix[l + j * nrow] = 0;
		ia = 0;
		break;
	    }

	    /* Generate pseudo-random number */
	    double U = unif_rand();
	    int nlm;
	    do {/* Outer Loop */

		/* Compute conditional expected value of MATRIX(L, M) */

		nlm = (int)(ia * (id / (double) ie) + 0.5);
		double x = exp(fact[ia] + fact[ib] + fact[ic] + fact[id]
			       - fact[ie] - fact[nlm]
			       - fact[id - nlm] - fact[ia - nlm] - fact[ii + nlm]);
		if (x >= U)
		    break;
		if (x == 0.)/* MM: I haven't seen this anymore */
		    error(_("rcont2 [%d,%d]: exp underflow to 0; algorithm failure"), l, m);

		double sumprb = x,
		    y = x;

		int nll = nlm;
		Rboolean lsp;
		do {
		    /* Increment entry in row L, column M */
		    double j = (id - nlm) * (double)(ia - nlm);
#ifdef DEBUG_rcont2
		    if(j > INT_MAX)
			REprintf("Incr.: j = %20.20g > INT_MAX !! (id,ia,nlm) = (%d,%d,%d)\n",
				 j, id,ia,nlm);
#endif
		    lsp = ((nlm == ia) || (nlm == id));
		    if (!lsp) {
			++nlm;
			x *= j / ((double) nlm * (ii + nlm));
			sumprb += x;
			if (sumprb >= U)
			    goto L160;
		    }

		    Rboolean lsm;
		    do {
			R_CheckUserInterrupt();

			/* Decrement entry in row L, column M */
			j = (nll * (double)(ii + nll));
#ifdef DEBUG_rcont2
			if(j > INT_MAX)
			    REprintf("Decr.: j = %20.20g > INT_MAX !! (ii,nll) = (%d,%d)\n",
				     j, ii,nll);
#endif
			lsm = (nll == 0);
			if (!lsm) {
			    --nll;
			    y *= j / ((double) (id - nll) * (ia - nll));
			    sumprb += y;
			    if (sumprb >= U) {
				nlm = nll;
				goto L160;
			    }
			    /* else */
			    if (!lsp)
				break;/* to while (!lsp) */
			}
		    } while (!lsm);

		} while (!lsp);

		U = sumprb * unif_rand();

	    } while (1); // 'Outer Loop'

L160:
	    matrix[l + m * nrow] = nlm;
	    ia -= nlm;
	    jwork[m] -= nlm;
	}// for (m = 0..nc_1-1)
	matrix[l + nc_1 * nrow] = ia;/* last column in row l */
    } // for (l = ...)

    /* Compute entries in last row of MATRIX */
    for (int m = 0; m < nc_1; ++m)
	matrix[nr_1 + m * nrow] = jwork[m];

    matrix[nr_1 + nc_1 * nrow] = ib - matrix[nr_1 + (nc_1-1) * nrow];

    return;
}
