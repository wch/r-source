#include <Rmath.h>
#include <R_ext/Random.h>
#include <R_ext/Applic.h>

#include "ctest.h"

/* Driver routine to call RCONT2 from R, B times.
   Calculates the Pearson chi-squared for each generated table.

   Mostly here for historical reasons now that we have r2dtable().
*/

void
chisqsim(int *nrow, int *ncol, int *nrowt, int *ncolt, int *n,
	 int *b, double *expected, int *observed, double *fact,
	 int *jwork, double *results)
{
    int i, j, iter;
    double chisq, e, o;

    /* Calculate log-factorials. */
    fact[0] = 0.;
    for(i = 1; i <= *n; i++)
	fact[i] = lgammafn((double) (i + 1));

    GetRNGstate();

    for(iter = 0; iter < *b; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate chi-squared value from the random table. */
	chisq = 0.;
	for (i = 0; i < *nrow; ++i) {
	    for (j = 0; j < *ncol; ++j) {
		e = expected[i + j * *nrow];
		o = observed[i + j * *nrow];
		chisq += (o - e) * (o - e) / e;
	    }
	}
	results[iter] = chisq;
    }

    PutRNGstate();

    return;
}
