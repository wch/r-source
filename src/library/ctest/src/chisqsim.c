/* Algorithm AS 159 Applied Statistics (1981), vol. 30, no. 1
   original (C) Royal Statistical Society 1981

   Generate random two-way table with given marginal totals.
   */
#ifndef Macintosh
#include <math.h>
#else
#include <fp.h>
#endif

#include <R_ext/Random.h>
#include <R_ext/Applic.h>

#include "ctest.h"

static void
rcont2(int *nrow, int *ncol, int *nrowt, int *ncolt, int *ntotal,
       double *fact, int *jwork, int *matrix)
{
    int nlmp, j, l, m, ia, ib, ic, jc, id, ie, ii, nrowtl, iap, idp,
	igp, ihp, iip, nll, nlm, nrowm, ncolm, lsm, lsp;
    double x, y, dummy, sumprb;

    --jwork;
    --ncolt;
    --nrowt;
    matrix -= *nrow + 1;

    nrowm = *nrow - 1;
    ncolm = *ncol - 1;

    ib = 0;			/* -Wall */

    /* Construct random matrix */
    for (j = 1; j <= ncolm; ++j) {
	jwork[j] = ncolt[j];
    }
    jc = *ntotal;

    /* HOP = ONE */
    for (l = 1; l <= nrowm; ++l) {
	nrowtl = nrowt[l];
	ia = nrowtl;
	ic = jc;
	jc -= nrowtl;
	for (m = 1; m <= ncolm; ++m) {
	    id = jwork[m];
	    ie = ic;
	    ic -= id;
	    ib = ie - ia;
	    ii = ib - id;

	    /* Test for zero entries in MATRIX */
	    if (ie != 0) {
		goto L130;
	    }
	    for (j = m; j <= *ncol; ++j) {
		matrix[l + j * *nrow] = 0;
	    }
	    goto L190;

	    /* Generate pseudo-random number */
L130:
	    dummy = unif_rand();

	    /* Compute conditional expected value of MATRIX(L, M) */
L131:
	    nlm = ia * id / (double) ie + 0.5;
	    iap = ia + 1;
	    idp = id + 1;
	    igp = idp - nlm;
	    ihp = iap - nlm;
	    nlmp = nlm + 1;
	    iip = ii + nlmp;
	    x = exp(fact[iap - 1] + fact[ib] + fact[ic] +
		    fact[idp - 1] - fact[ie] - fact[nlmp - 1] -
		    fact[igp - 1] - fact[ihp - 1] - fact[iip - 1]);
	    if (x >= dummy) {
		goto L160;
	    }
	    sumprb = x;
	    y = x;
	    nll = nlm;
	    lsp = (0);
	    lsm = (0);

	    /* Increment entry in row L, column M */
L140:
	    j = (id - nlm) * (ia - nlm);
	    if (j == 0) {
		goto L156;
	    }
	    ++nlm;
	    x = x * j / (double) (nlm * (ii + nlm));
	    sumprb += x;
	    if (sumprb >= dummy) {
		goto L160;
	    }
L150:
	    if (lsm) {
		goto L155;
	    }

	    /* Decrement entry in row L, column M */
	    j = nll * (ii + nll);
	    if (j == 0) {
		goto L154;
	    }
	    --nll;
	    y = y * j / (double) ((id - nll) * (ia - nll));
	    sumprb += y;
	    if (sumprb >= dummy) {
		goto L159;
	    }
	    if (! lsp) {
		goto L140;
	    }
	    goto L150;
L154:
	    lsm = (1);
L155:
	    if (!lsp) {
		goto L140;
	    }
	    dummy = sumprb * unif_rand();
	    goto L131;
L156:
	    lsp = (1);
	    goto L150;
L159:
	    nlm = nll;
L160:
	    matrix[l + m * *nrow] = nlm;
	    ia -= nlm;
	    jwork[m] -= nlm;
	}
	matrix[l + *ncol * *nrow] = ia;
L190:
	;
    }

    /* Compute entries in last row of MATRIX */

    for (m = 1; m <= ncolm; ++m) {
	matrix[*nrow + m * *nrow] = jwork[m];
    }
    matrix[*nrow + *ncol * *nrow] = ib - matrix[*nrow + ncolm * *nrow];

    return;
}

/* Driver routine to call RCONT2 from R, B times.
   Calculates the Pearson chi-squared for each generated table.
   */

void
chisqsim(int *nrow, int *ncol, int *nrowt, int *ncolt, int *n,
	 int *b, double *expected, int *observed, double *fact,
	 int *jwork, double *results)
{
    /* Local variables */
    int i, j, iter;
    double chi, e, o, x;

    /* Calculate log-factorials */
    x = 0.;
    fact[0] = 0.;
    for (i = 1; i <= *n; ++i) {
	x += log((double) i);
	fact[i] = x;
    }

    GetRNGstate();

    for (iter = 0; iter < *b; ++iter) {
	rcont2(nrow, ncol, nrowt, ncolt, n, fact, jwork, observed);
	/* Calculate chi-squared value from the random table: */
	chi = 0.;
	for (i = 0; i < *nrow; ++i) {
	    for (j = 0; j < *ncol; ++j) {
		e = expected[i + j * *nrow];
		o = observed[i + j * *nrow];
		chi += (o - e) * (o - e) / e;
	    }
	}
	results[iter] = chi;
    }

    PutRNGstate();

    return;
}
