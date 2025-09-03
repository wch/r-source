/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2023   Torsten Hothorn
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

/**
    Exact Distribution of Two-Sample Permutation Tests
    Streitberg and Röhmel Shift Algorithm

    *\file StreitbergRoehmel.c
*/

#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>		/* for imin2 */

/**
    The density of the permutation distribution for independent two-sample
    problems with integer-valued scores

    REFERENCES

    Streitberg, B. and Röhmel, J.  (1984).  Exact nonparametrics in APL.  APL
    Quote Quad 14(4), 313-325.

    Streitberg, B. and Röhmel, J.  (1986).  Exact distributions for permutations
    and rank tests: An introduction to some recently published algorithms.
    Statistical Software Newsletter 12(1), 10-17.

    Streitberg, B. and Röhmel, J.  (1987).  Exakte Verteilungen für Rang- und
    Randomisierungstests im allgemeinen c-Stichprobenfall.  EDV in Medizin und
    Biologie 18(1), 12-19.

    *\param x: score vector (typically ranks) of length n (total sample size)
    *\param m: integer, the number of observations in one of the two samples.
               The density of the statistic sum(x[seq_len(m)]) is computed.
*/

SEXP dpermdist2(SEXP x, SEXP m) {

    /* Computes the joint permutation distribution of the sum of the first 'm'
       elements of 'score_a' = (1,...,1) and 'score_b = x'.  In this case the exact
       conditional distribution in the independent two-sample problem is 
       computed, see Section 4 of Streitberg, B. and Röhmel, J. (1987) */

    int n, sum_a, sum_b = 0, sum_bp1, s_a = 0, s_b = 0, min_b, idx, idx2, ic;
    double msum = 0.0;
    SEXP ret;
    int *iscore_b;
    double *dH, *dret;

    n = LENGTH(x);
    iscore_b = INTEGER(x);

    /* optimization according to Streitberg and Röhmel
           sum_a := min(sum_a, m_a)
           sum_b := min(sum_b, m_b)
       where
           sum_a = sum(score_a) = sum(1,...,1) = n
             m_a = sum of m_a elements of score_a
           sum_b = sum(score_b)
             m_b = sum of (n - m_a + 1) largest elements of score_b
       whence
           sum_a > m_a => sum_a := min(sum_a, m_a) = m_a
           sum_b > m_b => sum_b := min(sum_b, m_b) = m_b */
    sum_a = INTEGER(m)[0];
    for (int i = n - sum_a; i < n; i++)
        sum_b += iscore_b[i];

    /* initialize H in Algorithm 'Verteilung 3' */
    sum_bp1 = sum_b + 1;
    dH = (double*) R_alloc((sum_a + 1) * sum_bp1, sizeof(double));
    for (int i = 0; i <= sum_a; i++) {
        idx = i * sum_bp1;
        for (int j = 0; j <= sum_b; j++)
            dH[idx + j] = 0.0;
    }

    /* start the shift algorithm with H[0,0] = 1 */
    dH[0] = 1.0;
    ic = 10000; /* interrupt check */
    for (int k = 0; k < n; k++) {
        s_a += 1; /* remember: score_a = (1,...,1) */
        s_b += iscore_b[k];
        /* compute H up to row sum_a (i.e., m_a) and column sum_b (i.e., m_b)
           Note: sum_a = min(sum_a, m) and sum_b = min(sum_b, c) */
        min_b = imin2(sum_b, s_b);
        for (int i = imin2(sum_a, s_a); i >= 1; i--) {
            idx = i * sum_bp1;
            idx2 = (i - 1) * sum_bp1 - iscore_b[k];
            for (int j = min_b; j >= iscore_b[k]; j--) {
                if(!(--ic)) {
                    R_CheckUserInterrupt();
                    ic = 10000;
                }
                dH[idx + j] += dH[idx2 + j];
            }
        }
    }

    PROTECT(ret = allocVector(REALSXP, sum_b));
    dret = REAL(ret);
    /* get the values for sample size sum_a (i.e., m_a) (in row m) and sum it up */
    idx = sum_a * sum_bp1 + 1;
    for (int j = 0; j < sum_b; j++) {
        if (!R_FINITE(dH[idx + j]))
            error("overflow error; cannot compute exact distribution");
        dret[j] = dH[idx + j];
        msum += dret[j];
    }
    if (!R_FINITE(msum) || msum == 0.0)
        error("overflow error; cannot compute exact distribution");
    /* compute probabilities and return the density x to R
       Note: the support is min(score_b):sum(score_b) */
    for (int j = 0; j < sum_b; j++)
        dret[j] /= msum;

    UNPROTECT(1);
    return(ret);
}

/**
    The density of the permutation distribution for one sample problems.

    REFERENCES

    Streitberg, B. and Röhmel, J.  (1984).  Exact nonparametrics in APL.  APL
    Quote Quad 14(4), 313-325.

    Streitberg, B. and Röhmel, J.  (1986).  Exact distributions for permutations
    and rank tests: An introduction to some recently published algorithms.
    Statistical Software Newsletter 12(1), 10-17.

    Streitberg, B. and Röhmel, J.  (1987).  Exakte Verteilungen für Rang- und
    Randomisierungstests im allgemeinen c-Stichprobenfall.  EDV in Medizin und
    Biologie 18(1), 12-19.

    *\param x: score vector (such as rank(abs(y)) for wilcoxsign_test) of length n
*/

SEXP dpermdist1(SEXP x) {

    /* Computes the permutation distribution of the sum of the absolute values of
       the _positive_ elements of 'x' following Section 2 of Streitberg, B.
       and Röhmel, J. (1987). Note that adding zeros to 'x' does not affect
       the distribution */

    int n, sum_a = 0, s_a = 0, ic;
    double msum = 0.0;
    SEXP ret;
    int *iscores;
    double *dH;

    n = LENGTH(x);
    iscores = INTEGER(x);

    for (int i = 0; i < n; i++)
        sum_a += iscores[i];

    /* initialize H in Algorithm 'Verteilung 2' */
    PROTECT(ret = allocVector(REALSXP, sum_a + 1));
    dH = REAL(ret);
    for (int i = 0; i <= sum_a; i++)
        dH[i] = 0.0;

    /* start the shift algorithm with H[0] = 1.0 */
    dH[0] = 1.0;
    ic = 10000; /* interrupt checking */
    for (int k = 0; k < n; k++) {
        s_a += iscores[k];
        for (int i = s_a; i >= iscores[k]; i--) {
            if (!(--ic)) {
                R_CheckUserInterrupt();
                ic = 10000;
            }
            dH[i] += dH[i - iscores[k]];
        }
    }

    /* get the number of permutations */
    for (int i = 0; i <= sum_a; i++) {
        if (!R_FINITE(dH[i]))
            error("overflow error: cannot compute exact distribution");
        msum += dH[i];
    }
    if (!R_FINITE(msum) || msum == 0.0)
        error("overflow error: cannot compute exact distribution");

    /* compute probabilities and return the density H to R */
    for (int i = 0; i <= sum_a; i++)
        dH[i] /= msum; /* 0 is a possible realization */

    UNPROTECT(1);
    return(ret);
}
