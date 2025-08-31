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
    problems

    REFERENCES

    Streitberg, B. and Röhmel, J.  (1984).  Exact nonparametrics in APL.  APL
    Quote Quad 14(4), 313-325.

    Streitberg, B. and Röhmel, J.  (1986).  Exact distributions for permutations
    and rank tests: An introduction to some recently published algorithms.
    Statistical Software Newsletter 12(1), 10-17.

    Streitberg, B. and Röhmel, J.  (1987).  Exakte Verteilungen für Rang- und
    Randomisierungstests im allgemeinen c-Stichprobenfall.  EDV in Medizin und
    Biologie 18(1), 12-19.

    *\param score_b score vector (typically ranks)
    *\param m_a integer indicating the sum of m_a elements of score_a
*/

SEXP cpermdist2(SEXP score_b, SEXP m_a) {
    /* Compute the joint permutation distribution of the sum of the first 'm_a'
       elements of 'score_a' and 'score_b'.  In this case the exact conditional
       distribution in the independent two-sample problem is computed. */

    int n, sum_a, sum_b = 0, sum_bp1, s_a = 0, s_b = 0, min_b, idx, idx2, ic;
    double msum = 0.0;
    SEXP x;
    int *iscore_b;
    double *dH, *dx;

    n = LENGTH(score_b);
    iscore_b = INTEGER(score_b);

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
    sum_a = INTEGER(m_a)[0];
    for (int i = n - sum_a; i < n; i++)
        sum_b += iscore_b[i];

    /* initialize H */
    sum_bp1 = sum_b + 1;
    dH = (double*) R_alloc((sum_a + 1) * sum_bp1, sizeof(double));
    for (int i = 0; i <= sum_a; i++) {
        idx = i * sum_bp1;
        for (int j = 0; j <= sum_b; j++)
            dH[idx + j] = 0.0;
    }

    /* start the shift algorithm with H[0,0] = 1 */
    dH[0] = 1.0;
    ic = 10000;
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

    PROTECT(x = allocVector(REALSXP, sum_b));
    dx = REAL(x);
    /* get the values for sample size sum_a (i.e., m_a) (in row m) and sum it up */
    idx = sum_a * sum_bp1 + 1;
    for (int j = 0; j < sum_b; j++) {
        if (!R_FINITE(dH[idx + j]))
            error("overflow error; cannot compute exact distribution");
        dx[j] = dH[idx + j];
        msum += dx[j];
    }
    if (!R_FINITE(msum) || msum == 0.0)
        error("overflow error; cannot compute exact distribution");
    /* compute probabilities and return the density x to R
       Note: the support is min(score_b):sum(score_b) */
    for (int j = 0; j < sum_b; j++)
        dx[j] = dx[j] / msum;

    UNPROTECT(1);
    return(x);
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

    *\param scores score vector (such as rank(abs(y)) for wilcoxsign_test)
*/

SEXP cpermdist1(SEXP scores) {
    /* compute the permutation distribution of the sum of the absolute values of
       the positive elements of 'scores' */

    int n, sum_a = 0, s_a = 0, ic;
    double msum = 0.0;
    SEXP H;
    int *iscores;
    double *dH;

    n = LENGTH(scores);
    iscores = INTEGER(scores);

    for (int i = 0; i < n; i++)
        sum_a += iscores[i];

    /* initialize H */
    PROTECT(H = allocVector(REALSXP, sum_a + 1));
    dH = REAL(H);
    for (int i = 0; i <= sum_a; i++)
        dH[i] = 0.0;

    /* start the shift algorithm with H[0] = 1.0 */
    dH[0] = 1.0;
    ic = 10000;
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
        dH[i] = dH[i] / msum; /* 0 is a possible realization */

    UNPROTECT(1);
    return(H);
}
