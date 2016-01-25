/*  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2010	The R Foundation
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

#include "modreg.h"
#include <math.h>

/* To be "exported" (as part of R's C API): */
/**
 * Modify the slopes  m_k := s'(x_k) using Fritsch & Carlson (1980)'s algorithm
 *
 * @param m  numeric vector of length n, the preliminary desired slopes s'(x_i), i = 1:n
 * @param S the divided differences (y_{i+1} - y_i) / (x_{i+1} - x_i);        i = 1:(n-1)
 * @param n  == length(m) == 1 + length(S)
 * @return m*: the modified m[]'s: Note that m[] is modified in place
 * @author Martin Maechler, Date: 19 Apr 2010
 */
void monoFC_mod(double *m, double S[], int n)
{
    if(n < 2)
	error(_("n must be at least two"));

    for(int k = 0; k < n - 1; k++) {
	/* modify both (m[k] & m[k+1]) if needed : */
	double Sk = S[k];
	int k1 = k + 1;
        if(Sk == 0.) { /* or |S| < eps ?? FIXME ?? */
	    m[k] = m[k1] = 0.;
        } else {
            double
		alpha = m[k ] / Sk,
		beta  = m[k1] / Sk, a2b3, ab23;
	    if((a2b3 = 2*alpha + beta - 3) > 0 &&
	       (ab23 = alpha + 2*beta - 3) > 0 &&
	       alpha * (a2b3 + ab23) < a2b3*a2b3) {
		/* we are outside the monotonocity region ==> fix slopes */
		double tauS = 3*Sk / sqrt(alpha*alpha + beta*beta);
		m[k ] = tauS * alpha;
		m[k1] = tauS * beta;
	    }
        }
    } /* end for */
}

SEXP monoFC_m(SEXP m, SEXP Sx)
{
    SEXP val;
    int n = LENGTH(m);

    if (isInteger(m))
	val = PROTECT(coerceVector(m, REALSXP));
    else {
	if (!isReal(m))
	    error(_("Argument m must be numeric"));
	val = PROTECT(duplicate(m));
    }
    if(n < 2) error(_("length(m) must be at least two"));
    if(!isReal(Sx) || LENGTH(Sx) != n-1)
	error(_("Argument Sx must be numeric vector one shorter than m[]"));

    /* Fix up the slopes m[] := val[]: */
    monoFC_mod(REAL(val), REAL(Sx), n);

    UNPROTECT(1);
    return(val);
}
