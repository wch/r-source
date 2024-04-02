/* --- Isotonic regression ---
 * code simplified from VR_mds_fn() which is part of MASS.c,
 * Copyright (C) 1995  Brian Ripley
 * ---
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2024	The R Core Team
 *  Copyright (C) 2003-2023	The R Foundation
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

// This is written to allow for long vectors, but the return values are not

#include "modreg.h"

SEXP isoreg(SEXP y)
{
    R_xlen_t n = XLENGTH(y), i;
    SEXP yc, yf, iKnots, ans;
    const char *anms[] = {"y", "yc", "yf", "iKnots", ""};

    PROTECT(ans = mkNamed(VECSXP, anms));

    SET_VECTOR_ELT(ans, 0, y);
    SET_VECTOR_ELT(ans, 1, yc = allocVector(REALSXP, n+1));
    SET_VECTOR_ELT(ans, 2, yf = allocVector(REALSXP, n));
    SET_VECTOR_ELT(ans, 3, iKnots= allocVector(INTSXP, n));

    if (n == 0) {
	UNPROTECT(1); /* ans */
	return ans; /* avoid segfault below */
    }

    /* unneeded: y = coerceVector(y, REALSXP); */
    /* yc := cumsum(0,y) */
    REAL(yc)[0] = 0.;
    double tmp = 0.;
    for (i = 0; i < n; i++) {
	tmp += REAL(y)[i];
	REAL(yc)[i + 1] = tmp;
    }
    if(!R_FINITE(REAL(yc)[n]))
	 error(_("non-finite sum(y) == %g is not allowed"), REAL(yc)[n]);
    R_xlen_t known = 0, ip = 0, n_ip = 0;
    do {
	double slope = R_PosInf;/*1e+200*/
	for (i = known + 1; i <= n; i++) {
	    tmp = (REAL(yc)[i] - REAL(yc)[known]) / (i - known);
	    if (tmp < slope) {
		slope = tmp;
		ip = i;
	    }
	}/* tmp := max{i= kn+1,.., n} slope(p[kn] -> p[i])  and
	  *  ip = argmax{...}... */
	INTEGER(iKnots)[n_ip++] = (int) ip;
	for (i = known; i < ip; i++)
	    REAL(yf)[i] = (REAL(yc)[ip] - REAL(yc)[known]) / (ip - known);
    } while ((known = ip) < n);

    if (n_ip < n)
	SET_VECTOR_ELT(ans, 3, xlengthgets(iKnots, n_ip));
    UNPROTECT(1);
    return(ans);
}
