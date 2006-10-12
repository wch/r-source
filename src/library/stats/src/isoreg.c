/* --- Isotonic regression ---
 * code simplified from VR_mds_fn() which is part of MASS.c,
 * Copyright (C) 1995  Brian Ripley
 * ---
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003	The R Foundation
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
 *  A copy of the GNU General Public License is available via WWW at
 *  http://www.gnu.org/copyleft/gpl.html.  You can also obtain it by
 *  writing to the Free Software Foundation, Inc.,
 *  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA.
 */
#include <R.h>
#include "modreg.h"

SEXP R_isoreg(SEXP y)
{
    int n = LENGTH(y), i, ip, known, n_ip;
    double tmp, slope;
    SEXP yc, yf, iKnots, ans, anames;

    /* unneeded: y = coerceVector(y, REALSXP); */

    /* nicer way to populate a named list ? */
    PROTECT(ans = allocVector(VECSXP, 4));/* list(.) with all */
    SET_VECTOR_ELT(ans, 0, y = y);
    SET_VECTOR_ELT(ans, 1, yc = allocVector(REALSXP, n+1));
    SET_VECTOR_ELT(ans, 2, yf = allocVector(REALSXP, n));
    SET_VECTOR_ELT(ans, 3, iKnots= allocVector(INTSXP, n));

    PROTECT(anames = allocVector(STRSXP, 4));
    SET_STRING_ELT(anames, 0, mkChar("y"));
    SET_STRING_ELT(anames, 1, mkChar("yc"));
    SET_STRING_ELT(anames, 2, mkChar("yf"));
    SET_STRING_ELT(anames, 3, mkChar("iKnots"));
    setAttrib(ans, R_NamesSymbol, anames);
    UNPROTECT(1);

    /* yc := cumsum(0,y) */
    REAL(yc)[0] = 0.;
    tmp = 0.;
    for (i = 0; i < n; i++) {
	tmp += REAL(y)[i];
	REAL(yc)[i + 1] = tmp;
    }
    known = 0; ip = 0, n_ip = 0;
    do {
	slope = R_PosInf;/*1e+200*/
	for (i = known + 1; i <= n; i++) {
	    tmp = (REAL(yc)[i] - REAL(yc)[known]) / (i - known);
	    if (tmp < slope) {
		slope = tmp;
		ip = i;
	    }
	}/* tmp := max{i= kn+1,.., n} slope(p[kn] -> p[i])  and
	  *  ip = argmax{...}... */
	INTEGER(iKnots)[n_ip++] = ip;
	for (i = known; i < ip; i++)
	    REAL(yf)[i] = (REAL(yc)[ip] - REAL(yc)[known]) / (ip - known);
    } while ((known = ip) < n);

    SETLENGTH(iKnots, n_ip);
    UNPROTECT(1);
    return(ans);
}
