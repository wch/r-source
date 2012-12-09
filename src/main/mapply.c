/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-12   The R Core Team
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
 *  http://www.r-project.org/Licenses/
 *
 */
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

SEXP attribute_hidden
do_mapply(SEXP f, SEXP varyingArgs, SEXP constantArgs, SEXP rho)
{

    int m = length(varyingArgs), *lengths, *counters, longest = 0, zero = 0;
    SEXP vnames = PROTECT(getAttrib(varyingArgs, R_NamesSymbol));
    Rboolean named = vnames != R_NilValue;

    lengths = (int *)  R_alloc(m, sizeof(int));
    for(int i = 0; i < m; i++) {
	lengths[i] = length(VECTOR_ELT(varyingArgs, i));
	if(lengths[i] == 0) zero++;
	if (lengths[i] > longest) longest = lengths[i];
    }
    if (zero && longest)
	error(_("Zero-length inputs cannot be mixed with those of non-zero length"));

    counters = (int *) R_alloc(m, sizeof(int));
    memset(counters, 0, m * sizeof(int));

    SEXP mindex = PROTECT(allocVector(VECSXP, m));
    SEXP nindex = PROTECT(allocVector(VECSXP, m));

    /* build a call like
       f(dots[[1]][[4]], dots[[2]][[4]], dots[[3]][[4]], d=7)
    */

    SEXP fcall = R_NilValue; // -Wall
    if (constantArgs == R_NilValue)
	;
    else if(isVectorList(constantArgs))
	fcall = VectorToPairList(constantArgs);
    else
	error(_("argument 'MoreArgs' of 'mapply' is not a list"));
    PROTECT_INDEX fi;
    PROTECT_WITH_INDEX(fcall, &fi);

    for(int j = m - 1; j >= 0; j--) {
	SET_VECTOR_ELT(mindex, j, ScalarInteger(j + 1));
	SET_VECTOR_ELT(nindex, j, allocVector(INTSXP, 1));
	SEXP tmp1 = PROTECT(lang3(R_Bracket2Symbol, install("dots"),
				  VECTOR_ELT(mindex, j)));
	SEXP tmp2 = PROTECT(lang3(R_Bracket2Symbol, tmp1,
				  VECTOR_ELT(nindex, j)));
	REPROTECT(fcall = LCONS(tmp2, fcall), fi);
	UNPROTECT(2);
	if (named && CHAR(STRING_ELT(vnames, j))[0] != '\0')
	    SET_TAG(fcall, install(translateChar(STRING_ELT(vnames, j))));
    }

    REPROTECT(fcall = LCONS(f, fcall), fi);

    SEXP ans = PROTECT(allocVector(VECSXP, longest));

    for(int i = 0; i < longest; i++) {
	for(int j = 0; j < m; j++) {
	    counters[j] = (++counters[j] > lengths[j]) ? 1 : counters[j];
	    INTEGER(VECTOR_ELT(nindex, j))[0] = counters[j];
	}
	SET_VECTOR_ELT(ans, i, eval(fcall, rho));
    }

    for(int j = 0; j < m; j++)
	if (counters[j] != lengths[j])
	    warning(_("longer argument not a multiple of length of shorter"));

    UNPROTECT(5);
    return ans;
}
