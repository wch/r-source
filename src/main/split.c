/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"

SEXP do_split(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP x, f, counts, vec, ans;
	int i, j, k, nobs, nlevs, nfac;

	checkArity(op, args);
	
	x = CAR(args);
	f = CADR(args);
	if(!isVector(x))
		errorcall(call, "first argument must be a vector\n");

	if(!isFactor(f))
		errorcall(call, "second argument must be a factor\n");
	nlevs = LEVELS(f);

        nfac=LENGTH(CADR(args));
	nobs=LENGTH(CAR(args));

	if(nobs  <= 0)
		return R_NilValue;

	if( nfac<=0 )
		errorcall(call,"Group length is 0 but data length > 0");
	
	if( nobs != nfac  )
		warningcall(call,"argument lengths differ\n");

	PROTECT(counts = allocVector(INTSXP, nlevs));
	for(i=0 ; i<nlevs ; i++)
		INTEGER(counts)[i] = 0;
	for(i=0 ; i<nobs ; i++) {
		j = INTEGER(f)[i%nfac];
		if(j != NA_INTEGER) {
			INTEGER(counts)[j-1] += 1;
		}
	}
	/* allocate pointers */
	PROTECT(vec = allocVector(STRSXP, nlevs));
	for(i=0 ; i<nlevs ; i++) {
		STRING(vec)[i] = allocVector(TYPEOF(x), INTEGER(counts)[i]);
		LEVELS(STRING(vec)[i])=LEVELS(x);
		setAttrib(STRING(vec)[i],R_LevelsSymbol,getAttrib(x,R_LevelsSymbol));
	}
	for(i=0 ; i<nlevs ; i++)
		INTEGER(counts)[i] = 0;
	for(i=0 ; i<nobs ; i++) {
		j = INTEGER(f)[i%nfac];
		if(j != NA_INTEGER) {
			k = INTEGER(counts)[j-1];
			switch(TYPEOF(x)) {
				case LGLSXP:
				case FACTSXP:
				case ORDSXP:
				case INTSXP:
					INTEGER(STRING(vec)[j-1])[k] = INTEGER(x)[i];
					break;
				case REALSXP:
					REAL(STRING(vec)[j-1])[k] = REAL(x)[i];
					break;
				case CPLXSXP:
					COMPLEX(STRING(vec)[j-1])[k] = COMPLEX(x)[i];
					break;
				case STRSXP:
					STRING(STRING(vec)[j-1])[k] = STRING(x)[i];
					break;
			}
			INTEGER(counts)[j-1] += 1;
		}
	}
	PROTECT(ans = allocList(nlevs));
	x = ans;
	for(i=0 ; i<nlevs ; i++) {
		CAR(x) = STRING(vec)[i];
		x = CDR(x);
	}
	UNPROTECT(3);
	x = ans;
	if((vec = getAttrib(f, R_LevelsSymbol)) != R_NilValue) {
		for(i=0 ; i<nlevs ; i++) {
			TAG(x) = install(CHAR(STRING(vec)[i]));
			x = CDR(x);
		}
	}
	return ans;
}
