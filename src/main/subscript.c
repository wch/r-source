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

	/* Get a single index for the [[ operator. */
	/* Check that only one index is being selected. */

int get1index(SEXP s, SEXP names, int pok)
{
	int k, i, len;

	if (length(s) > 1)
		error("attempt to select more than one element\n");
	if (length(s) < 1)
		error("attempt to select less than one element\n");
	if (TYPEOF(s) == INTSXP || TYPEOF(s) == LGLSXP)
		k = INTEGER(s)[0] - 1;
	else if (TYPEOF(s) == REALSXP)
		k = REAL(s)[0] - 1;
	else if (TYPEOF(s) == STRSXP) {
		k = -1;
		for (i = 0; i < length(names); i++)
			if (streql(CHAR(STRING(names)[i]), CHAR(STRING(s)[0]))) {
				k = i;
				break;
			}
		if( pok && k < 0 ) { /*partial match*/
			len=strlen(CHAR(STRING(s)[0]));
			for(i = 0; i < length(names); i++) {
				if(!strncmp(CHAR(STRING(names)[i]),CHAR(STRING(s)[0]), len))
					if(k == -1 )
						k = i;
					else
						k = -2;
			}
		}
			
	}
	else if (isSymbol(s)) {
		k = -1;
		for (i = 0; i < length(names); i++)
			if (streql(CHAR(STRING(names)[i]), CHAR(PRINTNAME(s)))) {
				k = i;
				break;
			}
	}
	else
		error("invalid subscript type\n");
	return k;
}

	/* Special Matrix Subscripting: Handles the case x[i] where */
	/* x is an n-way array and i is a matrix with n columns. */
	/* This code returns a vector containing the integer subscripts */
	/* to be extracted when x is regarded as unravelled. */

SEXP mat2indsub(SEXP dims, SEXP s)
{
	int tdim, j, i, nrs = nrows(s);
	SEXP rvec;

	PROTECT(rvec = allocVector(INTSXP, nrs));
	s = coerceVector(s, INTSXP);
	setIVector(INTEGER(rvec), nrs, 0);

	/* compute 0-based subscripts */
	for (i = 0; i < nrs; i++) {
		tdim = 1;
		for (j = 0; j < LENGTH(dims); j++) {
			if(INTEGER(s)[i + j * nrs] == NA_INTEGER) {
				INTEGER(rvec)[i] = NA_INTEGER;
				break;
			}
			if (INTEGER(s)[i + j * nrs] > INTEGER(dims)[j])
				error("subscript out of bounds\n");
			INTEGER(rvec)[i] += (INTEGER(s)[i+j*nrs] - 1) * tdim;
			tdim *= INTEGER(dims)[j];
		}
		/* transform to 1 based subscripting */
		if(INTEGER(rvec)[i] != NA_INTEGER)
			INTEGER(rvec)[i]++;
	}
	UNPROTECT(1);
	return (rvec);
}

static SEXP nullSubscript(int n)
{
	int i;
	SEXP index;

	index = allocVector(INTSXP, n);
	for (i = 0; i < n; i++)
		INTEGER(index)[i] = i + 1;
	return index;
}

static SEXP logicalSubscript(SEXP s, int ns, int nx)
{
	int count, i;
	SEXP index;

	if (ns != nx)
		error("invalid subscript type\n");
	count = 0;
	for (i = 0; i < nx; i++)
		if (LOGICAL(s)[i])
			count++;
	index = allocVector(INTSXP, count);
	count = 0;
	for (i = 0; i < nx; i++)
		if (LOGICAL(s)[i]) {
			if (LOGICAL(s)[i] == NA_LOGICAL)
				INTEGER(index)[count++] = NA_INTEGER;
			else
				INTEGER(index)[count++] = i + 1;
		}
	return index;
}

static SEXP negativeSubscript(SEXP s, int ns, int nx)
{
	SEXP index;
	int i;

	PROTECT(index = allocVector(INTSXP, nx));
	for (i = 0; i < nx; i++)
		INTEGER(index)[i] = 1;
	for (i = 0; i < ns; i++)
		if (INTEGER(s)[i] != 0)
			INTEGER(index)[-INTEGER(s)[i] - 1] = 0;
	s = logicalSubscript(index, nx, nx);
	UNPROTECT(1);
	return s;
}

static SEXP positiveSubscript(SEXP s, int ns, int nx)
{
	SEXP index;
	int i, zct = 0;

	for (i = 0; i < ns; i++)
		if (INTEGER(s)[i] == 0)
			zct++;

	if (zct) {
		index = allocVector(INTSXP, (ns - zct));
		for (i = 0, zct = 0; i < ns; i++)
			if (INTEGER(s)[i] != 0)
				INTEGER(index)[zct++] = INTEGER(s)[i];
		return index;
	}
	else
		return s;
}

static SEXP integerSubscript(SEXP s, int ns, int nx, int *stretch)
{
	int i, ii, min, max, canstretch;
	canstretch = *stretch;
	*stretch = 0;

	min = 0;
	max = 0;
	for (i = 0; i < ns; i++) {
		ii = INTEGER(s)[i];
		if (ii != NA_INTEGER) {
			if (ii < min)
				min = ii;
			if (ii > max)
				max = ii;
		}
	}

	if (min < -nx)
		error("subscript out of bounds\n");
	if (max > nx) {
		if(canstretch) *stretch = max;
		else error("subscript out of bounds\n");
	}

	if (min < 0) {
		if (max == 0) return negativeSubscript(s, ns, nx);
		else error("only 0's may mix with negative subscripts\n");
	}
	else return positiveSubscript(s, ns, nx);
	/*NOTREACHED*/
}

static SEXP stringSubscript(SEXP s, int ns, SEXP names)
{
	SEXP index;
	int i, j, nnames;

	PROTECT(names);
	PROTECT(s);

	index = allocVector(INTSXP, ns);
	nnames = length(names);
	for (i = 0; i < ns; i++) {
		INTEGER(index)[i] = nnames + 1;
		for (j = 0; j < nnames; j++)
			if (streql(CHAR(STRING(s)[i]), CHAR(STRING(names)[j]))) {
				INTEGER(index)[i] = j + 1;
				break;
			}
		if (INTEGER(index)[i] == nnames + 1)
			error("subscript out of bounds\n");
	}
	UNPROTECT(2);
	return index;
}

	/* Array Subscripts.  dim is the dimension (0 to k-1), s is */
	/* the subscript list, x is the array to be subscripted. */

SEXP arraySubscript(int dim, SEXP s, SEXP x)
{
	int i, nd, ns, stretch=0;
	SEXP dims, dnames;


	ns = length(s);
	dims = getAttrib(x, R_DimSymbol);
	nd = INTEGER(dims)[dim];

	switch (TYPEOF(s)) {
	case NILSXP:
		return allocVector(INTSXP, 0);
	case LGLSXP:
		return logicalSubscript(s, ns, nd);
	case INTSXP:
		return integerSubscript(s, ns, nd, &stretch);
	case REALSXP:
		return integerSubscript(coerceVector(s, INTSXP), ns, nd, &stretch);
	case STRSXP:
		dnames = getAttrib(x, R_DimNamesSymbol);
		if (dnames == R_NilValue)
			error("no dimnames attribute for array\n");
		for (i = 0; i < dim; i++)
			dnames = CDR(dnames);
		dnames = CAR(dnames);
		return stringSubscript(s, ns, dnames);
	case SYMSXP:
		if (s == R_MissingArg)
			return nullSubscript(nd);
	default:
		error("invalid subscript\n");
	}
	/*NOTREACHED*/
}


SEXP frameSubscript(int dim, SEXP s, SEXP x)
{
	int ns, nd, stretch=0;
	SEXP d;

	ns = length(s);
	if(dim == 0) nd = nrows(CAR(x));
	else if(dim == 1) nd = length(x);
	else error("too many subscripts for data frame\n");

	switch (TYPEOF(s)) {
	case NILSXP:
		return nullSubscript(nd);
	case LGLSXP:
		return logicalSubscript(s, ns, nd);
	case INTSXP:
		return integerSubscript(s, ns, nd, &stretch);
	case REALSXP:
		return integerSubscript(coerceVector(s, INTSXP), ns, nd, &stretch);
	case STRSXP:
		if(dim == 0) d = getAttrib(x, install("row.names"));
		else d = getAttrib(x, R_NamesSymbol);
		return stringSubscript(s, ns, d);
	case SYMSXP:
		if (s == R_MissingArg)
			return nullSubscript(nd);
	default:
		error("invalid subscript\n");
	}
	/*NOTREACHED*/
}

	/* Subscript creation.  The first thing we do is check to see */
	/* if there are any user supplied NULL's, these result in */
	/* returning a vector of length 0. */

SEXP makeSubscript(SEXP x, SEXP s, int *stretch)
{
	int nx, ns;
	SEXP names, tmp;

	if (isVector(x) || isList(x) || isLanguage(x)) {
		nx = length(x);
		ns = length(s);
		switch (TYPEOF(s)) {
		case NILSXP:
			*stretch = 0;
			return allocVector(INTSXP, 0);
		case LGLSXP:
			*stretch = 0;
			return logicalSubscript(s, ns, nx);
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
			return integerSubscript(s, ns, nx, stretch);
		case REALSXP:
			PROTECT(tmp = coerceVector(s, INTSXP));
			tmp = integerSubscript(tmp, ns, nx, stretch);
			UNPROTECT(1);
			return tmp;
		case STRSXP:
			*stretch = 0;
			names = getAttrib(x, R_NamesSymbol);
			return stringSubscript(s, ns, names);
		case SYMSXP:
			*stretch = 0;
			if (s == R_MissingArg) {
				return nullSubscript(nx);
			}
		default:
			error("invalid subscript type\n");
		}
	}
	else error("subscripting on non-vector\n");
	/*NOTREACHED*/
}
