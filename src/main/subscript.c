/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

static int integerOneIndex(int i, int len) {
    int index = -1;

    if (i > 0)
	index = i - 1;
    else if (i == 0 || len < 2)
	error("attempt to select less than one element");
    else if (len == 2 && i > -3)
	index = 2 + i;
    else
	error("attempt to select more than one element");
    return(index);
}

int OneIndex(SEXP x, SEXP s, int len, int partial, SEXP *newname)
{
    SEXP names;
    int i, index, nx;

    if (length(s) > 1)
	error("attempt to select more than one element");
    if (length(s) < 1)
	error("attempt to select less than one element");

    index = -1;
    *newname = R_NilValue;
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	index = integerOneIndex(INTEGER(s)[0], len);
	break;
    case REALSXP:
	index = integerOneIndex(REAL(s)[0], len);
	break;
    case STRSXP:
	nx = length(x);
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    /* Try for exact match */
	    for (i = 0; i < nx; i++)
		if (streql(CHAR(STRING(names)[i]),
			   CHAR(STRING(s)[0]))) {
		    index = i;
		    break;
		}
	    /* Try for partial match */
	    if (partial && index < 0) {
		len = strlen(CHAR(STRING(s)[0]));
		for(i = 0; i < nx; i++) {
		    if(!strncmp(CHAR(STRING(names)[i]),
				CHAR(STRING(s)[0]), len)) {
			if(index == -1 )
			    index = i;
			else
			    index = -2;
		    }
		}
	    }
	}
	if (index == -1)
	    index = nx;
	*newname = STRING(s)[0];
	break;
    case SYMSXP:
	nx = length(x);
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    for (i = 0; i < nx; i++)
		if (streql(CHAR(STRING(names)[i]),
			   CHAR(PRINTNAME(s)))) {
		    index = i;
		    break;
		}
	}
	if (index == -1)
	    index = nx;
	*newname = STRING(s)[0];
	break;
    default:
	error("invalid subscript type");
    }
    return index;
}

/* Get a single index for the [[ operator.
   Check that only one index is being selected. */

int get1index(SEXP s, SEXP names, int len, int pok)
{
    int index, i;
    double dblind;

    if (length(s) > 1)
	error("attempt to select more than one element");
    if (length(s) < 1)
	error("attempt to select less than one element");

    index = -1;
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	i = INTEGER(s)[0];
	if(i == NA_INTEGER) 
	    index = -1;
	else
	    index = integerOneIndex(i, len);
	break;
    case REALSXP:
	dblind = REAL(s)[0];
	if(ISNAN(dblind)) 
	    index = -1;
	else
	    index = integerOneIndex((int)dblind, len);
	break;
    case STRSXP:
	/* Try for exact match */
	for (i = 0; i < length(names); i++)
	    if (streql(CHAR(STRING(names)[i]), CHAR(STRING(s)[0]))) {
		index = i;
		break;
	    }
	/* Try for partial match */
	if (pok && index < 0) {
	    len = strlen(CHAR(STRING(s)[0]));
	    for(i = 0; i < length(names); i++) {
		if(!strncmp(CHAR(STRING(names)[i]), CHAR(STRING(s)[0]), len)) {
		    if(index == -1 )
			index = i;
		    else
			index = -2;
		}
	    }
	}
	break;
    case SYMSXP:
	for (i = 0; i < length(names); i++)
	    if (streql(CHAR(STRING(names)[i]), CHAR(PRINTNAME(s)))) {
		index = i;
		break;
	    }
    default:
	error("invalid subscript type");
    }
    return index;
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
		error("subscript out of bounds");
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

static SEXP logicalSubscript(SEXP s, int ns, int nx, int *stretch)
{
    int canstretch, count, i, nmax;
    SEXP index;
    canstretch = *stretch;
#ifdef OLD
    if (ns > nx)
	error("subscript (%d) out of bounds, should be at most %d",
	      ns, nx);
#else
    if (!canstretch && ns > nx)
	error("(subscript) logical subscript too long");
    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;
#endif
    if (ns == 0)
	return(allocVector(INTSXP, 0));
    count = 0;
#ifdef BROKEN
    for (i = 0; i < nx; i++)
#else
    for (i = 0; i < nmax; i++)
#endif
	if (LOGICAL(s)[i%ns])
	    count++;
    index = allocVector(INTSXP, count);
    count = 0;
#ifdef OLD
    for (i = 0; i < nx; i++)
#else
    for (i = 0; i < nmax; i++)
#endif
	if (LOGICAL(s)[i%ns]) {
	    if (LOGICAL(s)[i%ns] == NA_LOGICAL)
		INTEGER(index)[count++] = NA_INTEGER;
	    else
		INTEGER(index)[count++] = i + 1;
	}
    return index;
}

static SEXP negativeSubscript(SEXP s, int ns, int nx)
{
    SEXP index;
    int stretch = 0;
    int i;
    PROTECT(index = allocVector(INTSXP, nx));
    for (i = 0; i < nx; i++)
	INTEGER(index)[i] = 1;
    for (i = 0; i < ns; i++)
	if (INTEGER(s)[i] != 0)
	    INTEGER(index)[-INTEGER(s)[i] - 1] = 0;
    s = logicalSubscript(index, nx, nx, &stretch);
    UNPROTECT(1);
    return s;
}

static SEXP positiveSubscript(SEXP s, int ns, int nx)
{
    SEXP index;
    int i, zct = 0;
    for (i = 0; i < ns; i++) {
	if (INTEGER(s)[i] == 0)
	    zct++;
    }
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
	error("subscript out of bounds");
    if (max > nx) {
	if(canstretch) *stretch = max;
	else error("subscript out of bounds");
    }
    if (min < 0) {
	if (max == 0) return negativeSubscript(s, ns, nx);
	else error("only 0's may mix with negative subscripts");
    }
    else return positiveSubscript(s, ns, nx);
    return R_NilValue;
}

static SEXP stringSubscript(SEXP s, int ns, int nx, SEXP names, int *stretch)
{
    SEXP index, indexnames;
    int i, j, nnames, sub, extra;
    int canstretch = *stretch;
    PROTECT(s);
    PROTECT(names);
    PROTECT(index = allocVector(INTSXP, ns));
    PROTECT(indexnames = allocVector(STRSXP, ns));
    nnames = nx;
    extra = nnames;

    /* Process each of the subscripts */
    /* First we compare with the names on the vector */
    /* and then (if there is no match) with each of */
    /* the previous subscripts. */

    for (i = 0; i < ns; i++) {
	sub = 0;
	if (names != R_NilValue) {
	    for (j = 0; j < nnames; j++)
		if (NonNullStringMatch(STRING(s)[i], STRING(names)[j])) {
		    sub = j + 1;
		    STRING(indexnames)[i] = R_NilValue;
		    break;
		}
	}
	if (sub == 0) {
	    for (j = 0 ; j < i ; j++)
		if (NonNullStringMatch(STRING(s)[i], STRING(s)[j])) {
		    sub = INTEGER(index)[j];
/*		    STRING(indexnames)[i] = STRING(indexnames)[sub - 1];*/
		    STRING(indexnames)[i] = STRING(s)[j];
		    break;
		}
	}
	if (sub == 0) {
	    if (!canstretch)
		error("subscript out of bounds");
	    extra += 1;
	    sub = extra;
	    STRING(indexnames)[i] = STRING(s)[i];
	}
	INTEGER(index)[i] = sub;
    }
    /* Ghastly hack!  We attach the new names to the attribute */
    /* slot on the returned subscript vector. */
    if (extra != nnames) {
	*stretch = extra;
	ATTRIB(index) = indexnames;
    }
    UNPROTECT(4);    
    return index;
}

/* Array Subscripts.  dim is the dimension (0 to k-1), s is */
/* the subscript list, x is the array to be subscripted. */

SEXP arraySubscript(int dim, SEXP s, SEXP x)
{
    int nd, ns, stretch = 0;
    SEXP dims, dnames, tmp;
    ns = length(s);
    dims = getAttrib(x, R_DimSymbol);
    nd = INTEGER(dims)[dim];

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch);
    case REALSXP:
    	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch);
    	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = getAttrib(x, R_DimNamesSymbol);
	if (dnames == R_NilValue)
	    error("no dimnames attribute for array");
	dnames = VECTOR(dnames)[dim];
	return stringSubscript(s, ns, nd, dnames, &stretch);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	error("invalid subscript");
    }
    return R_NilValue;
}

/* Subscript creation.  The first thing we do is check to see */
/* if there are any user supplied NULL's, these result in */
/* returning a vector of length 0. */

SEXP makeSubscript(SEXP x, SEXP s, int *stretch)
{
    int nx, ns;
    SEXP ans, names, tmp;

    ans = R_NilValue;
    if (isVector(x) || isList(x) || isLanguage(x)) {
	names = getAttrib(x, R_NamesSymbol);
	nx = length(x);
	ns = length(s);
	PROTECT(s=duplicate(s));
	ATTRIB(s) = R_NilValue;
	switch (TYPEOF(s)) {
	case NILSXP:
	    *stretch = 0;
	    ans = allocVector(INTSXP, 0);
	    break;
	case LGLSXP:
	    /* *stretch = 0; */
	    ans = logicalSubscript(s, ns, nx, stretch);
	    break;
	case INTSXP:
	    ans = integerSubscript(s, ns, nx, stretch);
	    break;
	case REALSXP:
	    PROTECT(tmp = coerceVector(s, INTSXP));
	    ans = integerSubscript(tmp, ns, nx, stretch);
	    UNPROTECT(1);
	    break;
	case STRSXP:
	    *stretch = 0;
	    ans = stringSubscript(s, ns, nx, names, stretch);
	    break;
	case SYMSXP:
	    *stretch = 0;
	    if (s == R_MissingArg) {
		ans = nullSubscript(nx);
		break;
	    }
	default:
	    error("invalid subscript type");
	}
	UNPROTECT(1);
    }
    else error("subscripting on non-vector");
    return ans;
}
