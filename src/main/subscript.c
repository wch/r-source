/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2007  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

/* <UTF8> char here is handled as a whole string */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

/* We might get a call with R_NilValue from subassignment code */
#define ECALL(call, yy) if(call == R_NilValue) error(yy); else errorcall(call, yy);

static int integerOneIndex(int i, int len, SEXP call)
{
    int indx = -1;

    if (i > 0)
	indx = i - 1;
    else if (i == 0 || len < 2) {
	ECALL(call, _("attempt to select less than one element"));
    } else if (len == 2 && i > -3)
	indx = 2 + i;
    else {
	ECALL(call, _("attempt to select more than one element"));
    }
    return(indx);
}

int attribute_hidden
OneIndex(SEXP x, SEXP s, int len, int partial, SEXP *newname, int pos, SEXP call)
{
    SEXP names;
    int i, indx, nx;

    if (pos < 0 && length(s) > 1) {
	ECALL(call, _("attempt to select more than one element"));
    }
    if (pos < 0 && length(s) < 1) {
	ECALL(call, _("attempt to select less than one element"));
    }
    
    if(pos < 0) pos = 0;

    indx = -1;
    *newname = R_NilValue;
    switch(TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	indx = integerOneIndex(INTEGER(s)[pos], len, call);
	break;
    case REALSXP:
	indx = integerOneIndex(REAL(s)[pos], len, call);
	break;
    case STRSXP:
	nx = length(x);
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    /* Try for exact match */
	    for (i = 0; i < nx; i++)
		if (streql(translateChar(STRING_ELT(names, i)),
			   translateChar(STRING_ELT(s, pos)))) {
		    indx = i;
		    break;
		}
	    /* Try for partial match */
	    if (partial && indx < 0) {
		len = strlen(translateChar(STRING_ELT(s, pos)));
		for(i = 0; i < nx; i++) {
		    if(!strncmp(translateChar(STRING_ELT(names, i)),
				translateChar(STRING_ELT(s, pos)), len)) {
			if(indx == -1 )
			    indx = i;
			else
			    indx = -2;
		    }
		}
	    }
	}
	if (indx == -1)
	    indx = nx;
	*newname = STRING_ELT(s, pos);
	break;
    case SYMSXP:
	nx = length(x);
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    for (i = 0; i < nx; i++)
		if (streql(translateChar(STRING_ELT(names, i)),
			   translateChar(PRINTNAME(s)))) {
		    indx = i;
		    break;
		}
	}
	if (indx == -1)
	    indx = nx;
	*newname = STRING_ELT(s, pos);
	break;
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"), 
		      type2char(TYPEOF(s)));
    }
    return indx;
}

int attribute_hidden
get1index(SEXP s, SEXP names, int len, int pok, int pos, SEXP call)
{
/* Get a single index for the [[ operator.
   Check that only one index is being selected.
   pok : is "partial ok" ?
         if pok is -1, warn if partial matching occurs
*/
    int indx, i, warn_pok = 0;
    double dblind;
    const char *ss, *cur_name;

    if (pok == -1) {
        pok = 1;
        warn_pok = 1;
    }

    if (pos < 0 && length(s) != 1) {
	if (length(s) > 1) {
	    ECALL(call, _("attempt to select more than one element"));
	} else {
	    ECALL(call, _("attempt to select less than one element"));
	}
    } else
	if(pos >= length(s)) {
	    ECALL(call, _("internal error in use of recursive indexing"));
	}
    if(pos < 0) pos = 0;
    indx = -1;
    switch (TYPEOF(s)) {
    case LGLSXP:
    case INTSXP:
	i = INTEGER(s)[pos];
	if(i != NA_INTEGER)
	    indx = integerOneIndex(i, len, call);
	break;
    case REALSXP:
	dblind = REAL(s)[pos];
	if(!ISNAN(dblind))
	    indx = integerOneIndex((int)dblind, len, call);
	break;
    case STRSXP:
	/* NA matches nothing */
	if(STRING_ELT(s, pos) == NA_STRING) break;
	
	/* Try for exact match */
	ss = translateChar(STRING_ELT(s, pos));
	for (i = 0; i < length(names); i++) 
	    if (STRING_ELT(names, i) != NA_STRING) {
		if (streql(translateChar(STRING_ELT(names, i)), ss)) {
		    indx = i;
		    break;
		}
	    }
	/* Try for partial match */
	if (pok && indx < 0) {
	    len = strlen(ss);
	    for(i = 0; i < length(names); i++) {
		if (STRING_ELT(names, i) != NA_STRING) {
                    cur_name = translateChar(STRING_ELT(names, i));
		    if(!strncmp(cur_name, ss, len)) {
			if(indx == -1) {/* first one */
			    indx = i;
                            if (warn_pok) {
				if (call == R_NilValue)
				    warning(_("partial match of '%s' to '%s'"),
					    ss, cur_name);
				else
				    warningcall(call, 
						_("partial match of '%s' to '%s'"),
						ss, cur_name);
			    }
                        }
			else {
			    indx = -2;/* more than one partial match */
                            if (warn_pok) /* already given context */
                                warningcall(R_NilValue, 
					    _("further partial match of '%s' to '%s'"),
					    ss, cur_name);
                            break;
                        }
		    }
		}
	    }
	}
	break;
    case SYMSXP:
	for (i = 0; i < length(names); i++)
	    if (STRING_ELT(names, i) != NA_STRING &&
		streql(translateChar(STRING_ELT(names, i)), 
		       CHAR(PRINTNAME(s)))) {
		indx = i;
		break;
	    }
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"), 
		      type2char(TYPEOF(s)));
    }
    return indx;
}

/* Special Matrix Subscripting: Handles the case x[i] where */
/* x is an n-way array and i is a matrix with n columns. */
/* This code returns a vector containing the integer subscripts */
/* to be extracted when x is regarded as unravelled. */
/* Negative indices are not allowed. */
/* A zero anywhere in a row will cause a zero in the same */
/* position in the result. */

SEXP attribute_hidden mat2indsub(SEXP dims, SEXP s, SEXP call)
{
    int tdim, j, i, k, nrs = nrows(s);
    SEXP rvec;

    if (ncols(s) != LENGTH(dims)) {
	ECALL(call, _("incorrect number of columns in matrix subscript"));
    }
    
    PROTECT(rvec = allocVector(INTSXP, nrs));
    s = coerceVector(s, INTSXP);
    setIVector(INTEGER(rvec), nrs, 0);

    for (i = 0; i < nrs; i++) {
	tdim = 1;
	/* compute 0-based subscripts for a row (0 in the input gets -1
	   in the output here) */
	for (j = 0; j < LENGTH(dims); j++) {
	    k = INTEGER(s)[i + j * nrs];
	    if(k == NA_INTEGER) {
		INTEGER(rvec)[i] = NA_INTEGER;
		break;
	    }
	    if(k < 0) {
		ECALL(call, _("negative values are not allowed in a matrix subscript"));
	    }
	    if(k == 0) {
		INTEGER(rvec)[i] = -1;
		break;
	    }
	    if (k > INTEGER(dims)[j]) {
		ECALL(call, _("subscript out of bounds"));
	    }
	    INTEGER(rvec)[i] += (k - 1) * tdim;
	    tdim *= INTEGER(dims)[j];
	}
	/* transform to 1 based subscripting (0 in the input gets 0 
	   in the output here) */
	if(INTEGER(rvec)[i] != NA_INTEGER)
	    INTEGER(rvec)[i]++;
    }
    UNPROTECT(1);
    return (rvec);
}

static SEXP nullSubscript(int n)
{
    int i;
    SEXP indx;
    indx = allocVector(INTSXP, n);
    for (i = 0; i < n; i++)
	INTEGER(indx)[i] = i + 1;
    return indx;
}

static SEXP logicalSubscript(SEXP s, int ns, int nx, int *stretch, SEXP call)
{
    int canstretch, count, i, nmax;
    SEXP indx;
    canstretch = *stretch;
    if (!canstretch && ns > nx) {
	ECALL(call, _("(subscript) logical subscript too long"));
    }
    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;
    if (ns == 0)
	return(allocVector(INTSXP, 0));
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns])
	    count++;
    indx = allocVector(INTSXP, count);
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns]) {
	    if (LOGICAL(s)[i%ns] == NA_LOGICAL)
		INTEGER(indx)[count++] = NA_INTEGER;
	    else
		INTEGER(indx)[count++] = i + 1;
	}
    return indx;
}

static SEXP negativeSubscript(SEXP s, int ns, int nx, SEXP call)
{
    SEXP indx;
    int stretch = 0;
    int i, ix;
    PROTECT(indx = allocVector(LGLSXP, nx));
    for (i = 0; i < nx; i++)
	LOGICAL(indx)[i] = 1;
    for (i = 0; i < ns; i++) {
	ix = INTEGER(s)[i];
	if (ix != 0 && ix != NA_INTEGER && -ix <= nx)
	    LOGICAL(indx)[-ix - 1] = 0;
    }
    s = logicalSubscript(indx, nx, nx, &stretch, call);
    UNPROTECT(1);
    return s;
}

static SEXP positiveSubscript(SEXP s, int ns, int nx)
{
    SEXP indx;
    int i, zct = 0;
    for (i = 0; i < ns; i++) {
	if (INTEGER(s)[i] == 0)
	    zct++;
    }
    if (zct) {
	indx = allocVector(INTSXP, (ns - zct));
	for (i = 0, zct = 0; i < ns; i++)
	    if (INTEGER(s)[i] != 0)
		INTEGER(indx)[zct++] = INTEGER(s)[i];
	return indx;
    }
    else
	return s;
}

static SEXP integerSubscript(SEXP s, int ns, int nx, int *stretch, SEXP call)
{
    int i, ii, min, max, canstretch;
    Rboolean isna = FALSE;
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
	} else isna = TRUE;
    }
    if (max > nx) {
	if(canstretch) *stretch = max;
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (min < 0) {
	if (max == 0 && !isna) return negativeSubscript(s, ns, nx, call);
	else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    }
    else return positiveSubscript(s, ns, nx);
    return R_NilValue;
}

typedef SEXP (*StringEltGetter)(SEXP x, int i);

/* This uses a couple of horrible hacks in conjunction with
 * VectorAssign (in subassign.c).  If subscripting is used for
 * assignment, it is possible to extend a vector by supplying new
 * names, and we want to give the extended vector those names, so they
 * are returned as the attribute. Also, unset elements of the vector
 * of new names (places where a match was found) are indicated by
 * setting the element of the newnames vector to NULL, even though it
 * is a character vector.
*/

/* The original code (pre 2.0.0) used a ns x nx loop that was too
 * slow.  So now we hash.  Hashing is expensive on memory (up to 32nx
 * bytes) so it is only worth doing if ns * nx is large.  If nx is
 * large, then it will be too slow unless ns is very small.
 */

#define USE_HASHING 1
static SEXP
stringSubscript(SEXP s, int ns, int nx, SEXP names,
		StringEltGetter strg, int *stretch, Rboolean in, SEXP call)
{
    SEXP indx, indexnames;
    int i, j, nnames, sub, extra;
    int canstretch = *stretch;
#ifdef USE_HASHING
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = in && ( (ns > 1000 && nx) || (nx > 1000 && ns) || (ns * nx > 1000) );
#else
    Rboolean usehashing = FALSE;
#endif

    PROTECT(s);
    PROTECT(names);
    PROTECT(indexnames = allocVector(STRSXP, ns));
    nnames = nx;
    extra = nnames;

    /* Process each of the subscripts. First we compare with the names
     * on the vector and then (if there is no match) with each of the
     * previous subscripts, since (if assigning) we may have already
     * added an element of that name. (If we are not assigning, any
     * nonmatch will have given an error.)
     */

#ifdef USE_HASHING
    if(usehashing) {
	/* must be internal, so names contains a character vector */
	/* NB: this does not behave in the same way with respect to ""
	   and NA names: they will match */
	PROTECT(indx = match(names, s, 0));
	/* second pass to correct this */
	for (i = 0; i < ns; i++)
	    if(STRING_ELT(s, i) == NA_STRING || !CHAR(STRING_ELT(s, i))[0])
		INTEGER(indx)[i] = 0;
	for (i = 0; i < ns; i++) SET_STRING_ELT(indexnames, i, R_NilValue);
    } else {
#endif
	PROTECT(indx = allocVector(INTSXP, ns));
	for (i = 0; i < ns; i++) {
	    sub = 0;
	    if (names != R_NilValue) {
		for (j = 0; j < nnames; j++) {
		    SEXP names_j = strg(names, j);
		    if (!in && TYPEOF(names_j) != CHARSXP) {
			ECALL(call, _("character vector element does not have type CHARSXP"));
		    }
		    if (NonNullStringMatch(STRING_ELT(s, i), names_j)) {
			sub = j + 1;
			SET_STRING_ELT(indexnames, i, R_NilValue);
			break;
		    }
		}
	    }
	    INTEGER(indx)[i] = sub;
	}
#ifdef USE_HASHING
    }
#endif

    for (i = 0; i < ns; i++) {
	sub = INTEGER(indx)[i];
	if (sub == 0) {
	    for (j = 0 ; j < i ; j++)
		if (NonNullStringMatch(STRING_ELT(s, i), STRING_ELT(s, j))) {
		    sub = INTEGER(indx)[j];
		    SET_STRING_ELT(indexnames, i, STRING_ELT(s, j));
		    break;
		}
	}
	if (sub == 0) {
	    if (!canstretch) {
		ECALL(call, _("subscript out of bounds"));
	    }
	    extra += 1;
	    sub = extra;
	    SET_STRING_ELT(indexnames, i, STRING_ELT(s, i));
	}
	INTEGER(indx)[i] = sub;
    }
    /* We return the new names as the names attribute of the returned
       subscript vector. */
    if (extra != nnames)
	setAttrib(indx, R_NamesSymbol, indexnames);
    if (canstretch)
	*stretch = extra;
    UNPROTECT(4);
    return indx;
}

/* Array Subscripts.
    dim is the dimension (0 to k-1)
    s is the subscript list,
    dims is the dimensions of x
    dng is a function (usually getAttrib) that obtains the dimnames
    x is the array to be subscripted.
*/

typedef SEXP AttrGetter(SEXP x, SEXP data);

static SEXP 
int_arraySubscript(int dim, SEXP s, SEXP dims, AttrGetter dng,
		   StringEltGetter strg, SEXP x, Rboolean in, SEXP call)
{
    int nd, ns, stretch = 0;
    SEXP dnames, tmp;
    ns = length(s);
    nd = INTEGER(dims)[dim];

    switch (TYPEOF(s)) {
    case NILSXP:
	return allocVector(INTSXP, 0);
    case LGLSXP:
	return logicalSubscript(s, ns, nd, &stretch, call);
    case INTSXP:
	return integerSubscript(s, ns, nd, &stretch, call);
    case REALSXP:
    	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, call);
    	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = dng(x, R_DimNamesSymbol);
	if (dnames == R_NilValue) {
	    ECALL(call, _("no 'dimnames' attribute for array"));
	}
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, strg, &stretch, in, call);
    case SYMSXP:
	if (s == R_MissingArg)
	    return nullSubscript(nd);
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"), 
		      type2char(TYPEOF(s)));
    }
    return R_NilValue;
}

/* This is used by packages arules and cba. Seems dangerous as the 
   typedef is not exported */
SEXP
arraySubscript(int dim, SEXP s, SEXP dims, AttrGetter dng,
	       StringEltGetter strg, SEXP x)
{
    return int_arraySubscript(dim, s, dims, dng, strg, x, TRUE, R_NilValue);
}

/* Subscript creation.  The first thing we do is check to see */
/* if there are any user supplied NULL's, these result in */
/* returning a vector of length 0. */
/* if stretch is zero on entry then the vector x cannot be
   "stretched",
   otherwise, stretch returns the new required length for x
*/

SEXP attribute_hidden makeSubscript(SEXP x, SEXP s, int *stretch, SEXP call)
{
    int nx;
    SEXP ans;

    ans = R_NilValue;
    if (isVector(x) || isList(x) || isLanguage(x)) {
	nx = length(x);

	ans = vectorSubscript(nx, s, stretch, getAttrib, (STRING_ELT), 
			      x, call);
    }
    else {
	ECALL(call, _("subscripting on non-vector"));
    }
    return ans;

}

/* nx is the length of the object being subscripted,
   s is the R subscript value,
   dng gets a given attrib for x, which is the object we are
   subsetting,
*/

static SEXP 
int_vectorSubscript(int nx, SEXP s, int *stretch, AttrGetter dng,
		    StringEltGetter strg, SEXP x, Rboolean in, SEXP call)
{
    int ns;
    SEXP ans = R_NilValue, tmp;

    ns = length(s);
    /* special case for simple indices -- does not duplicate */
    if (ns == 1 && TYPEOF(s) == INTSXP && ATTRIB(s) == R_NilValue) {
	int i = INTEGER(s)[0];
	if (0 < i && i <= nx) {
	    *stretch = 0;
	    return s;
	}
    }
    PROTECT(s = duplicate(s));
    SET_ATTRIB(s, R_NilValue);
    SET_OBJECT(s, 0);
    switch (TYPEOF(s)) {
    case NILSXP:
	*stretch = 0;
	ans = allocVector(INTSXP, 0);
	break;
    case LGLSXP:
	/* *stretch = 0; */
	ans = logicalSubscript(s, ns, nx, stretch, call);
	break;
    case INTSXP:
	    ans = integerSubscript(s, ns, nx, stretch, call);
	    break;
    case REALSXP:
	PROTECT(tmp = coerceVector(s, INTSXP));
	ans = integerSubscript(tmp, ns, nx, stretch, call);
	UNPROTECT(1);
	break;
    case STRSXP:
    {
	SEXP names = dng(x, R_NamesSymbol);
	/* *stretch = 0; */
	ans = stringSubscript(s, ns, nx, names, strg, stretch, in, call);
    }
    break;
    case SYMSXP:
	*stretch = 0;
	if (s == R_MissingArg) {
	    ans = nullSubscript(nx);
	    break;
	}
    default:
	if (call == R_NilValue)
	    error(_("invalid subscript type '%s'"), type2char(TYPEOF(s)));
	else
	    errorcall(call, _("invalid subscript type '%s'"), 
		      type2char(TYPEOF(s)));
    }
    UNPROTECT(1);
    return ans;
}


SEXP attribute_hidden
vectorSubscript(int nx, SEXP s, int *stretch, AttrGetter dng,
		StringEltGetter strg, SEXP x, SEXP call)
{
    return int_vectorSubscript(nx, s, stretch, dng, strg, x, TRUE, call);
}

