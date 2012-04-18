/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2012  The R Core Team
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
 * EXPORTS:
 *
 *  OneIndex()        -- used for "[[<-" in ./subassign.c
 *  get1index()       -- used for "[["   in ./subassign.c & subset.c
 *  vectorIndex()     -- used for "[[" with a vector arg

 *  mat2indsub()      -- for "mat[i]"     "    "            "

 *  makeSubscript()   -- for "[" and "[<-" in ./subset.c and ./subassign.c,
 *			 and "[[<-" with a scalar in ./subassign.c
 *  vectorSubscript() -- for makeSubscript()   {currently unused externally}
 *  arraySubscript()  -- for "[i,j,..." and "[<-..." in ./subset.c, ./subassign.c
 */

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

/* Utility used (only in) do_subassign2_dflt(), i.e. "[[<-" in ./subassign.c : */
R_xlen_t attribute_hidden
OneIndex(SEXP x, SEXP s, int len, int partial, SEXP *newname, int pos, SEXP call)
{
    SEXP names;
    R_xlen_t i, indx, nx;

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
	nx = xlength(x);
	names = getAttrib(x, R_NamesSymbol);
	if (names != R_NilValue) {
	    /* Try for exact match */
	    for (i = 0; i < nx; i++) {
		const char *tmp = translateChar(STRING_ELT(names, i));
		if (!tmp[0]) continue;
		if (streql(tmp, translateChar(STRING_ELT(s, pos)))) {
		    indx = i;
		    break;
		}
	    }
	    /* Try for partial match */
	    if (partial && indx < 0) {
		len = strlen(translateChar(STRING_ELT(s, pos)));
		for(i = 0; i < nx; i++) {
		    const char *tmp = translateChar(STRING_ELT(names, i));
		    if (!tmp[0]) continue;
		    if(!strncmp(tmp, translateChar(STRING_ELT(s, pos)), len)) {
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
	nx = xlength(x);
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

R_xlen_t attribute_hidden
get1index(SEXP s, SEXP names, int len, int pok, int pos, SEXP call)
{
/* Get a single index for the [[ operator.
   Check that only one index is being selected.
   pok : is "partial ok" ?
	 if pok is -1, warn if partial matching occurs
*/
    int  warn_pok = 0;
    const char *ss, *cur_name;
    R_xlen_t indx;

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
    {
	int i = INTEGER(s)[pos];
	if(i != NA_INTEGER)
	    indx = integerOneIndex(i, len, call);
	break;
    }
    case REALSXP:
    {
	double dblind = REAL(s)[pos];
	if(!ISNAN(dblind)) {
	    if (dblind > 0) indx = dblind - 1;
	    else if (dblind == 0 || len < 2) {
		ECALL(call, _("attempt to select less than one element"));
	    } else if (len == 2 && dblind > -3)
		indx = 2 + dblind;
	    else {
		ECALL(call, _("attempt to select more than one element"));
	    }
	}
	break;
    }
    case STRSXP:
	/* NA matches nothing */
	if(STRING_ELT(s, pos) == NA_STRING) break;
	/* "" matches nothing: see names.Rd */
	if(!CHAR(STRING_ELT(s, pos))[0]) break;

	/* Try for exact match */
	ss = translateChar(STRING_ELT(s, pos));
	for (R_xlen_t i = 0; i < xlength(names); i++)
	    if (STRING_ELT(names, i) != NA_STRING) {
		if (streql(translateChar(STRING_ELT(names, i)), ss)) {
		    indx = i;
		    break;
		}
	    }
	/* Try for partial match */
	if (pok && indx < 0) {
	    len = strlen(ss);
	    for(R_xlen_t i = 0; i < xlength(names); i++) {
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
	for (R_xlen_t i = 0; i < xlength(names); i++)
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

SEXP attribute_hidden
vectorIndex(SEXP x, SEXP thesub, int start, int stop, int pok, SEXP call) 
{
    int i, offset;

    for(i = start; i < stop; i++) {
	if(!isVectorList(x) && !isPairList(x)) {
	    if (i)
		errorcall(call, _("recursive indexing failed at level %d\n"), i+1);
	    else
		errorcall(call, _("attempt to select more than one element"));
	}
	offset = get1index(thesub, getAttrib(x, R_NamesSymbol),
		           length(x), pok, i, call);  // must be less than len
	if(offset < 0 || offset >= length(x))
	    errorcall(call, _("no such index at level %d\n"), i+1);
	if(isPairList(x)) {
	    x = CAR(nthcdr(x, offset));
	} else {
	    x = VECTOR_ELT(x, offset);
    	}
    }
    return x;
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
    R_xlen_t NR = nrs;
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
	    k = INTEGER(s)[i + j * NR];
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
	    /* FIXME: this can overflow */
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

/*
Special Matrix Subscripting: For the case x[i] where x is an n-way
array and i is a character matrix with n columns, this code converts i
to an integer matrix by matching against the dimnames of x. NA values
in any row of i propagate to the result.  Unmatched entries result in
a subscript out of bounds error.  */

SEXP attribute_hidden strmat2intmat(SEXP s, SEXP dnamelist, SEXP call)
{
    /* XXX: assumes all args are protected */
    int nr = nrows(s), i, j, v;
    R_xlen_t idx, NR = nr;
    SEXP dnames, snames, si, sicol, s_elt;
    PROTECT(snames = allocVector(STRSXP, nr));
    PROTECT(si = allocVector(INTSXP, xlength(s)));
    dimgets(si, getAttrib(s, R_DimSymbol));
    for (i = 0; i < length(dnamelist); i++) {
        dnames = VECTOR_ELT(dnamelist, i);
        for (j = 0; j < nr; j++) {
            SET_STRING_ELT(snames, j, STRING_ELT(s, j + (i * NR)));
        }
        PROTECT(sicol = match(dnames, snames, 0));
        for (j = 0; j < nr; j++) {
            v = INTEGER(sicol)[j];
            idx = j + (i * NR);
            s_elt = STRING_ELT(s, idx);
            if (s_elt == NA_STRING) v = NA_INTEGER;
            if (!CHAR(s_elt)[0]) v = 0; /* disallow "" match */
            if (v == 0) errorcall(call, _("subscript out of bounds"));
            INTEGER(si)[idx] = v;
        }
        UNPROTECT(1);
    }
    UNPROTECT(2);
    return si;
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


// FIXME stretch needs to be R_xlen_t
static SEXP 
logicalSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, int *stretch, SEXP call)
{
    R_xlen_t count, i, nmax;
    int canstretch;
    SEXP indx;
    canstretch = *stretch;
    if (!canstretch && ns > nx) {
	ECALL(call, _("(subscript) logical subscript too long"));
    }
    nmax = (ns > nx) ? ns : nx;
    *stretch = (ns > nx) ? ns : 0;
    if (ns == 0) return(allocVector(INTSXP, 0));
#ifdef LONG_VECTOR_SUPPORT
    if (nmax > INT_MAX) {
	count = 0;
	for (i = 0; i < nmax; i++)
	    if (LOGICAL(s)[i%ns]) count++;
	indx = allocVector(INTSXP, count);
	count = 0;
	for (i = 0; i < nmax; i++)
	    if (LOGICAL(s)[i%ns]) {
		if (LOGICAL(s)[i%ns] == NA_LOGICAL)
		    REAL(indx)[count++] = NA_REAL;
		else
		    REAL(indx)[count++] = i + 1;
	    }
	return indx;
    }
#endif
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns]) count++;
    indx = allocVector(INTSXP, count);
    count = 0;
    for (i = 0; i < nmax; i++)
	if (LOGICAL(s)[i%ns]) {
	    if (LOGICAL(s)[i%ns] == NA_LOGICAL)
		INTEGER(indx)[count++] = NA_INTEGER;
	    else if (i >= INT_MAX)
		error("logical subscript selected >= INT_MAX");
	    else
		INTEGER(indx)[count++] = i + 1;
	}
    return indx;
}

/* FIXME: this is rather inefficient */
static SEXP negativeSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, SEXP call)
{
    SEXP indx;
    int ix, stretch = 0;
    R_xlen_t i;
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

static SEXP positiveSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx)
{
    SEXP indx;
    R_xlen_t i, zct = 0;
    for (i = 0; i < ns; i++) if (INTEGER(s)[i] == 0) zct++;
    if (zct) {
	indx = allocVector(INTSXP, (ns - zct));
	for (i = 0, zct = 0; i < ns; i++)
	    if (INTEGER(s)[i] != 0)
		INTEGER(indx)[zct++] = INTEGER(s)[i];
	return indx;

    } else return s;
}

static SEXP 
integerSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, int *stretch, SEXP call)
{
    R_xlen_t i;
    int ii, min, max, canstretch;
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

static SEXP 
realSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, int *stretch, SEXP call)
{
    R_xlen_t i;
    int canstretch;
    double ii, min, max;
    Rboolean isna = FALSE;
    canstretch = *stretch;
    *stretch = 0;
    min = 0;
    max = 0;
    for (i = 0; i < ns; i++) {
	ii = REAL(s)[i];
	if (R_FINITE(ii)) {
	    if (ii < min) min = ii;
	    if (ii > max) max = ii;
	} else isna = TRUE;
    }
    if (max > nx) {
#ifndef LONG_VECTOR_SUPPORT
	if (max > INT_MAX) {
	    ECALL(call, _("subscript too large for 32-bit R"));
	}
#endif
	if(canstretch) *stretch = max;
	else {
	    ECALL(call, _("subscript out of bounds"));
	}
    }
    if (min < 0) {
	if (max == 0 && !isna) {
	    SEXP indx;
	    int stretch = 0;
	    double dx;
	    R_xlen_t i, ix;
	    PROTECT(indx = allocVector(LGLSXP, nx));
	    for (i = 0; i < nx; i++) LOGICAL(indx)[i] = 1;
	    for (i = 0; i < ns; i++) {
		dx = REAL(s)[i];
		if (R_FINITE(dx) && dx != 0  && -dx <= nx) {
		    ix = -dx - 1;
		    LOGICAL(indx)[ix] = 0;
		}
	    }
	    s = logicalSubscript(indx, nx, nx, &stretch, call);
	    UNPROTECT(1);
	    return s;
	} else {
	    ECALL(call, _("only 0's may be mixed with negative subscripts"));
	}
    } else {
	/* Only return a REALSXP index if we need to */
	SEXP indx;
	R_xlen_t i, cnt = 0;
	Rboolean int_ok = TRUE;
	/* NB, indices will be truncated eventually,
	   so need to do that to take '0' into account */
	for (i = 0; i < ns; i++) {
	    double ds = REAL(s)[i];
	    if (!R_FINITE(ds)) {
		if (ds > INT_MAX) int_ok = FALSE;
		cnt++;
	    } else if ((R_xlen_t) ds != 0) cnt++;
	}
	if (int_ok) {
	    indx = allocVector(INTSXP, cnt);
	    for (i = 0, cnt = 0; i < ns; i++) {
		double ds = REAL(s)[i];
		int ia;
		if (!R_FINITE(ds)) ia = NA_INTEGER;
		else ia = ds;
		if (ia != 0) INTEGER(indx)[cnt++] = ia;
	    }
	} else {
	    indx = allocVector(REALSXP, cnt);
	    for (i = 0, cnt = 0; i < ns; i++) {
		R_xlen_t ia = REAL(s)[i];
		if (ia != 0) REAL(indx)[cnt++] = REAL(s)[i];
	    }
	}
	return indx;
    }
    return R_NilValue;
}

/* This uses a couple of horrible hacks in conjunction with
 * VectorAssign (in subassign.c).  If subscripting is used for
 * assignment, it is possible to extend a vector by supplying new
 * names, and we want to give the extended vector those names, so they
 * are returned as the use.names attribute. Also, unset elements of the vector
 * of new names (places where a match was found) are indicated by
 * setting the element of the newnames vector to NULL.
*/

/* The original code (pre 2.0.0) used a ns x nx loop that was too
 * slow.  So now we hash.  Hashing is expensive on memory (up to 32nx
 * bytes) so it is only worth doing if ns * nx is large.  If nx is
 * large, then it will be too slow unless ns is very small.
 */

static SEXP
stringSubscript(SEXP s, R_xlen_t ns, R_xlen_t nx, SEXP names,
		int *stretch, SEXP call)
{
    SEXP indx, indexnames;
    R_xlen_t i, j, nnames, extra, sub;
    int canstretch = *stretch;
    /* product may overflow, so check factors as well. */
    Rboolean usehashing = ( ((ns > 1000 && nx) || (nx > 1000 && ns)) || (ns * nx > 15*nx + ns) );

    PROTECT(s);
    PROTECT(names);
    PROTECT(indexnames = allocVector(VECSXP, ns));
    nnames = nx;
    extra = nnames;

    /* Process each of the subscripts. First we compare with the names
     * on the vector and then (if there is no match) with each of the
     * previous subscripts, since (if assigning) we may have already
     * added an element of that name. (If we are not assigning, any
     * nonmatch will have given an error.)
     */

    if(usehashing) {
	/* must be internal, so names contains a character vector */
	/* NB: this does not behave in the same way with respect to ""
	   and NA names: they will match */
	PROTECT(indx = match(names, s, 0));
	/* second pass to correct this */
	for (i = 0; i < ns; i++)
	    if(STRING_ELT(s, i) == NA_STRING || !CHAR(STRING_ELT(s, i))[0])
		INTEGER(indx)[i] = 0;
	for (i = 0; i < ns; i++) SET_VECTOR_ELT(indexnames, i, R_NilValue);
    } else {
	PROTECT(indx = allocVector(INTSXP, ns));
	for (i = 0; i < ns; i++) {
	    sub = 0;
	    if (names != R_NilValue) {
		for (j = 0; j < nnames; j++) {
		    SEXP names_j = STRING_ELT(names, j);
		    if (NonNullStringMatch(STRING_ELT(s, i), names_j)) {
			sub = j + 1;
			SET_VECTOR_ELT(indexnames, i, R_NilValue);
			break;
		    }
		}
	    }
	    INTEGER(indx)[i] = sub;
	}
    }


    for (i = 0; i < ns; i++) {
	sub = INTEGER(indx)[i];
	if (sub == 0) {
	    for (j = 0 ; j < i ; j++)
		if (NonNullStringMatch(STRING_ELT(s, i), STRING_ELT(s, j))) {
		    sub = INTEGER(indx)[j];
		    SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, j));
		    break;
		}
	}
	if (sub == 0) {
	    if (!canstretch) {
		ECALL(call, _("subscript out of bounds"));
	    }
	    extra += 1;
	    sub = extra;
	    SET_VECTOR_ELT(indexnames, i, STRING_ELT(s, i));
	}
	INTEGER(indx)[i] = sub;
    }
    /* We return the new names as the names attribute of the returned
       subscript vector. */
    if (extra != nnames)
	setAttrib(indx, R_UseNamesSymbol, indexnames);
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

attribute_hidden SEXP
int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call)
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
	/* We don't yet allow subscripts > INT_MAX */
	PROTECT(tmp = coerceVector(s, INTSXP));
	tmp = integerSubscript(tmp, ns, nd, &stretch, call);
	UNPROTECT(1);
	return tmp;
    case STRSXP:
	dnames = getAttrib(x, R_DimNamesSymbol);
	if (dnames == R_NilValue) {
	    ECALL(call, _("no 'dimnames' attribute for array"));
	}
	dnames = VECTOR_ELT(dnames, dim);
	return stringSubscript(s, ns, nd, dnames, &stretch, call);
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

/* This is used by packages arules, cba, proxy and seriation. */
typedef SEXP AttrGetter(SEXP x, SEXP data);
typedef SEXP (*StringEltGetter)(SEXP x, int i);

SEXP
arraySubscript(int dim, SEXP s, SEXP dims, AttrGetter dng,
	       StringEltGetter strg, SEXP x)
{
    return int_arraySubscript(dim, s, dims, x, R_NilValue);
}

/* Subscript creation.  The first thing we do is check to see */
/* if there are any user supplied NULL's, these result in */
/* returning a vector of length 0. */
/* if stretch is zero on entry then the vector x cannot be
   "stretched",
   otherwise, stretch returns the new required length for x
*/

static SEXP 
vectorSubscript(R_xlen_t nx, SEXP s, int *stretch, SEXP x, SEXP call);

SEXP attribute_hidden makeSubscript(SEXP x, SEXP s, int *stretch, SEXP call)
{
    SEXP ans;

    ans = R_NilValue;
    if (isVector(x) || isList(x) || isLanguage(x)) {
	ans = vectorSubscript(xlength(x), s, stretch, x, call);
    } else {
	ECALL(call, _("subscripting on non-vector"));
    }
    return ans;

}

/* nx is the length of the object being subscripted,
   s is the R subscript value.
*/

static SEXP
vectorSubscript(R_xlen_t nx, SEXP s, int *stretch, SEXP x, SEXP call)
{
    SEXP ans = R_NilValue;

    R_xlen_t ns = xlength(s);
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
	ans = realSubscript(s, ns, nx, stretch, call);
	break;
    case STRSXP:
    {
	SEXP names = getAttrib(x, R_NamesSymbol);
	/* *stretch = 0; */
	ans = stringSubscript(s, ns, nx, names, stretch, call);
    }
    break;
    case SYMSXP:
	*stretch = 0;
	if (s == R_MissingArg) {
	    /* FIXME: check that this is not a long vector
	       or use doubles */
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
