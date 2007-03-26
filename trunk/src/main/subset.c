/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2007   The R Development Core Team
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
 *
 *
 *  Vector and List Subsetting
 *
 *  There are three kinds of subscripting [, [[, and $.
 *  We have three different functions to compute these.
 *
 *
 *  Note on Matrix Subscripts
 *
 *  The special [ subscripting where dim(x) == ncol(subscript matrix)
 *  is handled inside VectorSubset. The subscript matrix is turned
 *  into a subscript vector of the appropriate size and then
 *  VectorSubset continues.  This provides coherence especially
 *  regarding attributes etc. (it would be quicker to handle this case
 *  separately, but then we would have more to keep in step.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

/* ExtractSubset does the transfer of elements from "x" to "result" */
/* according to the integer subscripts given in "indx". */

static SEXP ExtractSubset(SEXP x, SEXP result, SEXP indx, SEXP call)
{
    int i, ii, n, nx, mode;
    SEXP tmp, tmp2;
    mode = TYPEOF(x);
    n = LENGTH(indx);
    nx = length(x);
    tmp = result;

    if (x == R_NilValue)
	return x;

    for (i = 0; i < n; i++) {
	ii = INTEGER(indx)[i];
	if (ii != NA_INTEGER)
	    ii--;
	switch (mode) {
	case LGLSXP:
            if (0 <= ii && ii < nx && ii != NA_LOGICAL)
                LOGICAL(result)[i] = LOGICAL(x)[ii];
            else
                LOGICAL(result)[i] = NA_INTEGER;
            break;
	case INTSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		INTEGER(result)[i] = INTEGER(x)[ii];
	    else
		INTEGER(result)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		REAL(result)[i] = REAL(x)[ii];
	    else
		REAL(result)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER) {
		COMPLEX(result)[i] = COMPLEX(x)[ii];
	    }
	    else {
		COMPLEX(result)[i].r = NA_REAL;
		COMPLEX(result)[i].i = NA_REAL;
	    }
	    break;
	case STRSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		SET_STRING_ELT(result, i, STRING_ELT(x, ii));
	    else
		SET_STRING_ELT(result, i, NA_STRING);
	    break;
	case VECSXP:
	case EXPRSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		SET_VECTOR_ELT(result, i, VECTOR_ELT(x, ii));
	    else
		SET_VECTOR_ELT(result, i, R_NilValue);
	    break;
	case LISTSXP:
	    /* cannot happen: pairlists are coerced to lists */
	case LANGSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER) {
		tmp2 = nthcdr(x, ii);
		SETCAR(tmp, CAR(tmp2));
		SET_TAG(tmp, TAG(tmp2));
	    }
	    else
		SETCAR(tmp, R_NilValue);
	    tmp = CDR(tmp);
	    break;
	case RAWSXP:
	    if (0 <= ii && ii < nx && ii != NA_INTEGER)
		RAW(result)[i] = RAW(x)[ii];
	    else
		RAW(result)[i] = (Rbyte) 0;
	    break;
	default:
	    errorcall(call, R_MSG_ob_nonsub);
	}
    }
    return result;
}


/* This is for all cases with a single index, including 1D arrays and
   matrix indexing of arrays */
static SEXP VectorSubset(SEXP x, SEXP s, SEXP call)
{
    int n, mode, stretch = 1;
    SEXP indx, result, attrib, nattrib;

    if (s == R_MissingArg) return duplicate(x);

    PROTECT(s);
    attrib = getAttrib(x, R_DimSymbol);

    /* Check to see if we have special matrix subscripting. */
    /* If we do, make a real subscript vector and protect it. */

    if (isMatrix(s) && isArray(x) && (isInteger(s) || isReal(s)) &&
	    ncols(s) == length(attrib)) {
	s = mat2indsub(attrib, s);
	UNPROTECT(1);
	PROTECT(s);
    }

    /* Convert to a vector of integer subscripts */
    /* in the range 1:length(x). */

    PROTECT(indx = makeSubscript(x, s, &stretch));
    n = LENGTH(indx);

    /* Allocate the result. */

    mode = TYPEOF(x);
    /* No protection needed as ExtractSubset does not allocate */
    result = allocVector(mode, n);
    if (mode == VECSXP || mode == EXPRSXP)
	/* we do not duplicate the values when extracting the subset,
	   so to be conservative mark the result as NAMED = 2 */
	SET_NAMED(result, 2);

    PROTECT(result = ExtractSubset(x, result, indx, call));
    if (result != R_NilValue &&
	(
	    ((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue) ||
	    ( /* here we might have an array.  Use row names if 1D */
		isArray(x) && LENGTH(getAttrib(x, R_DimNamesSymbol)) == 1 &&
		(attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue &&
		(attrib = GetRowNames(attrib)) != R_NilValue
		)
	    )) {
	nattrib = allocVector(TYPEOF(attrib), n);
	PROTECT(nattrib); /* seems unneeded */
	nattrib = ExtractSubset(attrib, nattrib, indx, call);
	setAttrib(result, R_NamesSymbol, nattrib);
	UNPROTECT(1);
    }
    if (result != R_NilValue && (attrib = getAttrib(x, R_SrcrefSymbol)) != R_NilValue) {
	nattrib = allocVector(VECSXP, n);
	PROTECT(nattrib); /* seems unneeded */
	nattrib = ExtractSubset(attrib, nattrib, indx, call);
	setAttrib(result, R_SrcrefSymbol, nattrib);
	UNPROTECT(1);
    }    
    UNPROTECT(3);
    return result;
}


static SEXP MatrixSubset(SEXP x, SEXP s, SEXP call, int drop)
{
    SEXP attr, result, sr, sc, dim;
    int nr, nc, nrs, ncs;
    int i, j, ii, jj, ij, iijj;

    nr = nrows(x);
    nc = ncols(x);

    /* Note that "s" is protected on entry. */
    /* The following ensures that pointers remain protected. */
    dim = getAttrib(x, R_DimSymbol);

    sr = SETCAR(s, arraySubscript(0, CAR(s), dim, getAttrib,
				  (STRING_ELT), x));
    sc = SETCADR(s, arraySubscript(1, CADR(s), dim, getAttrib,
				   (STRING_ELT), x));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    PROTECT(sr);
    PROTECT(sc);
    result = allocVector(TYPEOF(x), nrs*ncs);
    PROTECT(result);
    for (i = 0; i < nrs; i++) {
	ii = INTEGER(sr)[i];
	if (ii != NA_INTEGER) {
	    if (ii < 1 || ii > nr)
		errorcall(call, R_MSG_subs_o_b);
	    ii--;
	}
	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj != NA_INTEGER) {
		if (jj < 1 || jj > nc)
		    errorcall(call, R_MSG_subs_o_b);
		jj--;
	    }
	    ij = i + j * nrs;
	    if (ii == NA_INTEGER || jj == NA_INTEGER) {
		switch (TYPEOF(x)) {
		case LGLSXP:
		case INTSXP:
		    INTEGER(result)[ij] = NA_INTEGER;
		    break;
		case REALSXP:
		    REAL(result)[ij] = NA_REAL;
		    break;
		case CPLXSXP:
		    COMPLEX(result)[ij].r = NA_REAL;
		    COMPLEX(result)[ij].i = NA_REAL;
		    break;
		case STRSXP:
		    SET_STRING_ELT(result, ij, NA_STRING);
		    break;
		case VECSXP:
		    SET_VECTOR_ELT(result, ij, R_NilValue);
		    break;
		case RAWSXP:
		    RAW(result)[ij] = (Rbyte) 0;
		    break;
		default:
		    error(_("matrix subscripting not handled for this type"));
		    break;
		}
	    }
	    else {
		iijj = ii + jj * nr;
		switch (TYPEOF(x)) {
		case LGLSXP:
                    LOGICAL(result)[ij] = LOGICAL(x)[iijj];
                    break;
		case INTSXP:
		    INTEGER(result)[ij] = INTEGER(x)[iijj];
		    break;
		case REALSXP:
		    REAL(result)[ij] = REAL(x)[iijj];
		    break;
		case CPLXSXP:
		    COMPLEX(result)[ij] = COMPLEX(x)[iijj];
		    break;
		case STRSXP:
		    SET_STRING_ELT(result, ij, STRING_ELT(x, iijj));
		    break;
		case VECSXP:
		    SET_VECTOR_ELT(result, ij, VECTOR_ELT(x, iijj));
		    break;
		case RAWSXP:
		    RAW(result)[ij] = RAW(x)[iijj];
		    break;
		default:
		    error(_("matrix subscripting not handled for this type"));
		    break;
		}
	    }
	}
    }
    if(nrs >= 0 && ncs >= 0) {
	PROTECT(attr = allocVector(INTSXP, 2));
	INTEGER(attr)[0] = nrs;
	INTEGER(attr)[1] = ncs;
	setAttrib(result, R_DimSymbol, attr);
	UNPROTECT(1);
    }

    /* The matrix elements have been transferred.  Now we need to */
    /* transfer the attributes.	 Most importantly, we need to subset */
    /* the dimnames of the returned value. */

    if (nrs >= 0 && ncs >= 0) {
	SEXP dimnames, dimnamesnames, newdimnames;
	dimnames = getAttrib(x, R_DimNamesSymbol);
	dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
	if (!isNull(dimnames)) {
	    PROTECT(newdimnames = allocVector(VECSXP, 2));
	    if (TYPEOF(dimnames) == VECSXP) {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(VECTOR_ELT(dimnames, 0),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(VECTOR_ELT(dimnames, 1),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    else {
	      SET_VECTOR_ELT(newdimnames, 0,
		    ExtractSubset(CAR(dimnames),
				  allocVector(STRSXP, nrs), sr, call));
	      SET_VECTOR_ELT(newdimnames, 1,
		    ExtractSubset(CADR(dimnames),
				  allocVector(STRSXP, ncs), sc, call));
	    }
	    setAttrib(newdimnames, R_NamesSymbol, dimnamesnames);
	    setAttrib(result, R_DimNamesSymbol, newdimnames);
	    UNPROTECT(1);
	}
    }
    /*  Probably should not do this:
    copyMostAttrib(x, result); */
    if (drop)
	DropDims(result);
    UNPROTECT(3);
    return result;
}


static SEXP ArraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
    int i, j, k, ii, jj, mode, n;
    int **subs, *indx, *offset, *bound;
    SEXP dimnames, dimnamesnames, p, q, r, result, xdims;
    char *vmaxsave;

    mode = TYPEOF(x);
    xdims = getAttrib(x, R_DimSymbol);
    k = length(xdims);

    vmaxsave = vmaxget();
    subs = (int**)R_alloc(k, sizeof(int*));
    indx = (int*)R_alloc(k, sizeof(int));
    offset = (int*)R_alloc(k, sizeof(int));
    bound = (int*)R_alloc(k, sizeof(int));

    /* Construct a vector to contain the returned values. */
    /* Store its extents. */

    n = 1;
    r = s;
    for (i = 0; i < k; i++) {
	SETCAR(r, arraySubscript(i, CAR(r), xdims, getAttrib,
				 (STRING_ELT), x));
	bound[i] = LENGTH(CAR(r));
	n *= bound[i];
	r = CDR(r);
    }
    PROTECT(result = allocVector(mode, n));
    r = s;
    for (i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER(CAR(r));
	r = CDR(r);
    }
    offset[0] = 1;
    for (i = 1; i < k; i++)
	offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];

    /* Transfer the subset elements from "x" to "a". */

    for (i = 0; i < n; i++) {
	ii = 0;
	for (j = 0; j < k; j++) {
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) {
		ii = NA_INTEGER;
		goto assignLoop;
	    }
	    if (jj < 1 || jj > INTEGER(xdims)[j])
		errorcall(call, R_MSG_subs_o_b);
	    ii += (jj - 1) * offset[j];
	}

      assignLoop:
	switch (mode) {
	case LGLSXP:
	    if (ii != NA_INTEGER)
		LOGICAL(result)[i] = LOGICAL(x)[ii];
	    else
		LOGICAL(result)[i] = NA_LOGICAL;
	    break;
	case INTSXP:
	    if (ii != NA_INTEGER)
		INTEGER(result)[i] = INTEGER(x)[ii];
	    else
		INTEGER(result)[i] = NA_INTEGER;
	    break;
	case REALSXP:
	    if (ii != NA_INTEGER)
		REAL(result)[i] = REAL(x)[ii];
	    else
		REAL(result)[i] = NA_REAL;
	    break;
	case CPLXSXP:
	    if (ii != NA_INTEGER) {
		COMPLEX(result)[i] = COMPLEX(x)[ii];
	    }
	    else {
		COMPLEX(result)[i].r = NA_REAL;
		COMPLEX(result)[i].i = NA_REAL;
	    }
	    break;
	case STRSXP:
	    if (ii != NA_INTEGER)
		SET_STRING_ELT(result, i, STRING_ELT(x, ii));
	    else
		SET_STRING_ELT(result, i, NA_STRING);
	    break;
	case VECSXP:
	    if (ii != NA_INTEGER)
		SET_VECTOR_ELT(result, i, VECTOR_ELT(x, ii));
	    else
		SET_VECTOR_ELT(result, i, R_NilValue);
	    break;
	case RAWSXP:
	    if (ii != NA_INTEGER)
		RAW(result)[i] = RAW(x)[ii];
	    else
		RAW(result)[i] = (Rbyte) 0;
	    break;
	default:
	    error(_("array subscripting not handled for this type"));
	    break;
	}
	if (n > 1) {
	    j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % k;
	    }
	}
    }

    PROTECT(xdims = allocVector(INTSXP, k));
    for(i = 0 ; i < k ; i++)
	INTEGER(xdims)[i] = bound[i];
    setAttrib(result, R_DimSymbol, xdims);
    UNPROTECT(1);

    /* The array elements have been transferred. */
    /* Now we need to transfer the attributes. */
    /* Most importantly, we need to subset the */
    /* dimnames of the returned value. */

    dimnames = getAttrib(x, R_DimNamesSymbol);
    dimnamesnames = getAttrib(dimnames, R_NamesSymbol);
    if (dimnames != R_NilValue) {
	/*SEXP xdims;
	int */ j = 0;
	PROTECT(xdims = allocVector(VECSXP, k));
	if (TYPEOF(dimnames) == VECSXP) {
	    r = s;
	    for (i = 0; i < k ; i++) {
		if (bound[i] > 0) {
		  SET_VECTOR_ELT(xdims, j++,
			ExtractSubset(VECTOR_ELT(dimnames, i),
				      allocVector(STRSXP, bound[i]),
				      CAR(r), call));
		} else { /* 0-length dims have NULL dimnames */ 
		    SET_VECTOR_ELT(xdims, j++, R_NilValue);
		}
		r = CDR(r);
	    }
	}
	else {
	    p = dimnames;
	    q = xdims;
	    r = s;
	    for(i = 0 ; i < k; i++) {
		SETCAR(q, allocVector(STRSXP, bound[i]));
		SETCAR(q, ExtractSubset(CAR(p), CAR(q), CAR(r), call));
		p = CDR(p);
		q = CDR(q);
		r = CDR(r);
	    }
	}
	setAttrib(xdims, R_NamesSymbol, dimnamesnames);
	setAttrib(result, R_DimNamesSymbol, xdims);
	UNPROTECT(1);
    }
    /* This was removed for matrices in 1998
       copyMostAttrib(x, result); */
    /* Free temporary memory */
    vmaxset(vmaxsave);
    if (drop)
	DropDims(result);
    UNPROTECT(1);
    return result;
}


/* Extracts the drop argument, if present, from the argument list.
   The object being subsetted must be the first argument. */
static void ExtractDropArg(SEXP el, int *drop)
{
    SEXP last = el;
    for (el = CDR(el); el != R_NilValue; el = CDR(el)) {
	if(TAG(el) == R_DropSymbol) {
	    *drop = asLogical(CAR(el));
	    if (*drop == NA_LOGICAL) *drop = 1;
	    SETCDR(last, CDR(el));
	}
	else last = el;
    }
}



/* The "[" subset operator.
 * This provides the most general form of subsetting. */

SEXP attribute_hidden do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is an */
    /* approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall through */
    /* to the generic code below.  Note that evaluation */
    /* retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "[", args, rho, &ans, 0, 0))
	return(ans);

    /* Method dispatch has failed, we now */
    /* run the generic internal code. */
    return do_subset_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subset_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, ax, px, x, subs;
    int drop, i, nsubs, type;

    /* By default we drop extents of length 1 */

    PROTECT(args);

    /* Handle case of extracting a single element from a simple vector
       directly to improve speed for this simple case. */
    if (CDDR(args) == R_NilValue) {
	int i;
	SEXP x = CAR(args);
	SEXP s = CADR(args);
	if (ATTRIB(x) == R_NilValue && ATTRIB(s) == R_NilValue) {
	    switch (TYPEOF(x)) {
	    case REALSXP:
		switch (TYPEOF(s)) {
		case REALSXP: i = (LENGTH(s) == 1) ? REAL(s)[0] : -1; break;
		case INTSXP: i = (LENGTH(s) == 1) ? INTEGER(s)[0] : -1; break;
		default:  i = -1;
		}
		if (i >= 1 && i <= LENGTH(x)) {
		    ans = allocVector(REALSXP, 1);
		    REAL(ans)[0] = REAL(x)[i-1];
		    UNPROTECT(1);
		    return ans;
		}
		break;
	    case INTSXP:
		switch (TYPEOF(s)) {
		case REALSXP: i = (LENGTH(s) == 1) ? REAL(s)[0] : -1; break;
		case INTSXP: i = (LENGTH(s) == 1) ? INTEGER(s)[0] : -1; break;
		default:  i = -1;
		}
		if (i >= 1 && i <= LENGTH(x)) {
		    ans = allocVector(INTSXP, 1);
		    INTEGER(ans)[0] = INTEGER(x)[i-1];
		    UNPROTECT(1);
		    return ans;
		}
		break;
	    default: break;
	    }
	}
    }

    drop = 1;
    ExtractDropArg(args, &drop);
    x = CAR(args);

    /* This was intended for compatibility with S, */
    /* but in fact S does not do this. */
    /* FIXME: replace the test by isNull ... ? */

    if (x == R_NilValue) {
	UNPROTECT(1);
	return x;
    }
    subs = CDR(args);
    nsubs = length(subs);
    type = TYPEOF(x);

    /* Here coerce pair-based objects into generic vectors. */
    /* All subsetting takes place on the generic vector form. */

    ax = x;
    if (isVector(x))
	PROTECT(ax);
    else if (isPairList(x)) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	if (ndim > 1) {
	    PROTECT(ax = allocArray(VECSXP, dim));
	    setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_DimNamesSymbol));
	}
	else {
	    PROTECT(ax = allocVector(VECSXP, length(x)));
	    setAttrib(ax, R_NamesSymbol, getAttrib(x, R_NamesSymbol));
	}
	for(px = x, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SET_VECTOR_ELT(ax, i++, CAR(px));
    }
    else errorcall(call, R_MSG_ob_nonsub);

    /* This is the actual subsetting code. */
    /* The separation of arrays and matrices is purely an optimization. */

    if(nsubs < 2) {
	SEXP dim = getAttrib(x, R_DimSymbol);
	int ndim = length(dim);
	ans = VectorSubset(ax, (nsubs == 1 ? CAR(subs) : R_MissingArg), call);
	/* one-dimensional arrays went through here, and they should
	   have their dimensions dropped only if the result has 
	   length one and drop == TRUE
	*/
	if(ndim == 1) {
	    SEXP attr, attrib, nattrib;
	    int len = length(ans);

	    if(!drop || len > 1) {
		PROTECT(ans);
		PROTECT(attr = allocVector(INTSXP, 1));
		INTEGER(attr)[0] = length(ans);
		setAttrib(ans, R_DimSymbol, attr);
		UNPROTECT(1);
		if((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue) {
		    /* reinstate dimnames, include names of dimnames */
		    PROTECT(nattrib = duplicate(attrib));
		    SET_VECTOR_ELT(nattrib, 0, 
				   getAttrib(ans, R_NamesSymbol));
		    setAttrib(ans, R_DimNamesSymbol, nattrib);
		    setAttrib(ans, R_NamesSymbol, R_NilValue);
		    UNPROTECT(1);
		}
		UNPROTECT(1);
	    }
	}
    } else {
	if (nsubs != length(getAttrib(x, R_DimSymbol)))
	    errorcall(call, _("incorrect number of dimensions"));
	if (nsubs == 2)
	    ans = MatrixSubset(ax, subs, call, drop);
	else
	    ans = ArraySubset(ax, subs, call, drop);
    }
    PROTECT(ans);

    /* Note: we do not coerce back to pair-based lists. */
    /* They are "defunct" in this version of R. */

    if (type == LANGSXP) {
	ax = ans;
	PROTECT(ans = allocList(LENGTH(ax)));
	if ( LENGTH(ax) > 0 )
	    SET_TYPEOF(ans, LANGSXP);
	for(px = ans, i = 0 ; px != R_NilValue ; px = CDR(px))
	    SETCAR(px, VECTOR_ELT(ax, i++));
	setAttrib(ans, R_DimSymbol, getAttrib(ax, R_DimSymbol));
	setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
	setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
	SET_NAMED(ans, NAMED(ax)); /* PR#7924 */
    }
    else {
	PROTECT(ans);
    }
    if (ATTRIB(ans) != R_NilValue) {
	setAttrib(ans, R_TspSymbol, R_NilValue);
	setAttrib(ans, R_ClassSymbol, R_NilValue);
    }
    UNPROTECT(4);
    return ans;
}


/* The [[ subset operator.  It needs to be fast. */
/* The arguments to this call are evaluated on entry. */

SEXP attribute_hidden do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "[[", args, rho, &ans, 0, 0))
	return(ans);

    /* Method dispatch has failed. */
    /* We now run the generic internal code. */

    return do_subset2_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subset2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans, dims, dimnames, indx, subs, x;
    int i, ndims, nsubs, offset = 0;
    int drop = 1;

    PROTECT(args);
    ExtractDropArg(args, &drop);
    x = CAR(args);

    /* This code was intended for compatibility with S, */
    /* but in fact S does not do this.	Will anyone notice? */

    if (x == R_NilValue) {
	UNPROTECT(1);
	return x;
    }

    /* Get the subscripting and dimensioning information */
    /* and check that any array subscripting is compatible. */

    subs = CDR(args);
    if(0 == (nsubs = length(subs)))
	errorcall(call, _("no index specified"));
    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    if(nsubs > 1 && nsubs != ndims)
	errorcall(call, _("incorrect number of subscripts"));

    /* split out ENVSXP for now */
    if( TYPEOF(x) == ENVSXP ) {
      if( nsubs != 1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	error(_("wrong arguments for subsetting an environment"));
      ans = findVarInFrame(x, install(translateChar(STRING_ELT(CAR(subs), 0))));
      if( TYPEOF(ans) == PROMSXP ) {
	    PROTECT(ans);
	    ans = eval(ans, R_GlobalEnv);
	    UNPROTECT(1);
      } else {
	    SET_NAMED(ans, 2);
      }
      
      UNPROTECT(1);
      if(ans == R_UnboundValue )
        return(R_NilValue);
      return(ans);
    }
    
    /* back to the regular program */
    if (!(isVector(x) || isList(x) || isLanguage(x)))
	errorcall(call, R_MSG_ob_nonsub);

    if(nsubs == 1) { /* vector indexing */
	SEXP thesub = CAR(subs);
	int i =-1, len = length(thesub);

	/* new case in 1.7.0, one vector index for a list */
	if(isVectorList(x) && length(CAR(subs)) > 1) {
	    for(i = 0; i < len - 1; i++) {
		if(!isVectorList(x))
		    error(_("recursive indexing failed at level %d\n"), i+1);
		offset = get1index(CAR(subs), getAttrib(x, R_NamesSymbol),
				   length(x), /*partial ok*/TRUE, i);
		if(offset < 0 || offset >= length(x))
		    error(_("no such index at level %d\n"), i+1);
		x = VECTOR_ELT(x, offset);
	    }
	}
	offset = get1index(CAR(subs), getAttrib(x, R_NamesSymbol),
			   length(x), /*partial ok*/TRUE, i);
	if (offset < 0 || offset >= length(x)) {
	    /* a bold attempt to get the same */
	    /* behaviour for $ and [[ */
	    if (offset < 0 && (isNewList(x) ||
			       isExpression(x) ||
			       isList(x) ||
			       isLanguage(x))) {
		UNPROTECT(1);
		return R_NilValue;
	    }
	    else errorcall(call, R_MSG_subs_o_b);
	}
    } else { /* matrix indexing */
	/* Here we use the fact that: */
	/* CAR(R_NilValue) = R_NilValue */
	/* CDR(R_NilValue) = R_NilValue */

	int ndn; /* Number of dimnames. Unlikely to be anything but
		    0 or nsubs, but just in case... */

	PROTECT(indx = allocVector(INTSXP, nsubs));
	dimnames = getAttrib(x, R_DimNamesSymbol);
	ndn = length(dimnames);
	for (i = 0; i < nsubs; i++) {
	    INTEGER(indx)[i] =
		get1index(CAR(subs), (i < ndn) ? VECTOR_ELT(dimnames, i) :
			  R_NilValue,
			  INTEGER(indx)[i], /*partial ok*/TRUE, -1);
	    subs = CDR(subs);
	    if (INTEGER(indx)[i] < 0 ||
		INTEGER(indx)[i] >= INTEGER(dims)[i])
		errorcall(call, R_MSG_subs_o_b);
	}
	offset = 0;
	for (i = (nsubs - 1); i > 0; i--)
	    offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	offset += INTEGER(indx)[0];
	UNPROTECT(1);
    }

    if(isPairList(x)) {
	ans = CAR(nthcdr(x, offset));
	if (NAMED(x) > NAMED(ans))
	    SET_NAMED(ans, NAMED(x));
    } else if(isVectorList(x)) {
	/* did unconditional duplication before 2.4.0 */
	ans = VECTOR_ELT(x, offset);
	if (NAMED(x) > NAMED(ans))
	    SET_NAMED(ans, NAMED(x));
    } else {
	ans = allocVector(TYPEOF(x), 1);
	switch (TYPEOF(x)) {
	case LGLSXP:
	case INTSXP:
	    INTEGER(ans)[0] = INTEGER(x)[offset];
	    break;
	case REALSXP:
	    REAL(ans)[0] = REAL(x)[offset];
	    break;
	case CPLXSXP:
	    COMPLEX(ans)[0] = COMPLEX(x)[offset];
	    break;
	case STRSXP:
	    SET_STRING_ELT(ans, 0, STRING_ELT(x, offset));
	    break;
	case RAWSXP:
	    RAW(ans)[0] = RAW(x)[offset];
	    break;
	default:
	    UNIMPLEMENTED_TYPE("do_subset2", x);
	}
    }
    UNPROTECT(1);
    return ans;
}


enum pmatch {
    NO_MATCH,
    EXACT_MATCH,
    PARTIAL_MATCH
};

/* A helper to partially match tags against a candidate. */
/* Returns: */
static
enum pmatch
pstrmatch(SEXP target, SEXP input, int slen)
{
    char *st="";

    if(target == R_NilValue)
	return NO_MATCH;

    switch (TYPEOF(target)) {
    case SYMSXP:
	st = CHAR(PRINTNAME(target));
	break;
    case CHARSXP:
	st = translateChar(target);
	break;
    }
    if(strncmp(st, translateChar(input), slen) == 0) {
	if (strlen(st) == slen)
	    return EXACT_MATCH;
	else
	    return PARTIAL_MATCH;
    }
    else return NO_MATCH;
}


/* The $ subset operator.
   We need to be sure to only evaluate the first argument.
   The second will be a symbol that needs to be matched, not evaluated.
*/
SEXP attribute_hidden do_subset3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP input, nlist, ans;

    checkArity(op, args);

    /* first translate CADR of args into a string so that we can
       pass it down to DispatchorEval and have it behave correctly */
    input = PROTECT(allocVector(STRSXP, 1));

    nlist = CADR(args);
    if(isSymbol(nlist) )
	SET_STRING_ELT(input, 0, PRINTNAME(nlist));
    else if(isString(nlist) )
	SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
    else {
	errorcall_return(call, _("invalid subscript type"));
    }

    /* replace the second argument with a string */

    /* Previously this was SETCADR(args, input); */
    /* which could cause problems when nlist was */
    /* ..., as in PR#8718 */
    PROTECT(args = CONS(CAR(args), CONS(input, R_NilValue)));

    /* If the first argument is an object and there is */
    /* an approriate method, we dispatch to that method, */
    /* otherwise we evaluate the arguments and fall */
    /* through to the generic code below.  Note that */
    /* evaluation retains any missing argument indicators. */

    if(DispatchOrEval(call, op, "$", args, env, &ans, 0, 0)) {
	UNPROTECT(2);
	return(ans);
    }

    UNPROTECT(2);
    return R_subset3_dflt(CAR(ans), STRING_ELT(input, 0));
}

/* used in eval.c */
SEXP attribute_hidden R_subset3_dflt(SEXP x, SEXP input)
{
    SEXP y, nlist;
    int slen;

    PROTECT(x);
    PROTECT(input);

    /* Optimisation to prevent repeated recalculation */
    slen = strlen(translateChar(input));

    /* If this is not a list object we return NULL. */
    /* Or should this be allocVector(VECSXP, 0)? */

    if (isPairList(x)) {
	SEXP xmatch = R_NilValue;
	int havematch;
	UNPROTECT(2);
	havematch = 0;
	for (y = x ; y != R_NilValue ; y = CDR(y)) {
	    switch(pstrmatch(TAG(y), input, slen)) {
	    case EXACT_MATCH:
		y = CAR(y);
		if (NAMED(x) > NAMED(y))
		    SET_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		xmatch = y;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if (havematch == 1) {
	    y = CAR(xmatch);
	    if (NAMED(x) > NAMED(y))
		SET_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if (isVectorList(x)) {
	int i, n, havematch, imatch=-1;
	nlist = getAttrib(x, R_NamesSymbol);
	UNPROTECT(2);
	n = length(nlist);
	havematch = 0;
	for (i = 0 ; i < n ; i = i + 1) {
	    switch(pstrmatch(STRING_ELT(nlist, i), input, slen)) {
	    case EXACT_MATCH:
		y = VECTOR_ELT(x, i);
		if (NAMED(x) > NAMED(y))
		    SET_NAMED(y, NAMED(x));
		return y;
	    case PARTIAL_MATCH:
		havematch++;
		if (havematch==1) {
		    /* partial matches can cause aliasing in eval.c:evalseq 
                       This is overkill, but alternative ways to prevent 
                       the aliasing appear to be even worse */
		    y=VECTOR_ELT(x,i);
		    SET_NAMED(y,2);
		    SET_VECTOR_ELT(x,i,y);
		}
		imatch = i;
		break;
	    case NO_MATCH:
		break;
	    }
	}
	if(havematch == 1) {
	    y = VECTOR_ELT(x, imatch);
	    if (NAMED(x) > NAMED(y))
		SET_NAMED(y, NAMED(x));
	    return y;
	}
	return R_NilValue;
    }
    else if( isEnvironment(x) ){
      	y = findVarInFrame(x, install(translateChar(input)));
      	if( TYPEOF(y) == PROMSXP ) {
	    PROTECT(y);
	    y = eval(y, R_GlobalEnv);
	    UNPROTECT(1);
      	}   	
        UNPROTECT(2);      
        if( y != R_UnboundValue ) {
            if (NAMED(x) > NAMED(y))
	    	SET_NAMED(y, NAMED(x));
	    return(y);
	}
      return R_NilValue;
    }
    else if( isVectorAtomic(x) ){
        warning("$ operator is not valid for atomic vectors, returning NULL");
    }
    else if( IS_S4_OBJECT(x) ){
        warning("$ operator not defined for this S4 class, returning NULL");
    }
    UNPROTECT(2);
    return R_NilValue;
}
