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

/*
 * Subscript Preamble:
 *
 * There are three kinds of subscripting [, [[, and $. We have
 * three different functions to do these. The special [ subscripting
 * where dim(x)==ncol(subscript matrix) is handled down inside
 * vectorSubset. The subscript matrix is turned into a subscript
 * vector of the appropriate size and then vectorSubset continues.
 * This provides coherence especially regarding attributes etc. (it
 * would be quicker to pull this case out and do it alone but then
 * we have 2 things to update re attributes).
 *
 */


static void SetArgsforUseMethod(SEXP x)
{
	char buf[4];
	int i=1;

	if(TAG(x) == R_NilValue)
		TAG(x) = install("x");
	for(x=CDR(x); x!=R_NilValue ; x=CDR(x)) {
		if(TAG(x) == R_NilValue) {
			if(i<10)
				sprintf(buf,"..%d",i);
			else
				sprintf(buf,".%d",i);
			TAG(x)=install(buf);
			i++;
		}
	}
}

SEXP fixLevels(SEXP result, SEXP arg)
{
	SEXP attrib, nattrib;

	if (isFactor(result)) {
		LEVELS(result) = LEVELS(arg);
		PROTECT(result);
		PROTECT(arg);
		if ((attrib = getAttrib(arg, nattrib = install("levels"))) != R_NilValue) {
			PROTECT(attrib);
			setAttrib(result, nattrib, attrib);
			UNPROTECT(1);
		}
		UNPROTECT(2);
	}
	return result;
}

	/* This does the transfer of elements from "x" to "result" */
	/* according to the integer subscripts given in "index". */

static SEXP ExtractSubset(SEXP x, SEXP result, SEXP index, SEXP call)
{
	int i, ii, n, nx, mode;
	SEXP tmp, tmp2;

	mode = TYPEOF(x);
	n = LENGTH(index);
	nx = length(x);
	tmp = result;

	if (x == R_NilValue)
		return (x);

	for (i = 0; i < n; i++) {
		ii = INTEGER(index)[i];
		if (ii != NA_INTEGER)
			ii--;
		switch (mode) {
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
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
		case VECSXP:
		case EXPRSXP:
			if (0 <= ii && ii < nx && ii != NA_INTEGER)
				VECTOR(result)[i] = STRING(x)[ii];
			else
				VECTOR(result)[i] = NA_STRING;
			break;
		case LISTSXP:
		case LANGSXP:
			if (0 <= ii && ii < nx && ii != NA_INTEGER) {
				tmp2 = nthcdr(x, ii);
				CAR(tmp) = CAR(tmp2);
				TAG(tmp) = TAG(tmp2);
			}
			else
				CAR(tmp) = R_NilValue;
			tmp = CDR(tmp);
			break;
		default:
			errorcall(call, "non-subsettable object\n");
		}
	}
	return result;
}

static SEXP vectorSubset(SEXP x, SEXP s, SEXP call)
{
	int n, mode, stretch=0;
	SEXP index, result, attrib, nattrib;

	if (s == R_MissingArg)
		return x;

	PROTECT(s);
	attrib = getAttrib(x, R_DimSymbol);

		/* Check to see if we have special matrix */
		/* subscripting.  If we do, make a real */
		/* subscript vector and protect it. */

	if (isMatrix(s) && isArray(x) &&
			(isInteger(s) || isReal(s)) &&
			ncols(s) == length(attrib)) {
		s = mat2indsub(attrib, s);
		UNPROTECT(1);
		PROTECT(s);
	}

		/* Convert to a vector of integer */
		/* subscripts in the range 1:length(x). */

	PROTECT(index = makeSubscript(x, s, &stretch));
	n = LENGTH(index);

		/* Allocate the result */

	mode = TYPEOF(x);
	/* if(mode == LANGSXP) mode = LISTSXP; */
	result = allocVector(mode, n);
	NAMED(result) = NAMED(x);
	if (isFactor(x)) LEVELS(result) = LEVELS(x);

	PROTECT(result = ExtractSubset(x, result, index, call));
	if (result != R_NilValue && 
	    (((attrib = getAttrib(x, R_NamesSymbol)) != R_NilValue)
	   || ((attrib = getAttrib(x, R_DimNamesSymbol)) != R_NilValue
	   && (attrib = CAR(attrib)) != R_NilValue))) {
		nattrib = allocVector(TYPEOF(attrib), n);
		PROTECT(nattrib);
		nattrib = ExtractSubset(attrib, nattrib, index, call);
		setAttrib(result, R_NamesSymbol, nattrib);
		UNPROTECT(1);
	}
	UNPROTECT(3);
	return result;
}

SEXP matrixSubset(SEXP x, SEXP s, SEXP call, int drop)
{
	SEXP a, attr, p, q, result, sr, sc;
	int nr, nc, nrs, ncs;
	int i, j, ii, jj, ij, iijj;

	nr = nrows(x);
	nc = ncols(x);

		/* s is protected */
		/* the following ensures that */
		/* pointers remain protected */

	sr = CAR(s) = arraySubscript(0, CAR(s), x);
	sc = CADR(s) = arraySubscript(1, CADR(s), x);
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
				errorcall(call, "subscript out of bounds\n");
			ii--;
		}
		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if (jj != NA_INTEGER) {
				if (jj < 1 || jj > nc)
					errorcall(call, "subscript out of bounds\n");
				jj--;
			}
			ij = i + j * nrs;
			if (ii == NA_INTEGER || jj == NA_INTEGER) {
				switch (TYPEOF(x)) {
				case LGLSXP:
				case INTSXP:
				case FACTSXP:
				case ORDSXP:
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
					STRING(result)[i] = NA_STRING;
					break;
				}
			}
			else {
				iijj = ii + jj * nr;
				switch (TYPEOF(x)) {
				case LGLSXP:
				case INTSXP:
				case FACTSXP:
				case ORDSXP:
					INTEGER(result)[ij] = INTEGER(x)[iijj];
					break;
				case REALSXP:
					REAL(result)[ij] = REAL(x)[iijj];
					break;
				case CPLXSXP:
					COMPLEX(result)[ij] = COMPLEX(x)[iijj];
					break;
				case STRSXP:
					STRING(result)[ij] = STRING(x)[iijj];
					break;
				}
			}
		}
	}
	if(nrs > 0 && ncs > 0) {
		PROTECT(attr = allocVector(INTSXP, 2));
		INTEGER(attr)[0] = nrs;
		INTEGER(attr)[1] = ncs;
		setAttrib(result, R_DimSymbol, attr);
		UNPROTECT(1);
	}

		/* The matrix elements have been transferred. */
		/* Now we need to transfer the attributes. */
		/* Most importantly, we need to subset the */
		/* dimnames of the returned value. */

	a = ATTRIB(x);
	while(a != R_NilValue) {
		if(TAG(a) == R_DimNamesSymbol) {
			if(nrs > 0 && ncs > 0) {
				PROTECT(attr = allocList(2));
				CAR(attr) = allocVector(STRSXP, nrs);
				CADR(attr) = allocVector(STRSXP, ncs);
				p = CAR(a);
				CAR(attr) = ExtractSubset(CAR(p), CAR(attr), sr, call);
				CADR(attr) = ExtractSubset(CADR(p), CADR(attr), sc, call);
				setAttrib(result, R_DimNamesSymbol, attr);
				UNPROTECT(1);
			}
		}
		if(TAG(a) == R_LevelsSymbol) {
			setAttrib(result, R_LevelsSymbol, CAR(a));
		}
		a = CDR(a);
	}
	if (drop) DropDims(result);
	UNPROTECT(3);
	return result;
}

SEXP frameSubset(SEXP x, SEXP s, SEXP call, int drop)
{
	SEXP sr, sc, a, ap, ss, xp, oat, s1;
	int nr, nc, ncs;
	int i;

	if ( !isFrame(x) )
		errorcall(call, "argument is not a data frame\n");

	nc = length(x);
	nr = nrows(CAR(x));

		/* s is protected */
		/* the following ensures that */
		/* pointers remain protected */

	switch(length(s)) {
	case 1:
		PROTECT(sr = allocVector(INTSXP, nr));
		for (i = 0; i < nr; i++)
			INTEGER(sr)[i] = i + 1;
		PROTECT(sc = frameSubscript(1, CAR(s), x));
		break;
	case 2:
		PROTECT(sr = frameSubscript(0, CAR(s), x));
		PROTECT(sc = frameSubscript(1, CADR(s), x));
		break;
	default:
		errorcall(call, "invalid data frame subsetting\n");
	}
	ncs = LENGTH(sc);
	if( ncs == 0 ) 
		error("zero is an invalid subscript for data frames\n"); 
	PROTECT(a = allocList(ncs));
	ap = a;
	PROTECT(ss = allocList(2));
	for(i=0 ; i<ncs ; i++) {
		xp = nthcdr(x, INTEGER(sc)[i]-1);
		if(isMatrix(CAR(xp))) {
			CAR(ss) = sr;
			CADR(ss) = arraySubscript(1,  R_MissingArg, CAR(xp));
			CAR(ap) = matrixSubset(CAR(xp), ss, call, drop);
			CAR(ap) = fixLevels(CAR(ap), CAR(xp));
			TAG(ap) = TAG(xp);
			oat = ATTRIB(CAR(ap));
			ATTRIB(CAR(ap)) = duplicate(ATTRIB(CAR(xp)));
			for( s1 = oat ; s1 != R_NilValue ; s1=CDR(s1) ) {
				if(TAG(s1)==R_DimSymbol )
					setAttrib(CAR(ap), R_DimSymbol, CAR(s1));
				if( TAG(s1) == R_LevelsSymbol )
					setAttrib(CAR(ap), R_LevelsSymbol, CAR(s1));
				if( TAG(s1) == R_DimNamesSymbol )
					setAttrib(CAR(ap), R_DimNamesSymbol,CAR(s1));
			}
			ap = CDR(ap);
		}
		else {
			CAR(ap) = vectorSubset(CAR(xp), sr, call);
			CAR(ap) = fixLevels(CAR(ap), CAR(xp));
			TAG(ap) = TAG(xp);
			oat = ATTRIB(ap);
			ATTRIB(CAR(ap)) = duplicate(ATTRIB(CAR(xp)));
			for( s1 = oat ; s1 != R_NilValue ; s1=CDR(s1) ) {
				if( TAG(s1) == R_LevelsSymbol )
					setAttrib(CAR(ap), R_LevelsSymbol, CAR(s1));
				if( TAG(s1) == R_NamesSymbol )
					setAttrib(CAR(ap), R_NamesSymbol, CAR(s1));
			}
			ap = CDR(ap);
		}
	}
	UNPROTECT(1);
	if(ncs == 1 && drop) {
		UNPROTECT(3);
		/* attach row.names(x) here as names(x) ? */
		return CAR(a);
	}
	xp = getAttrib(x, R_RowNamesSymbol);
	if(!isNull(xp)) {
		PROTECT(xp);
		PROTECT(ap = vectorSubset(xp, sr, call));
		setAttrib(a, R_RowNamesSymbol, ap);
		UNPROTECT(2);
	}
	DataFrameClass(a);
	UNPROTECT(3);
	return a;
}

static SEXP arraySubset(SEXP x, SEXP s, SEXP call, int drop)
{
	int i, j, k, ii, jj, mode, n;
	int **subs, *index, *offset, *bound;
	SEXP attr, a, p, q, r, result, xdims;
	char *vmaxsave;

	mode = TYPEOF(x);
	xdims = getAttrib(x, R_DimSymbol);
	k = length(xdims);

	vmaxsave = vmaxget();
	subs = (int**)R_alloc(k, sizeof(int*));
	index = (int*)R_alloc(k, sizeof(int));
	offset = (int*)R_alloc(k, sizeof(int));
	bound = (int*)R_alloc(k, sizeof(int));

		/* Construct a vector to contain the */
		/* returned values. Store its extents.*/

	n = 1;
	r = s;
	for (i = 0; i < k; i++) {
		CAR(r) = arraySubscript(i, CAR(r), x);
		bound[i] = LENGTH(CAR(r));
		n *= bound[i];
		r = CDR(r);
	}
	PROTECT(result = allocVector(mode, n));

		/* Construct the subscripting information. */
		/* NO ALLOCATION CAN TAKE PLACE FROM HERE TO */
		/* THE END OF THE ARRAY ELEMENT TRANSFER LOOP. */
		/* A HEAP COMPACTION COULD MOVE THE VALUES */
		/* WHICH SUBS IS POINTING AT. */

	/*
	n = 1;
	*/
	r = s;
	for (i = 0; i < k; i++) {
		index[i] = 0;
		subs[i] = INTEGER(CAR(r));
		r = CDR(r);
	}
	offset[0] = 1;
	for (i = 1; i < k; i++)
		offset[i] = offset[i - 1] * INTEGER(xdims)[i - 1];

		/* Transfer the subset elements from x to a. */

	for (i = 0; i < n; i++) {
		ii = 0;
		for (j = 0; j < k; j++) {
			jj = subs[j][index[j]];
			if (jj == NA_INTEGER) {
				ii = NA_INTEGER;
				goto assignLoop;
			}
			if (jj < 1 || jj > INTEGER(xdims)[j])
				errorcall(call, "subscript out of bounds\n");
			ii += (jj - 1) * offset[j];
		}

	assignLoop:
		switch (mode) {
		case LGLSXP:
			if (ii != NA_LOGICAL)
				LOGICAL(result)[i] = LOGICAL(x)[ii];
			else
				LOGICAL(result)[i] = NA_LOGICAL;
			break;
		case FACTSXP:
		case ORDSXP:
			if (ii != NA_INTEGER)
				FACTOR(result)[i] = FACTOR(x)[ii];
			else
				FACTOR(result)[i] = NA_FACTOR;
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
				STRING(result)[i] = STRING(x)[ii];
			else
				STRING(result)[i] = NA_STRING;
			break;
		}
		if (n > 1) {
			j = 0;
			while (++index[j] >= bound[j]) {
				index[j] = 0;
				j = ++j % k;
			}
		}
	}

	PROTECT(attr = allocVector(INTSXP, k));
	for(i=0 ; i<k ; i++)
		INTEGER(attr)[i] = bound[i];
	setAttrib(result, R_DimSymbol, attr);
	UNPROTECT(1);

		/* The array elements have been transferred. */
		/* Now we need to transfer the attributes. */
		/* Most importantly, we need to subset the */
		/* dimnames of the returned value. */ 


	a = ATTRIB(x);
	while(a != R_NilValue) {
		if(TAG(a) == R_DimNamesSymbol) {
			PROTECT(xdims = allocList(k));
			p = CAR(a);
			q = xdims;
			r = s;
			for(i=0 ; i<k ; i++) {
				CAR(q) = allocVector(STRSXP, bound[i]);
				CAR(q) = ExtractSubset(CAR(p), CAR(q), CAR(r), call);
				p = CDR(p);
				q = CDR(q); 
				r = CDR(r);
			}
			setAttrib(result, R_DimNamesSymbol, xdims);
			UNPROTECT(1);
		}
		if(TAG(a) == R_LevelsSymbol) {
			setAttrib(result, R_LevelsSymbol, CAR(a));
		}
		a = CDR(a);
	}
	/* Free Temporary memory */
	vmaxset(vmaxsave);
	if (drop) DropDims(result);
	UNPROTECT(1);
	return result;
}

static SEXP ExtractDropArg(SEXP el, int *drop)
{
	if(el == R_NilValue)
		return R_NilValue;
	if(TAG(el) == R_DropSymbol) {
		*drop = asLogical(CAR(el));
		if (*drop == NA_LOGICAL) *drop = 1;
		return ExtractDropArg(CDR(el), drop);
	}
	CDR(el) = ExtractDropArg(CDR(el), drop);
	return el;
}


	/* The [] subset operator.  This provides */
	/* the most general form of subsetting. */

SEXP do_subset(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans, dim, ax, px, x, subs;
	int drop, i, nsubs;

		/* If the first argument is an object and */
		/* there is an approriate method, we dispatch */
		/* to that method, otherwise we evaluate the */
		/* arguments and fall through to the generic */
		/* code below.  Note that evaluation retains */
		/* any missing arggument indicators. */

	if(DispatchOrEval(call, op, args, rho, &ans, 0))
		return(ans);

		/* Method dispatch has failed, we now just */
		/* run the generic internal code */
	
	PROTECT(args = ans);
	drop = 1;
	ExtractDropArg(args, &drop);
	x = CAR(args);

		/* This is intended for compatibility with S, */
		/* but in fact S does not do this. */

	if (x == R_NilValue) {
		UNPROTECT(1);
		return x;
	}	

	subs = CDR(args);

	if(isVector(x) || isList(x) || isLanguage(x)) {

		PROTECT(dim = getAttrib(x, R_DimSymbol));
		nsubs = length(subs);

		if(nsubs == 1) {
			ans = vectorSubset(x, CAR(subs), call);
		}
		else {
			if (nsubs != length(dim))
				errorcall(call, "incorrect number of dimensions\n");
			if(isList(x)) {
				PROTECT(ax = allocArray(STRSXP, dim));
				for(px=x, i=0 ; px!=R_NilValue ; px = CDR(px))
					STRING(ax)[i++] = CAR(px);
				setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
				if(nsubs == 2) ax = matrixSubset(ax, subs, call, drop);
				else ax = arraySubset(ax, subs, call, drop);
				PROTECT(ans = allocList(LENGTH(ax)));
				for(px=ans, i=0 ; px!=R_NilValue ; px = CDR(px))
					CAR(px) = STRING(ax)[i++];
				setAttrib(ans, R_DimSymbol, getAttrib(ax, R_DimSymbol));
				setAttrib(ans, R_DimNamesSymbol, getAttrib(ax, R_DimNamesSymbol));
				setAttrib(ans, R_NamesSymbol, getAttrib(ax, R_NamesSymbol));
				UNPROTECT(2);
			}
			else {
				if(nsubs == 2) ans = matrixSubset(x, subs, call, drop);
				else ans = arraySubset(x, subs, call, drop);
			}
		}
		UNPROTECT(1);
	}
	else errorcall(call, "object is not subsettable\n");

	setAttrib(ans, R_TspSymbol, R_NilValue);
	setAttrib(ans, R_ClassSymbol, R_NilValue);

	UNPROTECT(1);
	return fixLevels(ans, x);
}


	/* The [[]] subset operator.  It needs to be fast. */
	/* the arguments to this call are evaluated */

SEXP do_subset2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP ans, dims, dimnames, index, subs, x;
	int drop, i, ndims, nsubs, offset;

		/* If the first argument is an object and */
		/* there is an approriate method, we dispatch */
		/* to that method, otherwise we evaluate the */
		/* arguments and fall through to the generic */
		/* code below.  Note that evaluation retains */
		/* any missing arggument indicators. */

	if(DispatchOrEval(call, op, args, rho, &ans, 0))
		return(ans);

		/* Method dispatch has failed, we now just */
		/* run the generic internal code */
	
	PROTECT(args = ans);
	drop = 1;
	ExtractDropArg(args, &drop);
	x = CAR(args);
	
		/* This code was intended for compatibility with S, */
		/* but in fact S does not do this.  Will anyone notice? */

	if (x == R_NilValue) {
		UNPROTECT(1);
		return x;
	}

	subs = CDR(args);
	nsubs = length(subs);

	dims = getAttrib(x, R_DimSymbol);
	ndims = length(dims);
		
	if(nsubs > 1 && nsubs != ndims)
		errorcall(call, "incorrect number of subscripts\n");
		
	if (isVector(x) || isList(x) || isLanguage(x)) {
		
		if(nsubs == 1) {
			offset = get1index(CAR(subs), getAttrib(x, R_NamesSymbol),1);
			if (offset < 0 || offset >= length(x))
				/* a bold attempt to get the same behaviour
				   for $ and [[ */
				if( offset<0 && (isList(x) || isLanguage(x))) {
					UNPROTECT(1);
					return(R_NilValue);
				}
				else
					errorcall(call, "subscript out of bounds\n");
		}
		else {
			/* Here we use the fact that */
			/* CAR(R_NilValue) = R_NilValue */
			/* CDR(R_NilValue) = R_NilValue */
			PROTECT(index = allocVector(INTSXP, nsubs));
			dimnames = getAttrib(x, R_DimNamesSymbol);
			for (i = 0; i < nsubs; i++) {
				INTEGER(index)[i] = get1index(CAR(subs), CAR(dimnames),1);
				subs = CDR(subs);
				dimnames = CDR(dimnames);
				if (INTEGER(index)[i] < 0 || INTEGER(index)[i] >= INTEGER(dims)[i])
					errorcall(call, "subscript out of bounds\n");
			}
			offset = 0;
			for (i = (nsubs - 1); i > 0; i--)
				offset = (offset + INTEGER(index)[i]) * INTEGER(dims)[i - 1];
			offset += INTEGER(index)[0];
			UNPROTECT(1);
		}

	}
	else errorcall(call, "object is not subsettable\n");

	if(isList(x) || isLanguage(x)) {
		ans = CAR(nthcdr(x, offset));
		NAMED(ans) = NAMED(x);
		UNPROTECT(1);
		return ans;
	}
	if(isExpression(x)) {
		ans = duplicate(VECTOR(x)[offset]);
		UNPROTECT(1);
		return ans;
	}
	else {
		ans = allocVector(TYPEOF(x), 1);
		switch (TYPEOF(x)) {
		case LGLSXP:
		case FACTSXP:
		case ORDSXP:
		case INTSXP:
			INTEGER(ans)[0] = INTEGER(x)[offset];
			break;
		case REALSXP:
			REAL(ans)[0] = REAL(x)[offset];
			break;
		case STRSXP:
			STRING(ans)[0] = STRING(x)[offset];
			break;
		default:
			UNIMPLEMENTED("do_subset2");
		}
	}
	UNPROTECT(1);
	return fixLevels(ans, x);
}

	/* a helper to partially match tags against a candidate */
	/* return  0 for a perfect match  */
	/*	 1 for a partial match  */
	/*	-1 for no match         */

static int pstrmatch(SEXP target, char * input, int slen)
{
	int t,k,perfect;

	if(target==R_NilValue) return -1;

	k=strncmp(CHAR(PRINTNAME(target)), input, slen);

	if( k==0 ) {
		if (strlen(CHAR(PRINTNAME(target)))==slen)
			return 0;
		else
			return 1;
	}
	else return -1;
}


	/* The $ subset operator.  We need to be sure to */
	/* only evaluate the first arg.  The second will be */
	/*a symbol that needs to be matched, not evaluated */

SEXP do_subset3(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP x, y, nlist;
	int slen, posi, s, perfect, idx;
	char *input;

	checkArity(op, args);

	PROTECT(x = eval(CAR(args), env));

	if (!isList(x) && !isFrame(x) && !isLanguage(x) ) {
		UNPROTECT(1);
		return R_NilValue;
	}

	nlist = CADR(args);
	/* if (isString(nlist))
		nlist = install(CHAR(STRING(nlist)[0]));*/
	if( isSymbol(nlist) )
		input = CHAR(PRINTNAME(nlist));
	else if( isString(nlist) )
		input = CHAR(STRING(nlist)[0]);
	else
		errorcall(call,"invalid subscript type\n");

	slen = strlen(input);
	posi = idx = 0;

	UNPROTECT(1);
	for (y = x; y != R_NilValue; y = CDR(y)) {
		posi++;
		s = pstrmatch(TAG(y), input, slen);
		if( s == 0 )
			break;
		if( s == 1 ) {
			if( idx == 0 )
				idx = posi;
			else
				idx = -1; /* more than 1 partial match */
		}
	}
	if(s) {
		if( idx > 0)
			y=nthcdr(x,idx-1);
		else
			return R_NilValue; /* more than 1 partial match */
	}
	NAMED(CAR(y)) = NAMED(x);
	return CAR(y);
}

	/* Data Frame subsetting */

SEXP do_subsetdf(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP frame;
	int drop = 1;

	PROTECT(args = EvalArgs(args, rho, 0));
	ExtractDropArg(args, &drop);
	frame = frameSubset(CAR(args), CDR(args), call, drop);
	UNPROTECT(1);
	return frame;
}

SEXP do_subsetdf2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP frame;
	int drop = 1;

	PROTECT(args = EvalArgs(args, rho, 0));
	ExtractDropArg(args, &drop);
	frame = frameSubset(CAR(args), CDR(args), call, drop);
	UNPROTECT(1);
	if(isFrame(frame))
		return CAR(frame);
	return frame;
}
