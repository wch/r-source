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

	/* The following table shows the codes which have been */
	/* assigned to the type combinations in assignments of */
	/* the form  "x[s] <- y".  Here the type of y is given */
	/* across the top of the table and the type of x at the */
	/* side. */

	/*---------------------------------------------------*/
	/*       LGL FACT  ORD  INT REAL CPLX  STR EXPR LANG */
	/* LGL  1010 1011 1012 1013 1014 1015 1016 1020 1006 */
	/* FACT 1110 1111 1112 1113 1114 1115 1116 1120 1106 */
	/* ORD  1210 1211 1212 1213 1214 1215 1216 1220 1206 */
	/* INT  1310 1311 1312 1313 1314 1315 1316 1320 1306 */
	/* REAL 1410 1411 1412 1413 1414 1415 1416 1420 1406 */
	/* CPLX 1510 1511 1512 1513 1514 1515 1516 1520 1506 */
	/* STR  1610 1611 1612 1613 1614 1615 1616 1620 1606 */
	/* EXPR 2010 2011 2012 2013 2014 2015 2016 2020 2006 */
	/* LANG  610  611  612  613  614  615  616  620  606 */
	/*---------------------------------------------------*/

	/* The following table (which is laid out as described */
	/* above) contains "*" for those combinations where the */
	/* assignment has been implemented.  Some assignments */
	/* do not make a great deal of sense and we have chosen */
	/* to leave them unimplemented, although the addition */
	/* of new assignment combinations represents no great */
	/* difficulty. */

	/*--------------------------------------------------*/
	/*      LGL FACT  ORD  INT REAL CPLX  STR EXPR LANG */
	/* LGL    *    ?    ?    *    *    *    *    *      */
	/* FACT   ?    *                        *    *      */
	/* ORD    ?         *                   *    *      */
	/* INT    *              *    *    *    *    *      */
	/* REAL   *              *    *    *    *    *      */
	/* CPLX   *              *    *    *    *    *      */
	/* STR    *    *    *    *    *    *    *    *      */
	/* EXPR                                      *    * */
	/* LANG                                             */
	/*--------------------------------------------------*/

	/* The question marks in this table indicate combinations */
	/* which should probably be implemented, but which as yet */
	/* have not been.  The reason for the LGL row and column */
	/* are because we want to allow any assignment of the form */
	/* "x[s] <- NA" (col) and because the interpreted "ifelse" */
	/* requires assignment into a logical object. */


static void CompatibleFactorsCheck(SEXP x, SEXP y)
{
	if(!factorsConform(x,y))
		error("incompatible factors in subset assignment\n");
}

static void SetArgsforUseMethod(SEXP x)
{
	char buf[4];
	int i=1;

	if( TAG(x) == R_NilValue)
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

static int R_BoundChecking = 0;

SEXP do_checkbounds(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int flag;
	checkArity(op,args);
	flag = asLogical(CAR(args));
	if(flag == NA_LOGICAL)
		errorcall(call, "invalid argument\n");
	R_BoundChecking = flag;
	R_Visible = 0;
	return R_NilValue;
}

static SEXP EnlargeVector(SEXP x, int nnew)
{
	int i, n;
	SEXP newx, ap, alist, nap, newalist;

	if(R_BoundChecking)
		warning("assignment outside vector/list limits\n");

	if(isVector(x)) {
		n = length(x);
		PROTECT(x);
		PROTECT(newx = allocVector(TYPEOF(x), nnew));
		switch(TYPEOF(x)) {
			case FACTSXP:
			case ORDSXP:
				LEVELS(newx) = LEVELS(x);
				/* drop through here */
			case LGLSXP:
			case INTSXP:
				for(i=0 ; i<n ; i++)
					INTEGER(newx)[i] = INTEGER(x)[i];
				for(i=n ; i<nnew ; i++)
					INTEGER(newx)[i] = NA_INTEGER;
				break;
			case REALSXP:
				for(i=0 ; i<n ; i++)
					REAL(newx)[i] = REAL(x)[i];
				for(i=n ; i<nnew ; i++)
					REAL(newx)[i] = NA_REAL;
				break;
			case CPLXSXP:
				for(i=0 ; i<n ; i++)
					COMPLEX(newx)[i] = COMPLEX(x)[i];
				for(i=n ; i<nnew ; i++) {
					COMPLEX(newx)[i].r = NA_REAL;
					COMPLEX(newx)[i].i = NA_REAL;
				}
				break;
			case STRSXP:
				ap = mkChar("");
				for(i=0 ; i<n ; i++)
					STRING(newx)[i] = STRING(x)[i];
				for(i=n ; i<nnew ; i++)
					STRING(newx)[i] = ap;
				break;
			case EXPRSXP:
				for(i=0 ; i<n ; i++)
					VECTOR(newx)[i] = VECTOR(x)[i];
				for(i=n ; i<nnew ; i++)
					VECTOR(newx)[i] = R_NilValue;
				break;
		}
		ap = alist = ATTRIB(x);
		PROTECT(nap = newalist = CONS(R_NilValue, R_NilValue));
		while(ap != R_NilValue) {
			if(TAG(ap) == R_NamesSymbol) {
				CDR(nap) = CONS(R_NilValue, R_NilValue);
				nap = CDR(nap);
				CAR(nap) = EnlargeVector(CAR(ap), nnew);
				TAG(nap) = TAG(ap);
			}
			else if(TAG(ap) != R_DimSymbol && TAG(ap) != R_DimNamesSymbol) {
				CDR(nap) = CONS(R_NilValue, R_NilValue);
				nap = CDR(nap);
				CAR(nap) = CAR(ap);
				TAG(nap) = TAG(ap);
			}
			ap = CDR(ap);
		}
		ATTRIB(newx) = CDR(newalist);
		OBJECT(newx) = OBJECT(x);
		UNPROTECT(3);
		return newx;
	}
	else error("attempt to enlarge non-vector\n");
}

static void SubassignTypeFix(SEXP *x, SEXP *y, int which, int stretch)
{
	switch(which) {

	case 1010:	/* logical   <- logical   */
	case 1310:	/* integer   <- logical   */
	case 1410:	/* real      <- logical   */
	case 1510:	/* complex   <- logical   */
	case 1313:	/* integer   <- integer   */
	case 1413:	/* real      <- integer   */
	case 1513:	/* complex   <- integer   */
	case 1414:	/* real      <- real      */
	case 1514:	/* complex   <- real      */
	case 1515:	/* complex   <- complex   */
	case 1616:	/* character <- character */
	case 2020:	/* character <- character */

		break;

	case 1111:	/* factor    <- factor    */
	case 1212:	/* ordered   <- ordered   */

		CompatibleFactorsCheck(*x, *y);
		break;

	case 1013:	/* logical   <- integer   */

		*x = coerceVector(*x, INTSXP);
		break;

	case 1014:	/* logical   <- real      */
	case 1314:	/* integer   <- real      */

		*x = coerceVector(*x, REALSXP);
		break;

	case 1015:	/* logical   <- complex   */
	case 1315:	/* integer   <- complex   */
	case 1415:	/* real      <- complex   */

		*x = coerceVector(*x, CPLXSXP);
		break;

	case 1610:	/* character <- logical   */
	case 1611:	/* character <- factor    */
	case 1612:	/* character <- ordered   */
	case 1613:	/* character <- integer   */
	case 1614:	/* character <- real      */
	case 1615:	/* character <- complex   */

		*y = coerceVector(*y, STRSXP);
		break;

	case 1016:	/* logical   <- character */
	case 1116:	/* factor    <- character */
	case 1216:	/* ordered   <- character */
	case 1316:	/* integer   <- character */
	case 1416:	/* real      <- character */
	case 1516:	/* complex   <- character */

		*x = coerceVector(*x, STRSXP);
		break;

	case 2001:	/* expression <- symbol    */
	case 2006:	/* expression <- language  */
	case 2010:	/* expression <- logical   */
	case 2011:	/* expression <- factor    */
	case 2012:	/* expression <- ordered   */
	case 2013:	/* expression <- integer   */
	case 2014:	/* expression <- real      */
	case 2015:	/* expression <- complex   */
	case 2016:	/* expression <- character */

		*y = coerceVector(*y, EXPRSXP);
		break;

	default:
		error("incompatible types in subset assignment\n");

	}

	if(stretch)
		*x = EnlargeVector(*x, stretch);
}

static SEXP vectorAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
	SEXP dim, index;
	int i, ii, iy, n, ny, stretch, which;
	double ry;

		/* Check to see if we have special matrix */
		/* subscripting.  If we do, make a real */
		/* subscript vector. */

	if(isNull(x) && isNull(y)) {
		return R_NilValue;
	}

	dim = getAttrib(x, R_DimSymbol);
	if (isMatrix(s) && isArray(x) &&
			(isInteger(s) || isReal(s)) &&
			ncols(s) == length(dim)) {
		s = mat2indsub(dim, s);
	}
	PROTECT(s);

	stretch = 1;
	PROTECT(index = makeSubscript(x, s, &stretch));
	n = length(index);

#ifndef OLD
	which = 100 * TYPEOF(x) + TYPEOF(y);

		/* Here we make sure that the LHS has */
		/* been coerced into a form which can */
		/* accept elements from the RHS. */

	SubassignTypeFix(&x, &y, which, stretch);
#endif
	ny = length(y);

	if (n > 0 && ny == 0)
		errorcall(call, "nothing to replace with\n");

	if (n > 0 && n % ny)
		warning("number of items to replace is not a multiple of replacement length\n");

#ifdef OLD
	which = 100 * TYPEOF(x) + TYPEOF(y);

		/* Here we make sure that the LHS has */
		/* been coerced into a form which can */
		/* accept elements from the RHS. */

	SubassignTypeFix(&x, &y, which, stretch);
#endif

	PROTECT(x);

		/* Nasty bug fixed here.  When array elements */
		/* are being permuted the rhs must be duplicated */
		/* or the elements get trashed. */

	if (x == y) PROTECT(y = duplicate(y));
	else PROTECT(y);

		/* Note that we are now committed. */
		/* Since we are mutating existing */
		/* objects any changes we make now */
		/* are (likely to be) permanent. */
		/* Beware! */

	switch(which) {

	case 1010:	/* logical   <- logical   */
	case 1310:	/* integer   <- logical   */
	case 1111:	/* factor    <- factor    */
	case 1212:	/* ordered   <- ordered   */
	case 1013:	/* logical   <- integer   */
	case 1313:	/* integer   <- integer   */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			INTEGER(x)[ii] = INTEGER(y)[i % ny];
		}
		break;

	case 1410:	/* real      <- logical   */
	case 1413:	/* real      <- integer   */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			iy = INTEGER(y)[i % ny];
			if(iy == NA_INTEGER)
				REAL(x)[ii] = NA_REAL;
			else
				REAL(x)[ii] = iy;
		}
		break;

	case 1014:	/* logical   <- real      */
	case 1314:	/* integer   <- real      */
	case 1414:	/* real      <- real      */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			REAL(x)[ii] = REAL(y)[i % ny];
		}
		break;

	case 1510:	/* complex   <- logical   */
	case 1513:	/* complex   <- integer   */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			iy = INTEGER(y)[i % ny];
			if(iy == NA_INTEGER) {
				COMPLEX(x)[ii].r = NA_REAL;
				COMPLEX(x)[ii].i = NA_REAL;
			}
			else {
				COMPLEX(x)[ii].r = iy;
				COMPLEX(x)[ii].i = 0.0;
			}
		}
		break;

	case 1514:	/* complex   <- real      */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			ry = REAL(y)[i % ny];
			if(!FINITE(ry)) {
				COMPLEX(x)[ii].r = NA_REAL;
				COMPLEX(x)[ii].i = NA_REAL;
			}
			else {
				COMPLEX(x)[ii].r = ry;
				COMPLEX(x)[ii].i = 0.0;
			}
		}
		break;

	case 1015:	/* logical   <- complex   */
	case 1315:	/* integer   <- complex   */
	case 1415:	/* real      <- complex   */
	case 1515:	/* complex   <- complex   */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			COMPLEX(x)[ii] = COMPLEX(y)[i % ny];
		}
		break;

	case 1610:	/* character <- logical   */
	case 1611:	/* character <- factor    */
	case 1612:	/* character <- ordered   */
	case 1613:	/* character <- integer   */
	case 1614:	/* character <- real      */
	case 1615:	/* character <- complex   */
	case 1616:	/* character <- character */
	case 1016:	/* logical   <- character */
	case 1116:	/* factor    <- character */
	case 1216:	/* ordered   <- character */
	case 1316:	/* integer   <- character */
	case 1416:	/* real      <- character */
	case 1516:	/* complex   <- character */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			STRING(x)[ii] = STRING(y)[i % ny];
		}
		break;

	case 2001:
	case 2006:	/* expression <- language   */
	case 2010:	/* expression <- logical    */
	case 2011:	/* expression <- factor     */
	case 2012:	/* expression <- ordered    */
	case 2013:	/* expression <- integer    */
	case 2014:	/* expression <- real       */
	case 2015:	/* expression <- complex    */
	case 2016:	/* expression <- character  */
	case 2020:	/* expression <- expression */

		for (i=0; i<n; i++) {
			ii = INTEGER(index)[i];
			if(ii == NA_INTEGER) continue;
			ii = ii - 1;
			VECTOR(x)[ii] = VECTOR(y)[i % ny];
		}
		break;

	}
	UNPROTECT(4);
	return x;
}

static SEXP matrixAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
	int i, j, ii, jj, ij, iy, k, which;
	double ry;
	int nr, ny;
	int nrs, ncs;
	SEXP sr, sc;

	if (!isMatrix(x))
		error("incorrect number of subscripts on matrix\n");

	nr = nrows(x);
	ny = LENGTH(y);

		/* s has been protected. */
		/* No GC problems here. */

	sr = CAR(s) = arraySubscript(0, CAR(s), x);
	sc = CADR(s) = arraySubscript(1, CADR(s), x);
	nrs = LENGTH(sr);
	ncs = LENGTH(sc);
	
	/* <TSL> 21Oct97*/
	if (length(y)==0)
		error("Replacement length is zero\n");
	/* </TSL>  */

	if ((length(sr) * length(sc)) % length(y))
		error("no of items to replace is not a multiple of replacement length\n");

	which = 100 * TYPEOF(x) + TYPEOF(y);

	SubassignTypeFix(&x, &y, which, 0);

	PROTECT(x);

		/* Nasty bug fixed here.  When array elements */
		/* are being permuted the rhs must be duplicated */
		/* or the elements get trashed. */

	if (x == y) PROTECT(y = duplicate(y));
	else PROTECT(y);

		/* Note that we are now committed. */
		/* Since we are mutating existing */
		/* objects any changes we make now */
		/* are (likely to be) permanent. */
		/* Beware! */

	k = 0;
	switch(which) {

	case 1010:	/* logical   <- logical   */
	case 1310:	/* integer   <- logical   */
	case 1111:	/* factor    <- factor    */
	case 1212:	/* ordered   <- ordered   */
	case 1013:	/* logical   <- integer   */
	case 1313:	/* integer   <- integer   */

		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				INTEGER(x)[ij] = INTEGER(y)[k];
				k = (k + 1) % ny;
			}
		}
		break;

	case 1410:	/* real      <- logical   */
	case 1413:	/* real      <- integer   */

		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				iy = INTEGER(y)[k];
				if(iy == NA_INTEGER)
					REAL(x)[ij] = NA_REAL;
				else
					REAL(x)[ij] = iy;
				k = (k + 1) % ny;
			}
		}
		break;

	case 1014:	/* logical   <- real      */
	case 1314:	/* integer   <- real      */
	case 1414:	/* real      <- real      */

		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				REAL(x)[ij] = REAL(y)[k];
				k = (k + 1) % ny;
			}
		}
		break;

	case 1510:	/* complex   <- logical   */
	case 1513:	/* complex   <- integer   */

		for (j=0; j<ncs ; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i=0; i<nrs ; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				iy = INTEGER(y)[k];
				if(iy == NA_INTEGER) {
					COMPLEX(x)[ij].r = NA_REAL;
					COMPLEX(x)[ij].i = NA_REAL;
				}
				else {
					COMPLEX(x)[ij].r = iy;
					COMPLEX(x)[ij].i = 0.0;
				}
				k = (k + 1) % ny;
			}
		}
		break;

	case 1514:	/* complex   <- real      */
	
		for (j=0; j<ncs ; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				ry = REAL(y)[k];
				if(!FINITE(ry)) {
					COMPLEX(x)[ij].r = NA_REAL;
					COMPLEX(x)[ij].i = NA_REAL;
				}
				else {
					COMPLEX(x)[ij].r = ry;
					COMPLEX(x)[ij].i = 0.0;
				}
				k = (k + 1) % ny;
			}
		}
		break;

	case 1015:	/* logical   <- complex   */
	case 1315:	/* integer   <- complex   */
	case 1415:	/* real      <- complex   */
	case 1515:	/* complex   <- complex   */

		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				COMPLEX(x)[ij] = COMPLEX(y)[k];
				k = (k + 1) % ny;
			}
		}
		break;

	case 1610:	/* character <- logical   */
	case 1611:	/* character <- factor    */
	case 1612:	/* character <- ordered   */
	case 1613:	/* character <- integer   */
	case 1614:	/* character <- real      */
	case 1615:	/* character <- complex   */
	case 1616:	/* character <- character */
	case 1016:	/* logical   <- character */
	case 1116:	/* factor    <- character */
	case 1216:	/* ordered   <- character */
	case 1316:	/* integer   <- character */
	case 1416:	/* real      <- character */
	case 1516:	/* complex   <- character */

		for (j = 0; j < ncs; j++) {
			jj = INTEGER(sc)[j];
			if(jj == NA_INTEGER) continue;
			jj = jj - 1;
			for (i = 0; i < nrs; i++) {
				ii = INTEGER(sr)[i];
				if(ii == NA_INTEGER) continue;
				ii = ii - 1;
				ij = ii + jj * nr;
				STRING(x)[ij] = STRING(y)[k];
				k = (k + 1) % ny;
			}
		}
		break;
	default:
		error("incompatible types in subset assignment\n");
	}
	UNPROTECT(2);
	return x;
}

static SEXP arrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
	int i, j, ii, iy, jj, k, n, ny, which;
	int **subs, *index, *bound, *offset;
	SEXP dims, tmp;
	double ry;
	char *vmax = vmaxget();

	PROTECT(dims = getAttrib(x, R_DimSymbol));
	if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
		error("incorrect number of subscripts\n");

	subs = (int**)R_alloc(k, sizeof(int*));
	index = (int*)R_alloc(k, sizeof(int));
	bound = (int*)R_alloc(k, sizeof(int));
	offset = (int*)R_alloc(k, sizeof(int));

	ny = LENGTH(y);

		/* Expand the list of subscripts. */
		/* s is protected, so no GC problems here */

	tmp = s;
	for (i = 0; i < k; i++) {
		CAR(tmp) = arraySubscript(i, CAR(tmp), x);
		tmp = CDR(tmp);
	}

	n = 1;
	tmp = s;
	for (i = 0; i < k; i++) {
		index[i] = 0;
		subs[i] = INTEGER(CAR(tmp));
		bound[i] = LENGTH(CAR(tmp));
		n *= bound[i];
		tmp = CDR(tmp);
	}

	if (n % length(y))
		error("no of elements to replace is not a multiple of replacement length\n");

	offset[0] = 1;
	for (i = 1; i < k; i++)
		offset[i] = offset[i - 1] * INTEGER(dims)[i - 1];

	which = 100 * TYPEOF(x) + TYPEOF(y);

		/* Here we make sure that the LHS has */
		/* been coerced into a form which can */
		/* accept elements from the RHS. */

	SubassignTypeFix(&x, &y, which, 0);

	PROTECT(x);

		/* Nasty bug fixed here.  When array elements */
		/* are being permuted the rhs must be duplicated */
		/* or the elements get trashed. */

	if (x == y) PROTECT(y = duplicate(y));
	else PROTECT(y);

	for (i = 0; i < n; i++) {
		ii = 0;
		for (j = 0; j < k; j++) {
			jj = subs[j][index[j]];
			if(jj == NA_INTEGER) goto next_i;
			ii += (jj - 1) * offset[j];
		}

		switch (which) {

		case 1010:	/* logical   <- logical   */
		case 1310:	/* integer   <- logical   */
		case 1111:	/* factor    <- factor    */
		case 1212:	/* ordered   <- ordered   */
		case 1013:	/* logical   <- integer   */
		case 1313:	/* integer   <- integer   */

			INTEGER(x)[ii] = INTEGER(y)[i % ny];
			break;

		case 1410:	/* real      <- logical   */
		case 1413:	/* real      <- integer   */

			iy = INTEGER(y)[i % ny];
			if(iy == NA_INTEGER)
				REAL(x)[ii] = NA_REAL;
			else
				REAL(x)[ii] = iy;
			break;

		case 1014:	/* logical   <- real      */
		case 1314:	/* integer   <- real      */
		case 1414:	/* real      <- real      */

			REAL(x)[ii] = REAL(y)[i % ny];
			break;

		case 1510:	/* complex   <- logical   */
		case 1513:	/* complex   <- integer   */

			iy = INTEGER(y)[i % ny];
			if(iy == NA_INTEGER) {
				COMPLEX(x)[ii].r = NA_REAL;
				COMPLEX(x)[ii].i = NA_REAL;
			}
			else {
				COMPLEX(x)[ii].r = iy;
				COMPLEX(x)[ii].i = 0.0;
			}
			break;

		case 1514:	/* complex   <- real      */

			ry = REAL(y)[i % ny];
			if(!FINITE(ry)) {
				COMPLEX(x)[ii].r = NA_REAL;
				COMPLEX(x)[ii].i = NA_REAL;
			}
			else {
				COMPLEX(x)[ii].r = ry;
				COMPLEX(x)[ii].i = 0.0;
			}
			break;

		case 1015:	/* logical   <- complex   */
		case 1315:	/* integer   <- complex   */
		case 1415:	/* real      <- complex   */
		case 1515:	/* complex   <- complex   */

			COMPLEX(x)[ii] = COMPLEX(y)[i % ny];
			break;

		case 1610:	/* character <- logical   */
		case 1611:	/* character <- factor    */
		case 1612:	/* character <- ordered   */
		case 1613:	/* character <- integer   */
		case 1614:	/* character <- real      */
		case 1615:	/* character <- complex   */
		case 1616:	/* character <- character */
		case 1016:	/* logical   <- character */
		case 1116:	/* factor    <- character */
		case 1216:	/* ordered   <- character */
		case 1316:	/* integer   <- character */
		case 1416:	/* real      <- character */
		case 1516:	/* complex   <- character */

			STRING(x)[ii] = STRING(y)[i % ny];
			break;
		}
		if (n > 1) {
			j = 0;
			while (++index[j] >= bound[j]) {
				index[j] = 0;
				j = ++j % k;
			}
		}
	next_i:
		;
	}
	UNPROTECT(3);
	vmaxset(vmax);
	return x;
}

static SEXP SimpleListAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
	SEXP index, yi, yp;
	int i, ii, n, nx, ny, stretch=1;

	if (length(s) > 1)
		error("invalid number of subscripts to list assign\n");

	PROTECT(index = makeSubscript(x, CAR(s), &stretch));
	n = length(index);

		/* The shallow copy here is so that */
		/* permuting a list's elements will work */

	if(isList(y) || isFrame(y) || isLanguage(y) ) {
		PROTECT(y);
		ny = NAMED(y);
		yi = allocList(length(y));
		for(yp=yi ; yp!=R_NilValue ; yp=CDR(yp)) {
			CAR(yp) = CAR(y);
			NAMED(CAR(yp)) = ny | NAMED(CAR(y));
			y = CDR(y);
		}
		UNPROTECT(1);
		PROTECT(y = yi);
	}
	else PROTECT(y = CONS(y, R_NilValue));
	ny = length(y);
	nx = length(x);

	if (n > 0 && ny == 0)
		error("nothing to replace with\n");

	if (n > 0 && n % ny)
		error("no of items to replace is not a multiple of replacement length\n");

	if(stretch) {
		yi = allocList(stretch - nx);
		PROTECT(x = listAppend(x, yi));
		nx = stretch;
	}
	else PROTECT(x);

	for (i=0; i<n; i++) {
		ii = INTEGER(index)[i];
		if(ii == NA_INTEGER) continue;
		ii = ii - 1;
		yi = CAR(nthcdr(y, i % ny));
		if(NAMED(y) || NAMED(yi)) yi = duplicate(yi);
		else NAMED(yi) = 1;
		CAR(nthcdr(x, ii % nx)) = yi;
	}
	UNPROTECT(3);
	return x;
}

static SEXP listRemove(SEXP x, SEXP s)
{
	SEXP a, pa, px;
	int i, ii, *ind, ns, nx, stretch=0;
	char *h;

	h = vmaxget();
	nx = length(x);
	PROTECT(s = makeSubscript(x, s, &stretch));
	ns = length(s);
	ind = (int*)R_alloc(nx, sizeof(int));
	for(i=0 ; i<nx ; i++)
		ind[i] = 1;
	for(i=0 ; i<ns ; i++) {
		ii = INTEGER(s)[i];
		if(ii != NA_INTEGER)
			ind[ii - 1] = 0;
	}
	PROTECT(a = CONS(R_NilValue, R_NilValue));
	px = x;
	pa = a;
	for(i=0 ; i<nx ; i++) {
		if(ind[i]) {
			CDR(pa) = px;
			px = CDR(px);
			pa = CDR(pa);
			CDR(pa) = R_NilValue;
		}
		else {
			px = CDR(px);
		}
	}
	UNPROTECT(2);
	vmaxset(h);
	return CDR(a);
}

SEXP listAssign1(SEXP call, SEXP x, SEXP subs, SEXP y)
{
	SEXP ax, ay, px, py, dims;
	int i, nsubs, ny;

	nsubs = length(subs);
	switch (nsubs) {
	case 0:
		break;
	case 1:
		if(y == R_NilValue)
			x = listRemove(x, CAR(subs));
		else
			x = SimpleListAssign(call, x, subs, y);
		break;
	default:
		dims = getAttrib(x, R_DimSymbol);
		if (dims == R_NilValue || LENGTH(dims) != length(subs))
			error("incorrect number of subscripts\n");

	  	PROTECT(ax = allocArray(STRSXP, dims));
		for(px=x, i=0 ; px!=R_NilValue ; px = CDR(px))
			STRING(ax)[i++] = CAR(px);
		setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
		if(isList(y)) {
			ny = length(y);
			PROTECT(ay = allocVector(STRSXP, ny));
			for(py=y, i=0 ; py!=R_NilValue ; py = CDR(py))
				STRING(ay)[i++] = CAR(py);
		}
		else {
			ny = 1;
			PROTECT(ay = allocVector(STRSXP, 1));
			STRING(ay)[0] = y;
		}
		if(nsubs == 2) ax = matrixAssign(call, ax, subs, ay);
		else ax = arrayAssign(call, ax, subs, ay);
		for(px=x, i=0 ; px!=R_NilValue ; px = CDR(px))
			CAR(px) = duplicate(STRING(ax)[i++]);
		UNPROTECT(2);
		break;
	}
	return x;
}

static SEXP frameAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
	int i, j, ii, jj, ij, k;
	int nr, nc, ncy;
	int nrs, ncs;
	SEXP sr, sc, ss, xp, yp;

	nr = length(CAR(x));
	nc = length(x);

		/* s has been protected. */
		/* No GC problems here. */

	if (length(s) == 1) {
		PROTECT(sr = frameSubscript(0, R_NilValue, x));
		PROTECT(sc = frameSubscript(1, CAR(s), x));
	}
	else if (length(s) == 2) {
		PROTECT(sr = frameSubscript(0, CAR(s), x));
		PROTECT(sc = frameSubscript(1, CADR(s), x)); 
	}
	else error("incorrect number of subscripts on data frame\n");

	nrs = LENGTH(sr);
	ncs = LENGTH(sc);

	/* FIXME - if y is a matrix then convert it to data frame */

	if(isList(y) || isFrame(y))
		PROTECT(y);
	else {
		SEXP tmp;
		PROTECT(y);
		PROTECT(tmp = lang2(install("as.data.frame"), y));
		y = eval(tmp, R_NilValue);
		UNPROTECT(2);
		PROTECT(y);
	}
	ncy = length(y);

	PROTECT(ss = allocList(2));

	for(i=0 ; i<ncs ; i++) {
		ii = INTEGER(sc)[i]-1;
	 	xp = nthcdr(x, ii%nc);
		yp = nthcdr(y, ii%ncy);

		if ((length(sr) * length(sc)) % length(CAR(yp)))
			error("no of items to replace is not a multiple of replacement length\n");

		if(isMatrix(CAR(xp))) {
			CAR(ss) = sr;
			CADR(ss) = arraySubscript(1,  R_MissingArg, CAR(xp));
			CAR(xp) = matrixAssign(call, CAR(xp), ss, CAR(yp));
		}
		else {
			CAR(xp) = vectorAssign(call, CAR(xp), sr, CAR(yp));
		}
	}
	UNPROTECT(4);
	return x;
}

/*  This is a special version of EvalArgs.  */
/*  We don't want to evaluate the last argument  */
/*  It has already been evaluated by applydefine  */

static SEXP EvalSubassignArgs(SEXP el, SEXP rho)
{
	SEXP ans, h, tail;

	PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

	while(CDR(el) != R_NilValue) {

		/* If we have a ... symbol, we look to see what it */
		/* is bound to.  If its binding is Null (i.e. zero length) */
		/* we just ignore it and return the cdr with all its */
		/* expressions evaluated; if it is bound to a ... list */
		/* of promises, we force all the promises and then splice */
		/* the list of resulting values into the return value. */
		/* Anything else bound to a ... symbol is an error */

		if (CAR(el) == R_DotsSymbol) {
			h = findVar(CAR(el), rho);
			if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
				while(h != R_NilValue) {
					if(CAR(h) == R_MissingArg)
						CDR(tail) = CONS(R_MissingArg, R_NilValue);
					else
						CDR(tail) = CONS(eval(CAR(h), rho), R_NilValue);
					TAG(CDR(tail)) = TAG(h);
					tail = CDR(tail);
					h = CDR(h);
				}
			}
			else if(h != R_MissingArg)
				error("... used in an incorrect context\n");
		}
		else if (CAR(el) == R_MissingArg) {
			CDR(tail) = CONS(R_MissingArg, R_NilValue);
			tail = CDR(tail);
			TAG(tail) = TAG(el);
		}
		else {
			CDR(tail) = CONS(eval(CAR(el), rho), R_NilValue);
			tail = CDR(tail);
			TAG(tail) = TAG(el);
		}
		el = CDR(el);
	}
	CDR(tail) = CONS(CAR(el), R_NilValue);
	UNPROTECT(1);
	return CDR(ans);
}

static void SubAssignArgs(SEXP args, SEXP *x, SEXP *s, SEXP *y)
{
	SEXP p;
	*x = CAR(args);
	*s = p = CDR(args);
	while(CDDR(p) != R_NilValue)
		p = CDR(p);
	*y = CADR(p);
	CDR(p) = R_NilValue;
}

	/* The [<- operator.  x is the vector that is */
	/* to be assigned into, y is the vector  that */
	/* is going to provide the new values and s is */
	/* the vector of subscripts that are going to */
	/* be replaced.  On entry (CAR(args)) and the last */
	/* argument have been evaluated been the remainder */
	/* of args have not. */
	/* If this was called directly the CAR(args) and the last */
	/* arg won't have been. */

SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP subs, x, y;
	int i, nsubs;
	RCNTXT cntxt;

	CAR(args) = eval(CAR(args), rho);
#ifndef OLD
	PROTECT(CDR(args) = EvalSubassignArgs(CDR(args), rho));
#endif
	if(isObject(CAR(args)) && CAR(call) != install("[<-.default")) {
		/*SetArgsforUseMethod(args); */
		begincontext(&cntxt,CTXT_RETURN, call, rho, rho, args);
		if(usemethod("[<-", CAR(args), call, args, rho, &y)) {
			endcontext(&cntxt);
			UNPROTECT(1);
			return y;
		}
		endcontext(&cntxt);
	}

#ifdef OLD
	PROTECT(CDR(args) = EvalSubassignArgs(CDR(args), rho));
#endif
	if (NAMED(CAR(args)) == 2) {
		x = CAR(args) = duplicate(CAR(args));
	}
	SubAssignArgs(args, &x, &subs, &y);

		/* We can't modify an object which is named in */
		/* another environment.  NAMED(x)==2 indicates */
		/* that x was obtained through a promise evaluation */
		/* and hence it may be bound to a symbol elsewhere. */
		/* This will duplicate more often than necessary, */
		/* but saves over always duplicating. */

	nsubs = length(subs);
	if (isVector(x)) {
		switch (nsubs) {
		case 0:
			break;
		case 1:
			x = vectorAssign(call, x, CAR(subs), y);
			break;
		case 2:
			x = matrixAssign(call, x, subs, y);
			break;
		default:
			x = arrayAssign(call, x, subs, y);
			break;
		}
	}
	else if(isList(x) || isLanguage(x)) {
		x = listAssign1(call, x, subs, y);
	}
	else error("type error in subset assignment\n");

		/* Note the setting of NAMED(x) to zero here. */
		/* This means that the following assignment will */
		/* not duplicate the value.  This works because */
		/* at this point, x is guaranteed to have have */
		/* at most one symbol bound to it.  It does mean */
		/* that there will be multiple reference problems */
		/* if "[<-" is used in a naked fashion. */

	UNPROTECT(1);
	NAMED(x) = 0;
	return x;
}


	/* The [[<- assignment, it should be fast. */
	/* args[1] = object being subscripted */
	/* args[2] = list of subscripts */
	/* args[3] = replacement values */

SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP dims, index, names, subs, x, y, obj;
	int i, ndims, nsubs, offset, which;
	RCNTXT cntxt;

	CAR(args) = eval(CAR(args), rho);
	if(isObject(CAR(args)) && CAR(call) != install("[[<-.default") ) {
		/*SetArgsforUseMethod(args);*/
		begincontext(&cntxt,CTXT_RETURN, call, rho, rho, args);
		if(usemethod("[[<-", CAR(args), call, args, rho, &y)) {
			endcontext(&cntxt);
			return y;
		}
		endcontext(&cntxt);
	}

	PROTECT(CDR(args) = EvalSubassignArgs(CDR(args), rho));
	SubAssignArgs(args, &x, &subs, &y);
	if(isNull(x) && isNull(y)) {
		UNPROTECT(1);
		return R_NilValue;
	}
	if (NAMED(x) == 2) {
		CAR(args) = x = duplicate(x);
	}
	dims = getAttrib(x, R_DimSymbol);
	ndims = length(dims);
	nsubs = length(subs);
#ifdef OLD
	if (!isList(x) && !isLanguage(x) && length(y) > 1)
		error("number of elements supplied larger than number of elements to replace\n");
#endif

	if (isVector(x)) {
		if(!isExpression(x) && length(y) > 1)
			error("number of elements supplied larger than number of elements to replace\n");
		if (nsubs == 1) {
			offset = get1index(CAR(subs), getAttrib(x, R_NamesSymbol),0);
			if (offset < 0 || offset >= LENGTH(x))
				error("[[]] subscript out of bounds\n");
		}
		else {
			if (ndims != nsubs)
				error("[[]] improper number of subscripts\n");
			PROTECT(index = allocVector(INTSXP, ndims));
			names = getAttrib(x, R_DimNamesSymbol);
			for (i = 0; i < ndims; i++) {
				INTEGER(index)[i] = get1index(CAR(subs), isList(names) ? CAR(names) : R_NilValue,0);
				subs = CDR(subs);
				if (INTEGER(index)[i] < 0 || INTEGER(index)[i] >= INTEGER(dims)[i])
					error("[[]] subscript out of bounds\n");
			}
			offset = 0;
			for (i = (ndims - 1); i > 0; i--)
				offset = (offset + INTEGER(index)[i]) * INTEGER(dims)[i - 1];
			offset += INTEGER(index)[0];
			UNPROTECT(1);
		}
		which = 100 * TYPEOF(x) + TYPEOF(y);

		SubassignTypeFix(&x, &y, which, 0);

		switch (which) {

		case 1010:	/* logical   <- logical   */
		case 1310:	/* integer   <- logical   */
		case 1111:	/* factor    <- factor    */
		case 1212:	/* ordered   <- ordered   */
		case 1013:	/* logical   <- integer   */
		case 1313:	/* integer   <- integer   */

			INTEGER(x)[offset] = INTEGER(y)[0];
			break;

		case 1410:	/* real      <- logical   */
		case 1413:	/* real      <- integer   */

			if(INTEGER(y)[0] == NA_INTEGER)
				REAL(x)[offset] = NA_REAL;
			else
				REAL(x)[offset] = INTEGER(y)[0];
			break;

		case 1014:	/* logical   <- real      */
		case 1314:	/* integer   <- real      */
		case 1414:	/* real      <- real      */

			REAL(x)[offset] = REAL(y)[0];
			break;

		case 1510:	/* complex   <- logical   */
		case 1513:	/* complex   <- integer   */

			if(INTEGER(y)[0] == NA_INTEGER) {
				COMPLEX(x)[offset].r = NA_REAL;
				COMPLEX(x)[offset].i = NA_REAL;
			}
			else {
				COMPLEX(x)[offset].r = INTEGER(y)[0];
				COMPLEX(x)[offset].i = 0.0;
			}
			break;

		case 1514:	/* complex   <- real      */

			if(!FINITE(REAL(y)[0])) {
				COMPLEX(x)[offset].r = NA_REAL;
				COMPLEX(x)[offset].i = NA_REAL;
			}
			else {
				COMPLEX(x)[offset].r = REAL(y)[0];
				COMPLEX(x)[offset].i = 0.0;
			}
			break;

		case 1015:	/* logical   <- complex   */
		case 1315:	/* integer   <- complex   */
		case 1415:	/* real      <- complex   */
		case 1515:	/* complex   <- complex   */

			COMPLEX(x)[offset] = COMPLEX(y)[0];
			break;

		case 1610:	/* character <- logical   */
		case 1611:	/* character <- factor    */
		case 1612:	/* character <- ordered   */
		case 1613:	/* character <- integer   */
		case 1614:	/* character <- real      */
		case 1615:	/* character <- complex   */
		case 1616:	/* character <- character */
		case 1016:	/* logical   <- character */
		case 1116:	/* factor    <- character */
		case 1216:	/* ordered   <- character */
		case 1316:	/* integer   <- character */
		case 1416:	/* real      <- character */
		case 1516:	/* complex   <- character */

			STRING(x)[offset] = STRING(y)[0];
			break;

		case 2001:	/* expression <- symbol     */
		case 2006:	/* expression <- language   */
		case 2020:	/* expression <- expression */
		case 2010:	/* expression <- logical    */
		case 2011:	/* expression <- factor     */
		case 2012:	/* expression <- ordered    */
		case 2013:	/* expression <- integer    */
		case 2014:	/* expression <- real       */
		case 2015:	/* expression <- complex    */
		case 2016:	/* expression <- character  */

			VECTOR(x)[offset] = VECTOR(y)[0];
			break;

		default:
			error("incompatible types in subset assignment\n");
		}
	}
	else if (isList(x) || isLanguage(x) ) {
		if(NAMED(y)) y = duplicate(y);
		PROTECT(y);
		if(nsubs == 1) {
			if(isNull(y)) {
				x = listRemove(x, CAR(subs));
			}
			else {
				PROTECT(y = CONS(y, R_NilValue));
				x = SimpleListAssign(call, x, subs, y);
				UNPROTECT(1);
			}
		}
		else {
			if (ndims != nsubs)
				error("[[]] improper number of subscripts\n");
			PROTECT(index = allocVector(INTSXP, ndims));
			names = getAttrib(x, R_DimNamesSymbol);
			for (i = 0; i < ndims; i++) {
				INTEGER(index)[i] = get1index(CAR(subs), CAR(names),0);
				subs = CDR(subs);
				if (INTEGER(index)[i] < 0 || INTEGER(index)[i] >= INTEGER(dims)[i])
					error("[[]] subscript out of bounds\n");
			}
			offset = 0;
			for (i = (ndims - 1); i > 0; i--)
				offset = (offset + INTEGER(index)[i]) * INTEGER(dims)[i - 1];
			offset += INTEGER(index)[0];
			CAR(nthcdr(x, offset)) = duplicate(y);
			UNPROTECT(1);
		}
		UNPROTECT(1);
	}
	else error("type error in subset assignment\n");

	UNPROTECT(1);
	NAMED(x) = 0;
	return x;
}

SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP x, nlist, val, t;

	checkArity(op, args);

	PROTECT(x = eval(CAR(args), env));

	if (!isList(x) && !isLanguage(x))
		error("$ used on non-list\n");

	/* Note: rhs is already evaluated by evaluator */ 
	val = CADDR(args);
	if(NAMED(val)) val = duplicate(val);
	PROTECT(val);

	nlist = CADR(args);
	if (isString(nlist))
		nlist = install(CHAR(STRING(nlist)[0]));

	if(TAG(x) == nlist ) {
		if(val == R_NilValue)
			x= CDR(x);
		else
			CAR(x)= val;
	}
	else {
		for (t = x; t != R_NilValue; t = CDR(t))
			if (TAG(CDR(t)) == nlist) {
				if( val == R_NilValue )
					CDR(t)=CDDR(t);
				else
					CAR(CDR(t)) = val;
				break;
			}
			else if (CDR(t) == R_NilValue && val != R_NilValue) {
				SETCDR(t, allocSExp(LISTSXP));
				TAG(CDR(t)) = nlist;
				CADR(t) = val;
				break;
			}
	}
	if( x== R_NilValue && val != R_NilValue ) {
		x=allocList(1);
		CAR(x) = val;
		TAG(x) = nlist;
	}
	UNPROTECT(2);
	FrameClassFix(x);
	NAMED(x) = 0;
	return x;
}
 
	/* Data Frame Subsetting Methods */

SEXP do_subassigndf(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP subs, x, y; 
	PROTECT(args = EvalArgs(args, rho, 0));
	SubAssignArgs(args, &x, &subs, &y);
	switch(length(subs)) {
		case 1:
			FrameClassFix(x = listAssign1(call, x, subs, y));
			break;
		case 2:
			x = frameAssign(call, x, subs, y);
			break;
		default:
			errorcall(call, "invalid number of subscripts\n");
	}
	UNPROTECT(1);
	return x;
}

SEXP do_subassigndf2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP subs, x, y; 
	PROTECT(args = EvalArgs(args, rho, 0));
	SubAssignArgs(args, &x, &subs, &y);
	switch(length(subs)) {
		case 1:
			FrameClassFix(x = listAssign1(call, x, subs, y));
			break;
		case 2:
			x = frameAssign(call, x, subs, y);
			break;
		default:
			errorcall(call, "invalid number of subscripts\n");
	}
	UNPROTECT(1);
	return x;
}
