/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997-2000   The R Development Core Team
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
 *
 *
 *  Subset Mutation for Lists and Vectors
 *
 *  The following table shows the codes which have been assigned to the
 *  type combinations in assignments of the form "x[s] <- y".  Here the
 *  type of y is given across the top of the table and the type of x at
 *  the side.  (Note: the lack of 11 and 12 indices here is due to the
 *  removal of built-in factors).
 *
 *
 *          x \ y LANG  LGL  INT REAL CPLX  STR  VEC EXPR
 *
 *          LANG   606  610  613  614  615  616  619  620
 *          LGL   1006 1010 1013 1014 1015 1016 1019 1020
 *          INT   1306 1310 1313 1314 1315 1316 1319 1320
 *          REAL  1406 1410 1413 1414 1415 1416 1419 1420
 *          CPLX  1506 1510 1513 1514 1515 1516 1519 1520
 *          STR   1606 1610 1613 1614 1615 1616 1619 1620
 *          VEC   1906 1910 1913 1914 1915 1916 1919 1920
 *          EXPR  2006 2010 2013 2014 2015 2016 1719 2020
 *
 *
 *  The following table (which is laid out as described above) contains
 *  "*" for those combinations where the assignment has been implemented.
 *  Some assignments do not make a great deal of sense and we have chosen
 *  to leave them unimplemented, although the addition of new assignment
 *  combinations represents no great difficulty.
 *
 *
 *                LANG  LGL  INT REAL CPLX  STR  VEC EXPR
 *
 *          LANG
 *          LGL           *    *    *    *    *         *
 *          INT           *    *    *    *    *         *
 *          REAL          *    *    *    *    *         *
 *          CPLX          *    *    *    *    *         *
 *          STR           *    *    *    *    *         *
 *          VEC           *                        *
 *          EXPR     *                                  *
 *
 *  The reason for the LGL row and column are because we want to allow any
 *  assignment of the form "x[s] <- NA" (col) and because the interpreted
 *  "ifelse" requires assignment into a logical object.
 */

/*
 *  2000/02/17  Altered to allow closures/primitives in lists (VECSXPs) BDR
 */

/*
 *  2000/08/01  Also promises, expressions, environments when using [[ PD
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

static SEXP gcall;

/* "EnlargeVector" takes a vector "x" and changes its length to */
/* "newlen".  This makes it possible to assign values "past the */
/* end" of the vector or list although, unlike S, we only extend */
/* as much as is necessary. */

static SEXP EnlargeVector(SEXP x, int newlen)
{
    int i, len;
    SEXP newx, names, newnames;

    /* Sanity Checks */
    if (LOGICAL(GetOption(install("check.bounds"), R_NilValue))[0])
	warning("assignment outside vector/list limits");
    if (!isVector(x))
	error("attempt to enlarge non-vector");

    /* Enlarge the vector itself. */
    len = length(x);
    PROTECT(x);
    PROTECT(newx = allocVector(TYPEOF(x), newlen));

    /* Copy the elements into place. */
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (i = 0; i < len; i++)
	    INTEGER(newx)[i] = INTEGER(x)[i];
	for (i = len; i < newlen; i++)
	    INTEGER(newx)[i] = NA_INTEGER;
	break;
    case REALSXP:
	for (i = 0; i < len; i++)
	    REAL(newx)[i] = REAL(x)[i];
	for (i = len; i < newlen; i++)
	    REAL(newx)[i] = NA_REAL;
	break;
    case CPLXSXP:
	for (i = 0; i < len; i++)
	    COMPLEX(newx)[i] = COMPLEX(x)[i];
	for (i = len; i < newlen; i++) {
	    COMPLEX(newx)[i].r = NA_REAL;
	    COMPLEX(newx)[i].i = NA_REAL;
	}
	break;
    case STRSXP:
	for (i = 0; i < len; i++)
	    STRING(newx)[i] = STRING(x)[i];
	for (i = len; i < newlen; i++)
	    STRING(newx)[i] = R_BlankString;
	break;
    case EXPRSXP:
    case VECSXP:
	for (i = 0; i < len; i++)
	    VECTOR(newx)[i] = VECTOR(x)[i];
	for (i = len; i < newlen; i++)
	    VECTOR(newx)[i] = R_NilValue;
	break;
    }

    /* Adjust the attribute list. */
    names = getAttrib(x, R_NamesSymbol);
    if (!isNull(names)) {
	PROTECT(newnames = allocVector(STRSXP, newlen));
	for (i = 0; i < len; i++)
	    STRING(newnames)[i] = STRING(names)[i];
	for (i = len; i < newlen; i++)
	    STRING(newnames)[i] = R_BlankString;
	setAttrib(newx, R_NamesSymbol, newnames);
	UNPROTECT(1);
    }
    copyMostAttrib(x, newx);
    UNPROTECT(2);
    return newx;
}


static void SubassignTypeFix(SEXP *x, SEXP *y,
			     int which, int stretch, int level)
{
    switch (which) {

    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */

    case 1010:	/* logical    <- logical    */
    case 1310:	/* integer    <- logical    */
    case 1410:	/* real	      <- logical    */
    case 1510:	/* complex    <- logical    */
    case 1313:	/* integer    <- integer    */
    case 1413:	/* real	      <- integer    */
    case 1513:	/* complex    <- integer    */
    case 1414:	/* real	      <- real	    */
    case 1514:	/* complex    <- real	    */
    case 1515:	/* complex    <- complex    */
    case 1616:	/* character  <- character  */
    case 1919:  /* vector     <- vector     */
    case 2020:	/* expression <- expression */

	break;

    case 1013:	/* logical    <- integer    */

	*x = coerceVector(*x, INTSXP);
	break;

    case 1014:	/* logical    <- real	    */
    case 1314:	/* integer    <- real	    */

	*x = coerceVector(*x, REALSXP);
	break;

    case 1015:	/* logical    <- complex    */
    case 1315:	/* integer    <- complex    */
    case 1415:	/* real	      <- complex    */

	*x = coerceVector(*x, CPLXSXP);
	break;

    case 1610:	/* character  <- logical    */
    case 1613:	/* character  <- integer    */
    case 1614:	/* character  <- real	    */
    case 1615:	/* character  <- complex    */

	*y = coerceVector(*y, STRSXP);
	break;

    case 1016:	/* logical    <- character  */
    case 1316:	/* integer    <- character  */
    case 1416:	/* real	      <- character  */
    case 1516:	/* complex    <- character  */

	*x = coerceVector(*x, STRSXP);
	break;

    case 1901:  /* vector     <- symbol   */
    case 1904:  /* vector     <- environment   */
    case 1905:  /* vector     <- promise   */
    case 1906:  /* vector     <- language   */
    case 1910:  /* vector     <- logical    */
    case 1913:  /* vector     <- integer    */
    case 1914:  /* vector     <- real       */
    case 1915:  /* vector     <- complex    */
    case 1916:  /* vector     <- character  */
    case 1920:  /* vector     <- expression  */
    case 1903: case 1907: case 1908: case 1999: /* functions */

	if (level == 1) {
	    /* Coerce the RHS into a list */
	    *y = coerceVector(*y, VECSXP);
	}
	else {
	    /* Wrap the RHS in a list */
	    SEXP tmp = allocVector(VECSXP, 1);
	    VECTOR(tmp)[0] = *y;
	    *y = tmp;
	}
	break;

    case 1019:  /* logical    <- vector     */
    case 1319:  /* integer    <- vector     */
    case 1419:  /* real       <- vector     */
    case 1519:  /* complex    <- vector     */
    case 1619:  /* character  <- vector     */
	*x = coerceVector(*x, VECSXP);
	break;

    case 2001:	/* expression <- symbol	    */
    case 2006:	/* expression <- language   */
    case 2010:	/* expression <- logical    */
    case 2013:	/* expression <- integer    */
    case 2014:	/* expression <- real	    */
    case 2015:	/* expression <- complex    */
    case 2016:	/* expression <- character  */
    case 2019:  /* expression <- vector     */

	/* Note : No coercion is needed here. */
	/* We just insert the RHS into the LHS. */
	/* FIXME : is this true or should it be */
	/* just like the "vector" case. */
	break;

    default:
	errorcall(gcall, "incompatible types");

    }

    if (stretch)
	*x = EnlargeVector(*x, stretch);
}

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    int i, ii, len, lenw;
    len = length(x);
    lenw = length(which);
    /* calculate the length of the result */
    PROTECT(include = allocVector(INTSXP, len));
    for (i = 0; i < len; i++)
	INTEGER(include)[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = INTEGER(which)[i];
	if (0 < ii  && ii <= len)
	    INTEGER(include)[ii - 1] = 0;
    }
    ii = 0;
    for (i = 0; i < len; i++)
	ii += INTEGER(include)[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }
    PROTECT(xnew = allocVector(VECSXP, ii));
    ii = 0;
    for (i = 0; i < len; i++) {
	if (INTEGER(include)[i] == 1) {
	    VECTOR(xnew)[ii] = VECTOR(x)[i];
	    ii++;
	}
    }
    xnames = getAttrib(x, R_NamesSymbol);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		STRING(xnewnames)[ii] = STRING(xnames)[i];
		ii++;
	    }
	}
	setAttrib(xnew, R_NamesSymbol, xnewnames);
	UNPROTECT(1);
    }
    copyMostAttrib(x, xnew);
    UNPROTECT(2);
    return xnew;
}

static SEXP VectorAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    SEXP dim, index;
    int i, ii, iy, n, nx, ny, stretch, which;
    double ry;

    if (isNull(x) && isNull(y)) {
	return R_NilValue;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

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

    which = 100 * TYPEOF(x) + TYPEOF(y);

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */

    SubassignTypeFix(&x, &y, which, stretch, 1);
    ny = length(y);
    nx = length(x);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (n > 0 && ny == 0)
	    errorcall(call, "nothing to replace with");
	if (n > 0 && n % ny)
	    warning("number of items to replace is not a multiple of replacement length");
    }

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are (likely */
    /* to be) permanent.  Beware! */

    switch(which) {

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
    case 1013:	/* logical   <- integer	  */
    case 1313:	/* integer   <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    INTEGER(x)[ii] = INTEGER(y)[i % ny];
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    iy = INTEGER(y)[i % ny];
	    if (iy == NA_INTEGER)
		REAL(x)[ii] = NA_REAL;
	    else
		REAL(x)[ii] = iy;
	}
	break;

    case 1014:	/* logical   <- real	  */
    case 1314:	/* integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    REAL(x)[ii] = REAL(y)[i % ny];
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    iy = INTEGER(y)[i % ny];
	    if (iy == NA_INTEGER) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = iy;
		COMPLEX(x)[ii].i = 0.0;
	    }
	}
	break;

    case 1514:	/* complex   <- real	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    ry = REAL(y)[i % ny];
	    if (ISNA(ry)) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = ry;
		COMPLEX(x)[ii].i = 0.0;
	    }
	}
	break;

    case 1015:	/* logical   <- complex	  */
    case 1315:	/* integer   <- complex	  */
    case 1415:	/* real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    COMPLEX(x)[ii] = COMPLEX(y)[i % ny];
	}
	break;

    case 1610:	/* character <- logical	  */
    case 1613:	/* character <- integer	  */
    case 1614:	/* character <- real	  */
    case 1615:	/* character <- complex	  */
    case 1616:	/* character <- character */
    case 1016:	/* logical   <- character */
    case 1316:	/* integer   <- character */
    case 1416:	/* real	     <- character */
    case 1516:	/* complex   <- character */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    STRING(x)[ii] = STRING(y)[i % ny];
	}
	break;

    case 1019:  /* vector     <- logical   */
    case 1319:  /* vector     <- integer   */
    case 1419:  /* vector     <- real      */
    case 1519:  /* vector     <- complex   */
    case 1619:  /* vector     <- character */

    case 1910:  /* vector     <- logical    */
    case 1913:  /* vector     <- integer    */
    case 1914:  /* vector     <- real       */
    case 1915:  /* vector     <- complex    */
    case 1916:  /* vector     <- character  */

    case 1919:  /* vector     <- vector     */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    VECTOR(x)[ii] = VECTOR(y)[i % ny];
	}
	break;

    case 2001:
    case 2006:	/* expression <- language   */
    case 2010:	/* expression <- logical    */
    case 2013:	/* expression <- integer    */
    case 2014:	/* expression <- real	    */
    case 2015:	/* expression <- complex    */
    case 2016:	/* expression <- character  */
    case 2020:	/* expression <- expression */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(index)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    VECTOR(x)[ii] = VECTOR(y)[i % ny];
	}
	break;

    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */

	x = DeleteListElements(x, index);
	UNPROTECT(4);
	return x;
    }
    /* Check for additional named elements. */
    /* Note we are using a horrible hack in makeSubscript */
    /* Which passes the additional names back in the attribute */
    /* slot of the generated subscript vector.  (Shudder!) */
    if (ATTRIB(index) != R_NilValue) {
	SEXP newnames = ATTRIB(index);
	SEXP oldnames = getAttrib(x, R_NamesSymbol);
	if (oldnames != R_NilValue) {
	    for (i = 0; i < n; i++) {
		if (STRING(newnames)[i] != R_NilValue) {
		    ii = INTEGER(index)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    STRING(oldnames)[ii] = STRING(newnames)[i];
		}
	    }
	}
	else {
	    PROTECT(oldnames = allocVector(STRSXP, nx));
	    for (i = 0; i < nx; i++)
		STRING(oldnames)[i] = R_BlankString;
	    for (i = 0; i < n; i++) {
		if (STRING(newnames)[i] != R_NilValue) {
		    ii = INTEGER(index)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    STRING(oldnames)[ii] = STRING(newnames)[i];
		}
	    }
	    setAttrib(x, R_NamesSymbol, oldnames);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(4);
    return x;
}

static SEXP MatrixAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, jj, ij, iy, k, n, which;
    double ry;
    int nr, ny;
    int nrs, ncs;
    SEXP sr, sc;

    if (!isMatrix(x))
	error("incorrect number of subscripts on matrix");

    nr = nrows(x);
    ny = LENGTH(y);

    /* Note that "s" has been protected. */
    /* No GC problems here. */

    sr = CAR(s) = arraySubscript(0, CAR(s), x);
    sc = CADR(s) = arraySubscript(1, CADR(s), x);
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);

    n = nrs * ncs;

    /* <TSL> 21Oct97
       if (length(y) == 0)
       error("Replacement length is zero");
       </TSL>  */

    if (n > 0 && ny == 0)
	errorcall(call, "nothing to replace with");
    if (n > 0 && n % ny)
	errorcall(call, "number of items to replace is not a multiple of replacement length");

    which = 100 * TYPEOF(x) + TYPEOF(y);

    SubassignTypeFix(&x, &y, which, 0, 1);

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are permanent. */
    /* Beware! */

    k = 0;
    switch (which) {

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
    case 1013:	/* logical   <- integer	  */
    case 1313:	/* integer   <- integer	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		INTEGER(x)[ij] = INTEGER(y)[k];
		k = (k + 1) % ny;
	    }
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		iy = INTEGER(y)[k];
		if (iy == NA_INTEGER)
		    REAL(x)[ij] = NA_REAL;
		else
		    REAL(x)[ij] = iy;
		k = (k + 1) % ny;
	    }
	}
	break;

    case 1014:	/* logical   <- real	  */
    case 1314:	/* integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		REAL(x)[ij] = REAL(y)[k];
		k = (k + 1) % ny;
	    }
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		iy = INTEGER(y)[k];
		if (iy == NA_INTEGER) {
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

    case 1514:	/* complex   <- real	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		ry = REAL(y)[k];
		if (ISNA(ry)) {
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

    case 1015:	/* logical   <- complex	  */
    case 1315:	/* integer   <- complex	  */
    case 1415:	/* real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		COMPLEX(x)[ij] = COMPLEX(y)[k];
		k = (k + 1) % ny;
	    }
	}
	break;

    case 1610:	/* character <- logical	  */
    case 1613:	/* character <- integer	  */
    case 1614:	/* character <- real	  */
    case 1615:	/* character <- complex	  */
    case 1616:	/* character <- character */
    case 1016:	/* logical   <- character */
    case 1316:	/* integer   <- character */
    case 1416:	/* real	     <- character */
    case 1516:	/* complex   <- character */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		STRING(x)[ij] = STRING(y)[k];
		k = (k + 1) % ny;
	    }
	}
	break;
    default:
	error("incompatible types in subset assignment");
    }
    UNPROTECT(2);
    return x;
}


static SEXP ArrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, iy, jj, k=0, n, ny, which;
    int **subs, *index, *bound, *offset;
    SEXP dims, tmp;
    double ry;
    char *vmax = vmaxget();

    PROTECT(dims = getAttrib(x, R_DimSymbol));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	error("incorrect number of subscripts");

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

    if (n > 0 && ny == 0)
	errorcall(call, "nothing to replace with");
    if (n > 0 && n % ny)
	errorcall(call, "number of items to replace is not a multiple of replacement length");

    offset[0] = 1;
    for (i = 1; i < k; i++)
	offset[i] = offset[i - 1] * INTEGER(dims)[i - 1];

    which = 100 * TYPEOF(x) + TYPEOF(y);

    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    SubassignTypeFix(&x, &y, which, 0, 1);

    if (ny == 0) {
	UNPROTECT(1);
	return(x);
    }

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are permanent. */
    /* Beware! */

    for (i = 0; i < n; i++) {
	ii = 0;
	for (j = 0; j < k; j++) {
	    jj = subs[j][index[j]];
	    if (jj == NA_INTEGER) goto next_i;
	    ii += (jj - 1) * offset[j];
	}

	switch (which) {

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
	case 1013:	/* logical   <- integer	  */
	case 1313:	/* integer   <- integer	  */

	    INTEGER(x)[ii] = INTEGER(y)[i % ny];
	    break;

	case 1410:	/* real	     <- logical	  */
	case 1413:	/* real	     <- integer	  */

	    iy = INTEGER(y)[i % ny];
	    if (iy == NA_INTEGER)
		REAL(x)[ii] = NA_REAL;
	    else
		REAL(x)[ii] = iy;
	    break;

	case 1014:	/* logical   <- real	  */
	case 1314:	/* integer   <- real	  */
	case 1414:	/* real	     <- real	  */

	    REAL(x)[ii] = REAL(y)[i % ny];
	    break;

	case 1510:	/* complex   <- logical	  */
	case 1513:	/* complex   <- integer	  */

	    iy = INTEGER(y)[i % ny];
	    if (iy == NA_INTEGER) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = iy;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    break;

	case 1514:	/* complex   <- real	  */

	    ry = REAL(y)[i % ny];
	    if (ISNA(ry)) {
		COMPLEX(x)[ii].r = NA_REAL;
		COMPLEX(x)[ii].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[ii].r = ry;
		COMPLEX(x)[ii].i = 0.0;
	    }
	    break;

	case 1015:	/* logical   <- complex	  */
	case 1315:	/* integer   <- complex	  */
	case 1415:	/* real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[ii] = COMPLEX(y)[i % ny];
	    break;

	case 1610:	/* character <- logical	  */
	case 1613:	/* character <- integer	  */
	case 1614:	/* character <- real	  */
	case 1615:	/* character <- complex	  */
	case 1616:	/* character <- character */
	case 1016:	/* logical   <- character */
	case 1316:	/* integer   <- character */
	case 1416:	/* real	     <- character */
	case 1516:	/* complex   <- character */

	    STRING(x)[ii] = STRING(y)[i % ny];
	    break;
	}
	if (n > 1) {
	    j = 0;
	    while (++index[j] >= bound[j]) {
		index[j] = 0;
		j = (j + 1) % k;
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
    SEXP index, xi, yi, yp;
    int i, ii, n, nx, ny, stretch=1;

    if (length(s) > 1)
	error("invalid number of subscripts to list assign");

    PROTECT(index = makeSubscript(x, CAR(s), &stretch));
    n = length(index);

    /* The shallow copy here is so that */
    /* permuting a list's elements will work */

    if (isList(y) || isFrame(y) || isLanguage(y)) {
	PROTECT(y);
	ny = NAMED(y);
	yi = allocList(length(y));
	for (yp = yi; yp != R_NilValue; yp = CDR(yp)) {
	    CAR(yp) = CAR(y);
	    TAG(yp) = TAG(y);
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
	errorcall(call, "nothing to replace with");
    if (n > 0 && n % ny)
	errorcall(call, "no of items to replace is not a multiple of replacement length");

    if (stretch) {
	yi = allocList(stretch - nx);
	PROTECT(x = listAppend(x, yi));
	nx = stretch;
    }
    else PROTECT(x);

    for (i = 0; i < n; i++) {
	ii = INTEGER(index)[i];
	if (ii == NA_INTEGER) continue;
	ii = ii - 1;
	yi = nthcdr(y, i % ny);
	xi = nthcdr(x, ii % nx);
	if (NAMED(y) || NAMED(CAR(yi))) CAR(yi) = duplicate(CAR(yi));
	else NAMED(CAR(yi)) = 1;
	CAR(xi) = CAR(yi);
	if (TAG(yi) != R_NilValue)
	    TAG(xi) = TAG(yi);
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
    for (i = 0; i < nx; i++)
	ind[i] = 1;
    for (i = 0; i < ns; i++) {
	ii = INTEGER(s)[i];
	if (ii != NA_INTEGER)
	    ind[ii - 1] = 0;
    }
    PROTECT(a = CONS(R_NilValue, R_NilValue));
    px = x;
    pa = a;
    for (i = 0; i < nx; i++) {
	if (ind[i]) {
	    CDR(pa) = px;
	    px = CDR(px);
	    pa = CDR(pa);
	    CDR(pa) = R_NilValue;
	}
	else {
	    px = CDR(px);
	}
    }
    ATTRIB(CDR(a)) = ATTRIB(x);
    OBJECT(CDR(a)) = OBJECT(x);
    NAMED(CDR(a)) = NAMED(x);
    UNPROTECT(2);
    vmaxset(h);
    return CDR(a);
}

/* unused
static SEXP listAssign1(SEXP call, SEXP x, SEXP subs, SEXP y)
{
    SEXP ax, ay, px, py, dims;
    int i, nsubs, ny;

    nsubs = length(subs);
    switch (nsubs) {
    case 0:
	break;
    case 1:
	if (y == R_NilValue)
	    x = listRemove(x, CAR(subs));
	else
	    x = SimpleListAssign(call, x, subs, y);
	break;
    default:
	dims = getAttrib(x, R_DimSymbol);
	if (dims == R_NilValue || LENGTH(dims) != length(subs))
	    error("incorrect number of subscripts");

	PROTECT(ax = allocArray(STRSXP, dims));
	for (px = x, i = 0; px != R_NilValue; px = CDR(px))
	    STRING(ax)[i++] = CAR(px);
	setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	if (isList(y)) {
	    ny = length(y);
	    PROTECT(ay = allocVector(STRSXP, ny));
	    for (py = y, i = 0; py != R_NilValue; py = CDR(py))
		STRING(ay)[i++] = CAR(py);
	}
	else {
	    ny = 1;
	    PROTECT(ay = allocVector(STRSXP, 1));
	    STRING(ay)[0] = y;
	}
	if (nsubs == 2) ax = MatrixAssign(call, ax, subs, ay);
	else ax = ArrayAssign(call, ax, subs, ay);
	for (px = x, i = 0; px != R_NilValue; px = CDR(px))
	    CAR(px) = duplicate(STRING(ax)[i++]);
	UNPROTECT(2);
	break;
    }
    return x;
}
*/

static void SubAssignArgs(SEXP args, SEXP *x, SEXP *s, SEXP *y)
{
    SEXP p;
    if (length(args) < 3)
	error("SubAssignArgs: invalid number of arguments");
    *x = CAR(args);
    *s = p = CDR(args);
    while (CDDR(p) != R_NilValue)
	p = CDR(p);
    *y = CADR(p);
    CDR(p) = R_NilValue;
}


/* The [<- operator.  "x" is the vector that is to be assigned into, */
/* y is the vector that is going to provide the new values and subs is */
/* the vector of subscripts that are going to be replaced. */
/* On entry (CAR(args)) and the last argument have been evaluated */
/* and the remainder of args have not.  If this was called directly */
/* the CAR(args) and the last arg won't have been. */

SEXP do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP subs, x, y, ans;
    int nsubs, oldtype;

    /* This code performs an internal version of method dispatch. */
    /* We evaluate the first argument and attempt to dispatch on it. */
    /* If the dispatch fails, we "drop through" to the default code below. */

    gcall = call;
    if(DispatchOrEval(call, op, args, rho, &ans, 0))
      return(ans);

    PROTECT(args = ans);

    /* If there are multiple references to an object we must */
    /* duplicate it so that only the local version is mutated. */
    /* This will duplicate more often than necessary, but saves */
    /* over always duplicating. */
    /* FIXME: shouldn't x be protected? */

    if (NAMED(CAR(args)) == 2)
	x = CAR(args) = duplicate(CAR(args));

    SubAssignArgs(args, &x, &subs, &y);
    nsubs = length(subs);

    oldtype = 0;
    if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP) {
	oldtype = TYPEOF(x);
	PROTECT(x = PairToVectorList(x));
    }
    else if (length(x) == 0) {
	if (length(y) == 0) {
	    UNPROTECT(1);
	    return(x);
	}
	else
	    PROTECT(x = coerceVector(x, TYPEOF(y)));
    }
    else {
	PROTECT(x);
    }

    switch (TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case EXPRSXP:
    case VECSXP:
	switch (nsubs) {
	case 0:
	    break;
	case 1:
	    x = VectorAssign(call, x, CAR(subs), y);
	    break;
	case 2:
	    x = MatrixAssign(call, x, subs, y);
	    break;
	default:
	    x = ArrayAssign(call, x, subs, y);
	    break;
	}
	break;
    default:
	errorcall(call, "object is not subsetable");
	break;
    }

    if (oldtype == LANGSXP) {
	x = VectorToPairList(x);
	TYPEOF(x) = LANGSXP;
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    UNPROTECT(2);
    NAMED(x) = 0;
    return x;
}

static SEXP DeleteOneVectorListItem(SEXP x, int which)
{
    SEXP y, xnames, ynames;
    int i, k, n;
    n = length(x);
    if (0 <= which && which < n) {
	PROTECT(y = allocVector(VECSXP, n - 1));
	k = 0;
	for (i = 0 ; i < n; i++)
	    if(i != which)
		VECTOR(y)[k++] = VECTOR(x)[i];
	xnames = getAttrib(x, R_NamesSymbol);
	if (xnames != R_NilValue) {
	    PROTECT(ynames = allocVector(STRSXP, n - 1));
	    k = 0;
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    STRING(ynames)[k++] = STRING(xnames)[i];
	    setAttrib(y, R_NamesSymbol, ynames);
	    UNPROTECT(1);
	}
	copyMostAttrib(x, y);
	UNPROTECT(1);
	return y;
    }
    return x;
}

/* The [[<- operator, it should be fast. */
/* args[1] = object being subscripted */
/* args[2] = list of subscripts */
/* args[3] = replacement values */

SEXP do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP dims, index, names, newname, subs, x, y, ans;
    int i, ndims, nsubs, offset, stretch, which;

    gcall = call;

    if(DispatchOrEval(call, op, args, rho, &ans, 0))
      return(ans);

    PROTECT(args = ans);

    SubAssignArgs(args, &x, &subs, &y);

    /* Handle NULL left-hand sides.  If the right-hand side */
    /* is NULL, just return the left-hand size otherwise, */
    /* convert to a zero length list (VECSXP). */

    if (isNull(x)) {
        if (isNull(y)) {
            UNPROTECT(1);
	    return x;
        }
        UNPROTECT(1);
        PROTECT(x = allocVector(TYPEOF(y), 0));
    }

    /* Ensure that the LHS is a local variable. */
    /* If it is not, then make a local copy. */

    if (NAMED(x) == 2) {
	CAR(args) = x = duplicate(x);
    }
    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    nsubs = length(subs);

    stretch = 0;
    if (isVector(x)) {
	if (!isVectorList(x) && LENGTH(y) > 1)
	    error("more elements supplied than there are to replace");
	if (nsubs == 0 || CAR(subs) == R_MissingArg)
	    error("[[]] with missing subscript");
	if (nsubs == 1) {
	    offset = OneIndex(x, CAR(subs), length(x), 0, &newname);
	    if (isVectorList(x) && isNull(y)) {
		x = DeleteOneVectorListItem(x, offset);
		UNPROTECT(1);
		return x;
	    }
	    if (offset < 0)
		error("[[]] subscript out of bounds");
	    if (offset >= LENGTH(x))
		    stretch = offset + 1;
	}
	else {
	    if (ndims != nsubs)
		error("[[]] improper number of subscripts");
	    PROTECT(index = allocVector(INTSXP, ndims));
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(index)[i] = get1index(CAR(subs), isNull(names) ?
					      R_NilValue : VECTOR(names)[i],
					      INTEGER(dims)[i],
					      0);
		subs = CDR(subs);
		if (INTEGER(index)[i] < 0 ||
		    INTEGER(index)[i] >= INTEGER(dims)[i])
		    error("[[]] subscript out of bounds");
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + INTEGER(index)[i]) * INTEGER(dims)[i - 1];
	    offset += INTEGER(index)[0];
	    UNPROTECT(1);
	}
	which = 100 * TYPEOF(x) + TYPEOF(y);

	SubassignTypeFix(&x, &y, which, stretch, 2);
	PROTECT(x);

	switch (which) {

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
	case 1013:	/* logical   <- integer	  */
	case 1313:	/* integer   <- integer	  */

	    INTEGER(x)[offset] = INTEGER(y)[0];
	    break;

	case 1410:	/* real	     <- logical	  */
	case 1413:	/* real	     <- integer	  */

	    if (INTEGER(y)[0] == NA_INTEGER)
		REAL(x)[offset] = NA_REAL;
	    else
		REAL(x)[offset] = INTEGER(y)[0];
	    break;

	case 1014:	/* logical   <- real	  */
	case 1314:	/* integer   <- real	  */
	case 1414:	/* real	     <- real	  */

	    REAL(x)[offset] = REAL(y)[0];
	    break;

	case 1510:	/* complex   <- logical	  */
	case 1513:	/* complex   <- integer	  */

	    if (INTEGER(y)[0] == NA_INTEGER) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = INTEGER(y)[0];
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	case 1514:	/* complex   <- real	  */

	    if (ISNA(REAL(y)[0])) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = REAL(y)[0];
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	case 1015:	/* logical   <- complex	  */
	case 1315:	/* integer   <- complex	  */
	case 1415:	/* real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[offset] = COMPLEX(y)[0];
	    break;

	case 1610:	/* character <- logical	  */
	case 1613:	/* character <- integer	  */
	case 1614:	/* character <- real	  */
	case 1615:	/* character <- complex	  */
	case 1616:	/* character <- character */
	case 1016:	/* logical   <- character */
	case 1316:	/* integer   <- character */
	case 1416:	/* real	     <- character */
	case 1516:	/* complex   <- character */

	    STRING(x)[offset] = STRING(y)[0];
	    break;

	case 1019:      /* logical    <- vector     */
	case 1319:      /* integer    <- vector     */
	case 1419:      /* real       <- vector     */
	case 1519:      /* complex    <- vector     */
	case 1619:      /* character  <- vector     */

	case 1901:  /* vector     <- symbol     */
	case 1904:  /* vector     <- environment*/
	case 1905:  /* vector     <- promise    */
	case 1906:  /* vector     <- language   */
	case 1910:  /* vector     <- logical    */
	case 1913:  /* vector     <- integer    */
	case 1914:  /* vector     <- real       */
	case 1915:  /* vector     <- complex    */
	case 1916:  /* vector     <- character  */
	case 1920:  /* vector     <- expression */
	case 1903: case 1907: case 1908: case 1999: /* functions */

	    VECTOR(x)[offset] = VECTOR(y)[0];
	    break;

	case 2001:	/* expression <- symbol	    */
	case 2006:	/* expression <- language   */
	case 2010:	/* expression <- logical    */
	case 2013:	/* expression <- integer    */
	case 2014:	/* expression <- real	    */
	case 2015:	/* expression <- complex    */
	case 2016:	/* expression <- character  */
	case 1919:      /* vector     <- vector     */
	case 2020:	/* expression <- expression */

            if( NAMED(y) ) y = duplicate(y);
	    VECTOR(x)[offset] = y;
	    break;

	default:
	    error("incompatible types in subset assignment");
	}
	/* If we stretched, we may have a new name. */
	/* In this case we must create a names attribute */
	/* (if it doesn't already exist) and set the new */
	/* value in the names attribute. */
	if (stretch && newname != R_NilValue) {
	    names = getAttrib(x, R_NamesSymbol);
	    if (names == R_NilValue) {
		PROTECT(names = allocVector(STRSXP, length(x)));
		STRING(names)[offset] = newname;
		setAttrib(x, R_NamesSymbol, names);
		UNPROTECT(1);
	    }
	    else
		STRING(names)[offset] = newname;
	}
	UNPROTECT(1);
    }
    else if (isList(x) || isLanguage(x)) {
	/* if (NAMED(y)) */
	y = duplicate(y);
	PROTECT(y);
	if (nsubs == 1) {
	    if (isNull(y)) {
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
		error("[[]] improper number of subscripts");
	    PROTECT(index = allocVector(INTSXP, ndims));
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(index)[i] = get1index(CAR(subs), CAR(names),
					      INTEGER(dims)[i], 0);
		subs = CDR(subs);
		if (INTEGER(index)[i] < 0 ||
		    INTEGER(index)[i] >= INTEGER(dims)[i])
		    error("[[]] subscript out of bounds");
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
    else errorcall(gcall, "object is not subsetable");

    UNPROTECT(1);
    NAMED(x) = 0;
    return x;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string
*/
SEXP do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP x, nlist, val, t, ans, input;
    checkArity(op, args);
    gcall = call;

    /* Note the RHS has alreaty been evaluated at this point */

    input = allocVector(STRSXP, 1);

    nlist = CADR(args);
    if(isSymbol(nlist) )
	STRING(input)[0] = PRINTNAME(nlist);
    else if(isString(nlist) )
	STRING(input)[0] = STRING(nlist)[0];
    else {
	errorcall(call, "invalid subscript type");
	return R_NilValue; /*-Wall*/
    }

    /* replace the second argument with a string */
    CADR(args) = input;

    if(DispatchOrEval(call, op, args, env, &ans, 0))
      return(ans);

#ifdef OLD
    PROTECT(x = eval(CAR(args), env));
    val = eval( CADDR(args), env);
#else
    PROTECT(x = CAR(ans));
    val = CADDR(ans);
#endif
    if (NAMED(val)) val = duplicate(val);
    PROTECT(val);

    if ((isList(x) || isLanguage(x)) && !isNull(x)) {
	nlist = CADR(args);
	if (isString(nlist))
	    nlist = install(CHAR(STRING(nlist)[0]));
	if (TAG(x) == nlist) {
	    if (val == R_NilValue) {
		ATTRIB(CDR(x)) = ATTRIB(x);
		OBJECT(CDR(x)) = OBJECT(x);
		NAMED(CDR(x)) = NAMED(x);
		x = CDR(x);
	    }
	    else
		CAR(x) = val;
	}
	else {
	    for (t = x; t != R_NilValue; t = CDR(t))
		if (TAG(CDR(t)) == nlist) {
		    if (val == R_NilValue)
			CDR(t) = CDDR(t);
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
	if (x == R_NilValue && val != R_NilValue) {
	    x = allocList(1);
	    CAR(x) = val;
	    TAG(x) = nlist;
	}
    }
    else {
	int i, imatch, nx;
	SEXP names;

	if (!(isNewList(x) || isExpression(x))) {
	    warning("Coercing LHS to a list");
	    x = coerceVector(x, VECSXP);
	}
	names = getAttrib(x, R_NamesSymbol);
	nx = length(x);
	nlist = CADR(args);
	if (isString(nlist))
	    nlist = STRING(nlist)[0];
	else
	    nlist = PRINTNAME(nlist);
	if (isNull(val)) {
	    /* If "val" is NULL, this is an element deletion */
	    /* if there is a match to "nlist" otherwise "x" */
	    /* is unchanged.  The attributes need adjustment. */
	    if (names != R_NilValue) {
		imatch = -1;
		for (i = 0; i < nx; i++)
		    if (NonNullStringMatch(STRING(names)[i], nlist)) {
			imatch = i;
			break;
		    }
		if (imatch >= 0) {
		    SEXP ans, ansnames;
		    int ii;
		    PROTECT(ans = allocVector(VECSXP, nx - 1));
		    PROTECT(ansnames = allocVector(STRSXP, nx - 1));
		    for (i = 0, ii = 0; i < nx; i++)
			if (i != imatch) {
			    VECTOR(ans)[ii] = VECTOR(x)[i];
			    STRING(ansnames)[ii] = STRING(names)[i];
			    ii++;
			}
		    setAttrib(ans, R_NamesSymbol, ansnames);
		    copyMostAttrib(x, ans);
		    UNPROTECT(2);
		    x = ans;
		}
		/* else x is unchanged */
	    }
	}
	else {
	    /* If "val" is non-NULL, we are either replacing */
	    /* an existing list element or we are adding a new */
	    /* element. */
	    imatch = -1;
	    if (!isNull(names)) {
		for (i = 0; i < nx; i++)
		    if (NonNullStringMatch(STRING(names)[i], nlist)) {
			imatch = i;
			break;
		    }
	    }
	    if (imatch >= 0) {
		/* We are just replacing an element */
		VECTOR(x)[imatch] = val;
	    }
	    else {
		/* We are introducing a new element. */
		/* Enlarge the list, add the new element */
		/* and finally, adjust the attributes. */
		SEXP ans, ansnames;
		PROTECT(ans = allocVector(VECSXP, nx + 1));
		PROTECT(ansnames = allocVector(STRSXP, nx + 1));
		for (i = 0; i < nx; i++)
		    VECTOR(ans)[i] = VECTOR(x)[i];
		if (isNull(names)) {
		    for (i = 0; i < nx; i++)
			STRING(ansnames)[i] = R_BlankString;
		}
		else {
		    for (i = 0; i < nx; i++)
			STRING(ansnames)[i] = STRING(names)[i];
		}
		VECTOR(ans)[nx] = val;
		STRING(ansnames)[nx] = nlist;
		setAttrib(ans, R_NamesSymbol, ansnames);
		copyMostAttrib(x, ans);
		UNPROTECT(2);
		x = ans;
	    }
	}
    }
    UNPROTECT(2);
    NAMED(x) = 0;
    return x;
}
