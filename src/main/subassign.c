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
 *  Subset Mutation for Lists and Vectors
 *
 *  The following table shows the codes which have been assigned to the
 *  type combinations in assignments of the form "x[s] <- y".  Here the
 *  type of y is given across the top of the table and the type of x at
 *  the side.  (Note: the lack of 11 and 12 indices here is due to the
 *  removal of built-in factors).
 *
 *  NB these tables are out of date, and exclude tupes 21, 22, 23, 24 ...
 *
 x \ y   NIL  SYM CLOS  ENV PROM LANG SPE- BUI-  LGL  INT REAL CPLX  STR  VEC EXPR  FUN
			              CIAL LTIN
 LANG    600  601  603  604  605  606  607  608  610  613  614  615  616  619  620  699
 LGL    1000 1001 1003 1004 1005 1006 1007 1008 1010 1013 1014 1015 1016 1019 1020 1099
 INT    1300 1301 1303 1304 1305 1306 1307 1308 1310 1313 1314 1315 1316 1319 1320 1399
 REAL   1400 1401 1403 1404 1405 1406 1407 1408 1410 1413 1414 1415 1416 1419 1420 1499
 CPLX   1500 1501 1503 1504 1505 1506 1507 1508 1510 1513 1514 1515 1516 1519 1520 1599
 STR    1600 1601 1603 1604 1605 1606 1607 1608 1610 1613 1614 1615 1616 1619 1620 1699
 VEC    1900 1901 1903 1904 1905 1906 1907 1908 1910 1913 1914 1915 1916 1919 1920 1999
 EXPR   2000 2001 2003 2004 2005 2006 2007 2008 2010 2013 2014 2015 2016 2019 2020 2099
 *
 *
 *  The following table (which is laid out as described above) contains
 *  "*" for those combinations where the assignment has been implemented.
 *  Some assignments do not make a great deal of sense and we have chosen
 *  to leave them unimplemented, although the addition of new assignment
 *  combinations represents no great difficulty.
 *
 *       NIL   SYM CLOS  ENV PROM LANG SPE- BUI-  LGL  INT REAL CPLX  STR  VEC EXPR  FUN
 *				       CIAL LTIN
 LANG
 LGL						   *    *    *    *    *    *    *
 INT						   *    *    *    *    *    *    *
 REAL						   *    *    *    *    *    *    *
 CPLX						   *    *    *    *    *    *    *
 STR						   *    *    *    *    *    *    *
 VEC      *     *    *    *    *    *    *    *    *    *    *    *    *    *    *    *
 EXPR     *     *                   *		   *    *    *    *    *    *    *
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
#include <R_ext/RS.h> /* for test of S4 objects */

#if 0
static SEXP gcall;
#endif

/* EnlargeVector() takes a vector "x" and changes its length to "newlen".
   This allows to assign values "past the end" of the vector or list.
   Note that, unlike S, we only extend as much as is necessary.
*/
static SEXP EnlargeVector(SEXP x, R_len_t newlen)
{
    R_len_t i, len;
    SEXP newx, names, newnames;

    /* Sanity Checks */
    if (!isVector(x))
	error(_("attempt to enlarge non-vector"));

    /* Enlarge the vector itself. */
    len = length(x);
    if (LOGICAL(GetOption(install("check.bounds"), R_BaseEnv))[0])
	warning(_("assignment outside vector/list limits (extending from %d to %d)"),
		len, newlen);
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
	    SET_STRING_ELT(newx, i, STRING_ELT(x, i));
	for (i = len; i < newlen; i++)
	    SET_STRING_ELT(newx, i, NA_STRING); /* was R_BlankString  < 1.6.0 */
	break;
    case EXPRSXP:
    case VECSXP:
	for (i = 0; i < len; i++)
	    SET_VECTOR_ELT(newx, i, VECTOR_ELT(x, i));
	for (i = len; i < newlen; i++)
	    SET_VECTOR_ELT(newx, i, R_NilValue);
	break;
    case RAWSXP:
	for (i = 0; i < len; i++)
	    RAW(newx)[i] = RAW(x)[i];
	for (i = len; i < newlen; i++)
	    RAW(newx)[i] = (Rbyte) 0;
	break;
    default:
	UNIMPLEMENTED_TYPE("EnlargeVector", x);
    }

    /* Adjust the attribute list. */
    names = getAttrib(x, R_NamesSymbol);
    if (!isNull(names)) {
	PROTECT(newnames = allocVector(STRSXP, newlen));
	for (i = 0; i < len; i++)
	    SET_STRING_ELT(newnames, i, STRING_ELT(names, i));
	for (i = len; i < newlen; i++)
	    SET_STRING_ELT(newnames, i, R_BlankString);
	setAttrib(newx, R_NamesSymbol, newnames);
	UNPROTECT(1);
    }
    copyMostAttrib(x, newx);
    UNPROTECT(2);
    return newx;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in wich coerceVector should
   fail; namely, S4SXP */
static SEXP embedInVector(SEXP v)
{
    SEXP ans;
    PROTECT(ans = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, v);
    UNPROTECT(1);
    return (ans);
}

/* Level 1 is used in VectorAssign, MatrixAssign, ArrayAssign.
   That coerces RHS to a list or expression.

   Level 2 is used in do_subassign2_dflt. 
   This does not coerce when assigning into a list.
*/

static int SubassignTypeFix(SEXP *x, SEXP *y, int stretch, int level, 
			    SEXP call)
{
    /* A rather pointless optimization, but level 2 used to be handled
       differently */
    Rboolean redo_which = TRUE;
    int which = 100 * TYPEOF(*x) + TYPEOF(*y);
    /* coercion can lose the object bit */
    Rboolean x_is_object = OBJECT(*x);

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
    case 2424:	/* raw        <- raw        */

	redo_which = FALSE;
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
#ifdef BYTECODE
    case 1921:  /* vector     <- bytecode   */
#endif
    case 1922:  /* vector     <- external pointer */
    case 1923:  /* vector     <- weak reference */
    case 1924:  /* vector     <- raw */
    case 1903: case 1907: case 1908: case 1999: /* functions */

	if (level == 1) {
	    /* Coerce the RHS into a list */
	    *y = coerceVector(*y, VECSXP);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	    redo_which = FALSE;
	}
	break;

    case 1925: /* vector <- S4 */

	if (level == 1) {
	    /* Embed the RHS into a list */
	    *y = embedInVector(*y);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	    redo_which = FALSE;
	}
	break;

    case 1019:  /* logical    <- vector     */
    case 1319:  /* integer    <- vector     */
    case 1419:  /* real       <- vector     */
    case 1519:  /* complex    <- vector     */
    case 1619:  /* character  <- vector     */
    case 2419:  /* raw        <- vector     */
	*x = coerceVector(*x, VECSXP);
	break;

    case 1020:  /* logical    <- expression */
    case 1320:  /* integer    <- expression */
    case 1420:  /* real       <- expression */
    case 1520:  /* complex    <- expression */
    case 1620:  /* character  <- expression */
    case 2420:  /* raw        <- expression */
	*x = coerceVector(*x, EXPRSXP);
	break;

    case 2001:	/* expression <- symbol	    */
    case 2006:	/* expression <- language   */
    case 2010:	/* expression <- logical    */
    case 2013:	/* expression <- integer    */
    case 2014:	/* expression <- real	    */
    case 2015:	/* expression <- complex    */
    case 2016:	/* expression <- character  */
    case 2019:  /* expression <- vector     */

	if (level == 1) {
	    /* Coerce the RHS into a list */
	    *y = coerceVector(*y, VECSXP);
	} else {
	    /* Note : No coercion is needed here. */
	    /* We just insert the RHS into the LHS. */
	    redo_which = FALSE;
	}
	break;

    case 2025: /* expression <- S4 */

	if (level == 1) {
	    /* Embed the RHS into a list */
	    *y = embedInVector(*y);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	    redo_which = FALSE;
	}
	break;

    default:
	error(_("incompatible types (from %s to %s) in subassignment type fix"),
	      type2char(which%100), type2char(which/100));
    }

    if (stretch) {
	PROTECT(*y);
	*x = EnlargeVector(*x, stretch);
	UNPROTECT(1);
    }
    SET_OBJECT(*x, x_is_object);

    if(redo_which)
	return(100 * TYPEOF(*x) + TYPEOF(*y));
    else
	return(which);
}

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    R_len_t i, ii, len, lenw;
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
	    SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
	    ii++;
	}
    }
    xnames = getAttrib(x, R_NamesSymbol);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (INTEGER(include)[i] == 1) {
		SET_STRING_ELT(xnewnames, ii, STRING_ELT(xnames, i));
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
    SEXP dim, indx, newnames;
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
    PROTECT(indx = makeSubscript(x, s, &stretch));
    n = length(indx);
    if(length(y) > 1)
	for(i = 0; i < n; i++)
	    if(INTEGER(indx)[i] == NA_INTEGER)
		error(_("NAs are not allowed in subscripted assignments"));

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */
    which = SubassignTypeFix(&x, &y, stretch, 1, call);
    /* = 100 * TYPEOF(x) + TYPEOF(y);*/
    ny = length(y);
    nx = length(x);

    PROTECT(x);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (n > 0 && ny == 0)
	    error(_("nothing to replace with"));
	if (n > 0 && n % ny)
	    warning(_("number of items to replace is not a multiple of replacement length"));
    }


    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed. */
    /* Since we are mutating existing objects, */
    /* any changes we make now are (likely to be) permanent.  Beware! */

    switch(which) {
	/* because we have called SubassignTypeFix the commented
	   values cannot occur (and would be unsafe) */

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
    /* case 1013:  logical   <- integer	  */
    case 1313:	/* integer   <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    INTEGER(x)[ii] = INTEGER(y)[i % ny];
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    iy = INTEGER(y)[i % ny];
	    if (iy == NA_INTEGER)
		REAL(x)[ii] = NA_REAL;
	    else
		REAL(x)[ii] = iy;
	}
	break;

    /* case 1014:  logical   <- real	  */
    /* case 1314:  integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    REAL(x)[ii] = REAL(y)[i % ny];
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
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
	    ii = INTEGER(indx)[i];
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

    /* case 1015:  logical   <- complex	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1415:  real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
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
    /* case 1016:  logical   <- character */
    /* case 1316:  integer   <- character */
    /* case 1416:  real	     <- character */
    /* case 1516:  complex   <- character */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    SET_STRING_ELT(x, ii, STRING_ELT(y, i % ny));
	}
	break;

    /* case 1019:  logial     <- vector   */
    /* case 1319:  integer    <- vector   */
    /* case 1419:  real       <- vector   */
    /* case 1519:  complex    <- vector   */
    /* case 1619:  character  <- vector   */

    /* case 1910:  vector     <- logical    */
    /* case 1913:  vector     <- integer    */
    /* case 1914:  vector     <- real       */
    /* case 1915:  vector     <- complex    */
    /* case 1916:  vector     <- character  */

    case 1919:  /* vector     <- vector     */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    SET_VECTOR_ELT(x, ii, VECTOR_ELT(y, i % ny));
	}
	break;

    /* case 2001: */
    /* case 2006:  expression <- language   */
    /* case 2010:  expression <- logical    */
    /* case 2013:  expression <- integer    */
    /* case 2014:  expression <- real	    */
    /* case 2015:  expression <- complex    */
    /* case 2016:  expression <- character  */
    case 2019:	/* expression <- vector, needed if we have promoted a
		   RHS  to a list */
    case 2020:	/* expression <- expression */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    if (ii == NA_INTEGER) continue;
	    ii = ii - 1;
	    SET_VECTOR_ELT(x, ii, VECTOR_ELT(y, i % ny));
	}
	break;

    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */

	x = DeleteListElements(x, indx);
	UNPROTECT(4);
	return x;
	break;

    case 2424:	/* raw   <- raw	  */

	for (i = 0; i < n; i++) {
	    ii = INTEGER(indx)[i];
	    ii = ii - 1;
	    RAW(x)[ii] = RAW(y)[i % ny];
	}
	break;

    default:
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }
    /* Check for additional named elements. */
    /* Note makeSubscript passes the additional names back as the names
       attribute of the generated subscript vector */
    newnames = getAttrib(indx, R_NamesSymbol);
    if (newnames != R_NilValue) {
	SEXP oldnames = getAttrib(x, R_NamesSymbol);
	if (oldnames != R_NilValue) {
	    for (i = 0; i < n; i++) {
		if (STRING_ELT(newnames, i) != R_NilValue) {
		    ii = INTEGER(indx)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, STRING_ELT(newnames, i));
		}
	    }
	}
	else {
	    PROTECT(oldnames = allocVector(STRSXP, nx));
	    for (i = 0; i < nx; i++)
		SET_STRING_ELT(oldnames, i, R_BlankString);
	    for (i = 0; i < n; i++) {
		if (STRING_ELT(newnames, i) != R_NilValue) {
		    ii = INTEGER(indx)[i];
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, STRING_ELT(newnames, i));
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
    SEXP sr, sc, dim;

    if (!isMatrix(x))
	error(_("incorrect number of subscripts on matrix"));

    nr = nrows(x);
    ny = LENGTH(y);

    /* Note that "s" has been protected. */
    /* No GC problems here. */

    dim = getAttrib(x, R_DimSymbol);
    sr = SETCAR(s, arraySubscript(0, CAR(s), dim, getAttrib,
				  (STRING_ELT), x));
    sc = SETCADR(s, arraySubscript(1, CADR(s), dim, getAttrib,
				   (STRING_ELT), x));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);
    if(ny > 1) {
	for(i = 0; i < nrs; i++)
	    if(INTEGER(sr)[i] == NA_INTEGER)
		error(_("NAs are not allowed in subscripted assignments"));
	for(i = 0; i < ncs; i++)
	    if(INTEGER(sc)[i] == NA_INTEGER)
		error(_("NAs are not allowed in subscripted assignments"));
    }

    n = nrs * ncs;

    /* <TSL> 21Oct97
       if (length(y) == 0)
       error("Replacement length is zero");
       </TSL>  */

    if (n > 0 && ny == 0)
	error(_("nothing to replace with"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    which = SubassignTypeFix(&x, &y, 0, 1, call);

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
	/* because we have called SubassignTypeFix the commented
	   values cannot occur (and would be unsafe) */

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
    /* case 1013: logical   <- integer	  */
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

    /* case 1014:  logical   <- real	  */ 
    /* case 1314:  integer   <- real	  */
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

    /* case 1015:  logical   <- complex	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1415:  real	     <- complex	  */
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
    /* case 1016:  logical   <- character */
    /* case 1316:  integer   <- character */
    /* case 1416:  real	     <- character */
    /* case 1516:  complex   <- character */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		SET_STRING_ELT(x, ij, STRING_ELT(y, k));
		k = (k + 1) % ny;
	    }
	}
	break;
    case 1919: /* vector <- vector */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		SET_VECTOR_ELT(x, ij, VECTOR_ELT(y, k));
		k = (k + 1) % ny;
	    }
	}
	break;

    case 2424: /* raw   <- raw   */

	for (j = 0; j < ncs; j++) {
	    jj = INTEGER(sc)[j];
	    if (jj == NA_INTEGER) continue;
	    jj = jj - 1;
	    for (i = 0; i < nrs; i++) {
		ii = INTEGER(sr)[i];
		if (ii == NA_INTEGER) continue;
		ii = ii - 1;
		ij = ii + jj * nr;
		RAW(x)[ij] = RAW(y)[k];
		k = (k + 1) % ny;
	    }
	}
	break;

    default:
	error(_("incompatible types (from %s to %s) in matrix subset assignment"),
		  type2char(which%100), type2char(which/100));
    }
    UNPROTECT(2);
    return x;
}


static SEXP ArrayAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    int i, j, ii, iy, jj, k=0, n, ny, which;
    int **subs, *indx, *bound, *offset;
    SEXP dims, tmp;
    double ry;
    char *vmax = vmaxget();

    PROTECT(dims = getAttrib(x, R_DimSymbol));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	error(_("incorrect number of subscripts"));

    subs = (int**)R_alloc(k, sizeof(int*));
    indx = (int*)R_alloc(k, sizeof(int));
    bound = (int*)R_alloc(k, sizeof(int));
    offset = (int*)R_alloc(k, sizeof(int));

    ny = LENGTH(y);

    /* Expand the list of subscripts. */
    /* s is protected, so no GC problems here */

    tmp = s;
    for (i = 0; i < k; i++) {
	SETCAR(tmp, arraySubscript(i, CAR(tmp), dims, getAttrib,
				   (STRING_ELT), x));
	tmp = CDR(tmp);
    }

    n = 1;
    tmp = s;
    for (i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER(CAR(tmp));
	bound[i] = LENGTH(CAR(tmp));
	n *= bound[i];
	tmp = CDR(tmp);
    }

    if (n > 0 && ny == 0)
	error(_("nothing to replace with"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    if (ny > 1) { /* check for NAs in indices */
	for (i = 0; i < k; i++)
	    for (j = 0; j < bound[i]; j++)
		if (subs[i][j] == NA_INTEGER)
		    error(_("NAs are not allowed in subscripted assignments"));
    }

    offset[0] = 1;
    for (i = 1; i < k; i++)
	offset[i] = offset[i - 1] * INTEGER(dims)[i - 1];


    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    which = SubassignTypeFix(&x, &y, 0, 1, call);/* = 100 * TYPEOF(x) + TYPEOF(y);*/

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
	    jj = subs[j][indx[j]];
	    if (jj == NA_INTEGER) goto next_i;
	    ii += (jj - 1) * offset[j];
	}

	switch (which) {

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
	/* case 1013:	   logical   <- integer	  */
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

	/* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
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

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[ii] = COMPLEX(y)[i % ny];
	    break;

	case 1610:	/* character <- logical	  */
	case 1613:	/* character <- integer	  */
	case 1614:	/* character <- real	  */
	case 1615:	/* character <- complex	  */
	case 1616:	/* character <- character */
	/* case 1016:	   logical   <- character */
	/* case 1316:	   integer   <- character */
	/* case 1416:	   real	     <- character */
	/* case 1516:	   complex   <- character */

	    SET_STRING_ELT(x, ii, STRING_ELT(y, i % ny));
	    break;

	case 1919: /* vector <- vector */

	    SET_VECTOR_ELT(x, ii, VECTOR_ELT(y, i % ny));
	    break;

	case 2424: /* raw <- raw */

	    RAW(x)[ii] = RAW(y)[i % ny];
	    break;
	    
	default:
	error(_("incompatible types (from %s to %s) in array subset assignment"),
		  type2char(which%100), type2char(which/100));
	}
    next_i:
	;
	if (n > 1) {
	    j = 0;
	    while (++indx[j] >= bound[j]) {
		indx[j] = 0;
		j = (j + 1) % k;
	    }
	}
    }
    UNPROTECT(3);
    vmaxset(vmax);
    return x;
}


static SEXP SimpleListAssign(SEXP call, SEXP x, SEXP s, SEXP y)
{
    SEXP indx, xi, yi, yp;
    int i, ii, n, nx, ny, stretch=1;

    if (length(s) > 1)
	error(_("invalid number of subscripts to list assign"));

    PROTECT(indx = makeSubscript(x, CAR(s), &stretch));
    n = length(indx);

    /* The shallow copy here is so that */
    /* permuting a list's elements will work */

    if (isList(y) || isFrame(y) || isLanguage(y)) {
	PROTECT(y);
	ny = NAMED(y);
	yi = allocList(length(y));
	for (yp = yi; yp != R_NilValue; yp = CDR(yp)) {
	    SETCAR(yp, CAR(y));
	    SET_TAG(yp, TAG(y));
	    SET_NAMED(CAR(yp), ny | NAMED(CAR(y)));
	    y = CDR(y);
	}
	UNPROTECT(1);
	PROTECT(y = yi);
    }
    else PROTECT(y = CONS(y, R_NilValue));
    ny = length(y);
    nx = length(x);

    if (n > 0 && ny == 0)
	error(_("nothing to replace with"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    if (stretch) {
	yi = allocList(stretch - nx);
	PROTECT(x = listAppend(x, yi));
	nx = stretch;
    }
    else PROTECT(x);

    for (i = 0; i < n; i++) {
	ii = INTEGER(indx)[i];
	if (ii == NA_INTEGER) continue;
	ii = ii - 1;
	yi = nthcdr(y, i % ny);
	xi = nthcdr(x, ii % nx);
	if (NAMED(y) || NAMED(CAR(yi))) SETCAR(yi, duplicate(CAR(yi)));
	else SET_NAMED(CAR(yi), 1);
	SETCAR(xi, CAR(yi));
	if (TAG(yi) != R_NilValue)
	    SET_TAG(xi, TAG(yi));
    }
    UNPROTECT(3);
    return x;
}


static SEXP listRemove(SEXP x, SEXP s)
{
    SEXP a, pa, px;
    int i, ii, *ind, ns, nx, stretch=0;
    char *vmax;

    vmax = vmaxget();
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
	    SETCDR(pa, px);
	    px = CDR(px);
	    pa = CDR(pa);
	    SETCDR(pa, R_NilValue);
	}
	else {
	    px = CDR(px);
	}
    }
    SET_ATTRIB(CDR(a), ATTRIB(x));
    IS_S4_OBJECT(x) ?  SET_S4_OBJECT(CDR(a)) : UNSET_S4_OBJECT(CDR(a));
    SET_OBJECT(CDR(a), OBJECT(x));
    SET_NAMED(CDR(a), NAMED(x));
    UNPROTECT(2);
    vmaxset(vmax);
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
	    error(_("incorrect number of subscripts"));

	PROTECT(ax = allocArray(STRSXP, dims));
	for (px = x, i = 0; px != R_NilValue; px = CDR(px))
	    SET_STRING_ELT(ax, i++, CAR(px));
	setAttrib(ax, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
	if (isList(y)) {
	    ny = length(y);
	    PROTECT(ay = allocVector(STRSXP, ny));
	    for (py = y, i = 0; py != R_NilValue; py = CDR(py))
		SET_STRING_ELT(ay, i++, CAR(py));
	}
	else {
	    ny = 1;
	    PROTECT(ay = allocVector(STRSXP, 1));
	    SET_STRING_ELT(ay, 0, y);
	}
	if (nsubs == 2) ax = MatrixAssign(call, ax, subs, ay);
	else ax = ArrayAssign(call, ax, subs, ay);
	for (px = x, i = 0; px != R_NilValue; px = CDR(px))
	    CAR(px) = duplicate(STRING_ELT(ax, i++));
	UNPROTECT(2);
	break;
    }
    return x;
}
*/

static void SubAssignArgs(SEXP args, SEXP *x, SEXP *s, SEXP *y)
{
    SEXP p;
    if (length(args) < 2)
	error(_("SubAssignArgs: invalid number of arguments"));
    *x = CAR(args);
    if(length(args) == 2) {
	*s = R_NilValue;
	*y = CADR(args);
    }
    else {
	*s = p = CDR(args);
	while (CDDR(p) != R_NilValue)
	    p = CDR(p);
	*y = CADR(p);
	SETCDR(p, R_NilValue);
    }
}


/* The [<- operator.  "x" is the vector that is to be assigned into, */
/* y is the vector that is going to provide the new values and subs is */
/* the vector of subscripts that are going to be replaced. */
/* On entry (CAR(args)) and the last argument have been evaluated */
/* and the remainder of args have not.  If this was called directly */
/* the CAR(args) and the last arg won't have been. */

SEXP attribute_hidden do_subassign(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    /* This code performs an internal version of method dispatch. */
    /* We evaluate the first argument and attempt to dispatch on it. */
    /* If the dispatch fails, we "drop through" to the default code below. */

    if(DispatchOrEval(call, op, "[<-", args, rho, &ans, 0, 0))
      return(ans);

    return do_subassign_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP subs, x, y;
    int nsubs, oldtype; Rboolean S4;

    PROTECT(args);

    /* If there are multiple references to an object we must */
    /* duplicate it so that only the local version is mutated. */
    /* This will duplicate more often than necessary, but saves */
    /* over always duplicating. */
    /* FIXME: shouldn't x be protected?  It is (as args is)! */

    if (NAMED(CAR(args)) == 2)
	x = SETCAR(args, duplicate(CAR(args)));

    SubAssignArgs(args, &x, &subs, &y);
    S4 = IS_S4_OBJECT(x);
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
	else {
	    /* bug PR#2590 coerce only if null */
	    if(isNull(x)) PROTECT(x = coerceVector(x, TYPEOF(y)));
	    else PROTECT(x);
	}
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
    case RAWSXP:
	switch (nsubs) {
	case 0:
	    x = VectorAssign(call, x, R_MissingArg, y);
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
	error(_("object is not subsettable"));
	break;
    }

    if (oldtype == LANGSXP) {
	x = VectorToPairList(x);
	SET_TYPEOF(x, LANGSXP);
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    UNPROTECT(2);
    SET_NAMED(x, 0);
    if(S4) SET_S4_OBJECT(x);
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
		SET_VECTOR_ELT(y, k++, VECTOR_ELT(x, i));
	xnames = getAttrib(x, R_NamesSymbol);
	if (xnames != R_NilValue) {
	    PROTECT(ynames = allocVector(STRSXP, n - 1));
	    k = 0;
	    for (i = 0 ; i < n; i++)
		if(i != which)
		    SET_STRING_ELT(ynames, k++, STRING_ELT(xnames, i));
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

SEXP attribute_hidden do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    if(DispatchOrEval(call, op, "[[<-", args, rho, &ans, 0, 0))
      return(ans);

    return do_subassign2_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subassign2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP dims, indx, names, newname, subs, x, xtop, xup, y;
    int i, ndims, nsubs, offset, off = -1 /* -Wall */, stretch, which;
    Rboolean S4;

    PROTECT(args);

    SubAssignArgs(args, &x, &subs, &y);
    S4 = IS_S4_OBJECT(x);

    /* Handle NULL left-hand sides.  If the right-hand side */
    /* is NULL, just return the left-hand size otherwise, */
    /* convert to a zero length list (VECSXP). */

    if (isNull(x)) {
        if (isNull(y)) {
            UNPROTECT(1);
	    return x;
        }
        UNPROTECT(1);
	if (length(y) == 1)
	    PROTECT(x = allocVector(TYPEOF(y), 0));
	else
	    PROTECT(x = allocVector(VECSXP, 0));
    }

    /* Ensure that the LHS is a local variable. */
    /* If it is not, then make a local copy. */

    if (NAMED(x) == 2) {
	SETCAR(args, x = duplicate(x));
    }
    xtop = xup = x; /* x will be the element which is assigned to */

    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);
    nsubs = length(subs);

    /* ENVSXP special case first */
    if( TYPEOF(x) == ENVSXP) {
      if( nsubs!=1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	error(_("wrong args for environment subassignment"));
      defineVar(install(translateChar(STRING_ELT(CAR(subs),0))), y, x);
      UNPROTECT(1);
      return(x);
    }

    stretch = 0;
    if (isVector(x)) {
	Rboolean recursed = FALSE;
	if (!isVectorList(x) && LENGTH(y) > 1)
	    error(_("more elements supplied than there are to replace"));
	if (nsubs == 0 || CAR(subs) == R_MissingArg)
	    error(_("[[ ]] with missing subscript"));
	if (nsubs == 1) {
	    SEXP thesub = CAR(subs);
	    int i = -1, len = length(thesub);
	    /* new case in 1.7.0, one vector index for a list */
	    if(isVectorList(x) && length(thesub) > 1) {
		for(i = 0; i < len - 1; i++) {
		    if(LENGTH(x) == 0 || !isVectorList(x))
			error(_("recursive indexing failed at level %d\n"), 
			      i+1);
		    off = get1index(CAR(subs), getAttrib(x, R_NamesSymbol),
				    length(x), /*partial ok*/TRUE, i);
		    if(off < 0 || off >= LENGTH(x))
			error(_("no such index at level %d\n"), i+1);
		    xup = x;
		    recursed = TRUE;
		    x = VECTOR_ELT(x, off);
		}
	    }
	    if (recursed && !isVectorList(x) && LENGTH(y) > 1)
		error(_("more elements supplied than there are to replace"));
	    offset = OneIndex(x, thesub, length(x), 0, &newname, i);
	    if (isVectorList(x) && isNull(y)) {
		x = DeleteOneVectorListItem(x, offset);
		if(recursed) SET_VECTOR_ELT(xup, off, x);
		else xtop = x;
		UNPROTECT(1);
		return xtop;
	    }
	    if (offset < 0)
		error(_("[[ ]] subscript out of bounds"));
	    if (offset >= LENGTH(x))
		    stretch = offset + 1;
	}
	else {
	    if (ndims != nsubs)
		error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = allocVector(INTSXP, ndims));
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(indx)[i] = get1index(CAR(subs), isNull(names) ?
					      R_NilValue : VECTOR_ELT(names, i),
					      INTEGER(dims)[i],
					      /*partial ok*/FALSE, -1);
		subs = CDR(subs);
		if (INTEGER(indx)[i] < 0 ||
		    INTEGER(indx)[i] >= INTEGER(dims)[i])
		    error(_("[[ ]] subscript out of bounds"));
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	    offset += INTEGER(indx)[0];
	    UNPROTECT(1);
	}

	which = SubassignTypeFix(&x, &y, stretch, 2, call);
	PROTECT(x);

	switch (which) {
	    /* as from 2.3.0 'which' is after conversion */

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
        /* case 1013:	   logical   <- integer	  */
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
        /* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
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

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[offset] = COMPLEX(y)[0];
	    break;

	case 1610:	/* character <- logical	  */
	case 1613:	/* character <- integer	  */
	case 1614:	/* character <- real	  */
	case 1615:	/* character <- complex	  */
	case 1616:	/* character <- character */
	/* case 1016:	   logical   <- character */
	/* case 1316:	   integer   <- character */
	/* case 1416:	   real	     <- character */
	/* case 1516:	   complex   <- character */

	    SET_STRING_ELT(x, offset, STRING_ELT(y, 0));
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
#ifdef BYTECODE
	case 1921:  /* vector     <- bytecode   */
#endif
	case 1922:  /* vector     <- external pointer */
	case 1923:  /* vector     <- weak reference */
	case 1924:  /* vector     <- raw */
	case 1925:  /* vector     <- S4 */
	case 1903: case 1907: case 1908: case 1999: /* functions */

	    /* drop through: vectors and expressions are treated the same */

	case 2001:	/* expression <- symbol	    */
	case 2006:	/* expression <- language   */
	case 2010:	/* expression <- logical    */
	case 2013:	/* expression <- integer    */
	case 2014:	/* expression <- real	    */
	case 2015:	/* expression <- complex    */
	case 2016:	/* expression <- character  */
	case 2024:  /* expression     <- raw */
	case 2025:  /* expression     <- S4 */
	case 1919:      /* vector     <- vector     */
	case 2020:	/* expression <- expression */

            if( NAMED(y) ) y = duplicate(y);
	    SET_VECTOR_ELT(x, offset, y);
	    break;

	case 2424:      /* raw <- raw */

           RAW(x)[offset] = RAW(y)[0];
           break;

	default:
	    error(_("incompatible types (from %s to %s) in [[ assignment"),
		  type2char(which%100), type2char(which/100));
	}
	/* If we stretched, we may have a new name. */
	/* In this case we must create a names attribute */
	/* (if it doesn't already exist) and set the new */
	/* value in the names attribute. */
	if (stretch && newname != R_NilValue) {
	    names = getAttrib(x, R_NamesSymbol);
	    if (names == R_NilValue) {
		PROTECT(names = allocVector(STRSXP, length(x)));
		SET_STRING_ELT(names, offset, newname);
		setAttrib(x, R_NamesSymbol, names);
		UNPROTECT(1);
	    }
	    else
		SET_STRING_ELT(names, offset, newname);
	}
	if(recursed) SET_VECTOR_ELT(xup, off, x);
	else xtop = x;
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
		error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = allocVector(INTSXP, ndims));
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		INTEGER(indx)[i] = get1index(CAR(subs), CAR(names),
					     INTEGER(dims)[i],
					     /*partial ok*/FALSE, -1);
		subs = CDR(subs);
		if (INTEGER(indx)[i] < 0 ||
		    INTEGER(indx)[i] >= INTEGER(dims)[i])
		    error(_("[[ ]] subscript (%d) out of bounds"), i+1);
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + INTEGER(indx)[i]) * INTEGER(dims)[i - 1];
	    offset += INTEGER(indx)[0];
	    SETCAR(nthcdr(x, offset), duplicate(y));
	    UNPROTECT(1);
	}
	xtop = x;
	UNPROTECT(1);
    }
    else error(_("object is not subsettable"));

    UNPROTECT(1);
    SET_NAMED(xtop, 0);
    if(S4) SET_S4_OBJECT(xtop);
    return xtop;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string
*/
SEXP attribute_hidden do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP nlist, ans, input;
    int iS;

    checkArity(op, args);

    /* Note the RHS has already been evaluated at this point */

    input = allocVector(STRSXP, 1);

    nlist = CADR(args);
    iS = isSymbol(nlist);
    if (iS)
	SET_STRING_ELT(input, 0, PRINTNAME(nlist));
    else if(isString(nlist) )
	SET_STRING_ELT(input, 0, STRING_ELT(nlist, 0));
    else {
	error(_("invalid subscript type"));
	return R_NilValue; /*-Wall*/
    }

    /* replace the second argument with a string */
    SETCADR(args, input);

    if(DispatchOrEval(call, op, "$<-", args, env, &ans, 0, 0))
      return(ans);

    if (! iS)
	nlist = install(translateChar(STRING_ELT(input, 0)));

    return R_subassign3_dflt(call, CAR(ans), nlist, CADDR(ans));
}

/* used in methods_list_dispatch.c */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val)
{
    SEXP t;
    PROTECT_INDEX pvalidx, pxidx;
    Rboolean maybe_duplicate=FALSE;
    Rboolean S4;

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = IS_S4_OBJECT(x);

    if (NAMED(x) == 2)
	REPROTECT(x = duplicate(x), pxidx);

    /* If we aren't creating a new entry and NAMED>0
       we need to duplicate to prevent cycles.
       If we are creating a new entry we could duplicate
       or increase NAMED. We duplicate if NAMED==1, but
       not if NAMED==2 */
    if (NAMED(val) == 2)
	maybe_duplicate=TRUE;
    else if (NAMED(val)==1)
	REPROTECT(val = duplicate(val), pvalidx);

    if ((isList(x) || isLanguage(x)) && !isNull(x)) {
	if (TAG(x) == nlist) {
	    if (val == R_NilValue) {
		SET_ATTRIB(CDR(x), ATTRIB(x));
		IS_S4_OBJECT(x) ?  SET_S4_OBJECT(CDR(x)) : UNSET_S4_OBJECT(CDR(x));
		SET_OBJECT(CDR(x), OBJECT(x));
		SET_NAMED(CDR(x), NAMED(x));
		x = CDR(x);
	    }
	    else
		SETCAR(x, val);
	}
	else {
	    for (t = x; t != R_NilValue; t = CDR(t))
		if (TAG(CDR(t)) == nlist) {
		    if (val == R_NilValue)
			SETCDR(t, CDDR(t));
		    else
			SETCAR(CDR(t), val);
		    break;
		}
		else if (CDR(t) == R_NilValue && val != R_NilValue) {
		    SETCDR(t, allocSExp(LISTSXP));
		    SET_TAG(CDR(t), nlist);
		    SETCADR(t, val);
		    break;
		}
	}
	if (x == R_NilValue && val != R_NilValue) {
	    x = allocList(1);
	    SETCAR(x, val);
	    SET_TAG(x, nlist);
	}
    }
    /* cannot use isEnvironment since we do not want NULL here */
    else if( TYPEOF(x) == ENVSXP ) {
      defineVar(nlist, val, x);
    }
    else {
	int i, imatch, nx;
	SEXP names;

	if (!(isNewList(x) || isExpression(x))) {
	    warning(_("Coercing LHS to a list"));
	    REPROTECT(x = coerceVector(x, VECSXP), pxidx);
	}
	names = getAttrib(x, R_NamesSymbol);
	nx = length(x);
	nlist = PRINTNAME(nlist);
	if (isNull(val)) {
	    /* If "val" is NULL, this is an element deletion */
	    /* if there is a match to "nlist" otherwise "x" */
	    /* is unchanged.  The attributes need adjustment. */
	    if (names != R_NilValue) {
		imatch = -1;
		for (i = 0; i < nx; i++)
		    if (NonNullStringMatch(STRING_ELT(names, i), nlist)) {
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
			    SET_VECTOR_ELT(ans, ii, VECTOR_ELT(x, i));
			    SET_STRING_ELT(ansnames, ii, STRING_ELT(names, i));
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
		    if (NonNullStringMatch(STRING_ELT(names, i), nlist)) {
			imatch = i;
			break;
		    }
	    }
	    if (imatch >= 0) {
		/* We are just replacing an element */
		if (maybe_duplicate)
		    REPROTECT(val = duplicate(val), pvalidx);
		SET_VECTOR_ELT(x, imatch, val);
	    }
	    else {
		/* We are introducing a new element. */
		/* Enlarge the list, add the new element */
		/* and finally, adjust the attributes. */
		SEXP ans, ansnames;
		PROTECT(ans = allocVector(VECSXP, nx + 1));
		PROTECT(ansnames = allocVector(STRSXP, nx + 1));
		for (i = 0; i < nx; i++)
		    SET_VECTOR_ELT(ans, i, VECTOR_ELT(x, i));
		if (isNull(names)) {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, R_BlankString);
		}
		else {
		    for (i = 0; i < nx; i++)
			SET_STRING_ELT(ansnames, i, STRING_ELT(names, i));
		}
		SET_VECTOR_ELT(ans, nx, val);
		SET_STRING_ELT(ansnames, nx,  nlist);
		setAttrib(ans, R_NamesSymbol, ansnames);
		copyMostAttrib(x, ans);
		UNPROTECT(2);
		x = ans;
	    }
	}
    }
    UNPROTECT(2);
    SET_NAMED(x, 0);
    if(S4) SET_S4_OBJECT(x);
    return x;
}
