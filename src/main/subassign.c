/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2017   The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*		Warnings/Errors

    In this file we generally do not make use of the call, as it
    will be something like `[<-`(`*tmp`, ...) and that just confuses
    the user.  The call that is deduced from the context is generally
    much clearer.
 */

/*
 *
 *  Subset Mutation for Lists and Vectors
 *
 *  The following table shows the codes which have been assigned to the
 *  type combinations in assignments of the form "x[s] <- y".  Here the
 *  type of y is given across the top of the table and the type of x at
 *  the side.  (Note: the lack of 11 and 12 indices here is due to the
 *  removal of built-in factors).
 *
 *  NB these tables are out of date, and exclude types 21, 22, 23, 24 ...
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

#include <Defn.h>
#include <Internal.h>
#include <R_ext/RS.h> /* for test of S4 objects */
#include <R_ext/Itermacros.h>

/* The SET_STDVEC_LENGTH macro is used to modify the length of
   growable vectors. This would need to change to allow ALTREP vectors to
   grow in place.

   SETLENGTH is used when checking the write barrier.
   Always using SETLENGTH would be OK but maybe a little less efficient. */
#ifndef SET_STDVEC_LENGTH
# define SET_STDVEC_LENGTH(x, v) SETLENGTH(x, v)
#endif

/* This version of SET_VECTOR_ELT does not increment the REFCNT for
   the new vector->element link. It assumes that the old vector is
   becoming garbage and so it's references become no longer
   accessible. */
static R_INLINE void SET_VECTOR_ELT_NR(SEXP x, R_xlen_t i, SEXP v)
{
#ifdef COMPUTE_REFCNT_VALUES
    int ref = REFCNT(v);
    SET_VECTOR_ELT(x, i, v);
    SET_REFCNT(v, ref);
#else
    SET_VECTOR_ELT(x, i, v);
#endif
}

static R_INLINE SEXP getNames(SEXP x)
{
    /* defer to getAttrib if a 'dim' attribute is present */
    for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr))
	if (TAG(attr) == R_DimSymbol)
	    return getAttrib(x, R_NamesSymbol);

    /* don't use getAttrib since that would mark as immutable */
    for (SEXP attr = ATTRIB(x); attr != R_NilValue; attr = CDR(attr))
	if (TAG(attr) == R_NamesSymbol)
	    return CAR(attr);

    return R_NilValue;
}

/* EnlargeVector() takes a vector "x" and changes its length to "newlen".
   This allows to assign values "past the end" of the vector or list.
   Overcommit by a small percentage to allow more efficient vector growth.
*/
static SEXP EnlargeNames(SEXP, R_xlen_t, R_xlen_t);

static SEXP EnlargeVector(SEXP x, R_xlen_t newlen)
{
    R_xlen_t len, newtruelen;
    SEXP newx, names;
    static SEXP R_CheckBoundsSymbol = NULL;

    if (R_CheckBoundsSymbol == NULL)
	R_CheckBoundsSymbol = install("check.bounds");

    /* Sanity Checks */
    if (!isVector(x))
	error(_("attempt to enlarge non-vector"));

    /* Enlarge the vector itself. */
    len = xlength(x);
    if (LOGICAL(GetOption1(R_CheckBoundsSymbol))[0])
	warning(_("assignment outside vector/list limits (extending from %d to %d)"),
		len, newlen);

    /* if the vector is not shared, is growable. and has room, then
       increase its length */
    if (! MAYBE_SHARED(x) &&
	IS_GROWABLE(x) &&
	XTRUELENGTH(x) >= newlen) {
	SET_STDVEC_LENGTH(x, newlen);
	names = getNames(x);
	if (!isNull(names)) {
	    SEXP newnames = EnlargeNames(names, len, newlen);
	    if (names != newnames)
		setAttrib(x, R_NamesSymbol, newnames);
	}
	return x;
    }

    /* over-committing by 5% seems to be reasonable, but for
       experimenting the environment variable R_EXPAND_Frac can be
       used to adjust this */
    static double expand_dflt = 1.05;
    static double expand = 0;
    if (expand == 0) {
	char *envval = getenv("R_EXPAND_FRAC");
	expand = envval != NULL ? atof(envval) : expand_dflt;
	if (expand < 1 || expand > 2) {
	    expand = expand_dflt;
	    error("bad expand value");
	}
    }

    if (newlen > len) {
	double expanded_nlen = newlen * expand;
	if (expanded_nlen <= R_XLEN_T_MAX)
	    newtruelen = (R_xlen_t) expanded_nlen;
	else
	    newtruelen = newlen;
    }
    else
	/* sometimes this is called when no expansion is needed */
	newtruelen = newlen;

    /**** for now, don't cross the long vector boundary; drop when
	  ALTREP is merged */
/*
#ifdef ALTREP
#error drop the limitation to short vectors
#endif

    if (newtruelen > R_LEN_T_MAX) newtruelen = newlen;
*/
    PROTECT(x);
    PROTECT(newx = allocVector(TYPEOF(x), newtruelen));

    /* Copy the elements into place. */
    switch(TYPEOF(x)) {
    case LGLSXP:
    case INTSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    INTEGER0(newx)[i] = INTEGER_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    INTEGER0(newx)[i] = NA_INTEGER;
	break;
    case REALSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    REAL0(newx)[i] = REAL_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    REAL0(newx)[i] = NA_REAL;
	break;
    case CPLXSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    COMPLEX0(newx)[i] = COMPLEX_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++) {
	    COMPLEX0(newx)[i].r = NA_REAL;
	    COMPLEX0(newx)[i].i = NA_REAL;
	}
	break;
    case STRSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_STRING_ELT(newx, i, STRING_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_STRING_ELT(newx, i, NA_STRING); /* was R_BlankString  < 1.6.0 */
	break;
    case EXPRSXP:
    case VECSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    SET_VECTOR_ELT_NR(newx, i, VECTOR_ELT(x, i));
	for (R_xlen_t i = len; i < newtruelen; i++)
	    SET_VECTOR_ELT(newx, i, R_NilValue);
	break;
    case RAWSXP:
	for (R_xlen_t i = 0; i < len; i++)
	    RAW0(newx)[i] = RAW_ELT(x, i);
	for (R_xlen_t i = len; i < newtruelen; i++)
	    RAW0(newx)[i] = (Rbyte) 0;
	break;
    default:
	UNIMPLEMENTED_TYPE("EnlargeVector", x);
    }
    if (newlen < newtruelen) {
	SET_GROWABLE_BIT(newx);
	SET_TRUELENGTH(newx, newtruelen);
	SET_STDVEC_LENGTH(newx, newlen);
    }

    /* Adjust the attribute list. */
    names = getNames(x);
    if (!isNull(names))
	setAttrib(newx, R_NamesSymbol, EnlargeNames(names, len, newlen));
    copyMostAttrib(x, newx);
    UNPROTECT(2);
    return newx;
}

static SEXP EnlargeNames(SEXP names, R_xlen_t len, R_xlen_t newlen)
{
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != len)
	error(_("bad names attribute"));
    SEXP newnames = PROTECT(EnlargeVector(names, newlen));
    for (R_xlen_t i = len; i < newlen; i++)
	SET_STRING_ELT(newnames, i, R_BlankString);
    UNPROTECT(1);
    return newnames;
}

/* used instead of coerceVector to embed a non-vector in a list for
   purposes of SubassignTypeFix, for cases in wich coerceVector should
   fail; namely, S4SXP */
static SEXP embedInVector(SEXP v, SEXP call)
{
    SEXP ans;
    warningcall(call, "implicit list embedding of S4 objects is deprecated");
    PROTECT(ans = allocVector(VECSXP, 1));
    SET_VECTOR_ELT(ans, 0, v);
    UNPROTECT(1);
    return (ans);
}

static Rboolean dispatch_asvector(SEXP *x, SEXP call, SEXP rho) {
    static SEXP op = NULL;
    SEXP args;
    Rboolean ans;
    if (op == NULL)
        op = INTERNAL(install("as.vector"));
    PROTECT(args = list2(*x, mkString("any")));
    ans = DispatchOrEval(call, op, "as.vector", args, rho, x, 0, 1);
    UNPROTECT(1);
    return ans;
}

/* Level 1 is used in VectorAssign, MatrixAssign, ArrayAssign.
   That coerces RHS to a list or expression.

   Level 2 is used in do_subassign2_dflt.
   This does not coerce when assigning into a list.
*/

static int SubassignTypeFix(SEXP *x, SEXP *y, R_xlen_t stretch, int level,
			    SEXP call, SEXP rho)
{
    /* A rather pointless optimization, but level 2 used to be handled
       differently */
    Rboolean redo_which = TRUE;
    int which = 100 * TYPEOF(*x) + TYPEOF(*y);
    /* coercion can lose the object bit */
    Rboolean x_is_object = OBJECT(*x);

    switch (which) {
    case 1000:	/* logical    <- null       */
    case 1300:	/* integer    <- null       */
    case 1400:	/* real	      <- null       */
    case 1500:	/* complex    <- null       */
    case 1600:	/* character  <- null       */
    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */
    case 2400:	/* raw        <- null       */

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
    case 1902:	/* vector     <- pairlist */
    case 1904:  /* vector     <- environment   */
    case 1905:  /* vector     <- promise   */
    case 1906:  /* vector     <- language   */
    case 1910:  /* vector     <- logical    */
    case 1913:  /* vector     <- integer    */
    case 1914:  /* vector     <- real       */
    case 1915:  /* vector     <- complex    */
    case 1916:  /* vector     <- character  */
    case 1920:  /* vector     <- expression  */
    case 1921:  /* vector     <- bytecode   */
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
	    *y = embedInVector(*y, call);
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
    case 2002:  /* expression <- pairlist   */
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
	    *y = embedInVector(*y, call);
	} else {
	    /* Nothing to do here: duplicate when used (if needed) */
	    redo_which = FALSE;
	}
	break;

    case 1025: /* logical   <- S4 */
    case 1325: /* integer   <- S4 */
    case 1425: /* real      <- S4 */
    case 1525: /* complex   <- S4 */
    case 1625: /* character <- S4 */
    case 2425: /* raw       <- S4 */
        if (dispatch_asvector(y, call, rho)) {
            return SubassignTypeFix(x, y, stretch, level, call, rho);
        }

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

#ifdef LONG_VECTOR_SUPPORT
static R_INLINE R_xlen_t gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL_ELT(indx, i);
	return R_FINITE(d) ? (R_xlen_t) d : NA_INTEGER;
    } else
	return INTEGER_ELT(indx, i);
}
#else
#define R_SHORT_LEN_MAX INT_MAX
static R_INLINE int gi(SEXP indx, R_xlen_t i)
{
    if (TYPEOF(indx) == REALSXP) {
	double d = REAL_ELT(indx, i);
	if (!R_FINITE(d) || d < -R_SHORT_LEN_MAX || d > R_SHORT_LEN_MAX) return NA_INTEGER;
	return (int) d;
    } else
	return INTEGER_ELT(indx, i);
}
#endif

static SEXP DeleteListElements(SEXP x, SEXP which)
{
    SEXP include, xnew, xnames, xnewnames;
    R_xlen_t i, ii, len, lenw;
    len = xlength(x);
    lenw = xlength(which);
    /* calculate the length of the result */
    PROTECT(include = allocVector(INTSXP, len));
    int *pinclude = INTEGER0(include);
    for (i = 0; i < len; i++)
	pinclude[i] = 1;
    for (i = 0; i < lenw; i++) {
	ii = gi(which, i);
	if (0 < ii  && ii <= len)
	    pinclude[ii - 1] = 0;
    }
    ii = 0;
    for (i = 0; i < len; i++)
	ii += pinclude[i];
    if (ii == len) {
	UNPROTECT(1);
	return x;
    }
    PROTECT(xnew = allocVector(TYPEOF(x), ii));
    ii = 0;
    for (i = 0; i < len; i++) {
	if (pinclude[i] == 1) {
	    SET_VECTOR_ELT(xnew, ii, VECTOR_ELT(x, i));
	    ii++;
	}
    }
    xnames = getAttrib(x, R_NamesSymbol);
    if (xnames != R_NilValue) {
	PROTECT(xnewnames = allocVector(STRSXP, ii));
	ii = 0;
	for (i = 0; i < len; i++) {
	    if (pinclude[i] == 1) {
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

static R_INLINE SEXP VECTOR_ELT_FIX_NAMED(SEXP y, R_xlen_t i) {
    /* if RHS (container or element) has NAMED > 0 set NAMED = NAMEDMAX.
       Duplicating might be safer/more consistent (PR15098) */
    SEXP val = VECTOR_ELT(y, i);
    if ((NAMED(y) || NAMED(val)))
	ENSURE_NAMEDMAX(val);
    return val;
}

#define VECTOR_ASSIGN_LOOP(CODE) do {			\
	if (TYPEOF(indx) == INTSXP) {			\
	    const int *pindx = INTEGER_RO(indx);	\
	    MOD_ITERATE1(n, ny, i, iny, {		\
		    ii = pindx[i];			\
		    if (ii == NA_INTEGER) continue;	\
		    ii = ii - 1;			\
		    do { CODE } while (0);		\
		});					\
	}						\
	else /* could specialize this to REALSXP case */\
	    MOD_ITERATE1(n, ny, i, iny, {		\
		    ii = gi(indx, i);			\
		    if (ii == NA_INTEGER) continue;	\
		    ii = ii - 1;			\
		    do { CODE } while (0);		\
		});					\
    } while (0)

/**** This could use SET_REAL_ELT and such, but would also have to
      change byte code instructions in eval.c */

static SEXP VectorAssign(SEXP call, SEXP rho, SEXP x, SEXP s, SEXP y)
{
    SEXP indx, newnames;
    R_xlen_t i, ii, n, nx, ny, iny;
    int which;
    R_xlen_t stretch;

    /* try for quick return for simple scalar case */
    if (ATTRIB(s) == R_NilValue) {
	if (TYPEOF(x) == REALSXP && IS_SCALAR(y, REALSXP)) {
	    if (IS_SCALAR(s, INTSXP)) {
		R_xlen_t ival = SCALAR_IVAL(s);
		if (1 <= ival && ival <= XLENGTH(x)) {
		    REAL(x)[ival - 1] = SCALAR_DVAL(y);
		    return x;
		}
	    }
	    else if (IS_SCALAR(s, REALSXP)) {
		double dval = SCALAR_DVAL(s);
		if (R_FINITE(dval)) {
		    R_xlen_t ival = (R_xlen_t) dval;
		    if (1 <= ival && ival <= XLENGTH(x)) {
			REAL(x)[ival - 1] = SCALAR_DVAL(y);
			return x;
		    }
		}
	    }
	}
    }

    if (isNull(x) && isNull(y)) {
	return R_NilValue;
    }

    /* Check to see if we have special matrix subscripting. */
    /* If so, we manufacture a real subscript vector. */

    PROTECT(s);
    if (ATTRIB(s) != R_NilValue) { /* pretest to speed up simple case */
	SEXP dim = getAttrib(x, R_DimSymbol);
	if (isMatrix(s) && isArray(x) && ncols(s) == length(dim)) {
	    if (isString(s)) {
		SEXP dnames = PROTECT(GetArrayDimnames(x));
		s = strmat2intmat(s, dnames, call);
		UNPROTECT(2); /* dnames, s */
		PROTECT(s);
	    }
	    if (isInteger(s) || isReal(s)) {
		s = mat2indsub(dim, s, R_NilValue);
		UNPROTECT(1);
		PROTECT(s);
	    }
	}
    }

    stretch = 1;
    PROTECT(indx = makeSubscript(x, s, &stretch, R_NilValue));
    n = xlength(indx);
    if(xlength(y) > 1)
	for(i = 0; i < n; i++)
	    if(gi(indx, i) == NA_INTEGER)
		error(_("NAs are not allowed in subscripted assignments"));

    /* Here we make sure that the LHS has */
    /* been coerced into a form which can */
    /* accept elements from the RHS. */
    which = SubassignTypeFix(&x, &y, stretch, 1, call, rho);
    /* = 100 * TYPEOF(x) + TYPEOF(y);*/
    if (n == 0) {
	UNPROTECT(2);
	return x;
    }
    ny = xlength(y);
    nx = xlength(x);

    PROTECT(x);

    if ((TYPEOF(x) != VECSXP && TYPEOF(x) != EXPRSXP) || y != R_NilValue) {
	if (n > 0 && ny == 0)
	    error(_("replacement has length zero"));
	if (n > 0 && n % ny)
	    warning(_("number of items to replace is not a multiple of replacement length"));
    }


    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = shallow_duplicate(y));
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

	{
	    int *px = INTEGER(x);
	    VECTOR_ASSIGN_LOOP(px[ii] = INTEGER_ELT(y, iny););
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	{
	    double *px = REAL(x);
	    VECTOR_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, iny);
		    if (iy == NA_INTEGER)
			px[ii] = NA_REAL;
		    else
			px[ii] = iy;
		});
	}
	break;

    /* case 1014:  logical   <- real	  */
    /* case 1314:  integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	{
	    double *px = REAL(x);
	    VECTOR_ASSIGN_LOOP(px[ii] = REAL_ELT(y, iny););
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    VECTOR_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, iny);
		    if (iy == NA_INTEGER) {
			px[ii].r = NA_REAL;
			px[ii].i = NA_REAL;
		    }
		    else {
			px[ii].r = iy;
			px[ii].i = 0.0;
		    }
		});
	}
	break;

    case 1514:	/* complex   <- real	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    VECTOR_ASSIGN_LOOP({
		    double ry = REAL_ELT(y, iny);
		    if (ISNA(ry)) {
			px[ii].r = NA_REAL;
			px[ii].i = NA_REAL;
		    }
		    else {
			px[ii].r = ry;
			px[ii].i = 0.0;
		    }
		});
	}
	break;

    /* case 1015:  logical   <- complex	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1415:  real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    VECTOR_ASSIGN_LOOP(px[ii] = COMPLEX_ELT(y, iny););
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

	VECTOR_ASSIGN_LOOP(SET_STRING_ELT(x, ii, STRING_ELT(y, iny)););
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

	VECTOR_ASSIGN_LOOP({

		/* set NAMED on RHS value to NAMEDMAX if used more than once
		   (PR15098) */
		if (i >= ny)
		    ENSURE_NAMEDMAX(VECTOR_ELT(y, iny));

		SET_VECTOR_ELT(x, ii, VECTOR_ELT_FIX_NAMED(y, iny));
	    });
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

	VECTOR_ASSIGN_LOOP(SET_VECTOR_ELT(x, ii, VECTOR_ELT(y, iny)););
	break;

    case 1900:  /* vector     <- null       */
    case 2000:  /* expression <- null       */

	x = DeleteListElements(x, indx);
	UNPROTECT(4);
	return x;
	break;

    case 2424:	/* raw   <- raw	  */

	{
	    Rbyte *px = RAW(x);
	    VECTOR_ASSIGN_LOOP(px[ii] = RAW_ELT(y, iny););
	}
	break;

    default:
	warningcall(call, "sub assignment (*[*] <- *) not done; __bug?__");
    }
    /* Check for additional named elements. */
    /* Note makeSubscript passes the additional names back as the use.names
       attribute (a vector list) of the generated subscript vector */
    newnames = getAttrib(indx, R_UseNamesSymbol);
    if (newnames != R_NilValue) {
	SEXP oldnames = getAttrib(x, R_NamesSymbol);
	if (oldnames != R_NilValue) {
	    for (i = 0; i < n; i++) {
		if (VECTOR_ELT(newnames, i) != R_NilValue) {
		    ii = gi(indx, i);
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
		}
	    }
	}
	else {
	    PROTECT(oldnames = allocVector(STRSXP, nx));
	    for (i = 0; i < nx; i++)
		SET_STRING_ELT(oldnames, i, R_BlankString);
	    for (i = 0; i < n; i++) {
		if (VECTOR_ELT(newnames, i) != R_NilValue) {
		    ii = gi(indx, i);
		    if (ii == NA_INTEGER) continue;
		    ii = ii - 1;
		    SET_STRING_ELT(oldnames, ii, VECTOR_ELT(newnames, i));
		}
	    }
	    setAttrib(x, R_NamesSymbol, oldnames);
	    UNPROTECT(1);
	}
    }
    UNPROTECT(4);
    return x;
}

SEXP int_arraySubscript(int dim, SEXP s, SEXP dims, SEXP x, SEXP call);

#define MATRIX_ASSIGN_LOOP(CODE) do {			\
	R_xlen_t k = 0, NR = nr, ij;			\
	if (anyIdxNA)					\
	    for (int j = 0; j < ncs; j++) {		\
		int jj = psc[j];			\
		if (jj != NA_INTEGER) {			\
		    jj = jj - 1;			\
		    R_xlen_t offset = jj * NR;		\
		    for (int i = 0; i < nrs; i++) {	\
			int ii = psr[i];		\
			if (ii != NA_INTEGER) {		\
			    ij = ii + offset - 1;	\
			    do { CODE } while (0);	\
			    k++;			\
			    if (k == ny) k = 0;		\
			}				\
		    }					\
		}					\
	    }						\
	else						\
	    for (int j = 0; j < ncs; j++) {		\
		int jj = psc[j] - 1;			\
		R_xlen_t offset = jj * NR;		\
		for (int i = 0; i < nrs; i++) {		\
		    int ii = psr[i];			\
		    ij = ii + offset - 1;		\
		    do { CODE } while (0);		\
		    k++;				\
		    if (k == ny) k = 0;			\
		}					\
	    }						\
    } while (0)

static SEXP MatrixAssign(SEXP call, SEXP rho, SEXP x, SEXP s, SEXP y)
{
    int which;
    int nrs, ncs;
    SEXP sr, sc, dim;

    if (!isMatrix(x))
	error(_("incorrect number of subscripts on matrix"));

    int nr = nrows(x);
    R_xlen_t ny = XLENGTH(y);

    /* Note that "s" has been protected. */
    /* No GC problems here. */

    dim = getAttrib(x, R_DimSymbol);
    sr = SETCAR(s, int_arraySubscript(0, CAR(s), dim, x, call));
    sc = SETCADR(s, int_arraySubscript(1, CADR(s), dim, x, call));
    nrs = LENGTH(sr);
    ncs = LENGTH(sc);

    const int *psc = INTEGER_RO(sc);
    const int *psr = INTEGER_RO(sr);
    int anyIdxNA = FALSE;
    for(int i = 0; i < nrs; i++)
	if (psr[i] == NA_INTEGER) {
	    anyIdxNA = TRUE;
	    break;
	}
    for(int i = 0; i < ncs; i++)
	if (psc[i] == NA_INTEGER) {
	    anyIdxNA = TRUE;
	    break;
	}
    if(ny > 1 && anyIdxNA)
	error(_("NAs are not allowed in subscripted assignments"));

    R_xlen_t n = ((R_xlen_t) nrs) * ncs;

    /* <TSL> 21Oct97
       if (length(y) == 0)
       error("Replacement length is zero");
       </TSL>  */

    if (n > 0 && ny == 0)
	error(_("replacement has length zero"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    which = SubassignTypeFix(&x, &y, 0, 1, call, rho);
    if (n == 0) return x;

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = shallow_duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are permanent. */
    /* Beware! */

    switch (which) {
	/* because we have called SubassignTypeFix the commented
	   values cannot occur (and would be unsafe) */

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
    /* case 1013: logical   <- integer	  */
    case 1313:	/* integer   <- integer	  */

	{
	    int *px = INTEGER(x);
	    if (ALTREP(y))
		MATRIX_ASSIGN_LOOP(px[ij] = INTEGER_ELT(y, k););
	    else {
		int *py = INTEGER0(y);
		MATRIX_ASSIGN_LOOP(px[ij] = py[k];);
	    }
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	{
	    double *px = REAL(x);
	    MATRIX_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, k);
		    if (iy == NA_INTEGER)
			px[ij] = NA_REAL;
		    else
			px[ij] = iy;
		});
	}
	break;

    /* case 1014:  logical   <- real	  */
    /* case 1314:  integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	{
	    double *px = REAL(x);
	    if (ALTREP(y))
		MATRIX_ASSIGN_LOOP(px[ij] = REAL_ELT(y, k););
	    else {
		double *py = REAL0(y);
		MATRIX_ASSIGN_LOOP(px[ij] = py[k];);
	    }
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    MATRIX_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, k);
		    if (iy == NA_INTEGER) {
			px[ij].r = NA_REAL;
			px[ij].i = NA_REAL;
		    }
		    else {
			px[ij].r = iy;
			px[ij].i = 0.0;
		    }
		});
	}
	break;

    case 1514:	/* complex   <- real	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    MATRIX_ASSIGN_LOOP({
		    double ry = REAL_ELT(y, k);
		    if (ISNA(ry)) {
			px[ij].r = NA_REAL;
			px[ij].i = NA_REAL;
		    }
		    else {
			px[ij].r = ry;
			px[ij].i = 0.0;
		    }
		});
	}
	break;

    /* case 1015:  logical   <- complex	  */
    /* case 1315:  integer   <- complex	  */
    /* case 1415:  real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    MATRIX_ASSIGN_LOOP(px[ij] = COMPLEX_ELT(y, k););
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

	MATRIX_ASSIGN_LOOP(SET_STRING_ELT(x, ij, STRING_ELT(y, k)););
	break;

    case 1919: /* vector <- vector */

	/* set NAMED or RHS values to NAMEDMAX if they might be used more
	   than once (PR15098)*/
	if (ny < ncs * nrs)
	    for (R_xlen_t i = 0; i < ny; i++)
		ENSURE_NAMEDMAX(VECTOR_ELT(y, i));

	MATRIX_ASSIGN_LOOP(SET_VECTOR_ELT(x, ij, VECTOR_ELT_FIX_NAMED(y, k)););
	break;

    case 2424: /* raw   <- raw   */

	{
	    Rbyte *px = RAW(x);
	    MATRIX_ASSIGN_LOOP(px[ij] = RAW_ELT(y, k););
	}
	break;

    default:
	error(_("incompatible types (from %s to %s) in matrix subset assignment"),
		  type2char(which%100), type2char(which/100));
    }
    UNPROTECT(2);
    return x;
}

#define ARRAY_ASSIGN_LOOP(CODE) do {			\
	R_xlen_t i, iny;				\
	MOD_ITERATE1(n, ny, i, iny, {			\
		R_xlen_t ii = 0;			\
		for (int j = 0; j < k; j++) {		\
		    int jj = subs[j][indx[j]];		\
		    if (jj == NA_INTEGER) {		\
			ii = NA_INTEGER;		\
			break;				\
		    }					\
		    else				\
			ii += (jj - 1) * offset[j];	\
		}					\
		if (ii != NA_INTEGER)			\
		    do { CODE } while (0);		\
		if (n > 1) {				\
		    int j = 0;				\
		    while (++indx[j] >= bound[j]) {	\
			indx[j] = 0;			\
			j++;				\
			if (j == k) j = 0;		\
		    }					\
		}					\
	    });						\
    } while (0)

static SEXP ArrayAssign(SEXP call, SEXP rho, SEXP x, SEXP s, SEXP y)
{
    int k = 0;
    SEXP dims, tmp;
    const void *vmax = vmaxget();

    PROTECT(dims = getAttrib(x, R_DimSymbol));
    if (dims == R_NilValue || (k = LENGTH(dims)) != length(s))
	error(_("incorrect number of subscripts"));

    /* k is now the number of dims */
    const int **subs = (const int**) R_alloc(k, sizeof(int*));
    int *indx = (int*) R_alloc(k, sizeof(int));
    int *bound = (int*) R_alloc(k, sizeof(int));
    R_xlen_t *offset = (R_xlen_t*) R_alloc(k, sizeof(R_xlen_t));

    R_xlen_t ny = XLENGTH(y);

    /* Expand the list of subscripts. */
    /* s is protected, so no GC problems here */

    tmp = s;
    for (int i = 0; i < k; i++) {
	SETCAR(tmp, int_arraySubscript(i, CAR(tmp), dims, x, call));
	tmp = CDR(tmp);
    }

    R_xlen_t n = 1;
    tmp = s;
    for (int i = 0; i < k; i++) {
	indx[i] = 0;
	subs[i] = INTEGER_RO(CAR(tmp));
	bound[i] = LENGTH(CAR(tmp));
	n *= bound[i];
	tmp = CDR(tmp);
    }

    if (n > 0 && ny == 0)
	error(_("replacement has length zero"));
    if (n > 0 && n % ny)
	error(_("number of items to replace is not a multiple of replacement length"));

    if (ny > 1) { /* check for NAs in indices */
	for (int i = 0; i < k; i++)
	    for (int j = 0; j < bound[i]; j++)
		if (subs[i][j] == NA_INTEGER)
		    error(_("NAs are not allowed in subscripted assignments"));
    }

    offset[0] = 1;
    const int *pdims = INTEGER_RO(dims);
    for (int i = 1; i < k; i++)
	offset[i] = offset[i - 1] * pdims[i - 1];


    /* Here we make sure that the LHS has been coerced into */
    /* a form which can accept elements from the RHS. */

    int which = SubassignTypeFix(&x, &y, 0, 1, call, rho);/* = 100 * TYPEOF(x) + TYPEOF(y);*/

    if (n == 0) {
	UNPROTECT(1);
	return(x);
    }

    PROTECT(x);

    /* When array elements are being permuted the RHS */
    /* must be duplicated or the elements get trashed. */
    /* FIXME : this should be a shallow copy for list */
    /* objects.  A full duplication is wasteful. */

    if (x == y)
	PROTECT(y = shallow_duplicate(y));
    else
	PROTECT(y);

    /* Note that we are now committed.  Since we are mutating */
    /* existing objects any changes we make now are permanent. */
    /* Beware! */

    switch (which) {

    case 1010:	/* logical   <- logical	  */
    case 1310:	/* integer   <- logical	  */
		/* case 1013:	   logical   <- integer	  */
    case 1313:	/* integer   <- integer	  */

	{
	    int *px = INTEGER(x);
	    ARRAY_ASSIGN_LOOP(px[ii] = INTEGER_ELT(y, iny););
	}
	break;

    case 1410:	/* real	     <- logical	  */
    case 1413:	/* real	     <- integer	  */

	{
	    double *px = REAL(x);
	    ARRAY_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, iny);
		    if (iy == NA_INTEGER)
			px[ii] = NA_REAL;
		    else
			px[ii] = iy;
		});
	}
	break;

	/* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
    case 1414:	/* real	     <- real	  */

	{
	    double *px = REAL(x);
	    ARRAY_ASSIGN_LOOP(px[ii] = REAL_ELT(y, iny););
	}
	break;

    case 1510:	/* complex   <- logical	  */
    case 1513:	/* complex   <- integer	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    ARRAY_ASSIGN_LOOP({
		    int iy = INTEGER_ELT(y, iny);
		    if (iy == NA_INTEGER) {
			px[ii].r = NA_REAL;
			px[ii].i = NA_REAL;
		    }
		    else {
			px[ii].r = iy;
			px[ii].i = 0.0;
		    }
		});
	}
	break;

    case 1514:	/* complex   <- real	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    ARRAY_ASSIGN_LOOP({
		    double ry = REAL_ELT(y, iny);
		    if (ISNA(ry)) {
			px[ii].r = NA_REAL;
			px[ii].i = NA_REAL;
		    }
		    else {
			px[ii].r = ry;
			px[ii].i = 0.0;
		    }
		});
	}
	break;

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
    case 1515:	/* complex   <- complex	  */

	{
	    Rcomplex *px = COMPLEX(x);
	    ARRAY_ASSIGN_LOOP(px[ii] = COMPLEX_ELT(y, iny););
	}
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

	ARRAY_ASSIGN_LOOP(SET_STRING_ELT(x, ii, STRING_ELT(y, iny)););
	break;

    case 1919: /* vector <- vector */

	ARRAY_ASSIGN_LOOP({
		/* set NAMED on RHS value to NAMEDMAX if used more than once
		   (PR15098) */
		if (i >= ny)
		    ENSURE_NAMEDMAX(VECTOR_ELT(y, iny));

		SET_VECTOR_ELT(x, ii, VECTOR_ELT_FIX_NAMED(y, iny));
	    });
	break;

    case 2424: /* raw <- raw */

	{
	    Rbyte *px = RAW(x);
	    ARRAY_ASSIGN_LOOP(px[ii] = RAW_ELT(y, iny););
	}
	break;

    default:
	error(_("incompatible types (from %s to %s) in array subset assignment"),
	      type2char(which%100), type2char(which/100));
    }

    UNPROTECT(3);
    vmaxset(vmax);
    return x;
}

/* Use for pairlists */
static SEXP GetOneIndex(SEXP sub, int ind)
{
    if (ind < 0 || ind+1 > length(sub))
	error("internal error: index %d from length %d", ind, length(sub));
    if (length(sub) > 1) {
	switch (TYPEOF(sub)) {
	case INTSXP:
	    sub = ScalarInteger(INTEGER_ELT(sub, ind));
	    break;
	case REALSXP:
	    sub = ScalarReal(REAL_ELT(sub, ind));
	    break;
	case STRSXP:
	    sub = ScalarString(STRING_ELT(sub, ind));
	    break;
	default:
	    error(_("invalid subscript in list assign"));
	}
    }
    return sub;
}

/* This is only used for [[<-, so only adding one element */
static SEXP SimpleListAssign(SEXP call, SEXP x, SEXP s, SEXP y, int ind)
{
    SEXP indx, sub = CAR(s);
    int ii, n, nx;
    R_xlen_t stretch = 1;

    if (length(s) > 1)
	error(_("invalid number of subscripts to list assign"));

    PROTECT(sub = GetOneIndex(sub, ind));
    PROTECT(indx = makeSubscript(x, sub, &stretch, R_NilValue));

    n = length(indx);
    if (n > 1)
	error(_("invalid subscript in list assign"));

    nx = length(x);

    if (stretch) {
	SEXP t = CAR(s);
	SEXP yi = PROTECT(allocList((int)(stretch - nx)));
	/* This is general enough for only usage */
	if(isString(t) && length(t) == stretch - nx) {
	    SEXP z = yi;
	    int i;
	    for(i = 0; i < LENGTH(t); i++, z = CDR(z))
		SET_TAG(z, installTrChar(STRING_ELT(t, i)));
	}
	UNPROTECT(1);
	PROTECT(x = listAppend(x, yi));
	nx = (int) stretch;
    }
    else PROTECT(x);

    if (n == 1) {
	ii = asInteger(indx);
	if (ii != NA_INTEGER) {
	    ii = ii - 1;
	    SEXP xi = nthcdr(x, ii % nx);
	    SETCAR(xi, y);
	}
    }
    UNPROTECT(3);
    return x;
}

/* This is for x[[s[ind]]] <- NULL */

static SEXP listRemove(SEXP x, SEXP s, int ind)
{
    SEXP pv, px, val;
    int i, ii, *indx, ns, nx;
    R_xlen_t stretch=0;
    const void *vmax = vmaxget();
    nx = length(x);
    PROTECT(s = GetOneIndex(s, ind));
    PROTECT(s = makeSubscript(x, s, &stretch, R_NilValue));
    ns = length(s);
    indx = (int*) R_alloc(nx, sizeof(int));
    for (i = 0; i < nx; i++) indx[i] = 1;
    if (TYPEOF(s) == REALSXP) {
	for (i = 0; i < ns; i++) {
	    double di = REAL_ELT(s, i);
	    if (R_FINITE(di)) indx[(R_xlen_t) di - 1] = 0;
	}
    } else {
	for (i = 0; i < ns; i++) {
	    ii = INTEGER_ELT(s, i);
	    if (ii != NA_INTEGER) indx[ii - 1] = 0;
	}
    }

    px = x;
    pv = val = R_NilValue;
    for (i = 0; i < nx; i++) {
	if (indx[i]) {
	    if (val == R_NilValue)
		val = px;
	    pv = px;
	}
	else {
	    /* The current cell, to which px points, is removed and is
	       no longer accessible, so we can decrement the reference
	       count on it's fields. */
	    DECREMENT_REFCNT(CAR(px));
	    DECREMENT_REFCNT(CDR(px));
	    if (pv != R_NilValue)
		SETCDR(pv, CDR(px));
	}
	px = CDR(px);
    }
    if (val != R_NilValue) {
	SET_ATTRIB(val, ATTRIB(x));
	IS_S4_OBJECT(x) ?  SET_S4_OBJECT(val) : UNSET_S4_OBJECT(val);
	SET_OBJECT(val, OBJECT(x));
	RAISE_NAMED(val, NAMED(x));
    }
    UNPROTECT(2);
    vmaxset(vmax);
    return val;
}


// For  x[s] <- y  --- extract (x, s, y)  and return the number of indices
static R_INLINE int SubAssignArgs(SEXP args, SEXP *x, SEXP *s, SEXP *y)
{
    if (CDR(args) == R_NilValue)
	error(_("SubAssignArgs: invalid number of arguments"));
    *x = CAR(args);
    if (CDDR(args) == R_NilValue) {
	*s = R_NilValue;
	*y = CADR(args);
	return 0;
    }
    else {
	int nsubs = 1;
	SEXP p;
	*s = p = CDR(args);
	while (CDDR(p) != R_NilValue) {
	    p = CDR(p);
	    nsubs++;
	}
	*y = CADR(p);
	SETCDR(p, R_NilValue);
	return nsubs;
    }
}

/* Version of DispatchOrEval for "[" and friends that speeds up simple cases.
   Also defined in subset.c */
static R_INLINE
int R_DispatchOrEvalSP(SEXP call, SEXP op, const char *generic, SEXP args,
		    SEXP rho, SEXP *ans)
{
    SEXP prom = NULL;
    if (args != R_NilValue && CAR(args) != R_DotsSymbol) {
	SEXP x = eval(CAR(args), rho);
	PROTECT(x);
	INCREMENT_LINKS(x);
	if (! OBJECT(x)) {
	    *ans = CONS_NR(x, evalListKeepMissing(CDR(args), rho));
	    DECREMENT_LINKS(x);
	    UNPROTECT(1);
	    return FALSE;
	}
	prom = R_mkEVPROMISE_NR(CAR(args), x);
	args = CONS(prom, CDR(args));
	UNPROTECT(1);
    }
    PROTECT(args);
    int disp = DispatchOrEval(call, op, generic, args, rho, ans, 0, 0);
    if (prom) DECREMENT_LINKS(PRVALUE(prom));
    UNPROTECT(1);
    return disp;
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

    if(R_DispatchOrEvalSP(call, op, "[<-", args, rho, &ans))
/*     if(DispatchAnyOrEval(call, op, "[<-", args, rho, &ans, 0, 0)) */
      return(ans);

    return do_subassign_dflt(call, op, ans, rho);
}

SEXP attribute_hidden do_subassign_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP subs, x, y;
    int nsubs, oldtype; Rboolean S4;

    PROTECT(args);

    nsubs = SubAssignArgs(args, &x, &subs, &y);
    PROTECT(y); /* gets cut loose in SubAssignArs */

    /* make sure the LHS is duplicated if it matches one of the indices */
    /* otherwise this gets the wrong answer:
          permute <- structure(c(3L, 1L, 2L), dim = c(3, 1))
	  permute[permute, 1] <- 1:3
	  as.vector(permute)
    */
    for (SEXP s = subs; s != R_NilValue; s = CDR(s)) {
	SEXP idx = CAR(s);
	if (x == idx)
	    MARK_NOT_MUTABLE(x);
    }

    /* If there are multiple references to an object we must */
    /* duplicate it so that only the local version is mutated. */
    /* This will duplicate more often than necessary, but saves */
    /* over always duplicating. */

    if (MAYBE_SHARED(CAR(args)))
	x = SETCAR(args, shallow_duplicate(CAR(args)));

    S4 = IS_S4_OBJECT(x);

    oldtype = 0;
    if (TYPEOF(x) == LISTSXP || TYPEOF(x) == LANGSXP) {
	oldtype = TYPEOF(x);
	PROTECT(x = PairToVectorList(x));
    }
    else if (xlength(x) == 0) {
	if (xlength(y) == 0 && (isNull(x) || TYPEOF(x) == TYPEOF(y) ||
				// isVectorList(y):
				TYPEOF(y) == VECSXP || TYPEOF(y) == EXPRSXP)) {
	    UNPROTECT(2);  /* args, y */
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
	    x = VectorAssign(call, rho, x, R_MissingArg, y);
	    break;
	case 1:
	    x = VectorAssign(call, rho, x, CAR(subs), y);
	    break;
	case 2:
	    x = MatrixAssign(call, rho, x, subs, y);
	    break;
	default:
	    x = ArrayAssign(call, rho, x, subs, y);
	    break;
	}
	break;
    default:
	error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));
	break;
    }

    if (oldtype == LANGSXP) {
	if(length(x)) {
	    x = VectorToPairList(x);
	    SET_TYPEOF(x, LANGSXP);
	} else
	    error(_("result is zero-length and so cannot be a language object"));
    }

    /* Note the setting of NAMED(x) to zero here.  This means */
    /* that the following assignment will not duplicate the value. */
    /* This works because at this point, x is guaranteed to have */
    /* at most one symbol bound to it.  It does mean that there */
    /* will be multiple reference problems if "[<-" is used */
    /* in a naked fashion. */

    UNPROTECT(3); /* args, x, y */
    SETTER_CLEAR_NAMED(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}

static SEXP DeleteOneVectorListItem(SEXP x, R_xlen_t which)
{
    SEXP y, xnames, ynames;
    R_xlen_t i, k, n;
    n = xlength(x);
    if (0 <= which && which < n) {
	PROTECT(y = allocVector(TYPEOF(x), n - 1));
	k = 0;
	for (i = 0 ; i < n; i++)
	    if(i != which)
		SET_VECTOR_ELT_NR(y, k++, VECTOR_ELT(x, i));
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

/* The [[<- operator; should be fast.
 *     ====
 * args[1] =: x    = object being subscripted
 * args[2] =: subs = list of subscripts
 * args[3] =: y    = replacement values */
SEXP attribute_hidden do_subassign2(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans;

    if(R_DispatchOrEvalSP(call, op, "[[<-", args, rho, &ans))
/*     if(DispatchAnyOrEval(call, op, "[[<-", args, rho, &ans, 0, 0)) */
      return(ans);

    return do_subassign2_dflt(call, op, ans, rho);
}

SEXP attribute_hidden
do_subassign2_dflt(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP dims, indx, names, newname, subs, x, xtop, xup, y, thesub = R_NilValue, xOrig = R_NilValue;
    int i, ndims, nsubs, which, len = 0 /* -Wall */;
    R_xlen_t  stretch, offset, off = -1; /* -Wall */
    Rboolean S4, recursed=FALSE;

    PROTECT(args);

    nsubs = SubAssignArgs(args, &x, &subs, &y);
    PROTECT(y); /* gets cut loose in SubAssignArs */
    S4 = IS_S4_OBJECT(x);

    /* Handle NULL left-hand sides.  If the right-hand side */
    /* is NULL, just return the left-hand size otherwise, */
    /* convert to a zero length list (VECSXP). */

    if (isNull(x)) {
	if (isNull(y)) {
	    UNPROTECT(2); /* args, y */
	    return x;
	}
	if (length(y) == 1)
	    x = allocVector(TYPEOF(y), 0);
	else
	    x = allocVector(VECSXP, 0);
    }

    /* Ensure that the LHS is a local variable. */
    /* If it is not, then make a local copy. */

    if (MAYBE_SHARED(x))
	SETCAR(args, x = shallow_duplicate(x));

    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xOrig = x; /* will be an S4 object */
	x = R_getS4DataSlot(x, ANYSXP);
	if(TYPEOF(x) != ENVSXP)
	  errorcall(call, _("[[<- defined for objects of type \"S4\" only for subclasses of environment"));
    }

    PROTECT(x);
    xtop = xup = x; /* x will be the element which is assigned to */

    dims = getAttrib(x, R_DimSymbol);
    ndims = length(dims);

    int *pdims = NULL;
    if (ndims > 0) {
	if (TYPEOF(dims) == INTSXP)
	    pdims = INTEGER(dims);
	else
	    error(_("improper dimensions"));
    }

    /* ENVSXP special case first */
    if( TYPEOF(x) == ENVSXP) {
	if( nsubs!=1 || !isString(CAR(subs)) || length(CAR(subs)) != 1 )
	    error(_("wrong args for environment subassignment"));
	defineVar(installTrChar(STRING_ELT(CAR(subs), 0)), y, x);
	UNPROTECT(3); /* x, args, y */
	return(S4 ? xOrig : x);
    }

    /* new case in 1.7.0, one vector index for a list,
       more general as of 2.10.0 */
    if (nsubs == 1) {
	thesub = CAR(subs);
	len = length(thesub); /* depth of recursion, small */
	if (len > 1) {
	    xup = vectorIndex(x, thesub, 0, len-2, /*partial ok*/TRUE, call,
			      TRUE);
	    /* OneIndex sets newname, but it will be overwritten before being used. */
	    PROTECT(xup);
	    off = OneIndex(xup, thesub, xlength(xup), 0, &newname, len-2, R_NilValue);
	    x = vectorIndex(xup, thesub, len-2, len-1, TRUE, call, TRUE);
	    UNPROTECT(2); /* xup, x */
	    PROTECT(x);
	    recursed = TRUE;
	}
    }
    PROTECT(xup);

    stretch = 0;
    if (isVector(x)) {
	if (!isVectorList(x) && LENGTH(y) == 0)
	    error(_("replacement has length zero"));
	if (!isVectorList(x) && LENGTH(y) > 1)
	    error(_("more elements supplied than there are to replace"));
	if (nsubs == 0 || CAR(subs) == R_MissingArg)
	    error(_("[[ ]] with missing subscript"));
	if (nsubs == 1) {
	    offset = OneIndex(x, thesub, xlength(x), 0, &newname,
			      recursed ? len-1 : -1, R_NilValue);
	    if (isVectorList(x) && isNull(y)) {
		x = DeleteOneVectorListItem(x, offset);
		if(recursed) {
		    if(isVectorList(xup)) SET_VECTOR_ELT(xup, off, x);
		    else {
			PROTECT(x);
			xup = SimpleListAssign(call, xup, subs, x, len-2);
			UNPROTECT(1); /* x */
		    }
		} else xtop = x;
		UNPROTECT(4); /* xup, x, args, y */
		return xtop;
	    }
	    if (offset < 0)
		error(_("[[ ]] subscript out of bounds"));
	    if (offset >= XLENGTH(x))
		stretch = offset + 1;
	}
	else {
	    if (ndims != nsubs)
		error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = allocVector(INTSXP, ndims));
	    int *pindx = INTEGER0(indx);
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		pindx[i] = (int)
		    get1index(CAR(subs), isNull(names) ?
			      R_NilValue : VECTOR_ELT(names, i),
			      pdims[i],
			      /*partial ok*/FALSE, -1, call);
		subs = CDR(subs);
		if (pindx[i] < 0 ||
		    pindx[i] >= pdims[i])
		    error(_("[[ ]] subscript out of bounds"));
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + pindx[i]) * pdims[i - 1];
	    offset += pindx[0];
	    UNPROTECT(1); /* indx */
	}

	which = SubassignTypeFix(&x, &y, stretch, 2, call, rho);

	PROTECT(x);
	PROTECT(y);

	switch (which) {
	    /* as from 2.3.0 'which' is after conversion */

	case 1010:	/* logical   <- logical	  */
	case 1310:	/* integer   <- logical	  */
	/* case 1013:	   logical   <- integer	  */
	case 1313:	/* integer   <- integer	  */

	    INTEGER(x)[offset] = INTEGER_ELT(y, 0);
	    break;

	case 1410:	/* real	     <- logical	  */
	case 1413:	/* real	     <- integer	  */

	    if (INTEGER_ELT(y, 0) == NA_INTEGER)
		REAL(x)[offset] = NA_REAL;
	    else
		REAL(x)[offset] = INTEGER_ELT(y, 0);
	    break;
	/* case 1014:	   logical   <- real	  */
	/* case 1314:	   integer   <- real	  */
	case 1414:	/* real	     <- real	  */

	    REAL(x)[offset] = REAL(y)[0];
	    break;

	case 1510:	/* complex   <- logical	  */
	case 1513:	/* complex   <- integer	  */

	    if (INTEGER_ELT(y, 0) == NA_INTEGER) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = INTEGER_ELT(y, 0);
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	case 1514:	/* complex   <- real	  */

	    if (ISNA(REAL_ELT(y, 0))) {
		COMPLEX(x)[offset].r = NA_REAL;
		COMPLEX(x)[offset].i = NA_REAL;
	    }
	    else {
		COMPLEX(x)[offset].r = REAL_ELT(y, 0);
		COMPLEX(x)[offset].i = 0.0;
	    }
	    break;

	/* case 1015:	   logical   <- complex	  */
	/* case 1315:	   integer   <- complex	  */
	/* case 1415:	   real	     <- complex	  */
	case 1515:	/* complex   <- complex	  */

	    COMPLEX(x)[offset] = COMPLEX_ELT(y, 0);
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
	case 1902:  /* vector	  <- pairlist   */
	case 1904:  /* vector     <- environment*/
	case 1905:  /* vector     <- promise    */
	case 1906:  /* vector     <- language   */
	case 1910:  /* vector     <- logical    */
	case 1913:  /* vector     <- integer    */
	case 1914:  /* vector     <- real       */
	case 1915:  /* vector     <- complex    */
	case 1916:  /* vector     <- character  */
	case 1920:  /* vector     <- expression */
	case 1921:  /* vector     <- bytecode   */
	case 1922:  /* vector     <- external pointer */
	case 1923:  /* vector     <- weak reference */
	case 1924:  /* vector     <- raw */
	case 1925:  /* vector     <- S4 */
	case 1903: case 1907: case 1908: case 1999: /* functions */

	    /* drop through: vectors and expressions are treated the same */

	case 2001:	/* expression <- symbol	    */
	case 2002:	/* expression <- pairlist   */
	case 2006:	/* expression <- language   */
	case 2010:	/* expression <- logical    */
	case 2013:	/* expression <- integer    */
	case 2014:	/* expression <- real	    */
	case 2015:	/* expression <- complex    */
	case 2016:	/* expression <- character  */
	case 2024:	/* expression     <- raw */
	case 2025:	/* expression     <- S4 */
	case 1919:      /* vector     <- vector     */
	case 2020:	/* expression <- expression */

	    SET_VECTOR_ELT(x, offset, R_FixupRHS(x, y));
	    break;

	case 2424:      /* raw <- raw */

	    RAW(x)[offset] = RAW_ELT(y, 0);
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
		UNPROTECT(1); /* names */
	    }
	    else
		SET_STRING_ELT(names, offset, newname);
	}
	UNPROTECT(4); /* y, x, xup, x */
	PROTECT(x);
	PROTECT(xup);
    }
    else if (isPairList(x)) {
	y = R_FixupRHS(x, y);
	PROTECT(y);
	if (nsubs == 1) {
	    if (isNull(y)) {
		x = listRemove(x, CAR(subs), len-1);
	    }
	    else {
		x = SimpleListAssign(call, x, subs, y, len-1);
	    }
	}
	else {
	    if (ndims != nsubs)
		error(_("[[ ]] improper number of subscripts"));
	    PROTECT(indx = allocVector(INTSXP, ndims));
	    int *pindx = INTEGER0(indx);
	    names = getAttrib(x, R_DimNamesSymbol);
	    for (i = 0; i < ndims; i++) {
		pindx[i] = (int)
		    get1index(CAR(subs), VECTOR_ELT(names, i),
			      pdims[i],
			      /*partial ok*/FALSE, -1, call);
		subs = CDR(subs);
		if (pindx[i] < 0 ||
		    pindx[i] >= pdims[i])
		    error(_("[[ ]] subscript (%d) out of bounds"), i+1);
	    }
	    offset = 0;
	    for (i = (ndims - 1); i > 0; i--)
		offset = (offset + pindx[i]) * pdims[i - 1];
	    offset += pindx[0];
	    SEXP slot = nthcdr(x, (int) offset);
	    SETCAR(slot, duplicate(y));
	    /* FIXME: add name */
	    UNPROTECT(1); /* indx */
	}
	UNPROTECT(3); /* y, xup, x */
	PROTECT(x);
	PROTECT(xup);
    }
    else error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));

    if(recursed) {
	if (isVectorList(xup)) {
	    SET_VECTOR_ELT(xup, off, x);
	} else {
	    xup = SimpleListAssign(call, xup, subs, x, len-2);
	}
	if (len == 2)
	    xtop = xup;
    }
    else xtop = x;

    UNPROTECT(4); /* xup, x, args, y */
    SETTER_CLEAR_NAMED(xtop);
    if(S4) SET_S4_OBJECT(xtop);
    return xtop;
}

/* $<-(x, elt, val), and elt does not get evaluated it gets matched.
   to get DispatchOrEval to work we need to first translate it
   to a string
*/
SEXP attribute_hidden do_subassign3(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans, nlist = R_NilValue;

    checkArity(op, args);

    /* Note the RHS has already been evaluated at this point */
    PROTECT(args = fixSubset3Args(call, args, env, &nlist));

    if(R_DispatchOrEvalSP(call, op, "$<-", args, env, &ans)) {
	UNPROTECT(1); /* args */
	return(ans);
    }
    PROTECT(ans);
    if (nlist == R_NilValue)
	nlist = installTrChar(STRING_ELT(CADR(args), 0));
    ans = R_subassign3_dflt(call, CAR(ans), nlist, CADDR(ans));
    UNPROTECT(2); /* args, ans */
    return ans;
}

/* used in "$<-" (above) and methods_list_dispatch.c */
SEXP R_subassign3_dflt(SEXP call, SEXP x, SEXP nlist, SEXP val)
{
    SEXP t;
    PROTECT_INDEX pvalidx, pxidx;
    Rboolean maybe_duplicate=FALSE;
    Rboolean S4; SEXP xS4 = R_NilValue;

    PROTECT_WITH_INDEX(x, &pxidx);
    PROTECT_WITH_INDEX(val, &pvalidx);
    S4 = IS_S4_OBJECT(x);

    if (MAYBE_SHARED(x))
	REPROTECT(x = shallow_duplicate(x), pxidx);

    /* If we aren't creating a new entry and NAMED>0
       we need to duplicate to prevent cycles.
       If we are creating a new entry we could duplicate
       or increase NAMED. We duplicate if NAMED == 1, but
       not if NAMED > 1 */
    if (MAYBE_SHARED(val))
	maybe_duplicate=TRUE;
    else if (MAYBE_REFERENCED(val))
	REPROTECT(val = R_FixupRHS(x, val), pvalidx);
    /* code to allow classes to extend ENVSXP */
    if(TYPEOF(x) == S4SXP) {
	xS4 = x;
	REPROTECT(x = R_getS4DataSlot(x, ANYSXP), pxidx);
	if(x == R_NilValue)
	  errorcall(call, _("no method for assigning subsets of this S4 class"));
    }

    if ((isList(x) || isLanguage(x)) && !isNull(x)) {
	/* Here we do need to duplicate */
	if (maybe_duplicate)
	    REPROTECT(val = R_FixupRHS(x, val), pvalidx);
	if (TAG(x) == nlist) {
	    if (val == R_NilValue) {
		SET_ATTRIB(CDR(x), ATTRIB(x));
		IS_S4_OBJECT(x) ?  SET_S4_OBJECT(CDR(x)) : UNSET_S4_OBJECT(CDR(x));
		SET_OBJECT(CDR(x), OBJECT(x));
		RAISE_NAMED(CDR(x), NAMED(x));
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
    else if( TYPEOF(x) == SYMSXP || /* Used to 'work' in R < 2.8.0 */
	     TYPEOF(x) == CLOSXP ||
	     TYPEOF(x) == SPECIALSXP ||
	     TYPEOF(x) == BUILTINSXP) {
	error(R_MSG_ob_nonsub, type2char(TYPEOF(x)));
    }
    else {
	R_xlen_t i, imatch, nx;
	SEXP names;
	int type = VECSXP;

	if (isExpression(x))
	    type = EXPRSXP;
	else if (!isNewList(x)) {
	    warning(_("Coercing LHS to a list"));
	    REPROTECT(x = coerceVector(x, VECSXP), pxidx);
	}
	names = getAttrib(x, R_NamesSymbol);
	nx = xlength(x);
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
		    PROTECT(ans = allocVector(type, nx - 1));
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
		    REPROTECT(val = R_FixupRHS(x, val), pvalidx);
		SET_VECTOR_ELT(x, imatch, val);
	    }
	    else {
		/* We are introducing a new element (=> *no* duplication) */
		/* Enlarge the list, add the new element */
		/* and finally, adjust the attributes. */
		SEXP ans, ansnames;
		PROTECT(ans = allocVector(VECSXP, nx + 1));
		PROTECT(ansnames = allocVector(STRSXP, nx + 1));
		for (i = 0; i < nx; i++)
		    SET_VECTOR_ELT_NR(ans, i, VECTOR_ELT(x, i));
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
    if(xS4 != R_NilValue)
	x = xS4; /* x was an env't, the data slot of xS4 */
    SETTER_CLEAR_NAMED(x);
    if(S4) SET_S4_OBJECT(x);
    return x;
}
