/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-7  R Development Core Team
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

/* <UTF8> char here is either ASCII or handled as a whole */


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>

/* Implementation of identical(x, y) */

static Rboolean neWithNaN(double x,  double y);

/* primitive interface */

SEXP attribute_hidden do_identical(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return ScalarLogical( compute_identical(CAR(args), CADR(args)) );
}

/* do the two objects compute as identical? */
Rboolean attribute_hidden compute_identical(SEXP x, SEXP y)
{
    SEXP ax, ay;
    if(x == y)
	return TRUE;
    if(TYPEOF(x) != TYPEOF(y))
	return FALSE;
    if(OBJECT(x) != OBJECT(y))
	return FALSE;

    /* Attributes are special: they should be tagged pairlists.  We
       don't test them if they are not, and we do not test the order
       if they are.

       This code is not very efficient, but then neither is using
       pairlists for attributes.  If long attribute lists become more
       common (and they are used for S4 slots) we should store them in a hash
       table.
    */
    ax = ATTRIB(x); ay = ATTRIB(y);
    if(ax != R_NilValue || ay != R_NilValue) {
	if(ax == R_NilValue || ay == R_NilValue)
	    return FALSE;
	/* if(!compute_identical(ATTRIB(x),ATTRIB(y))) return FALSE; */
	if(TYPEOF(ax) != LISTSXP || TYPEOF(ay) != LISTSXP) {
	    warning(_("ignoring non-pairlist attributes"));
	} else {
	    SEXP elx, ely;
	    if(length(ax) != length(ay)) return FALSE;
	    /* They are the same length and should have 
	       unique non-empty non-NA tags */
	    for(elx = ax; elx != R_NilValue; elx = CDR(elx)) {
		const char *tx = CHAR(PRINTNAME(TAG(elx)));
		for(ely = ay; ely != R_NilValue; ely = CDR(ely))
		    if(streql(tx, CHAR(PRINTNAME(TAG(ely))))) {
			/* We need to treat row.names specially here */
			if(streql(tx, "row.names")) {
			    if(!compute_identical(getAttrib(x, R_RowNamesSymbol),
						  getAttrib(y, R_RowNamesSymbol)))
			       return FALSE;
			} else
			    if(!compute_identical(CAR(elx), CAR(ely))) 
				return FALSE;
			break;
		    }
		if(ely == R_NilValue) return FALSE;
	    }
	}
    }
    switch (TYPEOF(x)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
        if (length(x) != length(y)) return FALSE;
        /* Use memcmp (which is ISO C) to speed up the comparison */
        return memcmp((void *)LOGICAL(x), (void *)LOGICAL(y),
                      length(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case INTSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	return memcmp((void *)INTEGER(x), (void *)INTEGER(y), 
		      length(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case REALSXP:
    {
	double *xp = REAL(x), *yp = REAL(y);
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(neWithNaN(xp[i], yp[i])) return FALSE;
	return TRUE;
    }
    case CPLXSXP:
    {
	Rcomplex *xp = COMPLEX(x), *yp = COMPLEX(y);
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(neWithNaN(xp[i].r,  yp[i].r) || neWithNaN(xp[i].i,  yp[i].i))
		return FALSE;
	return TRUE;
    }
    case STRSXP:
    {
	int i, n = length(x), n1, n2;
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++) {
	    Rboolean na1 = (STRING_ELT(x, i) == NA_STRING),
		na2 = (STRING_ELT(y, i) == NA_STRING);
	    if(na1 ^ na2) return FALSE;
	    if(na1 && na2) continue;
	    /* NB: R strings can have embedded nuls */
	    n1 = LENGTH(STRING_ELT(x, i));
	    n2 = LENGTH(STRING_ELT(y, i));
	    if (n1 != n2) return FALSE;
	    if(memcmp(CHAR(STRING_ELT(x, i)), CHAR(STRING_ELT(y, i)), n1) != 0)
		return FALSE;
	}
	return TRUE;
    }
    case CHARSXP:
    {
	/* NB: R strings can have embedded nuls */
	int n1 = LENGTH(x), n2 = LENGTH(y);
	if (n1 != n2) return FALSE;
	if(memcmp(CHAR(x), CHAR(y), n1) != 0) return FALSE;
    }
    case VECSXP:
    case EXPRSXP: 
    {
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(!compute_identical(VECTOR_ELT(x, i),VECTOR_ELT(y, i)))
		return FALSE;
	return TRUE;
    }
    case LANGSXP:
    case LISTSXP: 
    {
	while (x != R_NilValue) {
	    if(y == R_NilValue)
		return FALSE;
	    if(!compute_identical(CAR(x), CAR(y)))
		return FALSE;
	    if(!compute_identical(PRINTNAME(TAG(x)), PRINTNAME(TAG(y))))
		return FALSE;
	    x = CDR(x);
	    y = CDR(y);
	}
	return(y == R_NilValue);
    }
    case CLOSXP:
	return(compute_identical(FORMALS(x), FORMALS(y)) &&
	       compute_identical(BODY_EXPR(x), BODY_EXPR(y)) &&
	       CLOENV(x) == CLOENV(y) ? TRUE : FALSE);
    case SPECIALSXP:
    case BUILTINSXP:
	return(PRIMOFFSET(x) == PRIMOFFSET(y) ? TRUE : FALSE);
    case ENVSXP:
    case SYMSXP:
    case EXTPTRSXP:
    case WEAKREFSXP:
#ifdef BYTECODE
    case BCODESXP: /**** is this the best approach? */
#endif
	return(x == y ? TRUE : FALSE);
    case RAWSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	return memcmp((void *)RAW(x), (void *)RAW(y), 
		      length(x) * sizeof(Rbyte)) == 0 ? TRUE : FALSE;

	/*  case PROMSXP: args are evaluated, so will not be seen */
	/* test for equality of the substituted expression -- or should
	   we require both expression and environment to be identical? */
	/*#define PREXPR(x)	((x)->u.promsxp.expr)
	  #define PRENV(x)	((x)->u.promsxp.env)
	  return(compute_identical(subsititute(PREXPR(x), PRENV(x)),
	  subsititute(PREXPR(y), PRENV(y))));*/
    case S4SXP:
        /* attributes already tested, so all slots identical */
        return TRUE;
    default:
	/* these are all supposed to be types that represent constant
	   entities, so no further testing required ?? */
	printf("Unknown Type: %s (%x)\n", type2char(TYPEOF(x)), TYPEOF(x));
	return TRUE;
    }
}

/* return TRUE if x and y differ, including the case
   that one, but not both are NaN.  Two NaN values are judged
   identical for this purpose, but NA != NaN */

static Rboolean neWithNaN(double x,  double y)
{
    if(R_IsNA(x))
	return(R_IsNA(y) ? FALSE : TRUE);
    if(ISNAN(x))
	return(ISNAN(y) ? FALSE : TRUE);
    return(x != y);
}
