/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-4  R Development Core Team
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
#include <Defn.h>

/* Implementation of identical(x, y) */

static Rboolean compute_identical(SEXP x, SEXP y);
static Rboolean neWithNaN(double x,  double y);

SEXP do_identical(SEXP x, SEXP y)
{
    SEXP ans;

    PROTECT(ans = allocVector(LGLSXP, 1));
    LOGICAL(ans)[0] = compute_identical(x, y);
    UNPROTECT(1);
    return(ans);
}

/* primitive interface */

SEXP do_ident(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    return do_identical(CAR(args), CADR(args));
}

/* do the two objects compute as identical? */
static Rboolean compute_identical(SEXP x, SEXP y)
{
    if(x == y)
	return TRUE;
    if(TYPEOF(x) != TYPEOF(y))
	return FALSE;
    if(OBJECT(x) != OBJECT(y))
	return FALSE;
    if(ATTRIB(x) != R_NilValue || ATTRIB(y) != R_NilValue) {
	if(ATTRIB(x) == R_NilValue || ATTRIB(y) == R_NilValue)
	    return FALSE;
	if(!compute_identical(ATTRIB(x),ATTRIB(y)))
	    return FALSE;
    }
    switch (TYPEOF(x)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
    case INTSXP:
	if (length(x) != length(y)) return FALSE;
	/* Use memcmp (which is ISO C) to speed up the comparison */
	/* Using INTEGER as data accessor works for both INTSXP and LGLSXP */
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
	int i, n = length(x);
	if(n != length(y)) return FALSE;
	for(i = 0; i < n; i++) {
	    Rboolean na1 = (STRING_ELT(x, i) == NA_STRING),
		na2 = (STRING_ELT(y, i) == NA_STRING);
	    if(na1 ^ na2) return FALSE;
	    if(na1 && na2) continue;
	    if(strcmp(CHAR(STRING_ELT(x, i)),
		      CHAR(STRING_ELT(y, i))) != 0)
		return FALSE;
	}
	return TRUE;
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

	/*  case PROMSXP: */
	/* test for equality of the substituted expression -- or should
	   we require both expression and environment to be identical? */
	/*#define PREXPR(x)	((x)->u.promsxp.expr)
	  #define PRENV(x)	((x)->u.promsxp.env)
	  return(compute_identical(subsititute(PREXPR(x), PRENV(x)),
	  subsititute(PREXPR(y), PRENV(y))));*/
    default:
	/* these are all supposed to be types that represent constant
	   entities, so no further testing required ?? */
	printf("Unknown Type: %s (%x)\n", CHAR(type2str(TYPEOF(x))), 
	       TYPEOF(x));
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
