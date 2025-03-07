/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2001-2025  The R Core Team
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


#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <Defn.h>
/* -> Rinternals.h which exports R_compute_identical() */

/* Implementation of identical(x, y) */

/* How are  R "double"s compared : */
typedef enum {
    bit_NA__num_bit = 0,// S's default - look at bit pattern, also for NA/NaN's
    bit_NA__num_eq  = 1,// bitwise comparison for NA / NaN; '==' for other numbers
 single_NA__num_bit = 2,// one kind of NA or NaN; for num, use 'bit'comparison
 single_NA__num_eq  = 3,// one kind of NA or NaN; for num, use '==' : R's DEFAULT
} ne_strictness_type;
/* NOTE:  ne_strict = NUM_EQ + (SINGLE_NA * 2)  = NUM_EQ | (SINGLE_NA << 1)   */

static bool neWithNaN(double x, double y, ne_strictness_type str);


static R_INLINE int asFlag(SEXP x, const char *name)
{
    int val = asLogical(x);
    if (val == NA_LOGICAL)
	error(_("invalid '%s' value"), name);
    return val;
}

/* .Internal(identical(..)) */
attribute_hidden SEXP do_identical(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int nargs = length(args);
    /* avoid problems with earlier (and future) versions captured in S4
       methods: but this should be fixed where it is caused, in
       'methods'!

       checkArity(op, args); */
    if (nargs < 5)
	error("%d arguments passed to .Internal(%s) which requires %d",
	      length(args), PRIMNAME(op), PRIMARITY(op));

    SEXP x = CAR(args); args = CDR(args);
    SEXP y = CAR(args); args = CDR(args);

    int num_as_bits = ! asFlag(CAR(args), "num.eq"); args = CDR(args);
    int NA_as_bits = ! asFlag(CAR(args), "single.NA"); args = CDR(args);
    int attr_by_order = ! asFlag(CAR(args), "attrib.as.set"); args = CDR(args);

    int use_bytecode = FALSE;
    if (nargs >= 6)
	use_bytecode = ! asFlag(CAR(args), "ignore.bytecode");

    int use_cloenv = TRUE;
    if (nargs >= 7)
	use_cloenv = ! asFlag(CADR(args), "ignore.environment");

    int use_srcref = FALSE;
    if (nargs >= 8)
	use_srcref = ! asFlag(CADDR(args), "ignore.srcref");

    int extptr_as_ref = FALSE;
    if (nargs >= 9)
	extptr_as_ref = asFlag(CADDDR(args), "extptr.as.ref");

    int flags = 0;
    if (num_as_bits) flags |= IDENT_NUM_AS_BITS;
    if (NA_as_bits) flags |= IDENT_NA_AS_BITS;
    if (attr_by_order) flags |= IDENT_ATTR_BY_ORDER;
    if (use_bytecode) flags |= IDENT_USE_BYTECODE;
    if (use_cloenv) flags |= IDENT_USE_CLOENV;
    if (use_srcref) flags |= IDENT_USE_SRCREF;
    if (extptr_as_ref) flags |= IDENT_EXTPTR_AS_REF;

    return ScalarLogical(R_compute_identical(x, y, flags));
}

#define NUM_EQ		(!(flags & IDENT_NUM_AS_BITS))
#define SINGLE_NA       (!(flags & IDENT_NA_AS_BITS))
#define ATTR_AS_SET     (!(flags & IDENT_ATTR_BY_ORDER))
#define IGNORE_BYTECODE (!(flags & IDENT_USE_BYTECODE))
#define IGNORE_ENV      (!(flags & IDENT_USE_CLOENV))
#define IGNORE_SRCREF   (!(flags & IDENT_USE_SRCREF))
#define EXTPTR_AS_REF   (flags & IDENT_EXTPTR_AS_REF)

/* do the two objects compute as identical?
   Also used in unique.c, eval.c, relop.c, dotcode.c, and exported */
Rboolean
R_compute_identical(SEXP x, SEXP y, int flags)
{
    if(x == y) /* same pointer */
	return TRUE;
    if(TYPEOF(x) != TYPEOF(y) ||
       OBJECT(x) != OBJECT(y) ||
       IS_S4_OBJECT(x) != IS_S4_OBJECT(y))
	return FALSE;

    /* Skip attribute checks for CHARSXP
       -- such attributes are used for the cache.  */
    if(TYPEOF(x) == CHARSXP) {
	/* This matches NAs */
	return Seql(x, y) == 1;
    }
    SEXP ax, ay;
    if (IGNORE_SRCREF && TYPEOF(x) == CLOSXP) {
	/* Remove "srcref" attribute - and below, treat body(x), body(y) */
	SEXP x_ = PROTECT(duplicate(x)), y_ = PROTECT(duplicate(y));
	setAttrib(x_, R_SrcrefSymbol, R_NilValue);
	setAttrib(y_, R_SrcrefSymbol, R_NilValue);
	ax = ATTRIB(x_); ay = ATTRIB(y_);
	UNPROTECT(2);
    }
    else {
	ax = ATTRIB(x); ay = ATTRIB(y);
    }

    if(ax != R_NilValue || ay != R_NilValue) {
	if(ax == R_NilValue || ay == R_NilValue)
	    return FALSE;
	/* Attributes are tagged pairlists with unique non-empty non-NA tags.
	   This code still includes a check and if they are not pairlists,
	   they are not compared, with a warning (could be turned into an error
	   or removed). */
	if(TYPEOF(ax) != LISTSXP || TYPEOF(ay) != LISTSXP) {
	    warning(_("ignoring non-pairlist attributes"));
	} else if (!ATTR_AS_SET) {
	    /* ax, ay might be fresh allocations from duplicating into
	       x_, y_) above, so need to be protected from possible
	       allocations in getAttrib and recursive calls to
	       R_compute_identical in the loop. */
	    PROTECT(ax);
	    PROTECT(ay);
	    while (ax != R_NilValue) {
		if (ay == R_NilValue) {
		    UNPROTECT(2); /* ax, ay */
		    return FALSE;
		}
		/* Need to check for R_RowNamesSymbol and treat specially */
		if (TAG(ax) == R_RowNamesSymbol) {
		    SEXP atrx = PROTECT(getAttrib(x, R_RowNamesSymbol));
		    SEXP atry = PROTECT(getAttrib(y, R_RowNamesSymbol));
		    if (!R_compute_identical(atrx, atry, flags)) {
			UNPROTECT(4); /* atrx, atry, ax, ay */
			return FALSE;
		    } 
		    UNPROTECT(2); /* atrx, atry */
		} else if (!R_compute_identical(CAR(ax), CAR(ay), flags)) {	  
		    UNPROTECT(2); /* ax, ay */
		    return FALSE;
		}
		if (!R_compute_identical(PRINTNAME(TAG(ax)),
					 PRINTNAME(TAG(ay)), flags)) {
		    UNPROTECT(2); /* ax, ay */
		    return FALSE;
		}
		ax = CDR(ax);
		ay = CDR(ay);
	    }
	    UNPROTECT(2); /* ax, ay */
	    if (ay != R_NilValue)
		return FALSE;
	} else /* ATTR_AS_SET */ {
	    /* This code is not very efficient, but then neither is using
	       pairlists for attributes.  If long attribute lists become more
	       common (and they are used for S4 slots) we should store them in
	       a hash table. */
	    SEXP elx, ely;
	    if(length(ax) != length(ay)) return FALSE;
	    /* They are the same length and should have
	       unique non-empty non-NA tags */
	    /* ax, ay might be fresh allocations from duplicating into
	       x_, y_) above, so need to be protected from possible
	       allocations in getAttrib and recursive calls to
	       R_compute_identical in the loop. */
	    PROTECT(ax);
	    PROTECT(ay);
	    for(elx = ax; elx != R_NilValue; elx = CDR(elx)) {
		const char *tx = CHAR(PRINTNAME(TAG(elx)));
		for(ely = ay; ely != R_NilValue; ely = CDR(ely))
		    if(streql(tx, CHAR(PRINTNAME(TAG(ely))))) {
			/* We need to treat row.names specially here */
			if(streql(tx, "row.names")) {
			    SEXP
				atrx = PROTECT(getAttrib(x, R_RowNamesSymbol)),
				atry = PROTECT(getAttrib(y, R_RowNamesSymbol));
			    if(!R_compute_identical(atrx, atry, flags)) {
				UNPROTECT(4); /* atrx, atry, ax, ay */
				return FALSE;
			    } else
				UNPROTECT(2); /* atrx, atry */
			} else
			    if(!R_compute_identical(CAR(elx), CAR(ely),
						    flags)) {
				UNPROTECT(2); /* ax, ay */
				return FALSE;
			    }
			break;
		    }
		if(ely == R_NilValue) {
		    UNPROTECT(2); /* ax, ay */
		    return FALSE;
		}
	    }
	    UNPROTECT(2); /* ax, ay */
	}
    }

    switch (TYPEOF(x)) {
    case NILSXP:
	return TRUE;
    case LGLSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)LOGICAL(x), (void *)LOGICAL(y),
		      xlength(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case INTSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)INTEGER(x), (void *)INTEGER(y),
		      xlength(x) * sizeof(int)) == 0 ? TRUE : FALSE;
    case REALSXP:
    {
	R_xlen_t n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	else {
	    double *xp = REAL(x), *yp = REAL(y);
	    int ne_strict = NUM_EQ | (SINGLE_NA << 1);
	    for(R_xlen_t i = 0; i < n; i++)
		if(neWithNaN(xp[i], yp[i], ne_strict)) return FALSE;
	}
	return TRUE;
    }
    case CPLXSXP:
    {
	R_xlen_t n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	else {
	    Rcomplex *xp = COMPLEX(x), *yp = COMPLEX(y);
	    int ne_strict = NUM_EQ | (SINGLE_NA << 1);
	    for(R_xlen_t i = 0; i < n; i++)
		if(neWithNaN(xp[i].r, yp[i].r, ne_strict) ||
		   neWithNaN(xp[i].i, yp[i].i, ne_strict))
		    return FALSE;
	}
	return TRUE;
    }
    case STRSXP:
    {
	R_xlen_t i, n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	for(i = 0; i < n; i++) {
	    /* This special-casing for NAs is not needed */
	    bool na1 = (STRING_ELT(x, i) == NA_STRING),
		na2 = (STRING_ELT(y, i) == NA_STRING);
	    if(na1 ^ na2) return FALSE;
	    if(na1 && na2) continue;
	    if (! Seql(STRING_ELT(x, i), STRING_ELT(y, i))) return FALSE;
	}
	return TRUE;
    }
    case CHARSXP: /* Probably unreachable, but better safe than sorry... */
    {
	/* This matches NAs */
	return Seql(x, y) == 1;
    }
    case VECSXP:
    case EXPRSXP:
    {
	R_xlen_t i, n = XLENGTH(x);
	if(n != XLENGTH(y)) return FALSE;
	for(i = 0; i < n; i++)
	    if(!R_compute_identical(VECTOR_ELT(x, i),VECTOR_ELT(y, i), flags))
		return FALSE;
	return TRUE;
    }
    case LANGSXP:
    case LISTSXP:
    case DOTSXP:
    {
	while (x != R_NilValue) {
	    if(y == R_NilValue)
		return FALSE;
	    if(!R_compute_identical(CAR(x), CAR(y), flags))
		return FALSE;
	    if(!R_compute_identical(PRINTNAME(TAG(x)), PRINTNAME(TAG(y)), flags))
		return FALSE;
	    x = CDR(x);
	    y = CDR(y);
	}
	return(y == R_NilValue);
    }
    case CLOSXP:
    {
	if(!R_compute_identical(FORMALS(x), FORMALS(y), flags)) return FALSE;
	if(IGNORE_BYTECODE) {
	    if(IGNORE_SRCREF) {
		SEXP x_ = PROTECT(R_body_no_src(x)),
		     y_ = PROTECT(R_body_no_src(y));
		Rboolean id_body = R_compute_identical(x_, y_, flags);
		UNPROTECT(2);
		if(!id_body) return FALSE;
	    } else if(!R_compute_identical(BODY_EXPR(x), BODY_EXPR(y), flags))
		return FALSE;
	} else { // !IGNORE_BYTECODE: use byte code for comparison of function bodies :
	    if(!R_compute_identical(BODY(x), BODY(y), flags)) return FALSE;
	}
	// now, formals and body are equal, check the environment(.)s:
	return (IGNORE_ENV || CLOENV(x) == CLOENV(y) ? TRUE : FALSE);
    }
    case SPECIALSXP:
    case BUILTINSXP:
	return(PRIMOFFSET(x) == PRIMOFFSET(y) ? TRUE : FALSE);
    case ENVSXP:
    case SYMSXP:
    case WEAKREFSXP: /**** is this the best approach? */
	return(x == y ? TRUE : FALSE);
    case BCODESXP:
	return R_compute_identical(BCODE_CODE(x), BCODE_CODE(y), flags) &&
	       R_compute_identical(BCODE_EXPR(x), BCODE_EXPR(y), flags) &&
	       R_compute_identical(BCODE_CONSTS(x), BCODE_CONSTS(y), flags);
    case EXTPTRSXP:
	if (EXTPTR_AS_REF)
	    return x == y ? TRUE : FALSE;
	else
	    return (EXTPTR_PTR(x) == EXTPTR_PTR(y) ? TRUE : FALSE);
    case RAWSXP:
	if (XLENGTH(x) != XLENGTH(y)) return FALSE;
	/* Use memcmp (which is ISO C90) to speed up the comparison */
	return memcmp((void *)RAW(x), (void *)RAW(y),
		      XLENGTH(x) * sizeof(Rbyte)) == 0 ? TRUE : FALSE;
    case PROMSXP:
    {
	// args are evaluated -- but can be seen from DOTSXP dissection
	/* test for equality of the substituted expression -- or should
	   we require both expression and environment to be identical? */
	SEXP sy = PROTECT(substitute(PREXPR(y), PRENV(y)));
	SEXP sx = PROTECT(substitute(PREXPR(x), PRENV(x)));
	Rboolean ans = R_compute_identical(sx, sy, flags);
	UNPROTECT(2); /* sx, sy */
	return ans;
    }
    case OBJSXP:
	/* attributes already tested, so all slots identical */
	return TRUE;
    default:
	/* these are all supposed to be types that represent constant
	   entities, so no further testing required ?? */
	printf("Unknown Type in identical(): %s (%x)\n", R_typeToChar(x), TYPEOF(x));
	return TRUE;
    }
}


/**
 * [N]ot [E]qual  (x, y)   <==>   x  "!="  y
 *  where the NA/NaN and "-0." / "+0." cases treatment depend on 'str'.
 *
 * @param x
 * @param y  the two "number"s to be compared
 * @param str a "strictness" indicator, one of 2*2 (one|bit)_NA__num_(eq|bit)
 *  "SINGLE_NA" means: x and y differ in the case
 *    that one, but not both are NaN.  Two NaN values are judged
 *    identical for this purpose, but NA != NaN
 *
 *  "NUM_EQ" means: (x != y) is used when both are not NA or NaN
 *  whereas "bit_NA" and "num_bit" use the bitwise memory comparison  memcmp();
 *  notably "*_num_bit" will differentiate '+0.' and '-0.'.
 *
 * @return FALSE or TRUE indicating if x or y differ
 */
static bool neWithNaN(double x, double y, ne_strictness_type str)
{
    switch (str) {
    case single_NA__num_eq:
    case single_NA__num_bit:
	if(R_IsNA(x))
	    return(R_IsNA(y) ? FALSE : TRUE);
	if(R_IsNA(y))
	    return(R_IsNA(x) ? FALSE : TRUE);
	if(ISNAN(x))
	    return(ISNAN(y) ? FALSE : TRUE);

    case bit_NA__num_eq:
    case bit_NA__num_bit:
	; /* do nothing */
    }

    switch (str) {
    case single_NA__num_eq:
	return(x != y);
    case bit_NA__num_eq:
	if(!ISNAN(x) && !ISNAN(y))
	    return(x != y);
	else /* bitwise check for NA/NaN's */
	    return memcmp((const void *) &x,
			  (const void *) &y, sizeof(double)) ? TRUE : FALSE;
    case bit_NA__num_bit:
    case single_NA__num_bit:
	return memcmp((const void *) &x,
		      (const void *) &y, sizeof(double)) ? TRUE : FALSE;
    default: /* Wall */
	return FALSE;
    }
}
