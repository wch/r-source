/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000-2014  The R Core Team
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
 */


/* We need to know the sizes of certain internal structures */
#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>

/* A count of the memory used by an object. The following assumptions
   are made.

   1) this is called from user-level, so only some types of objects are
      important.
   2) an object gets charged for all the space allocated on the heap
      and all the nodes specifically due to it, but not for the
      space for its name nor for .Internals it references.
*/
SEXP Rf_csduplicated(SEXP x);  /* from unique.c */

static R_size_t objectsize(SEXP s)
{
    R_size_t cnt = 0, vcnt = 0;
    SEXP tmp, dup;
    Rboolean isVec = FALSE;

    switch (TYPEOF(s)) {
    case NILSXP:
	return(0);
	break;
    case SYMSXP:
	break;
    case LISTSXP:
    case LANGSXP:
    case BCODESXP:
    case DOTSXP:
	cnt += objectsize(TAG(s));
	cnt += objectsize(CAR(s));
	cnt += objectsize(CDR(s));
	break;
    case CLOSXP:
	cnt += objectsize(FORMALS(s));
	cnt += objectsize(BODY(s));
	/* no charge for the environment */
	break;
    case ENVSXP:
    case PROMSXP:
    case SPECIALSXP:
    case BUILTINSXP:
	break;
    case CHARSXP:
	vcnt = BYTE2VEC(length(s)+1);
	isVec = TRUE;
	break;
    case LGLSXP:
    case INTSXP:
	vcnt = INT2VEC(xlength(s));
	isVec = TRUE;
	break;
    case REALSXP:
	vcnt = FLOAT2VEC(xlength(s));
	isVec = TRUE;
	break;
    case CPLXSXP:
	vcnt = COMPLEX2VEC(xlength(s));
	isVec = TRUE;
	break;
    case STRSXP:
	vcnt = PTR2VEC(xlength(s));
	PROTECT(dup = Rf_csduplicated(s));
	for (R_xlen_t i = 0; i < xlength(s); i++) {
	    tmp = STRING_ELT(s, i);
	    if(tmp != NA_STRING && !LOGICAL(dup)[i])
		cnt += objectsize(tmp);
	}
	isVec = TRUE;
	UNPROTECT(1);
	break;
    case ANYSXP:
	/* we don't know about these */
	break;
    case VECSXP:
    case EXPRSXP:
    case WEAKREFSXP:
	/* Generic Vector Objects */
	vcnt = PTR2VEC(xlength(s));
	for (R_xlen_t i = 0; i < xlength(s); i++)
	    cnt += objectsize(VECTOR_ELT(s, i));
	isVec = TRUE;
	break;
    case EXTPTRSXP:
	cnt += sizeof(void *);  /* the actual pointer */
	cnt += objectsize(EXTPTR_PROT(s));
	cnt += objectsize(EXTPTR_TAG(s));
	break;
    case RAWSXP:
	vcnt = BYTE2VEC(xlength(s));
	isVec = TRUE;
	break;
    case S4SXP:
	/* Has TAG and ATRIB but no CAR nor CDR */
	cnt += objectsize(TAG(s));
	break;
    default:
	UNIMPLEMENTED_TYPE("object.size", s);
    }
    /* add in node space:
       we need to take into account the rounding up that goes on
       in the node classes. */
    if(isVec) {
	cnt += sizeof(SEXPREC_ALIGN);
	if (vcnt > 16) cnt += 8*vcnt;
	else if (vcnt > 8) cnt += 128;
	else if (vcnt > 6) cnt += 64;
	else if (vcnt > 4) cnt += 48;
	else if (vcnt > 2) cnt += 32;
	else if (vcnt > 1) cnt += 16;
	else if (vcnt > 0) cnt += 8;
    } else cnt += sizeof(SEXPREC);
    /* add in attributes: these are fake for CHARSXPs */
    if(TYPEOF(s) != CHARSXP) cnt += objectsize(ATTRIB(s));
    return(cnt);
}


SEXP objectSize(SEXP x)
{
    return ScalarReal( (double) objectsize(x) );
}
