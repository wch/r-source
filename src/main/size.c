/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2000, 2001  the R Development Core Team
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

#define USE_RINTERNALS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

/* A count of the memory used by an object. The following assumptions
   are made.

   1) this is called from user-level, so only some types of objects are
      important.
   2) an object gets charged for all the space allocated on the heap
      and all the nodes specifically due to it, but not for the 
      space for its name nor for builtins it references
*/

static int objectsize(SEXP s)
{
    int i, cnt = 0, vcnt = 0;
    
    switch (TYPEOF(s)) {
    case NILSXP:
	return(0);
	break;
    case SYMSXP:
	break;
    case LISTSXP:
    case LANGSXP:
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
	break;
    case PROMSXP:
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	break;
    case CHARSXP:
	vcnt = BYTE2VEC(length(s)+1);
	break;
    case LGLSXP:
    case INTSXP:
	vcnt = INT2VEC(length(s));
	break;
    case REALSXP:
	vcnt = FLOAT2VEC(length(s));
	break;
    case CPLXSXP:
	vcnt = COMPLEX2VEC(length(s));
	break;
    case STRSXP:
	vcnt = PTR2VEC(length(s));
	for (i = 0; i < length(s); i++)
	    vcnt += BYTE2VEC(strlen(CHAR(STRING_ELT(s, i))) + 1);
	break;
    case DOTSXP:
	break;
    case VECSXP:
    case EXPRSXP:
	/* Generic Vector Objects */
	vcnt = PTR2VEC(length(s));
	for (i = 0; i < length(s); i++)
	    cnt += objectsize(VECTOR_ELT(s, i));
	break;
    default:
	error("object.size: unknown type %i", TYPEOF(s));
    }
    cnt += 8 * vcnt;
    /* add in node space */
    cnt += sizeof(SEXPREC);
    /* add in attributes */
    cnt += objectsize(ATTRIB(s));
    return(cnt);
}


SEXP do_objectsize(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP ans;
    int cnt = 0;
    
    checkArity(op, args);
    cnt = objectsize(CAR(args));
    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = cnt;
    UNPROTECT(1);
    return ans;
}
