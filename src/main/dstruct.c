/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001-2014  The R Core Team
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

#include "Defn.h"


/*  mkPRIMSXP - return a builtin function      */
/*              either "builtin" or "special"  */

/*  The value produced is cached do avoid the need for GC protection
    in cases where a .Primitive is produced by unserializing or
    reconstructed after a package has clobbered the value assigned to
    a symbol in the base package. */

SEXP attribute_hidden mkPRIMSXP(int offset, int eval)
{
    SEXP result;
    SEXPTYPE type = eval ? BUILTINSXP : SPECIALSXP;
    static SEXP PrimCache = NULL;
    static int FunTabSize = 0;
    
    if (PrimCache == NULL) {
	/* compute the number of entires in R_FunTab */
	while (R_FunTab[FunTabSize].name)
	    FunTabSize++;

	/* allocate and protect the cache */
	PrimCache = allocVector(VECSXP, FunTabSize);
	R_PreserveObject(PrimCache);
    }

    if (offset < 0 || offset >= FunTabSize)
	error("offset is out of R_FunTab range");

    result = VECTOR_ELT(PrimCache, offset);

    if (result == R_NilValue) {
	result = allocSExp(type);
	SET_PRIMOFFSET(result, offset);
	SET_VECTOR_ELT(PrimCache, offset, result);
    }
    else if (TYPEOF(result) != type)
	error("requested primitive type is not consistent with cached value");

    return result;
}

/* This is called by function() {}, where an invalid
   body should be impossible. When called from
   other places (eg do_asfunction) they
   should do this checking in advance */

/*  mkCLOSXP - return a closure with formals f,  */
/*             body b, and environment rho       */

SEXP attribute_hidden mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    SEXP c;
    PROTECT(formals);
    PROTECT(body);
    PROTECT(rho);
    c = allocSExp(CLOSXP);

#ifdef not_used_CheckFormals
    if(isList(formals))
	SET_FORMALS(c, formals);
    else
	error(_("invalid formal arguments for 'function'"));
#else
    SET_FORMALS(c, formals);
#endif
    switch (TYPEOF(body)) {
    case CLOSXP:
    case BUILTINSXP:
    case SPECIALSXP:
    case DOTSXP:
    case ANYSXP:
	error(_("invalid body argument for 'function'"));
	break;
    default:
	SET_BODY(c, body);
	break;
    }

    if(rho == R_NilValue)
	SET_CLOENV(c, R_GlobalEnv);
    else
	SET_CLOENV(c, rho);
    UNPROTECT(3);
    return c;
}

/* mkChar - make a character (CHARSXP) variable -- see Rinlinedfuns.h */

/*  mkSYMSXP - return a symsxp with the string  */
/*             name inserted in the name field  */

static int isDDName(SEXP name)
{
    const char *buf;
    char *endp;

    buf = CHAR(name);
    if( !strncmp(buf, "..", 2) && strlen(buf) > 2 ) {
	buf += 2;
	strtol(buf, &endp, 10); // discard value
	if( *endp != '\0')
	    return 0;
	else
	    return 1;
    }
    return 0;
}

SEXP attribute_hidden mkSYMSXP(SEXP name, SEXP value)

{
    SEXP c;
    int i;
    PROTECT(name);
    PROTECT(value);
    i = isDDName(name);
    c = allocSExp(SYMSXP);
    SET_PRINTNAME(c, name);
    SET_SYMVALUE(c, value);
    SET_DDVAL(c, i);
    UNPROTECT(2);
    return c;
}
