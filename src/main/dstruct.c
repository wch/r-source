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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"

/*  append - append second to the tail of first    */
/*           This operation is non-destructive     */
/*           i.e. first and second are duplicated  */

SEXP append(SEXP first, SEXP second)
{
    SEXP e;

    PROTECT(second);
    first = duplicate(first);
    UNPROTECT(1);
    PROTECT(first);
    second = duplicate(second);
    UNPROTECT(1);
    for (e = first; CDR(e) != R_NilValue; e = CDR(e));
    SETCDR(e, second);
    return first;
}


/*  mkPRIMSXP - return a builtin function      */
/*              either "builtin" or "special"  */

SEXP mkPRIMSXP(int offset, int eval)
{
    SEXP result = allocSExp(eval ? BUILTINSXP : SPECIALSXP);
    result->u.primsxp.offset = offset;
    return (result);
}


/*  mkCLOSXP - return a closure with formals f,  */
/*             body b, and environment rho       */

SEXP mkCLOSXP(SEXP formals, SEXP body, SEXP rho)
{
    SEXP c;
    PROTECT(formals);
    PROTECT(body);
    PROTECT(rho);
    c = allocSExp(CLOSXP);
    FORMALS(c) = formals;
    BODY(c) = body;
    if(rho == R_NilValue)
	CLOENV(c) = R_GlobalEnv;
    else
	CLOENV(c) = rho;
    UNPROTECT(3);
    return c;
}


/* mkChar - make a character (CHARSXP) variable */

SEXP mkChar(const char *name)
{
    SEXP c;

    if (streql(name, "NA"))
	return (NA_STRING);
    c = allocString(strlen(name));
    strcpy(CHAR(c), name);
    return c;
}


/*  mkSYMSXP - return a symsxp with the string  */
/*             name inserted in the name field  */

static int ddVal(SEXP name)
{
    char buf[128], *endp, *val;
    int rval;

    strcpy(buf, CHAR(name));
    if( !strncmp(buf,"..",2) && strlen(buf) > 2 ) {
        val = buf; val++; val++;
	rval = strtol(val, &endp, 10);
        if( *endp != '\0')
		return 0;
	else
		return rval;
    }
    return 0;
}

SEXP mkSYMSXP(SEXP name, SEXP value)

{
	SEXP c;
	int i;
	PROTECT(name);
	PROTECT(value);
	i = ddVal(name);
	c = allocSExp(SYMSXP);
	PRINTNAME(c) = name;
	SYMVALUE(c) = value;
	DDVAL(c) = i;
	UNPROTECT(2);
	return c;
}


/*  mkPROMISE - make a promise to evaluate an argument  */

SEXP mkPROMISE(SEXP expr, SEXP rho)
{
    SEXP p;

    PROTECT(expr);
    PROTECT(rho);
    p = allocSExp(PROMSXP);
    PREXPR(p) = expr;
    PRENV(p) = rho;
    PRVALUE(p) = R_UnboundValue;
    PRSEEN(p) = 0;
    UNPROTECT(2);
    return p;
}


/*  length - length of objects  */

int length(SEXP s)
{
    int i;
    switch (TYPEOF(s)) {
    case NILSXP:
	return 0;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case STRSXP:
    case CHARSXP:
    case VECSXP:
    case EXPRSXP:
	return LENGTH(s);
    case LISTSXP:
    case LANGSXP:
    case DOTSXP:
	i = 0;
	while (s != NULL && s != R_NilValue) {
	    i++;
	    s = CDR(s);
	}
	return i;
    case ENVSXP:
	return length(FRAME(s));
    default:
	return 1;
    }
}
