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
#include <config.h>
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
    SET_PRIMOFFSET(result, offset);
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

#ifdef not_used_CheckFormals
    if(isList(formals))
	SET_FORMALS(c, formals);
    else
        error("invalid formal arguments for \"function\"");
#else
    SET_FORMALS(c, formals);
#endif
    if(isList(body) || isLanguage(body) || isSymbol(body)
       || isExpression(body) || isVector(body))
	SET_BODY(c, body);
    else
        error("invalid body argument for \"function\"\n"
	      "Should NEVER happen; please bug.report() [mkCLOSXP]\n");

    if(rho == R_NilValue)
	SET_CLOENV(c, R_GlobalEnv);
    else
	SET_CLOENV(c, rho);
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
    char *buf, *endp;
    int rval;

    buf = CHAR(name);
    if( !strncmp(buf,"..",2) && strlen(buf) > 2 ) {
        buf += 2;
	rval = strtol(buf, &endp, 10);
        if( *endp != '\0')
		return 0;
	else
		return 1;
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
	SET_PRINTNAME(c, name);
	SET_SYMVALUE(c, value);
	SET_DDVAL(c, i);
	UNPROTECT(2);
	return c;
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
