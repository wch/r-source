/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 2001        The R Development Core Team
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
#include <Fileio.h>
#include <IOStuff.h>
#include <Parse.h>
#include <Rconnections.h>

extern IoBuffer R_ConsoleIob;
/* extern int errno; No longer used */

/* "do_parse" - the user interface input/output to files.

 The internal R_Parse.. functions are defined in ./gram.y (-> gram.c)

 .Internal( parse(file, n, text, prompt) )
 If there is text then that is read and the other arguments are ignored.
*/
SEXP do_parse(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP text, prompt, s, source;
    Rconnection con;
    Rboolean wasopen;
    int ifile, num;
    ParseStatus status;

    checkArity(op, args);
    R_ParseError = 0;
    R_ParseCnt = 0;

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    num = asInteger(CAR(args));				args = CDR(args);
    PROTECT(text = coerceVector(CAR(args), STRSXP));	args = CDR(args);
    prompt = CAR(args);					args = CDR(args);
    source = CAR(args);					args = CDR(args);

    if (prompt == R_NilValue)
	PROTECT(prompt);
    else
	PROTECT(prompt = coerceVector(prompt, STRSXP));

    if (isLogical(source) && length(source) && LOGICAL(source)[0])
    	KeepAllSource = TRUE;

    if (length(text) > 0) {
	if (num == NA_INTEGER)
	    num = -1;
	s = R_ParseVector(text, num, &status);
	if (status != PARSE_OK)
	    errorcall(call, "parse error");
    }
    else if (ifile >= 3) {/* file != "" */
	if (num == NA_INTEGER)
	    num = -1;
	if(!wasopen)
	    if(!con->open(con)) error("cannot open the connection");
	s = R_ParseConn(con, num, &status);
	if(!wasopen) con->close(con);
	if (status != PARSE_OK)
	    errorcall(call, "syntax error on line %d", R_ParseError);
    }
    else {
	if (num == NA_INTEGER)
	    num = 1;
	s = R_ParseBuffer(&R_ConsoleIob, num, &status, prompt);
	if (status != PARSE_OK)
	    errorcall(call, "parse error");
    }

    UNPROTECT(2);
    KeepAllSource = FALSE;
    return s;
}

/* return a protected STRSXP containing source lines from the buffer,
   assumes that end points to the character following the desired source,
   possibly null, but temporarily writeable */

SEXP collectLines(char *buffer, char *end)
{
    int lines = 0;
    char *p, *p0, save;
    SEXP source;

    for (p = buffer; p < end ; p++)
	if (*p == '\n') lines++;
    if ( *(end - 1) != '\n' ) lines++;
    PROTECT(source = allocVector(STRSXP, lines));
    p0 = buffer;
    lines = 0;
    for (p = buffer ; p < end ; p++)
	if ( *p == '\n' ) {
	    save = *p;
	    *p = '\0';
	    SET_STRING_ELT(source, lines++, mkChar(p0));
	    *p = save;
	    p0 = p + 1;
	}
    if ( *(end - 1) != '\n' ) {
	save = *end;
	*end = '\0';
	SET_STRING_ELT(source, lines++, mkChar(p0));
	*end = save;
    }
    return(source);
}

SEXP convertSource(SEXP e)
{
    SEXP source = getAttrib(e, R_SourceSymbol);

    if (!isNull(source) && isInteger(source) && length(source) ==  2) {
	source = collectLines(CHAR(SavedSource)+INTEGER(source)[0], CHAR(SavedSource)+INTEGER(source)[1]);
	setAttrib(e, R_SourceSymbol, source);
	UNPROTECT(1);
    }
    return(e);
}
