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
    SEXP text, prompt, s;
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
    if (prompt == R_NilValue)
	PROTECT(prompt);
    else
	PROTECT(prompt = coerceVector(prompt, STRSXP));

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
    return s;
}


