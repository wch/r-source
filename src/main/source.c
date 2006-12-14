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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
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

SEXP attribute_hidden getParseContext()
{
    int i, last = PARSE_CONTEXT_SIZE;
    char context[PARSE_CONTEXT_SIZE+1];

    SEXP ans = R_NilValue, ans2;
    int nn, nnn, nread;
    char c;

    context[last] = '\0';
    for (i=R_ParseContextLast; last>0 ; i--) {
	i = i % PARSE_CONTEXT_SIZE;
	context[--last] = R_ParseContext[i];
	if (!context[last]) {
	    last++;
	    break;
	}
    }
    
    nn = 16; /* initially allocate space for 16 lines */
    nnn = nn;
    PROTECT(ans = allocVector(STRSXP, nn));
    c = context[last];
    nread = 0;
    while(c) {
        nread++;
	if(nread >= nn) {
	    ans2 = allocVector(STRSXP, 2*nn);
	    for(i = 0; i < nn; i++)
		SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
	    nn *= 2;
	    UNPROTECT(1); /* old ans */
	    PROTECT(ans = ans2);
	}
	i = last;
	while((c = context[++i])) {
	    if(c == '\n') break;
	}
	context[i] = '\0';
	SET_STRING_ELT(ans, nread-1, mkChar(context + last));
	last = i+1;
    }
    /* get rid of empty line after last newline */
    if (nread && !length(STRING_ELT(ans, nread-1))) nread--;
    PROTECT(ans2 = allocVector(STRSXP, nread));
    for(i = 0; i < nread; i++)
	SET_STRING_ELT(ans2, i, STRING_ELT(ans, i));
    UNPROTECT(2);
    return ans2;
}    

void attribute_hidden getParseFilename(char* buffer, int buflen)
{
    buffer[0] = '\0';
    if (R_ParseErrorFile && !isNull(R_ParseErrorFile)) {
	SEXP filename;
	PROTECT(filename = findVar(install("filename"), R_ParseErrorFile));
	if (!isNull(filename)) 
	    strncpy(buffer, CHAR(STRING_ELT(filename, 0)), buflen-1);
	UNPROTECT(1);
    }
}

void attribute_hidden parseError(SEXP call, int linenum)
{
    SEXP context = getParseContext();
    int len = length(context);
    char filename[128];
    if (linenum) {
    	getParseFilename(filename, sizeof(filename)-2);
    	if (strlen(filename)) strcpy(filename + strlen(filename), ": ");
    	
	switch (len) {
	case 0: errorcall(call, _("%s%s on line %d"), 
			    filename, R_ParseErrorMsg, linenum); break;
	case 1: errorcall(call, _("%s%s at\n%d: %s"), 
			    filename, R_ParseErrorMsg, linenum, CHAR(STRING_ELT(context, 0))); break;
	default: errorcall(call, _("%s%s at\n%d: %s\n%d: %s"), 
			    filename, R_ParseErrorMsg, linenum-1, CHAR(STRING_ELT(context, len-2)),
			    linenum, CHAR(STRING_ELT(context, len-1))); break;
	}
    } else {
	switch (len) {
	case 0: errorcall(call, _("%s"), R_ParseErrorMsg); break;
	case 1: errorcall(call, _("%s in \"%s\""), 
			    R_ParseErrorMsg, CHAR(STRING_ELT(context, 0))); break;
	default: errorcall(call, _("%s in:\n\"%s\n%s\""), 
			    R_ParseErrorMsg, CHAR(STRING_ELT(context, len-2)),
			    CHAR(STRING_ELT(context, len-1))); break;
	}   
    }
}

/* "do_parse" - the user interface input/output to files.

 The internal R_Parse.. functions are defined in ./gram.y (-> gram.c)

 .Internal( parse(file, n, text, prompt, srcfile) )
 If there is text then that is read and the other arguments are ignored.
*/
SEXP attribute_hidden do_parse(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP text, prompt, s, source;
    Rconnection con;
    Rboolean wasopen;
    int ifile, num;
    ParseStatus status;

    checkArity(op, args);
    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    num = asInteger(CAR(args));				args = CDR(args);
    if (num == 0)
        return(allocVector(EXPRSXP, 0));
    PROTECT(text = coerceVector(CAR(args), STRSXP));	args = CDR(args);
    prompt = CAR(args);					args = CDR(args);
    source = CAR(args);					args = CDR(args);

    if (prompt == R_NilValue)
	PROTECT(prompt);
    else
	PROTECT(prompt = coerceVector(prompt, STRSXP));

    if (length(text) > 0) {
	if (num == NA_INTEGER)
	    num = -1;
	s = R_ParseVector(text, num, &status, source);
	if (status != PARSE_OK) parseError(call, 0);
    }
    else if (ifile >= 3) {/* file != "" */
	if (num == NA_INTEGER)
	    num = -1;
	if(!wasopen)
	    if(!con->open(con)) error(_("cannot open the connection"));
	s = R_ParseConn(con, num, &status, source);
	if(!wasopen) con->close(con);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else {
	if (num == NA_INTEGER)
	    num = 1;
	s = R_ParseBuffer(&R_ConsoleIob, num, &status, prompt, source);
	if (status != PARSE_OK) parseError(call, 0);
    }
    UNPROTECT(2);
    return s;
}


