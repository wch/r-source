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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "Fileio.h"
#include "IOStuff.h"
#include "Parse.h"

extern IoBuffer R_ConsoleIob;
extern int errno;

static int ValidFileSpec(SEXP f)
{
	if(isString(f) && length(f) > 0 && CHAR(STRING(f)[0])[0])
		return 1;
	return 0;
}

	/*  do_parse - the user interface input/output to files.  */
	/*  See parse, below, for the internal function.  The     */
	/*  arguments are "file", "number", "text", "prompt".     */
	/*  If there is text then that is read and the other      */
	/*  arguments are ignored.                                */

SEXP do_parse(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP file, text, prompt, s;
	FILE *fp;
	int num, pstacktop, status;

	checkArity(op, args);

	pstacktop = R_PPStackTop;
	R_ParseError = 0;
	R_ParseCnt = 0;

	PROTECT(file = coerceVector(CAR(args), STRSXP)); args = CDR(args);
	num = asInteger(CAR(args)); args = CDR(args);
	PROTECT(text = coerceVector(CAR(args), STRSXP)); args = CDR(args);
	prompt = CAR(args);
	if(prompt == R_NilValue) PROTECT(prompt);
	else PROTECT(prompt = coerceVector(prompt, STRSXP));
	args = CDR(args);

	if (length(text) > 0) {
		if(num == NA_INTEGER) num = -1;
		s = R_ParseVector(text, num, &status);
		if(status != PARSE_OK)
			errorcall(call, "parse error\n");
		UNPROTECT(3);
		return s;
	}
	else if(ValidFileSpec(file)) {
		if(num == NA_INTEGER) num = -1;
		fp = R_fopen(R_ExpandFileName(CHAR(STRING(file)[0])), "r");
		if (!fp) errorcall(call, "unable to open file for parsing\n");
		s = R_ParseFile(fp, num, &status);
		fclose(fp);
		if(status != PARSE_OK)
			errorcall(call, "syntax error on line %d\n",
				R_ParseError);
		UNPROTECT(3);
		return s;
	}
	else {
		if(num == NA_INTEGER) num = 1;
		s = R_ParseBuffer(&R_ConsoleIob, num, &status, prompt);
		if(status != PARSE_OK)
			errorcall(call, "parse error\n");
		UNPROTECT(3);
		return s;
	}
}
