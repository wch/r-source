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

#ifdef HAVE_LIBREADLINE
char *tilde_expand (char *);
#endif

extern int errno;
extern SEXP R_CurrentExpr;
extern SEXP growlist(SEXP, SEXP);
extern SEXP newlist(void);

SEXP parse(FILE *, int);

	/*  do_parse - the user interface input/output to files.  */
	/*  See parse, below, for the internal function.  The     */
	/*  arguments are "file", "number", "text", "prompt".     */
	/*  If there is text then that is read and the other      */
	/*  arguments are ignored.                                */

SEXP do_parse(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, t;
	FILE *fp;
	int num, pstacktop;

	checkArity(op, args);

	pstacktop = R_PPStackTop;

	R_ParseError = 0;
	R_ParseCnt = 0;

	s = CAR(nthcdr(args, 2));
	if (s != R_NilValue) {
		if (!isString(s))
			errorcall(call, "invalid text specification\n");
		R_ParseText = s;
		fp = NULL;
	}
	else {
		R_ParseText = R_NilValue;
		s = CAR(args);
		if (!isString(s))
			errorcall(call, "unable to open specified file for parsing\n");
		if (strlen(CHAR(STRING(s)[0])) == 0) {
			t = CAR(nthcdr(args, 3));
			R_CurrentExpr = R_NilValue;
			if (isString(t))
				yyprompt(CHAR(STRING(t)[0]));
			else
				yyprompt("> ");
			yyparse();
			ClearerrConsole();
			R_PPStackTop = pstacktop;
			return list1(R_CurrentExpr);
		}
		else {
#ifdef HAVE_LIBREADLINE
			fp = R_fopen(tilde_expand(CHAR(STRING(CAR(args))[0])), "r");
#else
			fp = R_fopen(CHAR(STRING(CAR(args))[0]), "r");
#endif
			if (!fp)
				errorcall(call, "unable to open specified file for parsing\n");
		}
	}
	if (isNumeric(CADR(args))) {
		num = asInteger(CADR(args));
		if(num == NA_INTEGER) num = -1;
	}
	else num = -1;

	s = parse(fp, num);
	if(fp) {
		if(R_ParseError) {
			error("an error occurred on line %d of file %s\n",
			R_ParseError, CHAR(STRING(CAR(args))[0]));
		}
		else {
			fclose(fp);
			ResetConsole();
		}
	}

	R_PPStackTop = pstacktop;
	PROTECT(s);
	t = allocVector(EXPRSXP, length(s));
	for(num=0 ; num<LENGTH(t) ; num++) {
		VECTOR(t)[num] = CAR(s);
		s = CDR(s);
	}
	UNPROTECT(1);
	return t;
}

	/*  parse - a function that reads a specified number        */
	/*  of items from a file. If the specified number is        */
	/*  negative then parse reads until an EOF is encountered.  */
	/*  Functions like do_edit rely on parse.  parse returns    */
	/*  a list each element of which is a parsed but            */
	/*  unevaluated expression, evaluation if required          */
	/*  should be handled by the calling function.              */

	/*  If an error in parsing occurs rval is set to nilvalue   */
	/*  and we halt parsing. The global variable R_ParseError   */
	/*  is set to 1 (we could set it to j and then we could     */
	/*  say what line of the file was not correct).             */


SEXP parse(FILE *fp, int number)
{
	int j, pflag;
	SEXP rval = R_NilValue, top;

		/* set the input source */

	if (R_ParseText == R_NilValue) {
		if (R_Console == 0)
			fclose(R_Inputfile);
		R_SetInput(R_FILE);
		R_Inputfile = fp;
	}
	else R_SetInput(R_TEXT);

	R_CurrentExpr = R_NilValue;
	pflag = 3;
	if (number > 0) {
		/* read in number objects from file fp */
		PROTECT(rval = top = allocList(number));
		for (j = 0; j < number; j++, top = CDR(top)) {
		again:	pflag = yyparse();
			if (pflag == 1) {	/* parse error */
				R_ParseError = R_ParseCnt;
				rval = R_NilValue;
				break;
			}
			if (pflag == 2)
				goto again;
			if (pflag == 0 || pflag == 4)
				error("parse: EOF encountered unexpectedly\n");
			if (R_CurrentExpr)
				CAR(top) = R_CurrentExpr;
		}
	}
	else {
		PROTECT(rval = newlist());
		for (j = 0;; j++) {
			pflag = yyparse();
			if (pflag == 1) {
				R_ParseError = R_ParseCnt;
				rval = R_NilValue;
				break;
			}
			if (pflag == 0) {
				rval = CDR(rval);
				break;
			}
			if (R_CurrentExpr)
				rval = growlist(rval, R_CurrentExpr);
		}
	}
	UNPROTECT(1);
	/* reset R_ParseText when we are done parsing */
	R_ParseText = R_NilValue;
	R_SetInput(R_CONSOLE);
	return rval;
}
