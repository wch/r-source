%{
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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

extern SEXP R_CommentSxp;

/* static */ void ResetComment(void);
static void AddComment(SEXP);
static void PushComment(void);
static void PopComment(void);
static int isComment(SEXP);
static void ifpush(void);
static void CheckFormalArgs(SEXP, SEXP);
static int KeywordLookup(char*);

SEXP listAppend(SEXP,SEXP);
SEXP newlist(void);
SEXP growlist(SEXP, SEXP);
SEXP firstarg(SEXP, SEXP);
SEXP nextarg(SEXP, SEXP, SEXP);
SEXP tagarg(SEXP, SEXP);

		/* These routines allocate constants */

SEXP mkString(char *);
SEXP mkInteger(char *);
SEXP mkFloat(char *);
SEXP mkComplex(char *);
SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);

		/* Internal lexer / parser state variables */

static int EatLines;
static int GenerateCode = 0;
static int EndOfFile = 0;
static FILE *InputFile = NULL;
static int (*xxgetc)();
static void (*xxungetc)();

static int newline = 0;			/* Used only for prompting */


	/* Soon to be defunct entry points */

void	R_SetInput(int);
int	R_fgetc(FILE*);
void	uncget(void);

void	yyinit(void);
int	yylex(void);
int	yyerror(char*);
void	yyprompt(char *, ...);
int	yywrap();

	/* Routines used to build the parse tree */

static SEXP xxnullformal(void);
static SEXP xxfirstformal0(SEXP);
static SEXP xxfirstformal1(SEXP, SEXP);
static SEXP xxaddformal0(SEXP, SEXP);
static SEXP xxaddformal1(SEXP, SEXP, SEXP);
static SEXP xxexprlist0();
static SEXP xxexprlist1(SEXP);
static SEXP xxexprlist2(SEXP, SEXP);
static SEXP xxsub0(void);
static SEXP xxsub1(SEXP);
static SEXP xxsymsub0(SEXP);
static SEXP xxsymsub1(SEXP, SEXP);
static SEXP xxnullsub0();
static SEXP xxnullsub1(SEXP);
static SEXP xxsublist1(SEXP);
static SEXP xxsublist2(SEXP, SEXP);
static SEXP xxcond(SEXP);
static SEXP xxifcond(SEXP);
static SEXP xxif(SEXP, SEXP, SEXP);
static SEXP xxifelse(SEXP, SEXP, SEXP, SEXP);
static SEXP xxforcond(SEXP, SEXP);
static SEXP xxfor(SEXP, SEXP, SEXP);
static SEXP xxwhile(SEXP, SEXP, SEXP);
static SEXP xxrepeat(SEXP, SEXP);
static SEXP xxnxtbrk(SEXP);
static SEXP xxfuncall(SEXP, SEXP);
static SEXP xxdefun(SEXP, SEXP, SEXP);
static SEXP xxunary(SEXP, SEXP);
static SEXP xxbinary(SEXP, SEXP, SEXP);
static SEXP xxparen(SEXP, SEXP);
static SEXP xxsubscript(SEXP, SEXP, SEXP);
static SEXP xxexprlist(SEXP, SEXP);
static int xxvalue(SEXP, int);

#define YYSTYPE		SEXP

%}

%token		END_OF_INPUT

%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION LEX_ERROR
%token		LBB ERROR
%token		LEFT_ASSIGN RIGHT_ASSIGN
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR

%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%left		RIGHT_ASSIGN
%nonassoc	'~' TILDE
%left		OR
%left		AND
%left		UNOT NOT
%nonassoc	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/' '%'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$'
%nonassoc	'(' '[' LBB

%%

prog	:	END_OF_INPUT			{ return 0; }
	|	'\n'				{ return xxvalue(NULL,2); }
	|	expr '\n'			{ return xxvalue($1,3); }
	|	expr ';'			{ return xxvalue($1,4); }
	|	error	 			{ YYABORT; }
	;

expr	: 	NUM_CONST			{ $$ = $1; }
	|	STR_CONST			{ $$ = $1; }
	|	NULL_CONST			{ $$ = $1; }
	|	SYMBOL				{ $$ = $1; }

	|	'{' exprlist '}'		{ $$ = xxexprlist($1,$2); }
	|	'(' expr ')'			{ $$ = xxparen($1,$2); }

	|	'-' expr %prec UMINUS		{ $$ = xxunary($1,$2); }
	|	'+' expr %prec UMINUS		{ $$ = xxunary($1,$2); }
	|	'!' expr %prec UNOT		{ $$ = xxunary($1,$2); }
	|	'~' expr %prec TILDE		{ $$ = xxunary($1,$2); }
	|	'?' expr			{ $$ = xxunary($1,$2); }

	|	expr ':'  expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '+'  expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '-' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '*' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '/' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '^' expr 			{ $$ = xxbinary($2,$1,$3); }
	|	expr SPECIAL expr		{ $$ = xxbinary($2,$1,$3); }
	|	expr '%' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr '~' expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr LT expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr LE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr EQ expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr NE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr GE expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr GT expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr AND expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr OR expr			{ $$ = xxbinary($2,$1,$3); }

	|	expr LEFT_ASSIGN expr 		{ $$ = xxbinary($2,$1,$3); }
	|	expr RIGHT_ASSIGN expr 		{ $$ = xxbinary($2,$3,$1); }
	|	FUNCTION '(' formlist ')' cr expr %prec LOW
						{ $$ = xxdefun($1,$3,$6); }
	|	expr '(' sublist ')'		{ $$ = xxfuncall($1,$3); }
	|	IF ifcond expr 			{ $$ = xxif($1,$2,$3); }
	|	IF ifcond expr ELSE expr	{ $$ = xxifelse($1,$2,$3,$5); }
	|	FOR forcond expr %prec FOR 	{ $$ = xxfor($1,$2,$3); }
	|	WHILE cond expr			{ $$ = xxwhile($1,$2,$3); }
	|	REPEAT expr			{ $$ = xxrepeat($1,$2); }
	|	expr LBB sublist ']' ']'	{ $$ = xxsubscript($1,$2,$3); }
	|	expr '[' sublist ']'		{ $$ = xxsubscript($1,$2,$3); }
	|	expr '$' SYMBOL			{ $$ = xxbinary($2,$1,$3); }
	|	expr '$' STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	NEXT				{ $$ = xxnxtbrk($1); }
	|	BREAK				{ $$ = xxnxtbrk($1); }
	;


cond	:	'(' expr ')'			{ $$ = xxcond($2); }
	;

ifcond	:	'(' expr ')'			{ $$ = xxifcond($2); }
	;

forcond :	'(' SYMBOL IN expr ')' 		{ $$ = xxforcond($2,$4); }
	;


exprlist:					{ $$ = xxexprlist0(); }
	|	expr				{ $$ = xxexprlist1($1); }
	|	exprlist ';' expr		{ $$ = xxexprlist2($1,$3); }
	|	exprlist ';'			{ $$ = $1; AddComment(CAR($$));}
	|	exprlist '\n' expr		{ $$ = xxexprlist2($1,$3); }
	|	exprlist '\n'			{ $$ = $1;}
	;

sublist	:	sub				{ $$ = xxsublist1($1); }
	|	sublist cr ',' sub		{ $$ = xxsublist2($1,$4); }
	;

sub	:					{ $$ = xxsub0(); }
	|	expr				{ $$ = xxsub1($1); }
	|	SYMBOL '=' 			{ $$ = xxsymsub0($1); }
	|	SYMBOL '=' expr			{ $$ = xxsymsub1($1,$3); }
	|	STR_CONST '=' 			{ $$ = xxsymsub0($1); }
	|	STR_CONST '=' expr		{ $$ = xxsymsub1($1,$3); }
	|	NULL_CONST '=' 			{ $$ = xxnullsub0(); }
	|	NULL_CONST '=' expr		{ $$ = xxnullsub1($3); }
	;

formlist:					{ $$ = xxnullformal(); }
	|	SYMBOL				{ $$ = xxfirstformal0($1); }
	|	SYMBOL '=' expr			{ $$ = xxfirstformal1($1,$3); }
	|	formlist ',' SYMBOL		{ $$ = xxaddformal0($1,$3); }
	|	formlist ',' SYMBOL '=' expr	{ $$ = xxaddformal1($1,$3,$5); }
	;

cr	:					{ EatLines = 1; }
	;
%%


/*----------------------------------------------------------------------------*/

static int xxvalue(SEXP v, int k)
{
	if(k > 2) UNPROTECT(1);
	R_CurrentExpr = v;
	return k;
}

static SEXP xxnullformal()
{
	SEXP ans;
	PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxfirstformal0(SEXP sym)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = firstarg(R_MissingArg, sym));
	return ans;
}

static SEXP xxfirstformal1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = firstarg(expr, sym));
	return ans;
}

static SEXP xxaddformal0(SEXP formlist, SEXP sym)
{
	SEXP ans;
	UNPROTECT(2);
	CheckFormalArgs(formlist ,sym);
	PROTECT(ans = nextarg(formlist, R_MissingArg, sym));
	return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(3);
	CheckFormalArgs(formlist, sym);
	PROTECT(ans = nextarg(formlist, expr, sym));
	return ans;
}

static SEXP xxexprlist0()
{
	SEXP ans;
	PROTECT(ans = newlist());
	return ans;
}

static SEXP xxexprlist1(SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(1);
	PROTECT(ans = growlist(newlist(), expr));
	return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(2);
	PROTECT(ans = growlist(exprlist, expr));
	return ans;
}

static SEXP xxsub0(void)
{
	SEXP ans;
	PROTECT(ans = lang2(R_MissingArg,R_NilValue));
	return ans;
}

static SEXP xxsub1(SEXP expr)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(expr, R_NilValue));
	return ans;
}

static SEXP xxsymsub0(SEXP sym)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(R_MissingArg, sym));
	return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = tagarg(expr, sym));
	return ans;
}

static SEXP xxnullsub0()
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(R_MissingArg, install("NULL")));
	return ans;
}

static SEXP xxnullsub1(SEXP expr)
{
	SEXP ans = install("NULL");
	UNPROTECT(2);
	PROTECT(ans = tagarg(expr, ans));
	return ans;
}


static SEXP xxsublist1(SEXP sub)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = firstarg(CAR(sub),CADR(sub)));
	return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = nextarg(sublist, CAR(sub), CADR(sub)));
	return ans;
}

static SEXP xxcond(SEXP expr)
{
	EatLines = 1;
	return expr;
}

static SEXP xxifcond(SEXP expr)
{
	ifpush();
	EatLines = 1;
	return expr;
}

static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(ifsym, cond, expr));
	return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
	SEXP ans;
	UNPROTECT(3);
	PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = LCONS(sym, expr));
	EatLines=1;
	return ans;
}

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
	return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(whilesym, cond, body));
	return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(repeatsym, body));
	return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
	PROTECT(keyword = lang1(keyword));
	return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
	SEXP ans;
	if(isString(expr))
		expr = install(CHAR(STRING(expr)[0])); 
	UNPROTECT(2);
	if(length(CDR(args)) == 1 && CADR(args) == R_MissingArg )
		ans = lang1(expr);
	else    
		ans = LCONS(expr, CDR(args));   
	PROTECT(ans);
	return ans;
}       

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{
	SEXP ans;
	AddComment(body);
	UNPROTECT(2);
	ans = lang3(fname, CDR(formals), body); 
	PROTECT(ans);
	PopComment();
	return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(op, arg));
	return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(n1, n2, n3));
	return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(n1, n2));
	return ans;
}

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
	SEXP ans;
	UNPROTECT(2);
	ans = LCONS(a2, LCONS(a1, CDR(a3)));
	PROTECT(ans);
	return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
	SEXP ans;
	UNPROTECT(1);
	TYPEOF(a2) = LANGSXP;
	CAR(a2) = a1;
	PROTECT(ans = a2);
	EatLines = 0;
	return ans;
}

/*----------------------------------------------------------------------------*/

SEXP tagarg(SEXP arg, SEXP tag)
{
	switch (TYPEOF(tag)) {
	case NILSXP:
	case SYMSXP:
	case STRSXP:
		return lang2(arg, tag);
	default:
		error("incorrect tag type\n");
	}
}

/*
 *  Stretchy List Structures
 *
 *  Lists are created and grown using a special dotted pair.
 *  The CAR of the list points to the last cons-cell in the
 *  list and the CDR points to the first.  The list can be
 *  extracted from the pair by taking its CDR, while the CAR
 *  gives fast access to the end of the list.
 */

	/* Create a stretchy-list dotted pair */

SEXP newlist(void)
{
	SEXP s = CONS(R_NilValue, R_NilValue);
	CAR(s) = s;
	return s;
}

	/* Add a new element at the end of a stretchy list */

SEXP growlist(SEXP l, SEXP s)
{
	SEXP tmp;
	PROTECT(l);
	tmp = CONS(s, R_NilValue);
	UNPROTECT(1);
	SETCDR(CAR(l), tmp);
	CAR(l) = tmp;
	return l;
}

/*
 *  Comment Handling
 *
 *  R_CommentSxp is of the same form as an expression list,
 *  each time a new { is encountered a new element is placed
 *  in the R_CommentSxp and when a } is encountered it is
 *  removed.
 *
 *  The following routine is referenced in error.c.
 *  That reference should be removed.
 */

/* static */ void ResetComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

static void PushComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

static void PopComment(void)
{
	R_CommentSxp = CDR(R_CommentSxp);
}

int isComment(SEXP l)
{
	if (isList(l) && isString(CAR(l))
	&& !strncmp(CHAR(STRING(CAR(l))[0]), "#", 1))
		return 1;
	else
		return 0;
}

static void AddComment(SEXP l)
{
	SEXP tcmt, cmt;
	int i, ncmt;

	tcmt = CAR(R_CommentSxp);

		/* Return if there are no comments */

	if (tcmt == R_NilValue || l == R_NilValue)
		return;

		/* Attach the comments as a comment attribute */

	ncmt = length(tcmt);
	cmt = allocVector(STRSXP, ncmt);
	for(i=0 ; i<ncmt ; i++) {
		STRING(cmt)[i] = CAR(tcmt);
		tcmt = CDR(tcmt);
	}
	PROTECT(cmt);
	setAttrib(l, R_CommentSymbol, cmt);
	UNPROTECT(1);
	
		/* Reset the comment accumulator */

	CAR(R_CommentSxp) = R_NilValue;
}

SEXP firstarg(SEXP s, SEXP tag)
{
	SEXP tmp;
	PROTECT(s);
	PROTECT(tag);
	tmp = newlist();
	tmp = growlist(tmp, s);
	TAG(CAR(tmp)) = tag;
	UNPROTECT(2);
	return tmp;
}

SEXP nextarg(SEXP l, SEXP s, SEXP tag)
{
	PROTECT(tag);
	l = growlist(l, s);
	TAG(CAR(l)) = tag;
	UNPROTECT(1);
	return l;
}

/*----------------------------------------------------------------------------*/

	/* Basic File IO */

	/* This code is here because at this particular instant it */
	/* seems closely related to cget(), which appears below.  */

int R_fgetc(FILE *fp)
{
	int c = fgetc(fp);
	return feof(fp) ? R_EOF : c;
}


static char *buf;		/* The input stream buffer */
static char *bufp;		/* Pointer within current buffer */
static int cnt = 0;		/* Pointer to character count */

static char buf1[MAXELTSIZE];	/* File or text buffer */

static char buf0[MAXELTSIZE];	/* Console buffer */
static char *bufp0;		/* Pointer within the console buffer */
static int cnt0 = 0;		/* Characters in the console buffer */

static current_input;

/*
//	Set the input stream for the parser
//	    input = 0	initialize
//	    input = 1	console
//	    input = 2	file
//	    input = 3	text
*/

void R_SetInput(int input)
{
	xxgetc = cget;
	xxungetc = uncget;

	switch (input) {

	case 0:			/* Initialization / Reset */
		cnt = cnt0 = 0;
		buf = buf0;
		bufp = buf0;
		break;

	case 1:			/* Restore console values */
		cnt = cnt0;
		buf = buf0;
		bufp = bufp0;
		R_Console = 1;
		break;

	case 2:			/* Text or file input */
	case 3:
		if(R_Console == 1) {
			cnt0 = cnt;
			bufp0 = bufp;
		}
		cnt = 0;
		buf = buf1;
		bufp = buf1;
		R_Console = 0;
		break;
	}
	current_input = input;
}


/*
//	Fetch a single character from the current input stream.
//	The stream has been set by a call to R_SetInput().
*/

int cget()
{
	if (--cnt < 0) {
		switch(current_input) {

		case 1:
			if (ReadKBD(buf, MAXELTSIZE) == 0) {
				ClearerrConsole();
				return R_EOF;
			}
			break;

		case 2:
			if (fgets(buf, MAXELTSIZE, R_Inputfile) == NULL) {
				ResetConsole();
				return R_EOF;
			}
			break;

		case 3:
			if (R_ParseCnt < LENGTH(R_ParseText)) {
				strcpy(buf, CHAR(STRING(R_ParseText)[(R_ParseCnt)]));
				strcat(buf, "\n");
			}
			else return R_EOF;
			break;

		}
		R_ParseCnt++;
		bufp = buf;
		cnt = strlen(buf);
		cnt--;
	}
	return *bufp++;
}


/*
//	Push n characters back onto the input stream.
//	This is only called when the characters are
//	currently in the input buffer so pushing back
//	beyond the start of the buffer is impossible.
*/

void uncget()
{
	cnt += 1;
	bufp -= 1;
}

/*----------------------------------------------------------------------------*/

/* TODO:
   The function "parse" in source.c needs to be moved here.
   It should work by calling these functions.
   With that change, "newlist" and "growlist" can become
   static local functions */

/*
 *  Parsing Entry Points:
 *
 *  The Following extry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IOBuffers
 *  (i.e. interactve use), files and R character strings.
 *
 *	SEXP R_ParseFile(FILE *fp, int gencode, int *status)
 *
 *	SEXP R_ParseVector(SEXP *text, int gencode, int *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int gencode, int *status)
 *	
 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *	status = 0 - there was no statement to parse
 *		 1 - complete statement
 *		 2 - incomplete statement
 *		 3 - syntax error
 */

SEXP R_ParseFile(FILE *fp, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();

	xxgetc = cget;
	xxungetc = uncget;

	switch(yyparse()) {
	    case 0:		/* End of file */
		*status = 2;
		break;
	    case 1:		/* Syntax error / incomplete */
		if(EndOfFile) *status = 2;
		else *status = 3;
		break;
	    case 2:		/* Empty Line */
		*status = 0;
		break;
	    case 3:		/* Valid expression '\n' terminated */
	    case 4:		/* Valid expression ';' terminated */
		*status = 1;
		break;
	}
	return R_CurrentExpr;
}

SEXP R_ParseVector(SEXP *text, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();
	xxgetc = cget;
	xxungetc = uncget;
	return R_NilValue;
}

SEXP R_ParseBuffer(void *buffer, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();
	xxgetc = cget;
	xxungetc = uncget;
	return R_NilValue;
}

/*----------------------------------------------------------------------------*/
/*
 *  Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  Input is read a line at a time, and, if the
 *  program is in batch mode, each input line is echoed to
 *  standard output after it is read.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  The lexical
 *  analyser maintains a symbol table (in a very messy fashion).
 *
 *  The fact that if statements need to parse differently
 *  depending on whether the statement is being interpreted or
 *  part of the body of a function causes the need for ifpop
 *  and ifpush. When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 */

static int reset = 1;
#ifndef DEBUG_LEX
static
#endif
char *parenp, parenstack[50];

static void ifpush(void)
{
	if (*parenp=='{' || *parenp=='[' || *parenp=='(' || *parenp == 'i')
		*++parenp = 'i';
}

static void ifpop(void)
{
	if (*parenp=='i')
		parenp--;
}

static int typeofnext(void)
{
	int k, c;

	c = cget();
	if (isdigit(c))
		k = 1;
	else if (isalpha(c) || c == '.')
		k = 2;
	else
		k = 3;
	uncget();
	return k;
}

static int nextchar(int expect)
{
	int c = cget();

	if (c == expect)
		return 1;
	else
		uncget();
	return 0;
}

		/* Special Symbols */
		/* Syntactic Keywords + Symbolic Constants */

struct {
	char *name;
	int token;
}
keywords[] = {
	{ "NULL",	NULL_CONST	},
	{ "NA",		NUM_CONST	},
	{ "TRUE",	NUM_CONST	},
	{ "FALSE",	NUM_CONST	},
	{ "GLOBAL.ENV",	NUM_CONST	},
	{ "function",	FUNCTION	},
	{ "while",	WHILE		},
	{ "repeat",	REPEAT		},
	{ "for",	FOR		},
	{ "if",		IF		},
	{ "in",		IN		},
	{ "else",	ELSE		},
	{ "next",	NEXT		},
	{ "break",	BREAK		},
	{ "...",	SYMBOL		},
	{ 0,		0		}
};


	/* KeywordLookup has side effects, it sets yylval */

static int KeywordLookup(char *s)
{
	int i;

	for (i = 0; keywords[i].name; i++) {
		if (strcmp(keywords[i].name, s) == 0) {
			switch (keywords[i].token) {
			case NULL_CONST:
				PROTECT(yylval = R_NilValue);
				EatLines = 0;
				break;
			case NUM_CONST:
				switch(i) {
				case 1:
					PROTECT(yylval = mkNA());
					break;
				case 2:
					PROTECT(yylval = mkTrue());
					break;
				case 3:
					PROTECT(yylval = mkFalse());
					break;
				case 4:
					PROTECT(yylval = R_GlobalEnv);
				}
				EatLines = 0;
				break;
			case FUNCTION:
			case WHILE:
			case REPEAT:
			case FOR:
			case IF:
				EatLines = 1;
				yylval = install(s);
				break;
			case IN:
				EatLines = 1;
				break;
			case ELSE:
				ifpop();
				EatLines = 1;
				break;
			case NEXT:
			case BREAK:
				EatLines = 0;
				yylval = install(s);
				break;
			case SYMBOL:
				PROTECT(yylval = install(s));
				EatLines = 0;
				break;
			}
			return keywords[i].token;
		}
	}
	return 0;
}

static void prompt()
{
	if (R_ParseText == R_NilValue && R_Console == 1)
		yyprompt(CHAR(STRING(GetOption(install("continue"), R_NilValue))[0]));
}

SEXP mkString(char *s)
{
	SEXP t;

	PROTECT(t = allocVector(STRSXP, 1));
	STRING(t)[0] = mkChar(s);
	UNPROTECT(1);
	return t;
}

SEXP mkFloat(char *s)
{
	SEXP t = allocVector(REALSXP, 1);
	REAL(t)[0] = atof(s);
	return t;
}

SEXP mkComplex(char *s)
{
	SEXP t = allocVector(CPLXSXP, 1);
	COMPLEX(t)[0].r = 0;
	COMPLEX(t)[0].i = atof(s);
	return t;
}

SEXP mkNA(void)
{
	SEXP t = allocVector(LGLSXP, 1);
	LOGICAL(t)[0] = NA_LOGICAL;
	return t;
}

SEXP mkTrue(void)
{
	SEXP s = allocVector(LGLSXP, 1);
	LOGICAL(s)[0] = 1;
	return s;
}

SEXP mkFalse(void)
{
	SEXP s = allocVector(LGLSXP, 1);
	LOGICAL(s)[0] = 0;
	return s;
}

void yyinit(void)
{
	newline = 0;
	reset = 1;
}

int yywrap()
{
	return feof(R_Inputfile);
}

#ifdef HAVE_LIBREADLINE
extern char R_prompt_buf[512];
#endif


void yyprompt(char *format, ...)
{
	va_list(ap);
	va_start(ap, format);
#ifdef HAVE_LIBREADLINE
	vsprintf(R_prompt_buf, format, ap);
#else
	REvprintf(format, ap);
#endif
	va_end(ap);
	fflush(stdout);
	RBusy(0);
}

int yyerror(char *s)
{
	int i;

	R_CommentSxp = R_NilValue;
	REprintf("%s", buf);
	for (i = 1; i < bufp - buf; i++) {
		REprintf(" ");
	}
	REprintf("^\n");
	if (R_Console == 0 && R_Inputfile != NULL) {
		fclose(R_Inputfile);
		ResetConsole();
	}
	else {
		FlushConsole();
		REprintf("Error: %s\n", s);
	}
	newline = 0;
	reset = 1;
	cnt = 0;
	return 0;
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
	int i;

	while( formlist != R_NilValue ) {
		if(TAG(formlist) == new ) {
			REprintf("%s", buf);
			for (i = 2; i < bufp - buf; i++) 
				REprintf(" ");
			REprintf("^\n");
			newline = 0;
			reset = 1;
			cnt = 0;
			error("Repeated formal argument.\n");
		}
		formlist=CDR(formlist);
	}
}

int yylex()
{
	SEXP f;
	int c, quote, kw;
	char *p, yytext[MAXELTSIZE];

	if (newline) {
		newline = 0;
		prompt();
	}

    again:
	if (reset) {
		parenp = parenstack;
		*parenp = ' ';
		reset = 0;
		EatLines = 0;
		ResetComment();
	}

	while ((c = xxgetc()) == ' ' || c == '\t' || c == '');

	if (c == '#') {
		p = yytext;
		*p++ = c;
		while ((c = xxgetc()) != '\n' && c != R_EOF)
			*p++ = c;
		*p = '\0';
		if(R_CommentSxp != R_NilValue) {
			f = mkChar(yytext);
			f = CONS(f, R_NilValue);
			CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
		}
	}


	if (c == R_EOF) return END_OF_INPUT;

		/* This code deals with context sensitivity to      */
		/* newlines.  The main problem is finding out       */
		/* whether a newline is followed by an ELSE clause. */
		/* This is only of importance if we are inside one  */
		/* of "(", "[", or "{".			     */

	if (c == '\n') {
		if (EatLines || *parenp == '[' || *parenp == '(') {
			prompt();
			goto again;
		}
		if (*parenp == 'i') {
			prompt();
			while ((c = xxgetc()) == ' ' || c == '\t');
			if (c == R_EOF) {
				error("unexpected end-of-file in parse\n");
			}
			if (c == '#') {
				p = yytext;
				*p++ = c;
				while ((c = xxgetc()) != '\n' && c != R_EOF)
					*p++ = c;
				*p = '\0';
				if(R_CommentSxp != R_NilValue) {
					f = mkChar(yytext);
					f = CONS(f, R_NilValue);
					CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
				}
			}
			if (c == '\n') {
				prompt();
				xxungetc();
				goto again;
			}
			if (c == '}' || c == ')' || c == ']' ) {
				while (*parenp == 'i')
					ifpop();
				parenp--;
				return c;
			}
			if (c == ',') {
				ifpop();
				return c;
			}
			xxungetc();
			if (!strncmp(bufp, "else", 4) && !isalnum(bufp[4]) && bufp[4] != '.') {
				EatLines = 1;
				bufp += 4;
				cnt -= 4;
				ifpop();
				return ELSE;
			}
			ifpop();
	 	   }
		else newline = 1;
		return '\n';
	}

		/* These are needed because both ";" and "," can */
		/* end an "if" clause without a newline.  Ifpop  */
		/* only does its thing in the right context.     */

	if (c == ';' || c == ',') {
		ifpop();
		return c;
	}

		/* Either digits or symbols can start with a "." */
		/* so we need to decide which it is and jump to  */
		/* the correct spot. */

	if (c == '.') {
		kw = typeofnext();
		if (kw >= 2) goto symbol;
	}

		/* literal numbers */

	if (c == '.' || isdigit(c)) {
		int seendot = (c == '.');
		int seenexp = 0;
		p = yytext;
		*p++ = c;
		while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E') {
			if (c == 'E' || c == 'e') {
				if (seenexp)
					break;
				seenexp = 1;
				seendot = 1;
				*p++ = c;
				c = xxgetc();
				if (!isdigit(c) && c != '+' && c != '-')
					break;
			}
			if (c == '.') {
				if (seendot)
					break;
				seendot = 1;
			}
			*p++ = c;
		}
		*p = '\0';
		if(c == 'i') {
			PROTECT(yylval = mkComplex(yytext));
		}
		else {
			PROTECT(yylval = mkFloat(yytext));
			xxungetc();
		}
		EatLines = 0;
		return NUM_CONST;
	}

	/* literal strings */

	if (c == '\"' || c == '\'') {
		quote = c;
		p = yytext;
		while ((c = xxgetc()) != R_EOF && c != quote) {
			if (c == '\n') {
				xxungetc();
				return ERROR;
			}
			if (c == '\\') {
				c = xxgetc();
				switch (c) {
				case 'a':
					c = '\a';
					break;
				case 'b':
					c = '\b';
					break;
				case 'f':
					c = '\f';
					break;
				case 'n':
					c = '\n';
					break;
				case 'r':
					c = '\r';
					break;
				case 't':
					c = '\t';
					break;
				case 'v':
					c = '\v';
					break;
				case '\\':
					c = '\\';
					break;
				}
			}
			*p++ = c;
		}
		*p = '\0';
		PROTECT(yylval = mkString(yytext));
		EatLines = 0;
		return STR_CONST;
	}

	/* special functions */
	if (c == '%') {
		p = yytext;
		*p++ = c;
		while ((c = xxgetc()) != R_EOF && c != '%') {
			if (c == '\n') {
				xxungetc();
				return ERROR;
			}
			*p++ = c;
		}
		if (c == '%')
			*p++ = c;
		*p++ = '\0';
		yylval = install(yytext);
		EatLines=1;
		return SPECIAL;
	}


	/* functions, constants and variables */

	/* gag, barf, but the punters want it */
	if (c == '_') {
		EatLines = 1;
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

    symbol:
	if (c == '.' || isalpha(c)) {
		p = yytext;
		do {
			*p++ = c;
		} while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.'));
		xxungetc();
		*p = '\0';

		if ((kw = KeywordLookup(yytext))) {
			if(kw == FUNCTION) PushComment();
			return kw;
		}

		PROTECT(yylval = install(yytext));
		EatLines = 0;
		return SYMBOL;
	}

	/* compound tokens */

	switch (c) {
	case '<':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install("<=");
			return LE;
		}
		if (nextchar('-')) {
			yylval = install("<-");
			return LEFT_ASSIGN;
		}
		if (nextchar('<'))
			if (nextchar('-')) {
				yylval = install("<<-");
				return LEFT_ASSIGN;
			}
			else
				return ERROR;
		yylval = install("<");
		return LT;
	case '-':
		EatLines = 1;
		if (nextchar('>'))
			if (nextchar('>')) {
				yylval = install("<<-");
				return RIGHT_ASSIGN;
			}
			else {
				yylval = install("<-");
				return RIGHT_ASSIGN;
			}
		yylval = install("-");
		return '-';
	case '>':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install(">=");
			return GE;
		}
		yylval = install(">");
		return GT;
	case '!':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install("!=");
			return NE;
		}
		yylval = install("!");
		return '!';
	case '=':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install("==");
			return EQ;
		}
		return '=';
	case ':':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install(":=");
			return LEFT_ASSIGN;
		}
		yylval = install(":");
		return ':';
	case '&':
		EatLines = 1;
		if (nextchar('&')) {
			yylval = install("&&");
			return AND;
		}
		yylval = install("&");
		return AND;
	case '|':
		EatLines = 1;
		if (nextchar('|')) {
			yylval = install("||");
			return OR;
		}
		yylval = install("|");
		return OR;
	case '{':
		*++parenp = c;
		yylval = install("{");
		PushComment();
		return c;
	case '}':
		ifpop();
		if(*parenp == '{')
			PopComment();
		parenp--;
		return c;
	case '(':
		*++parenp = c;
		yylval = install("(");
		return c;
	case ')':
		EatLines = 0;
		ifpop();
		parenp--;
		return c;
	case '[':
		*++parenp = c;
		if (nextchar('[')) {
			*++parenp = c;
			yylval = install("[[");
			return LBB;
		}
		yylval = install("[");
		return c;
	case ']':
		ifpop();
		EatLines = 0;
		parenp--;
		return c;
	case '?':
		EatLines = 1;
		strcpy(yytext, "help");
		yylval = install(yytext);
		return c;
	case '*':
		EatLines=1;
		if (nextchar('*'))
			c='^';
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	case '+':
	case '/':
	case '^':
	case '~':
	case '$':
		EatLines = 1;
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}
