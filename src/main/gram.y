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
#include "IOSupport.h"
#include "Parse.h"

	/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

	/* Functions used in the parsing process */

static void	AddComment(SEXP);
static void	CheckFormalArgs(SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);
static SEXP	GrowList(SEXP, SEXP);
static void	IfPush(void);
static int	IsComment(SEXP);
static int	KeywordLookup(char*);
static SEXP	NewList(void);
static SEXP	NextArg(SEXP, SEXP, SEXP);
static void	PopComment(void);
static void	PushComment(void);
static void	ResetComment(void);
static SEXP	TagArg(SEXP, SEXP);


	/* These routines allocate constants */

SEXP		mkComplex(char *);
SEXP		mkFalse(void);
SEXP		mkFloat(char *);
SEXP		mkInteger(char *);
SEXP		mkNA(void);
SEXP		mkString(char *);
SEXP		mkTrue(void);

	/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	(*xxgetc)();
static int	(*xxungetc)();

	/* Soon to be defunct entry points */

void		R_SetInput(int);
int		R_fgetc(FILE*);

	/* Routines used to build the parse tree */

static SEXP	xxnullformal(void);
static SEXP	xxfirstformal0(SEXP);
static SEXP	xxfirstformal1(SEXP, SEXP);
static SEXP	xxaddformal0(SEXP, SEXP);
static SEXP	xxaddformal1(SEXP, SEXP, SEXP);
static SEXP	xxexprlist0();
static SEXP	xxexprlist1(SEXP);
static SEXP	xxexprlist2(SEXP, SEXP);
static SEXP	xxsub0(void);
static SEXP	xxsub1(SEXP);
static SEXP	xxsymsub0(SEXP);
static SEXP	xxsymsub1(SEXP, SEXP);
static SEXP	xxnullsub0();
static SEXP	xxnullsub1(SEXP);
static SEXP	xxsublist1(SEXP);
static SEXP	xxsublist2(SEXP, SEXP);
static SEXP	xxcond(SEXP);
static SEXP	xxifcond(SEXP);
static SEXP	xxif(SEXP, SEXP, SEXP);
static SEXP	xxifelse(SEXP, SEXP, SEXP, SEXP);
static SEXP	xxforcond(SEXP, SEXP);
static SEXP	xxfor(SEXP, SEXP, SEXP);
static SEXP	xxwhile(SEXP, SEXP, SEXP);
static SEXP	xxrepeat(SEXP, SEXP);
static SEXP	xxnxtbrk(SEXP);
static SEXP	xxfuncall(SEXP, SEXP);
static SEXP	xxdefun(SEXP, SEXP, SEXP);
static SEXP	xxunary(SEXP, SEXP);
static SEXP	xxbinary(SEXP, SEXP, SEXP);
static SEXP	xxparen(SEXP, SEXP);
static SEXP	xxsubscript(SEXP, SEXP, SEXP);
static SEXP	xxexprlist(SEXP, SEXP);
static int	xxvalue(SEXP, int);

#define YYSTYPE		SEXP

%}

%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION
%token		LEFT_ASSIGN RIGHT_ASSIGN LBB
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
	if(GenerateCode)
		PROTECT(ans = FirstArg(R_MissingArg, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxfirstformal1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = FirstArg(expr, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxaddformal0(SEXP formlist, SEXP sym)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode) {
		CheckFormalArgs(formlist ,sym);
		PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(3);
	if(GenerateCode) {
		CheckFormalArgs(formlist, sym);
		PROTECT(ans = NextArg(formlist, expr, sym));
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist0()
{
	SEXP ans;
	if(GenerateCode)
		PROTECT(ans = NewList());
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist1(SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = GrowList(NewList(), expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = GrowList(exprlist, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsub0(void)
{
	SEXP ans;
	if(GenerateCode)
		PROTECT(ans = lang2(R_MissingArg,R_NilValue));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsub1(SEXP expr)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, R_NilValue));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsymsub0(SEXP sym)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(R_MissingArg, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnullsub0()
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(R_MissingArg, install("NULL")));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnullsub1(SEXP expr)
{
	SEXP ans = install("NULL");
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, ans));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}


static SEXP xxsublist1(SEXP sub)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxcond(SEXP expr)
{
	EatLines = 1;
	return expr;
}

static SEXP xxifcond(SEXP expr)
{
	EatLines = 1;
	return expr;
}

static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(ifsym, cond, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
	SEXP ans;
	UNPROTECT(3);
	if(GenerateCode)
		PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
	SEXP ans;
	EatLines = 1;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = LCONS(sym, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(whilesym, cond, body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(repeatsym, body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
	if(GenerateCode)
		PROTECT(keyword = lang1(keyword));
	else
		PROTECT(keyword = R_NilValue);
	return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
	SEXP ans;
	if(GenerateCode) {
		if(isString(expr))
			expr = install(CHAR(STRING(expr)[0])); 
		UNPROTECT(2);
		if(length(CDR(args)) == 1 && CADR(args) == R_MissingArg )
			ans = lang1(expr);
		else    
			ans = LCONS(expr, CDR(args));   
		PROTECT(ans);
	}
	else {
		UNPROTECT(2);
		PROTECT(ans = R_NilValue);
	}
	return ans;
}       

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{
	SEXP ans;
	AddComment(body);
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(fname, CDR(formals), body)); 
	else
		PROTECT(ans = R_NilValue);
	PopComment();
	return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(op, arg));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(n1, n2, n3));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(n1, n2));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = LCONS(a2, LCONS(a1, CDR(a3))));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
	SEXP ans;
	UNPROTECT(1);
	EatLines = 0;
	if(GenerateCode) {
		TYPEOF(a2) = LANGSXP;
		CAR(a2) = a1;
		PROTECT(ans = a2);
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

/*----------------------------------------------------------------------------*/

static SEXP TagArg(SEXP arg, SEXP tag)
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

static SEXP NewList(void)
{
	SEXP s = CONS(R_NilValue, R_NilValue);
	CAR(s) = s;
	return s;
}

	/* Add a new element at the end of a stretchy list */

static SEXP GrowList(SEXP l, SEXP s)
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

static void ResetComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

static void PushComment(void)
{
	if(GenerateCode)
		R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

static void PopComment(void)
{
	if(GenerateCode)
		R_CommentSxp = CDR(R_CommentSxp);
}

int IsComment(SEXP l)
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

	if(GenerateCode) {
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
}

static SEXP FirstArg(SEXP s, SEXP tag)
{
	SEXP tmp;
	PROTECT(s);
	PROTECT(tag);
	tmp = NewList();
	tmp = GrowList(tmp, s);
	TAG(CAR(tmp)) = tag;
	UNPROTECT(2);
	return tmp;
}

static SEXP NextArg(SEXP l, SEXP s, SEXP tag)
{
	PROTECT(tag);
	l = GrowList(l, s);
	TAG(CAR(l)) = tag;
	UNPROTECT(1);
	return l;
}

/*----------------------------------------------------------------------------*/

	/* Basic File IO */

	/* This code is here because at this particular instant it */
	/* seems closely related to cget(), which appears below.  */
	/* But now it doesn't.  Move this to iosupport.c or trash it */

int R_fgetc(FILE *fp)
{
	int c = fgetc(fp);
	return feof(fp) ? R_EOF : c;
}


/*----------------------------------------------------------------------------*/

/*
 *  Parsing Entry Points:
 *
 *  The Following entry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IOBuffers
 *  (i.e. interactve use), files and R character strings.

 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *  The following routines parse a single expression:
 *
 *	SEXP R_Parse1File(FILE *fp, int gencode, int *status)
 *
 *	SEXP R_Parse1Vector(TextBuffer *text, int gencode, int *status)
 *
 *	SEXP R_Parse1Buffer(IOBuffer *buffer, int gencode, int *status)
 *	
 *  The success of the parse is indicated as folllows:
 *
 *	status = PARSE_NULL       - there was no statement to parse
 *		 PARSE_OK	 - complete statement
 *		 PARSE_INCOMPLETE - incomplete statement
 *		 PARSE_ERROR      - syntax error
 *		 PARSE_EOF	- end of file
 *
 *  The following routines parse several expressions and return
 *  their values in a single expression vector.
 *
 *	SEXP R_ParseFile(FILE *fp, int n, int *status)
 *
 *	SEXP R_ParseVector(TextBuffer *text, int n, int *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int n, int *status)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing
 *  failed for some reason.
 */

static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[50], *contextp;

static SEXP ParseInit()
{
	contextp = contextstack;
	*contextp = ' ';
	SavedToken = 0;
	SavedLval = R_NilValue;
	EatLines = 0;
	EndOfFile = 0;
	ResetComment();
}

static int file_getc(void)
{
	int c = R_fgetc(R_Inputfile);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	if(c == '\n') R_ParseError += 1;
	return c;
}

static int file_ungetc(int c)
{
	if(c == '\n') R_ParseError -= 1;
	return ungetc(c, R_Inputfile);
}

SEXP R_Parse1File(FILE *fp, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	R_Inputfile = fp;
	xxgetc = file_getc;
	xxungetc = file_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	R_Inputfile = NULL;
	return R_CurrentExpr;
}

static IoBuffer *iob;

static int buffer_getc()
{
	int c = R_IoBufferGetc(iob);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	else return c;
}

static int buffer_ungetc(int c)
{
	return R_IoBufferUngetc(c, iob);
}

SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	iob = buffer;
	xxgetc = buffer_getc;
	xxungetc = buffer_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		if(EndOfFile == 2) *status = PARSE_INCOMPLETE;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	return R_CurrentExpr;
}

static TextBuffer *txtb;

static int text_getc()
{
	int c = R_TextBufferGetc(txtb);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	else return c;
}

static int text_ungetc(int c)
{
	return R_TextBufferUngetc(c, txtb);
}

SEXP R_Parse1Vector(TextBuffer *textb, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	txtb = textb;
	xxgetc = text_getc;
	xxungetc = text_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	return R_CurrentExpr;
}

SEXP R_ParseFile(FILE *fp, int n, int *status)
{
	SEXP rval, t;
	int i;

	R_ParseError = 1;
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			t = R_Parse1File(fp, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
				break;
			    case PARSE_INCOMPLETE:
			    case PARSE_ERROR:
			    case PARSE_EOF:
				rval = R_NilValue;
				break;
			}
		}
		UNPROTECT(1);
		return rval;
	}
	else {
		PROTECT(t = NewList());
		for(;;) {
			rval = R_Parse1File(fp, 1, status);
			switch(*status) {
			case PARSE_NULL:
				break;
			case PARSE_OK:
				t = GrowList(t, rval);
				break;
			case PARSE_INCOMPLETE:
			case PARSE_ERROR:
				UNPROTECT(1);
				return R_NilValue;
				break;
			case PARSE_EOF:
				t = CDR(t);
				rval = allocVector(EXPRSXP, length(t));
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
					t = CDR(t);
				}
				UNPROTECT(1);
				*status = PARSE_OK;
				return rval;
				break;
			}
		}
	}
}

SEXP R_ParseVector(SEXP text, int n, int *status)
{
	SEXP rval, t;
	TextBuffer textb;
	int i;

	R_TextBufferInit(&textb, text);
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			t = R_Parse1Vector(&textb, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
				break;
			    case PARSE_INCOMPLETE:
			    case PARSE_ERROR:
			    case PARSE_EOF:
				rval = R_NilValue;
				break;
			}
		}
		UNPROTECT(1);
		R_TextBufferFree(&textb);
		return rval;
	}
	else {
		PROTECT(t = NewList());
		for(;;) {
			rval = R_Parse1Vector(&textb, 1, status);
			switch(*status) {
			case PARSE_NULL:
				break;
			case PARSE_OK:
				t = GrowList(t, rval);
				break;
			case PARSE_INCOMPLETE:
			case PARSE_ERROR:
				R_TextBufferFree(&textb);
				UNPROTECT(1);
				return R_NilValue;
				break;
			case PARSE_EOF:
				R_TextBufferFree(&textb);
				t = CDR(t);
				rval = allocVector(EXPRSXP, length(t));
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
					t = CDR(t);
				}
				UNPROTECT(1);
				*status = PARSE_OK;
				return rval;
				break;
			}
		}
	}
}

static int prompt_type;

static char *Prompt(SEXP prompt, int type)
{
	if(type == 1) {
		if(length(prompt) <= 0) {
			return (char*)CHAR(STRING(GetOption(install("prompt"),
				R_NilValue))[0]);
		}
		else
			return CHAR(STRING(prompt)[0]);
	}
	else {
		return (char*)CHAR(STRING(GetOption(install("continue"),
			R_NilValue))[0]);
	}
}

SEXP R_ParseBuffer(IoBuffer *buffer, int n, int *status, SEXP prompt)
{
	SEXP rval, t;
	char *bufp, buf[1024];
	int c, i, prompt_type = 1;

	R_IoBufferWriteReset(buffer);
	buf[0] = '\0';
	bufp = buf;
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			if(!*bufp) {
				if(R_ReadConsole(Prompt(prompt, prompt_type),
					buf, 1024, 1) == 0) return;
				bufp = buf;
			}
			while(c = *bufp++) {
				R_IoBufferPutc(c, buffer);
				if(c == ';' || c == '\n') {
					break;
				}
			}
			t = R_Parse1Buffer(buffer, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
				break;
			    case PARSE_INCOMPLETE:
			    case PARSE_ERROR:
			    case PARSE_EOF:
				rval = R_NilValue;
				break;
			}
		}
		UNPROTECT(1);
		R_IoBufferWriteReset(buffer);
		return rval;
	}
	else {
		PROTECT(t = NewList());
		for(;;) {
			if(!*bufp) {
				if(R_ReadConsole(Prompt(prompt, prompt_type),
					buf, 1024, 1) == 0) return;
				bufp = buf;
			}
			while(c = *bufp++) {
				R_IoBufferPutc(c, buffer);
				if(c == ';' || c == '\n') {
					break;
				}
			}
			rval = R_Parse1Buffer(buffer, 1, status);
			switch(*status) {
			case PARSE_NULL:
				break;
			case PARSE_OK:
				t = GrowList(t, rval);
				break;
			case PARSE_INCOMPLETE:
			case PARSE_ERROR:
				R_IoBufferWriteReset(buffer);
				UNPROTECT(1);
				return R_NilValue;
				break;
			case PARSE_EOF:
				R_IoBufferWriteReset(buffer);
				t = CDR(t);
				rval = allocVector(EXPRSXP, length(t));
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
					t = CDR(t);
				}
				UNPROTECT(1);
				*status = PARSE_OK;
				return rval;
				break;
			}
		}
	}
}


/*----------------------------------------------------------------------------
 *
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
 *  and IfPush.  When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 *
 *----------------------------------------------------------------------------*/

static void IfPush(void)
{
	if ( *contextp==LBRACE || *contextp=='['
	  || *contextp=='(' || *contextp == 'i')
		*++contextp = 'i';
}

static void ifpop(void)
{
	if (*contextp=='i')
		*contextp-- = 0;
}

static int typeofnext(void)
{
	int k, c;

	c = xxgetc();
	if (isdigit(c))
		k = 1;
	else if (isalpha(c) || c == '.')
		k = 2;
	else
		k = 3;
	xxungetc(c);
	return k;
}

static int nextchar(int expect)
{
	int c = xxgetc();

	if (c == expect)
		return 1;
	else
		xxungetc(c);
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
				break;
			case FUNCTION:
			case WHILE:
			case REPEAT:
			case FOR:
			case IF:
			case NEXT:
			case BREAK:
				yylval = install(s);
				break;
			case IN:
			case ELSE:
				break;
			case SYMBOL:
				PROTECT(yylval = install(s));
				break;
			}
			return keywords[i].token;
		}
	}
	return 0;
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

int yyerror(char *s)
{
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
	int i;

	while( formlist != R_NilValue ) {
		if(TAG(formlist) == new ) {
			error("Repeated formal argument.\n");
		}
		formlist=CDR(formlist);
	}
}

static char yytext[MAXELTSIZE];

static int SkipSpace(void)
{
	int c;
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '')
		/* nothing */;
	return c;
}

	/* Note that with interactive use, EOF cannot occur inside */
	/* a comment.  However, semicolons inside comments make it */
	/* appear that this does happen.  For this reason we use the */
	/* special assignment EndOfFile=2 to indicate that this is */
	/* going on.  This is detected and dealt with in Parse1Buffer. */

static int SkipComment(void)
{
	char *p;
	SEXP f;
	int c;

	p = yytext;
	*p++ = '#';
	while ((c = xxgetc()) != '\n' && c != R_EOF)
		*p++ = c;
	*p = '\0';
	if(GenerateCode && R_CommentSxp != R_NilValue) {
		f = mkChar(yytext);
		f = CONS(f, R_NilValue);
		CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
	}
	if(c == R_EOF) EndOfFile = 2;
	return c;
}

static int NumericValue(int c)
{
	int seendot = (c == '.');
	int seenexp = 0;
	char *p = yytext;
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
		yylval = mkComplex(yytext);
	}
	else {
		xxungetc(c);
		yylval = mkFloat(yytext);
	}
	PROTECT(yylval);
	return NUM_CONST;
}

static int StringValue(int c)
{
	int quote = c;
	char *p = yytext;
	while ((c = xxgetc()) != R_EOF && c != quote) {
		if (c == '\n') {
			xxungetc(c);
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
	return STR_CONST;
}

static int SpecialValue(int c)
{
	char *p = yytext;
	*p++ = c;
	while ((c = xxgetc()) != R_EOF && c != '%') {
		if (c == '\n') {
			xxungetc(c);
			return ERROR;
		}
		*p++ = c;
	}
	if (c == '%')
		*p++ = c;
	*p++ = '\0';
	yylval = install(yytext);
	return SPECIAL;
}

static int SymbolValue(int c)
{
	int kw;
	char *p = yytext;
	do {
		*p++ = c;
	}
	while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.'));
	xxungetc(c);
	*p = '\0';
	if ((kw = KeywordLookup(yytext))) {
		if(kw == FUNCTION) PushComment();
		return kw;
	}
	PROTECT(yylval = install(yytext));
	return SYMBOL;
}

	/* Split the input stream into tokens. */
	/* This is the lowest of the parsing levels. */


static int token()
{
	int c, kw;
	char *p;

	if(SavedToken) {
		c = SavedToken;
		yylval = SavedLval;
		SavedLval = R_NilValue;
		SavedToken = 0;
		return c;
	}
		
    again:

	c = SkipSpace();

	if (c == '#') c = SkipComment();

	if (c == R_EOF) return END_OF_INPUT;

		/* Either digits or symbols can start with a "." */
		/* so we need to decide which it is and jump to  */
		/* the correct spot. */

	if (c == '.') {
		kw = typeofnext();
		if (kw >= 2) goto symbol;
	}

		/* literal numbers */

	if (c == '.' || isdigit(c)) return NumericValue(c);

		/* literal strings */

	if (c == '\"' || c == '\'') return StringValue(c);

		/* special functions */

	if (c == '%') return SpecialValue(c);

		/* functions, constants and variables */

    symbol:

	if (c == '.' || isalpha(c)) return SymbolValue(c);

		/* gag, barf, but the punters want it */

	if (c == '_') {
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

		/* compound tokens */

	switch (c) {
	case '<':
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
		if (nextchar('=')) {
			yylval = install(">=");
			return GE;
		}
		yylval = install(">");
		return GT;
	case '!':
		if (nextchar('=')) {
			yylval = install("!=");
			return NE;
		}
		yylval = install("!");
		return '!';
	case '=':
		if (nextchar('=')) {
			yylval = install("==");
			return EQ;
		}
		return '=';
	case ':':
		if (nextchar('=')) {
			yylval = install(":=");
			return LEFT_ASSIGN;
		}
		yylval = install(":");
		return ':';
	case '&':
		if (nextchar('&')) {
			yylval = install("&&");
			return AND;
		}
		yylval = install("&");
		return AND;
	case '|':
		if (nextchar('|')) {
			yylval = install("||");
			return OR;
		}
		yylval = install("|");
		return OR;
	case LBRACE:
		yylval = install("{");
		return c;
	case RBRACE:
		return c;
	case '(':
		yylval = install("(");
		return c;
	case ')':
		return c;
	case '[':
		if (nextchar('[')) {
			yylval = install("[[");
			return LBB;
		}
		yylval = install("[");
		return c;
	case ']':
		return c;
	case '?':
		strcpy(yytext, "help");
		yylval = install(yytext);
		return c;
	case '*':
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
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}

int yylex()
{
	int tok;

again:

	tok = token();

		/* Newlines must be handled in a context */
		/* sensitive way.  The following block of */
		/* deals directly with newlines in the */
		/* body of "if" statements. */

	if (tok == '\n') {

		if (EatLines || *contextp == '[' || *contextp == '(')
			goto again;

		/* The essence of this is that in the body of */
		/* an "if", any newline must be checked to */
		/* see if it is followed by an "else". */
		/* such newlines are discarded. */

		if (*contextp == 'i') {

			/* Find the next non-newline token */

			while(tok == '\n')
				tok = token();

			/* If we enounter "}", ")" or "]" then */
			/* we know that all immediately preceding */
			/* "if" bodies have been terminated. */
			/* The corresponding "i" values are */
			/* popped off the context stack. */

			if (tok == RBRACE || tok == ')' || tok == ']' ) {
				while (*contextp == 'i')
					ifpop();
				*contextp-- = 0;
				return tok;
			}

			/* When a "," is encountered, it terminates */
			/* just the immediately preceding "if" body */
			/* so we pop just a single "i" of the */
			/* context stack. */

			if (tok == ',') {
				ifpop();
				return tok;
			}

			/* Tricky! If we find an "else" we must */
			/* ignore the preceding newline.  Any other */
			/* token means that we must return the newline */
			/* to terminate the "if" and "push back" that */
			/* token so that we will obtain it on the next */
			/* call to token.  In either case sensitivity */
			/* is lost, so we pop the "i" from the context */
			/* stack. */

			if(tok == ELSE) {
				EatLines = 1;
				ifpop();
				return ELSE;
			}
			else {
				ifpop();
				SavedToken = tok;
				SavedLval = yylval;
				return '\n';
			}
		}
		else return '\n';
	}

		/* Additional context sensitivities */

	switch(tok) {

		/* Any newlines immediately following the */
		/* the following tokens are discarded. The */
		/* expressions are clearly incomplete. */

	case '+':
	case '-':
	case '*':
	case '/':
	case '^':
	case LT:
	case LE:
	case GE:
	case GT:
	case EQ:
	case OR:
	case AND:
	case SPECIAL:
	case FUNCTION:
	case WHILE:
	case REPEAT:
	case FOR:
	case IN:
	case '?':
	case '!':
	case '=':
	case ':':
	case '~':
	case '$':
	case LEFT_ASSIGN:
	case RIGHT_ASSIGN:
		EatLines = 1;
		break;

		/* Push any "if" statements found and */
		/* discard any immediately following newlines. */

	case IF:
		IfPush();
		EatLines = 1;
		break;

		/* Terminate any immediately preceding "if" */
		/* statements and discard any immediately */
		/* following newlines. */

	case ELSE:
		ifpop();
		EatLines = 1;
		break;
	
		/* These tokens terminate any immediately */
		/* preceding "if" statements. */

	case ';':
	case ',':
		ifpop();
		break;

		/* Any newlines following these tokens can */
		/* indicate the end of an expression. */

	case SYMBOL:
	case STR_CONST:
	case NUM_CONST:
	case NULL_CONST:
	case NEXT:
	case BREAK:
		EatLines = 0;
		break;

		/* Handle brackets, braces and parentheses */

	case LBB:
		*++contextp = '[';
		*++contextp = '[';
		break;
		
	case '[':
		*++contextp = tok;
		break;

	case LBRACE:
		*++contextp = tok;
		EatLines = 1;
		PushComment();
		break;

	case '(':
		*++contextp = tok;
		break;

	case ']':
		while (*contextp == 'i')
			ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	case RBRACE:
		while (*contextp == 'i')
			ifpop();
		if(*contextp == LBRACE)
			PopComment();
		*contextp-- = 0;
		break;

	case ')':
		while (*contextp == 'i')
			ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	}
	return tok;
}
