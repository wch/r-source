%{
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

extern SEXP listAppend(SEXP,SEXP);
void pushCmt();
void popCmt();

static int eatln;
extern SEXP R_CommentSxp;

#define YYSTYPE		SEXP
#ifdef YYBYACC
#define YYRETURN(x)	{ return(x); }
#else
#define YYRETURN(x)	{ free((void*)yys); free((void*)yyv); return(x); }
#endif

%}

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

prog:							{ newline = 0; }
	|	prog '\n'				{ R_CurrentExpr = NULL; return 2; }
	|	prog expr '\n'				{ R_CurrentExpr = $2; UNPROTECT(1); YYRETURN(3); }
	|	prog expr ';'				{ R_CurrentExpr = $2; UNPROTECT(1); YYRETURN(4); }
	|	prog error 				{ YYABORT; }
	;

expr:		NUM_CONST				{ $$ = $1; }
	|	STR_CONST				{ $$ = $1; }
	|	NULL_CONST				{ $$ = $1; }
	|	SYMBOL					{ $$ = $1; }
	|	'{' exprlist '}'			{ UNPROTECT(1); TYPEOF($2) = LANGSXP; CAR($2) = $1; $$ = $2; PROTECT($$); eatln = 0; }
	|	'(' expr ')'				{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	'-' expr %prec UMINUS			{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	'+' expr %prec UMINUS			{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	'!' expr %prec UNOT			{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	'~' expr %prec TILDE			{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	'?' expr				{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	expr ':'  expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '+'  expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '-' expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '*' expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '/' expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '^' expr 				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr SPECIAL expr			{ UNPROTECT(3); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '%' expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr '~' expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr LT expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr LE expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr EQ expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr NE expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr GE expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr GT expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr AND expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|	expr OR expr				{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|  	expr LEFT_ASSIGN expr 			{ UNPROTECT(2); $$ = lang3($2, $1, $3); PROTECT($$); }
	|  	expr RIGHT_ASSIGN expr 			{ UNPROTECT(2); $$ = lang3($2, $3, $1); PROTECT($$); }
	|	FUNCTION '(' formlist ')' gobble expr %prec LOW
							{ addcomment($6); UNPROTECT(2); $$ = lang3($1, CDR($3), $6); PROTECT($$); popCmt();}
	|	expr '(' sublist ')'			{ if(isString($1)) $1=install(CHAR(STRING($1)[0])); UNPROTECT(2); if(length(CDR($3)) == 1 && CADR($3) == R_MissingArg )
										$$ = lang1($1);
									else
										$$ = LCONS($1, CDR($3));
									PROTECT($$); }
	|	IF ifcond expr 				{ UNPROTECT(2); $$ = lang3($1, $2, $3); PROTECT($$);  }
	|	IF ifcond expr ELSE expr		{ UNPROTECT(3); $$ = lang4($1, $2, $3, $5); PROTECT($$); }
	|	FOR forcond expr	%prec FOR 	{ UNPROTECT(2); $$ = lang4($1, CAR($2), CDR($2), $3); PROTECT($$); }
	|	WHILE cond expr				{ UNPROTECT(2); $$ = lang3($1, $2, $3); PROTECT($$); }
	|	REPEAT expr				{ UNPROTECT(1); $$ = lang2($1, $2); PROTECT($$); }
	|	expr LBB sublist ']' ']'		{ UNPROTECT(2); $$ = LCONS($2, LCONS($1, CDR($3))); PROTECT($$); }
	|	expr '[' sublist ']'			{ UNPROTECT(2); $$ = LCONS($2, LCONS($1, CDR($3))); PROTECT($$); }
	|	expr '$' SYMBOL				{ $$ = lang3($2, $1, $3); UNPROTECT(2); PROTECT($$); }
	|	expr '$' STR_CONST			{ $$ = lang3($2, $1, $3); UNPROTECT(2); PROTECT($$); }
	|	NEXT					{ $$ = lang1($1); PROTECT($$); }
	|	BREAK					{ $$ = lang1($1); PROTECT($$); }
	;


cond:		'(' expr ')'				{ $$ = $2;  eatln = 1; }
	;

ifcond:		'(' expr ')'				{ $$ = $2; ifpush(); eatln = 1; }
	;

forcond:	'(' SYMBOL IN expr ')' 			{ UNPROTECT(2); $$ = LCONS($2,$4); PROTECT($$); eatln=1;}
	;


exprlist:						{ $$ = newlist(); PROTECT($$); }
	|	expr					{ addcomment($1); UNPROTECT(1); $$ = growlist(newlist(), $1); PROTECT($$);}
	|	exprlist ';' expr			{ addcomment($3); UNPROTECT(2); $$ = growlist($1, $3); PROTECT($$);}
	|	exprlist ';'				{ $$ = $1; addcomment(CAR($$));}
	|	exprlist '\n' expr			{ addcomment($3); UNPROTECT(2); $$ = growlist($1, $3); PROTECT($$);}
	|	exprlist '\n'				{ $$ = $1;}
	;

sublist:	sub					{ UNPROTECT(1); $$ = firstarg(CAR($1),CADR($1)); PROTECT($$); }
	|	sublist gobble ',' sub			{ UNPROTECT(2); $$ = nextarg($1, CAR($4), CADR($4)); PROTECT($$); }
	;

sub:							{ $$ = lang2(R_MissingArg,R_NilValue); PROTECT($$); }
	|	expr					{ UNPROTECT(1); $$ = tagarg($1, R_NilValue); PROTECT($$); }
	|	SYMBOL '=' expr				{ UNPROTECT(2); $$ = tagarg($3, $1); PROTECT($$); }
	|	SYMBOL '=' 				{ UNPROTECT(1); $$ = tagarg(R_MissingArg, $1); PROTECT($$); }
	|	STR_CONST '=' expr			{ UNPROTECT(2); $$ = tagarg($3, $1); PROTECT($$); }
	|	STR_CONST '=' 				{ UNPROTECT(1); $$ = tagarg(R_MissingArg, $1); PROTECT($$); }
	|	NULL_CONST '=' expr			{ UNPROTECT(2); $$ = tagarg($3, install("NULL")); PROTECT($$); }
	|	NULL_CONST '=' 				{ UNPROTECT(1); $$ = tagarg(R_MissingArg, install("NULL")); PROTECT($$); }
	;

formlist:						{ $$ = R_NilValue; PROTECT($$); }
	|	SYMBOL					{ UNPROTECT(1); $$ = firstarg(R_MissingArg, $1); PROTECT($$); }
	|	SYMBOL '=' expr				{ UNPROTECT(2); $$ = firstarg($3, $1); PROTECT($$); }
	|	formlist ',' SYMBOL			{ UNPROTECT(2); check_formals($1,$3); $$ = nextarg($1, R_MissingArg, $3); PROTECT($$); }
	|	formlist ',' SYMBOL '=' expr		{ UNPROTECT(3); check_formals($1,$3); $$ = nextarg($1, $5, $3); PROTECT($$); }
	;

gobble:	{eatln = 1;}
	;
%%

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

/* Lists are created and grown using a special dotted pair. */
/* The CAR of the list points to the last cons-cell in the list */
/* and the CDR points to the first.  The list can be extracted */
/* from the pair by taking its CDR, while the CAR gives fast access */
/* to the end of the list. */

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


	/* Comment Handling */

	/* R_CommentSxp is of the same form as an expression list, */
	/* each time a new { is encountered a new element is placed */
	/* in the R_CommentSxp and when a } is encountered it is */
	/* removed. */

extern void ResetComment()
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

void pushCmt()
{
	R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

void popCmt()
{
	R_CommentSxp = CDR(R_CommentSxp);
}

int isComment(SEXP l)
{
	if (isList(l) && isString(CAR(l)) && !strncmp(CHAR(STRING(CAR(l))[0]), "#", 1))
		return 1;
	else
		return 0;
}

void addcomment(SEXP l)
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


SEXP mkString(char *);
SEXP mkInteger(char *);
SEXP mkFloat(char *);
SEXP mkComplex(char *);
SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);


/*
//	Basic File IO:
//
//	This code is here because at this particular instant it
//	seems closely related to cget(), which appears below.
*/


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

void uncget(int n)
{
	cnt += n;
	bufp -= n;
}


/*
//	Lexical Analyzer:
//
//	Basic lexical analysis is performed by the following
//	routines.  Input is read a line at a time, and, if the
//	program is in batch mode, each input line is echoed to
//	standard output after it is read.
//
//	The function yylex() scans the input, breaking it into
//	tokens which are then passed to the parser.  The lexical
//	analyser maintains a symbol table (in a very messy fashion).
//
//	The fact that if statements need to parse differently
//	depending on whether the statement is being interpreted or
//	part of the body of a function causes the need for ifpop
//	and ifpush. When an if statement is encountered an 'i' is
//	pushed on a stack (provided there are parentheses active).
//	At later points this 'i' needs to be popped off of the if
//	stack.
*/

static int newline = 0;
static int reset = 1;
#ifndef DEBUG_LEX
static
#endif
char *parenp, parenstack[50];

static void ifpush(void)
{
	if (*parenp == '{' || *parenp == '[' || *parenp == '(' || *parenp == 'i')
		*++parenp = 'i';
}

static void ifpop(void)
{
	if (*parenp == 'i')
		parenp--;
}

static int typeofnext(void)
{
	int k, c;

	c = cget();
	if (isdigit(c)) k = 1;
	else if (isalpha(c) || c == '.')
		k = 2;
	else
		k = 3;
	uncget(1);
	return k;
}

static int nextchar(int expect)
{
	int c = cget();

	if (c == expect)
		return 1;
	else
		uncget(1);
	return 0;
}

		/* Special Symbols */
		/* Syntactic Keywords + Symbolic Constants */

struct {
	char *name;
	int token;
} keywords[] = {
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


	/* klookup has side effects, it sets yylval */

int klookup(s)
char *s;
{
	int i;

	for (i = 0; keywords[i].name; i++) {
		if (strcmp(keywords[i].name, s) == 0) {
			switch (keywords[i].token) {
			case NULL_CONST:
				PROTECT(yylval = R_NilValue);
				eatln = 0;
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
				eatln = 0;
				break;
			case FUNCTION:
			case WHILE:
			case REPEAT:
			case FOR:
			case IF:
				eatln = 1;
				yylval = install(s);
				break;
			case IN:
				eatln = 1;
				break;
			case ELSE:
				ifpop();
				eatln = 1;
				break;
			case NEXT:
			case BREAK:
				eatln = 0;
				yylval = install(s);
				break;
			case SYMBOL:
				PROTECT(yylval = install(s));
				eatln = 0;
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

void yyerror(char *s)
{
	int i;

	R_CommentSxp = R_NilValue;
	REprintf("%s", buf);
	for (i = 1; i < bufp - buf; i++) {
		REprintf(" ");
	}
	REprintf("^\n");
	if (R_Console == 0) {
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
}

void check_formals(SEXP formlist, SEXP new)
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
		eatln = 0;
		ResetComment();
	}

	while ((c = cget()) == ' ' || c == '\t' || c == '');

	if (c == '#') {
		p = yytext;
		*p++ = c;
		while ((c = cget()) != '\n' && c != R_EOF)
			*p++ = c;
		*p = '\0';
		if(R_CommentSxp != R_NilValue) {
			f = mkChar(yytext);
			f = CONS(f, R_NilValue);
			CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
		}
	}


	if (c == R_EOF) {
		return EOF;
	}

		/* This code deals with context sensitivity to      */
		/* newlines.  The main problem is finding out       */
		/* whether a newline is followed by an ELSE clause. */
		/* This is only of importance if we are inside one  */
		/* of "(", "[", or "{".			     */

	if (c == '\n') {
		if (eatln || *parenp == '[' || *parenp == '(') {
			prompt();
			goto again;
		}
		if (*parenp == 'i') {
			prompt();
			while ((c = cget()) == ' ' || c == '\t');
			if (c == R_EOF) {
				error("unexpected end-of-file in parse\n");
			}
			if (c == '#') {
				p = yytext;
				*p++ = c;
				while ((c = cget()) != '\n' && c != R_EOF)
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
				uncget(1);
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
			uncget(1);
			if (!strncmp(bufp, "else", 4) && !isalnum(bufp[4]) && bufp[4] != '.') {
				eatln = 1;
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
		while (isdigit(c = cget()) || c == '.' || c == 'e' || c == 'E') {
			if (c == 'E' || c == 'e') {
				if (seenexp)
					break;
				seenexp = 1;
				seendot = 1;
				*p++ = c;
				c = cget();
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
			uncget(1);
		}
		eatln = 0;
		return NUM_CONST;
	}

	/* literal strings */

	if (c == '\"' || c == '\'') {
		quote = c;
		p = yytext;
		while ((c = cget()) != R_EOF && c != quote) {
			if (c == '\n') {
				uncget(1);
				return ERROR;
			}
			if (c == '\\') {
				c = cget();
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
		eatln = 0;
		return STR_CONST;
	}

	/* special functions */
	if (c == '%') {
		p = yytext;
		*p++ = c;
		while ((c = cget()) != R_EOF && c != '%') {
			if (c == '\n') {
				uncget(1);
				return ERROR;
			}
			*p++ = c;
		}
		if (c == '%')
			*p++ = c;
		*p++ = '\0';
		PROTECT(yylval = install(yytext));
		eatln=1;
		return SPECIAL;
	}


	/* functions, constants and variables */

	/* gag, barf, but the punters want it */
	if (c == '_') {
		eatln = 1;
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

    symbol:
	if (c == '.' || isalpha(c)) {
		p = yytext;
		do {
			*p++ = c;
		} while ((c = cget()) != R_EOF && (isalnum(c) || c == '.'));
		uncget(1);
		*p = '\0';

		if ((kw = klookup(yytext))) {
			if(kw == FUNCTION) pushCmt();
			return kw;
		}

		PROTECT(yylval = install(yytext));
		eatln = 0;
		return SYMBOL;
	}

	/* compound tokens */

	switch (c) {
	case '<':
		eatln = 1;
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
		eatln = 1;
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
		eatln = 1;
		if (nextchar('=')) {
			yylval = install(">=");
			return GE;
		}
		yylval = install(">");
		return GT;
	case '!':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install("!=");
			return NE;
		}
		yylval = install("!");
		return '!';
	case '=':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install("==");
			return EQ;
		}
		return '=';
	case ':':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install(":=");
			return LEFT_ASSIGN;
		}
		yylval = install(":");
		return ':';
	case '&':
		eatln = 1;
		if (nextchar('&')) {
			yylval = install("&&");
			return AND;
		}
		yylval = install("&");
		return AND;
	case '|':
		eatln = 1;
		if (nextchar('|')) {
			yylval = install("||");
			return OR;
		}
		yylval = install("|");
		return OR;
	case '{':
		*++parenp = c;
		yylval = install("{");
		pushCmt();
		return c;
	case '}':
		ifpop();
		if(*parenp == '{')
			popCmt();
		parenp--;
		return c;
	case '(':
		*++parenp = c;
		yylval = install("(");
		return c;
	case ')':
		eatln = 0;
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
		eatln = 0;
		parenp--;
		return c;
	case '?':
		eatln = 1;
		strcpy(yytext, "help");
		yylval = install(yytext);
		return c;
	case '*':
		eatln=1;
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
		eatln = 1;
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}
