
# line 2 "gram.y"
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

# define END_OF_INPUT 257
# define ERROR 258
# define STR_CONST 259
# define NUM_CONST 260
# define NULL_CONST 261
# define SYMBOL 262
# define FUNCTION 263
# define LEFT_ASSIGN 264
# define RIGHT_ASSIGN 265
# define LBB 266
# define FOR 267
# define IN 268
# define IF 269
# define ELSE 270
# define WHILE 271
# define NEXT 272
# define BREAK 273
# define REPEAT 274
# define GT 275
# define GE 276
# define LT 277
# define LE 278
# define EQ 279
# define NE 280
# define AND 281
# define OR 282
# define LOW 283
# define TILDE 284
# define UNOT 285
# define NOT 286
# define SPECIAL 287
# define UMINUS 288
# define UPLUS 289

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#else
#include <malloc.h>
#include <memory.h>
#endif

#include <values.h>

#ifdef __cplusplus

#ifndef yyerror
	void yyerror(const char *);
#endif

#ifndef yylex
#ifdef __EXTERN_C__
	extern "C" { int yylex(void); }
#else
	int yylex(void);
#endif
#endif
	int yyparse(void);

#endif
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYSTYPE
#define YYSTYPE int
#endif
YYSTYPE yylval;
YYSTYPE yyval;
typedef int yytabelem;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
#if YYMAXDEPTH > 0
int yy_yys[YYMAXDEPTH], *yys = yy_yys;
YYSTYPE yy_yyv[YYMAXDEPTH], *yyv = yy_yyv;
#else	/* user does initial allocation */
int *yys;
YYSTYPE *yyv;
#endif
static int yymaxdepth = YYMAXDEPTH;
# define YYERRCODE 256

# line 237 "gram.y"



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
		ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	case RBRACE:
		ifpop();
		if(*contextp == LBRACE)
			PopComment();
		*contextp-- = 0;
		break;

	case ')':
		ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	}
	return tok;
}
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 55,
	126, 0,
	-2, 15,
-1, 73,
	126, 0,
	-2, 25,
-1, 74,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 26,
-1, 75,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 27,
-1, 76,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 28,
-1, 77,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 29,
-1, 78,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 30,
-1, 79,
	275, 0,
	276, 0,
	277, 0,
	278, 0,
	279, 0,
	280, 0,
	-2, 31,
	};
# define YYNPROD 74
# define YYLAST 733
yytabelem yyact[]={

    24,    48,    33,   120,   128,    45,   135,    29,    27,    48,
    28,    93,    30,    45,    92,   103,    99,    85,   126,   112,
   111,   134,   107,    26,   117,   110,    48,    33,    96,   109,
    45,    26,    29,    27,   106,    28,   108,    30,    84,   115,
   122,    63,   116,    61,    59,    57,    48,    33,    26,    25,
    45,   121,    29,    27,    62,    28,    47,    30,    60,    31,
    58,    98,    49,     1,    47,    48,    33,    31,    26,    45,
   119,    29,    27,     0,    28,     0,    30,    95,     0,     0,
     0,    47,     0,     0,    31,    90,    91,    26,     0,    48,
    33,    34,     0,    45,     0,    29,    27,     0,    28,     0,
    30,    47,     0,     0,    31,     0,     0,     0,    48,    33,
     0,    26,    45,    97,    29,    27,    34,    28,     0,    30,
    47,     0,     0,    31,     0,     0,     0,     0,    48,    33,
    26,     0,    45,     0,    29,    27,    34,    28,   127,    30,
   132,     0,     0,    94,    47,    48,    33,    31,     0,    45,
    26,    29,    27,    48,    28,    34,    30,    45,     0,     0,
     0,     0,     0,    47,     0,     0,    31,    26,     0,    48,
    33,     0,     0,    45,     0,    29,    27,     0,    28,    34,
    30,     0,     0,    47,     0,     0,    31,     0,     0,     0,
     0,    26,     0,     0,     0,    48,    33,     0,    34,    45,
    47,    29,    27,    31,    28,     0,    30,     0,    47,     0,
     0,    31,     0,     0,     0,     0,     0,    26,    34,     0,
     0,     0,     0,     0,    47,     0,     0,    31,     0,    43,
    44,    46,     0,     0,     0,    34,     0,     0,     0,    46,
    40,    39,    35,    36,    37,    38,    41,    42,     0,     0,
    47,     0,    32,    31,    43,    44,    46,    48,     0,     0,
    32,    45,     0,     0,     0,    40,    39,    35,    36,    37,
    38,    41,    42,     0,    43,    44,    46,    32,     0,    26,
     0,     0,     0,     0,     0,    40,    39,    35,    36,    37,
    38,    41,    42,    43,    44,    46,     0,    32,     0,     0,
     0,     0,     0,     0,    40,    39,    35,    36,    37,    38,
    41,    42,    47,     0,     0,    31,    32,    43,    44,    46,
     0,     0,     0,   118,     0,     0,     0,     0,    40,    39,
    35,    36,    37,    38,    41,    42,    43,    44,    46,     0,
    32,     0,     0,     0,     0,     0,     0,    40,    39,    35,
    36,    37,    38,    41,    42,     0,    43,    44,    46,    32,
     0,     0,     0,     0,     0,     0,     0,    40,    39,    35,
    36,    37,    38,    41,    42,    46,     0,     0,     0,    32,
     0,     0,     0,    46,    40,    39,    35,    36,    37,    38,
    41,    42,     0,     0,    48,    33,    32,     0,    45,    46,
    29,    27,     0,    28,     0,    30,     0,     0,    40,    39,
    35,    36,    37,    38,    41,    42,    26,     0,     0,     0,
    32,     0,    48,    33,     0,    46,    45,     0,    29,    27,
     0,    28,     3,    30,    40,    39,    35,    36,    37,    38,
    41,     0,     0,     0,    26,     0,    32,    48,    33,    47,
     0,    45,    31,    29,     0,    14,     0,     0,    30,     0,
     0,     0,    11,     0,     0,    13,     0,    12,     0,    26,
     0,     0,     0,     0,    14,     0,     0,    47,     0,     0,
    31,    11,     0,     0,    13,    16,    12,    46,     0,     0,
     0,    14,     0,     0,     0,    86,     4,     0,    11,     0,
     0,    13,    47,    12,    16,    31,    50,    51,    52,    53,
    54,    55,    56,     0,     0,     0,     0,    64,     0,     0,
     0,    16,    65,    66,    67,    68,    69,    70,    71,    72,
    73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
    83,     0,     0,     0,     0,    10,     0,     0,    15,     0,
     0,     0,     0,     0,   100,   101,   102,     0,   104,   105,
     0,     0,     0,     0,    10,     0,     0,    15,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    10,     0,     0,    15,     0,     0,     0,     0,     0,
     0,   113,   114,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   123,   124,   125,     0,     0,     0,
     0,     0,     0,   129,   130,     0,   131,     0,     0,     0,
     0,     0,     0,   133,    46,     0,     0,     0,     0,     0,
   136,     0,     0,    40,    39,    35,    36,    37,    38,     0,
     0,     0,     0,     0,     0,    32,     0,     0,     0,     0,
     0,     0,    46,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    32,     0,     0,     0,    46,     5,     2,
     0,     7,     6,     8,     9,    17,     0,     0,     0,    19,
     0,    18,     0,    20,    22,    23,    21,     0,    32,     0,
     7,     6,     8,     9,    17,     0,     0,     0,    19,     0,
    18,     0,    20,    22,    23,    21,     0,    88,     6,    89,
    87,    17,     0,     0,     0,    19,     0,    18,     0,    20,
    22,    23,    21 };
yytabelem yypact[]={

   422,-10000000,-10000000,-10000000,   -10,-10000000,-10000000,-10000000,-10000000,-10000000,
   441,   441,   441,   441,   441,   441,   441,     5,     4,     3,
     1,   441,-10000000,-10000000,-10000000,-10000000,   441,   441,   441,   441,
   441,   441,   441,   441,   441,   441,   441,   441,   441,   441,
   441,   441,   441,   441,   441,   458,   458,   458,  -248,    18,
    92,    72,   117,   117,   358,   133,    92,  -246,   441,   441,
   441,  -247,   441,   441,    92,   117,   411,   411,   -27,   -27,
   117,   221,   -27,   133,   386,   386,   386,   386,   386,   386,
   358,   159,    92,   109,    -7,-10000000,    92,   -25,   -32,   -36,
   -73,   -74,-10000000,-10000000,-10000000,   441,   441,-10000000,    -2,   -37,
    53,    29,    92,  -265,    92,    10,-10000000,    -4,   441,   441,
   441,   -75,-10000000,    92,    92,-10000000,  -258,   441,   441,-10000000,
   441,-10000000,   458,    92,    92,    92,-10000000,   441,   -40,    92,
    92,   -35,-10000000,    92,   441,-10000000,    92 };
yytabelem yypgo[]={

     0,    63,   495,    62,    61,    22,    38,    60,    58,    54,
    17 };
yytabelem yyr1[]={

     0,     1,     1,     1,     1,     1,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     9,
     7,     8,     3,     3,     3,     3,     3,     3,     6,     6,
    10,    10,    10,    10,    10,    10,    10,    10,     4,     4,
     4,     4,     4,     5 };
yytabelem yyr2[]={

     0,     3,     3,     5,     5,     3,     3,     3,     3,     3,
     7,     7,     5,     5,     5,     5,     5,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,    13,     9,     7,    11,
     7,     7,     5,    11,     9,     7,     7,     3,     3,     7,
     7,    11,     1,     3,     7,     5,     7,     5,     3,     9,
     1,     3,     5,     7,     5,     7,     5,     7,     1,     3,
     7,     7,    11,     1 };
yytabelem yychk[]={

-10000000,    -1,   257,    10,    -2,   256,   260,   259,   261,   262,
   123,    40,    45,    43,    33,   126,    63,   263,   269,   267,
   271,   274,   272,   273,    10,    59,    58,    43,    45,    42,
    47,    94,   287,    37,   126,   277,   278,   279,   280,   276,
   275,   281,   282,   264,   265,    40,   266,    91,    36,    -3,
    -2,    -2,    -2,    -2,    -2,    -2,    -2,    40,    -7,    40,
    -8,    40,    -9,    40,    -2,    -2,    -2,    -2,    -2,    -2,
    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
    -2,    -2,    -2,    -2,    -6,   -10,    -2,   262,   259,   261,
    -6,    -6,   262,   259,   125,    59,    10,    41,    -4,   262,
    -2,    -2,    -2,   262,    -2,    -2,    41,    -5,    61,    61,
    61,    93,    93,    -2,    -2,    41,    44,    61,   270,    41,
   268,    41,    44,    -2,    -2,    -2,    93,    -5,   262,    -2,
    -2,    -2,   -10,    -2,    61,    41,    -2 };
yytabelem yydef[]={

     0,    -2,     1,     2,     0,     5,     6,     7,     8,     9,
    52,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    47,    48,     3,     4,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    60,    60,    60,     0,     0,
    53,     0,    12,    13,    14,    -2,    16,    68,     0,     0,
     0,     0,     0,     0,    42,    17,    18,    19,    20,    21,
    22,    23,    24,    -2,    -2,    -2,    -2,    -2,    -2,    -2,
    32,    33,    34,    35,    73,    58,    61,     9,     7,     8,
    73,    73,    45,    46,    10,    55,    57,    11,     0,    69,
    38,     0,    40,     0,    41,     0,    37,     0,    62,    64,
    66,     0,    44,    54,    56,    73,     0,     0,     0,    50,
     0,    49,    60,    63,    65,    67,    43,     0,    71,    70,
    39,     0,    59,    36,     0,    51,    72 };
typedef struct
#ifdef __cplusplus
	yytoktype
#endif
{ char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	0	/* don't allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"END_OF_INPUT",	257,
	"ERROR",	258,
	"STR_CONST",	259,
	"NUM_CONST",	260,
	"NULL_CONST",	261,
	"SYMBOL",	262,
	"FUNCTION",	263,
	"LEFT_ASSIGN",	264,
	"RIGHT_ASSIGN",	265,
	"LBB",	266,
	"FOR",	267,
	"IN",	268,
	"IF",	269,
	"ELSE",	270,
	"WHILE",	271,
	"NEXT",	272,
	"BREAK",	273,
	"REPEAT",	274,
	"GT",	275,
	"GE",	276,
	"LT",	277,
	"LE",	278,
	"EQ",	279,
	"NE",	280,
	"AND",	281,
	"OR",	282,
	"?",	63,
	"LOW",	283,
	"~",	126,
	"TILDE",	284,
	"UNOT",	285,
	"NOT",	286,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"SPECIAL",	287,
	":",	58,
	"UMINUS",	288,
	"UPLUS",	289,
	"^",	94,
	"$",	36,
	"(",	40,
	"[",	91,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
	"prog : END_OF_INPUT",
	"prog : '\n'",
	"prog : expr '\n'",
	"prog : expr ';'",
	"prog : error",
	"expr : NUM_CONST",
	"expr : STR_CONST",
	"expr : NULL_CONST",
	"expr : SYMBOL",
	"expr : '{' exprlist '}'",
	"expr : '(' expr ')'",
	"expr : '-' expr",
	"expr : '+' expr",
	"expr : '!' expr",
	"expr : '~' expr",
	"expr : '?' expr",
	"expr : expr ':' expr",
	"expr : expr '+' expr",
	"expr : expr '-' expr",
	"expr : expr '*' expr",
	"expr : expr '/' expr",
	"expr : expr '^' expr",
	"expr : expr SPECIAL expr",
	"expr : expr '%' expr",
	"expr : expr '~' expr",
	"expr : expr LT expr",
	"expr : expr LE expr",
	"expr : expr EQ expr",
	"expr : expr NE expr",
	"expr : expr GE expr",
	"expr : expr GT expr",
	"expr : expr AND expr",
	"expr : expr OR expr",
	"expr : expr LEFT_ASSIGN expr",
	"expr : expr RIGHT_ASSIGN expr",
	"expr : FUNCTION '(' formlist ')' cr expr",
	"expr : expr '(' sublist ')'",
	"expr : IF ifcond expr",
	"expr : IF ifcond expr ELSE expr",
	"expr : FOR forcond expr",
	"expr : WHILE cond expr",
	"expr : REPEAT expr",
	"expr : expr LBB sublist ']' ']'",
	"expr : expr '[' sublist ']'",
	"expr : expr '$' SYMBOL",
	"expr : expr '$' STR_CONST",
	"expr : NEXT",
	"expr : BREAK",
	"cond : '(' expr ')'",
	"ifcond : '(' expr ')'",
	"forcond : '(' SYMBOL IN expr ')'",
	"exprlist : /* empty */",
	"exprlist : expr",
	"exprlist : exprlist ';' expr",
	"exprlist : exprlist ';'",
	"exprlist : exprlist '\n' expr",
	"exprlist : exprlist '\n'",
	"sublist : sub",
	"sublist : sublist cr ',' sub",
	"sub : /* empty */",
	"sub : expr",
	"sub : SYMBOL '='",
	"sub : SYMBOL '=' expr",
	"sub : STR_CONST '='",
	"sub : STR_CONST '=' expr",
	"sub : NULL_CONST '='",
	"sub : NULL_CONST '=' expr",
	"formlist : /* empty */",
	"formlist : SYMBOL",
	"formlist : SYMBOL '=' expr",
	"formlist : formlist ',' SYMBOL",
	"formlist : formlist ',' SYMBOL '=' expr",
	"cr : /* empty */",
};
#endif /* YYDEBUG */
# line	1 "/usr/ccs/bin/yaccpar"
/*
 * Copyright (c) 1993 by Sun Microsystems, Inc.
 */

#pragma ident	"@(#)yaccpar	6.12	93/06/07 SMI"

/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#define YYERROR		goto yyerrlab
#define YYACCEPT	return(0)
#define YYABORT		return(1)
#define YYBACKUP( newtoken, newvalue )\
{\
	if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
	{\
		yyerror( "syntax error - cannot backup" );\
		goto yyerrlab;\
	}\
	yychar = newtoken;\
	yystate = *yyps;\
	yylval = newvalue;\
	goto yynewstate;\
}
#define YYRECOVERING()	(!!yyerrflag)
#define YYNEW(type)	malloc(sizeof(type) * yynewmax)
#define YYCOPY(to, from, type) \
	(type *) memcpy(to, (char *) from, yynewmax * sizeof(type))
#define YYENLARGE( from, type) \
	(type *) realloc((char *) from, yynewmax * sizeof(type))
#ifndef YYDEBUG
#	define YYDEBUG	1	/* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;			/* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG		(-10000000)

/*
** global variables used by the parser
*/
YYSTYPE *yypv;			/* top of value stack */
int *yyps;			/* top of state stack */

int yystate;			/* current state */
int yytmp;			/* extra var (lasts between blocks) */

int yynerrs;			/* number of errors */
int yyerrflag;			/* error recovery flag */
int yychar;			/* current input token number */



#ifdef YYNMBCHARS
#define YYLEX()		yycvtok(yylex())
/*
** yycvtok - return a token if i is a wchar_t value that exceeds 255.
**	If i<255, i itself is the token.  If i>255 but the neither 
**	of the 30th or 31st bit is on, i is already a token.
*/
#if defined(__STDC__) || defined(__cplusplus)
int yycvtok(int i)
#else
int yycvtok(i) int i;
#endif
{
	int first = 0;
	int last = YYNMBCHARS - 1;
	int mid;
	wchar_t j;

	if(i&0x60000000){/*Must convert to a token. */
		if( yymbchars[last].character < i ){
			return i;/*Giving up*/
		}
		while ((last>=first)&&(first>=0)) {/*Binary search loop*/
			mid = (first+last)/2;
			j = yymbchars[mid].character;
			if( j==i ){/*Found*/ 
				return yymbchars[mid].tvalue;
			}else if( j<i ){
				first = mid + 1;
			}else{
				last = mid -1;
			}
		}
		/*No entry in the table.*/
		return i;/* Giving up.*/
	}else{/* i is already a token. */
		return i;
	}
}
#else/*!YYNMBCHARS*/
#define YYLEX()		yylex()
#endif/*!YYNMBCHARS*/

/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
#if defined(__STDC__) || defined(__cplusplus)
int yyparse(void)
#else
int yyparse()
#endif
{
	register YYSTYPE *yypvt;	/* top of value stack for $vars */

#if defined(__cplusplus) || defined(lint)
/*
	hacks to please C++ and lint - goto's inside switch should never be
	executed; yypvt is set to 0 to avoid "used before set" warning.
*/
	static int __yaccpar_lint_hack__ = 0;
	switch (__yaccpar_lint_hack__)
	{
		case 1: goto yyerrlab;
		case 2: goto yynewstate;
	}
	yypvt = 0;
#endif

	/*
	** Initialize externals - yyparse may be called more than once
	*/
	yypv = &yyv[-1];
	yyps = &yys[-1];
	yystate = 0;
	yytmp = 0;
	yynerrs = 0;
	yyerrflag = 0;
	yychar = -1;

#if YYMAXDEPTH <= 0
	if (yymaxdepth <= 0)
	{
		if ((yymaxdepth = YYEXPAND(0)) <= 0)
		{
			yyerror("yacc initialization error");
			YYABORT;
		}
	}
#endif

	{
		register YYSTYPE *yy_pv;	/* top of value stack */
		register int *yy_ps;		/* top of state stack */
		register int yy_state;		/* current state */
		register int  yy_n;		/* internal state number info */
	goto yystack;	/* moved from 6 lines above to here to please C++ */

		/*
		** get globals into registers.
		** branch to here only if YYBACKUP was called.
		*/
	yynewstate:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;
		goto yy_newstate;

		/*
		** get globals into registers.
		** either we just started, or we just finished a reduction
		*/
	yystack:
		yy_pv = yypv;
		yy_ps = yyps;
		yy_state = yystate;

		/*
		** top of for (;;) loop while no reductions done
		*/
	yy_stack:
		/*
		** put a state and value onto the stacks
		*/
#if YYDEBUG
		/*
		** if debugging, look up token value in list of value vs.
		** name pairs.  0 and negative (-1) are special values.
		** Note: linear search is used since time is not a real
		** consideration while debugging.
		*/
		if ( yydebug )
		{
			register int yy_i;

			printf( "State %d, token ", yy_state );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ++yy_ps >= &yys[ yymaxdepth ] )	/* room on stack? */
		{
			/*
			** reallocate and recover.  Note that pointers
			** have to be reset, or bad things will happen
			*/
			int yyps_index = (yy_ps - yys);
			int yypv_index = (yy_pv - yyv);
			int yypvt_index = (yypvt - yyv);
			int yynewmax;
#ifdef YYEXPAND
			yynewmax = YYEXPAND(yymaxdepth);
#else
			yynewmax = 2 * yymaxdepth;	/* double table size */
			if (yymaxdepth == YYMAXDEPTH)	/* first time growth */
			{
				char *newyys = (char *)YYNEW(int);
				char *newyyv = (char *)YYNEW(YYSTYPE);
				if (newyys != 0 && newyyv != 0)
				{
					yys = YYCOPY(newyys, yys, int);
					yyv = YYCOPY(newyyv, yyv, YYSTYPE);
				}
				else
					yynewmax = 0;	/* failed */
			}
			else				/* not first time */
			{
				yys = YYENLARGE(yys, int);
				yyv = YYENLARGE(yyv, YYSTYPE);
				if (yys == 0 || yyv == 0)
					yynewmax = 0;	/* failed */
			}
#endif
			if (yynewmax <= yymaxdepth)	/* tables not expanded */
			{
				yyerror( "yacc stack overflow" );
				YYABORT;
			}
			yymaxdepth = yynewmax;

			yy_ps = yys + yyps_index;
			yy_pv = yyv + yypv_index;
			yypvt = yyv + yypvt_index;
		}
		*yy_ps = yy_state;
		*++yy_pv = yyval;

		/*
		** we have a new state - find out what to do
		*/
	yy_newstate:
		if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
			goto yydefault;		/* simple state */
#if YYDEBUG
		/*
		** if debugging, need to mark whether new token grabbed
		*/
		yytmp = yychar < 0;
#endif
		if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
			yychar = 0;		/* reached EOF */
#if YYDEBUG
		if ( yydebug && yytmp )
		{
			register int yy_i;

			printf( "Received token " );
			if ( yychar == 0 )
				printf( "end-of-file\n" );
			else if ( yychar < 0 )
				printf( "-none-\n" );
			else
			{
				for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
					yy_i++ )
				{
					if ( yytoks[yy_i].t_val == yychar )
						break;
				}
				printf( "%s\n", yytoks[yy_i].t_name );
			}
		}
#endif /* YYDEBUG */
		if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
			goto yydefault;
		if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )	/*valid shift*/
		{
			yychar = -1;
			yyval = yylval;
			yy_state = yy_n;
			if ( yyerrflag > 0 )
				yyerrflag--;
			goto yy_stack;
		}

	yydefault:
		if ( ( yy_n = yydef[ yy_state ] ) == -2 )
		{
#if YYDEBUG
			yytmp = yychar < 0;
#endif
			if ( ( yychar < 0 ) && ( ( yychar = YYLEX() ) < 0 ) )
				yychar = 0;		/* reached EOF */
#if YYDEBUG
			if ( yydebug && yytmp )
			{
				register int yy_i;

				printf( "Received token " );
				if ( yychar == 0 )
					printf( "end-of-file\n" );
				else if ( yychar < 0 )
					printf( "-none-\n" );
				else
				{
					for ( yy_i = 0;
						yytoks[yy_i].t_val >= 0;
						yy_i++ )
					{
						if ( yytoks[yy_i].t_val
							== yychar )
						{
							break;
						}
					}
					printf( "%s\n", yytoks[yy_i].t_name );
				}
			}
#endif /* YYDEBUG */
			/*
			** look through exception table
			*/
			{
				register int *yyxi = yyexca;

				while ( ( *yyxi != -1 ) ||
					( yyxi[1] != yy_state ) )
				{
					yyxi += 2;
				}
				while ( ( *(yyxi += 2) >= 0 ) &&
					( *yyxi != yychar ) )
					;
				if ( ( yy_n = yyxi[1] ) < 0 )
					YYACCEPT;
			}
		}

		/*
		** check for syntax error
		*/
		if ( yy_n == 0 )	/* have an error */
		{
			/* no worry about speed here! */
			switch ( yyerrflag )
			{
			case 0:		/* new error */
				yyerror( "syntax error" );
				goto skip_init;
			yyerrlab:
				/*
				** get globals into registers.
				** we have a user generated syntax type error
				*/
				yy_pv = yypv;
				yy_ps = yyps;
				yy_state = yystate;
			skip_init:
				yynerrs++;
				/* FALLTHRU */
			case 1:
			case 2:		/* incompletely recovered error */
					/* try again... */
				yyerrflag = 3;
				/*
				** find state where "error" is a legal
				** shift action
				*/
				while ( yy_ps >= yys )
				{
					yy_n = yypact[ *yy_ps ] + YYERRCODE;
					if ( yy_n >= 0 && yy_n < YYLAST &&
						yychk[yyact[yy_n]] == YYERRCODE)					{
						/*
						** simulate shift of "error"
						*/
						yy_state = yyact[ yy_n ];
						goto yy_stack;
					}
					/*
					** current state has no shift on
					** "error", pop stack
					*/
#if YYDEBUG
#	define _POP_ "Error recovery pops state %d, uncovers state %d\n"
					if ( yydebug )
						printf( _POP_, *yy_ps,
							yy_ps[-1] );
#	undef _POP_
#endif
					yy_ps--;
					yy_pv--;
				}
				/*
				** there is no state on stack with "error" as
				** a valid shift.  give up.
				*/
				YYABORT;
			case 3:		/* no shift yet; eat a token */
#if YYDEBUG
				/*
				** if debugging, look up token in list of
				** pairs.  0 and negative shouldn't occur,
				** but since timing doesn't matter when
				** debugging, it doesn't hurt to leave the
				** tests here.
				*/
				if ( yydebug )
				{
					register int yy_i;

					printf( "Error recovery discards " );
					if ( yychar == 0 )
						printf( "token end-of-file\n" );
					else if ( yychar < 0 )
						printf( "token -none-\n" );
					else
					{
						for ( yy_i = 0;
							yytoks[yy_i].t_val >= 0;
							yy_i++ )
						{
							if ( yytoks[yy_i].t_val
								== yychar )
							{
								break;
							}
						}
						printf( "token %s\n",
							yytoks[yy_i].t_name );
					}
				}
#endif /* YYDEBUG */
				if ( yychar == 0 )	/* reached EOF. quit */
					YYABORT;
				yychar = -1;
				goto yy_newstate;
			}
		}/* end if ( yy_n == 0 ) */
		/*
		** reduction by production yy_n
		** put stack tops, etc. so things right after switch
		*/
#if YYDEBUG
		/*
		** if debugging, print the string that is the user's
		** specification of the reduction which is just about
		** to be done.
		*/
		if ( yydebug )
			printf( "Reduce by (%d) \"%s\"\n",
				yy_n, yyreds[ yy_n ] );
#endif
		yytmp = yy_n;			/* value to switch over */
		yypvt = yy_pv;			/* $vars top of value stack */
		/*
		** Look in goto table for next state
		** Sorry about using yy_state here as temporary
		** register variable, but why not, if it works...
		** If yyr2[ yy_n ] doesn't have the low order bit
		** set, then there is no action to be done for
		** this reduction.  So, no saving & unsaving of
		** registers done.  The only difference between the
		** code just after the if and the body of the if is
		** the goto yy_stack in the body.  This way the test
		** can be made before the choice of what to do is needed.
		*/
		{
			/* length of production doubled with extra bit */
			register int yy_len = yyr2[ yy_n ];

			if ( !( yy_len & 01 ) )
			{
				yy_len >>= 1;
				yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
				yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
					*( yy_ps -= yy_len ) + 1;
				if ( yy_state >= YYLAST ||
					yychk[ yy_state =
					yyact[ yy_state ] ] != -yy_n )
				{
					yy_state = yyact[ yypgo[ yy_n ] ];
				}
				goto yy_stack;
			}
			yy_len >>= 1;
			yyval = ( yy_pv -= yy_len )[1];	/* $$ = $1 */
			yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
				*( yy_ps -= yy_len ) + 1;
			if ( yy_state >= YYLAST ||
				yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
			{
				yy_state = yyact[ yypgo[ yy_n ] ];
			}
		}
					/* save until reenter driver code */
		yystate = yy_state;
		yyps = yy_ps;
		yypv = yy_pv;
	}
	/*
	** code supplied by user is placed in this switch
	*/
	switch( yytmp )
	{
		
case 1:
# line 138 "gram.y"
{ return 0; } break;
case 2:
# line 139 "gram.y"
{ return xxvalue(NULL,2); } break;
case 3:
# line 140 "gram.y"
{ return xxvalue(yypvt[-1],3); } break;
case 4:
# line 141 "gram.y"
{ return xxvalue(yypvt[-1],4); } break;
case 5:
# line 142 "gram.y"
{ YYABORT; } break;
case 6:
# line 145 "gram.y"
{ yyval = yypvt[-0]; } break;
case 7:
# line 146 "gram.y"
{ yyval = yypvt[-0]; } break;
case 8:
# line 147 "gram.y"
{ yyval = yypvt[-0]; } break;
case 9:
# line 148 "gram.y"
{ yyval = yypvt[-0]; } break;
case 10:
# line 150 "gram.y"
{ yyval = xxexprlist(yypvt[-2],yypvt[-1]); } break;
case 11:
# line 151 "gram.y"
{ yyval = xxparen(yypvt[-2],yypvt[-1]); } break;
case 12:
# line 153 "gram.y"
{ yyval = xxunary(yypvt[-1],yypvt[-0]); } break;
case 13:
# line 154 "gram.y"
{ yyval = xxunary(yypvt[-1],yypvt[-0]); } break;
case 14:
# line 155 "gram.y"
{ yyval = xxunary(yypvt[-1],yypvt[-0]); } break;
case 15:
# line 156 "gram.y"
{ yyval = xxunary(yypvt[-1],yypvt[-0]); } break;
case 16:
# line 157 "gram.y"
{ yyval = xxunary(yypvt[-1],yypvt[-0]); } break;
case 17:
# line 159 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 18:
# line 160 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 19:
# line 161 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 20:
# line 162 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 21:
# line 163 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 22:
# line 164 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 23:
# line 165 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 24:
# line 166 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 25:
# line 167 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 26:
# line 168 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 27:
# line 169 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 28:
# line 170 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 29:
# line 171 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 30:
# line 172 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 31:
# line 173 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 32:
# line 174 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 33:
# line 175 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 34:
# line 177 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 35:
# line 178 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-0],yypvt[-2]); } break;
case 36:
# line 180 "gram.y"
{ yyval = xxdefun(yypvt[-5],yypvt[-3],yypvt[-0]); } break;
case 37:
# line 181 "gram.y"
{ yyval = xxfuncall(yypvt[-3],yypvt[-1]); } break;
case 38:
# line 182 "gram.y"
{ yyval = xxif(yypvt[-2],yypvt[-1],yypvt[-0]); } break;
case 39:
# line 183 "gram.y"
{ yyval = xxifelse(yypvt[-4],yypvt[-3],yypvt[-2],yypvt[-0]); } break;
case 40:
# line 184 "gram.y"
{ yyval = xxfor(yypvt[-2],yypvt[-1],yypvt[-0]); } break;
case 41:
# line 185 "gram.y"
{ yyval = xxwhile(yypvt[-2],yypvt[-1],yypvt[-0]); } break;
case 42:
# line 186 "gram.y"
{ yyval = xxrepeat(yypvt[-1],yypvt[-0]); } break;
case 43:
# line 187 "gram.y"
{ yyval = xxsubscript(yypvt[-4],yypvt[-3],yypvt[-2]); } break;
case 44:
# line 188 "gram.y"
{ yyval = xxsubscript(yypvt[-3],yypvt[-2],yypvt[-1]); } break;
case 45:
# line 189 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 46:
# line 190 "gram.y"
{ yyval = xxbinary(yypvt[-1],yypvt[-2],yypvt[-0]); } break;
case 47:
# line 191 "gram.y"
{ yyval = xxnxtbrk(yypvt[-0]); } break;
case 48:
# line 192 "gram.y"
{ yyval = xxnxtbrk(yypvt[-0]); } break;
case 49:
# line 196 "gram.y"
{ yyval = xxcond(yypvt[-1]); } break;
case 50:
# line 199 "gram.y"
{ yyval = xxifcond(yypvt[-1]); } break;
case 51:
# line 202 "gram.y"
{ yyval = xxforcond(yypvt[-3],yypvt[-1]); } break;
case 52:
# line 206 "gram.y"
{ yyval = xxexprlist0(); } break;
case 53:
# line 207 "gram.y"
{ yyval = xxexprlist1(yypvt[-0]); } break;
case 54:
# line 208 "gram.y"
{ yyval = xxexprlist2(yypvt[-2],yypvt[-0]); } break;
case 55:
# line 209 "gram.y"
{ yyval = yypvt[-1]; AddComment(CAR(yyval));} break;
case 56:
# line 210 "gram.y"
{ yyval = xxexprlist2(yypvt[-2],yypvt[-0]); } break;
case 57:
# line 211 "gram.y"
{ yyval = yypvt[-1];} break;
case 58:
# line 214 "gram.y"
{ yyval = xxsublist1(yypvt[-0]); } break;
case 59:
# line 215 "gram.y"
{ yyval = xxsublist2(yypvt[-3],yypvt[-0]); } break;
case 60:
# line 218 "gram.y"
{ yyval = xxsub0(); } break;
case 61:
# line 219 "gram.y"
{ yyval = xxsub1(yypvt[-0]); } break;
case 62:
# line 220 "gram.y"
{ yyval = xxsymsub0(yypvt[-1]); } break;
case 63:
# line 221 "gram.y"
{ yyval = xxsymsub1(yypvt[-2],yypvt[-0]); } break;
case 64:
# line 222 "gram.y"
{ yyval = xxsymsub0(yypvt[-1]); } break;
case 65:
# line 223 "gram.y"
{ yyval = xxsymsub1(yypvt[-2],yypvt[-0]); } break;
case 66:
# line 224 "gram.y"
{ yyval = xxnullsub0(); } break;
case 67:
# line 225 "gram.y"
{ yyval = xxnullsub1(yypvt[-0]); } break;
case 68:
# line 228 "gram.y"
{ yyval = xxnullformal(); } break;
case 69:
# line 229 "gram.y"
{ yyval = xxfirstformal0(yypvt[-0]); } break;
case 70:
# line 230 "gram.y"
{ yyval = xxfirstformal1(yypvt[-2],yypvt[-0]); } break;
case 71:
# line 231 "gram.y"
{ yyval = xxaddformal0(yypvt[-2],yypvt[-0]); } break;
case 72:
# line 232 "gram.y"
{ yyval = xxaddformal1(yypvt[-4],yypvt[-2],yypvt[-0]); } break;
case 73:
# line 235 "gram.y"
{ EatLines = 1; } break;
# line	532 "/usr/ccs/bin/yaccpar"
	}
	goto yystack;		/* reset registers in driver code */
}

