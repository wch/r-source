%{
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2004  Robert Gentleman, Ross Ihaka and the
 *                            R Development Core Team
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

/* <UTF8>
   This uses byte-level access, which is generally OK as comparisons
   are with ASCII chars.

   isValidName has been changed to cope.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#endif

#include "IOStuff.h"		/*-> Defn.h */
#include "Fileio.h"
#include "Parse.h"

#define yyconst const

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static void	CheckFormalArgs(SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);
static SEXP	GrowList(SEXP, SEXP);
static void	IfPush(void);
static int	KeywordLookup(char*);
static SEXP	NewList(void);
static SEXP	NextArg(SEXP, SEXP, SEXP);
static SEXP	TagArg(SEXP, SEXP);

/* These routines allocate constants */

SEXP		mkComplex(char *);
SEXP		mkFalse(void);
SEXP		mkFloat(char *);
SEXP		mkInteger(char *);
SEXP		mkNA(void);
SEXP		mkString(yyconst char *);
SEXP		mkTrue(void);

/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxgetc();
static int	xxungetc();
static int 	xxcharcount, xxcharsave;

/* Handle function source */

/* FIXME: These arrays really ought to be dynamically extendable
   As from 1.6.0, SourceLine[] is, and the other two are checked.
*/

#define MAXFUNSIZE 131072
#define MAXLINESIZE  1024
#define MAXNEST       265

static unsigned char FunctionSource[MAXFUNSIZE];
static unsigned char SourceLine[MAXLINESIZE];
static unsigned char *FunctionStart[MAXNEST], *SourcePtr;
static int FunctionLevel = 0;
static int KeepSource;

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
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR
%token		NS_GET NS_GET_INT

%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR
%left		AND
%left		UNOT NOT
%nonassoc   	GT GE LT LE EQ NE
%left		'+' '-'
%left		'*' '/'
%left		SPECIAL
%left		':'
%left		UMINUS UPLUS
%right		'^'
%left		'$' '@'
%left		NS_GET NS_GET_INT
%nonassoc	'(' '[' LBB

%%

prog	:	END_OF_INPUT			{ return 0; }
	|	'\n'				{ return xxvalue(NULL,2); }
	|	expr_or_assign '\n'			{ return xxvalue($1,3); }
	|	expr_or_assign ';'			{ return xxvalue($1,4); }
	|	error	 			{ YYABORT; }
	;

expr_or_assign  :    expr                       { $$ = $1; }
                |    equal_assign               { $$ = $1; }
                ;

equal_assign    :    expr EQ_ASSIGN expr_or_assign              { $$ = xxbinary($2,$1,$3); }
                ;

expr	: 	NUM_CONST			{ $$ = $1; }
	|	STR_CONST			{ $$ = $1; }
	|	NULL_CONST			{ $$ = $1; }
	|	SYMBOL				{ $$ = $1; }

	|	'{' exprlist '}'		{ $$ = xxexprlist($1,$2); }
	|	'(' expr_or_assign ')'			{ $$ = xxparen($1,$2); }

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
	|	expr '?' expr			{ $$ = xxbinary($2,$1,$3); }
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
	|	FUNCTION '(' formlist ')' cr expr_or_assign %prec LOW
						{ $$ = xxdefun($1,$3,$6); }
	|	expr '(' sublist ')'		{ $$ = xxfuncall($1,$3); }
	|	IF ifcond expr_or_assign 			{ $$ = xxif($1,$2,$3); }
	|	IF ifcond expr_or_assign ELSE expr_or_assign	{ $$ = xxifelse($1,$2,$3,$5); }
	|	FOR forcond expr_or_assign %prec FOR 	{ $$ = xxfor($1,$2,$3); }
	|	WHILE cond expr_or_assign			{ $$ = xxwhile($1,$2,$3); }
	|	REPEAT expr_or_assign			{ $$ = xxrepeat($1,$2); }
	|	expr LBB sublist ']' ']'	{ $$ = xxsubscript($1,$2,$3); }
	|	expr '[' sublist ']'		{ $$ = xxsubscript($1,$2,$3); }
	|	SYMBOL NS_GET SYMBOL		{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET SYMBOL		{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET_INT SYMBOL	{ $$ = xxbinary($2,$1,$3); }
	|	SYMBOL NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET_INT SYMBOL	{ $$ = xxbinary($2,$1,$3); }
	|	STR_CONST NS_GET_INT STR_CONST	{ $$ = xxbinary($2,$1,$3); }
	|	expr '$' SYMBOL			{ $$ = xxbinary($2,$1,$3); }
	|	expr '$' STR_CONST		{ $$ = xxbinary($2,$1,$3); }
	|	expr '@' SYMBOL			{ $$ = xxbinary($2,$1,$3); }
	|	expr '@' STR_CONST		{ $$ = xxbinary($2,$1,$3); }
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
	|	expr_or_assign				{ $$ = xxexprlist1($1); }
	|	exprlist ';' expr_or_assign	{ $$ = xxexprlist2($1,$3); }
	|	exprlist ';'			{ $$ = $1; }
	|	exprlist '\n' expr_or_assign	{ $$ = xxexprlist2($1,$3); }
	|	exprlist '\n'			{ $$ = $1;}
	;

sublist	:	sub				{ $$ = xxsublist1($1); }
	|	sublist cr ',' sub		{ $$ = xxsublist2($1,$4); }
	;

sub	:					{ $$ = xxsub0(); }
	|	expr				{ $$ = xxsub1($1); }
	|	SYMBOL EQ_ASSIGN 			{ $$ = xxsymsub0($1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxsymsub1($1,$3); }
	|	STR_CONST EQ_ASSIGN 			{ $$ = xxsymsub0($1); }
	|	STR_CONST EQ_ASSIGN expr		{ $$ = xxsymsub1($1,$3); }
	|	NULL_CONST EQ_ASSIGN 			{ $$ = xxnullsub0(); }
	|	NULL_CONST EQ_ASSIGN expr		{ $$ = xxnullsub1($3); }
	;

formlist:					{ $$ = xxnullformal(); }
	|	SYMBOL				{ $$ = xxfirstformal0($1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxfirstformal1($1,$3); }
	|	formlist ',' SYMBOL		{ $$ = xxaddformal0($1,$3); }
	|	formlist ',' SYMBOL EQ_ASSIGN expr	{ $$ = xxaddformal1($1,$3,$5); }
	;

cr	:					{ EatLines = 1; }
	;
%%


/*----------------------------------------------------------------------------*/

static int (*ptr_getc)(void);
static int (*ptr_ungetc)(int);

static int xxgetc(void)
{
    int c = ptr_getc();
    if (c == EOF) {
        EndOfFile = 1;
        return R_EOF;
    }
    if (c == '\n') R_ParseError += 1;
    if ( KeepSource && GenerateCode && FunctionLevel > 0 ) {
	if(SourcePtr <  FunctionSource + MAXFUNSIZE)
	    *SourcePtr++ = c;
	else  error("function is too long to keep source");
    }
    xxcharcount++;
    return c;
}

static int xxungetc(int c)
{
    if (c == '\n') R_ParseError -= 1;
    if ( KeepSource && GenerateCode && FunctionLevel > 0 )
	SourcePtr--;
    xxcharcount--;
    return ptr_ungetc(c);
}

static int xxvalue(SEXP v, int k)
{
    if (k > 2) UNPROTECT_PTR(v);
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
    UNPROTECT_PTR(sym);
    if (GenerateCode)
	PROTECT(ans = FirstArg(R_MissingArg, sym));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxfirstformal1(SEXP sym, SEXP expr)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = FirstArg(expr, sym));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxaddformal0(SEXP formlist, SEXP sym)
{
    SEXP ans;
    if (GenerateCode) {
	CheckFormalArgs(formlist ,sym);
	PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr)
{
    SEXP ans;
    if (GenerateCode) {
	CheckFormalArgs(formlist, sym);
	PROTECT(ans = NextArg(formlist, expr, sym));
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

static SEXP xxexprlist0()
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = NewList());
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxexprlist1(SEXP expr)
{
    SEXP ans,tmp;
    if (GenerateCode) {
	PROTECT(tmp = NewList());
	PROTECT(ans = GrowList(tmp, expr));
	UNPROTECT(1);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = GrowList(exprlist, expr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(exprlist);
    return ans;
}

static SEXP xxsub0(void)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(R_MissingArg,R_NilValue));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxsub1(SEXP expr)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, R_NilValue));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxsymsub0(SEXP sym)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, sym));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, sym));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxnullsub0()
{
    SEXP ans;
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, install("NULL")));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxnullsub1(SEXP expr)
{
    SEXP ans = install("NULL");
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, ans));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}


static SEXP xxsublist1(SEXP sub)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sub);
    return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sub);
    UNPROTECT_PTR(sublist);
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
    if (GenerateCode)
	PROTECT(ans = lang3(ifsym, cond, expr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
    SEXP ans;
    if( GenerateCode)
	PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(elseexpr);
    UNPROTECT_PTR(ifexpr);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
    SEXP ans;
    EatLines = 1;
    if (GenerateCode)
	PROTECT(ans = LCONS(sym, expr));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(forcond);
    return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang3(whilesym, cond, body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(cond);
    return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(repeatsym, body));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
    if (GenerateCode)
	PROTECT(keyword = lang1(keyword));
    else
	PROTECT(keyword = R_NilValue);
    return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
    SEXP ans, sav_expr = expr;
    if(GenerateCode) {
	if (isString(expr))
	    expr = install(CHAR(STRING_ELT(expr, 0)));
	PROTECT(expr);
	if (length(CDR(args)) == 1 && CADR(args) == R_MissingArg && TAG(CDR(args)) == R_NilValue )
	    ans = lang1(expr);
	else
	    ans = LCONS(expr, CDR(args));
	UNPROTECT(1);
	PROTECT(ans);
    }
    else {
	PROTECT(ans = R_NilValue);
    }
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(sav_expr);
    return ans;
}

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{

    SEXP ans;
    SEXP source;

    if (GenerateCode) {
	if (!KeepSource)
	    PROTECT(source = R_NilValue);
	else {
	    unsigned char *p, *p0, *end;
	    int lines = 0, nc;

	    /*  If the function ends with an endline comment,  e.g.

		function()
	            print("Hey") # This comment

		we need some special handling to keep it from getting
		chopped off. Normally, we will have read one token too
		far, which is what xxcharcount and xxcharsave keeps
		track of.

	    */
	    end = SourcePtr - (xxcharcount - xxcharsave);
	    for (p = end ; p < SourcePtr && (*p == ' ' || *p == '\t') ; p++)
		;
	    if (*p == '#') {
		while (p < SourcePtr && *p != '\n')
		    p++;
		end = p;
	    }

	    for (p = FunctionStart[FunctionLevel]; p < end ; p++)
		if (*p == '\n') lines++;
	    if ( *(end - 1) != '\n' ) lines++;
	    PROTECT(source = allocVector(STRSXP, lines));
	    p0 = FunctionStart[FunctionLevel];
	    lines = 0;
	    for (p = FunctionStart[FunctionLevel]; p < end ; p++)
		if (*p == '\n' || p == end - 1) {
		    nc = p - p0;
		    if (*p != '\n')
			nc++;
		    if (nc <= MAXLINESIZE) {
			strncpy((char *)SourceLine, (char *)p0, nc);
			SourceLine[nc] = '\0';
			SET_STRING_ELT(source, lines++,
				       mkChar((char *)SourceLine));
		    } else { /* over-long line */
			char *LongLine = (char *) malloc(nc);
			if(!LongLine) 
			    error("unable to allocate space to source line");
			strncpy(LongLine, (char *)p0, nc);
			LongLine[nc] = '\0';
			SET_STRING_ELT(source, lines++,
				       mkChar((char *)LongLine));
			free(LongLine);
		    }
		    p0 = p + 1;
		}
	    /* PrintValue(source); */
	}
	PROTECT(ans = lang4(fname, CDR(formals), body, source));
	UNPROTECT_PTR(source);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(formals);
    FunctionLevel--;
    return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(op, arg));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(arg);
    return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang3(n1, n2, n3));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(n2);
    UNPROTECT_PTR(n3);
    return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = lang2(n1, n2));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(n2);
    return ans;
}


/* This should probably use CONS rather than LCONS, but
   it shouldn't matter and we would rather not meddle
   See PR#7055 */

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = LCONS(a2, LCONS(a1, CDR(a3))));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a3);
    UNPROTECT_PTR(a1);
    return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
    SEXP ans;
    EatLines = 0;
    if (GenerateCode) {
	SET_TYPEOF(a2, LANGSXP);
	SETCAR(a2, a1);
	PROTECT(ans = a2);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a2);
    return ans;
}

/*--------------------------------------------------------------------------*/

static SEXP TagArg(SEXP arg, SEXP tag)
{
    switch (TYPEOF(tag)) {
    case STRSXP:
    	tag = install(CHAR(STRING_ELT(tag, 0)));
    case NILSXP:
    case SYMSXP:
	return lang2(arg, tag);
    default:
	error("incorrect tag type"); return R_NilValue/* -Wall */;
    }
}


/* Stretchy List Structures : Lists are created and grown using a special */
/* dotted pair.  The CAR of the list points to the last cons-cell in the */
/* list and the CDR points to the first.  The list can be extracted from */
/* the pair by taking its CDR, while the CAR gives fast access to the end */
/* of the list. */


/* Create a stretchy-list dotted pair */

static SEXP NewList(void)
{
    SEXP s = CONS(R_NilValue, R_NilValue);
    SETCAR(s, s);
    return s;
}

/* Add a new element at the end of a stretchy list */

static SEXP GrowList(SEXP l, SEXP s)
{
    SEXP tmp;
    PROTECT(s);
    tmp = CONS(s, R_NilValue);
    UNPROTECT(1);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
    return l;
}

#if 0
/* Comment Handling :R_CommentSxp is of the same form as an expression */
/* list, each time a new { is encountered a new element is placed in the */
/* R_CommentSxp and when a } is encountered it is removed. */

static void ResetComment(void)
{
    R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

static void PushComment(void)
{
    if (GenerateCode)
	R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

static void PopComment(void)
{
    if (GenerateCode)
	R_CommentSxp = CDR(R_CommentSxp);
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
#endif

static SEXP FirstArg(SEXP s, SEXP tag)
{
    SEXP tmp;
    PROTECT(s);
    PROTECT(tag);
    PROTECT(tmp = NewList());
    tmp = GrowList(tmp, s);
    SET_TAG(CAR(tmp), tag);
    UNPROTECT(3);
    return tmp;
}

static SEXP NextArg(SEXP l, SEXP s, SEXP tag)
{
    PROTECT(tag);
    PROTECT(l);
    l = GrowList(l, s);
    SET_TAG(CAR(l), tag);
    UNPROTECT(2);
    return l;
}



/*--------------------------------------------------------------------------*/

/*
 *  Parsing Entry Points:
 *
 *  The Following entry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IOBuffers
 *  (i.e. interactve use), files and R character strings.
 *
 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *  The following routines parse a single expression:
 *
 *
 *	SEXP R_Parse1File(FILE *fp, int gencode, ParseStatus *status)
 *
 *	SEXP R_Parse1Vector(TextBuffer *text, int gencode, ParseStatus *status)
 *
 *	SEXP R_Parse1Buffer(IOBuffer *buffer, int gencode, ParseStatus *status)
 *
 *
 *  The success of the parse is indicated as folllows:
 *
 *
 *	status = PARSE_NULL       - there was no statement to parse
 *		 PARSE_OK	  - complete statement
 *		 PARSE_INCOMPLETE - incomplete statement
 *		 PARSE_ERROR      - syntax error
 *		 PARSE_EOF	  - end of file
 *
 *
 *  The following routines parse several expressions and return
 *  their values in a single expression vector.
 *
 *	SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status)
 *
 *	SEXP R_ParseVector(TextBuffer *text, int n, ParseStatus *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int n, ParseStatus *status)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing failed
 *  for some reason.
 */

static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[50], *contextp;

static void ParseInit()
{
    contextp = contextstack;
    *contextp = ' ';
    SavedToken = 0;
    SavedLval = R_NilValue;
    EatLines = 0;
    EndOfFile = 0;
    FunctionLevel=0;
    SourcePtr = FunctionSource;
    xxcharcount = 0;
    KeepSource = *LOGICAL(GetOption(install("keep.source"), R_NilValue));
}

static SEXP R_Parse1(ParseStatus *status)
{
    switch(yyparse()) {
    case 0:                     /* End of file */
        *status = PARSE_EOF;
        if (EndOfFile == 2) *status = PARSE_INCOMPLETE;
        break;
    case 1:                     /* Syntax error / incomplete */
        *status = PARSE_ERROR;
        if (EndOfFile) *status = PARSE_INCOMPLETE;
        break;
    case 2:                     /* Empty Line */
        *status = PARSE_NULL;
        break;
    case 3:                     /* Valid expr '\n' terminated */
    case 4:                     /* Valid expr ';' terminated */
        *status = PARSE_OK;
        break;
    }
    return R_CurrentExpr;
}

static FILE *fp_parse;

static int file_getc(void)
{
    return R_fgetc(fp_parse);
}

static int file_ungetc(int c)
{
    return ungetc(c, fp_parse);
}

SEXP R_Parse1File(FILE *fp, int gencode, ParseStatus *status)
{
    ParseInit();
    GenerateCode = gencode;
    fp_parse = fp;
    ptr_getc = file_getc;
    ptr_ungetc = file_ungetc;
    R_Parse1(status);
    return R_CurrentExpr;
}

static IoBuffer *iob;

static int buffer_getc()
{
    return R_IoBufferGetc(iob);
}

static int buffer_ungetc(int c)
{
    return R_IoBufferUngetc(c, iob);
}

SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status)
{
    ParseInit();
    GenerateCode = gencode;
    iob = buffer;
    ptr_getc = buffer_getc;
    ptr_ungetc = buffer_ungetc;
    R_Parse1(status);
    return R_CurrentExpr;
}

static TextBuffer *txtb;

static int text_getc()
{
    return R_TextBufferGetc(txtb);
}

static int text_ungetc(int c)
{
    return R_TextBufferUngetc(c, txtb);
}

SEXP R_Parse1Vector(TextBuffer *textb, int gencode, ParseStatus *status)
{
    ParseInit();
    GenerateCode = gencode;
    txtb = textb;
    ptr_getc = text_getc;
    ptr_ungetc = text_ungetc;
    R_Parse1(status);
    return R_CurrentExpr;
}

#define GENERAL
#ifdef GENERAL

SEXP R_Parse1General(int (*g_getc)(), int (*g_ungetc)(),
		     int gencode, ParseStatus *status)
{
    ParseInit();
    GenerateCode = gencode;
    ptr_getc = g_getc;
    ptr_ungetc = g_ungetc;
    R_Parse1(status);
    return R_CurrentExpr;
}
#endif

SEXP R_Parse(int n, ParseStatus *status)
{
    int i;
    SEXP t, rval;
    if (n >= 0) {
        PROTECT(rval = allocVector(EXPRSXP, n));
        for (i = 0 ; i < n ; i++) {
        try_again:
	    ParseInit();
            t = R_Parse1(status);
            switch(*status) {
            case PARSE_NULL:
                goto try_again;
                break;
            case PARSE_OK:
                SET_VECTOR_ELT(rval, i, t);
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
	    ParseInit();
            rval = R_Parse1(status);
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
                for (n = 0 ; n < LENGTH(rval) ; n++) {
                    SET_VECTOR_ELT(rval, n, CAR(t));
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

SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    fp_parse = fp;
    ptr_getc = file_getc;
    ptr_ungetc = file_ungetc;
    return R_Parse(n, status);
}

#include "Rconnections.h"
static Rconnection con_parse;

/* need to handle incomplete last line */
static int con_getc(void)
{
    int c;
    static int last=-1000;
    
    c = Rconn_fgetc(con_parse);
    if (c == EOF && last != '\n') c = '\n';
    return (last = c);
}

static int con_ungetc(int c)
{
    return Rconn_ungetc(c, con_parse);
}

SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    con_parse = con;;
    ptr_getc = con_getc;
    ptr_ungetc = con_ungetc;
    return R_Parse(n, status);
}

SEXP R_ParseVector(SEXP text, int n, ParseStatus *status)
{
    SEXP rval;
    TextBuffer textb;
    R_TextBufferInit(&textb, text);
    txtb = &textb;
    GenerateCode = 1;
    R_ParseError = 1;
    ptr_getc = text_getc;
    ptr_ungetc = text_ungetc;
    rval = R_Parse(n, status);
    R_TextBufferFree(&textb);
    return rval;
}

#ifdef GENERAL
SEXP R_ParseGeneral(int (*ggetc)(), int (*gungetc)(), int n,
		    ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    ptr_getc = ggetc;
    ptr_ungetc = gungetc;
    return R_Parse(n, status);
}
#endif

static char *Prompt(SEXP prompt, int type)
{
    if(type == 1) {
	if(length(prompt) <= 0) {
	    return (char*)CHAR(STRING_ELT(GetOption(install("prompt"),
						    R_NilValue), 0));
	}
	else
	    return CHAR(STRING_ELT(prompt, 0));
    }
    else {
	return (char*)CHAR(STRING_ELT(GetOption(install("continue"),
						R_NilValue), 0));
    }
}

SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt)
{
    SEXP rval, t;
    char *bufp, buf[1024];
    int c, i, prompt_type = 1;

    R_IoBufferWriteReset(buffer);
    buf[0] = '\0';
    bufp = buf;
    if (n >= 0) {
	PROTECT(rval = allocVector(EXPRSXP, n));
	for (i = 0 ; i < n ; i++) {
	try_again:
	    if(!*bufp) {
		if(R_ReadConsole(Prompt(prompt, prompt_type),
				 (unsigned char *)buf, 1024, 1) == 0)
		    return R_NilValue;
		bufp = buf;
	    }
	    while ((c = *bufp++)) {
		R_IoBufferPutc(c, buffer);
		if (c == ';' || c == '\n') {
		    break;
		}
	    }
	    t = R_Parse1Buffer(buffer, 1, status);
	    switch(*status) {
	    case PARSE_NULL:
		goto try_again;
		break;
	    case PARSE_OK:
		SET_VECTOR_ELT(rval, i, t);
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
	for (;;) {
	    if (!*bufp) {
		if(R_ReadConsole(Prompt(prompt, prompt_type),
				 (unsigned char *)buf, 1024, 1) == 0)
		   return R_NilValue;
		bufp = buf;
	    }
	    while ((c = *bufp++)) {
		R_IoBufferPutc(c, buffer);
		if (c == ';' || c == '\n') {
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
		for (n = 0 ; n < LENGTH(rval) ; n++) {
		    SET_VECTOR_ELT(rval, n, CAR(t));
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
 *  The Lexical Analyzer:
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
 */

static void IfPush(void)
{
    if (*contextp==LBRACE ||
	*contextp=='['    ||
	*contextp=='('    ||
	*contextp == 'i')
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
    { "NULL",	    NULL_CONST },
    { "NA",	    NUM_CONST  },
    { "TRUE",	    NUM_CONST  },
    { "FALSE",	    NUM_CONST  },
    { "GLOBAL.ENV", NUM_CONST  },
    { "Inf",	    NUM_CONST  },
    { "NaN",	    NUM_CONST  },
    { "function",   FUNCTION   },
    { "while",	    WHILE      },
    { "repeat",	    REPEAT     },
    { "for",	    FOR	       },
    { "if",	    IF	       },
    { "in",	    IN	       },
    { "else",	    ELSE       },
    { "next",	    NEXT       },
    { "break",	    BREAK      },
    { "...",	    SYMBOL     },
    { 0,	    0	       }
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
		    break;
		case 5:
		    PROTECT(yylval = allocVector(REALSXP, 1));
		    REAL(yylval)[0] = R_PosInf;
		    break;
		case 6:
		    PROTECT(yylval = allocVector(REALSXP, 1));
		    REAL(yylval)[0] = R_NaN;
		    break;
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


SEXP mkString(yyconst char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkChar(s));
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

void yyerror(char *s)
{
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
    while (formlist != R_NilValue) {
	if (TAG(formlist) == new) {
	    error("Repeated formal argument");
	}
	formlist = CDR(formlist);
    }
}

static char yytext[MAXELTSIZE];

#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext >= sizeof(yytext) - 1) \
        error("input buffer overflow"); \
	*(bp)++ = (c); \
} while(0)

static int SkipSpace(void)
{
    int c;
    while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f')
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
    DECLARE_YYTEXT_BUFP(yyp);
    int c;
    YYTEXT_PUSH('#', yyp);
    while ((c = xxgetc()) != '\n' && c != R_EOF)
	YYTEXT_PUSH(c, yyp);
    YYTEXT_PUSH('\0', yyp);
    if (c == R_EOF) EndOfFile = 2;
    return c;
}

static int NumericValue(int c)
{
    int seendot = (c == '.');
    int seenexp = 0;
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E') {
	if (c == 'E' || c == 'e') {
	    if (seenexp)
		break;
	    seenexp = 1;
	    seendot = 1;
	    YYTEXT_PUSH(c, yyp);
	    c = xxgetc();
	    if (!isdigit(c) && c != '+' && c != '-')
		break;
	}
	if (c == '.') {
	    if (seendot)
		break;
	    seendot = 1;
	}
	YYTEXT_PUSH(c, yyp);
    }
    YYTEXT_PUSH('\0', yyp);
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

/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */

static int StringValue(int c)
{
    int quote = c;
    DECLARE_YYTEXT_BUFP(yyp);
    while ((c = xxgetc()) != R_EOF && c != quote) {
	if (c == '\n') {
	    xxungetc(c);
	    /* Fix by Mark Bravington to allow multiline strings
             * by pretending we've seen a backslash. Was:
	     * return ERROR;
             */
	    c = '\\';
	}
	if (c == '\\') {
	    c = xxgetc();
	    if ('0' <= c && c <= '8') {
		int octal = c - '0';
		if ('0' <= (c = xxgetc()) && c <= '8') {
		    octal = 8 * octal + c - '0';
		    if ('0' <= (c = xxgetc()) && c <= '8') {
			octal = 8 * octal + c - '0';
		    }
		    else xxungetc(c);
		}
		else xxungetc(c);
		c = octal;
	    }
	    else {
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
	}
	YYTEXT_PUSH(c, yyp);
    }
    YYTEXT_PUSH('\0', yyp);
    PROTECT(yylval = mkString(yytext));
    return STR_CONST;
}

static int QuotedSymbolValue(int c)
{
    (void) StringValue(c); /* always returns STR_CONST */
    UNPROTECT(1);
    PROTECT(yylval = install(yytext));
    return SYMBOL;
}

static int SpecialValue(int c)
{
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    while ((c = xxgetc()) != R_EOF && c != '%') {
	if (c == '\n') {
	    xxungetc(c);
	    return ERROR;
	}
	YYTEXT_PUSH(c, yyp);
    }
    if (c == '%')
	YYTEXT_PUSH(c, yyp);
    YYTEXT_PUSH('\0', yyp);
    yylval = install(yytext);
    return SPECIAL;
}

#ifdef SUPPORT_UTF8
# include <wchar.h>
# include <wctype.h>
#endif

/* return 1 if name is a valid name 0 otherwise */
int isValidName(char *name)
{
    char *p = name;
    int i;

#ifdef SUPPORT_UTF8
    if(utf8locale) {
	/* the only way to establish which chars are alpha etc is to
	   use the wchar variants */
	int n = strlen(name), used;
	wchar_t wc;
	used = mbrtowc(&wc, p, n, NULL); p += used; n -= used;
	if (wc != '.' && !iswalpha(wc) ) return 0;
	if (wc == '.') {
	    mbrtowc(&wc, p, n, NULL);
	    if(iswdigit(wc)) return 0;
	}
	while((used = mbrtowc(&wc, p, n, NULL))) {
	    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
	    p += used; n -= used;
	}
	if (*p != '\0') return 0;
    } else {
	int c = *p++;
	if (c != '.' && !isalpha(c) ) return 0;
	if (c == '.' && isdigit((int)*p)) return 0;
	while ( c = *p++, (isalnum(c) || c == '.' || c == '_') ) ;
	if (c != '\0') return 0;
    }
#else
    int c = *p++;
    if (c != '.' && !isalpha(c) ) return 0;
    if (c == '.' && isdigit((int)*p)) return 0;
    while ( c = *p++, (isalnum(c) || c == '.' || c == '_') ) ;
    if (c != '\0') return 0;
#endif

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++)
        if (strcmp(keywords[i].name, name) == 0) return 0;

    return 1;
}


static int SymbolValue(int c)
{
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
    do {
	YYTEXT_PUSH(c, yyp);
    }
    while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.' || c == '_'));
    xxungetc(c);
    YYTEXT_PUSH('\0', yyp);
    if ((kw = KeywordLookup(yytext))) {
	if ( kw == FUNCTION ) {
	    if (FunctionLevel >= MAXNEST)
		error("functions nested too deeply in source code");
	    if ( FunctionLevel++ == 0 && GenerateCode) {
		strcpy((char *)FunctionSource, "function");
		SourcePtr = FunctionSource + 8;
	    }
	    FunctionStart[FunctionLevel] = SourcePtr - 8;
#if 0
	    printf("%d,%d\n", SourcePtr - FunctionSource, FunctionLevel);
#endif
	}
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
    if (SavedToken) {
	c = SavedToken;
	yylval = SavedLval;
	SavedLval = R_NilValue;
	SavedToken = 0;
	return c;
    }
    xxcharsave = xxcharcount; /* want to be able to go back one token */

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

    if (c == '.' || isdigit(c))
	return NumericValue(c);

    /* literal strings */

    if (c == '\"' || c == '\'')
	return StringValue(c);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return QuotedSymbolValue(c);
 symbol:

    if (c == '.' || isalpha(c))
	return SymbolValue(c);

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
	if (nextchar('<')) {
	    if (nextchar('-')) {
		yylval = install("<<-");
		return LEFT_ASSIGN;
	    }
	    else
		return ERROR;
	}
	yylval = install("<");
	return LT;
    case '-':
	if (nextchar('>')) {
	    if (nextchar('>')) {
		yylval = install("<<-");
		return RIGHT_ASSIGN;
	    }
	    else {
		yylval = install("<-");
		return RIGHT_ASSIGN;
	    }
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
	yylval = install("=");
	return EQ_ASSIGN;
    case ':':
	if (nextchar(':')) {
            if (nextchar(':')) {
		yylval = install(":::");
		return NS_GET_INT;
	    }
	    else {
		yylval = install("::");
		return NS_GET;
	    }
	}
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
	strcpy(yytext, "?");
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
    case '@':
	yytext[0] = c;
	yytext[1] = '\0';
	yylval = install(yytext);
	return c;
    default:
	return c;
    }
}

int yylex(void)
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
    case NE:
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
    case '@':
    case LEFT_ASSIGN:
    case RIGHT_ASSIGN:
    case EQ_ASSIGN:
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
