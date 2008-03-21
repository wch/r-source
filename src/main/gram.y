%{
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2008  Robert Gentleman, Ross Ihaka and the
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
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */

/* <UTF8>
   This uses byte-level access, which is generally OK as comparisons
   are with ASCII chars.

   typeofnext SymbolValue isValidName have been changed to cope.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "IOStuff.h"		/*-> Defn.h */
#include "Fileio.h"
#include "Parse.h"

#define YYERROR_VERBOSE 1

static void yyerror(char *);
static int yylex();
int yyparse(void);

/* alloca.h inclusion is now covered by Defn.h */

#define yyconst const

typedef struct yyltype
{
  int first_line;
  int first_column;

  int last_line;
  int last_column;
} yyltype;

# define YYLTYPE yyltype

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static void	CheckFormalArgs(SEXP, SEXP, YYLTYPE *);
static SEXP	FirstArg(SEXP, SEXP);
static SEXP	GrowList(SEXP, SEXP);
static void	IfPush(void);
static int	KeywordLookup(const char *);
static SEXP	NewList(void);
static SEXP	NextArg(SEXP, SEXP, SEXP);
static SEXP	TagArg(SEXP, SEXP, YYLTYPE *);

/* These routines allocate constants */

static SEXP	mkComplex(const char *);
SEXP		mkFalse(void);
static SEXP     mkFloat(const char *);
static SEXP	mkNA(void);
SEXP		mkTrue(void);

/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxgetc();
static int	xxungetc(int);
static int 	xxcharcount, xxcharsave;
static int	xxlineno, xxcolno, xxlinesave, xxcolsave;
static int	xxlastlinelen;

static SEXP     SrcFile = NULL;
static SEXP	SrcRefs = NULL;
static PROTECT_INDEX srindex;

#if defined(SUPPORT_MBCS)
# include <R_ext/rlocale.h>
/* # include <sys/param.h> what was this for? */
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif


static int mbcs_get_next(int c, wchar_t *wc)
{
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
	*wc = (wchar_t) c;
	return 1;
    }
    if(utf8locale) {
	clen = utf8clen(c);
	for(i = 1; i < clen; i++) {
	    s[i] = xxgetc();
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
	}
	res = mbrtowc(wc, s, clen, NULL);
	if(res == -1) error(_("invalid multibyte character in mbcs_get_next at line %d"), xxlineno);
    } else {
	/* This is not necessarily correct for stateful MBCS */
	while(clen <= MB_CUR_MAX) {
	    mbs_init(&mb_st);
	    res = mbrtowc(wc, s, clen, &mb_st);
	    if(res >= 0) break;
	    if(res == -1) 
		error(_("invalid multibyte character in mbcs_get_next at line %d"), xxlineno);
	    /* so res == -2 */
	    c = xxgetc();
	    if(c == R_EOF) error(_("EOF whilst reading MBCS char"));
	    s[clen++] = c;
	} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--) xxungetc(s[i]);
    return clen;
}

#endif

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
static SEXP	xxaddformal0(SEXP, SEXP, YYLTYPE *);
static SEXP	xxaddformal1(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxexprlist0();
static SEXP	xxexprlist1(SEXP, YYLTYPE *);
static SEXP	xxexprlist2(SEXP, SEXP, YYLTYPE *);
static SEXP	xxsub0(void);
static SEXP	xxsub1(SEXP, YYLTYPE *);
static SEXP	xxsymsub0(SEXP, YYLTYPE *);
static SEXP	xxsymsub1(SEXP, SEXP, YYLTYPE *);
static SEXP	xxnullsub0(YYLTYPE *);
static SEXP	xxnullsub1(SEXP, YYLTYPE *);
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
static int	xxvalue(SEXP, int, YYLTYPE *);

#define YYSTYPE		SEXP

%}

%token		END_OF_INPUT ERROR
%token		STR_CONST NUM_CONST NULL_CONST SYMBOL FUNCTION 
%token		LEFT_ASSIGN EQ_ASSIGN RIGHT_ASSIGN LBB
%token		FOR IN IF ELSE WHILE NEXT BREAK REPEAT
%token		GT GE LT LE EQ NE AND OR AND2 OR2
%token		NS_GET NS_GET_INT

/* This is the precedence table, low to high */
%left		'?'
%left		LOW WHILE FOR REPEAT
%right		IF
%left		ELSE
%right		LEFT_ASSIGN
%right		EQ_ASSIGN
%left		RIGHT_ASSIGN
%left		'~' TILDE
%left		OR OR2
%left		AND AND2
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
	|	'\n'				{ return xxvalue(NULL,2,NULL); }
	|	expr_or_assign '\n'			{ return xxvalue($1,3,&@1); }
	|	expr_or_assign ';'			{ return xxvalue($1,4,&@1); }
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
	|	expr AND2 expr			{ $$ = xxbinary($2,$1,$3); }
	|	expr OR2 expr			{ $$ = xxbinary($2,$1,$3); }

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
	|	expr_or_assign			{ $$ = xxexprlist1($1, &@1); }
	|	exprlist ';' expr_or_assign	{ $$ = xxexprlist2($1, $3, &@3); }
	|	exprlist ';'			{ $$ = $1; }
	|	exprlist '\n' expr_or_assign	{ $$ = xxexprlist2($1, $3, &@3); }
	|	exprlist '\n'			{ $$ = $1;}
	;

sublist	:	sub				{ $$ = xxsublist1($1); }
	|	sublist cr ',' sub		{ $$ = xxsublist2($1,$4); }
	;

sub	:					{ $$ = xxsub0(); }
	|	expr				{ $$ = xxsub1($1, &@1); }
	|	SYMBOL EQ_ASSIGN 			{ $$ = xxsymsub0($1, &@1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxsymsub1($1,$3, &@1); }
	|	STR_CONST EQ_ASSIGN 			{ $$ = xxsymsub0($1, &@1); }
	|	STR_CONST EQ_ASSIGN expr		{ $$ = xxsymsub1($1,$3, &@1); }
	|	NULL_CONST EQ_ASSIGN 			{ $$ = xxnullsub0(&@1); }
	|	NULL_CONST EQ_ASSIGN expr		{ $$ = xxnullsub1($3, &@1); }
	;

formlist:					{ $$ = xxnullformal(); }
	|	SYMBOL				{ $$ = xxfirstformal0($1); }
	|	SYMBOL EQ_ASSIGN expr			{ $$ = xxfirstformal1($1,$3); }
	|	formlist ',' SYMBOL		{ $$ = xxaddformal0($1,$3, &@3); }
	|	formlist ',' SYMBOL EQ_ASSIGN expr	{ $$ = xxaddformal1($1,$3,$5,&@3); }
	;

cr	:					{ EatLines = 1; }
	;
%%


/*----------------------------------------------------------------------------*/

static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth */

static int pushback[16];
static unsigned int npush = 0;

static int xxgetc(void)
{
    int c;

    if(npush) c = pushback[--npush]; else  c = ptr_getc();
    if (c == EOF) {
        EndOfFile = 1;
        return R_EOF;
    }
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = c;
    
    if (c == '\n') {
    	xxlineno += 1;
    	xxlastlinelen = xxcolno; 
    	xxcolno = 0;
    } else xxcolno++;
    
    if ( KeepSource && GenerateCode && FunctionLevel > 0 ) {
	if(SourcePtr <  FunctionSource + MAXFUNSIZE)
	    *SourcePtr++ = c;
	else  error(_("function is too long to keep source (at line %d)"), xxlineno);
    }
    xxcharcount++;
    return c;
}

static int xxungetc(int c)
{
    if (c == '\n') {
    	xxlineno -= 1;
    	xxcolno = xxlastlinelen; /* FIXME:  could we push back more than one line? */
    	xxlastlinelen = 0;
    } else xxcolno--;
    
    if ( KeepSource && GenerateCode && FunctionLevel > 0 )
	SourcePtr--;
    xxcharcount--;
    R_ParseContext[R_ParseContextLast--] = '\0';
    R_ParseContextLast = R_ParseContextLast % PARSE_CONTEXT_SIZE;
    if(npush >= 16) return EOF;
    pushback[npush++] = c;
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile)
{
    SEXP val;
    
    PROTECT(val = allocVector(INTSXP, 4));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_column;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_column;
    setAttrib(val, R_SrcfileSymbol, srcfile);
    setAttrib(val, R_ClassSymbol, mkString("srcref"));
    UNPROTECT(1);
    return val;
}

static SEXP attachSrcrefs(SEXP val, SEXP srcfile)
{
    SEXP t, srval;
    int n;
    
    PROTECT(val);
    t = CDR(SrcRefs);
    srval = allocVector(VECSXP, length(t));
    for (n = 0 ; n < LENGTH(srval) ; n++, t = CDR(t))
    	SET_VECTOR_ELT(srval, n, CAR(t));
    setAttrib(val, R_SrcrefSymbol, srval);
    setAttrib(val, R_SrcfileSymbol, srcfile);
    UNPROTECT(1);
    SrcRefs = NULL;
    return val;
}

static int xxvalue(SEXP v, int k, YYLTYPE *lloc)
{
    if (k > 2) {
    	if (SrcFile)
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
    	UNPROTECT_PTR(v);
    }
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

static SEXP xxaddformal0(SEXP formlist, SEXP sym, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
	CheckFormalArgs(formlist, sym, lloc);
	PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
	CheckFormalArgs(formlist, sym, lloc);
	PROTECT(ans = NextArg(formlist, expr, sym));
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

static SEXP xxexprlist0(void)
{
    SEXP ans;
    if (GenerateCode) {
	PROTECT(ans = NewList());
	if (SrcFile) {
	    setAttrib(ans, R_SrcrefSymbol, SrcRefs);
    	    REPROTECT(SrcRefs = NewList(), srindex);
    	}
    } 
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxexprlist1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans,tmp;
    if (GenerateCode) {
	PROTECT(tmp = NewList());
	if (SrcFile) {
	    setAttrib(tmp, R_SrcrefSymbol, SrcRefs);
    	    REPROTECT(SrcRefs = NewList(), srindex);
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
    	}	
	PROTECT(ans = GrowList(tmp, expr));
	UNPROTECT_PTR(tmp);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
	if (SrcFile) 
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);   
	PROTECT(ans = GrowList(exprlist, expr));
    }
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

static SEXP xxsub1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, R_NilValue, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxsymsub0(SEXP sym, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, sym, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, sym, lloc));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

static SEXP xxnullsub0(YYLTYPE *lloc)
{
    SEXP ans;
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(R_MissingArg, install("NULL"), lloc));
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxnullsub1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans = install("NULL");
    UNPROTECT_PTR(R_NilValue);
    if (GenerateCode)
	PROTECT(ans = TagArg(expr, ans, lloc));
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


static SEXP mkChar2(const char *name)
{
    if(!strIsASCII(name)) {
	if(known_to_be_latin1) return mkCharEnc(name, LATIN1_MASK);
	else if(known_to_be_utf8) return mkCharEnc(name, UTF8_MASK);
    }
    return mkChar(name);
}

static SEXP mkString2(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkChar2(s));
    UNPROTECT(1);
    return t;
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
	    /* FIXME: this should be whitespace */
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
				       mkChar2((char *)SourceLine));
		    } else { /* over-long line */
			char *LongLine = (char *) malloc(nc);
			if(!LongLine) 
			    error(_("unable to allocate space for source line %d"), xxlineno);
			strncpy(LongLine, (char *)p0, nc);
			LongLine[nc] = '\0';
			SET_STRING_ELT(source, lines++,
				       mkChar2((char *)LongLine));
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
	PROTECT(ans = LCONS(a2, CONS(a1, CDR(a3))));
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a3);
    UNPROTECT_PTR(a1);
    return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
    SEXP ans;
    SEXP prevSrcrefs;
    
    EatLines = 0;
    if (GenerateCode) {
	SET_TYPEOF(a2, LANGSXP);
	SETCAR(a2, a1);
	if (SrcFile) {
	    PROTECT(prevSrcrefs = getAttrib(a2, R_SrcrefSymbol));
	    PROTECT(ans = attachSrcrefs(a2, SrcFile));
	    REPROTECT(SrcRefs = prevSrcrefs, srindex);
	    /* SrcRefs got NAMED by being an attribute... */
	    SET_NAMED(SrcRefs, 0);
	    UNPROTECT_PTR(prevSrcrefs);
	} 
	else
	    PROTECT(ans = a2);	
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(a2);
    return ans;
}

/*--------------------------------------------------------------------------*/

static SEXP TagArg(SEXP arg, SEXP tag, YYLTYPE *lloc)
{
    switch (TYPEOF(tag)) {
    case STRSXP:
    	tag = install(translateChar(STRING_ELT(tag, 0)));
    case NILSXP:
    case SYMSXP:
	return lang2(arg, tag);
    default:
	error(_("incorrect tag type at line %d"), lloc->first_line); return R_NilValue/* -Wall */;
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
 *  Note that there are separate entry points for parsing IoBuffers
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
 *      [Unused]
 *
 *	SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status)
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
 *	SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
 *
 *	SEXP R_ParseVector(SEXP *text, int n, ParseStatus *status, SEXP srcfile)
 *
 *	SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt, SEXP srcfile)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing failed
 *  for some reason.
 */

#define CONTEXTSTACK_SIZE 50
static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[CONTEXTSTACK_SIZE], *contextp;

static void ParseInit(void)
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
    KeepSource = *LOGICAL(GetOption(install("keep.source"), R_BaseEnv));
    npush = 0;
}

static void ParseContextInit(void)
{
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
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

/* used in main.c and this file */
attribute_hidden
SEXP R_Parse1File(FILE *fp, int gencode, ParseStatus *status)
{
    ParseInit();
    ParseContextInit();
    GenerateCode = gencode;
    fp_parse = fp;
    ptr_getc = file_getc;
    R_Parse1(status);
    return R_CurrentExpr;
}

static IoBuffer *iob;

static int buffer_getc(void)
{
    return R_IoBufferGetc(iob);
}

/* Used only in main.c, rproxy_impl.c */
attribute_hidden
SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status)
{
    xxlineno = 1;
    xxcolno = 0;
    ParseInit();
    ParseContextInit();
    GenerateCode = gencode;
    iob = buffer;
    ptr_getc = buffer_getc;
    R_Parse1(status);
    return R_CurrentExpr;
}

static TextBuffer *txtb;

static int text_getc(void)
{
    return R_TextBufferGetc(txtb);
}


/* unused */
#ifdef PARSE_UNUSED
SEXP R_Parse1Vector(TextBuffer *textb, int gencode, ParseStatus *status)
{
    ParseInit();
    ParseContextInit();
    GenerateCode = gencode;
    txtb = textb;
    ptr_getc = text_getc;
    R_Parse1(status);
    return R_CurrentExpr;
}
#endif


#ifdef PARSE_UNUSED
/* Not used, and note ungetc is no longer needed */
attribute_hidden
SEXP R_Parse1General(int (*g_getc)(), int (*g_ungetc)(),
		     int gencode, ParseStatus *status)
{
    ParseInit();
    ParseContextInit();
    GenerateCode = gencode;
    ptr_getc = g_getc;
    R_Parse1(status);
    return R_CurrentExpr;
}
#endif

static SEXP R_Parse(int n, ParseStatus *status, SEXP srcfile)
{
    volatile int savestack;
    int i;
    SEXP t, rval;

    ParseContextInit();
    savestack = R_PPStackTop;
    PROTECT(t = NewList());
    
    xxlineno = 1;
    xxcolno = 0;    
    if (!isNull(srcfile)) {
	SrcFile = srcfile;
	PROTECT_WITH_INDEX(SrcRefs = NewList(), &srindex);
    } 
    else SrcFile = NULL;
    
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;
	ParseInit();
	rval = R_Parse1(status);
	switch(*status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
	    t = GrowList(t, rval);
	    i++;
	    break;
	case PARSE_INCOMPLETE:
	case PARSE_ERROR:
	    R_PPStackTop = savestack;
	    return R_NilValue;
	    break;
	case PARSE_EOF:
	    goto finish;
	    break;
	}
    }

finish:

    t = CDR(t);
    rval = allocVector(EXPRSXP, length(t));
    for (n = 0 ; n < LENGTH(rval) ; n++, t = CDR(t))
	SET_VECTOR_ELT(rval, n, CAR(t));
    if (SrcFile) {
    	rval = attachSrcrefs(rval, SrcFile);
        SrcFile = NULL;    
    }
    R_PPStackTop = savestack;
    *status = PARSE_OK;
    return rval;
}

/* used in edit.c */
attribute_hidden
SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    fp_parse = fp;
    ptr_getc = file_getc;
    return R_Parse(n, status, srcfile);
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

/* used in source.c */
attribute_hidden
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    con_parse = con;
    ptr_getc = con_getc;
    return R_Parse(n, status, srcfile);
}

/* This one is public, and used in source.c */
SEXP R_ParseVector(SEXP text, int n, ParseStatus *status, SEXP srcfile)
{
    SEXP rval;
    TextBuffer textb;
    R_TextBufferInit(&textb, text);
    txtb = &textb;
    GenerateCode = 1;
    ptr_getc = text_getc;
    rval = R_Parse(n, status, srcfile);
    R_TextBufferFree(&textb);
    return rval;
}

#ifdef PARSE_UNUSED
/* Not used, and note ungetc is no longer needed */
SEXP R_ParseGeneral(int (*ggetc)(), int (*gungetc)(), int n,
		    ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    ptr_getc = ggetc;
    return R_Parse(n, status, srcfile);
}
#endif

static const char *Prompt(SEXP prompt, int type)
{
    if(type == 1) {
	if(length(prompt) <= 0) {
	    return CHAR(STRING_ELT(GetOption(install("prompt"),
					     R_BaseEnv), 0));
	}
	else
	    return CHAR(STRING_ELT(prompt, 0));
    }
    else {
	return CHAR(STRING_ELT(GetOption(install("continue"),
					 R_BaseEnv), 0));
    }
}

/* used in source.c */
attribute_hidden
SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt, SEXP srcfile)
{
    SEXP rval, t;
    char *bufp, buf[1024];
    int c, i, prompt_type = 1;
    volatile int savestack;

    R_IoBufferWriteReset(buffer);
    buf[0] = '\0';
    bufp = buf;
    savestack = R_PPStackTop;
    PROTECT(t = NewList());
    
    xxlineno = 1;
    xxcolno = 0;      
    GenerateCode = 1;
    iob = buffer;
    ptr_getc = buffer_getc;    

    if (!isNull(srcfile)) {
	SrcFile = srcfile;
	PROTECT_WITH_INDEX(SrcRefs = NewList(), &srindex);
    }      
    else SrcFile = NULL;
    
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;
	if (!*bufp) {
	    if(R_ReadConsole((char *) Prompt(prompt, prompt_type),
			     (unsigned char *)buf, 1024, 1) == 0)
		goto finish;
	    bufp = buf;
	}
	while ((c = *bufp++)) {
	    R_IoBufferPutc(c, buffer);
	    if (c == ';' || c == '\n') break;
	}

	rval = R_Parse1Buffer(buffer, 1, status);
	
	/* Was a call to R_Parse1Buffer, but we don't want to reset xxlineno and xxcolno */
	ParseInit();
	ParseContextInit();
	R_Parse1(status);
        rval = R_CurrentExpr;
    
	switch(*status) {
	case PARSE_NULL:
	    break;
	case PARSE_OK:
	    t = GrowList(t, rval);
	    i++;
	    break;
	case PARSE_INCOMPLETE:
	case PARSE_ERROR:
	    R_IoBufferWriteReset(buffer);
	    R_PPStackTop = savestack;
	    return R_NilValue;
	    break;
	case PARSE_EOF:
	    goto finish;
	    break;
	}
    }
finish:
    R_IoBufferWriteReset(buffer);
    t = CDR(t);
    rval = allocVector(EXPRSXP, length(t));
    for (n = 0 ; n < LENGTH(rval) ; n++, t = CDR(t))
	SET_VECTOR_ELT(rval, n, CAR(t));
    if (SrcFile) {
    	rval = attachSrcrefs(rval, SrcFile);
        SrcFile = NULL;    
    }	
    R_PPStackTop = savestack;
    *status = PARSE_OK;
    return rval;
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
	*contextp == 'i') {
	if(contextp - contextstack >= CONTEXTSTACK_SIZE) 
	    error(_("contextstack overflow"));
	*++contextp = 'i';
    }
    
}

static void ifpop(void)
{
    if (*contextp=='i')
	*contextp-- = 0;
}

/* This is only called following ., so we only care if it is 
   an ANSI digit or not */
static int typeofnext(void)
{
    int k, c;

    c = xxgetc();
    if (isdigit(c)) k = 1; else k = 2;
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
static keywords[] = {
    { "NULL",	    NULL_CONST },
    { "NA",	    NUM_CONST  },
    { "TRUE",	    NUM_CONST  },
    { "FALSE",	    NUM_CONST  },
    { "Inf",	    NUM_CONST  },
    { "NaN",	    NUM_CONST  },
    { "NA_integer_", NUM_CONST  },
    { "NA_real_",    NUM_CONST  },
    { "NA_character_", NUM_CONST  },
    { "NA_complex_", NUM_CONST  },
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

static int KeywordLookup(const char *s)
{
    int i;
    for (i = 0; keywords[i].name; i++) {
	if (strcmp(keywords[i].name, s) == 0) {
	    switch (keywords[i].token) {
	    case NULL_CONST:
		PROTECT(yylval = R_NilValue);
		break;
	    case NUM_CONST:
		if(GenerateCode) {
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
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = R_PosInf;
			break;
		    case 5:
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = R_NaN;
			break;
		    case 6:
			PROTECT(yylval = allocVector(INTSXP, 1));
			INTEGER(yylval)[0] = NA_INTEGER;
			break;
		    case 7:
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = NA_REAL;
			break;
		    case 8:
			PROTECT(yylval = allocVector(STRSXP, 1));
			SET_STRING_ELT(yylval, 0, NA_STRING);
			break;
		    case 9:
			PROTECT(yylval = allocVector(CPLXSXP, 1));
			COMPLEX(yylval)[0].r = COMPLEX(yylval)[0].i = NA_REAL;
			break;
		    }
		} else
		    PROTECT(yylval = R_NilValue);
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

static SEXP mkFloat(const char *s)
{
    return ScalarReal(R_atof(s));
}

static SEXP mkInt(const char *s)
{
    double f = R_atof(s);  /* or R_strtol? */
    return ScalarInteger((int) f);
}

static SEXP mkComplex(const char *s)
{
    SEXP t = R_NilValue;
    double f;
    f = R_atof(s); /* FIXME: make certain the value is legitimate. */

    if(GenerateCode) {
       t = allocVector(CPLXSXP, 1);
       COMPLEX(t)[0].r = 0;
       COMPLEX(t)[0].i = f;
    }

    return t;
}

static SEXP mkNA(void)
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

static void yyerror(char *s)
{
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */    
#define YYENGLISH 8
	"$undefined",	"input", 	
	"END_OF_INPUT",	"end of input",
	"ERROR",	"input", 	
	"STR_CONST",	"string constant",
	"NUM_CONST",	"numeric constant",
	"SYMBOL",	"symbol",	
	"LEFT_ASSIGN",	"assignment",	
	"'\\n'",	"end of line",	
	"NULL_CONST",	"'NULL'",
	"FUNCTION",	"'function'",
	"EQ_ASSIGN",	"'='",
	"RIGHT_ASSIGN",	"'->'",
	"LBB",		"'[['",
	"FOR",		"'for'",
	"IN",		"'in'",
	"IF",		"'if'",
	"ELSE",		"'else'",
	"WHILE",	"'while'",
	"NEXT",		"'next'",
	"BREAK",	"'break'",
	"REPEAT",	"'repeat'",
	"GT",		"'>'",
	"GE",		"'>='",
	"LT",		"'<'",
	"LE",		"'<='",
	"EQ",		"'=='",
	"NE",		"'!='",
	"AND",		"'&'",
	"OR",		"'|'",
	"AND2",		"'&&'",
	"OR2",		"'||'",
	"NS_GET",	"'::'",
	"NS_GET_INT",	"':::'",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("end of input");
    _("string constant");
    _("numeric constant");
    _("symbol");	
    _("assignment");
    _("end of line");
#endif 
   
    R_ParseError = xxlineno;
    R_ParseErrorFile = SrcFile;
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i = 0; yytname_translations[i]; i += 2) {
    	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	    	sprintf(R_ParseErrorMsg, _("unexpected %s"), 
    	    	    i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	return;
    	    }
    	}
    	sprintf(R_ParseErrorMsg, _("unexpected %s"), s + sizeof yyunexpected - 1);
    } else {
    	strncpy(R_ParseErrorMsg, s, PARSE_ERROR_SIZE - 1);
    }	
}

static void CheckFormalArgs(SEXP formlist, SEXP _new, YYLTYPE *lloc)
{
    while (formlist != R_NilValue) {
	if (TAG(formlist) == _new) {
	    error(_("Repeated formal argument '%s' on line %d"), CHAR(PRINTNAME(_new)), 
	                                                         lloc->first_line);
	}
	formlist = CDR(formlist);
    }
}

static char yytext[MAXELTSIZE];

#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext >= sizeof(yytext) - 1) \
        error(_("input buffer overflow at line %d"), xxlineno); \
	*(bp)++ = (c); \
} while(0)

static int SkipSpace(void)
{
    int c;

#ifdef Win32
    if(!mbcslocale) { /* 0xa0 is NBSP in all 8-bit Windows locales */
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f' || 
	       (unsigned int) c == 0xa0) ;
	return c;
    } else {
	int i, clen;
	wchar_t wc;
	while (1) {
	    c = xxgetc();
	    if (c == ' ' || c == '\t' || c == '\f') continue;
	    if (c == '\n' || c == R_EOF) break;
	    if ((unsigned int) c < 0x80) break;
	    clen = mbcs_get_next(c, &wc);  /* always 2 */
	    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
	    for(i = 1; i < clen; i++) c = xxgetc();
	}
	return c;
    }
#endif
#if defined(SUPPORT_MBCS) && defined(__STDC_ISO_10646__)
    if(mbcslocale) { /* wctype functions need Unicode wchar_t */
	int i, clen;
	wchar_t wc;
	while (1) {
	    c = xxgetc();
	    if (c == ' ' || c == '\t' || c == '\f') continue;
	    if (c == '\n' || c == R_EOF) break;
	    if ((unsigned int) c < 0x80) break;
	    clen = mbcs_get_next(c, &wc);
	    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
	    for(i = 1; i < clen; i++) c = xxgetc();
	}
    } else
#endif
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f') ;
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
    int last = c;
    int nd = 0;
    int asNumeric = 0;

    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E' 
	   || c == 'x' || c == 'X' || c == 'L') 
    {
	if (c == 'L') /* must be at the end.  Won't allow 1Le3 (at present). */
	    break;

	if (c == 'x' || c == 'X') {
	    if (last != '0') break;
	    YYTEXT_PUSH(c, yyp);
	    while(isdigit(c = xxgetc()) || ('a' <= c && c <= 'f') ||
		  ('A' <= c && c <= 'F')) {
		YYTEXT_PUSH(c, yyp);
		nd++;
	    }
	    if (nd == 0) return ERROR;
	    if (c == 'p' || c == 'P') {
		YYTEXT_PUSH(c, yyp);
		c = xxgetc();
		if (!isdigit(c) && c != '+' && c != '-') return ERROR;
		if (c == '+' || c == '-') {
		    YYTEXT_PUSH(c, yyp);
		    c = xxgetc();
		}
		for(nd = 0; isdigit(c); c = xxgetc(), nd++) 
		    YYTEXT_PUSH(c, yyp);
		if (nd == 0) return ERROR;
	    }
	    break;
	}
	if (c == 'E' || c == 'e') {
	    if (seenexp)
		break;
	    seenexp = 1;
	    seendot = seendot == 1 ? seendot : 2;
	    YYTEXT_PUSH(c, yyp);
	    c = xxgetc();
	    if (!isdigit(c) && c != '+' && c != '-') return ERROR;
	    if (c == '+' || c == '-') {
		YYTEXT_PUSH(c, yyp);
		c = xxgetc();
		if (!isdigit(c)) return ERROR;
	    }
	}
	if (c == '.') {
	    if (seendot)
		break;
	    seendot = 1;
	}
	YYTEXT_PUSH(c, yyp);
	last = c;
    }
    YYTEXT_PUSH('\0', yyp);
    /* Make certain that things are okay. */
    if(c == 'L') {
	double a = R_atof(yytext);
	int b = (int) a; 
	/* We are asked to create an integer via the L, so we check that the 
	   double and int values are the same. If not, this is a problem and we
	   will not lose information and so use the numeric value.
	*/
	if(a != (double) b) {
	    if(GenerateCode) {
		if(seendot == 1 && seenexp == 0)
		    warning(_("integer literal %sL contains decimal; using numeric value"), yytext);
		else 
		    warning(_("non-integer value %s qualified with L; using numeric value"), yytext);
	    }
	    asNumeric = 1;
	    seenexp = 1;
	}
    }
    
    if(c == 'i') {
	yylval = GenerateCode ? mkComplex(yytext) : R_NilValue;
    } else if(c == 'L' && asNumeric == 0) {
	if(GenerateCode && seendot == 1 && seenexp == 0) 
	    warning(_("integer literal %sL contains unnecessary decimal point"), yytext);
	yylval = GenerateCode ? mkInt(yytext) : R_NilValue;
    } else {
	if(c != 'L')
	    xxungetc(c);
	yylval = GenerateCode ? mkFloat(yytext) : R_NilValue;
    }

    PROTECT(yylval);
    return NUM_CONST;
}

/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */


#define STEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
            nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)


/* The idea here is that if a string contains \u escapes that are not
   valid in the current locale, we should switch to UTF-8 for that
   string.  Needs wide-char support.
*/
#ifdef SUPPORT_MBCS
# ifdef Win32
#  define USE_UTF8_IF_POSSIBLE
# endif
#endif

#ifdef USE_UTF8_IF_POSSIBLE
#define WTEXT_PUSH(c) do { if(wcnt < 1000) wcs[wcnt++] = c; } while(0)

static SEXP mkStringUTF8(const wchar_t *wcs, int cnt)
{
    SEXP t;
    char *s;

/* NB: cnt includes the terminator */
#ifdef Win32
    s = alloca(cnt*4); /* UCS-2/UTF-16 so max 4 bytes per wchar_t */
#else
    s = alloca(cnt*6); /* max 6 bytes per wchar_t */
#endif
    R_CheckStack();
    wcstoutf8(s, wcs, cnt);
    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharEnc(s, UTF8_MASK));
    UNPROTECT(1);
    return t;
}
#else
#define WTEXT_PUSH(c)
#endif

#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--


static int StringValue(int c, Rboolean forSymbol)
{
    int quote = c;
    int have_warned = 0;
    char currtext[1010], *ct = currtext;
    char st0[MAXELTSIZE];
    unsigned int nstext = MAXELTSIZE;
    char *stext = st0, *bp = st0;

#ifdef USE_UTF8_IF_POSSIBLE
    int wcnt = 0;
    wchar_t wcs[1001];
    Rboolean use_wcs = FALSE;
#endif

    while ((c = xxgetc()) != R_EOF && c != quote) {
	CTEXT_PUSH(c);
	if (c == '\n') {
	    xxungetc(c);
	    /* Fix by Mark Bravington to allow multiline strings
             * by pretending we've seen a backslash. Was:
	     * return ERROR;
             */
	    c = '\\';
	}
	if (c == '\\') {
	    c = xxgetc(); CTEXT_PUSH(c);
	    if ('0' <= c && c <= '8') {
		int octal = c - '0';
		if ('0' <= (c = xxgetc()) && c <= '8') {
		    CTEXT_PUSH(c);
		    octal = 8 * octal + c - '0';
		    if ('0' <= (c = xxgetc()) && c <= '8') {
			CTEXT_PUSH(c);
			octal = 8 * octal + c - '0';
		    } else {
			xxungetc(c);
			CTEXT_POP();
		    }
		} else {
		    xxungetc(c);
		    CTEXT_POP();
		}
		c = octal;
	    }
	    else if(c == 'x') {
		int val = 0; int i, ext;
		for(i = 0; i < 2; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); CTEXT_POP(); break;}
		    val = 16*val + ext;
		}
		c = val;
	    }
	    else if(c == 'u') {
#ifndef SUPPORT_MBCS
		error(_("\\uxxxx sequences not supported (line %d)"), xxlineno);
#else
		unsigned int val = 0; int i, ext; size_t res;
		char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */ 
		Rboolean delim = FALSE;
		if((c = xxgetc()) == '{') {
		    delim = TRUE; 
		    CTEXT_PUSH(c);
		} else xxungetc(c);
		for(i = 0; i < 4; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); CTEXT_POP(); break;}
		    val = 16*val + ext;
		}
		if(delim) {
		    if((c = xxgetc()) != '}')
			error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
		    else CTEXT_PUSH(c);
		}
		WTEXT_PUSH(val);
		res = ucstomb(buff, val);
		if((int) res <= 0) {
#ifdef USE_UTF8_IF_POSSIBLE
		    if(!forSymbol) {
			use_wcs = TRUE;
		    } else
#endif
		    {
			if(delim)
			    error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
			else
			    error(_("invalid \\uxxxx sequence (line %d)"), xxlineno);
		    }
		} else
		    for(i = 0; i <  res; i++) STEXT_PUSH(buff[i]);
		continue;
#endif
	    }
	    else if(c == 'U') {
#ifndef SUPPORT_MBCS
		error(_("\\Uxxxxxxxx sequences not supported (line %d)"), xxlineno);
#else
		{
		    unsigned int val = 0; int i, ext; size_t res;
		    char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */ 
		    Rboolean delim = FALSE;
		    if((c = xxgetc()) == '{') {
			delim = TRUE;
			CTEXT_PUSH(c);
		    } else xxungetc(c);
		    for(i = 0; i < 8; i++) {
			c = xxgetc(); CTEXT_PUSH(c);
			if(c >= '0' && c <= '9') ext = c - '0';
			else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
			else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
			else {xxungetc(c); CTEXT_POP(); break;}
			val = 16*val + ext;
		    }
		    if(delim) {
			if((c = xxgetc()) != '}')
			    error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
			else CTEXT_PUSH(c);
		    }
		    res = ucstomb(buff, val);
		    if((int)res <= 0) {
			if(delim)
			    error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
			else
			    error(_("invalid \\Uxxxxxxxx sequence (line %d)"), xxlineno);
		    }
		    for(i = 0; i <  res; i++) STEXT_PUSH(buff[i]);
		    WTEXT_PUSH(val);
		    continue;
		}
#endif
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
		case '"':
		case '\'':
		case ' ':
		case '\n':
		    break;
		default:
		    if(GenerateCode && R_WarnEscapes) {
			have_warned++;
			warningcall(R_NilValue, _("'\\%c' is an unrecognized escape in a character string"), c);
		    }
		    break;
		}
	    }
	}
#if defined(SUPPORT_MBCS)
       else if(mbcslocale) {
           int i, clen;
           wchar_t wc = L'\0';
           clen = utf8locale ? utf8clen(c): mbcs_get_next(c, &wc);
	   WTEXT_PUSH(wc);
           for(i = 0; i < clen - 1; i++){
               STEXT_PUSH(c);
               c = xxgetc();
               if (c == R_EOF) break;
	       CTEXT_PUSH(c);
               if (c == '\n') {
                   xxungetc(c); CTEXT_POP();
                   c = '\\';
               }
           }
           if (c == R_EOF) break;
	   STEXT_PUSH(c);
	   continue;
       }
#endif /* SUPPORT_MBCS */
	STEXT_PUSH(c);
#ifdef USE_UTF8_IF_POSSIBLE
	if ((unsigned int) c < 0x80) WTEXT_PUSH(c);
	else { /* have an 8-bit char in the current encoding */
	    wchar_t wc;
	    char s[2] = " ";
	    s[0] = c;
	    mbrtowc(&wc, s, 1, NULL);
	    WTEXT_PUSH(wc);
	}
#endif
    }
    STEXT_PUSH('\0');
    WTEXT_PUSH(0);
    if(forSymbol) {
	PROTECT(yylval = install(stext));
	if(stext != st0) free(stext);
	return SYMBOL;
    } else {
#ifdef USE_UTF8_IF_POSSIBLE
	if(use_wcs) {
	    if(wcnt < 1000)
		PROTECT(yylval = mkStringUTF8(wcs, wcnt)); /* include terminator */
	    else
		error(_("string at line %d containing Unicode escapes not in this locale\nis too long (max 1000 chars)"), xxlineno);
	} else
#endif
	    PROTECT(yylval = mkString2(stext));
	if(stext != st0) free(stext);
	if(have_warned) {
	    *ct = '\0';
#ifdef ENABLE_NLS
	    warningcall(R_NilValue,
			ngettext("unrecognized escape removed from \"%s\"",
				 "unrecognized escapes removed from \"%s\"",
				 have_warned),
			currtext);
#else
	    warningcall(R_NilValue,
			"unrecognized escape(s) removed from \"%s\"", currtext);
#endif
	}
	return STR_CONST;
    }
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

/* return 1 if name is a valid name 0 otherwise */
int isValidName(const char *name)
{
    const char *p = name;
    int i;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
	/* the only way to establish which chars are alpha etc is to
	   use the wchar variants */
	int n = strlen(name), used;
	wchar_t wc;
	used = Mbrtowc(&wc, p, n, NULL); p += used; n -= used;
	if(used == 0) return 0;
	if (wc != L'.' && !iswalpha(wc) ) return 0;
	if (wc == L'.') {
	    /* We don't care about other than ASCII digits */
	    if(isdigit(0xff & (int)*p)) return 0;
	    /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
	}
	while((used = Mbrtowc(&wc, p, n, NULL))) {
	    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
	    p += used; n -= used;
	}
	if (*p != '\0') return 0;
    } else
#endif
    {
	int c = 0xff & *p++;
	if (c != '.' && !isalpha(c) ) return 0;
	if (c == '.' && isdigit(0xff & (int)*p)) return 0;
	while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
	if (c != '\0') return 0;
    }

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++)
        if (strcmp(keywords[i].name, name) == 0) return 0;

    return 1;
}


static int SymbolValue(int c)
{
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
#if defined(SUPPORT_MBCS)
    if(mbcslocale) {
	wchar_t wc; int i, clen;
	clen = utf8locale ? utf8clen(c) : mbcs_get_next(c, &wc);
	while(1) {
	    /* at this point we have seen one char, so push its bytes 
	       and get one more */
	    for(i = 0; i < clen; i++) {
	        YYTEXT_PUSH(c, yyp);
	        c = xxgetc();
            }
	    if(c == R_EOF) break;
	    if(c == '.' || c == '_') {
		clen = 1;
		continue;
	    }
	    clen = mbcs_get_next(c, &wc);
	    if(!iswalnum(wc)) break;
	}
    } else
#endif
	do {
	    YYTEXT_PUSH(c, yyp);
	} while ((c = xxgetc()) != R_EOF && 
		 (isalnum(c) || c == '.' || c == '_'));
    xxungetc(c);
    YYTEXT_PUSH('\0', yyp);
    if ((kw = KeywordLookup(yytext))) {
	if ( kw == FUNCTION ) {
	    if (FunctionLevel >= MAXNEST)
		error(_("functions nested too deeply in source code at line %d"), xxlineno);
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

static int token(void)
{
    int c;
#if defined(SUPPORT_MBCS)
    wchar_t wc;
#endif

    if (SavedToken) {
	c = SavedToken;
	yylval = SavedLval;
	SavedLval = R_NilValue;
	SavedToken = 0;
	yylloc.first_line = xxlinesave;
	yylloc.first_column = xxcolsave;	
	return c;
    }
    xxcharsave = xxcharcount; /* want to be able to go back one token */

    c = SkipSpace();
    if (c == '#') c = SkipComment();
    
    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;    

    if (c == R_EOF) return END_OF_INPUT;

    /* Either digits or symbols can start with a "." */
    /* so we need to decide which it is and jump to  */
    /* the correct spot. */

    if (c == '.' && typeofnext() >= 2) goto symbol;

    /* literal numbers */

    if (c == '.') return NumericValue(c);
    /* We don't care about other than ASCII digits */
    if (isdigit(c)) return NumericValue(c);

    /* literal strings */

    if (c == '\"' || c == '\'')
	return StringValue(c, FALSE);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return StringValue(c, TRUE);
 symbol:

    if (c == '.') return SymbolValue(c);
#if defined(SUPPORT_MBCS)
    if(mbcslocale) {
	mbcs_get_next(c, &wc);
	if (iswalpha(wc)) return SymbolValue(c);
    } else
#endif
	if (isalpha(c)) return SymbolValue(c);
 
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
	    return AND2;
	}
	yylval = install("&");
	return AND;
    case '|':
	if (nextchar('|')) {
	    yylval = install("||");
	    return OR2;
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

static void setlastloc(void)
{
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
}

static int yylex(void)
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
		setlastloc();
		return tok;
	    }

	    /* When a "," is encountered, it terminates */
	    /* just the immediately preceding "if" body */
	    /* so we pop just a single "i" of the */
	    /* context stack. */

	    if (tok == ',') {
		ifpop();
		setlastloc();		
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
		setlastloc();		
		return ELSE;
	    }
	    else {
		ifpop();
		SavedToken = tok;
		xxlinesave = yylloc.first_line;
		xxcolsave  = yylloc.first_column;	
		SavedLval = yylval;
		setlastloc();		
		return '\n';
	    }
	}
	else {
	    setlastloc();
	    return '\n';
	}
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
    case OR2:
    case AND2:
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
	if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = '[';
	*++contextp = '[';
	break;

    case '[':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = tok;
	break;

    case LBRACE:
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error(_("contextstack overflow at line %d"), xxlineno);
	*++contextp = tok;
	EatLines = 1;
	break;

    case '(':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE) 
	    error(_("contextstack overflow at line %d"), xxlineno);
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
    setlastloc();
    return tok;
}
