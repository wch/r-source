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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <Defn.h>
#include "Parse.h"

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

#define YYERROR_VERBOSE 1

static void yyerror(char *);
static int yylex();
static int yyparse(void);

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

static SEXP	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);


/* Internal lexer / parser state variables */

static int 	xxinRString;
static int	xxgetc();
static int	xxungetc(int);
static int	xxlineno, xxcolno;
static int	xxlastlinelen;
static int	xxmode, xxitemType, xxbraceDepth;  /* context for lexer */
static int	xxDebugTokens;  /* non-zero causes debug output to R console */
static SEXP	Value;

#define RLIKE 1		/* Includes R strings; xxinRString holds the opening quote char, or 0 outside a string */
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4

static SEXP     SrcFile = NULL;

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void	xxsavevalue(SEXP, YYLTYPE *);

static int	mkMarkup(int);
static int      mkIfdef(int);
static int	mkCode(int);
static int	mkText(int);
static int	mkVerb(int);
static int 	mkComment(int);

#define YYSTYPE		SEXP

%}

%debug

%token		END_OF_INPUT ERROR
%token		SECTIONHEADER RSECTIONHEADER VSECTIONHEADER
%token		SECTIONHEADER2
%token		RCODEMACRO LATEXMACRO VERBMACRO OPTMACRO ESCAPE
%token		LISTSECTION ITEMIZE DESCRIPTION NOITEM
%token		RCODEMACRO2 LATEXMACRO2 VERBMACRO2
%token		IFDEF ENDIF
%token		TEXT RCODE VERB COMMENT UNKNOWN

%%

RdFile	:	SectionList END_OF_INPUT	{ xxsavevalue($1, &@$); return 0; }
	|	error	 			{ PROTECT(Value = R_NilValue);  YYABORT; }
	;

SectionList:	Section				{ $$ = xxnewlist($1); }
	|	SectionList Section		{ $$ = xxlist($1, $2); }
	
Section:	VSECTIONHEADER VerbatimArg	{ $$ = xxmarkup($1, $2, &@$); }	
	|	RSECTIONHEADER RLikeArg		{ $$ = xxmarkup($1, $2, &@$); }
	|	SECTIONHEADER  LatexArg  	{ $$ = xxmarkup($1, $2, &@$); }
	|	LISTSECTION    Item2Arg		{ $$ = xxmarkup($1, $2, &@$); }
	|	SECTIONHEADER2 LatexArg LatexArg { $$ = xxmarkup2($1, $2, $3, &@$); }
	|	IFDEF IfDefTarget SectionList ENDIF { $$ = xxmarkup2($1, $2, $3, &@$); UNPROTECT_PTR($4); } 
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	TEXT				{ $$ = xxtag($1, TEXT, &@$); } /* must be whitespace */

ArgItems:	Item				{ $$ = xxnewlist($1); }
	|	ArgItems Item			{ $$ = xxlist($1, $2); }
	
Item:		TEXT				{ $$ = xxtag($1, TEXT, &@$); }
	|	RCODE				{ $$ = xxtag($1, RCODE, &@$); }
	|	VERB				{ $$ = xxtag($1, VERB, &@$); }
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	Markup				{ $$ = $1; }
	
Markup:		LATEXMACRO  LatexArg 		{ $$ = xxmarkup($1, $2, &@$); }
	|	LATEXMACRO2 LatexArg LatexArg   { $$ = xxmarkup2($1, $2, $3, &@$); }
	|	ITEMIZE     Item0Arg		{ $$ = xxmarkup($1, $2, &@$); }
	|	DESCRIPTION Item2Arg		{ $$ = xxmarkup($1, $2, &@$); }
	|	OPTMACRO    goOption LatexArg  	{ $$ = xxmarkup($1, $3, &@$); xxpopMode($2); }
	|	OPTMACRO    goOption Option LatexArg { $$ = xxOptionmarkup($1, $3, $4, &@$); xxpopMode($2); }
	|	RCODEMACRO  RLikeArg     	{ $$ = xxmarkup($1, $2, &@$); }
	|	RCODEMACRO2 RLikeArg RLikeArg 	{ $$ = xxmarkup2($1, $2, $2, &@$); }
	|	VERBMACRO   VerbatimArg		{ $$ = xxmarkup($1, $2, &@$); }
	|	VERBMACRO2  VerbatimArg		{ $$ = xxmarkup($1, $2, &@$); }
	|       VERBMACRO2  VerbatimArg VerbatimArg2 { $$ = xxmarkup2($1, $2, $3, &@$); }
	|	ESCAPE				{ $$ = xxmarkup($1, R_NilValue, &@$); }
	|	IFDEF IfDefTarget ArgItems ENDIF { $$ = xxmarkup2($1, $2, $3, &@$); UNPROTECT_PTR($4); } 
	
LatexArg:	goLatexLike '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

Item0Arg:	goItem0 '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

Item2Arg:	goItem2 '{' ArgItems  '}'	{ xxpopMode($1); $$ = $3; }

RLikeArg:	goRLike '{' ArgItems  '}'	{ xxpopMode($1); $$ = $3; }

VerbatimArg:	goVerbatim '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

/* This one executes the push after seeing the brace starting the optional second arg */

VerbatimArg2:   '{' goVerbatim2 ArgItems '}'    { xxpopMode($2); $$ = $3; }

IfDefTarget:	goLatexLike TEXT	{ xxpopMode($1); $$ = xxnewlist($2); }


goLatexLike:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, UNKNOWN); }

goRLike:	/* empty */			{ $$ = xxpushMode(RLIKE, UNKNOWN); }

goOption:	/* empty */			{ $$ = xxpushMode(INOPTION, UNKNOWN); }

goVerbatim:	/* empty */			{ $$ = xxpushMode(VERBATIM, UNKNOWN); }

goVerbatim2:    /* empty */			{ xxbraceDepth--; $$ = xxpushMode(VERBATIM, UNKNOWN); xxbraceDepth++; }

goItem0:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, ESCAPE); }

goItem2:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, LATEXMACRO2); }

Option:		'[' Item ']'			{ $$ = $2; }	
		
%%

static SEXP xxpushMode(int newmode, int newitem)
{
    SEXP ans;
    PROTECT(ans = allocVector(INTSXP, 3));
    
    INTEGER(ans)[0] = xxmode;		/* Lexer mode */
    INTEGER(ans)[1] = xxitemType;	/* What is \item? */
    INTEGER(ans)[2] = xxbraceDepth;	/* Brace depth used in RCODE and VERBATIM */
    INTEGER(ans)[3] = xxinRString;      /* Quote char that started a string */
    
#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = newmode;
    xxitemType = newitem;
    xxbraceDepth = 0;
    xxinRString = 0;
    
    return ans;
}

static void xxpopMode(SEXP oldmode) 
{
#if DEBUGVALS
    Rprintf("xxpopMode(%d, %s, %d) replaces %d, %s, %d\n", INTEGER(oldmode)[0], yytname[YYTRANSLATE(INTEGER(oldmode)[1])], INTEGER(oldmode)[2], 
    					xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = INTEGER(oldmode)[0];
    xxitemType = INTEGER(oldmode)[1]; 
    xxbraceDepth = INTEGER(oldmode)[2];
    xxinRString = INTEGER(oldmode)[3];
    
    UNPROTECT_PTR(oldmode);
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans, tmp;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PROTECT(tmp = NewList());
    PROTECT(ans = GrowList(tmp, item));
    UNPROTECT_PTR(tmp);
    UNPROTECT_PTR(item);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxlist(SEXP oldlist, SEXP item)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxlist(oldlist=%p, item=%p)", oldlist, item);
#endif
    PROTECT(ans = GrowList(oldlist, item));
    UNPROTECT_PTR(item);
    UNPROTECT_PTR(oldlist);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxmarkup(SEXP header, SEXP body, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup(header=%p, body=%p)", header, body);    
#endif
    if (isNull(body)) 
        PROTECT(ans = allocVector(VECSXP, 0));
    else {
	PROTECT(ans = PairToVectorList(CDR(body)));
    	UNPROTECT_PTR(body);	
    }
    setAttrib(ans, install("Rd_tag"), header);
    if (SrcFile) 
    	setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    UNPROTECT_PTR(header);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxOptionmarkup(SEXP header, SEXP option, SEXP body, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxOptionmarkup(header=%p, option=%p, body=%p)", header, option, body);    
#endif
    PROTECT(ans = PairToVectorList(CDR(body)));
    UNPROTECT_PTR(body);	
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);
    setAttrib(ans, install("Rd_option"), option);
    UNPROTECT_PTR(option);
    if (SrcFile) 
    	setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup2(SEXP header, SEXP body1, SEXP body2, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p)", header, body1, body2);        
#endif
    PROTECT(ans = allocVector(VECSXP, 2));
    if (!isNull(body1)) {
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    }
    SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    UNPROTECT_PTR(body2);    
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
    if (SrcFile) 
    	setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static void xxsavevalue(SEXP Rd, YYLTYPE *lloc)
{
    PROTECT(Value = PairToVectorList(CDR(Rd)));
    if (!isNull(Value)) {
    	setAttrib(Value, R_ClassSymbol, mkString("Rd"));
    	if (SrcFile) 
    	    setAttrib(Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    }
    UNPROTECT_PTR(Rd);
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    setAttrib(item, install("Rd_tag"), mkString(yytname[YYTRANSLATE(type)]));
    if (SrcFile) 
    	setAttrib(item, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    return item;
}
  
/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth and one almost #ifndef match */

static int pushback[20];
static unsigned int npush = 0;

static int xxgetc(void)
{
    int c;

    if(npush) c = pushback[--npush]; else  c = ptr_getc();
    if (c == EOF) return R_EOF;
    
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = c;
    
    if (c == '\n') {
    	xxlineno += 1;
    	xxlastlinelen = xxcolno; 
    	xxcolno = 0;
    } else xxcolno++;
    
    return c;
}

static int xxungetc(int c)
{
    if (c == '\n') {
    	xxlineno -= 1;
    	xxcolno = xxlastlinelen; /* FIXME:  could we push back more than one line? */
    	xxlastlinelen = 0;
    } else xxcolno--;
    
    R_ParseContext[R_ParseContextLast] = '\0';
    /* Mac OS X requires us to keep this non-negative */
    R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
	% PARSE_CONTEXT_SIZE;
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

static SEXP mkString2(const char *s, int len)
{
    SEXP t;
    cetype_t enc = CE_NATIVE;

    if(known_to_be_latin1) enc= CE_LATIN1;
    else if(known_to_be_utf8) enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
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

/*--------------------------------------------------------------------------*/

/*
 *  Parsing Entry Points:
 *
 *  The Following entry points provide Rd parsing facilities.
 *
 *	SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile)
 *
 */
 
static SEXP ParseRd(ParseStatus *status, SEXP srcfile)
{
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
    
    xxlineno = 1;
    xxcolno = 0;    
    
    if (!isNull(srcfile)) SrcFile = srcfile;
    else SrcFile = NULL;
    
    npush = 0;
    xxmode = LATEXLIKE; 
    xxitemType = UNKNOWN;
    xxbraceDepth = 0;
    xxinRString = 0;
    
    Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", Value);    
#endif    
    UNPROTECT_PTR(Value);
    return Value;
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
SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile)
{
    con_parse = con;
    ptr_getc = con_getc;
    return ParseRd(status, srcfile);
}

/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  
 *
 */


/* Special Symbols */
/* Section and R code headers */

struct {
    char *name;
    int token;
}
static keywords[] = {
    /* These sections contain Latex-like text */
    
    { "\\author",  SECTIONHEADER },
    { "\\concept", SECTIONHEADER },
    { "\\description",SECTIONHEADER },
    { "\\details", SECTIONHEADER },
    { "\\docType", SECTIONHEADER },
    
    { "\\encoding",SECTIONHEADER },
    { "\\format",  SECTIONHEADER },
    { "\\keyword", SECTIONHEADER },
    { "\\name",    SECTIONHEADER },
    { "\\note",    SECTIONHEADER },
    
    { "\\references", SECTIONHEADER },
    { "\\section", SECTIONHEADER2 },    
    { "\\seealso", SECTIONHEADER },
    { "\\source",  SECTIONHEADER },
    { "\\title",   SECTIONHEADER },

    /* These sections contain R-like text */
    
    { "\\examples",RSECTIONHEADER },
    { "\\usage",   RSECTIONHEADER },
    
    /* This section contains verbatim text */
    
    { "\\alias",   VSECTIONHEADER }, 
    { "\\synopsis",VSECTIONHEADER }, 
    { "\\Rdversion",VSECTIONHEADER }, 
    
    /* These macros take no arguments.  One character non-alpha escapes get the
       same token value */

    { "\\cr",      ESCAPE },
    { "\\dots",    ESCAPE },
    { "\\ldots",   ESCAPE },
    { "\\R",       ESCAPE },    
    { "\\tab",     ESCAPE },
    
    /* These macros take one LaTeX-like argument. */
    
    { "\\acronym", LATEXMACRO },
    { "\\bold",    LATEXMACRO },
    { "\\cite",    LATEXMACRO },
    { "\\dfn",     LATEXMACRO },
    { "\\dQuote",  LATEXMACRO },
    { "\\email",   LATEXMACRO },
    
    { "\\emph",    LATEXMACRO },    
    { "\\file",    LATEXMACRO },
    { "\\linkS4class", LATEXMACRO },
    { "\\pkg",	   LATEXMACRO },
    { "\\sQuote",  LATEXMACRO },
    
    { "\\strong",  LATEXMACRO },
    
    { "\\var",     LATEXMACRO },
    
    /* These are like SECTIONHEADER/LATEXMACRO, but they change the interpretation of \item */

    { "\\arguments",LISTSECTION },
    { "\\value",   LISTSECTION },
    
    { "\\describe",DESCRIPTION },
    { "\\enumerate",ITEMIZE },
    { "\\itemize", ITEMIZE },

    { "\\item",    NOITEM }, /* will change to UNKNOWN, ESCAPE, or LATEXMACRO2 depending on context */
    
    /* These macros take two LaTeX-like arguments. */
    
    { "\\enc",     LATEXMACRO2 },
    { "\\method",  LATEXMACRO2 },
    { "\\S3method",LATEXMACRO2 },
    { "\\S4method",LATEXMACRO2 },
    { "\\tabular", LATEXMACRO2 },
    
    /* These macros take one optional bracketed option and always take 
       one LaTeX-like argument */
       
    { "\\link",    OPTMACRO },       
       
    /* These markup macros require an R-like text argument */
    
    { "\\code",    RCODEMACRO },
    { "\\dontrun", RCODEMACRO },
    { "\\dontshow",RCODEMACRO },
    { "\\donttest",RCODEMACRO },
    { "\\testonly",RCODEMACRO },
    
    /* These macros take one verbatim arg and ignore everything except braces */
    
    { "\\command", VERBMACRO },
    { "\\env",     VERBMACRO },
    { "\\kbd", 	   VERBMACRO },	
    { "\\option",  VERBMACRO },
    { "\\preformatted", VERBMACRO },
    
    { "\\samp",    VERBMACRO },
    { "\\special", VERBMACRO },
    { "\\url",     VERBMACRO },
    
    /* These ones take one or two verbatim args */
    
    { "\\eqn",     VERBMACRO2 },
    { "\\deqn",    VERBMACRO2 },
    
    /* We parse IFDEF/IFNDEF as markup, not as a separate preprocessor step */
    
    { "#ifdef",    IFDEF },
    { "#ifndef",   IFDEF },
    { "#endif",    ENDIF },
    
    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7   

static int KeywordLookup(const char *s)
{
    int i;
    for (i = 0; keywords[i].name; i++) {
	if (strcmp(keywords[i].name, s) == 0) {
	    return keywords[i].token;
	}
    }
    return UNKNOWN;
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
#define YYENGLISH 7
	"$undefined",	"input", 	
	"SECTIONHEADER","text section header",
	"RSECTIONHEADER","code section header", 	
	"RCODEMACRO",	"code macro",
	"LATEXMACRO",	"text macro",
	"TEXT", 	"text",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("text section header");
    _("code section header");
    _("code macro");
    _("text macro");	
    _("code");
    _("text");
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

#define TEXT_PUSH(c) do {                  \
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

static void setfirstloc(void)
{
    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;
}

static void setlastloc(void)
{
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c;
    int outsideLiteral = xxmode == LATEXLIKE || xxmode == INOPTION || xxbraceDepth == 0;
    	
    setfirstloc();
    c = xxgetc();
    
    /* % comments are active everywhere */
    
    if ( c == '%') return mkComment(c);    

    if (c == R_EOF) return END_OF_INPUT;

    if (c == '\\') {
    	int lookahead = xxgetc();
    	xxungetc(lookahead);    	
    	if (xxmode == VERBATIM) {
    	    if (lookahead == LBRACE || lookahead == RBRACE)
    	    	return mkVerb(c);
    	} else {
    	    if (xxinRString && lookahead != 'l') 
		return mkCode(c);
	    
	    return mkMarkup(c);
	}
    }
    
    if (xxinRString) return mkCode(c);
 
    if (c == '#' && xxcolno == 1) return mkIfdef(c);
    
    if (c == LBRACE) {
    	xxbraceDepth++;
    	if (outsideLiteral) return c;
    }
    
    if (c == RBRACE) {
    	xxbraceDepth--;
    	if (outsideLiteral || xxbraceDepth == 0) return c;
    }
    
    if ( (c == '[' || c == ']') && xxmode == INOPTION ) return c; 
	
    switch (xxmode) {
	case RLIKE:     return mkCode(c);
	case INOPTION:
	case LATEXLIKE: return mkText(c);
	case VERBATIM:  return mkVerb(c);
    }
 
    return ERROR; /* We shouldn't get here. */
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0, lookahead;
    
    while(1) {
    	switch (c) {
    	case '\\': 
    	    lookahead = xxgetc();
    	    if (lookahead == LBRACE || lookahead == RBRACE || lookahead == '%') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    goto stop;
    	case ']':
    	    if (xxmode == INOPTION) goto stop;
            break;
    	case '%':
    	case LBRACE:
    	case RBRACE:
    	case R_EOF:
    	    goto stop;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') goto stop;
    	c = xxgetc();
    };
stop:
    if (c != '\n') xxungetc(c); /* newline causes a break, but we keep it */
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if (xxDebugTokens)
	Rprintf("mkText: %s\n", CHAR(STRING_ELT(yylval, 0))); 
    if(stext != st0) free(stext);
    return TEXT;
}

static int mkComment(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    do TEXT_PUSH(c);
    while ((c = xxgetc()) != '\n' && c != R_EOF);
    if (c == R_EOF) xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if (xxDebugTokens)
	Rprintf("mkComment: %s\n", CHAR(STRING_ELT(yylval, 0))); 
    if(stext != st0) free(stext);    
    return COMMENT;
}

static int mkCode(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE) xxbraceDepth--;
    if (c == RBRACE) xxbraceDepth++; 
    
    while(1) {
	int escaped = 0;
    	if (c == '\\') {
    	    int lookahead = xxgetc();
    	    if (lookahead == '\\' || lookahead == '%') {
    	         c = lookahead;
    	         escaped = 1;
    	    } else xxungetc(lookahead);
    	}
    	if ((!escaped && c == '%') || c == R_EOF) break;
    	if (xxinRString) {
    	    /* This stuff is messy, because there are two levels of escaping:
    	       The Rd escaping and the R code string escaping. */
    	    if (c == '\\') {
    		int lookahead = xxgetc();
    		if (lookahead == '\\') { /* This must be the 3rd backslash */
    		    lookahead = xxgetc();
    		    if (lookahead == xxinRString || lookahead == '\\') {	
    	    	    	TEXT_PUSH(c);
    	    	    	c = lookahead;
    	    	    	escaped = 1;
    	    	    } else xxungetc(lookahead);
    	    	} else if (lookahead == xxinRString) { /* There could be one or two before this */
    	    	    TEXT_PUSH(c);
    	    	    c = lookahead;
    	    	    escaped = 1;
    	    	} else if (!escaped && lookahead == 'l') { /* assume \link */
    	    	    xxungetc(lookahead);
    	    	    break;
    	    	} else xxungetc(lookahead);
    	    }
    	    if (!escaped && c == xxinRString)
    	    	xxinRString = 0;
    	} else {
    	    if (c == '#') {
    	    	do {
    	    	    TEXT_PUSH(c);
    	    	    c = xxgetc();
    	    	    if (c == LBRACE) xxbraceDepth++;
    	    	    else if (c == RBRACE) xxbraceDepth--;
    	    	} while (c != '\n' && c != R_EOF && xxbraceDepth > 0);
    	    	if (c == RBRACE) xxbraceDepth++; /* avoid double counting */
    	    }
    	    if (c == '\'' || c == '"' || c == '`') xxinRString = c;
    	    else if (c == '\\' && !escaped) {
    	    	int lookahead = xxgetc();
    	    	if (lookahead == LBRACE || lookahead == RBRACE) {
		    c = lookahead;
		} else if (isalpha(lookahead)) {
    	    	    xxungetc(lookahead);
    	    	    c = '\\';
    	    	    break;
    	    	} else {
    	    	    TEXT_PUSH('\\');
    	    	    c = lookahead;
    	    	}
    	    } else if (c == LBRACE) {
    	    	xxbraceDepth++;
    	    } else if (c == RBRACE) {
    	    	if (xxbraceDepth == 1) break;
    	    	else xxbraceDepth--;
    	    } else if (c == R_EOF) break;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    }
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if (xxDebugTokens) {
    	Rprintf("mkCode:  %s\n", CHAR(STRING_ELT(yylval, 0)));
    }
    if(stext != st0) free(stext);
    return RCODE; 
}

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval;
    
    TEXT_PUSH(c);
    while (isalnum((c = xxgetc()))) TEXT_PUSH(c);
    
    /* character escapes are processed as text, not markup */
    if (bp == stext+1) {
        TEXT_PUSH(c);
        TEXT_PUSH('\0');
        retval = TEXT;
        c = xxgetc();
    } else {
        TEXT_PUSH('\0');
        retval = KeywordLookup(stext);
        if (retval == NOITEM) 
            retval = xxitemType;
        else if (retval == ESCAPE && c == LBRACE) { /* include following {} for escapes */
            int lookahead = xxgetc();
            if (lookahead == RBRACE) {
            	bp--;
            	TEXT_PUSH(LBRACE);
            	TEXT_PUSH(RBRACE);
            	TEXT_PUSH('\0');
            	c = xxgetc();
            } else
            	xxungetc(lookahead);
        }
    }
    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
    if (xxDebugTokens)
    	Rprintf("mkMarkup:  %s\n", CHAR(STRING_ELT(yylval, 0)));
    if(stext != st0) free(stext);
    xxungetc(c);
    return retval;
}

static int mkIfdef(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval;
    
    TEXT_PUSH(c);
    while (isalpha((c = xxgetc())) && bp - stext <= DIRECTIVE_LEN) TEXT_PUSH(c);
    TEXT_PUSH('\0');
    xxungetc(c);
    retval = KeywordLookup(stext);
    PROTECT(yylval = mkString2(stext, bp - stext - 1));
    
    if (retval == UNKNOWN) {
    	UNPROTECT(1);
    	bp--; bp--;
    	for (; bp > stext; bp--) 
    	    xxungetc(*bp);
    	switch (xxmode) {
    	case RLIKE:     
    	    retval = mkCode(*bp);
    	    break;
    	case INOPTION:
    	case LATEXLIKE:
    	    retval = mkText(*bp);
    	    break;
    	case VERBATIM:
    	    retval = mkVerb(*bp);
    	    break;
	}
    }
    if (xxDebugTokens)
    	Rprintf("mkIfdef:  %s\n", CHAR(STRING_ELT(yylval, 0)));
    if(stext != st0) free(stext);
    return retval;
}

static int mkVerb(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE) xxbraceDepth--;
    if (c == RBRACE) xxbraceDepth++;     
    
    while(1) {
    	int escaped = 0;
        if (c == '\\') {
            int lookahead = xxgetc();
            if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		c = lookahead;
		escaped = 1;
	    } else xxungetc(lookahead);
        }
    	if ((!escaped && c == '%') || c == R_EOF) break;
	if (!escaped && c == LBRACE) 
	    xxbraceDepth++;
    	else if (!escaped && c == RBRACE) {
	    if (xxbraceDepth == 1) break;
	    else xxbraceDepth--;
	} else if ((!escaped && c == '%') || c == R_EOF) 
	    break;
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    };
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if (xxDebugTokens)
    	Rprintf("mkverb:  %s\n", CHAR(STRING_ELT(yylval, 0)));
    if(stext != st0) free(stext);
    return VERB;  
}

static int yylex(void)
{
    int tok = token();
    
    if (xxDebugTokens) {  
    	Rprintf("%d:%d: %s", xxlineno, xxcolno, yytname[YYTRANSLATE(tok)]);
    	if (xxinRString) Rprintf("(in %c%c)", xxinRString, xxinRString);
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

/* "do_parseRd" 

 .Internal( parseRd(file, srcfile, encoding, verbose) )
 If there is text then that is read and the other arguments are ignored.
*/

SEXP attribute_hidden do_parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s = R_NilValue, source;
    Rconnection con;
    Rboolean wasopen, old_latin1=known_to_be_latin1,
	old_utf8=known_to_be_utf8;
    int ifile;
    const char *encoding;
    ParseStatus status;

#if DEBUGMODE
    yydebug = 1;
#endif 

    checkArity(op, args);
    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    source = CAR(args);					args = CDR(args);
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"), "encoding");
    encoding = CHAR(STRING_ELT(CAR(args), 0)); /* ASCII */ args = CDR(args);
    known_to_be_latin1 = known_to_be_utf8 = FALSE;
    if(streql(encoding, "latin1")) known_to_be_latin1 = TRUE;
    if(streql(encoding, "UTF-8"))  known_to_be_utf8 = TRUE;
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    xxDebugTokens = asInteger(CAR(args));

    if (ifile >= 3) {/* file != "" */
	if(!wasopen) {
	    if(!con->open(con)) error(_("cannot open the connection"));
	    if(!con->canread) {
		con->close(con);
		error(_("cannot read from this connection"));
	    }
	} else if(!con->canread)
	    error(_("cannot read from this connection"));
	s = R_ParseRd(con, &status, source);
	if(!wasopen) con->close(con);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else error(_("invalid Rd file"));
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
    return s;
}
