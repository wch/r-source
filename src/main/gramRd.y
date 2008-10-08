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
#define DEBUGTOKENS 0		/* 1 causes lexer output to R console */
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
static SEXP	Value;

#define RLIKE 1
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4

static SEXP     SrcFile = NULL;
static SEXP	SrcRefs = NULL;

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
	if(res == -1) error(_("invalid multibyte character in parser at line %d"), xxlineno);
    } else {
	/* This is not necessarily correct for stateful MBCS */
	while(clen <= MB_CUR_MAX) {
	    mbs_init(&mb_st);
	    res = mbrtowc(wc, s, clen, &mb_st);
	    if(res >= 0) break;
	    if(res == -1) 
		error(_("invalid multibyte character in parser at line %d"), xxlineno);
	    /* so res == -2 */
	    c = xxgetc();
	    if(c == R_EOF) error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
	    s[clen++] = c;
	} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--) xxungetc(s[i]);
    return clen;
}

#endif

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, YYLTYPE *);
static void	xxsavevalue(SEXP);

static int	mkMarkup(int);
static int      mkIfdef(int);
static int	mkCode(int);
static int	mkText(int);
static int	mkVerb(int);

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
%token		TEXT RCODE VERB UNKNOWN

%%

RdFile	:	SectionList END_OF_INPUT	{ xxsavevalue($1); return 0; }
	|	error	 			{ PROTECT(Value = R_NilValue);  YYABORT; }
	;

SectionList:	Section				{ $$ = xxnewlist($1); }
	|	SectionList Section		{ $$ = xxlist($1, $2); }
	
Section:	VSECTIONHEADER VerbatimArg	{ $$ = xxmarkup($1, $2, &@1); }	
	|	RSECTIONHEADER RLikeArg		{ $$ = xxmarkup($1, $2, &@1); }
	|	SECTIONHEADER  LatexArg  	{ $$ = xxmarkup($1, $2, &@1); }
	|	LISTSECTION    Item2Arg		{ $$ = xxmarkup($1, $2, &@1); }
	|	SECTIONHEADER2 LatexArg LatexArg { $$ = xxmarkup2($1, $2, $3, &@1); }
	|	IFDEF IfDefTarget SectionList ENDIF { $$ = xxmarkup2($1, $2, $3, &@1); UNPROTECT_PTR($4); } 

ArgItems:	Item				{ $$ = xxnewlist($1); }
	|	ArgItems Item			{ $$ = xxlist($1, $2); }
	
Item:		TEXT
	|	RCODE
	|	VERB
	|	Markup				{ $$ = $1; }
	
Markup:		LATEXMACRO  LatexArg 		{ $$ = xxmarkup($1, $2, &@1); }
	|	LATEXMACRO2 LatexArg LatexArg   { $$ = xxmarkup2($1, $2, $3, &@1); }
	|	ITEMIZE     Item0Arg		{ $$ = xxmarkup($1, $2, &@1); }
	|	DESCRIPTION Item2Arg		{ $$ = xxmarkup($1, $2, &@1); }
	|	OPTMACRO    goOption LatexArg  	{ $$ = xxmarkup($1, $3, &@1); xxpopMode($2); }
	|	OPTMACRO    goOption Option LatexArg { $$ = xxOptionmarkup($1, $3, $4, &@1); xxpopMode($2); }
	|	RCODEMACRO  RLikeArg     	{ $$ = xxmarkup($1, $2, &@1); }
	|	RCODEMACRO2 RLikeArg RLikeArg 	{ $$ = xxmarkup2($1, $2, $2, &@1); }
	|	VERBMACRO   VerbatimArg		{ $$ = xxmarkup($1, $2, &@1); }
	|	VERBMACRO2  VerbatimArg		{ $$ = xxmarkup($1, $2, &@1); }
	|       VERBMACRO2  VerbatimArg VerbatimArg2 { $$ = xxmarkup2($1, $2, $3, &@1); }
	|	ESCAPE				{ $$ = xxmarkup($1, R_NilValue, &@1); }
	|	IFDEF IfDefTarget ArgItems ENDIF { $$ = xxmarkup2($1, $2, $3, &@1); UNPROTECT_PTR($4); } 
	
LatexArg:	goLatexLike '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

Item0Arg:	goItem0 '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

Item2Arg:	goItem2 '{' ArgItems  '}'	{ xxpopMode($1); $$ = $3; }

RLikeArg:	goRLike '{' ArgItems  '}'	{ xxpopMode($1); $$ = $3; }

VerbatimArg:	goVerbatim '{' ArgItems  '}' 	{ xxpopMode($1); $$ = $3; }

/* This one executes the push after seeing the brace starting the optional second arg */

VerbatimArg2:   '{' goVerbatim2 ArgItems '}'    { xxpopMode($2); $$ = $3; }

IfDefTarget:	goLatexLike TEXT 		{ xxpopMode($1); $$ = xxnewlist($2);  }


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
    
#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = newmode;
    xxitemType = newitem;
    xxbraceDepth = 0;
    
    return ans;
}

static void xxpopMode(SEXP oldmode) 
{
#if DEBUGMODE
    Rprintf("xxpopMode(%d, %s, %d) replaces %d, %s, %d\n", INTEGER(oldmode)[0], yytname[YYTRANSLATE(INTEGER(oldmode)[1])], INTEGER(oldmode)[2], 
    					xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = INTEGER(oldmode)[0];
    xxitemType = INTEGER(oldmode)[1]; 
    xxbraceDepth = INTEGER(oldmode)[2];
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
    setAttrib(ans, install("header"), header);
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
    setAttrib(ans, install("header"), header);
    UNPROTECT_PTR(header);
    setAttrib(ans, install("option"), option);
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
    setAttrib(ans, install("header"), header);
    UNPROTECT_PTR(header);    
    if (SrcFile) 
    	setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static void xxsavevalue(SEXP Rd)
{
    PROTECT(Value = PairToVectorList(CDR(Rd)));
    if (!isNull(Value))
    	setAttrib(Value, R_ClassSymbol, mkString("RdFile"));
    UNPROTECT_PTR(Rd);
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
    UNPROTECT_PTR(SrcRefs);
    SrcRefs = NULL;
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
    
    if (!isNull(srcfile)) {
	SrcFile = srcfile;
	PROTECT(SrcRefs = NewList());
    } 
    else SrcFile = NULL;
    
    npush = 0;
    xxmode = LATEXLIKE; 
    xxitemType = UNKNOWN;
    xxbraceDepth = 0;
    xxinRString = 0;
    
    Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

    if (SrcFile && !isNull(Value)) {
    	Value = attachSrcrefs(Value, SrcFile);
        SrcFile = NULL;    
    }
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
    
    { "\\name",    SECTIONHEADER },
    { "\\docType", SECTIONHEADER },
    { "\\title",   SECTIONHEADER },
    { "\\description",SECTIONHEADER },
    { "\\concept", SECTIONHEADER },
    { "\\details", SECTIONHEADER },
    { "\\format",  SECTIONHEADER },
    { "\\references", SECTIONHEADER },
    { "\\source",  SECTIONHEADER },
    { "\\note",    SECTIONHEADER },
    { "\\author",  SECTIONHEADER },
    { "\\seealso", SECTIONHEADER },
    { "\\keyword", SECTIONHEADER },
    { "\\encoding",SECTIONHEADER },
    { "\\section", SECTIONHEADER2 },    

    /* These sections contain R-like text */
    
    { "\\usage",   RSECTIONHEADER },
    { "\\examples",RSECTIONHEADER },
    
    /* These sections contain verbatim text */
    
    { "\\alias",   VSECTIONHEADER },    
    
    /* These macros take no arguments.  One character non-alpha escapes get the
       same token value */
    
    { "\\dots",    ESCAPE },
    { "\\ldots",   ESCAPE },
    { "\\cr",      ESCAPE },
    { "\\R",       ESCAPE },
    { "\\tab",     ESCAPE },
    
    /* These macros take one LaTeX-like argument. */
    
    { "\\bold",    LATEXMACRO },
    { "\\emph",    LATEXMACRO },    
    { "\\dQuote",  LATEXMACRO },
    { "\\sQuote",  LATEXMACRO },
    { "\\strong",  LATEXMACRO },
    { "\\email",   LATEXMACRO },
    { "\\file",    LATEXMACRO },
    { "\\pkg",	   LATEXMACRO },
    { "\\var",     LATEXMACRO },
    { "\\linkS4class", LATEXMACRO },
    
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
    
    { "\\env",     VERBMACRO },
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

static int SkipComment(void)
{
    int c;
    while ((c = xxgetc()) != '\n' && c != R_EOF) ;
    return c;
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

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c;
    int outsideLiteral;

    /* outside of literals (RLIKE and VERBATIM) skip white space; inside, just skip empty lines */
    /* in both places, skip % comments */
    do {
    	if( (outsideLiteral = (xxmode == LATEXLIKE || xxmode == INOPTION || xxbraceDepth == 0)) ) 
	    c = SkipSpace();
    	else 
    	    c = xxgetc();
   	if ( c == '%') c = SkipComment();
    } while (c == '\n');
    
    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;    

    if (c == R_EOF) return END_OF_INPUT;

    if (xxinRString) return mkCode(c);
 
    if (c == '\\') {
    	if (xxmode == VERBATIM) {
    	    int lookahead = xxgetc();
    	    xxungetc(lookahead);
    	    if (lookahead == LBRACE || lookahead == RBRACE)
    	    	return mkVerb(c);
    	}
    	else
    	    return mkMarkup(c);
    }
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
    	if (c == '%') c = SkipComment();
    	switch (c) {
    	case '\\': 
    	    lookahead = xxgetc();
    	    if (lookahead == LBRACE || lookahead == RBRACE || lookahead == '%') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    /* fall through to other cases ... */
    	case LBRACE:
    	case RBRACE:
    	case '\n':
    	case R_EOF:
    	case ']':
    	    if (c != ']' || xxmode == INOPTION) { /* ']' only breaks in INOPTION mode */
    	    	xxungetc(c);
    	    	PROTECT(yylval = mkString2(stext,  bp - stext));
#if DEBUGTOKENS
            	Rprintf("mktext: %s\n", CHAR(STRING_ELT(yylval, 0)));    	    
#endif
    	    	if(stext != st0) free(stext);
    	    	return TEXT;
    	    }
    	}
    	TEXT_PUSH(c);
    	c = xxgetc();
    };
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
    	if (xxinRString) {
    	    if (c == xxinRString) xxinRString = 0;
    	    if (c == '\\') {
    	    	TEXT_PUSH(c);
    	    	c = xxgetc();
    	    }
    	    if (c == '\n' || c == R_EOF) break;
    	} else {
    	    if (c == '%') c = SkipComment();
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
    	    else if (c == '\\') {
    	    	int lookahead = xxgetc();
    	    	if (lookahead == LBRACE || lookahead == RBRACE || lookahead == '%') {
		    c = lookahead;
		} else if (isalpha(c)) {
    	    	    xxungetc(c);
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
    	    } else if (c == '\n' || c == R_EOF) break;
    	}
    	TEXT_PUSH(c);
    	c = xxgetc();
    }
    xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
#if DEBUGTOKENS
    Rprintf("mkCode:  %s\n", CHAR(STRING_ELT(yylval, 0)));
#endif
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
    }
    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
#if DEBUGTOKENS
    Rprintf("mkMarkup:  %s\n", CHAR(STRING_ELT(yylval, 0)));
#endif
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
#if DEBUGTOKENS
    Rprintf("mkIfdef:  %s\n", CHAR(STRING_ELT(yylval, 0)));
#endif
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
	if (c == LBRACE) 
	    xxbraceDepth++;
    	else if (c == RBRACE) {
	    if (xxbraceDepth == 1) break;
	    else xxbraceDepth--;
	} else if (c == '\\') {
	    int lookahead = xxgetc();
	    if (lookahead == RBRACE || lookahead == LBRACE)
	    	c = lookahead;
	    else
	    	xxungetc(lookahead);
	} else if (c == R_EOF || c == '\n') 
	    break;
    	TEXT_PUSH(c);
    	c = xxgetc();
    };
    xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
#if DEBUGTOKENS
    Rprintf("mkverb:  %s\n", CHAR(STRING_ELT(yylval, 0)));  	    
#endif
    if(stext != st0) free(stext);
    return VERB;  
}

static void setlastloc(void)
{
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
}

static int yylex(void)
{
    int tok = token();
    
#if DEBUGTOKENS    
    Rprintf("%d:%d: %s\n", xxlineno, xxcolno, yytname[YYTRANSLATE(tok)]);
#endif

    setlastloc();
    return tok;
}

/* "do_parseRd" 

 .Internal( parseRd(file, srcfile, encoding) )
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

#if DEBUGTOKENS    
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
    encoding = CHAR(STRING_ELT(CAR(args), 0)); /* ASCII */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;
    if(streql(encoding, "latin1")) known_to_be_latin1 = TRUE;
    if(streql(encoding, "UTF-8"))  known_to_be_utf8 = TRUE;

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
