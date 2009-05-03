
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
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
} yyltype;

# define YYLTYPE yyltype
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))							\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	  (Current).first_byte   = (Current).last_byte =		\
	    YYRHSLOC (Rhs, 0).last_byte;				\
	}								\
    while (YYID (0))

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static SEXP	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);


/* Internal lexer / parser state variables */

static int 	xxinRString, xxQuoteLine, xxQuoteCol;
static int	xxinEqn;
static int	xxNewlineInString;
static int	xxgetc();
static int	xxungetc(int);
static int	xxlineno, xxbyteno, xxcolno;
static int	xxmode, xxitemType, xxbraceDepth;  /* context for lexer */
static int	xxDebugTokens;  /* non-zero causes debug output to R console */
static const char* xxBasename;     /* basename of file for error messages */
static SEXP	Value;

#define RLIKE 1		/* Includes R strings; xxinRString holds the opening quote char, or 0 outside a string */
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4

static SEXP     SrcFile;  /* parse_Rd will *always* supply a srcfile */

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void	xxsavevalue(SEXP, YYLTYPE *);
static void	xxWarnNewline();

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
%token		LATEXMACRO2 VERBMACRO2
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
	|	SECTIONHEADER2 LatexArg LatexArg2 { $$ = xxmarkup2($1, $2, $3, 2, &@$); }
	|	IFDEF IfDefTarget SectionList ENDIF { $$ = xxmarkup2($1, $2, $3, 2, &@$); UNPROTECT_PTR($4); } 
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	TEXT				{ $$ = xxtag($1, TEXT, &@$); } /* must be whitespace */

ArgItems:	Item				{ $$ = xxnewlist($1); }
	|	ArgItems Item			{ $$ = xxlist($1, $2); }
	
Item:		TEXT				{ $$ = xxtag($1, TEXT, &@$); }
	|	RCODE				{ $$ = xxtag($1, RCODE, &@$); }
	|	VERB				{ $$ = xxtag($1, VERB, &@$); }
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	UNKNOWN				{ $$ = xxtag($1, UNKNOWN, &@$); }
	|	Arg				{ $$ = xxmarkup(R_NilValue, $1, &@$); }
	|	Markup				{ $$ = $1; }	

Markup:		LATEXMACRO  LatexArg 		{ $$ = xxmarkup($1, $2, &@$); }
	|	LATEXMACRO2 LatexArg LatexArg2  { $$ = xxmarkup2($1, $2, $3, 2, &@$); }
	|	ITEMIZE     Item0Arg		{ $$ = xxmarkup($1, $2, &@$); }
	|	DESCRIPTION Item2Arg		{ $$ = xxmarkup($1, $2, &@$); }
	|	OPTMACRO    goOption LatexArg  	{ $$ = xxmarkup($1, $3, &@$); xxpopMode($2); }
	|	OPTMACRO    goOption Option LatexArg { $$ = xxOptionmarkup($1, $3, $4, &@$); xxpopMode($2); }
	|	RCODEMACRO  RLikeArg     	{ $$ = xxmarkup($1, $2, &@$); }
	|	VERBMACRO   VerbatimArg		{ $$ = xxmarkup($1, $2, &@$); }
	|	VERBMACRO2  VerbatimArg1	{ $$ = xxmarkup2($1, $2, R_NilValue, 1, &@$); }
	|       VERBMACRO2  VerbatimArg1 VerbatimArg2 { $$ = xxmarkup2($1, $2, $3, 2, &@$); }
	|	ESCAPE				{ $$ = xxmarkup($1, R_NilValue, &@$); }
	|	IFDEF IfDefTarget ArgItems ENDIF { $$ = xxmarkup2($1, $2, $3, 2, &@$); UNPROTECT_PTR($4); } 
	
LatexArg:	goLatexLike Arg		 	{ xxpopMode($1); $$ = $2; }

LatexArg2:	goLatexLike Arg			{ xxpopMode($1); $$ = $2; }
	|	goLatexLike TEXT		{ xxpopMode($1); $$ = xxnewlist($2); 
    	    					  warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, @2.first_line, @2.first_column); }	

Item0Arg:	goItem0 Arg		 	{ xxpopMode($1); $$ = $2; }

Item2Arg:	goItem2 Arg			{ xxpopMode($1); $$ = $2; }

RLikeArg:	goRLike Arg			{ xxpopMode($1); $$ = $2; }

VerbatimArg:	goVerbatim Arg		 	{ xxpopMode($1); $$ = $2; }

VerbatimArg1:	goVerbatim1 Arg			{ xxpopMode($1); $$ = $2; }

/* This one executes the push after seeing the brace starting the optional second arg */

VerbatimArg2:   '{' goVerbatim2 ArgItems '}'    { xxpopMode($2); $$ = $3; }
	|	'{' goVerbatim2 '}'		{ xxpopMode($2); $$ = xxnewlist(NULL); }

IfDefTarget:	goLatexLike TEXT	{ xxpopMode($1); $$ = xxnewlist($2); }


goLatexLike:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); }

goRLike:	/* empty */			{ $$ = xxpushMode(RLIKE, UNKNOWN, FALSE); }

goOption:	/* empty */			{ $$ = xxpushMode(INOPTION, UNKNOWN, FALSE); }

goVerbatim:	/* empty */			{ $$ = xxpushMode(VERBATIM, UNKNOWN, FALSE); }

goVerbatim1:	/* empty */			{ $$ = xxpushMode(VERBATIM, UNKNOWN, TRUE); }

goVerbatim2:    /* empty */			{ xxbraceDepth--; $$ = xxpushMode(VERBATIM, UNKNOWN, FALSE); xxbraceDepth++; }

goItem0:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, ESCAPE, FALSE); }

goItem2:	/* empty */			{ $$ = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); }

Arg:		'{' ArgItems  '}'		{ $$ = $2; }
	|	'{' '}'				{ $$ = xxnewlist(NULL); }

Option:		'[' Item ']'			{ $$ = $2; }	
		
%%

static SEXP xxpushMode(int newmode, int newitem, int neweqn)
{
    SEXP ans;
    PROTECT(ans = allocVector(INTSXP, 7));
    
    INTEGER(ans)[0] = xxmode;		/* Lexer mode */
    INTEGER(ans)[1] = xxitemType;	/* What is \item? */
    INTEGER(ans)[2] = xxbraceDepth;	/* Brace depth used in RCODE and VERBATIM */
    INTEGER(ans)[3] = xxinRString;      /* Quote char that started a string */
    INTEGER(ans)[4] = xxQuoteLine;      /* Where the quote was */
    INTEGER(ans)[5] = xxQuoteCol;       /*           "         */
    INTEGER(ans)[6] = xxinEqn;          /* In the first arg to \eqn or \deqn:  no escapes */
    
#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						xxmode, yytname[YYTRANSLATE(xxitemType)], xxbraceDepth);
#endif
    xxmode = newmode;
    xxitemType = newitem;
    xxbraceDepth = 0;
    xxinRString = 0;
    xxinEqn = neweqn;
    
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
    xxQuoteLine = INTEGER(oldmode)[4];
    xxQuoteCol  = INTEGER(oldmode)[5];
    xxinEqn	= INTEGER(oldmode)[6];
    
    UNPROTECT_PTR(oldmode);
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans, tmp;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PROTECT(tmp = NewList());
    if (item) {
    	PROTECT(ans = GrowList(tmp, item));
    	UNPROTECT_PTR(tmp);
    	UNPROTECT_PTR(item);
    } else ans = tmp;
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
    if (isNull(header))
    	PROTECT(header = mkString("LIST"));
    	
    setAttrib(ans, install("Rd_tag"), header);
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
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup2(SEXP header, SEXP body1, SEXP body2, int argcount, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p)", header, body1, body2);        
#endif
    
    PROTECT(ans = allocVector(VECSXP, argcount));
    if (!isNull(body1)) {
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    }
    if (!isNull(body2)) {
	if (argcount < 2) error("internal error: inconsistent argument count");
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    	UNPROTECT_PTR(body2);
    }
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
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
    	setAttrib(Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    }
    UNPROTECT_PTR(Rd);
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    setAttrib(item, install("Rd_tag"), mkString(yytname[YYTRANSLATE(type)]));
    setAttrib(item, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    return item;
}

static void xxWarnNewline()
{
    if (xxNewlineInString)
	warning(_("newline within quoted string at %s:%d"), xxBasename, xxNewlineInString);
}

  
/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth and one failed #ifdef or one numeric
   garbage markup match */

#define PUSHBACK_BUFSIZE 30

static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];

static int xxgetc(void)
{
    int c;
    
    if(npush) c = pushback[--npush]; else  c = ptr_getc();

    prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
    prevcols[prevpos] = xxcolno;
    prevbytes[prevpos] = xxbyteno;
    prevlines[prevpos] = xxlineno;    
    
    if (c == EOF) return R_EOF;
    
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = c;
    
    if (c == '\n') {
    	xxlineno += 1;
    	xxcolno = 1;
    	xxbyteno = 1;
    } else {
        xxcolno++;
    	xxbyteno++;
    }
    /* only advance column for 1st byte in UTF-8 */
    if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF && known_to_be_utf8) 
    	xxcolno--;

    if (c == '\t') xxcolno = ((xxcolno + 6) & ~7) + 1;
    
    R_ParseContextLine = xxlineno;
    
    return c;
}

static int xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    xxlineno = prevlines[prevpos];
    xxbyteno = prevbytes[prevpos];
    xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;
    
    R_ParseContextLine = xxlineno;
    
    R_ParseContext[R_ParseContextLast] = '\0';
    /* Mac OS X requires us to keep this non-negative */
    R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
	% PARSE_CONTEXT_SIZE;
    if(npush >= PUSHBACK_BUFSIZE - 2) return EOF;
    pushback[npush++] = c;
    return c;
}

static SEXP makeSrcref(YYLTYPE *lloc, SEXP srcfile)
{
    SEXP val;
    
    PROTECT(val = allocVector(INTSXP, 6));
    INTEGER(val)[0] = lloc->first_line;
    INTEGER(val)[1] = lloc->first_byte;
    INTEGER(val)[2] = lloc->last_line;
    INTEGER(val)[3] = lloc->last_byte;
    INTEGER(val)[4] = lloc->first_column;
    INTEGER(val)[5] = lloc->last_column;
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
    xxcolno = 1; 
    xxbyteno = 1;
    
    SrcFile = srcfile;
    
    npush = 0;
    xxmode = LATEXLIKE; 
    xxitemType = UNKNOWN;
    xxbraceDepth = 0;
    xxinRString = 0;
    xxNewlineInString = 0;
    xxinEqn = 0;
    
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
    { "\\name",    VSECTIONHEADER },
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
    { "\\dontshow",RCODEMACRO },
    { "\\donttest",RCODEMACRO },
    { "\\testonly",RCODEMACRO },
    
    /* These macros take one verbatim arg and ignore everything except braces */
    
    { "\\command", VERBMACRO },
    { "\\dontrun", VERBMACRO }, /* at least for now */    
    { "\\env",     VERBMACRO },
    { "\\kbd", 	   VERBMACRO },	
    { "\\option",  VERBMACRO },
    { "\\preformatted", VERBMACRO },
    
    { "\\samp",    VERBMACRO },
    { "\\special", VERBMACRO },
    { "\\url",     VERBMACRO },
    { "\\verb",    VERBMACRO },
    
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
#define YYENGLISH 16
	"$undefined",	"input", 	
	"SECTIONHEADER","section header",
	"RSECTIONHEADER","section header",
	"VSECTIONHEADER","section header",
	"LISTSECTION",	"section header",
	
	"LATEXMACRO",	"macro",
	"LATEXMACRO2",  "macro",
	"RCODEMACRO",	"macro",
	"VERBMACRO",    "macro",
	"VERBMACRO2",	"macro",
	
	"ESCAPE",	"macro",
	"ITEMIZE",	"macro",
	"IFDEF",	"conditional",
	"SECTIONHEADER2","section header",
	"OPTMACRO",	"macro",
	
	"DESCRIPTION",	"macro",
	"VERB",		"VERBATIM TEXT",
	0,		0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("macro");
    _("conditional");
    _("section header");
#endif 
   
    xxWarnNewline();	/* post newline warning if necessary */
    
    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = SrcFile;
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i, translated = FALSE;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i = 0; yytname_translations[i]; i += 2) {
    	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	    	sprintf(R_ParseErrorMsg, _("unexpected %s"), 
    	    	        i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	translated = TRUE;
    	    	break;
    	    }
    	}
    	if (!translated)
    	    sprintf(R_ParseErrorMsg, _("unexpected %s"),
    	                             s + sizeof yyunexpected - 1);
    	if (expecting) {
 	    translated = FALSE;
    	    for (i = 0; yytname_translations[i]; i += 2) {
    	    	if (!strcmp(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
    	    	    strcat(R_ParseErrorMsg, _(yyexpecting));
    	    	    strcat(R_ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	    translated = TRUE;
		    break;
		}
	    }
	    if (!translated) {
	    	strcat(R_ParseErrorMsg, _(yyexpecting));
	    	strcat(R_ParseErrorMsg, expecting + sizeof yyexpecting - 1);
	    }
	}
    } else {
    	sprintf(R_ParseErrorMsg, _("%s"),s);
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
    yylloc.first_byte = xxbyteno;
}

static void setlastloc(void)
{
    yylloc.last_line = prevlines[prevpos];
    yylloc.last_column = prevcols[prevpos];
    yylloc.last_byte = prevbytes[prevpos];
}

/* Split the input stream into tokens. */
/* This is the lowest of the parsing levels. */

static int token(void)
{
    int c, lookahead;
    int outsideLiteral = xxmode == LATEXLIKE || xxmode == INOPTION || xxbraceDepth == 0;

    setfirstloc();    
    c = xxgetc();

    switch (c) {
    	case '%': if (!xxinEqn) return mkComment(c);
    	    break;
	case '\\':
	    if (!xxinEqn) {
		lookahead = xxungetc(xxgetc());
		if (isalpha(lookahead) && xxmode != VERBATIM 
		    && (lookahead == 'l' || !xxinRString)) 
		    return mkMarkup(c);
	    }
	    break;
        case R_EOF:
            if (xxinRString) {
       		xxWarnNewline();
       		error(_("Unexpected end of input (in %c quoted string opened at %s:%d:%d)"), 
 			xxinRString, xxBasename, xxQuoteLine, xxQuoteCol);
    	    }
    	    return END_OF_INPUT; 
    	case '#':
    	    if (!xxinEqn && yylloc.first_column == 1) return mkIfdef(c);
    	    break;
    	case LBRACE:
    	    if (!xxinRString) {
    	    	xxbraceDepth++;
    	    	if (outsideLiteral) return c;
    	    }
    	    break;
    	case RBRACE:
    	    if (!xxinRString) {
    	    	xxbraceDepth--;
    	    	if (outsideLiteral || xxbraceDepth == 0) return c;
    	    }
    	    break;
    	case '[':
    	case ']':
    	    if (xxmode == INOPTION ) return c; 
    	    break;
    } 	    
	
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
    	    if (lookahead == LBRACE || lookahead == RBRACE ||
    	        lookahead == '%' || lookahead == '\\') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    if (isalpha(lookahead)) goto stop;
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
    else TEXT_PUSH(c);
    
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);    
    return COMMENT;
}

static int mkCode(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE && !xxinRString) xxbraceDepth--;
    if (c == RBRACE && !xxinRString) xxbraceDepth++; 
    
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
    	    	    int escaped = 0;
    	    	    TEXT_PUSH(c);
    	    	    c = xxgetc();
    	    	    if (c == '\\') {
		        int lookahead = xxgetc();
		        if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		            c = lookahead;
		            escaped = 1;
		        } else xxungetc(lookahead);
    		    }
    	    	    if (c == LBRACE && !escaped) xxbraceDepth++;
    	    	    else if (c == RBRACE && !escaped) xxbraceDepth--;
    	    	} while (c != '\n' && c != R_EOF && xxbraceDepth > 0);
    	    	if (c == RBRACE && !escaped) xxbraceDepth++; /* avoid double counting */
    	    }
    	    if (c == '\'' || c == '"' || c == '`') {
    	    	xxinRString = c;
    	    	xxQuoteLine = xxlineno;
    	    	xxQuoteCol  = xxcolno;
    	    } else if (c == '\\' && !escaped) {
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
    	if (c == '\n') {
    	    if (xxinRString && !xxNewlineInString) 
    	    	xxNewlineInString = xxlineno-1;
    	    break;
    	}
    	c = xxgetc();
    }
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);
    return RCODE; 
}

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval, attempt = 0;
    
    TEXT_PUSH(c);
    while (isalnum((c = xxgetc()))) TEXT_PUSH(c);
    
    while (attempt++ < 2) {
    	/* character escapes are processed as text, not markup */
    	if (bp == stext+1) {
    	    TEXT_PUSH(c);
    	    TEXT_PUSH('\0');
    	    retval = TEXT;
    	    c = xxgetc();
    	    break;
    	} else {
    	    TEXT_PUSH('\0');
    	    retval = KeywordLookup(stext);
    	    if (retval == UNKNOWN && attempt == 1) { /* try again, non-digits only */
    	    	bp--; 				     /* pop the \0 */
    	        while (isdigit(*(bp-1))) {
            	    xxungetc(c);
    	            c = *(--bp);                     /* pop the last letter into c */
            	}
            } else {
            	if (retval == NOITEM) 
    	    	    retval = xxitemType;
    	    	break;
    	    }
        }
    }
    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
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
    
    switch (retval) {
    case ENDIF:  /* eat chars to the end of the line */
    	do { c = xxgetc(); }
    	while (c != '\n' && c != R_EOF);
    	break;
    case UNKNOWN:
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
	break;
    }
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
		escaped = 1;
		if (xxinEqn) TEXT_PUSH(c);
		c = lookahead;
	    } else xxungetc(lookahead);
        }
        if (c == R_EOF) break;
        if (!escaped) {
    	    if (c == '%' && !xxinEqn) break;
	    else if (c == LBRACE) xxbraceDepth++;
    	    else if (c == RBRACE) {
	    	if (xxbraceDepth == 1) break;
	    	else xxbraceDepth--;
	    }
	}
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    };
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext,  bp - stext));
    if(stext != st0) free(stext);
    return VERB;  
}

static int yylex(void)
{
    int tok = token();
    
    if (xxDebugTokens) {
        Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
    	if (xxinRString) Rprintf("(in %c%c)", xxinRString, xxinRString);
    	if (tok > 255 && tok != END_OF_INPUT) 
    	    Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

/* "do_parseRd" 

 .Internal( parseRd(file, srcfile, encoding, verbose, basename) )
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
    xxDebugTokens = asInteger(CAR(args));		args = CDR(args);
    xxBasename = CHAR(STRING_ELT(CAR(args), 0));

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
