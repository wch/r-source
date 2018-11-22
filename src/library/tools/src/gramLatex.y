%{
/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2018  The R Core Team
 *  Copyright (C) 2010 Duncan Murdoch
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
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Parse.h>
#define STRICT_R_HEADERS
#include <R_ext/RS.h>           /* for R_chk_* allocation */
#include <ctype.h>
#include <R_ext/Print.h>
#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

/* bison creates a non-static symbol yylloc in both gramLatex.o and gramRd.o,
   so remap */

#define yylloc yyllocL

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
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
	if (N)								\
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
    while (0)

/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

/* Functions used in the parsing process */

static void	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);
static int	xxgetc();
static int	xxungetc(int);

/* Internal lexer / parser state variables */


static char const yyunknown[] = "unknown macro"; /* our message, not bison's */

typedef struct ParseState ParseState;
struct ParseState {
    int	xxlineno, xxbyteno, xxcolno;
    int	xxDebugTokens;  /* non-zero causes debug output to R console */
    SEXP	Value;
    int	xxinitvalue;
    SEXP	xxInVerbEnv;    /* Are we currently in a verbatim environment? If
				   so, this is the string to end it. If not, 
				   this is NULL */
    SEXP	xxVerbatimList;/* A STRSXP containing all the verbatim environment names */

    SEXP     SrcFile;  /* parseLatex will *always* supply a srcfile */
    
    ParseState *prevState;
};

static Rboolean busy = FALSE;
static ParseState parseState;

/* Routines used to build the parse tree */

static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static void	xxsavevalue(SEXP, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static SEXP 	xxenv(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxmath(SEXP, YYLTYPE *);
static SEXP	xxblock(SEXP, YYLTYPE *);
static void	xxSetInVerbEnv(SEXP);

static int	mkMarkup(int);
static int	mkText(int);
static int 	mkComment(int);
static int      mkVerb(int);
static int      mkVerbEnv();

static SEXP R_LatexTagSymbol = NULL;

#define YYSTYPE		SEXP

%}

%token		END_OF_INPUT ERROR
%token		MACRO
%token		TEXT COMMENT
%token	        BEGIN END VERB

/* Recent bison has <> to represent all of the destructors below, but we don't assume it */

/* I think we need to list everything here which occurs before the last item in a
   pattern, just in case the last item is unmatched and we need to back out.  But
   it is safe to list more, so we do. */

%destructor { UNPROTECT_PTR($$); } MACRO TEXT COMMENT BEGIN END

%%

Init:		Items END_OF_INPUT		{ xxsavevalue($1, &@$); YYACCEPT; }
	|	END_OF_INPUT			{ xxsavevalue(NULL, &@$); YYACCEPT; }
	|	error				{ PROTECT(parseState.Value = R_NilValue);  YYABORT; }
	;

Items:		Item				{ $$ = xxnewlist($1); }
	|	math				{ $$ = xxnewlist($1); }
	|	Items Item			{ $$ = xxlist($1, $2); }
	|	Items math			{ $$ = xxlist($1, $2); } 
	
nonMath:	Item				{ $$ = xxnewlist($1); }
	|	nonMath Item			{ $$ = xxlist($1, $2); }
	
Item:		TEXT				{ $$ = xxtag($1, TEXT, &@$); }
	|	COMMENT				{ $$ = xxtag($1, COMMENT, &@$); }
	|	MACRO				{ $$ = xxtag($1, MACRO, &@$); }
	|	VERB				{ $$ = xxtag($1, VERB, &@$); }
	|	environment			{ $$ = $1; }
	|	block				{ $$ = $1; }
	
environment:	BEGIN '{' TEXT '}' { xxSetInVerbEnv($3); } 
                Items END '{' TEXT '}' 	{ $$ = xxenv($3, $6, $9, &@$);
                                                  UNPROTECT_PTR($1); UNPROTECT_PTR($7); } 

math:		'$' nonMath '$'			{ $$ = xxmath($2, &@$); }

block:		'{' Items  '}'			{ $$ = xxblock($2, &@$); }
	|	'{' '}'				{ $$ = xxblock(NULL, &@$); }

%%

static SEXP xxnewlist(SEXP item)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PROTECT(ans = NewList());
    if (item) {
	GrowList(ans, item);
    	UNPROTECT_PTR(item);
    }
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxlist(SEXP list, SEXP item)
{
#if DEBUGVALS
    Rprintf("xxlist(list=%p, item=%p)", list, item);
#endif
    GrowList(list, item);
    UNPROTECT_PTR(item);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", list, length(list));
#endif
    return list;
}

static SEXP xxenv(SEXP begin, SEXP body, SEXP end, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxenv(begin=%p, body=%p, end=%p)", begin, body, end);    
#endif
    PROTECT(ans = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, begin);
    UNPROTECT_PTR(begin);
    if (!isNull(body)) {
	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body)));
    	UNPROTECT_PTR(body);	
    }
    /* FIXME:  check that begin and end match */
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    setAttrib(ans, R_LatexTagSymbol, mkString("ENVIRONMENT"));
    if (!isNull(end)) 
    	UNPROTECT_PTR(end);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmath(SEXP body, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmath(body=%p)", body);    
#endif
    PROTECT(ans = PairToVectorList(CDR(body)));
    UNPROTECT_PTR(body);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    setAttrib(ans, R_LatexTagSymbol, mkString("MATH"));
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxblock(SEXP body, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxblock(body=%p)", body);    
#endif
    if (!body) 
        PROTECT(ans = allocVector(VECSXP, 0));
    else {
	PROTECT(ans = PairToVectorList(CDR(body)));
    	UNPROTECT_PTR(body);	
    }
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    setAttrib(ans, R_LatexTagSymbol, mkString("BLOCK"));

#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static int VerbatimLookup(const char *s)
{
    int i;
    for (i = 0; i < length(parseState.xxVerbatimList); i++) {
    	if (strcmp(s, CHAR(STRING_ELT(parseState.xxVerbatimList, i))) == 0)
    	    return TRUE;
    }
    return FALSE;
}

static void xxSetInVerbEnv(SEXP envname)
{
    char buffer[256];
    if (VerbatimLookup(CHAR(STRING_ELT(envname, 0)))) {
    	snprintf(buffer, sizeof(buffer), "\\end{%s}", CHAR(STRING_ELT(envname, 0)));
    	PROTECT(parseState.xxInVerbEnv = ScalarString(mkChar(buffer)));
    } else parseState.xxInVerbEnv = NULL;
}

static void xxsavevalue(SEXP items, YYLTYPE *lloc)
{
    if (items) {
    	PROTECT(parseState.Value = PairToVectorList(CDR(items)));
    	UNPROTECT_PTR(items);
    } else {
    	PROTECT(parseState.Value = allocVector(VECSXP, 1));
    	SET_VECTOR_ELT(parseState.Value, 0, ScalarString(mkChar("")));
	setAttrib(VECTOR_ELT(parseState.Value, 0), R_LatexTagSymbol, mkString("TEXT"));
    }	
    if (!isNull(parseState.Value)) {
    	setAttrib(parseState.Value, R_ClassSymbol, mkString("LaTeX"));
    	setAttrib(parseState.Value, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    }
}

static SEXP xxtag(SEXP item, int type, YYLTYPE *lloc)
{
    setAttrib(item, R_LatexTagSymbol, mkString(yytname[YYTRANSLATE(type)]));
    setAttrib(item, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    return item;
}

/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth  */

#define PUSHBACK_BUFSIZE 30

static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];

static int xxgetc(void)
{
    int c, oldpos;
    
    if(npush) c = pushback[--npush]; else  c = ptr_getc();

    oldpos = prevpos;
    prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
    prevbytes[prevpos] = parseState.xxbyteno;
    prevlines[prevpos] = parseState.xxlineno;    
    /* We only advance the column for the 1st byte in UTF-8, so handle later bytes specially */
    if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF) {
    	parseState.xxcolno--;   
    	prevcols[prevpos] = prevcols[oldpos];
    } else 
    	prevcols[prevpos] = parseState.xxcolno;
    
    if (c == EOF) return R_EOF;
    
    R_ParseContextLast = (R_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    R_ParseContext[R_ParseContextLast] = (char) c;
    
    if (c == '\n') {
    	parseState.xxlineno += 1;
    	parseState.xxcolno = 1;
    	parseState.xxbyteno = 1;
    } else {
        parseState.xxcolno++;
    	parseState.xxbyteno++;
    }

    if (c == '\t') parseState.xxcolno = ((parseState.xxcolno + 6) & ~7) + 1;
    
    R_ParseContextLine = parseState.xxlineno;
    
    return c;
}

static int xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    parseState.xxlineno = prevlines[prevpos];
    parseState.xxbyteno = prevbytes[prevpos];
    parseState.xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;
    
    R_ParseContextLine = parseState.xxlineno;
    
    R_ParseContext[R_ParseContextLast] = '\0';
    /* macOS requires us to keep this non-negative */
    R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
	% PARSE_CONTEXT_SIZE;
    if(npush >= PUSHBACK_BUFSIZE - 2) return R_EOF;
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
    UNPROTECT(1); /* val */
    return val;
}

static SEXP mkString2(const char *s, size_t len)
{
    SEXP t;
    cetype_t enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, (int) len, enc));
    UNPROTECT(1); /* t */
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

static void GrowList(SEXP l, SEXP s)
{
    SEXP tmp;
    tmp = CONS(s, R_NilValue);
    SETCDR(CAR(l), tmp);
    SETCAR(l, tmp);
}

/*--------------------------------------------------------------------------*/

static void PutState(ParseState *state) {
    state->xxlineno = parseState.xxlineno;
    state->xxbyteno = parseState.xxbyteno;
    state->xxcolno = parseState.xxcolno;
    state->xxDebugTokens = parseState.xxDebugTokens;
    state->Value = parseState.Value;
    state->xxinitvalue = parseState.xxinitvalue;
    state->xxInVerbEnv = parseState.xxInVerbEnv;
    state->xxVerbatimList = parseState.xxVerbatimList;
    state->SrcFile = parseState.SrcFile; 
    state->prevState = parseState.prevState;
}

static void UseState(ParseState *state) {
    parseState.xxlineno = state->xxlineno;
    parseState.xxbyteno = state->xxbyteno;
    parseState.xxcolno = state->xxcolno;
    parseState.xxDebugTokens = state->xxDebugTokens;
    parseState.Value = state->Value;
    parseState.xxinitvalue = state->xxinitvalue;
    parseState.xxInVerbEnv = state->xxInVerbEnv;
    parseState.xxVerbatimList = state->xxVerbatimList;
    parseState.SrcFile = state->SrcFile; 
    parseState.prevState = state->prevState;
}

static SEXP ParseLatex(ParseStatus *status, SEXP srcfile)
{
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
    	
    parseState.xxInVerbEnv = NULL;
    
    parseState.xxlineno = 1;
    parseState.xxcolno = 1; 
    parseState.xxbyteno = 1;
    
    parseState.SrcFile = srcfile;
    
    npush = 0;
    
    parseState.Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", parseState.Value);    
#endif    
    UNPROTECT_PTR(parseState.Value);
    return parseState.Value;
}

static const char * nextchar_parse;

/* need to handle incomplete last line */
static int char_getc(void)
{
    int c;
    
    c = *nextchar_parse++;
    if (!c) {
    	c = R_EOF;
    	nextchar_parse--;
    }
    return (c);
}

static
SEXP R_ParseLatex(SEXP text, ParseStatus *status, SEXP srcfile)
{
    nextchar_parse = translateCharUTF8(STRING_ELT(text, 0));
    ptr_getc = char_getc;
    return ParseLatex(status, srcfile);
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
    
    { "\\begin",  BEGIN },
    { "\\end",    END },
    { "\\verb",   VERB },
    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7   

static int KeywordLookup(const char *s)
{
    int i;
    for (i = 0; keywords[i].name; i++) {
	if (strcmp(keywords[i].name, s) == 0) 
	    return keywords[i].token;
    }
    return MACRO;
}

static void yyerror(const char *s)
{
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */    
#define YYENGLISH 3
	"$undefined",	"input", 	
	"LATEXMACRO",	"macro",
	"ESCAPE",	"macro",
	0,		0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    static char const yyshortunexpected[] = "unexpected %s";
    static char const yylongunexpected[] = "unexpected %s '%s'";
    char *expecting;
    char ParseErrorMsg[PARSE_ERROR_SIZE];
    SEXP filename;
    char ParseErrorFilename[PARSE_ERROR_SIZE];
   
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i, translated = FALSE;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i = 0; yytname_translations[i]; i += 2) {
    	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	    	if (yychar < 256)
    	    	    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
			     _(yyshortunexpected), 
			     i/2 < YYENGLISH ? _(yytname_translations[i+1])
			     : yytname_translations[i+1]);
    	    	else
    	    	    snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
			     _(yylongunexpected), 
			     i/2 < YYENGLISH ? _(yytname_translations[i+1])
			     : yytname_translations[i+1], 
			     CHAR(STRING_ELT(yylval, 0)));
    	    	translated = TRUE;
    	    	break;
    	    }
    	}
    	if (!translated) {
    	    if (yychar < 256) 
    		snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, 
			 _(yyshortunexpected),
			 s + sizeof yyunexpected - 1);
    	    else
    	    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,
			 _(yylongunexpected),
			 s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
    	}
    	if (expecting) {
 	    translated = FALSE;
    	    for (i = 0; yytname_translations[i]; i += 2) {
    	    	if (!strcmp(expecting + sizeof yyexpecting - 1, yytname_translations[i])) {
    	    	    strcat(ParseErrorMsg, _(yyexpecting));
    	    	    strcat(ParseErrorMsg, i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1]);
    	    	    translated = TRUE;
		    break;
		}
	    }
	    if (!translated) {
	    	strcat(ParseErrorMsg, _(yyexpecting));
	    	strcat(ParseErrorMsg, expecting + sizeof yyexpecting - 1);
	    }
	}
    } else if (!strncmp(s, yyunknown, sizeof yyunknown-1)) {
    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, 
		 "%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
    } else {
    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE,"%s", s);
    }
    filename = findVar(install("filename"), parseState.SrcFile);
    if (isString(filename) && LENGTH(filename))
    	strncpy(ParseErrorFilename, CHAR(STRING_ELT(filename, 0)), PARSE_ERROR_SIZE - 1);
    else
        ParseErrorFilename[0] = '\0';
    if (yylloc.first_line != yylloc.last_line)
	warning("%s:%d-%d: %s", 
		ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
    else
	warning("%s:%d: %s", 
		ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
}

#define TEXT_PUSH(c) do {                  \
	size_t nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
            nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = ((char)c);		    \
} while(0)

static void setfirstloc(void)
{
    yylloc.first_line = parseState.xxlineno;
    yylloc.first_column = parseState.xxcolno;
    yylloc.first_byte = parseState.xxbyteno;
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
    int c;

    if (parseState.xxinitvalue) {
        yylloc.first_line = 0;
        yylloc.first_column = 0;
        yylloc.first_byte = 0;
        yylloc.last_line = 0;
        yylloc.last_column = 0;
        yylloc.last_byte = 0;
    	PROTECT(yylval = mkString(""));
        c = parseState.xxinitvalue;
    	parseState.xxinitvalue = 0;
    	return(c);
    }
    
    setfirstloc();    
    
    if (parseState.xxInVerbEnv)
    	return mkVerbEnv();    
    	
    c = xxgetc();
    
    switch (c) {
    	case '%': return mkComment(c);
	case '\\':return mkMarkup(c);
        case R_EOF:return END_OF_INPUT; 
    	case LBRACE:return c;
    	case RBRACE:return c;
    	case '$': return c;
    } 	    
    return mkText(c);
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    while(1) {
    	switch (c) {
    	case '\\': 
    	case '%':
    	case LBRACE:
    	case RBRACE:
    	case '$':
    	case R_EOF:
    	    goto stop;
    	}
    	TEXT_PUSH(c);
    	c = xxgetc();
    };
stop:
    xxungetc(c);
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

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int retval = 0;
    
    TEXT_PUSH(c);
    while (isalpha((c = xxgetc()))) TEXT_PUSH(c);
    
    /* One non-alpha allowed */
    if (bp - stext == 1) {
    	TEXT_PUSH(c);
    	TEXT_PUSH('\0');
    	retval = MACRO;
    } else {
	TEXT_PUSH('\0');       
        retval = KeywordLookup(stext);
        if (retval == VERB)
            retval = mkVerb(c); /* This makes the yylval */
        else if (c != ' ') /* Eat a space, but keep other terminators */
    	    xxungetc(c);
    }
    if (retval != VERB)
	PROTECT(yylval = mkString(stext));
    if(stext != st0) free(stext);
    return retval;
}

static int mkVerb(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int delim = c;   
    
    TEXT_PUSH('\\'); TEXT_PUSH('v'); TEXT_PUSH('e'); TEXT_PUSH('r'); TEXT_PUSH('b');
    TEXT_PUSH(c);
    while ((c = xxgetc()) != delim) TEXT_PUSH(c);
    TEXT_PUSH(c);
    
    PROTECT(yylval = mkString2(stext, bp - stext));
    if(stext != st0) free(stext);
    return VERB;  
}

static int mkVerbEnv()
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int matched = 0, i;
    int c;
    
    while ((c = xxgetc()) != R_EOF && CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched]) {
    	TEXT_PUSH(c);
    	if (c == CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched])
    	    matched++;
    	else
    	    matched = 0;
    }
    if ( !CHAR(STRING_ELT(parseState.xxInVerbEnv, 0))[matched] ) {
    	for (i = matched-1; i >= 0; i--) 
    	    xxungetc(*(--bp));    	    
    	UNPROTECT_PTR(parseState.xxInVerbEnv);
    	parseState.xxInVerbEnv = NULL;
    }
    	    
    PROTECT(yylval = mkString2(stext, bp - stext));
    if (stext != st0) free(stext);
    return VERB;
}

static int yylex(void)
{
    int tok = token();
    
    if (parseState.xxDebugTokens) {
        Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
    	if (tok > 255 && tok != END_OF_INPUT) 
    	    Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

static void PushState() {
    if (busy) {
    	ParseState *prev = malloc(sizeof(ParseState));
    	PutState(prev);
    	parseState.prevState = prev;
    } else 
        parseState.prevState = NULL;  
    busy = TRUE;
}

static void PopState() {
    if (parseState.prevState) {
    	ParseState *prev = parseState.prevState;
    	UseState(prev);
    	free(prev);
    } else
    	busy = FALSE;
}

static void InitSymbols(void)
{
    R_LatexTagSymbol = install("latex_tag");
}

/* "do_parseLatex" 

 .External2("parseLatex", file, srcfile, verbose, basename, warningCalls)
 If there is text then that is read and the other arguments are ignored.
*/

SEXP parseLatex(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);

    SEXP s = R_NilValue, source, text;
    ParseStatus status;
    InitSymbols();

#if DEBUGMODE
    yydebug = 1;
#endif 

    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';
    
    PushState();

    text = CAR(args);		                        args = CDR(args);

    source = CAR(args);					args = CDR(args);
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    parseState.xxDebugTokens = asInteger(CAR(args));	args = CDR(args);
    parseState.xxVerbatimList = CAR(args); 		args = CDR(args);

    s = R_ParseLatex(text, &status, source);
    
    PopState();
    	
    if (status != PARSE_OK) parseError(call, R_ParseError);
    return s;
}
