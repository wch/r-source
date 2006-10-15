
/*  A Bison parser, made from ./gram.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	END_OF_INPUT	257
#define	ERROR	258
#define	STR_CONST	259
#define	NUM_CONST	260
#define	NULL_CONST	261
#define	SYMBOL	262
#define	FUNCTION	263
#define	LEFT_ASSIGN	264
#define	EQ_ASSIGN	265
#define	RIGHT_ASSIGN	266
#define	LBB	267
#define	FOR	268
#define	IN	269
#define	IF	270
#define	ELSE	271
#define	WHILE	272
#define	NEXT	273
#define	BREAK	274
#define	REPEAT	275
#define	GT	276
#define	GE	277
#define	LT	278
#define	LE	279
#define	EQ	280
#define	NE	281
#define	AND	282
#define	OR	283
#define	NS_GET	284
#define	NS_GET_INT	285
#define	LOW	286
#define	TILDE	287
#define	UNOT	288
#define	NOT	289
#define	SPECIAL	290
#define	UMINUS	291
#define	UPLUS	292

#line 1 "./gram.y"

/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2006  Robert Gentleman, Ross Ihaka and the
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
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301
 *  USA.
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

static void yyerror(char *);
static int yylex();
int yyparse(void);

/* alloca.h inclusion is now covered by Defn.h */

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

static SEXP	mkComplex(char *);
SEXP		mkFalse(void);
static SEXP     mkFloat(char *);
static SEXP	mkNA(void);
SEXP		mkTrue(void);

/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxgetc();
static int	xxungetc();
static int 	xxcharcount, xxcharsave;

static int	conPrevChar, conThisChar, lastExprEnd, thisExprStart;
static SEXP     SrcFile;

#if defined(SUPPORT_MBCS)
# include <R_ext/Riconv.h>
# include <R_ext/rlocale.h>
# include <wchar.h>
# include <wctype.h>
# include <sys/param.h>
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

/* Previous versions (< 2.3.0) assumed wchar_t was in Unicode (and it
   commonly is).  This version does not. */
# ifdef Win32
static const char UNICODE[] = "UCS-2LE";
# else
#  ifdef WORDS_BIGENDIAN
static const char UNICODE[] = "UCS-4BE";
#  else
static const char UNICODE[] = "UCS-4LE";
# endif
#endif
#include <errno.h>

static size_t ucstomb(char *s, wchar_t wc, mbstate_t *ps)
{
    char     tocode[128];
    char     buf[16];
    void    *cd = NULL ;
    wchar_t  wcs[2];
    char    *inbuf = (char *) wcs;
    size_t   inbytesleft = sizeof(wchar_t);
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;
    
    if(wc == L'\0') {
	*s = '\0';
        return 1;
    }
    
    strcpy(tocode, "");
    memset(buf, 0, sizeof(buf));
    memset(wcs, 0, sizeof(wcs));
    wcs[0] = wc;

    if((void *)(-1) == (cd = Riconv_open("", (char *)UNICODE))) {
#ifndef  Win32
        /* locale set fuzzy case */
    	strncpy(tocode, locale2charset(NULL), sizeof(tocode));
	if((void *)(-1) == (cd = Riconv_open(tocode, (char *)UNICODE)))
            return (size_t)(-1); 
#else
        return (size_t)(-1);
#endif
    }
    
    status = Riconv(cd, &inbuf, &inbytesleft, &outbuf, &outbytesleft);
    Riconv_close(cd);

    if (status == (size_t) -1) {
        switch(errno){
        case EINVAL:
            return (size_t) -2;
        case EILSEQ:
            return (size_t) -1;
        case E2BIG:
            break;
        default:
            errno = EILSEQ;
            return (size_t) -1;
        }
    }
    strncpy(s, buf, sizeof(buf) - 1); /* ensure 0-terminated */
    return strlen(buf);
}

static int mbcs_get_next(int c, wchar_t *wc)
{
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    if((unsigned int)c < 0x80) {
	*wc = (wchar_t) c;
	return 1;
    }
    if(utf8locale) {
	clen = utf8clen(c);
	for(i = 1; i < clen; i++) {
	    s[i] = xxgetc();
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char"));
	}
	res = mbrtowc(wc, s, clen, NULL);
	if(res == -1) error(_("invalid multibyte character in mbcs_get_next"));
    } else {
	while(clen <= MB_CUR_MAX) {
	    mbs_init(&mb_st);
	    res = mbrtowc(wc, s, clen, &mb_st);
	    if(res >= 0) break;
	    if(res == -1) 
		error(_("invalid multibyte character in mbcs_get_next"));
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
static SEXP SrcFile;

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

#ifndef YYSTYPE
#define YYSTYPE int
#endif
#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		159
#define	YYFLAG		-32768
#define	YYNTBASE	60

#define YYTRANSLATE(x) ((unsigned)(x) <= 292 ? yytranslate[x] : 72)

static const char yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,    51,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    56,     2,     2,    47,    57,     2,     2,    49,
    55,    40,    38,    59,    39,     2,    41,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,    43,    52,     2,
     2,     2,    32,    48,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
    50,     2,    58,    46,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,    53,     2,    54,    34,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    33,    35,    36,    37,    42,
    44,    45
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     7,    10,    12,    14,    16,    20,    22,
    24,    26,    28,    32,    36,    39,    42,    45,    48,    51,
    55,    59,    63,    67,    71,    75,    79,    83,    87,    91,
    95,    99,   103,   107,   111,   115,   119,   123,   127,   131,
   138,   143,   147,   153,   157,   161,   164,   170,   175,   179,
   183,   187,   191,   195,   199,   203,   207,   211,   215,   219,
   223,   225,   227,   231,   235,   241,   242,   244,   248,   251,
   255,   258,   260,   265,   266,   268,   271,   275,   278,   282,
   285,   289,   290,   292,   296,   300,   306
};

static const short yyrhs[] = {     3,
     0,    51,     0,    61,    51,     0,    61,    52,     0,     1,
     0,    63,     0,    62,     0,    63,    11,    61,     0,     6,
     0,     5,     0,     7,     0,     8,     0,    53,    67,    54,
     0,    49,    61,    55,     0,    39,    63,     0,    38,    63,
     0,    56,    63,     0,    34,    63,     0,    32,    63,     0,
    63,    43,    63,     0,    63,    38,    63,     0,    63,    39,
    63,     0,    63,    40,    63,     0,    63,    41,    63,     0,
    63,    46,    63,     0,    63,    42,    63,     0,    63,    57,
    63,     0,    63,    34,    63,     0,    63,    32,    63,     0,
    63,    24,    63,     0,    63,    25,    63,     0,    63,    26,
    63,     0,    63,    27,    63,     0,    63,    23,    63,     0,
    63,    22,    63,     0,    63,    28,    63,     0,    63,    29,
    63,     0,    63,    10,    63,     0,    63,    12,    63,     0,
     9,    49,    70,    55,    71,    61,     0,    63,    49,    68,
    55,     0,    16,    65,    61,     0,    16,    65,    61,    17,
    61,     0,    14,    66,    61,     0,    18,    64,    61,     0,
    21,    61,     0,    63,    13,    68,    58,    58,     0,    63,
    50,    68,    58,     0,     8,    30,     8,     0,     8,    30,
     5,     0,     5,    30,     8,     0,     5,    30,     5,     0,
     8,    31,     8,     0,     8,    31,     5,     0,     5,    31,
     8,     0,     5,    31,     5,     0,    63,    47,     8,     0,
    63,    47,     5,     0,    63,    48,     8,     0,    63,    48,
     5,     0,    19,     0,    20,     0,    49,    63,    55,     0,
    49,    63,    55,     0,    49,     8,    15,    63,    55,     0,
     0,    61,     0,    67,    52,    61,     0,    67,    52,     0,
    67,    51,    61,     0,    67,    51,     0,    69,     0,    68,
    71,    59,    69,     0,     0,    63,     0,     8,    11,     0,
     8,    11,    63,     0,     5,    11,     0,     5,    11,    63,
     0,     7,    11,     0,     7,    11,    63,     0,     0,     8,
     0,     8,    11,    63,     0,    70,    59,     8,     0,    70,
    59,     8,    11,    63,     0,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
   289,   290,   291,   292,   293,   296,   297,   300,   303,   304,
   305,   306,   308,   309,   311,   312,   313,   314,   315,   317,
   318,   319,   320,   321,   322,   323,   324,   325,   326,   327,
   328,   329,   330,   331,   332,   333,   334,   336,   337,   338,
   340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
   350,   351,   352,   353,   354,   355,   356,   357,   358,   359,
   360,   361,   365,   368,   371,   375,   376,   377,   378,   379,
   380,   383,   384,   387,   388,   389,   390,   391,   392,   393,
   394,   397,   398,   399,   400,   401,   404
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","END_OF_INPUT",
"ERROR","STR_CONST","NUM_CONST","NULL_CONST","SYMBOL","FUNCTION","LEFT_ASSIGN",
"EQ_ASSIGN","RIGHT_ASSIGN","LBB","FOR","IN","IF","ELSE","WHILE","NEXT","BREAK",
"REPEAT","GT","GE","LT","LE","EQ","NE","AND","OR","NS_GET","NS_GET_INT","'?'",
"LOW","'~'","TILDE","UNOT","NOT","'+'","'-'","'*'","'/'","SPECIAL","':'","UMINUS",
"UPLUS","'^'","'$'","'@'","'('","'['","'\\n'","';'","'{'","'}'","')'","'!'",
"'%'","']'","','","prog","expr_or_assign","equal_assign","expr","cond","ifcond",
"forcond","exprlist","sublist","sub","formlist","cr", NULL
};
#endif

static const short yyr1[] = {     0,
    60,    60,    60,    60,    60,    61,    61,    62,    63,    63,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
    63,    63,    64,    65,    66,    67,    67,    67,    67,    67,
    67,    68,    68,    69,    69,    69,    69,    69,    69,    69,
    69,    70,    70,    70,    70,    70,    71
};

static const short yyr2[] = {     0,
     1,     1,     2,     2,     1,     1,     1,     3,     1,     1,
     1,     1,     3,     3,     2,     2,     2,     2,     2,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     6,
     4,     3,     5,     3,     3,     2,     5,     4,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     1,     1,     3,     3,     5,     0,     1,     3,     2,     3,
     2,     1,     4,     0,     1,     2,     3,     2,     3,     2,
     3,     0,     1,     3,     3,     5,     0
};

static const short yydefact[] = {     0,
     5,     1,    10,     9,    11,    12,     0,     0,     0,     0,
    61,    62,     0,     0,     0,     0,     0,     0,     2,    66,
     0,     0,     7,     6,     0,     0,     0,     0,    82,     0,
     0,     0,     0,     0,     0,    46,    19,    18,    16,    15,
     0,    67,     0,    17,     3,     4,     0,     0,     0,    74,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
    74,     0,    52,    51,    56,    55,    50,    49,    54,    53,
    83,     0,     0,    44,     0,    42,     0,    45,    14,    71,
    69,    13,    38,     8,    39,    10,    11,    12,    75,    87,
    72,    35,    34,    30,    31,    32,    33,    36,    37,    29,
    28,    21,    22,    23,    24,    26,    20,    25,    58,    57,
    60,    59,    87,    87,    27,     0,    87,     0,     0,    64,
     0,    63,    70,    68,    78,    80,    76,     0,     0,    41,
    48,    84,     0,    85,     0,    43,    79,    81,    77,    47,
    74,    40,     0,    65,    73,    86,     0,     0,     0
};

static const short yydefgoto[] = {   157,
    22,    23,    24,    35,    33,    31,    43,   100,   101,    82,
   139
};

static const short yypact[] = {   137,
-32768,-32768,    -9,-32768,-32768,    32,   -15,     6,    29,    34,
-32768,-32768,    61,    61,    61,    61,    61,    61,-32768,    61,
    61,    13,-32768,   224,     7,     9,    11,    22,     0,    52,
    61,    61,    61,    61,    61,-32768,   429,   505,    78,    78,
    16,-32768,   -41,   581,-32768,-32768,    61,    61,    61,   189,
    61,    61,    61,    61,    61,    61,    61,    61,    61,    61,
    61,    61,    61,    61,    61,    61,    61,    23,    24,   189,
   189,    61,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    62,   -50,    71,-32768,   265,    70,   306,-32768,-32768,    61,
    61,-32768,   429,-32768,   467,    -7,    77,    -5,   388,    36,
-32768,   619,   619,   619,   619,   619,   619,   581,   543,   429,
   505,   637,   637,   117,   117,   168,    78,    78,-32768,-32768,
-32768,-32768,    35,    38,   388,    61,-32768,    84,    61,-32768,
    61,-32768,-32768,-32768,    61,    61,    61,    39,    42,-32768,
-32768,   388,    61,    87,   347,-32768,   388,   388,   388,-32768,
   189,-32768,    61,-32768,-32768,   388,   102,   103,-32768
};

static const short yypgoto[] = {-32768,
    41,-32768,   -14,-32768,-32768,-32768,-32768,    14,   -47,-32768,
   -22
};


#define	YYLAST		694


static const short yytable[] = {    37,
    38,    39,    40,   135,   127,   137,    44,    81,   128,    90,
    91,    73,    92,    75,    74,    77,    76,    85,    78,    87,
    25,    26,    25,    26,    27,    28,    79,   119,   121,    80,
   120,   122,    93,    29,    95,    99,   102,   103,   104,   105,
   106,   107,   108,   109,   110,   111,   112,   113,   114,   115,
   116,   117,   118,    36,    30,    99,    99,   125,    41,    83,
    42,    27,    28,    45,    46,     3,     4,     5,     6,     7,
    89,    84,   126,    86,     8,    88,     9,    32,    10,    11,
    12,    13,    34,   123,   124,   129,   131,   136,    94,   140,
    50,   144,    14,   138,    15,   141,   150,   153,    16,    17,
   151,   158,   159,   155,   143,     0,     0,     0,     0,    18,
     0,   142,     0,    20,   145,     0,    21,     0,     0,     0,
   147,   148,   149,    67,    68,    69,    70,    71,     0,    50,
   133,   134,     0,     0,    72,     0,    99,     1,   156,     2,
     0,     3,     4,     5,     6,     7,     0,     0,     0,     0,
     8,     0,     9,     0,    10,    11,    12,    13,    65,    66,
     0,     0,    67,    68,    69,    70,    71,     0,    14,     0,
    15,   146,     0,    72,    16,    17,     0,     0,     0,     0,
    50,     0,     0,   152,     0,    18,     0,    19,     0,    20,
     0,     0,    21,    96,     4,    97,    98,     7,     0,     0,
     0,     0,     8,     0,     9,     0,    10,    11,    12,    13,
    66,     0,     0,    67,    68,    69,    70,    71,     0,     0,
    14,     0,    15,     0,    72,     0,    16,    17,     0,     0,
     0,     0,     0,    47,    48,    49,    50,    18,     0,     0,
     0,    20,     0,     0,    21,    51,    52,    53,    54,    55,
    56,    57,    58,     0,     0,    59,     0,    60,     0,     0,
     0,    61,    62,    63,    64,    65,    66,     0,     0,    67,
    68,    69,    70,    71,    47,     0,    49,    50,     0,     0,
    72,     0,     0,     0,     0,     0,    51,    52,    53,    54,
    55,    56,    57,    58,     0,     0,    59,     0,    60,     0,
     0,     0,    61,    62,    63,    64,    65,    66,     0,     0,
    67,    68,    69,    70,    71,    47,     0,    49,    50,   130,
     0,    72,     0,     0,     0,     0,     0,    51,    52,    53,
    54,    55,    56,    57,    58,     0,     0,    59,     0,    60,
     0,     0,     0,    61,    62,    63,    64,    65,    66,     0,
     0,    67,    68,    69,    70,    71,    47,     0,    49,    50,
   132,     0,    72,     0,     0,     0,     0,     0,    51,    52,
    53,    54,    55,    56,    57,    58,     0,     0,    59,     0,
    60,     0,     0,     0,    61,    62,    63,    64,    65,    66,
     0,     0,    67,    68,    69,    70,    71,    47,     0,    49,
    50,   154,     0,    72,     0,     0,     0,     0,     0,    51,
    52,    53,    54,    55,    56,    57,    58,     0,     0,    59,
     0,    60,     0,     0,     0,    61,    62,    63,    64,    65,
    66,     0,     0,    67,    68,    69,    70,    71,    47,     0,
    49,    50,     0,     0,    72,     0,     0,     0,     0,     0,
    51,    52,    53,    54,    55,    56,    57,    58,     0,     0,
     0,     0,    60,     0,     0,     0,    61,    62,    63,    64,
    65,    66,     0,     0,    67,    68,    69,    70,    71,    50,
     0,     0,     0,     0,     0,    72,     0,     0,    51,    52,
    53,    54,    55,    56,    57,    58,     0,     0,     0,     0,
    60,     0,     0,     0,    61,    62,    63,    64,    65,    66,
     0,     0,    67,    68,    69,    70,    71,    50,     0,     0,
     0,     0,     0,    72,     0,     0,    51,    52,    53,    54,
    55,    56,    57,    58,     0,     0,     0,     0,     0,     0,
     0,     0,    61,    62,    63,    64,    65,    66,     0,     0,
    67,    68,    69,    70,    71,    50,     0,     0,     0,     0,
     0,    72,     0,     0,    51,    52,    53,    54,    55,    56,
    57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    61,    62,    63,    64,    65,    66,     0,     0,    67,    68,
    69,    70,    71,    50,     0,     0,     0,     0,     0,    72,
     0,     0,    51,    52,    53,    54,    55,    56,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    61,    62,
    63,    64,    65,    66,     0,     0,    67,    68,    69,    70,
    71,    50,     0,     0,     0,     0,     0,    72,     0,     0,
-32768,-32768,-32768,-32768,-32768,-32768,     0,     0,     0,    50,
     0,     0,     0,     0,     0,     0,    61,    62,    63,    64,
    65,    66,     0,     0,    67,    68,    69,    70,    71,     0,
     0,     0,     0,     0,     0,    72,    63,    64,    65,    66,
     0,     0,    67,    68,    69,    70,    71,     0,     0,     0,
     0,     0,     0,    72
};

static const short yycheck[] = {    14,
    15,    16,    17,    11,    55,    11,    21,     8,    59,    51,
    52,     5,    54,     5,     8,     5,     8,    32,     8,    34,
    30,    31,    30,    31,    30,    31,     5,     5,     5,     8,
     8,     8,    47,    49,    49,    50,    51,    52,    53,    54,
    55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
    65,    66,    67,    13,    49,    70,    71,    72,    18,     8,
    20,    30,    31,    51,    52,     5,     6,     7,     8,     9,
    55,    31,    11,    33,    14,    35,    16,    49,    18,    19,
    20,    21,    49,    70,    71,    15,    17,    11,    48,    55,
    13,     8,    32,    58,    34,    58,    58,    11,    38,    39,
    59,     0,     0,   151,   127,    -1,    -1,    -1,    -1,    49,
    -1,   126,    -1,    53,   129,    -1,    56,    -1,    -1,    -1,
   135,   136,   137,    46,    47,    48,    49,    50,    -1,    13,
    90,    91,    -1,    -1,    57,    -1,   151,     1,   153,     3,
    -1,     5,     6,     7,     8,     9,    -1,    -1,    -1,    -1,
    14,    -1,    16,    -1,    18,    19,    20,    21,    42,    43,
    -1,    -1,    46,    47,    48,    49,    50,    -1,    32,    -1,
    34,   131,    -1,    57,    38,    39,    -1,    -1,    -1,    -1,
    13,    -1,    -1,   143,    -1,    49,    -1,    51,    -1,    53,
    -1,    -1,    56,     5,     6,     7,     8,     9,    -1,    -1,
    -1,    -1,    14,    -1,    16,    -1,    18,    19,    20,    21,
    43,    -1,    -1,    46,    47,    48,    49,    50,    -1,    -1,
    32,    -1,    34,    -1,    57,    -1,    38,    39,    -1,    -1,
    -1,    -1,    -1,    10,    11,    12,    13,    49,    -1,    -1,
    -1,    53,    -1,    -1,    56,    22,    23,    24,    25,    26,
    27,    28,    29,    -1,    -1,    32,    -1,    34,    -1,    -1,
    -1,    38,    39,    40,    41,    42,    43,    -1,    -1,    46,
    47,    48,    49,    50,    10,    -1,    12,    13,    -1,    -1,
    57,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    -1,    -1,    32,    -1,    34,    -1,
    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,    -1,
    46,    47,    48,    49,    50,    10,    -1,    12,    13,    55,
    -1,    57,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,
    25,    26,    27,    28,    29,    -1,    -1,    32,    -1,    34,
    -1,    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,
    -1,    46,    47,    48,    49,    50,    10,    -1,    12,    13,
    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    22,    23,
    24,    25,    26,    27,    28,    29,    -1,    -1,    32,    -1,
    34,    -1,    -1,    -1,    38,    39,    40,    41,    42,    43,
    -1,    -1,    46,    47,    48,    49,    50,    10,    -1,    12,
    13,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,    22,
    23,    24,    25,    26,    27,    28,    29,    -1,    -1,    32,
    -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,    42,
    43,    -1,    -1,    46,    47,    48,    49,    50,    10,    -1,
    12,    13,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,
    22,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
    -1,    -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,
    42,    43,    -1,    -1,    46,    47,    48,    49,    50,    13,
    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    22,    23,
    24,    25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,
    34,    -1,    -1,    -1,    38,    39,    40,    41,    42,    43,
    -1,    -1,    46,    47,    48,    49,    50,    13,    -1,    -1,
    -1,    -1,    -1,    57,    -1,    -1,    22,    23,    24,    25,
    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,    -1,
    46,    47,    48,    49,    50,    13,    -1,    -1,    -1,    -1,
    -1,    57,    -1,    -1,    22,    23,    24,    25,    26,    27,
    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    38,    39,    40,    41,    42,    43,    -1,    -1,    46,    47,
    48,    49,    50,    13,    -1,    -1,    -1,    -1,    -1,    57,
    -1,    -1,    22,    23,    24,    25,    26,    27,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,
    40,    41,    42,    43,    -1,    -1,    46,    47,    48,    49,
    50,    13,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
    22,    23,    24,    25,    26,    27,    -1,    -1,    -1,    13,
    -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,    41,
    42,    43,    -1,    -1,    46,    47,    48,    49,    50,    -1,
    -1,    -1,    -1,    -1,    -1,    57,    40,    41,    42,    43,
    -1,    -1,    46,    47,    48,    49,    50,    -1,    -1,    -1,
    -1,    -1,    -1,    57
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 1:
#line 289 "./gram.y"
{ return 0; ;
    break;}
case 2:
#line 290 "./gram.y"
{ return xxvalue(NULL,2); ;
    break;}
case 3:
#line 291 "./gram.y"
{ return xxvalue(yyvsp[-1],3); ;
    break;}
case 4:
#line 292 "./gram.y"
{ return xxvalue(yyvsp[-1],4); ;
    break;}
case 5:
#line 293 "./gram.y"
{ YYABORT; ;
    break;}
case 6:
#line 296 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 7:
#line 297 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 8:
#line 300 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 9:
#line 303 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 10:
#line 304 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 11:
#line 305 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 12:
#line 306 "./gram.y"
{ yyval = yyvsp[0]; ;
    break;}
case 13:
#line 308 "./gram.y"
{ yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); ;
    break;}
case 14:
#line 309 "./gram.y"
{ yyval = xxparen(yyvsp[-2],yyvsp[-1]); ;
    break;}
case 15:
#line 311 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); ;
    break;}
case 16:
#line 312 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); ;
    break;}
case 17:
#line 313 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); ;
    break;}
case 18:
#line 314 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); ;
    break;}
case 19:
#line 315 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); ;
    break;}
case 20:
#line 317 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 21:
#line 318 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 22:
#line 319 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 23:
#line 320 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 24:
#line 321 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 25:
#line 322 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 26:
#line 323 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 27:
#line 324 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 28:
#line 325 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 29:
#line 326 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 30:
#line 327 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 31:
#line 328 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 32:
#line 329 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 33:
#line 330 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 34:
#line 331 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 35:
#line 332 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 36:
#line 333 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 37:
#line 334 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 38:
#line 336 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 39:
#line 337 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); ;
    break;}
case 40:
#line 339 "./gram.y"
{ yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); ;
    break;}
case 41:
#line 340 "./gram.y"
{ yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); ;
    break;}
case 42:
#line 341 "./gram.y"
{ yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); ;
    break;}
case 43:
#line 342 "./gram.y"
{ yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); ;
    break;}
case 44:
#line 343 "./gram.y"
{ yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); ;
    break;}
case 45:
#line 344 "./gram.y"
{ yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); ;
    break;}
case 46:
#line 345 "./gram.y"
{ yyval = xxrepeat(yyvsp[-1],yyvsp[0]); ;
    break;}
case 47:
#line 346 "./gram.y"
{ yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); ;
    break;}
case 48:
#line 347 "./gram.y"
{ yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); ;
    break;}
case 49:
#line 348 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 50:
#line 349 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 51:
#line 350 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 52:
#line 351 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 53:
#line 352 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 54:
#line 353 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 55:
#line 354 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 56:
#line 355 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 57:
#line 356 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 58:
#line 357 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 59:
#line 358 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 60:
#line 359 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); ;
    break;}
case 61:
#line 360 "./gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); ;
    break;}
case 62:
#line 361 "./gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); ;
    break;}
case 63:
#line 365 "./gram.y"
{ yyval = xxcond(yyvsp[-1]); ;
    break;}
case 64:
#line 368 "./gram.y"
{ yyval = xxifcond(yyvsp[-1]); ;
    break;}
case 65:
#line 371 "./gram.y"
{ yyval = xxforcond(yyvsp[-3],yyvsp[-1]); ;
    break;}
case 66:
#line 375 "./gram.y"
{ yyval = xxexprlist0(); ;
    break;}
case 67:
#line 376 "./gram.y"
{ yyval = xxexprlist1(yyvsp[0]); ;
    break;}
case 68:
#line 377 "./gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); ;
    break;}
case 69:
#line 378 "./gram.y"
{ yyval = yyvsp[-1]; ;
    break;}
case 70:
#line 379 "./gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); ;
    break;}
case 71:
#line 380 "./gram.y"
{ yyval = yyvsp[-1];;
    break;}
case 72:
#line 383 "./gram.y"
{ yyval = xxsublist1(yyvsp[0]); ;
    break;}
case 73:
#line 384 "./gram.y"
{ yyval = xxsublist2(yyvsp[-3],yyvsp[0]); ;
    break;}
case 74:
#line 387 "./gram.y"
{ yyval = xxsub0(); ;
    break;}
case 75:
#line 388 "./gram.y"
{ yyval = xxsub1(yyvsp[0]); ;
    break;}
case 76:
#line 389 "./gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); ;
    break;}
case 77:
#line 390 "./gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); ;
    break;}
case 78:
#line 391 "./gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); ;
    break;}
case 79:
#line 392 "./gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); ;
    break;}
case 80:
#line 393 "./gram.y"
{ yyval = xxnullsub0(); ;
    break;}
case 81:
#line 394 "./gram.y"
{ yyval = xxnullsub1(yyvsp[0]); ;
    break;}
case 82:
#line 397 "./gram.y"
{ yyval = xxnullformal(); ;
    break;}
case 83:
#line 398 "./gram.y"
{ yyval = xxfirstformal0(yyvsp[0]); ;
    break;}
case 84:
#line 399 "./gram.y"
{ yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); ;
    break;}
case 85:
#line 400 "./gram.y"
{ yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); ;
    break;}
case 86:
#line 401 "./gram.y"
{ yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); ;
    break;}
case 87:
#line 404 "./gram.y"
{ EatLines = 1; ;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 406 "./gram.y"



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
    
    if (c == '\n') R_ParseError += 1;
    if ( KeepSource && GenerateCode && FunctionLevel > 0 ) {
	if(SourcePtr <  FunctionSource + MAXFUNSIZE)
	    *SourcePtr++ = c;
	else  error(_("function is too long to keep source"));
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
    R_ParseContext[R_ParseContextLast--] = '\0';
    R_ParseContextLast = R_ParseContextLast % PARSE_CONTEXT_SIZE;
    if(npush >= 16) return EOF;
    pushback[npush++] = c;
    return c;
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

static SEXP makeSrcref(int start, int length, SEXP srcfile)
{
    SEXP result;
    
    PROTECT(result = allocVector(INTSXP, 2));
    INTEGER(result)[0] = start;
    INTEGER(result)[1] = length;
    setAttrib(result, R_SrcfileSymbol, srcfile);
    setAttrib(result, R_ClassSymbol,  ScalarString(mkChar("srcref")));
    UNPROTECT(1);
    return result;
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
	setAttrib(expr, R_SrcrefSymbol, 
	   makeSrcref(thisExprStart, lastExprEnd-thisExprStart,  SrcFile));
	thisExprStart = conThisChar;
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
    if (GenerateCode) {
	setAttrib(expr, R_SrcrefSymbol, 
	   makeSrcref(thisExprStart, lastExprEnd-thisExprStart,  SrcFile));
	thisExprStart = conThisChar;    
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
			    error(("unable to allocate space for source line"));
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
	error(_("incorrect tag type")); return R_NilValue/* -Wall */;
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
 *	SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status)
 *
 *	SEXP R_ParseVector(SEXP *text, int n, ParseStatus *status)
 *
 *	SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt, SEXP srcfile)
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
    KeepSource = *LOGICAL(GetOption(install("keep.source"), R_BaseEnv));
    npush = 0;
    conPrevChar = 0;
    conThisChar = 0;
    lastExprEnd = 0;
    thisExprStart = 0;
}

static void ParseContextInit()
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

static int buffer_getc()
{
    return R_IoBufferGetc(iob);
}

/* Used only in main.c, rproxy_impl.c  and this file */
attribute_hidden
SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, ParseStatus *status)
{
    ParseInit();
    ParseContextInit();
    GenerateCode = gencode;
    iob = buffer;
    ptr_getc = buffer_getc;
    R_Parse1(status);
    return R_CurrentExpr;
}

static TextBuffer *txtb;

static int text_getc()
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

static SEXP R_Parse(int n, ParseStatus *status)
{
    volatile int savestack;
    int i;
    SEXP t, rval;

    ParseContextInit();
    savestack = R_PPStackTop;
    PROTECT(t = NewList());
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
    R_PPStackTop = savestack;
    *status = PARSE_OK;
    return rval;
}

/* used in edit.c */
attribute_hidden
SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    fp_parse = fp;
    ptr_getc = file_getc;
    return R_Parse(n, status);
}

#include "Rconnections.h"
static Rconnection con_parse;

/* need to handle incomplete last line */
static int con_getc(void)
{
    int c;
    static int last=-1000;
    
    conPrevChar = conThisChar;
    c = Rconn_fgetc(con_parse);
    if (con_parse->canseek) 
	conThisChar = con_parse->seek(con_parse, 0, 2, 1); /* 0, "current", "read" */
    if (c == EOF && last != '\n') c = '\n';
    return (last = c);
}

/* used in source.c */
attribute_hidden
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    con_parse = con;;
    ptr_getc = con_getc;
    return R_Parse(n, status);
}

/* This one is public, and used in source.c */
SEXP R_ParseVector(SEXP text, int n, ParseStatus *status)
{
    SEXP rval;
    TextBuffer textb;
    R_TextBufferInit(&textb, text);
    txtb = &textb;
    GenerateCode = 1;
    R_ParseError = 1;
    ptr_getc = text_getc;
    rval = R_Parse(n, status);
    R_TextBufferFree(&textb);
    return rval;
}

#ifdef PARSE_UNUSED
/* Not used, and note ungetc is no longer needed */
SEXP R_ParseGeneral(int (*ggetc)(), int (*gungetc)(), int n,
		    ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    ptr_getc = ggetc;
    return R_Parse(n, status);
}
#endif

static char *Prompt(SEXP prompt, int type)
{
    if(type == 1) {
	if(length(prompt) <= 0) {
	    return (char*)CHAR(STRING_ELT(GetOption(install("prompt"),
						    R_BaseEnv), 0));
	}
	else
	    return CHAR(STRING_ELT(prompt, 0));
    }
    else {
	return (char*)CHAR(STRING_ELT(GetOption(install("continue"),
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
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;
	if (!*bufp) {
	    if(R_ReadConsole(Prompt(prompt, prompt_type),
			     (unsigned char *)buf, 1024, 1) == 0)
		goto finish;
	    bufp = buf;
	}
	while ((c = *bufp++)) {
	    R_IoBufferPutc(c, buffer);
	    if (c == ';' || c == '\n') break;
	}
	
	SrcFile = srcfile;
	rval = R_Parse1Buffer(buffer, 1, status);
	SrcFile = R_NilValue;
	
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
	if(contextp - contextstack >=50) error("contextstack overflow");
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


static SEXP mkFloat(char *s)
{
    SEXP t = allocVector(REALSXP, 1);
    if(strlen(s) > 2 && (s[1] == 'x' || s[1] == 'X')) {
	double ret = 0; char *p = s + 2;
	for(; p; p++) {
	    if('0' <= *p && *p <= '9') ret = 16*ret + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ret = 16*ret + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ret = 16*ret + (*p -'A' + 10);
	    else break;
	}	
	REAL(t)[0] = ret;
    } else REAL(t)[0] = atof(s);
    return t;
}

static SEXP mkComplex(char *s)
{
    SEXP t = allocVector(CPLXSXP, 1);
    COMPLEX(t)[0].r = 0;
    COMPLEX(t)[0].i = atof(s);
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
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
    while (formlist != R_NilValue) {
	if (TAG(formlist) == new) {
	    error(_("Repeated formal argument"));
	}
	formlist = CDR(formlist);
    }
}

static char yytext[MAXELTSIZE];

#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext >= sizeof(yytext) - 1) \
        error(_("input buffer overflow")); \
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
    int last = c;
    int nd = 0;
    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E' 
	   || c == 'x' || c == 'X') 
    {
	if (c == 'x' || c == 'X') {
	    if (last != '0') break;
	    YYTEXT_PUSH(c, yyp);
	    while(isdigit(c = xxgetc()) || ('a' <= c && c <= 'f') ||
		  ('A' <= c && c <= 'F')) {
		YYTEXT_PUSH(c, yyp);
		nd++;
	    }
	    if(nd == 0) return ERROR;
	    break;
	}
	if (c == 'E' || c == 'e') {
	    if (seenexp)
		break;
	    seenexp = 1;
	    seendot = 1;
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
	    else if(c == 'x') {
		int val = 0; int i, ext;
		for(i = 0; i < 2; i++) {
		    c = xxgetc();
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); break;}
		    val = 16*val + ext;
		}
		c = val;
	    }
	    else if(c == 'u') {
#ifndef SUPPORT_MBCS
		error(_("\\uxxxx sequences not supported"));
#else
		wint_t val = 0; int i, ext; size_t res;
		char buff[16]; Rboolean delim = FALSE;
		if((c = xxgetc()) == '{') delim = TRUE; else xxungetc(c);
		for(i = 0; i < 4; i++) {
		    c = xxgetc();
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); break;}
		    val = 16*val + ext;
		}
		if(delim)
		    if((c = xxgetc()) != '}')
			error(_("invalid \\u{xxxx} sequence"));
		
		res = ucstomb(buff, val, NULL);
		if((int)res <= 0) {
		    if(delim)
			error(_("invalid \\u{xxxx} sequence"));
		    else
			error(_("invalid \\uxxxx sequence"));
		}
		for(i = 0; i <  res - 1; i++) YYTEXT_PUSH(buff[i], yyp);
		c = buff[res - 1]; /* pushed below */
#endif
	    }
	    else if(c == 'U') {
#ifdef Win32
		error(_("\\Uxxxxxxxx sequences are not supported on Windows"));
#else
		if(!mbcslocale) 
		     error(_("\\Uxxxxxxxx sequences are only valid in multibyte locales"));
#ifdef SUPPORT_MBCS
		else {
		    wint_t val = 0; int i, ext; size_t res;
		    char buff[16]; Rboolean delim = FALSE;
		    if((c = xxgetc()) == '{') delim = TRUE; else xxungetc(c);
		    for(i = 0; i < 8; i++) {
			c = xxgetc();
			if(c >= '0' && c <= '9') ext = c - '0';
			else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
			else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
			else {xxungetc(c); break;}
			val = 16*val + ext;
		    }
		    if(delim)
			if((c = xxgetc()) != '}')
			    error(_("invalid \\U{xxxxxxxx} sequence"));
		    res = ucstomb(buff, val, NULL);
		    if((int)res <= 0) {
			if(delim)
			    error(_("invalid \\U{xxxxxxxx} sequence"));
			else
			    error(("invalid \\Uxxxxxxxx sequence"));
		    }
		    for(i = 0; i <  res - 1; i++) YYTEXT_PUSH(buff[i], yyp);
		    c = buff[res - 1]; /* pushed below */
		}
#endif
#endif /* Win32 */
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
		case '%':
		    warning(_("'\\%%%%' is an unrecognized escape in a character string"));
		    break;
		default:
		    warning(_("'\\%c' is an unrecognized escape in a character string"), c);

		    break;
		}
	    }
	}
#if defined(SUPPORT_MBCS)
       else if(mbcslocale) {
           int i, clen;
           wchar_t wc = L'\0';
           clen = utf8locale ? utf8clen(c): mbcs_get_next(c, &wc);
           for(i = 0; i < clen - 1; i++){
               YYTEXT_PUSH(c,yyp);
               c = xxgetc();
               if (c == R_EOF) break;
               if (c == '\n') {
                   xxungetc(c);
                   c = '\\';
               }
           }
           if (c == R_EOF) break;
       }
#endif /* SUPPORT_MBCS */
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

/* return 1 if name is a valid name 0 otherwise */
int isValidName(char *name)
{
    char *p = name;
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
		error(_("functions nested too deeply in source code"));
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
    int c;
#if defined(SUPPORT_MBCS)
    wchar_t wc;
#endif

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
    if (c == R_EOF) {
      lastExprEnd = conPrevChar;
      return END_OF_INPUT;
    }
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
	return StringValue(c);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return QuotedSymbolValue(c);
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
    case ';':
    case '\n':
        lastExprEnd = conPrevChar;
        return c;
    default:
	return c;
    }
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
	if(contextp - contextstack >=49) error("contextstack overflow");
	*++contextp = '[';
	*++contextp = '[';
	break;

    case '[':
	if(contextp - contextstack >=50) error("contextstack overflow");
	*++contextp = tok;
	break;

    case LBRACE:
	if(contextp - contextstack >=50) error("contextstack overflow");
	*++contextp = tok;
	EatLines = 1;
	break;

    case '(':
	if(contextp - contextstack >=50) error("contextstack overflow");
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
