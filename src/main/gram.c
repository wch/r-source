/* A Bison parser, made from ./gram.y
   by GNU bison 1.35.  */

#define YYBISON 1  /* Identify Bison output.  */

# define	END_OF_INPUT	257
# define	ERROR	258
# define	STR_CONST	259
# define	NUM_CONST	260
# define	NULL_CONST	261
# define	SYMBOL	262
# define	FUNCTION	263
# define	LEFT_ASSIGN	264
# define	EQ_ASSIGN	265
# define	RIGHT_ASSIGN	266
# define	LBB	267
# define	FOR	268
# define	IN	269
# define	IF	270
# define	ELSE	271
# define	WHILE	272
# define	NEXT	273
# define	BREAK	274
# define	REPEAT	275
# define	GT	276
# define	GE	277
# define	LT	278
# define	LE	279
# define	EQ	280
# define	NE	281
# define	AND	282
# define	OR	283
# define	NS_GET	284
# define	NS_GET_INT	285
# define	LOW	286
# define	TILDE	287
# define	UNOT	288
# define	NOT	289
# define	SPECIAL	290
# define	UMINUS	291
# define	UPLUS	292

#line 1 "./gram.y"

/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  Robert Gentleman, Ross Ihaka and the
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

#ifndef YYSTYPE
# define YYSTYPE int
# define YYSTYPE_IS_TRIVIAL 1
#endif
#ifndef YYDEBUG
# define YYDEBUG 0
#endif



#define	YYFINAL		159
#define	YYFLAG		-32768
#define	YYNTBASE	60

/* YYTRANSLATE(YYLEX) -- Bison token number corresponding to YYLEX. */
#define YYTRANSLATE(x) ((unsigned)(x) <= 292 ? yytranslate[x] : 72)

/* YYTRANSLATE[YYLEX] -- Bison token number corresponding to YYLEX. */
static const char yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      51,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    56,     2,     2,    47,    57,     2,     2,
      49,    55,    40,    38,    59,    39,     2,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    43,    52,
       2,     2,     2,    32,    48,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    50,     2,    58,    46,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    53,     2,    54,    34,     2,     2,     2,
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
       2,     2,     2,     2,     2,     2,     1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    33,    35,    36,    37,
      42,    44,    45
};

#if YYDEBUG
static const short yyprhs[] =
{
       0,     0,     2,     4,     7,    10,    12,    14,    16,    20,
      22,    24,    26,    28,    32,    36,    39,    42,    45,    48,
      51,    55,    59,    63,    67,    71,    75,    79,    83,    87,
      91,    95,    99,   103,   107,   111,   115,   119,   123,   127,
     131,   138,   143,   147,   153,   157,   161,   164,   170,   175,
     179,   183,   187,   191,   195,   199,   203,   207,   211,   215,
     219,   223,   225,   227,   231,   235,   241,   242,   244,   248,
     251,   255,   258,   260,   265,   266,   268,   271,   275,   278,
     282,   285,   289,   290,   292,   296,   300,   306
};
static const short yyrhs[] =
{
       3,     0,    51,     0,    61,    51,     0,    61,    52,     0,
       1,     0,    63,     0,    62,     0,    63,    11,    61,     0,
       6,     0,     5,     0,     7,     0,     8,     0,    53,    67,
      54,     0,    49,    61,    55,     0,    39,    63,     0,    38,
      63,     0,    56,    63,     0,    34,    63,     0,    32,    63,
       0,    63,    43,    63,     0,    63,    38,    63,     0,    63,
      39,    63,     0,    63,    40,    63,     0,    63,    41,    63,
       0,    63,    46,    63,     0,    63,    42,    63,     0,    63,
      57,    63,     0,    63,    34,    63,     0,    63,    32,    63,
       0,    63,    24,    63,     0,    63,    25,    63,     0,    63,
      26,    63,     0,    63,    27,    63,     0,    63,    23,    63,
       0,    63,    22,    63,     0,    63,    28,    63,     0,    63,
      29,    63,     0,    63,    10,    63,     0,    63,    12,    63,
       0,     9,    49,    70,    55,    71,    61,     0,    63,    49,
      68,    55,     0,    16,    65,    61,     0,    16,    65,    61,
      17,    61,     0,    14,    66,    61,     0,    18,    64,    61,
       0,    21,    61,     0,    63,    13,    68,    58,    58,     0,
      63,    50,    68,    58,     0,     8,    30,     8,     0,     8,
      30,     5,     0,     5,    30,     8,     0,     5,    30,     5,
       0,     8,    31,     8,     0,     8,    31,     5,     0,     5,
      31,     8,     0,     5,    31,     5,     0,    63,    47,     8,
       0,    63,    47,     5,     0,    63,    48,     8,     0,    63,
      48,     5,     0,    19,     0,    20,     0,    49,    63,    55,
       0,    49,    63,    55,     0,    49,     8,    15,    63,    55,
       0,     0,    61,     0,    67,    52,    61,     0,    67,    52,
       0,    67,    51,    61,     0,    67,    51,     0,    69,     0,
      68,    71,    59,    69,     0,     0,    63,     0,     8,    11,
       0,     8,    11,    63,     0,     5,    11,     0,     5,    11,
      63,     0,     7,    11,     0,     7,    11,    63,     0,     0,
       8,     0,     8,    11,    63,     0,    70,    59,     8,     0,
      70,    59,     8,    11,    63,     0,     0
};

#endif

#if YYDEBUG
/* YYRLINE[YYN] -- source line where rule number YYN was defined. */
static const short yyrline[] =
{
       0,   164,   165,   166,   167,   168,   171,   172,   175,   178,
     179,   180,   181,   183,   184,   186,   187,   188,   189,   190,
     192,   193,   194,   195,   196,   197,   198,   199,   200,   201,
     202,   203,   204,   205,   206,   207,   208,   209,   211,   212,
     213,   215,   216,   217,   218,   219,   220,   221,   222,   223,
     224,   225,   226,   227,   228,   229,   230,   231,   232,   233,
     234,   235,   236,   240,   243,   246,   250,   251,   252,   253,
     254,   255,   258,   259,   262,   263,   264,   265,   266,   267,
     268,   269,   272,   273,   274,   275,   276,   279
};
#endif


#if (YYDEBUG) || defined YYERROR_VERBOSE

/* YYTNAME[TOKEN_NUM] -- String name of the token TOKEN_NUM. */
static const char *const yytname[] =
{
  "$", "error", "$undefined.", "END_OF_INPUT", "ERROR", "STR_CONST", 
  "NUM_CONST", "NULL_CONST", "SYMBOL", "FUNCTION", "LEFT_ASSIGN", 
  "EQ_ASSIGN", "RIGHT_ASSIGN", "LBB", "FOR", "IN", "IF", "ELSE", "WHILE", 
  "NEXT", "BREAK", "REPEAT", "GT", "GE", "LT", "LE", "EQ", "NE", "AND", 
  "OR", "NS_GET", "NS_GET_INT", "'?'", "LOW", "'~'", "TILDE", "UNOT", 
  "NOT", "'+'", "'-'", "'*'", "'/'", "SPECIAL", "':'", "UMINUS", "UPLUS", 
  "'^'", "'$'", "'@'", "'('", "'['", "'\\n'", "';'", "'{'", "'}'", "')'", 
  "'!'", "'%'", "']'", "','", "prog", "expr_or_assign", "equal_assign", 
  "expr", "cond", "ifcond", "forcond", "exprlist", "sublist", "sub", 
  "formlist", "cr", 0
};
#endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives. */
static const short yyr1[] =
{
       0,    60,    60,    60,    60,    60,    61,    61,    62,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    63,    63,    63,    63,    63,    63,    63,
      63,    63,    63,    64,    65,    66,    67,    67,    67,    67,
      67,    67,    68,    68,    69,    69,    69,    69,    69,    69,
      69,    69,    70,    70,    70,    70,    70,    71
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN. */
static const short yyr2[] =
{
       0,     1,     1,     2,     2,     1,     1,     1,     3,     1,
       1,     1,     1,     3,     3,     2,     2,     2,     2,     2,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       6,     4,     3,     5,     3,     3,     2,     5,     4,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     1,     1,     3,     3,     5,     0,     1,     3,     2,
       3,     2,     1,     4,     0,     1,     2,     3,     2,     3,
       2,     3,     0,     1,     3,     3,     5,     0
};

/* YYDEFACT[S] -- default rule to reduce with in state S when YYTABLE
   doesn't specify something else to do.  Zero means the default is an
   error. */
static const short yydefact[] =
{
       0,     5,     1,    10,     9,    11,    12,     0,     0,     0,
       0,    61,    62,     0,     0,     0,     0,     0,     0,     2,
      66,     0,     0,     7,     6,     0,     0,     0,     0,    82,
       0,     0,     0,     0,     0,     0,    46,    19,    18,    16,
      15,     0,    67,     0,    17,     3,     4,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    74,     0,    52,    51,    56,    55,    50,    49,    54,
      53,    83,     0,     0,    44,     0,    42,     0,    45,    14,
      71,    69,    13,    38,     8,    39,    10,    11,    12,    75,
      87,    72,    35,    34,    30,    31,    32,    33,    36,    37,
      29,    28,    21,    22,    23,    24,    26,    20,    25,    58,
      57,    60,    59,    87,    87,    27,     0,    87,     0,     0,
      64,     0,    63,    70,    68,    78,    80,    76,     0,     0,
      41,    48,    84,     0,    85,     0,    43,    79,    81,    77,
      47,    74,    40,     0,    65,    73,    86,     0,     0,     0
};

static const short yydefgoto[] =
{
     157,    22,    23,    24,    35,    33,    31,    43,   100,   101,
      82,   139
};

static const short yypact[] =
{
     137,-32768,-32768,    -9,-32768,-32768,    32,   -15,     6,    29,
      34,-32768,-32768,    61,    61,    61,    61,    61,    61,-32768,
      61,    61,    13,-32768,   224,     7,     9,    11,    22,     0,
      52,    61,    61,    61,    61,    61,-32768,   429,   505,    78,
      78,    16,-32768,   -41,   581,-32768,-32768,    61,    61,    61,
     189,    61,    61,    61,    61,    61,    61,    61,    61,    61,
      61,    61,    61,    61,    61,    61,    61,    61,    23,    24,
     189,   189,    61,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -32768,    62,   -50,    71,-32768,   265,    70,   306,-32768,-32768,
      61,    61,-32768,   429,-32768,   467,    -7,    77,    -5,   388,
      36,-32768,   601,   601,   601,   601,   601,   601,   581,   543,
     429,   505,   613,   613,   117,   117,   168,    78,    78,-32768,
  -32768,-32768,-32768,    35,    38,   388,    61,-32768,    84,    61,
  -32768,    61,-32768,-32768,-32768,    61,    61,    61,    39,    42,
  -32768,-32768,   388,    61,    87,   347,-32768,   388,   388,   388,
  -32768,   189,-32768,    61,-32768,-32768,   388,   102,   103,-32768
};

static const short yypgoto[] =
{
  -32768,    41,-32768,   -14,-32768,-32768,-32768,-32768,    14,   -47,
  -32768,   -22
};


#define	YYLAST		670


static const short yytable[] =
{
      37,    38,    39,    40,   135,   127,   137,    44,    81,   128,
      90,    91,    73,    92,    75,    74,    77,    76,    85,    78,
      87,    25,    26,    25,    26,    27,    28,    79,   119,   121,
      80,   120,   122,    93,    29,    95,    99,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,    36,    30,    99,    99,   125,    41,
      83,    42,    27,    28,    45,    46,     3,     4,     5,     6,
       7,    89,    84,   126,    86,     8,    88,     9,    32,    10,
      11,    12,    13,    34,   123,   124,   129,   131,   136,    94,
     140,    50,   144,    14,   138,    15,   141,   150,   153,    16,
      17,   151,   158,   159,   155,   143,     0,     0,     0,     0,
      18,     0,   142,     0,    20,   145,     0,    21,     0,     0,
       0,   147,   148,   149,    67,    68,    69,    70,    71,     0,
      50,   133,   134,     0,     0,    72,     0,    99,     1,   156,
       2,     0,     3,     4,     5,     6,     7,     0,     0,     0,
       0,     8,     0,     9,     0,    10,    11,    12,    13,    65,
      66,     0,     0,    67,    68,    69,    70,    71,     0,    14,
       0,    15,   146,     0,    72,    16,    17,     0,     0,     0,
       0,    50,     0,     0,   152,     0,    18,     0,    19,     0,
      20,     0,     0,    21,    96,     4,    97,    98,     7,     0,
       0,     0,     0,     8,     0,     9,     0,    10,    11,    12,
      13,    66,     0,     0,    67,    68,    69,    70,    71,     0,
       0,    14,     0,    15,     0,    72,     0,    16,    17,     0,
       0,     0,     0,     0,    47,    48,    49,    50,    18,     0,
       0,     0,    20,     0,     0,    21,    51,    52,    53,    54,
      55,    56,    57,    58,     0,     0,    59,     0,    60,     0,
       0,     0,    61,    62,    63,    64,    65,    66,     0,     0,
      67,    68,    69,    70,    71,    47,     0,    49,    50,     0,
       0,    72,     0,     0,     0,     0,     0,    51,    52,    53,
      54,    55,    56,    57,    58,     0,     0,    59,     0,    60,
       0,     0,     0,    61,    62,    63,    64,    65,    66,     0,
       0,    67,    68,    69,    70,    71,    47,     0,    49,    50,
     130,     0,    72,     0,     0,     0,     0,     0,    51,    52,
      53,    54,    55,    56,    57,    58,     0,     0,    59,     0,
      60,     0,     0,     0,    61,    62,    63,    64,    65,    66,
       0,     0,    67,    68,    69,    70,    71,    47,     0,    49,
      50,   132,     0,    72,     0,     0,     0,     0,     0,    51,
      52,    53,    54,    55,    56,    57,    58,     0,     0,    59,
       0,    60,     0,     0,     0,    61,    62,    63,    64,    65,
      66,     0,     0,    67,    68,    69,    70,    71,    47,     0,
      49,    50,   154,     0,    72,     0,     0,     0,     0,     0,
      51,    52,    53,    54,    55,    56,    57,    58,     0,     0,
      59,     0,    60,     0,     0,     0,    61,    62,    63,    64,
      65,    66,     0,     0,    67,    68,    69,    70,    71,    47,
       0,    49,    50,     0,     0,    72,     0,     0,     0,     0,
       0,    51,    52,    53,    54,    55,    56,    57,    58,     0,
       0,     0,     0,    60,     0,     0,     0,    61,    62,    63,
      64,    65,    66,     0,     0,    67,    68,    69,    70,    71,
      50,     0,     0,     0,     0,     0,    72,     0,     0,    51,
      52,    53,    54,    55,    56,    57,    58,     0,     0,     0,
       0,    60,     0,     0,     0,    61,    62,    63,    64,    65,
      66,     0,     0,    67,    68,    69,    70,    71,    50,     0,
       0,     0,     0,     0,    72,     0,     0,    51,    52,    53,
      54,    55,    56,    57,    58,     0,     0,     0,     0,     0,
       0,     0,     0,    61,    62,    63,    64,    65,    66,     0,
       0,    67,    68,    69,    70,    71,    50,     0,     0,     0,
       0,     0,    72,     0,     0,    51,    52,    53,    54,    55,
      56,    57,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    61,    62,    63,    64,    65,    66,     0,     0,    67,
      68,    69,    70,    71,    50,     0,     0,     0,     0,     0,
      72,     0,     0,    51,    52,    53,    54,    55,    56,     0,
       0,     0,     0,     0,    50,     0,     0,     0,     0,    61,
      62,    63,    64,    65,    66,     0,    50,    67,    68,    69,
      70,    71,     0,     0,     0,     0,     0,     0,    72,    61,
      62,    63,    64,    65,    66,     0,     0,    67,    68,    69,
      70,    71,     0,    63,    64,    65,    66,     0,    72,    67,
      68,    69,    70,    71,     0,     0,     0,     0,     0,     0,
      72
};

static const short yycheck[] =
{
      14,    15,    16,    17,    11,    55,    11,    21,     8,    59,
      51,    52,     5,    54,     5,     8,     5,     8,    32,     8,
      34,    30,    31,    30,    31,    30,    31,     5,     5,     5,
       8,     8,     8,    47,    49,    49,    50,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    13,    49,    70,    71,    72,    18,
       8,    20,    30,    31,    51,    52,     5,     6,     7,     8,
       9,    55,    31,    11,    33,    14,    35,    16,    49,    18,
      19,    20,    21,    49,    70,    71,    15,    17,    11,    48,
      55,    13,     8,    32,    58,    34,    58,    58,    11,    38,
      39,    59,     0,     0,   151,   127,    -1,    -1,    -1,    -1,
      49,    -1,   126,    -1,    53,   129,    -1,    56,    -1,    -1,
      -1,   135,   136,   137,    46,    47,    48,    49,    50,    -1,
      13,    90,    91,    -1,    -1,    57,    -1,   151,     1,   153,
       3,    -1,     5,     6,     7,     8,     9,    -1,    -1,    -1,
      -1,    14,    -1,    16,    -1,    18,    19,    20,    21,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    -1,    32,
      -1,    34,   131,    -1,    57,    38,    39,    -1,    -1,    -1,
      -1,    13,    -1,    -1,   143,    -1,    49,    -1,    51,    -1,
      53,    -1,    -1,    56,     5,     6,     7,     8,     9,    -1,
      -1,    -1,    -1,    14,    -1,    16,    -1,    18,    19,    20,
      21,    43,    -1,    -1,    46,    47,    48,    49,    50,    -1,
      -1,    32,    -1,    34,    -1,    57,    -1,    38,    39,    -1,
      -1,    -1,    -1,    -1,    10,    11,    12,    13,    49,    -1,
      -1,    -1,    53,    -1,    -1,    56,    22,    23,    24,    25,
      26,    27,    28,    29,    -1,    -1,    32,    -1,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,    47,    48,    49,    50,    10,    -1,    12,    13,    -1,
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
      12,    13,    55,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
      32,    -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    47,    48,    49,    50,    10,
      -1,    12,    13,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    -1,
      -1,    -1,    -1,    34,    -1,    -1,    -1,    38,    39,    40,
      41,    42,    43,    -1,    -1,    46,    47,    48,    49,    50,
      13,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    -1,    -1,    -1,
      -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    13,    -1,
      -1,    -1,    -1,    -1,    57,    -1,    -1,    22,    23,    24,
      25,    26,    27,    28,    29,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    47,    48,    49,    50,    13,    -1,    -1,    -1,
      -1,    -1,    57,    -1,    -1,    22,    23,    24,    25,    26,
      27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    39,    40,    41,    42,    43,    -1,    -1,    46,
      47,    48,    49,    50,    13,    -1,    -1,    -1,    -1,    -1,
      57,    -1,    -1,    22,    23,    24,    25,    26,    27,    -1,
      -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,    -1,    38,
      39,    40,    41,    42,    43,    -1,    13,    46,    47,    48,
      49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    57,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    47,    48,
      49,    50,    -1,    40,    41,    42,    43,    -1,    57,    46,
      47,    48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,
      57
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison/bison.simple"

/* Skeleton output parser for bison,

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002 Free Software
   Foundation, Inc.

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

/* This is the parser code that is written into each bison parser when
   the %semantic_parser declaration is not specified in the grammar.
   It was written by Richard Stallman by simplifying the hairy parser
   used when %semantic_parser is specified.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

#if ! defined (yyoverflow) || defined (YYERROR_VERBOSE)

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# if YYSTACK_USE_ALLOCA
#  define YYSTACK_ALLOC alloca
# else
#  ifndef YYSTACK_USE_ALLOCA
#   if defined (alloca) || defined (_ALLOCA_H)
#    define YYSTACK_ALLOC alloca
#   else
#    ifdef __GNUC__
#     define YYSTACK_ALLOC __builtin_alloca
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning. */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
# else
#  if defined (__STDC__) || defined (__cplusplus)
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   define YYSIZE_T size_t
#  endif
#  define YYSTACK_ALLOC malloc
#  define YYSTACK_FREE free
# endif
#endif /* ! defined (yyoverflow) || defined (YYERROR_VERBOSE) */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (YYLTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
# if YYLSP_NEEDED
  YYLTYPE yyls;
# endif
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAX (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# if YYLSP_NEEDED
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE) + sizeof (YYLTYPE))	\
      + 2 * YYSTACK_GAP_MAX)
# else
#  define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAX)
# endif

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  register YYSIZE_T yyi;		\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (0)
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAX;	\
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif


#if ! defined (YYSIZE_T) && defined (__SIZE_TYPE__)
# define YYSIZE_T __SIZE_TYPE__
#endif
#if ! defined (YYSIZE_T) && defined (size_t)
# define YYSIZE_T size_t
#endif
#if ! defined (YYSIZE_T)
# if defined (__STDC__) || defined (__cplusplus)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# endif
#endif
#if ! defined (YYSIZE_T)
# define YYSIZE_T unsigned int
#endif

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");			\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).

   When YYLLOC_DEFAULT is run, CURRENT is set the location of the
   first token.  By default, to implement support for ranges, extend
   its range to the last symbol.  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)       	\
   Current.last_line   = Rhs[N].last_line;	\
   Current.last_column = Rhs[N].last_column;
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#if YYPURE
# if YYLSP_NEEDED
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, &yylloc, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval, &yylloc)
#  endif
# else /* !YYLSP_NEEDED */
#  ifdef YYLEX_PARAM
#   define YYLEX		yylex (&yylval, YYLEX_PARAM)
#  else
#   define YYLEX		yylex (&yylval)
#  endif
# endif /* !YYLSP_NEEDED */
#else /* !YYPURE */
# define YYLEX			yylex ()
#endif /* !YYPURE */


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (0)
/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
#endif /* !YYDEBUG */

/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   SIZE_MAX < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#if YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif

#ifdef YYERROR_VERBOSE

# ifndef yystrlen
#  if defined (__GLIBC__) && defined (_STRING_H)
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
#   if defined (__STDC__) || defined (__cplusplus)
yystrlen (const char *yystr)
#   else
yystrlen (yystr)
     const char *yystr;
#   endif
{
  register const char *yys = yystr;

  while (*yys++ != '\0')
    continue;

  return yys - yystr - 1;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined (__GLIBC__) && defined (_STRING_H) && defined (_GNU_SOURCE)
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
#   if defined (__STDC__) || defined (__cplusplus)
yystpcpy (char *yydest, const char *yysrc)
#   else
yystpcpy (yydest, yysrc)
     char *yydest;
     const char *yysrc;
#   endif
{
  register char *yyd = yydest;
  register const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif
#endif

#line 315 "/usr/share/bison/bison.simple"


/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
#  define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL
# else
#  define YYPARSE_PARAM_ARG YYPARSE_PARAM
#  define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
# endif
#else /* !YYPARSE_PARAM */
# define YYPARSE_PARAM_ARG
# define YYPARSE_PARAM_DECL
#endif /* !YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
# ifdef YYPARSE_PARAM
int yyparse (void *);
# else
int yyparse (void);
# endif
#endif

/* YY_DECL_VARIABLES -- depending whether we use a pure parser,
   variables are global, or local to YYPARSE.  */

#define YY_DECL_NON_LSP_VARIABLES			\
/* The lookahead symbol.  */				\
int yychar;						\
							\
/* The semantic value of the lookahead symbol. */	\
YYSTYPE yylval;						\
							\
/* Number of parse errors so far.  */			\
int yynerrs;

#if YYLSP_NEEDED
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES			\
						\
/* Location data for the lookahead symbol.  */	\
YYLTYPE yylloc;
#else
# define YY_DECL_VARIABLES			\
YY_DECL_NON_LSP_VARIABLES
#endif


/* If nonreentrant, generate the variables here. */

#if !YYPURE
YY_DECL_VARIABLES
#endif  /* !YYPURE */

int
yyparse (YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  /* If reentrant, generate the variables here. */
#if YYPURE
  YY_DECL_VARIABLES
#endif  /* !YYPURE */

  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yychar1 = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack. */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;

#if YYLSP_NEEDED
  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
#endif

#if YYLSP_NEEDED
# define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
# define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  YYSIZE_T yystacksize = YYINITDEPTH;


  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
#if YYLSP_NEEDED
  YYLTYPE yyloc;
#endif

  /* When reducing, the number of symbols on the RHS of the reduced
     rule. */
  int yylen;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
#if YYLSP_NEEDED
  yylsp = yyls;
#endif
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed. so pushing a state here evens the stacks.
     */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack. Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	short *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  */
# if YYLSP_NEEDED
	YYLTYPE *yyls1 = yyls;
	/* This used to be a conditional around just the two extra args,
	   but that might be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
# else
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);
# endif
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
# if YYLSP_NEEDED
	YYSTACK_RELOCATE (yyls);
# endif
# undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
#if YYLSP_NEEDED
      yylsp = yyls + yysize - 1;
#endif

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
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
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yychar1 = YYTRANSLATE (yychar);

#if YYDEBUG
     /* We have to keep this `#if YYDEBUG', since we use variables
	which are defined only if `YYDEBUG' is set.  */
      if (yydebug)
	{
	  YYFPRINTF (stderr, "Next token is %d (%s",
		     yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise
	     meaning of a token, for further debugging info.  */
# ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
# endif
	  YYFPRINTF (stderr, ")\n");
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
  YYDPRINTF ((stderr, "Shifting token %d (%s), ",
	      yychar, yytname[yychar1]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  yystate = yyn;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to the semantic value of
     the lookahead token.  This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

#if YYLSP_NEEDED
  /* Similarly for the default location.  Let the user run additional
     commands if for instance locations are ranges.  */
  yyloc = yylsp[1-yylen];
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
#endif

#if YYDEBUG
  /* We have to keep this `#if YYDEBUG', since we use variables which
     are defined only if `YYDEBUG' is set.  */
  if (yydebug)
    {
      int yyi;

      YYFPRINTF (stderr, "Reducing via rule %d (line %d), ",
		 yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (yyi = yyprhs[yyn]; yyrhs[yyi] > 0; yyi++)
	YYFPRINTF (stderr, "%s ", yytname[yyrhs[yyi]]);
      YYFPRINTF (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif

  switch (yyn) {

case 1:
#line 164 "./gram.y"
{ return 0; }
    break;
case 2:
#line 165 "./gram.y"
{ return xxvalue(NULL,2); }
    break;
case 3:
#line 166 "./gram.y"
{ return xxvalue(yyvsp[-1],3); }
    break;
case 4:
#line 167 "./gram.y"
{ return xxvalue(yyvsp[-1],4); }
    break;
case 5:
#line 168 "./gram.y"
{ YYABORT; }
    break;
case 6:
#line 171 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 7:
#line 172 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 8:
#line 175 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 9:
#line 178 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 10:
#line 179 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 11:
#line 180 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 12:
#line 181 "./gram.y"
{ yyval = yyvsp[0]; }
    break;
case 13:
#line 183 "./gram.y"
{ yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); }
    break;
case 14:
#line 184 "./gram.y"
{ yyval = xxparen(yyvsp[-2],yyvsp[-1]); }
    break;
case 15:
#line 186 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;
case 16:
#line 187 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;
case 17:
#line 188 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;
case 18:
#line 189 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;
case 19:
#line 190 "./gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;
case 20:
#line 192 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 21:
#line 193 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 22:
#line 194 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 23:
#line 195 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 24:
#line 196 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 25:
#line 197 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 26:
#line 198 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 27:
#line 199 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 28:
#line 200 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 29:
#line 201 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 30:
#line 202 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 31:
#line 203 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 32:
#line 204 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 33:
#line 205 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 34:
#line 206 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 35:
#line 207 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 36:
#line 208 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 37:
#line 209 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 38:
#line 211 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 39:
#line 212 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); }
    break;
case 40:
#line 214 "./gram.y"
{ yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); }
    break;
case 41:
#line 215 "./gram.y"
{ yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); }
    break;
case 42:
#line 216 "./gram.y"
{ yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;
case 43:
#line 217 "./gram.y"
{ yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); }
    break;
case 44:
#line 218 "./gram.y"
{ yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;
case 45:
#line 219 "./gram.y"
{ yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;
case 46:
#line 220 "./gram.y"
{ yyval = xxrepeat(yyvsp[-1],yyvsp[0]); }
    break;
case 47:
#line 221 "./gram.y"
{ yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); }
    break;
case 48:
#line 222 "./gram.y"
{ yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); }
    break;
case 49:
#line 223 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 50:
#line 224 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 51:
#line 225 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 52:
#line 226 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 53:
#line 227 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 54:
#line 228 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 55:
#line 229 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 56:
#line 230 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 57:
#line 231 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 58:
#line 232 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 59:
#line 233 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 60:
#line 234 "./gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;
case 61:
#line 235 "./gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
    break;
case 62:
#line 236 "./gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
    break;
case 63:
#line 240 "./gram.y"
{ yyval = xxcond(yyvsp[-1]); }
    break;
case 64:
#line 243 "./gram.y"
{ yyval = xxifcond(yyvsp[-1]); }
    break;
case 65:
#line 246 "./gram.y"
{ yyval = xxforcond(yyvsp[-3],yyvsp[-1]); }
    break;
case 66:
#line 250 "./gram.y"
{ yyval = xxexprlist0(); }
    break;
case 67:
#line 251 "./gram.y"
{ yyval = xxexprlist1(yyvsp[0]); }
    break;
case 68:
#line 252 "./gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
    break;
case 69:
#line 253 "./gram.y"
{ yyval = yyvsp[-1]; }
    break;
case 70:
#line 254 "./gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
    break;
case 71:
#line 255 "./gram.y"
{ yyval = yyvsp[-1];}
    break;
case 72:
#line 258 "./gram.y"
{ yyval = xxsublist1(yyvsp[0]); }
    break;
case 73:
#line 259 "./gram.y"
{ yyval = xxsublist2(yyvsp[-3],yyvsp[0]); }
    break;
case 74:
#line 262 "./gram.y"
{ yyval = xxsub0(); }
    break;
case 75:
#line 263 "./gram.y"
{ yyval = xxsub1(yyvsp[0]); }
    break;
case 76:
#line 264 "./gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
    break;
case 77:
#line 265 "./gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
    break;
case 78:
#line 266 "./gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
    break;
case 79:
#line 267 "./gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
    break;
case 80:
#line 268 "./gram.y"
{ yyval = xxnullsub0(); }
    break;
case 81:
#line 269 "./gram.y"
{ yyval = xxnullsub1(yyvsp[0]); }
    break;
case 82:
#line 272 "./gram.y"
{ yyval = xxnullformal(); }
    break;
case 83:
#line 273 "./gram.y"
{ yyval = xxfirstformal0(yyvsp[0]); }
    break;
case 84:
#line 274 "./gram.y"
{ yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); }
    break;
case 85:
#line 275 "./gram.y"
{ yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); }
    break;
case 86:
#line 276 "./gram.y"
{ yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); }
    break;
case 87:
#line 279 "./gram.y"
{ EatLines = 1; }
    break;
}

#line 705 "/usr/share/bison/bison.simple"


  yyvsp -= yylen;
  yyssp -= yylen;
#if YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;
#if YYLSP_NEEDED
  *++yylsp = yyloc;
#endif

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  char *yymsg;
	  int yyx, yycount;

	  yycount = 0;
	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  for (yyx = yyn < 0 ? -yyn : 0;
	       yyx < (int) (sizeof (yytname) / sizeof (char *)); yyx++)
	    if (yycheck[yyx + yyn] == yyx)
	      yysize += yystrlen (yytname[yyx]) + 15, yycount++;
	  yysize += yystrlen ("parse error, unexpected ") + 1;
	  yysize += yystrlen (yytname[YYTRANSLATE (yychar)]);
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "parse error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[YYTRANSLATE (yychar)]);

	      if (yycount < 5)
		{
		  yycount = 0;
		  for (yyx = yyn < 0 ? -yyn : 0;
		       yyx < (int) (sizeof (yytname) / sizeof (char *));
		       yyx++)
		    if (yycheck[yyx + yyn] == yyx)
		      {
			const char *yyq = ! yycount ? ", expecting " : " or ";
			yyp = yystpcpy (yyp, yyq);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yycount++;
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exhausted");
	}
      else
#endif /* defined (YYERROR_VERBOSE) */
	yyerror ("parse error");
    }
  goto yyerrlab1;


/*--------------------------------------------------.
| yyerrlab1 -- error raised explicitly by an action |
`--------------------------------------------------*/
yyerrlab1:
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;
      YYDPRINTF ((stderr, "Discarding token %d (%s).\n",
		  yychar, yytname[yychar1]));
      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;


/*-------------------------------------------------------------------.
| yyerrdefault -- current state does not do anything special for the |
| error token.                                                       |
`-------------------------------------------------------------------*/
yyerrdefault:
#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */

  /* If its default is to accept any token, ok.  Otherwise pop it.  */
  yyn = yydefact[yystate];
  if (yyn)
    goto yydefault;
#endif


/*---------------------------------------------------------------.
| yyerrpop -- pop the current state because it cannot handle the |
| error token                                                    |
`---------------------------------------------------------------*/
yyerrpop:
  if (yyssp == yyss)
    YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#if YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG
  if (yydebug)
    {
      short *yyssp1 = yyss - 1;
      YYFPRINTF (stderr, "Error: state stack now");
      while (yyssp1 != yyssp)
	YYFPRINTF (stderr, " %d", *++yyssp1);
      YYFPRINTF (stderr, "\n");
    }
#endif

/*--------------.
| yyerrhandle.  |
`--------------*/
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

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;
#if YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

/*---------------------------------------------.
| yyoverflowab -- parser overflow comes here.  |
`---------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}
#line 281 "./gram.y"



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
	    return ERROR;
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

/* return 1 if name is a valid name 0 otherwise */
int isValidName(char *name)
{
    char *p;
    int c, i;

    p = name;
    c = *p++;

    if( c != '.' && !isalpha(c) )
        return 0;

    if (c == '.' && isdigit((int)*p)) 
	return 0;

    while ( c = *p++, (isalnum(c) || c == '.' 
#ifdef UNDERSCORE_IN_NAMES
		       || c == '_'
#endif
		) )
	;

    if (c != '\0') return 0;

    if (strcmp(name, "...") == 0) 
	return 1;

    for (i = 0; keywords[i].name != NULL; i++)
        if (strcmp(keywords[i].name, name) == 0)
                return 0;
    
    return 1;
}


static int SymbolValue(int c)
{
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
    do {
	YYTEXT_PUSH(c, yyp);
    }
    while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.' 
#ifdef UNDERSCORE_IN_NAMES
				       || c == '_'
#endif
	       ));
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
