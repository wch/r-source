/* A Bison parser, made by GNU Bison 1.875c.  */

/* Skeleton parser for Yacc-like parsing with Bison,
   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

/* Written by Richard Stallman by simplifying the original so called
   ``semantic'' parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 0



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     END_OF_INPUT = 258,
     ERROR = 259,
     STR_CONST = 260,
     NUM_CONST = 261,
     NULL_CONST = 262,
     SYMBOL = 263,
     FUNCTION = 264,
     LEFT_ASSIGN = 265,
     EQ_ASSIGN = 266,
     RIGHT_ASSIGN = 267,
     LBB = 268,
     FOR = 269,
     IN = 270,
     IF = 271,
     ELSE = 272,
     WHILE = 273,
     NEXT = 274,
     BREAK = 275,
     REPEAT = 276,
     GT = 277,
     GE = 278,
     LT = 279,
     LE = 280,
     EQ = 281,
     NE = 282,
     AND = 283,
     OR = 284,
     NS_GET = 285,
     NS_GET_INT = 286,
     LOW = 287,
     TILDE = 288,
     NOT = 289,
     UNOT = 290,
     SPECIAL = 291,
     UPLUS = 292,
     UMINUS = 293
   };
#endif
#define END_OF_INPUT 258
#define ERROR 259
#define STR_CONST 260
#define NUM_CONST 261
#define NULL_CONST 262
#define SYMBOL 263
#define FUNCTION 264
#define LEFT_ASSIGN 265
#define EQ_ASSIGN 266
#define RIGHT_ASSIGN 267
#define LBB 268
#define FOR 269
#define IN 270
#define IF 271
#define ELSE 272
#define WHILE 273
#define NEXT 274
#define BREAK 275
#define REPEAT 276
#define GT 277
#define GE 278
#define LT 279
#define LE 280
#define EQ 281
#define NE 282
#define AND 283
#define OR 284
#define NS_GET 285
#define NS_GET_INT 286
#define LOW 287
#define TILDE 288
#define NOT 289
#define UNOT 290
#define SPECIAL 291
#define UPLUS 292
#define UMINUS 293




/* Copy the first part of user declarations.  */
#line 1 "gram.y"

/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2005  Robert Gentleman, Ross Ihaka and the
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

   typeofnext SymbolValue isValidName have been changed to cope.
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

#ifdef SUPPORT_MBCS
# include <wchar.h>
# include <wctype.h>

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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

#if ! defined (YYSTYPE) && ! defined (YYSTYPE_IS_DECLARED)
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



/* Copy the second part of user declarations.  */


/* Line 214 of yacc.c.  */
#line 344 "y.tab.c"

#if ! defined (yyoverflow) || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   define YYSTACK_ALLOC alloca
#  endif
# else
#  if defined (alloca) || defined (_ALLOCA_H)
#   define YYSTACK_ALLOC alloca
#  else
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
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
#endif /* ! defined (yyoverflow) || YYERROR_VERBOSE */


#if (! defined (yyoverflow) \
     && (! defined (__cplusplus) \
	 || (defined (YYSTYPE_IS_TRIVIAL) && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  short yyss;
  YYSTYPE yyvs;
  };

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (short) + sizeof (YYSTYPE))				\
      + YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined (__GNUC__) && 1 < __GNUC__
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
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (0)

#endif

#if defined (__STDC__) || defined (__cplusplus)
   typedef signed char yysigned_char;
#else
   typedef short yysigned_char;
#endif

/* YYFINAL -- State number of the termination state. */
#define YYFINAL  46
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   697

/* YYNTOKENS -- Number of terminals. */
#define YYNTOKENS  60
/* YYNNTS -- Number of nonterminals. */
#define YYNNTS  13
/* YYNRULES -- Number of rules. */
#define YYNRULES  88
/* YYNRULES -- Number of states. */
#define YYNSTATES  159

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   293

#define YYTRANSLATE(YYX) 						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const unsigned char yytranslate[] =
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
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    33,    35,    36,
      37,    42,    44,    45
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const unsigned short yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    13,    15,    17,    19,
      23,    25,    27,    29,    31,    35,    39,    42,    45,    48,
      51,    54,    58,    62,    66,    70,    74,    78,    82,    86,
      90,    94,    98,   102,   106,   110,   114,   118,   122,   126,
     130,   134,   141,   146,   150,   156,   160,   164,   167,   173,
     178,   182,   186,   190,   194,   198,   202,   206,   210,   214,
     218,   222,   226,   228,   230,   234,   238,   244,   245,   247,
     251,   254,   258,   261,   263,   268,   269,   271,   274,   278,
     281,   285,   288,   292,   293,   295,   299,   303,   309
};

/* YYRHS -- A `-1'-separated list of the rules' RHS. */
static const yysigned_char yyrhs[] =
{
      61,     0,    -1,     3,    -1,    51,    -1,    62,    51,    -1,
      62,    52,    -1,     1,    -1,    64,    -1,    63,    -1,    64,
      11,    62,    -1,     6,    -1,     5,    -1,     7,    -1,     8,
      -1,    53,    68,    54,    -1,    49,    62,    55,    -1,    39,
      64,    -1,    38,    64,    -1,    56,    64,    -1,    34,    64,
      -1,    32,    64,    -1,    64,    43,    64,    -1,    64,    38,
      64,    -1,    64,    39,    64,    -1,    64,    40,    64,    -1,
      64,    41,    64,    -1,    64,    46,    64,    -1,    64,    42,
      64,    -1,    64,    57,    64,    -1,    64,    34,    64,    -1,
      64,    32,    64,    -1,    64,    24,    64,    -1,    64,    25,
      64,    -1,    64,    26,    64,    -1,    64,    27,    64,    -1,
      64,    23,    64,    -1,    64,    22,    64,    -1,    64,    28,
      64,    -1,    64,    29,    64,    -1,    64,    10,    64,    -1,
      64,    12,    64,    -1,     9,    49,    71,    55,    72,    62,
      -1,    64,    49,    69,    55,    -1,    16,    66,    62,    -1,
      16,    66,    62,    17,    62,    -1,    14,    67,    62,    -1,
      18,    65,    62,    -1,    21,    62,    -1,    64,    13,    69,
      58,    58,    -1,    64,    50,    69,    58,    -1,     8,    30,
       8,    -1,     8,    30,     5,    -1,     5,    30,     8,    -1,
       5,    30,     5,    -1,     8,    31,     8,    -1,     8,    31,
       5,    -1,     5,    31,     8,    -1,     5,    31,     5,    -1,
      64,    47,     8,    -1,    64,    47,     5,    -1,    64,    48,
       8,    -1,    64,    48,     5,    -1,    19,    -1,    20,    -1,
      49,    64,    55,    -1,    49,    64,    55,    -1,    49,     8,
      15,    64,    55,    -1,    -1,    62,    -1,    68,    52,    62,
      -1,    68,    52,    -1,    68,    51,    62,    -1,    68,    51,
      -1,    70,    -1,    69,    72,    59,    70,    -1,    -1,    64,
      -1,     8,    11,    -1,     8,    11,    64,    -1,     5,    11,
      -1,     5,    11,    64,    -1,     7,    11,    -1,     7,    11,
      64,    -1,    -1,     8,    -1,     8,    11,    64,    -1,    71,
      59,     8,    -1,    71,    59,     8,    11,    64,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const unsigned short yyrline[] =
{
       0,   213,   213,   214,   215,   216,   217,   220,   221,   224,
     227,   228,   229,   230,   232,   233,   235,   236,   237,   238,
     239,   241,   242,   243,   244,   245,   246,   247,   248,   249,
     250,   251,   252,   253,   254,   255,   256,   257,   258,   260,
     261,   262,   264,   265,   266,   267,   268,   269,   270,   271,
     272,   273,   274,   275,   276,   277,   278,   279,   280,   281,
     282,   283,   284,   285,   289,   292,   295,   299,   300,   301,
     302,   303,   304,   307,   308,   311,   312,   313,   314,   315,
     316,   317,   318,   321,   322,   323,   324,   325,   328
};
#endif

#if YYDEBUG || YYERROR_VERBOSE
/* YYTNME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals. */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "STR_CONST",
  "NUM_CONST", "NULL_CONST", "SYMBOL", "FUNCTION", "LEFT_ASSIGN",
  "EQ_ASSIGN", "RIGHT_ASSIGN", "LBB", "FOR", "IN", "IF", "ELSE", "WHILE",
  "NEXT", "BREAK", "REPEAT", "GT", "GE", "LT", "LE", "EQ", "NE", "AND",
  "OR", "NS_GET", "NS_GET_INT", "'?'", "LOW", "'~'", "TILDE", "NOT",
  "UNOT", "'+'", "'-'", "'*'", "'/'", "SPECIAL", "':'", "UPLUS", "UMINUS",
  "'^'", "'$'", "'@'", "'('", "'['", "'\\n'", "';'", "'{'", "'}'", "')'",
  "'!'", "'%'", "']'", "','", "$accept", "prog", "expr_or_assign",
  "equal_assign", "expr", "cond", "ifcond", "forcond", "exprlist",
  "sublist", "sub", "formlist", "cr", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const unsigned short yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,    63,   287,   126,   288,   289,   290,    43,    45,
      42,    47,   291,    58,   292,   293,    94,    36,    64,    40,
      91,    10,    59,   123,   125,    41,    33,    37,    93,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const unsigned char yyr1[] =
{
       0,    60,    61,    61,    61,    61,    61,    62,    62,    63,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    65,    66,    67,    68,    68,    68,
      68,    68,    68,    69,    69,    70,    70,    70,    70,    70,
      70,    70,    70,    71,    71,    71,    71,    71,    72
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const unsigned char yyr2[] =
{
       0,     2,     1,     1,     2,     2,     1,     1,     1,     3,
       1,     1,     1,     1,     3,     3,     2,     2,     2,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     6,     4,     3,     5,     3,     3,     2,     5,     4,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     1,     1,     3,     3,     5,     0,     1,     3,
       2,     3,     2,     1,     4,     0,     1,     2,     3,     2,
       3,     2,     3,     0,     1,     3,     3,     5,     0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const unsigned char yydefact[] =
{
       0,     6,     2,    11,    10,    12,    13,     0,     0,     0,
       0,    62,    63,     0,     0,     0,     0,     0,     0,     3,
      67,     0,     0,     0,     8,     7,     0,     0,     0,     0,
      83,     0,     0,     0,     0,     0,     0,    47,    20,    19,
      17,    16,     0,    68,     0,    18,     1,     4,     5,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    75,    75,     0,    53,    52,    57,    56,    51,
      50,    55,    54,    84,     0,     0,    45,     0,    43,     0,
      46,    15,    72,    70,    14,    39,     9,    40,    11,    12,
      13,    76,    88,    73,    36,    35,    31,    32,    33,    34,
      37,    38,    30,    29,    22,    23,    24,    25,    27,    21,
      26,    59,    58,    61,    60,    88,    88,    28,     0,    88,
       0,     0,    65,     0,    64,    71,    69,    79,    81,    77,
       0,     0,    42,    49,    85,     0,    86,     0,    44,    80,
      82,    78,    48,    75,    41,     0,    66,    74,    87
};

/* YYDEFGOTO[NTERM-NUM]. */
static const short yydefgoto[] =
{
      -1,    22,    23,    24,    25,    36,    34,    32,    44,   102,
     103,    84,   141
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -51
static const short yypact[] =
{
      81,   -51,   -51,    75,   -51,   -51,    91,   -33,   -31,   -29,
     -27,   -51,   -51,   140,   140,   140,   140,   140,   140,   -51,
     140,   140,     8,    40,   -51,   227,     7,     9,    22,    24,
      20,    25,   140,   140,   140,   140,   140,   -51,   432,   508,
     116,   116,     2,   -51,   -41,   584,   -51,   -51,   -51,   140,
     140,   140,   192,   140,   140,   140,   140,   140,   140,   140,
     140,   140,   140,   140,   140,   140,   140,   140,   140,   140,
      26,    59,   192,   192,   140,   -51,   -51,   -51,   -51,   -51,
     -51,   -51,   -51,    51,   -50,    53,   -51,   268,    64,   309,
     -51,   -51,   140,   140,   -51,   432,   -51,   470,    -7,    65,
      -5,   391,    27,   -51,   622,   622,   622,   622,   622,   622,
     584,   546,   432,   508,   640,   640,    23,    23,    61,   116,
     116,   -51,   -51,   -51,   -51,    28,    36,   391,   140,   -51,
      70,   140,   -51,   140,   -51,   -51,   -51,   140,   140,   140,
      38,    39,   -51,   -51,   391,   140,    92,   350,   -51,   391,
     391,   391,   -51,   192,   -51,   140,   -51,   -51,   391
};

/* YYPGOTO[NTERM-NUM].  */
static const yysigned_char yypgoto[] =
{
     -51,   -51,    43,   -51,   -14,   -51,   -51,   -51,   -51,    54,
     -37,   -51,   -17
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const short yytable[] =
{
      38,    39,    40,    41,   137,   129,   139,    45,    46,   130,
      92,    93,    75,    94,    77,    76,    30,    78,    31,    87,
      33,    89,    35,    26,    27,    28,    29,    79,    83,    81,
      80,   121,    82,    85,   122,    95,    52,    97,   101,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,    37,    91,   101,   101,
     127,    42,   128,    43,   123,    67,    68,   124,   131,    69,
      70,    71,    72,    73,    52,    86,   138,    88,   146,    90,
      74,   133,     1,   142,     2,   140,     3,     4,     5,     6,
       7,    47,    48,    96,   143,     8,   152,     9,   153,    10,
      11,    12,    13,   155,    68,    26,    27,    69,    70,    71,
      72,    73,   145,    14,   144,    15,   157,   147,    74,    16,
      17,    28,    29,   149,   150,   151,   125,   126,     0,    52,
      18,     0,    19,     0,    20,   135,   136,    21,     0,   101,
       0,   158,     0,     0,     0,     3,     4,     5,     6,     7,
       0,     0,     0,     0,     8,     0,     9,     0,    10,    11,
      12,    13,    69,    70,    71,    72,    73,     0,     0,     0,
       0,     0,    14,    74,    15,     0,   148,     0,    16,    17,
       0,     0,     0,     0,     0,     0,     0,     0,   154,    18,
       0,     0,     0,    20,     0,     0,    21,    98,     4,    99,
     100,     7,     0,     0,     0,     0,     8,     0,     9,     0,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    14,     0,    15,     0,     0,     0,
      16,    17,     0,     0,     0,     0,     0,    49,    50,    51,
      52,    18,     0,     0,     0,    20,     0,     0,    21,    53,
      54,    55,    56,    57,    58,    59,    60,     0,     0,    61,
       0,    62,     0,     0,     0,    63,    64,    65,    66,    67,
      68,     0,     0,    69,    70,    71,    72,    73,    49,     0,
      51,    52,     0,     0,    74,     0,     0,     0,     0,     0,
      53,    54,    55,    56,    57,    58,    59,    60,     0,     0,
      61,     0,    62,     0,     0,     0,    63,    64,    65,    66,
      67,    68,     0,     0,    69,    70,    71,    72,    73,    49,
       0,    51,    52,   132,     0,    74,     0,     0,     0,     0,
       0,    53,    54,    55,    56,    57,    58,    59,    60,     0,
       0,    61,     0,    62,     0,     0,     0,    63,    64,    65,
      66,    67,    68,     0,     0,    69,    70,    71,    72,    73,
      49,     0,    51,    52,   134,     0,    74,     0,     0,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
       0,     0,    61,     0,    62,     0,     0,     0,    63,    64,
      65,    66,    67,    68,     0,     0,    69,    70,    71,    72,
      73,    49,     0,    51,    52,   156,     0,    74,     0,     0,
       0,     0,     0,    53,    54,    55,    56,    57,    58,    59,
      60,     0,     0,    61,     0,    62,     0,     0,     0,    63,
      64,    65,    66,    67,    68,     0,     0,    69,    70,    71,
      72,    73,    49,     0,    51,    52,     0,     0,    74,     0,
       0,     0,     0,     0,    53,    54,    55,    56,    57,    58,
      59,    60,     0,     0,     0,     0,    62,     0,     0,     0,
      63,    64,    65,    66,    67,    68,     0,     0,    69,    70,
      71,    72,    73,    52,     0,     0,     0,     0,     0,    74,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
       0,     0,     0,     0,    62,     0,     0,     0,    63,    64,
      65,    66,    67,    68,     0,     0,    69,    70,    71,    72,
      73,    52,     0,     0,     0,     0,     0,    74,     0,     0,
      53,    54,    55,    56,    57,    58,    59,    60,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,    65,    66,
      67,    68,     0,     0,    69,    70,    71,    72,    73,    52,
       0,     0,     0,     0,     0,    74,     0,     0,    53,    54,
      55,    56,    57,    58,    59,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,    65,    66,    67,    68,
       0,     0,    69,    70,    71,    72,    73,    52,     0,     0,
       0,     0,     0,    74,     0,     0,    53,    54,    55,    56,
      57,    58,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    63,    64,    65,    66,    67,    68,     0,     0,
      69,    70,    71,    72,    73,    52,     0,     0,     0,     0,
       0,    74,     0,     0,    -1,    -1,    -1,    -1,    -1,    -1,
       0,     0,     0,    52,     0,     0,     0,     0,     0,     0,
      63,    64,    65,    66,    67,    68,     0,     0,    69,    70,
      71,    72,    73,     0,     0,     0,     0,     0,     0,    74,
      65,    66,    67,    68,     0,     0,    69,    70,    71,    72,
      73,     0,     0,     0,     0,     0,     0,    74
};

static const short yycheck[] =
{
      14,    15,    16,    17,    11,    55,    11,    21,     0,    59,
      51,    52,     5,    54,     5,     8,    49,     8,    49,    33,
      49,    35,    49,    30,    31,    30,    31,     5,     8,     5,
       8,     5,     8,     8,     8,    49,    13,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    13,    55,    72,    73,
      74,    18,    11,    20,     5,    42,    43,     8,    15,    46,
      47,    48,    49,    50,    13,    32,    11,    34,     8,    36,
      57,    17,     1,    55,     3,    58,     5,     6,     7,     8,
       9,    51,    52,    50,    58,    14,    58,    16,    59,    18,
      19,    20,    21,    11,    43,    30,    31,    46,    47,    48,
      49,    50,   129,    32,   128,    34,   153,   131,    57,    38,
      39,    30,    31,   137,   138,   139,    72,    73,    -1,    13,
      49,    -1,    51,    -1,    53,    92,    93,    56,    -1,   153,
      -1,   155,    -1,    -1,    -1,     5,     6,     7,     8,     9,
      -1,    -1,    -1,    -1,    14,    -1,    16,    -1,    18,    19,
      20,    21,    46,    47,    48,    49,    50,    -1,    -1,    -1,
      -1,    -1,    32,    57,    34,    -1,   133,    -1,    38,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   145,    49,
      -1,    -1,    -1,    53,    -1,    -1,    56,     5,     6,     7,
       8,     9,    -1,    -1,    -1,    -1,    14,    -1,    16,    -1,
      18,    19,    20,    21,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    32,    -1,    34,    -1,    -1,    -1,
      38,    39,    -1,    -1,    -1,    -1,    -1,    10,    11,    12,
      13,    49,    -1,    -1,    -1,    53,    -1,    -1,    56,    22,
      23,    24,    25,    26,    27,    28,    29,    -1,    -1,    32,
      -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,    42,
      43,    -1,    -1,    46,    47,    48,    49,    50,    10,    -1,
      12,    13,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
      32,    -1,    34,    -1,    -1,    -1,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    47,    48,    49,    50,    10,
      -1,    12,    13,    55,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    -1,
      -1,    32,    -1,    34,    -1,    -1,    -1,    38,    39,    40,
      41,    42,    43,    -1,    -1,    46,    47,    48,    49,    50,
      10,    -1,    12,    13,    55,    -1,    57,    -1,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      -1,    -1,    32,    -1,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    42,    43,    -1,    -1,    46,    47,    48,    49,
      50,    10,    -1,    12,    13,    55,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    -1,    -1,    32,    -1,    34,    -1,    -1,    -1,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    47,    48,
      49,    50,    10,    -1,    12,    13,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    47,
      48,    49,    50,    13,    -1,    -1,    -1,    -1,    -1,    57,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      -1,    -1,    -1,    -1,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    42,    43,    -1,    -1,    46,    47,    48,    49,
      50,    13,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    38,    39,    40,    41,
      42,    43,    -1,    -1,    46,    47,    48,    49,    50,    13,
      -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    22,    23,
      24,    25,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    38,    39,    40,    41,    42,    43,
      -1,    -1,    46,    47,    48,    49,    50,    13,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    22,    23,    24,    25,
      26,    27,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,    47,    48,    49,    50,    13,    -1,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    22,    23,    24,    25,    26,    27,
      -1,    -1,    -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    47,
      48,    49,    50,    -1,    -1,    -1,    -1,    -1,    -1,    57,
      40,    41,    42,    43,    -1,    -1,    46,    47,    48,    49,
      50,    -1,    -1,    -1,    -1,    -1,    -1,    57
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const unsigned char yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,     9,    14,    16,
      18,    19,    20,    21,    32,    34,    38,    39,    49,    51,
      53,    56,    61,    62,    63,    64,    30,    31,    30,    31,
      49,    49,    67,    49,    66,    49,    65,    62,    64,    64,
      64,    64,    62,    62,    68,    64,     0,    51,    52,    10,
      11,    12,    13,    22,    23,    24,    25,    26,    27,    28,
      29,    32,    34,    38,    39,    40,    41,    42,    43,    46,
      47,    48,    49,    50,    57,     5,     8,     5,     8,     5,
       8,     5,     8,     8,    71,     8,    62,    64,    62,    64,
      62,    55,    51,    52,    54,    64,    62,    64,     5,     7,
       8,    64,    69,    70,    64,    64,    64,    64,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      64,     5,     8,     5,     8,    69,    69,    64,    11,    55,
      59,    15,    55,    17,    55,    62,    62,    11,    11,    11,
      58,    72,    55,    58,    64,    72,     8,    64,    62,    64,
      64,    64,    58,    59,    62,    11,    55,    70,    64
};

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
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


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
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { 								\
      yyerror ("syntax error: cannot back up");\
      YYERROR;							\
    }								\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

/* YYLLOC_DEFAULT -- Compute the default location (before the actions
   are run).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)		\
   ((Current).first_line   = (Rhs)[1].first_line,	\
    (Current).first_column = (Rhs)[1].first_column,	\
    (Current).last_line    = (Rhs)[N].last_line,	\
    (Current).last_column  = (Rhs)[N].last_column)
#endif

/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

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

# define YYDSYMPRINT(Args)			\
do {						\
  if (yydebug)					\
    yysymprint Args;				\
} while (0)

# define YYDSYMPRINTF(Title, Token, Value, Location)		\
do {								\
  if (yydebug)							\
    {								\
      YYFPRINTF (stderr, "%s ", Title);				\
      yysymprint (stderr, 					\
                  Token, Value);	\
      YYFPRINTF (stderr, "\n");					\
    }								\
} while (0)

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_stack_print (short *bottom, short *top)
#else
static void
yy_stack_print (bottom, top)
    short *bottom;
    short *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (/* Nothing. */; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yy_reduce_print (int yyrule)
#else
static void
yy_reduce_print (yyrule)
    int yyrule;
#endif
{
  int yyi;
  unsigned int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %u), ",
             yyrule - 1, yylno);
  /* Print the symbols being reduced, and their result.  */
  for (yyi = yyprhs[yyrule]; 0 <= yyrhs[yyi]; yyi++)
    YYFPRINTF (stderr, "%s ", yytname [yyrhs[yyi]]);
  YYFPRINTF (stderr, "-> %s\n", yytname [yyr1[yyrule]]);
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (Rule);		\
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YYDSYMPRINT(Args)
# define YYDSYMPRINTF(Title, Token, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
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

#if defined (YYMAXDEPTH) && YYMAXDEPTH == 0
# undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

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

#endif /* !YYERROR_VERBOSE */



#if YYDEBUG
/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yysymprint (FILE *yyoutput, int yytype, YYSTYPE *yyvaluep)
#else
static void
yysymprint (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  if (yytype < YYNTOKENS)
    {
      YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
# ifdef YYPRINT
      YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
    }
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  switch (yytype)
    {
      default:
        break;
    }
  YYFPRINTF (yyoutput, ")");
}

#endif /* ! YYDEBUG */
/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

#if defined (__STDC__) || defined (__cplusplus)
static void
yydestruct (int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yytype, yyvaluep)
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  /* Pacify ``unused variable'' warnings.  */
  (void) yyvaluep;

  switch (yytype)
    {

      default:
        break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM);
# else
int yyparse ();
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
# if defined (__STDC__) || defined (__cplusplus)
int yyparse (void *YYPARSE_PARAM)
# else
int yyparse (YYPARSE_PARAM)
  void *YYPARSE_PARAM;
# endif
#else /* ! YYPARSE_PARAM */
#if defined (__STDC__) || defined (__cplusplus)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  register int yystate;
  register int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  short	yyssa[YYINITDEPTH];
  short *yyss = yyssa;
  register short *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  register YYSTYPE *yyvsp;



#define YYPOPSTACK   (yyvsp--, yyssp--)

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;


  /* When reducing, the number of symbols on the RHS of the reduced
     rule.  */
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

  if (yyss + yystacksize - 1 <= yyssp)
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
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow ("parser stack overflow",
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),

		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyoverflowlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyoverflowlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	short *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyoverflowlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);

#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;


      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
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
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YYDSYMPRINTF ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */
  YYDPRINTF ((stderr, "Shifting token %s, ", yytname[yytoken]));

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;


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

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 213 "gram.y"
    { return 0; }
    break;

  case 3:
#line 214 "gram.y"
    { return xxvalue(NULL,2); }
    break;

  case 4:
#line 215 "gram.y"
    { return xxvalue(yyvsp[-1],3); }
    break;

  case 5:
#line 216 "gram.y"
    { return xxvalue(yyvsp[-1],4); }
    break;

  case 6:
#line 217 "gram.y"
    { YYABORT; }
    break;

  case 7:
#line 220 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 8:
#line 221 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 9:
#line 224 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 10:
#line 227 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 11:
#line 228 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 12:
#line 229 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 13:
#line 230 "gram.y"
    { yyval = yyvsp[0]; }
    break;

  case 14:
#line 232 "gram.y"
    { yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); }
    break;

  case 15:
#line 233 "gram.y"
    { yyval = xxparen(yyvsp[-2],yyvsp[-1]); }
    break;

  case 16:
#line 235 "gram.y"
    { yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;

  case 17:
#line 236 "gram.y"
    { yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;

  case 18:
#line 237 "gram.y"
    { yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;

  case 19:
#line 238 "gram.y"
    { yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;

  case 20:
#line 239 "gram.y"
    { yyval = xxunary(yyvsp[-1],yyvsp[0]); }
    break;

  case 21:
#line 241 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 22:
#line 242 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 23:
#line 243 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 24:
#line 244 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 25:
#line 245 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 26:
#line 246 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 27:
#line 247 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 28:
#line 248 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 29:
#line 249 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 30:
#line 250 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 31:
#line 251 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 32:
#line 252 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 33:
#line 253 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 34:
#line 254 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 35:
#line 255 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 36:
#line 256 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 37:
#line 257 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 38:
#line 258 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 39:
#line 260 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 40:
#line 261 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); }
    break;

  case 41:
#line 263 "gram.y"
    { yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); }
    break;

  case 42:
#line 264 "gram.y"
    { yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); }
    break;

  case 43:
#line 265 "gram.y"
    { yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;

  case 44:
#line 266 "gram.y"
    { yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); }
    break;

  case 45:
#line 267 "gram.y"
    { yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;

  case 46:
#line 268 "gram.y"
    { yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
    break;

  case 47:
#line 269 "gram.y"
    { yyval = xxrepeat(yyvsp[-1],yyvsp[0]); }
    break;

  case 48:
#line 270 "gram.y"
    { yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); }
    break;

  case 49:
#line 271 "gram.y"
    { yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); }
    break;

  case 50:
#line 272 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 51:
#line 273 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 52:
#line 274 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 53:
#line 275 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 54:
#line 276 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 55:
#line 277 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 56:
#line 278 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 57:
#line 279 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 58:
#line 280 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 59:
#line 281 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 60:
#line 282 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 61:
#line 283 "gram.y"
    { yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
    break;

  case 62:
#line 284 "gram.y"
    { yyval = xxnxtbrk(yyvsp[0]); }
    break;

  case 63:
#line 285 "gram.y"
    { yyval = xxnxtbrk(yyvsp[0]); }
    break;

  case 64:
#line 289 "gram.y"
    { yyval = xxcond(yyvsp[-1]); }
    break;

  case 65:
#line 292 "gram.y"
    { yyval = xxifcond(yyvsp[-1]); }
    break;

  case 66:
#line 295 "gram.y"
    { yyval = xxforcond(yyvsp[-3],yyvsp[-1]); }
    break;

  case 67:
#line 299 "gram.y"
    { yyval = xxexprlist0(); }
    break;

  case 68:
#line 300 "gram.y"
    { yyval = xxexprlist1(yyvsp[0]); }
    break;

  case 69:
#line 301 "gram.y"
    { yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
    break;

  case 70:
#line 302 "gram.y"
    { yyval = yyvsp[-1]; }
    break;

  case 71:
#line 303 "gram.y"
    { yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
    break;

  case 72:
#line 304 "gram.y"
    { yyval = yyvsp[-1];}
    break;

  case 73:
#line 307 "gram.y"
    { yyval = xxsublist1(yyvsp[0]); }
    break;

  case 74:
#line 308 "gram.y"
    { yyval = xxsublist2(yyvsp[-3],yyvsp[0]); }
    break;

  case 75:
#line 311 "gram.y"
    { yyval = xxsub0(); }
    break;

  case 76:
#line 312 "gram.y"
    { yyval = xxsub1(yyvsp[0]); }
    break;

  case 77:
#line 313 "gram.y"
    { yyval = xxsymsub0(yyvsp[-1]); }
    break;

  case 78:
#line 314 "gram.y"
    { yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
    break;

  case 79:
#line 315 "gram.y"
    { yyval = xxsymsub0(yyvsp[-1]); }
    break;

  case 80:
#line 316 "gram.y"
    { yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
    break;

  case 81:
#line 317 "gram.y"
    { yyval = xxnullsub0(); }
    break;

  case 82:
#line 318 "gram.y"
    { yyval = xxnullsub1(yyvsp[0]); }
    break;

  case 83:
#line 321 "gram.y"
    { yyval = xxnullformal(); }
    break;

  case 84:
#line 322 "gram.y"
    { yyval = xxfirstformal0(yyvsp[0]); }
    break;

  case 85:
#line 323 "gram.y"
    { yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); }
    break;

  case 86:
#line 324 "gram.y"
    { yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); }
    break;

  case 87:
#line 325 "gram.y"
    { yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); }
    break;

  case 88:
#line 328 "gram.y"
    { EatLines = 1; }
    break;


    }

/* Line 993 of yacc.c.  */
#line 1904 "y.tab.c"

  yyvsp -= yylen;
  yyssp -= yylen;


  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;


  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (YYPACT_NINF < yyn && yyn < YYLAST)
	{
	  YYSIZE_T yysize = 0;
	  int yytype = YYTRANSLATE (yychar);
	  const char* yyprefix;
	  char *yymsg;
	  int yyx;

	  /* Start YYX at -YYN if negative to avoid negative indexes in
	     YYCHECK.  */
	  int yyxbegin = yyn < 0 ? -yyn : 0;

	  /* Stay within bounds of both yycheck and yytname.  */
	  int yychecklim = YYLAST - yyn;
	  int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
	  int yycount = 0;

	  yyprefix = ", expecting ";
	  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	      {
		yysize += yystrlen (yyprefix) + yystrlen (yytname [yyx]);
		yycount += 1;
		if (yycount == 5)
		  {
		    yysize = 0;
		    break;
		  }
	      }
	  yysize += (sizeof ("syntax error, unexpected ")
		     + yystrlen (yytname[yytype]));
	  yymsg = (char *) YYSTACK_ALLOC (yysize);
	  if (yymsg != 0)
	    {
	      char *yyp = yystpcpy (yymsg, "syntax error, unexpected ");
	      yyp = yystpcpy (yyp, yytname[yytype]);

	      if (yycount < 5)
		{
		  yyprefix = ", expecting ";
		  for (yyx = yyxbegin; yyx < yyxend; ++yyx)
		    if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
		      {
			yyp = yystpcpy (yyp, yyprefix);
			yyp = yystpcpy (yyp, yytname[yyx]);
			yyprefix = " or ";
		      }
		}
	      yyerror (yymsg);
	      YYSTACK_FREE (yymsg);
	    }
	  else
	    yyerror ("syntax error; also virtual memory exhausted");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror ("syntax error");
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* If at end of input, pop the error token,
	     then the rest of the stack, then return failure.  */
	  if (yychar == YYEOF)
	     for (;;)
	       {
		 YYPOPSTACK;
		 if (yyssp == yyss)
		   YYABORT;
		 YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
		 yydestruct (yystos[*yyssp], yyvsp);
	       }
        }
      else
	{
	  YYDSYMPRINTF ("Error: discarding", yytoken, &yylval, &yylloc);
	  yydestruct (yytoken, &yylval);
	  yychar = YYEMPTY;

	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

#ifdef __GNUC__
  /* Pacify GCC when the user code never invokes YYERROR and the label
     yyerrorlab therefore never appears in user code.  */
  if (0)
     goto yyerrorlab;
#endif

  yyvsp -= yylen;
  yyssp -= yylen;
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      YYDSYMPRINTF ("Error: popping", yystos[*yyssp], yyvsp, yylsp);
      yydestruct (yystos[yystate], yyvsp);
      YYPOPSTACK;
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  YYDPRINTF ((stderr, "Shifting error token, "));

  *++yyvsp = yylval;


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

#ifndef yyoverflow
/*----------------------------------------------.
| yyoverflowlab -- parser overflow comes here.  |
`----------------------------------------------*/
yyoverflowlab:
  yyerror ("parser stack overflow");
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  return yyresult;
}


#line 330 "gram.y"



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
    KeepSource = *LOGICAL(GetOption(install("keep.source"), R_BaseEnv));
    npush = 0;
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

#define GENERAL
#ifdef GENERAL

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

SEXP R_Parse(int n, ParseStatus *status)
{
    int i;
    SEXP t, rval;
    ParseContextInit();
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

SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status)
{
    GenerateCode = 1;
    R_ParseError = 1;
    con_parse = con;;
    ptr_getc = con_getc;
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
#ifdef SUPPORT_MBCS
	    /* Only realy valid in UTF-8, but useful shorthand elsewhere */
	    else if(mbcslocale && c == 'u') {
		wint_t val = 0; int i, ext; size_t res;
		char buff[5]; Rboolean delim = FALSE;
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
		res = wcrtomb(buff, val, NULL); /* should always be valid */
		if((int)res <= 0) error(_("invalid \\uxxxx sequence"));
		for(i = 0; i <  res - 1; i++) YYTEXT_PUSH(buff[i], yyp);
		c = buff[res - 1]; /* pushed below */
	    }
#ifndef Win32
	    else if(mbcslocale && c == 'U') {
		wint_t val = 0; int i, ext; size_t res;
		char buff[9]; Rboolean delim = FALSE;
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
		res = wcrtomb(buff, val, NULL); /* should always be valid */
		if((int)res <= 0) error(("invalid \\Uxxxxxxxx sequence"));
		for(i = 0; i <  res - 1; i++) YYTEXT_PUSH(buff[i], yyp);
		c = buff[res - 1]; /* pushed below */
	    }
#endif
#endif
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
#ifdef SUPPORT_MBCS
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
#ifdef SUPPORT_MBCS
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
	    if(c == '.' || c == '_') continue;
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
#ifdef SUPPORT_MBCS
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
	return StringValue(c);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return QuotedSymbolValue(c);
 symbol:

    if (c == '.') return SymbolValue(c);
#ifdef SUPPORT_MBCS
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


