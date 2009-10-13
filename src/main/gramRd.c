/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

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
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 1



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     END_OF_INPUT = 258,
     ERROR = 259,
     SECTIONHEADER = 260,
     RSECTIONHEADER = 261,
     VSECTIONHEADER = 262,
     SECTIONHEADER2 = 263,
     RCODEMACRO = 264,
     SEXPR = 265,
     RDOPTS = 266,
     LATEXMACRO = 267,
     VERBMACRO = 268,
     OPTMACRO = 269,
     ESCAPE = 270,
     LISTSECTION = 271,
     ITEMIZE = 272,
     DESCRIPTION = 273,
     NOITEM = 274,
     LATEXMACRO2 = 275,
     VERBMACRO2 = 276,
     LATEXMACRO3 = 277,
     IFDEF = 278,
     ENDIF = 279,
     TEXT = 280,
     RCODE = 281,
     VERB = 282,
     COMMENT = 283,
     UNKNOWN = 284,
     STARTFILE = 285,
     STARTFRAGMENT = 286
   };
#endif
/* Tokens.  */
#define END_OF_INPUT 258
#define ERROR 259
#define SECTIONHEADER 260
#define RSECTIONHEADER 261
#define VSECTIONHEADER 262
#define SECTIONHEADER2 263
#define RCODEMACRO 264
#define SEXPR 265
#define RDOPTS 266
#define LATEXMACRO 267
#define VERBMACRO 268
#define OPTMACRO 269
#define ESCAPE 270
#define LISTSECTION 271
#define ITEMIZE 272
#define DESCRIPTION 273
#define NOITEM 274
#define LATEXMACRO2 275
#define VERBMACRO2 276
#define LATEXMACRO3 277
#define IFDEF 278
#define ENDIF 279
#define TEXT 280
#define RCODE 281
#define VERB 282
#define COMMENT 283
#define UNKNOWN 284
#define STARTFILE 285
#define STARTFRAGMENT 286




/* Copy the first part of user declarations.  */
#line 2 "gramRd.y"

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
#define STRICT_R_HEADERS
#include <R_ext/RS.h>           /* for R_chk_* allocation */

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

static Rboolean wCalls = TRUE;

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

/* Flags used to mark presence of IFDEF or Sexpr in the dynamicFlag attribute */

#define STATIC 0
#define HAS_IFDEF 1
#define HAS_SEXPR 2

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
static int	xxinitvalue;
static char const yyunknown[] = "unknown macro"; /* our message, not bison's */


#define RLIKE 1		/* Includes R strings; xxinRString holds the opening quote char, or 0 outside a string */
#define LATEXLIKE 2
#define VERBATIM 3
#define INOPTION 4
#define COMMENTMODE 5   /* only used in deparsing */
#define UNKNOWNMODE 6   /* ditto */

static SEXP     SrcFile;  /* parse_Rd will *always* supply a srcfile */

/* Routines used to build the parse tree */

static SEXP	xxpushMode(int, int, int);
static void	xxpopMode(SEXP);
static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, int, int, YYLTYPE *);
static SEXP	xxmarkup3(SEXP, SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, int, YYLTYPE *);
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



/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 326 "gramRd.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
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
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  21
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   490

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  36
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  73
/* YYNRULES -- Number of states.  */
#define YYNSTATES  126

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   286

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    34,     2,    35,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    32,     2,    33,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     7,    11,    13,    16,    18,    20,    23,
      26,    29,    32,    35,    38,    42,    47,    51,    56,    58,
      60,    63,    65,    68,    70,    72,    74,    76,    78,    80,
      82,    85,    88,    92,    97,   100,   103,   107,   112,   115,
     119,   124,   127,   130,   134,   136,   141,   144,   147,   150,
     153,   156,   159,   164,   168,   171,   174,   179,   183,   186,
     187,   188,   189,   190,   191,   192,   193,   194,   195,   199,
     202,   207,   211,   216
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      37,     0,    -1,    30,    39,     3,    -1,    31,    38,     3,
      -1,     1,    -1,    55,    42,    -1,    40,    -1,    41,    -1,
      40,    41,    -1,     7,    51,    -1,    11,    51,    -1,     6,
      49,    -1,     5,    45,    -1,    16,    48,    -1,     8,    45,
      46,    -1,    23,    54,    40,    24,    -1,    10,    58,    50,
      -1,    10,    58,    65,    50,    -1,    28,    -1,    25,    -1,
       1,    41,    -1,    43,    -1,    42,    43,    -1,    25,    -1,
      26,    -1,    27,    -1,    28,    -1,    29,    -1,    64,    -1,
      44,    -1,     1,    43,    -1,    12,    45,    -1,    20,    45,
      46,    -1,    22,    45,    46,    46,    -1,    17,    47,    -1,
      18,    48,    -1,    14,    58,    45,    -1,    14,    58,    65,
      45,    -1,     9,    49,    -1,    10,    58,    50,    -1,    10,
      58,    65,    50,    -1,    13,    51,    -1,    21,    52,    -1,
      21,    52,    53,    -1,    15,    -1,    23,    54,    42,    24,
      -1,    55,    64,    -1,    55,    64,    -1,    55,    25,    -1,
      62,    64,    -1,    63,    64,    -1,    56,    64,    -1,    32,
      57,    42,    33,    -1,    32,    57,    33,    -1,    59,    64,
      -1,    60,    64,    -1,    32,    61,    42,    33,    -1,    32,
      61,    33,    -1,    55,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    32,    42,    33,    -1,    32,
      33,    -1,    32,    42,     1,    33,    -1,    32,     1,    33,
      -1,    32,    42,     1,     3,    -1,    34,    43,    35,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   177,   177,   178,   179,   182,   185,   188,   189,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,   201,
     202,   204,   205,   207,   208,   209,   210,   211,   212,   213,
     214,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   232,   234,   235,   244,
     246,   248,   252,   253,   255,   257,   261,   262,   264,   267,
     269,   271,   273,   275,   277,   279,   281,   283,   285,   286,
     287,   288,   289,   291
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "SECTIONHEADER",
  "RSECTIONHEADER", "VSECTIONHEADER", "SECTIONHEADER2", "RCODEMACRO",
  "SEXPR", "RDOPTS", "LATEXMACRO", "VERBMACRO", "OPTMACRO", "ESCAPE",
  "LISTSECTION", "ITEMIZE", "DESCRIPTION", "NOITEM", "LATEXMACRO2",
  "VERBMACRO2", "LATEXMACRO3", "IFDEF", "ENDIF", "TEXT", "RCODE", "VERB",
  "COMMENT", "UNKNOWN", "STARTFILE", "STARTFRAGMENT", "'{'", "'}'", "'['",
  "']'", "$accept", "Init", "RdFragment", "RdFile", "SectionList",
  "Section", "ArgItems", "Item", "Markup", "LatexArg", "LatexArg2",
  "Item0Arg", "Item2Arg", "RLikeArg", "RLikeArg2", "VerbatimArg",
  "VerbatimArg1", "VerbatimArg2", "IfDefTarget", "goLatexLike", "goRLike",
  "goRLike2", "goOption", "goVerbatim", "goVerbatim1", "goVerbatim2",
  "goItem0", "goItem2", "Arg", "Option", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   123,   125,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    36,    37,    37,    37,    38,    39,    40,    40,    41,
      41,    41,    41,    41,    41,    41,    41,    41,    41,    41,
      41,    42,    42,    43,    43,    43,    43,    43,    43,    43,
      43,    44,    44,    44,    44,    44,    44,    44,    44,    44,
      44,    44,    44,    44,    44,    44,    45,    46,    46,    47,
      48,    49,    50,    50,    51,    52,    53,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    63,    64,    64,
      64,    64,    64,    65
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     3,     1,     2,     1,     1,     2,     2,
       2,     2,     2,     2,     3,     4,     3,     4,     1,     1,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     3,     4,     2,     2,     3,     4,     2,     3,
       4,     2,     2,     3,     1,     4,     2,     2,     2,     2,
       2,     2,     4,     3,     2,     2,     4,     3,     2,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     3,     2,
       4,     3,     4,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     4,     0,    59,     0,     0,    59,    60,    63,    59,
      62,    63,    67,    59,    19,    18,     0,     0,     7,     0,
       0,     1,    20,    12,     0,    11,     0,     9,     0,    59,
       0,    10,    13,     0,     0,     0,     2,     8,     3,     0,
      60,    62,    59,    63,    62,    44,    66,    67,    59,    64,
      59,    59,    23,    24,    25,    26,    27,     0,     0,    21,
      29,    28,    46,    51,    54,    14,     0,    61,     0,    16,
       0,    50,     0,    58,    30,    38,     0,    31,    41,    59,
      34,     0,    35,    59,    42,     0,    59,     0,     0,    69,
       0,    22,    48,    47,     0,     0,    17,    15,    39,     0,
      36,    59,    49,    32,    65,    43,    55,    59,     0,    71,
       0,    68,    53,     0,    73,    40,    37,     0,    33,    45,
      72,    70,    52,    57,     0,    56
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,    19,    16,    17,    18,    58,    59,    60,    23,
      65,    80,    32,    25,    69,    27,    84,   105,    34,    24,
      26,    94,    30,    28,    85,   117,    81,    33,    61,    70
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -71
static const yytype_int16 yypact[] =
{
      19,   -71,   462,   -71,     2,   462,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,   -71,   -71,   -71,     8,   412,   -71,    14,
     382,   -71,   -71,   -71,    -9,   -71,    -9,   -71,    -9,   -71,
      -1,   -71,   -71,    -9,   462,     4,   -71,   -71,   -71,   382,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   117,   324,   -71,
     -71,   -71,   -71,   -71,   -71,   -71,   -13,   -71,   382,   -71,
      -2,   -71,   437,   -71,   -71,   -71,    -1,   -71,   -71,     5,
     -71,    -9,   -71,   -71,     3,    -9,   -71,   382,   146,   -71,
     175,   -71,   -71,   -71,   204,     6,   -71,   -71,   -71,    -2,
     -71,   -71,   -71,   -71,   -71,   -71,   -71,   -71,   353,   -71,
      88,   -71,   -71,   233,   -71,   -71,   -71,   262,   -71,   -71,
     -71,   -71,   -71,   -71,   291,   -71
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -71,   -71,   -71,   -71,     9,     1,   -49,   -36,   -71,    -8,
     -70,   -71,    10,    11,   -55,    -7,   -71,   -71,    -5,    -3,
     -71,   -71,   -17,   -71,   -71,   -71,   -71,   -71,   -19,   -51
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -7
static const yytype_int8 yytable[] =
{
      20,    29,    21,    74,    31,    62,    22,    63,    90,    64,
      35,    36,    92,   103,    71,    96,   107,    38,    37,    57,
       1,    98,    91,    57,    76,    99,    66,    79,   101,    73,
      67,    67,    95,    68,    77,   104,    78,   118,   108,    68,
      83,   114,    86,    72,   115,   113,    87,    93,    35,     2,
       3,    75,    74,     0,    91,     0,     0,    82,     0,     0,
       0,     0,   102,     0,     0,     0,   106,     0,   124,     0,
       0,   100,    91,    37,    74,     0,     0,    91,     0,     0,
      66,     0,     0,    66,     0,     0,     0,     0,    91,    39,
       0,   120,     0,   116,     0,     0,     0,    40,    41,     0,
      42,    43,    44,    45,    66,    46,    47,     0,    48,    49,
      50,    51,     0,    52,    53,    54,    55,    56,    88,     0,
      57,   121,     0,     0,     0,     0,    40,    41,     0,    42,
      43,    44,    45,     0,    46,    47,     0,    48,    49,    50,
      51,     0,    52,    53,    54,    55,    56,    39,     0,    57,
      89,     0,     0,     0,     0,    40,    41,     0,    42,    43,
      44,    45,     0,    46,    47,     0,    48,    49,    50,    51,
       0,    52,    53,    54,    55,    56,   110,     0,    57,   109,
       0,     0,     0,     0,    40,    41,     0,    42,    43,    44,
      45,     0,    46,    47,     0,    48,    49,    50,    51,     0,
      52,    53,    54,    55,    56,    39,     0,    57,   111,     0,
       0,     0,     0,    40,    41,     0,    42,    43,    44,    45,
       0,    46,    47,     0,    48,    49,    50,    51,     0,    52,
      53,    54,    55,    56,    39,     0,    57,   112,     0,     0,
       0,     0,    40,    41,     0,    42,    43,    44,    45,     0,
      46,    47,     0,    48,    49,    50,    51,     0,    52,    53,
      54,    55,    56,    39,     0,    57,   122,     0,     0,     0,
       0,    40,    41,     0,    42,    43,    44,    45,     0,    46,
      47,     0,    48,    49,    50,    51,     0,    52,    53,    54,
      55,    56,    39,     0,    57,   123,     0,     0,     0,     0,
      40,    41,     0,    42,    43,    44,    45,     0,    46,    47,
       0,    48,    49,    50,    51,     0,    52,    53,    54,    55,
      56,     0,     0,    57,   125,    39,     0,    -5,     0,     0,
       0,     0,     0,    40,    41,     0,    42,    43,    44,    45,
       0,    46,    47,     0,    48,    49,    50,    51,     0,    52,
      53,    54,    55,    56,    39,     0,    57,     0,     0,     0,
       0,     0,    40,    41,     0,    42,    43,    44,    45,     0,
      46,    47,     0,    48,    49,    50,    51,   119,    52,    53,
      54,    55,    56,    39,     0,    57,     0,     0,     0,     0,
       0,    40,    41,     0,    42,    43,    44,    45,     0,    46,
      47,     0,    48,    49,    50,    51,     0,    52,    53,    54,
      55,    56,     0,     5,    57,    -6,     0,     6,     7,     8,
       9,     0,    10,    11,     0,     0,     0,     0,    12,     0,
       0,     0,     0,     0,     0,    13,     0,    14,     5,     0,
      15,     0,     6,     7,     8,     9,     0,    10,    11,     0,
       0,     0,     0,    12,     0,     0,     0,     0,     0,     0,
      13,    97,    14,     5,     0,    15,     0,     6,     7,     8,
       9,     0,    10,    11,     0,     0,     0,     0,    12,     0,
       0,     0,     0,     0,     0,    13,     0,    14,     0,     0,
      15
};

static const yytype_int8 yycheck[] =
{
       3,     9,     0,    39,    11,    24,     5,    26,    57,    28,
      13,     3,    25,    83,    33,    70,    86,     3,    17,    32,
       1,    76,    58,    32,    41,    76,    29,    44,    79,    25,
      32,    32,    68,    34,    42,    32,    43,   107,    87,    34,
      48,    35,    50,    34,    99,    94,    51,    66,    51,    30,
      31,    40,    88,    -1,    90,    -1,    -1,    47,    -1,    -1,
      -1,    -1,    81,    -1,    -1,    -1,    85,    -1,   117,    -1,
      -1,    79,   108,    72,   110,    -1,    -1,   113,    -1,    -1,
      83,    -1,    -1,    86,    -1,    -1,    -1,    -1,   124,     1,
      -1,     3,    -1,   101,    -1,    -1,    -1,     9,    10,    -1,
      12,    13,    14,    15,   107,    17,    18,    -1,    20,    21,
      22,    23,    -1,    25,    26,    27,    28,    29,     1,    -1,
      32,    33,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      13,    14,    15,    -1,    17,    18,    -1,    20,    21,    22,
      23,    -1,    25,    26,    27,    28,    29,     1,    -1,    32,
      33,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,
      14,    15,    -1,    17,    18,    -1,    20,    21,    22,    23,
      -1,    25,    26,    27,    28,    29,     1,    -1,    32,    33,
      -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,
      15,    -1,    17,    18,    -1,    20,    21,    22,    23,    -1,
      25,    26,    27,    28,    29,     1,    -1,    32,    33,    -1,
      -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,    15,
      -1,    17,    18,    -1,    20,    21,    22,    23,    -1,    25,
      26,    27,    28,    29,     1,    -1,    32,    33,    -1,    -1,
      -1,    -1,     9,    10,    -1,    12,    13,    14,    15,    -1,
      17,    18,    -1,    20,    21,    22,    23,    -1,    25,    26,
      27,    28,    29,     1,    -1,    32,    33,    -1,    -1,    -1,
      -1,     9,    10,    -1,    12,    13,    14,    15,    -1,    17,
      18,    -1,    20,    21,    22,    23,    -1,    25,    26,    27,
      28,    29,     1,    -1,    32,    33,    -1,    -1,    -1,    -1,
       9,    10,    -1,    12,    13,    14,    15,    -1,    17,    18,
      -1,    20,    21,    22,    23,    -1,    25,    26,    27,    28,
      29,    -1,    -1,    32,    33,     1,    -1,     3,    -1,    -1,
      -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,    15,
      -1,    17,    18,    -1,    20,    21,    22,    23,    -1,    25,
      26,    27,    28,    29,     1,    -1,    32,    -1,    -1,    -1,
      -1,    -1,     9,    10,    -1,    12,    13,    14,    15,    -1,
      17,    18,    -1,    20,    21,    22,    23,    24,    25,    26,
      27,    28,    29,     1,    -1,    32,    -1,    -1,    -1,    -1,
      -1,     9,    10,    -1,    12,    13,    14,    15,    -1,    17,
      18,    -1,    20,    21,    22,    23,    -1,    25,    26,    27,
      28,    29,    -1,     1,    32,     3,    -1,     5,     6,     7,
       8,    -1,    10,    11,    -1,    -1,    -1,    -1,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    25,     1,    -1,
      28,    -1,     5,     6,     7,     8,    -1,    10,    11,    -1,
      -1,    -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      23,    24,    25,     1,    -1,    28,    -1,     5,     6,     7,
       8,    -1,    10,    11,    -1,    -1,    -1,    -1,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    23,    -1,    25,    -1,    -1,
      28
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    30,    31,    37,     1,     5,     6,     7,     8,
      10,    11,    16,    23,    25,    28,    39,    40,    41,    38,
      55,     0,    41,    45,    55,    49,    56,    51,    59,    45,
      58,    51,    48,    63,    54,    55,     3,    41,     3,     1,
       9,    10,    12,    13,    14,    15,    17,    18,    20,    21,
      22,    23,    25,    26,    27,    28,    29,    32,    42,    43,
      44,    64,    64,    64,    64,    46,    55,    32,    34,    50,
      65,    64,    40,    25,    43,    49,    58,    45,    51,    58,
      47,    62,    48,    45,    52,    60,    45,    54,     1,    33,
      42,    43,    25,    64,    57,    43,    50,    24,    50,    65,
      45,    65,    64,    46,    32,    53,    64,    46,    42,    33,
       1,    33,    33,    42,    35,    50,    45,    61,    46,    24,
       3,    33,    33,    33,    42,    33
};

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
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
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
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {
      case 5: /* "SECTIONHEADER" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1413 "gramRd.c"
	break;
      case 6: /* "RSECTIONHEADER" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1418 "gramRd.c"
	break;
      case 7: /* "VSECTIONHEADER" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1423 "gramRd.c"
	break;
      case 8: /* "SECTIONHEADER2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1428 "gramRd.c"
	break;
      case 9: /* "RCODEMACRO" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1433 "gramRd.c"
	break;
      case 10: /* "SEXPR" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1438 "gramRd.c"
	break;
      case 12: /* "LATEXMACRO" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1443 "gramRd.c"
	break;
      case 13: /* "VERBMACRO" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1448 "gramRd.c"
	break;
      case 14: /* "OPTMACRO" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1453 "gramRd.c"
	break;
      case 15: /* "ESCAPE" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1458 "gramRd.c"
	break;
      case 16: /* "LISTSECTION" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1463 "gramRd.c"
	break;
      case 17: /* "ITEMIZE" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1468 "gramRd.c"
	break;
      case 18: /* "DESCRIPTION" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1473 "gramRd.c"
	break;
      case 19: /* "NOITEM" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1478 "gramRd.c"
	break;
      case 20: /* "LATEXMACRO2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1483 "gramRd.c"
	break;
      case 21: /* "VERBMACRO2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1488 "gramRd.c"
	break;
      case 22: /* "LATEXMACRO3" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1493 "gramRd.c"
	break;
      case 23: /* "IFDEF" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1498 "gramRd.c"
	break;
      case 24: /* "ENDIF" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1503 "gramRd.c"
	break;
      case 25: /* "TEXT" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1508 "gramRd.c"
	break;
      case 26: /* "RCODE" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1513 "gramRd.c"
	break;
      case 27: /* "VERB" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1518 "gramRd.c"
	break;
      case 28: /* "COMMENT" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1523 "gramRd.c"
	break;
      case 29: /* "UNKNOWN" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1528 "gramRd.c"
	break;
      case 30: /* "STARTFILE" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1533 "gramRd.c"
	break;
      case 31: /* "STARTFRAGMENT" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1538 "gramRd.c"
	break;
      case 42: /* "ArgItems" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1543 "gramRd.c"
	break;
      case 45: /* "LatexArg" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1548 "gramRd.c"
	break;
      case 50: /* "RLikeArg2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1553 "gramRd.c"
	break;
      case 52: /* "VerbatimArg1" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1558 "gramRd.c"
	break;
      case 53: /* "VerbatimArg2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1563 "gramRd.c"
	break;
      case 54: /* "IfDefTarget" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1568 "gramRd.c"
	break;
      case 55: /* "goLatexLike" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1573 "gramRd.c"
	break;
      case 56: /* "goRLike" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1578 "gramRd.c"
	break;
      case 57: /* "goRLike2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1583 "gramRd.c"
	break;
      case 58: /* "goOption" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1588 "gramRd.c"
	break;
      case 59: /* "goVerbatim" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1593 "gramRd.c"
	break;
      case 60: /* "goVerbatim1" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1598 "gramRd.c"
	break;
      case 61: /* "goVerbatim2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1603 "gramRd.c"
	break;
      case 62: /* "goItem0" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1608 "gramRd.c"
	break;
      case 63: /* "goItem2" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1613 "gramRd.c"
	break;
      case 65: /* "Option" */
#line 167 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1618 "gramRd.c"
	break;

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

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
  yylsp = yyls;
#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

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

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
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
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
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

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
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

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 177 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; ;}
    break;

  case 3:
#line 178 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; ;}
    break;

  case 4:
#line 179 "gramRd.y"
    { PROTECT(Value = R_NilValue);  YYABORT; ;}
    break;

  case 5:
#line 182 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); UNPROTECT_PTR((yyvsp[(1) - (2)])); ;}
    break;

  case 6:
#line 185 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 7:
#line 188 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 8:
#line 189 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 9:
#line 191 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 10:
#line 192 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), HAS_SEXPR, &(yyloc)); ;}
    break;

  case 11:
#line 193 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 12:
#line 194 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 13:
#line 195 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 14:
#line 196 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); ;}
    break;

  case 15:
#line 197 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); ;}
    break;

  case 16:
#line 198 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 17:
#line 199 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), STATIC, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 18:
#line 200 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); ;}
    break;

  case 19:
#line 201 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); ;}
    break;

  case 20:
#line 202 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 21:
#line 204 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 22:
#line 205 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 23:
#line 207 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); ;}
    break;

  case 24:
#line 208 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), RCODE, &(yyloc)); ;}
    break;

  case 25:
#line 209 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), VERB, &(yyloc)); ;}
    break;

  case 26:
#line 210 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); ;}
    break;

  case 27:
#line 211 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), UNKNOWN, &(yyloc)); yyerror(yyunknown); ;}
    break;

  case 28:
#line 212 "gramRd.y"
    { (yyval) = xxmarkup(R_NilValue, (yyvsp[(1) - (1)]), STATIC, &(yyloc)); ;}
    break;

  case 29:
#line 213 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 30:
#line 214 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 31:
#line 216 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 32:
#line 217 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); ;}
    break;

  case 33:
#line 218 "gramRd.y"
    { (yyval) = xxmarkup3((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), STATIC, &(yyloc)); ;}
    break;

  case 34:
#line 219 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 35:
#line 220 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 36:
#line 221 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), STATIC, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 37:
#line 222 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), STATIC, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 38:
#line 223 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 39:
#line 224 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 40:
#line 225 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 41:
#line 226 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); ;}
    break;

  case 42:
#line 227 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), R_NilValue, 1, STATIC, &(yyloc)); ;}
    break;

  case 43:
#line 228 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); ;}
    break;

  case 44:
#line 229 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (1)]), R_NilValue, STATIC, &(yyloc)); ;}
    break;

  case 45:
#line 230 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); ;}
    break;

  case 46:
#line 232 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 47:
#line 234 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 48:
#line 235 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist((yyvsp[(2) - (2)])); 
     						  if(wCalls)
    	    					      warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, (yylsp[(2) - (2)]).first_line, (yylsp[(2) - (2)]).first_column); 
     						  else
    	    					      warningcall(R_NilValue, _("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, (yylsp[(2) - (2)]).first_line, (yylsp[(2) - (2)]).first_column); 
						;}
    break;

  case 49:
#line 244 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 50:
#line 246 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 51:
#line 248 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 52:
#line 252 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 53:
#line 253 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); ;}
    break;

  case 54:
#line 255 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 55:
#line 257 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 56:
#line 261 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 57:
#line 262 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); ;}
    break;

  case 58:
#line 264 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist((yyvsp[(2) - (2)])); ;}
    break;

  case 59:
#line 267 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); ;}
    break;

  case 60:
#line 269 "gramRd.y"
    { (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); ;}
    break;

  case 61:
#line 271 "gramRd.y"
    { xxbraceDepth--; (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); xxbraceDepth++; ;}
    break;

  case 62:
#line 273 "gramRd.y"
    { (yyval) = xxpushMode(INOPTION, UNKNOWN, FALSE); ;}
    break;

  case 63:
#line 275 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); ;}
    break;

  case 64:
#line 277 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, TRUE); ;}
    break;

  case 65:
#line 279 "gramRd.y"
    { xxbraceDepth--; (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); xxbraceDepth++; ;}
    break;

  case 66:
#line 281 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, ESCAPE, FALSE); ;}
    break;

  case 67:
#line 283 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); ;}
    break;

  case 68:
#line 285 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 69:
#line 286 "gramRd.y"
    { (yyval) = xxnewlist(NULL); ;}
    break;

  case 70:
#line 287 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); ;}
    break;

  case 71:
#line 288 "gramRd.y"
    { (yyval) = xxnewlist(NULL); ;}
    break;

  case 72:
#line 289 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); ;}
    break;

  case 73:
#line 291 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2309 "gramRd.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

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
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, &yylloc);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
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

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

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
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 293 "gramRd.y"


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

static int getDynamicFlag(SEXP item)
{
    SEXP flag = getAttrib(item, install("dynamicFlag"));
    if (isNull(flag)) return 0;
    else return INTEGER(flag)[0];
}

static void setDynamicFlag(SEXP item, int flag)
{
    if (flag)
    	setAttrib(item, install("dynamicFlag"), ScalarInteger(flag));
}

static SEXP xxnewlist(SEXP item)
{
    SEXP ans, tmp;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PROTECT(tmp = NewList());
    if (item) {
    	int flag = getDynamicFlag(item);
    	PROTECT(ans = GrowList(tmp, item));
    	setDynamicFlag(ans, flag);
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
    int flag = getDynamicFlag(oldlist) | getDynamicFlag(item);
#if DEBUGVALS
    Rprintf("xxlist(oldlist=%p, item=%p)", oldlist, item);
#endif
    PROTECT(ans = GrowList(oldlist, item));
    UNPROTECT_PTR(item);
    UNPROTECT_PTR(oldlist);
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p is length %d\n", ans, length(ans));
#endif
    return ans;
}

static SEXP xxmarkup(SEXP header, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup(header=%p, body=%p)", header, body);    
#endif
    if (isNull(body)) 
        PROTECT(ans = allocVector(VECSXP, 0));
    else {
        flag |= getDynamicFlag(body);
	PROTECT(ans = PairToVectorList(CDR(body)));
    	UNPROTECT_PTR(body);	
    }
    if (isNull(header))
    	PROTECT(header = mkString("LIST"));
    	
    setAttrib(ans, install("Rd_tag"), header);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    UNPROTECT_PTR(header);
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxOptionmarkup(SEXP header, SEXP option, SEXP body, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxOptionmarkup(header=%p, option=%p, body=%p)", header, option, body);    
#endif
    flag |= getDynamicFlag(body);
    PROTECT(ans = PairToVectorList(CDR(body)));
    UNPROTECT_PTR(body);	
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);
    flag |= getDynamicFlag(option);
    setAttrib(ans, install("Rd_option"), option);
    UNPROTECT_PTR(option);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);    
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup2(SEXP header, SEXP body1, SEXP body2, int argcount, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p)", header, body1, body2);        
#endif
    
    PROTECT(ans = allocVector(VECSXP, argcount));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	if (argcount < 2) error("internal error: inconsistent argument count");
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    	UNPROTECT_PTR(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmarkup3(SEXP header, SEXP body1, SEXP body2, SEXP body3, int flag, YYLTYPE *lloc)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmarkup2(header=%p, body1=%p, body2=%p, body3=%p)", header, body1, body2, body3);        
#endif
    
    PROTECT(ans = allocVector(VECSXP, 3));
    if (!isNull(body1)) {
    	int flag1 = getDynamicFlag(body1);
    	SET_VECTOR_ELT(ans, 0, PairToVectorList(CDR(body1)));
    	UNPROTECT_PTR(body1);
    	setDynamicFlag(VECTOR_ELT(ans, 0), flag1);
    	flag |= flag1;
    }
    if (!isNull(body2)) {
    	int flag2;
	flag2 = getDynamicFlag(body2);
    	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body2)));    
    	UNPROTECT_PTR(body2);
    	setDynamicFlag(VECTOR_ELT(ans, 1), flag2);
    	flag |= flag2;
    }
    if (!isNull(body3)) {
    	int flag3;
	flag3 = getDynamicFlag(body3);
    	SET_VECTOR_ELT(ans, 2, PairToVectorList(CDR(body3)));    
    	UNPROTECT_PTR(body3);
    	setDynamicFlag(VECTOR_ELT(ans, 2), flag3);
    	flag |= flag3;
    }    
    setAttrib(ans, install("Rd_tag"), header);
    UNPROTECT_PTR(header);    
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    setDynamicFlag(ans, flag);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static void xxsavevalue(SEXP Rd, YYLTYPE *lloc)
{
    int flag = getDynamicFlag(Rd);
    PROTECT(Value = PairToVectorList(CDR(Rd)));
    if (!isNull(Value)) {
    	setAttrib(Value, R_ClassSymbol, mkString("Rd"));
    	setAttrib(Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    	setDynamicFlag(Value, flag);
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
    if (xxNewlineInString) {
	if(wCalls)
	    warning(_("newline within quoted string at %s:%d"), 
		    xxBasename, xxNewlineInString);
	else
	    warningcall(R_NilValue,
			_("newline within quoted string at %s:%d"), 
			xxBasename, xxNewlineInString);
    }
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
    if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF)
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
    cetype_t enc = CE_UTF8;

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
 
static SEXP ParseRd(ParseStatus *status, SEXP srcfile, Rboolean fragment)
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
    if (fragment) xxinitvalue = STARTFRAGMENT;
    else	  xxinitvalue = STARTFILE;
    
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
SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile, Rboolean fragment)
{
    con_parse = con;
    ptr_getc = con_getc;
    return ParseRd(status, srcfile, fragment);
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
    
    /* These sections contain verbatim text */
    
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
    { "\\command", LATEXMACRO },
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
    { "\\if",      LATEXMACRO2 },
    { "\\method",  LATEXMACRO2 },
    { "\\S3method",LATEXMACRO2 },
    { "\\S4method",LATEXMACRO2 },
    { "\\tabular", LATEXMACRO2 },
    
    /* This macro takes three LaTeX-like arguments. */
    
    { "\\ifelse",  LATEXMACRO3 },
    
    /* These macros take one optional bracketed option and always take 
       one LaTeX-like argument */
       
    { "\\link",    OPTMACRO },
       
    /* These markup macros require an R-like text argument */
    
    { "\\code",    RCODEMACRO },
    { "\\dontshow",RCODEMACRO },
    { "\\donttest",RCODEMACRO },
    { "\\testonly",RCODEMACRO },
    
    /* This macro take one optional bracketed option and one R-like argument */
    
    { "\\Sexpr",   SEXPR },
    
    /* This is just like a VSECTIONHEADER, but it needs SEXPR processing */
    
    { "\\RdOpts",   RDOPTS },
    
    /* These macros take one verbatim arg and ignore everything except braces */
    
    { "\\dontrun", VERBMACRO }, /* at least for now */    
    { "\\env",     VERBMACRO },
    { "\\kbd", 	   VERBMACRO },	
    { "\\option",  VERBMACRO },
    { "\\out",     VERBMACRO },
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
#define YYENGLISH 17
	"$undefined",	"input", 	
	"SECTIONHEADER","section header",
	"RSECTIONHEADER","section header",
	"VSECTIONHEADER","section header",
	"LISTSECTION",	"section header",
	
	"LATEXMACRO",	"macro",
	"LATEXMACRO2",  "macro",
	"LATEXMACRO3",  "macro",
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
    static char const yyshortunexpected[] = "unexpected %s";
    static char const yylongunexpected[] = "unexpected %s '%s'";
    char *expecting;
    char ParseErrorMsg[PARSE_ERROR_SIZE];
    SEXP filename;
    char ParseErrorFilename[PARSE_ERROR_SIZE];
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("macro");
    _("conditional");
    _("section header");
#endif 
   
    xxWarnNewline();	/* post newline warning if necessary */
    
    /*
    R_ParseError     = yylloc.first_line;
    R_ParseErrorCol  = yylloc.first_column;
    R_ParseErrorFile = SrcFile;
    */
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i, translated = FALSE;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i = 0; yytname_translations[i]; i += 2) {
    	    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
    	        sprintf(ParseErrorMsg, yychar < 256 ? _(yyshortunexpected): _(yylongunexpected), 
    	    	        i/2 < YYENGLISH ? _(yytname_translations[i+1])
    	    	                    : yytname_translations[i+1], CHAR(STRING_ELT(yylval, 0)));
    	    	translated = TRUE;
    	    	break;
    	    }
    	}
    	if (!translated)
    	    sprintf(ParseErrorMsg, yychar < 256 ? _(yyshortunexpected) : _(yylongunexpected),
    	                             s + sizeof yyunexpected - 1, CHAR(STRING_ELT(yylval, 0)));
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
    	sprintf(ParseErrorMsg, "%s '%s'", s, CHAR(STRING_ELT(yylval, 0)));
    } else {
    	sprintf(ParseErrorMsg, "%s", s);
    }
    filename = findVar(install("filename"), SrcFile);
    if (!isNull(filename))
    	strncpy(ParseErrorFilename, CHAR(STRING_ELT(filename, 0)), PARSE_ERROR_SIZE - 1);
    else
        ParseErrorFilename[0] = '\0';
    if (wCalls) {
	if (yylloc.first_line != yylloc.last_line)
	    warning("%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warning("%s:%d: %s", 
		    ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
    } else {
	if (yylloc.first_line != yylloc.last_line)
	    warningcall(R_NilValue, "%s:%d-%d: %s", 
		    ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
	else
	    warningcall(R_NilValue, "%s:%d: %s", 
			ParseErrorFilename, yylloc.first_line, ParseErrorMsg);
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

    if (xxinitvalue) {
        yylloc.first_line = 0;
        yylloc.first_column = 0;
        yylloc.first_byte = 0;
        yylloc.last_line = 0;
        yylloc.last_column = 0;
        yylloc.last_byte = 0;
    	PROTECT(yylval = mkString(""));
        c = xxinitvalue;
    	xxinitvalue = 0;
    	return(c);
    }
    
    setfirstloc();    
    c = xxgetc();

    switch (c) {
    	case '%': if (!xxinEqn) return mkComment(c);
    	    break;
	case '\\':
	    if (!xxinEqn) {
		lookahead = xxungetc(xxgetc());
		if (isalpha(lookahead) && xxmode != VERBATIM 
		    /* In R strings, only link or var is allowed as markup */
		    && (lookahead == 'l' || lookahead == 'v' || !xxinRString)) 
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
    
    xxungetc(c);
    
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
    	    	    } else {
    	    	    	xxungetc(lookahead); /* put back the 4th char */
    	    	    	xxungetc('\\');	     /* and the 3rd */
    	    	    }
    	    	} else if (lookahead == xxinRString) { /* There could be one or two before this */
    	    	    TEXT_PUSH(c);
    	    	    c = lookahead;
    	    	    escaped = 1;
    	    	} else if (!escaped && (lookahead == 'l' || lookahead == 'v')) { 
    	    	    /* assume \link or \var; this breaks vertical tab, but does anyone ever use that? */
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
    int retval = 0, attempt = 0;
    
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

 .Internal( parseRd(file, srcfile, encoding, verbose, basename, warningCalls) )
 If there is text then that is read and the other arguments are ignored.
*/

SEXP attribute_hidden do_parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s = R_NilValue, source;
    Rconnection con;
    Rboolean wasopen, fragment;
    int ifile, wcall;
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
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    xxDebugTokens = asInteger(CAR(args));		args = CDR(args);
    xxBasename = CHAR(STRING_ELT(CAR(args), 0));	args = CDR(args);
    fragment = asLogical(CAR(args));			args = CDR(args);
    wcall = asLogical(CAR(args));
    if (wcall == NA_LOGICAL)
    	error(_("invalid '%s' value"), "warningCalls");
    wCalls = wcall;

    if (ifile >= 3) {/* file != "" */
	if(!wasopen) {
	    if(!con->open(con)) error(_("cannot open the connection"));
	    if(!con->canread) {
		con->close(con);
		error(_("cannot read from this connection"));
	    }
	} else if(!con->canread)
	    error(_("cannot read from this connection"));
	s = R_ParseRd(con, &status, source, fragment);
	if(!wasopen) con->close(con);
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else error(_("invalid Rd file"));
    return s;
}

/* "do_deparseRd" 

 .Internal( deparseRd(element, state) )
*/

SEXP attribute_hidden do_deparseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP e, state, result;
    int  outlen, *statevals, quoteBraces, inRComment;
    const char *c;
    char *outbuf, *out, lookahead;
    Rboolean escape;

    checkArity(op, args);
    
    e = CAR(args);                       args = CDR(args);
    if(!isString(e) || length(e) != 1) 
    	error(_("deparseRd only supports deparsing character elements"));
    e = STRING_ELT(e, 0);
    
    state = CAR(args);
    if(!isInteger(state) || length(state) != 5) error(_("bad state"));
    xxbraceDepth = INTEGER(state)[0];
    xxinRString = INTEGER(state)[1];
    xxmode = INTEGER(state)[2];
    xxinEqn = INTEGER(state)[3];
    quoteBraces = INTEGER(state)[4];
    
    
    if (xxmode != LATEXLIKE && xxmode != RLIKE && xxmode != VERBATIM && xxmode != COMMENTMODE 
     && xxmode != INOPTION  && xxmode != UNKNOWNMODE)
    	error(_("bad text mode %d in deparseRd"), xxmode);
    
    for (c = CHAR(e), outlen=0; *c; c++) {
    	outlen++;
    	/* any special char might be escaped */
    	if (*c == '{' || *c == '}' || *c == '%' || *c == '\\') outlen++;
    }
    out = outbuf = R_chk_calloc(outlen+1, sizeof(char));
    inRComment = FALSE;
    for (c = CHAR(e); *c; c++) {
    	escape = FALSE;
    	if (xxmode != UNKNOWNMODE) {
	    switch (*c) {
	    case '\\':
		if (xxmode == RLIKE && xxinRString) {
		    lookahead = *(c+1);
		    if (lookahead == '\\' || lookahead == xxinRString || lookahead == 'l') 
		    	escape = TRUE;
		    break;
		}          /* fall through to % case for non-strings... */    
	    case '%':
		if (xxmode != COMMENTMODE && !xxinEqn)
		    escape = TRUE;
		break;
	    case LBRACE:
	    case RBRACE:
		if (quoteBraces)
		    escape = TRUE;
		else if (!xxinRString && !xxinEqn && (xxmode == RLIKE || xxmode == VERBATIM)) {
		    if (*c == LBRACE) xxbraceDepth++;
		    else if (xxbraceDepth <= 0) escape = TRUE;
		    else xxbraceDepth--;
		}
		break;
	    case '\'':
	    case '"':
	    case '`':
	    	if (xxmode == RLIKE) {
		    if (xxinRString) {
			if (xxinRString == *c) xxinRString = 0;
		    } else if (!inRComment) xxinRString = *c;
		}
		break;
	    case '#':
	    	if (xxmode == RLIKE && !xxinRString) 
	    	    inRComment = TRUE;
	    	break;
	    case '\n':
	    	inRComment = FALSE;
	    	break;
	    }
	}
    	if (escape)
    	    *out++ = '\\';
    	*out++ = *c;
    }
    *out = '\0';
    PROTECT(result = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, ScalarString(mkChar(outbuf)));
    SET_VECTOR_ELT(result, 1, duplicate(state));
    R_chk_free(outbuf);

    statevals = INTEGER( VECTOR_ELT(result, 1) );
    statevals[0] = xxbraceDepth;
    statevals[1] = xxinRString;
    UNPROTECT(1);
    return result;
}


