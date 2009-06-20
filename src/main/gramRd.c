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
     LATEXMACRO = 266,
     VERBMACRO = 267,
     OPTMACRO = 268,
     ESCAPE = 269,
     LISTSECTION = 270,
     ITEMIZE = 271,
     DESCRIPTION = 272,
     NOITEM = 273,
     LATEXMACRO2 = 274,
     VERBMACRO2 = 275,
     IFDEF = 276,
     ENDIF = 277,
     TEXT = 278,
     RCODE = 279,
     VERB = 280,
     COMMENT = 281,
     UNKNOWN = 282,
     STARTFILE = 283,
     STARTFRAGMENT = 284
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
#define LATEXMACRO 266
#define VERBMACRO 267
#define OPTMACRO 268
#define ESCAPE 269
#define LISTSECTION 270
#define ITEMIZE 271
#define DESCRIPTION 272
#define NOITEM 273
#define LATEXMACRO2 274
#define VERBMACRO2 275
#define IFDEF 276
#define ENDIF 277
#define TEXT 278
#define RCODE 279
#define VERB 280
#define COMMENT 281
#define UNKNOWN 282
#define STARTFILE 283
#define STARTFRAGMENT 284




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
static int	xxinitvalue;
static char const yyunknown[] = "unknown macro"; /* our message, not bison's */


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
#line 310 "gramRd.c"

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
#define YYFINAL  20
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   444

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  34
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  30
/* YYNRULES -- Number of rules.  */
#define YYNRULES  71
/* YYNRULES -- Number of states.  */
#define YYNSTATES  120

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   284

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
       2,    32,     2,    33,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    30,     2,    31,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint8 yyprhs[] =
{
       0,     0,     3,     7,    11,    13,    16,    18,    20,    23,
      26,    29,    32,    35,    39,    44,    48,    53,    55,    57,
      60,    62,    65,    67,    69,    71,    73,    75,    77,    79,
      82,    85,    89,    92,    95,    99,   104,   107,   111,   116,
     119,   122,   126,   128,   133,   136,   139,   142,   145,   148,
     151,   156,   160,   163,   166,   171,   175,   178,   179,   180,
     181,   182,   183,   184,   185,   186,   187,   191,   194,   199,
     203,   208
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      35,     0,    -1,    28,    37,     3,    -1,    29,    36,     3,
      -1,     1,    -1,    53,    40,    -1,    38,    -1,    39,    -1,
      38,    39,    -1,     7,    49,    -1,     6,    47,    -1,     5,
      43,    -1,    15,    46,    -1,     8,    43,    44,    -1,    21,
      52,    38,    22,    -1,    10,    56,    48,    -1,    10,    56,
      63,    48,    -1,    26,    -1,    23,    -1,     1,    39,    -1,
      41,    -1,    40,    41,    -1,    23,    -1,    24,    -1,    25,
      -1,    26,    -1,    27,    -1,    62,    -1,    42,    -1,     1,
      41,    -1,    11,    43,    -1,    19,    43,    44,    -1,    16,
      45,    -1,    17,    46,    -1,    13,    56,    43,    -1,    13,
      56,    63,    43,    -1,     9,    47,    -1,    10,    56,    48,
      -1,    10,    56,    63,    48,    -1,    12,    49,    -1,    20,
      50,    -1,    20,    50,    51,    -1,    14,    -1,    21,    52,
      40,    22,    -1,    53,    62,    -1,    53,    62,    -1,    53,
      23,    -1,    60,    62,    -1,    61,    62,    -1,    54,    62,
      -1,    30,    55,    40,    31,    -1,    30,    55,    31,    -1,
      57,    62,    -1,    58,    62,    -1,    30,    59,    40,    31,
      -1,    30,    59,    31,    -1,    53,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    30,    40,    31,
      -1,    30,    31,    -1,    30,    40,     1,    31,    -1,    30,
       1,    31,    -1,    30,    40,     1,     3,    -1,    32,    41,
      33,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   158,   158,   159,   160,   163,   166,   169,   170,   172,
     173,   174,   175,   176,   177,   178,   179,   180,   181,   182,
     184,   185,   187,   188,   189,   190,   191,   192,   193,   194,
     196,   197,   198,   199,   200,   201,   202,   203,   204,   205,
     206,   207,   208,   209,   211,   213,   214,   218,   220,   222,
     226,   227,   229,   231,   235,   236,   238,   241,   243,   245,
     247,   249,   251,   253,   255,   257,   259,   260,   261,   262,
     263,   265
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "SECTIONHEADER",
  "RSECTIONHEADER", "VSECTIONHEADER", "SECTIONHEADER2", "RCODEMACRO",
  "SEXPR", "LATEXMACRO", "VERBMACRO", "OPTMACRO", "ESCAPE", "LISTSECTION",
  "ITEMIZE", "DESCRIPTION", "NOITEM", "LATEXMACRO2", "VERBMACRO2", "IFDEF",
  "ENDIF", "TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "STARTFILE",
  "STARTFRAGMENT", "'{'", "'}'", "'['", "']'", "$accept", "Init",
  "RdFragment", "RdFile", "SectionList", "Section", "ArgItems", "Item",
  "Markup", "LatexArg", "LatexArg2", "Item0Arg", "Item2Arg", "RLikeArg",
  "RLikeArg2", "VerbatimArg", "VerbatimArg1", "VerbatimArg2",
  "IfDefTarget", "goLatexLike", "goRLike", "goRLike2", "goOption",
  "goVerbatim", "goVerbatim1", "goVerbatim2", "goItem0", "goItem2", "Arg",
  "Option", 0
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
     123,   125,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    34,    35,    35,    35,    36,    37,    38,    38,    39,
      39,    39,    39,    39,    39,    39,    39,    39,    39,    39,
      40,    40,    41,    41,    41,    41,    41,    41,    41,    41,
      42,    42,    42,    42,    42,    42,    42,    42,    42,    42,
      42,    42,    42,    42,    43,    44,    44,    45,    46,    47,
      48,    48,    49,    50,    51,    51,    52,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,    62,    62,    62,
      62,    63
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     3,     1,     2,     1,     1,     2,     2,
       2,     2,     2,     3,     4,     3,     4,     1,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     3,     2,     2,     3,     4,     2,     3,     4,     2,
       2,     3,     1,     4,     2,     2,     2,     2,     2,     2,
       4,     3,     2,     2,     4,     3,     2,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     3,     2,     4,     3,
       4,     3
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     4,     0,    57,     0,     0,    57,    58,    61,    57,
      60,    65,    57,    18,    17,     0,     0,     7,     0,     0,
       1,    19,    11,     0,    10,     0,     9,     0,    57,     0,
      12,     0,     0,     0,     2,     8,     3,     0,    58,    60,
      57,    61,    60,    42,    64,    65,    57,    62,    57,    22,
      23,    24,    25,    26,     0,     0,    20,    28,    27,    44,
      49,    52,    13,     0,    59,     0,    15,     0,    48,     0,
      56,    29,    36,     0,    30,    39,    57,    32,     0,    33,
      57,    40,     0,     0,     0,    67,     0,    21,    46,    45,
       0,     0,    16,    14,    37,     0,    34,    57,    47,    31,
      63,    41,    53,     0,    69,     0,    66,    51,     0,    71,
      38,    35,     0,    43,    70,    68,    50,    55,     0,    54
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     4,    18,    15,    16,    17,    55,    56,    57,    22,
      62,    77,    30,    24,    66,    26,    81,   101,    32,    23,
      25,    90,    29,    27,    82,   112,    78,    31,    58,    67
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -55
static const yytype_int16 yypact[] =
{
       6,   -55,   418,   -55,     9,   418,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   -55,   -55,    12,   383,   -55,    19,   355,
     -55,   -55,   -55,    -3,   -55,    -3,   -55,    -3,   -55,    -7,
     -55,    -3,   418,     5,   -55,   -55,   -55,   355,   -55,   -55,
     -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   -55,   108,   301,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   -12,   -55,   355,   -55,    -1,   -55,   395,
     -55,   -55,   -55,    -7,   -55,   -55,     1,   -55,    -3,   -55,
     -55,     8,    -3,   355,   135,   -55,   162,   -55,   -55,   -55,
     189,     3,   -55,   -55,   -55,    -1,   -55,   -55,   -55,   -55,
     -55,   -55,   -55,   328,   -55,    81,   -55,   -55,   216,   -55,
     -55,   -55,   243,   -55,   -55,   -55,   -55,   -55,   270,   -55
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -55,   -55,   -55,   -55,    10,     0,   -51,   -35,   -55,    -9,
     -40,   -55,     2,     7,   -54,    11,   -55,   -55,    -5,    -2,
     -55,   -55,   -25,   -55,   -55,   -55,   -55,   -55,   -19,   -52
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -7
static const yytype_int8 yytable[] =
{
      28,    19,    71,    86,    59,    21,    60,     1,    61,    20,
      33,    88,    68,    92,    73,    34,    35,    76,    54,    94,
      87,    95,    36,    64,    97,    65,    63,    54,    70,    64,
      91,    74,   103,    65,     2,     3,   109,    80,   100,   108,
      99,   110,    69,    83,    89,    72,    33,    79,     0,    71,
       0,    87,    75,     0,     0,     0,     0,     0,     0,    98,
       0,   118,     0,   102,     0,     0,     0,    96,    87,    35,
      71,     0,     0,    87,     0,     0,     0,     0,    63,     0,
       0,     0,    37,    87,   114,     0,     0,     0,   111,     0,
      38,    39,    40,    41,    42,    43,     0,    44,    45,     0,
      46,    47,    48,     0,    49,    50,    51,    52,    53,    84,
       0,    54,   115,     0,     0,     0,     0,    38,    39,    40,
      41,    42,    43,     0,    44,    45,     0,    46,    47,    48,
       0,    49,    50,    51,    52,    53,    37,     0,    54,    85,
       0,     0,     0,     0,    38,    39,    40,    41,    42,    43,
       0,    44,    45,     0,    46,    47,    48,     0,    49,    50,
      51,    52,    53,   105,     0,    54,   104,     0,     0,     0,
       0,    38,    39,    40,    41,    42,    43,     0,    44,    45,
       0,    46,    47,    48,     0,    49,    50,    51,    52,    53,
      37,     0,    54,   106,     0,     0,     0,     0,    38,    39,
      40,    41,    42,    43,     0,    44,    45,     0,    46,    47,
      48,     0,    49,    50,    51,    52,    53,    37,     0,    54,
     107,     0,     0,     0,     0,    38,    39,    40,    41,    42,
      43,     0,    44,    45,     0,    46,    47,    48,     0,    49,
      50,    51,    52,    53,    37,     0,    54,   116,     0,     0,
       0,     0,    38,    39,    40,    41,    42,    43,     0,    44,
      45,     0,    46,    47,    48,     0,    49,    50,    51,    52,
      53,    37,     0,    54,   117,     0,     0,     0,     0,    38,
      39,    40,    41,    42,    43,     0,    44,    45,     0,    46,
      47,    48,     0,    49,    50,    51,    52,    53,     0,     0,
      54,   119,    37,     0,    -5,     0,     0,     0,     0,     0,
      38,    39,    40,    41,    42,    43,     0,    44,    45,     0,
      46,    47,    48,     0,    49,    50,    51,    52,    53,    37,
       0,    54,     0,     0,     0,     0,     0,    38,    39,    40,
      41,    42,    43,     0,    44,    45,     0,    46,    47,    48,
     113,    49,    50,    51,    52,    53,    37,     0,    54,     0,
       0,     0,     0,     0,    38,    39,    40,    41,    42,    43,
       0,    44,    45,     0,    46,    47,    48,     0,    49,    50,
      51,    52,    53,     0,     5,    54,    -6,     0,     6,     7,
       8,     9,     0,    10,     0,     0,     5,     0,    11,     0,
       6,     7,     8,     9,    12,    10,    13,     0,     0,    14,
      11,     0,     0,     0,     0,     0,    12,    93,    13,     5,
       0,    14,     0,     6,     7,     8,     9,     0,    10,     0,
       0,     0,     0,    11,     0,     0,     0,     0,     0,    12,
       0,    13,     0,     0,    14
};

static const yytype_int8 yycheck[] =
{
       9,     3,    37,    54,    23,     5,    25,     1,    27,     0,
      12,    23,    31,    67,    39,     3,    16,    42,    30,    73,
      55,    73,     3,    30,    76,    32,    28,    30,    23,    30,
      65,    40,    83,    32,    28,    29,    33,    46,    30,    90,
      80,    95,    32,    48,    63,    38,    48,    45,    -1,    84,
      -1,    86,    41,    -1,    -1,    -1,    -1,    -1,    -1,    78,
      -1,   112,    -1,    82,    -1,    -1,    -1,    76,   103,    69,
     105,    -1,    -1,   108,    -1,    -1,    -1,    -1,    80,    -1,
      -1,    -1,     1,   118,     3,    -1,    -1,    -1,    97,    -1,
       9,    10,    11,    12,    13,    14,    -1,    16,    17,    -1,
      19,    20,    21,    -1,    23,    24,    25,    26,    27,     1,
      -1,    30,    31,    -1,    -1,    -1,    -1,     9,    10,    11,
      12,    13,    14,    -1,    16,    17,    -1,    19,    20,    21,
      -1,    23,    24,    25,    26,    27,     1,    -1,    30,    31,
      -1,    -1,    -1,    -1,     9,    10,    11,    12,    13,    14,
      -1,    16,    17,    -1,    19,    20,    21,    -1,    23,    24,
      25,    26,    27,     1,    -1,    30,    31,    -1,    -1,    -1,
      -1,     9,    10,    11,    12,    13,    14,    -1,    16,    17,
      -1,    19,    20,    21,    -1,    23,    24,    25,    26,    27,
       1,    -1,    30,    31,    -1,    -1,    -1,    -1,     9,    10,
      11,    12,    13,    14,    -1,    16,    17,    -1,    19,    20,
      21,    -1,    23,    24,    25,    26,    27,     1,    -1,    30,
      31,    -1,    -1,    -1,    -1,     9,    10,    11,    12,    13,
      14,    -1,    16,    17,    -1,    19,    20,    21,    -1,    23,
      24,    25,    26,    27,     1,    -1,    30,    31,    -1,    -1,
      -1,    -1,     9,    10,    11,    12,    13,    14,    -1,    16,
      17,    -1,    19,    20,    21,    -1,    23,    24,    25,    26,
      27,     1,    -1,    30,    31,    -1,    -1,    -1,    -1,     9,
      10,    11,    12,    13,    14,    -1,    16,    17,    -1,    19,
      20,    21,    -1,    23,    24,    25,    26,    27,    -1,    -1,
      30,    31,     1,    -1,     3,    -1,    -1,    -1,    -1,    -1,
       9,    10,    11,    12,    13,    14,    -1,    16,    17,    -1,
      19,    20,    21,    -1,    23,    24,    25,    26,    27,     1,
      -1,    30,    -1,    -1,    -1,    -1,    -1,     9,    10,    11,
      12,    13,    14,    -1,    16,    17,    -1,    19,    20,    21,
      22,    23,    24,    25,    26,    27,     1,    -1,    30,    -1,
      -1,    -1,    -1,    -1,     9,    10,    11,    12,    13,    14,
      -1,    16,    17,    -1,    19,    20,    21,    -1,    23,    24,
      25,    26,    27,    -1,     1,    30,     3,    -1,     5,     6,
       7,     8,    -1,    10,    -1,    -1,     1,    -1,    15,    -1,
       5,     6,     7,     8,    21,    10,    23,    -1,    -1,    26,
      15,    -1,    -1,    -1,    -1,    -1,    21,    22,    23,     1,
      -1,    26,    -1,     5,     6,     7,     8,    -1,    10,    -1,
      -1,    -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    21,
      -1,    23,    -1,    -1,    26
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    28,    29,    35,     1,     5,     6,     7,     8,
      10,    15,    21,    23,    26,    37,    38,    39,    36,    53,
       0,    39,    43,    53,    47,    54,    49,    57,    43,    56,
      46,    61,    52,    53,     3,    39,     3,     1,     9,    10,
      11,    12,    13,    14,    16,    17,    19,    20,    21,    23,
      24,    25,    26,    27,    30,    40,    41,    42,    62,    62,
      62,    62,    44,    53,    30,    32,    48,    63,    62,    38,
      23,    41,    47,    56,    43,    49,    56,    45,    60,    46,
      43,    50,    58,    52,     1,    31,    40,    41,    23,    62,
      55,    41,    48,    22,    48,    63,    43,    63,    62,    44,
      30,    51,    62,    40,    31,     1,    31,    31,    40,    33,
      48,    43,    59,    22,     3,    31,    31,    31,    40,    31
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
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1384 "gramRd.c"
	break;
      case 6: /* "RSECTIONHEADER" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1389 "gramRd.c"
	break;
      case 7: /* "VSECTIONHEADER" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1394 "gramRd.c"
	break;
      case 8: /* "SECTIONHEADER2" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1399 "gramRd.c"
	break;
      case 9: /* "RCODEMACRO" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1404 "gramRd.c"
	break;
      case 10: /* "SEXPR" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1409 "gramRd.c"
	break;
      case 11: /* "LATEXMACRO" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1414 "gramRd.c"
	break;
      case 12: /* "VERBMACRO" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1419 "gramRd.c"
	break;
      case 13: /* "OPTMACRO" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1424 "gramRd.c"
	break;
      case 14: /* "ESCAPE" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1429 "gramRd.c"
	break;
      case 15: /* "LISTSECTION" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1434 "gramRd.c"
	break;
      case 16: /* "ITEMIZE" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1439 "gramRd.c"
	break;
      case 17: /* "DESCRIPTION" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1444 "gramRd.c"
	break;
      case 18: /* "NOITEM" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1449 "gramRd.c"
	break;
      case 19: /* "LATEXMACRO2" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1454 "gramRd.c"
	break;
      case 20: /* "VERBMACRO2" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1459 "gramRd.c"
	break;
      case 21: /* "IFDEF" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1464 "gramRd.c"
	break;
      case 22: /* "ENDIF" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1469 "gramRd.c"
	break;
      case 23: /* "TEXT" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1474 "gramRd.c"
	break;
      case 24: /* "RCODE" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1479 "gramRd.c"
	break;
      case 25: /* "VERB" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1484 "gramRd.c"
	break;
      case 26: /* "COMMENT" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1489 "gramRd.c"
	break;
      case 27: /* "UNKNOWN" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1494 "gramRd.c"
	break;
      case 28: /* "STARTFILE" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1499 "gramRd.c"
	break;
      case 29: /* "STARTFRAGMENT" */
#line 150 "gramRd.y"
	{ UNPROTECT_PTR((*yyvaluep)); };
#line 1504 "gramRd.c"
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
#line 158 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; ;}
    break;

  case 3:
#line 159 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; ;}
    break;

  case 4:
#line 160 "gramRd.y"
    { PROTECT(Value = R_NilValue);  YYABORT; ;}
    break;

  case 5:
#line 163 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); UNPROTECT_PTR((yyvsp[(1) - (2)])); ;}
    break;

  case 6:
#line 166 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 7:
#line 169 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 8:
#line 170 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 9:
#line 172 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 10:
#line 173 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 11:
#line 174 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 12:
#line 175 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 13:
#line 176 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, &(yyloc)); ;}
    break;

  case 14:
#line 177 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); ;}
    break;

  case 15:
#line 178 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 16:
#line 179 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 17:
#line 180 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); ;}
    break;

  case 18:
#line 181 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); ;}
    break;

  case 19:
#line 182 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 20:
#line 184 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); ;}
    break;

  case 21:
#line 185 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); ;}
    break;

  case 22:
#line 187 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); ;}
    break;

  case 23:
#line 188 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), RCODE, &(yyloc)); ;}
    break;

  case 24:
#line 189 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), VERB, &(yyloc)); ;}
    break;

  case 25:
#line 190 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); ;}
    break;

  case 26:
#line 191 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), UNKNOWN, &(yyloc)); yyerror(yyunknown); ;}
    break;

  case 27:
#line 192 "gramRd.y"
    { (yyval) = xxmarkup(R_NilValue, (yyvsp[(1) - (1)]), &(yyloc)); ;}
    break;

  case 28:
#line 193 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 29:
#line 194 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 30:
#line 196 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 31:
#line 197 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, &(yyloc)); ;}
    break;

  case 32:
#line 198 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 33:
#line 199 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 34:
#line 200 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 35:
#line 201 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 36:
#line 202 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 37:
#line 203 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); ;}
    break;

  case 38:
#line 204 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); ;}
    break;

  case 39:
#line 205 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), &(yyloc)); ;}
    break;

  case 40:
#line 206 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), R_NilValue, 1, &(yyloc)); ;}
    break;

  case 41:
#line 207 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, &(yyloc)); ;}
    break;

  case 42:
#line 208 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (1)]), R_NilValue, &(yyloc)); ;}
    break;

  case 43:
#line 209 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); ;}
    break;

  case 44:
#line 211 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 45:
#line 213 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 46:
#line 214 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist((yyvsp[(2) - (2)])); 
    	    					  warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            xxBasename, (yylsp[(2) - (2)]).first_line, (yylsp[(2) - (2)]).first_column); ;}
    break;

  case 47:
#line 218 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 48:
#line 220 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 49:
#line 222 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 50:
#line 226 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 51:
#line 227 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); ;}
    break;

  case 52:
#line 229 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 53:
#line 231 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); ;}
    break;

  case 54:
#line 235 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); ;}
    break;

  case 55:
#line 236 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); ;}
    break;

  case 56:
#line 238 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist((yyvsp[(2) - (2)])); ;}
    break;

  case 57:
#line 241 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); ;}
    break;

  case 58:
#line 243 "gramRd.y"
    { (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); ;}
    break;

  case 59:
#line 245 "gramRd.y"
    { xxbraceDepth--; (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); xxbraceDepth++; ;}
    break;

  case 60:
#line 247 "gramRd.y"
    { (yyval) = xxpushMode(INOPTION, UNKNOWN, FALSE); ;}
    break;

  case 61:
#line 249 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); ;}
    break;

  case 62:
#line 251 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, TRUE); ;}
    break;

  case 63:
#line 253 "gramRd.y"
    { xxbraceDepth--; (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); xxbraceDepth++; ;}
    break;

  case 64:
#line 255 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, ESCAPE, FALSE); ;}
    break;

  case 65:
#line 257 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); ;}
    break;

  case 66:
#line 259 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;

  case 67:
#line 260 "gramRd.y"
    { (yyval) = xxnewlist(NULL); ;}
    break;

  case 68:
#line 261 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); ;}
    break;

  case 69:
#line 262 "gramRd.y"
    { (yyval) = xxnewlist(NULL); ;}
    break;

  case 70:
#line 263 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); ;}
    break;

  case 71:
#line 265 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2180 "gramRd.c"
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


#line 267 "gramRd.y"


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
    { "\\RdOpts",   VSECTIONHEADER },
    
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
    
    /* These macros take one optional bracketed option and one R-like argument */
    
    { "\\Sexpr",   SEXPR },
    
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
    	        if (yychar < 256)
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
    if (yylloc.first_line != yylloc.last_line)
    	warning("%s:%d-%d: %s", ParseErrorFilename, yylloc.first_line, yylloc.last_line, ParseErrorMsg);
    else
    	warning("%s:%d: %s", ParseErrorFilename, yylloc.first_line, ParseErrorMsg);

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

 .Internal( parseRd(file, srcfile, encoding, verbose, basename) )
 If there is text then that is read and the other arguments are ignored.
*/

SEXP attribute_hidden do_parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s = R_NilValue, source;
    Rconnection con;
    Rboolean wasopen, old_latin1=known_to_be_latin1,
	old_utf8=known_to_be_utf8, fragment;
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
    xxBasename = CHAR(STRING_ELT(CAR(args), 0));	args = CDR(args);
    fragment = asLogical(CAR(args));

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
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
    return s;
}

