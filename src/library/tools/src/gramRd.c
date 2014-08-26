/* A Bison parser, made by GNU Bison 2.7.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2012 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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
#define YYBISON_VERSION "2.7"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
/* Line 371 of yacc.c  */
#line 1 "gramRd.y"

/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2013  The R Core Team
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

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Parse.h>
#define STRICT_R_HEADERS
#include <R_ext/RS.h>           /* for R_chk_* allocation */
#include <ctype.h>
#include <Rmath.h> /* for imax2(.),..*/
#undef _
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("tools", String)
#else
#define _(String) (String)
#endif

/* bison creates a non-static symbol yylloc in both gramLatex.o and gramRd.o,
   so remap */

#define yylloc yyllocR

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

static Rboolean wCalls = TRUE;
static Rboolean warnDups = FALSE;

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

static SEXP	GrowList(SEXP, SEXP);
static int	KeywordLookup(const char *);
static SEXP	UserMacroLookup(const char *);
static SEXP	InstallKeywords();
static SEXP	NewList(void);
static SEXP     makeSrcref(YYLTYPE *, SEXP);
static int	xxgetc();
static int	xxungetc(int);

/* Flags used to mark need for postprocessing in the dynamicFlag attribute */

#define STATIC 0
#define HAS_IFDEF 1
#define HAS_SEXPR 2

/* Internal lexer / parser state variables */

static char const yyunknown[] = "unknown macro"; /* our message, not bison's */


typedef struct ParseState ParseState;
struct ParseState {
    int xxinRString, xxQuoteLine, xxQuoteCol;
    int	xxinEqn;
    int	xxNewlineInString;
    int	xxlineno, xxbyteno, xxcolno;
    int	xxmode, xxitemType, xxbraceDepth;  /* context for lexer */
    int	xxDebugTokens;  /* non-zero causes debug output to R console */
    const char* xxBasename;     /* basename of file for error messages */
    SEXP	Value;
    int	xxinitvalue;
    SEXP	xxMacroList;/* A hashed environment containing all the standard and user-defined macro names */
    ParseState *prevState;
};

static Rboolean busy = FALSE;
static ParseState parseState;

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
static SEXP	xxnewlist2(SEXP, SEXP);
static SEXP	xxnewlist3(SEXP, SEXP, SEXP);
static SEXP	xxnewlist4(SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist5(SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist6(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist7(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist8(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
static SEXP	xxnewlist9(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static SEXP	xxlist(SEXP, SEXP);
static SEXP	xxmarkup(SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxmarkup2(SEXP, SEXP, SEXP, int, int, YYLTYPE *);
static SEXP	xxmarkup3(SEXP, SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxOptionmarkup(SEXP, SEXP, SEXP, int, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static void	xxsavevalue(SEXP, YYLTYPE *);
static void	xxWarnNewline();
static SEXP	xxnewcommand(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxusermacro(SEXP, SEXP, YYLTYPE *);
static int	mkMarkup(int);
static int      mkIfdef(int);
static int	mkCode(int);
static int	mkText(int);
static int	mkVerb(int);
static int 	mkComment(int);

#define YYSTYPE		SEXP


/* Line 371 of yacc.c  */
#line 250 "gramRd.c"

# ifndef YY_NULL
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULL nullptr
#  else
#   define YY_NULL 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

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
     VERBLATEX = 277,
     LATEXMACRO3 = 278,
     NEWCOMMAND = 279,
     USERMACRO = 280,
     USERMACRO1 = 281,
     USERMACRO2 = 282,
     USERMACRO3 = 283,
     USERMACRO4 = 284,
     USERMACRO5 = 285,
     USERMACRO6 = 286,
     USERMACRO7 = 287,
     USERMACRO8 = 288,
     USERMACRO9 = 289,
     IFDEF = 290,
     ENDIF = 291,
     TEXT = 292,
     RCODE = 293,
     VERB = 294,
     COMMENT = 295,
     UNKNOWN = 296,
     STARTFILE = 297,
     STARTFRAGMENT = 298
   };
#endif


#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
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

extern YYSTYPE yylval;
extern YYLTYPE yylloc;
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



/* Copy the second part of user declarations.  */

/* Line 390 of yacc.c  */
#line 369 "gramRd.c"

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
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(N) (N)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
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
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
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
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  33
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   832

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  48
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  31
/* YYNRULES -- Number of rules.  */
#define YYNRULES  89
/* YYNRULES -- Number of states.  */
#define YYNSTATES  194

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   298

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
       2,    46,     2,    47,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    44,     2,    45,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     7,    11,    13,    16,    18,    20,    23,
      26,    29,    32,    35,    38,    42,    47,    52,    56,    61,
      63,    65,    67,    70,    72,    75,    77,    79,    81,    83,
      85,    87,    89,    91,    94,    97,   101,   106,   109,   112,
     116,   121,   124,   128,   133,   136,   139,   143,   145,   150,
     155,   159,   163,   165,   168,   172,   177,   183,   190,   198,
     208,   219,   231,   234,   237,   240,   243,   246,   249,   254,
     258,   261,   264,   269,   273,   276,   277,   278,   279,   280,
     281,   282,   283,   284,   285,   289,   292,   297,   301,   306
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      49,     0,    -1,    42,    51,     3,    -1,    43,    50,     3,
      -1,     1,    -1,    68,    54,    -1,    52,    -1,    53,    -1,
      52,    53,    -1,     7,    64,    -1,    11,    64,    -1,     6,
      62,    -1,     5,    58,    -1,    16,    61,    -1,     8,    58,
      59,    -1,    35,    67,    52,    36,    -1,    35,    67,    52,
       1,    -1,    10,    71,    63,    -1,    10,    71,    78,    63,
      -1,    40,    -1,    37,    -1,    57,    -1,     1,    53,    -1,
      55,    -1,    54,    55,    -1,    37,    -1,    38,    -1,    39,
      -1,    40,    -1,    41,    -1,    77,    -1,    56,    -1,    57,
      -1,     1,    55,    -1,    12,    58,    -1,    20,    58,    59,
      -1,    23,    58,    59,    59,    -1,    17,    60,    -1,    18,
      61,    -1,    14,    71,    58,    -1,    14,    71,    78,    58,
      -1,     9,    62,    -1,    10,    71,    63,    -1,    10,    71,
      78,    63,    -1,    13,    64,    -1,    21,    65,    -1,    21,
      65,    66,    -1,    15,    -1,    35,    67,    54,    36,    -1,
      35,    67,    54,     1,    -1,    22,    65,    59,    -1,    24,
      65,    64,    -1,    25,    -1,    26,    64,    -1,    27,    64,
      64,    -1,    28,    64,    64,    64,    -1,    29,    64,    64,
      64,    64,    -1,    30,    64,    64,    64,    64,    64,    -1,
      31,    64,    64,    64,    64,    64,    64,    -1,    32,    64,
      64,    64,    64,    64,    64,    64,    64,    -1,    33,    64,
      64,    64,    64,    64,    64,    64,    64,    64,    -1,    34,
      64,    64,    64,    64,    64,    64,    64,    64,    64,    64,
      -1,    68,    77,    -1,    68,    77,    -1,    68,    37,    -1,
      75,    77,    -1,    76,    77,    -1,    69,    77,    -1,    44,
      70,    54,    45,    -1,    44,    70,    45,    -1,    72,    77,
      -1,    73,    77,    -1,    44,    74,    54,    45,    -1,    44,
      74,    45,    -1,    68,    37,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    44,    54,    45,    -1,    44,
      45,    -1,    44,    54,     1,    45,    -1,    44,     1,    45,
      -1,    44,    54,     1,     3,    -1,    46,    55,    47,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   214,   214,   215,   216,   219,   222,   225,   226,   228,
     229,   230,   231,   232,   233,   234,   235,   236,   237,   238,
     239,   240,   241,   243,   244,   246,   247,   248,   249,   250,
     251,   252,   253,   254,   256,   257,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,   271,
     272,   274,   275,   276,   277,   279,   281,   283,   285,   287,
     290,   293,   298,   300,   301,   310,   312,   314,   318,   319,
     321,   323,   327,   328,   330,   333,   335,   337,   339,   341,
     343,   345,   347,   349,   351,   352,   353,   354,   355,   357
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "SECTIONHEADER",
  "RSECTIONHEADER", "VSECTIONHEADER", "SECTIONHEADER2", "RCODEMACRO",
  "SEXPR", "RDOPTS", "LATEXMACRO", "VERBMACRO", "OPTMACRO", "ESCAPE",
  "LISTSECTION", "ITEMIZE", "DESCRIPTION", "NOITEM", "LATEXMACRO2",
  "VERBMACRO2", "VERBLATEX", "LATEXMACRO3", "NEWCOMMAND", "USERMACRO",
  "USERMACRO1", "USERMACRO2", "USERMACRO3", "USERMACRO4", "USERMACRO5",
  "USERMACRO6", "USERMACRO7", "USERMACRO8", "USERMACRO9", "IFDEF", "ENDIF",
  "TEXT", "RCODE", "VERB", "COMMENT", "UNKNOWN", "STARTFILE",
  "STARTFRAGMENT", "'{'", "'}'", "'['", "']'", "$accept", "Init",
  "RdFragment", "RdFile", "SectionList", "Section", "ArgItems", "Item",
  "Markup", "UserMacro", "LatexArg", "LatexArg2", "Item0Arg", "Item2Arg",
  "RLikeArg", "RLikeArg2", "VerbatimArg", "VerbatimArg1", "VerbatimArg2",
  "IfDefTarget", "goLatexLike", "goRLike", "goRLike2", "goOption",
  "goVerbatim", "goVerbatim1", "goVerbatim2", "goItem0", "goItem2", "Arg",
  "Option", YY_NULL
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
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   123,   125,    91,    93
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    48,    49,    49,    49,    50,    51,    52,    52,    53,
      53,    53,    53,    53,    53,    53,    53,    53,    53,    53,
      53,    53,    53,    54,    54,    55,    55,    55,    55,    55,
      55,    55,    55,    55,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    57,    57,    57,    57,    57,    57,    57,    57,    57,
      57,    57,    58,    59,    59,    60,    61,    62,    63,    63,
      64,    65,    66,    66,    67,    68,    69,    70,    71,    72,
      73,    74,    75,    76,    77,    77,    77,    77,    77,    78
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     3,     3,     1,     2,     1,     1,     2,     2,
       2,     2,     2,     2,     3,     4,     4,     3,     4,     1,
       1,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     3,     4,     2,     2,     3,
       4,     2,     3,     4,     2,     2,     3,     1,     4,     4,
       3,     3,     1,     2,     3,     4,     5,     6,     7,     9,
      10,    11,     2,     2,     2,     2,     2,     2,     4,     3,
       2,     2,     4,     3,     2,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     3,     2,     4,     3,     4,     3
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     4,     0,    75,     0,     0,    75,    76,    79,    75,
      78,    79,    83,    80,    52,    79,    79,    79,    79,    79,
      79,    79,    79,    79,    75,    20,    19,     0,     0,     7,
      21,     0,     0,     1,    22,    12,     0,    11,     0,     9,
       0,    75,     0,    10,    13,     0,    79,     0,    53,    79,
      79,    79,    79,    79,    79,    79,    79,     0,     0,     2,
       8,     3,     0,    76,    78,    75,    79,    78,    47,    82,
      83,    75,    80,    80,    75,    75,    25,    26,    27,    28,
      29,     0,     0,    23,    31,    32,    30,    62,    67,    70,
      14,     0,    77,     0,    17,     0,    66,    51,    71,    54,
      79,    79,    79,    79,    79,    79,    79,     0,    74,    33,
      41,     0,    34,    44,    75,    37,     0,    38,    75,    45,
      75,    75,     0,     0,    85,     0,    24,    64,    63,     0,
       0,    18,    55,    79,    79,    79,    79,    79,    79,     0,
      15,    42,     0,    39,    75,    65,    35,    81,    46,    50,
      75,     0,    87,     0,    84,    69,     0,    89,    56,    79,
      79,    79,    79,    79,    43,    40,     0,    36,     0,    48,
      88,    86,    68,    57,    79,    79,    79,    79,    73,     0,
      58,    79,    79,    79,    72,    79,    79,    79,    59,    79,
      79,    60,    79,    61
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    31,    27,    28,    29,    82,    83,    84,    85,
      35,    90,   115,    44,    37,    94,    39,    46,   148,    57,
      36,    38,   129,    42,    40,    47,   166,   116,    45,    86,
      95
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -94
static const yytype_int16 yypact[] =
{
      19,   -94,   792,   -94,    25,   792,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,    28,   718,   -94,
     -94,    34,   638,   -94,   -94,   -94,     4,   -94,     4,   -94,
       4,   -94,   -30,   -94,   -94,     4,   -94,     4,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   792,    16,   -94,
     -94,   -94,   638,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   265,   556,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -22,   -94,   638,   -94,    12,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   755,   -94,   -94,
     -94,   -30,   -94,   -94,     8,   -94,     4,   -94,   -94,    15,
     -94,   -94,   638,   306,   -94,   347,   -94,   -94,   -94,   388,
      13,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   680,
     -94,   -94,    12,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   597,   -94,   224,   -94,   -94,   429,   -94,   -94,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   470,   -94,   179,   -94,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   511,
     -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,   -94,
     -94,   -94,   -94,   -94
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -94,   -94,   -94,   -94,     7,    -2,   -64,   -10,   -94,    22,
      -8,   -50,   -94,    -3,     6,   -93,   -11,   -26,   -94,     0,
      10,   -94,   -94,   -35,   -94,   -94,   -94,   -94,   -94,   -17,
     -78
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -50
static const yytype_int16 yytable[] =
{
      43,    41,   131,    34,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    32,    92,   127,    93,   125,   141,    87,
       1,    88,    81,    89,    30,    33,    60,    30,    96,   111,
      98,    59,   114,   142,    58,    97,   144,    61,    99,   100,
     101,   102,   103,   104,   105,   106,   119,   120,    81,   164,
      30,    91,   109,   108,    93,   113,    92,   112,   151,   147,
     157,     2,     3,   118,   107,   156,   121,   117,   146,   110,
     149,   150,   126,     0,   128,   122,     0,     0,     0,    30,
       0,     0,     0,   130,     0,    58,     0,     0,     0,   132,
     133,   134,   135,   136,   137,   138,     0,     0,     0,   145,
     167,     0,   179,     0,     0,    60,   143,     0,     0,     0,
       0,     0,     0,   109,     0,   126,     0,     0,     0,     0,
       0,     0,   158,   159,   160,   161,   162,   163,    91,    30,
      91,    91,     0,     0,     0,     0,   165,    34,     0,     0,
       0,   126,     0,   109,     0,     0,   126,     0,   173,   174,
     175,   176,   177,     0,     0,     0,     0,     0,   109,     0,
      91,    30,     0,   180,   181,   182,   183,     0,     0,   126,
     185,   186,   187,     0,   188,   189,   190,     0,   191,   192,
      62,   193,   -49,     0,     0,     0,     0,     0,    63,    64,
       0,    65,    66,    67,    68,     0,    69,    70,     0,    71,
      72,    73,    74,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    75,   -49,    76,    77,    78,    79,
      80,     0,     0,    81,   -49,    62,   -49,   170,     0,     0,
       0,     0,     0,    63,    64,     0,    65,    66,    67,    68,
       0,    69,    70,     0,    71,    72,    73,    74,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    75,
       0,    76,    77,    78,    79,    80,   123,     0,    81,   171,
       0,     0,     0,     0,    63,    64,     0,    65,    66,    67,
      68,     0,    69,    70,     0,    71,    72,    73,    74,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      75,     0,    76,    77,    78,    79,    80,    62,     0,    81,
     124,     0,     0,     0,     0,    63,    64,     0,    65,    66,
      67,    68,     0,    69,    70,     0,    71,    72,    73,    74,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    75,     0,    76,    77,    78,    79,    80,   153,     0,
      81,   152,     0,     0,     0,     0,    63,    64,     0,    65,
      66,    67,    68,     0,    69,    70,     0,    71,    72,    73,
      74,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    75,     0,    76,    77,    78,    79,    80,    62,
       0,    81,   154,     0,     0,     0,     0,    63,    64,     0,
      65,    66,    67,    68,     0,    69,    70,     0,    71,    72,
      73,    74,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    75,     0,    76,    77,    78,    79,    80,
      62,     0,    81,   155,     0,     0,     0,     0,    63,    64,
       0,    65,    66,    67,    68,     0,    69,    70,     0,    71,
      72,    73,    74,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    75,     0,    76,    77,    78,    79,
      80,    62,     0,    81,   172,     0,     0,     0,     0,    63,
      64,     0,    65,    66,    67,    68,     0,    69,    70,     0,
      71,    72,    73,    74,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    75,     0,    76,    77,    78,
      79,    80,    62,     0,    81,   178,     0,     0,     0,     0,
      63,    64,     0,    65,    66,    67,    68,     0,    69,    70,
       0,    71,    72,    73,    74,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    75,     0,    76,    77,
      78,    79,    80,     0,     0,    81,   184,    62,     0,    -5,
       0,     0,     0,     0,     0,    63,    64,     0,    65,    66,
      67,    68,     0,    69,    70,     0,    71,    72,    73,    74,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    75,     0,    76,    77,    78,    79,    80,   168,     0,
      81,     0,     0,     0,     0,     0,    63,    64,     0,    65,
      66,    67,    68,     0,    69,    70,     0,    71,    72,    73,
      74,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    75,   169,    76,    77,    78,    79,    80,    62,
       0,    81,     0,     0,     0,     0,     0,    63,    64,     0,
      65,    66,    67,    68,     0,    69,    70,     0,    71,    72,
      73,    74,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    75,     0,    76,    77,    78,    79,    80,
       0,     5,    81,   -16,     0,     6,     7,     8,     9,     0,
      10,    11,     0,     0,     0,     0,    12,     0,     0,     0,
       0,     0,     0,     0,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,   -16,    25,     0,     5,
      26,    -6,     0,     6,     7,     8,     9,     0,    10,    11,
       0,     0,     0,     0,    12,     0,     0,     0,     0,     0,
       0,     0,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,     0,    25,   139,     0,    26,     0,
       6,     7,     8,     9,     0,    10,    11,     0,     0,     0,
       0,    12,     0,     0,     0,     0,     0,     0,     0,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,   140,    25,     5,     0,    26,     0,     6,     7,     8,
       9,     0,    10,    11,     0,     0,     0,     0,    12,     0,
       0,     0,     0,     0,     0,     0,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,     0,    25,
       0,     0,    26
};

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-94)))

#define yytable_value_is_error(Yytable_value) \
  YYID (0)

static const yytype_int16 yycheck[] =
{
      11,     9,    95,     5,    15,    16,    17,    18,    19,    20,
      21,    22,    23,     3,    44,    37,    46,    81,   111,    36,
       1,    38,    44,    40,     2,     0,    28,     5,    45,    64,
      47,     3,    67,   111,    24,    46,   114,     3,    49,    50,
      51,    52,    53,    54,    55,    56,    72,    73,    44,   142,
      28,    41,    62,    37,    46,    66,    44,    65,   122,    44,
      47,    42,    43,    71,    57,   129,    74,    70,   118,    63,
     120,   121,    82,    -1,    91,    75,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    93,    -1,    75,    -1,    -1,    -1,   100,
     101,   102,   103,   104,   105,   106,    -1,    -1,    -1,   116,
     150,    -1,   166,    -1,    -1,   107,   114,    -1,    -1,    -1,
      -1,    -1,    -1,   123,    -1,   125,    -1,    -1,    -1,    -1,
      -1,    -1,   133,   134,   135,   136,   137,   138,   118,   107,
     120,   121,    -1,    -1,    -1,    -1,   144,   139,    -1,    -1,
      -1,   151,    -1,   153,    -1,    -1,   156,    -1,   159,   160,
     161,   162,   163,    -1,    -1,    -1,    -1,    -1,   168,    -1,
     150,   139,    -1,   174,   175,   176,   177,    -1,    -1,   179,
     181,   182,   183,    -1,   185,   186,   187,    -1,   189,   190,
       1,   192,     3,    -1,    -1,    -1,    -1,    -1,     9,    10,
      -1,    12,    13,    14,    15,    -1,    17,    18,    -1,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    -1,    -1,    44,    45,     1,    47,     3,    -1,    -1,
      -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,    15,
      -1,    17,    18,    -1,    20,    21,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      -1,    37,    38,    39,    40,    41,     1,    -1,    44,    45,
      -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,    14,
      15,    -1,    17,    18,    -1,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    -1,    37,    38,    39,    40,    41,     1,    -1,    44,
      45,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,
      14,    15,    -1,    17,    18,    -1,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,     1,    -1,
      44,    45,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      13,    14,    15,    -1,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    -1,    37,    38,    39,    40,    41,     1,
      -1,    44,    45,    -1,    -1,    -1,    -1,     9,    10,    -1,
      12,    13,    14,    15,    -1,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
       1,    -1,    44,    45,    -1,    -1,    -1,    -1,     9,    10,
      -1,    12,    13,    14,    15,    -1,    17,    18,    -1,    20,
      21,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    -1,    37,    38,    39,    40,
      41,     1,    -1,    44,    45,    -1,    -1,    -1,    -1,     9,
      10,    -1,    12,    13,    14,    15,    -1,    17,    18,    -1,
      20,    21,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    -1,    37,    38,    39,
      40,    41,     1,    -1,    44,    45,    -1,    -1,    -1,    -1,
       9,    10,    -1,    12,    13,    14,    15,    -1,    17,    18,
      -1,    20,    21,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    -1,    37,    38,
      39,    40,    41,    -1,    -1,    44,    45,     1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,    13,
      14,    15,    -1,    17,    18,    -1,    20,    21,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    -1,    37,    38,    39,    40,    41,     1,    -1,
      44,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,    12,
      13,    14,    15,    -1,    17,    18,    -1,    20,    21,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,     1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,     9,    10,    -1,
      12,    13,    14,    15,    -1,    17,    18,    -1,    20,    21,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,    38,    39,    40,    41,
      -1,     1,    44,     3,    -1,     5,     6,     7,     8,    -1,
      10,    11,    -1,    -1,    -1,    -1,    16,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    24,    25,    26,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    -1,     1,
      40,     3,    -1,     5,     6,     7,     8,    -1,    10,    11,
      -1,    -1,    -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    -1,    37,     1,    -1,    40,    -1,
       5,     6,     7,     8,    -1,    10,    11,    -1,    -1,    -1,
      -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,     1,    -1,    40,    -1,     5,     6,     7,
       8,    -1,    10,    11,    -1,    -1,    -1,    -1,    16,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    24,    25,    26,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    -1,    37,
      -1,    -1,    40
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,    42,    43,    49,     1,     5,     6,     7,     8,
      10,    11,    16,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    37,    40,    51,    52,    53,
      57,    50,    68,     0,    53,    58,    68,    62,    69,    64,
      72,    58,    71,    64,    61,    76,    65,    73,    64,    64,
      64,    64,    64,    64,    64,    64,    64,    67,    68,     3,
      53,     3,     1,     9,    10,    12,    13,    14,    15,    17,
      18,    20,    21,    22,    23,    35,    37,    38,    39,    40,
      41,    44,    54,    55,    56,    57,    77,    77,    77,    77,
      59,    68,    44,    46,    63,    78,    77,    64,    77,    64,
      64,    64,    64,    64,    64,    64,    64,    52,    37,    55,
      62,    71,    58,    64,    71,    60,    75,    61,    58,    65,
      65,    58,    67,     1,    45,    54,    55,    37,    77,    70,
      55,    63,    64,    64,    64,    64,    64,    64,    64,     1,
      36,    63,    78,    58,    78,    77,    59,    44,    66,    59,
      59,    54,    45,     1,    45,    45,    54,    47,    64,    64,
      64,    64,    64,    64,    63,    58,    74,    59,     1,    36,
       3,    45,    45,    64,    64,    64,    64,    64,    45,    54,
      64,    64,    64,    64,    45,    64,    64,    64,    64,    64,
      64,    64,    64,    64
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
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))

/* Error token number */
#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (YYID (N))                                                     \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (YYID (0))
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

__attribute__((__unused__))
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static unsigned
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
#else
static unsigned
yy_location_print_ (yyo, yylocp)
    FILE *yyo;
    YYLTYPE const * const yylocp;
#endif
{
  unsigned res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += fprintf (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += fprintf (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += fprintf (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += fprintf (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += fprintf (yyo, "-%d", end_col);
    }
  return res;
 }

#  define YY_LOCATION_PRINT(File, Loc)          \
  yy_location_print_ (File, &(Loc))

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
  FILE *yyo = yyoutput;
  YYUSE (yyo);
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
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
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
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      YYFPRINTF (stderr, "\n");
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

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULL, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULL;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULL, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
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
      case 5: /* SECTIONHEADER */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1656 "gramRd.c"
        break;
      case 6: /* RSECTIONHEADER */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1663 "gramRd.c"
        break;
      case 7: /* VSECTIONHEADER */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1670 "gramRd.c"
        break;
      case 8: /* SECTIONHEADER2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1677 "gramRd.c"
        break;
      case 9: /* RCODEMACRO */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1684 "gramRd.c"
        break;
      case 10: /* SEXPR */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1691 "gramRd.c"
        break;
      case 12: /* LATEXMACRO */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1698 "gramRd.c"
        break;
      case 13: /* VERBMACRO */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1705 "gramRd.c"
        break;
      case 14: /* OPTMACRO */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1712 "gramRd.c"
        break;
      case 15: /* ESCAPE */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1719 "gramRd.c"
        break;
      case 16: /* LISTSECTION */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1726 "gramRd.c"
        break;
      case 17: /* ITEMIZE */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1733 "gramRd.c"
        break;
      case 18: /* DESCRIPTION */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1740 "gramRd.c"
        break;
      case 19: /* NOITEM */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1747 "gramRd.c"
        break;
      case 20: /* LATEXMACRO2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1754 "gramRd.c"
        break;
      case 21: /* VERBMACRO2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1761 "gramRd.c"
        break;
      case 22: /* VERBLATEX */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1768 "gramRd.c"
        break;
      case 23: /* LATEXMACRO3 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1775 "gramRd.c"
        break;
      case 24: /* NEWCOMMAND */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1782 "gramRd.c"
        break;
      case 25: /* USERMACRO */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1789 "gramRd.c"
        break;
      case 26: /* USERMACRO1 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1796 "gramRd.c"
        break;
      case 27: /* USERMACRO2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1803 "gramRd.c"
        break;
      case 28: /* USERMACRO3 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1810 "gramRd.c"
        break;
      case 29: /* USERMACRO4 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1817 "gramRd.c"
        break;
      case 30: /* USERMACRO5 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1824 "gramRd.c"
        break;
      case 31: /* USERMACRO6 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1831 "gramRd.c"
        break;
      case 32: /* USERMACRO7 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1838 "gramRd.c"
        break;
      case 33: /* USERMACRO8 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1845 "gramRd.c"
        break;
      case 34: /* USERMACRO9 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1852 "gramRd.c"
        break;
      case 35: /* IFDEF */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1859 "gramRd.c"
        break;
      case 36: /* ENDIF */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1866 "gramRd.c"
        break;
      case 37: /* TEXT */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1873 "gramRd.c"
        break;
      case 38: /* RCODE */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1880 "gramRd.c"
        break;
      case 39: /* VERB */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1887 "gramRd.c"
        break;
      case 40: /* COMMENT */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1894 "gramRd.c"
        break;
      case 41: /* UNKNOWN */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1901 "gramRd.c"
        break;
      case 42: /* STARTFILE */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1908 "gramRd.c"
        break;
      case 43: /* STARTFRAGMENT */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1915 "gramRd.c"
        break;
      case 54: /* ArgItems */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1922 "gramRd.c"
        break;
      case 58: /* LatexArg */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1929 "gramRd.c"
        break;
      case 63: /* RLikeArg2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1936 "gramRd.c"
        break;
      case 65: /* VerbatimArg1 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1943 "gramRd.c"
        break;
      case 66: /* VerbatimArg2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1950 "gramRd.c"
        break;
      case 67: /* IfDefTarget */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1957 "gramRd.c"
        break;
      case 68: /* goLatexLike */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1964 "gramRd.c"
        break;
      case 69: /* goRLike */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1971 "gramRd.c"
        break;
      case 70: /* goRLike2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1978 "gramRd.c"
        break;
      case 71: /* goOption */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1985 "gramRd.c"
        break;
      case 72: /* goVerbatim */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1992 "gramRd.c"
        break;
      case 73: /* goVerbatim1 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 1999 "gramRd.c"
        break;
      case 74: /* goVerbatim2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 2006 "gramRd.c"
        break;
      case 75: /* goItem0 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 2013 "gramRd.c"
        break;
      case 76: /* goItem2 */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 2020 "gramRd.c"
        break;
      case 78: /* Option */
/* Line 1398 of yacc.c  */
#line 202 "gramRd.y"
        { UNPROTECT_PTR((*yyvaluep)); };
/* Line 1398 of yacc.c  */
#line 2027 "gramRd.c"
        break;

      default:
        break;
    }
}




/* The lookahead symbol.  */
int yychar;


#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval YY_INITIAL_VALUE(yyval_default);

/* Location data for the lookahead symbol.  */
YYLTYPE yylloc
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;


/* Number of syntax errors so far.  */
int yynerrs;


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
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.
       `yyls': related to locations.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    /* The location stack.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls;
    YYLTYPE *yylsp;

    /* The locations where the error started and ended.  */
    YYLTYPE yyerror_range[3];

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yylsp = yyls = yylsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  yylsp[0] = yylloc;
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
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
	YYSTACK_RELOCATE (yyls_alloc, yyls);
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

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
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
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
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
/* Line 1792 of yacc.c  */
#line 214 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; }
    break;

  case 3:
/* Line 1792 of yacc.c  */
#line 215 "gramRd.y"
    { xxsavevalue((yyvsp[(2) - (3)]), &(yyloc)); UNPROTECT_PTR((yyvsp[(1) - (3)])); return 0; }
    break;

  case 4:
/* Line 1792 of yacc.c  */
#line 216 "gramRd.y"
    { PROTECT(parseState.Value = R_NilValue);  YYABORT; }
    break;

  case 5:
/* Line 1792 of yacc.c  */
#line 219 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); UNPROTECT_PTR((yyvsp[(1) - (2)])); }
    break;

  case 6:
/* Line 1792 of yacc.c  */
#line 222 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 7:
/* Line 1792 of yacc.c  */
#line 225 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); }
    break;

  case 8:
/* Line 1792 of yacc.c  */
#line 226 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 9:
/* Line 1792 of yacc.c  */
#line 228 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 10:
/* Line 1792 of yacc.c  */
#line 229 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), HAS_SEXPR, &(yyloc)); }
    break;

  case 11:
/* Line 1792 of yacc.c  */
#line 230 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 12:
/* Line 1792 of yacc.c  */
#line 231 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 13:
/* Line 1792 of yacc.c  */
#line 232 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 14:
/* Line 1792 of yacc.c  */
#line 233 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); }
    break;

  case 15:
/* Line 1792 of yacc.c  */
#line 234 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); }
    break;

  case 16:
/* Line 1792 of yacc.c  */
#line 235 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); }
    break;

  case 17:
/* Line 1792 of yacc.c  */
#line 236 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); }
    break;

  case 18:
/* Line 1792 of yacc.c  */
#line 237 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); }
    break;

  case 19:
/* Line 1792 of yacc.c  */
#line 238 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); }
    break;

  case 20:
/* Line 1792 of yacc.c  */
#line 239 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); }
    break;

  case 21:
/* Line 1792 of yacc.c  */
#line 240 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 22:
/* Line 1792 of yacc.c  */
#line 241 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 23:
/* Line 1792 of yacc.c  */
#line 243 "gramRd.y"
    { (yyval) = xxnewlist((yyvsp[(1) - (1)])); }
    break;

  case 24:
/* Line 1792 of yacc.c  */
#line 244 "gramRd.y"
    { (yyval) = xxlist((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)])); }
    break;

  case 25:
/* Line 1792 of yacc.c  */
#line 246 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), TEXT, &(yyloc)); }
    break;

  case 26:
/* Line 1792 of yacc.c  */
#line 247 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), RCODE, &(yyloc)); }
    break;

  case 27:
/* Line 1792 of yacc.c  */
#line 248 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), VERB, &(yyloc)); }
    break;

  case 28:
/* Line 1792 of yacc.c  */
#line 249 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), COMMENT, &(yyloc)); }
    break;

  case 29:
/* Line 1792 of yacc.c  */
#line 250 "gramRd.y"
    { (yyval) = xxtag((yyvsp[(1) - (1)]), UNKNOWN, &(yyloc)); yyerror(yyunknown); }
    break;

  case 30:
/* Line 1792 of yacc.c  */
#line 251 "gramRd.y"
    { (yyval) = xxmarkup(R_NilValue, (yyvsp[(1) - (1)]), STATIC, &(yyloc)); }
    break;

  case 31:
/* Line 1792 of yacc.c  */
#line 252 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 32:
/* Line 1792 of yacc.c  */
#line 253 "gramRd.y"
    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 33:
/* Line 1792 of yacc.c  */
#line 254 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 34:
/* Line 1792 of yacc.c  */
#line 256 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 35:
/* Line 1792 of yacc.c  */
#line 257 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); }
    break;

  case 36:
/* Line 1792 of yacc.c  */
#line 258 "gramRd.y"
    { (yyval) = xxmarkup3((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), STATIC, &(yyloc)); }
    break;

  case 37:
/* Line 1792 of yacc.c  */
#line 259 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 38:
/* Line 1792 of yacc.c  */
#line 260 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 39:
/* Line 1792 of yacc.c  */
#line 261 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), STATIC, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); }
    break;

  case 40:
/* Line 1792 of yacc.c  */
#line 262 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), STATIC, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); }
    break;

  case 41:
/* Line 1792 of yacc.c  */
#line 263 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 42:
/* Line 1792 of yacc.c  */
#line 264 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (3)])); }
    break;

  case 43:
/* Line 1792 of yacc.c  */
#line 265 "gramRd.y"
    { (yyval) = xxOptionmarkup((yyvsp[(1) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)]), HAS_SEXPR, &(yyloc)); xxpopMode((yyvsp[(2) - (4)])); }
    break;

  case 44:
/* Line 1792 of yacc.c  */
#line 266 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), STATIC, &(yyloc)); }
    break;

  case 45:
/* Line 1792 of yacc.c  */
#line 267 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (2)]), (yyvsp[(2) - (2)]), R_NilValue, 1, STATIC, &(yyloc)); }
    break;

  case 46:
/* Line 1792 of yacc.c  */
#line 268 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); }
    break;

  case 47:
/* Line 1792 of yacc.c  */
#line 269 "gramRd.y"
    { (yyval) = xxmarkup((yyvsp[(1) - (1)]), R_NilValue, STATIC, &(yyloc)); }
    break;

  case 48:
/* Line 1792 of yacc.c  */
#line 270 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); UNPROTECT_PTR((yyvsp[(4) - (4)])); }
    break;

  case 49:
/* Line 1792 of yacc.c  */
#line 271 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (4)]), (yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), 2, HAS_IFDEF, &(yyloc)); }
    break;

  case 50:
/* Line 1792 of yacc.c  */
#line 272 "gramRd.y"
    { (yyval) = xxmarkup2((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), 2, STATIC, &(yyloc)); }
    break;

  case 51:
/* Line 1792 of yacc.c  */
#line 274 "gramRd.y"
    { (yyval) = xxnewcommand((yyvsp[(1) - (3)]), (yyvsp[(2) - (3)]), (yyvsp[(3) - (3)]), &(yyloc)); }
    break;

  case 52:
/* Line 1792 of yacc.c  */
#line 275 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (1)]), xxnewlist(NULL), &(yyloc)); }
    break;

  case 53:
/* Line 1792 of yacc.c  */
#line 276 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (2)]), xxnewlist((yyvsp[(2) - (2)])), &(yyloc)); }
    break;

  case 54:
/* Line 1792 of yacc.c  */
#line 278 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (3)]), xxnewlist2((yyvsp[(2) - (3)]), (yyvsp[(3) - (3)])), &(yyloc)); }
    break;

  case 55:
/* Line 1792 of yacc.c  */
#line 280 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (4)]), xxnewlist3((yyvsp[(2) - (4)]), (yyvsp[(3) - (4)]), (yyvsp[(4) - (4)])), &(yyloc)); }
    break;

  case 56:
/* Line 1792 of yacc.c  */
#line 282 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (5)]), xxnewlist4((yyvsp[(2) - (5)]), (yyvsp[(3) - (5)]), (yyvsp[(4) - (5)]), (yyvsp[(5) - (5)])), &(yyloc)); }
    break;

  case 57:
/* Line 1792 of yacc.c  */
#line 284 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (6)]), xxnewlist5((yyvsp[(2) - (6)]), (yyvsp[(3) - (6)]), (yyvsp[(4) - (6)]), (yyvsp[(5) - (6)]), (yyvsp[(6) - (6)])), &(yyloc)); }
    break;

  case 58:
/* Line 1792 of yacc.c  */
#line 286 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (7)]), xxnewlist6((yyvsp[(2) - (7)]), (yyvsp[(3) - (7)]), (yyvsp[(4) - (7)]), (yyvsp[(5) - (7)]), (yyvsp[(6) - (7)]), (yyvsp[(7) - (7)])), &(yyloc)); }
    break;

  case 59:
/* Line 1792 of yacc.c  */
#line 289 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (9)]), xxnewlist7((yyvsp[(2) - (9)]), (yyvsp[(3) - (9)]), (yyvsp[(4) - (9)]), (yyvsp[(5) - (9)]), (yyvsp[(6) - (9)]), (yyvsp[(7) - (9)]), (yyvsp[(8) - (9)])), &(yyloc)); }
    break;

  case 60:
/* Line 1792 of yacc.c  */
#line 292 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (10)]), xxnewlist8((yyvsp[(2) - (10)]), (yyvsp[(3) - (10)]), (yyvsp[(4) - (10)]), (yyvsp[(5) - (10)]), (yyvsp[(6) - (10)]), (yyvsp[(7) - (10)]), (yyvsp[(8) - (10)]), (yyvsp[(9) - (10)])), &(yyloc)); }
    break;

  case 61:
/* Line 1792 of yacc.c  */
#line 295 "gramRd.y"
    { (yyval) = xxusermacro((yyvsp[(1) - (11)]), xxnewlist9((yyvsp[(2) - (11)]), (yyvsp[(3) - (11)]), (yyvsp[(4) - (11)]), (yyvsp[(5) - (11)]), (yyvsp[(6) - (11)]), (yyvsp[(7) - (11)]), (yyvsp[(8) - (11)]), (yyvsp[(9) - (11)]), (yyvsp[(10) - (11)])), &(yyloc)); }
    break;

  case 62:
/* Line 1792 of yacc.c  */
#line 298 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 63:
/* Line 1792 of yacc.c  */
#line 300 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 64:
/* Line 1792 of yacc.c  */
#line 301 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist((yyvsp[(2) - (2)])); 
     						  if(wCalls)
    	    					      warning(_("bad markup (extra space?) at %s:%d:%d"), 
    	    					            parseState.xxBasename, (yylsp[(2) - (2)]).first_line, (yylsp[(2) - (2)]).first_column); 
     						  else
    	    					      warningcall(R_NilValue, _("bad markup (extra space?) at %s:%d:%d"), 
    	    					            parseState.xxBasename, (yylsp[(2) - (2)]).first_line, (yylsp[(2) - (2)]).first_column); 
						}
    break;

  case 65:
/* Line 1792 of yacc.c  */
#line 310 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 66:
/* Line 1792 of yacc.c  */
#line 312 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 67:
/* Line 1792 of yacc.c  */
#line 314 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 68:
/* Line 1792 of yacc.c  */
#line 318 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); }
    break;

  case 69:
/* Line 1792 of yacc.c  */
#line 319 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); }
    break;

  case 70:
/* Line 1792 of yacc.c  */
#line 321 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 71:
/* Line 1792 of yacc.c  */
#line 323 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = (yyvsp[(2) - (2)]); }
    break;

  case 72:
/* Line 1792 of yacc.c  */
#line 327 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (4)])); (yyval) = (yyvsp[(3) - (4)]); }
    break;

  case 73:
/* Line 1792 of yacc.c  */
#line 328 "gramRd.y"
    { xxpopMode((yyvsp[(2) - (3)])); (yyval) = xxnewlist(NULL); }
    break;

  case 74:
/* Line 1792 of yacc.c  */
#line 330 "gramRd.y"
    { xxpopMode((yyvsp[(1) - (2)])); (yyval) = xxnewlist(xxtag((yyvsp[(2) - (2)]), TEXT, &(yyloc))); }
    break;

  case 75:
/* Line 1792 of yacc.c  */
#line 333 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, UNKNOWN, FALSE); }
    break;

  case 76:
/* Line 1792 of yacc.c  */
#line 335 "gramRd.y"
    { (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); }
    break;

  case 77:
/* Line 1792 of yacc.c  */
#line 337 "gramRd.y"
    { parseState.xxbraceDepth--; (yyval) = xxpushMode(RLIKE, UNKNOWN, FALSE); parseState.xxbraceDepth++; }
    break;

  case 78:
/* Line 1792 of yacc.c  */
#line 339 "gramRd.y"
    { (yyval) = xxpushMode(INOPTION, UNKNOWN, FALSE); }
    break;

  case 79:
/* Line 1792 of yacc.c  */
#line 341 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); }
    break;

  case 80:
/* Line 1792 of yacc.c  */
#line 343 "gramRd.y"
    { (yyval) = xxpushMode(VERBATIM, UNKNOWN, TRUE); }
    break;

  case 81:
/* Line 1792 of yacc.c  */
#line 345 "gramRd.y"
    { parseState.xxbraceDepth--; (yyval) = xxpushMode(VERBATIM, UNKNOWN, FALSE); parseState.xxbraceDepth++; }
    break;

  case 82:
/* Line 1792 of yacc.c  */
#line 347 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, ESCAPE, FALSE); }
    break;

  case 83:
/* Line 1792 of yacc.c  */
#line 349 "gramRd.y"
    { (yyval) = xxpushMode(LATEXLIKE, LATEXMACRO2, FALSE); }
    break;

  case 84:
/* Line 1792 of yacc.c  */
#line 351 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); }
    break;

  case 85:
/* Line 1792 of yacc.c  */
#line 352 "gramRd.y"
    { (yyval) = xxnewlist(NULL); }
    break;

  case 86:
/* Line 1792 of yacc.c  */
#line 353 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); }
    break;

  case 87:
/* Line 1792 of yacc.c  */
#line 354 "gramRd.y"
    { (yyval) = xxnewlist(NULL); }
    break;

  case 88:
/* Line 1792 of yacc.c  */
#line 355 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (4)]); }
    break;

  case 89:
/* Line 1792 of yacc.c  */
#line 357 "gramRd.y"
    { (yyval) = (yyvsp[(2) - (3)]); }
    break;


/* Line 1792 of yacc.c  */
#line 2878 "gramRd.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
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
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }

  yyerror_range[1] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
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

  /* Else will try to reuse lookahead token after shifting the error
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

  yyerror_range[1] = yylsp[1-yylen];
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
      if (!yypact_value_is_default (yyn))
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

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the lookahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, yyerror_range, 2);
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

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
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


/* Line 2055 of yacc.c  */
#line 359 "gramRd.y"


static SEXP xxpushMode(int newmode, int newitem, int neweqn)
{
    SEXP ans;
    PROTECT(ans = allocVector(INTSXP, 7));
    
    INTEGER(ans)[0] = parseState.xxmode;		/* Lexer mode */
    INTEGER(ans)[1] = parseState.xxitemType;	/* What is \item? */
    INTEGER(ans)[2] = parseState.xxbraceDepth;	/* Brace depth used in RCODE and VERBATIM */
    INTEGER(ans)[3] = parseState.xxinRString;      /* Quote char that started a string */
    INTEGER(ans)[4] = parseState.xxQuoteLine;      /* Where the quote was */
    INTEGER(ans)[5] = parseState.xxQuoteCol;       /*           "         */
    INTEGER(ans)[6] = parseState.xxinEqn;          /* In the first arg to \eqn or \deqn:  no escapes */
    
#if DEBUGMODE
    Rprintf("xxpushMode(%d, %s) pushes %d, %s, %d\n", newmode, yytname[YYTRANSLATE(newitem)], 
    						parseState.xxmode, yytname[YYTRANSLATE(parseState.xxitemType)], parseState.xxbraceDepth);
#endif
    parseState.xxmode = newmode;
    parseState.xxitemType = newitem;
    parseState.xxbraceDepth = 0;
    parseState.xxinRString = 0;
    parseState.xxinEqn = neweqn;
    
    return ans;
}

static void xxpopMode(SEXP oldmode) 
{
#if DEBUGVALS
    Rprintf("xxpopMode(%d, %s, %d) replaces %d, %s, %d\n", INTEGER(oldmode)[0], yytname[YYTRANSLATE(INTEGER(oldmode)[1])], INTEGER(oldmode)[2], 
    					parseState.xxmode, yytname[YYTRANSLATE(parseState.xxitemType)], parseState.xxbraceDepth);
#endif
    parseState.xxmode = INTEGER(oldmode)[0];
    parseState.xxitemType = INTEGER(oldmode)[1]; 
    parseState.xxbraceDepth = INTEGER(oldmode)[2];
    parseState.xxinRString = INTEGER(oldmode)[3];
    parseState.xxQuoteLine = INTEGER(oldmode)[4];
    parseState.xxQuoteCol  = INTEGER(oldmode)[5];
    parseState.xxinEqn	= INTEGER(oldmode)[6];
    
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

static SEXP xxnewlist2(SEXP item1, SEXP item2)
{
    return xxlist(xxnewlist(item1), item2);
}

static SEXP xxnewlist3(SEXP item1, SEXP item2, SEXP item3)
{
    return xxlist(xxnewlist2(item1, item2), item3);
}

static SEXP xxnewlist4(SEXP item1, SEXP item2, SEXP item3, SEXP item4)
{
    return xxlist(xxnewlist3(item1, item2, item3), item4);
}

static SEXP xxnewlist5(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5)
{
    return xxlist(xxnewlist4(item1, item2, item3, item4), item5);
}

static SEXP xxnewlist6(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6)
{
    return xxlist(xxnewlist5(item1, item2, item3, item4, item5), item6);
}

static SEXP xxnewlist7(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7)
{
    return xxlist(xxnewlist6(item1, item2, item3, item4, item5, item6), item7);
}

static SEXP xxnewlist8(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8)
{
    return xxlist(xxnewlist7(item1, item2, item3, item4, item5, item6, item7), item8);
}

static SEXP xxnewlist9(SEXP item1, SEXP item2, SEXP item3, SEXP item4, SEXP item5, 
		       SEXP item6, SEXP item7, SEXP item8, SEXP item9)
{
    return xxlist(xxnewlist8(item1, item2, item3, item4, item5, item6, item7, item8), 
                  item9);
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

static SEXP xxnewcommand(SEXP cmd, SEXP name, SEXP defn, YYLTYPE *lloc)
{
    SEXP ans, prev, thename, thedefn;
    char buffer[128];
    const char *c;
    int maxarg = 0;
#if DEBUGVALS
    Rprintf("xxnewcommand(cmd=%p, name=%p, defn=%p)", cmd, name, defn);
#endif
    thename = CADR(name);
    thedefn = CADR(defn);
    if (TYPEOF(thedefn) == STRSXP)
    	PROTECT(thedefn = mkString(CHAR(STRING_ELT(thedefn,0))));
    else
    	PROTECT(thedefn = mkString(""));
    if (warnDups) {
	prev = findVar(installChar(STRING_ELT(thename, 0)), parseState.xxMacroList);
    	if (prev != R_UnboundValue && strcmp(CHAR(STRING_ELT(cmd,0)), "\\renewcommand")) {
	    snprintf(buffer, sizeof(buffer), _("Macro '%s' previously defined."), 
                 CHAR(STRING_ELT(thename, 0)));
            yyerror(buffer);
        }
    }
    for (c = CHAR(STRING_ELT(thedefn, 0)); *c; c++) {
    	if (*c == '#' && isdigit(*(c+1))) 
    	    maxarg = imax2(maxarg, *(c+1) - '0');
    }
    if (maxarg > 4) {
    	snprintf(buffer, sizeof(buffer), _("At most 4 arguments are allowed for user defined macros."));
	yyerror(buffer);
    }
    PROTECT(ans = ScalarInteger(USERMACRO + maxarg));
    setAttrib(ans, install("Rd_tag"), cmd);
    setAttrib(ans, install("definition"), thedefn);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    defineVar(installChar(STRING_ELT(thename, 0)), ans, parseState.xxMacroList);

    UNPROTECT_PTR(thedefn);
    UNPROTECT_PTR(cmd);
    UNPROTECT_PTR(name);
    UNPROTECT_PTR(defn); 
    return ans;
}

#define START_MACRO -2
#define END_MACRO -3
    	
static SEXP xxusermacro(SEXP macro, SEXP args, YYLTYPE *lloc)
{
    SEXP ans, value, nextarg;
    int i,len;
    const char *c, *start ;
    
#if DEBUGVALS
    Rprintf("xxusermacro(macro=%p, args=%p)", macro, args);
#endif
    len = length(args)-1;
    PROTECT(ans = allocVector(STRSXP, len + 1));
    value = UserMacroLookup(CHAR(STRING_ELT(macro,0)));
    if (TYPEOF(value) == STRSXP)
    	SET_STRING_ELT(ans, 0, STRING_ELT(value, 0));
    else
    	error(_("No macro definition for '%s'."), CHAR(STRING_ELT(macro,0)));
/*    Rprintf("len = %d", len); */
    for (i = 0, nextarg=args; i < len; i++, nextarg = CDR(nextarg)) {
/*        Rprintf("arg i is");
        PrintValue(CADR(CADR(nextarg))); */
	SET_STRING_ELT(ans, i+1, STRING_ELT(CADR(CADR(nextarg)), 0));
    }	
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(macro);    
    /* Now push the expanded macro onto the input stream, in reverse order */
    xxungetc(END_MACRO);
    start = CHAR(STRING_ELT(ans, 0));
    for (c = start + strlen(start); c > start; c--) {
    	if (c > start + 1 && *(c-2) == '#' && isdigit(*(c-1))) {
    	    int which = *(c-1) - '0';
    	    const char *arg = CHAR(STRING_ELT(ans, which));
    	    for (size_t ii = strlen(arg); ii > 0; ii--) xxungetc(arg[ii-1]);
    	    c--;
    	} else {
    	    xxungetc(*(c-1));
    	}
    }
    xxungetc(START_MACRO);
    
    setAttrib(ans, install("Rd_tag"), mkString("USERMACRO"));
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
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
    PROTECT(parseState.Value = PairToVectorList(CDR(Rd)));
    if (!isNull(parseState.Value)) {
    	setAttrib(parseState.Value, R_ClassSymbol, mkString("Rd"));
    	setAttrib(parseState.Value, R_SrcrefSymbol, makeSrcref(lloc, SrcFile));
    	setDynamicFlag(parseState.Value, flag);
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
    if (parseState.xxNewlineInString) {
	if(wCalls)
	    warning(_("newline within quoted string at %s:%d"), 
		    parseState.xxBasename, parseState.xxNewlineInString);
	else
	    warningcall(R_NilValue,
			_("newline within quoted string at %s:%d"), 
			parseState.xxBasename, parseState.xxNewlineInString);
    }
}

  
/*----------------------------------------------------------------------------*/


static int (*ptr_getc)(void);

/* Private pushback, since file ungetc only guarantees one byte.
   We need arbitrarily large size, since this is how macros are expanded. */
   
#define PUSH_BACK(c) do {                  \
	if (npush >= pushsize - 1) {             \
	    int *old = pushbase;              \
            pushsize *= 2;                    \
	    pushbase = malloc(pushsize*sizeof(int));         \
	    if(!pushbase) error(_("unable to allocate buffer for long macro at line %d"), parseState.xxlineno);\
	    memmove(pushbase, old, npush*sizeof(int));        \
	    if(old != pushback) free(old); }	    \
	pushbase[npush++] = (c);                        \
} while(0)

   

#define PUSHBACK_BUFSIZE 32

static int pushback[PUSHBACK_BUFSIZE];
static int *pushbase;
static unsigned int npush, pushsize;
static int macrolevel;
static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];


static int xxgetc(void)
{
    int c, oldpos;
    
    do {
    	if(npush) {    	
    	    c = pushbase[--npush]; 
    	    if (c == START_MACRO) {
    	    	macrolevel++;
    	    	if (macrolevel > 1000) 
    	    	    error(_("macros nested too deeply: infinite recursion?"));
    	    } else if (c == END_MACRO) macrolevel--;
    	} else  c = ptr_getc();
    } while (c == START_MACRO || c == END_MACRO);
    
    if (!macrolevel) {
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
    }
    /* Rprintf("get %c\n", c); */
    return c;
}

static int xxungetc(int c)
{
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    if (c == END_MACRO) macrolevel++;
    if (!macrolevel) {
    	parseState.xxlineno = prevlines[prevpos];
    	parseState.xxbyteno = prevbytes[prevpos];
    	parseState.xxcolno  = prevcols[prevpos];
    	prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;
    
    	R_ParseContextLine = parseState.xxlineno;
    
    	R_ParseContext[R_ParseContextLast] = '\0';
    	/* Mac OS X requires us to keep this non-negative */
    	R_ParseContextLast = (R_ParseContextLast + PARSE_CONTEXT_SIZE - 1) 
		% PARSE_CONTEXT_SIZE;
    }
    if (c == START_MACRO) macrolevel--;
    PUSH_BACK(c);
    /* Rprintf("unget %c;", c); */
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

static SEXP mkString2(const char *s, size_t len)
{
    SEXP t;
    cetype_t enc = CE_UTF8;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, (int) len, enc));
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
 
static SEXP ParseRd(ParseStatus *status, SEXP srcfile, Rboolean fragment, SEXP macros)
{
    Rboolean keepmacros = !isLogical(macros) || asLogical(macros);
    
    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
    
    parseState.xxlineno = 1;
    parseState.xxcolno = 1; 
    parseState.xxbyteno = 1;
    
    SrcFile = srcfile;
    
    npush = 0;
    pushbase = pushback;
    pushsize = PUSHBACK_BUFSIZE;
    macrolevel = 0;
    
    parseState.xxmode = LATEXLIKE; 
    parseState.xxitemType = UNKNOWN;
    parseState.xxbraceDepth = 0;
    parseState.xxinRString = 0;
    parseState.xxNewlineInString = 0;
    parseState.xxinEqn = 0;
    if (fragment) parseState.xxinitvalue = STARTFRAGMENT;
    else	  parseState.xxinitvalue = STARTFILE;
    
    if (!isEnvironment(macros))
	macros = InstallKeywords();
	
    PROTECT(macros);
    PROTECT(parseState.xxMacroList = R_NewHashedEnv(macros, ScalarInteger(0)));
    UNPROTECT_PTR(macros);
    
    parseState.Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;
    
    if (keepmacros && !isNull(parseState.Value))
	setAttrib(parseState.Value, install("macros"), parseState.xxMacroList);

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", parseState.Value);    
#endif    
    UNPROTECT_PTR(parseState.Value);
    UNPROTECT_PTR(parseState.xxMacroList);
    
    if (pushbase != pushback) free(pushbase);
    
    return parseState.Value;
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

static
SEXP R_ParseRd(Rconnection con, ParseStatus *status, SEXP srcfile, Rboolean fragment, SEXP macros)
{
    con_parse = con;
    ptr_getc = con_getc;
    return ParseRd(status, srcfile, fragment, macros);
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

/* When adding keywords here, make sure all the handlers 
   are also modified:  checkRd, Rd2HTML, Rd2latex, Rd2txt, any other new ones... */
   
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
    { "\\subsection", LATEXMACRO2 },
    
    /* This macro takes one verbatim and one LaTeX-like argument. */
    
    { "\\href",    VERBLATEX },
    
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
    
    /* This macro takes one optional bracketed option and one R-like argument */
    
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
    { "\\figure",  VERBMACRO2 },
    
    /* We parse IFDEF/IFNDEF as markup, not as a separate preprocessor step */ 
    
    { "#ifdef",    IFDEF },
    { "#ifndef",   IFDEF },
    { "#endif",    ENDIF },
    
    /* These allow user defined macros */
    { "\\newcommand", NEWCOMMAND },
    { "\\renewcommand", NEWCOMMAND },
    
    { 0,	   0	      }
    /* All other markup macros are rejected. */
};

/* Record the longest # directive here */
#define DIRECTIVE_LEN 7   

static SEXP InstallKeywords()
{
    int i, num;
    SEXP result, name, val;
    num = sizeof(keywords)/sizeof(keywords[0]);
    PROTECT(result = R_NewHashedEnv(R_EmptyEnv, ScalarInteger(num)));
    for (i = 0; keywords[i].name; i++) {
        PROTECT(name = install(keywords[i].name));
        PROTECT(val = ScalarInteger(keywords[i].token));
    	defineVar(name, val, result);
    	UNPROTECT(2);
    }
    UNPROTECT(1);
    return result;
}
    	
static int KeywordLookup(const char *s)
{
    SEXP rec = findVar(install(s), parseState.xxMacroList);
    if (rec == R_UnboundValue) return UNKNOWN;
    else return INTEGER(rec)[0];
}

static SEXP UserMacroLookup(const char *s)
{
    SEXP rec = findVar(install(s), parseState.xxMacroList);
    if (rec == R_UnboundValue) error(_("Unable to find macro %s"), s);
    return getAttrib(rec, install("definition"));
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
    		snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, _(yyshortunexpected),
			s + sizeof yyunexpected - 1);
    	    else
    	    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, _(yylongunexpected),
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
    	snprintf(ParseErrorMsg, PARSE_ERROR_SIZE, "%s", s);
    }
    filename = findVar(install("filename"), SrcFile);
    if (isString(filename) && length(filename))
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
	size_t nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
            nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = ((char) c);		    \
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
    int c, lookahead;
    int outsideLiteral = parseState.xxmode == LATEXLIKE || parseState.xxmode == INOPTION || parseState.xxbraceDepth == 0;

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
    c = xxgetc();

    switch (c) {
    	case '%': if (!parseState.xxinEqn) return mkComment(c);
    	    break;
	case '\\':
	    if (!parseState.xxinEqn) {
		lookahead = xxungetc(xxgetc());
		if (isalpha(lookahead) && parseState.xxmode != VERBATIM 
		    /* In R strings, only link or var is allowed as markup */
		    && (lookahead == 'l' || lookahead == 'v' || !parseState.xxinRString)) 
		    return mkMarkup(c);
	    }
	    break;
        case R_EOF:
            if (parseState.xxinRString) {
       		xxWarnNewline();
       		error(_("Unexpected end of input (in %c quoted string opened at %s:%d:%d)"), 
 			parseState.xxinRString, parseState.xxBasename, parseState.xxQuoteLine, parseState.xxQuoteCol);
    	    }
    	    return END_OF_INPUT; 
    	case '#':
    	    if (!parseState.xxinEqn && yylloc.first_column == 1) return mkIfdef(c);
    	    break;
    	case LBRACE:
    	    if (!parseState.xxinRString) {
    	    	parseState.xxbraceDepth++;
    	    	if (outsideLiteral) return c;
    	    }
    	    break;
    	case RBRACE:
    	    if (!parseState.xxinRString) {
    	    	parseState.xxbraceDepth--;
    	    	if (outsideLiteral || parseState.xxbraceDepth == 0) return c;
    	    }
    	    break;
    	case '[':
    	case ']':
    	    if (parseState.xxmode == INOPTION ) return c; 
    	    break;
    } 	    
	
    switch (parseState.xxmode) {
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
    	    lookahead = (char) xxgetc();
    	    if (lookahead == LBRACE || lookahead == RBRACE ||
    	        lookahead == '%' || lookahead == '\\') {
    	    	c = lookahead;
    	    	break;
    	    }
    	    xxungetc(lookahead);
    	    if (isalpha(lookahead)) goto stop;
    	case ']':
    	    if (parseState.xxmode == INOPTION) goto stop;
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
    PROTECT(yylval = mkString2(stext, bp - stext));
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
    
    PROTECT(yylval = mkString2(stext, bp - stext));
    if(stext != st0) free(stext);    
    return COMMENT;
}

static int mkCode(int c)
{
    char st0[INITBUFSIZE];
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    /* Avoid double counting initial braces */
    if (c == LBRACE && !parseState.xxinRString) parseState.xxbraceDepth--;
    if (c == RBRACE && !parseState.xxinRString) parseState.xxbraceDepth++; 
    
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
    	if (parseState.xxinRString) {
    	    /* This stuff is messy, because there are two levels of escaping:
    	       The Rd escaping and the R code string escaping. */
    	    if (c == '\\') {
    		int lookahead = xxgetc();
    		if (lookahead == '\\') { /* This must be the 3rd backslash */
    		    lookahead = xxgetc();
    		    if (lookahead == parseState.xxinRString || lookahead == '\\') {	
    	    	    	TEXT_PUSH(c);
    	    	    	c = lookahead;
    	    	    	escaped = 1;
    	    	    } else {
    	    	    	xxungetc(lookahead); /* put back the 4th char */
    	    	    	xxungetc('\\');	     /* and the 3rd */
    	    	    }
    	    	} else if (lookahead == parseState.xxinRString) { /* There could be one or two before this */
    	    	    TEXT_PUSH(c);
    	    	    c = lookahead;
    	    	    escaped = 1;
    	    	} else if (!escaped && (lookahead == 'l' || lookahead == 'v')) { 
    	    	    /* assume \link or \var; this breaks vertical tab, but does anyone ever use that? */
    	    	    xxungetc(lookahead);
    	    	    break;
    	    	} else xxungetc(lookahead);
    	    }
    	    if (!escaped && c == parseState.xxinRString)
    	    	parseState.xxinRString = 0;
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
    	    	    if (c == LBRACE && !escaped) parseState.xxbraceDepth++;
    	    	    else if (c == RBRACE && !escaped) parseState.xxbraceDepth--;
    	    	} while (c != '\n' && c != R_EOF && parseState.xxbraceDepth > 0);
    	    	if (c == RBRACE && !escaped) parseState.xxbraceDepth++; /* avoid double counting */
    	    }
    	    if (c == '\'' || c == '"' || c == '`') {
    	    	parseState.xxinRString = c;
    	    	parseState.xxQuoteLine = parseState.xxlineno;
    	    	parseState.xxQuoteCol  = parseState.xxcolno;
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
    	    	parseState.xxbraceDepth++;
    	    } else if (c == RBRACE) {
    	    	if (parseState.xxbraceDepth == 1) break;
    	    	else parseState.xxbraceDepth--;
    	    } else if (c == R_EOF) break;
    	}
    	TEXT_PUSH(c);
    	if (c == '\n') {
    	    if (parseState.xxinRString && !parseState.xxNewlineInString) 
    	    	parseState.xxNewlineInString = parseState.xxlineno-1;
    	    break;
    	}
    	c = xxgetc();
    }
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext, bp - stext));
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
    	    	    retval = parseState.xxitemType;
    	    	break;
    	    }
        }
    }
    PROTECT(yylval = mkString2(stext, bp - stext - 1));
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
    	switch (parseState.xxmode) {
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
    if (c == LBRACE) parseState.xxbraceDepth--;
    if (c == RBRACE) parseState.xxbraceDepth++;     
    
    while(1) {
    	int escaped = 0;
        if (c == '\\') {
            int lookahead = xxgetc();
            if (lookahead == '\\' || lookahead == '%' || lookahead == LBRACE || lookahead == RBRACE) {
		escaped = 1;
		if (parseState.xxinEqn) TEXT_PUSH(c);
		c = lookahead;
	    } else xxungetc(lookahead);
        }
        if (c == R_EOF) break;
        if (!escaped) {
    	    if (c == '%' && !parseState.xxinEqn) break;
	    else if (c == LBRACE) parseState.xxbraceDepth++;
    	    else if (c == RBRACE) {
	    	if (parseState.xxbraceDepth == 1) break;
	    	else parseState.xxbraceDepth--;
	    }
	}
    	TEXT_PUSH(c);
    	if (c == '\n') break;
    	c = xxgetc();
    };
    if (c != '\n') xxungetc(c);
    PROTECT(yylval = mkString2(stext, bp - stext));
    if(stext != st0) free(stext);
    return VERB;  
}

static int yylex(void)
{
    int tok = token();
    
    if (parseState.xxDebugTokens) {
        Rprintf("%d:%d: %s", yylloc.first_line, yylloc.first_column, yytname[YYTRANSLATE(tok)]);
    	if (parseState.xxinRString) Rprintf("(in %c%c)", parseState.xxinRString, parseState.xxinRString);
    	if (tok > 255 && tok != END_OF_INPUT) 
    	    Rprintf(": %s", CHAR(STRING_ELT(yylval, 0)));
	Rprintf("\n");
    }
    setlastloc();
    return tok;
}

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

static void PutState(ParseState *state) {
    state->xxinRString = parseState.xxinRString;
    state->xxQuoteLine = parseState.xxQuoteLine;
    state->xxQuoteCol = parseState.xxQuoteCol;
    state->xxinEqn = parseState.xxinEqn;
    state->xxNewlineInString = parseState.xxNewlineInString;
    state->xxlineno = parseState.xxlineno;
    state->xxbyteno = parseState.xxbyteno;
    state->xxcolno = parseState.xxcolno;
    state->xxmode = parseState.xxmode;
    state->xxitemType = parseState.xxitemType;
    state->xxbraceDepth = parseState.xxbraceDepth;
    state->xxDebugTokens = parseState.xxDebugTokens;
    state->xxBasename = parseState.xxBasename;
    state->Value = parseState.Value;
    state->xxinitvalue = parseState.xxinitvalue;
    state->xxMacroList = parseState.xxMacroList;
    state->prevState = parseState.prevState;
}

static void UseState(ParseState *state) {
    parseState.xxinRString = state->xxinRString;
    parseState.xxQuoteLine = state->xxQuoteLine;
    parseState.xxQuoteCol = state->xxQuoteCol;
    parseState.xxinEqn = state->xxinEqn;
    parseState.xxNewlineInString = state->xxNewlineInString;
    parseState.xxlineno = state->xxlineno;
    parseState.xxbyteno = state->xxbyteno;
    parseState.xxcolno = state->xxcolno;
    parseState.xxmode = state->xxmode;
    parseState.xxitemType = state->xxitemType;
    parseState.xxbraceDepth = state->xxbraceDepth;
    parseState.xxDebugTokens = state->xxDebugTokens;
    parseState.xxBasename = state->xxBasename;
    parseState.Value = state->Value;
    parseState.xxinitvalue = state->xxinitvalue;
    parseState.xxMacroList = state->xxMacroList;
    parseState.prevState = state->prevState;
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

/* "do_parseRd" 

 .External2(C_parseRd,file, srcfile, encoding, verbose, basename, warningCalls, macros, warndups)
 If there is text then that is read and the other arguments are ignored.
*/

SEXP C_parseRd(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);

    SEXP s = R_NilValue, source;
    Rconnection con;
    Rboolean wasopen, fragment;
    int ifile, wcall;
    ParseStatus status;
    RCNTXT cntxt;
    SEXP macros;

#if DEBUGMODE
    yydebug = 1;
#endif 

    R_ParseError = 0;
    R_ParseErrorMsg[0] = '\0';
    
    PushState();

    ifile = asInteger(CAR(args));                       args = CDR(args);

    con = getConnection(ifile);
    wasopen = con->isopen;
    source = CAR(args);					args = CDR(args);
    /* encoding is unused */
    args = CDR(args);
    if(!isLogical(CAR(args)) || LENGTH(CAR(args)) != 1)
    	error(_("invalid '%s' value"), "verbose");
    parseState.xxDebugTokens = asInteger(CAR(args));		args = CDR(args);
    parseState.xxBasename = CHAR(STRING_ELT(CAR(args), 0));	args = CDR(args);
    fragment = asLogical(CAR(args));				args = CDR(args);
    wcall = asLogical(CAR(args));				args = CDR(args);
    if (wcall == NA_LOGICAL)
    	error(_("invalid '%s' value"), "warningCalls");
    wCalls = wcall;
    macros = CAR(args);						args = CDR(args);
    warnDups = asLogical(CAR(args));

    if (ifile >= 3) {/* file != "" */
	if(!wasopen) {
	    if(!con->open(con)) error(_("cannot open the connection"));
	    /* Set up a context which will close the connection on error */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			 R_NilValue, R_NilValue);
	    cntxt.cend = &con_cleanup;
	    cntxt.cenddata = con;
	}
	if(!con->canread) error(_("cannot read from this connection"));
	s = R_ParseRd(con, &status, source, fragment, macros);
	if(!wasopen) endcontext(&cntxt);
	PopState();
	if (status != PARSE_OK) parseError(call, R_ParseError);
    }
    else {
      PopState();
      error(_("invalid Rd file"));
    }
    return s;
}

/* "do_deparseRd" 

 .External2(C_deparseRd, element, state)
*/

SEXP C_deparseRd(SEXP e, SEXP state)
{
    SEXP result;
    int  outlen, *statevals, quoteBraces, inRComment;
    const char *c;
    char *outbuf, *out, lookahead;
    Rboolean escape;

    if(!isString(e) || length(e) != 1) 
    	error(_("'deparseRd' only supports deparsing character elements"));
    e = STRING_ELT(e, 0);
    
    if(!isInteger(state) || length(state) != 5) error(_("bad state"));
    
    PushState();
    
    parseState.xxbraceDepth = INTEGER(state)[0];
    parseState.xxinRString = INTEGER(state)[1];
    parseState.xxmode = INTEGER(state)[2];
    parseState.xxinEqn = INTEGER(state)[3];
    quoteBraces = INTEGER(state)[4];
    
    if (parseState.xxmode != LATEXLIKE && parseState.xxmode != RLIKE && parseState.xxmode != VERBATIM && parseState.xxmode != COMMENTMODE 
     && parseState.xxmode != INOPTION  && parseState.xxmode != UNKNOWNMODE) {
        PopState();
    	error(_("bad text mode %d in 'deparseRd'"), parseState.xxmode);
    }
    
    for (c = CHAR(e), outlen=0; *c; c++) {
    	outlen++;
    	/* any special char might be escaped */
    	if (*c == '{' || *c == '}' || *c == '%' || *c == '\\') outlen++;
    }
    out = outbuf = R_chk_calloc(outlen+1, sizeof(char));
    inRComment = FALSE;
    for (c = CHAR(e); *c; c++) {
    	escape = FALSE;
    	if (parseState.xxmode != UNKNOWNMODE) {
	    switch (*c) {
	    case '\\':
		if (parseState.xxmode == RLIKE && parseState.xxinRString) {
		    lookahead = *(c+1);
		    if (lookahead == '\\' || lookahead == parseState.xxinRString || lookahead == 'l') 
		    	escape = TRUE;
		    break;
		}          /* fall through to % case for non-strings... */    
	    case '%':
		if (parseState.xxmode != COMMENTMODE && !parseState.xxinEqn)
		    escape = TRUE;
		break;
	    case LBRACE:
	    case RBRACE:
		if (quoteBraces)
		    escape = TRUE;
		else if (!parseState.xxinRString && !parseState.xxinEqn && (parseState.xxmode == RLIKE || parseState.xxmode == VERBATIM)) {
		    if (*c == LBRACE) parseState.xxbraceDepth++;
		    else if (parseState.xxbraceDepth <= 0) escape = TRUE;
		    else parseState.xxbraceDepth--;
		}
		break;
	    case '\'':
	    case '"':
	    case '`':
	    	if (parseState.xxmode == RLIKE) {
		    if (parseState.xxinRString) {
			if (parseState.xxinRString == *c) parseState.xxinRString = 0;
		    } else if (!inRComment) parseState.xxinRString = *c;
		}
		break;
	    case '#':
	    	if (parseState.xxmode == RLIKE && !parseState.xxinRString) 
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
    statevals[0] = parseState.xxbraceDepth;
    statevals[1] = parseState.xxinRString;
    
    PopState();
    
    UNPROTECT(1);
    return result;
}

