/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */

/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2022  The R Core Team
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
#ifndef STRICT_R_HEADERS
# define STRICT_R_HEADERS
#endif
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

/* bison creates a non-static symbol yylloc (and other) in both gramLatex.o
   and gramRd.o, so remap */

#define yylloc yyllocL
#undef yynerrs /* from Defn.h */
#define yynerrs yynerrsL
#undef yychar /* from Defn.h */
#define yychar yycharL
#undef yylval /* from Defn.h */
#define yylval yylvalL

#define DEBUGVALS 0		/* 1 causes detailed internal state output to R console */	
#define DEBUGMODE 0		/* 1 causes Bison output of parse state, to stdout or stderr */

#define YYERROR_VERBOSE 1

static void yyerror(const char *);
static int yylex(void);
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
static int	xxgetc(void);
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
    SEXP	xxVerbList;    /* A STRSXP containing all the verbatim command names */

    SEXP     SrcFile;  /* parseLatex will *always* supply a srcfile */
    SEXP mset; /* precious mset for protecting parser semantic values */
    ParseState *prevState;
};

static Rboolean busy = FALSE;
static ParseState parseState;

#define PRESERVE_SV(x) R_PreserveInMSet((x), parseState.mset)
#define RELEASE_SV(x)  R_ReleaseFromMSet((x), parseState.mset)

/* Routines used to build the parse tree */

static SEXP	xxnewlist(SEXP);
static SEXP	xxlist(SEXP, SEXP);
static void	xxsavevalue(SEXP, YYLTYPE *);
static SEXP	xxtag(SEXP, int, YYLTYPE *);
static SEXP 	xxenv(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxmath(SEXP, YYLTYPE *, Rboolean);
static SEXP	xxblock(SEXP, YYLTYPE *);
static void	xxSetInVerbEnv(SEXP);

static int	mkMarkup(int);
static int	mkText(int);
static int 	mkComment(int);
static int      mkVerb(int);
static int	mkVerb2(const char *, int);
static int      mkVerbEnv(void);
static int	mkDollar(int);

static SEXP R_LatexTagSymbol = NULL;

#define YYSTYPE		SEXP



# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    END_OF_INPUT = 258,            /* END_OF_INPUT  */
    ERROR = 259,                   /* ERROR  */
    MACRO = 260,                   /* MACRO  */
    TEXT = 261,                    /* TEXT  */
    COMMENT = 262,                 /* COMMENT  */
    BEGIN = 263,                   /* BEGIN  */
    END = 264,                     /* END  */
    VERB = 265,                    /* VERB  */
    VERB2 = 266,                   /* VERB2  */
    TWO_DOLLARS = 267              /* TWO_DOLLARS  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif
/* Token kinds.  */
#define YYEMPTY -2
#define YYEOF 0
#define YYerror 256
#define YYUNDEF 257
#define END_OF_INPUT 258
#define ERROR 259
#define MACRO 260
#define TEXT 261
#define COMMENT 262
#define BEGIN 263
#define END 264
#define VERB 265
#define VERB2 266
#define TWO_DOLLARS 267

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


extern YYSTYPE yylval;
extern YYLTYPE yylloc;

int yyparse (void);



/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_END_OF_INPUT = 3,               /* END_OF_INPUT  */
  YYSYMBOL_ERROR = 4,                      /* ERROR  */
  YYSYMBOL_MACRO = 5,                      /* MACRO  */
  YYSYMBOL_TEXT = 6,                       /* TEXT  */
  YYSYMBOL_COMMENT = 7,                    /* COMMENT  */
  YYSYMBOL_BEGIN = 8,                      /* BEGIN  */
  YYSYMBOL_END = 9,                        /* END  */
  YYSYMBOL_VERB = 10,                      /* VERB  */
  YYSYMBOL_VERB2 = 11,                     /* VERB2  */
  YYSYMBOL_TWO_DOLLARS = 12,               /* TWO_DOLLARS  */
  YYSYMBOL_13_ = 13,                       /* '{'  */
  YYSYMBOL_14_ = 14,                       /* '}'  */
  YYSYMBOL_15_ = 15,                       /* '$'  */
  YYSYMBOL_YYACCEPT = 16,                  /* $accept  */
  YYSYMBOL_Init = 17,                      /* Init  */
  YYSYMBOL_Items = 18,                     /* Items  */
  YYSYMBOL_nonMath = 19,                   /* nonMath  */
  YYSYMBOL_Item = 20,                      /* Item  */
  YYSYMBOL_environment = 21,               /* environment  */
  YYSYMBOL_22_1 = 22,                      /* $@1  */
  YYSYMBOL_math = 23,                      /* math  */
  YYSYMBOL_displaymath = 24,               /* displaymath  */
  YYSYMBOL_block = 25                      /* block  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

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
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
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
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
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
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  25
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   122

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  16
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  10
/* YYNRULES -- Number of rules.  */
#define YYNRULES  25
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  42

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   267


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    15,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    13,     2,    14,     2,     2,     2,     2,
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
       5,     6,     7,     8,     9,    10,    11,    12
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,   186,   186,   187,   188,   191,   192,   193,   194,   195,
     196,   198,   199,   201,   202,   203,   204,   205,   206,   207,
     209,   209,   213,   215,   217,   218
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "END_OF_INPUT",
  "ERROR", "MACRO", "TEXT", "COMMENT", "BEGIN", "END", "VERB", "VERB2",
  "TWO_DOLLARS", "'{'", "'}'", "'$'", "$accept", "Init", "Items",
  "nonMath", "Item", "environment", "$@1", "math", "displaymath", "block", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-13)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int8 yypact[] =
{
      32,   -13,   -13,   -13,   -13,   -13,   -12,   -13,   -13,   109,
      54,   109,     5,    43,   -13,   -13,   -13,   -13,   -13,     0,
      14,   -13,   -13,    65,    98,   -13,   -13,   -13,   -13,   -13,
      -4,   -13,   -13,   -13,   -13,   -13,    87,    76,    -1,     3,
       2,   -13
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     4,     3,    15,    13,    14,     0,    16,    17,     0,
       0,     0,     0,     0,     5,    18,     6,     7,    19,     0,
       0,    11,    25,     0,     0,     1,     2,     8,     9,    10,
       0,    23,    12,    24,    22,    20,     0,     0,     0,     0,
       0,    21
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -13,   -13,    -7,    12,    -9,   -13,   -13,    -6,    -5,   -13
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
       0,    12,    13,    20,    14,    15,    36,    16,    17,    18
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      21,    19,    21,    23,    27,    25,    30,    28,    29,    40,
      35,    32,    39,     0,    27,    32,    41,    28,    29,     3,
       4,     5,     6,    24,     7,     8,    31,    10,    27,    37,
       0,    28,    29,     1,     0,     2,     0,     3,     4,     5,
       6,     0,     7,     8,     9,    10,    26,    11,     3,     4,
       5,     6,     0,     7,     8,     9,    10,     0,    11,     3,
       4,     5,     6,     0,     7,     8,     9,    10,    22,    11,
       3,     4,     5,     6,     0,     7,     8,     9,    10,    33,
      11,     3,     4,     5,     6,    38,     7,     8,     9,    10,
       0,    11,     3,     4,     5,     6,     0,     7,     8,     9,
      10,     0,    11,     3,     4,     5,     6,     0,     7,     8,
       0,    10,     0,    34,     3,     4,     5,     6,     0,     7,
       8,     0,    10
};

static const yytype_int8 yycheck[] =
{
       9,    13,    11,    10,    13,     0,     6,    13,    13,     6,
      14,    20,    13,    -1,    23,    24,    14,    23,    23,     5,
       6,     7,     8,    11,    10,    11,    12,    13,    37,    36,
      -1,    37,    37,     1,    -1,     3,    -1,     5,     6,     7,
       8,    -1,    10,    11,    12,    13,     3,    15,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    -1,    15,     5,
       6,     7,     8,    -1,    10,    11,    12,    13,    14,    15,
       5,     6,     7,     8,    -1,    10,    11,    12,    13,    14,
      15,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      -1,    15,     5,     6,     7,     8,    -1,    10,    11,    12,
      13,    -1,    15,     5,     6,     7,     8,    -1,    10,    11,
      -1,    13,    -1,    15,     5,     6,     7,     8,    -1,    10,
      11,    -1,    13
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,    10,    11,    12,
      13,    15,    17,    18,    20,    21,    23,    24,    25,    13,
      19,    20,    14,    18,    19,     0,     3,    20,    23,    24,
       6,    12,    20,    14,    15,    14,    22,    18,     9,    13,
       6,    14
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr1[] =
{
       0,    16,    17,    17,    17,    18,    18,    18,    18,    18,
      18,    19,    19,    20,    20,    20,    20,    20,    20,    20,
      22,    21,    23,    24,    25,    25
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     1,     1,     1,     1,     1,     2,     2,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       0,    10,     3,     3,     3,     2
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
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
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]));
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
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






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yykind)
    {
    case YYSYMBOL_MACRO: /* MACRO  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_TEXT: /* TEXT  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_COMMENT: /* COMMENT  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_BEGIN: /* BEGIN  */
            { RELEASE_SV((*yyvaluep)); }
        break;

    case YYSYMBOL_END: /* END  */
            { RELEASE_SV((*yyvaluep)); }
        break;

      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
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

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* Init: Items END_OF_INPUT  */
                                                { xxsavevalue(yyvsp[-1], &(yyloc)); YYACCEPT; }
    break;

  case 3: /* Init: END_OF_INPUT  */
                                                { xxsavevalue(NULL, &(yyloc)); YYACCEPT; }
    break;

  case 4: /* Init: error  */
                                                { PRESERVE_SV(parseState.Value = R_NilValue);  YYABORT; }
    break;

  case 5: /* Items: Item  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 6: /* Items: math  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 7: /* Items: displaymath  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 8: /* Items: Items Item  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 9: /* Items: Items math  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 10: /* Items: Items displaymath  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 11: /* nonMath: Item  */
                                                { yyval = xxnewlist(yyvsp[0]); }
    break;

  case 12: /* nonMath: nonMath Item  */
                                                { yyval = xxlist(yyvsp[-1], yyvsp[0]); }
    break;

  case 13: /* Item: TEXT  */
                                                { yyval = xxtag(yyvsp[0], TEXT, &(yyloc)); }
    break;

  case 14: /* Item: COMMENT  */
                                                { yyval = xxtag(yyvsp[0], COMMENT, &(yyloc)); }
    break;

  case 15: /* Item: MACRO  */
                                                { yyval = xxtag(yyvsp[0], MACRO, &(yyloc)); }
    break;

  case 16: /* Item: VERB  */
                                                { yyval = xxtag(yyvsp[0], VERB, &(yyloc)); }
    break;

  case 17: /* Item: VERB2  */
                                                { yyval = xxtag(yyvsp[0], VERB, &(yyloc)); }
    break;

  case 18: /* Item: environment  */
                                                { yyval = yyvsp[0]; }
    break;

  case 19: /* Item: block  */
                                                { yyval = yyvsp[0]; }
    break;

  case 20: /* $@1: %empty  */
                                   { xxSetInVerbEnv(yyvsp[-1]); }
    break;

  case 21: /* environment: BEGIN '{' TEXT '}' $@1 Items END '{' TEXT '}'  */
                                        { yyval = xxenv(yyvsp[-7], yyvsp[-4], yyvsp[-1], &(yyloc));
                                                  RELEASE_SV(yyvsp[-9]); RELEASE_SV(yyvsp[-3]); }
    break;

  case 22: /* math: '$' nonMath '$'  */
                                                { yyval = xxmath(yyvsp[-1], &(yyloc), FALSE); }
    break;

  case 23: /* displaymath: TWO_DOLLARS nonMath TWO_DOLLARS  */
                                                { yyval = xxmath(yyvsp[-1], &(yyloc), TRUE); }
    break;

  case 24: /* block: '{' Items '}'  */
                                                { yyval = xxblock(yyvsp[-1], &(yyloc)); }
    break;

  case 25: /* block: '{' '}'  */
                                                { yyval = xxblock(NULL, &(yyloc)); }
    break;



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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
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
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
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
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}



static SEXP xxnewlist(SEXP item)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxnewlist(item=%p)", item);
#endif    
    PRESERVE_SV(ans = NewList());
    if (item) {
	GrowList(ans, item);
	RELEASE_SV(item);
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
    RELEASE_SV(item);
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
    PRESERVE_SV(ans = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(ans, 0, begin);
    RELEASE_SV(begin);
    if (!isNull(body)) {
	SET_VECTOR_ELT(ans, 1, PairToVectorList(CDR(body)));
	RELEASE_SV(body);
    }
    /* FIXME:  check that begin and end match */
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    setAttrib(ans, R_LatexTagSymbol, mkString("ENVIRONMENT"));
    if (!isNull(end)) 
	RELEASE_SV(end);
#if DEBUGVALS
    Rprintf(" result: %p\n", ans);    
#endif
    return ans;
}

static SEXP xxmath(SEXP body, YYLTYPE *lloc, Rboolean display)
{
    SEXP ans;
#if DEBUGVALS
    Rprintf("xxmath(body=%p, display=%d)", body, display);    
#endif
    PRESERVE_SV(ans = PairToVectorList(CDR(body)));
    RELEASE_SV(body);
    setAttrib(ans, R_SrcrefSymbol, makeSrcref(lloc, parseState.SrcFile));
    setAttrib(ans, R_LatexTagSymbol, 
        mkString(display ? "DISPLAYMATH" : "MATH"));
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
        PRESERVE_SV(ans = allocVector(VECSXP, 0));
    else {
	PRESERVE_SV(ans = PairToVectorList(CDR(body)));
	RELEASE_SV(body);
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
	PRESERVE_SV(parseState.xxInVerbEnv = ScalarString(mkChar(buffer)));
    } else parseState.xxInVerbEnv = NULL;
}

static void xxsavevalue(SEXP items, YYLTYPE *lloc)
{
    if (items) {
	PRESERVE_SV(parseState.Value = PairToVectorList(CDR(items)));
	RELEASE_SV(items);
    } else {
	PRESERVE_SV(parseState.Value = allocVector(VECSXP, 1));
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
    state->xxVerbList = parseState.xxVerbList;
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
    parseState.xxVerbList = state->xxVerbList;
    parseState.SrcFile = state->SrcFile; 
    parseState.prevState = state->prevState;
}

static void InitSymbols(void)
{
    if (!R_LatexTagSymbol)
	R_LatexTagSymbol = install("latex_tag");
}

static SEXP ParseLatex(ParseStatus *status, SEXP srcfile)
{
    InitSymbols();

    R_ParseContextLast = 0;
    R_ParseContext[0] = '\0';
    	
    parseState.xxInVerbEnv = NULL;
    
    parseState.xxlineno = 1;
    parseState.xxcolno = 1; 
    parseState.xxbyteno = 1;
    
    parseState.SrcFile = srcfile;

    PROTECT(parseState.mset = R_NewPreciousMSet(50));
    
    npush = 0;
    
    parseState.Value = R_NilValue;
    
    if (yyparse()) *status = PARSE_ERROR;
    else *status = PARSE_OK;

#if DEBUGVALS
    Rprintf("ParseRd result: %p\n", parseState.Value);    
#endif

    RELEASE_SV(parseState.Value);
    UNPROTECT(1); /* parseState.mset */

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
    
    for (i = 0; i < length(parseState.xxVerbList); i++) {
    	if (strcmp(CHAR(STRING_ELT(parseState.xxVerbList, i)), s) == 0)
    	    return VERB2;
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

#define TEXT_PUSH(c) do {		    \
	size_t nc = bp - stext;		    \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
	    nstext *= 2;		    \
	    stext = malloc(nstext);	    \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), parseState.xxlineno);\
	    memmove(stext, old, nc);        \
	    if(st1) free(st1);		    \
	    st1 = stext;		    \
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
	PRESERVE_SV(yylval = mkString(""));
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
    	case '$': return mkDollar(c);
    } 	    
    return mkText(c);
}

#define INITBUFSIZE 128

static int mkText(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
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
    PRESERVE_SV(yylval = mkString2(stext,  bp - stext));
    if(st1) free(st1);
    return TEXT;
}

static int mkComment(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    
    do TEXT_PUSH(c);
    while ((c = xxgetc()) != '\n' && c != R_EOF);
    
    if (c == R_EOF) xxungetc(c);
    else TEXT_PUSH(c);
    
    PRESERVE_SV(yylval = mkString2(stext,  bp - stext));
    if(st1) free(st1);    
    return COMMENT;
}

static int mkDollar(int c)
{
    int retval = c;
    
    if ((c = xxgetc()) == '$')
        retval = TWO_DOLLARS;
    else
        xxungetc(c);

    return retval;
}

static int mkMarkup(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
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
        else if (retval == VERB2)
            retval = mkVerb2(stext, c); /* ditto */
        else if (c != ' ') /* Eat a space, but keep other terminators */
    	    xxungetc(c);
    }
    if (retval != VERB)
	PRESERVE_SV(yylval = mkString(stext));
    if(st1) free(st1);
    return retval;
}

static int mkVerb(int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int delim = c;   
    
    TEXT_PUSH('\\'); TEXT_PUSH('v'); TEXT_PUSH('e'); TEXT_PUSH('r'); TEXT_PUSH('b');
    TEXT_PUSH(c);
    while (((c = xxgetc()) != delim) && c != R_EOF) TEXT_PUSH(c);
    if (c != R_EOF) TEXT_PUSH(c);
    
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return VERB;  
}

static int mkVerb2(const char *s, int c)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
    unsigned int nstext = INITBUFSIZE;
    char *stext = st0, *bp = st0;
    int c0, delim = '}';  
    
    while (*s) TEXT_PUSH(*s++);
    
    TEXT_PUSH(c);
    while (((c = xxgetc()) != delim) && c != R_EOF) TEXT_PUSH(c);
    if (c != R_EOF) TEXT_PUSH(c);
    
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if(st1) free(st1);
    return VERB;  
}

static int mkVerbEnv(void)
{
    char st0[INITBUFSIZE];
    char *st1 = NULL;
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
        xxungetc(c);
    	for (i = matched-1; i >= 0; i--) 
    	    xxungetc(*(--bp));    	    
	RELEASE_SV(parseState.xxInVerbEnv);
    	parseState.xxInVerbEnv = NULL;
    }
    	    
    PRESERVE_SV(yylval = mkString2(stext, bp - stext));
    if (st1) free(st1);
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

static void PushState(void) {
    if (busy) {
    	ParseState *prev = malloc(sizeof(ParseState));
	if (prev == NULL) error("unable to allocate in PushState");
    	PutState(prev);
    	parseState.prevState = prev;
    } else 
        parseState.prevState = NULL;  
    busy = TRUE;
}

static void PopState(void) {
    if (parseState.prevState) {
    	ParseState *prev = parseState.prevState;
    	UseState(prev);
    	free(prev);
    } else
    	busy = FALSE;
}

/* "do_parseLatex" 

 .External2("parseLatex", text, srcfile, verbose, verbatim, verb)
 If there is text then that is read and the other arguments are ignored.
*/

SEXP parseLatex(SEXP call, SEXP op, SEXP args, SEXP env)
{
    args = CDR(args);

    SEXP s = R_NilValue, source, text;
    ParseStatus status;

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
    parseState.xxVerbList = CAR(args);

    s = R_ParseLatex(text, &status, source);
    
    PopState();
    	
    if (status != PARSE_OK) parseError(call, R_ParseError);
    return s;
}
