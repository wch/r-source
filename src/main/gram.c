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
     AND2 = 285,
     OR2 = 286,
     NS_GET = 287,
     NS_GET_INT = 288,
     LOW = 289,
     TILDE = 290,
     NOT = 291,
     UNOT = 292,
     SPECIAL = 293,
     UPLUS = 294,
     UMINUS = 295
   };
#endif
/* Tokens.  */
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
#define AND2 285
#define OR2 286
#define NS_GET 287
#define NS_GET_INT 288
#define LOW 289
#define TILDE 290
#define NOT 291
#define UNOT 292
#define SPECIAL 293
#define UPLUS 294
#define UMINUS 295




/* Copy the first part of user declarations.  */


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

#define YYERROR_VERBOSE 1

static void yyerror(char *);
static int yylex();
int yyparse(void);

/* alloca.h inclusion is now covered by Defn.h */

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

static void	CheckFormalArgs(SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);
static SEXP	GrowList(SEXP, SEXP);
static void	IfPush(void);
static int	KeywordLookup(const char *);
static SEXP	NewList(void);
static SEXP	NextArg(SEXP, SEXP, SEXP);
static SEXP	TagArg(SEXP, SEXP);

/* These routines allocate constants */

static SEXP	mkComplex(const char *);
SEXP		mkFalse(void);
static SEXP     mkFloat(const char *);
static SEXP	mkNA(void);
SEXP		mkTrue(void);

/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	xxgetc();
static int	xxungetc(int);
static int 	xxcharcount, xxcharsave;
static int	xxlineno, xxcolno, xxlinesave, xxcolsave;
static int	xxlastlinelen;

static SEXP     SrcFile = NULL;
static SEXP	SrcRefs = NULL;
static PROTECT_INDEX srindex;

#if defined(SUPPORT_MBCS)
# include <R_ext/Riconv.h>
# include <R_ext/rlocale.h>
# include <sys/param.h>
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif

#ifdef WORDS_BIGENDIAN
static const char UNICODE[] = "UCS-4BE";
#else
static const char UNICODE[] = "UCS-4LE";
#endif
#include <errno.h>

static size_t ucstomb(char *s, const unsigned int wc, mbstate_t *ps)
{
    char     tocode[128];
    char     buf[16];
    void    *cd = NULL ;
    unsigned int  wcs[2];
    const char *inbuf = (const char *) wcs;
    size_t   inbytesleft = sizeof(unsigned int); /* better be 4 */
    char    *outbuf = buf;
    size_t   outbytesleft = sizeof(buf);
    size_t   status;
    
    if(wc == 0) {
	*s = '\0';
        return 1;
    }
    
    strcpy(tocode, "");
    memset(buf, 0, sizeof(buf));
    memset(wcs, 0, sizeof(wcs));
    wcs[0] = wc;

    if((void *)(-1) == (cd = Riconv_open("", UNICODE))) {
#ifndef  Win32
        /* locale set fuzzy case */
    	strncpy(tocode, locale2charset(NULL), sizeof(tocode));
	if((void *)(-1) == (cd = Riconv_open(tocode, UNICODE)))
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
	    if(s[i] == R_EOF) error(_("EOF whilst reading MBCS char"));
	}
	res = mbrtowc(wc, s, clen, NULL);
	if(res == -1) error(_("invalid multibyte character in mbcs_get_next"));
    } else {
	/* This is not necessarily correct for stateful MBCS */
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
static SEXP	xxexprlist1(SEXP, YYLTYPE *);
static SEXP	xxexprlist2(SEXP, SEXP, YYLTYPE *);
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
static int	xxvalue(SEXP, int, YYLTYPE *);

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
#define YYFINAL  46
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   709

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  62
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  13
/* YYNRULES -- Number of rules.  */
#define YYNRULES  90
/* YYNRULES -- Number of states.  */
#define YYNSTATES  163

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   295

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      53,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    58,     2,     2,    49,    59,     2,     2,
      51,    57,    42,    40,    61,    41,     2,    43,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    45,    54,
       2,     2,     2,    34,    50,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    52,     2,    60,    48,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    55,     2,    56,    36,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    35,
      37,    38,    39,    44,    46,    47
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    13,    15,    17,    19,
      23,    25,    27,    29,    31,    35,    39,    42,    45,    48,
      51,    54,    58,    62,    66,    70,    74,    78,    82,    86,
      90,    94,    98,   102,   106,   110,   114,   118,   122,   126,
     130,   134,   138,   142,   149,   154,   158,   164,   168,   172,
     175,   181,   186,   190,   194,   198,   202,   206,   210,   214,
     218,   222,   226,   230,   234,   236,   238,   242,   246,   252,
     253,   255,   259,   262,   266,   269,   271,   276,   277,   279,
     282,   286,   289,   293,   296,   300,   301,   303,   307,   311,
     317
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      63,     0,    -1,     3,    -1,    53,    -1,    64,    53,    -1,
      64,    54,    -1,     1,    -1,    66,    -1,    65,    -1,    66,
      11,    64,    -1,     6,    -1,     5,    -1,     7,    -1,     8,
      -1,    55,    70,    56,    -1,    51,    64,    57,    -1,    41,
      66,    -1,    40,    66,    -1,    58,    66,    -1,    36,    66,
      -1,    34,    66,    -1,    66,    45,    66,    -1,    66,    40,
      66,    -1,    66,    41,    66,    -1,    66,    42,    66,    -1,
      66,    43,    66,    -1,    66,    48,    66,    -1,    66,    44,
      66,    -1,    66,    59,    66,    -1,    66,    36,    66,    -1,
      66,    34,    66,    -1,    66,    24,    66,    -1,    66,    25,
      66,    -1,    66,    26,    66,    -1,    66,    27,    66,    -1,
      66,    23,    66,    -1,    66,    22,    66,    -1,    66,    28,
      66,    -1,    66,    29,    66,    -1,    66,    30,    66,    -1,
      66,    31,    66,    -1,    66,    10,    66,    -1,    66,    12,
      66,    -1,     9,    51,    73,    57,    74,    64,    -1,    66,
      51,    71,    57,    -1,    16,    68,    64,    -1,    16,    68,
      64,    17,    64,    -1,    14,    69,    64,    -1,    18,    67,
      64,    -1,    21,    64,    -1,    66,    13,    71,    60,    60,
      -1,    66,    52,    71,    60,    -1,     8,    32,     8,    -1,
       8,    32,     5,    -1,     5,    32,     8,    -1,     5,    32,
       5,    -1,     8,    33,     8,    -1,     8,    33,     5,    -1,
       5,    33,     8,    -1,     5,    33,     5,    -1,    66,    49,
       8,    -1,    66,    49,     5,    -1,    66,    50,     8,    -1,
      66,    50,     5,    -1,    19,    -1,    20,    -1,    51,    66,
      57,    -1,    51,    66,    57,    -1,    51,     8,    15,    66,
      57,    -1,    -1,    64,    -1,    70,    54,    64,    -1,    70,
      54,    -1,    70,    53,    64,    -1,    70,    53,    -1,    72,
      -1,    71,    74,    61,    72,    -1,    -1,    66,    -1,     8,
      11,    -1,     8,    11,    66,    -1,     5,    11,    -1,     5,
      11,    66,    -1,     7,    11,    -1,     7,    11,    66,    -1,
      -1,     8,    -1,     8,    11,    66,    -1,    73,    61,     8,
      -1,    73,    61,     8,    11,    66,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   296,   296,   297,   298,   299,   300,   303,   304,   307,
     310,   311,   312,   313,   315,   316,   318,   319,   320,   321,
     322,   324,   325,   326,   327,   328,   329,   330,   331,   332,
     333,   334,   335,   336,   337,   338,   339,   340,   341,   342,
     343,   345,   346,   347,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   374,   377,   380,   384,
     385,   386,   387,   388,   389,   392,   393,   396,   397,   398,
     399,   400,   401,   402,   403,   406,   407,   408,   409,   410,
     413
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "STR_CONST",
  "NUM_CONST", "NULL_CONST", "SYMBOL", "FUNCTION", "LEFT_ASSIGN",
  "EQ_ASSIGN", "RIGHT_ASSIGN", "LBB", "FOR", "IN", "IF", "ELSE", "WHILE",
  "NEXT", "BREAK", "REPEAT", "GT", "GE", "LT", "LE", "EQ", "NE", "AND",
  "OR", "AND2", "OR2", "NS_GET", "NS_GET_INT", "'?'", "LOW", "'~'",
  "TILDE", "NOT", "UNOT", "'+'", "'-'", "'*'", "'/'", "SPECIAL", "':'",
  "UPLUS", "UMINUS", "'^'", "'$'", "'@'", "'('", "'['", "'\\n'", "';'",
  "'{'", "'}'", "')'", "'!'", "'%'", "']'", "','", "$accept", "prog",
  "expr_or_assign", "equal_assign", "expr", "cond", "ifcond", "forcond",
  "exprlist", "sublist", "sub", "formlist", "cr", 0
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
     285,   286,   287,   288,    63,   289,   126,   290,   291,   292,
      43,    45,    42,    47,   293,    58,   294,   295,    94,    36,
      64,    40,    91,    10,    59,   123,   125,    41,    33,    37,
      93,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    62,    63,    63,    63,    63,    63,    64,    64,    65,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    67,    68,    69,    70,
      70,    70,    70,    70,    70,    71,    71,    72,    72,    72,
      72,    72,    72,    72,    72,    73,    73,    73,    73,    73,
      74
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     1,     1,     1,     3,
       1,     1,     1,     1,     3,     3,     2,     2,     2,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     6,     4,     3,     5,     3,     3,     2,
       5,     4,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     1,     1,     3,     3,     5,     0,
       1,     3,     2,     3,     2,     1,     4,     0,     1,     2,
       3,     2,     3,     2,     3,     0,     1,     3,     3,     5,
       0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     2,    11,    10,    12,    13,     0,     0,     0,
       0,    64,    65,     0,     0,     0,     0,     0,     0,     3,
      69,     0,     0,     0,     8,     7,     0,     0,     0,     0,
      85,     0,     0,     0,     0,     0,     0,    49,    20,    19,
      17,    16,     0,    70,     0,    18,     1,     4,     5,     0,
       0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,    77,     0,    55,    54,    59,
      58,    53,    52,    57,    56,    86,     0,     0,    47,     0,
      45,     0,    48,    15,    74,    72,    14,    41,     9,    42,
      11,    12,    13,    78,    90,    75,    36,    35,    31,    32,
      33,    34,    37,    38,    39,    40,    30,    29,    22,    23,
      24,    25,    27,    21,    26,    61,    60,    63,    62,    90,
      90,    28,     0,    90,     0,     0,    67,     0,    66,    73,
      71,    81,    83,    79,     0,     0,    44,    51,    87,     0,
      88,     0,    46,    82,    84,    80,    50,    77,    43,     0,
      68,    76,    89
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    22,    23,    24,    25,    36,    34,    32,    44,   104,
     105,    86,   145
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -53
static const yytype_int16 yypact[] =
{
      83,   -53,   -53,    37,   -53,   -53,    61,   -35,   -33,   -29,
     -27,   -53,   -53,   144,   144,   144,   144,   144,   144,   -53,
     144,   144,     8,    56,   -53,   235,     7,     9,    15,    24,
      58,    70,   144,   144,   144,   144,   144,   -53,   450,   530,
     141,   141,     2,   -53,   -43,   610,   -53,   -53,   -53,   144,
     144,   144,   198,   144,   144,   144,   144,   144,   144,   144,
     144,   144,   144,   144,   144,   144,   144,   144,   144,   144,
     144,   144,    25,    26,   198,   198,   144,   -53,   -53,   -53,
     -53,   -53,   -53,   -53,   -53,    53,   -52,    65,   -53,   278,
      66,   321,   -53,   -53,   144,   144,   -53,   450,   -53,   490,
      -7,    74,    -5,   407,    27,   -53,   650,   650,   650,   650,
     650,   650,   610,   570,   610,   570,   450,   530,    63,    63,
      23,    23,   122,   141,   141,   -53,   -53,   -53,   -53,    39,
      38,   407,   144,   -53,    92,   144,   -53,   144,   -53,   -53,
     -53,   144,   144,   144,    60,    55,   -53,   -53,   407,   144,
     119,   364,   -53,   407,   407,   407,   -53,   198,   -53,   144,
     -53,   -53,   407
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -53,   -53,    45,   -53,   -14,   -53,   -53,   -53,   -53,    51,
     -26,   -53,    -1
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      38,    39,    40,    41,   141,   133,   143,    45,    46,   134,
      94,    95,    77,    96,    79,    78,    30,    80,    31,    89,
      81,    91,    33,    82,    35,    26,    27,    28,    29,    83,
     125,   127,    84,   126,   128,    97,    52,    99,   103,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    37,    93,
     103,   103,   131,    42,   132,    43,    85,    69,    70,    26,
      27,    71,    72,    73,    74,    75,    52,    88,    87,    90,
     135,    92,    76,   137,     1,   142,     2,   144,     3,     4,
       5,     6,     7,    28,    29,    98,   146,     8,   147,     9,
     150,    10,    11,    12,    13,    67,    68,    69,    70,    47,
      48,    71,    72,    73,    74,    75,   157,    14,   148,    15,
     156,   151,    76,    16,    17,   129,   130,   153,   154,   155,
     159,   161,   149,     0,    18,    52,    19,     0,    20,   139,
     140,    21,     0,   103,     0,   162,     0,     0,     0,     3,
       4,     5,     6,     7,    52,     0,     0,     0,     8,     0,
       9,     0,    10,    11,    12,    13,     0,    70,     0,     0,
      71,    72,    73,    74,    75,     0,     0,     0,    14,     0,
      15,    76,   152,     0,    16,    17,     0,     0,     0,    71,
      72,    73,    74,    75,   158,    18,     0,     0,     0,    20,
      76,     0,    21,   100,     4,   101,   102,     7,     0,     0,
       0,     0,     8,     0,     9,     0,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    14,     0,    15,     0,     0,     0,    16,    17,
       0,     0,     0,     0,     0,    49,    50,    51,    52,    18,
       0,     0,     0,    20,     0,     0,    21,    53,    54,    55,
      56,    57,    58,    59,    60,    61,    62,     0,     0,    63,
       0,    64,     0,     0,     0,    65,    66,    67,    68,    69,
      70,     0,     0,    71,    72,    73,    74,    75,    49,     0,
      51,    52,     0,     0,    76,     0,     0,     0,     0,     0,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,    63,     0,    64,     0,     0,     0,    65,    66,
      67,    68,    69,    70,     0,     0,    71,    72,    73,    74,
      75,    49,     0,    51,    52,   136,     0,    76,     0,     0,
       0,     0,     0,    53,    54,    55,    56,    57,    58,    59,
      60,    61,    62,     0,     0,    63,     0,    64,     0,     0,
       0,    65,    66,    67,    68,    69,    70,     0,     0,    71,
      72,    73,    74,    75,    49,     0,    51,    52,   138,     0,
      76,     0,     0,     0,     0,     0,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,     0,     0,    63,     0,
      64,     0,     0,     0,    65,    66,    67,    68,    69,    70,
       0,     0,    71,    72,    73,    74,    75,    49,     0,    51,
      52,   160,     0,    76,     0,     0,     0,     0,     0,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,     0,
       0,    63,     0,    64,     0,     0,     0,    65,    66,    67,
      68,    69,    70,     0,     0,    71,    72,    73,    74,    75,
      49,     0,    51,    52,     0,     0,    76,     0,     0,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,     0,    64,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,     0,    64,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    53,    54,    55,    56,    57,    58,    59,     0,
      61,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    53,    54,    55,    56,    57,    58,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    -1,    -1,    -1,    -1,    -1,    -1,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,     0,     0,     0,     0,     0,     0,    76
};

static const yytype_int16 yycheck[] =
{
      14,    15,    16,    17,    11,    57,    11,    21,     0,    61,
      53,    54,     5,    56,     5,     8,    51,     8,    51,    33,
       5,    35,    51,     8,    51,    32,    33,    32,    33,     5,
       5,     5,     8,     8,     8,    49,    13,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    13,    57,
      74,    75,    76,    18,    11,    20,     8,    44,    45,    32,
      33,    48,    49,    50,    51,    52,    13,    32,     8,    34,
      15,    36,    59,    17,     1,    11,     3,    60,     5,     6,
       7,     8,     9,    32,    33,    50,    57,    14,    60,    16,
       8,    18,    19,    20,    21,    42,    43,    44,    45,    53,
      54,    48,    49,    50,    51,    52,    61,    34,   132,    36,
      60,   135,    59,    40,    41,    74,    75,   141,   142,   143,
      11,   157,   133,    -1,    51,    13,    53,    -1,    55,    94,
      95,    58,    -1,   157,    -1,   159,    -1,    -1,    -1,     5,
       6,     7,     8,     9,    13,    -1,    -1,    -1,    14,    -1,
      16,    -1,    18,    19,    20,    21,    -1,    45,    -1,    -1,
      48,    49,    50,    51,    52,    -1,    -1,    -1,    34,    -1,
      36,    59,   137,    -1,    40,    41,    -1,    -1,    -1,    48,
      49,    50,    51,    52,   149,    51,    -1,    -1,    -1,    55,
      59,    -1,    58,     5,     6,     7,     8,     9,    -1,    -1,
      -1,    -1,    14,    -1,    16,    -1,    18,    19,    20,    21,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    34,    -1,    36,    -1,    -1,    -1,    40,    41,
      -1,    -1,    -1,    -1,    -1,    10,    11,    12,    13,    51,
      -1,    -1,    -1,    55,    -1,    -1,    58,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    -1,    -1,    34,
      -1,    36,    -1,    -1,    -1,    40,    41,    42,    43,    44,
      45,    -1,    -1,    48,    49,    50,    51,    52,    10,    -1,
      12,    13,    -1,    -1,    59,    -1,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    34,    -1,    36,    -1,    -1,    -1,    40,    41,
      42,    43,    44,    45,    -1,    -1,    48,    49,    50,    51,
      52,    10,    -1,    12,    13,    57,    -1,    59,    -1,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    -1,    -1,    34,    -1,    36,    -1,    -1,
      -1,    40,    41,    42,    43,    44,    45,    -1,    -1,    48,
      49,    50,    51,    52,    10,    -1,    12,    13,    57,    -1,
      59,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    34,    -1,
      36,    -1,    -1,    -1,    40,    41,    42,    43,    44,    45,
      -1,    -1,    48,    49,    50,    51,    52,    10,    -1,    12,
      13,    57,    -1,    59,    -1,    -1,    -1,    -1,    -1,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    -1,
      -1,    34,    -1,    36,    -1,    -1,    -1,    40,    41,    42,
      43,    44,    45,    -1,    -1,    48,    49,    50,    51,    52,
      10,    -1,    12,    13,    -1,    -1,    59,    -1,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    13,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    36,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    13,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    13,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    -1,
      30,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    13,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    22,    23,    24,    25,    26,    27,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    13,    -1,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    22,    23,    24,    25,    26,    27,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      40,    41,    42,    43,    44,    45,    -1,    -1,    48,    49,
      50,    51,    52,    -1,    -1,    -1,    -1,    -1,    -1,    59
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,     9,    14,    16,
      18,    19,    20,    21,    34,    36,    40,    41,    51,    53,
      55,    58,    63,    64,    65,    66,    32,    33,    32,    33,
      51,    51,    69,    51,    68,    51,    67,    64,    66,    66,
      66,    66,    64,    64,    70,    66,     0,    53,    54,    10,
      11,    12,    13,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    34,    36,    40,    41,    42,    43,    44,
      45,    48,    49,    50,    51,    52,    59,     5,     8,     5,
       8,     5,     8,     5,     8,     8,    73,     8,    64,    66,
      64,    66,    64,    57,    53,    54,    56,    66,    64,    66,
       5,     7,     8,    66,    71,    72,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,     5,     8,     5,     8,    71,
      71,    66,    11,    57,    61,    15,    57,    17,    57,    64,
      64,    11,    11,    11,    60,    74,    57,    60,    66,    74,
       8,    66,    64,    66,    66,    66,    60,    61,    64,    11,
      57,    72,    66
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

    { return 0; }
    break;

  case 3:

    { return xxvalue(NULL,2,NULL); }
    break;

  case 4:

    { return xxvalue((yyvsp[(1) - (2)]),3,&(yylsp[(1) - (2)])); }
    break;

  case 5:

    { return xxvalue((yyvsp[(1) - (2)]),4,&(yylsp[(1) - (2)])); }
    break;

  case 6:

    { YYABORT; }
    break;

  case 7:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 8:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 9:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 10:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 11:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 12:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 13:

    { (yyval) = (yyvsp[(1) - (1)]); }
    break;

  case 14:

    { (yyval) = xxexprlist((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)])); }
    break;

  case 15:

    { (yyval) = xxparen((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)])); }
    break;

  case 16:

    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 17:

    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 18:

    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 19:

    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 20:

    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 21:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 22:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 23:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 24:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 25:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 26:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 27:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 28:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 29:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 30:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 31:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 32:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 33:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 34:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 35:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 36:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 37:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 38:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 39:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 40:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 41:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 42:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])); }
    break;

  case 43:

    { (yyval) = xxdefun((yyvsp[(1) - (6)]),(yyvsp[(3) - (6)]),(yyvsp[(6) - (6)])); }
    break;

  case 44:

    { (yyval) = xxfuncall((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)])); }
    break;

  case 45:

    { (yyval) = xxif((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 46:

    { (yyval) = xxifelse((yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)])); }
    break;

  case 47:

    { (yyval) = xxfor((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 48:

    { (yyval) = xxwhile((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 49:

    { (yyval) = xxrepeat((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)])); }
    break;

  case 50:

    { (yyval) = xxsubscript((yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(3) - (5)])); }
    break;

  case 51:

    { (yyval) = xxsubscript((yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(3) - (4)])); }
    break;

  case 52:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 53:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 54:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 55:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 56:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 57:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 58:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 59:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 60:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 61:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 62:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 63:

    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 64:

    { (yyval) = xxnxtbrk((yyvsp[(1) - (1)])); }
    break;

  case 65:

    { (yyval) = xxnxtbrk((yyvsp[(1) - (1)])); }
    break;

  case 66:

    { (yyval) = xxcond((yyvsp[(2) - (3)])); }
    break;

  case 67:

    { (yyval) = xxifcond((yyvsp[(2) - (3)])); }
    break;

  case 68:

    { (yyval) = xxforcond((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])); }
    break;

  case 69:

    { (yyval) = xxexprlist0(); }
    break;

  case 70:

    { (yyval) = xxexprlist1((yyvsp[(1) - (1)]), &(yylsp[(1) - (1)])); }
    break;

  case 71:

    { (yyval) = xxexprlist2((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), &(yylsp[(3) - (3)])); }
    break;

  case 72:

    { (yyval) = (yyvsp[(1) - (2)]); }
    break;

  case 73:

    { (yyval) = xxexprlist2((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)]), &(yylsp[(3) - (3)])); }
    break;

  case 74:

    { (yyval) = (yyvsp[(1) - (2)]);}
    break;

  case 75:

    { (yyval) = xxsublist1((yyvsp[(1) - (1)])); }
    break;

  case 76:

    { (yyval) = xxsublist2((yyvsp[(1) - (4)]),(yyvsp[(4) - (4)])); }
    break;

  case 77:

    { (yyval) = xxsub0(); }
    break;

  case 78:

    { (yyval) = xxsub1((yyvsp[(1) - (1)])); }
    break;

  case 79:

    { (yyval) = xxsymsub0((yyvsp[(1) - (2)])); }
    break;

  case 80:

    { (yyval) = xxsymsub1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 81:

    { (yyval) = xxsymsub0((yyvsp[(1) - (2)])); }
    break;

  case 82:

    { (yyval) = xxsymsub1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 83:

    { (yyval) = xxnullsub0(); }
    break;

  case 84:

    { (yyval) = xxnullsub1((yyvsp[(3) - (3)])); }
    break;

  case 85:

    { (yyval) = xxnullformal(); }
    break;

  case 86:

    { (yyval) = xxfirstformal0((yyvsp[(1) - (1)])); }
    break;

  case 87:

    { (yyval) = xxfirstformal1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 88:

    { (yyval) = xxaddformal0((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); }
    break;

  case 89:

    { (yyval) = xxaddformal1((yyvsp[(1) - (5)]),(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)])); }
    break;

  case 90:

    { EatLines = 1; }
    break;


/* Line 1267 of yacc.c.  */

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
    
    if (c == '\n') {
    	xxlineno += 1;
    	xxlastlinelen = xxcolno; 
    	xxcolno = 0;
    } else xxcolno++;
    
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
    if (c == '\n') {
    	xxlineno -= 1;
    	xxcolno = xxlastlinelen; /* FIXME:  could we push back more than one line? */
    	xxlastlinelen = 0;
    } else xxcolno--;
    
    if ( KeepSource && GenerateCode && FunctionLevel > 0 )
	SourcePtr--;
    xxcharcount--;
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
    SrcRefs = NULL;
    return val;
}

static int xxvalue(SEXP v, int k, YYLTYPE *lloc)
{
    if (k > 2) {
    	if (SrcFile)
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
    	UNPROTECT_PTR(v);
    }
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
    if (GenerateCode) {
	PROTECT(ans = NewList());
	if (SrcFile) {
	    setAttrib(ans, R_SrcrefSymbol, SrcRefs);
    	    REPROTECT(SrcRefs = NewList(), srindex);
    	}
    } 
    else
	PROTECT(ans = R_NilValue);
    return ans;
}

static SEXP xxexprlist1(SEXP expr, YYLTYPE *lloc)
{
    SEXP ans,tmp;
    if (GenerateCode) {
	PROTECT(tmp = NewList());
	if (SrcFile) {
	    setAttrib(tmp, R_SrcrefSymbol, SrcRefs);
    	    REPROTECT(SrcRefs = NewList(), srindex);
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);
    	}	
	PROTECT(ans = GrowList(tmp, expr));
	UNPROTECT_PTR(tmp);
    }
    else
	PROTECT(ans = R_NilValue);
    UNPROTECT_PTR(expr);
    return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    if (GenerateCode) {
	if (SrcFile) 
    	    REPROTECT(SrcRefs = GrowList(SrcRefs, makeSrcref(lloc, SrcFile)), srindex);   
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


static SEXP mkChar2(const char *name)
{
    if(!utf8strIsASCII(name)) {
	if(known_to_be_latin1) return mkCharEnc(name, LATIN1_MASK);
	else if(known_to_be_utf8) return mkCharEnc(name, UTF8_MASK);
    }
    return mkChar(name);
}

static SEXP mkString2(const char *s)
{
    SEXP t;

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkChar2(s));
    UNPROTECT(1);
    return t;
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
	    /* FIXME: this should be whitespace */
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
				       mkChar2((char *)SourceLine));
		    } else { /* over-long line */
			char *LongLine = (char *) malloc(nc);
			if(!LongLine) 
			    error(("unable to allocate space for source line"));
			strncpy(LongLine, (char *)p0, nc);
			LongLine[nc] = '\0';
			SET_STRING_ELT(source, lines++,
				       mkChar2((char *)LongLine));
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
    SEXP prevSrcrefs;
    
    EatLines = 0;
    if (GenerateCode) {
	SET_TYPEOF(a2, LANGSXP);
	SETCAR(a2, a1);
	if (SrcFile) {
	    PROTECT(prevSrcrefs = getAttrib(a2, R_SrcrefSymbol));
	    PROTECT(ans = attachSrcrefs(a2, SrcFile));
	    REPROTECT(SrcRefs = prevSrcrefs, srindex);
	    /* SrcRefs got NAMED by being an attribute... */
	    SET_NAMED(SrcRefs, 0);
	    UNPROTECT_PTR(prevSrcrefs);
	} 
	else
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
    	tag = install(translateChar(STRING_ELT(tag, 0)));
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
 *	SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
 *
 *	SEXP R_ParseVector(SEXP *text, int n, ParseStatus *status, SEXP srcfile)
 *
 *	SEXP R_ParseBuffer(IoBuffer *buffer, int n, ParseStatus *status, SEXP prompt, SEXP srcfile)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing failed
 *  for some reason.
 */

#define CONTEXTSTACK_SIZE 50
static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[CONTEXTSTACK_SIZE], *contextp;

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

static SEXP R_Parse(int n, ParseStatus *status, SEXP srcfile)
{
    volatile int savestack;
    int i;
    SEXP t, rval;

    ParseContextInit();
    savestack = R_PPStackTop;
    PROTECT(t = NewList());
    
    xxlineno = 1;
    xxcolno = 0;    
    if (!isNull(srcfile)) {
	SrcFile = srcfile;
	PROTECT_WITH_INDEX(SrcRefs = NewList(), &srindex);
    } 
    else SrcFile = NULL;
    
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
    if (SrcFile) {
    	rval = attachSrcrefs(rval, SrcFile);
        SrcFile = NULL;    
    }
    R_PPStackTop = savestack;
    *status = PARSE_OK;
    return rval;
}

/* used in edit.c */
attribute_hidden
SEXP R_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    fp_parse = fp;
    ptr_getc = file_getc;
    return R_Parse(n, status, srcfile);
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
SEXP R_ParseConn(Rconnection con, int n, ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    con_parse = con;
    ptr_getc = con_getc;
    return R_Parse(n, status, srcfile);
}

/* This one is public, and used in source.c */
SEXP R_ParseVector(SEXP text, int n, ParseStatus *status, SEXP srcfile)
{
    SEXP rval;
    TextBuffer textb;
    R_TextBufferInit(&textb, text);
    txtb = &textb;
    GenerateCode = 1;
    ptr_getc = text_getc;
    rval = R_Parse(n, status, srcfile);
    R_TextBufferFree(&textb);
    return rval;
}

#ifdef PARSE_UNUSED
/* Not used, and note ungetc is no longer needed */
SEXP R_ParseGeneral(int (*ggetc)(), int (*gungetc)(), int n,
		    ParseStatus *status, SEXP srcfile)
{
    GenerateCode = 1;
    ptr_getc = ggetc;
    return R_Parse(n, status, srcfile);
}
#endif

static const char *Prompt(SEXP prompt, int type)
{
    if(type == 1) {
	if(length(prompt) <= 0) {
	    return CHAR(STRING_ELT(GetOption(install("prompt"),
					     R_BaseEnv), 0));
	}
	else
	    return CHAR(STRING_ELT(prompt, 0));
    }
    else {
	return CHAR(STRING_ELT(GetOption(install("continue"),
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
    
    xxlineno = 1;
    xxcolno = 0;      
    if (!isNull(srcfile)) {
	SrcFile = srcfile;
	PROTECT_WITH_INDEX(SrcRefs = NewList(), &srindex);
    }      
    else SrcFile = NULL;
    
    for(i = 0; ; ) {
	if(n >= 0 && i >= n) break;
	if (!*bufp) {
	    if(R_ReadConsole((char *) Prompt(prompt, prompt_type),
			     (unsigned char *)buf, 1024, 1) == 0)
		goto finish;
	    bufp = buf;
	}
	while ((c = *bufp++)) {
	    R_IoBufferPutc(c, buffer);
	    if (c == ';' || c == '\n') break;
	}

	rval = R_Parse1Buffer(buffer, 1, status);
	
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
    if (SrcFile) {
    	rval = attachSrcrefs(rval, SrcFile);
        SrcFile = NULL;    
    }	
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
	if(contextp - contextstack >= CONTEXTSTACK_SIZE) 
	    error("contextstack overflow");
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
    { "Inf",	    NUM_CONST  },
    { "NaN",	    NUM_CONST  },
    { "NA_integer_", NUM_CONST  },
    { "NA_real_",    NUM_CONST  },
    { "NA_character_", NUM_CONST  },
    { "NA_complex_", NUM_CONST  },
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

static int KeywordLookup(const char *s)
{
    int i;
    for (i = 0; keywords[i].name; i++) {
	if (strcmp(keywords[i].name, s) == 0) {
	    switch (keywords[i].token) {
	    case NULL_CONST:
		PROTECT(yylval = R_NilValue);
		break;
	    case NUM_CONST:
		if(GenerateCode) {
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
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = R_PosInf;
			break;
		    case 5:
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = R_NaN;
			break;
		    case 6:
			PROTECT(yylval = allocVector(INTSXP, 1));
			INTEGER(yylval)[0] = NA_INTEGER;
			break;
		    case 7:
			PROTECT(yylval = allocVector(REALSXP, 1));
			REAL(yylval)[0] = NA_REAL;
			break;
		    case 8:
			PROTECT(yylval = allocVector(STRSXP, 1));
			SET_STRING_ELT(yylval, 0, NA_STRING);
			break;
		    case 9:
			PROTECT(yylval = allocVector(CPLXSXP, 1));
			COMPLEX(yylval)[0].r = COMPLEX(yylval)[0].i = NA_REAL;
			break;
		    }
		} else
		    PROTECT(yylval = R_NilValue);
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


static SEXP mkFloat(const char *s)
{
    double f;
    if(strlen(s) > 2 && (s[1] == 'x' || s[1] == 'X')) {
	double ret = 0; const char *p = s + 2;
	for(; p; p++) {
	    if('0' <= *p && *p <= '9') ret = 16*ret + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ret = 16*ret + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ret = 16*ret + (*p -'A' + 10);
	    else break;
	}	
	f = ret;
    } else f = atof(s);
    return ScalarReal(f);
}

static SEXP mkInt(const char *s)
{
    double f;
    if(strlen(s) > 2 && (s[1] == 'x' || s[1] == 'X')) {
	double ret = 0; const char *p = s + 2;
	for(; p; p++) {
	    if('0' <= *p && *p <= '9') ret = 16*ret + (*p -'0');
	    else if('a' <= *p && *p <= 'f') ret = 16*ret + (*p -'a' + 10);
	    else if('A' <= *p && *p <= 'F') ret = 16*ret + (*p -'A' + 10);
	    else break;
	}	
	f = ret;
    } else f = atof(s);
    return ScalarInteger((int) f);
}

static SEXP mkComplex(const char *s)
{
    SEXP t = R_NilValue;
    double f;
    f = atof(s); /* make certain the value is legitimate. */

    if(GenerateCode) {
       t = allocVector(CPLXSXP, 1);
       COMPLEX(t)[0].r = 0;
       COMPLEX(t)[0].i = f;
    }

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
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */    
#define YYENGLISH 8
	"$undefined",	"input", 	
	"END_OF_INPUT",	"end of input",
	"ERROR",	"input", 	
	"STR_CONST",	"string constant",
	"NUM_CONST",	"numeric constant",
	"SYMBOL",	"symbol",	
	"LEFT_ASSIGN",	"assignment",	
	"'\\n'",	"end of line",	
	"NULL_CONST",	"'NULL'",
	"FUNCTION",	"'function'",
	"EQ_ASSIGN",	"'='",
	"RIGHT_ASSIGN",	"'->'",
	"LBB",		"'[['",
	"FOR",		"'for'",
	"IN",		"'in'",
	"IF",		"'if'",
	"ELSE",		"'else'",
	"WHILE",	"'while'",
	"NEXT",		"'next'",
	"BREAK",	"'break'",
	"REPEAT",	"'repeat'",
	"GT",		"'>'",
	"GE",		"'>='",
	"LT",		"'<'",
	"LE",		"'<='",
	"EQ",		"'=='",
	"NE",		"'!='",
	"AND",		"'&'",
	"OR",		"'|'",
	"AND2",		"'&&'",
	"OR2",		"'||'",
	"NS_GET",	"'::'",
	"NS_GET_INT",	"':::'",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
 #if 0
 /* these are just here to trigger the internationalization */
    _("input"); 	
    _("end of input");
    _("string constant");
    _("numeric constant");
    _("symbol");	
    _("assignment");
    _("end of line");
#endif 
   
    R_ParseError = xxlineno;
    R_ParseErrorFile = SrcFile;
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
	int i;
    	/* Edit the error message */    
    	expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
    	if (expecting) *expecting = '\0';
    	for (i=0; yytname_translations[i]; i += 2) {
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

static void CheckFormalArgs(SEXP formlist, SEXP _new)
{
    while (formlist != R_NilValue) {
	if (TAG(formlist) == _new) {
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
#if defined(SUPPORT_UTF8) && defined(__STDC_ISO_10646__)
    if(utf8locale) { /* Might work in other MBCS locales
			but ctype functions need Unicode wchar_t */
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
    int asNumeric = 0;

    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E' 
	   || c == 'x' || c == 'X' || c == 'L') 
    {
	if (c == 'L') /* must be at the end.  Won't allow 1Le3 (at present). */
	    break;

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
	    seendot = seendot == 1 ? seendot : 2;
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
    /* Make certain that things are okay. */
    if(c == 'L') {
	double a = atof(yytext);
	int b = (int) atof(yytext); 
	/* We are asked to create an integer via the L, so we check that the 
	   double and int values are the same. If not, this is a problem and we
	   will not lose information and so use the numeric value.
	*/
	if(a != (double) b) {
	    if(GenerateCode) {
		if(seendot == 1 && seenexp == 0)
		    warning(_("integer literal %sL contains decimal; using numeric value"), yytext);
		else 
		    warning(_("non-integer value %s qualified with L; using numeric value"), yytext);
	    }
	    asNumeric = 1;
	    seenexp = 1;
	}
    }
    
    if(c == 'i') {
	yylval = GenerateCode ? mkComplex(yytext) : R_NilValue;
    } else if(c == 'L' && asNumeric == 0) {
	if(GenerateCode && seendot == 1 && seenexp == 0) 
	    warning(_("integer literal %sL contains unnecessary decimal point"), yytext);
	yylval = GenerateCode ? mkInt(yytext) : R_NilValue;
    }
    else {
	if(c != 'L')
	    xxungetc(c);
	yylval = GenerateCode ? mkFloat(yytext) : R_NilValue;
    }

    PROTECT(yylval);
    return NUM_CONST;
}

/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */


#define STEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
            nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string"));\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)
#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--

static int StringValue(int c, Rboolean forSymbol)
{
    int quote = c;
    int have_warned = 0;
    char currtext[1010], *ct = currtext;
    char st0[MAXELTSIZE];
    unsigned int nstext = MAXELTSIZE;
    char *stext = st0, *bp = st0;

    while ((c = xxgetc()) != R_EOF && c != quote) {
	CTEXT_PUSH(c);
	if (c == '\n') {
	    xxungetc(c);
	    /* Fix by Mark Bravington to allow multiline strings
             * by pretending we've seen a backslash. Was:
	     * return ERROR;
             */
	    c = '\\';
	}
	if (c == '\\') {
	    c = xxgetc(); CTEXT_PUSH(c);
	    if ('0' <= c && c <= '8') {
		int octal = c - '0';
		if ('0' <= (c = xxgetc()) && c <= '8') {
		    CTEXT_PUSH(c);
		    octal = 8 * octal + c - '0';
		    if ('0' <= (c = xxgetc()) && c <= '8') {
			CTEXT_PUSH(c);
			octal = 8 * octal + c - '0';
		    } else {
			xxungetc(c);
			CTEXT_POP();
		    }
		} else {
		    xxungetc(c);
		    CTEXT_POP();
		}
		c = octal;
	    }
	    else if(c == 'x') {
		int val = 0; int i, ext;
		for(i = 0; i < 2; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); CTEXT_POP(); break;}
		    val = 16*val + ext;
		}
		c = val;
	    }
	    else if(c == 'u') {
#ifndef SUPPORT_MBCS
		error(_("\\uxxxx sequences not supported"));
#else
		unsigned int val = 0; int i, ext; size_t res;
		char buff[16]; Rboolean delim = FALSE;
		if((c = xxgetc()) == '{') {
		    delim = TRUE; 
		    CTEXT_PUSH(c);
		} else xxungetc(c);
		for(i = 0; i < 4; i++) {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if(c >= '0' && c <= '9') ext = c - '0';
		    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
		    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
		    else {xxungetc(c); CTEXT_POP(); break;}
		    val = 16*val + ext;
		}
		if(delim) {
		    if((c = xxgetc()) != '}')
			error(_("invalid \\u{xxxx} sequence"));
		    else CTEXT_PUSH(c);
		}
		res = ucstomb(buff, val, NULL);
		if((int)res <= 0) {
		    if(delim)
			error(_("invalid \\u{xxxx} sequence"));
		    else
			error(_("invalid \\uxxxx sequence"));
		}
		for(i = 0; i <  res - 1; i++) STEXT_PUSH(buff[i]);
		c = buff[res - 1]; /* pushed below */
#endif
	    }
	    else if(c == 'U') {
#ifndef SUPPORT_MBCS
		error(_("\\Uxxxxxxxx sequences not supported"));
#else
		{
		    unsigned int val = 0; int i, ext; size_t res;
		    char buff[16]; Rboolean delim = FALSE;
		    if((c = xxgetc()) == '{') {
			delim = TRUE;
			CTEXT_PUSH(c);
		    } else xxungetc(c);
		    for(i = 0; i < 8; i++) {
			c = xxgetc(); CTEXT_PUSH(c);
			if(c >= '0' && c <= '9') ext = c - '0';
			else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
			else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
			else {xxungetc(c); CTEXT_POP(); break;}
			val = 16*val + ext;
		    }
		    if(delim) {
			if((c = xxgetc()) != '}')
			    error(_("invalid \\U{xxxxxxxx} sequence"));
			else CTEXT_PUSH(c);
		    }
		    res = ucstomb(buff, val, NULL);
		    if((int)res <= 0) {
			if(delim)
			    error(_("invalid \\U{xxxxxxxx} sequence"));
			else
			    error(_("invalid \\Uxxxxxxxx sequence"));
		    }
		    for(i = 0; i <  res - 1; i++) STEXT_PUSH(buff[i]);
		    c = buff[res - 1]; /* pushed below */
		}
#endif
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
		default:
		    if(GenerateCode && R_WarnEscapes) {
			have_warned++;
			warningcall(R_NilValue, _("'\\%c' is an unrecognized escape in a character string"), c);
		    }
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
               STEXT_PUSH(c);
               c = xxgetc();
               if (c == R_EOF) break;
	       CTEXT_PUSH(c);
               if (c == '\n') {
                   xxungetc(c); CTEXT_POP();
                   c = '\\';
               }
           }
           if (c == R_EOF) break;
       }
#endif /* SUPPORT_MBCS */
	STEXT_PUSH(c);
    }
    STEXT_PUSH('\0');
    if(forSymbol) {
	PROTECT(yylval = install(stext));
	if(stext != st0) free(stext);
	return SYMBOL;
    } else {
	PROTECT(yylval = mkString2(stext));
	if(stext != st0) free(stext);
	if(have_warned) {
	    *ct = '\0';
#ifdef ENABLE_NLS
	    warningcall(R_NilValue,
			ngettext("unrecognized escape removed from \"%s\"",
				 "unrecognized escapes removed from \"%s\"",
				 have_warned),
			currtext);
#else
	    warningcall(R_NilValue,
			"unrecognized escape(s) removed from \"%s\"", currtext);
#endif
	}
	return STR_CONST;
    }
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
int isValidName(const char *name)
{
    const char *p = name;
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
	yylloc.first_line = xxlinesave;
	yylloc.first_column = xxcolsave;	
	return c;
    }
    xxcharsave = xxcharcount; /* want to be able to go back one token */

    c = SkipSpace();
    if (c == '#') c = SkipComment();
    
    yylloc.first_line = xxlineno;
    yylloc.first_column = xxcolno;    

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
	return StringValue(c, FALSE);

    /* special functions */

    if (c == '%')
	return SpecialValue(c);

    /* functions, constants and variables */

    if (c == '`')
	return StringValue(c, TRUE);
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
	    return AND2;
	}
	yylval = install("&");
	return AND;
    case '|':
	if (nextchar('|')) {
	    yylval = install("||");
	    return OR2;
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

static void setlastloc()
{
    yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
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
		setlastloc();
		return tok;
	    }

	    /* When a "," is encountered, it terminates */
	    /* just the immediately preceding "if" body */
	    /* so we pop just a single "i" of the */
	    /* context stack. */

	    if (tok == ',') {
		ifpop();
		setlastloc();		
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
		setlastloc();		
		return ELSE;
	    }
	    else {
		ifpop();
		SavedToken = tok;
		xxlinesave = yylloc.first_line;
		xxcolsave  = yylloc.first_column;	
		SavedLval = yylval;
		setlastloc();		
		return '\n';
	    }
	}
	else {
	    setlastloc();
	    return '\n';
	}
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
    case OR2:
    case AND2:
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
	if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
	    error("contextstack overflow");
	*++contextp = '[';
	*++contextp = '[';
	break;

    case '[':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error("contextstack overflow");
	*++contextp = tok;
	break;

    case LBRACE:
	if(contextp - contextstack >= CONTEXTSTACK_SIZE)
	    error("contextstack overflow");
	*++contextp = tok;
	EatLines = 1;
	break;

    case '(':
	if(contextp - contextstack >= CONTEXTSTACK_SIZE) 
	    error("contextstack overflow");
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
    setlastloc();
    return tok;
}

