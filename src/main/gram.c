#ifndef lint
static char yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define yyclearin (yychar=(-1))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
#define YYPREFIX "yy"
#line 2 "gram.y"
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996, 1997  Robert Gentleman and Ross Ihaka
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
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "Defn.h"
#include "IOSupport.h"
#include "Parse.h"

	/* Useful defines so editors don't get confused ... */

#define LBRACE	'{'
#define RBRACE	'}'

	/* Functions used in the parsing process */

static void	AddComment(SEXP);
static void	CheckFormalArgs(SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);
static SEXP	GrowList(SEXP, SEXP);
static void	IfPush(void);
static int	IsComment(SEXP);
static int	KeywordLookup(char*);
static SEXP	NewList(void);
static SEXP	NextArg(SEXP, SEXP, SEXP);
static void	PopComment(void);
static void	PushComment(void);
static void	ResetComment(void);
static SEXP	TagArg(SEXP, SEXP);


	/* These routines allocate constants */

SEXP		mkComplex(char *);
SEXP		mkFalse(void);
SEXP		mkFloat(char *);
SEXP		mkInteger(char *);
SEXP		mkNA(void);
SEXP		mkString(char *);
SEXP		mkTrue(void);

	/* Internal lexer / parser state variables */

static int	EatLines = 0;
static int	GenerateCode = 0;
static int	EndOfFile = 0;
static int	(*xxgetc)();
static int	(*xxungetc)();

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

#line 119 "y.tab.c"
#define END_OF_INPUT 257
#define ERROR 258
#define STR_CONST 259
#define NUM_CONST 260
#define NULL_CONST 261
#define SYMBOL 262
#define FUNCTION 263
#define LEFT_ASSIGN 264
#define RIGHT_ASSIGN 265
#define LBB 266
#define FOR 267
#define IN 268
#define IF 269
#define ELSE 270
#define WHILE 271
#define NEXT 272
#define BREAK 273
#define REPEAT 274
#define GT 275
#define GE 276
#define LT 277
#define LE 278
#define EQ 279
#define NE 280
#define AND 281
#define OR 282
#define LOW 283
#define TILDE 284
#define UNOT 285
#define NOT 286
#define SPECIAL 287
#define UMINUS 288
#define UPLUS 289
#define YYERRCODE 256
short yylhs[] = {                                        -1,
    0,    0,    0,    0,    0,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    8,    6,
    7,    2,    2,    2,    2,    2,    2,    5,    5,    9,
    9,    9,    9,    9,    9,    9,    9,    3,    3,    3,
    3,    3,    4,
};
short yylen[] = {                                         2,
    1,    1,    2,    2,    1,    1,    1,    1,    1,    3,
    3,    2,    2,    2,    2,    2,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    6,    4,    3,    5,    3,
    3,    2,    5,    4,    3,    3,    1,    1,    3,    3,
    5,    0,    1,    3,    2,    3,    2,    1,    4,    0,
    1,    2,    3,    2,    3,    2,    3,    0,    1,    3,
    3,    5,    0,
};
short yydefred[] = {                                      0,
    5,    1,    7,    6,    8,    9,    0,    0,    0,    0,
   47,   48,    0,    0,    0,    0,    0,    0,    2,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    3,    4,    0,    0,    0,    0,    0,    0,
    0,    0,   11,    0,    0,   10,    0,    0,    0,    0,
    0,    0,    0,   58,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   46,   45,    0,    0,    0,   73,    0,    0,   50,
    0,   49,    0,    0,    0,    0,    0,    0,    0,   37,
   44,    0,    0,    0,    0,    0,    0,    0,    0,   43,
    0,    0,    0,   51,   59,    0,
};
short yydgoto[] = {                                      22,
   82,   38,   66,  119,   83,   28,   26,   30,   84,
};
short yysindex[] = {                                    730,
    0,    0,    0,    0,    0,    0,  -35,  -33,  -24,  -18,
    0,    0, 1422, 1422, 1422, 1422, 1422, 1422,    0, 1422,
 1422,    0,  -10, -252, -228, 1422, 1422, 1422, 1422, 1422,
 1193, 1193,  -34,  189,  189,  937, 1193,    7, 1388, 1422,
 1422, 1443, 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422,
 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422, 1422, -258,
 1443, 1443,    0,    0,  -20,  -23, -213, 1193,  993, 1046,
 1116, 1193,    0, 1422, 1422,    0, 1193, 1246,   36,   37,
   38, 1193,  -32,    0, 1405, 1405, 1405, 1405, 1405, 1405,
 1388, 1308,  -34,  166,  166,  123,  123,  123,  242,  189,
  189,    0,    0,   59,    8, 1422,    0, -159, 1422,    0,
 1422,    0, 1193, 1193, 1422, 1422, 1422,   12,   73,    0,
    0, 1193, 1422,   57, 1140, 1193, 1193, 1193, 1193,    0,
 1443, 1193, 1422,    0,    0, 1193,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   15,
    0,    0,    0,  -21,    0,    0,    0,    0,    0,    0,
  126,  156,  127,    9,   28,    0,   19,    0,  437,    0,
    0,  -30,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   -1,  -30,    0,    0,    1,    0,    0,  397,    0,  529,
    0,  458,    0,   84,  103,    0,  735,  743,  830,  849,
  868,  -29,   75,    0,  439,  441,  460,  479,  487,  540,
  559,  706,  754,  399,  418,  105,  361,  380,   86,   48,
   67,    0,    0,   75,   75,    0,    0,    0,    0,    0,
    0,    0,  110,  151,  -13,   -5,  114,    0,    0,    0,
    0,    3,    0,   18,    0,  744,  134,  143,  147,    0,
  182,  756,    0,    0,    0,   35,
};
short yygindex[] = {                                      0,
 1707,    0,    0,   17,   21,    0,    0,    0,   -9,
};
#define YYTABLESIZE 1840
short yytable[] = {                                      63,
  102,   60,   56,  103,   24,   61,   25,   54,   52,   65,
   53,   61,   55,   60,   61,   27,   74,  107,   13,   68,
  108,   29,   68,   58,   52,   60,   56,   64,   53,   61,
   64,   54,   52,   67,   53,   66,   55,   12,   66,   60,
  106,   69,   60,   70,   69,   13,   70,   58,   64,   13,
   13,   13,   13,   13,  109,   13,   62,   17,   71,   59,
  118,   71,   60,   61,   12,   75,   13,   13,   12,   12,
   12,   12,   12,   52,   12,   72,   22,   53,   72,   64,
   62,  104,  105,   59,   17,   12,   12,   66,   17,   17,
   17,   17,   17,   57,   17,   23,  115,  116,  117,  120,
  121,   13,  124,   22,  130,   17,   17,   22,   22,   22,
   22,   22,   55,   22,   20,   51,  131,  133,   73,   56,
   12,  135,   23,  123,   22,   22,   23,   23,   23,   23,
   23,   76,   23,   13,   13,   42,   15,    0,    0,   52,
   17,   20,   57,   53,   23,   20,   20,   20,   20,   20,
    0,   20,   12,   12,   62,    0,    0,   62,   60,   22,
   54,   55,   61,   20,    0,   16,   42,   15,   56,   42,
   15,    0,   17,   17,   65,    0,    0,   65,   23,    0,
   58,    0,    0,   67,   42,   15,   67,   63,    0,    0,
   63,   22,   22,    0,    0,    0,   16,   20,    0,   16,
    0,   60,   56,    0,    0,   61,   62,   54,   57,   54,
   23,   23,   55,   62,   16,    0,   59,    0,   42,   15,
    0,    0,   60,   58,   60,   60,   65,   55,   61,   20,
   20,   42,    0,    0,   56,   67,    0,    0,    0,   63,
   43,   44,   45,   46,   47,   48,   49,   50,   16,    0,
   42,   15,   57,   40,   41,   42,   62,    0,    0,   59,
    0,    0,    0,    0,   43,   44,   45,   46,   47,   48,
   49,   50,   13,   13,   60,   54,   57,   60,   13,   62,
   16,   61,   59,   13,   13,   13,   13,   13,   13,   13,
   13,   12,   12,    0,    0,   13,    0,   12,    0,   58,
    0,    0,   12,   12,   12,   12,   12,   12,   12,   12,
    0,   17,   17,    0,   12,    0,    0,   17,    0,    0,
    0,    0,   17,   17,   17,   17,   17,   17,   17,   17,
   22,   22,   62,    0,   17,   59,   22,    0,    0,    0,
    0,   22,   22,   22,   22,   22,   22,   22,   22,   23,
   23,    0,    0,   22,    0,   23,    0,    0,    0,    0,
   23,   23,   23,   23,   23,   23,   23,   23,   20,   20,
   21,    0,   23,    0,   20,    0,    0,    0,    0,   20,
   20,   20,   20,   20,   20,   20,   20,    0,   42,   24,
   15,   15,    0,    0,    0,   42,   15,   21,    0,    0,
    0,   21,   21,   21,   21,   21,   40,   21,   18,   57,
    0,    0,    0,    0,    0,    0,   24,    0,    0,   21,
   24,   24,   24,   24,   24,   16,   24,   19,    0,    0,
    0,   42,    0,    0,    0,    0,    0,   40,   24,   18,
   40,   18,   18,   18,    0,    0,   14,    0,   31,    0,
   30,    0,   57,   21,   42,   40,    0,   18,   19,    0,
   19,   19,   19,    0,    0,    0,    0,   41,    0,   26,
    0,    0,   24,    0,    0,    0,   19,   14,    0,   31,
   14,   30,   31,    0,   30,   21,   21,    0,   27,   40,
    0,   18,    0,    0,    0,   14,   28,   31,   41,   30,
   26,   41,    0,   26,   24,   24,    0,   42,    0,    0,
   19,    0,    0,    0,    0,    0,   41,    0,   26,   27,
    0,   40,   27,   18,   18,    0,    0,   28,    0,   14,
   28,   31,    0,   30,    0,    0,    0,   27,   38,    0,
    0,    0,   19,   19,    0,   28,    0,    0,    0,   29,
   41,    0,   26,    0,    0,    0,    0,    0,    0,    0,
    0,   14,   14,   31,   31,   30,   30,    0,   32,   38,
    0,   27,   38,    0,    0,    0,    0,    0,    0,   28,
   29,    0,   41,   29,   26,   26,    0,   38,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   29,   32,
    0,    0,   32,   27,   27,    0,    0,    0,    0,    0,
    0,   28,   28,    0,    0,    0,    0,   32,    0,    0,
    0,   38,    0,    0,   21,   21,    0,    0,    0,    0,
   21,    0,   29,    0,    0,   21,   21,   21,   21,   21,
   21,   21,   21,   24,   24,    0,    0,    0,    0,   24,
    0,   32,    0,   38,   24,   24,   24,   24,   24,   24,
   24,   24,   18,   18,   29,   29,   40,    0,   18,    0,
    0,    0,    0,   18,   18,   18,   18,   18,   18,   18,
   18,   19,   19,   32,   32,    0,    0,   19,    0,    0,
    0,    0,   19,   19,   19,   19,   19,   19,   19,   19,
   14,   14,   31,   31,   30,   30,   14,    0,   31,    0,
   30,    0,    0,    0,    0,   33,    0,   14,   14,   31,
   31,   30,   30,   26,   26,    0,    0,   41,    0,   26,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   19,
   26,   26,   27,   27,   34,    0,   33,    0,   27,   33,
   28,   28,   35,   39,    0,    0,   28,    0,    0,   27,
   27,    0,   21,   25,   33,   36,    0,   28,   28,   18,
    0,    0,   16,    0,   17,   34,    0,    0,   34,    0,
    0,    0,    0,   35,   39,    0,   35,   39,    0,    0,
    0,    0,   14,   34,   25,    0,   36,   25,   33,   36,
    0,   35,   39,   29,   29,    0,    0,    0,    0,   29,
    0,    0,   25,    0,   36,    0,    0,    0,    0,    0,
   29,   29,   32,   32,    0,    0,    0,   34,   32,    0,
   33,   33,    0,    0,    0,   35,   39,    0,    0,   32,
   32,    0,    0,    0,    0,    0,   25,    0,   36,    0,
    0,    0,   20,    0,    0,   15,    0,    0,    0,   34,
    0,    0,    0,    0,    0,    7,    7,   35,   39,    7,
    7,    7,    7,    7,    7,    0,    7,    0,   25,    0,
   36,    0,    0,    0,    8,    8,    0,    7,    8,    8,
    8,    8,    8,    8,    0,    8,    0,    0,    0,    0,
    0,    0,    0,    9,    9,    0,    8,    9,    9,    9,
    9,    9,    9,    0,    9,    0,    0,    0,    0,    0,
    7,    0,    7,    7,    0,    9,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    8,
    0,    8,    8,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    7,    0,    0,    9,    0,
    9,    9,    0,    0,    0,    0,    0,    0,    0,   33,
   33,    0,   60,   56,    8,   33,   61,   73,   54,   52,
    0,   53,    0,   55,    0,    1,    2,   33,    3,    4,
    5,    6,    7,    9,   58,    0,    8,    0,    9,    0,
   10,   11,   12,   13,   34,    0,   35,   35,    0,    0,
    0,    0,   35,   39,    0,    0,    0,   25,   25,    0,
    0,    0,    0,   25,    0,   36,    0,   62,   60,   56,
   59,    0,   61,  110,   54,   52,    0,   53,    0,   55,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   58,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   51,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   60,   56,   62,    0,   61,   59,   54,   52,    0,
   53,    0,   55,    7,    7,    7,    0,    0,    0,    0,
    0,    0,    0,   58,    7,    7,    7,    7,    7,    7,
    7,    7,    8,    8,    8,    0,    7,    0,   51,    0,
    0,    0,    0,    8,    8,    8,    8,    8,    8,    8,
    8,    9,    9,    9,    0,    8,   62,    0,    0,   59,
    0,    0,    9,    9,    9,    9,    9,    9,    9,    9,
    0,   60,   56,    0,    9,   61,  112,   54,   52,    0,
   53,    0,   55,    0,    0,    0,    0,    0,    0,    0,
    0,   51,    0,   58,    0,   60,   56,    0,    0,   61,
  134,   54,   52,    0,   53,    0,   55,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   58,    0,    0,
   40,   41,   42,    0,    0,    0,   62,    0,    0,   59,
    0,   43,   44,   45,   46,   47,   48,   49,   50,    0,
    0,    0,    0,   57,    0,    0,    0,    0,   60,   56,
   62,    0,   61,   59,   54,   52,    0,   53,    0,   55,
    0,   51,    0,    0,    0,    0,    0,    0,    0,    0,
   58,    0,    0,    0,    0,    0,   40,   41,   42,    0,
    0,    0,    0,    0,    0,   51,    0,   43,   44,   45,
   46,   47,   48,   49,   50,    0,    0,    0,    0,   57,
    0,   60,   56,   62,    0,   61,   59,   54,   52,    0,
   53,    0,   55,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   58,    0,    0,    0,    0,    0,   40,
   41,   42,    0,    0,    0,  111,    0,    0,   51,    0,
   43,   44,   45,   46,   47,   48,   49,   50,    0,    0,
    0,    0,   57,    0,    0,    0,   62,    0,    0,   59,
    0,    0,    0,   60,   56,    0,    0,   61,    0,   54,
   52,    0,   53,    0,   55,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   58,    0,    0,    0,    0,
    0,   51,    0,    0,    0,    0,    0,    0,    0,   40,
   41,   42,    0,    0,    0,    0,    0,    0,    0,    0,
   43,   44,   45,   46,   47,   48,   49,   50,   62,    0,
    0,   59,   57,   40,   41,   42,    0,    0,    0,    0,
    0,    0,    0,    0,   43,   44,   45,   46,   47,   48,
   49,   50,    0,   60,   56,    0,   57,   61,    0,   54,
   52,    0,   53,    0,   55,    0,    0,    0,    0,    0,
   60,   56,    0,    0,   61,   58,   54,   52,    0,   53,
    0,   55,    0,    0,   21,    0,   40,   41,   42,    0,
    0,   18,   58,    0,   16,    0,   17,   43,   44,   45,
   46,   47,   48,   49,   50,   21,    0,    0,   62,   57,
    0,   59,   18,    0,   14,   16,    0,   17,    0,    0,
    0,    0,    0,    0,    0,   62,    0,    0,   59,    0,
    0,    0,    0,    0,    0,   14,    0,    0,    0,    0,
    0,   42,    0,    0,    0,    0,    0,    0,    0,    0,
   43,   44,   45,   46,   47,   48,   49,   50,    0,    0,
    0,    0,   57,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   20,    0,    0,   15,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   20,    0,    0,   15,    0,
    0,    0,    0,   42,    0,    0,    0,    0,    0,    0,
    0,    0,   43,   44,   45,   46,   47,   48,   49,    0,
    0,    0,    0,    0,   57,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   42,    0,    0,    0,    0,    0,    0,
    0,    0,   43,   44,   45,   46,   47,   48,    0,    0,
   42,    0,    0,    0,   57,    0,    0,    0,    0,    0,
    3,    4,    5,    6,    7,    0,    0,    0,    8,    0,
    9,   57,   10,   11,   12,   13,    0,    0,    0,    0,
    0,   79,    4,   80,   81,    7,   23,    0,    0,    8,
    0,    9,    0,   10,   11,   12,   13,    0,    0,   31,
   32,   33,   34,   35,   36,    0,   37,   39,    0,    0,
    0,    0,   68,   69,   70,   71,   72,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   77,   78,    0,   85,
   86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
   96,   97,   98,   99,  100,  101,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
  113,  114,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,  122,    0,    0,  125,    0,  126,    0,    0,
    0,  127,  128,  129,    0,    0,    0,    0,    0,  132,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  136,
};
short yycheck[] = {                                      10,
  259,   36,   37,  262,   40,   40,   40,   42,   43,  262,
   45,   41,   47,   44,   44,   40,   10,   41,   10,   41,
   44,   40,   44,   58,   10,   36,   37,   41,   10,   40,
   44,   42,   43,  262,   45,   41,   47,   10,   44,   41,
   61,   41,   44,   41,   44,   37,   44,   58,   59,   41,
   42,   43,   44,   45,  268,   47,   91,   10,   41,   94,
   93,   44,   93,   93,   37,   59,   58,   59,   41,   42,
   43,   44,   45,   59,   47,   41,   10,   59,   44,   93,
   91,   61,   62,   94,   37,   58,   59,   93,   41,   42,
   43,   44,   45,   10,   47,   10,   61,   61,   61,   41,
   93,   93,  262,   37,   93,   58,   59,   41,   42,   43,
   44,   45,   10,   47,   10,  126,   44,   61,   44,   10,
   93,  131,   37,  107,   58,   59,   41,   42,   43,   44,
   45,  125,   47,  125,  126,   10,   10,   -1,   -1,  125,
   93,   37,   59,  125,   59,   41,   42,   43,   44,   45,
   -1,   47,  125,  126,   41,   -1,   -1,   44,   36,   93,
   10,   59,   40,   59,   -1,   10,   41,   41,   59,   44,
   44,   -1,  125,  126,   41,   -1,   -1,   44,   93,   -1,
   58,   -1,   -1,   41,   59,   59,   44,   41,   -1,   -1,
   44,  125,  126,   -1,   -1,   -1,   41,   93,   -1,   44,
   -1,   36,   37,   -1,   -1,   40,   93,   42,  125,   59,
  125,  126,   47,   91,   59,   -1,   94,   -1,   93,   93,
   -1,   -1,   41,   58,   36,   44,   93,  125,   40,  125,
  126,  266,   -1,   -1,  125,   93,   -1,   -1,   -1,   93,
  275,  276,  277,  278,  279,  280,  281,  282,   93,   -1,
  125,  125,  287,  264,  265,  266,   91,   -1,   -1,   94,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,  264,  265,   93,  125,  287,   36,  270,   91,
  125,   40,   94,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,   -1,   -1,  287,   -1,  270,   -1,   58,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,  264,  265,   -1,  287,   -1,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
  264,  265,   91,   -1,  287,   94,  270,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,  264,
  265,   -1,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,  264,  265,
   10,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,  275,
  276,  277,  278,  279,  280,  281,  282,   -1,  266,   10,
  264,  265,   -1,   -1,   -1,  270,  270,   37,   -1,   -1,
   -1,   41,   42,   43,   44,   45,   10,   47,   10,  287,
   -1,   -1,   -1,   -1,   -1,   -1,   37,   -1,   -1,   59,
   41,   42,   43,   44,   45,  270,   47,   10,   -1,   -1,
   -1,  266,   -1,   -1,   -1,   -1,   -1,   41,   59,   41,
   44,   43,   44,   45,   -1,   -1,   10,   -1,   10,   -1,
   10,   -1,  287,   93,  266,   59,   -1,   59,   41,   -1,
   43,   44,   45,   -1,   -1,   -1,   -1,   10,   -1,   10,
   -1,   -1,   93,   -1,   -1,   -1,   59,   41,   -1,   41,
   44,   41,   44,   -1,   44,  125,  126,   -1,   10,   93,
   -1,   93,   -1,   -1,   -1,   59,   10,   59,   41,   59,
   41,   44,   -1,   44,  125,  126,   -1,  266,   -1,   -1,
   93,   -1,   -1,   -1,   -1,   -1,   59,   -1,   59,   41,
   -1,  125,   44,  125,  126,   -1,   -1,   41,   -1,   93,
   44,   93,   -1,   93,   -1,   -1,   -1,   59,   10,   -1,
   -1,   -1,  125,  126,   -1,   59,   -1,   -1,   -1,   10,
   93,   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  125,  126,  125,  126,  125,  126,   -1,   10,   41,
   -1,   93,   44,   -1,   -1,   -1,   -1,   -1,   -1,   93,
   41,   -1,  125,   44,  125,  126,   -1,   59,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   59,   41,
   -1,   -1,   44,  125,  126,   -1,   -1,   -1,   -1,   -1,
   -1,  125,  126,   -1,   -1,   -1,   -1,   59,   -1,   -1,
   -1,   93,   -1,   -1,  264,  265,   -1,   -1,   -1,   -1,
  270,   -1,   93,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,  264,  265,   -1,   -1,   -1,   -1,  270,
   -1,   93,   -1,  125,  275,  276,  277,  278,  279,  280,
  281,  282,  264,  265,  125,  126,  270,   -1,  270,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,  125,  126,   -1,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
  264,  265,  264,  265,  264,  265,  270,   -1,  270,   -1,
  270,   -1,   -1,   -1,   -1,   10,   -1,  281,  282,  281,
  282,  281,  282,  264,  265,   -1,   -1,  270,   -1,  270,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   10,
  281,  282,  264,  265,   10,   -1,   41,   -1,  270,   44,
  264,  265,   10,   10,   -1,   -1,  270,   -1,   -1,  281,
  282,   -1,   33,   10,   59,   10,   -1,  281,  282,   40,
   -1,   -1,   43,   -1,   45,   41,   -1,   -1,   44,   -1,
   -1,   -1,   -1,   41,   41,   -1,   44,   44,   -1,   -1,
   -1,   -1,   63,   59,   41,   -1,   41,   44,   93,   44,
   -1,   59,   59,  264,  265,   -1,   -1,   -1,   -1,  270,
   -1,   -1,   59,   -1,   59,   -1,   -1,   -1,   -1,   -1,
  281,  282,  264,  265,   -1,   -1,   -1,   93,  270,   -1,
  125,  126,   -1,   -1,   -1,   93,   93,   -1,   -1,  281,
  282,   -1,   -1,   -1,   -1,   -1,   93,   -1,   93,   -1,
   -1,   -1,  123,   -1,   -1,  126,   -1,   -1,   -1,  125,
   -1,   -1,   -1,   -1,   -1,   36,   37,  125,  125,   40,
   41,   42,   43,   44,   45,   -1,   47,   -1,  125,   -1,
  125,   -1,   -1,   -1,   36,   37,   -1,   58,   40,   41,
   42,   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   36,   37,   -1,   58,   40,   41,   42,
   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   91,   -1,   93,   94,   -1,   58,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,
   -1,   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,   91,   -1,
   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  264,
  265,   -1,   36,   37,  126,  270,   40,   41,   42,   43,
   -1,   45,   -1,   47,   -1,  256,  257,  282,  259,  260,
  261,  262,  263,  126,   58,   -1,  267,   -1,  269,   -1,
  271,  272,  273,  274,  270,   -1,  264,  265,   -1,   -1,
   -1,   -1,  270,  270,   -1,   -1,   -1,  264,  265,   -1,
   -1,   -1,   -1,  270,   -1,  270,   -1,   91,   36,   37,
   94,   -1,   40,   41,   42,   43,   -1,   45,   -1,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   36,   37,   91,   -1,   40,   94,   42,   43,   -1,
   45,   -1,   47,  264,  265,  266,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,  275,  276,  277,  278,  279,  280,
  281,  282,  264,  265,  266,   -1,  287,   -1,  126,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,  266,   -1,  287,   91,   -1,   -1,   94,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,   36,   37,   -1,  287,   40,   41,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  126,   -1,   58,   -1,   36,   37,   -1,   -1,   40,
   41,   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   -1,   -1,
  264,  265,  266,   -1,   -1,   -1,   91,   -1,   -1,   94,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,   -1,
   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,   36,   37,
   91,   -1,   40,   94,   42,   43,   -1,   45,   -1,   47,
   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   -1,   -1,   -1,   -1,   -1,  264,  265,  266,   -1,
   -1,   -1,   -1,   -1,   -1,  126,   -1,  275,  276,  277,
  278,  279,  280,  281,  282,   -1,   -1,   -1,   -1,  287,
   -1,   36,   37,   91,   -1,   40,   94,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,   -1,  264,
  265,  266,   -1,   -1,   -1,  270,   -1,   -1,  126,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   -1,   -1,
   -1,   -1,  287,   -1,   -1,   -1,   91,   -1,   -1,   94,
   -1,   -1,   -1,   36,   37,   -1,   -1,   40,   -1,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,
   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  264,
  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   91,   -1,
   -1,   94,  287,  264,  265,  266,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,   -1,   36,   37,   -1,  287,   40,   -1,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   36,   37,   -1,   -1,   40,   58,   42,   43,   -1,   45,
   -1,   47,   -1,   -1,   33,   -1,  264,  265,  266,   -1,
   -1,   40,   58,   -1,   43,   -1,   45,  275,  276,  277,
  278,  279,  280,  281,  282,   33,   -1,   -1,   91,  287,
   -1,   94,   40,   -1,   63,   43,   -1,   45,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   -1,   -1,   94,   -1,
   -1,   -1,   -1,   -1,   -1,   63,   -1,   -1,   -1,   -1,
   -1,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   -1,   -1,
   -1,   -1,  287,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  123,   -1,   -1,  126,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  123,   -1,   -1,  126,   -1,
   -1,   -1,   -1,  266,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,   -1,
   -1,   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  266,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,   -1,   -1,
  266,   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,   -1,
  259,  260,  261,  262,  263,   -1,   -1,   -1,  267,   -1,
  269,  287,  271,  272,  273,  274,   -1,   -1,   -1,   -1,
   -1,  259,  260,  261,  262,  263,    0,   -1,   -1,  267,
   -1,  269,   -1,  271,  272,  273,  274,   -1,   -1,   13,
   14,   15,   16,   17,   18,   -1,   20,   21,   -1,   -1,
   -1,   -1,   26,   27,   28,   29,   30,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   40,   41,   -1,   43,
   44,   45,   46,   47,   48,   49,   50,   51,   52,   53,
   54,   55,   56,   57,   58,   59,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   74,   75,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  106,   -1,   -1,  109,   -1,  111,   -1,   -1,
   -1,  115,  116,  117,   -1,   -1,   -1,   -1,   -1,  123,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  133,
};
#define YYFINAL 22
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 289
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,"'\\n'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,"'!'",0,0,"'$'","'%'",0,0,"'('","')'","'*'","'+'","','","'-'",0,"'/'",0,0,0,
0,0,0,0,0,0,0,"':'","';'",0,"'='",0,"'?'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,"'['",0,"']'","'^'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'{'",0,"'}'","'~'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"END_OF_INPUT","ERROR",
"STR_CONST","NUM_CONST","NULL_CONST","SYMBOL","FUNCTION","LEFT_ASSIGN",
"RIGHT_ASSIGN","LBB","FOR","IN","IF","ELSE","WHILE","NEXT","BREAK","REPEAT",
"GT","GE","LT","LE","EQ","NE","AND","OR","LOW","TILDE","UNOT","NOT","SPECIAL",
"UMINUS","UPLUS",
};
char *yyrule[] = {
"$accept : prog",
"prog : END_OF_INPUT",
"prog : '\\n'",
"prog : expr '\\n'",
"prog : expr ';'",
"prog : error",
"expr : NUM_CONST",
"expr : STR_CONST",
"expr : NULL_CONST",
"expr : SYMBOL",
"expr : '{' exprlist '}'",
"expr : '(' expr ')'",
"expr : '-' expr",
"expr : '+' expr",
"expr : '!' expr",
"expr : '~' expr",
"expr : '?' expr",
"expr : expr ':' expr",
"expr : expr '+' expr",
"expr : expr '-' expr",
"expr : expr '*' expr",
"expr : expr '/' expr",
"expr : expr '^' expr",
"expr : expr SPECIAL expr",
"expr : expr '%' expr",
"expr : expr '~' expr",
"expr : expr LT expr",
"expr : expr LE expr",
"expr : expr EQ expr",
"expr : expr NE expr",
"expr : expr GE expr",
"expr : expr GT expr",
"expr : expr AND expr",
"expr : expr OR expr",
"expr : expr LEFT_ASSIGN expr",
"expr : expr RIGHT_ASSIGN expr",
"expr : FUNCTION '(' formlist ')' cr expr",
"expr : expr '(' sublist ')'",
"expr : IF ifcond expr",
"expr : IF ifcond expr ELSE expr",
"expr : FOR forcond expr",
"expr : WHILE cond expr",
"expr : REPEAT expr",
"expr : expr LBB sublist ']' ']'",
"expr : expr '[' sublist ']'",
"expr : expr '$' SYMBOL",
"expr : expr '$' STR_CONST",
"expr : NEXT",
"expr : BREAK",
"cond : '(' expr ')'",
"ifcond : '(' expr ')'",
"forcond : '(' SYMBOL IN expr ')'",
"exprlist :",
"exprlist : expr",
"exprlist : exprlist ';' expr",
"exprlist : exprlist ';'",
"exprlist : exprlist '\\n' expr",
"exprlist : exprlist '\\n'",
"sublist : sub",
"sublist : sublist cr ',' sub",
"sub :",
"sub : expr",
"sub : SYMBOL '='",
"sub : SYMBOL '=' expr",
"sub : STR_CONST '='",
"sub : STR_CONST '=' expr",
"sub : NULL_CONST '='",
"sub : NULL_CONST '=' expr",
"formlist :",
"formlist : SYMBOL",
"formlist : SYMBOL '=' expr",
"formlist : formlist ',' SYMBOL",
"formlist : formlist ',' SYMBOL '=' expr",
"cr :",
};
#endif
#ifndef YYSTYPE
typedef int YYSTYPE;
#endif
#ifdef YYSTACKSIZE
#undef YYMAXDEPTH
#define YYMAXDEPTH YYSTACKSIZE
#else
#ifdef YYMAXDEPTH
#define YYSTACKSIZE YYMAXDEPTH
#else
#define YYSTACKSIZE 500
#define YYMAXDEPTH 500
#endif
#endif
int yydebug;
int yynerrs;
int yyerrflag;
int yychar;
short *yyssp;
YYSTYPE *yyvsp;
YYSTYPE yyval;
YYSTYPE yylval;
short yyss[YYSTACKSIZE];
YYSTYPE yyvs[YYSTACKSIZE];
#define yystacksize YYSTACKSIZE
#line 238 "gram.y"


/*----------------------------------------------------------------------------*/

static int xxvalue(SEXP v, int k)
{
	if(k > 2) UNPROTECT(1);
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
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = FirstArg(R_MissingArg, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxfirstformal1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = FirstArg(expr, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxaddformal0(SEXP formlist, SEXP sym)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode) {
		CheckFormalArgs(formlist ,sym);
		PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(3);
	if(GenerateCode) {
		CheckFormalArgs(formlist, sym);
		PROTECT(ans = NextArg(formlist, expr, sym));
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist0()
{
	SEXP ans;
	if(GenerateCode)
		PROTECT(ans = NewList());
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist1(SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = GrowList(NewList(), expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = GrowList(exprlist, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsub0(void)
{
	SEXP ans;
	if(GenerateCode)
		PROTECT(ans = lang2(R_MissingArg,R_NilValue));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsub1(SEXP expr)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, R_NilValue));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsymsub0(SEXP sym)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(R_MissingArg, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, sym));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnullsub0()
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = TagArg(R_MissingArg, install("NULL")));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnullsub1(SEXP expr)
{
	SEXP ans = install("NULL");
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = TagArg(expr, ans));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}


static SEXP xxsublist1(SEXP sub)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
	else
		PROTECT(ans = R_NilValue);
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
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(ifsym, cond, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
	SEXP ans;
	UNPROTECT(3);
	if(GenerateCode)
		PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
	SEXP ans;
	EatLines = 1;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = LCONS(sym, expr));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(whilesym, cond, body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(repeatsym, body));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
	if(GenerateCode)
		PROTECT(keyword = lang1(keyword));
	else
		PROTECT(keyword = R_NilValue);
	return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
	SEXP ans;
	if(GenerateCode) {
		if(isString(expr))
			expr = install(CHAR(STRING(expr)[0])); 
		UNPROTECT(2);
		if(length(CDR(args)) == 1 && CADR(args) == R_MissingArg )
			ans = lang1(expr);
		else    
			ans = LCONS(expr, CDR(args));   
		PROTECT(ans);
	}
	else {
		UNPROTECT(2);
		PROTECT(ans = R_NilValue);
	}
	return ans;
}       

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{
	SEXP ans;
	AddComment(body);
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(fname, CDR(formals), body)); 
	else
		PROTECT(ans = R_NilValue);
	PopComment();
	return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(op, arg));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = lang3(n1, n2, n3));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
	SEXP ans;
	UNPROTECT(1);
	if(GenerateCode)
		PROTECT(ans = lang2(n1, n2));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
	SEXP ans;
	UNPROTECT(2);
	if(GenerateCode)
		PROTECT(ans = LCONS(a2, LCONS(a1, CDR(a3))));
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
	SEXP ans;
	UNPROTECT(1);
	EatLines = 0;
	if(GenerateCode) {
		TYPEOF(a2) = LANGSXP;
		CAR(a2) = a1;
		PROTECT(ans = a2);
	}
	else
		PROTECT(ans = R_NilValue);
	return ans;
}

/*----------------------------------------------------------------------------*/

static SEXP TagArg(SEXP arg, SEXP tag)
{
	switch (TYPEOF(tag)) {
	case NILSXP:
	case SYMSXP:
	case STRSXP:
		return lang2(arg, tag);
	default:
		error("incorrect tag type\n");
	}
}

/*
 *  Stretchy List Structures
 *
 *  Lists are created and grown using a special dotted pair.
 *  The CAR of the list points to the last cons-cell in the
 *  list and the CDR points to the first.  The list can be
 *  extracted from the pair by taking its CDR, while the CAR
 *  gives fast access to the end of the list.
 */

	/* Create a stretchy-list dotted pair */

static SEXP NewList(void)
{
	SEXP s = CONS(R_NilValue, R_NilValue);
	CAR(s) = s;
	return s;
}

	/* Add a new element at the end of a stretchy list */

static SEXP GrowList(SEXP l, SEXP s)
{
	SEXP tmp;
	PROTECT(l);
	tmp = CONS(s, R_NilValue);
	UNPROTECT(1);
	SETCDR(CAR(l), tmp);
	CAR(l) = tmp;
	return l;
}

/*
 *  Comment Handling
 *
 *  R_CommentSxp is of the same form as an expression list,
 *  each time a new { is encountered a new element is placed
 *  in the R_CommentSxp and when a } is encountered it is
 *  removed.
 *
 *  The following routine is referenced in error.c.
 *  That reference should be removed.
 */

static void ResetComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

static void PushComment(void)
{
	if(GenerateCode)
		R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

static void PopComment(void)
{
	if(GenerateCode)
		R_CommentSxp = CDR(R_CommentSxp);
}

int IsComment(SEXP l)
{
	if (isList(l) && isString(CAR(l))
	&& !strncmp(CHAR(STRING(CAR(l))[0]), "#", 1))
		return 1;
	else
		return 0;
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

static SEXP FirstArg(SEXP s, SEXP tag)
{
	SEXP tmp;
	PROTECT(s);
	PROTECT(tag);
	tmp = NewList();
	tmp = GrowList(tmp, s);
	TAG(CAR(tmp)) = tag;
	UNPROTECT(2);
	return tmp;
}

static SEXP NextArg(SEXP l, SEXP s, SEXP tag)
{
	PROTECT(tag);
	l = GrowList(l, s);
	TAG(CAR(l)) = tag;
	UNPROTECT(1);
	return l;
}

/*----------------------------------------------------------------------------*/

	/* Basic File IO */

	/* This code is here because at this particular instant it */
	/* seems closely related to cget(), which appears below.  */
	/* But now it doesn't.  Move this to iosupport.c or trash it */

int R_fgetc(FILE *fp)
{
	int c = fgetc(fp);
	return feof(fp) ? R_EOF : c;
}


/*----------------------------------------------------------------------------*/

/*
 *  Parsing Entry Points:
 *
 *  The Following entry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IOBuffers
 *  (i.e. interactve use), files and R character strings.

 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *  The following routines parse a single expression:
 *
 *	SEXP R_Parse1File(FILE *fp, int gencode, int *status)
 *
 *	SEXP R_Parse1Vector(TextBuffer *text, int gencode, int *status)
 *
 *	SEXP R_Parse1Buffer(IOBuffer *buffer, int gencode, int *status)
 *	
 *  The success of the parse is indicated as folllows:
 *
 *	status = PARSE_NULL       - there was no statement to parse
 *		 PARSE_OK	 - complete statement
 *		 PARSE_INCOMPLETE - incomplete statement
 *		 PARSE_ERROR      - syntax error
 *		 PARSE_EOF	- end of file
 *
 *  The following routines parse several expressions and return
 *  their values in a single expression vector.
 *
 *	SEXP R_ParseFile(FILE *fp, int n, int *status)
 *
 *	SEXP R_ParseVector(TextBuffer *text, int n, int *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int n, int *status)
 *
 *  Here, status is 1 for a successful parse and 0 if parsing
 *  failed for some reason.
 */

static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[50], *contextp;

static SEXP ParseInit()
{
	contextp = contextstack;
	*contextp = ' ';
	SavedToken = 0;
	SavedLval = R_NilValue;
	EatLines = 0;
	EndOfFile = 0;
	ResetComment();
}

static int file_getc(void)
{
	int c = R_fgetc(R_Inputfile);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	if(c == '\n') R_ParseError += 1;
	return c;
}

static int file_ungetc(int c)
{
	if(c == '\n') R_ParseError -= 1;
	return ungetc(c, R_Inputfile);
}

SEXP R_Parse1File(FILE *fp, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	R_Inputfile = fp;
	xxgetc = file_getc;
	xxungetc = file_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	return R_CurrentExpr;
}

static IoBuffer *iob;

static int buffer_getc()
{
	int c = R_IoBufferGetc(iob);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	else return c;
}

static int buffer_ungetc(int c)
{
	return R_IoBufferUngetc(c, iob);
}

SEXP R_Parse1Buffer(IoBuffer *buffer, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	iob = buffer;
	xxgetc = buffer_getc;
	xxungetc = buffer_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		if(EndOfFile == 2) *status = PARSE_INCOMPLETE;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	return R_CurrentExpr;
}

static TextBuffer *txtb;

static int text_getc()
{
	int c = R_TextBufferGetc(txtb);
	if(c == EOF) {
		EndOfFile = 1;
		return R_EOF;
	}
	else return c;
}

static int text_ungetc(int c)
{
	return R_TextBufferUngetc(c, txtb);
}

SEXP R_Parse1Vector(TextBuffer *textb, int gencode, int *status)
{
	ParseInit();
	GenerateCode = gencode;
	txtb = textb;
	xxgetc = text_getc;
	xxungetc = text_ungetc;

	switch(yyparse()) {
	    case 0:			/* End of file */
		*status = PARSE_EOF;
		break;
	    case 1:			/* Syntax error / incomplete */
		*status = PARSE_ERROR;
		if(EndOfFile) *status = PARSE_INCOMPLETE;
		break;
	    case 2:			/* Empty Line */
		*status = PARSE_NULL;
		break;
	    case 3:			/* Valid expr '\n' terminated */
	    case 4:			/* Valid expr ';' terminated */
		*status = PARSE_OK;
		break;
	}
	return R_CurrentExpr;
}

SEXP R_ParseFile(FILE *fp, int n, int *status)
{
	SEXP rval, t;
	int i;

	R_ParseError = 1;
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			t = R_Parse1File(fp, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
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
			rval = R_Parse1File(fp, 1, status);
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
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
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

SEXP R_ParseVector(SEXP text, int n, int *status)
{
	SEXP rval, t;
	TextBuffer textb;
	int i;

	R_TextBufferInit(&textb, text);
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			t = R_Parse1Vector(&textb, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
				break;
			    case PARSE_INCOMPLETE:
			    case PARSE_ERROR:
			    case PARSE_EOF:
				rval = R_NilValue;
				break;
			}
		}
		UNPROTECT(1);
		R_TextBufferFree(&textb);
		return rval;
	}
	else {
		PROTECT(t = NewList());
		for(;;) {
			rval = R_Parse1Vector(&textb, 1, status);
			switch(*status) {
			case PARSE_NULL:
				break;
			case PARSE_OK:
				t = GrowList(t, rval);
				break;
			case PARSE_INCOMPLETE:
			case PARSE_ERROR:
				R_TextBufferFree(&textb);
				UNPROTECT(1);
				return R_NilValue;
				break;
			case PARSE_EOF:
				R_TextBufferFree(&textb);
				t = CDR(t);
				rval = allocVector(EXPRSXP, length(t));
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
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

static int prompt_type;

static char *Prompt(SEXP prompt, int type)
{
	if(type == 1) {
		if(length(prompt) <= 0) {
			return (char*)CHAR(STRING(GetOption(install("prompt"),
				R_NilValue))[0]);
		}
		else
			return CHAR(STRING(prompt)[0]);
	}
	else {
		return (char*)CHAR(STRING(GetOption(install("continue"),
			R_NilValue))[0]);
	}
}

SEXP R_ParseBuffer(IoBuffer *buffer, int n, int *status, SEXP prompt)
{
	SEXP rval, t;
	char *bufp, buf[1024];
	int c, i, prompt_type = 1;

	R_IoBufferWriteReset(buffer);
	buf[0] = '\0';
	bufp = buf;
	if(n >= 0) {
		PROTECT(rval = allocVector(EXPRSXP, n));
		for(i=0 ; i<n ; i++) {
		    try_again:
			if(!*bufp) {
				if(R_ReadConsole(Prompt(prompt, prompt_type),
					buf, 1024, 1) == 0) return;
				bufp = buf;
			}
			while(c = *bufp++) {
				R_IoBufferPutc(c, buffer);
				if(c == ';' || c == '\n') {
					break;
				}
			}
			t = R_Parse1Buffer(buffer, 1, status);
			switch(*status) {
			    case PARSE_NULL:
				goto try_again;
				break;
			    case PARSE_OK:
				VECTOR(rval)[i] = t;
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
		for(;;) {
			if(!*bufp) {
				if(R_ReadConsole(Prompt(prompt, prompt_type),
					buf, 1024, 1) == 0) return;
				bufp = buf;
			}
			while(c = *bufp++) {
				R_IoBufferPutc(c, buffer);
				if(c == ';' || c == '\n') {
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
				for(n=0 ; n<LENGTH(rval) ; n++) {
					VECTOR(rval)[n] = CAR(t);
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
 *  Lexical Analyzer:
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
 *----------------------------------------------------------------------------*/

static void IfPush(void)
{
	if ( *contextp==LBRACE || *contextp=='['
	  || *contextp=='(' || *contextp == 'i')
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
	{ "NULL",	NULL_CONST	},
	{ "NA",		NUM_CONST	},
	{ "TRUE",	NUM_CONST	},
	{ "FALSE",	NUM_CONST	},
	{ "GLOBAL.ENV",	NUM_CONST	},
	{ "function",	FUNCTION	},
	{ "while",	WHILE		},
	{ "repeat",	REPEAT		},
	{ "for",	FOR		},
	{ "if",		IF		},
	{ "in",		IN		},
	{ "else",	ELSE		},
	{ "next",	NEXT		},
	{ "break",	BREAK		},
	{ "...",	SYMBOL		},
	{ 0,		0		}
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


SEXP mkString(char *s)
{
	SEXP t;

	PROTECT(t = allocVector(STRSXP, 1));
	STRING(t)[0] = mkChar(s);
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

int yyerror(char *s)
{
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
	int i;

	while( formlist != R_NilValue ) {
		if(TAG(formlist) == new ) {
			error("Repeated formal argument.\n");
		}
		formlist=CDR(formlist);
	}
}

static char yytext[MAXELTSIZE];

static int SkipSpace(void)
{
	int c;
	while ((c = xxgetc()) == ' ' || c == '\t' || c == '')
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
	char *p;
	SEXP f;
	int c;

	p = yytext;
	*p++ = '#';
	while ((c = xxgetc()) != '\n' && c != R_EOF)
		*p++ = c;
	*p = '\0';
	if(GenerateCode && R_CommentSxp != R_NilValue) {
		f = mkChar(yytext);
		f = CONS(f, R_NilValue);
		CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
	}
	if(c == R_EOF) EndOfFile = 2;
	return c;
}

static int NumericValue(int c)
{
	int seendot = (c == '.');
	int seenexp = 0;
	char *p = yytext;
	*p++ = c;
	while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E') {
		if (c == 'E' || c == 'e') {
			if (seenexp)
				break;
			seenexp = 1;
			seendot = 1;
			*p++ = c;
			c = xxgetc();
			if (!isdigit(c) && c != '+' && c != '-')
				break;
		}
		if (c == '.') {
			if (seendot)
				break;
			seendot = 1;
		}
		*p++ = c;
	}
	*p = '\0';
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

static int StringValue(int c)
{
	int quote = c;
	char *p = yytext;
	while ((c = xxgetc()) != R_EOF && c != quote) {
		if (c == '\n') {
			xxungetc(c);
			return ERROR;
		}
		if (c == '\\') {
			c = xxgetc();
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
		*p++ = c;
	}
	*p = '\0';
	PROTECT(yylval = mkString(yytext));
	return STR_CONST;
}

static int SpecialValue(int c)
{
	char *p = yytext;
	*p++ = c;
	while ((c = xxgetc()) != R_EOF && c != '%') {
		if (c == '\n') {
			xxungetc(c);
			return ERROR;
		}
		*p++ = c;
	}
	if (c == '%')
		*p++ = c;
	*p++ = '\0';
	yylval = install(yytext);
	return SPECIAL;
}

static int SymbolValue(int c)
{
	int kw;
	char *p = yytext;
	do {
		*p++ = c;
	}
	while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.'));
	xxungetc(c);
	*p = '\0';
	if ((kw = KeywordLookup(yytext))) {
		if(kw == FUNCTION) PushComment();
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
	char *p;

	if(SavedToken) {
		c = SavedToken;
		yylval = SavedLval;
		SavedLval = R_NilValue;
		SavedToken = 0;
		return c;
	}
		
    again:

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

	if (c == '.' || isdigit(c)) return NumericValue(c);

		/* literal strings */

	if (c == '\"' || c == '\'') return StringValue(c);

		/* special functions */

	if (c == '%') return SpecialValue(c);

		/* functions, constants and variables */

    symbol:

	if (c == '.' || isalpha(c)) return SymbolValue(c);

		/* gag, barf, but the punters want it */

	if (c == '_') {
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

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
		if (nextchar('<'))
			if (nextchar('-')) {
				yylval = install("<<-");
				return LEFT_ASSIGN;
			}
			else
				return ERROR;
		yylval = install("<");
		return LT;
	case '-':
		if (nextchar('>'))
			if (nextchar('>')) {
				yylval = install("<<-");
				return RIGHT_ASSIGN;
			}
			else {
				yylval = install("<-");
				return RIGHT_ASSIGN;
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
		return '=';
	case ':':
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
		strcpy(yytext, "help");
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
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}

int yylex()
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
	case LEFT_ASSIGN:
	case RIGHT_ASSIGN:
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
		PushComment();
		break;

	case '(':
		*++contextp = tok;
		break;

	case ']':
		ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	case RBRACE:
		ifpop();
		if(*contextp == LBRACE)
			PopComment();
		*contextp-- = 0;
		break;

	case ')':
		ifpop();
		*contextp-- = 0;
		EatLines = 0;
		break;

	}
	return tok;
}
#line 2384 "y.tab.c"
#define YYABORT goto yyabort
#define YYREJECT goto yyabort
#define YYACCEPT goto yyaccept
#define YYERROR goto yyerrlab
int
yyparse()
{
    register int yym, yyn, yystate;
#if YYDEBUG
    register char *yys;
    extern char *getenv();

    if (yys = getenv("YYDEBUG"))
    {
        yyn = *yys;
        if (yyn >= '0' && yyn <= '9')
            yydebug = yyn - '0';
    }
#endif

    yynerrs = 0;
    yyerrflag = 0;
    yychar = (-1);

    yyssp = yyss;
    yyvsp = yyvs;
    *yyssp = yystate = 0;

yyloop:
    if (yyn = yydefred[yystate]) goto yyreduce;
    if (yychar < 0)
    {
        if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, reading %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
    }
    if ((yyn = yysindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: state %d, shifting to state %d\n",
                    YYPREFIX, yystate, yytable[yyn]);
#endif
        if (yyssp >= yyss + yystacksize - 1)
        {
            goto yyoverflow;
        }
        *++yyssp = yystate = yytable[yyn];
        *++yyvsp = yylval;
        yychar = (-1);
        if (yyerrflag > 0)  --yyerrflag;
        goto yyloop;
    }
    if ((yyn = yyrindex[yystate]) && (yyn += yychar) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yychar)
    {
        yyn = yytable[yyn];
        goto yyreduce;
    }
    if (yyerrflag) goto yyinrecovery;
#ifdef lint
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#ifdef lint
    goto yyerrlab;
#endif
yyerrlab:
    ++yynerrs;
yyinrecovery:
    if (yyerrflag < 3)
    {
        yyerrflag = 3;
        for (;;)
        {
            if ((yyn = yysindex[*yyssp]) && (yyn += YYERRCODE) >= 0 &&
                    yyn <= YYTABLESIZE && yycheck[yyn] == YYERRCODE)
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: state %d, error recovery shifting\
 to state %d\n", YYPREFIX, *yyssp, yytable[yyn]);
#endif
                if (yyssp >= yyss + yystacksize - 1)
                {
                    goto yyoverflow;
                }
                *++yyssp = yystate = yytable[yyn];
                *++yyvsp = yylval;
                goto yyloop;
            }
            else
            {
#if YYDEBUG
                if (yydebug)
                    printf("%sdebug: error recovery discarding state %d\n",
                            YYPREFIX, *yyssp);
#endif
                if (yyssp <= yyss) goto yyabort;
                --yyssp;
                --yyvsp;
            }
        }
    }
    else
    {
        if (yychar == 0) goto yyabort;
#if YYDEBUG
        if (yydebug)
        {
            yys = 0;
            if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
            if (!yys) yys = "illegal-symbol";
            printf("%sdebug: state %d, error recovery discards token %d (%s)\n",
                    YYPREFIX, yystate, yychar, yys);
        }
#endif
        yychar = (-1);
        goto yyloop;
    }
yyreduce:
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: state %d, reducing by rule %d (%s)\n",
                YYPREFIX, yystate, yyn, yyrule[yyn]);
#endif
    yym = yylen[yyn];
    yyval = yyvsp[1-yym];
    switch (yyn)
    {
case 1:
#line 138 "gram.y"
{ return 0; }
break;
case 2:
#line 139 "gram.y"
{ return xxvalue(NULL,2); }
break;
case 3:
#line 140 "gram.y"
{ return xxvalue(yyvsp[-1],3); }
break;
case 4:
#line 141 "gram.y"
{ return xxvalue(yyvsp[-1],4); }
break;
case 5:
#line 142 "gram.y"
{ YYABORT; }
break;
case 6:
#line 145 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 7:
#line 146 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 8:
#line 147 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 9:
#line 148 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 10:
#line 150 "gram.y"
{ yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); }
break;
case 11:
#line 151 "gram.y"
{ yyval = xxparen(yyvsp[-2],yyvsp[-1]); }
break;
case 12:
#line 153 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 13:
#line 154 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 14:
#line 155 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 15:
#line 156 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 16:
#line 157 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 17:
#line 159 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 18:
#line 160 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 19:
#line 161 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 20:
#line 162 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 21:
#line 163 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 22:
#line 164 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 23:
#line 165 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 24:
#line 166 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 25:
#line 167 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 26:
#line 168 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 27:
#line 169 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 28:
#line 170 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 29:
#line 171 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 30:
#line 172 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 31:
#line 173 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 32:
#line 174 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 33:
#line 175 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 34:
#line 177 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 35:
#line 178 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); }
break;
case 36:
#line 180 "gram.y"
{ yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); }
break;
case 37:
#line 181 "gram.y"
{ yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); }
break;
case 38:
#line 182 "gram.y"
{ yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 39:
#line 183 "gram.y"
{ yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); }
break;
case 40:
#line 184 "gram.y"
{ yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 41:
#line 185 "gram.y"
{ yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 42:
#line 186 "gram.y"
{ yyval = xxrepeat(yyvsp[-1],yyvsp[0]); }
break;
case 43:
#line 187 "gram.y"
{ yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); }
break;
case 44:
#line 188 "gram.y"
{ yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); }
break;
case 45:
#line 189 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 46:
#line 190 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 47:
#line 191 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 48:
#line 192 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 49:
#line 196 "gram.y"
{ yyval = xxcond(yyvsp[-1]); }
break;
case 50:
#line 199 "gram.y"
{ yyval = xxifcond(yyvsp[-1]); }
break;
case 51:
#line 202 "gram.y"
{ yyval = xxforcond(yyvsp[-3],yyvsp[-1]); }
break;
case 52:
#line 206 "gram.y"
{ yyval = xxexprlist0(); }
break;
case 53:
#line 207 "gram.y"
{ yyval = xxexprlist1(yyvsp[0]); }
break;
case 54:
#line 208 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 55:
#line 209 "gram.y"
{ yyval = yyvsp[-1]; AddComment(CAR(yyval));}
break;
case 56:
#line 210 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 57:
#line 211 "gram.y"
{ yyval = yyvsp[-1];}
break;
case 58:
#line 214 "gram.y"
{ yyval = xxsublist1(yyvsp[0]); }
break;
case 59:
#line 215 "gram.y"
{ yyval = xxsublist2(yyvsp[-3],yyvsp[0]); }
break;
case 60:
#line 218 "gram.y"
{ yyval = xxsub0(); }
break;
case 61:
#line 219 "gram.y"
{ yyval = xxsub1(yyvsp[0]); }
break;
case 62:
#line 220 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 63:
#line 221 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 64:
#line 222 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 65:
#line 223 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 66:
#line 224 "gram.y"
{ yyval = xxnullsub0(); }
break;
case 67:
#line 225 "gram.y"
{ yyval = xxnullsub1(yyvsp[0]); }
break;
case 68:
#line 228 "gram.y"
{ yyval = xxnullformal(); }
break;
case 69:
#line 229 "gram.y"
{ yyval = xxfirstformal0(yyvsp[0]); }
break;
case 70:
#line 230 "gram.y"
{ yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); }
break;
case 71:
#line 231 "gram.y"
{ yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); }
break;
case 72:
#line 232 "gram.y"
{ yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); }
break;
case 73:
#line 235 "gram.y"
{ EatLines = 1; }
break;
#line 2817 "y.tab.c"
    }
    yyssp -= yym;
    yystate = *yyssp;
    yyvsp -= yym;
    yym = yylhs[yyn];
    if (yystate == 0 && yym == 0)
    {
#if YYDEBUG
        if (yydebug)
            printf("%sdebug: after reduction, shifting from state 0 to\
 state %d\n", YYPREFIX, YYFINAL);
#endif
        yystate = YYFINAL;
        *++yyssp = YYFINAL;
        *++yyvsp = yyval;
        if (yychar < 0)
        {
            if ((yychar = yylex()) < 0) yychar = 0;
#if YYDEBUG
            if (yydebug)
            {
                yys = 0;
                if (yychar <= YYMAXTOKEN) yys = yyname[yychar];
                if (!yys) yys = "illegal-symbol";
                printf("%sdebug: state %d, reading %d (%s)\n",
                        YYPREFIX, YYFINAL, yychar, yys);
            }
#endif
        }
        if (yychar == 0) goto yyaccept;
        goto yyloop;
    }
    if ((yyn = yygindex[yym]) && (yyn += yystate) >= 0 &&
            yyn <= YYTABLESIZE && yycheck[yyn] == yystate)
        yystate = yytable[yyn];
    else
        yystate = yydgoto[yym];
#if YYDEBUG
    if (yydebug)
        printf("%sdebug: after reduction, shifting from state %d \
to state %d\n", YYPREFIX, *yyssp, yystate);
#endif
    if (yyssp >= yyss + yystacksize - 1)
    {
        goto yyoverflow;
    }
    *++yyssp = yystate;
    *++yyvsp = yyval;
    goto yyloop;
yyoverflow:
    yyerror("yacc stack overflow");
yyabort:
    return (1);
yyaccept:
    return (0);
}
