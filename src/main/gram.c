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

#include "IOStuff.h"/*-> Defn.h */
#include "Fileio.h"
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
/* NOT_used static int	IsComment(SEXP);*/
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

#line 120 "y.tab.c"
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
short yysindex[] = {                                    794,
    0,    0,    0,    0,    0,    0,  -39,  -37,  -31,  -26,
    0,    0, 1361, 1361, 1361, 1361, 1361, 1361,    0, 1361,
 1361,    0,  -10, -246, -240, 1361, 1361, 1361, 1361, 1361,
 1227, 1227, 1331,  -32,  -32, 1062, 1227,   15, 1509, 1361,
 1361, 1404, 1361, 1361, 1361, 1361, 1361, 1361, 1361, 1361,
 1361, 1361, 1361, 1361, 1361, 1361, 1361, 1361, 1361, -257,
 1404, 1404,    0,    0,  -27,  -23, -244, 1227, 1134, 1153,
 1177, 1227,    0, 1361, 1361,    0, 1227, 1475,  -20,   -6,
   -4, 1227,  -62,    0,  -30,  -30,  -30,  -30,  -30,  -30,
 1509, 1492, 1331,  147,  147,  123,  123,  123,  122,  -32,
  -32,    0,    0,   41,    6, 1361,    0, -162, 1361,    0,
 1361,    0, 1227, 1227, 1361, 1361, 1361,    8,   59,    0,
    0, 1227, 1361,   44, 1196, 1227, 1227, 1227, 1227,    0,
 1404, 1227, 1361,    0,    0, 1227,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   19,
    0,    0,    0,  -21,    0,    0,    0,    0,    0,    0,
  126,  156,  127,    9,   28,    0,   84,    0,  757,    0,
    0,  -33,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   -1,  -33,    0,    0,    1,    0,    0,  397,    0,  389,
    0,  435,    0,  110,  151,    0,  498,  458,  867,  886,
 1033,   -5,   69,    0,  437,  456,  475,  494,  719,  738,
  767,  776,  439,  399,  418,  105,  361,  380,   86,   48,
   67,    0,    0,   69,   69,    0,    0,    0,    0,    0,
    0,    0,  186,  189,   39,  131,  182,    0,    0,    0,
    0,    3,    0,   22,    0,  755,  202,  206,  371,    0,
  391,  774,    0,    0,    0,   35,
};
short yygindex[] = {                                      0,
 1777,    0,    0,   10,   36,    0,    0,    0,  -13,
};
#define YYTABLESIZE 1910
short yytable[] = {                                      63,
   24,  102,   25,   60,  103,   60,   56,   61,   27,   61,
   60,   54,   52,   29,   53,   65,   55,  107,   13,   68,
  108,   67,   68,  109,   74,   60,   56,   58,   52,   61,
  118,   54,   52,  106,   53,   61,   55,   12,   61,   60,
  115,   69,   60,   70,   69,   13,   70,   58,   64,   13,
   13,   13,   13,   13,  116,   13,  117,   17,   62,   60,
   62,   59,   71,   59,   12,   71,   13,   13,   12,   12,
   12,   12,   12,   75,   12,   72,   22,   52,   72,   64,
   62,  120,   64,   59,   17,   12,   12,   61,   17,   17,
   17,   17,   17,   53,   17,   23,  104,  105,  121,  124,
  130,   13,  131,   22,  133,   17,   17,   22,   22,   22,
   22,   22,   73,   22,   20,   51,  123,  135,    0,   57,
   12,    0,   23,    0,   22,   22,   23,   23,   23,   23,
   23,   64,   23,   13,   13,   42,   15,    0,    0,   76,
   17,   20,   53,   52,   23,   20,   20,   20,   20,   20,
    0,   20,   12,   12,    0,    0,    0,   60,   60,   22,
   55,   61,   61,   20,    0,   16,   42,   15,   57,   42,
   15,   66,   17,   17,   66,    0,    0,    0,   23,   58,
   58,    0,   60,   56,   42,   15,   61,    0,   54,    0,
    0,   22,   22,   55,    0,   56,   16,   20,   54,   16,
    0,    0,    0,    0,   58,    0,    0,    0,   53,   55,
   23,   23,   62,   62,   16,   59,   59,    0,   42,   15,
    0,    0,   62,   66,    0,   62,    0,    0,    0,   20,
   20,    0,    0,   42,   57,   42,    0,   62,    0,    0,
   59,    0,   65,    0,   56,   65,   67,   54,   16,   67,
   42,   15,   15,   40,   41,   42,   57,    0,    0,    0,
    0,    0,    0,    0,   43,   44,   45,   46,   47,   48,
   49,   50,   13,   13,   62,   55,   57,    0,   13,    0,
   16,    0,    0,   13,   13,   13,   13,   13,   13,   13,
   13,   12,   12,    0,   65,   13,    0,   12,   67,    0,
    0,    0,   12,   12,   12,   12,   12,   12,   12,   12,
   56,   17,   17,   54,   12,    0,    0,   17,    0,    0,
    0,    0,   17,   17,   17,   17,   17,   17,   17,   17,
   22,   22,    0,    0,   17,    0,   22,    0,    0,    0,
    0,   22,   22,   22,   22,   22,   22,   22,   22,   23,
   23,    0,    0,   22,    0,   23,    0,    0,    0,    0,
   23,   23,   23,   23,   23,   23,   23,   23,   20,   20,
   21,    0,   23,    0,   20,    0,    0,    0,    0,   20,
   20,   20,   20,   20,   20,   20,   20,   42,   42,   24,
   15,   15,    0,    0,    0,   42,   15,   21,   38,    0,
    0,   21,   21,   21,   21,   21,   40,   21,   18,   57,
    0,   63,   42,    0,   63,    0,   24,    0,    0,   21,
   24,   24,   24,   24,   24,   16,   24,   19,    0,   38,
    0,   60,   38,   57,   60,    0,    0,   40,   24,   18,
   40,   18,   18,   18,   41,    0,   31,   38,   25,    0,
    0,    0,    0,   21,    0,   40,    0,   18,   19,    0,
   19,   19,   19,   63,    0,   30,    0,   35,    0,    0,
    0,    0,   24,    0,    0,   41,   19,   31,   41,   25,
   31,   38,   25,   60,   26,   21,   21,    0,    0,   40,
    0,   18,    0,   41,    0,   31,   30,   25,   35,   30,
    0,   35,    0,   27,   24,   24,    0,   34,    0,    0,
   19,    0,    0,   38,   30,   26,   35,    0,   26,    0,
    0,   40,    0,   18,   18,    0,    0,   41,    0,   31,
    0,   25,    0,   26,   27,    0,    0,   27,   34,    0,
    0,   34,   19,   19,    0,    0,    0,    0,   30,    0,
   35,    0,   27,    0,    0,    0,   34,    0,    0,   41,
    0,   31,   31,   25,   25,    0,    0,   26,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   30,   30,   35,    0,    0,    0,   27,    0,    0,    0,
   34,    0,    0,    0,    0,    0,    0,    0,    0,   26,
   26,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   27,   27,
    0,    0,   34,    0,   21,   21,    0,    0,    0,    0,
   21,    0,    0,    0,    0,   21,   21,   21,   21,   21,
   21,   21,   21,   24,   24,    0,    0,    0,    0,   24,
    0,    0,    0,    0,   24,   24,   24,   24,   24,   24,
   24,   24,   18,   18,    0,    0,   40,    0,   18,    0,
    0,    0,    0,   18,   18,   18,   18,   18,   18,   18,
   18,   19,   19,    0,    0,    0,    0,   19,    0,    0,
    0,    0,   19,   19,   19,   19,   19,   19,   19,   19,
   31,   31,   25,   25,   41,    0,   31,    0,   25,    0,
    0,   31,   31,   31,   31,   31,   31,   31,   31,   30,
   30,   35,   35,    0,    0,   30,    0,   35,   28,    0,
   30,   30,   30,   30,   30,   30,   30,   30,   26,   26,
    0,    0,    0,    0,   26,    0,    0,   29,    0,   26,
   26,   26,   26,   26,   26,   26,   26,   27,   27,   28,
    0,    0,   28,   27,   39,    0,   14,   34,   27,   27,
   27,   27,   27,   27,   27,   27,   32,   28,   29,    0,
    0,   29,    0,   36,    0,   33,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   39,   29,   14,   39,    0,
   14,    0,    0,   19,    0,    0,    0,   32,    0,    0,
   32,   28,    0,   39,   36,   14,   33,   36,    0,   33,
    0,    0,    0,    0,    0,   32,   21,    0,    0,    0,
   29,    0,   36,   18,   33,    0,   16,    0,   17,    0,
    0,    0,    0,   28,   28,    0,    0,   39,    0,   14,
    0,    0,    0,    0,    0,    0,   14,    0,    0,   32,
    0,    0,   29,   29,    0,    0,   36,    0,   33,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   39,
    0,   14,   14,    0,    0,    0,    0,    0,    0,    0,
    0,   32,   32,    0,    0,    0,    0,    0,   36,    0,
   33,   33,    7,    7,    0,    0,    7,    7,    7,    7,
    7,    7,    0,    7,    0,    0,   20,    0,    0,   15,
    0,    8,    8,    0,    7,    8,    8,    8,    8,    8,
    8,    0,    8,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    8,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    7,    0,    7,
    7,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    8,    0,    8,    8,
    0,    0,   28,   28,    0,    0,    0,    0,   28,    0,
    0,    0,    7,   28,   28,   28,   28,   28,   28,   28,
   28,   29,   29,    0,    0,    0,    0,   29,    0,    0,
    0,    8,   29,   29,   29,   29,   29,   29,   29,   29,
   14,   14,    0,    0,   39,    0,   14,    0,    0,    0,
   32,   32,    0,    0,    0,    0,   32,   14,   14,   33,
   33,    0,    0,   36,    0,   33,    0,   32,   32,    1,
    2,    0,    3,    4,    5,    6,    7,   33,    0,    0,
    8,    0,    9,    0,   10,   11,   12,   13,    9,    9,
    0,    0,    9,    9,    9,    9,    9,    9,    0,    9,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    9,    0,    0,    0,    0,    0,    0,   60,   56,    0,
    0,   61,   73,   54,   52,    0,   53,    0,   55,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   58,
    0,    0,    0,    9,    0,    9,    9,    0,    0,    0,
    7,    7,    7,    0,    0,    0,    0,    0,    0,    0,
    0,    7,    7,    7,    7,    7,    7,    7,    7,    8,
    8,    8,   62,    7,    0,   59,    0,    0,    9,    0,
    8,    8,    8,    8,    8,    8,    8,    8,    0,   60,
   56,    0,    8,   61,  110,   54,   52,    0,   53,    0,
   55,    0,    0,    0,    0,    0,    0,   51,   60,   56,
    0,   58,   61,    0,   54,   52,    0,   53,    0,   55,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   58,    0,   60,   56,    0,    0,   61,  112,   54,   52,
    0,   53,    0,   55,   62,    0,    0,   59,    0,    0,
    0,   60,   56,    0,   58,   61,  134,   54,   52,    0,
   53,    0,   55,   62,    0,    0,   59,    0,    0,    0,
    0,    0,    0,   58,    0,    0,    0,    0,    0,   51,
    0,    0,   60,   56,    0,    0,   61,   62,   54,   52,
   59,   53,    0,   55,    0,    0,    0,    0,   51,    0,
    0,    0,    0,    0,   58,    0,   62,    0,    0,   59,
    0,    0,    0,    0,    0,    0,    9,    9,    9,    0,
    0,    0,   51,    0,    0,    0,    0,    9,    9,    9,
    9,    9,    9,    9,    9,    0,    0,   62,    0,    9,
   59,   51,    0,    0,    0,   40,   41,   42,    0,    0,
    0,    0,    0,    0,    0,    0,   43,   44,   45,   46,
   47,   48,   49,   50,    0,    0,    0,    0,   57,    0,
    0,    0,   51,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   60,   56,    0,    0,
   61,    0,   54,   52,    0,   53,    0,   55,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   58,    0,
    0,    0,    0,   21,    0,    0,    0,   40,   41,   42,
   18,    0,    0,   16,    0,   17,    0,    0,   43,   44,
   45,   46,   47,   48,   49,   50,   40,   41,   42,    0,
   57,   62,  111,   14,   59,    0,    0,   43,   44,   45,
   46,   47,   48,   49,   50,    0,   21,    0,    0,   57,
   40,   41,   42,   18,    0,    0,   16,    0,   17,    0,
    0,   43,   44,   45,   46,   47,   48,   49,   50,   40,
   41,   42,    0,   57,    0,    0,   14,    0,    0,    0,
   43,   44,   45,   46,   47,   48,   49,   50,    0,    0,
    0,    0,   57,   20,    0,    0,   15,    0,    0,    0,
   40,   41,   42,    0,    0,    0,    0,    0,    0,    0,
    0,   43,   44,   45,   46,   47,   48,   49,   50,    0,
   60,   56,    0,   57,   61,    0,   54,   52,    0,   53,
    0,   55,    0,    0,    0,    0,   20,   60,   56,   15,
    0,   61,   58,   54,   52,    0,   53,    0,   55,    0,
    0,    0,    0,    0,   60,   56,    0,    0,   61,   58,
   54,   52,    0,   53,    0,   55,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   62,   58,    0,   59,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   62,    0,    0,   59,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   42,    0,    0,   62,
   51,    0,   59,    0,    0,   43,   44,   45,   46,   47,
   48,   49,   50,    0,    0,    0,    0,   57,    0,    3,
    4,    5,    6,    7,    0,    0,    0,    8,    0,    9,
    0,   10,   11,   12,   13,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   79,    4,   80,   81,    7,    0,    0,    0,
    8,    0,    9,    0,   10,   11,   12,   13,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   42,    0,    0,    0,    0,    0,    0,    0,    0,   43,
   44,   45,   46,   47,   48,   49,   50,   42,    0,    0,
    0,   57,    0,    0,    0,    0,   43,   44,   45,   46,
   47,   48,   49,    0,   42,    0,   23,    0,   57,    0,
    0,    0,    0,   43,   44,   45,   46,   47,   48,   31,
   32,   33,   34,   35,   36,   57,   37,   39,    0,    0,
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
   40,  259,   40,   36,  262,   36,   37,   40,   40,   40,
   44,   42,   43,   40,   45,  262,   47,   41,   10,   41,
   44,  262,   44,  268,   10,   36,   37,   58,   10,   40,
   93,   42,   43,   61,   45,   41,   47,   10,   44,   41,
   61,   41,   44,   41,   44,   37,   44,   58,   59,   41,
   42,   43,   44,   45,   61,   47,   61,   10,   91,   93,
   91,   94,   41,   94,   37,   44,   58,   59,   41,   42,
   43,   44,   45,   59,   47,   41,   10,   59,   44,   41,
   91,   41,   44,   94,   37,   58,   59,   93,   41,   42,
   43,   44,   45,   10,   47,   10,   61,   62,   93,  262,
   93,   93,   44,   37,   61,   58,   59,   41,   42,   43,
   44,   45,   44,   47,   10,  126,  107,  131,   -1,   10,
   93,   -1,   37,   -1,   58,   59,   41,   42,   43,   44,
   45,   93,   47,  125,  126,   10,   10,   -1,   -1,  125,
   93,   37,   59,  125,   59,   41,   42,   43,   44,   45,
   -1,   47,  125,  126,   -1,   -1,   -1,   36,   36,   93,
   10,   40,   40,   59,   -1,   10,   41,   41,   59,   44,
   44,   41,  125,  126,   44,   -1,   -1,   -1,   93,   58,
   58,   -1,   36,   37,   59,   59,   40,   -1,   42,   -1,
   -1,  125,  126,   47,   -1,   10,   41,   93,   10,   44,
   -1,   -1,   -1,   -1,   58,   -1,   -1,   -1,  125,   59,
  125,  126,   91,   91,   59,   94,   94,   -1,   93,   93,
   -1,   -1,   41,   93,   -1,   44,   -1,   -1,   -1,  125,
  126,   -1,   -1,  266,  125,  266,   -1,   91,   -1,   -1,
   94,   -1,   41,   -1,   59,   44,   41,   59,   93,   44,
  125,  125,  126,  264,  265,  266,  287,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,  264,  265,   93,  125,  287,   -1,  270,   -1,
  125,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,   -1,   93,  287,   -1,  270,   93,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
  125,  264,  265,  125,  287,   -1,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
  264,  265,   -1,   -1,  287,   -1,  270,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,  264,
  265,   -1,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,  264,  265,
   10,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,  275,
  276,  277,  278,  279,  280,  281,  282,  266,  266,   10,
  264,  265,   -1,   -1,   -1,  270,  270,   37,   10,   -1,
   -1,   41,   42,   43,   44,   45,   10,   47,   10,  287,
   -1,   41,  266,   -1,   44,   -1,   37,   -1,   -1,   59,
   41,   42,   43,   44,   45,  270,   47,   10,   -1,   41,
   -1,   41,   44,  287,   44,   -1,   -1,   41,   59,   41,
   44,   43,   44,   45,   10,   -1,   10,   59,   10,   -1,
   -1,   -1,   -1,   93,   -1,   59,   -1,   59,   41,   -1,
   43,   44,   45,   93,   -1,   10,   -1,   10,   -1,   -1,
   -1,   -1,   93,   -1,   -1,   41,   59,   41,   44,   41,
   44,   93,   44,   93,   10,  125,  126,   -1,   -1,   93,
   -1,   93,   -1,   59,   -1,   59,   41,   59,   41,   44,
   -1,   44,   -1,   10,  125,  126,   -1,   10,   -1,   -1,
   93,   -1,   -1,  125,   59,   41,   59,   -1,   44,   -1,
   -1,  125,   -1,  125,  126,   -1,   -1,   93,   -1,   93,
   -1,   93,   -1,   59,   41,   -1,   -1,   44,   41,   -1,
   -1,   44,  125,  126,   -1,   -1,   -1,   -1,   93,   -1,
   93,   -1,   59,   -1,   -1,   -1,   59,   -1,   -1,  125,
   -1,  125,  126,  125,  126,   -1,   -1,   93,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  125,  126,  125,   -1,   -1,   -1,   93,   -1,   -1,   -1,
   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,  126,
   -1,   -1,  125,   -1,  264,  265,   -1,   -1,   -1,   -1,
  270,   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,  264,  265,   -1,   -1,   -1,   -1,  270,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,  264,  265,   -1,   -1,  270,   -1,  270,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,   -1,   -1,   -1,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
  264,  265,  264,  265,  270,   -1,  270,   -1,  270,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,  264,
  265,  264,  265,   -1,   -1,  270,   -1,  270,   10,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,  264,  265,
   -1,   -1,   -1,   -1,  270,   -1,   -1,   10,   -1,  275,
  276,  277,  278,  279,  280,  281,  282,  264,  265,   41,
   -1,   -1,   44,  270,   10,   -1,   10,  270,  275,  276,
  277,  278,  279,  280,  281,  282,   10,   59,   41,   -1,
   -1,   44,   -1,   10,   -1,   10,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   41,   59,   41,   44,   -1,
   44,   -1,   -1,   10,   -1,   -1,   -1,   41,   -1,   -1,
   44,   93,   -1,   59,   41,   59,   41,   44,   -1,   44,
   -1,   -1,   -1,   -1,   -1,   59,   33,   -1,   -1,   -1,
   93,   -1,   59,   40,   59,   -1,   43,   -1,   45,   -1,
   -1,   -1,   -1,  125,  126,   -1,   -1,   93,   -1,   93,
   -1,   -1,   -1,   -1,   -1,   -1,   63,   -1,   -1,   93,
   -1,   -1,  125,  126,   -1,   -1,   93,   -1,   93,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
   -1,  125,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  125,  126,   -1,   -1,   -1,   -1,   -1,  125,   -1,
  125,  126,   36,   37,   -1,   -1,   40,   41,   42,   43,
   44,   45,   -1,   47,   -1,   -1,  123,   -1,   -1,  126,
   -1,   36,   37,   -1,   58,   40,   41,   42,   43,   44,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   -1,   93,
   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   91,   -1,   93,   94,
   -1,   -1,  264,  265,   -1,   -1,   -1,   -1,  270,   -1,
   -1,   -1,  126,  275,  276,  277,  278,  279,  280,  281,
  282,  264,  265,   -1,   -1,   -1,   -1,  270,   -1,   -1,
   -1,  126,  275,  276,  277,  278,  279,  280,  281,  282,
  264,  265,   -1,   -1,  270,   -1,  270,   -1,   -1,   -1,
  264,  265,   -1,   -1,   -1,   -1,  270,  281,  282,  264,
  265,   -1,   -1,  270,   -1,  270,   -1,  281,  282,  256,
  257,   -1,  259,  260,  261,  262,  263,  282,   -1,   -1,
  267,   -1,  269,   -1,  271,  272,  273,  274,   36,   37,
   -1,   -1,   40,   41,   42,   43,   44,   45,   -1,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   -1,   -1,   -1,   -1,   -1,   -1,   36,   37,   -1,
   -1,   40,   41,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,
   -1,   -1,   -1,   91,   -1,   93,   94,   -1,   -1,   -1,
  264,  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,  264,
  265,  266,   91,  287,   -1,   94,   -1,   -1,  126,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   -1,   36,
   37,   -1,  287,   40,   41,   42,   43,   -1,   45,   -1,
   47,   -1,   -1,   -1,   -1,   -1,   -1,  126,   36,   37,
   -1,   58,   40,   -1,   42,   43,   -1,   45,   -1,   47,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   58,   -1,   36,   37,   -1,   -1,   40,   41,   42,   43,
   -1,   45,   -1,   47,   91,   -1,   -1,   94,   -1,   -1,
   -1,   36,   37,   -1,   58,   40,   41,   42,   43,   -1,
   45,   -1,   47,   91,   -1,   -1,   94,   -1,   -1,   -1,
   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,   -1,  126,
   -1,   -1,   36,   37,   -1,   -1,   40,   91,   42,   43,
   94,   45,   -1,   47,   -1,   -1,   -1,   -1,  126,   -1,
   -1,   -1,   -1,   -1,   58,   -1,   91,   -1,   -1,   94,
   -1,   -1,   -1,   -1,   -1,   -1,  264,  265,  266,   -1,
   -1,   -1,  126,   -1,   -1,   -1,   -1,  275,  276,  277,
  278,  279,  280,  281,  282,   -1,   -1,   91,   -1,  287,
   94,  126,   -1,   -1,   -1,  264,  265,  266,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  275,  276,  277,  278,
  279,  280,  281,  282,   -1,   -1,   -1,   -1,  287,   -1,
   -1,   -1,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   36,   37,   -1,   -1,
   40,   -1,   42,   43,   -1,   45,   -1,   47,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   -1,
   -1,   -1,   -1,   33,   -1,   -1,   -1,  264,  265,  266,
   40,   -1,   -1,   43,   -1,   45,   -1,   -1,  275,  276,
  277,  278,  279,  280,  281,  282,  264,  265,  266,   -1,
  287,   91,  270,   63,   94,   -1,   -1,  275,  276,  277,
  278,  279,  280,  281,  282,   -1,   33,   -1,   -1,  287,
  264,  265,  266,   40,   -1,   -1,   43,   -1,   45,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,  264,
  265,  266,   -1,  287,   -1,   -1,   63,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   -1,   -1,
   -1,   -1,  287,  123,   -1,   -1,  126,   -1,   -1,   -1,
  264,  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,   -1,
   36,   37,   -1,  287,   40,   -1,   42,   43,   -1,   45,
   -1,   47,   -1,   -1,   -1,   -1,  123,   36,   37,  126,
   -1,   40,   58,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   36,   37,   -1,   -1,   40,   58,
   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   58,   -1,   94,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   91,   -1,   -1,   94,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,  266,   -1,   -1,   91,
  126,   -1,   94,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,   -1,   -1,   -1,   -1,  287,   -1,  259,
  260,  261,  262,  263,   -1,   -1,   -1,  267,   -1,  269,
   -1,  271,  272,  273,  274,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  259,  260,  261,  262,  263,   -1,   -1,   -1,
  267,   -1,  269,   -1,  271,  272,  273,  274,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  275,
  276,  277,  278,  279,  280,  281,  282,  266,   -1,   -1,
   -1,  287,   -1,   -1,   -1,   -1,  275,  276,  277,  278,
  279,  280,  281,   -1,  266,   -1,    0,   -1,  287,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,   13,
   14,   15,   16,   17,   18,  287,   20,   21,   -1,   -1,
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
#line 239 "gram.y"


/*----------------------------------------------------------------------------*/

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
    AddComment(expr);
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
    AddComment(expr);
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
	    expr = install(CHAR(STRING(expr)[0])); 
	PROTECT(expr);
	if (length(CDR(args)) == 1 && CADR(args) == R_MissingArg)
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
    AddComment(body);
    if (GenerateCode)
	PROTECT(ans = lang3(fname, CDR(formals), body));
    else
	PROTECT(ans = R_NilValue);
    PopComment();
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(formals);
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
	TYPEOF(a2) = LANGSXP;
	CAR(a2) = a1;
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
    case NILSXP:
    case SYMSXP:
    case STRSXP:
	return lang2(arg, tag);
    default:
	error("incorrect tag type\n"); return R_NilValue/* -Wall */;
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
    CAR(s) = s;
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
    CAR(l) = tmp;
    return l;
}

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

#ifdef NOT_used
int IsComment(SEXP l)
{
    if (isList(l) && isString(CAR(l))
	&& !strncmp(CHAR(STRING(CAR(l))[0]), "#", 1))
	return 1;
    else
	return 0;
}
#endif

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
    PROTECT(tmp = NewList());
    tmp = GrowList(tmp, s);
    TAG(CAR(tmp)) = tag;
    UNPROTECT(3);
    return tmp;
}

static SEXP NextArg(SEXP l, SEXP s, SEXP tag)
{
    PROTECT(tag);
    PROTECT(l);
    l = GrowList(l, s);
    TAG(CAR(l)) = tag;
    UNPROTECT(2);
    return l;
}

/*--------------------------------------------------------------------------*/

/* Basic File IO : This code is here because at this particular instant */
/* it seems closely related to cget(), which appears below.  But now it */
/* doesn't.  Move this to iosupport.c or trash it */

int R_fgetc(FILE *fp)
{
    int c = fgetc(fp);
    /* get rid of  CR in CRLF line termination */
    if (c == '\r') {
	c = fgetc(fp);
	/* retain CR's with no following linefeed */
	if (c != '\n') {
	    ungetc(c,fp);
	    return('\r');
	}
    }
    return feof(fp) ? R_EOF : c;
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
 *	SEXP R_Parse1File(FILE *fp, int gencode, int *status)
 *
 *	SEXP R_Parse1Vector(TextBuffer *text, int gencode, int *status)
 *
 *	SEXP R_Parse1Buffer(IOBuffer *buffer, int gencode, int *status)
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
 *	SEXP R_ParseFile(FILE *fp, int n, int *status)
 *
 *	SEXP R_ParseVector(TextBuffer *text, int n, int *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int n, int *status)
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
    ResetComment();
}

static int file_getc(void)
{
    int c = R_fgetc(R_Inputfile);
    if (c == EOF) {
	EndOfFile = 1;
	return R_EOF;
    }
    if (c == '\n') R_ParseError += 1;
    return c;
}

static int file_ungetc(int c)
{
    if (c == '\n') R_ParseError -= 1;
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
	if (EndOfFile) *status = PARSE_INCOMPLETE;
	break;
    case 2:			/* Empty Line */
	*status = PARSE_NULL;
	break;
    case 3:			/* Valid expr '\n' terminated */
    case 4:			/* Valid expr ';' terminated */
	*status = PARSE_OK;
	break;
    }
    R_Inputfile = NULL;
    return R_CurrentExpr;
}

static IoBuffer *iob;

static int buffer_getc()
{
    int c = R_IoBufferGetc(iob);
    if (c == EOF) {
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
    if (c == EOF) {
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
    if (n >= 0) {
	PROTECT(rval = allocVector(EXPRSXP, n));
	for (i = 0 ; i < n ; i++) {
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
		for (n = 0 ; n < LENGTH(rval) ; n++) {
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
    if (n >= 0) {
	PROTECT(rval = allocVector(EXPRSXP, n));
	for (i = 0 ; i < n ; i++) {
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
		for (n = 0 ; n < LENGTH(rval) ; n++) {
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
    if (n >= 0) {
	PROTECT(rval = allocVector(EXPRSXP, n));
	for (i = 0 ; i < n ; i++) {
	try_again:
	    if(!*bufp) {
		if(R_ReadConsole(Prompt(prompt, prompt_type),
				 buf, 1024, 1) == 0) return R_NilValue;
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
	for (;;) {
	    if (!*bufp) {
		if(R_ReadConsole(Prompt(prompt, prompt_type),
				 buf, 1024, 1) == 0) return R_NilValue;
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

void yyerror(char *s)
{
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
    while (formlist != R_NilValue) {
	if (TAG(formlist) == new) {
	    error("Repeated formal argument.\n");
	}
	formlist = CDR(formlist);
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
    if (GenerateCode && R_CommentSxp != R_NilValue) {
	f = mkChar(yytext);
	f = CONS(f, R_NilValue);
	CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
    }
    if (c == R_EOF) EndOfFile = 2;
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

/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */

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

/* return 1 if name is a valid name 0 otherwise */
int isValidName(char *name)
{
    char *p;
    int c, i;

    p = name;
    c = *p++;

    if( c != '.' && !isalpha(c) )
        return 0;

    for (i = 0; keywords[i].name; i++)
        if (strcmp(keywords[i].name, name) == 0 && !(strcmp(name, "...")==0))
                return 0;

    if (c == '.' ) {
	if( strlen(name)==1 )
	    return 1;
        while ( c = *p++ )
	    if( !isdigit(c) ) {
		if( !isalpha(c) )
		    return 0;
		else
		    break;
	    };
	if( c == '\0' )
 	    return 0;
    }
    while ( c = *p++, (isalnum(c) || c=='.') );
    if (c == '\0')
        return 1;
    else
        return 0;
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
    if (SavedToken) {
	c = SavedToken;
	yylval = SavedLval;
	SavedLval = R_NilValue;
	SavedToken = 0;
	return c;
    }
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

 symbol:

    if (c == '.' || isalpha(c))
	return SymbolValue(c);

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
	while (*contextp == 'i')
	    ifpop();
	*contextp-- = 0;
	EatLines = 0;
	break;

    case RBRACE:
	while (*contextp == 'i')
	    ifpop();
	if(*contextp == LBRACE)
	    PopComment();
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
#line 2476 "y.tab.c"
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
#line 139 "gram.y"
{ return 0; }
break;
case 2:
#line 140 "gram.y"
{ return xxvalue(NULL,2); }
break;
case 3:
#line 141 "gram.y"
{ return xxvalue(yyvsp[-1],3); }
break;
case 4:
#line 142 "gram.y"
{ return xxvalue(yyvsp[-1],4); }
break;
case 5:
#line 143 "gram.y"
{ YYABORT; }
break;
case 6:
#line 146 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 7:
#line 147 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 8:
#line 148 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 9:
#line 149 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 10:
#line 151 "gram.y"
{ yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); }
break;
case 11:
#line 152 "gram.y"
{ yyval = xxparen(yyvsp[-2],yyvsp[-1]); }
break;
case 12:
#line 154 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 13:
#line 155 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 14:
#line 156 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 15:
#line 157 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 16:
#line 158 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 17:
#line 160 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 18:
#line 161 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 19:
#line 162 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 20:
#line 163 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 21:
#line 164 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 22:
#line 165 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 23:
#line 166 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 24:
#line 167 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 25:
#line 168 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 26:
#line 169 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 27:
#line 170 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 28:
#line 171 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 29:
#line 172 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 30:
#line 173 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 31:
#line 174 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 32:
#line 175 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 33:
#line 176 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 34:
#line 178 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 35:
#line 179 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); }
break;
case 36:
#line 181 "gram.y"
{ yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); }
break;
case 37:
#line 182 "gram.y"
{ yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); }
break;
case 38:
#line 183 "gram.y"
{ yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 39:
#line 184 "gram.y"
{ yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); }
break;
case 40:
#line 185 "gram.y"
{ yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 41:
#line 186 "gram.y"
{ yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 42:
#line 187 "gram.y"
{ yyval = xxrepeat(yyvsp[-1],yyvsp[0]); }
break;
case 43:
#line 188 "gram.y"
{ yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); }
break;
case 44:
#line 189 "gram.y"
{ yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); }
break;
case 45:
#line 190 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 46:
#line 191 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 47:
#line 192 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 48:
#line 193 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 49:
#line 197 "gram.y"
{ yyval = xxcond(yyvsp[-1]); }
break;
case 50:
#line 200 "gram.y"
{ yyval = xxifcond(yyvsp[-1]); }
break;
case 51:
#line 203 "gram.y"
{ yyval = xxforcond(yyvsp[-3],yyvsp[-1]); }
break;
case 52:
#line 207 "gram.y"
{ yyval = xxexprlist0(); }
break;
case 53:
#line 208 "gram.y"
{ yyval = xxexprlist1(yyvsp[0]); }
break;
case 54:
#line 209 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 55:
#line 210 "gram.y"
{ yyval = yyvsp[-1]; AddComment(CAR(yyval));}
break;
case 56:
#line 211 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 57:
#line 212 "gram.y"
{ yyval = yyvsp[-1];}
break;
case 58:
#line 215 "gram.y"
{ yyval = xxsublist1(yyvsp[0]); }
break;
case 59:
#line 216 "gram.y"
{ yyval = xxsublist2(yyvsp[-3],yyvsp[0]); }
break;
case 60:
#line 219 "gram.y"
{ yyval = xxsub0(); }
break;
case 61:
#line 220 "gram.y"
{ yyval = xxsub1(yyvsp[0]); }
break;
case 62:
#line 221 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 63:
#line 222 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 64:
#line 223 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 65:
#line 224 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 66:
#line 225 "gram.y"
{ yyval = xxnullsub0(); }
break;
case 67:
#line 226 "gram.y"
{ yyval = xxnullsub1(yyvsp[0]); }
break;
case 68:
#line 229 "gram.y"
{ yyval = xxnullformal(); }
break;
case 69:
#line 230 "gram.y"
{ yyval = xxfirstformal0(yyvsp[0]); }
break;
case 70:
#line 231 "gram.y"
{ yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); }
break;
case 71:
#line 232 "gram.y"
{ yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); }
break;
case 72:
#line 233 "gram.y"
{ yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); }
break;
case 73:
#line 236 "gram.y"
{ EatLines = 1; }
break;
#line 2909 "y.tab.c"
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
