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

extern SEXP R_CommentSxp;

/* static */ void ResetComment(void);
static void AddComment(SEXP);
static void PushComment(void);
static void PopComment(void);
static int isComment(SEXP);
static void ifpush(void);
static void CheckFormalArgs(SEXP, SEXP);
static int KeywordLookup(char*);

SEXP listAppend(SEXP,SEXP);
SEXP newlist(void);
SEXP growlist(SEXP, SEXP);
SEXP firstarg(SEXP, SEXP);
SEXP nextarg(SEXP, SEXP, SEXP);
SEXP tagarg(SEXP, SEXP);

		/* These routines allocate constants */

SEXP mkString(char *);
SEXP mkInteger(char *);
SEXP mkFloat(char *);
SEXP mkComplex(char *);
SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);

		/* Internal lexer / parser state variables */

static int EatLines;
static int GenerateCode = 0;
static int EndOfFile = 0;
static FILE *InputFile = NULL;
static int (*xxgetc)();
static void (*xxungetc)();

static int newline = 0;			/* Used only for prompting */


	/* Soon to be defunct entry points */

void	R_SetInput(int);
int	R_fgetc(FILE*);
void	uncget(void);

void	yyinit(void);
int	yylex(void);
int	yyerror(char*);
void	yyprompt(char *, ...);
int	yywrap();

	/* Routines used to build the parse tree */

static SEXP xxnullformal(void);
static SEXP xxfirstformal0(SEXP);
static SEXP xxfirstformal1(SEXP, SEXP);
static SEXP xxaddformal0(SEXP, SEXP);
static SEXP xxaddformal1(SEXP, SEXP, SEXP);
static SEXP xxexprlist0();
static SEXP xxexprlist1(SEXP);
static SEXP xxexprlist2(SEXP, SEXP);
static SEXP xxsub0(void);
static SEXP xxsub1(SEXP);
static SEXP xxsymsub0(SEXP);
static SEXP xxsymsub1(SEXP, SEXP);
static SEXP xxnullsub0();
static SEXP xxnullsub1(SEXP);
static SEXP xxsublist1(SEXP);
static SEXP xxsublist2(SEXP, SEXP);
static SEXP xxcond(SEXP);
static SEXP xxifcond(SEXP);
static SEXP xxif(SEXP, SEXP, SEXP);
static SEXP xxifelse(SEXP, SEXP, SEXP, SEXP);
static SEXP xxforcond(SEXP, SEXP);
static SEXP xxfor(SEXP, SEXP, SEXP);
static SEXP xxwhile(SEXP, SEXP, SEXP);
static SEXP xxrepeat(SEXP, SEXP);
static SEXP xxnxtbrk(SEXP);
static SEXP xxfuncall(SEXP, SEXP);
static SEXP xxdefun(SEXP, SEXP, SEXP);
static SEXP xxunary(SEXP, SEXP);
static SEXP xxbinary(SEXP, SEXP, SEXP);
static SEXP xxparen(SEXP, SEXP);
static SEXP xxsubscript(SEXP, SEXP, SEXP);
static SEXP xxexprlist(SEXP, SEXP);
static int xxvalue(SEXP, int);

#define YYSTYPE		SEXP

#line 124 "y.tab.c"
#define END_OF_INPUT 257
#define STR_CONST 258
#define NUM_CONST 259
#define NULL_CONST 260
#define SYMBOL 261
#define FUNCTION 262
#define LEX_ERROR 263
#define LBB 264
#define ERROR 265
#define LEFT_ASSIGN 266
#define RIGHT_ASSIGN 267
#define FOR 268
#define IN 269
#define IF 270
#define ELSE 271
#define WHILE 272
#define NEXT 273
#define BREAK 274
#define REPEAT 275
#define GT 276
#define GE 277
#define LT 278
#define LE 279
#define EQ 280
#define NE 281
#define AND 282
#define OR 283
#define LOW 284
#define TILDE 285
#define UNOT 286
#define NOT 287
#define SPECIAL 288
#define UMINUS 289
#define UPLUS 290
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
    0,   58,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   46,   45,    0,    0,    0,   73,    0,    0,   50,
    0,   49,    0,    0,    0,    0,    0,    0,    0,   37,
   44,    0,    0,    0,    0,    0,    0,    0,    0,   43,
    0,    0,    0,   51,   59,    0,
};
short yydgoto[] = {                                      22,
   80,   38,   66,  119,   81,   28,   26,   30,   82,
};
short yysindex[] = {                                    613,
    0,    0,    0,    0,    0,    0,  -39,  -37,  -31,  -26,
    0,    0, 1447, 1447, 1447, 1447, 1447, 1447,    0, 1447,
 1447,    0,  -10, -245, -243, 1447, 1447, 1447, 1447, 1447,
 1305, 1305, 1196,  -32,  -32,  947, 1305,   19, 1392, 1473,
 1447, 1447, 1447, 1447, 1447, 1447, 1447, 1447, 1447, 1447,
 1447, 1447, 1447, 1447, 1447, 1447, 1447, 1447, 1447, -256,
 1473, 1473,    0,    0,  -27,  -21, -247, 1305,  989, 1018,
 1056, 1305,    0, 1447, 1447,    0,  -20,   -6,   -4, 1305,
  -69,    0, 1305, 1328,  -30,  -30,  -30,  -30,  -30,  -30,
 1392, 1355, 1196, 1411, 1411,  125,  125,  125,  -15,  -32,
  -32,    0,    0,   -1,  -62, 1447,    0, -187, 1447,    0,
 1447,    0, 1305, 1305, 1447, 1447, 1447,  -11,   55,    0,
    0, 1305, 1447,   42, 1267, 1305, 1305, 1305, 1305,    0,
 1473, 1305, 1447,    0,    0, 1305,
};
short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   84,
    0,    0,    0,    1,    0,    0,    0,    0,    0,    0,
  122,  128,  649,    9,   28,    0,  103,    0,  350,  -33,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    3,  -33,    0,    0,   22,    0,    0,  509,    0,  411,
    0,  751,    0,  108,  112,    0,  860,  885,  919,   -5,
   61,    0,  752,  679,  439,  446,  448,  474,  493,  499,
  519,  569,  714,  401,  420,  105,  158,  360,   86,   48,
   67,    0,    0,   61,   61,    0,    0,    0,    0,    0,
    0,    0,  127,  130,   95,  114,  115,    0,    0,    0,
    0,   39,    0,   56,    0,  753,  134,  136,  150,    0,
  169,  773,    0,    0,    0,   57,
};
short yygindex[] = {                                      0,
 1736,    0,    0,   10,   58,    0,    0,    0,   -7,
};
#define YYTABLESIZE 1869
short yytable[] = {                                      63,
   24,  102,   25,   60,  103,   60,   56,   61,   27,   61,
   60,   54,   52,   29,   53,   65,   55,   67,   13,  107,
   60,  109,  108,  118,   61,   60,   56,   58,   74,   61,
  121,   54,   52,  106,   53,   61,   55,   12,   61,  120,
  115,   68,   58,   60,   68,   13,   60,   58,   64,   13,
   13,   13,   13,   13,  116,   13,  117,   17,   62,   60,
   62,   59,   69,   59,   12,   69,   13,   13,   12,   12,
   12,   12,   12,  124,   12,   62,   22,   75,   59,   70,
   62,  130,   70,   59,   17,   12,   12,   61,   17,   17,
   17,   17,   17,   52,   17,   23,   71,   72,  131,   71,
   72,   13,  133,   22,   73,   17,   17,   22,   22,   22,
   22,   22,   53,   22,   20,   51,  123,   57,  104,  105,
   12,   55,   23,  135,   22,   22,   23,   23,   23,   23,
   23,   42,   23,   13,   13,   64,   56,   16,   64,   54,
   17,   20,   52,   76,   23,   20,   20,   20,   20,   20,
    0,   20,   12,   12,   66,   62,    0,   66,   62,   22,
   60,   53,   42,   20,   61,   42,   57,   21,   16,    0,
   55,   16,   17,   17,   65,    0,   67,   65,   23,   67,
   42,    0,   58,    0,    0,   56,   16,   64,   54,    0,
   63,   22,   22,   63,   21,    0,    0,   20,   21,   21,
   21,   21,   21,    0,   21,    0,   66,   62,   52,   60,
   23,   23,   60,    0,   42,   62,   21,    0,   59,    0,
   16,    0,    0,    0,    0,    0,   65,   53,   67,   20,
   20,   40,   57,   40,    0,    0,   55,    0,    0,    0,
    0,    0,   63,    0,    0,    0,   42,    0,   40,    0,
   21,   56,   16,   40,   54,   41,   42,   57,    0,    0,
    0,   60,    0,    0,    0,   43,   44,   45,   46,   47,
   48,   49,   50,    0,   13,   13,    0,   57,    0,   13,
    0,    0,   21,   21,   13,   13,   13,   13,   13,   13,
   13,   13,    0,   12,   12,    0,   13,    0,   12,    0,
    0,    0,    0,   12,   12,   12,   12,   12,   12,   12,
   12,    0,    0,   17,   17,   12,    0,    0,   17,    0,
    0,    0,    0,   17,   17,   17,   17,   17,   17,   17,
   17,    0,   22,   22,    0,   17,    0,   22,    0,    0,
    0,    0,   22,   22,   22,   22,   22,   22,   22,   22,
    0,   23,   23,    0,   22,    0,   23,    0,    0,   14,
    0,   23,   23,   23,   23,   23,   23,   23,   23,   24,
   20,   20,    0,   23,    0,   20,    0,    0,    0,    0,
   20,   20,   20,   20,   20,   20,   20,   20,   40,    0,
   14,    0,   42,   14,    0,    0,   24,    0,   16,    0,
   24,   24,   24,   24,   24,    0,   24,    0,   14,    0,
   18,    0,   57,    0,    0,    0,    0,    0,   24,    0,
   38,    0,    0,   21,   21,    0,    0,    0,   21,   19,
    0,    0,    0,   21,   21,   21,   21,   21,   21,   21,
   21,   18,   14,   18,   18,   18,    0,    0,   31,    0,
    0,   38,   24,    0,   38,   30,    0,   26,    0,   18,
   19,    0,   19,   19,   19,    0,    0,    0,    0,   38,
    0,    0,    0,    0,   14,   14,    0,    0,   19,   31,
    0,    0,   31,   27,   24,   24,   30,    0,   26,   30,
    0,   26,    0,   18,    0,    0,    0,   31,    0,    0,
    0,    0,   28,   38,   30,    0,   26,    0,   29,    0,
    0,    0,   19,    0,   27,    0,    0,   27,   40,    0,
    0,    0,    0,    0,    0,   18,   18,    0,   32,    0,
    0,   31,   27,   28,    0,   38,   28,    0,   30,   29,
   26,    0,   29,    0,   19,   19,    0,    0,    0,   40,
    0,   28,   40,    0,    0,    0,    0,   29,    0,   32,
    0,    0,   32,   31,   31,    0,   27,   40,    0,    0,
   30,   30,   26,   26,    0,    0,    0,   32,   33,    0,
    0,    0,    0,    0,    0,   28,    0,    0,    0,    0,
    0,   29,    0,    0,    0,    0,    0,    0,   27,   27,
    0,   40,    0,    0,    0,    0,    0,    0,    0,   33,
    0,   32,   33,    0,    0,   14,   14,   28,   28,    0,
   14,    0,   19,   29,   29,   24,   24,   33,    0,    0,
   24,   14,   14,   40,    0,   24,   24,   24,   24,   24,
   24,   24,   24,   32,   32,   21,    0,    0,    0,    0,
    0,    0,   18,    0,    0,   16,    0,   17,   15,    0,
    0,   33,    0,    0,    0,    0,   18,   18,    0,    0,
    0,   18,    0,    0,    0,   14,   18,   18,   18,   18,
   18,   18,   18,   18,    0,   19,   19,    0,   35,   15,
   19,    0,   15,   33,   33,   19,   19,   19,   19,   19,
   19,   19,   19,    0,   31,   31,    0,   15,    0,   31,
    0,   30,   30,   26,   26,    0,   30,    0,   26,   35,
   31,   31,   35,   25,    0,    0,    0,   30,   30,   26,
   26,    0,    0,    0,    0,   20,    0,   35,   15,   27,
   27,   15,    0,    0,   27,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   25,   27,   27,   25,   28,   28,
   41,   34,   39,   28,   29,   29,    0,    0,    0,   29,
    0,   35,   25,   15,   28,   28,    0,    0,    0,   40,
   29,   29,   36,    0,   32,   32,    0,    0,    0,   32,
    0,   41,   34,   39,   41,   34,   39,    0,    0,    0,
   32,   32,    0,   35,    0,    0,   25,    0,    0,   41,
   34,   39,    0,   36,    0,    0,   36,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   36,    0,    0,   33,   33,    0,    0,   25,   33,
    0,    0,    0,   41,   34,   39,    0,    0,    0,    0,
    0,   33,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   36,    0,    0,    1,    2,
    3,    4,    5,    6,    7,   41,   34,   39,    0,    0,
    8,    0,    9,    0,   10,   11,   12,   13,    0,    0,
    0,    0,    0,    0,    0,    7,    7,   36,    0,    7,
    7,    7,    7,    7,    7,    0,    7,    0,    0,    0,
    0,    0,    0,    0,   15,   15,    0,    7,    0,   15,
    8,    8,    0,    0,    8,    8,    8,    8,    8,    8,
    0,    8,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    8,    0,   35,   35,    0,    0,    0,   35,
    7,    0,    7,    7,    9,    9,    0,    0,    9,    9,
    9,    9,    9,    9,    0,    9,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    8,    9,    8,    8,   25,
   25,    0,   60,   56,   25,    7,   61,   73,   54,   52,
    0,   53,    0,   55,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,   58,    0,    0,    0,    0,    9,
    8,    9,    9,    0,    0,    0,    0,    0,    0,    0,
    0,   41,   34,   39,   60,   56,    0,    0,   61,  110,
   54,   52,    0,   53,    0,   55,    0,   62,    0,    0,
   59,    0,    0,   36,    9,    0,   58,    0,    0,    0,
    0,    0,    0,   60,   56,    0,    0,   61,    0,   54,
   52,    0,   53,    0,   55,    0,    0,    0,    0,    0,
    0,    0,   51,    0,    0,   58,    0,    0,    0,   62,
    0,    0,   59,    0,    0,    0,    0,    0,    0,    0,
    0,   60,   56,    0,    0,   61,  112,   54,   52,    0,
   53,    0,   55,    0,    0,    0,    0,    0,   62,    0,
    0,   59,    0,   58,   51,    0,    0,    0,    0,    0,
    0,    0,    0,    7,    0,    7,    7,    0,    0,    0,
    0,    0,    0,    0,    0,    7,    7,    7,    7,    7,
    7,    7,    7,   51,    0,    0,   62,    7,    8,   59,
    8,    8,    0,    0,    0,    0,    0,    0,    0,    0,
    8,    8,    8,    8,    8,    8,    8,    8,    0,    0,
    0,    0,    8,    0,    0,    0,    0,    0,    0,    0,
    0,   51,    9,    0,    9,    9,    0,    0,    0,    0,
    0,    0,    0,    0,    9,    9,    9,    9,    9,    9,
    9,    9,    0,    0,    0,    0,    9,    0,    0,    0,
   40,    0,   41,   42,    0,    0,    0,    0,    0,    0,
    0,    0,   43,   44,   45,   46,   47,   48,   49,   50,
    0,   60,   56,    0,   57,   61,    0,   54,   52,    0,
   53,    0,   55,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   40,   58,   41,   42,    0,    0,    0,    0,
    0,    0,    0,    0,   43,   44,   45,   46,   47,   48,
   49,   50,    0,    0,    0,    0,   57,    0,    0,    0,
    0,   40,    0,   41,   42,    0,   62,    0,  111,   59,
    0,    0,    0,   43,   44,   45,   46,   47,   48,   49,
   50,    0,   60,   56,    0,   57,   61,  134,   54,   52,
    0,   53,    0,   55,    0,    0,    0,    0,    0,   40,
    0,   41,   42,    0,   58,    0,    0,    0,    0,    0,
    0,   43,   44,   45,   46,   47,   48,   49,   50,    0,
   60,   56,    0,   57,   61,    0,   54,   52,    0,   53,
    0,   55,    0,    0,    0,    0,    0,   62,    0,    0,
   59,    0,   58,   60,   56,    0,    0,   61,    0,   54,
   52,    0,   53,    0,   55,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   58,    0,    0,    0,    0,
   60,   56,   51,    0,   61,   62,   54,   52,   59,   53,
    0,   55,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   58,    0,    0,    0,    0,    0,   62,    0,
    0,   59,    0,    0,    0,    0,    0,   60,   56,    0,
   51,   61,    0,   54,   52,    0,   53,    0,   55,    0,
    0,    0,    0,    0,    0,   62,   60,   56,   59,   58,
   61,    0,   54,   51,    0,    0,    0,   55,    0,   40,
    0,    0,    0,    0,    0,    0,    0,    0,   58,    0,
    0,   43,   44,   45,   46,   47,   48,   49,   50,   21,
    0,    0,   62,   57,    0,   59,   18,    0,    0,   16,
    0,   17,    0,    0,    0,    0,    0,    0,    0,    0,
    0,   62,    0,    0,   59,   21,    0,    0,    0,   14,
    0,    0,   18,    0,    0,   16,    0,   17,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   40,    0,   41,   42,    0,   14,    0,    0,    0,    0,
    0,    0,   43,   44,   45,   46,   47,   48,   49,   50,
    0,    0,    0,    0,   57,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   40,   20,
   41,   42,   15,    0,    0,    0,    0,    0,    0,    0,
   43,   44,   45,   46,   47,   48,   49,   50,    0,    0,
    0,   40,   57,    0,    0,   20,    0,    0,   15,    0,
    0,    0,    0,   43,   44,   45,   46,   47,   48,   49,
   50,    0,    0,    0,    0,   57,    0,    0,   40,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   43,   44,   45,   46,   47,   48,   49,    0,    0,    0,
    0,    0,   57,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   40,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,   43,   44,   45,
   46,   47,   48,    0,   40,    0,    0,    0,    0,   57,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   57,    0,
    0,    0,    0,    0,    3,    4,    5,    6,    7,    0,
    0,    0,    0,    0,    8,    0,    9,    0,   10,   11,
   12,   13,    0,    0,    0,    0,    0,    0,    0,    0,
   77,    4,   78,   79,    7,   23,    0,    0,    0,    0,
    8,    0,    9,    0,   10,   11,   12,   13,   31,   32,
   33,   34,   35,   36,    0,   37,   39,    0,    0,    0,
    0,   68,   69,   70,   71,   72,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   83,   84,   85,   86,
   87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
   97,   98,   99,  100,  101,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  113,
  114,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  122,    0,    0,  125,    0,  126,    0,    0,    0,
  127,  128,  129,    0,    0,    0,    0,    0,  132,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  136,
};
short yycheck[] = {                                      10,
   40,  258,   40,   36,  261,   36,   37,   40,   40,   40,
   44,   42,   43,   40,   45,  261,   47,  261,   10,   41,
   36,  269,   44,   93,   40,   36,   37,   58,   10,   40,
   93,   42,   43,   61,   45,   41,   47,   10,   44,   41,
   61,   41,   58,   41,   44,   37,   44,   58,   59,   41,
   42,   43,   44,   45,   61,   47,   61,   10,   91,   93,
   91,   94,   41,   94,   37,   44,   58,   59,   41,   42,
   43,   44,   45,  261,   47,   91,   10,   59,   94,   41,
   91,   93,   44,   94,   37,   58,   59,   93,   41,   42,
   43,   44,   45,   10,   47,   10,   41,   41,   44,   44,
   44,   93,   61,   37,   44,   58,   59,   41,   42,   43,
   44,   45,   10,   47,   10,  126,  107,   10,   61,   62,
   93,   10,   37,  131,   58,   59,   41,   42,   43,   44,
   45,   10,   47,  125,  126,   41,   10,   10,   44,   10,
   93,   37,   59,  125,   59,   41,   42,   43,   44,   45,
   -1,   47,  125,  126,   41,   41,   -1,   44,   44,   93,
   36,   59,   41,   59,   40,   44,   59,   10,   41,   -1,
   59,   44,  125,  126,   41,   -1,   41,   44,   93,   44,
   59,   -1,   58,   -1,   -1,   59,   59,   93,   59,   -1,
   41,  125,  126,   44,   37,   -1,   -1,   93,   41,   42,
   43,   44,   45,   -1,   47,   -1,   93,   93,  125,   41,
  125,  126,   44,   -1,   93,   91,   59,   -1,   94,   -1,
   93,   -1,   -1,   -1,   -1,   -1,   93,  125,   93,  125,
  126,  264,  125,  264,   -1,   -1,  125,   -1,   -1,   -1,
   -1,   -1,   93,   -1,   -1,   -1,  125,   -1,  264,   -1,
   93,  125,  125,  264,  125,  266,  267,  288,   -1,   -1,
   -1,   93,   -1,   -1,   -1,  276,  277,  278,  279,  280,
  281,  282,  283,   -1,  266,  267,   -1,  288,   -1,  271,
   -1,   -1,  125,  126,  276,  277,  278,  279,  280,  281,
  282,  283,   -1,  266,  267,   -1,  288,   -1,  271,   -1,
   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,  282,
  283,   -1,   -1,  266,  267,  288,   -1,   -1,  271,   -1,
   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,  282,
  283,   -1,  266,  267,   -1,  288,   -1,  271,   -1,   -1,
   -1,   -1,  276,  277,  278,  279,  280,  281,  282,  283,
   -1,  266,  267,   -1,  288,   -1,  271,   -1,   -1,   10,
   -1,  276,  277,  278,  279,  280,  281,  282,  283,   10,
  266,  267,   -1,  288,   -1,  271,   -1,   -1,   -1,   -1,
  276,  277,  278,  279,  280,  281,  282,  283,  264,   -1,
   41,   -1,  271,   44,   -1,   -1,   37,   -1,  271,   -1,
   41,   42,   43,   44,   45,   -1,   47,   -1,   59,   -1,
   10,   -1,  288,   -1,   -1,   -1,   -1,   -1,   59,   -1,
   10,   -1,   -1,  266,  267,   -1,   -1,   -1,  271,   10,
   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,  282,
  283,   41,   93,   43,   44,   45,   -1,   -1,   10,   -1,
   -1,   41,   93,   -1,   44,   10,   -1,   10,   -1,   59,
   41,   -1,   43,   44,   45,   -1,   -1,   -1,   -1,   59,
   -1,   -1,   -1,   -1,  125,  126,   -1,   -1,   59,   41,
   -1,   -1,   44,   10,  125,  126,   41,   -1,   41,   44,
   -1,   44,   -1,   93,   -1,   -1,   -1,   59,   -1,   -1,
   -1,   -1,   10,   93,   59,   -1,   59,   -1,   10,   -1,
   -1,   -1,   93,   -1,   41,   -1,   -1,   44,   10,   -1,
   -1,   -1,   -1,   -1,   -1,  125,  126,   -1,   10,   -1,
   -1,   93,   59,   41,   -1,  125,   44,   -1,   93,   41,
   93,   -1,   44,   -1,  125,  126,   -1,   -1,   -1,   41,
   -1,   59,   44,   -1,   -1,   -1,   -1,   59,   -1,   41,
   -1,   -1,   44,  125,  126,   -1,   93,   59,   -1,   -1,
  125,  126,  125,  126,   -1,   -1,   -1,   59,   10,   -1,
   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,   -1,   -1,
   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,  125,  126,
   -1,   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   41,
   -1,   93,   44,   -1,   -1,  266,  267,  125,  126,   -1,
  271,   -1,   10,  125,  126,  266,  267,   59,   -1,   -1,
  271,  282,  283,  125,   -1,  276,  277,  278,  279,  280,
  281,  282,  283,  125,  126,   33,   -1,   -1,   -1,   -1,
   -1,   -1,   40,   -1,   -1,   43,   -1,   45,   10,   -1,
   -1,   93,   -1,   -1,   -1,   -1,  266,  267,   -1,   -1,
   -1,  271,   -1,   -1,   -1,   63,  276,  277,  278,  279,
  280,  281,  282,  283,   -1,  266,  267,   -1,   10,   41,
  271,   -1,   44,  125,  126,  276,  277,  278,  279,  280,
  281,  282,  283,   -1,  266,  267,   -1,   59,   -1,  271,
   -1,  266,  267,  266,  267,   -1,  271,   -1,  271,   41,
  282,  283,   44,   10,   -1,   -1,   -1,  282,  283,  282,
  283,   -1,   -1,   -1,   -1,  123,   -1,   59,  126,  266,
  267,   93,   -1,   -1,  271,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   41,  282,  283,   44,  266,  267,
   10,   10,   10,  271,  266,  267,   -1,   -1,   -1,  271,
   -1,   93,   59,  125,  282,  283,   -1,   -1,   -1,  271,
  282,  283,   10,   -1,  266,  267,   -1,   -1,   -1,  271,
   -1,   41,   41,   41,   44,   44,   44,   -1,   -1,   -1,
  282,  283,   -1,  125,   -1,   -1,   93,   -1,   -1,   59,
   59,   59,   -1,   41,   -1,   -1,   44,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   59,   -1,   -1,  266,  267,   -1,   -1,  125,  271,
   -1,   -1,   -1,   93,   93,   93,   -1,   -1,   -1,   -1,
   -1,  283,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   93,   -1,   -1,  256,  257,
  258,  259,  260,  261,  262,  125,  125,  125,   -1,   -1,
  268,   -1,  270,   -1,  272,  273,  274,  275,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   36,   37,  125,   -1,   40,
   41,   42,   43,   44,   45,   -1,   47,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  266,  267,   -1,   58,   -1,  271,
   36,   37,   -1,   -1,   40,   41,   42,   43,   44,   45,
   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   -1,  266,  267,   -1,   -1,   -1,  271,
   91,   -1,   93,   94,   36,   37,   -1,   -1,   40,   41,
   42,   43,   44,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   58,   93,   94,  266,
  267,   -1,   36,   37,  271,  126,   40,   41,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,   91,
  126,   93,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  271,  271,  271,   36,   37,   -1,   -1,   40,   41,
   42,   43,   -1,   45,   -1,   47,   -1,   91,   -1,   -1,
   94,   -1,   -1,  271,  126,   -1,   58,   -1,   -1,   -1,
   -1,   -1,   -1,   36,   37,   -1,   -1,   40,   -1,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  126,   -1,   -1,   58,   -1,   -1,   -1,   91,
   -1,   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   36,   37,   -1,   -1,   40,   41,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   91,   -1,
   -1,   94,   -1,   58,  126,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  264,   -1,  266,  267,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  276,  277,  278,  279,  280,
  281,  282,  283,  126,   -1,   -1,   91,  288,  264,   94,
  266,  267,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  276,  277,  278,  279,  280,  281,  282,  283,   -1,   -1,
   -1,   -1,  288,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  126,  264,   -1,  266,  267,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,
  282,  283,   -1,   -1,   -1,   -1,  288,   -1,   -1,   -1,
  264,   -1,  266,  267,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  276,  277,  278,  279,  280,  281,  282,  283,
   -1,   36,   37,   -1,  288,   40,   -1,   42,   43,   -1,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  264,   58,  266,  267,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,
  282,  283,   -1,   -1,   -1,   -1,  288,   -1,   -1,   -1,
   -1,  264,   -1,  266,  267,   -1,   91,   -1,  271,   94,
   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,  282,
  283,   -1,   36,   37,   -1,  288,   40,   41,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,  264,
   -1,  266,  267,   -1,   58,   -1,   -1,   -1,   -1,   -1,
   -1,  276,  277,  278,  279,  280,  281,  282,  283,   -1,
   36,   37,   -1,  288,   40,   -1,   42,   43,   -1,   45,
   -1,   47,   -1,   -1,   -1,   -1,   -1,   91,   -1,   -1,
   94,   -1,   58,   36,   37,   -1,   -1,   40,   -1,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   -1,   -1,   -1,   -1,
   36,   37,  126,   -1,   40,   91,   42,   43,   94,   45,
   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   58,   -1,   -1,   -1,   -1,   -1,   91,   -1,
   -1,   94,   -1,   -1,   -1,   -1,   -1,   36,   37,   -1,
  126,   40,   -1,   42,   43,   -1,   45,   -1,   47,   -1,
   -1,   -1,   -1,   -1,   -1,   91,   36,   37,   94,   58,
   40,   -1,   42,  126,   -1,   -1,   -1,   47,   -1,  264,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   58,   -1,
   -1,  276,  277,  278,  279,  280,  281,  282,  283,   33,
   -1,   -1,   91,  288,   -1,   94,   40,   -1,   -1,   43,
   -1,   45,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   91,   -1,   -1,   94,   33,   -1,   -1,   -1,   63,
   -1,   -1,   40,   -1,   -1,   43,   -1,   45,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  264,   -1,  266,  267,   -1,   63,   -1,   -1,   -1,   -1,
   -1,   -1,  276,  277,  278,  279,  280,  281,  282,  283,
   -1,   -1,   -1,   -1,  288,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  264,  123,
  266,  267,  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  276,  277,  278,  279,  280,  281,  282,  283,   -1,   -1,
   -1,  264,  288,   -1,   -1,  123,   -1,   -1,  126,   -1,
   -1,   -1,   -1,  276,  277,  278,  279,  280,  281,  282,
  283,   -1,   -1,   -1,   -1,  288,   -1,   -1,  264,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  276,  277,  278,  279,  280,  281,  282,   -1,   -1,   -1,
   -1,   -1,  288,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  264,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  276,  277,  278,
  279,  280,  281,   -1,  264,   -1,   -1,   -1,   -1,  288,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  288,   -1,
   -1,   -1,   -1,   -1,  258,  259,  260,  261,  262,   -1,
   -1,   -1,   -1,   -1,  268,   -1,  270,   -1,  272,  273,
  274,  275,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  258,  259,  260,  261,  262,    0,   -1,   -1,   -1,   -1,
  268,   -1,  270,   -1,  272,  273,  274,  275,   13,   14,
   15,   16,   17,   18,   -1,   20,   21,   -1,   -1,   -1,
   -1,   26,   27,   28,   29,   30,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   41,   42,   43,   44,
   45,   46,   47,   48,   49,   50,   51,   52,   53,   54,
   55,   56,   57,   58,   59,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   74,
   75,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  106,   -1,   -1,  109,   -1,  111,   -1,   -1,   -1,
  115,  116,  117,   -1,   -1,   -1,   -1,   -1,  123,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  133,
};
#define YYFINAL 22
#ifndef YYDEBUG
#define YYDEBUG 0
#endif
#define YYMAXTOKEN 290
#if YYDEBUG
char *yyname[] = {
"end-of-file",0,0,0,0,0,0,0,0,0,"'\\n'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,"'!'",0,0,"'$'","'%'",0,0,"'('","')'","'*'","'+'","','","'-'",0,"'/'",0,0,0,
0,0,0,0,0,0,0,"':'","';'",0,"'='",0,"'?'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,"'['",0,"']'","'^'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,"'{'",0,"'}'","'~'",0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"END_OF_INPUT","STR_CONST",
"NUM_CONST","NULL_CONST","SYMBOL","FUNCTION","LEX_ERROR","LBB","ERROR",
"LEFT_ASSIGN","RIGHT_ASSIGN","FOR","IN","IF","ELSE","WHILE","NEXT","BREAK",
"REPEAT","GT","GE","LT","LE","EQ","NE","AND","OR","LOW","TILDE","UNOT","NOT",
"SPECIAL","UMINUS","UPLUS",
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
#line 245 "gram.y"


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
	PROTECT(ans = firstarg(R_MissingArg, sym));
	return ans;
}

static SEXP xxfirstformal1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = firstarg(expr, sym));
	return ans;
}

static SEXP xxaddformal0(SEXP formlist, SEXP sym)
{
	SEXP ans;
	UNPROTECT(2);
	CheckFormalArgs(formlist ,sym);
	PROTECT(ans = nextarg(formlist, R_MissingArg, sym));
	return ans;
}

static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(3);
	CheckFormalArgs(formlist, sym);
	PROTECT(ans = nextarg(formlist, expr, sym));
	return ans;
}

static SEXP xxexprlist0()
{
	SEXP ans;
	PROTECT(ans = newlist());
	return ans;
}

static SEXP xxexprlist1(SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(1);
	PROTECT(ans = growlist(newlist(), expr));
	return ans;
}

static SEXP xxexprlist2(SEXP exprlist, SEXP expr)
{
	SEXP ans;
	AddComment(expr);
	UNPROTECT(2);
	PROTECT(ans = growlist(exprlist, expr));
	return ans;
}

static SEXP xxsub0(void)
{
	SEXP ans;
	PROTECT(ans = lang2(R_MissingArg,R_NilValue));
	return ans;
}

static SEXP xxsub1(SEXP expr)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(expr, R_NilValue));
	return ans;
}

static SEXP xxsymsub0(SEXP sym)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(R_MissingArg, sym));
	return ans;
}

static SEXP xxsymsub1(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = tagarg(expr, sym));
	return ans;
}

static SEXP xxnullsub0()
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = tagarg(R_MissingArg, install("NULL")));
	return ans;
}

static SEXP xxnullsub1(SEXP expr)
{
	SEXP ans = install("NULL");
	UNPROTECT(2);
	PROTECT(ans = tagarg(expr, ans));
	return ans;
}


static SEXP xxsublist1(SEXP sub)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = firstarg(CAR(sub),CADR(sub)));
	return ans;
}

static SEXP xxsublist2(SEXP sublist, SEXP sub)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = nextarg(sublist, CAR(sub), CADR(sub)));
	return ans;
}

static SEXP xxcond(SEXP expr)
{
	EatLines = 1;
	return expr;
}

static SEXP xxifcond(SEXP expr)
{
	ifpush();
	EatLines = 1;
	return expr;
}

static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(ifsym, cond, expr));
	return ans;
}

static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr)
{
	SEXP ans;
	UNPROTECT(3);
	PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	return ans;
}

static SEXP xxforcond(SEXP sym, SEXP expr)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = LCONS(sym, expr));
	EatLines=1;
	return ans;
}

static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
	return ans;
}

static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(whilesym, cond, body));
	return ans;
}

static SEXP xxrepeat(SEXP repeatsym, SEXP body)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(repeatsym, body));
	return ans;
}

static SEXP xxnxtbrk(SEXP keyword)
{
	PROTECT(keyword = lang1(keyword));
	return keyword;
}

static SEXP xxfuncall(SEXP expr, SEXP args)
{
	SEXP ans;
	if(isString(expr))
		expr = install(CHAR(STRING(expr)[0])); 
	UNPROTECT(2);
	if(length(CDR(args)) == 1 && CADR(args) == R_MissingArg )
		ans = lang1(expr);
	else    
		ans = LCONS(expr, CDR(args));   
	PROTECT(ans);
	return ans;
}       

static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body)
{
	SEXP ans;
	AddComment(body);
	UNPROTECT(2);
	ans = lang3(fname, CDR(formals), body); 
	PROTECT(ans);
	PopComment();
	return ans;
}

static SEXP xxunary(SEXP op, SEXP arg)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(op, arg));
	return ans;
}

static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3)
{
	SEXP ans;
	UNPROTECT(2);
	PROTECT(ans = lang3(n1, n2, n3));
	return ans;
}

static SEXP xxparen(SEXP n1, SEXP n2)
{
	SEXP ans;
	UNPROTECT(1);
	PROTECT(ans = lang2(n1, n2));
	return ans;
}

static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3)
{
	SEXP ans;
	UNPROTECT(2);
	ans = LCONS(a2, LCONS(a1, CDR(a3)));
	PROTECT(ans);
	return ans;
}

static SEXP xxexprlist(SEXP a1, SEXP a2)
{
	SEXP ans;
	UNPROTECT(1);
	TYPEOF(a2) = LANGSXP;
	CAR(a2) = a1;
	PROTECT(ans = a2);
	EatLines = 0;
	return ans;
}

/*----------------------------------------------------------------------------*/

SEXP tagarg(SEXP arg, SEXP tag)
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

SEXP newlist(void)
{
	SEXP s = CONS(R_NilValue, R_NilValue);
	CAR(s) = s;
	return s;
}

	/* Add a new element at the end of a stretchy list */

SEXP growlist(SEXP l, SEXP s)
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

/* static */ void ResetComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

static void PushComment(void)
{
	R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

static void PopComment(void)
{
	R_CommentSxp = CDR(R_CommentSxp);
}

int isComment(SEXP l)
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

SEXP firstarg(SEXP s, SEXP tag)
{
	SEXP tmp;
	PROTECT(s);
	PROTECT(tag);
	tmp = newlist();
	tmp = growlist(tmp, s);
	TAG(CAR(tmp)) = tag;
	UNPROTECT(2);
	return tmp;
}

SEXP nextarg(SEXP l, SEXP s, SEXP tag)
{
	PROTECT(tag);
	l = growlist(l, s);
	TAG(CAR(l)) = tag;
	UNPROTECT(1);
	return l;
}

/*----------------------------------------------------------------------------*/

	/* Basic File IO */

	/* This code is here because at this particular instant it */
	/* seems closely related to cget(), which appears below.  */

int R_fgetc(FILE *fp)
{
	int c = fgetc(fp);
	return feof(fp) ? R_EOF : c;
}


static char *buf;		/* The input stream buffer */
static char *bufp;		/* Pointer within current buffer */
static int cnt = 0;		/* Pointer to character count */

static char buf1[MAXELTSIZE];	/* File or text buffer */

static char buf0[MAXELTSIZE];	/* Console buffer */
static char *bufp0;		/* Pointer within the console buffer */
static int cnt0 = 0;		/* Characters in the console buffer */

static current_input;

/*
//	Set the input stream for the parser
//	    input = 0	initialize
//	    input = 1	console
//	    input = 2	file
//	    input = 3	text
*/

void R_SetInput(int input)
{
	xxgetc = cget;
	xxungetc = uncget;

	switch (input) {

	case 0:			/* Initialization / Reset */
		cnt = cnt0 = 0;
		buf = buf0;
		bufp = buf0;
		break;

	case 1:			/* Restore console values */
		cnt = cnt0;
		buf = buf0;
		bufp = bufp0;
		R_Console = 1;
		break;

	case 2:			/* Text or file input */
	case 3:
		if(R_Console == 1) {
			cnt0 = cnt;
			bufp0 = bufp;
		}
		cnt = 0;
		buf = buf1;
		bufp = buf1;
		R_Console = 0;
		break;
	}
	current_input = input;
}


/*
//	Fetch a single character from the current input stream.
//	The stream has been set by a call to R_SetInput().
*/

int cget()
{
	if (--cnt < 0) {
		switch(current_input) {

		case 1:
			if (ReadKBD(buf, MAXELTSIZE) == 0) {
				ClearerrConsole();
				return R_EOF;
			}
			break;

		case 2:
			if (fgets(buf, MAXELTSIZE, R_Inputfile) == NULL) {
				ResetConsole();
				return R_EOF;
			}
			break;

		case 3:
			if (R_ParseCnt < LENGTH(R_ParseText)) {
				strcpy(buf, CHAR(STRING(R_ParseText)[(R_ParseCnt)]));
				strcat(buf, "\n");
			}
			else return R_EOF;
			break;

		}
		R_ParseCnt++;
		bufp = buf;
		cnt = strlen(buf);
		cnt--;
	}
	return *bufp++;
}


/*
//	Push n characters back onto the input stream.
//	This is only called when the characters are
//	currently in the input buffer so pushing back
//	beyond the start of the buffer is impossible.
*/

void uncget()
{
	cnt += 1;
	bufp -= 1;
}

/*----------------------------------------------------------------------------*/

/* TODO:
   The function "parse" in source.c needs to be moved here.
   It should work by calling these functions.
   With that change, "newlist" and "growlist" can become
   static local functions */

/*
 *  Parsing Entry Points:
 *
 *  The Following extry points provide language parsing facilities.
 *  Note that there are separate entry points for parsing IOBuffers
 *  (i.e. interactve use), files and R character strings.
 *
 *	SEXP R_ParseFile(FILE *fp, int gencode, int *status)
 *
 *	SEXP R_ParseVector(SEXP *text, int gencode, int *status)
 *
 *	SEXP R_ParseBuffer(IOBuffer *buffer, int gencode, int *status)
 *	
 *  The entry points provide the same functionality, they just
 *  set things up in slightly different ways.
 *
 *	status = 0 - there was no statement to parse
 *		 1 - complete statement
 *		 2 - incomplete statement
 *		 3 - syntax error
 */

SEXP R_ParseFile(FILE *fp, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();

	xxgetc = cget;
	xxungetc = uncget;

	switch(yyparse()) {
	    case 0:		/* End of file */
		*status = 2;
		break;
	    case 1:		/* Syntax error / incomplete */
		if(EndOfFile) *status = 2;
		else *status = 3;
		break;
	    case 2:		/* Empty Line */
		*status = 0;
		break;
	    case 3:		/* Valid expression '\n' terminated */
	    case 4:		/* Valid expression ';' terminated */
		*status = 1;
		break;
	}
	return R_CurrentExpr;
}

SEXP R_ParseVector(SEXP *text, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();
	xxgetc = cget;
	xxungetc = uncget;
	return R_NilValue;
}

SEXP R_ParseBuffer(void *buffer, int gencode, int *status)
{
	GenerateCode = gencode;
	EndOfFile = 0;
	R_ParseError = 0;
	ResetComment();
	xxgetc = cget;
	xxungetc = uncget;
	return R_NilValue;
}

/*----------------------------------------------------------------------------*/
/*
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
 *  and ifpush. When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 */

static int reset = 1;
#ifndef DEBUG_LEX
static
#endif
char *parenp, parenstack[50];

static void ifpush(void)
{
	if (*parenp=='{' || *parenp=='[' || *parenp=='(' || *parenp == 'i')
		*++parenp = 'i';
}

static void ifpop(void)
{
	if (*parenp=='i')
		parenp--;
}

static int typeofnext(void)
{
	int k, c;

	c = cget();
	if (isdigit(c))
		k = 1;
	else if (isalpha(c) || c == '.')
		k = 2;
	else
		k = 3;
	uncget();
	return k;
}

static int nextchar(int expect)
{
	int c = cget();

	if (c == expect)
		return 1;
	else
		uncget();
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
				EatLines = 0;
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
				EatLines = 0;
				break;
			case FUNCTION:
			case WHILE:
			case REPEAT:
			case FOR:
			case IF:
				EatLines = 1;
				yylval = install(s);
				break;
			case IN:
				EatLines = 1;
				break;
			case ELSE:
				ifpop();
				EatLines = 1;
				break;
			case NEXT:
			case BREAK:
				EatLines = 0;
				yylval = install(s);
				break;
			case SYMBOL:
				PROTECT(yylval = install(s));
				EatLines = 0;
				break;
			}
			return keywords[i].token;
		}
	}
	return 0;
}

static void prompt()
{
	if (R_ParseText == R_NilValue && R_Console == 1)
		yyprompt(CHAR(STRING(GetOption(install("continue"), R_NilValue))[0]));
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

void yyinit(void)
{
	newline = 0;
	reset = 1;
}

int yywrap()
{
	return feof(R_Inputfile);
}

#ifdef HAVE_LIBREADLINE
extern char R_prompt_buf[512];
#endif


void yyprompt(char *format, ...)
{
	va_list(ap);
	va_start(ap, format);
#ifdef HAVE_LIBREADLINE
	vsprintf(R_prompt_buf, format, ap);
#else
	REvprintf(format, ap);
#endif
	va_end(ap);
	fflush(stdout);
	RBusy(0);
}

int yyerror(char *s)
{
	int i;

	R_CommentSxp = R_NilValue;
	REprintf("%s", buf);
	for (i = 1; i < bufp - buf; i++) {
		REprintf(" ");
	}
	REprintf("^\n");
	if (R_Console == 0 && R_Inputfile != NULL) {
		fclose(R_Inputfile);
		ResetConsole();
	}
	else {
		FlushConsole();
		REprintf("Error: %s\n", s);
	}
	newline = 0;
	reset = 1;
	cnt = 0;
	return 0;
}

static void CheckFormalArgs(SEXP formlist, SEXP new)
{
	int i;

	while( formlist != R_NilValue ) {
		if(TAG(formlist) == new ) {
			REprintf("%s", buf);
			for (i = 2; i < bufp - buf; i++) 
				REprintf(" ");
			REprintf("^\n");
			newline = 0;
			reset = 1;
			cnt = 0;
			error("Repeated formal argument.\n");
		}
		formlist=CDR(formlist);
	}
}

int yylex()
{
	SEXP f;
	int c, quote, kw;
	char *p, yytext[MAXELTSIZE];

	if (newline) {
		newline = 0;
		prompt();
	}

    again:
	if (reset) {
		parenp = parenstack;
		*parenp = ' ';
		reset = 0;
		EatLines = 0;
		ResetComment();
	}

	while ((c = xxgetc()) == ' ' || c == '\t' || c == '');

	if (c == '#') {
		p = yytext;
		*p++ = c;
		while ((c = xxgetc()) != '\n' && c != R_EOF)
			*p++ = c;
		*p = '\0';
		if(R_CommentSxp != R_NilValue) {
			f = mkChar(yytext);
			f = CONS(f, R_NilValue);
			CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
		}
	}


	if (c == R_EOF) return END_OF_INPUT;

		/* This code deals with context sensitivity to      */
		/* newlines.  The main problem is finding out       */
		/* whether a newline is followed by an ELSE clause. */
		/* This is only of importance if we are inside one  */
		/* of "(", "[", or "{".			     */

	if (c == '\n') {
		if (EatLines || *parenp == '[' || *parenp == '(') {
			prompt();
			goto again;
		}
		if (*parenp == 'i') {
			prompt();
			while ((c = xxgetc()) == ' ' || c == '\t');
			if (c == R_EOF) {
				error("unexpected end-of-file in parse\n");
			}
			if (c == '#') {
				p = yytext;
				*p++ = c;
				while ((c = xxgetc()) != '\n' && c != R_EOF)
					*p++ = c;
				*p = '\0';
				if(R_CommentSxp != R_NilValue) {
					f = mkChar(yytext);
					f = CONS(f, R_NilValue);
					CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
				}
			}
			if (c == '\n') {
				prompt();
				xxungetc();
				goto again;
			}
			if (c == '}' || c == ')' || c == ']' ) {
				while (*parenp == 'i')
					ifpop();
				parenp--;
				return c;
			}
			if (c == ',') {
				ifpop();
				return c;
			}
			xxungetc();
			if (!strncmp(bufp, "else", 4) && !isalnum(bufp[4]) && bufp[4] != '.') {
				EatLines = 1;
				bufp += 4;
				cnt -= 4;
				ifpop();
				return ELSE;
			}
			ifpop();
	 	   }
		else newline = 1;
		return '\n';
	}

		/* These are needed because both ";" and "," can */
		/* end an "if" clause without a newline.  Ifpop  */
		/* only does its thing in the right context.     */

	if (c == ';' || c == ',') {
		ifpop();
		return c;
	}

		/* Either digits or symbols can start with a "." */
		/* so we need to decide which it is and jump to  */
		/* the correct spot. */

	if (c == '.') {
		kw = typeofnext();
		if (kw >= 2) goto symbol;
	}

		/* literal numbers */

	if (c == '.' || isdigit(c)) {
		int seendot = (c == '.');
		int seenexp = 0;
		p = yytext;
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
			PROTECT(yylval = mkComplex(yytext));
		}
		else {
			PROTECT(yylval = mkFloat(yytext));
			xxungetc();
		}
		EatLines = 0;
		return NUM_CONST;
	}

	/* literal strings */

	if (c == '\"' || c == '\'') {
		quote = c;
		p = yytext;
		while ((c = xxgetc()) != R_EOF && c != quote) {
			if (c == '\n') {
				xxungetc();
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
		EatLines = 0;
		return STR_CONST;
	}

	/* special functions */
	if (c == '%') {
		p = yytext;
		*p++ = c;
		while ((c = xxgetc()) != R_EOF && c != '%') {
			if (c == '\n') {
				xxungetc();
				return ERROR;
			}
			*p++ = c;
		}
		if (c == '%')
			*p++ = c;
		*p++ = '\0';
		yylval = install(yytext);
		EatLines=1;
		return SPECIAL;
	}


	/* functions, constants and variables */

	/* gag, barf, but the punters want it */
	if (c == '_') {
		EatLines = 1;
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

    symbol:
	if (c == '.' || isalpha(c)) {
		p = yytext;
		do {
			*p++ = c;
		} while ((c = xxgetc()) != R_EOF && (isalnum(c) || c == '.'));
		xxungetc();
		*p = '\0';

		if ((kw = KeywordLookup(yytext))) {
			if(kw == FUNCTION) PushComment();
			return kw;
		}

		PROTECT(yylval = install(yytext));
		EatLines = 0;
		return SYMBOL;
	}

	/* compound tokens */

	switch (c) {
	case '<':
		EatLines = 1;
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
		EatLines = 1;
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
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install(">=");
			return GE;
		}
		yylval = install(">");
		return GT;
	case '!':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install("!=");
			return NE;
		}
		yylval = install("!");
		return '!';
	case '=':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install("==");
			return EQ;
		}
		return '=';
	case ':':
		EatLines = 1;
		if (nextchar('=')) {
			yylval = install(":=");
			return LEFT_ASSIGN;
		}
		yylval = install(":");
		return ':';
	case '&':
		EatLines = 1;
		if (nextchar('&')) {
			yylval = install("&&");
			return AND;
		}
		yylval = install("&");
		return AND;
	case '|':
		EatLines = 1;
		if (nextchar('|')) {
			yylval = install("||");
			return OR;
		}
		yylval = install("|");
		return OR;
	case '{':
		*++parenp = c;
		yylval = install("{");
		PushComment();
		return c;
	case '}':
		ifpop();
		if(*parenp == '{')
			PopComment();
		parenp--;
		return c;
	case '(':
		*++parenp = c;
		yylval = install("(");
		return c;
	case ')':
		EatLines = 0;
		ifpop();
		parenp--;
		return c;
	case '[':
		*++parenp = c;
		if (nextchar('[')) {
			*++parenp = c;
			yylval = install("[[");
			return LBB;
		}
		yylval = install("[");
		return c;
	case ']':
		ifpop();
		EatLines = 0;
		parenp--;
		return c;
	case '?':
		EatLines = 1;
		strcpy(yytext, "help");
		yylval = install(yytext);
		return c;
	case '*':
		EatLines=1;
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
		EatLines = 1;
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}
#line 2019 "y.tab.c"
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
#line 145 "gram.y"
{ return 0; }
break;
case 2:
#line 146 "gram.y"
{ return xxvalue(NULL,2); }
break;
case 3:
#line 147 "gram.y"
{ return xxvalue(yyvsp[-1],3); }
break;
case 4:
#line 148 "gram.y"
{ return xxvalue(yyvsp[-1],4); }
break;
case 5:
#line 149 "gram.y"
{ YYABORT; }
break;
case 6:
#line 152 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 7:
#line 153 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 8:
#line 154 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 9:
#line 155 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 10:
#line 157 "gram.y"
{ yyval = xxexprlist(yyvsp[-2],yyvsp[-1]); }
break;
case 11:
#line 158 "gram.y"
{ yyval = xxparen(yyvsp[-2],yyvsp[-1]); }
break;
case 12:
#line 160 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 13:
#line 161 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 14:
#line 162 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 15:
#line 163 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 16:
#line 164 "gram.y"
{ yyval = xxunary(yyvsp[-1],yyvsp[0]); }
break;
case 17:
#line 166 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 18:
#line 167 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 19:
#line 168 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 20:
#line 169 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 21:
#line 170 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 22:
#line 171 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 23:
#line 172 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 24:
#line 173 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 25:
#line 174 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 26:
#line 175 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 27:
#line 176 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 28:
#line 177 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 29:
#line 178 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 30:
#line 179 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 31:
#line 180 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 32:
#line 181 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 33:
#line 182 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 34:
#line 184 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 35:
#line 185 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[0],yyvsp[-2]); }
break;
case 36:
#line 187 "gram.y"
{ yyval = xxdefun(yyvsp[-5],yyvsp[-3],yyvsp[0]); }
break;
case 37:
#line 188 "gram.y"
{ yyval = xxfuncall(yyvsp[-3],yyvsp[-1]); }
break;
case 38:
#line 189 "gram.y"
{ yyval = xxif(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 39:
#line 190 "gram.y"
{ yyval = xxifelse(yyvsp[-4],yyvsp[-3],yyvsp[-2],yyvsp[0]); }
break;
case 40:
#line 191 "gram.y"
{ yyval = xxfor(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 41:
#line 192 "gram.y"
{ yyval = xxwhile(yyvsp[-2],yyvsp[-1],yyvsp[0]); }
break;
case 42:
#line 193 "gram.y"
{ yyval = xxrepeat(yyvsp[-1],yyvsp[0]); }
break;
case 43:
#line 194 "gram.y"
{ yyval = xxsubscript(yyvsp[-4],yyvsp[-3],yyvsp[-2]); }
break;
case 44:
#line 195 "gram.y"
{ yyval = xxsubscript(yyvsp[-3],yyvsp[-2],yyvsp[-1]); }
break;
case 45:
#line 196 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 46:
#line 197 "gram.y"
{ yyval = xxbinary(yyvsp[-1],yyvsp[-2],yyvsp[0]); }
break;
case 47:
#line 198 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 48:
#line 199 "gram.y"
{ yyval = xxnxtbrk(yyvsp[0]); }
break;
case 49:
#line 203 "gram.y"
{ yyval = xxcond(yyvsp[-1]); }
break;
case 50:
#line 206 "gram.y"
{ yyval = xxifcond(yyvsp[-1]); }
break;
case 51:
#line 209 "gram.y"
{ yyval = xxforcond(yyvsp[-3],yyvsp[-1]); }
break;
case 52:
#line 213 "gram.y"
{ yyval = xxexprlist0(); }
break;
case 53:
#line 214 "gram.y"
{ yyval = xxexprlist1(yyvsp[0]); }
break;
case 54:
#line 215 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 55:
#line 216 "gram.y"
{ yyval = yyvsp[-1]; AddComment(CAR(yyval));}
break;
case 56:
#line 217 "gram.y"
{ yyval = xxexprlist2(yyvsp[-2],yyvsp[0]); }
break;
case 57:
#line 218 "gram.y"
{ yyval = yyvsp[-1];}
break;
case 58:
#line 221 "gram.y"
{ yyval = xxsublist1(yyvsp[0]); }
break;
case 59:
#line 222 "gram.y"
{ yyval = xxsublist2(yyvsp[-3],yyvsp[0]); }
break;
case 60:
#line 225 "gram.y"
{ yyval = xxsub0(); }
break;
case 61:
#line 226 "gram.y"
{ yyval = xxsub1(yyvsp[0]); }
break;
case 62:
#line 227 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 63:
#line 228 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 64:
#line 229 "gram.y"
{ yyval = xxsymsub0(yyvsp[-1]); }
break;
case 65:
#line 230 "gram.y"
{ yyval = xxsymsub1(yyvsp[-2],yyvsp[0]); }
break;
case 66:
#line 231 "gram.y"
{ yyval = xxnullsub0(); }
break;
case 67:
#line 232 "gram.y"
{ yyval = xxnullsub1(yyvsp[0]); }
break;
case 68:
#line 235 "gram.y"
{ yyval = xxnullformal(); }
break;
case 69:
#line 236 "gram.y"
{ yyval = xxfirstformal0(yyvsp[0]); }
break;
case 70:
#line 237 "gram.y"
{ yyval = xxfirstformal1(yyvsp[-2],yyvsp[0]); }
break;
case 71:
#line 238 "gram.y"
{ yyval = xxaddformal0(yyvsp[-2],yyvsp[0]); }
break;
case 72:
#line 239 "gram.y"
{ yyval = xxaddformal1(yyvsp[-4],yyvsp[-2],yyvsp[0]); }
break;
case 73:
#line 242 "gram.y"
{ EatLines = 1; }
break;
#line 2452 "y.tab.c"
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
