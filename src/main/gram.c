#ifndef lint
static char const yysccsid[] = "@(#)yaccpar	1.9 (Berkeley) 02/21/93";
#endif
#define YYBYACC 1
#define YYMAJOR 1
#define YYMINOR 9
#define YYLEX yylex()
#define YYEMPTY -1
#define yyclearin (yychar=(YYEMPTY))
#define yyerrok (yyerrflag=0)
#define YYRECOVERING (yyerrflag!=0)
/* cfront 1.2 defines "c_plusplus" instead of "__cplusplus" */
#ifdef c_plusplus
#ifndef __cplusplus
#define __cplusplus
#endif
#endif
#ifdef __cplusplus
extern "C" { char *getenv(const char *); }
#else
extern char *getenv();
extern int yylex();
extern int yyparse();
#endif
#define YYPREFIX "yy"
#line 2 "gram.y"
/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
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

extern SEXP listAppend(SEXP,SEXP);
void pushCmt();
void popCmt();

static int eatln;
extern SEXP R_CommentSxp;

#define YYSTYPE		SEXP
#ifdef YYBYACC
#define YYRETURN(x)	{ return(x); }
#else
#define YYRETURN(x)	{ free((void*)yys); free((void*)yyv); return(x); }
#endif

#line 63 "y.tab.c"
#define STR_CONST 257
#define NUM_CONST 258
#define NULL_CONST 259
#define SYMBOL 260
#define FUNCTION 261
#define LEX_ERROR 262
#define LBB 263
#define ERROR 264
#define LEFT_ASSIGN 265
#define RIGHT_ASSIGN 266
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
const short yylhs[] = {                                        -1,
    0,    0,    0,    0,    0,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
    1,    1,    1,    1,    1,    1,    1,    1,    8,    6,
    7,    2,    2,    2,    2,    2,    2,    5,    5,    9,
    9,    9,    9,    9,    9,    9,    9,    3,    3,    3,
    3,    3,    4,
};
const short yylen[] = {                                         2,
    0,    2,    3,    3,    2,    1,    1,    1,    1,    3,
    3,    2,    2,    2,    2,    2,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
    3,    3,    3,    3,    3,    6,    4,    3,    5,    3,
    3,    2,    5,    4,    3,    3,    1,    1,    3,    3,
    5,    0,    1,    3,    2,    3,    2,    1,    4,    0,
    1,    3,    2,    3,    2,    3,    2,    0,    1,    3,
    3,    5,    0,
};
const short yydefred[] = {                                      1,
    0,    5,    7,    6,    8,    9,    0,    0,    0,    0,
   47,   48,    0,    0,    0,    0,    0,    0,    2,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    3,    4,    0,    0,    0,    0,    0,    0,    0,
    0,   11,    0,    0,   10,    0,    0,    0,    0,    0,
   58,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
   46,   45,    0,    0,    0,   73,    0,    0,   50,    0,
   49,    0,    0,    0,    0,    0,    0,    0,   37,   44,
    0,    0,    0,    0,    0,    0,    0,    0,   43,    0,
    0,    0,   51,   59,    0,
};
const short yydgoto[] = {                                       1,
   79,   37,   65,  118,   80,   27,   25,   29,   81,
};
const short yysindex[] = {                                      0,
  564,    0,    0,    0,    0,    0,  -29,  -26,  -24,  -18,
    0,    0, 1247, 1247, 1247, 1247, 1247, 1247,    0, 1247,
 1247,  -10, -236, -229, 1247, 1247, 1247, 1247, 1247, 1266,
 1266, 1309,  -31,  -31,  920, 1266,   15, 1329, 1365, 1247,
 1247, 1247, 1247, 1247, 1247, 1247, 1247, 1247, 1247, 1247,
 1247, 1247, 1247, 1247, 1247, 1247, 1247, 1247, -256, 1365,
 1365,    0,    0,  -27,  -23, -227, 1266,  948,  991, 1018,
 1266,    0, 1247, 1247,    0,   -6,    5,   21, 1266,    4,
    0, 1266, 1289, 1348, 1348, 1348, 1348, 1348, 1348, 1329,
  -30, 1309,  147,  147,  145,  145,  145,  168,  -31,  -31,
    0,    0,   57,    6, 1247,    0, -160, 1247,    0, 1247,
    0, 1266, 1266, 1247, 1247, 1247,    8,   59,    0,    0,
 1266, 1247,   44, 1197, 1266, 1266, 1266, 1266,    0, 1365,
 1266, 1247,    0,    0, 1266,
};
const short yyrindex[] = {                                      0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   19,
    0,    0,  -21,    0,    0,    0,    0,    0,    0,  414,
  439,  127,    9,   28,    0,   84,    0,  435,  -36,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   -1,
  -36,    0,    0,    1,    0,    0,  724,    0,  389,    0,
  750,    0,  103,  110,    0,  821,  860,  883,   -5,   73,
    0,  759,  527,  437,  456,  458,  477,  485,  506,  606,
  670,  705,  397,  416,  105,  359,  378,   86,   48,   67,
    0,    0,   73,   73,    0,    0,    0,    0,    0,    0,
    0,  129,  148,   39,  122,  126,    0,    0,    0,    0,
    3,    0,   18,    0,  760,  131,  136,  199,    0,  219,
  767,    0,    0,    0,   35,
};
const short yygindex[] = {                                      0,
 1627,    0,    0,   12,  -58,    0,    0,    0,  -11,
};
#define YYTABLESIZE 1759
const short yytable[] = {                                      62,
  101,  103,  104,  102,   59,   59,   55,   60,   60,   60,
   23,   53,   51,   24,   52,   26,   54,  106,   13,   68,
  107,   28,   68,   64,   73,   59,   55,   57,   52,   60,
   66,   53,   51,  105,   52,   61,   54,   12,   61,   60,
  108,   69,   60,   70,   69,   13,   70,   57,   63,   13,
   13,   13,   13,   13,  114,   13,   60,   17,   71,   61,
   61,   71,   58,   58,   12,  115,   13,   13,   12,   12,
   12,   12,   12,   74,   12,   72,   22,   52,   72,   65,
   61,  116,   65,   58,   17,   12,   12,   61,   17,   17,
   17,   17,   17,   53,   17,   23,  117,  119,  120,  123,
  129,   13,  130,   22,  132,   17,   17,   22,   22,   22,
   22,   22,   57,   22,   20,   50,   73,  122,  134,   55,
   12,    0,   23,    0,   22,   22,   23,   23,   23,   23,
   23,   65,   23,   13,   13,    0,   15,    0,   56,   75,
   17,   20,   53,   52,   23,   20,   20,   20,   20,   20,
    0,   20,   12,   12,    0,    0,    0,   54,    0,   22,
    0,   57,   67,   20,    0,   67,   63,   15,   55,   63,
   15,   64,   17,   17,   64,    0,   66,    0,   23,   66,
   59,    0,   59,   55,   60,   15,   60,   56,   53,    0,
    0,   22,   22,   54,    0,    0,    0,   20,    0,    0,
    0,    0,   57,   59,   57,    0,   54,   60,   53,    0,
   23,   23,    0,    0,   67,    0,    0,    0,   63,   15,
    0,    0,    0,   64,    0,   57,    0,   57,   66,   20,
   20,   39,   39,    0,   55,   61,    0,   61,   58,   62,
   58,    0,   62,    0,   42,   43,   44,   45,   46,   47,
   48,   15,   39,   56,   40,   41,   56,    0,   61,   60,
    0,   58,   60,    0,   42,   43,   44,   45,   46,   47,
   48,   49,   54,   13,   13,    0,   56,    0,   13,    0,
    0,    0,    0,   13,   13,   13,   13,   13,   13,   13,
   13,   62,   12,   12,    0,   13,    0,   12,    0,    0,
    0,    0,   12,   12,   12,   12,   12,   12,   12,   12,
    0,   60,   17,   17,   12,    0,    0,   17,    0,    0,
    0,    0,   17,   17,   17,   17,   17,   17,   17,   17,
    0,   22,   22,    0,   17,    0,   22,    0,    0,    0,
    0,   22,   22,   22,   22,   22,   22,   22,   22,    0,
   23,   23,    0,   22,    0,   23,    0,    0,    0,    0,
   23,   23,   23,   23,   23,   23,   23,   23,   21,   20,
   20,    0,   23,    0,   20,    0,    0,    0,    0,   20,
   20,   20,   20,   20,   20,   20,   20,   24,    0,    0,
    0,   15,   15,    0,    0,   21,   15,    0,   38,   21,
   21,   21,   21,   21,    0,   21,   18,   39,    0,   39,
    0,    0,    0,    0,   24,    0,    0,   21,   24,   24,
   24,   24,   24,   42,   24,   19,    0,    0,    0,   38,
   39,   56,   38,   56,    0,    0,   24,   18,    0,   18,
   18,   18,    0,    0,   14,    0,   31,   38,   16,    0,
    0,   21,    0,    0,   42,   18,   19,   42,   19,   19,
   19,    0,    0,    0,    0,   30,    0,   26,    0,    0,
   24,    0,   42,    0,   19,   14,    0,   31,   14,   16,
   31,   38,   16,   21,   21,    0,   27,    0,    0,   18,
    0,    0,    0,   14,   28,   31,   30,   16,   26,   30,
    0,   26,   24,   24,    0,    0,   42,    0,   19,    0,
    0,    0,    0,   38,   30,   29,   26,   27,    0,    0,
   27,   18,   18,    0,    0,   28,    0,   14,   28,   31,
    0,   16,    0,    0,    0,   27,   35,    0,   42,    0,
   19,   19,    0,   28,    0,    0,   29,    0,   30,   29,
   26,    0,    0,    0,    0,    0,    0,    0,    0,   14,
   14,   31,   31,   16,   29,    0,    0,   35,    0,   27,
   35,    0,    0,   19,    0,    0,    0,   28,    0,    0,
   30,   30,   26,   26,    0,   35,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   21,    0,   29,    0,
    0,   27,   27,   18,    0,    0,   16,    0,   17,   28,
   28,    0,    0,    0,    0,   32,    0,    0,    0,   35,
    0,    0,    0,   21,   21,    0,   14,    0,   21,    0,
   29,   29,    0,   21,   21,   21,   21,   21,   21,   21,
   21,    0,   24,   24,    0,    0,   32,   24,    0,   32,
    0,   35,   24,   24,   24,   24,   24,   24,   24,   24,
    0,   18,   18,    0,   32,    0,   18,    0,    0,    0,
    0,   18,   18,   18,   18,   18,   18,   18,   18,   33,
   19,   19,    0,   42,    0,   19,   20,    0,    0,   15,
   19,   19,   19,   19,   19,   19,   19,   19,   32,   14,
   14,   31,   31,    0,   14,    0,   31,    0,   16,    0,
   33,    0,    0,   33,   25,   14,   14,   31,   31,    0,
   30,   30,   26,   26,    0,   30,    0,   26,   33,    0,
   32,   32,    0,   40,    0,    0,   30,   30,   26,   26,
    0,   27,   27,    0,    0,   25,   27,    0,   25,   28,
   28,    0,    0,    0,   28,    0,    0,   27,   27,   41,
    0,    0,   33,   25,   40,   28,   28,   40,   34,   39,
   29,   29,    0,    0,    0,   29,   36,    0,    0,    0,
    0,    0,   40,    0,    0,    0,   29,   29,    0,    0,
   41,   35,   35,   41,   33,   33,   35,   25,    0,   34,
   39,    0,   34,   39,    0,    0,    0,   36,   41,    0,
   36,    0,    0,    0,    0,    0,   40,   34,   39,    2,
    3,    4,    5,    6,    7,   36,    0,    0,    0,   25,
    8,    0,    9,    0,   10,   11,   12,   13,    0,    0,
    0,    0,   41,    0,    0,    0,    0,    0,   40,    0,
    0,   34,   39,    0,    0,    0,    7,    7,    0,   36,
    7,    7,    7,    7,    7,    7,    0,    7,    0,    0,
   32,   32,    0,    0,   41,   32,    0,    0,    7,    0,
    0,    0,    0,   34,   39,    0,   32,   32,    0,    0,
    0,   36,    0,    0,    0,    8,    8,    0,    0,    8,
    8,    8,    8,    8,    8,    0,    8,    0,    0,    0,
    0,    7,    0,    7,    7,    0,    0,    8,    9,    9,
    0,    0,    9,    9,    9,    9,    9,    9,    0,    9,
    0,    0,    0,    0,   33,   33,    0,    0,    0,   33,
    9,    0,    0,    0,    0,    0,    7,    0,    0,    0,
    8,   33,    8,    8,    0,   59,   55,    0,    0,   60,
   72,   53,   51,    0,   52,    0,   54,    0,    0,   25,
   25,    0,    0,    9,   25,    9,    9,   57,    0,    0,
    0,    0,    0,   59,   55,    8,    0,   60,  109,   53,
   51,    0,   52,   40,   54,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,   57,    0,    0,    9,    0,
   61,    0,    0,   58,    0,    0,    0,    0,    0,   41,
    0,    0,    0,    0,    0,    0,   59,   55,   34,   39,
   60,    0,   53,   51,    0,   52,   36,   54,   61,    0,
    0,   58,    0,    0,    0,   50,    0,    0,   57,    0,
    0,    0,    0,   59,   55,    0,    0,   60,  111,   53,
   51,    0,   52,    0,   54,    0,    0,    0,    0,    0,
    0,    0,    0,   50,    0,   57,    0,    0,    0,    0,
    0,   61,    0,    7,   58,    7,    7,    0,    0,    0,
    0,    0,    0,    0,    0,    7,    7,    7,    7,    7,
    7,    7,    7,    0,    0,    0,    0,    7,   61,    0,
    0,   58,    0,    0,    0,    0,   50,    0,    0,    0,
    0,    0,    8,    0,    8,    8,    0,    0,    0,    0,
    0,    0,    0,    0,    8,    8,    8,    8,    8,    8,
    8,    8,    0,   50,    0,    9,    8,    9,    9,    0,
    0,    0,    0,    0,    0,    0,    0,    9,    9,    9,
    9,    9,    9,    9,    9,    0,    0,    0,    0,    9,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   39,    0,   40,   41,    0,    0,    0,    0,
    0,    0,    0,    0,   42,   43,   44,   45,   46,   47,
   48,   49,    0,    0,    0,    0,   56,    0,    0,    0,
   39,    0,   40,   41,    0,    0,    0,    0,    0,    0,
    0,    0,   42,   43,   44,   45,   46,   47,   48,   49,
    0,    0,   59,   55,   56,    0,   60,  133,   53,   51,
    0,   52,    0,   54,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   39,   57,   40,   41,    0,    0,    0,
  110,    0,    0,    0,    0,   42,   43,   44,   45,   46,
   47,   48,   49,    0,    0,    0,    0,   56,    0,   21,
   39,    0,   40,   41,    0,    0,   18,   61,    0,   16,
   58,   17,   42,   43,   44,   45,   46,   47,   48,   49,
    0,   59,   55,    0,   56,   60,    0,   53,   51,   14,
   52,    0,   54,    0,    0,    0,    0,    0,    0,    0,
    0,    0,   50,   57,   59,   55,    0,    0,   60,    0,
   53,   51,    0,   52,    0,   54,    0,    0,    0,    0,
    0,    0,    0,    0,   59,   55,   57,    0,   60,    0,
   53,   51,    0,   52,    0,   54,   61,    0,    0,   58,
    0,    0,    0,    0,   59,   55,   57,    0,   60,   20,
   53,   51,   15,   52,    0,   54,    0,    0,    0,   61,
    0,    0,   58,   59,   55,    0,   57,   60,    0,   53,
   51,   50,   52,    0,   54,    0,    0,   21,    0,   61,
    0,    0,   58,    0,   18,   57,    0,   16,    0,   17,
    0,    0,    0,    0,   50,    0,    0,    0,    0,   61,
    0,    0,   58,    0,    0,    0,    0,   14,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,   61,    0,
    0,   58,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,   39,
    0,   40,   41,    0,    0,    0,    0,    0,    0,    0,
    0,   42,   43,   44,   45,   46,   47,   48,   49,    0,
    0,    0,    0,   56,    0,    0,    0,   20,    0,    0,
   15,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    3,    4,    5,    6,    7,    0,    0,
    0,    0,    0,    8,    0,    9,    0,   10,   11,   12,
   13,    0,    0,    0,    0,    0,    0,    0,   39,    0,
   40,   41,    0,    0,    0,    0,    0,    0,    0,    0,
   42,   43,   44,   45,   46,   47,   48,   49,    0,    0,
    0,   39,   56,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,   42,   43,   44,   45,   46,   47,   48,
   49,   39,    0,    0,    0,   56,    0,    0,    0,    0,
    0,    0,    0,   42,   43,   44,   45,   46,   47,   48,
   49,   39,    0,    0,    0,   56,    0,    0,    0,    0,
    0,    0,    0,   42,   43,   44,   45,   46,   47,    0,
   39,    0,    0,    0,    0,   56,    0,    0,    0,    0,
    0,   76,    4,   77,   78,    7,    0,   22,    0,    0,
    0,    8,    0,    9,   56,   10,   11,   12,   13,   30,
   31,   32,   33,   34,   35,    0,   36,   38,    0,    0,
    0,   67,   68,   69,   70,   71,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,   82,   83,   84,   85,
   86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
   96,   97,   98,   99,  100,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,  112,
  113,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
    0,  121,    0,    0,  124,    0,  125,    0,    0,    0,
  126,  127,  128,    0,    0,    0,    0,    0,  131,    0,
    0,    0,    0,    0,    0,    0,    0,    0,  135,
};
const short yycheck[] = {                                      10,
  257,   60,   61,  260,   36,   36,   37,   44,   40,   40,
   40,   42,   43,   40,   45,   40,   47,   41,   10,   41,
   44,   40,   44,  260,   10,   36,   37,   58,   10,   40,
  260,   42,   43,   61,   45,   41,   47,   10,   44,   41,
  268,   41,   44,   41,   44,   37,   44,   58,   59,   41,
   42,   43,   44,   45,   61,   47,   93,   10,   41,   91,
   91,   44,   94,   94,   37,   61,   58,   59,   41,   42,
   43,   44,   45,   59,   47,   41,   10,   59,   44,   41,
   91,   61,   44,   94,   37,   58,   59,   93,   41,   42,
   43,   44,   45,   10,   47,   10,   93,   41,   93,  260,
   93,   93,   44,   37,   61,   58,   59,   41,   42,   43,
   44,   45,   10,   47,   10,  126,   44,  106,  130,   10,
   93,   -1,   37,   -1,   58,   59,   41,   42,   43,   44,
   45,   93,   47,  125,  126,   -1,   10,   -1,   10,  125,
   93,   37,   59,  125,   59,   41,   42,   43,   44,   45,
   -1,   47,  125,  126,   -1,   -1,   -1,   10,   -1,   93,
   -1,   59,   41,   59,   -1,   44,   41,   41,   59,   44,
   44,   41,  125,  126,   44,   -1,   41,   -1,   93,   44,
   36,   -1,   36,   37,   40,   59,   40,   59,   42,   -1,
   -1,  125,  126,   47,   -1,   -1,   -1,   93,   -1,   -1,
   -1,   -1,   58,   36,   58,   -1,   59,   40,  125,   -1,
  125,  126,   -1,   -1,   93,   -1,   -1,   -1,   93,   93,
   -1,   -1,   -1,   93,   -1,   58,   -1,  125,   93,  125,
  126,  263,  263,   -1,  125,   91,   -1,   91,   94,   41,
   94,   -1,   44,   -1,  275,  276,  277,  278,  279,  280,
  281,  125,  263,  125,  265,  266,  287,   -1,   91,   41,
   -1,   94,   44,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,  125,  265,  266,   -1,  287,   -1,  270,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,   93,  265,  266,   -1,  287,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,   93,  265,  266,  287,   -1,   -1,  270,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,  265,  266,   -1,  287,   -1,  270,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,   -1,
  265,  266,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   10,  265,
  266,   -1,  287,   -1,  270,   -1,   -1,   -1,   -1,  275,
  276,  277,  278,  279,  280,  281,  282,   10,   -1,   -1,
   -1,  265,  266,   -1,   -1,   37,  270,   -1,   10,   41,
   42,   43,   44,   45,   -1,   47,   10,  263,   -1,  263,
   -1,   -1,   -1,   -1,   37,   -1,   -1,   59,   41,   42,
   43,   44,   45,   10,   47,   10,   -1,   -1,   -1,   41,
  263,  287,   44,  287,   -1,   -1,   59,   41,   -1,   43,
   44,   45,   -1,   -1,   10,   -1,   10,   59,   10,   -1,
   -1,   93,   -1,   -1,   41,   59,   41,   44,   43,   44,
   45,   -1,   -1,   -1,   -1,   10,   -1,   10,   -1,   -1,
   93,   -1,   59,   -1,   59,   41,   -1,   41,   44,   41,
   44,   93,   44,  125,  126,   -1,   10,   -1,   -1,   93,
   -1,   -1,   -1,   59,   10,   59,   41,   59,   41,   44,
   -1,   44,  125,  126,   -1,   -1,   93,   -1,   93,   -1,
   -1,   -1,   -1,  125,   59,   10,   59,   41,   -1,   -1,
   44,  125,  126,   -1,   -1,   41,   -1,   93,   44,   93,
   -1,   93,   -1,   -1,   -1,   59,   10,   -1,  125,   -1,
  125,  126,   -1,   59,   -1,   -1,   41,   -1,   93,   44,
   93,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  125,
  126,  125,  126,  125,   59,   -1,   -1,   41,   -1,   93,
   44,   -1,   -1,   10,   -1,   -1,   -1,   93,   -1,   -1,
  125,  126,  125,  126,   -1,   59,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   33,   -1,   93,   -1,
   -1,  125,  126,   40,   -1,   -1,   43,   -1,   45,  125,
  126,   -1,   -1,   -1,   -1,   10,   -1,   -1,   -1,   93,
   -1,   -1,   -1,  265,  266,   -1,   63,   -1,  270,   -1,
  125,  126,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,   -1,  265,  266,   -1,   -1,   41,  270,   -1,   44,
   -1,  125,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,  265,  266,   -1,   59,   -1,  270,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,   10,
  265,  266,   -1,  270,   -1,  270,  123,   -1,   -1,  126,
  275,  276,  277,  278,  279,  280,  281,  282,   93,  265,
  266,  265,  266,   -1,  270,   -1,  270,   -1,  270,   -1,
   41,   -1,   -1,   44,   10,  281,  282,  281,  282,   -1,
  265,  266,  265,  266,   -1,  270,   -1,  270,   59,   -1,
  125,  126,   -1,   10,   -1,   -1,  281,  282,  281,  282,
   -1,  265,  266,   -1,   -1,   41,  270,   -1,   44,  265,
  266,   -1,   -1,   -1,  270,   -1,   -1,  281,  282,   10,
   -1,   -1,   93,   59,   41,  281,  282,   44,   10,   10,
  265,  266,   -1,   -1,   -1,  270,   10,   -1,   -1,   -1,
   -1,   -1,   59,   -1,   -1,   -1,  281,  282,   -1,   -1,
   41,  265,  266,   44,  125,  126,  270,   93,   -1,   41,
   41,   -1,   44,   44,   -1,   -1,   -1,   41,   59,   -1,
   44,   -1,   -1,   -1,   -1,   -1,   93,   59,   59,  256,
  257,  258,  259,  260,  261,   59,   -1,   -1,   -1,  125,
  267,   -1,  269,   -1,  271,  272,  273,  274,   -1,   -1,
   -1,   -1,   93,   -1,   -1,   -1,   -1,   -1,  125,   -1,
   -1,   93,   93,   -1,   -1,   -1,   36,   37,   -1,   93,
   40,   41,   42,   43,   44,   45,   -1,   47,   -1,   -1,
  265,  266,   -1,   -1,  125,  270,   -1,   -1,   58,   -1,
   -1,   -1,   -1,  125,  125,   -1,  281,  282,   -1,   -1,
   -1,  125,   -1,   -1,   -1,   36,   37,   -1,   -1,   40,
   41,   42,   43,   44,   45,   -1,   47,   -1,   -1,   -1,
   -1,   91,   -1,   93,   94,   -1,   -1,   58,   36,   37,
   -1,   -1,   40,   41,   42,   43,   44,   45,   -1,   47,
   -1,   -1,   -1,   -1,  265,  266,   -1,   -1,   -1,  270,
   58,   -1,   -1,   -1,   -1,   -1,  126,   -1,   -1,   -1,
   91,  282,   93,   94,   -1,   36,   37,   -1,   -1,   40,
   41,   42,   43,   -1,   45,   -1,   47,   -1,   -1,  265,
  266,   -1,   -1,   91,  270,   93,   94,   58,   -1,   -1,
   -1,   -1,   -1,   36,   37,  126,   -1,   40,   41,   42,
   43,   -1,   45,  270,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   58,   -1,   -1,  126,   -1,
   91,   -1,   -1,   94,   -1,   -1,   -1,   -1,   -1,  270,
   -1,   -1,   -1,   -1,   -1,   -1,   36,   37,  270,  270,
   40,   -1,   42,   43,   -1,   45,  270,   47,   91,   -1,
   -1,   94,   -1,   -1,   -1,  126,   -1,   -1,   58,   -1,
   -1,   -1,   -1,   36,   37,   -1,   -1,   40,   41,   42,
   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  126,   -1,   58,   -1,   -1,   -1,   -1,
   -1,   91,   -1,  263,   94,  265,  266,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,   -1,   -1,   -1,   -1,  287,   91,   -1,
   -1,   94,   -1,   -1,   -1,   -1,  126,   -1,   -1,   -1,
   -1,   -1,  263,   -1,  265,  266,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,   -1,  126,   -1,  263,  287,  265,  266,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,  275,  276,  277,
  278,  279,  280,  281,  282,   -1,   -1,   -1,   -1,  287,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  263,   -1,  265,  266,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,
  281,  282,   -1,   -1,   -1,   -1,  287,   -1,   -1,   -1,
  263,   -1,  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,   -1,   36,   37,  287,   -1,   40,   41,   42,   43,
   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  263,   58,  265,  266,   -1,   -1,   -1,
  270,   -1,   -1,   -1,   -1,  275,  276,  277,  278,  279,
  280,  281,  282,   -1,   -1,   -1,   -1,  287,   -1,   33,
  263,   -1,  265,  266,   -1,   -1,   40,   91,   -1,   43,
   94,   45,  275,  276,  277,  278,  279,  280,  281,  282,
   -1,   36,   37,   -1,  287,   40,   -1,   42,   43,   63,
   45,   -1,   47,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,  126,   58,   36,   37,   -1,   -1,   40,   -1,
   42,   43,   -1,   45,   -1,   47,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   36,   37,   58,   -1,   40,   -1,
   42,   43,   -1,   45,   -1,   47,   91,   -1,   -1,   94,
   -1,   -1,   -1,   -1,   36,   37,   58,   -1,   40,  123,
   42,   43,  126,   45,   -1,   47,   -1,   -1,   -1,   91,
   -1,   -1,   94,   36,   37,   -1,   58,   40,   -1,   42,
   43,  126,   45,   -1,   47,   -1,   -1,   33,   -1,   91,
   -1,   -1,   94,   -1,   40,   58,   -1,   43,   -1,   45,
   -1,   -1,   -1,   -1,  126,   -1,   -1,   -1,   -1,   91,
   -1,   -1,   94,   -1,   -1,   -1,   -1,   63,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   91,   -1,
   -1,   94,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  263,
   -1,  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  275,  276,  277,  278,  279,  280,  281,  282,   -1,
   -1,   -1,   -1,  287,   -1,   -1,   -1,  123,   -1,   -1,
  126,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  257,  258,  259,  260,  261,   -1,   -1,
   -1,   -1,   -1,  267,   -1,  269,   -1,  271,  272,  273,
  274,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  263,   -1,
  265,  266,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
  275,  276,  277,  278,  279,  280,  281,  282,   -1,   -1,
   -1,  263,  287,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  263,   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,  281,
  282,  263,   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,  275,  276,  277,  278,  279,  280,   -1,
  263,   -1,   -1,   -1,   -1,  287,   -1,   -1,   -1,   -1,
   -1,  257,  258,  259,  260,  261,   -1,    1,   -1,   -1,
   -1,  267,   -1,  269,  287,  271,  272,  273,  274,   13,
   14,   15,   16,   17,   18,   -1,   20,   21,   -1,   -1,
   -1,   25,   26,   27,   28,   29,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   40,   41,   42,   43,
   44,   45,   46,   47,   48,   49,   50,   51,   52,   53,
   54,   55,   56,   57,   58,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   73,
   74,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,
   -1,  105,   -1,   -1,  108,   -1,  110,   -1,   -1,   -1,
  114,  115,  116,   -1,   -1,   -1,   -1,   -1,  122,   -1,
   -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1,  132,
};
#define YYFINAL 1
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
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,"STR_CONST","NUM_CONST",
"NULL_CONST","SYMBOL","FUNCTION","LEX_ERROR","LBB","ERROR","LEFT_ASSIGN",
"RIGHT_ASSIGN","FOR","IN","IF","ELSE","WHILE","NEXT","BREAK","REPEAT","GT","GE",
"LT","LE","EQ","NE","AND","OR","LOW","TILDE","UNOT","NOT","SPECIAL","UMINUS",
"UPLUS",
};
const char * const yyrule[] = {
"$accept : prog",
"prog :",
"prog : prog '\\n'",
"prog : prog expr '\\n'",
"prog : prog expr ';'",
"prog : prog error",
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
"expr : FUNCTION '(' formlist ')' gobble expr",
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
"sublist : sublist gobble ',' sub",
"sub :",
"sub : expr",
"sub : SYMBOL '=' expr",
"sub : SYMBOL '='",
"sub : STR_CONST '=' expr",
"sub : STR_CONST '='",
"sub : NULL_CONST '=' expr",
"sub : NULL_CONST '='",
"formlist :",
"formlist : SYMBOL",
"formlist : SYMBOL '=' expr",
"formlist : formlist ',' SYMBOL",
"formlist : formlist ',' SYMBOL '=' expr",
"gobble :",
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
#line 167 "gram.y"

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

/* Lists are created and grown using a special dotted pair. */
/* The CAR of the list points to the last cons-cell in the list */
/* and the CDR points to the first.  The list can be extracted */
/* from the pair by taking its CDR, while the CAR gives fast access */
/* to the end of the list. */

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


	/* Comment Handling */

	/* R_CommentSxp is of the same form as an expression list, */
	/* each time a new { is encountered a new element is placed */
	/* in the R_CommentSxp and when a } is encountered it is */
	/* removed. */

extern void ResetComment()
{
	R_CommentSxp = CONS(R_NilValue, R_NilValue);
}

void pushCmt()
{
	R_CommentSxp = CONS(R_NilValue, R_CommentSxp);
}

void popCmt()
{
	R_CommentSxp = CDR(R_CommentSxp);
}

int isComment(SEXP l)
{
	if (isList(l) && isString(CAR(l)) && !strncmp(CHAR(STRING(CAR(l))[0]), "#", 1))
		return 1;
	else
		return 0;
}

void addcomment(SEXP l)
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


SEXP mkString(char *);
SEXP mkInteger(char *);
SEXP mkFloat(char *);
SEXP mkComplex(char *);
SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);


/*
//	Basic File IO:
//
//	This code is here because at this particular instant it
//	seems closely related to cget(), which appears below.
*/


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

void uncget(int n)
{
	cnt += n;
	bufp -= n;
}


/*
//	Lexical Analyzer:
//
//	Basic lexical analysis is performed by the following
//	routines.  Input is read a line at a time, and, if the
//	program is in batch mode, each input line is echoed to
//	standard output after it is read.
//
//	The function yylex() scans the input, breaking it into
//	tokens which are then passed to the parser.  The lexical
//	analyser maintains a symbol table (in a very messy fashion).
//
//	The fact that if statements need to parse differently
//	depending on whether the statement is being interpreted or
//	part of the body of a function causes the need for ifpop
//	and ifpush. When an if statement is encountered an 'i' is
//	pushed on a stack (provided there are parentheses active).
//	At later points this 'i' needs to be popped off of the if
//	stack.
*/

static int newline = 0;
static int reset = 1;
#ifndef DEBUG_LEX
static
#endif
char *parenp, parenstack[50];

static void ifpush(void)
{
	if (*parenp == '{' || *parenp == '[' || *parenp == '(' || *parenp == 'i')
		*++parenp = 'i';
}

static void ifpop(void)
{
	if (*parenp == 'i')
		parenp--;
}

static int typeofnext(void)
{
	int k, c;

	c = cget();
	if (isdigit(c)) k = 1;
	else if (isalpha(c) || c == '.')
		k = 2;
	else
		k = 3;
	uncget(1);
	return k;
}

static int nextchar(int expect)
{
	int c = cget();

	if (c == expect)
		return 1;
	else
		uncget(1);
	return 0;
}

		/* Special Symbols */
		/* Syntactic Keywords + Symbolic Constants */

struct {
	char *name;
	int token;
} keywords[] = {
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


	/* klookup has side effects, it sets yylval */

int klookup(s)
char *s;
{
	int i;

	for (i = 0; keywords[i].name; i++) {
		if (strcmp(keywords[i].name, s) == 0) {
			switch (keywords[i].token) {
			case NULL_CONST:
				PROTECT(yylval = R_NilValue);
				eatln = 0;
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
				eatln = 0;
				break;
			case FUNCTION:
			case WHILE:
			case REPEAT:
			case FOR:
			case IF:
				eatln = 1;
				yylval = install(s);
				break;
			case IN:
				eatln = 1;
				break;
			case ELSE:
				ifpop();
				eatln = 1;
				break;
			case NEXT:
			case BREAK:
				eatln = 0;
				yylval = install(s);
				break;
			case SYMBOL:
				PROTECT(yylval = install(s));
				eatln = 0;
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

void yyerror(char *s)
{
	int i;

	R_CommentSxp = R_NilValue;
	REprintf("%s", buf);
	for (i = 1; i < bufp - buf; i++) {
		REprintf(" ");
	}
	REprintf("^\n");
	if (R_Console == 0) {
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
}

void check_formals(SEXP formlist, SEXP new)
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
		eatln = 0;
		ResetComment();
	}

	while ((c = cget()) == ' ' || c == '\t' || c == '');

	if (c == '#') {
		p = yytext;
		*p++ = c;
		while ((c = cget()) != '\n' && c != R_EOF)
			*p++ = c;
		*p = '\0';
		if(R_CommentSxp != R_NilValue) {
			f = mkChar(yytext);
			f = CONS(f, R_NilValue);
			CAR(R_CommentSxp) = listAppend(CAR(R_CommentSxp), f);
		}
	}


	if (c == R_EOF) {
		return EOF;
	}

		/* This code deals with context sensitivity to      */
		/* newlines.  The main problem is finding out       */
		/* whether a newline is followed by an ELSE clause. */
		/* This is only of importance if we are inside one  */
		/* of "(", "[", or "{".			     */

	if (c == '\n') {
		if (eatln || *parenp == '[' || *parenp == '(') {
			prompt();
			goto again;
		}
		if (*parenp == 'i') {
			prompt();
			while ((c = cget()) == ' ' || c == '\t');
			if (c == R_EOF) {
				error("unexpected end-of-file in parse\n");
			}
			if (c == '#') {
				p = yytext;
				*p++ = c;
				while ((c = cget()) != '\n' && c != R_EOF)
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
				uncget(1);
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
			uncget(1);
			if (!strncmp(bufp, "else", 4) && !isalnum(bufp[4]) && bufp[4] != '.') {
				eatln = 1;
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
		while (isdigit(c = cget()) || c == '.' || c == 'e' || c == 'E') {
			if (c == 'E' || c == 'e') {
				if (seenexp)
					break;
				seenexp = 1;
				seendot = 1;
				*p++ = c;
				c = cget();
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
			uncget(1);
		}
		eatln = 0;
		return NUM_CONST;
	}

	/* literal strings */

	if (c == '\"' || c == '\'') {
		quote = c;
		p = yytext;
		while ((c = cget()) != R_EOF && c != quote) {
			if (c == '\n') {
				uncget(1);
				return ERROR;
			}
			if (c == '\\') {
				c = cget();
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
		eatln = 0;
		return STR_CONST;
	}

	/* special functions */
	if (c == '%') {
		p = yytext;
		*p++ = c;
		while ((c = cget()) != R_EOF && c != '%') {
			if (c == '\n') {
				uncget(1);
				return ERROR;
			}
			*p++ = c;
		}
		if (c == '%')
			*p++ = c;
		*p++ = '\0';
		PROTECT(yylval = install(yytext));
		eatln=1;
		return SPECIAL;
	}


	/* functions, constants and variables */

	/* gag, barf, but the punters want it */
	if (c == '_') {
		eatln = 1;
		yylval = install("<-");
		return LEFT_ASSIGN;
	}

    symbol:
	if (c == '.' || isalpha(c)) {
		p = yytext;
		do {
			*p++ = c;
		} while ((c = cget()) != R_EOF && (isalnum(c) || c == '.'));
		uncget(1);
		*p = '\0';

		if ((kw = klookup(yytext))) {
			if(kw == FUNCTION) pushCmt();
			return kw;
		}

		PROTECT(yylval = install(yytext));
		eatln = 0;
		return SYMBOL;
	}

	/* compound tokens */

	switch (c) {
	case '<':
		eatln = 1;
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
		eatln = 1;
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
		eatln = 1;
		if (nextchar('=')) {
			yylval = install(">=");
			return GE;
		}
		yylval = install(">");
		return GT;
	case '!':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install("!=");
			return NE;
		}
		yylval = install("!");
		return '!';
	case '=':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install("==");
			return EQ;
		}
		return '=';
	case ':':
		eatln = 1;
		if (nextchar('=')) {
			yylval = install(":=");
			return LEFT_ASSIGN;
		}
		yylval = install(":");
		return ':';
	case '&':
		eatln = 1;
		if (nextchar('&')) {
			yylval = install("&&");
			return AND;
		}
		yylval = install("&");
		return AND;
	case '|':
		eatln = 1;
		if (nextchar('|')) {
			yylval = install("||");
			return OR;
		}
		yylval = install("|");
		return OR;
	case '{':
		*++parenp = c;
		yylval = install("{");
		pushCmt();
		return c;
	case '}':
		ifpop();
		if(*parenp == '{')
			popCmt();
		parenp--;
		return c;
	case '(':
		*++parenp = c;
		yylval = install("(");
		return c;
	case ')':
		eatln = 0;
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
		eatln = 0;
		parenp--;
		return c;
	case '?':
		eatln = 1;
		strcpy(yytext, "help");
		yylval = install(yytext);
		return c;
	case '*':
		eatln=1;
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
		eatln = 1;
		yytext[0] = c;
		yytext[1] = '\0';
		yylval = install(yytext);
		return c;
	default:
		return c;
	}
}
#line 1575 "y.tab.c"
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

    if ((yys = getenv("YYDEBUG")))
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
    if ((yyn = yydefred[yystate])) goto yyreduce;
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
#if defined(lint) || defined(__GNUC__)
    goto yynewerror;
#endif
yynewerror:
    yyerror("syntax error");
#if defined(lint) || defined(__GNUC__)
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
#line 67 "gram.y"
{ newline = 0; }
break;
case 2:
#line 68 "gram.y"
{ R_CurrentExpr = NULL; return 2; }
break;
case 3:
#line 69 "gram.y"
{ R_CurrentExpr = yyvsp[-1]; UNPROTECT(1); YYRETURN(3); }
break;
case 4:
#line 70 "gram.y"
{ R_CurrentExpr = yyvsp[-1]; UNPROTECT(1); YYRETURN(4); }
break;
case 5:
#line 71 "gram.y"
{ YYABORT; }
break;
case 6:
#line 74 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 7:
#line 75 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 8:
#line 76 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 9:
#line 77 "gram.y"
{ yyval = yyvsp[0]; }
break;
case 10:
#line 78 "gram.y"
{ UNPROTECT(1); TYPEOF(yyvsp[-1]) = LANGSXP; CAR(yyvsp[-1]) = yyvsp[-2]; yyval = yyvsp[-1]; PROTECT(yyval); eatln = 0; }
break;
case 11:
#line 79 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-2], yyvsp[-1]); PROTECT(yyval); }
break;
case 12:
#line 80 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 13:
#line 81 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 14:
#line 82 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 15:
#line 83 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 16:
#line 84 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 17:
#line 85 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 18:
#line 86 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 19:
#line 87 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 20:
#line 88 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 21:
#line 89 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 22:
#line 90 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 23:
#line 91 "gram.y"
{ UNPROTECT(3); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 24:
#line 92 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 25:
#line 93 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 26:
#line 94 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 27:
#line 95 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 28:
#line 96 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 29:
#line 97 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 30:
#line 98 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 31:
#line 99 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 32:
#line 100 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 33:
#line 101 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 34:
#line 102 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 35:
#line 103 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-1], yyvsp[0], yyvsp[-2]); PROTECT(yyval); }
break;
case 36:
#line 105 "gram.y"
{ addcomment(yyvsp[0]); UNPROTECT(2); yyval = lang3(yyvsp[-5], CDR(yyvsp[-3]), yyvsp[0]); PROTECT(yyval); popCmt();}
break;
case 37:
#line 106 "gram.y"
{ if(isString(yyvsp[-3])) yyvsp[-3]=install(CHAR(STRING(yyvsp[-3])[0])); UNPROTECT(2); if(length(CDR(yyvsp[-1])) == 1 && CADR(yyvsp[-1]) == R_MissingArg )
										yyval = lang1(yyvsp[-3]);
									else
										yyval = LCONS(yyvsp[-3], CDR(yyvsp[-1]));
									PROTECT(yyval); }
break;
case 38:
#line 111 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-2], yyvsp[-1], yyvsp[0]); PROTECT(yyval);  }
break;
case 39:
#line 112 "gram.y"
{ UNPROTECT(3); yyval = lang4(yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[0]); PROTECT(yyval); }
break;
case 40:
#line 113 "gram.y"
{ UNPROTECT(2); yyval = lang4(yyvsp[-2], CAR(yyvsp[-1]), CDR(yyvsp[-1]), yyvsp[0]); PROTECT(yyval); }
break;
case 41:
#line 114 "gram.y"
{ UNPROTECT(2); yyval = lang3(yyvsp[-2], yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 42:
#line 115 "gram.y"
{ UNPROTECT(1); yyval = lang2(yyvsp[-1], yyvsp[0]); PROTECT(yyval); }
break;
case 43:
#line 116 "gram.y"
{ UNPROTECT(2); yyval = LCONS(yyvsp[-3], LCONS(yyvsp[-4], CDR(yyvsp[-2]))); PROTECT(yyval); }
break;
case 44:
#line 117 "gram.y"
{ UNPROTECT(2); yyval = LCONS(yyvsp[-2], LCONS(yyvsp[-3], CDR(yyvsp[-1]))); PROTECT(yyval); }
break;
case 45:
#line 118 "gram.y"
{ yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); UNPROTECT(2); PROTECT(yyval); }
break;
case 46:
#line 119 "gram.y"
{ yyval = lang3(yyvsp[-1], yyvsp[-2], yyvsp[0]); UNPROTECT(2); PROTECT(yyval); }
break;
case 47:
#line 120 "gram.y"
{ yyval = lang1(yyvsp[0]); PROTECT(yyval); }
break;
case 48:
#line 121 "gram.y"
{ yyval = lang1(yyvsp[0]); PROTECT(yyval); }
break;
case 49:
#line 125 "gram.y"
{ yyval = yyvsp[-1];  eatln = 1; }
break;
case 50:
#line 128 "gram.y"
{ yyval = yyvsp[-1]; ifpush(); eatln = 1; }
break;
case 51:
#line 131 "gram.y"
{ UNPROTECT(2); yyval = LCONS(yyvsp[-3],yyvsp[-1]); PROTECT(yyval); eatln=1;}
break;
case 52:
#line 135 "gram.y"
{ yyval = newlist(); PROTECT(yyval); }
break;
case 53:
#line 136 "gram.y"
{ addcomment(yyvsp[0]); UNPROTECT(1); yyval = growlist(newlist(), yyvsp[0]); PROTECT(yyval);}
break;
case 54:
#line 137 "gram.y"
{ addcomment(yyvsp[0]); UNPROTECT(2); yyval = growlist(yyvsp[-2], yyvsp[0]); PROTECT(yyval);}
break;
case 55:
#line 138 "gram.y"
{ yyval = yyvsp[-1]; addcomment(CAR(yyval));}
break;
case 56:
#line 139 "gram.y"
{ addcomment(yyvsp[0]); UNPROTECT(2); yyval = growlist(yyvsp[-2], yyvsp[0]); PROTECT(yyval);}
break;
case 57:
#line 140 "gram.y"
{ yyval = yyvsp[-1];}
break;
case 58:
#line 143 "gram.y"
{ UNPROTECT(1); yyval = firstarg(CAR(yyvsp[0]),CADR(yyvsp[0])); PROTECT(yyval); }
break;
case 59:
#line 144 "gram.y"
{ UNPROTECT(2); yyval = nextarg(yyvsp[-3], CAR(yyvsp[0]), CADR(yyvsp[0])); PROTECT(yyval); }
break;
case 60:
#line 147 "gram.y"
{ yyval = lang2(R_MissingArg,R_NilValue); PROTECT(yyval); }
break;
case 61:
#line 148 "gram.y"
{ UNPROTECT(1); yyval = tagarg(yyvsp[0], R_NilValue); PROTECT(yyval); }
break;
case 62:
#line 149 "gram.y"
{ UNPROTECT(2); yyval = tagarg(yyvsp[0], yyvsp[-2]); PROTECT(yyval); }
break;
case 63:
#line 150 "gram.y"
{ UNPROTECT(1); yyval = tagarg(R_MissingArg, yyvsp[-1]); PROTECT(yyval); }
break;
case 64:
#line 151 "gram.y"
{ UNPROTECT(2); yyval = tagarg(yyvsp[0], yyvsp[-2]); PROTECT(yyval); }
break;
case 65:
#line 152 "gram.y"
{ UNPROTECT(1); yyval = tagarg(R_MissingArg, yyvsp[-1]); PROTECT(yyval); }
break;
case 66:
#line 153 "gram.y"
{ UNPROTECT(2); yyval = tagarg(yyvsp[0], install("NULL")); PROTECT(yyval); }
break;
case 67:
#line 154 "gram.y"
{ UNPROTECT(1); yyval = tagarg(R_MissingArg, install("NULL")); PROTECT(yyval); }
break;
case 68:
#line 157 "gram.y"
{ yyval = R_NilValue; PROTECT(yyval); }
break;
case 69:
#line 158 "gram.y"
{ UNPROTECT(1); yyval = firstarg(R_MissingArg, yyvsp[0]); PROTECT(yyval); }
break;
case 70:
#line 159 "gram.y"
{ UNPROTECT(2); yyval = firstarg(yyvsp[0], yyvsp[-2]); PROTECT(yyval); }
break;
case 71:
#line 160 "gram.y"
{ UNPROTECT(2); check_formals(yyvsp[-2],yyvsp[0]); yyval = nextarg(yyvsp[-2], R_MissingArg, yyvsp[0]); PROTECT(yyval); }
break;
case 72:
#line 161 "gram.y"
{ UNPROTECT(3); check_formals(yyvsp[-4],yyvsp[-2]); yyval = nextarg(yyvsp[-4], yyvsp[0], yyvsp[-2]); PROTECT(yyval); }
break;
case 73:
#line 164 "gram.y"
{eatln = 1;}
break;
#line 2012 "y.tab.c"
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
