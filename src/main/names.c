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
#include "Print.h"
#include "names.h"

#ifdef Win32
SEXP do_winedit(SEXP, SEXP, SEXP, SEXP);
SEXP do_sysfile(SEXP, SEXP, SEXP, SEXP);
SEXP do_getenv(SEXP, SEXP, SEXP, SEXP);
#endif


/*
 *	printname  c-entry  offset  eval  arity	 pp-info  mark
 *
 *	Note:	We now consider eval to be made up of two digits XY.
 *		X = 1 says that this is an internal function which must
 *		be accessed with a .Internal call, any other value is
 *		accessable directly.
 *		Y = 1 says evaluate arguments before calling and Y=0 says
 *		don't evaluate.
 *	New Apr 9/96: eval is made up of three digits XYZ. X=1 says that
 *		we should switch R_Visible off (the least common situation).
 *		Y=1 says that this is an internal function (as above) and
 *		Z=1 says evaluate arguments before calling and Z=0 says
 *		don't evaluate.
 *
 * E.g:		SEXP do_cat(SEXP, SEXP, SEXP, SEXP);
 *
 */

FUNTAB R_FunTab[] =
{
/* Language Related Constructs */

{"if",		do_if,		0,	0,	-1,	PP_IF,		0},
{"while",	do_while,	0,	0,	-1,	PP_WHILE,	0},
{"for",		do_for,		0,	0,	-1,	PP_FOR,		0},
{"repeat",	do_repeat,	0,	0,	-1,	PP_REPEAT,	0},
{"break",	do_break,	CTXT_BREAK,0,	-1,	PP_BREAK,	0},
{"next",	do_break,	CTXT_NEXT,0,	-1,	PP_NEXT,	0},
{"return",	do_return,	0,	1,	-1,	PP_RETURN,	0},
{"stop",	do_stop,	0,	1,	1,	PP_FUNCALL,	0},
{"warning",	do_warning,	0,	101,	1,	PP_FUNCALL,	0},
{"function",	do_function,	0,	0,	-1,	PP_FUNCTION,	0},
{"<-",		do_set,		1,	100,	-1,	PP_ASSIGN,	0},
{"<<-",		do_set,		2,	100,	-1,	PP_ASSIGN2,	0},
{"{",		do_begin,	0,	0,	-1,	PP_CURLY,	0},
{"(",		do_paren,	0,	1,	1,	PP_PAREN,	0},
{"[",		do_subset,	1,	0,	-1,	PP_SUBSET,	0},
{"[[",		do_subset2,	2,	0,	2,	PP_SUBSET,	0},
{"$",		do_subset3,	3,	0,	2,	PP_DOLLAR,	0},
{"[<-",		do_subassign,	0,	0,	3,	PP_SUBASS,	0},
{"[[<-",	do_subassign2,	1,	100,	3,	PP_SUBASS,	0},
{"$<-",		do_subassign3,	1,	0,	3,	PP_SUBASS,	0},
{"switch",	do_switch,	0,	10,	-1,	PP_FUNCALL,	0},
{"browser",	do_browser,	0,	100,	0,	PP_FUNCALL,	0},
{"debug",	do_debug,	0,	101,	1,	PP_FUNCALL,	0},
{"undebug",	do_debug,	1,	101,	1,	PP_FUNCALL,	0},
{"trace",	do_trace,	0,	101,	1,	PP_FUNCALL,	0},
{"untrace",	do_trace,	1,	101,	1,	PP_FUNCALL,	0},
{".Internal",	do_internal,	0,	0,	1,	PP_FUNCALL,	0},
{"on.exit",	do_onexit,	0,	100,	1,	PP_FUNCALL,	0},
{"Recall",	do_recall,	0,	10,	-1,	PP_FUNCALL,	0},
{"delay",	do_delay,	0,	11,	2,	PP_FUNCALL,	0},


/* Binary Operators */

{"+",		do_arith,	PLUSOP,	1,	2,	PP_BINARY,	0},
{"-",		do_arith,	MINUSOP,1,	2,	PP_BINARY,	0},
{"*",		do_arith,	TIMESOP,1,	2,	PP_BINARY,	0},
{"/",		do_arith,	DIVOP,	1,	2,	PP_BINARY2,	0},
{"^",		do_arith,	POWOP,	1,	2,	PP_BINARY2,	0},
{"%%",		do_arith,	MODOP,	1,	2,	PP_BINARY2,	0},
{"%/%",		do_arith,	IDIVOP,	1,	2,	PP_BINARY2,	0},
{"%*%",		do_matprod,	0,	1,	2,	PP_BINARY,	0},
{"crossprod",	do_matprod,	1,	11,	2,	PP_FUNCALL,	0},
{"==",		do_relop,	EQOP,	1,	2,	PP_BINARY,	0},
{"!=",		do_relop,	NEOP,	1,	2,	PP_BINARY,	0},
{"<",		do_relop,	LTOP,	1,	2,	PP_BINARY,	0},
{"<=",		do_relop,	LEOP,	1,	2,	PP_BINARY,	0},
{">=",		do_relop,	GEOP,	1,	2,	PP_BINARY,	0},
{">",		do_relop,	GTOP,	1,	2,	PP_BINARY,	0},
{"&",		do_logic,	1,	1,	2,	PP_BINARY,	0},
{"|",		do_logic,	2,	1,	2,	PP_BINARY,	0},
{"!",		do_logic,	3,	1,	1,	PP_UNARY,	0},
{"&&",		do_logic2,	1,	0,	2,	PP_BINARY,	0},
{"||",		do_logic2,	2,	0,	2,	PP_BINARY,	0},
{":",		do_seq,		0,	1,	2,	PP_BINARY2,	0},
{"~",		do_tilde,	0,	0,	2,	PP_BINARY,	0},


/* Logic Related Functions */

{"all",		do_logic3,	1,	11,	-1,	PP_FUNCALL,	0},
{"any",		do_logic3,	2,	11,	-1,	PP_FUNCALL,	0},


/* Vectors, Matrices and Arrays */

{"vector",	do_makevector,	0,	1,	2,	PP_FUNCALL,	0},
{"factor",	do_makefactor,	0,	11,	3,	PP_FUNCALL,	0},
{"complex",	do_complex,	0,	11,	3,	PP_FUNCALL,	0},
{"matrix",	do_matrix,	0,	11,	4,	PP_FUNCALL,	0},
{"array",	do_array,	0,	1,	2,	PP_FUNCALL,	0},
{"length",	do_length,	0,	1,	1,	PP_FUNCALL,	0},
{"length<-",	do_lengthgets,	0,	1,	2,	PP_FUNCALL,	0},
{"nlevels",	do_nlevels,	0,	1,	1,	PP_FUNCALL,	0},
{"row",		do_rowscols,	1,	11,	1,	PP_FUNCALL,	0},
{"col",		do_rowscols,	2,	11,	1,	PP_FUNCALL,	0},
{"c",		do_c,		0,	0,	-1,	PP_FUNCALL,	0},
{"unlist",	do_c,		1,	0,	-1,	PP_FUNCALL,	0},
{"cbind",	do_bind,	1,	11,	-1,	PP_FUNCALL,	0},
{"rbind",	do_bind,	2,	1,	-1,	PP_FUNCALL,	0},
{"drop",	do_drop,	0,	1,	1,	PP_FUNCALL,	0},
{"class",	do_class,	0,	1,	1,	PP_FUNCALL,	0},
{"class<-",	do_classgets,	0,	1,	2,	PP_FUNCALL,	0},
{"unclass",	do_unclass,	0,	1,	1,	PP_FUNCALL,	0},
{"names",	do_names,	0,	11,	1,	PP_FUNCALL,	0},
{"names<-",	do_namesgets,	0,	11,	2,	PP_FUNCALL,	0},
{"dimnames",	do_dimnames,	0,	1,	1,	PP_FUNCALL,	0},
{"dimnames<-",	do_dimnamesgets,0,	1,	2,	PP_FUNCALL,	0},
{"all.names",	do_allnames,	0,	11,	4,	PP_FUNCALL,	0},
{"row.names",	do_rownames,	0,	1,	1,	PP_FUNCALL,	0},
{"dim",		do_dim,		0,	1,	1,	PP_FUNCALL,	0},
{"dim<-",	do_dimgets,	0,	1,	2,	PP_FUNCALL,	0},
{"levels",	do_levels,	0,	1,	1,	PP_FUNCALL,	0},
{"levels<-",	do_levelsgets,	0,	1,	2,	PP_FUNCALL,	0},
{"attributes",	do_attributes,	0,	1,	1,	PP_FUNCALL,	0},
{"attributes<-",do_attributesgets,0,	1,	1,	PP_FUNCALL,	0},
{"attr",	do_attr,	0,	1,	2,	PP_FUNCALL,	0},
{"attr<-",	do_attrgets,	0,	0,	3,	PP_FUNCALL,	0},
{"comment",	do_comment,	0,	1,	1,	PP_FUNCALL,	0},
{"comment<-",	do_commentgets,	0,	1,	2,	PP_FUNCALL,	0},
{"get",		do_get,		1,	10,	4,	PP_FUNCALL,	0},
{"exists",	do_get,		0,	10,	4,	PP_FUNCALL,	0},
{"assign",	do_assign,	0,	110,	4,	PP_FUNCALL,	0},
{"remove",	do_remove,	0,	111,	3,	PP_FUNCALL,	0},
{"duplicated",	do_duplicated,	0,	1,	1,	PP_FUNCALL,	0},
{"unique",	do_duplicated,	1,	1,	1,	PP_FUNCALL,	0},
{"match",	do_match,	0,	11,	3,	PP_FUNCALL,	0},
{"pmatch",	do_pmatch,	0,	11,	2,	PP_FUNCALL,	0},
{"match.call",	do_matchcall,	0,	11,	3,	PP_FUNCALL,	0},
{"complete.cases",do_compcases,	0,	11,	1,	PP_FUNCALL,	0},
{"attach",	do_attach,	0,	111,	3,	PP_FUNCALL,	0},
{"detach",	do_detach,	0,	111,	1,	PP_FUNCALL,	0},
{"search",	do_search,	0,	1,	0,	PP_FUNCALL,	0},


/* Mathematical Functions */
{"round",	do_round,	10001,	1,	1,	PP_FUNCALL,	0},
{"atan",	do_atan,	10002,	1,	1,	PP_FUNCALL,	0},
{"log",		do_log,		10003,	1,	1,	PP_FUNCALL,	0},

{"abs",		do_math1,	0,	1,	1,	PP_FUNCALL,	0},
{"floor",	do_math1,	1,	1,	1,	PP_FUNCALL,	0},
{"ceiling",	do_math1,	2,	1,	1,	PP_FUNCALL,	0},
{"sqrt",	do_math1,	3,	1,	1,	PP_FUNCALL,	0},
{"sign",	do_math1,	4,	1,	1,	PP_FUNCALL,	0},
{"trunc",	do_math1,	5,	1,	1,	PP_FUNCALL,	0},

{"exp",		do_math1,	10,	1,	1,	PP_FUNCALL,	0},

{"cos",		do_math1,	20,	1,	1,	PP_FUNCALL,	0},
{"sin",		do_math1,	21,	1,	1,	PP_FUNCALL,	0},
{"tan",		do_math1,	22,	1,	1,	PP_FUNCALL,	0},
{"acos",	do_math1,	23,	1,	1,	PP_FUNCALL,	0},
{"asin",	do_math1,	24,	1,	1,	PP_FUNCALL,	0},

{"cosh",	do_math1,	30,	1,	1,	PP_FUNCALL,	0},
{"sinh",	do_math1,	31,	1,	1,	PP_FUNCALL,	0},
{"tanh",	do_math1,	32,	1,	1,	PP_FUNCALL,	0},
{"acosh",	do_math1,	33,	1,	1,	PP_FUNCALL,	0},
{"asinh",	do_math1,	34,	1,	1,	PP_FUNCALL,	0},
{"atanh",	do_math1,	35,	1,	1,	PP_FUNCALL,	0},

{"lgamma",	do_math1,	40,	1,	1,	PP_FUNCALL,	0},
{"gamma",	do_math1,	41,	1,	1,	PP_FUNCALL,	0},

/* Polygamma Functions */

{"digamma",	do_math1,	42,	1,	1,	PP_FUNCALL,	0},
{"trigamma",	do_math1,	43,	1,	1,	PP_FUNCALL,	0},
{"tetragamma",	do_math1,	44,	1,	1,	PP_FUNCALL,	0},
{"pentagamma",	do_math1,	45,	1,	1,	PP_FUNCALL,	0},

/* Mathematical Functions of Two Variables */

{"atan2",	do_math2,	0,	1,	2,	PP_FUNCALL,	0},
{"signif",	do_math2,	1,	1,	2,	PP_FUNCALL,	0},

{"lbeta",	do_math2,	2,	1,	2,	PP_FUNCALL,	0},
{"beta",	do_math2,	3,	1,	2,	PP_FUNCALL,	0},
{"lchoose",	do_math2,	4,	1,	2,	PP_FUNCALL,	0},
{"choose",	do_math2,	5,	1,	2,	PP_FUNCALL,	0},

{"dchisq",	do_math2,	6,	11,	2,	PP_FUNCALL,	0},
{"pchisq",	do_math2,	7,	11,	2,	PP_FUNCALL,	0},
{"qchisq",	do_math2,	8,	11,	2,	PP_FUNCALL,	0},

{"dexp",	do_math2,	9,	11,	2,	PP_FUNCALL,	0},
{"pexp",	do_math2,	10,	11,	2,	PP_FUNCALL,	0},
{"qexp",	do_math2,	11,	11,	2,	PP_FUNCALL,	0},

{"dgeom",	do_math2,	12,	11,	2,	PP_FUNCALL,	0},
{"pgeom",	do_math2,	13,	11,	2,	PP_FUNCALL,	0},
{"qgeom",	do_math2,	14,	11,	2,	PP_FUNCALL,	0},

{"dpois",	do_math2,	15,	11,	2,	PP_FUNCALL,	0},
{"ppois",	do_math2,	16,	11,	2,	PP_FUNCALL,	0},
{"qpois",	do_math2,	17,	11,	2,	PP_FUNCALL,	0},

{"dt",		do_math2,	18,	11,	2,	PP_FUNCALL,	0},
{"pt",		do_math2,	19,	11,	2,	PP_FUNCALL,	0},
{"qt",		do_math2,	20,	11,	2,	PP_FUNCALL,	0},


#ifdef COMPLEX_DATA
/* Mathematical Functions of a Complex Argument */

{"Re",		do_cmathfuns,	1,	1,	1,	PP_FUNCALL,	0},
{"Im",		do_cmathfuns,	2,	1,	1,	PP_FUNCALL,	0},
{"Mod",		do_cmathfuns,	3,	1,	1,	PP_FUNCALL,	0},
{"Arg",		do_cmathfuns,	4,	1,	1,	PP_FUNCALL,	0},
{"Conj",	do_cmathfuns,	5,	1,	1,	PP_FUNCALL,	0},

#endif

/* Mathematical Functions of Three Variables */

{"dbeta",	do_math3,	1,	11,	3,	PP_FUNCALL,	0},
{"pbeta",	do_math3,	2,	11,	3,	PP_FUNCALL,	0},
{"qbeta",	do_math3,	3,	11,	3,	PP_FUNCALL,	0},

{"dbinom",	do_math3,	4,	11,	3,	PP_FUNCALL,	0},
{"pbinom",	do_math3,	5,	11,	3,	PP_FUNCALL,	0},
{"qbinom",	do_math3,	6,	11,	3,	PP_FUNCALL,	0},

{"dcauchy",	do_math3,	7,	11,	3,	PP_FUNCALL,	0},
{"pcauchy",	do_math3,	8,	11,	3,	PP_FUNCALL,	0},
{"qcauchy",	do_math3,	9,	11,	3,	PP_FUNCALL,	0},

{"df",		do_math3,	10,	11,	3,	PP_FUNCALL,	0},
{"pf",		do_math3,	11,	11,	3,	PP_FUNCALL,	0},
{"qf",		do_math3,	12,	11,	3,	PP_FUNCALL,	0},

{"dgamma",	do_math3,	13,	11,	3,	PP_FUNCALL,	0},
{"pgamma",	do_math3,	14,	11,	3,	PP_FUNCALL,	0},
{"qgamma",	do_math3,	15,	11,	3,	PP_FUNCALL,	0},

{"dlnorm",	do_math3,	16,	11,	3,	PP_FUNCALL,	0},
{"plnorm",	do_math3,	17,	11,	3,	PP_FUNCALL,	0},
{"qlnorm",	do_math3,	18,	11,	3,	PP_FUNCALL,	0},

{"dlogis",	do_math3,	19,	11,	3,	PP_FUNCALL,	0},
{"plogis",	do_math3,	20,	11,	3,	PP_FUNCALL,	0},
{"qlogis",	do_math3,	21,	11,	3,	PP_FUNCALL,	0},

{"dnbinom",	do_math3,	22,	11,	3,	PP_FUNCALL,	0},
{"pnbinom",	do_math3,	23,	11,	3,	PP_FUNCALL,	0},
{"qnbinom",	do_math3,	24,	11,	3,	PP_FUNCALL,	0},

{"dnorm",	do_math3,	25,	11,	3,	PP_FUNCALL,	0},
{"pnorm",	do_math3,	26,	11,	3,	PP_FUNCALL,	0},
{"qnorm",	do_math3,	27,	11,	3,	PP_FUNCALL,	0},

{"dunif",	do_math3,	28,	11,	3,	PP_FUNCALL,	0},
{"punif",	do_math3,	29,	11,	3,	PP_FUNCALL,	0},
{"qunif",	do_math3,	30,	11,	3,	PP_FUNCALL,	0},

{"dweibull",	do_math3,	31,	11,	3,	PP_FUNCALL,	0},
{"pweibull",	do_math3,	32,	11,	3,	PP_FUNCALL,	0},
{"qweibull",	do_math3,	33,	11,	3,	PP_FUNCALL,	0},

{"dnchisq",	do_math3,	34,	11,	3,	PP_FUNCALL,	0},
{"pnchisq",	do_math3,	35,	11,	3,	PP_FUNCALL,	0},
{"qnchisq",	do_math3,	36,	11,	3,	PP_FUNCALL,	0},


/* Mathematical Functions of Four Variables */

{"dhyper",	do_math4,	1,	11,	4,	PP_FUNCALL,	0},
{"phyper",	do_math4,	2,	11,	4,	PP_FUNCALL,	0},
{"qhyper",	do_math4,	3,	11,	4,	PP_FUNCALL,	0},


/* Random Numbers */

{"rchisq",	do_random1,	0,	11,	2,	PP_FUNCALL,	0},
{"rexp",	do_random1,	1,	11,	2,	PP_FUNCALL,	0},
{"rgeom",	do_random1,	2,	11,	2,	PP_FUNCALL,	0},
{"rpois",	do_random1,	3,	11,	2,	PP_FUNCALL,	0},
{"rt",		do_random1,	4,	11,	2,	PP_FUNCALL,	0},

{"rbeta",	do_random2,	0,	11,	3,	PP_FUNCALL,	0},
{"rbinom",	do_random2,	1,	11,	3,	PP_FUNCALL,	0},
{"rcauchy",	do_random2,	2,	11,	3,	PP_FUNCALL,	0},
{"rf",		do_random2,	3,	11,	3,	PP_FUNCALL,	0},
{"rgamma",	do_random2,	4,	11,	3,	PP_FUNCALL,	0},
{"rlnorm",	do_random2,	5,	11,	3,	PP_FUNCALL,	0},
{"rlogis",	do_random2,	6,	11,	3,	PP_FUNCALL,	0},
{"rnbinom",	do_random2,	7,	11,	3,	PP_FUNCALL,	0},
{"rnorm",	do_random2,	8,	11,	3,	PP_FUNCALL,	0},
{"runif",	do_random2,	9,	11,	3,	PP_FUNCALL,	0},
{"rweibull",	do_random2,	10,	11,	3,	PP_FUNCALL,	0},

{"rhyper",	do_random3,	0,	11,	4,	PP_FUNCALL,	0},

{"sample",	do_sample,	0,	11,	3,	PP_FUNCALL,	0},


/* Data Summaries */

{"sum",		do_summary,	0,	11,	1,	PP_FUNCALL,	0},
{"mean",	do_summary,	1,	11,	1,	PP_FUNCALL,	0},
{"min",		do_summary,	2,	11,	1,	PP_FUNCALL,	0},
{"max",		do_summary,	3,	11,	1,	PP_FUNCALL,	0},
{"prod",	do_summary,	4,	11,	1,	PP_FUNCALL,	0},
{"cov",		do_cov,		0,	11,	3,	PP_FUNCALL,	0},
{"cor",		do_cov,		1,	11,	3,	PP_FUNCALL,	0},

{"cumsum",	do_cum,		1,	1,	1,	PP_FUNCALL,	0},
{"cumprod",	do_cum,		2,	1,	1,	PP_FUNCALL,	0},
{"cummax",	do_cum,		3,	1,	1,	PP_FUNCALL,	0},
{"cummin",	do_cum,		4,	1,	1,	PP_FUNCALL,	0},

/* Type coercion */

{"as.factor",	do_as,		FACTSXP,11,	1,	PP_FUNCALL,	0},
{"as.unordered",do_as,		FACTSXP,11,	1,	PP_FUNCALL,	0},
{"as.ordered",	do_as,		ORDSXP,	11,	1,	PP_FUNCALL,	0},
{"as.vector",	do_as,		101,	11,	2,	PP_FUNCALL,	0},
{"as.name",	do_as,		102,	11,	1,	PP_FUNCALL,	0},
{"as.matrix.data.frame", do_asmatrixdf,	0, 11,	1,	PP_FUNCALL,	0},
{"codes",	do_codes,	0,	1,	1,	PP_FUNCALL,	0},
{"codes<-",	do_codesgets,	0,	1,	2,	PP_FUNCALL,	0},
{"paste",	do_paste,	0,	11,	3,	PP_FUNCALL,	0},
{"format",	do_format,	0,	1,	-1,	PP_FUNCALL,	0},
{"format.info",	do_formatinfo,	0,	1,	1,	PP_FUNCALL,	0},
{"cat",		do_cat,		0,	111,	6,	PP_FUNCALL,	0},
{"call",	do_call,	0,	0,	-1,	PP_FUNCALL,	0},
{"do.call",	do_docall,	0,	1,	2,	PP_FUNCALL,	0},
{"as.call",	do_ascall,	0,	1,	1,	PP_FUNCALL,	0},
{"type.convert",do_typecvt,	1,	11,	3,	PP_FUNCALL,	0},


/* String Manipulation */

{"nchar",	do_nchar,	1,	1,	1,	PP_FUNCALL,	0},
{"substr",	do_substr,	1,	11,	3,	PP_FUNCALL,	0},
{"strsplit",	do_strsplit,	1,	11,	2,	PP_FUNCALL,	0},
{"abbreviate",	do_abbrev,	1,	11,	3,	PP_FUNCALL,	0},
{"grep",	do_grep,	1,	11,	5,	PP_FUNCALL,	0},
{"sub",		do_gsub,	0,	11,	5,	PP_FUNCALL,	0},
{"gsub",	do_gsub,	1,	11,	5,	PP_FUNCALL,	0},


/* Type Checking */

{"is.null",	do_is,		NILSXP,	1,	1,	PP_FUNCALL,	0},
{"is.logical",	do_is,		LGLSXP,	1,	1,	PP_FUNCALL,	0},
{"is.unordered",do_is,		FACTSXP,1,	1,	PP_FUNCALL,	0},
{"is.ordered",	do_is,		ORDSXP,	1,	1,	PP_FUNCALL,	0},
{"is.integer",	do_is,		INTSXP,	1,	1,	PP_FUNCALL,	0},
{"is.real",	do_is,		REALSXP,1,	1,	PP_FUNCALL,	0},
{"is.double",	do_is,		REALSXP,1,	1,	PP_FUNCALL,	0},
{"is.complex",	do_is,		CPLXSXP,1,	1,	PP_FUNCALL,	0},
{"is.character",do_is,		STRSXP,	1,	1,	PP_FUNCALL,	0},
{"is.name",	do_is,		SYMSXP,	1,	1,	PP_FUNCALL,	0},
{"is.environment",do_is,	ENVSXP,	1,	1,	PP_FUNCALL,	0},
{"is.list",	do_is,		LISTSXP,1,	1,	PP_FUNCALL,	0},
{"is.expression",do_is,		EXPRSXP,1,	1,	PP_FUNCALL,	0},

{"is.object",	do_is,		50,	1,	1,	PP_FUNCALL,	0},
{"is.factor",	do_is,		75,	1,	1,	PP_FUNCALL,	0},

{"is.numeric",	do_is,		100,	1,	1,	PP_FUNCALL,	0},
{"is.matrix",	do_is,		101,	1,	1,	PP_FUNCALL,	0},
{"is.array",	do_is,		102,	1,	1,	PP_FUNCALL,	0},
{"is.ts",	do_is,		103,	1,	1,	PP_FUNCALL,	0},

{"is.atomic",	do_is,		200,	1,	1,	PP_FUNCALL,	0},
{"is.recursive",do_is,		201,	1,	1,	PP_FUNCALL,	0},

{"is.call",	do_is,		300,	1,	1,	PP_FUNCALL,	0},
{"is.language",	do_is,		301,	1,	1,	PP_FUNCALL,	0},
{"is.function",	do_is,		302,	1,	1,	PP_FUNCALL,	0},

{"is.single",	do_is,		999,	1,	1,	PP_FUNCALL,	0},

{"is.vector",	do_isvector,	0,	11,	2,	PP_FUNCALL,	0},
{"is.na",	do_isna,	0,	1,	1,	PP_FUNCALL,	0},


/* Miscellaneous */

#ifdef HAVE_TIMES
{"proc.time",	do_proctime,	0,	1,	0,	PP_FUNCALL,	0},
#endif
{"Version",	do_version,	0,	1,	0,	PP_FUNCALL,	0},
{"machine",	do_machine,	0,	1,	0,	PP_FUNCALL,	0},
{"Machine",	do_Machine,	0,	1,	0,	PP_FUNCALL,	0},
{"system",	do_system,	0,	11,	2,	PP_FUNCALL,	0},
#ifdef Win32
{"system.file",	do_sysfile,	0,	11,	2,	PP_FUNCALL,	0},
{"getenv",	do_getenv,	0,	11,	1,	PP_FUNCALL,	0},
#endif
{"parse",	do_parse,	0,	11,	4,	PP_FUNCALL,	0},
{"save",	do_save,	0,	111,	 3,	 PP_FUNCALL,	 0},
{"load",	do_load,	0,	111,	 1,	 PP_FUNCALL,	 0},
{"deparse",	do_deparse,	0,	1,	2,	PP_FUNCALL,	0},
{"dput",	do_dput,	0,	111,	2,	PP_FUNCALL,	0},
{"dump",	do_dump,	0,	111,	2,	PP_FUNCALL,	0},
{"substitute",	do_substitute,	0,	0,	-1,	PP_FUNCALL,	0},
{"quit",	do_quit,	0,	111,	1,	PP_FUNCALL,	0},
{"interactive",	do_interactive,	0,	0,	0,	PP_FUNCALL,	0},
{"readline",	do_readln,	0,	1,	0,	PP_FUNCALL,	0},
{"menu",	do_menu,	0,	11,	1,	PP_FUNCALL,	0},
{"print.default",do_printdefault,0,	111,	5,	PP_FUNCALL,	0},
{"prmatrix",	do_printmatrix, 0,	111,	5,	PP_FUNCALL,	0},
{"invisible",	do_invisible,	0,	101,	1,	PP_FUNCALL,	0},
{"gc",		do_gc,		0,	101,	0,	PP_FUNCALL,	0},
{"gcinfo",	do_gcinfo,	0,	101,	1,	PP_FUNCALL,	0},
{"rep",		do_rep,		0,	11,	2,	PP_FUNCALL,	0},
{"list",	do_makelist,	1,	1,	-1,	PP_FUNCALL,	0},
{"split",	do_split,	0,	11,	2,	PP_FUNCALL,	0},
{"symbol.C",	do_symbol,	0,	1,	1,	PP_FOREIGN,	0},
{"symbol.For",	do_symbol,	1,	1,	1,	PP_FOREIGN,	0},
{"is.loaded",	do_isloaded,	0,	1,	1,	PP_FOREIGN,	0},
{".C",		do_dotCode,	0,	1,	-1,	PP_FOREIGN,	0},
{".Fortran",	do_dotCode,	1,	1,	-1,	PP_FOREIGN,	0},
{"dyn.load",	do_dynload,	0,	111,	1,	PP_FUNCALL,	0},
{"dyn.unload",	do_dynunload,	0,	111,	1,	PP_FUNCALL,	0},
{"ls",		do_ls,		1,	11,	2,	PP_FUNCALL,	0},
{"typeof",	do_typeof,	1,	1,	1,	PP_FUNCALL,	0},
{"eval",	do_eval,	1,	11,	2,	PP_FUNCALL,	0},
{"expression",	do_expression,	1,	0,	-1,	PP_FUNCALL,	0},
{"sys.parent",	do_sys,		1,	10,	-1,	PP_FUNCALL,	0},
{"sys.call",	do_sys,		2,	10,	-1,	PP_FUNCALL,	0},
{"sys.frame",	do_sys,		3,	10,	-1,	PP_FUNCALL,	0},
{"sys.nframe",	do_sys,		4,	10,	-1,	PP_FUNCALL,	0},
{"sys.calls",	do_sys,		5,	10,	-1,	PP_FUNCALL,	0},
{"sys.frames",	do_sys,		6,	10,	-1,	PP_FUNCALL,	0},
{"sys.on.exit",	do_sys,		7,	10,	-1,	PP_FUNCALL,	0},
{"sys.parents",	do_sys,		8,	10,	-1,	PP_FUNCALL,	0},
{"sys.function",do_sys,		9,	10,	-1,	PP_FUNCALL,	0},
{"sort",	do_sort,	1,	11,	1,	PP_FUNCALL,	0},
{"psort",	do_psort,	0,	11,	2,	PP_FUNCALL,	0},
{"order",	do_order,	0,	1,	-1,	PP_FUNCALL,	0},
{"rank",	do_rank,	0,	1,	1,	PP_FUNCALL,	0},
{"missing",	do_missing,	1,	0,	1,	PP_FUNCALL,	0},
{"nargs",	do_nargs,	1,	0,	0,	PP_FUNCALL,	0},
{"scan",	do_scan,	0,	11,	10,	PP_FUNCALL,	0},
{"count.fields",do_countfields,	0,	11,	3,	PP_FUNCALL,	0},
{"t.default",	do_transpose,	0,	1,	1,	PP_FUNCALL,	0},
{"aperm",	do_aperm,	0,	11,	3,	PP_FUNCALL,	0},
{"builtins",	do_builtins,	0,	11,	1,	PP_FUNCALL,	0},
{"edit",	do_edit,	0,	11,	3,	PP_FUNCALL,	0},
{"dataentry",	do_dataentry,	0,	1,	1,	PP_FUNCALL,	0},
{"args",	do_args,	0,	1,	1,	PP_FUNCALL,	0},
{"formals",	do_formals,	0,	11,	1,	PP_FUNCALL,	0},
{"body",	do_body,	0,	11,	1,	PP_FUNCALL,	0},
{"globalenv",	do_globalenv,	0,	1,	0,	PP_FUNCALL,	0},
{"environment",	do_envir,	0,	11,	1,	PP_FUNCALL,	0},
{"environment<-",do_envirgets,	0,	1,	2,	PP_FUNCALL,	0},
{"options",	do_options,	0,	11,	1,	PP_FUNCALL,	0},
{"check.bounds",do_checkbounds,	0,	1,	1,	PP_FUNCALL,	0},
{"sink",	do_sink,	0,	101,	1,	PP_FUNCALL,	0},
{"lib.fixup",	do_libfixup,	0,	101,	2,	PP_FUNCALL,	0},
{"pos.to.env",	do_pos2env,	0,	1,	1,	PP_FUNCALL,	0},

/* Data Frames */

{"data.frame",	do_dataframe,	0,	11,	3,	PP_FUNCALL,	0},
{"is.data.frame",do_is,		80,	1,	1,	PP_FUNCALL,	0},
{"[.data.frame",do_subsetdf,	0,	0,	-1,	PP_FUNCALL,	0},
{"[[.data.frame",do_subsetdf2,	0,	0,	-1,	PP_FUNCALL,	0},
{"[<-.data.frame",do_subassigndf,0,	0,	-1,	PP_FUNCALL,	0},
{"[[<-.data.frame",do_subassigndf2,0,	0,	-1,	PP_FUNCALL,	0},
{"print.data.frame",do_printdf,	0,	101,	-1,	PP_FUNCALL,	0},
{"any.data.frame",do_anydf,	0,	1,	-1,	PP_FUNCALL,	0},

/* Complex Valued Functions */
#ifdef COMPLEX_DATA
{"fft",		do_fft,		0,	11,	2,	PP_FUNCALL,	0},
{"mvfft",	do_mvfft,	0,	11,	2,	PP_FUNCALL,	0},
{"polyroot",	do_polyroot,	0,	1,	1,	PP_FUNCALL,	0},
#endif
{"nextn",	do_nextn,	0,	11,	2,	PP_FUNCALL,	0},

/* Graphics */

{"device",	do_device,	0,	111,	3,	PP_FUNCALL,	0},
{"dev.off",	do_devoff,	0,	101,	0,	PP_FUNCALL,	0},
{"rgb",		do_rgb,		0,	11,	4,	PP_FUNCALL,	0},
{"hsv",		do_hsv,		0,	11,	4,	PP_FUNCALL,	0},
{"gray",	do_gray,	0,	1,	1,	PP_FUNCALL,	0},
{"colors",	do_colors,	0,	1,	0,	PP_FUNCALL,	0},
{"palette",	do_palette,	0,	11,	1,	PP_FUNCALL,	0},
{"plot.new",	do_plot_new,	0,	111,	1,	PP_FUNCALL,	0},
{"plot.window",	do_plot_window,	0,	101,	3,	PP_FUNCALL,	0},
{"axis",	do_axis,	0,	111,	7,	PP_FUNCALL,	0},
{"plot.xy",	do_plot_xy,	0,	111,	6,	PP_FUNCALL,	0},
{"text",	do_text,	0,	111,	6,	PP_FUNCALL,	0},
{"mtext",	do_mtext,	0,	111,	5,	PP_FUNCALL,	0},
{"title",	do_title,	0,	111,	4,	PP_FUNCALL,	0},
{"abline",	do_abline,	0,	111,	6,	PP_FUNCALL,	0},
{"box",		do_box,		0,	111,	3,	PP_FUNCALL,	0},
{"rect",	do_rect,	0,	111,	6,	PP_FUNCALL,	0},
{"polygon",	do_polygon,	0,	111,	5,	PP_FUNCALL,	0},
{"par",		do_par,		0,	11,	1,	PP_FUNCALL,	0},
{"par2",	do_par2,	0,	11,	1,	PP_FUNCALL,	0},
{"segments",	do_segments,	0,	111,	6,	PP_FUNCALL,	0},
{"arrows",	do_arrows,	0,	111,	9,	PP_FUNCALL,	0},
{"locator",	do_locator,	0,	11,	1,	PP_FUNCALL,	0},
{"identify",	do_identify,	0,	11,	3,	PP_FUNCALL,	0},
{"strheight",   do_strheight,   0,      11,     3, 	PP_FUNCALL, 	0},
{"strwidth",	do_strwidth,	0,	11,	3,	PP_FUNCALL,	0},
{"contour",	do_contour,	0,	11,	6,	PP_FUNCALL,	0},
{"image",	do_image,	0,	11,	5,	PP_FUNCALL,	0},
{"dend",	do_dend,	0,	111,	6,	PP_FUNCALL,	0},
{"save.plot",	do_saveplot,	0,	101,	1,	PP_FUNCALL,	0},
{"print.plot",	do_printplot,	0,	101,	0,	PP_FUNCALL,	0},
{"text.math",	do_mathtext,	0,	100,	1,	PP_FUNCALL,	0},
{"erase",	do_erase,	0,	111,	1,	PP_FUNCALL,	0},

/* Objects */
{"UseMethod",	do_usemethod,	0,	 0,	-1,	PP_FUNCALL,	0},
{"NextMethod",	do_nextmethod,	0,	10,	-1,	PP_FUNCALL,	0},

/* Modelling Functionality */

{"nlm",		do_nlm,		0,	11,	11,	PP_FUNCALL,	0},
{"fmin",	do_fmin,	0,	11,	4,	PP_FUNCALL,	0},
{"zeroin",	do_zeroin,	0,	11,	4,	PP_FUNCALL,	0},
{"terms.formula",do_termsform,	0,	11,	5,	PP_FUNCALL,	0},
{"update.formula",do_updateform,0,	1,	2,	PP_FUNCALL,	0},
{"model.frame",	do_modelframe,	0,	11,	5,	PP_FUNCALL,	0},
{"model.matrix",do_modelmatrix,	0,	11,	2,	PP_FUNCALL,	0},

{"D",		do_D,		0,	1,	2,	PP_FUNCALL,	0},
{"deriv.default",do_deriv,	0,	11,	4,	PP_FUNCALL,	0},

{NULL,		NULL,		0,	0,	0,	0,		0},
};

int StrToInternal(char *s)
{
	int i;
	for (i = 0; R_FunTab[i].name; i++)
		if(strcmp(s, R_FunTab[i].name) == 0) return i;
	return 0;
}

/* string hashing */
int hashpjw(s)
char *s;
{
	char *p;
	unsigned h = 0, g;
	for (p = s; *p; p = p + 1) {
		h = (h << 4) + (*p);
		if ((g = h & 0xf0000000) != 0) {
			h = h ^ (g >> 24);
			h = h ^ g;
		}
	}
	return h % HSIZE;
}

extern void installFunTab(int i)
{

	if ( (R_FunTab[i].eval % 100 )/10 )
		INTERNAL(install(R_FunTab[i].name)) = mkPRIMSXP(i, R_FunTab[i].eval % 10);
	else
		SYMVALUE(install(R_FunTab[i].name)) = mkPRIMSXP(i, R_FunTab[i].eval % 10);
	/*
	printf("%d %s %d\n", i, R_FunTab[i].name, R_FunTab[i].code);
	*/
}

void SymbolShortcuts()
{
	R_Bracket2Symbol = install("[[");
	R_BracketSymbol = install("[");
	R_ClassSymbol = install("class");
	R_DimNamesSymbol = install("dimnames");
	R_DimSymbol = install("dim");
	R_DollarSymbol = install("$");
	R_DotsSymbol = install("...");
	R_DropSymbol = install("drop");
	R_LevelsSymbol = install("levels");
	R_ModeSymbol = install("mode");
	R_NamesSymbol = install("names");
	R_NaRmSymbol = install("na.rm");
	R_RowNamesSymbol = install("row.names");
	R_SeedsSymbol = install(".Random.seed");
	R_LastvalueSymbol = install(".Last.value");
	R_TspSymbol = install("tsp");
	R_CommentSymbol = install("comment");
}

/* initialize the symbol table */
void InitNames()
{
	int i;

	/* THIS MUST BE THE FIRST CONS CELL ALLOCATED */
	/* OR ARMAGEDON HAPPENS. */

	R_NilValue = allocSExp(NILSXP);
	CAR(R_NilValue) = R_NilValue;
	CDR(R_NilValue) = R_NilValue;
	TAG(R_NilValue) = R_NilValue;
	ATTRIB(R_NilValue) = R_NilValue;

	R_UnboundValue = allocSExp(SYMSXP);
	SYMVALUE(R_UnboundValue) = R_UnboundValue;
	PRINTNAME(R_UnboundValue) = R_NilValue;
	ATTRIB(R_UnboundValue) = R_NilValue;

	R_MissingArg = allocSExp(SYMSXP);
	SYMVALUE(R_MissingArg) = R_MissingArg;
	PRINTNAME(R_MissingArg) = mkChar("");
	ATTRIB(R_MissingArg) = R_NilValue;

	R_CommentSxp = R_NilValue;
	R_ParseText = R_NilValue;

	/* changed from mkChar so mkChar can see if it is getting "NA" */
	/* and then retrun NA_STRING rather than alloc a new CHAR */

	NA_STRING = allocString(strlen("NA"));
	strcpy(CHAR(NA_STRING), "NA");
	print_na_string = NA_STRING;

	if (!(R_SymbolTable = (SEXP *) malloc(HSIZE * sizeof(SEXP))))
		R_Suicide("couldn't allocate memory for symbol table");

	for (i = 0; i < HSIZE; i++)
		R_SymbolTable[i] = R_NilValue;

	/* Sets up a set of globals so that a symbol table */
	/* search can be avoided when matching something like */
	/* dim or dimnames */

	SymbolShortcuts();

	for (i = 0; R_FunTab[i].name; i++)
		installFunTab(i);
}

/* install - probe the symbol table */
/* If name is not found, install it. */
/* Returns the symbol corresponding to the string "name". */
SEXP install(char *name)
{
	SEXP sym;
	int i;

	if(*name == '\0')
		error("attempt to use zero-length variable name\n");

	i = hashpjw(name);

		/* check to see if the symbol is already there */
	for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym))
		if (strcmp(name, CHAR(PRINTNAME(CAR(sym)))) == 0)
			return (CAR(sym));

		/* make a new symbol node and link it into the list */
	sym = mkSYMSXP(mkChar(name), R_UnboundValue);
	R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
	return (sym);
}

SEXP do_internal(SEXP call, SEXP op, SEXP args, SEXP env)
{
	SEXP s, fun;

	int save = R_PPStackTop;

	checkArity(op, args);

	s = CAR(args);
	fun = CAR(s);
	if (!isSymbol(fun))
		errorcall(call, "invalid internal function\n");

	if (INTERNAL(fun) == R_NilValue)
		errorcall(call, "no internal function \"%s\"\n", CHAR(PRINTNAME(fun)));
	args = CDR(s);
	if (TYPEOF(INTERNAL(fun)) == BUILTINSXP)
		args = evalList(args, env);
	PROTECT(args);
	R_Visible = 1 - PRIMPRINT(INTERNAL(fun));
	args = PRIMFUN(INTERNAL(fun)) (s, INTERNAL(fun), args, env);
	UNPROTECT(1);
	if(save != R_PPStackTop) {
		printf("stack imbalance in internal %s, %d then %d\n",
			PRIMNAME(INTERNAL(fun)), save, R_PPStackTop);
	}
	return (args);
}
