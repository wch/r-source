/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2001  Robert Gentleman, Ross Ihaka and the
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
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define __R_Names__
#include "Defn.h"
#include "Print.h"
#include "arithmetic.h"

/* Table of  .Internal(.) and .Primitive(.)  R functions
 * =====     =========	      ==========
 *
 * Each entry is a line with
 *
 * printname	c-entry	offset	eval	arity	pp-info
 * ---------	-------	------	----	-----	-------
 *2	name	cfun	code	eval	arity	gram
 *3 PRIMNAME	PRIMFUN	PRIMVAL	 [*]  PRIMARITY PPINFO
 *
 * where "2" are the component names of the FUNTAB struct (Defn.h)
 * and	 "3" are the accessor macros. [*]: PRIMPRINT(.) uses the eval component
 *
 * printname:	The function name in R
 *
 * c-entry:	The name of the corresponding C function,
 *		actually declared in names.h.
 *		Convention:
 *		 - all start with "do_",
 *		 - all return SEXP.
 *		 - all have argument list
 *			 (SEXP call, SEXP op, SEXP args, SEXP env)
 *
 * offset:	the 'op' (offset pointer) above; used for C functions
 *		which deal with more than one R function...
 *
 * eval:	= XYZ (three digits)  [New Apr 9/96, before only had "YZ"].
 *		  --- where e.g. '1' means '001'
 *		X=1 says that we should switch R_Visible off
 *		    (the least common situation).
 *		Y=1 says that this is an internal function which must
 *		    be accessed with a	.Internal(.) call, any other value is
 *		    accessible directly and printed in R as ".Primitive(..)".
 *		Z=1 says evaluate arguments before calling and
 *		Z=0 says don't evaluate.
 *
 * arity:	How many arguments are required/allowed;  "-1"	meaning ``any''
 *
 * pp-info:	Deparsing Info (-> names.h )
 *
 */
FUNTAB R_FunTab[] =
{

/* Language Related Constructs */

/* printname	c-entry		offset	eval	arity	pp-info
 * ---------	-------		------	----	-----	------- */
{"if",		do_if,		0,	0,	-1,	PP_IF},
{"while",	do_while,	0,	0,	-1,	PP_WHILE},
{"for",		do_for,		0,	0,	-1,	PP_FOR},
{"repeat",	do_repeat,	0,	0,	-1,	PP_REPEAT},
{"break",	do_break, CTXT_BREAK,	0,	-1,	PP_BREAK},
{"next",	do_break, CTXT_NEXT,	0,	-1,	PP_NEXT},
{"return",	do_return,	0,	0,	-1,	PP_RETURN},
{"stop",	do_stop,	0,	11,	1,	PP_FUNCALL},
{"warning",	do_warning,	0,	111,	1,	PP_FUNCALL},
{"geterrmessage",do_geterrmessage, 0,	11,	0,	PP_FUNCALL},
{"restart",	do_restart,	0,	11,	1,	PP_FUNCALL},
{"function",	do_function,	0,	0,	-1,	PP_FUNCTION},
{"as.function.default",do_asfunction,0,	11,	2,	PP_FUNCTION},
{"<-",		do_set,		1,	100,	-1,	PP_ASSIGN},
{"<<-",		do_set,		2,	100,	-1,	PP_ASSIGN2},
{"{",		do_begin,	0,	0,	-1,	PP_CURLY},
{"(",		do_paren,	0,	1,	1,	PP_PAREN},
{"[",		do_subset,	1,	0,	-1,	PP_SUBSET},
{"[[",		do_subset2,	2,	0,	2,	PP_SUBSET},
{"$",		do_subset3,	3,	0,	2,	PP_DOLLAR},
{"[<-",		do_subassign,	0,	0,	3,	PP_SUBASS},
{"[[<-",	do_subassign2,	1,	100,	3,	PP_SUBASS},
{"$<-",		do_subassign3,	1,	0,	3,	PP_SUBASS},
{"switch",	do_switch,	0,	10,	-1,	PP_FUNCALL},
{"browser",	do_browser,	0,	100,	0,	PP_FUNCALL},
{"debug",	do_debug,	0,	101,	1,	PP_FUNCALL},
{"undebug",	do_debug,	1,	101,	1,	PP_FUNCALL},
{"trace",	do_trace,	0,	101,	1,	PP_FUNCALL},
{"untrace",	do_trace,	1,	101,	1,	PP_FUNCALL},
{".Internal",	do_internal,	0,	0,	1,	PP_FUNCALL},
{"on.exit",	do_onexit,	0,	100,	1,	PP_FUNCALL},
{"Recall",	do_recall,	0,	10,	-1,	PP_FUNCALL},
{"delay",	do_delay,	0,	11,	2,	PP_FUNCALL},
{".Alias",	do_alias,	0,	1,	1,	PP_FUNCALL},
{".Primitive",	do_primitive,	0,	1,	1,	PP_FUNCALL},


/* Binary Operators */

{"+",		do_arith,	PLUSOP,	1,	2,	PP_BINARY},
{"-",		do_arith,	MINUSOP,1,	2,	PP_BINARY},
{"*",		do_arith,	TIMESOP,1,	2,	PP_BINARY},
{"/",		do_arith,	DIVOP,	1,	2,	PP_BINARY2},
{"^",		do_arith,	POWOP,	1,	2,	PP_BINARY2},
{"%%",		do_arith,	MODOP,	1,	2,	PP_BINARY2},
{"%/%",		do_arith,	IDIVOP,	1,	2,	PP_BINARY2},
{"%*%",		do_matprod,	0,	1,	2,	PP_BINARY},
{"crossprod",	do_matprod,	1,	11,	2,	PP_FUNCALL},
{"==",		do_relop,	EQOP,	1,	2,	PP_BINARY},
{"!=",		do_relop,	NEOP,	1,	2,	PP_BINARY},
{"<",		do_relop,	LTOP,	1,	2,	PP_BINARY},
{"<=",		do_relop,	LEOP,	1,	2,	PP_BINARY},
{">=",		do_relop,	GEOP,	1,	2,	PP_BINARY},
{">",		do_relop,	GTOP,	1,	2,	PP_BINARY},
{"&",		do_logic,	1,	1,	2,	PP_BINARY},
{"|",		do_logic,	2,	1,	2,	PP_BINARY},
{"!",		do_logic,	3,	1,	1,	PP_UNARY},
{"&&",		do_logic2,	1,	0,	2,	PP_BINARY},
{"||",		do_logic2,	2,	0,	2,	PP_BINARY},
{":",		do_seq,		0,	1,	2,	PP_BINARY2},
{"~",		do_tilde,	0,	0,	2,	PP_BINARY},


/* Logic Related Functions */

{"all",		do_logic3,	1,	11,	-1,	PP_FUNCALL},
{"any",		do_logic3,	2,	11,	-1,	PP_FUNCALL},


/* Vectors, Matrices and Arrays */

/* printname	c-entry		offset	eval	arity	pp-info
 * ---------	-------		------	----	-----	------- */
{"vector",	do_makevector,	0,	11,	2,	PP_FUNCALL},
{"complex",	do_complex,	0,	11,	3,	PP_FUNCALL},
{"matrix",	do_matrix,	0,	11,	4,	PP_FUNCALL},
/*MM(98/4/22){"array",	do_array,0,	1,	2,	PP_FUNCALL},*/
{"length",	do_length,	0,	1,	1,	PP_FUNCALL},
{"length<-",	do_lengthgets,	0,	1,	2,	PP_FUNCALL},
{"row",		do_rowscols,	1,	11,	1,	PP_FUNCALL},
{"col",		do_rowscols,	2,	11,	1,	PP_FUNCALL},
{"c",/* bind.c:*/do_c,		0,	0,	-1,	PP_FUNCALL},
{"unlist",	do_unlist,	0,	10,	3,	PP_FUNCALL},
{"cbind",	do_bind,	1,	10,	-1,	PP_FUNCALL},
{"rbind",	do_bind,	2,	10,	-1,	PP_FUNCALL},
{"drop",	do_drop,	0,	11,	1,	PP_FUNCALL},
{"class",	do_class,	0,	1,	1,	PP_FUNCALL},
{"class<-",	do_classgets,	0,	1,	2,	PP_FUNCALL},
{"unclass",	do_unclass,	0,	1,	1,	PP_FUNCALL},
{"names",	do_names,	0,	11,	1,	PP_FUNCALL},
{"names<-",	do_namesgets,	0,	11,	2,	PP_FUNCALL},
{"dimnames",	do_dimnames,	0,	0,	1,	PP_FUNCALL},
{"dimnames<-",	do_dimnamesgets,0,	0,	2,	PP_FUNCALL},
{"all.names",	do_allnames,	0,	11,	4,	PP_FUNCALL},
{"dim",		do_dim,		0,	0,	1,	PP_FUNCALL},
{"dim<-",	do_dimgets,	0,	0,	2,	PP_FUNCALL},
{"attributes",	do_attributes,	0,	1,	1,	PP_FUNCALL},
{"attributes<-",do_attributesgets,0,	1,	1,	PP_FUNCALL},
{"attr",	do_attr,	0,	1,	2,	PP_FUNCALL},
{"attr<-",	do_attrgets,	0,	0,	3,	PP_FUNCALL},
{"comment",	do_comment,	0,	11,	1,	PP_FUNCALL},
{"comment<-",	do_commentgets,	0,	11,	2,	PP_FUNCALL},
{"get",		do_get,		1,	10,	4,	PP_FUNCALL},
{"exists",	do_get,		0,	10,	4,	PP_FUNCALL},
{"assign",	do_assign,	0,	110,	4,	PP_FUNCALL},
{"remove",	do_remove,	0,	111,	3,	PP_FUNCALL},
{"duplicated",	do_duplicated,	0,	11,	1,	PP_FUNCALL},
{"unique",	do_duplicated,	1,	11,	1,	PP_FUNCALL},
{"which.min",	do_first_min,	1,	11,	1,	PP_FUNCALL},
{"which.max",	do_first_max,	1,	11,	1,	PP_FUNCALL},
{"unique",	do_duplicated,	1,	11,	1,	PP_FUNCALL},
{"match",	do_match,	0,	11,	3,	PP_FUNCALL},
{"pmatch",	do_pmatch,	0,	11,	3,	PP_FUNCALL},
{"charmatch",	do_charmatch,	0,	11,	2,	PP_FUNCALL},
{"match.call",	do_matchcall,	0,	11,	3,	PP_FUNCALL},
{"complete.cases",do_compcases,	0,	11,	1,	PP_FUNCALL},

{"attach",	do_attach,	0,	111,	3,	PP_FUNCALL},
{"detach",	do_detach,	0,	111,	1,	PP_FUNCALL},
{"search",	do_search,	0,	11,	0,	PP_FUNCALL},


/* Mathematical Functions */
{"round",	do_round,	10001,	11,	1,	PP_FUNCALL},
{"atan",	do_atan,	10002,	1,	1,	PP_FUNCALL},
{"log",		do_log,		10003,	11,	1,	PP_FUNCALL},
{"signif",	do_signif,	10004,	11,	1,	PP_FUNCALL},

/* KH(1999/09/12)-> complex: {"abs", do_math1, 0, 1, 1, PP_FUNCALL}, */
{"floor",	do_math1,	1,	1,	1,	PP_FUNCALL},
{"ceiling",	do_math1,	2,	1,	1,	PP_FUNCALL},
{"sqrt",	do_math1,	3,	1,	1,	PP_FUNCALL},
{"sign",	do_math1,	4,	1,	1,	PP_FUNCALL},
{"trunc",	do_math1,	5,	1,	1,	PP_FUNCALL},

{"exp",		do_math1,	10,	1,	1,	PP_FUNCALL},
{"log1p",	do_math1,	12,	11,	1,	PP_FUNCALL},

{"cos",		do_math1,	20,	1,	1,	PP_FUNCALL},
{"sin",		do_math1,	21,	1,	1,	PP_FUNCALL},
{"tan",		do_math1,	22,	1,	1,	PP_FUNCALL},
{"acos",	do_math1,	23,	1,	1,	PP_FUNCALL},
{"asin",	do_math1,	24,	1,	1,	PP_FUNCALL},

{"cosh",	do_math1,	30,	1,	1,	PP_FUNCALL},
{"sinh",	do_math1,	31,	1,	1,	PP_FUNCALL},
{"tanh",	do_math1,	32,	1,	1,	PP_FUNCALL},
{"acosh",	do_math1,	33,	1,	1,	PP_FUNCALL},
{"asinh",	do_math1,	34,	1,	1,	PP_FUNCALL},
{"atanh",	do_math1,	35,	1,	1,	PP_FUNCALL},

{"lgamma",	do_math1,	40,	11,	1,	PP_FUNCALL},
{"gamma",	do_math1,	41,	11,	1,	PP_FUNCALL},

/* Polygamma Functions */

{"digamma",	do_math1,	42,	11,	1,	PP_FUNCALL},
{"trigamma",	do_math1,	43,	11,	1,	PP_FUNCALL},
{"tetragamma",	do_math1,	44,	11,	1,	PP_FUNCALL},
{"pentagamma",	do_math1,	45,	11,	1,	PP_FUNCALL},

{"gammaCody",	do_math1,	46,	11,	1,	PP_FUNCALL},

/* Mathematical Functions of Two Numeric (+ 1-2 int) Variables */

{"atan2",	do_math2,	0,	11,	2,	PP_FUNCALL},

{"lbeta",	do_math2,	2,	11,	2,	PP_FUNCALL},
{"beta",	do_math2,	3,	11,	2,	PP_FUNCALL},
{"lchoose",	do_math2,	4,	11,	2,	PP_FUNCALL},
{"choose",	do_math2,	5,	11,	2,	PP_FUNCALL},

{"dchisq",	do_math2,	6,	11,	2+1,	PP_FUNCALL},
{"pchisq",	do_math2,	7,	11,	2+2,	PP_FUNCALL},
{"qchisq",	do_math2,	8,	11,	2+2,	PP_FUNCALL},

{"dexp",	do_math2,	9,	11,	2+1,	PP_FUNCALL},
{"pexp",	do_math2,	10,	11,	2+2,	PP_FUNCALL},
{"qexp",	do_math2,	11,	11,	2+2,	PP_FUNCALL},

{"dgeom",	do_math2,	12,	11,	2+1,	PP_FUNCALL},
{"pgeom",	do_math2,	13,	11,	2+2,	PP_FUNCALL},
{"qgeom",	do_math2,	14,	11,	2+2,	PP_FUNCALL},

{"dpois",	do_math2,	15,	11,	2+1,	PP_FUNCALL},
{"ppois",	do_math2,	16,	11,	2+2,	PP_FUNCALL},
{"qpois",	do_math2,	17,	11,	2+2,	PP_FUNCALL},

{"dt",		do_math2,	18,	11,	2+1,	PP_FUNCALL},
{"pt",		do_math2,	19,	11,	2+2,	PP_FUNCALL},
{"qt",		do_math2,	20,	11,	2+2,	PP_FUNCALL},

{"dsignrank",	do_math2,	21,	11,	2+1,	PP_FUNCALL},
{"psignrank",	do_math2,	22,	11,	2+2,	PP_FUNCALL},
{"qsignrank",	do_math2,	23,	11,	2+2,	PP_FUNCALL},

{"besselJ",	do_math2,	24,	11,	2,	PP_FUNCALL},
{"besselY",	do_math2,	25,	11,	2,	PP_FUNCALL},

/* Mathematical Functions of a Complex Argument */

{"Re",		do_cmathfuns,	1,	1,	1,	PP_FUNCALL},
{"Im",		do_cmathfuns,	2,	1,	1,	PP_FUNCALL},
{"Mod",		do_cmathfuns,	3,	1,	1,	PP_FUNCALL},
{"Arg",		do_cmathfuns,	4,	1,	1,	PP_FUNCALL},
{"Conj",	do_cmathfuns,	5,	1,	1,	PP_FUNCALL},
{"abs",		do_cmathfuns,	6,	1,	1,	PP_FUNCALL},


/* Mathematical Functions of Three Numeric (+ 1-2 int) Variables */

{"dbeta",	do_math3,	1,	11,	3+1,	PP_FUNCALL},
{"pbeta",	do_math3,	2,	11,	3+2,	PP_FUNCALL},
{"qbeta",	do_math3,	3,	11,	3+2,	PP_FUNCALL},

{"dbinom",	do_math3,	4,	11,	3+1,	PP_FUNCALL},
{"pbinom",	do_math3,	5,	11,	3+2,	PP_FUNCALL},
{"qbinom",	do_math3,	6,	11,	3+2,	PP_FUNCALL},

{"dcauchy",	do_math3,	7,	11,	3+1,	PP_FUNCALL},
{"pcauchy",	do_math3,	8,	11,	3+2,	PP_FUNCALL},
{"qcauchy",	do_math3,	9,	11,	3+2,	PP_FUNCALL},

{"df",		do_math3,	10,	11,	3+1,	PP_FUNCALL},
{"pf",		do_math3,	11,	11,	3+2,	PP_FUNCALL},
{"qf",		do_math3,	12,	11,	3+2,	PP_FUNCALL},

{"dgamma",	do_math3,	13,	11,	3+1,	PP_FUNCALL},
{"pgamma",	do_math3,	14,	11,	3+2,	PP_FUNCALL},
{"qgamma",	do_math3,	15,	11,	3+2,	PP_FUNCALL},

{"dlnorm",	do_math3,	16,	11,	3+1,	PP_FUNCALL},
{"plnorm",	do_math3,	17,	11,	3+2,	PP_FUNCALL},
{"qlnorm",	do_math3,	18,	11,	3+2,	PP_FUNCALL},

{"dlogis",	do_math3,	19,	11,	3+1,	PP_FUNCALL},
{"plogis",	do_math3,	20,	11,	3+2,	PP_FUNCALL},
{"qlogis",	do_math3,	21,	11,	3+2,	PP_FUNCALL},

{"dnbinom",	do_math3,	22,	11,	3+1,	PP_FUNCALL},
{"pnbinom",	do_math3,	23,	11,	3+2,	PP_FUNCALL},
{"qnbinom",	do_math3,	24,	11,	3+2,	PP_FUNCALL},

{"dnorm",	do_math3,	25,	11,	3+1,	PP_FUNCALL},
{"pnorm",	do_math3,	26,	11,	3+2,	PP_FUNCALL},
{"qnorm",	do_math3,	27,	11,	3+2,	PP_FUNCALL},

{"dunif",	do_math3,	28,	11,	3+1,	PP_FUNCALL},
{"punif",	do_math3,	29,	11,	3+2,	PP_FUNCALL},
{"qunif",	do_math3,	30,	11,	3+2,	PP_FUNCALL},

{"dweibull",	do_math3,	31,	11,	3+1,	PP_FUNCALL},
{"pweibull",	do_math3,	32,	11,	3+2,	PP_FUNCALL},
{"qweibull",	do_math3,	33,	11,	3+2,	PP_FUNCALL},

{"dnchisq",	do_math3,	34,	11,	3+1,	PP_FUNCALL},
{"pnchisq",	do_math3,	35,	11,	3+2,	PP_FUNCALL},
{"qnchisq",	do_math3,	36,	11,	3+2,	PP_FUNCALL},

{"dnt",		do_math3,	37,	11,	3+1,	PP_FUNCALL},
{"pnt",		do_math3,	38,	11,	3+2,	PP_FUNCALL},
{"qnt",		do_math3,	39,	11,	3+2,	PP_FUNCALL},

{"dwilcox",	do_math3,	40,	11,	3+1,	PP_FUNCALL},
{"pwilcox",	do_math3,	41,	11,	3+2,	PP_FUNCALL},
{"qwilcox",	do_math3,	42,	11,	3+2,	PP_FUNCALL},

{"besselI",	do_math3,	43,	11,	3,	PP_FUNCALL},
{"besselK",	do_math3,	44,	11,	3,	PP_FUNCALL},

/* Mathematical Functions of Four Numeric (+ 1-2 int) Variables */

{"dhyper",	do_math4,	1,	11,	4+1,	PP_FUNCALL},
{"phyper",	do_math4,	2,	11,	4+2,	PP_FUNCALL},
{"qhyper",	do_math4,	3,	11,	4+2,	PP_FUNCALL},

{"dnbeta",	do_math4,	4,	11,	4+1,	PP_FUNCALL},
{"pnbeta",	do_math4,	5,	11,	4+2,	PP_FUNCALL},
{"qnbeta",	do_math4,	6,	11,	4+2,	PP_FUNCALL},

{"dnf",		do_math4,	7,	11,	4+1,	PP_FUNCALL},
{"pnf",		do_math4,	8,	11,	4+2,	PP_FUNCALL},
{"qnf",		do_math4,	9,	11,	4+2,	PP_FUNCALL},

{"dtukey",	do_math4,	10,	11,	4+1,	PP_FUNCALL},
{"ptukey",	do_math4,	11,	11,	4+2,	PP_FUNCALL},
{"qtukey",	do_math4,	12,	11,	4+2,	PP_FUNCALL},

/* Random Numbers */

{"rchisq",	do_random1,	0,	11,	2,	PP_FUNCALL},
{"rexp",	do_random1,	1,	11,	2,	PP_FUNCALL},
{"rgeom",	do_random1,	2,	11,	2,	PP_FUNCALL},
{"rpois",	do_random1,	3,	11,	2,	PP_FUNCALL},
{"rt",		do_random1,	4,	11,	2,	PP_FUNCALL},
{"rsignrank",	do_random1,	5,	11,	2,	PP_FUNCALL},

{"rbeta",	do_random2,	0,	11,	3,	PP_FUNCALL},
{"rbinom",	do_random2,	1,	11,	3,	PP_FUNCALL},
{"rcauchy",	do_random2,	2,	11,	3,	PP_FUNCALL},
{"rf",		do_random2,	3,	11,	3,	PP_FUNCALL},
{"rgamma",	do_random2,	4,	11,	3,	PP_FUNCALL},
{"rlnorm",	do_random2,	5,	11,	3,	PP_FUNCALL},
{"rlogis",	do_random2,	6,	11,	3,	PP_FUNCALL},
{"rnbinom",	do_random2,	7,	11,	3,	PP_FUNCALL},
{"rnorm",	do_random2,	8,	11,	3,	PP_FUNCALL},
{"runif",	do_random2,	9,	11,	3,	PP_FUNCALL},
{"rweibull",	do_random2,	10,	11,	3,	PP_FUNCALL},
{"rwilcox",	do_random2,	11,	11,	3,	PP_FUNCALL},

{"rhyper",	do_random3,	0,	11,	4,	PP_FUNCALL},

{"sample",	do_sample,	0,	11,	4,	PP_FUNCALL},

{"RNGkind",	do_RNGkind,	0,	11,	2,	PP_FUNCALL},
{"set.seed",	do_setseed,	0,	11,	2,	PP_FUNCALL},

/* Data Summaries */

{"sum",		do_summary,	0,	11,	-1,	PP_FUNCALL},
  /*MM{"mean",	do_summary,	1,	11,	1,	PP_FUNCALL},*/
{"min",		do_summary,	2,	11,	-1,	PP_FUNCALL},
{"max",		do_summary,	3,	11,	-1,	PP_FUNCALL},
{"prod",	do_summary,	4,	11,	-1,	PP_FUNCALL},
{"range",	do_range,	0,	11,	-1,	PP_FUNCALL},
{"cov",		do_cov,		0,	11,	3,	PP_FUNCALL},
{"cor",		do_cov,		1,	11,	3,	PP_FUNCALL},

{"cumsum",	do_cum,		1,	1,	1,	PP_FUNCALL},
{"cumprod",	do_cum,		2,	1,	1,	PP_FUNCALL},
{"cummax",	do_cum,		3,	1,	1,	PP_FUNCALL},
{"cummin",	do_cum,		4,	1,	1,	PP_FUNCALL},

/* Type coercion */

{"as.character",do_ascharacter,	0,	0,	1,	PP_FUNCALL},
{"as.vector",	do_asvector,	0,	10,	2,	PP_FUNCALL},
{"paste",	do_paste,	0,	11,	3,	PP_FUNCALL},
{"format",	do_format,	0,	11,	-1,	PP_FUNCALL},
{"format.info",	do_formatinfo,	0,	11,	1,	PP_FUNCALL},
{"cat",		do_cat,		0,	111,	6,	PP_FUNCALL},
{"call",	do_call,	0,	0,	-1,	PP_FUNCALL},
{"do.call",	do_docall,	0,	11,	2,	PP_FUNCALL},
{"as.call",	do_ascall,	0,	1,	1,	PP_FUNCALL},
{"type.convert",do_typecvt,	1,	11,	4,	PP_FUNCALL},


/* String Manipulation */

{"nchar",	do_nchar,	1,	11,	1,	PP_FUNCALL},
{"substr",	do_substr,	1,	11,	3,	PP_FUNCALL},
{"strsplit",	do_strsplit,	1,	11,	3,	PP_FUNCALL},
{"abbreviate",	do_abbrev,	1,	11,	3,	PP_FUNCALL},
{"make.names",	do_makenames,	0,	11,	1,	PP_FUNCALL},
{"grep",	do_grep,	1,	11,	5,	PP_FUNCALL},
{"sub",		do_gsub,	0,	11,	5,	PP_FUNCALL},
{"gsub",	do_gsub,	1,	11,	5,	PP_FUNCALL},
{"regexpr",	do_regexpr,	1,	11,	3,	PP_FUNCALL},
{"tolower",	do_tolower,	1,	11,	1,	PP_FUNCALL},
{"toupper",	do_toupper,	1,	11,	1,	PP_FUNCALL},
{"chartr",	do_chartr,	1,	11,	3,	PP_FUNCALL},


/* Type Checking (typically implemented in ./coerce.c ) */

{"is.null",	do_is,		NILSXP,	1,	1,	PP_FUNCALL},
{"is.logical",	do_is,		LGLSXP,	1,	1,	PP_FUNCALL},
{"is.integer",	do_is,		INTSXP,	1,	1,	PP_FUNCALL},
{"is.real",	do_is,		REALSXP,1,	1,	PP_FUNCALL},
{"is.double",	do_is,		REALSXP,1,	1,	PP_FUNCALL},
{"is.complex",	do_is,		CPLXSXP,1,	1,	PP_FUNCALL},
{"is.character",do_is,		STRSXP,	1,	1,	PP_FUNCALL},
{"is.symbol",	do_is,		SYMSXP,	1,	1,	PP_FUNCALL},
{"is.environment",do_is,	ENVSXP,	1,	1,	PP_FUNCALL},
{"is.list",	do_is,		VECSXP,	1,	1,	PP_FUNCALL},
{"is.pairlist",	do_is,		LISTSXP,1,	1,	PP_FUNCALL},
{"is.expression",do_is,		EXPRSXP,1,	1,	PP_FUNCALL},

{"is.object",	do_is,		50,	1,	1,	PP_FUNCALL},
{"is.factor",	do_is,		75,	1,	1,	PP_FUNCALL},

{"is.numeric",	do_is,		100,	1,	1,	PP_FUNCALL},
{"is.matrix",	do_is,		101,	1,	1,	PP_FUNCALL},
{"is.array",	do_is,		102,	1,	1,	PP_FUNCALL},
{"is.ts",	do_is,		103,	1,	1,	PP_FUNCALL},

{"is.atomic",	do_is,		200,	1,	1,	PP_FUNCALL},
{"is.recursive",do_is,		201,	1,	1,	PP_FUNCALL},

{"is.call",	do_is,		300,	1,	1,	PP_FUNCALL},
{"is.language",	do_is,		301,	1,	1,	PP_FUNCALL},
{"is.function",	do_is,		302,	1,	1,	PP_FUNCALL},

{"is.single",	do_is,		999,	1,	1,	PP_FUNCALL},

{"is.vector",	do_isvector,	0,	11,	2,	PP_FUNCALL},
{"is.na",	do_isna,	0,	1,	1,	PP_FUNCALL},
{"is.nan",	do_isnan,	0,	1,	1,	PP_FUNCALL},
{"is.finite",	do_isfinite,	0,	1,	1,	PP_FUNCALL},
{"is.infinite",	do_isinfinite,	0,	1,	1,	PP_FUNCALL},


/* Miscellaneous */

#ifdef HAVE_TIMES
{"proc.time",	do_proctime,	0,	1,	0,	PP_FUNCALL},
{"gc.time",	do_gctime,	0,	1,	0,	PP_FUNCALL},
#endif
{"Version",	do_version,	0,	11,	0,	PP_FUNCALL},
{"machine",	do_machine,	0,	11,	0,	PP_FUNCALL},
{"Machine",	do_Machine,	0,	11,	0,	PP_FUNCALL},
{"commandArgs", do_commandArgs, 0,	11,	0,	PP_FUNCALL},
#ifdef Win32
{"system",	do_system,	0,	11,	3,	PP_FUNCALL},
#else
{"system",	do_system,	0,	11,	2,	PP_FUNCALL},
#endif
#ifdef Win32
{"unlink",	do_unlink,	0,	11,	2,	PP_FUNCALL},
{"help.start",	do_helpstart,	0,	11,	0,	PP_FUNCALL},
{"show.help.item",do_helpitem,	0,	11,	3,	PP_FUNCALL},
{"flush.console",do_flushconsole,0,	11,	0,	PP_FUNCALL},
{"int.unzip",	do_int_unzip,	0,	11,    -1,	PP_FUNCALL},
{"win.version", do_winver,	0,	11,	0,	PP_FUNCALL},
{"saveDevga",	do_saveDevga,	0,	11,	3,	PP_FUNCALL},
{"shell.exec",	do_shellexec,	0,	11,	1,	PP_FUNCALL},
{"dir.create",	do_dircreate,	0,	11,	1,	PP_FUNCALL},
{"winDialog",	do_windialog,	0,	11,	2,	PP_FUNCALL},
{"winDialogString", do_windialogstring, 0, 11,	2,	PP_FUNCALL},
{"winMenuAdd",	do_winmenuadd,	0,	11,	3,	PP_FUNCALL},
{"winMenuDel",	do_winmenudel,	0,	11,	2,	PP_FUNCALL},
{"memory.size",	do_memsize,	0,	11,	1,	PP_FUNCALL},
{"DLL.version",	do_dllversion,	0,	11,	1,	PP_FUNCALL},
{"bringToTop",	do_bringtotop,	0,	11,	1,	PP_FUNCALL},
#endif
#ifdef Macintosh
{"unlink",	do_unlink,	0,	11,	2,	PP_FUNCALL},
{"help.start",	do_helpstart,	0,	11,	0,	PP_FUNCALL},
{"show.help.item",do_helpitem,	0,	11,	3,	PP_FUNCALL},
{"int.unzip",	do_int_unzip,	0,	11,    -1,	PP_FUNCALL},
{"dir.create",	do_dircreate,	0,	11,	1,	PP_FUNCALL},
#endif
{"parse",	do_parse,	0,	11,	4,	PP_FUNCALL},
{"save",	do_save,	0,	111,	3,	PP_FUNCALL},
{"load",	do_load,	0,	111,	2,	PP_FUNCALL},
{"deparse",	do_deparse,	0,	11,	2,	PP_FUNCALL},
{"dput",	do_dput,	0,	111,	2,	PP_FUNCALL},
{"dump",	do_dump,	0,	111,	2,	PP_FUNCALL},
{"substitute",	do_substitute,	0,	0,	-1,	PP_FUNCALL},
{"quit",	do_quit,	0,	111,	3,	PP_FUNCALL},
{"interactive",	do_interactive,	0,	0,	0,	PP_FUNCALL},
{"readline",	do_readln,	0,	11,	1,	PP_FUNCALL},
{"menu",	do_menu,	0,	11,	1,	PP_FUNCALL},
{"print.default",do_printdefault,0,	111,	6,	PP_FUNCALL},
{"print.matrix",do_printmatrix, 0,	111,	5,	PP_FUNCALL},
{"invisible",	do_invisible,	0,	101,	1,	PP_FUNCALL},
{"gc",		do_gc,		0,	11,	1,	PP_FUNCALL},
{"gcinfo",	do_gcinfo,	0,	11,	1,	PP_FUNCALL},
{"gctorture",	do_gctorture,	0,	11,	1,	PP_FUNCALL},
{"memory.profile",do_memoryprofile, 0,	11,	0,	PP_FUNCALL},
{"rep",		do_rep,		0,	11,	2,	PP_FUNCALL},
{"list",	do_makelist,	1,	1,	-1,	PP_FUNCALL},
{"split",	do_split,	0,	11,	2,	PP_FUNCALL},
{"symbol.C",	do_symbol,	0,	1,	1,	PP_FOREIGN},
{"symbol.For",	do_symbol,	1,	1,	1,	PP_FOREIGN},
{"is.loaded",	do_isloaded,	0,	1,	1,	PP_FOREIGN},
{".C",		do_dotCode,	0,	1,	-1,	PP_FOREIGN},
{".Fortran",	do_dotCode,	1,	1,	-1,	PP_FOREIGN},
{".External",   do_External,    0,      1,      -1,     PP_FOREIGN},
{".Call",       do_dotcall,     0,      1,      -1,     PP_FOREIGN},
{".External.graphics", do_Externalgr, 0, 1,	-1,	PP_FOREIGN},
{".Call.graphics", do_dotcallgr, 0,	1,	-1,	PP_FOREIGN},
{"dyn.load",	do_dynload,	0,	111,	3,	PP_FUNCALL},
{"dyn.unload",	do_dynunload,	0,	111,	1,	PP_FUNCALL},
{"ls",		do_ls,		1,	11,	2,	PP_FUNCALL},
{"typeof",	do_typeof,	1,	11,	1,	PP_FUNCALL},
{"eval",	do_eval,	0,	11,	3,	PP_FUNCALL},
{"eval.with.vis",do_eval,	1,	11,	3,	PP_FUNCALL},
{"expression",	do_expression,	1,	0,	-1,	PP_FUNCALL},
{"sys.parent",	do_sys,		1,	10,	-1,	PP_FUNCALL},
{"sys.call",	do_sys,		2,	10,	-1,	PP_FUNCALL},
{"sys.frame",	do_sys,		3,	10,	-1,	PP_FUNCALL},
{"sys.nframe",	do_sys,		4,	10,	-1,	PP_FUNCALL},
{"sys.calls",	do_sys,		5,	10,	-1,	PP_FUNCALL},
{"sys.frames",	do_sys,		6,	10,	-1,	PP_FUNCALL},
{"sys.on.exit",	do_sys,		7,	10,	-1,	PP_FUNCALL},
{"sys.parents",	do_sys,		8,	10,	-1,	PP_FUNCALL},
{"sys.function",do_sys,		9,	10,	-1,	PP_FUNCALL},
{"parent.frame",do_parentframe,	0,	10,	-1,	PP_FUNCALL},
{"sort",	do_sort,	1,	11,	1,	PP_FUNCALL},
{"psort",	do_psort,	0,	11,	2,	PP_FUNCALL},
{"order",	do_order,	0,	11,	-1,	PP_FUNCALL},
{"rank",	do_rank,	0,	11,	1,	PP_FUNCALL},
{"missing",	do_missing,	1,	0,	1,	PP_FUNCALL},
{"nargs",	do_nargs,	1,	0,	0,	PP_FUNCALL},
{"scan",	do_scan,	0,	11,	14,	PP_FUNCALL},
{"count.fields",do_countfields,	0,	11,	5,	PP_FUNCALL},
{"t.default",	do_transpose,	0,	11,	1,	PP_FUNCALL},
{"aperm",	do_aperm,	0,	11,	3,	PP_FUNCALL},
{"builtins",	do_builtins,	0,	11,	1,	PP_FUNCALL},
{"edit",	do_edit,	0,	11,	3,	PP_FUNCALL},
{"dataentry",	do_dataentry,	0,	11,	1,	PP_FUNCALL},
{"args",	do_args,	0,	11,	1,	PP_FUNCALL},
{"formals",	do_formals,	0,	11,	1,	PP_FUNCALL},
{"body",	do_body,	0,	11,	1,	PP_FUNCALL},
{"globalenv",	do_globalenv,	0,	1,	0,	PP_FUNCALL},
{"environment",	do_envir,	0,	11,	1,	PP_FUNCALL},
{"environment<-",do_envirgets,	0,	1,	2,	PP_FUNCALL},
{"options",	do_options,	0,	11,	1,	PP_FUNCALL},
{"sink",	do_sink,	0,	111,	3,	PP_FUNCALL},
{"lib.fixup",	do_libfixup,	0,	111,	2,	PP_FUNCALL},
{"pos.to.env",	do_pos2env,	0,	1,	1,	PP_FUNCALL},
{"lapply",	do_lapply,	0,	10,	2,	PP_FUNCALL},
{"apply",	do_apply,	0,	11,	3,	PP_FUNCALL},
{"Rprof",	do_Rprof,	0,	11,	3,	PP_FUNCALL},
{"object.size",	do_objectsize,	0,	11,	1,	PP_FUNCALL},
{"mem.limits",	do_memlimits,	0,	11,	2,	PP_FUNCALL},
{"merge",	do_merge,	0,	11,	2,	PP_FUNCALL},
#if 0
{"visibleflag", do_visibleflag,	0,	1,	0,	PP_FUNCALL},
#endif

/* Functions To Interact with the Operating System */

{"file.show",	do_fileshow,	0,	111,	5,	PP_FUNCALL},
{"file.create",	do_filecreate,	0,	11,	1,	PP_FUNCALL},
{"file.remove",	do_fileremove,	0,	11,	1,	PP_FUNCALL},
{"file.append",	do_fileappend,	0,	11,	2,	PP_FUNCALL},
{"list.files",	do_listfiles,	0,	11,	4,	PP_FUNCALL},
{"file.exists", do_fileexists,	0,	11,	1,	PP_FUNCALL},
{"file.choose", do_filechoose,	0,	11,	1,	PP_FUNCALL},
{"file.info",	do_fileinfo,	0,	11,	1,	PP_FUNCALL},
{"file.access",	do_fileaccess,	0,	11,	2,	PP_FUNCALL},
{"tempfile",	do_tempfile,	0,	11,	1,	PP_FUNCALL},
{"R.home",	do_Rhome,	0,	11,	0,	PP_FUNCALL},
{"date",	do_date,	0,	11,	0,	PP_FUNCALL},
{"Platform",	do_Platform,	0,	11,	0,	PP_FUNCALL},
{"index.search",do_indexsearch, 0,	11,	5,	PP_FUNCALL},
{"getenv",	do_getenv,	0,	11,	1,	PP_FUNCALL},
{"putenv",	do_putenv,	0,	11,	1,	PP_FUNCALL},
{"getwd",	do_getwd,	0,	11,	0,	PP_FUNCALL},
{"setwd",	do_setwd,	0,	11,	1,	PP_FUNCALL},
{"basename",	do_basename,	0,	11,	1,	PP_FUNCALL},
{"dirname",	do_dirname,	0,	11,	1,	PP_FUNCALL},
{"Sys.info",	do_sysinfo,	0,	11,	0,	PP_FUNCALL},
{"Sys.sleep",	do_syssleep,	0,	11,	1,	PP_FUNCALL},
{"getlocale",	do_getlocale,	0,	11,	1,	PP_FUNCALL},
{"setlocale",	do_setlocale,	0,	11,	2,	PP_FUNCALL},
{"localeconv",	do_localeconv,	0,	11,	0,	PP_FUNCALL},
{"path.expand",	do_pathexpand,	0,	11,	1,	PP_FUNCALL},

/* Complex Valued Functions */
{"fft",		do_fft,		0,	11,	2,	PP_FUNCALL},
{"mvfft",	do_mvfft,	0,	11,	2,	PP_FUNCALL},
{"nextn",	do_nextn,	0,	11,	2,	PP_FUNCALL},
{"polyroot",	do_polyroot,	0,	11,	1,	PP_FUNCALL},

/* Device Drivers */

{"PS",		do_PS,		0,	111,   14,	PP_FUNCALL},
{"PicTeX",	do_PicTeX,	0,	111,	6,	PP_FUNCALL},
{"XFig",	do_XFig,	0,	111,   12,	PP_FUNCALL},
#ifdef Win32
{"devga",	do_devga,	0,	111,	7,	PP_FUNCALL},
#endif
#ifdef Unix
{"X11",		do_X11,		0,	111,	7,	PP_FUNCALL},
{"gnome",	do_Gnome,	0,	111,	4,	PP_FUNCALL},
{"GTK",		do_GTK,		0,	111,	4,	PP_FUNCALL},
#endif
#ifdef Macintosh
{"Macintosh",	do_Macintosh,	0,	111,	4,	PP_FUNCALL},
#endif

/* Graphics */

{"dev.control",	do_devcontrol,	0,	111,	0,	PP_FUNCALL},
{"dev.copy",	do_devcopy,	0,	111,	1,	PP_FUNCALL},
{"dev.cur",	do_devcur,	0,	111,	0,	PP_FUNCALL},
/*
{"device",	do_device,	0,	111,	3,	PP_FUNCALL},
*/
{"dev.next",	do_devnext,	0,	111,	1,	PP_FUNCALL},
{"dev.off",	do_devoff,	0,	111,	1,	PP_FUNCALL},
{"dev.prev",	do_devprev,	0,	111,	1,	PP_FUNCALL},
{"dev.set",	do_devset,	0,	111,	1,	PP_FUNCALL},
{"rgb",		do_rgb,		0,	11,	4,	PP_FUNCALL},
{"hsv",		do_hsv,		0,	11,	4,	PP_FUNCALL},
{"gray",	do_gray,	0,	11,	1,	PP_FUNCALL},
{"colors",	do_colors,	0,	11,	0,	PP_FUNCALL},
{"palette",	do_palette,	0,	11,	1,	PP_FUNCALL},
{"plot.new",	do_plot_new,	0,	111,	0,	PP_FUNCALL},
{"plot.window",	do_plot_window,	0,	111,	3,	PP_FUNCALL},
{"axis",	do_axis,	0,	111,	7,	PP_FUNCALL},
{"plot.xy",	do_plot_xy,	0,	111,	6,	PP_FUNCALL},
{"text",	do_text,	0,	111,	7,	PP_FUNCALL},
{"mtext",	do_mtext,	0,	111,	5,	PP_FUNCALL},
{"title",	do_title,	0,	111,	4,	PP_FUNCALL},
{"abline",	do_abline,	0,	111,	6,	PP_FUNCALL},
{"box",		do_box,		0,	111,	3,	PP_FUNCALL},
{"rect",	do_rect,	0,	111,	6,	PP_FUNCALL},
{"polygon",	do_polygon,	0,	111,	5,	PP_FUNCALL},
{"par",		do_par,		0,	11,	1,	PP_FUNCALL},
{"segments",	do_segments,	0,	111,	6,	PP_FUNCALL},
{"arrows",	do_arrows,	0,	111,	9,	PP_FUNCALL},
{"layout",	do_layout,	0,	111,	10,	PP_FUNCALL},
{"locator",	do_locator,	0,	11,	2,	PP_FUNCALL},
{"identify",	do_identify,	0,	11,	6,	PP_FUNCALL},
{"strheight",	do_strheight,	0,	11,	3,	PP_FUNCALL},
{"strwidth",	do_strwidth,	0,	11,	3,	PP_FUNCALL},
{"contour",	do_contour,	0,	11,	12,	PP_FUNCALL},
{"image",	do_image,	0,	11,	4,	PP_FUNCALL},
{"dend",	do_dend,	0,	111,	6,	PP_FUNCALL},
{"dend.window",	do_dendwindow,	0,	111,	6,	PP_FUNCALL},
{"replay",	do_replay,	0,	111,	0,	PP_FUNCALL},
{"erase",	do_erase,	0,	111,	1,	PP_FUNCALL},
{"dotplot",	do_dotplot,	0,	111,	1,	PP_FUNCALL},
{"persp",	do_persp,	0,	111,	4,	PP_FUNCALL},
{"filledcontour",do_filledcontour,0,	111,	5,	PP_FUNCALL},
{"getDL",	do_getDL,	0,	111,	0,	PP_FUNCALL},
{"playDL",	do_playDL,	0,	111,	1,	PP_FUNCALL},
{"getGPar",	do_getGPar,	0,	111,	0,	PP_FUNCALL},
{"setGPar",	do_setGPar,	0,	111,	1,	PP_FUNCALL},
{"symbols",	do_symbols,	0,	111,	-1,	PP_FUNCALL},

/* Objects */
{"inherits",	do_inherits,	0,	11,	3,	PP_FUNCALL},
{"UseMethod",	do_usemethod,	0,	 0,	-1,	PP_FUNCALL},
{"NextMethod",	do_nextmethod,	0,	10,	-1,	PP_FUNCALL},

/* Modelling Functionality */

{"nlm",		do_nlm,		0,	11,	11,	PP_FUNCALL},
{"fmin",	do_fmin,	0,	11,	4,	PP_FUNCALL},
{"zeroin",	do_zeroin,	0,	11,	5,	PP_FUNCALL},
{"optim",	do_optim,	0,	11,	7,	PP_FUNCALL},
{"optimhess",	do_optimhess,	0,	11,	4,	PP_FUNCALL},
{"terms.formula",do_termsform,	0,	11,	5,	PP_FUNCALL},
{"update.formula",do_updateform,0,	11,	2,	PP_FUNCALL},
{"model.frame",	do_modelframe,	0,	11,	8,	PP_FUNCALL},
{"model.matrix",do_modelmatrix,	0,	11,	2,	PP_FUNCALL},

{"D",		do_D,		0,	11,	2,	PP_FUNCALL},
{"deriv.default",do_deriv,	0,	11,	4,	PP_FUNCALL},

/* History manipulation */
{"loadhistory", do_loadhistory,	0,	11,	1,	PP_FUNCALL},
{"savehistory", do_savehistory,	0,	11,	1,	PP_FUNCALL},

/* date-time manipulations */
{"Sys.time",	do_systime,	0,	11,	0,	PP_FUNCALL},
{"as.POSIXct",	do_asPOSIXct,	0,	11,	2,	PP_FUNCALL},
{"as.POSIXlt",	do_asPOSIXlt,	0,	11,	2,	PP_FUNCALL},
{"format.POSIXlt",do_formatPOSIXlt,0,	11,	3,	PP_FUNCALL},
{"strptime",	do_strptime,	0,	11,	2,	PP_FUNCALL},

/* Connections */
{"stdin", 	do_stdin,	0,      11,     0,      PP_FUNCALL},
{"stdout", 	do_stdout,	0,      11,     0,      PP_FUNCALL},
{"stderr", 	do_stderr,	0,      11,     0,      PP_FUNCALL},
{"readLines", 	do_readLines,	0,      11,     3,      PP_FUNCALL},
{"writeLines", 	do_writelines,	0,      11,     3,      PP_FUNCALL},
{"readBin", 	do_readbin,	0,      11,     5,      PP_FUNCALL},
{"writeBin", 	do_writebin,	0,      11,     4,      PP_FUNCALL},
{"open", 	do_open,	0,      11,     3,      PP_FUNCALL},
{"isOpen", 	do_isopen,	0,      11,     2,      PP_FUNCALL},
{"isIncomplete",do_isincomplete,0,      11,     1,      PP_FUNCALL},
{"isSeekable", 	do_isseekable,	0,      11,     1,      PP_FUNCALL},
{"close", 	do_close,	0,      11,     2,      PP_FUNCALL},
{"file", 	do_file,	0,      11,     3,      PP_FUNCALL},
{"pipe", 	do_pipe,	0,      11,     2,      PP_FUNCALL},
{"seek", 	do_seek,	0,      11,     3,      PP_FUNCALL},
{"pushBack", 	do_pushback,	0,      11,     3,      PP_FUNCALL},
{"pushBackLength",do_pushbacklength,0,  11,     1,      PP_FUNCALL},
{"textConnection",do_textconnection,0,	11,     3,      PP_FUNCALL},
{"getAllConnections",do_getallconnections,0,	11,     0,      PP_FUNCALL},
{"summary.connection",do_sumconnection,0,	11,     1,      PP_FUNCALL},

{NULL,		NULL,		0,	0,	0,	0},
};


SEXP do_primitive(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP name;
    int i;
    checkArity(op, args);
    name = CAR(args);
    if (!isString(name) || length(name) < 1 ||
	STRING_ELT(name, 0) == R_NilValue)
	errorcall(call, "string argument required");
    for (i = 0; R_FunTab[i].name; i++)
	if (strcmp(CHAR(STRING_ELT(name, 0)), R_FunTab[i].name) == 0) {
	    if ((R_FunTab[i].eval % 100 )/10)
		return mkPRIMSXP(i, R_FunTab[i].eval % 10);
	    else
		return mkPRIMSXP(i, R_FunTab[i].eval % 10);
	}
    errorcall(call, "no such primitive function");
    return(R_NilValue);		/* -Wall */
}

int StrToInternal(char *s)
{
    int i;
    for (i = 0; R_FunTab[i].name; i++)
	if (strcmp(s, R_FunTab[i].name) == 0) return i;
    return 0;
}

#ifdef OLD
/* string hashing */
int hashpjw(char *s)
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
#endif

static void installFunTab(int i)
{
    if ((R_FunTab[i].eval % 100 )/10)
	SET_INTERNAL(install(R_FunTab[i].name),
		     mkPRIMSXP(i, R_FunTab[i].eval % 10));
    else
	SET_SYMVALUE(install(R_FunTab[i].name),
		     mkPRIMSXP(i, R_FunTab[i].eval % 10));
}

static void SymbolShortcuts()
{
    R_Bracket2Symbol = install("[[");
    R_BracketSymbol = install("[");
    R_BraceSymbol = install("{");
    R_TmpvalSymbol = install("*tmp*");
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
    R_SourceSymbol = install("source");
    R_DotEnvSymbol = install(".Environment");
    R_RecursiveSymbol = install("recursive");
    R_UseNamesSymbol = install("use.names");
}

extern SEXP framenames; /* from model.c */

/* initialize the symbol table */
void InitNames()
{
    int i;
    /* R_UnboundValue */
    R_UnboundValue = allocSExp(SYMSXP);
    SET_SYMVALUE(R_UnboundValue, R_UnboundValue);
    SET_PRINTNAME(R_UnboundValue, R_NilValue);
    SET_ATTRIB(R_UnboundValue, R_NilValue);
    /* R_MissingArg */
    R_MissingArg = allocSExp(SYMSXP);
    SET_SYMVALUE(R_MissingArg, R_MissingArg);
    SET_PRINTNAME(R_MissingArg, mkChar(""));
    SET_ATTRIB(R_MissingArg, R_NilValue);
    /* Parser Structures */
    R_CommentSxp = R_NilValue;
    R_ParseText = R_NilValue;
    /* String constants (CHARSXP values */
    /* Note: changed from mkChar so mkChar can see if it is getting
       "NA" and then retrun NA_STRING rather than alloc a new CHAR */
    /* NA_STRING */
    NA_STRING = allocString(strlen("NA"));
    strcpy(CHAR(NA_STRING), "NA");
    R_print.na_string = NA_STRING;
    /* R_BlankString */
    R_BlankString = mkChar("");
    /* Initialize the symbol Table */
    if (!(R_SymbolTable = (SEXP *) malloc(HSIZE * sizeof(SEXP))))
	R_Suicide("couldn't allocate memory for symbol table");
    for (i = 0; i < HSIZE; i++)
	R_SymbolTable[i] = R_NilValue;
    /* Set up a set of globals so that a symbol table search can be
       avoided when matching something like dim or dimnames. */
    SymbolShortcuts();
    /*  Builtin Functions */
    for (i = 0; R_FunTab[i].name; i++)
	installFunTab(i);
    /*  Unbound values which are to be preserved through GCs */
    R_PreciousList = R_NilValue;
    framenames = R_NilValue;
}


/*  install - probe the symbol table */
/*  If "name" is not found, it is installed in the symbol table.
    The symbol corresponding to the string "name" is returned. */

SEXP install(char *name)
{
    char buf[MAXIDSIZE+1];
    SEXP sym;
    int i, hashcode;

    if (*name == '\0')
	error("attempt to use zero-length variable name");
    if (strlen(name) > MAXIDSIZE)
	error("symbol print-name too long");
    strcpy(buf, name);
    hashcode = R_Newhashpjw(buf);
    i = hashcode % HSIZE;
    /* Check to see if the symbol is already present;  if it is, return it. */
    for (sym = R_SymbolTable[i]; sym != R_NilValue; sym = CDR(sym))
	if (strcmp(buf, CHAR(PRINTNAME(CAR(sym)))) == 0)
	    return (CAR(sym));
    /* Create a new symbol node and link it into the table. */
    sym = mkSYMSXP(mkChar(buf), R_UnboundValue);
    SET_HASHVALUE(PRINTNAME(sym), hashcode);
    SET_HASHASH(PRINTNAME(sym), 1);
    R_SymbolTable[i] = CONS(sym, R_SymbolTable[i]);
    return (sym);
}


/*  do_internal - This is the code for .Internal(). */

SEXP do_internal(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP s, fun;
    int save = R_PPStackTop;
    checkArity(op, args);
    s = CAR(args);
    if (!isPairList(s))
	errorcall(call, "invalid .Internal() argument");
    fun = CAR(s);
    if (!isSymbol(fun))
	errorcall(call, "invalid internal function");
    if (INTERNAL(fun) == R_NilValue)
	errorcall(call, "no internal function \"%s\"", CHAR(PRINTNAME(fun)));
    args = CDR(s);
    if (TYPEOF(INTERNAL(fun)) == BUILTINSXP)
	args = evalList(args, env);
    PROTECT(args);
    R_Visible = 1 - PRIMPRINT(INTERNAL(fun));
    args = PRIMFUN(INTERNAL(fun)) (s, INTERNAL(fun), args, env);
    UNPROTECT(1);
    if (save != R_PPStackTop) {
	REprintf("stack imbalance in internal %s, %d then %d",
	       PRIMNAME(INTERNAL(fun)), save, R_PPStackTop);
    }
    return (args);
}
#undef __R_Names__
