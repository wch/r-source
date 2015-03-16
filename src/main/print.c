/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998	Robert Gentleman and Ross Ihaka.
 *  Copyright (C) 2000-2015	The R Core Team.
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
 *
 *
 *  print.default()  ->	 do_printdefault (with call tree below)
 *
 *  auto-printing   ->  PrintValueEnv
 *                      -> PrintValueRec
 *                      -> call print() for objects
 *  Note that auto-printing does not call print.default.
 *  PrintValue, R_PV are similar to auto-printing.
 *
 *  do_printdefault
 *	-> PrintDefaults
 *	-> CustomPrintValue
 *	    -> PrintValueRec
 *		-> __ITSELF__  (recursion)
 *		-> PrintGenericVector	-> PrintValueRec  (recursion)
 *		-> printList		-> PrintValueRec  (recursion)
 *		-> printAttributes	-> PrintValueRec  (recursion)
 *		-> PrintExpression
 *		-> printVector		>>>>> ./printvector.c
 *		-> printNamedVector	>>>>> ./printvector.c
 *		-> printMatrix		>>>>> ./printarray.c
 *		-> printArray		>>>>> ./printarray.c
 *
 *  do_prmatrix
 *	-> PrintDefaults
 *	-> printMatrix			>>>>> ./printarray.c
 *
 *
 *  See ./printutils.c	 for general remarks on Printing
 *			 and the Encode.. utils.
 *
 *  Also ./printvector.c,  ./printarray.c
 *
 *  do_sink moved to connections.c as of 1.3.0
 *
 *  <FIXME> These routines are not re-entrant: they reset the
 *  global R_print.
 *  </FIXME>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <Internal.h>
#include "Print.h"
#include "Fileio.h"
#include "Rconnections.h"
#include <S.h>


/* Global print parameter struct: */
R_print_par_t R_print;

static void printAttributes(SEXP, SEXP, Rboolean);
static void PrintSpecial(SEXP);
static void PrintLanguageEtc(SEXP, Rboolean, Rboolean);


#define TAGBUFLEN 256
#define TAGBUFLEN0 TAGBUFLEN + 6
static char tagbuf[TAGBUFLEN0];


/* Used in X11 module for dataentry */
/* NB this is called by R.app even though it is in no public header, so
   alter there if you alter this */
void PrintDefaults(void)
{
    R_print.na_string = NA_STRING;
    R_print.na_string_noquote = mkChar("<NA>");
    R_print.na_width = (int) strlen(CHAR(R_print.na_string));
    R_print.na_width_noquote = (int) strlen(CHAR(R_print.na_string_noquote));
    R_print.quote = 1;
    R_print.right = Rprt_adj_left;
    R_print.digits = GetOptionDigits();
    R_print.scipen = asInteger(GetOption1(install("scipen")));
    if (R_print.scipen == NA_INTEGER) R_print.scipen = 0;
    R_print.max = asInteger(GetOption1(install("max.print")));
    if (R_print.max == NA_INTEGER || R_print.max < 0) R_print.max = 99999;
    else if(R_print.max == INT_MAX) R_print.max--; // so we can add
    R_print.gap = 1;
    R_print.width = GetOptionWidth();
    R_print.useSource = USESOURCE;
    R_print.cutoff = GetOptionCutoff();
}

SEXP attribute_hidden do_invisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    switch (length(args)) {
    case 0:
	return R_NilValue;
    case 1:
	check1arg(args, call, "x");
	return CAR(args);
    default:
	checkArity(op, args); /* must fail */
	return call;/* never used, just for -Wall */
    }
}

#if 0
SEXP attribute_hidden do_visibleflag(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarLogical(R_Visible);
}
#endif

/* This is *only* called via outdated R_level prmatrix() : */
SEXP attribute_hidden do_prmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int quote;
    SEXP a, x, rowlab, collab, naprint;
    char *rowname = NULL, *colname = NULL;

    checkArity(op,args);
    PrintDefaults();
    a = args;
    x = CAR(a); a = CDR(a);
    rowlab = CAR(a); a = CDR(a);
    collab = CAR(a); a = CDR(a);

    quote = asInteger(CAR(a)); a = CDR(a);
    R_print.right = (Rprt_adj) asInteger(CAR(a)); a = CDR(a);
    naprint = CAR(a);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    error(_("invalid 'na.print' specification"));
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    (int) strlen(CHAR(R_print.na_string));
    }

    if (length(rowlab) == 0) rowlab = R_NilValue;
    if (length(collab) == 0) collab = R_NilValue;
    if (!isNull(rowlab) && !isString(rowlab))
	error(_("invalid row labels"));
    if (!isNull(collab) && !isString(collab))
	error(_("invalid column labels"));

    printMatrix(x, 0, getAttrib(x, R_DimSymbol), quote, R_print.right,
		rowlab, collab, rowname, colname);
    PrintDefaults(); /* reset, as na.print.etc may have been set */
    return x;
}/* do_prmatrix */

/* .Internal( print.function(f, useSource, ...)) */
SEXP attribute_hidden do_printfunction(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s = CAR(args);
    switch (TYPEOF(s)) {
    case CLOSXP:
	PrintLanguageEtc(s, asLogical(CADR(args)), /*is closure = */ TRUE);
	printAttributes(s, rho, FALSE);
	break;
    case BUILTINSXP:
    case SPECIALSXP:
	PrintSpecial(s);
	break;

    default: /* if(!isFunction(s)) */
	errorcall(call,
		  _("non-function argument to .Internal(print.function(.))"));
    }
    return s;
}

/* PrintLanguage() or PrintClosure() : */
static void PrintLanguageEtc(SEXP s, Rboolean useSource, Rboolean isClosure)
{
    int i;
    SEXP t = getAttrib(s, R_SrcrefSymbol);
    if (!isInteger(t) || !useSource)
	t = deparse1w(s, 0, useSource | DEFAULTDEPARSE);
    else {
        PROTECT(t = lang2(install("as.character"), t));
        t = eval(t, R_BaseEnv);
        UNPROTECT(1);
    }
    PROTECT(t);
    for (i = 0; i < LENGTH(t); i++)
	Rprintf("%s\n", CHAR(STRING_ELT(t, i))); /* translated */
    UNPROTECT(1);
    if (isClosure) {
	if (isByteCode(BODY(s))) Rprintf("<bytecode: %p>\n", BODY(s));
	t = CLOENV(s);
	if (t != R_GlobalEnv)
	    Rprintf("%s\n", EncodeEnvironment(t));
    }
}

static
void PrintClosure(SEXP s, Rboolean useSource)
{
    PrintLanguageEtc(s, useSource, TRUE);
}

static
void PrintLanguage(SEXP s, Rboolean useSource)
{
    PrintLanguageEtc(s, useSource, FALSE);
}

/* .Internal(print.default(x, digits, quote, na.print, print.gap,
			   right, max, useS4)) */
SEXP attribute_hidden do_printdefault(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, naprint;
    int tryS4;
    Rboolean callShow = FALSE;

    checkArity(op, args);
    PrintDefaults();

    x = CAR(args); args = CDR(args);

    if(!isNull(CAR(args))) {
	R_print.digits = asInteger(CAR(args));
	if (R_print.digits == NA_INTEGER ||
	    R_print.digits < R_MIN_DIGITS_OPT ||
	    R_print.digits > R_MAX_DIGITS_OPT)
	    error(_("invalid '%s' argument"), "digits");
    }
    args = CDR(args);

    R_print.quote = asLogical(CAR(args));
    if(R_print.quote == NA_LOGICAL)
	error(_("invalid '%s' argument"), "quote");
    args = CDR(args);

    naprint = CAR(args);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    error(_("invalid 'na.print' specification"));
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    (int) strlen(CHAR(R_print.na_string));
    }
    args = CDR(args);

    if(!isNull(CAR(args))) {
	R_print.gap = asInteger(CAR(args));
	if (R_print.gap == NA_INTEGER || R_print.gap < 0)
	    error(_("'gap' must be non-negative integer"));
    }
    args = CDR(args);

    R_print.right = (Rprt_adj) asLogical(CAR(args)); /* Should this be asInteger()? */
    if(R_print.right == NA_LOGICAL)
	error(_("invalid '%s' argument"), "right");
    args = CDR(args);

    if(!isNull(CAR(args))) {
	R_print.max = asInteger(CAR(args));
	if(R_print.max == NA_INTEGER || R_print.max < 0)
	    error(_("invalid '%s' argument"), "max");
	else if(R_print.max == INT_MAX) R_print.max--; // so we can add
    }
    args = CDR(args);

    R_print.useSource = asLogical(CAR(args));
    if(R_print.useSource == NA_LOGICAL)
	error(_("invalid '%s' argument"), "useSource");
    if(R_print.useSource) R_print.useSource = USESOURCE;
    args = CDR(args);

    tryS4 = asLogical(CAR(args));
    if(tryS4 == NA_LOGICAL)
	error(_("invalid 'tryS4' internal argument"));

    if(tryS4 && IS_S4_OBJECT(x) && isMethodsDispatchOn())
	callShow = TRUE;

    if(callShow) {
	/* we need to get show from the methods namespace if it is
	   not visible on the search path. */
	SEXP call, showS;
	showS = findVar(install("show"), rho);
	if(showS == R_UnboundValue) {
	    SEXP methodsNS = R_FindNamespace(mkString("methods"));
	    if(methodsNS == R_UnboundValue)
		error("missing methods namespace: this should not happen");
	    PROTECT(methodsNS);
	    showS = findVarInFrame3(methodsNS, install("show"), TRUE);
	    UNPROTECT(1);
	    if(showS == R_UnboundValue)
		error("missing show() in methods namespace: this should not happen");
	}
	PROTECT(call = lang2(showS, x));
	eval(call, rho);
	UNPROTECT(1);
    } else {
	CustomPrintValue(x, rho);
    }

    PrintDefaults(); /* reset, as na.print etc may have been set */
    return x;
}/* do_printdefault */


/* FIXME : We need a general mechanism for "rendering" symbols. */
/* It should make sure that it quotes when there are special */
/* characters and also take care of ansi escapes properly. */

static void PrintGenericVector(SEXP s, SEXP env)
{
    int i, taglen, ns, w, d, e, wr, dr, er, wi, di, ei;
    SEXP dims, t, names, newcall, tmp;
    char pbuf[115], *ptag, save[TAGBUFLEN0];

    ns = length(s);
    if((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	/* FIXME: check (ns <= R_print.max +1) ? ns : R_print.max; */
	for (i = 0; i < ns; i++) {
	    switch(TYPEOF(PROTECT(tmp = VECTOR_ELT(s, i)))) {
	    case NILSXP:
		snprintf(pbuf, 115, "NULL");
		break;
	    case LGLSXP:
		if (LENGTH(tmp) == 1) {
		    formatLogical(LOGICAL(tmp), 1, &w);
		    snprintf(pbuf, 115, "%s",
			     EncodeLogical(LOGICAL(tmp)[0], w));
		} else
		    snprintf(pbuf, 115, "Logical,%d", LENGTH(tmp));
		break;
	    case INTSXP:
		/* factors are stored as integers */
		if (inherits(tmp, "factor")) {
		    snprintf(pbuf, 115, "factor,%d", LENGTH(tmp));
		} else {
		    if (LENGTH(tmp) == 1) {
			formatInteger(INTEGER(tmp), 1, &w);
			snprintf(pbuf, 115, "%s",
				 EncodeInteger(INTEGER(tmp)[0], w));
		    } else
			snprintf(pbuf, 115, "Integer,%d", LENGTH(tmp));
		}
		break;
	    case REALSXP:
		if (LENGTH(tmp) == 1) {
		    formatReal(REAL(tmp), 1, &w, &d, &e, 0);
		    snprintf(pbuf, 115, "%s",
			     EncodeReal0(REAL(tmp)[0], w, d, e, OutDec));
		} else
		    snprintf(pbuf, 115, "Numeric,%d", LENGTH(tmp));
		break;
	    case CPLXSXP:
		if (LENGTH(tmp) == 1) {
		    Rcomplex *x = COMPLEX(tmp);
		    if (ISNA(x[0].r) || ISNA(x[0].i))
			/* formatReal(NA) --> w=R_print.na_width, d=0, e=0 */
			snprintf(pbuf, 115, "%s",
				 EncodeReal0(NA_REAL, R_print.na_width, 0, 0, OutDec));
		    else {
			formatComplex(x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
			snprintf(pbuf, 115, "%s",
				 EncodeComplex(x[0],
					       wr, dr, er, wi, di, ei, OutDec));
		    }
		} else
		snprintf(pbuf, 115, "Complex,%d", LENGTH(tmp));
		break;
	    case STRSXP:
		if (LENGTH(tmp) == 1) {
		    const void *vmax = vmaxget();
		    /* This can potentially overflow */
		    const char *ctmp = translateChar(STRING_ELT(tmp, 0));
		    int len = (int) strlen(ctmp);
		    if(len < 100)
			snprintf(pbuf, 115, "\"%s\"", ctmp);
		    else {
			snprintf(pbuf, 101, "\"%s\"", ctmp);
			pbuf[100] = '"'; pbuf[101] = '\0';
			strcat(pbuf, " [truncated]");
		    }
		    vmaxset(vmax);
		} else
		snprintf(pbuf, 115, "Character,%d", LENGTH(tmp));
		break;
	    case RAWSXP:
		snprintf(pbuf, 115, "Raw,%d", LENGTH(tmp));
		break;
	    case LISTSXP:
	    case VECSXP:
		snprintf(pbuf, 115, "List,%d", length(tmp));
		break;
	    case LANGSXP:
		snprintf(pbuf, 115, "Expression");
		break;
	    default:
		snprintf(pbuf, 115, "?");
		break;
	    }
	    UNPROTECT(1); /* tmp */
	    pbuf[114] = '\0';
	    SET_STRING_ELT(t, i, mkChar(pbuf));
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    const char *rn, *cn;
	    GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    /* as from 1.5.0: don't quote here as didn't in array case */
	    printMatrix(t, 0, dims, 0, R_print.right, rl, cl,
			rn, cn);
	}
	else {
	    names = GetArrayDimnames(s);
	    printArray(t, dims, 0, Rprt_adj_left, names);
	}
	UNPROTECT(2);
    }
    else { // no dim()
	names = getAttrib(s, R_NamesSymbol);
	taglen = (int) strlen(tagbuf);
	ptag = tagbuf + taglen;
	PROTECT(newcall = allocList(2));
	SETCAR(newcall, install("print"));
	SET_TYPEOF(newcall, LANGSXP);

	if(ns > 0) {
	    int n_pr = (ns <= R_print.max +1) ? ns : R_print.max;
	    /* '...max +1'  ==> will omit at least 2 ==> plural in msg below */
	    for (i = 0; i < n_pr; i++) {
		if (i > 0) Rprintf("\n");
		if (names != R_NilValue &&
		    STRING_ELT(names, i) != R_NilValue &&
		    *CHAR(STRING_ELT(names, i)) != '\0') {
		    const void *vmax = vmaxget();
		    /* Bug for L <- list(`a\\b` = 1, `a\\c` = 2)  :
		       const char *ss = translateChar(STRING_ELT(names, i));
		    */
		    const char *ss = EncodeChar(STRING_ELT(names, i));
		    if (taglen + strlen(ss) > TAGBUFLEN) {
		    	if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else {
			/* we need to distinguish character NA from "NA", which
			   is a valid (if non-syntactic) name */
			if (STRING_ELT(names, i) == NA_STRING)
			    sprintf(ptag, "$<NA>");
			else if( isValidName(ss) )
			    sprintf(ptag, "$%s", ss);
			else
			    sprintf(ptag, "$`%s`", ss);
		    }
		    vmaxset(vmax);
		}
		else {
		    if (taglen + IndexWidth(i) > TAGBUFLEN) {
		    	if (taglen <= TAGBUFLEN)
			    sprintf(ptag, "$...");
		    } else
			sprintf(ptag, "[[%d]]", i+1);
		}
		Rprintf("%s\n", tagbuf);
		if(isObject(VECTOR_ELT(s, i))) {
		    /* need to preserve tagbuf */
		    strcpy(save, tagbuf);
		    SETCADR(newcall, VECTOR_ELT(s, i));
		    eval(newcall, env);
		    strcpy(tagbuf, save);
		}
		else PrintValueRec(VECTOR_ELT(s, i), env);
		*ptag = '\0';
	    }
	    Rprintf("\n");
	    if(n_pr < ns)
		Rprintf(" [ reached getOption(\"max.print\") -- omitted %d entries ]\n",
			ns - n_pr);
	}
	else { /* ns = length(s) == 0 */
	    const void *vmax = vmaxget();
	    /* Formal classes are represented as empty lists */
	    const char *className = NULL;
	    SEXP klass;
	    if(isObject(s) && isMethodsDispatchOn()) {
		klass = getAttrib(s, R_ClassSymbol);
		if(length(klass) == 1) {
		    /* internal version of isClass() */
		    char str[201];
		    const char *ss = translateChar(STRING_ELT(klass, 0));
		    snprintf(str, 200, ".__C__%s", ss);
		    if(findVar(install(str), env) != R_UnboundValue)
			className = ss;
		}
	    }
	    if(className) {
		Rprintf("An object of class \"%s\"\n", className);
		UNPROTECT(1);
		printAttributes(s, env, TRUE);
		vmaxset(vmax);
		return;
	    }
	    else {
		if(names != R_NilValue) Rprintf("named ");
		Rprintf("list()\n");
	    }
	    vmaxset(vmax);
	}
	UNPROTECT(1);
    }
    printAttributes(s, env, FALSE);
} // PrintGenericVector


// For pairlist()s only --- the predecessor of PrintGenericVector() above,
// and hence very similar  (and no longer compatible!)
static void printList(SEXP s, SEXP env)
{
    int i, taglen;
    SEXP dims, dimnames, t, newcall;
    char pbuf[101], *ptag;
    const char *rn, *cn;

    if ((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	// special case: array-like list
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	i = 0;
	while(s != R_NilValue) {
	    switch(TYPEOF(CAR(s))) {

	    case NILSXP:
		snprintf(pbuf, 100, "NULL");
		break;

	    case LGLSXP:
		snprintf(pbuf, 100, "Logical,%d", LENGTH(CAR(s)));
		break;

	    case INTSXP:
	    case REALSXP:
		snprintf(pbuf, 100, "Numeric,%d", LENGTH(CAR(s)));
		break;

	    case CPLXSXP:
		snprintf(pbuf, 100, "Complex,%d", LENGTH(CAR(s)));
		break;

	    case STRSXP:
		snprintf(pbuf, 100, "Character,%d", LENGTH(CAR(s)));
		break;

	    case RAWSXP:
		snprintf(pbuf, 100, "Raw,%d", LENGTH(CAR(s)));
		break;

	    case LISTSXP:
		snprintf(pbuf, 100, "List,%d", length(CAR(s)));
		break;

	    case LANGSXP:
		snprintf(pbuf, 100, "Expression");
		break;

	    default:
		snprintf(pbuf, 100, "?");
		break;
	    }
	    pbuf[100] ='\0';
	    SET_STRING_ELT(t, i++, mkChar(pbuf));
	    s = CDR(s);
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    printMatrix(t, 0, dims, R_print.quote, R_print.right, rl, cl,
			rn, cn);
	}
	else {
	    dimnames = getAttrib(s, R_DimNamesSymbol);
	    printArray(t, dims, 0, Rprt_adj_left, dimnames);
	}
	UNPROTECT(2);
    }
    else { // no dim()
	i = 1;
	taglen = (int) strlen(tagbuf);
	ptag = tagbuf + taglen;
	PROTECT(newcall = allocList(2));
	SETCAR(newcall, install("print"));
	SET_TYPEOF(newcall, LANGSXP);
	while (TYPEOF(s) == LISTSXP) {
	    if (i > 1) Rprintf("\n");
	    if (TAG(s) != R_NilValue && isSymbol(TAG(s))) {
		if (taglen + strlen(CHAR(PRINTNAME(TAG(s)))) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else {
		    /* we need to distinguish character NA from "NA", which
		       is a valid (if non-syntactic) name */
		    if (PRINTNAME(TAG(s)) == NA_STRING)
			sprintf(ptag, "$<NA>");
		    else if( isValidName(CHAR(PRINTNAME(TAG(s)))) )
			sprintf(ptag, "$%s", CHAR(PRINTNAME(TAG(s))));
		    else
			sprintf(ptag, "$`%s`", EncodeChar(PRINTNAME(TAG(s))));
		}
	    }
	    else {
		if (taglen + IndexWidth(i) > TAGBUFLEN) {
		    if (taglen <= TAGBUFLEN)
			sprintf(ptag, "$...");
		} else
		    sprintf(ptag, "[[%d]]", i);
	    }
	    Rprintf("%s\n", tagbuf);
	    if(isObject(CAR(s))) {
		SETCADR(newcall, CAR(s));
		eval(newcall, env);
	    }
	    else PrintValueRec(CAR(s),env);
	    *ptag = '\0';
	    s = CDR(s);
	    i++;
	}
	if (s != R_NilValue) {
	    Rprintf("\n. \n\n");
	    PrintValueRec(s,env);
	}
	Rprintf("\n");
	UNPROTECT(1);
    }
    printAttributes(s, env, FALSE);
}

static void PrintExpression(SEXP s)
{
    SEXP u;
    int i, n;

    u = deparse1w(s, 0, R_print.useSource | DEFAULTDEPARSE);
    n = LENGTH(u);
    for (i = 0; i < n; i++)
	Rprintf("%s\n", CHAR(STRING_ELT(u, i))); /*translated */
}

static void PrintSpecial(SEXP s)
{
    /* This is OK as .Internals are not visible to be printed */
    char *nm = PRIMNAME(s);
    SEXP env, s2;
    PROTECT_INDEX xp;
    PROTECT_WITH_INDEX(env = findVarInFrame3(R_BaseEnv,
					     install(".ArgsEnv"), TRUE),
		       &xp);
    if (TYPEOF(env) == PROMSXP) REPROTECT(env = eval(env, R_BaseEnv), xp);
    s2 = findVarInFrame3(env, install(nm), TRUE);
    if(s2 == R_UnboundValue) {
	REPROTECT(env = findVarInFrame3(R_BaseEnv,
					install(".GenericArgsEnv"), TRUE),
		  xp);
	if (TYPEOF(env) == PROMSXP)
	    REPROTECT(env = eval(env, R_BaseEnv), xp);
	s2 = findVarInFrame3(env, install(nm), TRUE);
    }
    if(s2 != R_UnboundValue) {
	SEXP t;
	PROTECT(s2);
	t = deparse1(s2, 0, DEFAULTDEPARSE);
	Rprintf("%s ", CHAR(STRING_ELT(t, 0))); /* translated */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
	UNPROTECT(1);
    } else /* missing definition, e.g. 'if' */
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
    UNPROTECT(1);
}

/* PrintValueRec -- recursively print an SEXP

 * This is the "dispatching" function for  print.default()
 */
void attribute_hidden PrintValueRec(SEXP s, SEXP env)
{
    SEXP t;

#ifdef Win32
    WinCheckUTF8();
#endif
    if(!isMethodsDispatchOn() && (IS_S4_OBJECT(s) || TYPEOF(s) == S4SXP) ) {
	SEXP cl = getAttrib(s, R_ClassSymbol);
	if(isNull(cl)) {
	    /* This might be a mistaken S4 bit set */
	    if(TYPEOF(s) == S4SXP)
		Rprintf("<S4 object without a class>\n");
	    else
		Rprintf("<Object of type '%s' with S4 bit but without a class>\n",
			type2char(TYPEOF(s)));
	} else {
	    SEXP pkg = getAttrib(s, R_PackageSymbol);
	    if(isNull(pkg)) {
		Rprintf("<S4 object of class \"%s\">\n",
			CHAR(STRING_ELT(cl, 0)));
	    } else {
		Rprintf("<S4 object of class \"%s\" from package '%s'>\n",
			CHAR(STRING_ELT(cl, 0)), CHAR(STRING_ELT(pkg, 0)));
	    }
	}
	return;
    }
    switch (TYPEOF(s)) {
    case NILSXP:
	Rprintf("NULL\n");
	break;
    case SYMSXP: /* Use deparse here to handle backtick quotification
		  * of "weird names" */
	t = deparse1(s, 0, SIMPLEDEPARSE);
	Rprintf("%s\n", CHAR(STRING_ELT(t, 0))); /* translated */
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	PrintSpecial(s);
	break;
    case CHARSXP:
	Rprintf("<CHARSXP: ");
	Rprintf("%s", EncodeString(s, 0, '"', Rprt_adj_left));
	Rprintf(">\n");
        return; /* skip attribute printing for CHARSXP; they are used */
                /* in managing the CHARSXP cache. */
    case EXPRSXP:
	PrintExpression(s);
	break;
    case LANGSXP:
	PrintLanguage(s, FALSE);
	break;
    case CLOSXP:
	PrintClosure(s, FALSE);
	break;
    case ENVSXP:
	Rprintf("%s\n", EncodeEnvironment(s));
	break;
    case PROMSXP:
	Rprintf("<promise: %p>\n", s);
	break;
    case DOTSXP:
	Rprintf("<...>\n");
	break;
    case VECSXP:
	PrintGenericVector(s, env); /* handles attributes/slots */
	return;
    case LISTSXP:
	printList(s,env);
	break;
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case RAWSXP:
	PROTECT(t = getAttrib(s, R_DimSymbol));
	if (TYPEOF(t) == INTSXP) {
	    if (LENGTH(t) == 1) {
		const void *vmax = vmaxget();
		PROTECT(t = getAttrib(s, R_DimNamesSymbol));
		if (t != R_NilValue && VECTOR_ELT(t, 0) != R_NilValue) {
		    SEXP nn = getAttrib(t, R_NamesSymbol);
		    const char *title = NULL;

		    if (!isNull(nn))
			title = translateChar(STRING_ELT(nn, 0));

		    printNamedVector(s, VECTOR_ELT(t, 0), R_print.quote, title);
		}
		else
		    printVector(s, 1, R_print.quote);
		UNPROTECT(1);
		vmaxset(vmax);
	    }
	    else if (LENGTH(t) == 2) {
		SEXP rl, cl;
		const char *rn, *cn;
		GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
		printMatrix(s, 0, t, R_print.quote, R_print.right, rl, cl,
			    rn, cn);
	    }
	    else {
		SEXP dimnames;
		dimnames = GetArrayDimnames(s);
		printArray(s, t, R_print.quote, R_print.right, dimnames);
	    }
	}
	else {
	    UNPROTECT(1);
	    PROTECT(t = getAttrib(s, R_NamesSymbol));
	    if (t != R_NilValue)
		printNamedVector(s, t, R_print.quote, NULL);
	    else
		printVector(s, 1, R_print.quote);
	}
	UNPROTECT(1);
	break;
    case EXTPTRSXP:
	Rprintf("<pointer: %p>\n", R_ExternalPtrAddr(s));
	break;
    case BCODESXP:
	Rprintf("<bytecode: %p>\n", s);
	break;
    case WEAKREFSXP:
	Rprintf("<weak reference>\n");
	break;
    case S4SXP:
	/*  we got here because no show method, usually no class.
	    Print the "slots" as attributes, since we don't know the class.
	*/
	Rprintf("<S4 Type Object>\n");
	break;
    default:
	UNIMPLEMENTED_TYPE("PrintValueRec", s);
    }
    printAttributes(s, env, FALSE);
#ifdef Win32
    WinUTF8out = FALSE;
#endif
}

/* 2000-12-30 PR#715: remove list tags from tagbuf here
   to avoid $a$battr("foo").  Need to save and restore, since
   attributes might be lists with attributes or just have attributes ...
 */
static void printAttributes(SEXP s, SEXP env, Rboolean useSlots)
{
    SEXP a;
    char *ptag;
    char save[TAGBUFLEN0] = "\0";

    a = ATTRIB(s);
    if (a != R_NilValue) {
	strcpy(save, tagbuf);
	/* remove the tag if it looks like a list not an attribute */
	if (strlen(tagbuf) > 0 &&
	    *(tagbuf + strlen(tagbuf) - 1) != ')')
	    tagbuf[0] = '\0';
	ptag = tagbuf + strlen(tagbuf);
	while (a != R_NilValue) {
	    if(useSlots && TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    if(isArray(s) || isList(s)) {
		if(TAG(a) == R_DimSymbol ||
		   TAG(a) == R_DimNamesSymbol)
		    goto nextattr;
	    }
	    if(inherits(s, "factor")) {
		if(TAG(a) == R_LevelsSymbol)
		    goto nextattr;
		if(TAG(a) == R_ClassSymbol)
		    goto nextattr;
	    }
	    if(isFrame(s)) {
		if(TAG(a) == R_RowNamesSymbol)
		    goto nextattr;
	    }
	    if(!isArray(s)) {
		if (TAG(a) == R_NamesSymbol)
		    goto nextattr;
	    }
	    if(TAG(a) == R_CommentSymbol || TAG(a) == R_SrcrefSymbol
	       || TAG(a) == R_WholeSrcrefSymbol || TAG(a) == R_SrcfileSymbol)
		goto nextattr;
	    if(useSlots)
		sprintf(ptag, "Slot \"%s\":", EncodeChar(PRINTNAME(TAG(a))));
	    else
		sprintf(ptag, "attr(,\"%s\")", EncodeChar(PRINTNAME(TAG(a))));
	    Rprintf("%s", tagbuf); Rprintf("\n");
	    if (TAG(a) == R_RowNamesSymbol) {
		/* need special handling AND protection */
		SEXP val;
		PROTECT(val = getAttrib(s, R_RowNamesSymbol));
		PrintValueRec(val, env);
		UNPROTECT(1);
		goto nextattr;
	    }
	    if (isMethodsDispatchOn() && IS_S4_OBJECT(CAR(a))) {
		SEXP s, showS;

		showS = findVar(install("show"), env);
		if(showS == R_UnboundValue) {
		    SEXP methodsNS = R_FindNamespace(mkString("methods"));
		    if(methodsNS == R_UnboundValue)
			error("missing methods namespace: this should not happen");
		    PROTECT(showS);
		    PROTECT(methodsNS);
		    showS = findVarInFrame3(methodsNS, install("show"), TRUE);
		    UNPROTECT(2);
		    if(showS == R_UnboundValue)
			error("missing show() in methods namespace: this should not happen");
		}
		PROTECT(s = lang2(showS, CAR(a)));
		eval(s, env);
		UNPROTECT(1);
	    } else if (isObject(CAR(a))) {
		/* Need to construct a call to
		   print(CAR(a), digits)
		   based on the R_print structure, then eval(call, env).
		   See do_docall for the template for this sort of thing.

		   quote, right, gap should probably be included if
		   they have non-missing values.

		   This will not dispatch to show() as 'digits' is supplied.
		*/
		SEXP s, t, na_string = R_print.na_string,
		    na_string_noquote = R_print.na_string_noquote;
		int quote = R_print.quote,
		    digits = R_print.digits, gap = R_print.gap,
		    na_width = R_print.na_width,
		    na_width_noquote = R_print.na_width_noquote;
		Rprt_adj right = R_print.right;

		PROTECT(t = s = allocList(3));
		SET_TYPEOF(s, LANGSXP);
		SETCAR(t, install("print")); t = CDR(t);
		SETCAR(t,  CAR(a)); t = CDR(t);
		SETCAR(t, ScalarInteger(digits));
		SET_TAG(t, install("digits"));
		eval(s, env);
		UNPROTECT(1);
		R_print.quote = quote;
		R_print.right = right;
		R_print.digits = digits;
		R_print.gap = gap;
		R_print.na_width = na_width;
		R_print.na_width_noquote = na_width_noquote;
		R_print.na_string = na_string;
		R_print.na_string_noquote = na_string_noquote;
	    } else
		PrintValueRec(CAR(a), env);
	nextattr:
	    *ptag = '\0';
	    a = CDR(a);
	}
	strcpy(tagbuf, save);
    }
}/* printAttributes */


/* Print an S-expression using (possibly) local options.
   This is used for auto-printing from main.c */

void attribute_hidden PrintValueEnv(SEXP s, SEXP env)
{
    PrintDefaults();
    tagbuf[0] = '\0';
    PROTECT(s);
    if(isObject(s) || isFunction(s)) {
	/*
	  The intention here is to call show() on S4 objects, otherwise
	  print(), so S4 methods for show() have precedence over those for
	  print() to conform with the "green book", p. 332
	*/
	SEXP call, showS, prinfun;
	SEXP xsym = install("x");
	if(isMethodsDispatchOn() && IS_S4_OBJECT(s)) {
	    /*
	      Note that we cannot assume that show() is visible from
	      'env', but we can assume there is a loaded "methods"
	      namespace.  It is tempting to cache the value of show in
	      the namespace, but the latter could be unloaded and
	      reloaded in a session.
	    */
	    showS = findVar(install("show"), env);
	    if(showS == R_UnboundValue) {
		SEXP methodsNS = R_FindNamespace(mkString("methods"));
		if(methodsNS == R_UnboundValue)
		    error("missing methods namespace: this should not happen");
		PROTECT(methodsNS);
		showS = findVarInFrame3(methodsNS, install("show"), TRUE);
		UNPROTECT(1);
		if(showS == R_UnboundValue)
		    error("missing show() in methods namespace: this should not happen");
	    }
	    prinfun = showS;
	}
	else /* S3 */
	    prinfun = install("print");

	/* Bind value to a variable in a local environment, similar to
	   a local({ x <- <value>; print(x) }) call. This avoids
	   problems in previous approaches with value duplication and
	   evaluating the value, which might be a call object. */
	PROTECT(call = lang2(prinfun, xsym));
	PROTECT(env = NewEnvironment(R_NilValue, R_NilValue, env));
	defineVar(xsym, s, env);
	eval(call, env);
	defineVar(xsym, R_NilValue, env); /* to eliminate reference to s */
	UNPROTECT(2);
    } else PrintValueRec(s, env);
    UNPROTECT(1);
}


/* Print an S-expression using global options */

void PrintValue(SEXP s)
{
    PrintValueEnv(s, R_GlobalEnv);
}


/* Ditto, but only for objects, for use in debugging */

void R_PV(SEXP s)
{
    if(isObject(s)) PrintValueEnv(s, R_GlobalEnv);
}


void attribute_hidden CustomPrintValue(SEXP s, SEXP env)
{
    tagbuf[0] = '\0';
    PrintValueRec(s, env);
}


/* xxxpr are mostly for S compatibility (as mentioned in V&R).
   The actual interfaces are now in xxxpr.f
 */

attribute_hidden
int F77_NAME(dblep0) (const char *label, int *nchar, double *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = (int) strlen(label);
    if(nc > 255) {
	warning(_("invalid character length in 'dblepr'"));
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printRealVector(data, *ndata, 1);
    return(0);
}

attribute_hidden
int F77_NAME(intpr0) (const char *label, int *nchar, int *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = (int) strlen(label);
    if(nc > 255) {
	warning(_("invalid character length in 'intpr'"));
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printIntegerVector(data, *ndata, 1);
    return(0);
}

attribute_hidden
int F77_NAME(realp0) (const char *label, int *nchar, float *data, int *ndata)
{
    int k, nc = *nchar, nd = *ndata;
    double *ddata;

    if(nc < 0) nc = (int) strlen(label);
    if(nc > 255) {
	warning(_("invalid character length in 'realpr'"));
	nc = 0;
    }
    else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(nd > 0) {
	ddata = (double *) malloc(nd*sizeof(double));
	if(!ddata) error(_("memory allocation error in 'realpr'"));
	for (k = 0; k < nd; k++) ddata[k] = (double) data[k];
	printRealVector(ddata, nd, 1);
	free(ddata);
    }
    return(0);
}

/* Fortran-callable error routine for lapack */

void NORET F77_NAME(xerbla)(const char *srname, int *info)
{
   /* srname is not null-terminated.  It should be 6 characters. */
    char buf[7];
    strncpy(buf, srname, 6);
    buf[6] = '\0';
    error(_("BLAS/LAPACK routine '%6s' gave error code %d"), buf, -(*info));
}
