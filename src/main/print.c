/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995-1998	Robert Gentleman and Ross Ihaka.
 *  Copyright (C) 2000-2003	The R Development Core Team.
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
 *
 *
 *  print.default()  ->	 do_printdefault & its sub-functions.
 *			 do_sink, do_invisible
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
 *  do_sink.c moved to connections.c as of 1.3.0
 *
 *  <FIXME> These routines are not re-entrant: they reset the
 *  global R_print.
 *  </FIXME>
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"
#include "Fileio.h"
#include "Rconnections.h"
#include <S.h>

/* Global print parameter struct: */
R_print_par_t R_print;

static void printAttributes(SEXP, SEXP, Rboolean);

#define TAGBUFLEN 256
static char tagbuf[TAGBUFLEN + 5];


void PrintDefaults(SEXP rho)
{
    R_print.na_string = NA_STRING;
    R_print.na_string_noquote = mkChar("<NA>");
    R_print.na_width = strlen(CHAR(R_print.na_string));
    R_print.na_width_noquote = strlen(CHAR(R_print.na_string_noquote));
    R_print.quote = 1;
    R_print.right = 0;
    R_print.digits = GetOptionDigits(rho);
    R_print.gap = 1;
    R_print.width = GetOptionWidth(rho);
}

SEXP do_invisible(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    switch (length(args)) {
    case 0:
	return R_NilValue;
    case 1:
	return CAR(args);
    default:
	checkArity(op, args);
	return call;/* never used, just for -Wall */
    }
}

#if 0
SEXP do_visibleflag(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    return ScalarLogical(R_Visible);
}
#endif

SEXP do_prmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int quote;
    SEXP a, x, rowlab, collab, naprint;
    char *rowname = NULL, *colname = NULL;

    checkArity(op,args);
    PrintDefaults(rho);
    a = args;
    x = CAR(a); a = CDR(a);
    rowlab = CAR(a); a = CDR(a);
    collab = CAR(a); a = CDR(a);

    quote = asInteger(CAR(a)); a = CDR(a);
    R_print.right = asInteger(CAR(a)); a = CDR(a);
    naprint = CAR(a);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    errorcall(call, "invalid na.print specification");
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    strlen(CHAR(R_print.na_string));
    }

    if (length(rowlab) == 0) rowlab = R_NilValue;
    if (length(collab) == 0) collab = R_NilValue;
    if (!isNull(rowlab) && !isString(rowlab))
	errorcall(call, "invalid row labels");
    if (!isNull(collab) && !isString(collab))
	errorcall(call, "invalid column labels");

    printMatrix(x, 0, getAttrib(x, R_DimSymbol), quote, R_print.right,
		rowlab, collab, rowname, colname);
    PrintDefaults(rho); /* reset, as na.print.etc may have been set */
    return x;
}/* do_prmatrix */


/* .Internal(print.default(x, digits, quote, na.print, print.gap, 
                           right, useS4)) */
SEXP do_printdefault(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP x, naprint;
    int tryS4;
    Rboolean callShow = FALSE;

    checkArity(op, args);
    PrintDefaults(rho);

    x = CAR(args); args = CDR(args);

    if(!isNull(CAR(args))) {
	R_print.digits = asInteger(CAR(args));
	if (R_print.digits == NA_INTEGER ||
	    R_print.digits < R_MIN_DIGITS_OPT ||
	    R_print.digits > R_MAX_DIGITS_OPT)
		errorcall(call, "invalid digits parameter");
    }
    args = CDR(args);

    R_print.quote = asLogical(CAR(args));
    if(R_print.quote == NA_LOGICAL)
	errorcall(call, "invalid quote parameter");
    args = CDR(args);

    naprint = CAR(args);
    if(!isNull(naprint))  {
	if(!isString(naprint) || LENGTH(naprint) < 1)
	    errorcall(call, "invalid na.print specification");
	R_print.na_string = R_print.na_string_noquote = STRING_ELT(naprint, 0);
	R_print.na_width = R_print.na_width_noquote =
	    strlen(CHAR(R_print.na_string));
    }
    args = CDR(args);

    if(!isNull(CAR(args))) {
	R_print.gap = asInteger(CAR(args));
	if (R_print.gap == NA_INTEGER || R_print.gap < 1 || R_print.gap > 10)
	    errorcall(call, "invalid gap parameter");
    }
    args = CDR(args);

    R_print.right = asLogical(CAR(args));
    if(R_print.right == NA_LOGICAL)
	errorcall(call, "invalid right parameter");
    args = CDR(args);

    tryS4 = asLogical(CAR(args));
    if(R_print.right == NA_LOGICAL)
	errorcall(call, "invalid tryS4 internal parameter");

    if(tryS4 && isObject(x) && isMethodsDispatchOn()) {
	SEXP class = getAttrib(x, R_ClassSymbol);
	if(length(class) == 1) {
	    /* internal version of isClass() */
	    char str[201];
	    snprintf(str, 200, ".__C__%s", CHAR(STRING_ELT(class, 0)));
	    if(findVar(install(str), rho) != R_UnboundValue)
		callShow = TRUE;
	}
    }

    if(callShow) {
	SEXP call;
	PROTECT(call = lang2(install("show"), x));
	eval(call, rho);
	UNPROTECT(1);
    } else {
	CustomPrintValue(x, rho);
    }
    
    PrintDefaults(rho); /* reset, as na.print.etc may have been set */
    return x;
}/* do_printdefault */


/* FIXME : We need a general mechanism for "rendering" symbols. */
/* It should make sure that it quotes when there are special */
/* characters and also take care of ansi escapes properly. */

static void PrintGenericVector(SEXP s, SEXP env)
{
    int i, taglen, ns, w, d, e, wr, dr, er, wi, di, ei;
    SEXP dims, t, names, newcall, tmp;
    char *pbuf, *ptag, *rn, *cn, save[TAGBUFLEN + 5];

    ns = length(s);
    if((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	for (i = 0 ; i < ns ; i++) {
	    switch(TYPEOF(PROTECT(tmp = VECTOR_ELT(s, i)))) {
	    case NILSXP:
		pbuf = Rsprintf("NULL");
		break;
	    case LGLSXP:
		if (LENGTH(tmp) == 1) {
		    formatLogical(LOGICAL(tmp), 1, &w);
		    pbuf = Rsprintf("%s", EncodeLogical(LOGICAL(tmp)[0], w));
		} else
		    pbuf = Rsprintf("Logical,%d", LENGTH(tmp));
		break;
	    case INTSXP:
		/* factors are stored as integers */
		if (inherits(tmp, "factor")) {
		    pbuf = Rsprintf("factor,%d", LENGTH(tmp));
		} else {
		    if (LENGTH(tmp) == 1) {
			formatInteger(INTEGER(tmp), 1, &w);
			pbuf = Rsprintf("%s", EncodeInteger(INTEGER(tmp)[0],
							    w));
		    } else
			pbuf = Rsprintf("Integer,%d", LENGTH(tmp));
		}
		break;
	    case REALSXP:
		if (LENGTH(tmp) == 1) {
		    formatReal(REAL(tmp), 1, &w, &d, &e, 0);
		    pbuf = Rsprintf("%s", EncodeReal(REAL(tmp)[0], w, d, e));
		} else
		    pbuf = Rsprintf("Numeric,%d", LENGTH(tmp));
		break;
	    case CPLXSXP:
		if (LENGTH(tmp) == 1) {
		    Rcomplex *x = COMPLEX(tmp);
		    formatComplex(x, 1, &wr, &dr, &er, &wi, &di, &ei, 0);
		    if (ISNA(x[0].r) || ISNA(x[0].i))
			pbuf = Rsprintf("%s", EncodeReal(NA_REAL, w, 0, 0));
		    else
			pbuf = Rsprintf("%s", EncodeComplex(x[0],
			wr, dr, er, wi, di, ei));
		} else
		pbuf = Rsprintf("Complex,%d", LENGTH(tmp));
		break;
	    case STRSXP:
		if (LENGTH(tmp) == 1) {
		    pbuf = Rsprintf("\"%s\"", CHAR(STRING_ELT(tmp, 0)));
		} else
		pbuf = Rsprintf("Character,%d", LENGTH(tmp));
		break;
	    case LISTSXP:
	    case VECSXP:
		pbuf = Rsprintf("List,%d", length(tmp));
		break;
	    case LANGSXP:
		pbuf = Rsprintf("Expression");
		break;
	    default:
		pbuf = Rsprintf("?");
		break;
	    }
	    UNPROTECT(1); /* tmp */
	    SET_STRING_ELT(t, i, mkChar(pbuf));
	}
	if (LENGTH(dims) == 2) {
	    SEXP rl, cl;
	    GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
	    /* as from 1.5.0: don't quote here as didn't in array case */
	    printMatrix(t, 0, dims, 0, R_print.right, rl, cl,
			rn, cn);
	}
	else {
	    names = GetArrayDimnames(s);
	    printArray(t, dims, 0, names);
	}
	UNPROTECT(2);
    }
    else {
	names = getAttrib(s, R_NamesSymbol);
	taglen = strlen(tagbuf);
	ptag = tagbuf + taglen;
	PROTECT(newcall = allocList(2));
	SETCAR(newcall, install("print"));
	SET_TYPEOF(newcall, LANGSXP);

	if(ns > 0) {
	for (i = 0 ; i < ns ; i++) {
	    if (i > 0) Rprintf("\n");
	    if (names != R_NilValue &&
		STRING_ELT(names, i) != R_NilValue &&
		*CHAR(STRING_ELT(names, i)) != '\0') {
		if (taglen + strlen(CHAR(STRING_ELT(names, i))) > TAGBUFLEN)
		    sprintf(ptag, "$...");
		else {
		    if( isValidName(CHAR(STRING_ELT(names, i))) )
			sprintf(ptag, "$%s", CHAR(STRING_ELT(names, i)));
		    else
			sprintf(ptag, "$\"%s\"", CHAR(STRING_ELT(names, i)));
		}
	    }
	    else {
		if (taglen + IndexWidth(i) > TAGBUFLEN)
		    sprintf(ptag, "$...");
		else
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
	}
	else {
	    /* Formal classes are represented as empty lists */
	    char *className = NULL;
	    SEXP class;
	    if(isObject(s) && isMethodsDispatchOn()) {
		class = getAttrib(s, R_ClassSymbol);
		if(length(class) == 1) {
		    /* internal version of isClass() */
		    char str[201];
		    snprintf(str, 200, ".__C__%s", CHAR(STRING_ELT(class, 0)));
		    if(findVar(install(str), env) != R_UnboundValue)
			className = CHAR(STRING_ELT(class, 0));
		}
	    }
	    if(className) {
		Rprintf("An object of class \"%s\"\n", className);
		UNPROTECT(1);
		printAttributes(s, env, TRUE);
		return;
	    } else
		Rprintf("list()\n");
	}
	UNPROTECT(1);
    }
    printAttributes(s, env, FALSE);
}


static void printList(SEXP s, SEXP env)
{
    int i, taglen;
    SEXP dims, dimnames, t, newcall;
    char *pbuf, *ptag, *rn, *cn;

    if ((dims = getAttrib(s, R_DimSymbol)) != R_NilValue && length(dims) > 1) {
	PROTECT(dims);
	PROTECT(t = allocArray(STRSXP, dims));
	i = 0;
	while(s != R_NilValue) {
	    switch(TYPEOF(CAR(s))) {

	    case NILSXP:
		pbuf = Rsprintf("NULL");
		break;

	    case LGLSXP:
		pbuf = Rsprintf("Logical,%d", LENGTH(CAR(s)));
		break;

	    case INTSXP:
	    case REALSXP:
		pbuf = Rsprintf("Numeric,%d", LENGTH(CAR(s)));
		break;

	    case CPLXSXP:
		pbuf = Rsprintf("Complex,%d", LENGTH(CAR(s)));
		break;

	    case STRSXP:
		pbuf = Rsprintf("Character,%d", LENGTH(CAR(s)));
		break;

	    case LISTSXP:
		pbuf = Rsprintf("List,%d", length(CAR(s)));
		break;

	    case LANGSXP:
		pbuf = Rsprintf("Expression");
		break;

	    default:
		pbuf = Rsprintf("?");
		break;
	    }
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
	    printArray(t, dims, 0, dimnames);
	}
	UNPROTECT(2);
    }
    else {
	i = 1;
	taglen = strlen(tagbuf);
	ptag = tagbuf + taglen;
	PROTECT(newcall = allocList(2));
	SETCAR(newcall, install("print"));
	SET_TYPEOF(newcall, LANGSXP);
	while (TYPEOF(s) == LISTSXP) {
	    if (i > 1) Rprintf("\n");
	    if (TAG(s) != R_NilValue && isSymbol(TAG(s))) {
		if (taglen + strlen(CHAR(PRINTNAME(TAG(s)))) > TAGBUFLEN)
		    sprintf(ptag, "$...");
		else {
		    if( isValidName(CHAR(PRINTNAME(TAG(s)))) )
			sprintf(ptag, "$%s", CHAR(PRINTNAME(TAG(s))));
		    else
			sprintf(ptag, "$\"%s\"", CHAR(PRINTNAME(TAG(s))));
		}
	    }
	    else {
		if (taglen + IndexWidth(i) > TAGBUFLEN)
		    sprintf(ptag, "$...");
		else
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

    u = deparse1(s, 0);
    n = LENGTH(u);
    for (i = 0; i < n ; i++)
	Rprintf("%s\n", CHAR(STRING_ELT(u, i)));
}


/* PrintValueRec -- recursively print an SEXP

 * This is the "dispatching" function for  print.default()
 */

static void PrintEnvir(SEXP rho)
{
    if (rho == R_GlobalEnv)
	Rprintf("<environment: R_GlobalEnv>\n");
    else if (R_IsPackageEnv(rho))
	Rprintf("<environment: %s>\n",
		CHAR(STRING_ELT(R_PackageEnvName(rho), 0)));
#ifdef EXPERIMENTAL_NAMESPACES
    else if (R_IsNamespaceEnv(rho))
	Rprintf("<environment: namespace:%s>\n",
		CHAR(STRING_ELT(R_NamespaceEnvSpec(rho), 0)));
#endif
    else Rprintf("<environment: %p>\n", rho);
}

void PrintValueRec(SEXP s,SEXP env)
{
    int i;
    SEXP t;

    switch (TYPEOF(s)) {
    case NILSXP:
	Rprintf("NULL\n");
	break;
    case SYMSXP:
	Rprintf("%s\n", CHAR(PRINTNAME(s)));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	Rprintf(".Primitive(\"%s\")\n", PRIMNAME(s));
	break;
    case CHARSXP:
	Rprintf("<CHARSXP: ");
	Rprintf(EncodeString(CHAR(s), 0, '"', Rprt_adj_left));
	Rprintf(">\n");
	break;
    case EXPRSXP:
	PrintExpression(s);
	break;
    case CLOSXP:
    case LANGSXP:
	t = getAttrib(s, R_SourceSymbol);
	if (isNull(t))
	    t = deparse1(s, 0);
	for (i = 0; i < LENGTH(t); i++)
	    Rprintf("%s\n", CHAR(STRING_ELT(t, i)));
	if (TYPEOF(s) == CLOSXP) t = CLOENV(s);
	else t = R_GlobalEnv;
	if (t != R_GlobalEnv)
	    PrintEnvir(t);
	break;
    case ENVSXP:
	PrintEnvir(s);
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
	PROTECT(t = getAttrib(s, R_DimSymbol));
	if (TYPEOF(t) == INTSXP) {
	    if (LENGTH(t) == 1) {
		PROTECT(t = getAttrib(s, R_DimNamesSymbol));
		if (t != R_NilValue && VECTOR_ELT(t, 0) != R_NilValue) {
		    SEXP nn = getAttrib(t, R_NamesSymbol);
		    char *title = NULL;

		    if (!isNull(nn))
		        title = CHAR(STRING_ELT(nn, 0));

		    printNamedVector(s, VECTOR_ELT(t, 0), R_print.quote, title);
		}
		else
		    printVector(s, 1, R_print.quote);
		UNPROTECT(1);
	    }
	    else if (LENGTH(t) == 2) {
		SEXP rl, cl;
		char *rn, *cn;
		GetMatrixDimnames(s, &rl, &cl, &rn, &cn);
		printMatrix(s, 0, t, R_print.quote, R_print.right, rl, cl,
			    rn, cn);
	    }
	    else {
		SEXP dimnames;
		dimnames = GetArrayDimnames(s);
		printArray(s, t, R_print.quote, dimnames);
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
    case WEAKREFSXP:
	Rprintf("<weak reference>\n");
	break;
    default:
	UNIMPLEMENTED("PrintValueRec");
    }
    printAttributes(s,env, FALSE);
}

/* 2000-12-30 PR#715: remove list tags from tagbuf here
   to avoid $a$battr("foo").  Need to save and restore, since
   attributes might be lists with attributes or just have attributes ...
 */
static void printAttributes(SEXP s, SEXP env, Rboolean useSlots)
{
    SEXP a;
    char *ptag;
    char save[TAGBUFLEN + 5] = "\0";

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
	    if(isFactor(s)) {
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
	    if(TAG(a) == R_CommentSymbol || TAG(a) == R_SourceSymbol)
		goto nextattr;
	    if(useSlots)
		sprintf(ptag, "Slot \"%s\":",
			EncodeString(CHAR(PRINTNAME(TAG(a))), 0, 0,
				     Rprt_adj_left));
	    else
		sprintf(ptag, "attr(,\"%s\")",
			EncodeString(CHAR(PRINTNAME(TAG(a))), 0, 0,
				     Rprt_adj_left));
	    Rprintf("%s", tagbuf); Rprintf("\n");
	    if (isObject(CAR(a))) {
		/* Need to construct a call to
		   print(CAR(a), digits)
		   based on the R_print structure, then eval(call, env).
		   See do_docall for the template for this sort of thing.

		   quote, right, gap should probably be included if
		   they have non-missing values.
		*/
		SEXP s, t, na_string = R_print.na_string,
		    na_string_noquote = R_print.na_string_noquote;
		int quote = R_print.quote, right = R_print.right,
		    digits = R_print.digits, gap = R_print.gap,
		    na_width = R_print.na_width,
		    na_width_noquote = R_print.na_width_noquote;
		PROTECT(t = s = allocList(3));
		SET_TYPEOF(s, LANGSXP);
		SETCAR(t, install("print")); t = CDR(t);
		SETCAR(t,  CAR(a)); t = CDR(t);
		SETCAR(t, allocVector(INTSXP, 1));
		INTEGER(CAR(t))[0] = digits;
		SET_TAG(t, install("digits")); /* t = CDR(t);
		CAR(t) = allocVector(LGLSXP, 1);
		LOGICAL(CAR(t))[0] = quote;
		SET_TAG(t, install("quote")); t = CDR(t);
		CAR(t) = allocVector(LGLSXP, 1);
		LOGICAL(CAR(t))[0] = right;
		SET_TAG(t, install("right")); t = CDR(t);
		CAR(t) = allocVector(INTSXP, 1);
		INTEGER(CAR(t))[0] = gap;
		SET_TAG(t, install("gap")); */
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
   This is used for auto-printing */

void PrintValueEnv(SEXP s, SEXP env)
{
    SEXP call;
    char *autoprint = "print";

    PrintDefaults(env);
    tagbuf[0] = '\0';
    PROTECT(s);
    if(isObject(s)) {
	/* The intention here is call show() on S4 objects, otherwise
	   print(), so S4 methods for show() have precedence over those for
	   print(). We decided not to do this, at least for now
	*/
        /*if(isMethodsDispatchOn()) {
	    SEXP class = getAttrib(s, R_ClassSymbol);
	    if(length(class) == 1) {
		char str[201];
		snprintf(str, 200, ".__C__%s", CHAR(STRING_ELT(class, 0)));
		if(findVar(install(str), env) != R_UnboundValue)
		    autoprint = "show";
	    }
	}*/
	PROTECT(call = lang2(install(autoprint), s));
	eval(call, env);
	UNPROTECT(1);
    }
    else {
	PrintValueRec(s, env);
    }
    UNPROTECT(1);
}


/* Print an S-expression using global options */

void PrintValue(SEXP s)
{
    PrintValueEnv(s, R_NilValue);
}


/* Ditto, but only for objects, for use in debugging */

void R_PV(SEXP s)
{
    if(isObject(s)) PrintValueEnv(s, R_NilValue);
}


void CustomPrintValue(SEXP s, SEXP env)
{
    tagbuf[0] = '\0';
    PrintValueRec(s, env);
}


/* xxxpr are mostly for S compatibility (as mentioned in V&R) */

int F77_NAME(dblep0) (char *label, int *nchar, double *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = strlen(label);
    if(nc > 255) {
	warning("invalid character length in dblepr");
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printRealVector(data, *ndata, 1);
    return(0);
}

int F77_NAME(intpr0) (char *label, int *nchar, int *data, int *ndata)
{
    int k, nc = *nchar;

    if(nc < 0) nc = strlen(label);
    if(nc > 255) {
	warning("invalid character length in intpr");
	nc = 0;
    } else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(*ndata > 0) printIntegerVector(data, *ndata, 1);
    return(0);
}

int F77_NAME(realp0) (char *label, int *nchar, float *data, int *ndata)
{
    int k, nc = *nchar, nd=*ndata;
    double *ddata;

    if(nc < 0) nc = strlen(label);
    if(nc > 255) {
	warning("invalid character length in realpr");
	nc = 0;
    }
    else if(nc > 0) {
	for (k = 0; k < nc; k++)
	    Rprintf("%c", label[k]);
	Rprintf("\n");
    }
    if(nd > 0) {
	ddata = malloc(nd*sizeof(double));
	if(!ddata) error("memory allocation error in realpr");
	for (k = 0; k < nd; k++) ddata[k] = (double) data[k];
	printRealVector(ddata, nd, 1);
	free(ddata);
    }
    return(0);
}

/* Fortran-callable error routine for lapack */

void F77_NAME(xerbla)(char *srname, int *info)
{
   /* srname is not null-terminated.  It should be 6 characters. */
    char buf[7];
    strncpy(buf, srname, 6);
    buf[6] = '\0';
    error("LAPACK routine %6s gave error code %d", buf, -(*info));
}
