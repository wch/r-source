
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

/*== see ./printutils.c  for general remarks on Printing and the Encode.. utils.
 */
#include "Defn.h"
#include "Print.h"
#include "Fileio.h"

static void printAttributes(SEXP);
void PrintValueRec(SEXP);

int R_print_width;
SEXP print_na_string;
int print_na_width;
int print_quote;
int print_digits;
int print_gap;

#define TAGBUFLEN 256
static char tagbuf[TAGBUFLEN + 5];


void PrintDefaults(SEXP rho)
{
	print_na_string = NA_STRING;
	print_na_width = strlen(CHAR(print_na_string));
	print_quote = 1;
	print_digits = GetOptionDigits(rho);
	print_gap = 1;
	R_print_width = GetOptionWidth(rho);
}

SEXP do_sink(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	FILE *fp;

	switch (length(args)) {
	case 0:
		if(R_Sinkfile) fclose(R_Sinkfile);
		R_Sinkfile = NULL;
		R_Outputfile = R_Consolefile;
		return R_NilValue;
	case 1:
		if(TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
			errorcall(call, "invalid file name\n");
		if( !(fp=R_fopen(CHAR(STRING(CAR(args))[0]), "w")))
			errorcall(call, "unable to open file\n");
		R_Sinkfile = fp;
		R_Outputfile = fp;
		return R_NilValue;
	default:
		checkArity(op, args);
	}
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
	}
}

SEXP do_printmatrix(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	int quote, right;
	SEXP a, x, rowlab, collab, oldnames;

	checkArity(op,args);

	PrintDefaults(rho);

	a=args;
	x = CAR(a); a=CDR(a);
	rowlab = CAR(a); a= CDR(a);
	collab = CAR(a); a= CDR(a);
	quote = asInteger(CAR(a)); a=CDR(a);
	right = asInteger(CAR(a));

	PROTECT(oldnames = getAttrib(x,R_DimNamesSymbol));
	/* fix up the dimnames */
	if( length(rowlab) || length(collab) || rowlab==R_NilValue ||
			collab==R_NilValue ) {
		a = oldnames;
		if( a == R_NilValue )
			a = allocList(2);
		if( length(rowlab) || rowlab==R_NilValue )
			CAR(a) = rowlab;
		if( length(collab) || collab==R_NilValue )
			CADR(a) = collab;
		PROTECT(a);
		setAttrib(x,R_DimNamesSymbol,a);
		UNPROTECT(1);
	}
	printMatrix(x, 0, getAttrib(x,R_DimSymbol),quote);
	setAttrib(x,R_DimNamesSymbol, oldnames);
	UNPROTECT(1);
	return x;
}

	
SEXP do_printdefault(SEXP call, SEXP op, SEXP args, SEXP rho)
{
	SEXP x, naprint;

	checkArity(op, args);
	
	PrintDefaults(rho);

	x = CAR(args); args = CDR(args);

	if(!isNull(CAR(args))) {
		print_digits = asInteger(CAR(args));
		if (print_digits == NA_INTEGER || print_digits < 1 || print_digits > 22)
			errorcall(call, "invalid digits parameter\n");
	}
	args = CDR(args);

	print_quote = asLogical(CAR(args));
	if(print_quote == NA_LOGICAL)
		errorcall(call, "invalid quote parameter\n");
	args = CDR(args);

	naprint = CAR(args);
	if(!isNull(naprint))  {
		if(!isString(naprint) || LENGTH(naprint) < 1)
			errorcall(call, "invalid na.print specification\n");
		print_na_string = STRING(naprint)[0];
		print_na_width = strlen(CHAR(print_na_string));
	}
	args = CDR(args);

	if(!isNull(CAR(args))) {
		print_gap = asInteger(CAR(args));
		if (print_gap == NA_INTEGER || print_gap < 1 || print_gap > 10)
			errorcall(call, "invalid gap parameter\n");
	}

	CustomPrintValue(x);
	return x;
}

static printList(SEXP s)
{
	int i, taglen;
	SEXP dims, t, newcall;
	char *pbuf, *ptag;

	if((dims = getAttrib(s, R_DimSymbol)) != R_NilValue
	  && length(dims) > 1) {
		PROTECT(dims);
		PROTECT(t = allocArray(STRSXP, dims));
		setAttrib(t, R_DimNamesSymbol, getAttrib(s, R_DimNamesSymbol));
		i = 0;
		while(s != R_NilValue) {
			switch(TYPEOF(CAR(s))) {
				case NILSXP:
					pbuf = Rsprintf("NULL");
					break;
				case LGLSXP:
					pbuf = Rsprintf("Logical,%d", LENGTH(CAR(s)));
					break;
				case FACTSXP:
				case ORDSXP:
					pbuf = Rsprintf("Factor,%d", LENGTH(CAR(s)));
					break;
				case INTSXP:
				case REALSXP:
					pbuf = Rsprintf("Numeric,%d", LENGTH(CAR(s)));
					break;
#ifdef COMPLEX_DATA
				case CPLXSXP:
					pbuf = Rsprintf("Complex,%d", LENGTH(CAR(s)));
					break;
#endif
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
			STRING(t)[i++] = mkChar(pbuf);
			s = CDR(s);
		}
		if (LENGTH(dims) == 2)
			printMatrix(t, 0, dims, 0);
		else
			printArray(t, 0);
		UNPROTECT(2);
	}
	else {
		i = 1;
		taglen = strlen(tagbuf);
		ptag = tagbuf + taglen;
		PROTECT(newcall = allocList(2));
		CAR(newcall) = install("print");
		TYPEOF(newcall) = LANGSXP;
		while (TYPEOF(s) == LISTSXP) {
			if (i > 1) Rprintf("\n");
			if (TAG(s) != R_NilValue && isSymbol(TAG(s))) {
				if (taglen + strlen(CHAR(PRINTNAME(TAG(s)))) > TAGBUFLEN)
					sprintf(ptag, "$...");
				else
					sprintf(ptag, "$%s", CHAR(PRINTNAME(TAG(s))));
			}
			else {
				if (taglen + IndexWidth(i) > TAGBUFLEN)
					sprintf(ptag, "$...");
				else
					sprintf(ptag, "[[%d]]", i);
			}
			Rprintf("%s\n", tagbuf);
			if(isObject(CAR(s))) {
				CADR(newcall) = CAR(s);
				eval(newcall, R_NilValue);
			}
			else PrintValueRec(CAR(s));
			*ptag = '\0';
			s = CDR(s);
			i += 1;
		}
		if (s != R_NilValue) {
			Rprintf("\n. \n\n");
			PrintValueRec(s);
		}
		Rprintf("\n");
		UNPROTECT(1);
	}
	printAttributes(s);
}

static void PrintExpression(SEXP s)
{
	SEXP u, v, nms;
	int i, n;

	PROTECT(u = v = allocList(LENGTH(s)+1));
	TYPEOF(u) = LANGSXP;
	CAR(u) = install("expression");
	u = CDR(u);
#ifdef NEW
	nms = getAttrib(s, R_NamesSymbol);
#endif
	n = LENGTH(s);
	for(i=0 ; i<n ; i++) {
		CAR(u) = VECTOR(s)[i];
		u = CDR(u);
	}
	u = deparse1(v, 0);
	n = LENGTH(u);
	for (i=0; i<n ; i++)
		Rprintf("%s\n", CHAR(STRING(u)[i]));
	UNPROTECT(1);
}

	/* PrintValueRec - recursively print an SEXP */

void PrintValueRec(SEXP s)
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
		Rprintf("<primitive: %s>\n", PRIMNAME(s));
		break;
	case CHARSXP:
		Rprintf("\"%s\"\n", EncodeString(CHAR(s),0,'"'));
		break;
	case EXPRSXP:
		PrintExpression(s);
		break;
	case CLOSXP:
	case LANGSXP:
		t = deparse1(s, 0);
		for (i = 0; i < LENGTH(t); i++)
			Rprintf("%s\n", CHAR(STRING(t)[i]));
		if (TYPEOF(s) == CLOSXP) t = CLOENV(s);
		else t = R_GlobalEnv;
		if (t != R_GlobalEnv)
			Rprintf("<environment: %p>\n", t);
		break;
	case ENVSXP:
		if (s == R_GlobalEnv) Rprintf("<environment: R_GlobalEnv>\n");
		else Rprintf("<environment: %p>\n", s);
		break;
	case PROMSXP:
		Rprintf("<promise: %p>\n", s);
		break;
	case DOTSXP:
		Rprintf("<...>\n");
		break;
	case LISTSXP:
		printList(s);
		break;
	case LGLSXP:
	case FACTSXP:
	case ORDSXP:
	case INTSXP:
	case REALSXP:
	case STRSXP:
#ifdef COMPLEX_DATA
	case CPLXSXP:
#endif
		PROTECT(t = getAttrib(s, R_DimSymbol));
		if (TYPEOF(t) == INTSXP) {
			if (LENGTH(t) == 1) {
				PROTECT(t = getAttrib(s, R_DimNamesSymbol));
				if (t != R_NilValue && CAR(t) != R_NilValue)
					printNamedVector(s, CAR(t), print_quote);
				else
					printVector(s, 1, print_quote);
				UNPROTECT(1);
			}
			else if (LENGTH(t) == 2)
				printMatrix(s, 0, t, print_quote);
			else
				printArray(s, print_quote);
		}
		else {
			UNPROTECT(1);
			PROTECT(t = getAttrib(s, R_NamesSymbol));
			if (t != R_NilValue)
				printNamedVector(s, t, print_quote);
			else
				printVector(s, 1, print_quote);
		}
		UNPROTECT(1);
		break;
	default:
		UNIMPLEMENTED("PrintValueRec");
	}
	printAttributes(s);
}

static void printAttributes(SEXP s)
{
	SEXP a;
	SEXP R_LevelsSymbol;
	char *ptag;
	int i;

	a = ATTRIB(s);
	if (a != R_NilValue) {
		R_LevelsSymbol = install("levels");
		ptag = tagbuf + strlen(tagbuf);
		i = 1;
		while (a != R_NilValue) {
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
			if(TAG(a) == R_CommentSymbol)
				goto nextattr;
			if (i > 1)
				Rprintf("\n");
			sprintf(ptag, "attr(,\"%s\")", EncodeString(CHAR(PRINTNAME(TAG(a))),0,0));
			Rprintf("%s\n", tagbuf);
			PrintValueRec(CAR(a));
		nextattr:
			*ptag = '\0';
			a = CDR(a);
		}
	}
}



	/* Print an S-expression using (possibly) local options */

void PrintValueEnv(SEXP s, SEXP env)
{
	SEXP call;

	PrintDefaults(env);

	tagbuf[0] = '\0';

	PROTECT(s);

	if(isObject(s)) {
		PROTECT(call = lang2(install("print"), s));
		eval(call, env);
		UNPROTECT(1);
	}
	else {
		PrintValueRec(s);
	}
	UNPROTECT(1);
}


	/* Print an S-expression using global options */

void PrintValue(SEXP s)
{
	PrintValueEnv(s, R_NilValue);
}


void CustomPrintValue(SEXP s)
{
	tagbuf[0] = '\0';
	PrintValueRec(s);
}
