/*
 *  R : A Computer Language for Statistical Data Analysis
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#ifdef HAVE_CONFIG_H
#include <Rconfig.h>
#endif

#include "Defn.h"
#include "Print.h"

#define MAX_WIDTH	200
#define MIN_WIDTH	10
#define MIN_DIGITS	1
#define MAX_DIGITS	22
#define MIN_EXPRESSIONS 25
#define MAX_EXPRESSIONS 100000

/*
 *  "prompt"
 *  "continue"
 *  "editor"
 *  "expressions"
 *  "width"
 *  "digits"
 *  "contrasts"
 *
 *  "echo"
 *  "error"
 *  "free"
 *  "keep"
 *  "length"
 *  "memory"
 *  "object.size"
 *  "pager"
 *  "reference"
 *  "scrap"
 *  "show"
 *  "ts.eps"
 *  "warn"
 */

static SEXP Options(void)
{
    return install(".Options");
}

static SEXP FindTaggedItem(SEXP lst, SEXP tag)
{
    for ( ; lst!=R_NilValue ; lst=CDR(lst)) {
	if (TAG(lst) == tag)
	    return lst;
    }
    return R_NilValue;
}

SEXP GetOption(SEXP tag, SEXP rho)
{
    SEXP opt = findVar(Options(), R_NilValue);
    if (!isList(opt))
	error("corrupted options list");
    opt = FindTaggedItem(opt, tag);
    return CAR(opt);
}

int GetOptionWidth(SEXP rho)
{
    int w;
    w = asInteger(GetOption(install("width"), rho));
    if (w < MIN_WIDTH || w > MAX_WIDTH) {
	warning("invalid printing width, used 80");
	return 80;
    }
    return w;
}

int GetOptionDigits(SEXP rho)
{
    int d;
    d = asInteger(GetOption(install("digits"), rho));
    if (d < MIN_DIGITS || d > MAX_DIGITS) {
	warning("invalid printing digits, used 7");
	return 7;
    }
    return d;
}


/* Change the value of an option or add a new option or, */
/* if called with value R_NilValue, remove that option. */

static SEXP SetOption(SEXP tag, SEXP value)
{
    SEXP opt, old, t;
    t = opt = SYMVALUE(Options());
    if (!isList(opt))
	error("corrupted options list");
    opt = FindTaggedItem(opt, tag);

    /* The option is being removed. */
    if (value == R_NilValue) {
	for ( ; t != R_NilValue ; t = CDR(t))
	    if (TAG(CDR(t)) == tag) {
		old = CAR(t);
		CDR(t)=CDDR(t);
		return old;
	    }
	return R_NilValue;
    }
    /* If the option is new, a new slot */
    /* is added to the end of .Options */
    if (opt == R_NilValue) {
	while (CDR(t) != R_NilValue)
	    t = CDR(t);
	PROTECT(value);
	CDR(t) = allocList(1);
	UNPROTECT(1);
	opt = CDR(t);
	TAG(opt) = tag;
    }
    old = CAR(opt);
    CAR(opt) = value;
    return old;
}

/* Set the width of lines for printing i.e. like options(width=...) */
/* Returns the previous value for the options. */

int R_SetOptionWidth(int w)
{
    SEXP t, v;
    if (w < MIN_WIDTH) w = MIN_WIDTH;
    if (w > MAX_WIDTH) w = MAX_WIDTH;
    PROTECT(t = install("width"));
    PROTECT(v = ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(2);
    return INTEGER(v)[0];
}

int R_SetOptionWarn(int w)
{
    SEXP t, v;

    t = install("warn");
    PROTECT(v = ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(1);
    return INTEGER(v)[0];
}

/* Note that options are stored as a dotted pair list */
/* This is barely historical, but is also useful. */

void InitOptions(void)
{
    SEXP t, val, v;
    PROTECT(v = val = allocList(11));

    TAG(v) = install("prompt");
    CAR(v) = mkString("> ");
    v = CDR(v);

    TAG(v) = install("continue");
    CAR(v) = mkString("+ ");
    v = CDR(v);

    TAG(v) = install("editor");
    CAR(v) = mkString("vi");
    v = CDR(v);

    TAG(v) = install("expressions");
    CAR(v) = ScalarInteger(100);
    v = CDR(v);

    TAG(v) = install("width");
    CAR(v) = ScalarInteger(80);
    v = CDR(v);

    TAG(v) = install("digits");
    CAR(v) = ScalarInteger(7);
    v = CDR(v);

    TAG(v) = install("contrasts");
    CAR(v) = allocVector(STRSXP, 2);
    STRING(CAR(v))[0] = mkChar("contr.treatment");
    STRING(CAR(v))[1] = mkChar("contr.poly");
    PROTECT(t = allocVector(STRSXP, 2));
    STRING(t)[0] = mkChar("unordered");
    STRING(t)[1] = mkChar("ordered");
    namesgets(CAR(v), t);
    v = CDR(v);

    TAG(v) = install("verbose");
    CAR(v) = allocVector(LGLSXP, 1);
    LOGICAL(CAR(v))[0] = R_Verbose;
    v = CDR(v);

    TAG(v) = install("echo");
    CAR(v) = allocVector(LGLSXP, 1);
    LOGICAL(CAR(v))[0] = !R_Slave;
    v = CDR(v);

    TAG(v) = install("check.bounds");
    CAR(v) = allocVector(LGLSXP, 1);
    LOGICAL(CAR(v))[0] = 0;	/* no checking */
    v = CDR(v);

    TAG(v) = install("keep.source");
    CAR(v) = allocVector(LGLSXP, 1);
    LOGICAL(CAR(v))[0] = 0;	/* no storage of function source */
                                /* turned on after load of base  */
    SYMVALUE(install(".Options")) = val;
    UNPROTECT(2);
}

#if 0
/* FIXME : This functionality should be universal */
/* See also in bind.c. */

/* static */ SEXP EnsureString(SEXP s)
{
    switch(TYPEOF(s)) {
    case SYMSXP:
	s = PRINTNAME(s);
	break;
    case STRSXP:
	s = STRING(s)[0];
	break;
    case CHARSXP:
	break;
    case NILSXP:
	s = R_BlankString;
	break;
    default:
	error("invalid tag in name extraction");
    }
    return s;
}
#endif

SEXP do_options(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP argi= R_NilValue, argnames= R_NilValue, namei= R_NilValue,
	names, options, s, tag, value; /* = R_Nil..: -Wall */
    int i, k, n;

    /* Locate the options values in the symbol table.
       This will need to change if options are to live in the session
       frame.
       */

    options = SYMVALUE(Options());

    /* This is the zero argument case.  We alloc up a real list and
       write the system values into it.
       */

    if (args == R_NilValue) {
	n = length(options);
	PROTECT(value = allocVector(VECSXP, n));
	PROTECT(names = allocVector(STRSXP, n));
	i = 0;
	while (options != R_NilValue) {
	    VECTOR(names)[i] = PRINTNAME(TAG(options));
	    VECTOR(value)[i] = duplicate(CAR(options));
	    i = i + 1; options = CDR(options);
	}
	setAttrib(value, R_NamesSymbol, names);
	UNPROTECT(2);
	return value;
    }

    /* The arguments to "options" can either be a sequence of name =
       value form, or can be a single list.  This means that we must
       code so that both forms will work.
       [ Vomits quietly onto shoes ... ]
       */

    n = length(args);
    if (n == 1 && (isPairList(CAR(args)) || isVectorList(CAR(args)))
        && TAG(args) == R_NilValue ) {
	args = CAR(args);
	n = length(args);
    }
    PROTECT(value = allocVector(VECSXP, n));
    PROTECT(names = allocVector(STRSXP, n));

    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
	argnames = R_NilValue;
	break;
    case VECSXP:
	argnames = getAttrib(args, R_NamesSymbol);
	break;
    }

    R_Visible = 0;
    for (i = 0 ; i < n ; i++) {

	switch (TYPEOF(args)) {
	case LISTSXP:
	    argi = CAR(args);
	    namei = EnsureString(TAG(args));
	    args = CDR(args);
	    break;
	case VECSXP:
	    argi = VECTOR(args)[i];
	    namei = EnsureString(STRING(argnames)[i]);
	    break;
	}

	if (*CHAR(namei)) {
	    tag = install(CHAR(namei));
	    if (streql(CHAR(namei), "width")) {
		k = asInteger(argi);
		if (k < MIN_WIDTH || k > MAX_WIDTH)
		    errorcall(call, "invalid width parameter");
		VECTOR(value)[i] = SetOption(tag, ScalarInteger(k));
	    }
	    else if (streql(CHAR(namei), "digits")) {
		k = asInteger(argi);
		if (k < MIN_DIGITS || k > MAX_DIGITS)
		    errorcall(call, "invalid digits parameter");
		VECTOR(value)[i] = SetOption(tag, ScalarInteger(k));
	    }
	    else if (streql(CHAR(namei), "expressions")) {
		k = asInteger(argi);
		if (k < 25 || k > MAX_EXPRESSIONS)
		    errorcall(call, "expressions parameter invalid");
		VECTOR(value)[i] = SetOption(tag, ScalarInteger(k));
	    }
	    else if (streql(CHAR(namei), "editor")) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    errorcall(call, "invalid editor parameter");
		VECTOR(value)[i] = SetOption(tag, ScalarString(s));
	    }
	    else if (streql(CHAR(namei), "continue")) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    errorcall(call, "invalid continue parameter");
		VECTOR(value)[i] = SetOption(tag, ScalarString(s));
	    }
	    else if (streql(CHAR(namei), "prompt")) {
		s = asChar(argi);
		if (s == NA_STRING || length(s) == 0)
		    errorcall(call, "prompt parameter invalid");
		VECTOR(value)[i] = SetOption(tag, ScalarString(s));
	    }
	    else if (streql(CHAR(namei), "contrasts")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 2)
		    errorcall(call, "contrasts parameter invalid");
		VECTOR(value)[i] = SetOption(tag, argi);
	    }
	    else if (streql(CHAR(namei), "warn")) {
		if (!isNumeric(argi) || length(argi) != 1)
		    errorcall(call, "warn parameter invalid");
                VECTOR(value)[i] = SetOption(tag, argi);
	    }
	    else if ( streql(CHAR(namei), "warning.expression") )  {
		if( !isLanguage(argi) &&  ! isExpression(argi) )
		    errorcall(call, "warning.expression parameter invalid");
		VECTOR(value)[i] = SetOption(tag, argi);
	    }
	    else if ( streql(CHAR(namei), "error") ) {
		if( !isLanguage(argi) &&  !isExpression(argi) )
		    errorcall(call, "error parameter invalid");
		VECTOR(value)[i] = SetOption(tag, argi);
	    }
	    else if (streql(CHAR(namei), "echo")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    errorcall(call, "echo parameter invalid");
		k = asInteger(argi);
		/* Should be quicker than checking options(echo)
		   every time R prompts for input:
		   */
		R_Slave = !k;
		VECTOR(value)[i] = SetOption(tag, ScalarLogical(k));
	    }
	    else {
		VECTOR(value)[i] = SetOption(tag, duplicate(argi));
	    }
	    STRING(names)[i] = namei;
	}
	else {
	    if (!isString(argi) || LENGTH(argi) <= 0)
		errorcall(call, "invalid argument");
	    VECTOR(value)[i] = duplicate(CAR(FindTaggedItem(options,
				     install(CHAR(STRING(argi)[0])))));
	    STRING(names)[i] = STRING(argi)[0];
	    R_Visible = 1;
	}
    }
    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}
