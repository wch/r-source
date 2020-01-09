/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1998-2019   The R Core Team.
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include <Internal.h>
#include "Print.h"
#include <Rinternals.h>

/* The global var. R_Expressions is in Defn.h */
#define R_MIN_EXPRESSIONS_OPT	25
#define R_MAX_EXPRESSIONS_OPT	500000

/* Interface to the (polymorphous!)  options(...)  command.
 *
 * We have two kind of options:
 *   1) those used exclusively from R code,
 *	typically initialized in Rprofile.

 *	Their names need not appear here, but may, when we want
 *	to make sure that they are assigned `valid' values only.
 *
 *   2) Those used (and sometimes set) from C code;
 *	Either accessing and/or setting a global C variable,
 *	or just accessed by e.g.  GetOption1(install("pager"))
 *
 * A (complete?!) list of these (2):
 *
 *	"prompt"
 *	"continue"
 *	"expressions"
 *	"width"
 *	"digits"
 *	"echo"
 *	"verbose"
 *	"keep.source"
 *	"keep.source.pkgs"
 *	"keep.parse.data"
 *	"keep.parse.data.pkgs"
 *	"browserNLdisabled"

 *	"de.cellwidth"		../unix/X11/ & ../gnuwin32/dataentry.c
 *	"device"
 *	"pager"
 *	"paper.size"		./devPS.c

 *	"timeout"		./connections.c

 *      "deparse.max.lines"     ./deparse.c (& PrintCall() in ./eval.c, ./main.c

 *	"check.bounds"
 *	"error"
 *	"error.messages"
 *	"show.error.messages"
 *	"warn"
 *	"warning.length"
 *	"warning.expression"
 *	"nwarnings"

 *	"matprod"
 *      "PCRE_study"
 *      "PCRE_use_JIT"

 *
 * S additionally/instead has (and one might think about some)
 * "free",	"keep"
 * "length",	"memory"
 * "object.size"
 * "reference", "show"
 * "scrap"

 * R_NilValue is not a valid value for any option, but is used to signal a
 * missing option by FindTaggedItem/GetOption and higher-level functions.
 */


static SEXP Options(void)
{
    static SEXP sOptions = NULL;
    if(!sOptions) sOptions = install(".Options");
    return sOptions;
}

static SEXP FindTaggedItem(SEXP lst, SEXP tag)
{
    for ( ; lst != R_NilValue ; lst = CDR(lst)) {
	if (TAG(lst) == tag) {
	    if (CAR(lst) == R_NilValue)
		error("option %s has NULL value", CHAR(PRINTNAME(tag)));
	    return lst;
	}
    }
    return R_NilValue;
}

static SEXP makeErrorCall(SEXP fun)
{
  SEXP call;
  PROTECT(call = allocList(1));
  SET_TYPEOF(call, LANGSXP);
  SETCAR(call, fun);
  UNPROTECT(1);
  return call;
}

SEXP GetOption(SEXP tag, SEXP rho)
{
    return GetOption1(tag);
}


SEXP GetOption1(SEXP tag)
{
    SEXP opt = SYMVALUE(Options());
    if (!isList(opt)) error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);
    return CAR(opt);
}

int FixupWidth(SEXP width, warn_type warn)
{
    int w = asInteger(width);
    if (w == NA_INTEGER || w < R_MIN_WIDTH_OPT || w > R_MAX_WIDTH_OPT) {
	switch(warn) {
	case iWARN: warning(_("invalid printing width %d, used 80"), w);
	case iSILENT:
	    return 80; // for SILENT and WARN
	case iERROR: error(_("invalid printing width"));
	}
    }
    return w;
}
int GetOptionWidth(void)
{
    return FixupWidth(GetOption1(install("width")), iWARN);
}

int FixupDigits(SEXP digits, warn_type warn)
{
    int d = asInteger(digits);
    if (d == NA_INTEGER || d < R_MIN_DIGITS_OPT || d > R_MAX_DIGITS_OPT) {
	switch(warn) {
	case iWARN: warning(_("invalid printing digits %d, used 7"), d);
	case iSILENT:
	    return 7; // for SILENT and WARN
	case iERROR: error(_("invalid printing digits %d"), d);
	}
    }
    return d;
}
int GetOptionDigits(void)
{
    return FixupDigits(GetOption1(install("digits")), iWARN);
}

attribute_hidden
int GetOptionCutoff(void)
{
    int w;
    w = asInteger(GetOption1(install("deparse.cutoff")));
    if (w == NA_INTEGER || w <= 0) {
	warning(_("invalid 'deparse.cutoff', used 60"));
	w = 60;
    }
    return w;
}

attribute_hidden
Rboolean Rf_GetOptionDeviceAsk(void)
{
    int ask;
    ask = asLogical(GetOption1(install("device.ask.default")));
    if(ask == NA_LOGICAL) {
	warning(_("invalid value for \"device.ask.default\", using FALSE"));
	return FALSE;
    }
    return ask != 0;
}


/* Change the value of an option or add a new option or, */
/* if called with value R_NilValue, remove that option. */

static SEXP SetOption(SEXP tag, SEXP value)
{
    SEXP opt, old, t;
    PROTECT(value);
    t = opt = SYMVALUE(Options());
    if (!isList(opt))
	error(_("corrupted options list"));
    opt = FindTaggedItem(opt, tag);

    /* The option is being removed. */
    if (value == R_NilValue) {
	for ( ; t != R_NilValue ; t = CDR(t))
	    if (TAG(CDR(t)) == tag) {
		old = CAR(CDR(t));
		SETCDR(t, CDDR(t));
		UNPROTECT(1); /* value */
		return old;
	    }
	UNPROTECT(1); /* value */
	return R_NilValue;
    }
    /* If the option is new, a new slot */
    /* is added to the end of .Options */
    if (opt == R_NilValue) {
	while (CDR(t) != R_NilValue)
	    t = CDR(t);
	SETCDR(t, allocList(1));
	opt = CDR(t);
	SET_TAG(opt, tag);
    }
    old = CAR(opt);
    SETCAR(opt, value);
    UNPROTECT(1); /* value */
    return old;
}

/* Set the width of lines for printing i.e. like options(width=...) */
/* Returns the previous value for the options. */

int attribute_hidden R_SetOptionWidth(int w)
{
    SEXP t, v;
    if (w < R_MIN_WIDTH_OPT) w = R_MIN_WIDTH_OPT;
    if (w > R_MAX_WIDTH_OPT) w = R_MAX_WIDTH_OPT;
    PROTECT(t = install("width"));
    PROTECT(v = ScalarInteger(w));
    v = SetOption(t, v);
    UNPROTECT(2);
    return INTEGER(v)[0];
}

int attribute_hidden R_SetOptionWarn(int w)
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

void attribute_hidden InitOptions(void)
{
    SEXP val, v;
    char *p;

    /* options set here should be included into mandatory[] in do_options */
#ifdef HAVE_RL_COMPLETION_MATCHES
    PROTECT(v = val = allocList(23));
#else
    PROTECT(v = val = allocList(22));
#endif

    SET_TAG(v, install("prompt"));
    SETCAR(v, mkString("> "));
    v = CDR(v);

    SET_TAG(v, install("continue"));
    SETCAR(v, mkString("+ "));
    v = CDR(v);

    SET_TAG(v, install("expressions"));
    SETCAR(v, ScalarInteger(R_Expressions));
    v = CDR(v);

    SET_TAG(v, install("width"));
    SETCAR(v, ScalarInteger(80));
    v = CDR(v);

    SET_TAG(v, install("deparse.cutoff"));
    SETCAR(v, ScalarInteger(60));
    v = CDR(v);

    SET_TAG(v, install("digits"));
    SETCAR(v, ScalarInteger(7));
    v = CDR(v);

    SET_TAG(v, install("echo"));
    SETCAR(v, ScalarLogical(!R_NoEcho));
    v = CDR(v);

    SET_TAG(v, install("verbose"));
    SETCAR(v, ScalarLogical(R_Verbose));
    v = CDR(v);

    SET_TAG(v, install("check.bounds"));
    SETCAR(v, ScalarLogical(0));	/* no checking */
    v = CDR(v);

    p = getenv("R_KEEP_PKG_SOURCE");
    R_KeepSource = (p && (strcmp(p, "yes") == 0)) ? 1 : 0;

    SET_TAG(v, install("keep.source")); /* overridden in Common.R */
    SETCAR(v, ScalarLogical(R_KeepSource));
    v = CDR(v);

    SET_TAG(v, install("keep.source.pkgs"));
    SETCAR(v, ScalarLogical(R_KeepSource));
    v = CDR(v);

    SET_TAG(v, install("keep.parse.data"));
    SETCAR(v, ScalarLogical(TRUE));
    v = CDR(v);

    p = getenv("R_KEEP_PKG_PARSE_DATA");
    SET_TAG(v, install("keep.parse.data.pkgs"));
    SETCAR(v, ScalarLogical((p && (strcmp(p, "yes") == 0)) ? TRUE : FALSE));
    v = CDR(v);

    SET_TAG(v, install("warning.length"));
    SETCAR(v, ScalarInteger(1000));
    v = CDR(v);

    SET_TAG(v, install("nwarnings"));
    SETCAR(v, ScalarInteger(50));
    v = CDR(v);

    SET_TAG(v, install("OutDec"));
    SETCAR(v, mkString("."));
    v = CDR(v);

    SET_TAG(v, install("browserNLdisabled"));
    SETCAR(v, ScalarLogical(FALSE));
    v = CDR(v);

    p = getenv("R_C_BOUNDS_CHECK");
    R_CBoundsCheck = (p && (strcmp(p, "yes") == 0)) ? 1 : 0;

    SET_TAG(v, install("CBoundsCheck"));
    SETCAR(v, ScalarLogical(R_CBoundsCheck));
    v = CDR(v);

    SET_TAG(v, install("matprod"));
    switch(R_Matprod) {
	case MATPROD_DEFAULT: p = "default"; break;
	case MATPROD_INTERNAL: p = "internal"; break;
	case MATPROD_BLAS: p = "blas"; break;
	case MATPROD_DEFAULT_SIMD: p = "default.simd"; break;
    }
    SETCAR(v, mkString(p));
    v = CDR(v);

    SET_TAG(v, install("PCRE_study"));
    if (R_PCRE_study == -1)
	SETCAR(v, ScalarLogical(TRUE));
    else if (R_PCRE_study == -2)
	SETCAR(v, ScalarLogical(FALSE));
    else
	SETCAR(v, ScalarInteger(R_PCRE_study));
    v = CDR(v);

    SET_TAG(v, install("PCRE_use_JIT"));
    SETCAR(v, ScalarLogical(R_PCRE_use_JIT));
    v = CDR(v);

    SET_TAG(v, install("PCRE_limit_recursion"));
    R_PCRE_limit_recursion = NA_LOGICAL;
    SETCAR(v, ScalarLogical(R_PCRE_limit_recursion));
    v = CDR(v);
    /* options set here should be included into mandatory[] in do_options */

#ifdef HAVE_RL_COMPLETION_MATCHES
    /* value from Rf_initialize_R */
    SET_TAG(v, install("rl_word_breaks"));
    SETCAR(v, mkString(" \t\n\"\\'`><=%;,|&{()}"));
    set_rl_word_breaks(" \t\n\"\\'`><=%;,|&{()}");
#endif

    SET_SYMVALUE(install(".Options"), val);
    UNPROTECT(1);
}


SEXP attribute_hidden do_getOption(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP x = CAR(args);
    if (!isString(x) || LENGTH(x) != 1)
	error(_("'%s' must be a character string"), "x");
    return duplicate(GetOption1(installTrChar(STRING_ELT(x, 0))));
}


/* This needs to manage R_Visible */
SEXP attribute_hidden do_options(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP names, value, options;

    /* Locate the options values in the symbol table.
       This will need to change if options are to live in the session
       frame.
       */

    options = SYMVALUE(Options());

    /* This code is not re-entrant and people have used it in
       finalizers.

       If a re-entrancy lock needs to be added, note that it
       would apply to R_SetOption* too.
    */

    checkArity(op, args);
    if (args == R_NilValue) {
	/* This is the zero argument case.
	   We alloc up a vector list and write the system values into it.
	*/
	int n = length(options);
	PROTECT(value = allocVector(VECSXP, n));
	PROTECT(names = allocVector(STRSXP, n));
	for (int i = 0; i < n; i++) {
	    SET_STRING_ELT(names, i, PRINTNAME(TAG(options)));
	    SET_VECTOR_ELT(value, i, duplicate(CAR(options)));
	    options = CDR(options);
	}
	SEXP sind = PROTECT(allocVector(INTSXP, n));
	int *indx = INTEGER(sind);
	for (int i = 0; i < n; i++) indx[i] = i;
	orderVector1(indx, n, names, TRUE, FALSE, R_NilValue);
	SEXP value2 = PROTECT(allocVector(VECSXP, n));
	SEXP names2 = PROTECT(allocVector(STRSXP, n));
	for(int i = 0; i < n; i++) {
	    SET_STRING_ELT(names2, i, STRING_ELT(names, indx[i]));
	    SET_VECTOR_ELT(value2, i, VECTOR_ELT(value, indx[i]));
	}
	setAttrib(value2, R_NamesSymbol, names2);
	UNPROTECT(5);
	R_Visible = TRUE;
	return value2;
    }

    /* The arguments to "options" can either be a sequence of
       name = value form, or can be a single list.
       This means that we must code so that both forms will work.
       [ Vomits quietly onto shoes ... ]
       */

    int n = length(args);
    if (n == 1 && (isPairList(CAR(args)) || isVectorList(CAR(args)))
	&& TAG(args) == R_NilValue ) {
	args = CAR(args);
	n = length(args);
    }
    PROTECT(value = allocVector(VECSXP, n));
    PROTECT(names = allocVector(STRSXP, n));

    SEXP argnames = R_NilValue;
    switch (TYPEOF(args)) {
    case NILSXP:
    case LISTSXP:
	break;
    case VECSXP:
	if(n > 0) {
	    argnames = getAttrib(args, R_NamesSymbol);
	    if(LENGTH(argnames) != n)
		error(_("list argument has no valid names"));
	}
	break;
    default:
	UNIMPLEMENTED_TYPE("options", args);
    }

    R_Visible = FALSE;
    for (int i = 0 ; i < n ; i++) { /* i-th argument */
	SEXP argi = R_NilValue, namei = R_NilValue;
	switch (TYPEOF(args)) {
	case LISTSXP:
	    argi = CAR(args);
	    namei = EnsureString(TAG(args)); /* gives "" for no tag */
	    args = CDR(args);
	    break;
	case VECSXP:
	    argi = VECTOR_ELT(args, i);
	    namei = STRING_ELT(argnames, i);
	    break;
	default: /* already checked, but be safe here */
	    UNIMPLEMENTED_TYPE("options", args);
	}

	if (*CHAR(namei)) { /* name = value  ---> assignment */
	    SEXP tag = installTrChar(namei);
	    SET_STRING_ELT(names, i, namei);

	    if (argi == R_NilValue) {
		/* Handle option removal separately to simplify value checking
		   for known options below; mandatory means not allowed to be
		   removed once set, but not all have to be set at startup. */
		const char *mandatory[] = {"prompt", "continue", "expressions",
		  "width", "deparse.cutoff", "digits", "echo", "verbose",
		  "check.bounds", "keep.source", "keep.source.pkgs",
		  "keep.parse.data", "keep.parse.data.pkgs", "warning.length",
		  "nwarnings", "OutDec", "browserNLdisabled", "CBoundsCheck",
		  "matprod", "PCRE_study", "PCRE_use_JIT",
		  "PCRE_limit_recursion", "rl_word_breaks",
		  /* ^^^ from InitOptions ^^^ */
		  "warn", "max.print", "show.error.messages",
		  /* ^^^ from Common.R ^^^ */
		  NULL};
		for(int j = 0; mandatory[j] != NULL; j++)
		    if (streql(CHAR(namei), mandatory[j]))
			error(_("option '%s' cannot be deleted"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, R_NilValue));
	    } else if (streql(CHAR(namei), "width")) {
		int k = asInteger(argi);
		if (k < R_MIN_WIDTH_OPT || k > R_MAX_WIDTH_OPT)
		    error(_("invalid '%s' parameter, allowed %d...%d"),
			  CHAR(namei), R_MIN_WIDTH_OPT, R_MAX_WIDTH_OPT);
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "deparse.cutoff")) {
		int k = asInteger(argi);
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "digits")) {
		int k = asInteger(argi);
		if (k < R_MIN_DIGITS_OPT || k > R_MAX_DIGITS_OPT)
		    error(_("invalid '%s' parameter, allowed %d...%d"),
			  CHAR(namei), R_MIN_DIGITS_OPT, R_MAX_DIGITS_OPT);
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "expressions")) {
		int k = asInteger(argi);
		if (k < R_MIN_EXPRESSIONS_OPT || k > R_MAX_EXPRESSIONS_OPT)
		    error(_("invalid '%s' parameter, allowed %d...%d"), CHAR(namei),
			  R_MIN_EXPRESSIONS_OPT, R_MAX_EXPRESSIONS_OPT);
		R_Expressions = R_Expressions_keep = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "keep.source")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_KeepSource = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "editor") && isString(argi)) {
		SEXP s =  asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarString(s)));
	    }
	    else if (streql(CHAR(namei), "continue")) {
		SEXP s =  asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    error(_("invalid value for '%s'"), CHAR(namei));
		/* We want to make sure these are in the native encoding */
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, mkString(translateChar(s))));
	    }
	    else if (streql(CHAR(namei), "prompt")) {
		SEXP s =  asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    error(_("invalid value for '%s'"), CHAR(namei));
		/* We want to make sure these are in the native encoding */
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, mkString(translateChar(s))));
	    }
	    else if (streql(CHAR(namei), "contrasts")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 2)
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(CHAR(namei), "check.bounds")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		/* R_CheckBounds = k; */
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "warn")) {
		if (!isNumeric(argi) || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k;
		// k = asInteger(argi) wld give both error + warning
		if(TYPEOF(argi) == REALSXP) {
		    int w;
		    k = IntegerFromReal(REAL_ELT(argi, 0), &w);
		} else {
		    k = asInteger(argi);
		}
		if (k == NA_INTEGER)
		    error(_("invalid value for '%s'"), CHAR(namei));
#ifdef _NOT_YET_
		char *p = getenv("R_WARN_BOUNDS_OPT");
		if ((p && (strcmp(p, "yes") == 0)) && (k < -1 || k > 2)) {
		    int k_n = (k < 0) ? -1 : 2;
		    REprintf(_("value for '%s' outside of -1:2 is set to %d\n"),
			     CHAR(namei), k_n);
		    k = k_n;
		}
#endif
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "warning.length")) {
		int k = asInteger(argi);
		if (k < 100 || k > 8170)
		    error(_("invalid value for '%s'"), CHAR(namei));
		R_WarnLength = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if ( streql(CHAR(namei), "warning.expression") )  {
		if( !isLanguage(argi) &&  ! isExpression(argi) )
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
	    else if (streql(CHAR(namei), "max.print")) {
		int k = asInteger(argi);
		if (k < 1) error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "nwarnings")) {
		int k = asInteger(argi);
		if (k < 1) error(_("invalid value for '%s'"), CHAR(namei));
		R_nwarnings = k;
		R_CollectWarnings = 0; /* force a reset */
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if ( streql(CHAR(namei), "error") ) {
		if(isFunction(argi))
		  argi = makeErrorCall(argi);
		else if( !isLanguage(argi) &&  !isExpression(argi) )
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
	    }
/* handle this here to avoid GetOption during error handling */
	    else if ( streql(CHAR(namei), "show.error.messages") ) {
		if( !isLogical(argi) && LENGTH(argi) != 1 )
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, argi));
		R_ShowErrorMessages = LOGICAL(argi)[0];
	    }
	    else if (streql(CHAR(namei), "echo")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		/* Should be quicker than checking options(echo)
		   every time R prompts for input:
		   */
		R_NoEcho = !k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "OutDec")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		static char sdec[11];
		if(R_nchar(STRING_ELT(argi, 0), Chars,
			   /* allowNA = */ FALSE, /* keepNA = */ FALSE,
			   "OutDec") != 1) // will become an error
		    warning(_("'OutDec' must be a string of one character"));
		strncpy(sdec, CHAR(STRING_ELT(argi, 0)), 10);
		sdec[10] = '\0';
		OutDec = sdec;
		SET_VECTOR_ELT(value, i, SetOption(tag, duplicate(argi)));
	    }
	    else if (streql(CHAR(namei), "max.contour.segments")) {
		int k = asInteger(argi);
		if (k < 0) // also many times above: rely on  NA_INTEGER  <  <finite_int>
		    error(_("invalid value for '%s'"), CHAR(namei));
		max_contour_segments = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "rl_word_breaks")) {
		if (TYPEOF(argi) != STRSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
#ifdef HAVE_RL_COMPLETION_MATCHES
		set_rl_word_breaks(CHAR(STRING_ELT(argi, 0)));
#endif
		SET_VECTOR_ELT(value, i, SetOption(tag, duplicate(argi)));
	    }
	    else if (streql(CHAR(namei), "warnPartialMatchDollar")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_warn_partial_match_dollar = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "warnPartialMatchArgs")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_warn_partial_match_args = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "warnPartialMatchAttr")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_warn_partial_match_attr = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "showWarnCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_ShowWarnCalls = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "showErrorCalls")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_ShowErrorCalls = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "showNCalls")) {
		int k = asInteger(argi);
		if (k < 30 || k > 500 || k == NA_INTEGER || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		R_NShowCalls = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarInteger(k)));
	    }
	    else if (streql(CHAR(namei), "par.ask.default")) {
		error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }
	    else if (streql(CHAR(namei), "browserNLdisabled")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		if (k == NA_LOGICAL)
		    error(_("invalid value for '%s'"), CHAR(namei));
		R_DisableNLinBrowser = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "CBoundsCheck")) {
		if (TYPEOF(argi) != LGLSXP || LENGTH(argi) != 1)
		    error(_("invalid value for '%s'"), CHAR(namei));
		int k = asLogical(argi);
		R_CBoundsCheck = k;
		SET_VECTOR_ELT(value, i, SetOption(tag, ScalarLogical(k)));
	    }
	    else if (streql(CHAR(namei), "matprod")) {
		SEXP s = asChar(argi);
		if (s == NA_STRING || LENGTH(s) == 0)
		    error(_("invalid value for '%s'"), CHAR(namei));
		if (streql(CHAR(s), "default"))
		    R_Matprod = MATPROD_DEFAULT;
		else if (streql(CHAR(s), "internal"))
		    R_Matprod = MATPROD_INTERNAL;
		else if (streql(CHAR(s), "blas"))
		    R_Matprod = MATPROD_BLAS;
		else if (streql(CHAR(s), "default.simd")) {
		    R_Matprod = MATPROD_DEFAULT_SIMD;
#if !defined(_OPENMP) || !defined(HAVE_OPENMP_SIMDRED)
		    warning(_("OpenMP SIMD is not supported in this build of R"));
#endif
		} else
		    error(_("invalid value for '%s'"), CHAR(namei));
		SET_VECTOR_ELT(value, i, SetOption(tag, duplicate(argi)));
	    }
	    else if (streql(CHAR(namei), "PCRE_study")) {
		if (TYPEOF(argi) == LGLSXP) {
		    int k = asLogical(argi) > 0;
		    R_PCRE_study = k ? -1 : -2;
		    SET_VECTOR_ELT(value, i,
				   SetOption(tag, ScalarLogical(k)));
		} else {
		    R_PCRE_study = asInteger(argi);
		    if (R_PCRE_study < 0) {
			R_PCRE_study = -2;
			SET_VECTOR_ELT(value, i,
				       SetOption(tag, ScalarLogical(-2)));
		    } else
			SET_VECTOR_ELT(value, i,
				       SetOption(tag, ScalarInteger(R_PCRE_study)));
		}
#ifdef HAVE_PCRE2
		if (R_PCRE_study != -2)
		    warning(_("'PCRE_study' has no effect with PCRE2"));
#endif
	    }
	    else if (streql(CHAR(namei), "PCRE_use_JIT")) {
		int use_JIT = asLogical(argi);
		R_PCRE_use_JIT = (use_JIT > 0); // NA_LOGICAL is < 0
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, ScalarLogical(R_PCRE_use_JIT)));
	    }
	    else if (streql(CHAR(namei), "PCRE_limit_recursion")) {
		R_PCRE_limit_recursion = asLogical(argi);
		SET_VECTOR_ELT(value, i,
			       SetOption(tag, ScalarLogical(R_PCRE_limit_recursion)));
		/* could warn for PCRE2 >= 10.30, but the value is ignored also when
		   JIT is used  */
	    }
	    else {
		SET_VECTOR_ELT(value, i, SetOption(tag, duplicate(argi)));
	    }
	}
	else { /* querying arg */
	    const char *tag;
	    if (!isString(argi) || LENGTH(argi) <= 0)
		error(_("invalid argument"));
	    tag = translateChar(STRING_ELT(argi, 0));
	    if (streql(tag, "par.ask.default")) {
		error(_("\"par.ask.default\" has been replaced by \"device.ask.default\""));
	    }

	    SET_VECTOR_ELT(value, i, duplicate(CAR(FindTaggedItem(options, install(tag)))));
	    SET_STRING_ELT(names, i, STRING_ELT(argi, 0));
	    R_Visible = TRUE;
	}
    } /* for() */
    setAttrib(value, R_NamesSymbol, names);
    UNPROTECT(2);
    return value;
}
