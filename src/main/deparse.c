/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997--2018  The R Core Team
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
 *
 *
 *  IMPLEMENTATION NOTES:
 *
 *  Deparsing has 3 layers.
 *  - The user interfaces, do_deparse(), do_dput(), and do_dump() should
 *    not be called from an internal function.
 *  - unless nlines > 0, the actual deparsing via deparse2() needs
 *    to be done twice, once to count things up and a second time to put
 *    them into the string vector for return.
 *  - Printing this to a file is handled by the calling routine.
 *
 *  Current call paths:
 *
 *    do_deparse() ------------> deparse1WithCutoff()
 *    do_dput() -> deparse1() -> deparse1WithCutoff()
 *    do_dump() -> deparse1() -> deparse1WithCutoff()
 *  ---------
 *  Workhorse: deparse1WithCutoff() -> deparse2() -> deparse2buff() --> {<itself>, ...}
 *  ---------  ~~~~~~~~~~~~~~~~~~ `-- implicit arg R_BrowseLines == getOption("deparse.max.lines")
 *
 *  ./errors.c: PrintWarnings() | warningcall_dflt() ... -> deparse1s() -> deparse1WithCutoff()
 *  ./print.c : Print[Language|Closure|Expression]()    --> deparse1w() -> deparse1WithCutoff()
 *  bind.c,match.c,..: c|rbind(), match(), switch()...-> deparse1line() -> deparse1WithCutoff()
 *
 *  INDENTATION:
 *
 *  Indentation is carried out in the routine printtab2buff at the
 *  bottom of this file.  It seems like this should be settable via
 *  options.
 *
 *
 *  LocalParseData VARIABLES  (historically GLOBALs):
 *
 *  linenumber:	 counts the number of lines that have been written,
 *		 this is used to setup storage for deparsing.
 *
 *  len:	 counts the length of the current line, it will be
 *		 used to determine when to break lines.
 *
 *  incurly:	 keeps track of whether we are inside a curly or not,
 *		 this affects the printing of if-then-else.
 *
 *  inlist:	 keeps track of whether we are inside a list or not,
 *		 this affects the printing of if-then-else.
 *
 *  startline:	 indicator TRUE=start of a line (so we can tab out to
 *		 the correct place).
 *
 *  indent:	 how many tabs should be written at the start of
 *		 a line.
 *
 *  buff:	 contains the current string, we attempt to break
 *		 lines at cutoff, but can unlimited length.
 *
 *  lbreak:	 often used to indicate whether a line has been
 *		 broken, this makes sure that that indenting behaves
 *		 itself.
 */

/* DTL ('duncan'):
* The code here used to use static variables to share values
* across the different routines. These have now been collected
* into a struct named  LocalParseData and this is explicitly
* passed between the different routines. This avoids the needs
* for the global variables and allows multiple evaluators, potentially
* in different threads, to work on their own independent copies
* that are local to their call stacks. This avoids any issues
* with interrupts, etc. not restoring values.

* The previous issue with the global "cutoff" variable is now implemented
* by creating a deparse1WithCutoff() routine which takes the cutoff from
* the caller and passes this to the different routines as a member of the
* LocalParseData struct. Access to the deparse1() routine remains unaltered.
* This is exactly as Ross had suggested ...
*
* One possible fix is to restructure the code with another function which
* takes a cutoff value as a parameter.	 Then "do_deparse" and "deparse1"
* could each call this deeper function with the appropriate argument.
* I wonder why I didn't just do this? -- it would have been quicker than
* writing this note.  I guess it needs a bit more thought ...
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
#include <float.h> /* for DBL_DIG */
#include <Print.h>
#include <Fileio.h>
#ifdef Win32
#include <trioremap.h>
#endif

#define BUFSIZE 512

#define MIN_Cutoff 20
#define DEFAULT_Cutoff 60
#define MAX_Cutoff (BUFSIZE - 12)
/* ----- MAX_Cutoff  <	BUFSIZE !! */

#include "RBufferUtils.h"

typedef R_StringBuffer DeparseBuffer;

typedef struct {
    int linenumber;
    int len; // FIXME: size_t
    int incurly;
    int inlist;
    Rboolean startline; /* = TRUE; */
    int indent;
    SEXP strvec;

    DeparseBuffer buffer;

    int cutoff;
    int backtick;
    int opts;
    int sourceable;
#ifdef longstring_WARN
    int longstring;
#endif
    int maxlines;
    Rboolean active;
    int isS4;
    Rboolean fnarg; /* fn argument, so parenthesize = as assignment */
} LocalParseData;

static SEXP deparse1WithCutoff(SEXP call, Rboolean abbrev, int cutoff,
			       Rboolean backtick, int opts, int nlines);
static void args2buff(SEXP, int, int, LocalParseData *);
static void deparse2buff(SEXP, LocalParseData *);
static void print2buff(const char *, LocalParseData *);
static void printtab2buff(int, LocalParseData *);
static void writeline(LocalParseData *);
static void vec2buff   (SEXP, LocalParseData *, Rboolean do_names);
static void vector2buff(SEXP, LocalParseData *);
static void src2buff1(SEXP, LocalParseData *);
static Rboolean src2buff(SEXP, int, LocalParseData *);
static void linebreak(Rboolean *lbreak, LocalParseData *);
static void deparse2(SEXP, SEXP, LocalParseData *);

// .Internal(deparse(expr, width.cutoff, backtick, .deparseOpts(control), nlines))
SEXP attribute_hidden do_deparse(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP expr = CAR(args); args = CDR(args);
    int cut0 = DEFAULT_Cutoff;
    if(!isNull(CAR(args))) {
	cut0 = asInteger(CAR(args));
	if(cut0 == NA_INTEGER|| cut0 < MIN_Cutoff || cut0 > MAX_Cutoff) {
	    warning(_("invalid 'cutoff' value for 'deparse', using default"));
	    cut0 = DEFAULT_Cutoff;
	}
    }
    args = CDR(args);
    int backtick = isNull(CAR(args)) ? 0 : asLogical(CAR(args));
    args = CDR(args);
    int opts = isNull(CAR(args)) ? SHOWATTRIBUTES : asInteger(CAR(args));
    args = CDR(args);
    int nlines = asInteger(CAR(args));
    if (nlines == NA_INTEGER) nlines = -1;
    return deparse1WithCutoff(expr, FALSE, cut0, backtick, opts, nlines);
}

// deparse1() version *looking* at getOption("deparse.max.lines")
SEXP deparse1m(SEXP call, Rboolean abbrev, int opts)
{
    Rboolean backtick = TRUE;
    int old_bl = R_BrowseLines,
        blines = asInteger(GetOption1(install("deparse.max.lines")));
    if (blines != NA_INTEGER && blines > 0)
        R_BrowseLines = blines;
    SEXP result = deparse1WithCutoff(call, abbrev, DEFAULT_Cutoff, backtick,
				     opts, 0);
    R_BrowseLines = old_bl;
    return result;
}

// deparse1() version with R_BrowseLines := 0
SEXP deparse1(SEXP call, Rboolean abbrev, int opts)
{
    Rboolean backtick = TRUE;
    int old_bl = R_BrowseLines;
    R_BrowseLines = 0;
    SEXP result = deparse1WithCutoff(call, abbrev, DEFAULT_Cutoff, backtick,
				     opts, 0);
    R_BrowseLines = old_bl;
    return result;
}


/* used for language objects in print() */
attribute_hidden
SEXP deparse1w(SEXP call, Rboolean abbrev, int opts)
{
    Rboolean backtick = TRUE;
    return deparse1WithCutoff(call, abbrev, R_print.cutoff, backtick, opts, -1);
}

static SEXP deparse1WithCutoff(SEXP call, Rboolean abbrev, int cutoff,
			       Rboolean backtick, int opts, int nlines)
{
/* Arg. abbrev:
	If abbrev is TRUE, then the returned value
	is a STRSXP of length 1 with at most 13 characters.
	This is used for plot labelling etc.
*/
    SEXP svec;
    int savedigits;
    Rboolean need_ellipses = FALSE;
    LocalParseData localData =
	{/* linenumber */ 0,
	 0, 0, 0, /*startline = */TRUE, 0,
	 NULL,
	 /* DeparseBuffer= */ {NULL, 0, BUFSIZE},
	 DEFAULT_Cutoff, FALSE, 0, TRUE,
#ifdef longstring_WARN
	 FALSE,
#endif
	 /* maxlines = */ INT_MAX,
	 /* active = */TRUE, 0, FALSE};
    localData.cutoff = cutoff;
    localData.backtick = backtick;
    localData.opts = opts;
    localData.strvec = R_NilValue;

    PrintDefaults(); /* from global options() */
    savedigits = R_print.digits;
    R_print.digits = DBL_DIG;/* MAX precision */

    svec = R_NilValue;
    if (nlines > 0) {
	localData.linenumber = localData.maxlines = nlines;
    } else { // default: nlines = -1 (from R), or = 0 (from other C fn's)
	if(R_BrowseLines > 0)// not by default; e.g. from getOption("deparse.max.lines")
	    localData.maxlines = R_BrowseLines + 1; // enough to determine linenumber
	deparse2(call, svec, &localData);
	localData.active = TRUE;
	if(R_BrowseLines > 0 && localData.linenumber > R_BrowseLines) {
	    localData.linenumber = R_BrowseLines + 1;
	    need_ellipses = TRUE;
	}
    }
    PROTECT(svec = allocVector(STRSXP, localData.linenumber));
    deparse2(call, svec, &localData);
    if (abbrev) {
	char data[14];
	strncpy(data, CHAR(STRING_ELT(svec, 0)), 10);
	data[10] = '\0';
	if (strlen(CHAR(STRING_ELT(svec, 0))) > 10) strcat(data, "...");
	svec = mkString(data);
    } else if(need_ellipses) {
	SET_STRING_ELT(svec, R_BrowseLines, mkChar("  ..."));
    }
    if(nlines > 0 && localData.linenumber < nlines) {
	UNPROTECT(1); /* old svec value */
	PROTECT(svec);
	svec = lengthgets(svec, localData.linenumber);
    }
    UNPROTECT(1);
    PROTECT(svec); /* protect from warning() allocating, PR#14356 */
    R_print.digits = savedigits;
    /*: Don't warn anymore, we do deal with most (-> 'S4SXP' below)
    if ((opts & WARNINCOMPLETE) && localData.isS4)
	warning(_("deparse of an S4 object may not always be source()able"));
	else */
    if ((opts & WARNINCOMPLETE) && !localData.sourceable)
	warning(_("deparse may be incomplete"));
#ifdef longstring_WARN
    if ((opts & WARNINCOMPLETE) && localData.longstring)
	warning(_("deparse may be not be source()able in R < 2.7.0"));
#endif
    /* somewhere lower down might have allocated ... */
    R_FreeStringBuffer(&(localData.buffer));
    UNPROTECT(1);
    return svec;
}

/* deparse1line(), e.g. for non-trivial list entries in as.character(<list>).
 * --------------
 * Concatenates all lines into one long one.
 * This is needed in terms.formula, where we must be able
 * to deparse a term label into a single line of text so
 * that it can be reparsed correctly */
SEXP deparse1line_(SEXP call, Rboolean abbrev, int opts)
{
    Rboolean backtick=TRUE;
    int lines;
    SEXP temp = PROTECT(
	    deparse1WithCutoff(call, abbrev, MAX_Cutoff, backtick, opts, -1));
    if ((lines = length(temp)) > 1) {
	char *buf;
	int i;
	size_t len;
	const void *vmax;
	cetype_t enc = CE_NATIVE;
	for (len = 0, i = 0; i < length(temp); i++) {
	    SEXP s = STRING_ELT(temp, i);
	    cetype_t thisenc = getCharCE(s);
	    len += strlen(CHAR(s));  // FIXME: check for overflow?
	    if (thisenc != CE_NATIVE)
		enc = thisenc; /* assume only one non-native encoding */
	}
	vmax = vmaxget();
	buf = R_alloc((size_t) len+lines, sizeof(char));
	*buf = '\0';
	for (i = 0; i < length(temp); i++) {
	    strcat(buf, CHAR(STRING_ELT(temp, i)));
	    if (i < lines - 1)
		strcat(buf, "\n");
	}
	temp = ScalarString(mkCharCE(buf, enc));
	vmaxset(vmax);
    }
    UNPROTECT(1);
    return(temp);
}

SEXP deparse1line(SEXP call, Rboolean abbrev)
{
    return deparse1line_(call, abbrev, SIMPLEDEPARSE);
}


// called only from ./errors.c  for calls in warnings and errors :
SEXP attribute_hidden deparse1s(SEXP call)
{
   Rboolean backtick=TRUE;
   return
       deparse1WithCutoff(call, FALSE, DEFAULT_Cutoff, backtick,
			  DEFAULTDEPARSE, /* nlines = */ 1);
}

#include "Rconnections.h"

static void con_cleanup(void *data)
{
    Rconnection con = data;
    if(con->isopen) con->close(con);
}

// .Internal(dput(x, file, .deparseOpts(control)))
SEXP attribute_hidden do_dput(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    SEXP tval = CAR(args),
	saveenv = R_NilValue; // -Wall
    if (TYPEOF(tval) == CLOSXP) {
	PROTECT(saveenv = CLOENV(tval));
	SET_CLOENV(tval, R_GlobalEnv);
    }
    int opts = isNull(CADDR(args)) ? SHOWATTRIBUTES : asInteger(CADDR(args));
    tval = deparse1(tval, 0, opts);
    if (TYPEOF(CAR(args)) == CLOSXP) {
	SET_CLOENV(CAR(args), saveenv);
	UNPROTECT(1);
    }
    PROTECT(tval); /* against Rconn_printf */
    if(!inherits(CADR(args), "connection"))
	error(_("'file' must be a character string or connection"));
    int ifile = asInteger(CADR(args));
    if (ifile != 1) {
	Rconnection con = getConnection(ifile);
	RCNTXT cntxt;
	Rboolean wasopen = con->isopen;
	if(!wasopen) {
	    char mode[5];
	    strcpy(mode, con->mode);
	    strcpy(con->mode, "w");
	    if(!con->open(con)) error(_("cannot open the connection"));
	    strcpy(con->mode, mode);
	    /* Set up a context which will close the connection on error */
	    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			 R_NilValue, R_NilValue);
	    cntxt.cend = &con_cleanup;
	    cntxt.cenddata = con;
	}
	if(!con->canwrite) error(_("cannot write to this connection"));
	Rboolean havewarned = FALSE;
	for (int i = 0; i < LENGTH(tval); i++) {
	    int res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, i)));
	    if(!havewarned &&
	       res < strlen(CHAR(STRING_ELT(tval, i))) + 1) {
		warning(_("wrote too few characters"));
		havewarned = TRUE;
	    }
	}
	if(!wasopen) {endcontext(&cntxt); con->close(con);}
    }
    else { // ifile == 1 : "Stdout"
	for (int i = 0; i < LENGTH(tval); i++)
	    Rprintf("%s\n", CHAR(STRING_ELT(tval, i)));
    }
    UNPROTECT(1); /* tval */
    return (CAR(args));
}

// .Internal(dump(list, file, envir, opts, evaluate))
SEXP attribute_hidden do_dump(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP names = CAR(args),
	 file = CADR(args);
    if(!inherits(file, "connection"))
	error(_("'file' must be a character string or connection"));
    if(!isString(names))
	error( _("character arguments expected"));
    int nobjs = length(names);
    if(nobjs < 1 || length(file) < 1)
	error(_("zero-length argument"));
    SEXP source = CADDR(args);
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error(_("invalid '%s' argument"), "envir");
    int opts = asInteger(CADDDR(args));
    /* <NOTE>: change this if extra options are added */
    if(opts == NA_INTEGER || opts < 0 || opts > 2048)
	error(_("'opts' should be small non-negative integer"));
    // evaluate :
    if (!asLogical(CAD4R(args))) opts |= DELAYPROMISES;

    SEXP objs, o = PROTECT(objs = allocList(nobjs));
    int nout = 0;
    for (int i = 0; i < nobjs; i++, o = CDR(o)) {
	SET_TAG(o, installTrChar(STRING_ELT(names, i)));
	SETCAR(o, findVar(TAG(o), source));
	if (CAR(o) == R_UnboundValue)
	    warning(_("object '%s' not found"), EncodeChar(PRINTNAME(TAG(o))));
	else nout++;
    }
    o = objs;
    SEXP outnames = PROTECT(allocVector(STRSXP, nout)); // -> result
    if(nout > 0) {
	if(INTEGER(file)[0] == 1) {
	    for (int i = 0, nout = 0; i < nobjs; i++) {
		if (CAR(o) == R_UnboundValue) continue;
		const char *obj_name = translateChar(STRING_ELT(names, i));
		SET_STRING_ELT(outnames, nout++, STRING_ELT(names, i));
		if(isValidName(obj_name)) Rprintf("%s <-\n", obj_name);
		else if(opts & S_COMPAT) Rprintf("\"%s\" <-\n", obj_name);
		else Rprintf("`%s` <-\n", obj_name);
		SEXP tval = PROTECT(deparse1(CAR(o), 0, opts));
		for (int j = 0; j < LENGTH(tval); j++)
		    Rprintf("%s\n", CHAR(STRING_ELT(tval, j)));/* translated */
		UNPROTECT(1); /* tval */
		o = CDR(o);
	    }
	}
	else {
	    Rconnection con = getConnection(INTEGER(file)[0]);
	    Rboolean wasopen = con->isopen;
	    RCNTXT cntxt;
	    if(!wasopen) {
		char mode[5];
		strcpy(mode, con->mode);
		strcpy(con->mode, "w");
		if(!con->open(con)) error(_("cannot open the connection"));
		strcpy(con->mode, mode);
		/* Set up a context which will close the connection on error */
		begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
			     R_NilValue, R_NilValue);
		cntxt.cend = &con_cleanup;
		cntxt.cenddata = con;
	    }
	    if(!con->canwrite) error(_("cannot write to this connection"));
	    Rboolean havewarned = FALSE;
	    for (int i = 0, nout = 0; i < nobjs; i++) {
		if (CAR(o) == R_UnboundValue) continue;
		SET_STRING_ELT(outnames, nout++, STRING_ELT(names, i));
		int res;
		const char *s = translateChar(STRING_ELT(names, i));
		unsigned int extra = 6;
		if(isValidName(s)) {
		    extra = 4;
		    res = Rconn_printf(con, "%s <-\n", s);
		} else if(opts & S_COMPAT)
		    res = Rconn_printf(con, "\"%s\" <-\n", s);
		else
		    res = Rconn_printf(con, "`%s` <-\n", s);
		if(!havewarned && res < strlen(s) + extra)
		    warning(_("wrote too few characters"));
		SEXP tval = PROTECT(deparse1(CAR(o), 0, opts));
		for (int j = 0; j < LENGTH(tval); j++) {
		    res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, j)));
		    if(!havewarned &&
		       res < strlen(CHAR(STRING_ELT(tval, j))) + 1) {
			warning(_("wrote too few characters"));
			havewarned = TRUE;
		    }
		}
		UNPROTECT(1); /* tval */
		o = CDR(o);
	    }
	    if(!wasopen) {endcontext(&cntxt); con->close(con);}
	}
    }

    UNPROTECT(2);
    return outnames;
}

static void linebreak(Rboolean *lbreak, LocalParseData *d)
{
    if (d->len > d->cutoff) {
	if (!*lbreak) {
	    *lbreak = TRUE;
	    d->indent++;
	}
	writeline(d);
    }
}

static void deparse2(SEXP what, SEXP svec, LocalParseData *d)
{
    d->strvec = svec;
    d->linenumber = 0;
    d->indent = 0;
    deparse2buff(what, d);
    writeline(d);
}


/* curlyahead looks at s to see if it is a list with
   the first op being a curly.  You need this kind of
   lookahead info to print if statements correctly.  */
static Rboolean
curlyahead(SEXP s)
{
    if (isList(s) || isLanguage(s))
	if (TYPEOF(CAR(s)) == SYMSXP && CAR(s) == R_BraceSymbol)
	    return TRUE;
    return FALSE;
}

/* needsparens looks at an arg to a unary or binary operator to
   determine if it needs to be parenthesized when deparsed
   mainop is a unary or binary operator,
   arg is an argument to it, on the left if left == 1 */

static Rboolean needsparens(PPinfo mainop, SEXP arg, unsigned int left)
{
    PPinfo arginfo;
    if (TYPEOF(arg) == LANGSXP) {
	if (TYPEOF(CAR(arg)) == SYMSXP) {
	    if ((TYPEOF(SYMVALUE(CAR(arg))) == BUILTINSXP) ||
		(TYPEOF(SYMVALUE(CAR(arg))) == SPECIALSXP)) {
		arginfo = PPINFO(SYMVALUE(CAR(arg)));
		switch(arginfo.kind) {
		case PP_BINARY:	      /* Not all binary ops are binary! */
		case PP_BINARY2:
		    switch(length(CDR(arg))) {
		    case 1:
			if (!left)
			    return FALSE;
			if (arginfo.precedence == PREC_SUM)   /* binary +/- precedence upgraded as unary */
			    arginfo.precedence = PREC_SIGN;
		    case 2:
			if (mainop.precedence == PREC_COMPARE &&
			    arginfo.precedence == PREC_COMPARE)
		          return TRUE;     /*   a < b < c   is not legal syntax */
			break;
		    default:
			return FALSE;
		    }
		case PP_SUBSET:
		    if (mainop.kind == PP_DOLLAR)
		    	return FALSE;
		    /* fall through, don't break... */
		case PP_ASSIGN:
		case PP_ASSIGN2:
		case PP_UNARY:
		case PP_DOLLAR:
		    /* Same as other unary operators above */
		    if (arginfo.precedence == PREC_NOT && !left)
			return FALSE;
		    if (mainop.precedence > arginfo.precedence
			|| (mainop.precedence == arginfo.precedence && left == mainop.rightassoc)) {
			return TRUE;
		    }
		    break;
		case PP_FOR:
		case PP_IF:
		case PP_WHILE:
		case PP_REPEAT:
		    return left == 1;
		    break;
		default:
		    return FALSE;
		}
	    } else if (isUserBinop(CAR(arg))) {
		if (mainop.precedence > PREC_PERCENT
		    || (mainop.precedence == PREC_PERCENT && left == mainop.rightassoc)) {
		    return TRUE;
		}
	    }
	}
    }
    else if ((TYPEOF(arg) == CPLXSXP) && (length(arg) == 1)) {
	if (mainop.precedence > PREC_SUM
	    || (mainop.precedence == PREC_SUM && left == mainop.rightassoc)) {
	    return TRUE;
	}
    }
    return FALSE;
}


/* does the character() vector x contain one `NA_character_` or is all "",
 * or if(isAtomic) does it have one "recursive" or "use.names" ?  */
static Rboolean usable_nice_names(SEXP x, Rboolean isAtomic)
{
    if(TYPEOF(x) == STRSXP) {
	R_xlen_t i, n = xlength(x);
	Rboolean all_0 = TRUE;
	if(isAtomic) // c(*, recursive=, use.names=): cannot use these as nice_names
	    for (i = 0; i < n; i++) {
		if (STRING_ELT(x, i) == NA_STRING
		    || strcmp(CHAR(STRING_ELT(x, i)), "recursive") == 0
		    || strcmp(CHAR(STRING_ELT(x, i)), "use.names") == 0)
		    return FALSE;
		else if (all_0 && *CHAR(STRING_ELT(x, i))) /* length test */
		    all_0 = FALSE;
	    }
	else
	    for (i = 0; i < n; i++) {
		if (STRING_ELT(x, i) == NA_STRING)
		    return FALSE;
		else if (all_0 && *CHAR(STRING_ELT(x, i))) /* length test */
		    all_0 = FALSE;
	    }

	return !all_0;
    }
    return TRUE;
}


typedef enum { UNKNOWN = -1,
	       SIMPLE = 0,
	       OK_NAMES,   // no structure(*); names written as  (n1 = v1, ..)
	       STRUC_ATTR, // use structure(*, <attr> = *, ..) for non-names only
	       STRUC_NMS_A // use structure(*, <attr> = *, ..)  for names, too
} attr_type;

#ifdef DEBUG_DEPARSE
static const char* attrT2char(attr_type typ) {
    switch(typ) {
    case UNKNOWN: return "UNKNOWN";
    case SIMPLE: return "SIMPLE";
    case OK_NAMES: return "OK_NAMES";
    case STRUC_ATTR: return "STRUC_ATTR";
    case STRUC_NMS_A: return "STRUC_NMS_A";
    default: return "_unknown_ attr_type -- should *NOT* happen!";
    }
}
# define ChTF(_logic_) (_logic_ ? "TRUE" : "FALSE")
#endif

/* Exact semantic of NICE_NAMES and SHOWATTRIBUTES i.e. "niceNames" and "showAttributes"

C|  depCtrl   | attr1() result
-| -----------+-----------------------------------------------------------------------------
1|  NN &&  SA | STRUCT_ATTR + NN  or  STRUC_NMS_A (if NN are not "allowed")
2| !NN &&  SA | if(has attr) STRUC_NMS_A  else "SIMPLE"
3|  NN && !SA | OK_NAMES   ||  SIMPLE  if(!has_names)
4| !NN && !SA | SIMPLE


C|  depCtrl   : what should   deparse(*, control = depCtrl)   do ?
-| -----------+-----------------------------------------------------------------------------
1|  NN &&  SA : all attributes(but srcref); names "NICE"ly (<nam> = <val>) if valid [no NA]
2| !NN &&  SA : all attributes( "    "   ) use structure(..) incl names but no _nice_ names
3|  NN && !SA : no attributes but names, names nicely even when "wrong" (i.e. NA in names(.))
4| !NN && !SA : no attributes shown, not even names

*/

// is *only* called  if (d->opts & SHOW_ATTR_OR_NMS) = d->opts & (SHOW_A | NICE_N)
static attr_type attr1(SEXP s, LocalParseData *d)
{
    SEXP a = ATTRIB(s), nm = getAttrib(s, R_NamesSymbol);
    attr_type attr = UNKNOWN;
    Rboolean
	nice_names = d->opts & NICE_NAMES,
	show_attr  = d->opts & SHOWATTRIBUTES,
	has_names = !isNull(nm), ok_names;
#ifdef DEBUG_DEPARSE
    REprintf("  attr1(): has_names = %s", ChTF(has_names));
#endif
    if(has_names) {
	// ok only if there's no  NA_character_,.. in names() nor all """
	ok_names = nice_names && usable_nice_names(nm, isVectorAtomic(s));
#ifdef DEBUG_DEPARSE
	REprintf(", ok_names = %s", ChTF(ok_names));
#endif
	if(!ok_names)
	    attr = show_attr ? STRUC_NMS_A :
		/* nice_names */  OK_NAMES; // even when not ok
    }

    while(attr == UNKNOWN && !isNull(a)) {
	if(has_names && TAG(a) == R_NamesSymbol) {
	    // also  ok_names = TRUE
	} else if(show_attr && TAG(a) != R_SrcrefSymbol) {
	    attr = STRUC_ATTR;
	    break;
	}
	// else
	a = CDR(a);
    }
    if(attr == UNKNOWN)
	attr = has_names ? OK_NAMES : SIMPLE;

    if(attr >= STRUC_ATTR) {
	print2buff("structure(", d);
    } else if(has_names) { // attr <= OK_NAMES
    }
#ifdef DEBUG_DEPARSE
    REprintf(", return()ing %s\n", attrT2char(attr));
#endif
    return attr;
}

static void attr2(SEXP s, LocalParseData *d, Rboolean not_names)
{
    SEXP a = ATTRIB(s);
    while(!isNull(a)) {
	if(TAG(a) != R_SrcrefSymbol &&
	   !(TAG(a) == R_NamesSymbol && not_names)) {
	    print2buff(", ", d);
	    if(TAG(a) == R_DimSymbol) {
		print2buff(".Dim", d);
	    }
	    else if(TAG(a) == R_DimNamesSymbol) {
		print2buff(".Dimnames", d);
	    }
	    else if(TAG(a) == R_NamesSymbol) {
		print2buff(".Names", d);
	    }
	    else if(TAG(a) == R_TspSymbol) {
		print2buff(".Tsp", d);
	    }
	    else if(TAG(a) == R_LevelsSymbol) {
		print2buff(".Label", d);
	    }
	    else {
		/* TAG(a) might contain spaces etc */
		const char *tag = CHAR(PRINTNAME(TAG(a)));
		int d_opts_in = d->opts;
		d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
		if(isValidName(tag))
		    deparse2buff(TAG(a), d);
		else {
		    print2buff("\"", d);
		    deparse2buff(TAG(a), d);
		    print2buff("\"", d);
		}
		d->opts = d_opts_in;
	    }
	    print2buff(" = ", d);
	    Rboolean fnarg = d->fnarg;
	    d->fnarg = TRUE;
	    deparse2buff(CAR(a), d);
	    d->fnarg = fnarg;
	}
	a = CDR(a);
    }
    print2buff(")", d);
}


static void printcomment(SEXP s, LocalParseData *d)
{
    SEXP cmt;
    int i, ncmt;
    const void *vmax = vmaxget();

    /* look for old-style comments first */

    if(isList(TAG(s)) && !isNull(TAG(s))) {
	for (s = TAG(s); s != R_NilValue; s = CDR(s)) {
	    print2buff(translateChar(STRING_ELT(CAR(s), 0)), d);
	    writeline(d);
	}
    }
    else {
	cmt = getAttrib(s, R_CommentSymbol);
	ncmt = length(cmt);
	for(i = 0 ; i < ncmt ; i++) {
	    print2buff(translateChar(STRING_ELT(cmt, i)), d);
	    writeline(d);
	}
    }
    vmaxset(vmax);
}


static const char *quotify(SEXP name, int quote)
{
    const char *s = CHAR(name);

    /* If a symbol is not a valid name, put it in quotes, escaping
     * any quotes in the string itself */

    if (isValidName(s) || *s == '\0') return s;

    return EncodeString(name, 0, quote, Rprt_adj_none);
}

/* check for whether we need to parenthesize a caller.  The unevaluated ones
   are tricky:
   We want
     x$f(z)
     x[n](z)
     base::mean(x)
   but
     (f+g)(z)
     (function(x) 1)(x)
     etc.
*/
static Rboolean parenthesizeCaller(SEXP s)
{
    SEXP op, sym;
    if (TYPEOF(s) == LANGSXP) { /* unevaluated */
	op = CAR(s);
	if (TYPEOF(op) == SYMSXP) {
	    if (isUserBinop(op)) return TRUE;   /* %foo% */
	    sym = SYMVALUE(op);
	    if (TYPEOF(sym) == BUILTINSXP
		|| TYPEOF(sym) == SPECIALSXP) {
		if (PPINFO(sym).precedence >= PREC_SUBSET
		    || PPINFO(sym).kind == PP_FUNCALL
		    || PPINFO(sym).kind == PP_PAREN
		    || PPINFO(sym).kind == PP_CURLY) return FALSE; /* x$f(z) or x[n](z) or f(z) or (f) or {f} */
		else return TRUE;		/* (f+g)(z) etc. */
	    }
	    return FALSE;			/* regular function call */
	 } else
	    return TRUE;			/* something strange, like (1)(x) */
    } else
	return TYPEOF(s) == CLOSXP;
}

/* This is the recursive part of deparsing. */

#define SIMPLE_OPTS (~QUOTEEXPRESSIONS & ~SHOWATTRIBUTES & ~DELAYPROMISES)
/* keep KEEPINTEGER | USESOURCE | KEEPNA | S_COMPAT, also
   WARNINCOMPLETE but that is not used below this point. */
#define SHOW_ATTR_OR_NMS (SHOWATTRIBUTES | NICE_NAMES)

static void deparse2buff(SEXP s, LocalParseData *d)
{
    Rboolean lookahead = FALSE, lbreak = FALSE, fnarg = d->fnarg;
    attr_type attr = STRUC_ATTR;
    SEXP t;
    int d_opts_in = d->opts, i, n;

    d->fnarg = FALSE;

    if (!d->active) return;

    Rboolean hasS4_t = TYPEOF(s) == S4SXP;
    if (IS_S4_OBJECT(s) || hasS4_t) {
	d->isS4 = TRUE;
	/* const void *vmax = vmaxget(); */
	SEXP class = getAttrib(s, R_ClassSymbol),
	    cl_def = TYPEOF(class) == STRSXP ? STRING_ELT(class, 0) : R_NilValue;
	if(TYPEOF(cl_def) == CHARSXP) { // regular S4 objects
	    print2buff("new(\"", d);
	    print2buff(translateChar(cl_def), d);
	    print2buff("\", ", d);
	    SEXP slotNms; // ---- slotNms := methods::.slotNames(s)  ---------
	    // computed alternatively, slotNms := names(getClassDef(class)@slots) :
	    static SEXP R_getClassDef = NULL, R_slots = NULL, R_asS3 = NULL;
	    if(R_getClassDef == NULL)
		R_getClassDef = findFun(install("getClassDef"), R_MethodsNamespace);
	    if(R_slots == NULL) R_slots = install("slots");
	    if(R_asS3  == NULL) R_asS3  = install("asS3");
	    SEXP e = PROTECT(lang2(R_getClassDef, class));
	    cl_def = PROTECT(eval(e, R_BaseEnv)); // correct env?
	    slotNms = // names( cl_def@slots ) :
		getAttrib(R_do_slot(cl_def, R_slots), R_NamesSymbol);
	    UNPROTECT(2); // (e, cl_def)
	    int n;
	    Rboolean has_Data = FALSE;// does it have ".Data" slot?
	    if(TYPEOF(slotNms) == STRSXP && (n = LENGTH(slotNms))) {
		PROTECT(slotNms);
		SEXP slotlist = PROTECT(allocVector(VECSXP, n));
		// := structure(lapply(slotNms, slot, object=s), names=slotNms)
		for(int i=0; i < n; i++) {
		    SEXP slot_i = STRING_ELT(slotNms, i);
		    SET_VECTOR_ELT(slotlist, i, R_do_slot(s, installTrChar(slot_i)));
		    if(!hasS4_t && !has_Data)
			has_Data = (strcmp(CHAR(slot_i), ".Data") == 0);
		}
		setAttrib(slotlist, R_NamesSymbol, slotNms);
		vec2buff(slotlist, d, TRUE);
		/*-----------------*/
		UNPROTECT(2); // (slotNms, slotlist)
	    }
	    if(!hasS4_t && !has_Data) {
		// may have *non*-slot contents, (i.e., not in .Data)
		// ==> additionally deparse asS3(s) :
		e = PROTECT(lang2(R_asS3, s)); // = asS3(s)
		SEXP S3_s = PROTECT(eval(e, R_BaseEnv)); // correct env?
		print2buff(", ", d);
		deparse2buff(S3_s, d);
		UNPROTECT(2); // (e, S3_s)
	    }
	    print2buff(")", d);
	}
	else { // exception: class is not CHARSXP
	    if(isNull(cl_def) && isNull(ATTRIB(s))) // special
		print2buff("getClass(\"S4\")@prototype", d);
	    else { // irregular S4 ((does this ever trigger ??))
		d->sourceable = FALSE;
		print2buff("<S4 object of class ", d);
		deparse2buff(class, d);
		print2buff(">", d);
	    }
	}
	/* vmaxset(vmax); */
	return;
    } // if( S4 )

    // non-S4 cases:
    switch (TYPEOF(s)) {
    case NILSXP:
	print2buff("NULL", d);
	break;
    case SYMSXP: {
	Rboolean
	    doquote = (d_opts_in & QUOTEEXPRESSIONS) && strlen(CHAR(PRINTNAME(s)));
	if (doquote) {
	    attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	    print2buff("quote(", d);
	}
	if (d_opts_in & S_COMPAT) {
	    print2buff(quotify(PRINTNAME(s), '"'), d);
	} else if (d->backtick)
	    print2buff(quotify(PRINTNAME(s), '`'), d);
	else
	    print2buff(CHAR(PRINTNAME(s)), d);
	if (doquote) {
	    print2buff(")", d);
	    if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	}
	break;
    }
    case CHARSXP:
    {
	const void *vmax = vmaxget();
	const char *ts = translateChar(s);
#ifdef longstring_WARN
	/* versions of R < 2.7.0 cannot parse strings longer than 8192 chars */
	if(strlen(ts) >= 8192) d->longstring = TRUE;
#endif
	print2buff(ts, d);
	vmaxset(vmax);
	break;
    }
    case SPECIALSXP:
    case BUILTINSXP:
	print2buff(".Primitive(\"", d);
	print2buff(PRIMNAME(s), d);
	print2buff("\")", d);
	break;
    case PROMSXP:
	if(d->opts & DELAYPROMISES) {
	    d->sourceable = FALSE;
	    print2buff("<promise: ", d);
	    d->opts &= ~QUOTEEXPRESSIONS; /* don't want delay(quote()) */
	    deparse2buff(PREXPR(s), d);
	    d->opts = d_opts_in;
	    print2buff(">", d);
	} else {
	    PROTECT(s = eval(s, R_EmptyEnv)); /* eval uses env of promise */
	    deparse2buff(s, d);
	    UNPROTECT(1);
	}
	break;
    case CLOSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	if ((d->opts & USESOURCE)
	    && !isNull(t = getAttrib(s, R_SrcrefSymbol)))
		src2buff1(t, d);
	else {
	    /* We have established that we don't want to use the
	       source for this function */
	    d->opts &= SIMPLE_OPTS & ~USESOURCE;
	    print2buff("function (", d);
	    args2buff(FORMALS(s), 0, 1, d);
	    print2buff(") ", d);

	    writeline(d);
	    deparse2buff(BODY_EXPR(s), d);
	    d->opts = d_opts_in;
	}
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	break;
    case ENVSXP:
	d->sourceable = FALSE;
	print2buff("<environment>", d);
	break;
    case VECSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	print2buff("list(", d);
	d->opts = d_opts_in;// vec2buff() must use unchanged d
	vec2buff(s, d, attr == OK_NAMES || attr == STRUC_ATTR);
	d->opts |= NICE_NAMES;
	print2buff(")", d);
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	d->opts = d_opts_in;
	break;
    case EXPRSXP:
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	if(length(s) <= 0)
	    print2buff("expression()", d);
	else {
	    int locOpts = d->opts;
	    print2buff("expression(", d);
	    d->opts &= SIMPLE_OPTS;
	    vec2buff(s, d, attr == OK_NAMES || attr == STRUC_ATTR);
	    d->opts = locOpts;
	    print2buff(")", d);
	}
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	d->opts = d_opts_in;
	break;
    case LISTSXP: {
	attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(s, d) : SIMPLE;
	/* pairlist(x=) cannot be evaluated, hence with missings we use
	   as.pairlist(alist(...)) to allow evaluation of deparsed formals */
	Rboolean missing = FALSE;
	for(t=s; t != R_NilValue; t=CDR(t))
	    if (CAR(t) == R_MissingArg) {
		missing = TRUE;
		break;
	    }
	if (missing)
	    print2buff("as.pairlist(alist(", d);
	else
	    print2buff("pairlist(", d);
	d->inlist++;
	for (t=s ; CDR(t) != R_NilValue ; t=CDR(t) ) {
	    if( TAG(t) != R_NilValue ) {
		d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
		deparse2buff(TAG(t), d);
		d->opts = d_opts_in;
		print2buff(" = ", d);
	    }
	    deparse2buff(CAR(t), d);
	    print2buff(", ", d);
	}
	if( TAG(t) != R_NilValue ) {
	    d->opts = SIMPLEDEPARSE; /* turn off quote()ing */
	    deparse2buff(TAG(t), d);
	    d->opts = d_opts_in;
	    print2buff(" = ", d);
	}
	deparse2buff(CAR(t), d);
	if (missing)
	    print2buff("))", d);
	else
	    print2buff(")", d);
	d->inlist--;
	if(attr >= STRUC_ATTR) attr2(s, d, (attr == STRUC_ATTR));
	break;
    }
    case LANGSXP:
	printcomment(s, d);
	if (!isNull(ATTRIB(s)))
	    d->sourceable = FALSE;
	SEXP op = CAR(s);
	Rboolean doquote = FALSE;
	Rboolean maybe_quote = d_opts_in & QUOTEEXPRESSIONS;
	if (maybe_quote) {
	    // do *not* quote() formulas:
	    doquote = // := op is not `~` (tilde) :
		!((TYPEOF(op) == SYMSXP) &&
		  !strcmp(CHAR(PRINTNAME(op)), "~"));
	    if (doquote) {
		print2buff("quote(", d);
		d->opts &= SIMPLE_OPTS;
	    } else { // `~`
		d->opts &= ~QUOTEEXPRESSIONS;
	    }
	}
	if (TYPEOF(op) == SYMSXP) {
	    int userbinop = 0;
	    if ((TYPEOF(SYMVALUE(op)) == BUILTINSXP) ||
		(TYPEOF(SYMVALUE(op)) == SPECIALSXP) ||
		(userbinop = isUserBinop(op))) {
		PPinfo fop;
		Rboolean parens;
		s = CDR(s);
		if (userbinop) {
		    if (isNull(getAttrib(s, R_NamesSymbol))) {
			// not quite right for spacing, but can't be unary :
			fop.kind = PP_BINARY2;
			fop.precedence = PREC_PERCENT;
			fop.rightassoc = 0;
		    } else
			// if args are named, deparse as function call (PR#15350):
			fop.kind = PP_FUNCALL;
		} else
		    fop = PPINFO(SYMVALUE(op));
		if (fop.kind == PP_BINARY) {
		    switch (length(s)) {
		    case 1:
			fop.kind = PP_UNARY;
			if (fop.precedence == PREC_SUM)
			    // binary +/- precedence upgraded as unary
			    fop.precedence = PREC_SIGN;
			break;
		    case 2:
			break;
		    default:
			fop.kind = PP_FUNCALL;
			break;
		    }
		}
		else if (fop.kind == PP_BINARY2) {
		    if (length(s) != 2)
			fop.kind = PP_FUNCALL;
		    else if (userbinop)
			fop.kind = PP_BINARY;
		}
		switch (fop.kind) {
		case PP_IF:
		    print2buff("if (", d);
		    /* print the predicate */
		    deparse2buff(CAR(s), d);
		    print2buff(") ", d);
		    if (d->incurly && !d->inlist ) {
			lookahead = curlyahead(CAR(CDR(s)));
			if (!lookahead) {
			    writeline(d);
			    d->indent++;
			}
		    }
		    /* need to find out if there is an else */
		    if (length(s) > 2) {
			deparse2buff(CAR(CDR(s)), d);
			if (d->incurly && !d->inlist) {
			    writeline(d);
			    if (!lookahead)
				d->indent--;
			}
			else
			    print2buff(" ", d);
			print2buff("else ", d);
			deparse2buff(CAR(CDDR(s)), d);
		    }
		    else {
			deparse2buff(CAR(CDR(s)), d);
			if (d->incurly && !lookahead && !d->inlist )
			    d->indent--;
		    }
		    break;
		case PP_WHILE:
		    print2buff("while (", d);
		    deparse2buff(CAR(s), d);
		    print2buff(") ", d);
		    deparse2buff(CADR(s), d);
		    break;
		case PP_FOR:
		    print2buff("for (", d);
		    deparse2buff(CAR(s), d);
		    print2buff(" in ", d);
		    deparse2buff(CADR(s), d);
		    print2buff(") ", d);
		    deparse2buff(CADR(CDR(s)), d);
		    break;
		case PP_REPEAT:
		    print2buff("repeat ", d);
		    deparse2buff(CAR(s), d);
		    break;
		case PP_CURLY:
		    print2buff("{", d);
		    d->incurly += 1;
		    d->indent++;
		    writeline(d);
		    while (s != R_NilValue) {
			deparse2buff(CAR(s), d);
			writeline(d);
			s = CDR(s);
		    }
		    d->indent--;
		    print2buff("}", d);
		    d->incurly -= 1;
		    break;
		case PP_PAREN:
		    print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    print2buff(")", d);
		    break;
		case PP_SUBSET:
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("[", d);
		    else
			print2buff("[[", d);
		    args2buff(CDR(s), 0, 0, d);
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("]", d);
		    else
			print2buff("]]", d);
		    break;
		case PP_FUNCALL:
		case PP_RETURN:
		    if (d->backtick)
			print2buff(quotify(PRINTNAME(op), '`'), d);
		    else
			print2buff(quotify(PRINTNAME(op), '"'), d);
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 0, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FOREIGN:
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 1, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FUNCTION:
		    printcomment(s, d);
		    if (!(d->opts & USESOURCE) || !isString(CADDR(s))) {
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("(", d);
			args2buff(FORMALS(s), 0, 1, d);
			print2buff(") ", d);
			deparse2buff(CADR(s), d);
		    } else {
			s = CADDR(s);
			n = length(s);
			const void *vmax = vmaxget();
			for(i = 0 ; i < n ; i++) {
			    print2buff(translateChar(STRING_ELT(s, i)), d);
			    writeline(d);
			}
			vmaxset(vmax);
		    }
		    break;
		case PP_ASSIGN:
		case PP_ASSIGN2: {
		    Rboolean outerparens = fnarg && !strcmp(CHAR(PRINTNAME(op)), "=");
		    if (outerparens)
		    	print2buff("(", d);
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff(" ", d);
		    if ((parens = needsparens(fop, CADR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (outerparens)
		    	print2buff(")", d);
		    break;
		}
		case PP_DOLLAR:
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    /*temp fix to handle printing of x$a's */
		    if( isString(CADR(s)) &&
			isValidName(CHAR(STRING_ELT(CADR(s), 0))))
			deparse2buff(STRING_ELT(CADR(s), 0), d);
		    else {
			if ((parens = needsparens(fop, CADR(s), 0)))
			    print2buff("(", d);
			deparse2buff(CADR(s), d);
			if (parens)
			    print2buff(")", d);
		    }
		    break;
		case PP_BINARY:
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    print2buff(" ", d);
		    linebreak(&lbreak, d);
		    if ((parens = needsparens(fop, CADR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    if (lbreak) {
			d->indent--;
			lbreak = FALSE;
		    }
		    break;
		case PP_BINARY2:	/* no space between op and args */
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    if ((parens = needsparens(fop, CADR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    break;
		case PP_UNARY:
		    print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
		    if ((parens = needsparens(fop, CAR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    break;
		case PP_BREAK:
		    print2buff("break", d);
		    break;
		case PP_NEXT:
		    print2buff("next", d);
		    break;
		case PP_SUBASS:
		    if(d->opts & S_COMPAT) {
			print2buff("\"", d);
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("\'(", d);
		    } else {
			print2buff("`", d);
			print2buff(CHAR(PRINTNAME(op)), d); /* ASCII */
			print2buff("`(", d);
		    }
		    args2buff(s, 0, 0, d);
		    print2buff(")", d);
		    break;
		default:
		    d->sourceable = FALSE;
		    UNIMPLEMENTED("deparse2buff");
		}
	    }
	    else {
		SEXP val = R_NilValue; /* -Wall */
		if (isSymbol(CAR(s))) {
		    val = SYMVALUE(CAR(s));
		    if (TYPEOF(val) == PROMSXP)
			val = eval(val, R_BaseEnv);
		}
		if ( isSymbol(CAR(s))
		  && TYPEOF(val) == CLOSXP
		  && streql(CHAR(PRINTNAME(CAR(s))), "::") ) { //  :: is special case
		    deparse2buff(CADR(s), d);
		    print2buff("::", d);
		    deparse2buff(CADDR(s), d);
		}
		else if ( isSymbol(CAR(s))
		  && TYPEOF(val) == CLOSXP
		  && streql(CHAR(PRINTNAME(CAR(s))), ":::") ) { // ::: is special case
		    deparse2buff(CADR(s), d);
		    print2buff(":::", d);
		    deparse2buff(CADDR(s), d);
		}
		else {
		    if ( isSymbol(CAR(s)) ){
			if(d->opts & S_COMPAT)
			    print2buff(quotify(PRINTNAME(CAR(s)), '\''), d);
			else
			    print2buff(quotify(PRINTNAME(CAR(s)), '`'), d);
		    }
		    else
			deparse2buff(CAR(s), d);
		    print2buff("(", d);
		    args2buff(CDR(s), 0, 0, d);
		    print2buff(")", d);
		}
	    }
	} // end{op : SYMSXP }
	else if (TYPEOF(op) == CLOSXP || TYPEOF(op) == SPECIALSXP
		 || TYPEOF(op) == BUILTINSXP) {
	    if (parenthesizeCaller(op)) {
		print2buff("(", d);
		deparse2buff(op, d);
		print2buff(")", d);
	    } else
		deparse2buff(op, d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	else { /* we have a lambda expression */
	    if (parenthesizeCaller(op)) {
		print2buff("(", d);
		deparse2buff(op, d);
		print2buff(")", d);
	    } else
		deparse2buff(op, d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	if (maybe_quote) {
	    d->opts = d_opts_in;
	    if(doquote)
		print2buff(")", d);
	}
	break; // end{case LANGSXP} ---------------------------------------------
    case STRSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
    case RAWSXP:
	vector2buff(s, d);
	break;
    case EXTPTRSXP:
    {
	char tpb[32]; /* need 12+2+2*sizeof(void*) */
	d->sourceable = FALSE;
	snprintf(tpb, 32, "<pointer: %p>", R_ExternalPtrAddr(s));
	tpb[31] = '\0';
	print2buff(tpb, d);
    }
	break;
    case BCODESXP:
	d->sourceable = FALSE;
	print2buff("<bytecode>", d);
	break;
    case WEAKREFSXP:
	d->sourceable = FALSE;
	print2buff("<weak reference>", d);
	break;
    case S4SXP: {
	error("'S4SXP': should not happen - please report");
      break;
    }
    default:
	d->sourceable = FALSE;
	UNIMPLEMENTED_TYPE("deparse2buff", s);
    }
}


/* If there is a string array active point to that, and */
/* otherwise we are counting lines so don't do anything. */

static void writeline(LocalParseData *d)
{
    if (d->strvec != R_NilValue && d->linenumber < d->maxlines)
	SET_STRING_ELT(d->strvec, d->linenumber, mkChar(d->buffer.data));
    d->linenumber++;
    if (d->linenumber >= d->maxlines) d->active = FALSE;
    /* reset */
    d->len = 0;
    d->buffer.data[0] = '\0';
    d->startline = TRUE;
}

static void print2buff(const char *strng, LocalParseData *d)
{
    size_t tlen, bufflen;

    if (d->startline) {
	d->startline = FALSE;
	printtab2buff(d->indent, d);	/*if at the start of a line tab over */
    }
    tlen = strlen(strng);
    R_AllocStringBuffer(0, &(d->buffer));
    bufflen = strlen(d->buffer.data);
    R_AllocStringBuffer(bufflen + tlen, &(d->buffer));
    strcat(d->buffer.data, strng);
    d->len += (int) tlen;
}

/*
 * Encodes a complex value as a syntactically correct
 * string that can be reparsed by R. This is required
 * because by default strings like '1+Infi' or '3+NaNi'
 * are produced which are not valid complex literals.
 */

#define NB 1000  /* Same as printutils.c */
#define NB2 2*NB+25
static const char *EncodeNonFiniteComplexElement(Rcomplex x, char* buff)
{
    int w, d, e, wi, di, ei;

    // format a first time to get width/decimals
    formatComplex(&x, 1, &w, &d, &e, &wi, &di, &ei, 0);

    char Re[NB];
    char Im[NB];

    strcpy(Re, EncodeReal0(x.r, w, d, e, "."));
    strcpy(Im, EncodeReal0(x.i, wi, di, ei, "."));

    snprintf(buff, NB2, "complex(real=%s, imaginary=%s)", Re, Im);
    buff[NB2-1] = '\0';
    return buff;
}

static void deparse2buf_name(SEXP nv, int i, LocalParseData *d) {
    if (!isNull(nv) && !isNull(STRING_ELT(nv, i))
	&& *CHAR(STRING_ELT(nv, i))) { /* length test */
	/* d->opts = SIMPLEDEPARSE; This seems pointless */
	if(isValidName(translateChar(STRING_ELT(nv, i))))
	    deparse2buff(STRING_ELT(nv, i), d);
	else if(d->backtick) {
	    print2buff("`", d);
	    deparse2buff(STRING_ELT(nv, i), d);
	    print2buff("`", d);
	} else {
	    print2buff("\"", d);
	    deparse2buff(STRING_ELT(nv, i), d);
	    print2buff("\"", d);
	}
	/* d->opts = d_opts_in; */
	print2buff(" = ", d);
    }
}

// deparse atomic vectors :
static void vector2buff(SEXP vector, LocalParseData *d)
{
    // Known here:  TYPEOF(vector)  is one of the 6 atomic *SXPs
    const char *strp;
    char *buff = 0, hex[64]; // 64 is more than enough
    int i, d_opts_in = d->opts,
	tlen = length(vector),
	quote = isString(vector) ? '"' : 0;
    Rboolean surround = FALSE, allNA,
	intSeq = FALSE; // := TRUE iff integer sequence 'm:n' (up *or* down)
    if(TYPEOF(vector) == INTSXP) {
	int *vec = INTEGER(vector), d_i;
	intSeq = (tlen > 1 &&
		  vec[0] != NA_INTEGER &&
		  vec[1] != NA_INTEGER &&
		  abs(d_i = vec[1] - vec[0]) == 1);
	if(intSeq) for(i = 2; i < tlen; i++) {
	    if((vec[i] == NA_INTEGER) || (vec[i] - vec[i-1]) != d_i) {
		intSeq = FALSE;
		break;
	    }
	}
    }

    SEXP nv = R_NilValue;
    Rboolean do_names = d_opts_in & SHOW_ATTR_OR_NMS;// iff TRUE use '<tag_i> = <comp_i>'
    if(do_names) {
	nv = getAttrib(vector, R_NamesSymbol); // only "do names" if have names:
	if(isNull(nv))
	    do_names = FALSE;
    }
    Rboolean
	STR_names, // if true, use structure(.,*) for names even if(nice_names)
	need_c = tlen > 1; // (?) only TRUE iff SHOW_ATTR_OR_NMS
    STR_names = do_names && (intSeq || tlen == 0);
#ifdef DEBUG_DEPARSE
    REprintf("vector2buff(v): length(v) = %d; initial (do|STR)_names) = (%s,%s)\n",
	     tlen, ChTF(do_names), ChTF(STR_names));
#endif
    if (STR_names) // use structure(.,*) for names even if(nice_names)
	d->opts &= ~NICE_NAMES;
    attr_type attr = (d_opts_in & SHOW_ATTR_OR_NMS) ? attr1(vector, d) : SIMPLE;
    if(do_names) do_names = (attr == OK_NAMES || attr == STRUC_ATTR);
    if(!need_c) need_c = do_names; // c(a = *) but not c(1)
#ifdef DEBUG_DEPARSE
    REprintf(" -> final (do|STR)_names) = (%s,%s), attr = %s\n",
	     ChTF(do_names), ChTF(STR_names), attrT2char(attr));
#endif
    if (tlen == 0) {
	switch(TYPEOF(vector)) {
	case LGLSXP: print2buff("logical(0)", d); break;
	case INTSXP: print2buff("integer(0)", d); break;
	case REALSXP: print2buff("numeric(0)", d); break;
	case CPLXSXP: print2buff("complex(0)", d); break;
	case STRSXP: print2buff("character(0)", d); break;
	case RAWSXP: print2buff("raw(0)", d); break;
	default: UNIMPLEMENTED_TYPE("vector2buff", vector);
	}
    }
    else if(TYPEOF(vector) == INTSXP) {
	/* We treat integer separately, as S_compatible is relevant.

	   Also, it is neat to deparse m:n in that form,
	   so we do so as from 2.5.0, and for m > n, from 3.5.0
	 */
	if(intSeq) { // m:n
		strp = EncodeElement(vector, 0, '"', '.');
		print2buff(strp, d);
		print2buff(":", d);
		strp = EncodeElement(vector, tlen - 1, '"', '.');
		print2buff(strp, d);
	} else {
	    int *vec = INTEGER(vector);
	    Rboolean addL = d->opts & KEEPINTEGER & !(d->opts & S_COMPAT);
	    allNA = (d->opts & KEEPNA) || addL;
	    for(i = 0; i < tlen; i++)
		if(vec[i] != NA_INTEGER) {
		    allNA = FALSE;
		    break;
		}
	    if((d->opts & KEEPINTEGER && (d->opts & S_COMPAT))) {
		print2buff("as.integer(", d); surround = TRUE;
	    }
	    allNA = allNA && !(d->opts & S_COMPAT);
	    if(need_c) print2buff("c(", d);
	    for (i = 0; i < tlen; i++) {
		if(do_names) // put '<tag> = '
		    deparse2buf_name(nv, i, d);
		if(allNA && vec[i] == NA_INTEGER) {
		    print2buff("NA_integer_", d);
		} else {
		    strp = EncodeElement(vector, i, quote, '.');
		    print2buff(strp, d);
		    if(addL && vec[i] != NA_INTEGER) print2buff("L", d);
		}
		if (i < (tlen - 1)) print2buff(", ", d);
		if (tlen > 1 && d->len > d->cutoff) writeline(d);
		if (!d->active) break;
	    }
	    if(need_c)   print2buff(")", d);
	    if(surround) print2buff(")", d);
	}
    } else { // tlen > 0;  _not_ INTSXP
	allNA = d->opts & KEEPNA;
	if((d->opts & KEEPNA) && TYPEOF(vector) == REALSXP) {
	    for(i = 0; i < tlen; i++)
		if(!ISNA(REAL(vector)[i])) {
		    allNA = FALSE;
		    break;
		}
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.double(", d); surround = TRUE;
	    }
	} else if((d->opts & KEEPNA) && TYPEOF(vector) == CPLXSXP) {
	    Rcomplex *vec = COMPLEX(vector);
	    for(i = 0; i < tlen; i++) {
		if( !ISNA(vec[i].r) && !ISNA(vec[i].i) ) {
		    allNA = FALSE;
		    break;
		}
	    }
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.complex(", d); surround = TRUE;

	    }
	} else if((d->opts & KEEPNA) && TYPEOF(vector) == STRSXP) {
	    for(i = 0; i < tlen; i++)
		if(STRING_ELT(vector, i) != NA_STRING) {
		    allNA = FALSE;
		    break;
		}
	    if(allNA && (d->opts & S_COMPAT)) {
		print2buff("as.character(", d); surround = TRUE;
	    }
	} else if(TYPEOF(vector) == RAWSXP) {
	    print2buff("as.raw(", d); surround = TRUE;
 	}
	if(need_c) print2buff("c(", d);
	allNA = allNA && !(d->opts & S_COMPAT);
	for (i = 0; i < tlen; i++) {
	    if(do_names) // put '<tag> = '
		deparse2buf_name(nv, i, d);
	    if(allNA && TYPEOF(vector) == REALSXP &&
	       ISNA(REAL(vector)[i])) {
		strp = "NA_real_";
	    } else if (TYPEOF(vector) == CPLXSXP &&
		       (ISNA(COMPLEX(vector)[i].r)
			&& ISNA(COMPLEX(vector)[i].i)) ) {
		strp = allNA ? "NA_complex_" : EncodeElement(vector, i, quote, '.');
	    } else if(TYPEOF(vector) == CPLXSXP &&
		      (ISNAN(COMPLEX(vector)[i].r) || !R_FINITE(COMPLEX(vector)[i].i)) ) {
		if (!buff)
		    buff = alloca(NB2);
		strp = EncodeNonFiniteComplexElement(COMPLEX(vector)[i], buff);
	    } else if (allNA && TYPEOF(vector) == STRSXP &&
		       STRING_ELT(vector, i) == NA_STRING) {
		strp = "NA_character_";
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & S_COMPAT)) {
		int w, d, e;
		formatReal(&REAL(vector)[i], 1, &w, &d, &e, 0);
		strp = EncodeReal2(REAL(vector)[i], w, d, e);
	    } else if (TYPEOF(vector) == STRSXP) {
		const void *vmax = vmaxget();
#ifdef longstring_WARN
		const char *ts = translateChar(STRING_ELT(vector, i));
		/* versions of R < 2.7.0 cannot parse strings longer than 8192 chars */
		if(strlen(ts) >= 8192) d->longstring = TRUE;
#endif
		strp = EncodeElement(vector, i, quote, '.');
		vmaxset(vmax);
	    } else if (TYPEOF(vector) == RAWSXP) {
		strp = EncodeRaw(RAW(vector)[i], "0x");
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & HEXNUMERIC)) {
		double x = REAL(vector)[i];
		// Windows warns here, but incorrectly as this is C99
		// and the snprintf used from trio is compliant.
		if (R_FINITE(x)) {
		    snprintf(hex, 32, "%a", x);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == REALSXP && (d->opts & DIGITS16)) {
		double x = REAL(vector)[i];
		if (R_FINITE(x)) {
		    snprintf(hex, 32, "%.17g", x);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == CPLXSXP && (d->opts & HEXNUMERIC)) {
		Rcomplex z =  COMPLEX(vector)[i];
		if (R_FINITE(z.r) && R_FINITE(z.i)) {
		    snprintf(hex, 64, "%a + %ai", z.r, z.i);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else if (TYPEOF(vector) == CPLXSXP && (d->opts & DIGITS16)) {
		Rcomplex z =  COMPLEX(vector)[i];
		if (R_FINITE(z.r) && R_FINITE(z.i)) {
		    snprintf(hex, 64, "%.17g%+.17gi", z.r, z.i);
		    strp = hex;
		} else
		    strp = EncodeElement(vector, i, quote, '.');
	    } else
		strp = EncodeElement(vector, i, quote, '.');
	    print2buff(strp, d);
	    if (i < (tlen - 1)) print2buff(", ", d);
	    if (tlen > 1 && d->len > d->cutoff) writeline(d);
	    if (!d->active) break;
	} // for(i in 1:tlen)
	if(need_c  ) print2buff(")", d);
	if(surround) print2buff(")", d);
    }
    if(attr >= STRUC_ATTR) attr2(vector, d, (attr == STRUC_ATTR));
    if (STR_names) d->opts = d_opts_in;
} // vector2buff()


/* src2buff1: Deparse one source ref to buffer */

static void src2buff1(SEXP srcref, LocalParseData *d)
{
    int i,n;
    const void *vmax = vmaxget();
    PROTECT(srcref);

    PROTECT(srcref = lang2(R_AsCharacterSymbol, srcref));
    PROTECT(srcref = eval(srcref, R_BaseEnv));
    n = length(srcref);
    for(i = 0 ; i < n ; i++) {
	print2buff(translateChar(STRING_ELT(srcref, i)), d);
	if(i < n-1) writeline(d);
    }
    UNPROTECT(3);
    vmaxset(vmax);
}

/* src2buff : Deparse source element k to buffer, if possible; return FALSE on failure */

static Rboolean src2buff(SEXP sv, int k, LocalParseData *d)
{
    SEXP t;

    if (TYPEOF(sv) == VECSXP && length(sv) > k && !isNull(t = VECTOR_ELT(sv, k))) {
	src2buff1(t, d);
	return TRUE;
    }
    else return FALSE;
}

/* Deparse vectors of S-expressions, i.e., list() and expression() objects.
   In particular, this deparses objects of mode expression. */
static void vec2buff(SEXP v, LocalParseData *d,
		     Rboolean do_names) // iff TRUE use '<tag_i> = <comp_i>'
{
    Rboolean lbreak = FALSE;
    const void *vmax = vmaxget();
    int n = length(v);
    SEXP nv;
    if(do_names) {
	nv = getAttrib(v, R_NamesSymbol); // only "do names" if have names:
	if (isNull(nv))
	    do_names = FALSE;
    }

    SEXP sv; // Srcref or NULL
    if (d->opts & USESOURCE) {
	sv = getAttrib(v, R_SrcrefSymbol);
	if (TYPEOF(sv) != VECSXP)
	    sv = R_NilValue;
    } else
	sv = R_NilValue;

    for(int i = 0 ; i < n ; i++) {
	if (i > 0)
	    print2buff(", ", d);
	linebreak(&lbreak, d);
	if(do_names) // put '<tag> = '
	    deparse2buf_name(nv, i, d);
	if (!src2buff(sv, i, d))
	    deparse2buff(VECTOR_ELT(v, i), d);
    }
    if (lbreak)
	d->indent--;
    vmaxset(vmax);
}

static void args2buff(SEXP arglist, int lineb, int formals, LocalParseData *d)
{
    Rboolean lbreak = FALSE;

    while (arglist != R_NilValue) {
	if (TYPEOF(arglist) != LISTSXP && TYPEOF(arglist) != LANGSXP)
	    error(_("badly formed function expression"));
	if (TAG(arglist) != R_NilValue) {
	    SEXP s = TAG(arglist);

	    if( s == R_DotsSymbol )
		print2buff(CHAR(PRINTNAME(s)), d);
	    else if(d->backtick)
		print2buff(quotify(PRINTNAME(s), '`'), d);
	    else
		print2buff(quotify(PRINTNAME(s), '"'), d);

	    if(formals) {
		if (CAR(arglist) != R_MissingArg) {
		    print2buff(" = ", d);
		    d->fnarg = TRUE;
		    deparse2buff(CAR(arglist), d);
		}
	    }
	    else {
		print2buff(" = ", d);
		if (CAR(arglist) != R_MissingArg) {
		    d->fnarg = TRUE;
		    deparse2buff(CAR(arglist), d);
		}
	    }
	}
	else {
	  d->fnarg = TRUE;
	  deparse2buff(CAR(arglist), d);
	}
	arglist = CDR(arglist);
	if (arglist != R_NilValue) {
	    print2buff(", ", d);
	    linebreak(&lbreak, d);
	}
    }
    if (lbreak)
	d->indent--;
}

/* This code controls indentation.  Used to follow the S style, */
/* (print 4 tabs and then start printing spaces only) but I */
/* modified it to be closer to emacs style (RI). */

static void printtab2buff(int ntab, LocalParseData *d)
{
    int i;

    for (i = 1; i <= ntab; i++)
	if (i <= 4)
	    print2buff("    ", d);
	else
	    print2buff("  ", d);
}
