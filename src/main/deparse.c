/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2003  Robert Gentleman, Ross Ihaka and the
 *			      R Development Core Team
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
 *  IMPLEMENTATION NOTES:
 *
 *  Deparsing, has 3 layers.  The user interface, do_deparse, should
 *  not be called from an internal function, the actual deparsing needs
 *  to be done twice, once to count things up and a second time to put
 *  them into the string vector for return.  Printing this to a file
 *  is handled by the calling routine.
 *
 *
 *  INDENTATION:
 *
 *  Indentation is carried out in the routine printtab2buff at the
 *  botton of this file.  It seems like this should be settable via
 *  options.
 *
 *
 *  GLOBAL VARIABLES:
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

/*
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

#include <Defn.h>
#include <Print.h>
#include <Fileio.h>

#define BUFSIZE 512

#define MIN_Cutoff 20
#define DEFAULT_Cutoff 60
#define MAX_Cutoff (BUFSIZE - 12)
/* ----- MAX_Cutoff  <	BUFSIZE !! */

extern int isValidName(char*);

#include "RBufferUtils.h"

typedef R_StringBuffer DeparseBuffer;

typedef struct {
 int linenumber;
 int len;
 int incurly;
 int inlist;
 Rboolean startline; /* = TRUE; */
 int indent;
 SEXP strvec;

 DeparseBuffer buffer;

 int cutoff;
} LocalParseData;

static SEXP deparse1WithCutoff(SEXP call, Rboolean abbrev, int cutoff);
static void args2buff(SEXP, int, int, LocalParseData *);
static void deparse2buff(SEXP, LocalParseData *);
static void print2buff(char *, LocalParseData *);
static void printtab2buff(int, LocalParseData *);
static void scalar2buff(SEXP, LocalParseData *);
static void writeline(LocalParseData *);
static void vector2buff(SEXP, LocalParseData *);
static void vec2buff(SEXP, LocalParseData *);
static void linebreak(Rboolean *lbreak, LocalParseData *);
static void deparse2(SEXP, SEXP, LocalParseData *);

/*
  Perhaps we can consolidate all the AllocBuffers into a single
  routine across the files that provide something like this:
    character.c,
    saveload.c & scan.c (are identical modulo variable names.)
    deparse.c, printutils.c (identical and merged)
 */
void R_AllocStringBuffer(int blen, DeparseBuffer *buf)
{
    if(blen >= 0) {
	if(blen*sizeof(char) < buf->bufsize) return;
	blen = (blen+1)*sizeof(char);
	if(blen < buf->defaultSize) blen = buf->defaultSize;
	if(buf->data == NULL){
		buf->data = (char *) malloc(blen);
		buf->data[0] = '\0';
	} else
		buf->data = (char *) realloc(buf->data, blen);
	buf->bufsize = blen;
	if(!buf->data) {
		buf->bufsize = 0;
		error("Could not allocate memory for Encodebuf");
	}
    } else {
	if(buf->bufsize == buf->defaultSize) return;
	free(buf->data);
	buf->data = (char *) malloc(buf->defaultSize);
	buf->bufsize = buf->defaultSize;
    }
}

void R_FreeStringBuffer(DeparseBuffer *buf)
{
	if (buf->data != NULL)
		free(buf->data);
}

SEXP do_deparse(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ca1;
    int  cut0;
    /*checkArity(op, args);*/
    if(length(args) < 1) errorcall(call, "too few arguments");

    ca1 = CAR(args); args = CDR(args);
    cut0 = DEFAULT_Cutoff;
    if(!isNull(CAR(args))) {
	cut0 = asInteger(CAR(args));
	if(cut0 == NA_INTEGER|| cut0 < MIN_Cutoff || cut0 > MAX_Cutoff) {
	    warning("invalid `cutoff' for deparse, using default");
	    cut0 = DEFAULT_Cutoff;
	}
    }
    ca1 = deparse1WithCutoff(ca1, 0, cut0);
    return ca1;
}

SEXP deparse1(SEXP call, Rboolean abbrev)
{
    return(deparse1WithCutoff(call, abbrev, DEFAULT_Cutoff));
}

static SEXP deparse1WithCutoff(SEXP call, Rboolean abbrev, int cutoff)
{
/* Arg. abbrev:
	If abbrev is TRUE, then the returned value
	is a STRSXP of length 1 with at most 10 characters.
	This is used for plot labelling etc.
*/
    SEXP svec;
    int savedigits;
    /* The following gives warning (when "-pedantic" is used):
       In function `deparse1WithCutoff': initializer element
       is not computable at load time -- and its because of R_NilValue
       (RH 7.1 gcc 2.96  wrongly gives an error with "-pedantic")
    */
    LocalParseData localData =
	    {0, 0, 0, 0, /*startline = */TRUE, 0,
	     NULL,
	     /*DeparseBuffer=*/{NULL, 0, BUFSIZE},
	     DEFAULT_Cutoff};
    DeparseBuffer *buffer = &localData.buffer;
    localData.cutoff = cutoff;
    localData.strvec = R_NilValue;

    PrintDefaults(R_NilValue);/* from global options() */
    savedigits = R_print.digits;
    R_print.digits = DBL_DIG;/* MAX precision */

    svec = R_NilValue;
    deparse2(call, svec, &localData);/* just to determine linenumber..*/
    PROTECT(svec = allocVector(STRSXP, localData.linenumber));
    deparse2(call, svec, &localData);
    UNPROTECT(1);
    if (abbrev) {
	R_AllocStringBuffer(0, buffer);
	buffer->data[0] = '\0';
	strncat(buffer->data, CHAR(STRING_ELT(svec, 0)), 10);
	if (strlen(CHAR(STRING_ELT(svec, 0))) > 10)
	    strcat(buffer->data, "...");
	svec = mkString(buffer->data);
    }
    R_print.digits = savedigits;
    R_FreeStringBuffer(buffer);
    return svec;
}

/* deparse1line uses the maximum cutoff rather than the default */
/* This is needed in terms.formula, where we must be able */
/* to deparse a term label into a single line of text so */
/* that it can be reparsed correctly */
SEXP deparse1line(SEXP call, Rboolean abbrev)
{
   SEXP temp;

   temp = deparse1WithCutoff(call, abbrev, MAX_Cutoff);
   return(temp);
}

#include "Rconnections.h"

SEXP do_dput(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP saveenv, tval;
    int i, ifile, res;
    Rboolean wasopen, havewarned = FALSE;
    Rconnection con = (Rconnection) 1; /* stdout */

    checkArity(op, args);

    tval = CAR(args);
    saveenv = R_NilValue;	/* -Wall */
    if (TYPEOF(tval) == CLOSXP) {
	PROTECT(saveenv = CLOENV(tval));
	SET_CLOENV(tval, R_GlobalEnv);
    }
    tval = deparse1(tval, 0);
    if (TYPEOF(CAR(args)) == CLOSXP) {
	SET_CLOENV(CAR(args), saveenv);
	UNPROTECT(1);
    }
    ifile = asInteger(CADR(args));

    wasopen = 1;
    if (ifile != 1) {
	con = getConnection(ifile);
	wasopen = con->isopen;
	if(!wasopen)
	    if(!con->open(con)) error("cannot open the connection");
    }/* else: "Stdout" */
    for (i = 0; i < LENGTH(tval); i++)
	if (ifile == 1)
	    Rprintf("%s\n", CHAR(STRING_ELT(tval, i)));
	else {
	    res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, i)));
	    if(!havewarned &&
	       res < strlen(CHAR(STRING_ELT(tval, i))) + 1)
		warningcall(call, "wrote too few characters");
	}
    if (!wasopen) con->close(con);
    return (CAR(args));
}

SEXP do_dump(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, names, o, objs, tval, source;
    int i, j, nobjs, res;
    Rboolean wasopen, havewarned = FALSE;
    Rconnection con;

    checkArity(op, args);

    names = CAR(args);
    file = CADR(args);
    if(!isString(names))
	errorcall(call, "character arguments expected");
    nobjs = length(names);
    if(nobjs < 1 || length(file) < 1)
	errorcall(call, "zero length argument");
    source = CADDR(args);
    if (source != R_NilValue && TYPEOF(source) != ENVSXP)
	error("bad environment");

    PROTECT(o = objs = allocList(nobjs));

    for (j = 0; j < nobjs; j++, o = CDR(o)) {
	SET_TAG(o, install(CHAR(STRING_ELT(names, j))));
	SETCAR(o, findVar(TAG(o), source));
	if (CAR(o) == R_UnboundValue)
	    error("Object \"%s\" not found", CHAR(PRINTNAME(TAG(o))));
    }
    o = objs;
    if(INTEGER(file)[0] == 1) {
	for (i = 0; i < nobjs; i++) {
	    Rprintf("\"%s\" <-\n", CHAR(STRING_ELT(names, i)));
	    if (TYPEOF(CAR(o)) != CLOSXP ||
		isNull(tval = getAttrib(CAR(o), R_SourceSymbol)))
	    tval = deparse1(CAR(o), 0);
	    for (j = 0; j < LENGTH(tval); j++) {
		Rprintf("%s\n", CHAR(STRING_ELT(tval, j)));
	    }
	    o = CDR(o);
	}
    }
    else {
	con = getConnection(INTEGER(file)[0]);
	wasopen = con->isopen;
	if (!wasopen)
	    if(!con->open(con)) error("cannot open the connection");
	for (i = 0; i < nobjs; i++) {
	    res = Rconn_printf(con, "\"%s\" <-\n", CHAR(STRING_ELT(names, i)));
	    if(!havewarned &&
	       res < strlen(CHAR(STRING_ELT(names, i))) + 4)
		warningcall(call, "wrote too few characters");
	    if (TYPEOF(CAR(o)) != CLOSXP ||
		isNull(tval = getAttrib(CAR(o), R_SourceSymbol)))
	    tval = deparse1(CAR(o), 0);
	    for (j = 0; j < LENGTH(tval); j++) {
		res = Rconn_printf(con, "%s\n", CHAR(STRING_ELT(tval, j)));
		if(!havewarned &&
		   res < strlen(CHAR(STRING_ELT(tval, j))) + 1)
		    warningcall(call, "wrote too few characters");
	    }
	    o = CDR(o);
	}
	if (!wasopen) con->close(con);
    }

    UNPROTECT(1);
    R_Visible = 0;
    return names;
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
	if (TYPEOF(CAR(s)) == SYMSXP && CAR(s) == install("{"))
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
			break;
		    default:
			return FALSE;
		    }
		case PP_ASSIGN:
		case PP_ASSIGN2:
		case PP_SUBSET:
		case PP_UNARY:
		case PP_DOLLAR:
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

static void attr1(SEXP s, LocalParseData *d)
{
    if(ATTRIB(s) != R_NilValue)
	print2buff("structure(", d);
}

static void attr2(SEXP s, LocalParseData *d)
{
    if(ATTRIB(s) != R_NilValue) {
	SEXP a = ATTRIB(s);
	while(!isNull(a)) {
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
		char *tag = CHAR(PRINTNAME(TAG(a)));
		if(isValidName(tag))
		    deparse2buff(TAG(a), d);
		else {
		    print2buff("\"", d);
		    deparse2buff(TAG(a), d);
		    print2buff("\"", d);
		}
	    }
	    print2buff(" = ", d);
	    deparse2buff(CAR(a), d);
	    a = CDR(a);
	}
	print2buff(")", d);
    }
}


static void printcomment(SEXP s, LocalParseData *d)
{
    SEXP cmt;
    int i, ncmt;

    /* look for old-style comments first */

    if(isList(TAG(s)) && !isNull(TAG(s))) {
	for (s = TAG(s); s != R_NilValue; s = CDR(s)) {
	    print2buff(CHAR(STRING_ELT(CAR(s), 0)), d);
	    writeline(d);
	}
    }
    else {
	cmt = getAttrib(s, R_CommentSymbol);
	ncmt = length(cmt);
	for(i = 0 ; i < ncmt ; i++) {
	    print2buff(CHAR(STRING_ELT(cmt, i)), d);
	    writeline(d);
	}
    }
}

	/* This is the recursive part of deparsing. */

static void deparse2buff(SEXP s, LocalParseData *d)
{
    PPinfo fop;
    Rboolean lookahead = FALSE, lbreak = FALSE, parens;
    SEXP op, t;
    char tpb[120];

    switch (TYPEOF(s)) {
    case NILSXP:
	print2buff("NULL", d);
	break;
    case SYMSXP:
	print2buff(CHAR(PRINTNAME(s)), d);
	break;
    case CHARSXP:
	print2buff(CHAR(s), d);
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	sprintf(tpb, ".Primitive(\"%s\")", PRIMNAME(s));
	print2buff(tpb, d);
	break;
    case PROMSXP:
	deparse2buff(PREXPR(s), d);
	break;
    case CLOSXP:
	print2buff("function (", d);
	args2buff(FORMALS(s), 0, 1, d);
	print2buff(") ", d);
	writeline(d);
	deparse2buff(BODY(s), d);
	break;
    case ENVSXP:
	print2buff("<environment>", d);
	break;
    case VECSXP:
	attr1(s, d);
	print2buff("list(", d);
	vec2buff(s, d);
	print2buff(")", d);
	attr2(s, d);
	break;
    case EXPRSXP:
	if(length(s) <= 0)
	    print2buff("expression()", d);
	else {
	    print2buff("expression(", d);
	    vec2buff(s, d);
	    print2buff(")", d);
	}
	break;
    case LISTSXP:
	attr1(s, d);
	print2buff("list(", d);
	d->inlist++;
	for (t=s ; CDR(t) != R_NilValue ; t=CDR(t) ) {
	    if( TAG(t) != R_NilValue ) {
		deparse2buff(TAG(t), d);
		print2buff(" = ", d);
	    }
	    deparse2buff(CAR(t), d);
	    print2buff(", ", d);
	}
	if( TAG(t) != R_NilValue ) {
	    deparse2buff(TAG(t), d);
	    print2buff(" = ", d);
	}
	deparse2buff(CAR(t), d);
	print2buff(")", d);
	d->inlist--;
	attr2(s, d);
	break;
    case LANGSXP:
	printcomment(s, d);
	if (TYPEOF(CAR(s)) == SYMSXP) {
	    if ((TYPEOF(SYMVALUE(CAR(s))) == BUILTINSXP) ||
		(TYPEOF(SYMVALUE(CAR(s))) == SPECIALSXP)) {
		op = CAR(s);
		fop = PPINFO(SYMVALUE(op));
		s = CDR(s);
		if (fop.kind == PP_BINARY) {
		    switch (length(s)) {
		    case 1:
			fop.kind = PP_UNARY;
			if (fop.precedence == PREC_SUM)   /* binary +/- precedence upgraded as unary */
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
		    deparse2buff(CAR(s), d);
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
		    if (isValidName(CHAR(PRINTNAME(op))))
			print2buff(CHAR(PRINTNAME(op)), d);
		    else {
			print2buff("\"", d);
			print2buff(CHAR(PRINTNAME(op)), d);
			print2buff("\"", d);
		    }
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 0, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FOREIGN:
		    print2buff(CHAR(PRINTNAME(op)), d);
		    print2buff("(", d);
		    d->inlist++;
		    args2buff(s, 1, 0, d);
		    d->inlist--;
		    print2buff(")", d);
		    break;
		case PP_FUNCTION:
		    printcomment(s, d);
		    print2buff(CHAR(PRINTNAME(op)), d);
		    print2buff("(", d);
		    args2buff(FORMALS(s), 0, 1, d);
		    print2buff(") ", d);
		    deparse2buff(CADR(s), d);
		    break;
		case PP_ASSIGN:
		case PP_ASSIGN2:
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d);
		    print2buff(" ", d);
		    if ((parens = needsparens(fop, CADR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    break;
		case PP_DOLLAR:
		    if ((parens = needsparens(fop, CAR(s), 1)))
			print2buff("(", d);
		    deparse2buff(CAR(s), d);
		    if (parens)
			print2buff(")", d);
		    deparse2buff(op, d);
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
		    print2buff(CHAR(PRINTNAME(op)), d);
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
		    print2buff(CHAR(PRINTNAME(op)), d);
		    if ((parens = needsparens(fop, CADR(s), 0)))
			print2buff("(", d);
		    deparse2buff(CADR(s), d);
		    if (parens)
			print2buff(")", d);
		    break;
		case PP_UNARY:
		    print2buff(CHAR(PRINTNAME(op)), d);
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
		    print2buff("\"", d);
		    print2buff(CHAR(PRINTNAME(op)), d);
		    print2buff("\"(", d);
		    args2buff(s, 0, 0, d);
		    print2buff(")", d);
		    break;
		default:
		    UNIMPLEMENTED("deparse2buff");
		}
	    }
	    else {
		if(isSymbol(CAR(s)) && isUserBinop(CAR(s))) {
		    op = CAR(s);
		    s = CDR(s);
		    deparse2buff(CAR(s), d);
		    print2buff(" ", d);
		    print2buff(CHAR(PRINTNAME(op)), d);
		    print2buff(" ", d);
		    linebreak(&lbreak, d);
		    deparse2buff(CADR(s), d);
		    if (lbreak) {
			d->indent--;
			lbreak = FALSE;
		    }
		    break;
		}
		else {
		    if ( isSymbol(CAR(s))
		      && TYPEOF(SYMVALUE(CAR(s))) == CLOSXP
		      && streql(CHAR(PRINTNAME(CAR(s))), "::") ){ /*  :: is special case */
		    	deparse2buff(CADR(s), d);
		    	print2buff("::", d);
			deparse2buff(CADDR(s), d);
		    }
		    else {
			if ( isSymbol(CAR(s)) ){
			    if ( !isValidName(CHAR(PRINTNAME(CAR(s)))) ){

				print2buff("\"", d);
				print2buff(CHAR(PRINTNAME(CAR(s))), d);
				print2buff("\"", d);
			    } else
				print2buff(CHAR(PRINTNAME(CAR(s))), d);
			}
			else
			    deparse2buff(CAR(s), d);
			print2buff("(", d);
			args2buff(CDR(s), 0, 0, d);
			print2buff(")", d);
		    }
		}
	    }
	}
	else if (TYPEOF(CAR(s)) == CLOSXP || TYPEOF(CAR(s)) == SPECIALSXP
		 || TYPEOF(CAR(s)) == BUILTINSXP) {
	    deparse2buff(CAR(s), d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	else { /* we have a lambda expression */
	    deparse2buff(CAR(s), d);
	    print2buff("(", d);
	    args2buff(CDR(s), 0, 0, d);
	    print2buff(")", d);
	}
	break;
    case STRSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	attr1(s, d);
	vector2buff(s, d);
	attr2(s, d);
	break;
    case EXTPTRSXP:
	sprintf(tpb, "<pointer: %p>", R_ExternalPtrAddr(s));
	print2buff(tpb, d);
	break;
    case WEAKREFSXP:
	sprintf(tpb, "<weak reference>");
	print2buff(tpb, d);
	break;
    default:
	UNIMPLEMENTED("deparse2buff");
    }
}


/* If there is a string array active point to that, and */
/* otherwise we are counting lines so don't do anything. */

static void writeline(LocalParseData *d)
{
    if (d->strvec != R_NilValue)
	SET_STRING_ELT(d->strvec, d->linenumber, mkChar(d->buffer.data));
    d->linenumber++;
    /* reset */
    d->len = 0;
    d->buffer.data[0] = '\0';
    d->startline = TRUE;
}

static void print2buff(char *strng, LocalParseData *d)
{
    int tlen, bufflen;

    if (d->startline) {
	d->startline = FALSE;
	printtab2buff(d->indent, d);	/*if at the start of a line tab over */
    }
    tlen = strlen(strng);
    R_AllocStringBuffer(0, &(d->buffer));
    bufflen = strlen(d->buffer.data);
    /*if (bufflen + tlen > BUFSIZE) {
	buff[0] = '\0';
	error("string too long in deparse");
	}*/
    R_AllocStringBuffer(bufflen + tlen, &(d->buffer));
    strcat(d->buffer.data, strng);
    d->len += tlen;
}

static void scalar2buff(SEXP inscalar, LocalParseData *d)
{
    char *strp;
    strp = EncodeElement(inscalar, 0, '"');
    print2buff(strp, d);
}

static void vector2buff(SEXP vector, LocalParseData *d)
{
    int tlen, i, quote;
    char *strp;

    tlen = length(vector);
    if( isString(vector) )
	quote='"';
    else
	quote=0;
    if (tlen == 0) {
	switch(TYPEOF(vector)) {
	case LGLSXP: print2buff("logical(0)", d); break;
	case INTSXP: print2buff("numeric(0)", d); break;
	case REALSXP: print2buff("numeric(0)", d); break;
	case CPLXSXP: print2buff("complex(0)", d); break;
	case STRSXP: print2buff("character(0)", d); break;
	}
    }
    else if (tlen == 1) {
	scalar2buff(vector, d);
    }
    else {
	print2buff("c(", d);
	for (i = 0; i < tlen; i++) {
	    strp = EncodeElement(vector, i, quote);
	    print2buff(strp, d);
	    if (i < (tlen - 1))
		print2buff(", ", d);
	    if (d->len > d->cutoff)
		writeline(d);
	}
	print2buff(")", d);
    }

}

/* vec2buff : New Code */
/* Deparse vectors of S-expressions. */
/* In particular, this deparses objects of mode expression. */

static void vec2buff(SEXP v, LocalParseData *d)
{
    SEXP nv;
    int i, n;
    Rboolean lbreak = FALSE;

    n = length(v);
    nv = getAttrib(v, R_NamesSymbol);
    if (length(nv) == 0) nv = R_NilValue;

    for(i = 0 ; i < n ; i++) {
	if (i > 0)
	    print2buff(", ", d);
	linebreak(&lbreak, d);
	if (!isNull(nv) && !isNull(STRING_ELT(nv, i))
	    && *CHAR(STRING_ELT(nv, i))) {
	    if( isValidName(CHAR(STRING_ELT(nv, i))) )
		deparse2buff(STRING_ELT(nv, i), d);
	    else {
		print2buff("\"", d);
		deparse2buff(STRING_ELT(nv, i), d);
		print2buff("\"", d);
	    }
	    print2buff(" = ", d);
	}
	deparse2buff(VECTOR_ELT(v, i), d);
    }
    if (lbreak)
	d->indent--;
}

static void args2buff(SEXP arglist, int lineb, int formals, LocalParseData *d)
{
    Rboolean lbreak = FALSE;

    while (arglist != R_NilValue) {
	if (TAG(arglist) != R_NilValue) {
#if 0
	    deparse2buff(TAG(arglist));
#else
	    char tpb[120];
	    SEXP s = TAG(arglist);

	    if( s == R_DotsSymbol || isValidName(CHAR(PRINTNAME(s))) )
		print2buff(CHAR(PRINTNAME(s)), d);
	    else {
		if( strlen(CHAR(PRINTNAME(s)))< 117 ) {
		    sprintf(tpb,"\"%s\"",CHAR(PRINTNAME(s)));
		    print2buff(tpb, d);
		}
		else {
		    sprintf(tpb,"\"");
		    strncat(tpb, CHAR(PRINTNAME(s)), 117);
		    strcat(tpb, "\"");
		    print2buff(tpb, d);
		}
	    }
#endif
	    if(formals) {
		if (CAR(arglist) != R_MissingArg) {
		    print2buff(" = ", d);
		    deparse2buff(CAR(arglist), d);
		}
	    }
	    else {
		print2buff(" = ", d);
		if (CAR(arglist) != R_MissingArg) {
		    deparse2buff(CAR(arglist), d);
		}
	    }
	}
	else deparse2buff(CAR(arglist), d);
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
