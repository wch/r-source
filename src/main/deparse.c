/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  Robert Gentleman, Ross Ihaka and the
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
 *  startline:	 indicator 0=start of a line (so we can tab out to
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


/* FIXME : The code below saves and restores the value of the global */
/* variable "cutoff".  This could create a problem if a user interrupts */
/* the deparse before there is a chance to restore the value.  One */
/* possible fix is to restructure the code with another function which */
/* takes a cutoff value as a parameter.	 Then "do_deparse" and "deparse1" */
/* could each call this deeper function with the appropriate argument. */
/* I wonder why I didn't just do this? -- it would have been quicker than */
/* writing this note.  I guess it needs a bit more thought ... */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"
#include "Print.h"
#include "names.h"
#include "Fileio.h"

#define BUFSIZE 512

#define MIN_Cutoff 20
#define DEFAULT_Cutoff 60
#define MAX_Cutoff (BUFSIZE - 12)
/* ----- MAX_Cutoff  <	BUFSIZE !! */

static int cutoff = DEFAULT_Cutoff;
extern int isValidName(char*);

/*static char buff[BUFSIZE];*/
static int linenumber;
static int len;
static int incurly = 0;
static int inlist = 0;
static int startline = 0;
static int indent = 0;
static SEXP strvec;

static void args2buff(SEXP, int, int);
static void deparse2buff(SEXP);
static void print2buff(char *);
static void printtab2buff(int);
static void scalar2buff(SEXP);
static void writeline(void);
static void vector2buff(SEXP);
static void vec2buff(SEXP);
static void linebreak();
static void deparse2(SEXP, SEXP);


static char *buff=NULL;

static void AllocBuffer(int len)
{
    static int bufsize = 0;
    if(len*sizeof(char) < bufsize) return;
    len = (len+1)*sizeof(char);
    if(len < BUFSIZE) len = BUFSIZE;
    if(buff == NULL){
	buff = (char *) malloc(len);
	buff[0] = '\0';
    } else
	buff = (char *) realloc(buff, len);
    bufsize = len;
    if(!buff) {
	bufsize = 0;
	error("Could not allocate memory for Encodebuf");
    }
}


SEXP do_deparse(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ca1;
    int savecutoff, cut0;
    /*checkArity(op, args);*/
    if(length(args) < 1) errorcall(call, "too few arguments");

    ca1 = CAR(args); args = CDR(args);
    savecutoff = cutoff;
    cutoff = DEFAULT_Cutoff;
    if(!isNull(CAR(args))) {
	cut0 = asInteger(CAR(args));
	if(cut0 == NA_INTEGER|| cut0 < MIN_Cutoff || cut0 > MAX_Cutoff)
	    warning("invalid 'cutoff' for deparse, used default");
	else
	    cutoff = cut0;
    }
    ca1 = deparse1(ca1, 0);
    cutoff = savecutoff;
    return ca1;
}

SEXP deparse1(SEXP call, int abbrev)
{
/* Argument  abbrev ("logical"):
	If abbrev is 1(TRUE), then the returned value
	is a STRSXP of length 1 with at most 10 characters.
	This is used for plot labelling etc.
*/
    SEXP svec;
    int savedigits;

    PrintDefaults(R_NilValue);/* from global options() */
    savedigits = R_print.digits;
    R_print.digits = DBL_DIG;/* MAX precision */

    svec = R_NilValue;
    deparse2(call, svec);/* just to determine linenumber..*/
    PROTECT(svec = allocVector(STRSXP, linenumber));
    deparse2(call, svec);
    UNPROTECT(1);
    if (abbrev == 1) {
	AllocBuffer(0);
	buff[0] = '\0';
	strncat(buff, CHAR(STRING(svec)[0]), 10);
	if (strlen(CHAR(STRING(svec)[0])) > 10)
	    strcat(buff, "...");
	svec = mkString(buff);
    }
    R_print.digits = savedigits;
    return svec;
}

/* deparse1line uses the maximum cutoff rather than the default */
/* This is needed in terms.formula, where we must be able */
/* to deparse a term label into a single line of text so */
/* that it can be reparsed correctly */
SEXP deparse1line(SEXP call, int abbrev)
{
   int savecutoff;
   SEXP temp;

   savecutoff = cutoff;
   cutoff = MAX_Cutoff;
   temp = deparse1(call, abbrev);
   cutoff = savecutoff;
   return(temp);
}


SEXP do_dput(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    FILE *fp;
    SEXP file, saveenv, tval;
    int i;

    checkArity(op, args);

    tval = CAR(args);
    saveenv = R_NilValue;	/* -Wall */
    if (TYPEOF(tval) == CLOSXP) {
	PROTECT(saveenv = CLOENV(tval));
	CLOENV(tval) = R_GlobalEnv;
    }
    tval = deparse1(tval, 0);
    if (TYPEOF(CAR(args)) == CLOSXP) {
	CLOENV(CAR(args)) = saveenv;
	UNPROTECT(1);
    }
    file = CADR(args);
    if (!isValidString(file))
	errorcall(call, "file name must be a valid character string");

    fp = NULL;
    if (strlen(CHAR(STRING(file)[0])) > 0) {
	fp = R_fopen(R_ExpandFileName(CHAR(STRING(file)[0])), "w");
	if (!fp)
	    errorcall(call, "unable to open file");
    }/* else: "Stdout" */
    for (i = 0; i < LENGTH(tval); i++)
	if (fp == NULL)
	    Rprintf("%s\n", CHAR(STRING(tval)[i]));
	else
	    fprintf(fp, "%s\n", CHAR(STRING(tval)[i]));
    if (fp != NULL)
	fclose(fp);
    return (CAR(args));
}

SEXP do_dump(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP file, names, o, objs, tval;
    int i, j, nobjs;
    FILE *fp;

    checkArity(op, args);

    names = CAR(args);
    file = CADR(args);
    if(!isString(names) || !isString(file))
	errorcall(call, "character arguments expected");
    nobjs = length(names);
    if(nobjs < 1 || length(file) < 1)
	errorcall(call, "zero length argument");

    fp = NULL;

    PROTECT(o = objs = allocList(nobjs));

    for(i = 0 ; i < nobjs ; i++) {
	CAR(o) = eval(install(CHAR(STRING(names)[i])), rho);
	o = CDR(o);
    }

    o = objs;
    if(strlen(CHAR(STRING(file)[0])) == 0) {
	for (i = 0; i < nobjs; i++) {
	    Rprintf("\"%s\" <-\n", CHAR(STRING(names)[i]));
	    if (TYPEOF(CAR(o)) != CLOSXP || 
		isNull(tval = getAttrib(CAR(o), R_SourceSymbol)))
	    tval = deparse1(CAR(o), 0);
	    for (j = 0; j<LENGTH(tval); j++) {
		Rprintf("%s\n", CHAR(STRING(tval)[j]));
	    }
	    o = CDR(o);
	}
    }
    else {
	if(!(fp = R_fopen(R_ExpandFileName(CHAR(STRING(file)[0])), "w")))
	    errorcall(call, "unable to open file");
	for (i = 0; i < nobjs; i++) {
	    fprintf(fp, "\"%s\" <-\n", CHAR(STRING(names)[i]));
	    if (TYPEOF(CAR(o)) != CLOSXP || 
		isNull(tval = getAttrib(CAR(o), R_SourceSymbol)))
	    tval = deparse1(CAR(o), 0);
	    for (j = 0; j<LENGTH(tval); j++) {
		fprintf(fp, "%s\n", CHAR(STRING(tval)[j]));
	    }
	    o = CDR(o);
	}
	fclose(fp);
    }

    UNPROTECT(1);
    R_Visible = 0;
    return names;
}

static void linebreak(int *lbreak)
{
    if (len > cutoff) {
	if (!*lbreak) {
	    *lbreak = 1;
	    indent++;
	}
	writeline();
    }
}

static void deparse2(SEXP what, SEXP svec)
{
    strvec = svec;
    linenumber = 0;
    indent = 0;
    deparse2buff(what);
    writeline();
}


/* curlyahead looks at s to see if it is a list with */
/* the first op being a curly. You need this kind of */
/* lookahead info to print if statements correctly.  */

static int curlyahead(SEXP s)
{
    if (isList(s) || isLanguage(s))
	if (TYPEOF(CAR(s)) == SYMSXP && CAR(s) == install("{"))
	    return 1;
    return 0;
}

static void attr1(SEXP s)
{
    if(ATTRIB(s) != R_NilValue)
	print2buff("structure(");
}

static void attr2(SEXP s)
{
    if(ATTRIB(s) != R_NilValue) {
	SEXP a = ATTRIB(s);
	while(!isNull(a)) {
	    print2buff(", ");
	    if(TAG(a) == R_DimSymbol) {
		print2buff(".Dim");
	    }
	    else if(TAG(a) == R_DimNamesSymbol) {
		print2buff(".Dimnames");
	    }
	    else if(TAG(a) == R_NamesSymbol) {
		print2buff(".Names");
	    }
	    else if(TAG(a) == R_TspSymbol) {
		print2buff(".Tsp");
	    }
	    else if(TAG(a) == R_LevelsSymbol) {
		print2buff(".Label");
	    }
	    else deparse2buff(TAG(a));
	    print2buff(" = ");
	    deparse2buff(CAR(a));
	    a = CDR(a);
	}
	print2buff(")");
    }
}


static void printcomment(SEXP s)
{
    SEXP cmt;
    int i, ncmt;

    /* look for old-style comments first */

    if(isList(TAG(s)) && !isNull(TAG(s))) {
	for (s = TAG(s); s != R_NilValue; s = CDR(s)) {
	    print2buff(CHAR(STRING(CAR(s))[0]));
	    writeline();
	}
    }
    else {
	cmt = getAttrib(s, R_CommentSymbol);
	ncmt = length(cmt);
	for(i = 0 ; i < ncmt ; i++) {
	    print2buff(CHAR(STRING(cmt)[i]));
	    writeline();
	}
    }
}

	/* This is the recursive part of deparsing. */

static void deparse2buff(SEXP s)
{
    int fop, lookahead = 0, lbreak = 0;
    SEXP op, t;
    char tpb[120];

    switch (TYPEOF(s)) {
    case NILSXP:
	print2buff("NULL");
	break;
    case SYMSXP:
#if 1
	print2buff(CHAR(PRINTNAME(s)));
#else
	/* I'm pretty sure this is WRONG:
	   Blindly putting special symbols in ""s causes more trouble
	   than it solves
	   --pd
	   */
	if( isValidName(CHAR(PRINTNAME(s))) )
	    print2buff(CHAR(PRINTNAME(s)));
	else {
	    if( strlen(CHAR(PRINTNAME(s)))< 117 ) {
		sprintf(tpb,"\"%s\"",CHAR(PRINTNAME(s)));
		print2buff(tpb);
	    }
	    else {
		sprintf(tpb,"\"");
		strncat(tpb, CHAR(PRINTNAME(s)), 117);
		strcat(tpb, "\"");
		print2buff(tpb);
	    }
	}
#endif
	break;
    case CHARSXP:
	print2buff(CHAR(s));
	break;
    case SPECIALSXP:
    case BUILTINSXP:
	sprintf(tpb, ".Primitive(\"%s\")", PRIMNAME(s));
	print2buff(tpb);
	break;
    case PROMSXP:
	deparse2buff(PREXPR(s));
	break;
    case CLOSXP:
	print2buff("function (");
	args2buff(FORMALS(s), 0, 1);
	print2buff(") ");
	writeline();
	deparse2buff(BODY(s));
	break;
    case ENVSXP:
	print2buff("<environment>");
	break;
    case VECSXP:
	if(length(s) <= 0)
	    print2buff("list()");
	else {
	    attr1(s);
	    print2buff("list(");
	    vec2buff(s);
	    print2buff(")");
	    attr2(s);
	}
	break;
    case EXPRSXP:
	if(length(s) <= 0)
	    print2buff("expression()");
	else {
	    print2buff("expression(");
	    vec2buff(s);
	    print2buff(")");
	}
	break;
    case LISTSXP:
	attr1(s);
	print2buff("list(");
	inlist++;
	for (t=s ; CDR(t) != R_NilValue ; t=CDR(t) ) {
	    if( TAG(t) != R_NilValue ) {
		deparse2buff(TAG(t));
		print2buff(" = ");
	    }
	    deparse2buff(CAR(t));
	    print2buff(", ");
	}
	if( TAG(t) != R_NilValue ) {
	    deparse2buff(TAG(t));
	    print2buff(" = ");
	}
	deparse2buff(CAR(t));
	print2buff(")" );
	inlist--;
	attr2(s);
	break;
    case LANGSXP:
	printcomment(s);
	if (TYPEOF(CAR(s)) == SYMSXP) {
	    if ((TYPEOF(SYMVALUE(CAR(s))) == BUILTINSXP) ||
		(TYPEOF(SYMVALUE(CAR(s))) == SPECIALSXP)) {
		op = CAR(s);
		fop = PPINFO(SYMVALUE(op));
		s = CDR(s);
		if (fop == PP_BINARY) {
		    switch (length(s)) {
		    case 1:
			fop = PP_UNARY;
			break;
		    case 2:
			break;
		    default:
			fop = PP_FUNCALL;
			break;
		    }
		}
		else if (fop == PP_BINARY2) {
		    if (length(s) != 2)
			fop = PP_FUNCALL;
		}
		switch (fop) {
		case PP_IF:
		    print2buff("if (");
		    /* print the predicate */
		    deparse2buff(CAR(s));
		    print2buff(") ");
		    if (incurly && !inlist ) {
			lookahead = curlyahead(CAR(CDR(s)));
			if (!lookahead) {
			    writeline();
			    indent++;
			}
		    }
		    /* need to find out if there is an else */
		    if (length(s) > 2) {
			deparse2buff(CAR(CDR(s)));
			if (incurly && !inlist) {
			    writeline();
			    if (!lookahead)
				indent--;
			}
			else
			    print2buff(" ");
			print2buff("else ");
			deparse2buff(CAR(CDDR(s)));
		    }
		    else {
			deparse2buff(CAR(CDR(s)));
			if (incurly && !lookahead && !inlist )
			    indent--;
		    }
		    break;
		case PP_WHILE:
		    print2buff("while (");
		    deparse2buff(CAR(s));
		    print2buff(") ");
		    deparse2buff(CADR(s));
		    break;
		case PP_FOR:
		    print2buff("for (");
		    deparse2buff(CAR(s));
		    print2buff(" in ");
		    deparse2buff(CADR(s));
		    print2buff(") ");
		    deparse2buff(CADR(CDR(s)));
		    break;
		case PP_REPEAT:
		    print2buff("repeat ");
		    deparse2buff(CAR(s));
		    break;
		case PP_CURLY:
		    print2buff("{");
		    incurly += 1;
		    indent++;
		    writeline();
		    while (s != R_NilValue) {
			deparse2buff(CAR(s));
			writeline();
			s = CDR(s);
		    }
		    indent--;
		    print2buff("}");
		    incurly -= 1;
		    break;
		case PP_PAREN:
		    print2buff("(");
		    deparse2buff(CAR(s));
		    print2buff(")");
		    break;
		case PP_SUBSET:
		    deparse2buff(CAR(s));
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("[");
		    else
			print2buff("[[");
		    args2buff(CDR(s), 0, 0);
		    if (PRIMVAL(SYMVALUE(op)) == 1)
			print2buff("]");
		    else
			print2buff("]]");
		    break;
		case PP_FUNCALL:
		case PP_RETURN:
		    if (isValidName(CHAR(PRINTNAME(op))))
			print2buff(CHAR(PRINTNAME(op)));
		    else {
			print2buff("\"");
			print2buff(CHAR(PRINTNAME(op)));
			print2buff("\"");
		    }
		    print2buff("(");
		    inlist++;
		    args2buff(s, 0, 0);
		    inlist--;
		    print2buff(")");
		    break;
		case PP_FOREIGN:
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff("(");
		    inlist++;
		    args2buff(s, 1, 0);
		    inlist--;
		    print2buff(")");
		    break;
		case PP_FUNCTION:
		    printcomment(s);
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff("(");
		    args2buff(FORMALS(s), 0, 1);
		    print2buff(") ");
		    deparse2buff(CADR(s));
		    break;
		case PP_ASSIGN:
		case PP_ASSIGN2:
		    deparse2buff(CAR(s));
		    print2buff(" ");
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff(" ");
		    deparse2buff(CADR(s));
		    break;
		case PP_DOLLAR:
		    deparse2buff(CAR(s));
		    print2buff("$");
		    /*temp fix to handle printing of x$a's */
		    if( isString(CADR(s)) &&
			isValidName(CHAR(STRING(CADR(s))[0])))
			deparse2buff(STRING(CADR(s))[0]);
		    else
			deparse2buff(CADR(s));
		    break;
		case PP_BINARY:
		    deparse2buff(CAR(s));
		    print2buff(" ");
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff(" ");
		    linebreak(&lbreak);
		    deparse2buff(CADR(s));
		    if (lbreak) {
			indent--;
			lbreak = 0;
		    }
		    break;
		case PP_BINARY2:	/* no space between op and args */
		    deparse2buff(CAR(s));
		    print2buff(CHAR(PRINTNAME(op)));
		    deparse2buff(CADR(s));
		    break;
		case PP_UNARY:
		    print2buff(CHAR(PRINTNAME(op)));
		    deparse2buff(CAR(s));
		    break;
		case PP_BREAK:
		    print2buff("break");
		    break;
		case PP_NEXT:
		    print2buff("next");
		    break;
		case PP_SUBASS:
		    print2buff("\"");
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff("\"(");
		    args2buff(s, 0, 0);
		    print2buff(")");
		    break;
		default:
		    UNIMPLEMENTED("deparse2buff");
		}
	    }
	    else {
		if(isSymbol(CAR(s)) && isUserBinop(CAR(s))) {
		    op = CAR(s);
		    s = CDR(s);
		    deparse2buff(CAR(s));
		    print2buff(" ");
		    print2buff(CHAR(PRINTNAME(op)));
		    print2buff(" ");
		    linebreak(&lbreak);
		    deparse2buff(CADR(s));
		    if (lbreak) {
			indent--;
			lbreak = 0;
		    }
		    break;
		}
		else {
		    deparse2buff(CAR(s));
		    print2buff("(");
		    args2buff(CDR(s), 0, 0);
		    print2buff(")");
		}
	    }
	}
	else if (TYPEOF(CAR(s)) == CLOSXP || TYPEOF(CAR(s)) == SPECIALSXP
		 || TYPEOF(CAR(s)) == BUILTINSXP) {
	    deparse2buff(CAR(s));
	    print2buff("(");
	    args2buff(CDR(s), 0, 0);
	    print2buff(")");
	}
	else { /* we have a lambda expression */
	    deparse2buff(CAR(s));
	    print2buff("(");
	    args2buff(CDR(s), 0, 0);
	    print2buff(")");
	}
	break;
    case STRSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case CPLXSXP:
	attr1(s);
	vector2buff(s);
	attr2(s);
	break;
    default:
	UNIMPLEMENTED("deparse2buff");
    }
}


/* If there is a string array active point to that, and */
/* otherwise we are counting lines so don't do anything. */

static void writeline()
{
    if (strvec != R_NilValue)
	STRING(strvec)[linenumber] = mkChar(buff);
    linenumber++;
    /* reset */
    len = 0;
    buff[0] = '\0';
    startline = 0;
}

static void print2buff(char *strng)
{
    int tlen, bufflen;

    if (startline == 0) {
	startline = 1;
	printtab2buff(indent);	/*if at the start of a line tab over */
    }
    tlen = strlen(strng);
    AllocBuffer(0);
    bufflen = strlen(buff);
    /*if (bufflen + tlen > BUFSIZE) {
	buff[0] = '\0';
	error("string too long in deparse");
	}*/
    AllocBuffer(bufflen + tlen);
    strcat(buff, strng);
    len += tlen;
}

static void scalar2buff(SEXP inscalar)
{
    char *strp;
    strp = EncodeElement(inscalar, 0, '"');
    print2buff(strp);
}

static void vector2buff(SEXP vector)
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
	case LGLSXP: print2buff("logical(0)"); break;
	case INTSXP: print2buff("numeric(0)"); break;
	case REALSXP: print2buff("numeric(0)"); break;
	case CPLXSXP: print2buff("complex(0)"); break;
	case STRSXP: print2buff("character(0)"); break;
	}
    }
    else if (tlen == 1) {
	scalar2buff(vector);
    }
    else {
	print2buff("c(");
	for (i = 0; i < tlen; i++) {
	    strp = EncodeElement(vector, i, quote);
	    print2buff(strp);
	    if (i < (tlen - 1))
		print2buff(", ");
	    if (len > cutoff)
		writeline();
	}
	print2buff(")");
    }

}

/* vec2buff : New Code */
/* Deparse vectors of S-expressions. */
/* In particular, this deparses objects of mode expression. */

static void vec2buff(SEXP v)
{
    SEXP nv;
    int i, lbreak, n;

    lbreak = 0;
    n = length(v);
    nv = getAttrib(v, R_NamesSymbol);
    if (length(nv) == 0) nv = R_NilValue;

    for(i = 0 ; i < n ; i++) {
	if (i > 0)
	    print2buff(", ");
	linebreak(&lbreak);
	if (!isNull(nv) && !isNull(STRING(nv)[i])
	    && *CHAR(STRING(nv)[i])) {
	    if( isValidName(CHAR(STRING(nv)[i])) )
		deparse2buff(STRING(nv)[i]);
	    else {
		print2buff("\"");
		deparse2buff(STRING(nv)[i]);
		print2buff("\"");
	    }
	    print2buff(" = ");
	}
	deparse2buff(VECTOR(v)[i]);
    }
    if (lbreak)
	indent--;
}

static void args2buff(SEXP arglist, int lineb, int formals)
{
    int lbreak = 0;

    while (arglist != R_NilValue) {
	if (TAG(arglist) != R_NilValue) {
#if 0
	    deparse2buff(TAG(arglist));
#else
	    char tpb[120];
	    SEXP s = TAG(arglist);

	    if( s == R_DotsSymbol || isValidName(CHAR(PRINTNAME(s))) )
		print2buff(CHAR(PRINTNAME(s)));
	    else {
		if( strlen(CHAR(PRINTNAME(s)))< 117 ) {
		    sprintf(tpb,"\"%s\"",CHAR(PRINTNAME(s)));
		    print2buff(tpb);
		}
		else {
		    sprintf(tpb,"\"");
		    strncat(tpb, CHAR(PRINTNAME(s)), 117);
		    strcat(tpb, "\"");
		    print2buff(tpb);
		}
	    }
#endif
	    if(formals) {
		if (CAR(arglist) != R_MissingArg) {
		    print2buff(" = ");
		    deparse2buff(CAR(arglist));
		}
	    }
	    else {
		print2buff(" = ");
		if (CAR(arglist) != R_MissingArg) {
		    deparse2buff(CAR(arglist));
		}
	    }
	}
	else deparse2buff(CAR(arglist));
	arglist = CDR(arglist);
	if (arglist != R_NilValue) {
	    print2buff(", ");
	    linebreak(&lbreak);
	}
    }
    if (lbreak)
	indent--;
}

/* This code controls indentation.  Used to follow the S style, */
/* (print 4 tabs and then start printing spaces only) but I */
/* modified it to be closer to emacs style (RI). */

static void printtab2buff(int ntab)
{
    int i;

    for (i = 1; i <= ntab; i++)
	if (i <= 4)
	    print2buff("    ");
	else
	    print2buff("  ");
}
