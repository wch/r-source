/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2000  The R Development Core Team.
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

#include "Defn.h"
/* limit on call length at which errorcall/warningcall is split over
   two lines */
#define LONGCALL 30

static void jump_now();

/*
Different values of inError are used to indicate different places
in the error handling.
*/
static int inError = 0;
static int inWarning = 0;

/* Interface / Calling Hierarchy :

  R__stop()   -> do_error ->   errorcall --> jump_to_toplevel --> jump_now
			 /
		    error

  R__warning()-> do_warning   -> warningcall -> if(warn >= 2) errorcall
			     /
		    warning /

  ErrorMessage	 : similar to errorcall()   but with message from ErrorDB[]

  WarningMessage : similar to warningcall() but with message from WarningDB[].
*/


void onintr()
{
    REprintf("\n");
    jump_to_toplevel();
}

static void setupwarnings(void)
{
    R_Warnings = allocVector(VECSXP, 50);
    setAttrib(R_Warnings, R_NamesSymbol, allocVector(STRSXP, 50));
}

#define BUFSIZE 8192
void warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list(ap);
    va_start(ap, format);
    vsprintf(buf, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    warningcall(R_NilValue, buf);
}

void warningcall(SEXP call, char *format, ...)
{
    int w, slen;
    SEXP names, s;
    char *dcall, buf[BUFSIZE];
    RCNTXT *cptr;

    s = GetOption(install("warning.expression"), R_NilValue);
    if( s!= R_NilValue ) {
	if( !isLanguage(s) &&  ! isExpression(s) )
	    error("invalid option \"warning.expression\"");
	cptr = R_GlobalContext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->callflag )
	    cptr = cptr->nextcontext;
	eval(s, cptr->cloenv);
	return;
    }

    w = asInteger(GetOption(install("warn"), R_NilValue));

    if( w == NA_INTEGER ) /* set to a sensible value */
	w = 0;

    if(w<0 || inWarning || inError)  {/* ignore if w<0 or already in here*/
	return;
    }
    inWarning = 1;

    if(w>=2) { /* make it an error */
	va_list(ap);
	va_start(ap, format);
	slen = vsprintf(buf, format, ap);
	va_end(ap);
	errorcall(call, "(converted from warning) %s", buf);
    }
    else if(w==1) {	/* print as they happen */
	va_list(ap);
	if( call != R_NilValue ) {
	    dcall = CHAR(STRING(deparse1(call, 0))[0]);
	    REprintf("Warning in %s : ", dcall);
	    if (strlen(dcall) > LONGCALL) REprintf("\n	 ");
	}
	else
	    REprintf("Warning: ");
	va_start(ap, format);
	REvprintf(format, ap);
	va_end(ap);
	REprintf("\n");
    }
    else if(w==0) {	/* collect them */
	va_list(ap);
	va_start(ap, format);
	if(!R_CollectWarnings)
	    setupwarnings();
	if( R_CollectWarnings > 49 )
	    return;
	VECTOR(R_Warnings)[R_CollectWarnings] = call;
	slen = vsprintf(buf, format, ap);
	va_end(ap);
	names = CAR(ATTRIB(R_Warnings));
	STRING(names)[R_CollectWarnings++] = mkChar(buf);
    }
    /* else:  w <= -1 */
    inWarning = 0;
}

void PrintWarnings(void)
{
    int i;
    SEXP names, s, t;

    inWarning = 1;
    if( R_CollectWarnings == 1 ) {
	REprintf("Warning message: \n");
	names = CAR(ATTRIB(R_Warnings));
	if( VECTOR(R_Warnings)[0] == R_NilValue )
	   REprintf("%s \n", CHAR(STRING(names)[0]));
	else
	   REprintf("%s in: %s \n", CHAR(STRING(names)[0]),
		CHAR(STRING(deparse1(VECTOR(R_Warnings)[0],0))[0]));
    }
    else if( R_CollectWarnings <= 10 ) {
	REprintf("Warning messages: \n");
	names = CAR(ATTRIB(R_Warnings));
	for(i=0; i<R_CollectWarnings; i++) {
	    if( STRING(R_Warnings)[i] == R_NilValue )
	       REprintf("%d: %s \n",i+1, CHAR(STRING(names)[i]));
	    else
	       REprintf("%d: %s in: %s \n", i+1, CHAR(STRING(names)[i]),
		   CHAR(STRING(deparse1(VECTOR(R_Warnings)[i], 0))[0]));
	}
    }
    else {
	REprintf("There were %d warnings (use warnings() to see them)\n",
		 R_CollectWarnings);
    }
    /* now truncate and install last.warning */
    PROTECT(s = allocVector(VECSXP, R_CollectWarnings));
    PROTECT(t = allocVector(STRSXP, R_CollectWarnings));
    names = CAR(ATTRIB(R_Warnings));
    for(i=0; i<R_CollectWarnings; i++) {
	VECTOR(s)[i] = VECTOR(R_Warnings)[i];
	VECTOR(t)[i] = VECTOR(names)[i];
    }
    setAttrib(s, R_NamesSymbol, t);
    defineVar(install("last.warning"), s, R_GlobalEnv);
    UNPROTECT(2);
    inWarning = 0;
    R_CollectWarnings = 0;
    R_Warnings = R_NilValue;
    return;
}


void errorcall(SEXP call, char *format,...)
{
    char buf[BUFSIZE], *p, *dcall;

    va_list(ap);

    if ( inError ) {
	if( inError == 3 )
	    REprintf("Error during wrapup \n");
	jump_now();
    }

    if(call != R_NilValue ) {
	dcall = CHAR(STRING(deparse1(call, 0))[0]);
	sprintf(buf, "Error in %s : ", dcall);
	if (strlen(dcall) > LONGCALL) strcat(buf, "\n	");
    }
    else
	sprintf(buf, "Error: ");

    p = buf + strlen(buf);
    va_start(ap, format);
    vsprintf(p, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(*p != '\n') strcat(buf, "\n");
    REprintf("%s", buf);
    jump_to_toplevel();
}

void error(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list(ap);
    va_start(ap, format);
    vsprintf(buf, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(*p != '\n') strcat(buf, "\n");
    errorcall(R_GlobalContext->call, buf);
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the top repl loop */

void jump_to_toplevel()
{
    RCNTXT *c;
    SEXP s, t;
    int haveHandler;
    int nback = 0;

    inError = 1;

    if( R_CollectWarnings ) {
	inError = 2;
	REprintf("In addition: ");
	PrintWarnings();
	inError = 1;
    }

    /*now see if options("error") is set */
    s = GetOption(install("error"), R_NilValue);
    haveHandler = ( s != R_NilValue );
    if (haveHandler) {
	if( !isLanguage(s) &&  ! isExpression(s) )  /* shouldn't happen */
	    REprintf("invalid option \"error\"\n");
	else {
	    c = R_GlobalContext;
	    while ( !(c->callflag & CTXT_FUNCTION) && c->callflag )
		c = c->nextcontext;
	    inError = 3;
	    if (isLanguage(s))
		eval(s, c->cloenv);
	    else /* expression */
	    {
		int i, n = LENGTH(s);
		for (i = 0 ; i < n ; i++)
		    eval(VECTOR(s)[i], c->cloenv);
	    }
	    inError = 1;
	}
    }

    if (R_Inputfile != NULL)
	fclose(R_Inputfile);
    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    vmaxset(NULL);
    if (R_GlobalContext->cend != NULL)
	(R_GlobalContext->cend) ();
    for (c = R_GlobalContext; c; c = c->nextcontext) {
	if (c->cloenv != R_NilValue && c->conexit != R_NilValue)
	    eval(c->conexit, c->cloenv);
	if (c->callflag == CTXT_RETURN || c->callflag == CTXT_GENERIC )
	    nback++;
	if (c->callflag == CTXT_RESTART) {
		inError=0;
		findcontext(CTXT_RESTART, c->cloenv, R_DollarSymbol);
	}
    }
    if ( !R_Interactive && !haveHandler && inError ) {
	REprintf("Execution halted\n");
	R_CleanUp(SA_NOSAVE, 1, 0); /* quit, no save, no .Last, status=1 */
    }
    if (R_Sinkfile) R_Outputfile = R_Sinkfile;
    else R_Outputfile = R_Consolefile;
    PROTECT(s = allocList(nback));
    t = s;
    for (c = R_GlobalContext ; c ; c = c->nextcontext)
	if (c->callflag & CTXT_FUNCTION ) {
	    CAR(t) = deparse1(c->call, 0);
	    t = CDR(t);
	}
    setVar(install(".Traceback"), s, R_GlobalEnv);
    UNPROTECT(1);
    jump_now();
}

/*
   Absolutely no allocation can be triggered in jump_now.
   The error could be an out of memory error and any allocation
   could result in an infinite-loop condition. All you can do
   is reset things and exit.
*/
static void jump_now()
{
    if( inError == 2 )
	REprintf("Lost warning messages\n");
    inError=0;
    inWarning=0;
    R_PPStackTop = 0;
    R_Warnings = R_NilValue;
    R_CollectWarnings = 0;
    LONGJMP(R_ToplevelContext->cjmpbuf, 0);
}

#ifdef OLD_Macintosh

#include <signal.h>
#include <errno.h>

void isintrpt()
{
    register EvQElPtr q;

    for (q = (EvQElPtr) GetEvQHdr()->qHead; q; q = (EvQElPtr) q->qLink)
	if (q->evtQWhat == keyDown && (char) q->evtQMessage == '.')
	    if (q->evtQModifiers & cmdKey) {
		FlushEvents(keyDownMask, 0);
		raise(SIGINT);
				/* errno = EINTR; */
		return;
	    }
}
#endif

void do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;

    cptr = R_GlobalContext->nextcontext;
    while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext != NULL)
	cptr = cptr->nextcontext;

    if (CAR(args) != R_NilValue) {
      CAR(args) = coerceVector(CAR(args), STRSXP);
      if(!isValidString(CAR(args)))
	  errorcall(cptr->call, " [invalid string in stop(.)]");
      errorcall(cptr->call, "%s", CHAR(STRING(CAR(args))[0]));
    }
    else
      errorcall(cptr->call, "");
    /*NOTREACHED*/
}

SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;

    cptr = R_GlobalContext->nextcontext;
    while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext != NULL)
	cptr = cptr->nextcontext;
    if (CAR(args) != R_NilValue) {
	CAR(args) = coerceVector(CAR(args), STRSXP);
	if(!isValidString(CAR(args)))
	    warningcall(cptr->call, " [invalid string in warning(.)]");
	else
	    warningcall(cptr->call,"%s", CHAR(STRING(CAR(args))[0]));
    }
    else
	warningcall(cptr->call,"");
    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
void WrongArgCount(char *s)
{
    error("incorrect number of arguments to \"%s\"", s);
}


void  UNIMPLEMENTED(char *s)
{
    error("Unimplemented feature in %s", s);
}

static struct {
    int	index;
    char*	format;
}
ErrorDB[] = {
    { ERROR_NUMARGS,		"invalid number of arguments\n"	      },
    { ERROR_ARGTYPE,		"invalid argument type\n"	      },

    { ERROR_TSVEC_MISMATCH,	"time-series/vector length mismatch\n"},
    { ERROR_INCOMPAT_ARGS,	"incompatible arguments\n"	      },

    { ERROR_UNIMPLEMENTED,	"unimplemented feature in %s\n",      },
    { ERROR_UNKNOWN,		"unknown error (report this!)\n",     }
};

void  ErrorMessage(SEXP call, int which_error, ...)
{
    int i;
    va_list(ap);
    char *dcall;
    if (inError)
	jump_now();
    if (call != R_NilValue) {
	dcall = CHAR(STRING(deparse1(call, 0))[0]);
	REprintf("Error in %s : ", dcall);
	if (strlen(dcall) > LONGCALL) REprintf("\n   ");
    }
    else
	REprintf("Error: ");	/* -- dcall = ??? */
    i = 0;
    while(ErrorDB[i].index != ERROR_UNKNOWN) {
	if (ErrorDB[i].index == which_error)
	    break;
	i++;
    }
    va_start(ap, which_error);
    REvprintf(ErrorDB[i].format, ap);
    va_end(ap);
    jump_to_toplevel();
}

static struct {
    int	    index;
    char*   format;
}
WarningDB[] = {
    { WARNING_UNKNOWN,		"unknown warning (report this!)\n", }
};

void  WarningMessage(SEXP call, int which_warn, ...)
{
    int i;
    va_list(ap);
    char *dcall;
    if (inError)
	jump_now();
    if (call != R_NilValue) {
	dcall = CHAR(STRING(deparse1(call, 0))[0]);
	REprintf("Warning in %s : ", dcall);
    }
    else
	REprintf("Warning: ");	/* -- dcall = ??? */
    i = 0;
    while(WarningDB[i].index != WARNING_UNKNOWN) {
	if (WarningDB[i].index == which_warn)
	    break;
	i++;
    }
    va_start(ap, which_warn);
    REvprintf(WarningDB[i].format, ap);
    va_end(ap);
}
