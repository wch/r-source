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

void jump_to_toplevel();
static void jump_now();

static int inError = 0;
static int inWarning = 0;

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
            error("invalid option \"warning.expression\"\n"); 
        cptr = R_GlobalContext;
        while (cptr->callflag != CTXT_RETURN && cptr->callflag )
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

    if(w==1) { /* print as they happen */
        va_list(ap);
        if( call != R_NilValue ) {
            dcall = CHAR(STRING(deparse1(call, 0))[0]);
            REprintf("Warning in %s : ", dcall);
        }
        else
            REprintf("Warning: ");
        va_start(ap, format);
        REvprintf(format, ap);
        va_end(ap);
        REprintf("\n");
    }
    if(w==0) {  /* collect them */
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
	       REprintf("%d: %s in: %s \n",i+1, CHAR(STRING(names)[i]),
		   CHAR(STRING(deparse1(VECTOR(R_Warnings)[i], 0))[0]));
	}
    }
    else {
	REprintf("There were %d warnings (use warnings() to see them)\n", R_CollectWarnings);
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
    R_CollectWarnings=0;
    R_Warnings=R_NilValue;
    return;
}


void errorcall(SEXP call, char *format,...)
{
    char buf[BUFSIZE], *p;
    va_list(ap);
    char *dcall;
    if (inError )
	jump_now();
#ifdef FOO
    RCNTXT *cptr;
    cptr = R_GlobalContext;
    while (cptr->callflag != CTXT_RETURN && cptr->nextcontext != NULL)
        cptr = cptr->nextcontext;
    if( cptr->callflag == CTXT_RETURN )
        do_browser(cptr->call, R_NilValue, R_NilValue, cptr->cloenv);
#endif
    if(call != R_NilValue ) {
        dcall = CHAR(STRING(deparse1(call, 0))[0]);
        sprintf(buf, "Error in %s : ", dcall);
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
#ifdef NEWERROR
    char *dcall;
#endif
    va_list(ap);
    if (inError)
	jump_now();
#ifdef NEWERROR
    if (R_GlobalContext->call == R_NilValue)
	dcall = CHAR(STRING(deparse1(R_CurrentExpr, 0))[0]);
    else
	dcall = CHAR(STRING(deparse1(R_GlobalContext->call, 0))[0]);
    sprintf(buf, "Error in %s : ", dcall);
#else
    sprintf(buf, "Error: ");
#endif
    p = buf + strlen(buf);
    va_start(ap, format);
    vsprintf(p, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(*p != '\n') strcat(buf, "\n");
    REprintf("%s", buf);
    jump_to_toplevel();
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the top repl loop */

void jump_to_toplevel()
{
    RCNTXT *c;
    SEXP s, t;
    int nback = 0;
    inError = 1;
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
	if (c->callflag == CTXT_RETURN)
	    nback++;
	if (c->callflag == CTXT_RESTART) {
		inError=0;
		findcontext(CTXT_RESTART, c->cloenv, R_DollarSymbol);
	}
    }
    if (R_Sinkfile) R_Outputfile = R_Sinkfile;
    else R_Outputfile = R_Consolefile;
    PROTECT(s = allocList(nback));
    t = s;
    for (c = R_GlobalContext ; c ; c = c->nextcontext)
	if (c->callflag == CTXT_RETURN) {
	    CAR(t) = deparse1(c->call, 0);
	    t = CDR(t);
	}
    setVar(install(".Traceback"), s, R_GlobalEnv);
    UNPROTECT(1);
    jump_now();
}

static void jump_now()
{
    inError=0;
    inWarning=0;
    R_PPStackTop = 0;
    /* I think the following is needed somewhere near here: BDR
       if (R_CollectWarnings) PrintWarnings();*/
    R_Warnings = R_NilValue;
    R_CollectWarnings = 0;
    if (R_Interactive) LONGJMP(R_ToplevelContext->cjmpbuf, 0);
    else REprintf("Execution halted\n");
    exit(1);
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
    CAR(args) = coerceVector(CAR(args), STRSXP);
    if (length(CAR(args)) <= 0)
	error("\n");
    else
	error("%s\n", CHAR(STRING(CAR(args))[0]));
    /*NOTREACHED*/
}

SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;

    cptr = R_GlobalContext->nextcontext;
    while (cptr->callflag != CTXT_RETURN && cptr->nextcontext != NULL)
	cptr = cptr->nextcontext;
    if (CAR(args) != R_NilValue) {
	CAR(args) = coerceVector(CAR(args), STRSXP);
	warningcall(cptr->call,"%s", CHAR(STRING(CAR(args))[0]));
    }
    else
	warningcall(cptr->call,"%s", "");
    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
void WrongArgCount(char *s)
{
    error("incorrect number of arguments to \"%s\"\n", s);
}


void  UNIMPLEMENTED(char *s)
{
    error("Unimplemented feature in %s\n", s);
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
