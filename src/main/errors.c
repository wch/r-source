/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1997--2002  The R Development Core Team.
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

#include <Defn.h>
/* -> Errormsg.h */
#include <Startup.h> /* rather cleanup ..*/
#include <Rconnections.h>

#ifndef min
#define min(a, b) (a<b?a:b)
#endif

/* limit on call length at which errorcall/warningcall is split over
   two lines */
#define LONGCALL 30

/* This is now non-static so that other applications that link or load libR.so
   can override it by defining their own version and hence by-pass the longjmp
   back into the standard R event loop.
 */
void jump_now();

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

  ErrorMessage()-> errorcall   (but with message from ErrorDB[])

  WarningMessage()-> warningcall (but with message from WarningDB[]).
*/


void onintr()
{
    REprintf("\n");
    jump_to_toplevel();
}

/* SIGUSR1: save and quit
   SIGUSR2: save and quit, don't run .Last or on.exit().
*/

void onsigusr1()
{
    RCNTXT *c;

    inError = 1;

    if( R_CollectWarnings ) {
	inError = 2;
	REprintf("In addition: ");
	PrintWarnings();
	inError = 1;
    }


    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;

    /* Bail out if there is a CTXT_RESTART on the stack--do we really
       want this? */
    for (c = R_GlobalContext; c; c = c->nextcontext) {
	if (IS_RESTART_BIT_SET(c->callflag)) {
	    inError=0;
	    findcontext(CTXT_RESTART, c->cloenv, R_RestartToken);
	}
    }

    /* Run all onexit/cend code on the stack (without stopping at
       intervening CTXT_TOPLEVEL's.  Since intervening CTXT_TOPLEVEL's
       get used by what are conceptually concurrent computations, this
       is a bit like telling all active threads to terminate and clean
       up on the way out. */
    R_run_onexits(NULL);

    R_CleanUp(SA_SAVE, 2, 1); /* quit, save,  .Last, status=2 */
}


void onsigusr2()
{
    inError = 1;

    if( R_CollectWarnings ) {
	inError = 2;
	REprintf("In addition: ");
	PrintWarnings();
	inError = 1;
    }


    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_CleanUp(SA_SAVE, 0, 0);
}


static void setupwarnings(void)
{
    R_Warnings = allocVector(VECSXP, 50);
    setAttrib(R_Warnings, R_NamesSymbol, allocVector(STRSXP, 50));
}

/* Rvsnprintf: like vsnprintf, but guaranteed to null-terminate. */
static int Rvsnprintf(char *buf, size_t size, const char  *format, va_list ap)
{
    int val;
    val = vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    return val;
}

#define BUFSIZE 8192
void warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    warningcall(R_NilValue, buf);
}

/* temporary hook to allow experimenting with alternate warning mechanisms */
static void (*R_WarningHook)(SEXP, char *) = NULL;

void warningcall(SEXP call, const char *format, ...)
{
    int w;
    SEXP names, s;
    char *dcall, buf[BUFSIZE];
    RCNTXT *cptr;

    if (R_WarningHook != NULL) {
	va_list(ap);
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	R_WarningHook(call, buf);
	return;
    }

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

    if(w < 0 || inWarning || inError)  {/* ignore if w<0 or already in here*/
	return;
    }
    inWarning = 1;

    if(w >= 2) { /* make it an error */
	va_list(ap);
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	inWarning = 0; /* PR#1570 */
	errorcall(call, "(converted from warning) %s", buf);
    }
    else if(w == 1) {	/* print as they happen */
	va_list(ap);
	if( call != R_NilValue ) {
	    dcall = CHAR(STRING_ELT(deparse1(call, 0), 0));
	    REprintf("Warning in %s : ", dcall);
	    if (strlen(dcall) > LONGCALL) REprintf("\n	 ");
	}
	else
	    REprintf("Warning: ");
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	REprintf("%s\n", buf);
    }
    else if(w == 0) {	/* collect them */
	va_list(ap);
	va_start(ap, format);
	if(!R_CollectWarnings)
	    setupwarnings();
	if( R_CollectWarnings > 49 )
	    return;
	SET_VECTOR_ELT(R_Warnings, R_CollectWarnings, call);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	names = CAR(ATTRIB(R_Warnings));
	SET_STRING_ELT(names, R_CollectWarnings++, mkChar(buf));
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
	if( VECTOR_ELT(R_Warnings, 0) == R_NilValue )
	   REprintf("%s \n", CHAR(STRING_ELT(names, 0)));
	else
	   REprintf("%s in: %s \n", CHAR(STRING_ELT(names, 0)),
		CHAR(STRING_ELT(deparse1(VECTOR_ELT(R_Warnings, 0),0), 0)));
    }
    else if( R_CollectWarnings <= 10 ) {
	REprintf("Warning messages: \n");
	names = CAR(ATTRIB(R_Warnings));
	for(i=0; i<R_CollectWarnings; i++) {
	    if( STRING_ELT(R_Warnings, i) == R_NilValue )
	       REprintf("%d: %s \n",i+1, CHAR(STRING_ELT(names, i)));
	    else
	       REprintf("%d: %s in: %s \n", i+1, CHAR(STRING_ELT(names, i)),
		   CHAR(STRING_ELT(deparse1(VECTOR_ELT(R_Warnings,i), 0), 0)));
	}
    }
    else {
	if (R_CollectWarnings < 50)
	    REprintf("There were %d warnings (use warnings() to see them)\n",
		     R_CollectWarnings);
	else
	    REprintf("There were 50 or more warnings (use warnings() to see the first 50)\n");
    }
    /* now truncate and install last.warning */
    PROTECT(s = allocVector(VECSXP, R_CollectWarnings));
    PROTECT(t = allocVector(STRSXP, R_CollectWarnings));
    names = CAR(ATTRIB(R_Warnings));
    for(i=0; i<R_CollectWarnings; i++) {
	SET_VECTOR_ELT(s, i, VECTOR_ELT(R_Warnings, i));
	SET_VECTOR_ELT(t, i, VECTOR_ELT(names, i));
    }
    setAttrib(s, R_NamesSymbol, t);
    defineVar(install("last.warning"), s, R_GlobalEnv);
    UNPROTECT(2);
    inWarning = 0;
    R_CollectWarnings = 0;
    R_Warnings = R_NilValue;
    return;
}

static char errbuf[BUFSIZE];

/* temporary hook to allow experimenting with alternate error mechanisms */
static void (*R_ErrorHook)(SEXP, char *) = NULL;

void errorcall(SEXP call, const char *format,...)
{
    char *p, *dcall;

    va_list(ap);

    if (R_ErrorHook != NULL) {
	char buf[BUFSIZE];
	void (*hook)(SEXP, char *) = R_ErrorHook;
	R_ErrorHook = NULL; /* to avoid recursion */
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	hook(call, buf);
    }

    if (inError) {
	if(inError == 3) {
	     /* Can REprintf generate an error? If so we should guard for it */
	    REprintf("Error during wrapup: ");
	    /* this does NOT try to print the call since that could
               cause a cascade of error calls */
	    va_start(ap, format);
	    Rvsnprintf(errbuf, sizeof(errbuf), format, ap);
	    va_end(ap);
	    REprintf("%s\n", errbuf);
	}
	jump_now();
    }

    if(call != R_NilValue) {
	char *head = "Error in ";
	char *mid = " : ";
	char *tail = "\n\t";/* <- TAB */
	int len = strlen(head) + strlen(mid) + strlen(tail);

	inError = 1;
	dcall = CHAR(STRING_ELT(deparse1(call, 0), 0));
	if (strlen(dcall) + len < BUFSIZE) {
	    sprintf(errbuf, "%s%s%s", head, dcall, mid);
	    if (strlen(dcall) > LONGCALL) strcat(errbuf, tail);
	}
	else
	    sprintf(errbuf, "Error: ");
    }
    else
	sprintf(errbuf, "Error: ");

    p = errbuf + strlen(errbuf);
    va_start(ap, format);
    Rvsnprintf(p, min(BUFSIZE, R_WarnLength) - strlen(errbuf), format, ap);
    va_end(ap);
    p = errbuf + strlen(errbuf) - 1;
    if(*p != '\n') strcat(errbuf, "\n");
    if (R_ShowErrorMessages) REprintf("%s", errbuf);
    jump_to_toplevel();
}

SEXP do_geterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP res;

    checkArity(op, args);
    PROTECT(res = allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, mkChar(errbuf));
    UNPROTECT(1);
    return res;
}

void error(const char *format, ...)
{
    char buf[BUFSIZE];

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    /* This can be called before R_GlobalContext is defined, so... */
    errorcall(R_GlobalContext ?
	      R_GlobalContext->call : R_NilValue, "%s", buf);
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the innermost TOPLEVEL context */

void jump_to_toplevel()
{
    RCNTXT *c;
    SEXP s, t;
    int haveHandler;
    int nback = 0;

    inError = 1;

    if( R_ShowErrorMessages && R_CollectWarnings ) {
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
	    inError = 3;
	    if (isLanguage(s))
		eval(s, R_GlobalEnv);
	    else /* expression */
	    {
		int i, n = LENGTH(s);
		for (i = 0 ; i < n ; i++)
		    eval(VECTOR_ELT(s, i), R_GlobalEnv);
	    }
	    inError = 1;
	}
    }

    /* reset some stuff--not sure (all) this belongs here */
    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;

    /* find the jump target; do the jump if target is a CTXT_RESTART */
    for (c = R_GlobalContext; c; c = c->nextcontext) {
	if (c->callflag & CTXT_FUNCTION )
	    nback++;
	if (IS_RESTART_BIT_SET(c->callflag)) {
	    inError=0;
	    findcontext(CTXT_RESTART, c->cloenv, R_RestartToken);
	}
	if (c->callflag == CTXT_TOPLEVEL)
	    break;
    }

    /* run onexit/cend code for all contexts down to but not including
       the jump target */
    R_run_onexits(c);

    if ( !R_Interactive && !haveHandler && inError ) {
	REprintf("Execution halted\n");
	R_CleanUp(SA_NOSAVE, 1, 0); /* quit, no save, no .Last, status=1 */
    }

    PROTECT(s = allocList(nback));
    t = s;
    for (c = R_GlobalContext ;
	 c != NULL && c->callflag != CTXT_TOPLEVEL;
	 c = c->nextcontext)
	if (c->callflag & CTXT_FUNCTION ) {
	    SETCAR(t, deparse1(c->call, 0));
	    t = CDR(t);
	}
    setVar(install(".Traceback"), s, R_GlobalEnv);
    UNPROTECT(1);
    jump_now();
}

/*
   Absolutely no allocation can be triggered in jump_now except
   whatever happens in R_run_onexits.  The error could be an out of
   memory error and any allocation could result in an infinite-loop
   condition. All you can do is reset things and exit.  */
void jump_now()
{
    RCNTXT *c;

    /* find the jump target; do the jump if target is a CTXT_RESTART */
    for (c = R_GlobalContext; c; c = c->nextcontext) {
	if (IS_RESTART_BIT_SET(c->callflag)) {
	    inError=0;
	    findcontext(CTXT_RESTART, c->cloenv, R_RestartToken);
	}
	if (c->callflag == CTXT_TOPLEVEL)
	    break;
    }
    /* at this point we should have c == R_TopLevelContext */

    /* Run onexit/cend code for all contexts down to but not including
       the jump target.  Ordinarily this will already have been done,
       but it may not have been completed in a recursive error
       situation.  This may cause recursive calls to jump_now, but the
       possible number of such recursive calls is limited since each
       exit function is removed before it its executed.  This is not a
       great design because we could run out of other resources that
       are on the stack (like C stack for example).  The right thing
       to do is arrange execute exit *after* the LONGJMP, but that
       requires a more extensive redesign of the non-local transfer of
       control mechanism.  LT. */
    R_run_onexits(R_ToplevelContext);

    if( inError == 2 )
	REprintf("Lost warning messages\n");
    inError=0;
    inWarning=0;
    R_Warnings = R_NilValue;
    R_CollectWarnings = 0;

    R_GlobalContext = R_ToplevelContext;
    R_restore_globals(R_GlobalContext);

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

SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* error(.) : really doesn't return anything; but all do_foo() must be SEXP */
    RCNTXT *cptr;
    SEXP c_call;

    if(asLogical(CAR(args))) {/* find context -> "Error in ..:" */
	cptr = R_GlobalContext->nextcontext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->nextcontext != NULL)
	    cptr = cptr->nextcontext;
	c_call = cptr->call;
    }
    else
	c_call = R_NilValue;

    args = CDR(args);

    if (CAR(args) != R_NilValue) { /* message */
      SETCAR(args, coerceVector(CAR(args), STRSXP));
      if(!isValidString(CAR(args)))
	  errorcall(c_call, " [invalid string in stop(.)]");
      errorcall(c_call, "%s", CHAR(STRING_ELT(CAR(args), 0)));
    }
    else
      errorcall(c_call, "");
    /* never called: */return c_call;
}

SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    SEXP c_call;

    if(asLogical(CAR(args))) {/* find context -> "... in: ..:" */
	cptr = R_GlobalContext->nextcontext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && 
		cptr->nextcontext != NULL)
	    cptr = cptr->nextcontext;
	c_call = cptr->call;
    } else
	c_call = R_NilValue;

    args = CDR(args);
    if (CAR(args) != R_NilValue) {
	SETCAR(args, coerceVector(CAR(args), STRSXP));
	if(!isValidString(CAR(args)))
	    warningcall(c_call, " [invalid string in warning(.)]");
	else
	    warningcall(c_call, "%s", CHAR(STRING_ELT(CAR(args), 0)));
    }
    else
	warningcall(c_call, "");
    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
void WrongArgCount(const char *s)
{
    error("incorrect number of arguments to \"%s\"", s);
}


void UNIMPLEMENTED(const char *s)
{
    error("Unimplemented feature in %s", s);
}

/* ERROR_.. codes in Errormsg.h */
static struct {
    const R_WARNING code;
    const char* const format;
}
const ErrorDB[] = {
    { ERROR_NUMARGS,		"invalid number of arguments"		},
    { ERROR_ARGTYPE,		"invalid argument type"			},

    { ERROR_TSVEC_MISMATCH,	"time-series/vector length mismatch"	},
    { ERROR_INCOMPAT_ARGS,	"incompatible arguments"		},

    { ERROR_UNIMPLEMENTED,	"unimplemented feature in %s"		},
    { ERROR_UNKNOWN,		"unknown error (report this!)"		}
};

static struct {
    R_WARNING code;
    char* format;
}
WarningDB[] = {
    { WARNING_coerce_NA,	"NAs introduced by coercion"		},
    { WARNING_coerce_INACC,	"inaccurate integer conversion in coercion" },
    { WARNING_coerce_IMAG,	"imaginary parts discarded in coercion" },

    { WARNING_UNKNOWN,		"unknown warning (report this!)"	},
};


void ErrorMessage(SEXP call, int which_error, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list(ap);

    i = 0;
    while(ErrorDB[i].code != ERROR_UNKNOWN) {
	if (ErrorDB[i].code == which_error)
	    break;
	i++;
    }

    va_start(ap, which_error);
    Rvsnprintf(buf, BUFSIZE, ErrorDB[i].format, ap);
    va_end(ap);
    errorcall(call, "%s", buf);
}

void WarningMessage(SEXP call, R_WARNING which_warn, ...)
{
    int i;
    char buf[BUFSIZE];
    va_list(ap);

    i = 0;
    while(WarningDB[i].code != WARNING_UNKNOWN) {
	if (WarningDB[i].code == which_warn)
	    break;
	i++;
    }

    va_start(ap, which_warn);
    Rvsnprintf(buf, BUFSIZE, WarningDB[i].format, ap);
    va_end(ap);
    warningcall(call, "%s", buf);
}


/* Temporary hooks to allow experimenting with alternate error and
   warning mechanisms.  They are not in the header files for now, but
   the following snippet can serve as a header file: */

void R_ReturnOrRestart(SEXP val, SEXP env, Rboolean restart);
void R_PrintDeferredWarnings(void);
SEXP R_GetTraceback(int);
void R_SetErrmessage(char *s);
void R_SetErrorHook(void (*hook)(SEXP, char *));
void R_SetWarningHook(void (*hook)(SEXP, char *));
void R_JumpToToplevel(Rboolean restart);


void R_SetWarningHook(void (*hook)(SEXP, char *))
{
    R_WarningHook = hook;
}

void R_SetErrorHook(void (*hook)(SEXP, char *))
{
    R_ErrorHook = hook;
}

void R_ReturnOrRestart(SEXP val, SEXP env, Rboolean restart)
{
    int mask;
    RCNTXT *c;

    mask = CTXT_BROWSER | CTXT_FUNCTION;

    for (c = R_GlobalContext; c; c = c->nextcontext) {
	if (c->callflag & mask && c->cloenv == env)
	    findcontext(mask, env, val);
	else if (restart && IS_RESTART_BIT_SET(c->callflag))
	    findcontext(CTXT_RESTART, c->cloenv, R_RestartToken);
	else if (c->callflag == CTXT_TOPLEVEL)
	    error("No function to return from, jumping to top level");
    }
}

void R_JumpToToplevel(Rboolean restart)
{
    RCNTXT *c;

    /* Find the target for the jump */
    for (c = R_GlobalContext; c != NULL; c = c->nextcontext) {
	if (restart && IS_RESTART_BIT_SET(c->callflag))
	    findcontext(CTXT_RESTART, c->cloenv, R_RestartToken);
	else if (c->callflag == CTXT_TOPLEVEL)
	    break;
    }
    if (c != R_ToplevelContext)
	warning("top level inconsistency?");

    /* Run onexit/cend code for everything above the target. */
    R_run_onexits(c);

    R_ToplevelContext = R_GlobalContext = c;
    R_restore_globals(R_GlobalContext);
    LONGJMP(c->cjmpbuf, CTXT_TOPLEVEL);
}

void R_SetErrmessage(char *s)
{
    strncpy(errbuf, s, sizeof(errbuf));
    errbuf[sizeof(errbuf) - 1] = 0;
}

void R_PrintDeferredWarnings(void)
{
    if( R_ShowErrorMessages && R_CollectWarnings ) {
        REprintf("In addition: ");
        PrintWarnings();
    }
}    

/* doesn't stop at TOPLEVEL--should once browser is changed to use RESTART */
SEXP R_GetTraceback(int skip)
{
    int nback = 0, ns;
    RCNTXT *c;
    SEXP s, t;

    for (c = R_GlobalContext, ns = skip;
	 c != NULL && c->callflag != CTXT_TOPLEVEL;
	 c = c->nextcontext)
	if (c->callflag & CTXT_FUNCTION ) {
	    if (ns > 0)
		ns--;
	    else
		nback++;
	}

    PROTECT(s = allocList(nback));
    t = s;
    for (c = R_GlobalContext ;
	 c != NULL && c->callflag != CTXT_TOPLEVEL;
	 c = c->nextcontext)
	if (c->callflag & CTXT_FUNCTION ) {
	    if (skip > 0)
		skip--;
	    else {
		SETCAR(t, deparse1(c->call, 0));
		t = CDR(t);
	    }
	}
    UNPROTECT(1);
    return s;
}

