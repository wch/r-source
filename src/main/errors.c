/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2015  The R Core Team.
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

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>
/* -> Errormsg.h */
#include <Startup.h> /* rather cleanup ..*/
#include <Rconnections.h>
#include <Rinterface.h>
#include <R_ext/GraphicsEngine.h> /* for GEonExit */
#include <Rmath.h> /* for imax2 */
#include <R_ext/Print.h>

#ifndef min
#define min(a, b) (a<b?a:b)
#endif

/* Total line length, in chars, before splitting in warnings/errors */
#define LONGWARN 75

/*
Different values of inError are used to indicate different places
in the error handling.
*/
static int inError = 0;
static int inWarning = 0;
static int inPrintWarnings = 0;
static int immediateWarning = 0;
static int noBreakWarning = 0;

static void try_jump_to_restart(void);
// The next is crucial to the use of NORET attributes.
static void NORET
jump_to_top_ex(Rboolean, Rboolean, Rboolean, Rboolean, Rboolean);
static void signalInterrupt(void);
static char * R_ConciseTraceback(SEXP call, int skip);

/* Interface / Calling Hierarchy :

  R__stop()   -> do_error ->   errorcall --> (eventually) jump_to_top_ex
			 /
		    error

  R__warning()-> do_warning   -> warningcall -> if(warn >= 2) errorcall
			     /
		    warning /

  ErrorMessage()-> errorcall   (but with message from ErrorDB[])

  WarningMessage()-> warningcall (but with message from WarningDB[]).
*/

static uintptr_t R_OldCStackLimit = 0;

void NORET R_SignalCStackOverflow(intptr_t usage)
{
    /* We do need some stack space to process error recovery, so
       temporarily raise the limit.  We have 5% head room because we
       reduced R_CStackLimit to 95% of the initial value in
       setup_Rmainloop.
    */
    if (R_OldCStackLimit == 0) {
	R_OldCStackLimit = R_CStackLimit;
	R_CStackLimit = (uintptr_t) (R_CStackLimit / 0.95);
    }

    errorcall(R_NilValue, "C stack usage  %ld is too close to the limit",
	      usage);
    /* Do not translate this, to save stack space */
}

void (R_CheckStack)(void)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    /* printf("usage %ld\n", usage); */
    if(R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))
	R_SignalCStackOverflow(usage);
}

void R_CheckStack2(size_t extra)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    /* do it this way, as some compilers do usage + extra
       in unsigned arithmetic */
    usage += extra;
    if(R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))
	R_SignalCStackOverflow(usage);

}

void R_CheckUserInterrupt(void)
{
    R_CheckStack();

    /* Don't do any processing of interrupts, timing limits, or other
       asynchronous events if interrupts are suspended. */
    if (R_interrupts_suspended) return;

    /* This is the point where GUI systems need to do enough event
       processing to determine whether there is a user interrupt event
       pending.  Need to be careful not to do too much event
       processing though: if event handlers written in R are allowed
       to run at this point then we end up with concurrent R
       evaluations and that can cause problems until we have proper
       concurrency support. LT */

    R_ProcessEvents(); /* Also processes timing limits */
#ifndef Win32
    if (R_interrupts_pending) onintr();
#endif
}

void onintr()
{
    if (R_interrupts_suspended) {
	R_interrupts_pending = 1;
	return;
    }
    else R_interrupts_pending = 0;
    signalInterrupt();

    REprintf("\n");
    /* Attempt to run user error option, save a traceback, show
       warnings, and reset console; also stop at restart (try/browser)
       frames.  Not clear this is what we really want, but this
       preserves current behavior */
    jump_to_top_ex(TRUE, TRUE, TRUE, TRUE, FALSE);
}

/* SIGUSR1: save and quit
   SIGUSR2: save and quit, don't run .Last or on.exit().

   These do far more processing than is allowed in a signal handler ....
*/

RETSIGTYPE attribute_hidden onsigusr1(int dummy)
{
    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR1, onsigusr1);
	return;
    }

    inError = 1;

    if(R_CollectWarnings) PrintWarnings();

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = NULL;
    R_ParseErrorMsg[0] = '\0';

    /* Bail out if there is a browser/try on the stack--do we really
       want this?  No, as from R 2.4.0
    try_jump_to_restart(); */

    /* Run all onexit/cend code on the stack (without stopping at
       intervening CTXT_TOPLEVEL's.  Since intervening CTXT_TOPLEVEL's
       get used by what are conceptually concurrent computations, this
       is a bit like telling all active threads to terminate and clean
       up on the way out. */
    R_run_onexits(NULL);

    R_CleanUp(SA_SAVE, 2, 1); /* quit, save,  .Last, status=2 */
}


RETSIGTYPE attribute_hidden onsigusr2(int dummy)
{
    inError = 1;

    if (R_interrupts_suspended) {
	/**** ought to save signal and handle after suspend */
	REprintf(_("interrupts suspended; signal ignored"));
	signal(SIGUSR2, onsigusr2);
	return;
    }

    if(R_CollectWarnings) PrintWarnings();

    R_ResetConsole();
    R_FlushConsole();
    R_ClearerrConsole();
    R_ParseError = 0;
    R_ParseErrorFile = NULL;
    R_ParseErrorMsg[0] = '\0';
    R_CleanUp(SA_SAVE, 0, 0);
}


static void setupwarnings(void)
{
    R_Warnings = allocVector(VECSXP, R_nwarnings);
    setAttrib(R_Warnings, R_NamesSymbol, allocVector(STRSXP, R_nwarnings));
}

/* Rvsnprintf: like vsnprintf, but guaranteed to null-terminate. */
#ifdef Win32
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);

static int Rvsnprintf(char *buf, size_t size, const char  *format, va_list ap)
{
    int val;
    val = trio_vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    return val;
}
#else
static int Rvsnprintf(char *buf, size_t size, const char  *format, va_list ap)
{
    int val;
    val = vsnprintf(buf, size, format, ap);
    buf[size-1] = '\0';
    return val;
}
#endif

#define BUFSIZE 8192
static R_INLINE void RprintTrunc(char *buf)
{
    if(R_WarnLength < BUFSIZE - 20 && strlen(buf) == R_WarnLength) {
	strcat(buf, " ");
	strcat(buf, _("[... truncated]"));
    }
}

void warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;
    RCNTXT *c = R_GlobalContext;

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    RprintTrunc(buf);
    if (c && (c->callflag & CTXT_BUILTIN)) c = c->nextcontext;
    warningcall(c ? c->call : R_NilValue, "%s", buf);
}

/* declarations for internal condition handling */

static void vsignalError(SEXP call, const char *format, va_list ap);
static void vsignalWarning(SEXP call, const char *format, va_list ap);
static void NORET invokeRestart(SEXP, SEXP);

static void reset_inWarning(void *data)
{
    inWarning = 0;
}

#include <rlocale.h>

static int wd(const char * buf)
{
    int nc = (int) mbstowcs(NULL, buf, 0), nw;
    if(nc > 0 && nc < 2000) {
	wchar_t wc[2000];
	mbstowcs(wc, buf, nc + 1);
	nw = Ri18n_wcswidth(wc, 2147483647);
	return (nw < 1) ? nc : nw;
    }
    return nc;
}

static void vwarningcall_dflt(SEXP call, const char *format, va_list ap)
{
    int w;
    SEXP names, s;
    const char *dcall;
    char buf[BUFSIZE];
    RCNTXT *cptr;
    RCNTXT cntxt;

    if (inWarning)
	return;

    s = GetOption1(install("warning.expression"));
    if( s != R_NilValue ) {
	if( !isLanguage(s) &&  ! isExpression(s) )
	    error(_("invalid option \"warning.expression\""));
	cptr = R_GlobalContext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->callflag )
	    cptr = cptr->nextcontext;
	eval(s, cptr->cloenv);
	return;
    }

    w = asInteger(GetOption1(install("warn")));

    if( w == NA_INTEGER ) /* set to a sensible value */
	w = 0;

    if( w <= 0 && immediateWarning ) w = 1;

    if( w < 0 || inWarning || inError) /* ignore if w<0 or already in here*/
	return;

    /* set up a context which will restore inWarning if there is an exit */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &reset_inWarning;

    inWarning = 1;

    if(w >= 2) { /* make it an error */
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	RprintTrunc(buf);
	inWarning = 0; /* PR#1570 */
	errorcall(call, _("(converted from warning) %s"), buf);
    }
    else if(w == 1) {	/* print as they happen */
	char *tr;
	if( call != R_NilValue ) {
	    dcall = CHAR(STRING_ELT(deparse1s(call), 0));
	} else dcall = "";
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
	RprintTrunc(buf);

	if(dcall[0] == '\0') REprintf(_("Warning:"));
	else {
	    REprintf(_("Warning in %s :"), dcall);
	    if(!(noBreakWarning ||
		 ( mbcslocale && 18 + wd(dcall) + wd(buf) <= LONGWARN) ||
		 (!mbcslocale && 18 + strlen(dcall) + strlen(buf) <= LONGWARN)))
		REprintf("\n ");
	}
	REprintf(" %s\n", buf);
	if(R_ShowWarnCalls && call != R_NilValue) {
	    tr = R_ConciseTraceback(call, 0);
	    if (strlen(tr)) {REprintf(_("Calls:")); REprintf(" %s\n", tr);}
	}
    }
    else if(w == 0) {	/* collect them */
	if(!R_CollectWarnings) setupwarnings();
	if(R_CollectWarnings < R_nwarnings) {
	    SET_VECTOR_ELT(R_Warnings, R_CollectWarnings, call);
	    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength+1), format, ap);
	    RprintTrunc(buf);
	    if(R_ShowWarnCalls && call != R_NilValue) {
		char *tr =  R_ConciseTraceback(call, 0);
		size_t nc = strlen(tr);
		if (nc && nc + (int)strlen(buf) + 8 < BUFSIZE) {
		    strcat(buf, "\n");
		    strcat(buf, _("Calls:"));
		    strcat(buf, " ");
		    strcat(buf, tr);
		}
	    }
	    names = CAR(ATTRIB(R_Warnings));
	    SET_STRING_ELT(names, R_CollectWarnings++, mkChar(buf));
	}
    }
    /* else:  w <= -1 */
    endcontext(&cntxt);
    inWarning = 0;
}

static void warningcall_dflt(SEXP call, const char *format,...)
{
    va_list(ap);

    va_start(ap, format);
    vwarningcall_dflt(call, format, ap);
    va_end(ap);
}

void warningcall(SEXP call, const char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
}

void warningcall_immediate(SEXP call, const char *format, ...)
{
    va_list(ap);

    immediateWarning = 1;
    va_start(ap, format);
    vsignalWarning(call, format, ap);
    va_end(ap);
    immediateWarning = 0;
}

static void cleanup_PrintWarnings(void *data)
{
    if (R_CollectWarnings) {
	R_CollectWarnings = 0;
	R_Warnings = R_NilValue;
	REprintf(_("Lost warning messages\n"));
    }
    inPrintWarnings = 0;
}

attribute_hidden
void PrintWarnings(void)
{
    int i;
    char *header;
    SEXP names, s, t;
    RCNTXT cntxt;

    if (R_CollectWarnings == 0)
	return;
    else if (inPrintWarnings) {
	if (R_CollectWarnings) {
	    R_CollectWarnings = 0;
	    R_Warnings = R_NilValue;
	    REprintf(_("Lost warning messages\n"));
	}
	return;
    }

    /* set up a context which will restore inPrintWarnings if there is
       an exit */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &cleanup_PrintWarnings;

    inPrintWarnings = 1;
    header = ngettext("Warning message:", "Warning messages:",
		      R_CollectWarnings);
    if( R_CollectWarnings == 1 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	if( VECTOR_ELT(R_Warnings, 0) == R_NilValue )
	    REprintf("%s \n", CHAR(STRING_ELT(names, 0)));
	else {
	    const char *dcall, *msg = CHAR(STRING_ELT(names, 0));
	    dcall = CHAR(STRING_ELT(deparse1s(VECTOR_ELT(R_Warnings, 0)), 0));
	    REprintf(_("In %s :"), dcall);
	    if (mbcslocale) {
		int msgline1;
		char *p = strchr(msg, '\n');
		if (p) {
		    *p = '\0';
		    msgline1 = wd(msg);
		    *p = '\n';
		} else msgline1 = wd(msg);
		if (6 + wd(dcall) + msgline1 > LONGWARN) REprintf("\n ");
	    } else {
		size_t msgline1 = strlen(msg);
		char *p = strchr(msg, '\n');
		if (p) msgline1 = (int)(p - msg);
		if (6 + strlen(dcall) + msgline1 > LONGWARN) REprintf("\n ");
	    }
	    REprintf(" %s\n", msg);
	}
    } else if( R_CollectWarnings <= 10 ) {
	REprintf("%s\n", header);
	names = CAR(ATTRIB(R_Warnings));
	for(i = 0; i < R_CollectWarnings; i++) {
	    if( VECTOR_ELT(R_Warnings, i) == R_NilValue ) {
		REprintf("%d: %s \n", i+1, CHAR(STRING_ELT(names, i)));
	    } else {
		const char *dcall, *msg = CHAR(STRING_ELT(names, i));
		dcall = CHAR(STRING_ELT(deparse1s(VECTOR_ELT(R_Warnings, i)), 0));
		REprintf("%d: ", i + 1);
		REprintf(_("In %s :"), dcall);
		if (mbcslocale) {
		    int msgline1;
		    char *p = strchr(msg, '\n');
		    if (p) {
			*p = '\0';
			msgline1 = wd(msg);
			*p = '\n';
		    } else msgline1 = wd(msg);
		    if (10 + wd(dcall) + msgline1 > LONGWARN) {
			REprintf("\n ");
		    }
		} else {
		    size_t msgline1 = strlen(msg);
		    char *p = strchr(msg, '\n');
		    if (p) msgline1 = (int)(p - msg);
		    if (10 + strlen(dcall) + msgline1 > LONGWARN) {
			REprintf("\n ");
		    }
		}
		REprintf(" %s\n", msg);
	    }
	}
    } else {
	if (R_CollectWarnings < R_nwarnings)
	    REprintf(ngettext("There was %d warning (use warnings() to see it)",
			      "There were %d warnings (use warnings() to see them)",
			      R_CollectWarnings),
		     R_CollectWarnings);
	else
	    REprintf(_("There were %d or more warnings (use warnings() to see the first %d)"),
		     R_nwarnings, R_nwarnings);
	REprintf("\n");
    }
    /* now truncate and install last.warning */
    PROTECT(s = allocVector(VECSXP, R_CollectWarnings));
    PROTECT(t = allocVector(STRSXP, R_CollectWarnings));
    names = CAR(ATTRIB(R_Warnings));
    for(i = 0; i < R_CollectWarnings; i++) {
	SET_VECTOR_ELT(s, i, VECTOR_ELT(R_Warnings, i));
	SET_STRING_ELT(t, i, STRING_ELT(names, i));
    }
    setAttrib(s, R_NamesSymbol, t);
    SET_SYMVALUE(install("last.warning"), s);
    UNPROTECT(2);

    endcontext(&cntxt);

    inPrintWarnings = 0;
    R_CollectWarnings = 0;
    R_Warnings = R_NilValue;
    return;
}

/* Return a constructed source location (e.g. filename#123) from a srcref.  If the srcref
   is not valid "" will be returned.
*/

static SEXP GetSrcLoc(SEXP srcref)
{
    SEXP sep, line, result, srcfile;
    if (TYPEOF(srcref) != INTSXP || length(srcref) < 4)
	return ScalarString(mkChar(""));

    PROTECT(srcref);
    PROTECT(srcfile = R_GetSrcFilename(srcref));
    SEXP e2 = PROTECT(lang2( install("basename"), srcfile));
    PROTECT(srcfile = eval(e2, R_BaseEnv ) );
    PROTECT(sep = ScalarString(mkChar("#")));
    PROTECT(line = ScalarInteger(INTEGER(srcref)[0]));
    SEXP e = PROTECT(lang4( install("paste0"), srcfile, sep, line ));
    result = eval(e, R_BaseEnv );
    UNPROTECT(7);
    return result;
}

static char errbuf[BUFSIZE];

const char *R_curErrorBuf() {
    return (const char *)errbuf;
}

/* temporary hook to allow experimenting with alternate error mechanisms */
static void (*R_ErrorHook)(SEXP, char *) = NULL;

static void restore_inError(void *data)
{
    int *poldval = (int *) data;
    inError = *poldval;
    R_Expressions = R_Expressions_keep;
}

static void NORET
verrorcall_dflt(SEXP call, const char *format, va_list ap)
{
    RCNTXT cntxt;
    char *p, *tr;
    int oldInError;

    if (inError) {
	/* fail-safe handler for recursive errors */
	if(inError == 3) {
	     /* Can REprintf generate an error? If so we should guard for it */
	    REprintf(_("Error during wrapup: "));
	    /* this does NOT try to print the call since that could
	       cause a cascade of error calls */
	    Rvsnprintf(errbuf, sizeof(errbuf), format, ap);
	    REprintf("%s\n", errbuf);
	}
	if (R_Warnings != R_NilValue) {
	    R_CollectWarnings = 0;
	    R_Warnings = R_NilValue;
	    REprintf(_("Lost warning messages\n"));
	}
	R_Expressions = R_Expressions_keep;
	jump_to_top_ex(FALSE, FALSE, FALSE, FALSE, FALSE);
    }

    /* set up a context to restore inError value on exit */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &restore_inError;
    cntxt.cenddata = &oldInError;
    oldInError = inError;
    inError = 1;

    if(call != R_NilValue) {
	char tmp[BUFSIZE], tmp2[BUFSIZE];
	char *head = _("Error in "), *tail = "\n  ";
	SEXP srcloc = R_NilValue; // -Wall
	size_t len = 0;	// indicates if srcloc has been set
	int protected = 0, skip = NA_INTEGER;
	SEXP opt = GetOption1(install("show.error.locations"));
	if (!isNull(opt)) {
	    if (TYPEOF(opt) == STRSXP && length(opt) == 1) {
		if (pmatch(ScalarString(mkChar("top")), opt, 0)) skip = 0;
		else if (pmatch(ScalarString(mkChar("bottom")), opt, 0)) skip = -1;
	    } else if (TYPEOF(opt) == LGLSXP)
		skip = asLogical(opt) == 1 ? 0 : NA_INTEGER;
	    else
		skip = asInteger(opt);
	}

	const char *dcall = CHAR(STRING_ELT(deparse1s(call), 0));
	snprintf(tmp2, BUFSIZE,  "%s", head);
	if (skip != NA_INTEGER) {
	    PROTECT(srcloc = GetSrcLoc(R_GetCurrentSrcref(skip)));
	    protected++;
	    len = strlen(CHAR(STRING_ELT(srcloc, 0)));
	    if (len)
		snprintf(tmp2, BUFSIZE,  _("Error in %s (from %s) : "),
			 dcall, CHAR(STRING_ELT(srcloc, 0)));
	}

	Rvsnprintf(tmp, min(BUFSIZE, R_WarnLength) - strlen(head), format, ap);
	if (strlen(tmp2) + strlen(tail) + strlen(tmp) < BUFSIZE) {
	    if(len) snprintf(errbuf, BUFSIZE,
			     _("Error in %s (from %s) : "),
			     dcall, CHAR(STRING_ELT(srcloc, 0)));
	    else snprintf(errbuf, BUFSIZE,  _("Error in %s : "), dcall);
	    if (mbcslocale) {
		int msgline1;
		char *p = strchr(tmp, '\n');
		if (p) {
		    *p = '\0';
		    msgline1 = wd(tmp);
		    *p = '\n';
		} else msgline1 = wd(tmp);
		if (14 + wd(dcall) + msgline1 > LONGWARN)
		    strcat(errbuf, tail);
	    } else {
		size_t msgline1 = strlen(tmp);
		char *p = strchr(tmp, '\n');
		if (p) msgline1 = (int)(p - tmp);
		if (14 + strlen(dcall) + msgline1 > LONGWARN)
		    strcat(errbuf, tail);
	    }
	    strcat(errbuf, tmp);
	} else {
	    snprintf(errbuf, BUFSIZE, _("Error: "));
	    strcat(errbuf, tmp); // FIXME
	}
	UNPROTECT(protected);
    }
    else {
	snprintf(errbuf, BUFSIZE, _("Error: "));
	p = errbuf + strlen(errbuf);
	Rvsnprintf(p, min(BUFSIZE, R_WarnLength) - strlen(errbuf), format, ap);
    }

    p = errbuf + strlen(errbuf) - 1;
    if(*p != '\n') strcat(errbuf, "\n");

    if(R_ShowErrorCalls && call != R_NilValue) {  /* assume we want to avoid deparse */
	tr = R_ConciseTraceback(call, 0);
	size_t nc = strlen(tr);
	if (nc && nc + strlen(errbuf) + 8 < BUFSIZE) {
	    strcat(errbuf, _("Calls:"));
	    strcat(errbuf, " ");
	    strcat(errbuf, tr);
	    strcat(errbuf, "\n");
	}
    }
    if (R_ShowErrorMessages) REprintf("%s", errbuf);

    if( R_ShowErrorMessages && R_CollectWarnings ) {
	REprintf(_("In addition: "));
	PrintWarnings();
    }

    jump_to_top_ex(TRUE, TRUE, TRUE, TRUE, FALSE);

    /* not reached */
    endcontext(&cntxt);
    inError = oldInError;
}

static void NORET errorcall_dflt(SEXP call, const char *format,...)
{
    va_list(ap);

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

void NORET errorcall(SEXP call, const char *format,...)
{
    va_list(ap);

    va_start(ap, format);
    vsignalError(call, format, ap);
    va_end(ap);

    if (R_ErrorHook != NULL) {
	char buf[BUFSIZE];
	void (*hook)(SEXP, char *) = R_ErrorHook;
	R_ErrorHook = NULL; /* to avoid recursion */
	va_start(ap, format);
	Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
	va_end(ap);
	hook(call, buf);
    }

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

SEXP attribute_hidden do_geterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
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
    RCNTXT *c = R_GlobalContext;

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    /* This can be called before R_GlobalContext is defined, so... */
    /* If profiling is on, this can be a CTXT_BUILTIN */
    if (c && (c->callflag & CTXT_BUILTIN)) c = c->nextcontext;
    errorcall(c ? c->call : R_NilValue, "%s", buf);
}

static void try_jump_to_restart(void)
{
    SEXP list;

    for (list = R_RestartStack; list != R_NilValue; list = CDR(list)) {
	SEXP restart = CAR(list);
	if (TYPEOF(restart) == VECSXP && LENGTH(restart) > 1) {
	    SEXP name = VECTOR_ELT(restart, 0);
	    if (TYPEOF(name) == STRSXP && LENGTH(name) == 1) {
		const char *cname = CHAR(STRING_ELT(name, 0));
		if (! strcmp(cname, "browser") ||
		    ! strcmp(cname, "tryRestart") ||
		    ! strcmp(cname, "abort")) /**** move abort eventually? */
		    invokeRestart(restart, R_NilValue);
	    }
	}
    }
}

/* Unwind the call stack in an orderly fashion */
/* calling the code installed by on.exit along the way */
/* and finally longjmping to the innermost TOPLEVEL context */

static void jump_to_top_ex(Rboolean traceback,
			   Rboolean tryUserHandler,
			   Rboolean processWarnings,
			   Rboolean resetConsole,
			   Rboolean ignoreRestartContexts)
{
    RCNTXT cntxt;
    SEXP s;
    int haveHandler, oldInError;

    /* set up a context to restore inError value on exit */
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = &restore_inError;
    cntxt.cenddata = &oldInError;

    oldInError = inError;

    haveHandler = FALSE;

    if (tryUserHandler && inError < 3) {
	if (! inError)
	    inError = 1;

	/* now see if options("error") is set */
	s = GetOption1(install("error"));
	haveHandler = ( s != R_NilValue );
	if (haveHandler) {
	    if( !isLanguage(s) &&  ! isExpression(s) )  /* shouldn't happen */
		REprintf(_("invalid option \"error\"\n"));
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
		inError = oldInError;
	    }
	}
	inError = oldInError;
    }

    /* print warnings if there are any left to be printed */
    if( processWarnings && R_CollectWarnings )
	PrintWarnings();

    /* reset some stuff--not sure (all) this belongs here */
    if (resetConsole) {
	R_ResetConsole();
	R_FlushConsole();
	R_ClearerrConsole();
	R_ParseError = 0;
	R_ParseErrorFile = NULL;
	R_ParseErrorMsg[0] = '\0';
    }

    /*
     * Reset graphics state
     */
    GEonExit();

    /* WARNING: If oldInError > 0 ABSOLUTELY NO ALLOCATION can be
       triggered after this point except whatever happens in writing
       the traceback and R_run_onexits.  The error could be an out of
       memory error and any allocation could result in an
       infinite-loop condition. All you can do is reset things and
       exit.  */

    /* jump to a browser/try if one is on the stack */
    if (! ignoreRestartContexts)
	try_jump_to_restart();
    /* at this point, i.e. if we have not exited in
       try_jump_to_restart, we are heading for R_ToplevelContext */

    /* only run traceback if we are not going to bail out of a
       non-interactive session */
    if (R_Interactive || haveHandler) {
	/* write traceback if requested, unless we're already doing it
	   or there is an inconsistency between inError and oldInError
	   (which should not happen) */
	if (traceback && inError < 2 && inError == oldInError) {
	    inError = 2;
	    PROTECT(s = R_GetTraceback(0));
	    SET_SYMVALUE(install(".Traceback"), s);
	    /* should have been defineVar
	       setVar(install(".Traceback"), s, R_GlobalEnv); */
	    UNPROTECT(1);
	    inError = oldInError;
	}
    }

    /* Run onexit/cend code for all contexts down to but not including
       the jump target.  This may cause recursive calls to
       jump_to_top_ex, but the possible number of such recursive
       calls is limited since each exit function is removed before it
       is executed.  In addition, all but the first should have
       inError > 0.  This is not a great design because we could run
       out of other resources that are on the stack (like C stack for
       example).  The right thing to do is arrange to execute exit
       code *after* the LONGJMP, but that requires a more extensive
       redesign of the non-local transfer of control mechanism.
       LT. */
    R_run_onexits(R_ToplevelContext);

    R_GlobalContext = R_ToplevelContext;
    R_restore_globals(R_GlobalContext);

    /* if we are in the process of handling a C stack overflow we need
       to restore the C stack limit before the jump */
    if (R_OldCStackLimit != 0) {
	R_CStackLimit = R_OldCStackLimit;
	R_OldCStackLimit = 0;
    }

    LONGJMP(R_ToplevelContext->cjmpbuf, 0);
    /* not reached
    endcontext(&cntxt);
    inError = oldInError;
    */
}

void NORET jump_to_toplevel()
{
    /* no traceback, no user error option; for now, warnings are
       printed here and console is reset -- eventually these should be
       done after arriving at the jump target.  Now ignores
       try/browser frames--it really is a jump to toplevel */
    jump_to_top_ex(FALSE, FALSE, TRUE, TRUE, TRUE);
}

/* #define DEBUG_GETTEXT 1 */

/* gettext(domain, string) */
SEXP attribute_hidden do_gettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
#ifdef ENABLE_NLS
    const char *domain = "", *cfn;
    char *buf;
    SEXP ans, string = CADR(args);
    int i, n = LENGTH(string);

    checkArity(op, args);
    if(isNull(string) || !n) return string;

    if(!isString(string)) errorcall(call, _("invalid '%s' value"), "string");

    if(isNull(CAR(args))) {
	RCNTXT *cptr;
	SEXP rho = R_BaseEnv;
	for (cptr = R_GlobalContext->nextcontext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_FUNCTION) {
		/* stop() etc have internal call to .makeMessage */
		cfn = CHAR(STRING_ELT(deparse1s(CAR(cptr->call)), 0));
		if(streql(cfn, "stop") || streql(cfn, "warning")
		   || streql(cfn, "message")) continue;
		rho = cptr->cloenv;
	    }
	while(rho != R_EmptyEnv) {
	    if (rho == R_GlobalEnv) break;
	    else if (R_IsNamespaceEnv(rho)) {
		domain = translateChar(STRING_ELT(R_NamespaceEnvSpec(rho), 0));
		break;
	    }
	    rho = CDR(rho);
	}
	if(strlen(domain)) {
	    size_t len = strlen(domain)+3;
	    R_CheckStack2(len);
	    buf = (char *) alloca(len);
	    snprintf(buf, len, "R-%s", domain);
	    domain = buf;
	}
    } else if(isString(CAR(args)))
	domain = translateChar(STRING_ELT(CAR(args),0));
    else if(isLogical(CAR(args)) && LENGTH(CAR(args)) == 1 && LOGICAL(CAR(args))[0] == NA_LOGICAL) ;
    else errorcall(call, _("invalid '%s' value"), "domain");

    if(strlen(domain)) {
	PROTECT(ans = allocVector(STRSXP, n));
	for(i = 0; i < n; i++) {
	    int ihead = 0, itail = 0;
	    const char * This = translateChar(STRING_ELT(string, i));
	    char *tmp, *head = NULL, *tail = NULL, *p, *tr;
	    R_CheckStack2(strlen(This) + 1);
	    tmp = (char *) alloca(strlen(This) + 1);
	    strcpy(tmp, This);
	    /* strip leading and trailing white spaces and
	       add back after translation */
	    for(p = tmp;
		*p && (*p == ' ' || *p == '\t' || *p == '\n');
		p++, ihead++) ;
	    if(ihead > 0) {
		R_CheckStack2(ihead + 1);
		head = (char *) alloca(ihead + 1);
		strncpy(head, tmp, ihead);
		head[ihead] = '\0';
		tmp += ihead;
		}
	    if(strlen(tmp))
		for(p = tmp+strlen(tmp)-1;
		    p >= tmp && (*p == ' ' || *p == '\t' || *p == '\n');
		    p--, itail++) ;
	    if(itail > 0) {
		R_CheckStack2(itail + 1);
		tail = (char *) alloca(itail + 1);
		strcpy(tail, tmp+strlen(tmp)-itail);
		tmp[strlen(tmp)-itail] = '\0';
	    }
	    if(strlen(tmp)) {
#ifdef DEBUG_GETTEXT
		REprintf("translating '%s' in domain '%s'\n", tmp, domain);
#endif
		tr = dgettext(domain, tmp);
		R_CheckStack2(strlen(tr) + ihead + itail + 1);
		tmp = (char *) alloca(strlen(tr) + ihead + itail + 1);
		tmp[0] ='\0';
		if(ihead > 0) strcat(tmp, head);
		strcat(tmp, tr);
		if(itail > 0) strcat(tmp, tail);
		SET_STRING_ELT(ans, i, mkChar(tmp));
	    } else
		SET_STRING_ELT(ans, i, mkChar(This));
	}
	UNPROTECT(1);
	return ans;
    } else return CADR(args);
#else
    return CADR(args);
#endif
}

/* ngettext(n, msg1, msg2, domain) */
SEXP attribute_hidden do_ngettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef ENABLE_NLS
    const char *domain = "", *cfn;;
    char *buf;
    SEXP ans, sdom = CADDDR(args);
#endif
    SEXP msg1 = CADR(args), msg2 = CADDR(args);
    int n = asInteger(CAR(args));

    checkArity(op, args);
    if(n == NA_INTEGER || n < 0) error(_("invalid '%s' argument"), "n");
    if(!isString(msg1) || LENGTH(msg1) != 1)
	error(_("'%s' must be a character string"), "msg1");
    if(!isString(msg2) || LENGTH(msg2) != 1)
	error(_("'%s' must be a character string"), "msg2");

#ifdef ENABLE_NLS
    if(isNull(sdom)) {
	RCNTXT *cptr;
	SEXP rho = R_BaseEnv;
	for (cptr = R_GlobalContext->nextcontext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_FUNCTION) {
		/* stop() etc have internal call to .makeMessage */
		cfn = CHAR(STRING_ELT(deparse1s(CAR(cptr->call)), 0));
		if(streql(cfn, "stop") || streql(cfn, "warning")
		   || streql(cfn, "message")) continue;
		rho = cptr->cloenv;
	    }
	while(rho != R_EmptyEnv) {
	    if (rho == R_GlobalEnv) break;
	    else if (R_IsNamespaceEnv(rho)) {
		domain = translateChar(STRING_ELT(R_NamespaceEnvSpec(rho), 0));
		break;
	    }
	    rho = CDR(rho);
	}
	if(strlen(domain)) {
	    size_t len = strlen(domain)+3;
	    R_CheckStack2(len);
	    buf = (char *) alloca(len);
	    snprintf(buf, len, "R-%s", domain);
	    domain = buf;
	}
    } else if(isString(sdom))
	domain = CHAR(STRING_ELT(sdom,0));
    else if(isLogical(sdom) && LENGTH(sdom) == 1 && LOGICAL(sdom)[0] == NA_LOGICAL) ;
    else errorcall(call, _("invalid '%s' value"), "domain");

    /* libintl seems to malfunction if given a message of "" */
    if(strlen(domain) && length(STRING_ELT(msg1, 0))) {
	char *fmt = dngettext(domain,
			      translateChar(STRING_ELT(msg1, 0)),
			      translateChar(STRING_ELT(msg2, 0)),
			      n);
	PROTECT(ans = mkString(fmt));
	UNPROTECT(1);
	return ans;
    } else
#endif
	return n == 1 ? msg1 : msg2;
}


/* bindtextdomain(domain, dirname) */
SEXP attribute_hidden do_bindtextdomain(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef ENABLE_NLS
    char *res;

    checkArity(op, args);
    if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	errorcall(call, _("invalid '%s' value"), "domain");
    if(isNull(CADR(args))) {
	res = bindtextdomain(translateChar(STRING_ELT(CAR(args),0)), NULL);
    } else {
	if(!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	    errorcall(call, _("invalid '%s' value"), "dirname");
	res = bindtextdomain(translateChar(STRING_ELT(CAR(args),0)),
			     translateChar(STRING_ELT(CADR(args),0)));
    }
    if(res) return mkString(res);
    /* else this failed */
#endif
    return R_NilValue;
}

static SEXP findCall(void)
{
    RCNTXT *cptr;
    for (cptr = R_GlobalContext->nextcontext;
	 cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	 cptr = cptr->nextcontext)
	if (cptr->callflag & CTXT_FUNCTION)
	    return cptr->call;
    return R_NilValue;
}

SEXP attribute_hidden NORET do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
/* error(.) : really doesn't return anything; but all do_foo() must be SEXP */
    SEXP c_call;
    checkArity(op, args);

    if(asLogical(CAR(args))) /* find context -> "Error in ..:" */
	c_call = findCall();
    else
	c_call = R_NilValue;

    args = CDR(args);

    if (CAR(args) != R_NilValue) { /* message */
      SETCAR(args, coerceVector(CAR(args), STRSXP));
      if(!isValidString(CAR(args)))
	  errorcall(c_call, _(" [invalid string in stop(.)]"));
      errorcall(c_call, "%s", translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
      errorcall(c_call, "");
    /* never called: */
}

SEXP attribute_hidden do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP c_call;
    checkArity(op, args);

    if(asLogical(CAR(args))) /* find context -> "... in: ..:" */
	c_call = findCall();
    else
	c_call = R_NilValue;

    args = CDR(args);
    if(asLogical(CAR(args))) { /* immediate = TRUE */
	immediateWarning = 1;
    } else
	immediateWarning = 0;
    args = CDR(args);
    if(asLogical(CAR(args))) { /* noBreak = TRUE */
	noBreakWarning = 1;
    } else
	noBreakWarning = 0;
    args = CDR(args);
    if (CAR(args) != R_NilValue) {
	SETCAR(args, coerceVector(CAR(args), STRSXP));
	if(!isValidString(CAR(args)))
	    warningcall(c_call, _(" [invalid string in warning(.)]"));
	else
	    warningcall(c_call, "%s", translateChar(STRING_ELT(CAR(args), 0)));
    }
    else
	warningcall(c_call, "");
    immediateWarning = 0; /* reset to internal calls */
    noBreakWarning = 0;

    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
attribute_hidden
void NORET WrongArgCount(const char *s)
{
    error(_("incorrect number of arguments to \"%s\""), s);
}


void NORET UNIMPLEMENTED(const char *s)
{
    error(_("unimplemented feature in %s"), s);
}

/* ERROR_.. codes in Errormsg.h */
static struct {
    const R_ERROR code;
    const char* const format;
}
const ErrorDB[] = {
    { ERROR_NUMARGS,		N_("invalid number of arguments")	},
    { ERROR_ARGTYPE,		N_("invalid argument type")		},

    { ERROR_TSVEC_MISMATCH,	N_("time-series/vector length mismatch")},
    { ERROR_INCOMPAT_ARGS,	N_("incompatible arguments")		},

    { ERROR_UNIMPLEMENTED,	N_("unimplemented feature in %s")	},
    { ERROR_UNKNOWN,		N_("unknown error (report this!)")	}
};

static struct {
    R_WARNING code;
    char* format;
}
WarningDB[] = {
    { WARNING_coerce_NA,	N_("NAs introduced by coercion")	},
    { WARNING_coerce_INACC,	N_("inaccurate integer conversion in coercion")},
    { WARNING_coerce_IMAG,	N_("imaginary parts discarded in coercion") },

    { WARNING_UNKNOWN,		N_("unknown warning (report this!)")	},
};


attribute_hidden
void NORET ErrorMessage(SEXP call, int which_error, ...)
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
    Rvsnprintf(buf, BUFSIZE, _(ErrorDB[i].format), ap);
    va_end(ap);
    errorcall(call, "%s", buf);
}

attribute_hidden
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
    Rvsnprintf(buf, BUFSIZE, _(WarningDB[i].format), ap);
    va_end(ap);
    warningcall(call, "%s", buf);
}

#ifdef UNUSED
/* temporary hook to allow experimenting with alternate warning mechanisms */
static void (*R_WarningHook)(SEXP, char *) = NULL;

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
	    error(_("No function to return from, jumping to top level"));
    }
}

void NORET R_JumpToToplevel(Rboolean restart)
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
	warning(_("top level inconsistency?"));

    /* Run onexit/cend code for everything above the target. */
    R_run_onexits(c);

    R_ToplevelContext = R_GlobalContext = c;
    R_restore_globals(R_GlobalContext);

    /* if we are in the process of handling a C stack overflow we need
       to restore the C stack limit before the jump */
    if (R_OldCStackLimit != 0) {
	R_CStackLimit = R_OldCStackLimit;
	R_OldCStackLimit = 0;
    }

    LONGJMP(c->cjmpbuf, CTXT_TOPLEVEL);
}
#endif

static void R_SetErrmessage(const char *s)
{
    strncpy(errbuf, s, sizeof(errbuf));
    errbuf[sizeof(errbuf) - 1] = 0;
}

static void R_PrintDeferredWarnings(void)
{
    if( R_ShowErrorMessages && R_CollectWarnings ) {
	REprintf(_("In addition: "));
	PrintWarnings();
    }
}

attribute_hidden
SEXP R_GetTraceback(int skip)
{
    int nback = 0, ns;
    RCNTXT *c;
    SEXP s, t;

    for (c = R_GlobalContext, ns = skip;
	 c != NULL && c->callflag != CTXT_TOPLEVEL;
	 c = c->nextcontext)
	if (c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
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
	if (c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
	    if (skip > 0)
		skip--;
	    else {
		SETCAR(t, deparse1(c->call, 0, DEFAULTDEPARSE));
		if (c->srcref && !isNull(c->srcref))
		    setAttrib(CAR(t), R_SrcrefSymbol, duplicate(c->srcref));
		t = CDR(t);
	    }
	}
    UNPROTECT(1);
    return s;
}

SEXP attribute_hidden do_traceback(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int skip;

    checkArity(op, args);
    skip = asInteger(CAR(args));

    if (skip == NA_INTEGER || skip < 0 )
	error(_("invalid '%s' value"), "skip");

    return R_GetTraceback(skip);
}

static char * R_ConciseTraceback(SEXP call, int skip)
{
    static char buf[560];
    RCNTXT *c;
    size_t nl;
    int ncalls = 0;
    Rboolean too_many = FALSE;
    const char *top = "" /* -Wall */;

    buf[0] = '\0';
    for (c = R_GlobalContext;
	 c != NULL && c->callflag != CTXT_TOPLEVEL;
	 c = c->nextcontext)
	if (c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN) ) {
	    if (skip > 0)
		skip--;
	    else {
		SEXP fun = CAR(c->call);
		const char *this = (TYPEOF(fun) == SYMSXP) ?
		    CHAR(PRINTNAME(fun)) : "<Anonymous>";
		if(streql(this, "stop") ||
		   streql(this, "warning") ||
		   streql(this, "suppressWarnings") ||
		   streql(this, ".signalSimpleWarning")) {
		    buf[0] =  '\0'; ncalls = 0; too_many = FALSE;
		} else {
		    ncalls++;
		    if(too_many) {
			top = this;
		    } else if(strlen(buf) > R_NShowCalls) {
			memmove(buf+4, buf, strlen(buf)+1);
			memcpy(buf, "... ", 4);
			too_many = TRUE;
			top = this;
		    } else if(strlen(buf)) {
			nl = strlen(this);
			memmove(buf+nl+4, buf, strlen(buf)+1);
			memcpy(buf, this, strlen(this));
			memcpy(buf+nl, " -> ", 4);
		    } else
			memcpy(buf, this, strlen(this)+1);
		}
	    }
	}
    if(too_many && (nl = strlen(top)) < 50) {
	memmove(buf+nl+1, buf, strlen(buf)+1);
	memcpy(buf, top, strlen(top));
	memcpy(buf+nl, " ", 1);
    }
    /* don't add Calls if it adds no extra information */
    /* However: do we want to include the call in the list if it is a
       primitive? */
    if (ncalls == 1 && TYPEOF(call) == LANGSXP) {
	SEXP fun = CAR(call);
	const char *this = (TYPEOF(fun) == SYMSXP) ?
	    CHAR(PRINTNAME(fun)) : "<Anonymous>";
	if(streql(buf, this)) return "";
    }
    return buf;
}



static SEXP mkHandlerEntry(SEXP klass, SEXP parentenv, SEXP handler, SEXP rho,
			   SEXP result, int calling)
{
    SEXP entry = allocVector(VECSXP, 5);
    SET_VECTOR_ELT(entry, 0, klass);
    SET_VECTOR_ELT(entry, 1, parentenv);
    SET_VECTOR_ELT(entry, 2, handler);
    SET_VECTOR_ELT(entry, 3, rho);
    SET_VECTOR_ELT(entry, 4, result);
    SETLEVELS(entry, calling);
    return entry;
}

/**** rename these??*/
#define IS_CALLING_ENTRY(e) LEVELS(e)
#define ENTRY_CLASS(e) VECTOR_ELT(e, 0)
#define ENTRY_CALLING_ENVIR(e) VECTOR_ELT(e, 1)
#define ENTRY_HANDLER(e) VECTOR_ELT(e, 2)
#define ENTRY_TARGET_ENVIR(e) VECTOR_ELT(e, 3)
#define ENTRY_RETURN_RESULT(e) VECTOR_ELT(e, 4)

#define RESULT_SIZE 3

SEXP attribute_hidden do_addCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP classes, handlers, parentenv, target, oldstack, newstack, result;
    int calling, i, n;
    PROTECT_INDEX osi;

    checkArity(op, args);

    classes = CAR(args); args = CDR(args);
    handlers = CAR(args); args = CDR(args);
    parentenv = CAR(args); args = CDR(args);
    target = CAR(args); args = CDR(args);
    calling = asLogical(CAR(args));

    if (classes == R_NilValue || handlers == R_NilValue)
	return R_HandlerStack;

    if (TYPEOF(classes) != STRSXP || TYPEOF(handlers) != VECSXP ||
	LENGTH(classes) != LENGTH(handlers))
	error(_("bad handler data"));

    n = LENGTH(handlers);
    oldstack = R_HandlerStack;

    PROTECT(result = allocVector(VECSXP, RESULT_SIZE));
    PROTECT_WITH_INDEX(newstack = oldstack, &osi);

    for (i = n - 1; i >= 0; i--) {
	SEXP klass = STRING_ELT(classes, i);
	SEXP handler = VECTOR_ELT(handlers, i);
	SEXP entry = mkHandlerEntry(klass, parentenv, handler, target, result,
				    calling);
	REPROTECT(newstack = CONS(entry, newstack), osi);
    }

    R_HandlerStack = newstack;
    UNPROTECT(2);

    return oldstack;
}

SEXP attribute_hidden do_resetCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    R_HandlerStack = CAR(args);
    return R_NilValue;
}

static SEXP findSimpleErrorHandler(void)
{
    SEXP list;
    for (list = R_HandlerStack; list != R_NilValue; list = CDR(list)) {
	SEXP entry = CAR(list);
	if (! strcmp(CHAR(ENTRY_CLASS(entry)), "simpleError") ||
	    ! strcmp(CHAR(ENTRY_CLASS(entry)), "error") ||
	    ! strcmp(CHAR(ENTRY_CLASS(entry)), "condition"))
	    return list;
    }
    return R_NilValue;
}

static void vsignalWarning(SEXP call, const char *format, va_list ap)
{
    char buf[BUFSIZE];
    SEXP hooksym, hcall, qcall;

    hooksym = install(".signalSimpleWarning");
    if (SYMVALUE(hooksym) != R_UnboundValue &&
	SYMVALUE(R_QuoteSymbol) != R_UnboundValue) {
	PROTECT(qcall = LCONS(R_QuoteSymbol, LCONS(call, R_NilValue)));
	PROTECT(hcall = LCONS(qcall, R_NilValue));
	Rvsnprintf(buf, BUFSIZE - 1, format, ap);
	hcall = LCONS(mkString(buf), hcall);
	PROTECT(hcall = LCONS(hooksym, hcall));
	eval(hcall, R_GlobalEnv);
	UNPROTECT(3);
    }
    else vwarningcall_dflt(call, format, ap);
}

static void NORET gotoExitingHandler(SEXP cond, SEXP call, SEXP entry)
{
    SEXP rho = ENTRY_TARGET_ENVIR(entry);
    SEXP result = ENTRY_RETURN_RESULT(entry);
    SET_VECTOR_ELT(result, 0, cond);
    SET_VECTOR_ELT(result, 1, call);
    SET_VECTOR_ELT(result, 2, ENTRY_HANDLER(entry));
    findcontext(CTXT_FUNCTION, rho, result);
}

static void vsignalError(SEXP call, const char *format, va_list ap)
{
    char localbuf[BUFSIZE];
    SEXP list, oldstack;

    oldstack = R_HandlerStack;
    Rvsnprintf(localbuf, BUFSIZE - 1, format, ap);
    while ((list = findSimpleErrorHandler()) != R_NilValue) {
	char *buf = errbuf;
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	strncpy(buf, localbuf, BUFSIZE - 1);
	/*	Rvsnprintf(buf, BUFSIZE - 1, format, ap);*/
	buf[BUFSIZE - 1] = 0;
	if (IS_CALLING_ENTRY(entry)) {
	    if (ENTRY_HANDLER(entry) == R_RestartToken)
		return; /* go to default error handling; do not reset stack */
	    else {
		/* if we are in the process of handling a C stack
		   overflow, treat all calling handlers ar failed */
		if (R_OldCStackLimit)
		    break;
		SEXP hooksym, hcall, qcall;
		/* protect oldstack here, not outside loop, so handler
		   stack gets unwound in case error is protect stack
		   overflow */
		PROTECT(oldstack);
		hooksym = install(".handleSimpleError");
		PROTECT(qcall = LCONS(R_QuoteSymbol,
				      LCONS(call, R_NilValue)));
		PROTECT(hcall = LCONS(qcall, R_NilValue));
		hcall = LCONS(mkString(buf), hcall);
		hcall = LCONS(ENTRY_HANDLER(entry), hcall);
		PROTECT(hcall = LCONS(hooksym, hcall));
		eval(hcall, R_GlobalEnv);
		UNPROTECT(4);
	    }
	}
	else gotoExitingHandler(R_NilValue, call, entry);
    }
    R_HandlerStack = oldstack;
}

static SEXP findConditionHandler(SEXP cond)
{
    int i;
    SEXP list;
    SEXP classes = getAttrib(cond, R_ClassSymbol);

    if (TYPEOF(classes) != STRSXP)
	return R_NilValue;

    /**** need some changes here to allow conditions to be S4 classes */
    for (list = R_HandlerStack; list != R_NilValue; list = CDR(list)) {
	SEXP entry = CAR(list);
	for (i = 0; i < LENGTH(classes); i++)
	    if (! strcmp(CHAR(ENTRY_CLASS(entry)),
			 CHAR(STRING_ELT(classes, i))))
		return list;
    }
    return R_NilValue;
}

SEXP attribute_hidden do_signalCondition(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP list, cond, msg, ecall, oldstack;

    checkArity(op, args);

    cond = CAR(args);
    msg = CADR(args);
    ecall = CADDR(args);

    PROTECT(oldstack = R_HandlerStack);
    while ((list = findConditionHandler(cond)) != R_NilValue) {
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    if (h == R_RestartToken) {
		const char *msgstr = NULL;
		if (TYPEOF(msg) == STRSXP && LENGTH(msg) > 0)
		    msgstr = translateChar(STRING_ELT(msg, 0));
		else error(_("error message not a string"));
		errorcall_dflt(ecall, "%s", msgstr);
	    }
	    else {
		SEXP hcall = LCONS(h, LCONS(cond, R_NilValue));
		PROTECT(hcall);
		eval(hcall, R_GlobalEnv);
		UNPROTECT(1);
	    }
	}
	else gotoExitingHandler(cond, ecall, entry);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1);
    return R_NilValue;
}

static SEXP findInterruptHandler(void)
{
    SEXP list;
    for (list = R_HandlerStack; list != R_NilValue; list = CDR(list)) {
	SEXP entry = CAR(list);
	if (! strcmp(CHAR(ENTRY_CLASS(entry)), "interrupt") ||
	    ! strcmp(CHAR(ENTRY_CLASS(entry)), "condition"))
	    return list;
    }
    return R_NilValue;
}

static SEXP getInterruptCondition(void)
{
    /**** FIXME: should probably pre-allocate this */
    SEXP cond, klass;
    PROTECT(cond = allocVector(VECSXP, 0));
    PROTECT(klass = allocVector(STRSXP, 2));
    SET_STRING_ELT(klass, 0, mkChar("interrupt"));
    SET_STRING_ELT(klass, 1, mkChar("condition"));
    classgets(cond, klass);
    UNPROTECT(2);
    return cond;
}

static void signalInterrupt(void)
{
    SEXP list, cond, oldstack;

    PROTECT(oldstack = R_HandlerStack);
    while ((list = findInterruptHandler()) != R_NilValue) {
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	PROTECT(cond = getInterruptCondition());
	if (IS_CALLING_ENTRY(entry)) {
	    SEXP h = ENTRY_HANDLER(entry);
	    SEXP hcall = LCONS(h, LCONS(cond, R_NilValue));
	    PROTECT(hcall);
	    eval(hcall, R_GlobalEnv);
	    UNPROTECT(1);
	}
	else gotoExitingHandler(cond, R_NilValue, entry);
	UNPROTECT(1);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1);
}

void attribute_hidden
R_InsertRestartHandlers(RCNTXT *cptr, Rboolean browser)
{
    SEXP klass, rho, entry, name;

    if ((cptr->handlerstack != R_HandlerStack ||
	 cptr->restartstack != R_RestartStack)) {
	if (IS_RESTART_BIT_SET(cptr->callflag))
	    return;
	else
	    error(_("handler or restart stack mismatch in old restart"));
    }

    /**** need more here to keep recursive errors in browser? */
    rho = cptr->cloenv;
    PROTECT(klass = mkChar("error"));
    entry = mkHandlerEntry(klass, rho, R_RestartToken, rho, R_NilValue, TRUE);
    R_HandlerStack = CONS(entry, R_HandlerStack);
    UNPROTECT(1);
    PROTECT(name = mkString(browser ? "browser" : "tryRestart"));
    PROTECT(entry = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(entry, 0, name);
    SET_VECTOR_ELT(entry, 1, R_MakeExternalPtr(cptr, R_NilValue, R_NilValue));
    setAttrib(entry, R_ClassSymbol, mkString("restart"));
    R_RestartStack = CONS(entry, R_RestartStack);
    UNPROTECT(2);
}

SEXP attribute_hidden do_dfltWarn(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *msg;
    SEXP ecall;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    msg = translateChar(STRING_ELT(CAR(args), 0));
    ecall = CADR(args);

    warningcall_dflt(ecall, "%s", msg);
    return R_NilValue;
}

SEXP attribute_hidden NORET do_dfltStop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    const char *msg;
    SEXP ecall;

    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    msg = translateChar(STRING_ELT(CAR(args), 0));
    ecall = CADR(args);

    errorcall_dflt(ecall, "%s", msg);
}


/*
 * Restart Handling
 */

SEXP attribute_hidden do_getRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i;
    SEXP list;
    checkArity(op, args);
    i = asInteger(CAR(args));
    for (list = R_RestartStack;
	 list != R_NilValue && i > 1;
	 list = CDR(list), i--);
    if (list != R_NilValue)
	return CAR(list);
    else if (i == 1) {
	/**** need to pre-allocate */
	SEXP name, entry;
	PROTECT(name = mkString("abort"));
	PROTECT(entry = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(entry, 0, name);
	SET_VECTOR_ELT(entry, 1, R_NilValue);
	setAttrib(entry, R_ClassSymbol, mkString("restart"));
	UNPROTECT(2);
	return entry;
    }
    else return R_NilValue;
}

/* very minimal error checking --just enough to avoid a segfault */
#define CHECK_RESTART(r) do { \
    SEXP __r__ = (r); \
    if (TYPEOF(__r__) != VECSXP || LENGTH(__r__) < 2) \
	error(_("bad restart")); \
} while (0)

SEXP attribute_hidden do_addRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    CHECK_RESTART(CAR(args));
    R_RestartStack = CONS(CAR(args), R_RestartStack);
    return R_NilValue;
}

#define RESTART_EXIT(r) VECTOR_ELT(r, 1)

static void NORET invokeRestart(SEXP r, SEXP arglist)
{
    SEXP exit = RESTART_EXIT(r);

    if (exit == R_NilValue) {
	R_RestartStack = R_NilValue;
	jump_to_toplevel();
    }
    else {
	for (; R_RestartStack != R_NilValue;
	     R_RestartStack = CDR(R_RestartStack))
	    if (exit == RESTART_EXIT(CAR(R_RestartStack))) {
		R_RestartStack = CDR(R_RestartStack);
		if (TYPEOF(exit) == EXTPTRSXP) {
		    RCNTXT *c = (RCNTXT *) R_ExternalPtrAddr(exit);
		    R_JumpToContext(c, CTXT_RESTART, R_RestartToken);
		}
		else findcontext(CTXT_FUNCTION, exit, arglist);
	    }
	error(_("restart not on stack"));
    }
}

SEXP attribute_hidden NORET do_invokeRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    CHECK_RESTART(CAR(args));
    invokeRestart(CAR(args), CADR(args));
}

SEXP attribute_hidden do_addTryHandlers(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (R_GlobalContext == R_ToplevelContext ||
	! (R_GlobalContext->callflag & CTXT_FUNCTION))
	errorcall(call, _("not in a try context"));
    SET_RESTART_BIT_ON(R_GlobalContext->callflag);
    R_InsertRestartHandlers(R_GlobalContext, FALSE);
    return R_NilValue;
}

SEXP attribute_hidden do_seterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP msg;

    checkArity(op, args);
    msg = CAR(args);
    if(!isString(msg) || LENGTH(msg) != 1)
	error(_("error message must be a character string"));
    R_SetErrmessage(CHAR(STRING_ELT(msg, 0)));
    return R_NilValue;
}

SEXP attribute_hidden
do_printDeferredWarnings(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    R_PrintDeferredWarnings();
    return R_NilValue;
}

SEXP attribute_hidden
do_interruptsSuspended(SEXP call, SEXP op, SEXP args, SEXP env)
{
    int orig_value = R_interrupts_suspended;
    if (args != R_NilValue)
	R_interrupts_suspended = asLogical(CAR(args));
    return ScalarLogical(orig_value);
}

/* These functions are to be used in error messages, and available for others to use in the API
   GetCurrentSrcref returns the first non-NULL srcref after skipping skip of them.  If it
   doesn't find one it returns NULL. */

SEXP
R_GetCurrentSrcref(int skip)
{
    RCNTXT *c = R_GlobalContext;
    SEXP srcref = R_Srcref;
    if (skip < 0) { /* to count up from the bottom, we need to count them all first */
	while (c) {
	    if (srcref && srcref != R_NilValue)
		skip++;
	    srcref = c->srcref;
	    c = c->nextcontext;
	};
	if (skip < 0) return R_NilValue; /* not enough there */
	c = R_GlobalContext;
	srcref = R_Srcref;
    }
    while (c && (skip || !srcref || srcref == R_NilValue)) {
	if (srcref && srcref != R_NilValue)
	    skip--;
	srcref = c->srcref;
	c = c->nextcontext;
    }
    if (skip || !srcref)
	srcref = R_NilValue;
    return srcref;
}

/* Return the filename corresponding to a srcref, or "" if none is found */

SEXP
R_GetSrcFilename(SEXP srcref)
{
    SEXP srcfile = getAttrib(srcref, R_SrcfileSymbol);
    if (TYPEOF(srcfile) != ENVSXP)
	return ScalarString(mkChar(""));
    srcfile = findVar(install("filename"), srcfile);
    if (TYPEOF(srcfile) != STRSXP)
	return ScalarString(mkChar(""));
    return srcfile;
}
