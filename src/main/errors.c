/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995--2025  The R Core Team.
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
/* -> Errormsg.h , R_ext/Error.h */
#include <Internal.h>
#include <Startup.h> /* rather cleanup ..*/
#include <Rconnections.h>
#include <Rinterface.h>
#include <R_ext/GraphicsEngine.h> /* for GEonExit */
#include <Rmath.h> /* for imax2 */
#include <R_ext/Print.h>
#include <stdarg.h>

/* eval() sets R_Visible = TRUE. Thas may not be wanted when eval() is
   used in C code. This is a version that saves/restores R_Visible.
   This should probably be moved to eval.c, be make public, and used
   in  more places. LT */
static SEXP evalKeepVis(SEXP e, SEXP rho)
{
    Rboolean oldvis = R_Visible;
    SEXP val = eval(e, rho);
    R_Visible = oldvis;
    return val;
}

#ifndef min
#define min(a, b) (a<b?a:b)
#endif
#ifndef max
#define max(a, b) (a>b?a:b)
#endif

/* Total line length, in chars, before splitting in warnings/errors */
#define LONGWARN 75

/*
Different values of inError are used to indicate different places
in the error handling:
inError = 1: In internal error handling, e.g. `verrorcall_dflt`, others.
inError = 2: Writing traceback
inError = 3: In user error handler (i.e. options(error=handler))
*/
static int inError = 0;
static int inWarning = 0;
static int inPrintWarnings = 0;
static int immediateWarning = 0;
static int noBreakWarning = 0;

static void try_jump_to_restart(void);
// The next is crucial to the use of NORET attributes.
NORET static void 
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

NORET void R_SignalCStackOverflow(intptr_t usage)
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

    SEXP cond = R_makeCStackOverflowError(R_NilValue, usage);
    PROTECT(cond);
    /* calling handlers at this point might produce a C stack
       overflow/SEGFAULT so treat them as failed and skip them */
    /* use R_NilValue as the call to avoid using stack in deparsing */
    R_signalErrorConditionEx(cond, R_NilValue, TRUE);
    UNPROTECT(1); /* cond; not reached */
}

void attribute_no_sanitizer_instrumentation (R_CheckStack)(void)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    /* printf("usage %ld\n", usage); */
    if(R_CStackLimit != -1 && usage > ((intptr_t) R_CStackLimit))
	R_SignalCStackOverflow(usage);
}

void attribute_no_sanitizer_instrumentation R_CheckStack2(size_t extra)
{
    int dummy;
    intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);

    if (INTPTR_MAX - usage < extra)
	/* addition would overflow, this is definitely too much */
	R_SignalCStackOverflow(INTPTR_MAX);

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
    if (R_interrupts_pending) onintr();
}

static SEXP getInterruptCondition(void);
static void addInternalRestart(RCNTXT *, const char *);

static void onintrEx(Rboolean resumeOK)
{
    if (R_interrupts_suspended) {
	R_interrupts_pending = 1;
	return;
    }
    else R_interrupts_pending = 0;

    if (resumeOK) {
	SEXP rho = R_GlobalContext->cloenv;
	int dbflag = RDEBUG(rho);
	RCNTXT restartcontext;
	begincontext(&restartcontext, CTXT_RESTART, R_NilValue, R_GlobalEnv,
		     R_BaseEnv, R_NilValue, R_NilValue);
	if (SETJMP(restartcontext.cjmpbuf)) {
	    SET_RDEBUG(rho, dbflag); /* in case browser() has messed with it */
	    R_ReturnedValue = R_NilValue;
	    R_Visible = FALSE;
	    endcontext(&restartcontext);
	    return;
	}
	addInternalRestart(&restartcontext, "resume");
	signalInterrupt();
	endcontext(&restartcontext);
    }
    else signalInterrupt();

    /* Interrupts do not inherit from error, so we should not run the
       user erro handler. But we have been, so as a transition,
       continue to use options('error') if options('interrupt') is not
       set */
    Rboolean tryUserError = GetOption1(install("interrupt")) == R_NilValue;

    REprintf("\n");
    /* Attempt to save a traceback, show warnings, and reset console;
       also stop at restart (try/browser) frames.  Not clear this is
       what we really want, but this preserves current behavior */
    jump_to_top_ex(TRUE, tryUserError, TRUE, TRUE, FALSE);
}

void onintr(void)  { onintrEx(TRUE); }
void onintrNoResume(void) { onintrEx(FALSE); }

/* SIGUSR1: save and quit
   SIGUSR2: save and quit, don't run .Last or on.exit().

   These do far more processing than is allowed in a signal handler ....
*/

attribute_hidden void onsigusr1(int dummy)
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


attribute_hidden void onsigusr2(int dummy)
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

/* Rvsnprintf_mbcs: like vsnprintf, but guaranteed to null-terminate and not to
   split multi-byte characters, except if size is zero in which case the buffer
   is untouched and thus may not be null-terminated.

   This function may be invoked by the error handler via REvprintf.  Do not
   change it unless you are SURE that your changes are compatible with the
   error handling mechanism.

   REvprintf is also used in R_Suicide on Unix.

   Dangerous pattern: `Rvsnprintf_mbcs(buf, size - n, )` with n >= size */
#ifdef Win32
int trio_vsnprintf(char *buffer, size_t bufferSize, const char *format,
		   va_list args);

attribute_hidden
int Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
{
    int val;
    val = trio_vsnprintf(buf, size, format, ap);
    if (size) {
	if (val < 0) buf[0] = '\0'; /* not all uses check val < 0 */
	else buf[size-1] = '\0';
	if (val >= size)
	    mbcsTruncateToValid(buf);
    }
    return val;
}
#else
attribute_hidden
int Rvsnprintf_mbcs(char *buf, size_t size, const char *format, va_list ap)
{
    int val;
    val = vsnprintf(buf, size, format, ap);
    if (size) {
	if (val < 0) buf[0] = '\0'; /* not all uses check val < 0 */
	else buf[size-1] = '\0';
	if (val >= size)
	    mbcsTruncateToValid(buf);
    }
    return val;
}
#endif

/* Rsnprintf_mbcs: like snprintf, but guaranteed to null-terminate and
   not to split multi-byte characters, except if size is zero in which
   case the buffer is untouched and thus may not be null-terminated.

   Dangerous pattern: `Rsnprintf_mbcs(buf, size - n, )` with maybe n >= size*/
attribute_hidden
int Rsnprintf_mbcs(char *str, size_t size, const char *format, ...)
{
    int val;
    va_list ap;

    va_start(ap, format);
    val = Rvsnprintf_mbcs(str, size, format, ap);
    va_end(ap);

    return val;
}

/* Rstrncat: like strncat, but guaranteed not to split multi-byte characters */
static char *Rstrncat(char *dest, const char *src, size_t n)
{
    size_t after;
    size_t before = strlen(dest);

    strncat(dest, src, n);

    after = strlen(dest);
    if (after - before == n)
	/* the string may have been truncated, but we cannot know for sure
	   because str may not be null terminated */
	mbcsTruncateToValid(dest + before);

    return dest;
}

/* Rstrncpy: like strncpy, but guaranteed to null-terminate and not to
   split multi-byte characters */
static char *Rstrncpy(char *dest, const char *src, size_t n)
{
    strncpy(dest, src, n);
    if (n) {
	dest[n-1] = '\0';
	mbcsTruncateToValid(dest);
    }
    return dest;
}

#define BUFSIZE 8192
static R_INLINE void RprintTrunc(char *buf, int truncated)
{
    if (truncated) {
	char *msg = _("[... truncated]");
	if (strlen(buf) + 1 + strlen(msg) < BUFSIZE) {
	    strcat(buf, " ");
	    strcat(buf, msg);
	}
    }
}

static SEXP getCurrentCall(void)
{
    RCNTXT *c = R_GlobalContext;

    /* This can be called before R_GlobalContext is defined, so... */
    /* If profiling is on, this can be a CTXT_BUILTIN */

    if (c && (c->callflag & CTXT_BUILTIN)) c = c->nextcontext;
    if (c == R_GlobalContext && R_BCIntActive)
	return R_getBCInterpreterExpression();
    else
	return c ? c->call : R_NilValue;
}

void warning(const char *format, ...)
{
    char buf[BUFSIZE], *p;

    va_list(ap);
    va_start(ap, format);
    size_t psize;
    int pval;

    psize = min(BUFSIZE, R_WarnLength+1);
    pval = Rvsnprintf_mbcs(buf, psize, format, ap);
    va_end(ap);
    p = buf + strlen(buf) - 1;
    if(strlen(buf) > 0 && *p == '\n') *p = '\0';
    RprintTrunc(buf, pval >= psize);
    SEXP call = PROTECT(getCurrentCall());
    warningcall(call, "%s", buf);
    UNPROTECT(1);
}

/* declarations for internal condition handling */

static void vsignalError(SEXP call, const char *format, va_list ap);
static void vsignalWarning(SEXP call, const char *format, va_list ap);
NORET static void invokeRestart(SEXP, SEXP);

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
	// FIXME: width could conceivably exceed MAX_INT.
#ifdef USE_RI18N_WIDTH
	nw = Ri18n_wcswidth(wc, 2147483647);
#else
	nw = wcswidth(wc, 2147483647);
#endif
	return (nw < 0) ? nc : nw;
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
    size_t psize;
    int pval;

    if (inWarning)
	return;

    s = GetOption1(install("warning.expression"));
    if( s != R_NilValue ) {
	if( !isLanguage(s) &&  ! isExpression(s) )
	    error(_("invalid option \"warning.expression\""));
	cptr = R_GlobalContext;
	while ( !(cptr->callflag & CTXT_FUNCTION) && cptr->callflag )
	    cptr = cptr->nextcontext;
	evalKeepVis(s, cptr->cloenv);
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
	psize = min(BUFSIZE, R_WarnLength+1);
	pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	RprintTrunc(buf, pval >= psize);
	inWarning = 0; /* PR#1570 */
	errorcall(call, _("(converted from warning) %s"), buf);
    }
    else if(w == 1) {	/* print as they happen */
	char *tr;
	if( call != R_NilValue ) {
	    dcall = CHAR(STRING_ELT(deparse1s(call), false));
	} else dcall = "";
	psize = min(BUFSIZE, R_WarnLength+1);
	pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	RprintTrunc(buf, pval >= psize);

	if(dcall[0] == '\0') REprintf(_("Warning:"));
	else {
	    REprintf(_("Warning in %s :"), dcall);
	    // This did not allow for buf containing line breaks
	    // We can put the first line on the same line as the warning
	    // if it fits within LONGWARN.
	    char buf1[BUFSIZE];
	    strncpy(buf1, buf, BUFSIZE);
	    char *p = strstr(buf1, "\n");
	    if(p) *p = '\0';
	    if(!(noBreakWarning ||
		 ( mbcslocale && (18 + wd(dcall) + wd(buf1) <= LONGWARN)) ||
		 (!mbcslocale && (18 + strlen(dcall) + strlen(buf1) <= LONGWARN))))
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
	    psize = min(BUFSIZE, R_WarnLength+1);
	    pval = Rvsnprintf_mbcs(buf, psize, format, ap);
	    RprintTrunc(buf, pval >= psize);
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

static char errbuf[BUFSIZE + 1]; /* add 1 to leave room for a null byte */

#define ERRBUFCAT(txt) Rstrncat(errbuf, txt, BUFSIZE - strlen(errbuf))

const char *R_curErrorBuf(void) {
    return (const char *)errbuf;
}

static void restore_inError(void *data)
{
    int *poldval = (int *) data;
    inError = *poldval;
    R_Expressions = R_Expressions_keep;
}

/* Do not check constants on error more than this number of times per one
   R process lifetime; if so many errors are generated, the performance
   overhead due to the checks would be too high, and the program is doing
   something strange anyway (i.e. running no-segfault tests). The constant
   checks in GC and session exit (or .Call) do not have such limit. */
static int allowedConstsChecks = 1000;

/* Construct newline terminated error message, write it to global errbuf, and
   possibly display with REprintf. */
NORET static void
verrorcall_dflt(SEXP call, const char *format, va_list ap)
{
    if (allowedConstsChecks > 0) {
	allowedConstsChecks--;
	R_checkConstants(TRUE);
    }

    if (inError) {
	/* fail-safe handler for recursive errors */
	if(inError == 3) {
	     /* Can REprintf generate an error? If so we should guard for it */
	    REprintf(_("Error during wrapup: "));
	    /* this does NOT try to print the call since that could
	       cause a cascade of error calls */
	    Rvsnprintf_mbcs(errbuf, sizeof(errbuf), format, ap);
	    REprintf("%s\n", errbuf);
	}
	if (R_Warnings != R_NilValue) {
	    R_CollectWarnings = 0;
	    R_Warnings = R_NilValue;
	    REprintf(_("Lost warning messages\n"));
	}
	REprintf(_("Error: no more error handlers available "
		   "(recursive errors?); invoking 'abort' restart\n"));
	R_Expressions = R_Expressions_keep;
	jump_to_top_ex(FALSE, FALSE, FALSE, FALSE, FALSE);
    }

    /* set up a context to restore inError value on exit */
    RCNTXT cntxt;
    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    int oldInError;
    cntxt.cend = &restore_inError;
    cntxt.cenddata = &oldInError;
    oldInError = inError;
    inError = 1;

    // For use with Rv?snprintf, which truncates at size - 1, hence the + 1
    size_t msg_len = min(BUFSIZE, R_WarnLength) + 1;

    if(call != R_NilValue) {
	char tmp[BUFSIZE], tmp2[BUFSIZE];
	char *head = _("Error in "), *tail = "\n  ";
	SEXP srcloc = R_NilValue; // -Wall
	size_t len = 0;	// indicates if srcloc has been set
	int protected = 0, show = 0;
	SEXP opt = GetOption1(install("show.error.locations"));
	if (length(opt) == 1 &&
	    (asLogical(opt) == 1 ||
	     (TYPEOF(opt) == STRSXP &&
	      pmatch(ScalarString(mkChar("top")), opt, 0))))
	    	show = 1;

	const char *dcall = CHAR(STRING_ELT(deparse1s(call), 0));
	Rsnprintf_mbcs(tmp2, BUFSIZE,  "%s", head);
	if (show) {
	    PROTECT(srcloc = GetSrcLoc(R_GetCurrentSrcref(NA_INTEGER)));
	    protected++;
	    len = strlen(CHAR(STRING_ELT(srcloc, 0)));
	    if (len)
		Rsnprintf_mbcs(tmp2, BUFSIZE,  _("Error in %s (from %s) : "),
			       dcall, CHAR(STRING_ELT(srcloc, 0)));
	}

	Rvsnprintf_mbcs(tmp, max(msg_len - strlen(head), 0), format, ap);
	if (strlen(tmp2) + strlen(tail) + strlen(tmp) < BUFSIZE) {
	    if(len) Rsnprintf_mbcs(errbuf, BUFSIZE,
				   _("Error in %s (from %s) : "),
				   dcall, CHAR(STRING_ELT(srcloc, 0)));
	    else Rsnprintf_mbcs(errbuf, BUFSIZE,  _("Error in %s : "), dcall);
	    if (mbcslocale) {
		int msgline1;
		char *p = strchr(tmp, '\n');
		if (p) {
		    *p = '\0';
		    msgline1 = wd(tmp);
		    *p = '\n';
		} else msgline1 = wd(tmp);
		// gcc 8 warns here
		// 'output may be truncated copying between 0 and 8191 bytes from a string of length 8191'
		// but truncation is intentional.
		if (14 + wd(dcall) + msgline1 > LONGWARN)
		    ERRBUFCAT(tail);
	    } else {
		size_t msgline1 = strlen(tmp);
		char *p = strchr(tmp, '\n');
		if (p) msgline1 = (int)(p - tmp);
		if (14 + strlen(dcall) + msgline1 > LONGWARN)
		    ERRBUFCAT(tail);
	    }
	    ERRBUFCAT(tmp);
	} else {
	    Rsnprintf_mbcs(errbuf, BUFSIZE, _("Error: "));
	    ERRBUFCAT(tmp);
	}
	UNPROTECT(protected);
    }
    else {
	Rsnprintf_mbcs(errbuf, BUFSIZE, _("Error: "));
	char *p = errbuf + strlen(errbuf);
	Rvsnprintf_mbcs(p, max(msg_len - strlen(errbuf), 0), format, ap);
    }
    /* Approximate truncation detection, may produce false positives.  Assumes
       R_MB_CUR_MAX > 0. Note: approximation is fine, as the string may include
       dots, anyway */
    size_t nc = strlen(errbuf); // > 0, ignoring possibility of failure
    if (nc > BUFSIZE - 1 - (R_MB_CUR_MAX - 1)) {
	size_t end = min(nc + 1, (BUFSIZE + 1) - 4); // room for "...\n\0"
	for(size_t i = end; i <= BUFSIZE + 1; ++i) errbuf[i - 1] = '\0';
	mbcsTruncateToValid(errbuf);
	ERRBUFCAT("...\n");
    } else {
	char *p = errbuf + nc - 1;
	if(*p != '\n') {
	    ERRBUFCAT("\n");  // guaranteed to have room for this
	    ++nc;
	}
	if(R_ShowErrorCalls && call != R_NilValue) {  /* assume we want to avoid deparse */
	    char *tr = R_ConciseTraceback(call, 0);
	    size_t nc_tr = strlen(tr);
	    if (nc_tr) {
		char * call_trans = _("Calls:");
		if (nc_tr + nc + strlen(call_trans) + 2 < BUFSIZE + 1) {
		    ERRBUFCAT(call_trans);
		    ERRBUFCAT(" ");
		    ERRBUFCAT(tr);
		    ERRBUFCAT("\n");
		}
	    }
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

NORET static void errorcall_dflt(SEXP call, const char *format,...)
{
    va_list(ap);

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

NORET void errorcall(SEXP call, const char *format,...)
{
    va_list(ap);

    if (call == R_CurrentExpression)
	/* behave like error( */
	call = getCurrentCall();

    va_start(ap, format);
    vsignalError(call, format, ap);
    va_end(ap);

    va_start(ap, format);
    verrorcall_dflt(call, format, ap);
    va_end(ap);
}

/* Like errorcall, but copies all data for the error message into a buffer
   before doing anything else. */
NORET attribute_hidden
void errorcall_cpy(SEXP call, const char *format, ...)
{
    char buf[BUFSIZE];

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf_mbcs(buf, BUFSIZE, format, ap);
    va_end(ap);

    errorcall(call, "%s", buf);
}

// geterrmessage(): Return (the global) 'errbuf' as R string
attribute_hidden SEXP do_geterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    SEXP res = PROTECT(allocVector(STRSXP, 1));
    SET_STRING_ELT(res, 0, mkChar(errbuf));
    UNPROTECT(1);
    return res;
}

void error(const char *format, ...)
{
    char buf[BUFSIZE];

    va_list(ap);
    va_start(ap, format);
    Rvsnprintf_mbcs(buf, min(BUFSIZE, R_WarnLength), format, ap);
    va_end(ap);
    errorcall(getCurrentCall(), "%s", buf);
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

    /* don't use options("error") when handling a C stack overflow */
    if (R_OldCStackLimit == 0 && tryUserHandler && inError < 3) {
	if (! inError)
	    inError = 1;

	/* now see if options("error") is set */
	s = GetOption1(install("error"));
	haveHandler = ( s != R_NilValue );
	if (haveHandler) {
	    if( !isLanguage(s) &&  ! isExpression(s) )  /* shouldn't happen */
		REprintf(_("invalid option \"error\"\n"));
	    else {
		R_CheckStack();
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
       the traceback.  The error could be an out of memory error and
       any allocation could result in an infinite-loop condition. All
       you can do is reset things and exit.  */

    /* jump to a browser/try if one is on the stack */
    if (! ignoreRestartContexts)
	try_jump_to_restart();
    /* at this point, i.e. if we have not exited in
       try_jump_to_restart, we are heading for R_ToplevelContext */

    /* only run traceback if we are not going to bail out of a
       non-interactive session */

    if (R_Interactive || haveHandler || R_isTRUE(GetOption1(install("catch.script.errors")))) {
	/* write traceback if requested, unless we're already doing it
	   or there is an inconsistency between inError and oldInError
	   (which should not happen) */
	if (traceback && inError < 2 && inError == oldInError) {
	    inError = 2;
	    PROTECT(s = R_GetTracebackOnly(0));
	    SET_SYMVALUE(install(".Traceback"), s);
	    /* should have been defineVar
	       setVar(install(".Traceback"), s, R_GlobalEnv); */
	    UNPROTECT(1);
	    inError = oldInError;
	}
    }

    R_jumpctxt(R_ToplevelContext, 0, NULL);
}

NORET void jump_to_toplevel(void)
{
    /* no traceback, no user error option; for now, warnings are
       printed here and console is reset -- eventually these should be
       done after arriving at the jump target.  Now ignores
       try/browser frames--it really is a jump to toplevel */
    jump_to_top_ex(FALSE, FALSE, TRUE, TRUE, TRUE);
}

/* #define DEBUG_GETTEXT 1 */

#ifdef DEBUG_GETTEXT
# include <Print.h>
# define GETT_PRINT(...) REprintf(__VA_ARGS__)
#else
# define GETT_PRINT(...) do {} while(0)
#endif

#ifdef ENABLE_NLS
/* Called from do_gettext() and do_ngettext() */
static const char * determine_domain_gettext(SEXP domain_, Rboolean up)
{
    const char *domain = "";
    char *buf; // will be returned

    /* If TYPEOF(cptr->callfun) == CLOSXP (not .Primitive("eval")),
     * ENCLOS(cptr->cloenv) is CLOENV(cptr->callfun) */
    /* R_findParentContext(cptr, 1)->cloenv == cptr->sysparent */
    if(isNull(domain_)) {
	RCNTXT *cptr;
	GETT_PRINT(">> determine_domain_gettext(), first rho=%s\n", EncodeEnvironment(rho));

	/* stop() etc have internal call to .makeMessage */
	/* gettextf calls gettext */

	SEXP rho = R_EmptyEnv;
	if(R_GlobalContext->callflag & CTXT_FUNCTION) {
	    if(up) {
		SEXP call = R_GlobalContext->call;
		/* The call is of the form
		   <symbol>(<symbol>, domain = domain [possible other argument]) */
		rho =
		    (isSymbol(CAR(call)) && (call = CDR(call)) != R_NilValue &&
		     TAG(call) == R_NilValue && isSymbol(CAR(call)) &&
		     (call = CDR(call)) != R_NilValue &&
		     isSymbol(TAG(call)) && streql(CHAR(PRINTNAME(TAG(call))), "domain") &&
		     isSymbol(CAR(call)) && streql(CHAR(PRINTNAME(CAR(call))), "domain") &&
		     (cptr = R_findParentContext(R_GlobalContext, 1)))
		    ? cptr->sysparent
		    : R_GlobalContext->sysparent;
	    }
	    else
		rho = R_GlobalContext->sysparent;
	}
	GETT_PRINT(" .. rho1_domain_ => rho=%s\n", EncodeEnvironment(rho));

	SEXP ns = R_NilValue;
	int cnt = 0;
	    while(rho != R_EmptyEnv) {
		if (rho == R_GlobalEnv) break;
		else if (R_IsNamespaceEnv(rho)) {
		    ns = R_NamespaceEnvSpec(rho);
		    break;
		}
		if(++cnt <= 5 || cnt > 99) { // diagnose "inf." loop
		    GETT_PRINT("  cnt=%4d, rho=%s\n", cnt, EncodeEnvironment(rho));
		    if(cnt > 111) break;
		}
		if(rho == ENCLOS(rho)) break; // *did* happen; now keep for safety
		rho = ENCLOS(rho);
	    }
	if (!isNull(ns)) {
	    PROTECT(ns);
	    domain = translateChar(STRING_ELT(ns, 0));
	    if (strlen(domain)) {
		size_t len = strlen(domain)+3;
		buf = R_alloc(len, sizeof(char));
		Rsnprintf_mbcs(buf, len, "R-%s", domain);
		UNPROTECT(1); /* ns */
		GETT_PRINT("Managed to determine 'domain' from environment as: '%s'\n", buf);
		return (const char*) buf;
	    }
	    UNPROTECT(1); /* ns */
	}
	return NULL;

    } else if(isString(domain_)) {
	domain = translateChar(STRING_ELT(domain_, 0));
	if (!strlen(domain))
	    return NULL;
	return domain;

    } else if(isLogical(domain_) && LENGTH(domain_) == 1 && LOGICAL(domain_)[0] == NA_LOGICAL)
	return NULL;
    else error(_("invalid '%s' value"), "domain");
}
#endif


/* gettext(domain, string, trim) */
attribute_hidden SEXP do_gettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef _gettext_3_args_only_
    checkArity(op, args);
#else
    // legacy code allowing "captured" 2-arg calls
    int nargs = length(args);
    if (nargs < 2 || nargs > 3)
	errorcall(call, "either 2 or 3 arguments are required");
#endif
#ifdef ENABLE_NLS
    SEXP string = CADR(args);
    int n = LENGTH(string);

    if(isNull(string) || !n) return string;

    if(!isString(string)) error(_("invalid '%s' value"), "string");

    const char * domain = determine_domain_gettext(CAR(args), /*up*/TRUE);

    if(domain && strlen(domain)) {
	SEXP ans = PROTECT(allocVector(STRSXP, n));
	Rboolean trim;
#ifdef _gettext_3_args_only_
#else
	if(nargs == 2)
	    trim = TRUE;
	else
#endif
	    trim = asRbool(CADDR(args), call);
	for(int i = 0; i < n; i++) {
	    int ihead = 0, itail = 0;
	    const char * This = translateChar(STRING_ELT(string, i));
	    char *tmp, *head = NULL, *tail = NULL, *tr;
	    const char *p;

	    if(trim) {
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
		    Rstrncpy(head, tmp, ihead + 1);
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

		p = tmp;
	    } else
		p = This;
	    if(strlen(p)) {
		GETT_PRINT("translating '%s' in domain '%s'\n", p, domain);
		tr = dgettext(domain, p);
		if(ihead > 0 || itail > 0) {
 		R_CheckStack2(        strlen(tr) + ihead + itail + 1);
		tmp = (char *) alloca(strlen(tr) + ihead + itail + 1);
		tmp[0] ='\0';
		if(ihead > 0) strcat(tmp, head);
		strcat(tmp, tr);
		if(itail > 0) strcat(tmp, tail);
		} else
		    tmp = tr;
		SET_STRING_ELT(ans, i, mkChar(tmp));
	    } else
		SET_STRING_ELT(ans, i, mkChar(This));
	}
	UNPROTECT(1);
	return ans;
    } else
#endif
	// no NLS or no domain :
	return CADR(args);
}

/* ngettext(n, msg1, msg2, domain) */
attribute_hidden SEXP do_ngettext(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP msg1 = CADR(args), msg2 = CADDR(args);
    int n = asInteger(CAR(args));

    checkArity(op, args);
    if(n == NA_INTEGER || n < 0) error(_("invalid '%s' argument"), "n");
    if(!isString(msg1) || LENGTH(msg1) != 1)
	error(_("'%s' must be a character string"), "msg1");
    if(!isString(msg2) || LENGTH(msg2) != 1)
	error(_("'%s' must be a character string"), "msg2");

#ifdef ENABLE_NLS
    const char * domain = determine_domain_gettext(CADDDR(args), /*up*/FALSE);

    if(domain && strlen(domain)) {
	/* libintl seems to malfunction if given a message of "" */
	if(length(STRING_ELT(msg1, 0))) {
	    char *fmt = dngettext(domain,
		      translateChar(STRING_ELT(msg1, 0)),
		      translateChar(STRING_ELT(msg2, 0)),
		      n);
	    return mkString(fmt);
	}
    }
#endif
    return n == 1 ? msg1 : msg2;
}


/* bindtextdomain(domain, dirname) */
attribute_hidden SEXP do_bindtextdomain(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef ENABLE_NLS
    checkArity(op, args);
    if(isNull(CAR(args)) && isNull(CADR(args))) {
	textdomain(textdomain(NULL)); // flush the cache
	return ScalarLogical(TRUE);
    }
    else if(!isString(CAR(args)) || LENGTH(CAR(args)) != 1)
	error(_("invalid '%s' value"), "domain");

    char *res;
    if(isNull(CADR(args))) {
	res = bindtextdomain(translateChar(STRING_ELT(CAR(args),0)), NULL);
    } else {
	if(!isString(CADR(args)) || LENGTH(CADR(args)) != 1)
	    error(_("invalid '%s' value"), "dirname");
	res = bindtextdomain(translateChar(STRING_ELT(CAR (args),0)),
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

NORET attribute_hidden SEXP do_stop(SEXP call, SEXP op, SEXP args, SEXP rho)
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
      errorcall(c_call, "%s", "");
    /* never called: */
}

attribute_hidden SEXP do_warning(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	warningcall(c_call, "%s", "");
    immediateWarning = 0; /* reset to internal calls */
    noBreakWarning = 0;

    return CAR(args);
}

/* Error recovery for incorrect argument count error. */
NORET attribute_hidden
void WrongArgCount(const char *s)
{
    error(_("incorrect number of arguments to \"%s\""), s);
}


NORET void UNIMPLEMENTED(const char *s)
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


NORET attribute_hidden
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
    Rvsnprintf_mbcs(buf, BUFSIZE, _(ErrorDB[i].format), ap);
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

/* clang pre-3.9.0 says
      warning: passing an object that undergoes default argument promotion to
      'va_start' has undefined behavior [-Wvarargs]
*/
    va_start(ap, which_warn);
    Rvsnprintf_mbcs(buf, BUFSIZE, _(WarningDB[i].format), ap);
    va_end(ap);
    warningcall(call, "%s", buf);
}

static void R_SetErrmessage(const char *s)
{
    Rstrncpy(errbuf, s, sizeof(errbuf) - 1);
}

static void R_PrintDeferredWarnings(void)
{
    if( R_ShowErrorMessages && R_CollectWarnings ) {
	REprintf(_("In addition: "));
	PrintWarnings();
    }
}

/* if srcref indicates it is in bytecode, it needs a fixup */
static SEXP fixBCSrcref(SEXP srcref, RCNTXT *c)
{
    if (srcref == R_InBCInterpreter)
	srcref = R_findBCInterpreterSrcref(c);
    return srcref;
}

/*
 * Return the traceback without deparsing the calls
 */
attribute_hidden
SEXP R_GetTracebackOnly(int skip)
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
		SETCAR(t, duplicate(c->call));
		if (c->srcref && !isNull(c->srcref)) {
		    SEXP sref;
		    if (c->srcref == R_InBCInterpreter)
			sref = R_findBCInterpreterSrcref(c);
		    else
			sref = c->srcref;
		    setAttrib(CAR(t), R_SrcrefSymbol, duplicate(sref));
		}
		t = CDR(t);
	    }
	}
    UNPROTECT(1);
    return s;
}
/*
 * Return the traceback with calls deparsed
 */
attribute_hidden
SEXP R_GetTraceback(int skip)
{
    int nback = 0;
    SEXP s, t, u, v;
    s = PROTECT(R_GetTracebackOnly(skip));
    for(t = s; t != R_NilValue; t = CDR(t)) nback++;
    u = v = PROTECT(allocList(nback));

    for(t = s; t != R_NilValue; t = CDR(t), v=CDR(v)) {
	SEXP sref = getAttrib(CAR(t), R_SrcrefSymbol);
	SEXP dep = PROTECT(deparse1m(CAR(t), 0, DEFAULTDEPARSE));
	if (!isNull(sref))
	    setAttrib(dep, R_SrcrefSymbol, duplicate(sref));
	SETCAR(v, dep);
	UNPROTECT(1);
    }
    UNPROTECT(2);
    return u;
}

attribute_hidden SEXP do_traceback(SEXP call, SEXP op, SEXP args, SEXP rho)
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
#define CLEAR_ENTRY_CALLING_ENVIR(e) SET_VECTOR_ELT(e, 1, R_NilValue)
#define CLEAR_ENTRY_TARGET_ENVIR(e) SET_VECTOR_ELT(e, 3, R_NilValue)

attribute_hidden SEXP R_UnwindHandlerStack(SEXP target)
{
    SEXP hs;

    /* check that the target is in the current stack */
    for (hs = R_HandlerStack; hs != target && hs != R_NilValue; hs = CDR(hs))
	if (hs == target)
	    break;
    if (hs != target)
	return target; /* restoring a saved stack */

    for (hs = R_HandlerStack; hs != target; hs = CDR(hs)) {
	/* pop top handler; may not be needed */
	R_HandlerStack = CDR(hs);

	/* clear the two environments to reduce reference counts */
	CLEAR_ENTRY_CALLING_ENVIR(CAR(hs));
	CLEAR_ENTRY_TARGET_ENVIR(CAR(hs));
    }
    return target;
}

#define RESULT_SIZE 4

static SEXP R_HandlerResultToken = NULL;

attribute_hidden void R_FixupExitingHandlerResult(SEXP result)
{
    /* The internal error handling mechanism stores the error message
       in 'errbuf'.  If an on.exit() action is processed while jumping
       to an exiting handler for such an error, then endcontext()
       calls R_FixupExitingHandlerResult to save the error message
       currently in the buffer before processing the on.exit
       action. This is in case an error occurs in the on.exit action
       that over-writes the buffer. The allocation should occur in a
       more favorable stack context than before the jump. The
       R_HandlerResultToken is used to make sure the result being
       modified is associated with jumping to an exiting handler. */
    if (result != NULL &&
	TYPEOF(result) == VECSXP &&
	XLENGTH(result) == RESULT_SIZE &&
	VECTOR_ELT(result, 0) == R_NilValue &&
	VECTOR_ELT(result, RESULT_SIZE - 1) == R_HandlerResultToken) {
	SET_VECTOR_ELT(result, 0, mkString(errbuf));
    }
}

attribute_hidden SEXP do_addCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP classes, handlers, parentenv, target, oldstack, newstack, result;
    int calling, i, n;
    PROTECT_INDEX osi;

    if (R_HandlerResultToken == NULL) {
	R_HandlerResultToken = allocVector(VECSXP, 1);
	R_PreserveObject(R_HandlerResultToken);
    }

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
    SET_VECTOR_ELT(result, RESULT_SIZE - 1, R_HandlerResultToken);
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

attribute_hidden SEXP do_resetCondHands(SEXP call, SEXP op, SEXP args, SEXP rho)
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
    SEXP hooksym, hcall, qcall, qfun;

    hooksym = install(".signalSimpleWarning");
    if (SYMVALUE(hooksym) != R_UnboundValue &&
	SYMVALUE(R_QuoteSymbol) != R_UnboundValue) {
	qfun = lang3(R_DoubleColonSymbol, R_BaseSymbol, R_QuoteSymbol);
	PROTECT(qfun);
	PROTECT(qcall = LCONS(qfun, LCONS(call, R_NilValue)));
	PROTECT(hcall = LCONS(qcall, R_NilValue));
	Rvsnprintf_mbcs(buf, BUFSIZE - 1, format, ap);
	hcall = LCONS(mkString(buf), hcall);
	PROTECT(hcall = LCONS(hooksym, hcall));
	evalKeepVis(hcall, R_GlobalEnv);
	UNPROTECT(4);
    }
    else vwarningcall_dflt(call, format, ap);
}

NORET static void gotoExitingHandler(SEXP cond, SEXP call, SEXP entry)
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
    /* This function does not protect or restore the old handler
       stack. On return R_HandlerStack will be R_NilValue (unless
       R_RestartToken is encountered). */
    char localbuf[BUFSIZE];
    SEXP list;

    Rvsnprintf_mbcs(localbuf, BUFSIZE - 1, format, ap);
    while ((list = findSimpleErrorHandler()) != R_NilValue) {
	char *buf = errbuf;
	SEXP entry = CAR(list);
	R_HandlerStack = CDR(list);
	Rstrncpy(buf, localbuf, BUFSIZE);
	/*	Rvsnprintf(buf, BUFSIZE - 1, format, ap);*/
	if (IS_CALLING_ENTRY(entry)) {
	    if (ENTRY_HANDLER(entry) == R_RestartToken) {
		UNPROTECT(1); /* oldstack */
		break; /* go to default error handling */
	    } else {
		/* if we are in the process of handling a C stack
		   overflow, treat all calling handlers as failed */
		if (R_OldCStackLimit)
		    continue;
		SEXP hooksym, hcall, qcall, qfun;
		PROTECT(entry); /* protect since no longer on the stack */
		hooksym = install(".handleSimpleError");
		qfun = lang3(R_DoubleColonSymbol, R_BaseSymbol,
		             R_QuoteSymbol);
		PROTECT(qfun);
		PROTECT(qcall = LCONS(qfun,
				      LCONS(call, R_NilValue)));
		PROTECT(hcall = LCONS(qcall, R_NilValue));
		hcall = LCONS(mkString(buf), hcall);
		hcall = LCONS(ENTRY_HANDLER(entry), hcall);
		PROTECT(hcall = LCONS(hooksym, hcall));
		eval(hcall, R_GlobalEnv);
		UNPROTECT(5);
	    }
	}
	else gotoExitingHandler(R_NilValue, call, entry);
    }
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

attribute_hidden SEXP do_signalCondition(SEXP call, SEXP op, SEXP args, SEXP rho)
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
	    evalKeepVis(hcall, R_GlobalEnv);
	    UNPROTECT(1);
	}
	else gotoExitingHandler(cond, R_NilValue, entry);
	UNPROTECT(1);
    }
    R_HandlerStack = oldstack;
    UNPROTECT(1);

    SEXP h = GetOption1(install("interrupt"));
    if (h != R_NilValue) {
	SEXP call = PROTECT(LCONS(h, R_NilValue));
	evalKeepVis(call, R_GlobalEnv);
	UNPROTECT(1);
    }
}


static void checkRestartStacks(RCNTXT *cptr)
{
    if ((cptr->handlerstack != R_HandlerStack ||
	 cptr->restartstack != R_RestartStack)) {
	if (IS_RESTART_BIT_SET(cptr->callflag))
	    return;
	else
	    error(_("handler or restart stack mismatch in old restart"));
    }
}

static void addInternalRestart(RCNTXT *cptr, const char *cname)
{
    checkRestartStacks(cptr);
    SEXP entry, name;

    PROTECT(name = mkString(cname));
    PROTECT(entry = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(entry, 0, name);
    SET_VECTOR_ELT(entry, 1, R_MakeExternalPtr(cptr, R_NilValue, R_NilValue));
    setAttrib(entry, R_ClassSymbol, mkString("restart"));
    R_RestartStack = CONS(entry, R_RestartStack);
    UNPROTECT(2);
}

attribute_hidden void
R_InsertRestartHandlers(RCNTXT *cptr, const char *cname)
{
    SEXP klass, rho, entry;

    checkRestartStacks(cptr);

    /**** need more here to keep recursive errors in browser? */
    SEXP h = GetOption1(install("browser.error.handler"));
    if (! isFunction(h)) h = R_RestartToken;
    rho = cptr->cloenv;
    PROTECT(klass = mkChar("error"));
    entry = mkHandlerEntry(klass, rho, h, rho, R_NilValue, TRUE);
    R_HandlerStack = CONS(entry, R_HandlerStack);
    UNPROTECT(1);

    addInternalRestart(cptr, cname);
}

attribute_hidden SEXP do_dfltWarn(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    SEXP ecall = CADR(args);

    warningcall_dflt(ecall, "%s", msg);
    return R_NilValue;
}

NORET attribute_hidden SEXP do_dfltStop(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    if (TYPEOF(CAR(args)) != STRSXP || LENGTH(CAR(args)) != 1)
	error(_("bad error message"));
    const char *msg = translateChar(STRING_ELT(CAR(args), 0));
    SEXP ecall = CADR(args);

    errorcall_dflt(ecall, "%s", msg);
}


/*
 * Restart Handling
 */

attribute_hidden SEXP do_getRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
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

attribute_hidden SEXP do_addRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    CHECK_RESTART(CAR(args));
    R_RestartStack = CONS(CAR(args), R_RestartStack);
    return R_NilValue;
}

#define RESTART_EXIT(r) VECTOR_ELT(r, 1)

NORET static void invokeRestart(SEXP r, SEXP arglist)
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

NORET attribute_hidden 
SEXP do_invokeRestart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    CHECK_RESTART(CAR(args));
    invokeRestart(CAR(args), CADR(args));
}

attribute_hidden SEXP do_addTryHandlers(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    if (R_GlobalContext == R_ToplevelContext ||
	! (R_GlobalContext->callflag & CTXT_FUNCTION))
	error(_("not in a try context"));
    SET_RESTART_BIT_ON(R_GlobalContext->callflag);
    R_InsertRestartHandlers(R_GlobalContext, "tryRestart");
    return R_NilValue;
}

attribute_hidden SEXP do_seterrmessage(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP msg;

    checkArity(op, args);
    msg = CAR(args);
    if(!isString(msg) || LENGTH(msg) != 1)
	error(_("error message must be a character string"));
    R_SetErrmessage(CHAR(STRING_ELT(msg, 0)));
    return R_NilValue;
}

attribute_hidden SEXP
do_printDeferredWarnings(SEXP call, SEXP op, SEXP args, SEXP env)
{
    checkArity(op, args);
    R_PrintDeferredWarnings();
    return R_NilValue;
}

attribute_hidden SEXP
do_interruptsSuspended(SEXP call, SEXP op, SEXP args, SEXP env)
{
    Rboolean orig_value = R_interrupts_suspended;
    if (args != R_NilValue)
	R_interrupts_suspended = asRbool(CAR(args), call);
    return ScalarLogical(orig_value);
}

#if 0
attribute_hidden void
R_BadValueInRCode(SEXP value, SEXP call, SEXP rho, const char *rawmsg,
                  const char *errmsg, const char *warnmsg,
                  const char *varname, Rboolean errByDefault)
{
    /* disable GC so that use of this temporary checking code does not
       introduce new PROTECT errors e.g. in asLogical() use */
    R_CHECK_THREAD;
    int enabled = R_GCEnabled;
    R_GCEnabled = FALSE;
    int nprotect = 0;
    char *check = getenv(varname);
    const void *vmax = vmaxget();
    Rboolean err = check && StringTrue(check);
    if (!err && check && StringFalse(check))
	check = NULL; /* disabled */
    Rboolean abort = FALSE; /* R_Suicide/abort */
    Rboolean verbose = FALSE;
    Rboolean warn = FALSE;
    const char *pkgname = 0;
    if (!err && check) {
	const char *pprefix = "package:";
	const char *aprefix = "abort";
	const char *vprefix = "verbose";
	const char *wprefix = "warn";
	const char *cpname = "_R_CHECK_PACKAGE_NAME_";
	size_t lpprefix = strlen(pprefix);
	size_t laprefix = strlen(aprefix);
	size_t lvprefix = strlen(vprefix);
	size_t lwprefix = strlen(wprefix);
	size_t lcpname = strlen(cpname);
	Rboolean ignore = FALSE;

	SEXP spkg = R_NilValue;
	for(; rho != R_EmptyEnv; rho = ENCLOS(rho))
	    if (R_IsPackageEnv(rho)) {
		PROTECT(spkg = R_PackageEnvName(rho));
		nprotect++;
		break;
	    } else if (R_IsNamespaceEnv(rho)) {
		PROTECT(spkg = R_NamespaceEnvSpec(rho));
		nprotect++;
		break;
	    }
	if (spkg != R_NilValue)
	    pkgname = translateChar(STRING_ELT(spkg, 0));
	/* Sometimes pkgname is like 
	      package:MoTBFs,
	   so we need tp strip it off.
	   This is independent of pprefix.
	*/
	if (strstr(pkgname, "package:"))  pkgname += 8;

	while (check[0] != '\0') {
	    if (!strncmp(pprefix, check, lpprefix)) {
		/* check starts with "package:" */
		check += lpprefix;
		size_t arglen = 0;
		const char *sep = strchr(check, ',');
		if (sep)
		    arglen = sep - check;
		else
		    arglen = strlen(check);
		ignore = TRUE;
		if (pkgname) {
		    // a named package
		    if (!strncmp(check, pkgname, arglen) && strlen(pkgname) == arglen)
			ignore = FALSE;
		    // 'this package' 
		    else if (!strncmp(check, cpname, arglen) && lcpname == arglen) {
			/* package name specified in _R_CHECK_PACKAGE_NAME */
			const char *envpname = getenv(cpname);
			if (envpname && !strcmp(envpname, pkgname))
			    ignore = FALSE;
		    }
		    // "all_base" , that is all standard packages.
		    else if (!strncmp(check, "all_base", arglen) && arglen == 8) {
			char *std[] = {
			    "base",
			    "compiler",
			    // datasets has no code
			    "grDevies",
			    "graphics",
			    "grid",
			    "methods",
			    "parallel",
			    "splines",
			    "stats",
			    "stats4",
			    "utils",
			    "tools"
			};
			int nstd = sizeof(std)/sizeof(char *);
			for (int i = 0; i < nstd; i++)
			    if(!strcmp(std[i], pkgname)) {
				ignore = FALSE;
				break;
			    }
		    }
		}
		check += arglen;
	    } else if (!strncmp(aprefix, check, laprefix)) {
		/* check starts with "abort" */
		check += laprefix;
		abort = TRUE;
	    } else if (!strncmp(vprefix, check, lvprefix)) {
		/* check starts with "verbose" */
		check += lvprefix;
		verbose = TRUE;
	    } else if (!strncmp(wprefix, check, lwprefix)) {
		/* check starts with "warn" */
		check += lwprefix;
		warn = TRUE;
	    } else if (check[0] == ',') {
		check++;
	    } else
		error("invalid value of %s", varname);
	} // end of while (check[0] != '\0')
 
	if (ignore) {
	    abort = FALSE; /* err is FALSE */
	    verbose = FALSE;
	    warn = FALSE;
	} else if (!abort && !warn)
	    err = TRUE;
    }
    if (verbose) {
	int oldout = R_OutputCon;
	R_OutputCon = 2;
	int olderr = R_ErrorCon;
	R_ErrorCon = 2;
	REprintf(" ----------- FAILURE REPORT -------------- \n");
	REprintf(" --- failure: %s ---\n", rawmsg);
	REprintf(" --- srcref --- \n");
	SrcrefPrompt("", R_getCurrentSrcref());
	REprintf("\n");
	if (pkgname) {
	    REprintf(" --- package (from environment) --- \n");
	    REprintf("%s\n", pkgname);
	}
	REprintf(" --- call from context --- \n");
	PrintValue(R_GlobalContext->call);
	REprintf(" --- call from argument --- \n");
	PrintValue(call);
	REprintf(" --- R stacktrace ---\n");
	printwhere();
	REprintf(" --- value of length: %d type: %s ---\n",
		 length(value), R_typeToChar(value));
	PrintValue(value);
	REprintf(" --- function from context --- \n");
	if (R_GlobalContext->callfun != NULL &&
	    TYPEOF(R_GlobalContext->callfun) == CLOSXP)
	    PrintValue(R_GlobalContext->callfun);
	REprintf(" --- function search by body ---\n");
	if (R_GlobalContext->callfun != NULL &&
	    TYPEOF(R_GlobalContext->callfun) == CLOSXP)
	    findFunctionForBody(R_ClosureExpr(R_GlobalContext->callfun));
	REprintf(" ----------- END OF FAILURE REPORT -------------- \n");
	R_OutputCon = oldout;
	R_ErrorCon = olderr;
    }
    if (abort)
	R_Suicide(rawmsg);
    else if (warn)
	warningcall(call, warnmsg);
    else if (err || errByDefault)
	errorcall(call, errmsg);
    vmaxset(vmax);
    UNPROTECT(nprotect);
    R_GCEnabled = enabled;
}
#endif

/* These functions are to be used in error messages, and available for others to use in the API
   GetCurrentSrcref returns the first non-NULL srcref after skipping skip of them.  If it
   doesn't find one it returns NULL. */

SEXP
R_GetCurrentSrcref(int skip)
{
    RCNTXT *c = R_GlobalContext;
    SEXP srcref = NULL;
    int keep_looking = skip == NA_INTEGER;
    if (keep_looking) skip = 0;
    if (skip < 0) { /* to count up from the bottom, we need to count them all first */
	while (c) {
	    if (c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
		skip++;
	    c = c->nextcontext;
	};
	if (skip < 0) return R_NilValue; /* not enough there */
	c = R_GlobalContext;
    }
    	
    /* If skip = NA, try current active srcref first. */
    if (keep_looking) {
    	srcref = R_getCurrentSrcref();
        if (srcref && !isNull(srcref))
    	  return srcref;
    }
    
    /* Go to the first call */
    while (c && !(c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN)))
    	c = c->nextcontext;
    
    /* Now skip enough calls, regardless of srcref presence */
    while (c && skip) {
    	if (c->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    skip--;
	c = c->nextcontext;
    }
    /* Now get the next srcref.  If skip was not NA, don't
       keep looking. */
    do {
	if (!c) break;
        srcref = fixBCSrcref(c->srcref, c);
        c = c->nextcontext;
    } while (keep_looking && !(srcref && !isNull(srcref)));
    if (!srcref)
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
    srcfile = R_findVar(install("filename"), srcfile);
    if (TYPEOF(srcfile) != STRSXP)
	return ScalarString(mkChar(""));
    return srcfile;
}


/*
 * C level tryCatch support
 */

/* There are two functions:

       R_TryCatchError    handles error conditions;

       R_TryCatch         can handle any condition type and allows a
                          finalize action.
*/

SEXP R_tryCatchError(SEXP (*body)(void *), void *bdata,
		     SEXP (*handler)(SEXP, void *), void *hdata)
{
    SEXP val;
    SEXP cond = Rf_mkString("error");

    PROTECT(cond);
    val = R_tryCatch(body, bdata, cond, handler, hdata, NULL, NULL);
    UNPROTECT(1);
    return val;
}

/* This implementation uses R's tryCatch via calls from C to R to
   invoke R's tryCatch, and then back to C to infoke the C
   body/handler functions via a .Internal helper. This makes the
   implementation fairly simple but not fast. If performance becomes
   an issue we can look into a pure C implementation. LT */

typedef struct {
    SEXP (*body)(void *);
    void *bdata;
    SEXP (*handler)(SEXP, void *);
    void *hdata;
    void (*finally)(void *);
    void *fdata;
    Rboolean suspended;
} tryCatchData_t;

static SEXP default_tryCatch_handler(SEXP cond, void *data)
{
    return R_NilValue;
}

static void default_tryCatch_finally(void *data) { }

static SEXP trycatch_callback = NULL;
static const char* trycatch_callback_source =
    "function(addr, classes, fin) {\n"
    "    handler <- function(cond)\n"
    "        .Internal(C_tryCatchHelper(addr, 1L, cond))\n"
    "    handlers <- rep_len(alist(handler), length(classes))\n"
    "    names(handlers) <- classes\n"
    "    if (fin)\n"
    "	     handlers <- c(handlers,\n"
    "            alist(finally = .Internal(C_tryCatchHelper(addr, 2L))))\n"
    "    args <- c(alist(.Internal(C_tryCatchHelper(addr, 0L))), handlers)\n"
    "    do.call('tryCatch', args)\n"
    "}";

SEXP R_tryCatch(SEXP (*body)(void *), void *bdata,
		SEXP conds,
		SEXP (*handler)(SEXP, void *), void *hdata,
		void (*finally)(void *), void *fdata)
{
    if (body == NULL) error("must supply a body function");

    if (trycatch_callback == NULL) {
	trycatch_callback = R_ParseEvalString(trycatch_callback_source,
					      R_BaseNamespace);
	R_PreserveObject(trycatch_callback);
    }

    tryCatchData_t tcd = {
	.body = body,
	.bdata = bdata,
	.handler = handler != NULL ? handler : default_tryCatch_handler,
	.hdata = hdata,
	.finally = finally != NULL ? finally : default_tryCatch_finally,
	.fdata = fdata,
	.suspended = R_interrupts_suspended
    };

    /* Interrupts are suspended while in the infrastructure R code and
       enabled, if they were on entry to R_tryCatch, while calling the
       body function in do_tryCatchHelper */

    R_interrupts_suspended = TRUE;

    if (conds == NULL) conds = allocVector(STRSXP, 0);
    PROTECT(conds);
    SEXP fin = finally != NULL ? R_TrueValue : R_FalseValue;
    SEXP tcdptr = R_MakeExternalPtr(&tcd, R_NilValue, R_NilValue);
    SEXP expr = lang4(trycatch_callback, tcdptr, conds, fin);
    PROTECT(expr);
    SEXP val = evalKeepVis(expr, R_GlobalEnv);
    UNPROTECT(2); /* conds, expr */
    R_interrupts_suspended = tcd.suspended;
    return val;
}

attribute_hidden
SEXP do_tryCatchHelper(SEXP call, SEXP op, SEXP args, SEXP env)
{
    SEXP eptr = CAR(args);
    SEXP sw = CADR(args);
    SEXP cond = CADDR(args);

    if (TYPEOF(eptr) != EXTPTRSXP)
	error("not an external pointer");

    tryCatchData_t *ptcd = R_ExternalPtrAddr(CAR(args));

    switch (asInteger(sw)) {
    case 0:
	if (ptcd->suspended)
	    /* Interrupts were suspended for the call to R_TryCatch,
	       so leave them that way */
	    return ptcd->body(ptcd->bdata);
	else {
	    /* Interrupts were not suspended for the call to
	       R_TryCatch, but were suspended for the call through
	       R. So enable them for the body and suspend again on the
	       way out. */
	    R_interrupts_suspended = FALSE;
	    SEXP val = ptcd->body(ptcd->bdata);
	    R_interrupts_suspended = TRUE;
	    return val;
	}
    case 1:
	if (ptcd->handler != NULL)
	    return ptcd->handler(cond, ptcd->hdata);
	else return R_NilValue;
    case 2:
	if (ptcd->finally != NULL)
	    ptcd->finally(ptcd->fdata);
	return R_NilValue;
    default: return R_NilValue; /* should not happen */
    }
}


/* R_withCallingErrorHandler establishes a calling handler for
   conditions inheriting from class 'error'. The handler is
   established without calling back into the R implementation. This
   should therefore be much more efficient than the current R_tryCatch
   implementation. */

SEXP R_withCallingErrorHandler(SEXP (*body)(void *), void *bdata,
			       SEXP (*handler)(SEXP, void *), void *hdata)
{
    /* This defines the lambda expression for th handler. The `addr`
       variable will be defined in the closure environment and contain
       an external pointer to the callback data. */
    static const char* wceh_callback_source =
	"function(cond) .Internal(C_tryCatchHelper(addr, 1L, cond))";

    static SEXP wceh_callback = NULL;
    static SEXP wceh_class = NULL;
    static SEXP addr_sym = NULL;

    if (body == NULL) error("must supply a body function");

    if (wceh_callback == NULL) {
	wceh_callback = R_ParseEvalString(wceh_callback_source,
					  R_BaseNamespace);
	R_PreserveObject(wceh_callback);
	wceh_class = mkChar("error");
	R_PreserveObject(wceh_class);
	addr_sym = install("addr");
    }

    /* record the C-level handler information */
    tryCatchData_t tcd = {
	.handler = handler != NULL ? handler : default_tryCatch_handler,
	.hdata = hdata
    };
    SEXP tcdptr = R_MakeExternalPtr(&tcd, R_NilValue, R_NilValue);

    /* create the R handler function closure */
    SEXP env = CONS(tcdptr, R_NilValue);
    SET_TAG(env, addr_sym);
    env = NewEnvironment(R_NilValue, env, R_BaseNamespace);
    PROTECT(env);
    SEXP h = duplicate(wceh_callback);
    SET_CLOENV(h, env);
    UNPROTECT(1); /* env */

    /* push the handler on the handler stack */
    SEXP oldstack = R_HandlerStack;
    PROTECT(oldstack);
    PROTECT(h);
    SEXP entry = mkHandlerEntry(wceh_class, R_GlobalEnv, h, R_NilValue,
				R_NilValue, /* OK for a calling handler */
				TRUE);
    R_HandlerStack = CONS(entry, R_HandlerStack);
    UNPROTECT(1); /* h */

    SEXP val = body(bdata);

    /* restore the handler stack */
    R_HandlerStack = oldstack;
    UNPROTECT(1); /* oldstack */

    return val;
}

attribute_hidden SEXP do_addGlobHands(SEXP call, SEXP op,SEXP args, SEXP rho)
{
    /* check for handlers on the stack before proceeding (PR1826). */
    SEXP oldstk = R_ToplevelContext->handlerstack;
    for (RCNTXT *cptr = R_GlobalContext;
	 cptr != R_ToplevelContext;
	 cptr = cptr->nextcontext)
	if (cptr->handlerstack != oldstk)
	    error("should not be called with handlers on the stack");

    R_HandlerStack = R_NilValue;
    do_addCondHands(call, op, args, rho);

    /* This is needed to handle intermediate contexts that would
       restore the handler stack to the value when begincontext was
       called. This function should only be called in a context where
       there are no handlers on the stack. */
    for (RCNTXT *cptr = R_GlobalContext;
	 cptr != R_ToplevelContext;
	 cptr = cptr->nextcontext)
	if (cptr->handlerstack == oldstk)
	    cptr->handlerstack = R_HandlerStack;
	else /* should not happen after the check above */
	    error("should not be called with handlers on the stack");

    R_ToplevelContext->handlerstack = R_HandlerStack;
    return R_NilValue;
}


/* signaling conditions from C code */

static void R_signalCondition(SEXP cond, SEXP call,
			      int restoreHandlerStack,
			      int exitOnly)
{
    if (restoreHandlerStack) {
	SEXP oldstack = R_HandlerStack;
	PROTECT(oldstack);
	R_signalCondition(cond, call, FALSE, exitOnly);
	R_HandlerStack = oldstack;
	UNPROTECT(1); /* oldstack */
    }
    else {
	SEXP list;
	while ((list = findConditionHandler(cond)) != R_NilValue) {
	    SEXP entry = CAR(list);
	    R_HandlerStack = CDR(list);
	    if (IS_CALLING_ENTRY(entry)) {
		SEXP h = ENTRY_HANDLER(entry);
		if (h == R_RestartToken)
		    break;
		else if (! exitOnly) {
		    R_CheckStack();
		    SEXP hcall = LCONS(h, LCONS(cond, R_NilValue));
		    PROTECT(hcall);
		    eval(hcall, R_GlobalEnv);
		    UNPROTECT(1); /* hcall */
		}
	    }
	    else gotoExitingHandler(cond, call, entry);
	}
    }
}

NORET attribute_hidden /* for now */
void R_signalErrorConditionEx(SEXP cond, SEXP call, int exitOnly)
{
    /* caller must make sure that 'cond' and 'call' are protected. */
    R_signalCondition(cond, call, TRUE, exitOnly);

    /* the first element of 'cond' must be a scalar string to be used
       as the error message in default error processing. */
    if (TYPEOF(cond) != VECSXP || LENGTH(cond) == 0)
	error(_("condition object must be a VECSXP of length at least one"));
    SEXP elt = VECTOR_ELT(cond, 0);
    if (TYPEOF(elt) != STRSXP || LENGTH(elt) != 1)
	error(_("first element of condition object must be a scalar string"));

    errorcall_dflt(call, "%s", translateChar(STRING_ELT(elt, 0)));
}

NORET attribute_hidden /* for now */
void R_signalErrorCondition(SEXP cond, SEXP call)
{
    R_signalErrorConditionEx(cond, call, FALSE);
}

attribute_hidden /* for now */
void R_signalWarningCondition(SEXP cond)
{
    static SEXP condSym = NULL;
    static SEXP expr = NULL;
    if (expr == NULL) {
        condSym = install("cond");
        expr = R_ParseString("warning(cond)");
        R_PreserveObject(expr);
    }
    SEXP env = PROTECT(R_NewEnv(R_BaseNamespace, FALSE, 0));
    defineVar(condSym, cond, env);
    evalKeepVis(expr, env);
    UNPROTECT(1); /* env*/
}


/* creating internal error conditions */

/* use a static global buffer to create messages for the error
   condition objects to save stack space */
static char emsg_buf[BUFSIZE];

attribute_hidden /* for now */
SEXP R_vmakeErrorCondition(SEXP call,
			   const char *classname, const char *subclassname,
			   int nextra, const char *format, va_list ap)
{
    if (call == R_CurrentExpression)
	/* behave like error() */
	call = getCurrentCall();
    PROTECT(call);
    int nelem = nextra + 2;
    SEXP cond = PROTECT(allocVector(VECSXP, nelem));

    Rvsnprintf_mbcs(emsg_buf, BUFSIZE, format, ap);
    SET_VECTOR_ELT(cond, 0, mkString(emsg_buf));
    SET_VECTOR_ELT(cond, 1, call);

    SEXP names = allocVector(STRSXP, nelem);
    setAttrib(cond, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_STRING_ELT(names, 1, mkChar("call"));

    SEXP klass = allocVector(STRSXP, subclassname == NULL ? 3 : 4);
    setAttrib(cond, R_ClassSymbol, klass);
    if (subclassname == NULL) {
	SET_STRING_ELT(klass, 0, mkChar(classname));
	SET_STRING_ELT(klass, 1, mkChar("error"));
	SET_STRING_ELT(klass, 2, mkChar("condition"));
    }
    else {
	SET_STRING_ELT(klass, 0, mkChar(subclassname));
	SET_STRING_ELT(klass, 1, mkChar(classname));
	SET_STRING_ELT(klass, 2, mkChar("error"));
	SET_STRING_ELT(klass, 3, mkChar("condition"));
    }

    UNPROTECT(2); /* cond, call */

    return cond;
}

attribute_hidden /* for now */
SEXP R_makeErrorCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    SEXP cond = R_vmakeErrorCondition(call, classname, subclassname,
				      nextra, format, ap);
    va_end(ap);
    return cond;
}

NORET void R_MissingArgError_c(const char* arg, SEXP call, const char* subclass)
{
    if (call == R_CurrentExpression) /* as error() */
	call = getCurrentCall();
    PROTECT(call);
    SEXP cond;
    if(*arg)
	cond = R_makeErrorCondition(call, "missingArgError", subclass, 0,
				    _("argument \"%s\" is missing, with no default"), arg);
    else
	cond = R_makeErrorCondition(call, "missingArgError", subclass, 0,
				    _("argument is missing, with no default"));
    PROTECT(cond);
    R_signalErrorCondition(cond, call);
    UNPROTECT(2); /* not reached */
}

NORET void R_MissingArgError(SEXP symbol, SEXP call, const char* subclass)
{
    R_MissingArgError_c(CHAR(PRINTNAME(symbol)), call, subclass);
}


attribute_hidden /* for now */
void R_setConditionField(SEXP cond, R_xlen_t idx, const char *name, SEXP val)
{
    PROTECT(cond);
    PROTECT(val);
    /**** maybe this should be a general set named vector elt */
    /**** or maybe it should check that cond inherits from "condition" */
    /**** or maybe just fill in the next empty slot and not take an index */
    if (TYPEOF(cond) != VECSXP)
	error("bad condition argument");
    if (idx < 0 || idx >= XLENGTH(cond))
	error("bad field index");
    SEXP names = getAttrib(cond, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(cond))
	error("bad names attribute on condition object");
    SET_VECTOR_ELT(cond, idx, val);
    SET_STRING_ELT(names, idx, mkChar(name));
    UNPROTECT(2); /* cond, val */
}

attribute_hidden
SEXP R_makeNotSubsettableError(SEXP x, SEXP call)
{
    SEXP cond = R_makeErrorCondition(call, "notSubsettableError", NULL, 1,
				     R_MSG_ob_nonsub, R_typeToChar(x));
    PROTECT(cond);
    R_setConditionField(cond, 2, "object", x);
    UNPROTECT(1);
    return cond;
}

attribute_hidden
SEXP R_makeMissingSubscriptError(SEXP x, SEXP call)
{
    SEXP cond = R_makeErrorCondition(call, "MissingSubscriptError", NULL, 1,
				     R_MSG_miss_subs);
    PROTECT(cond);
    R_setConditionField(cond, 2, "object", x);
    UNPROTECT(1);
    return cond;
}

attribute_hidden
SEXP R_makeMissingSubscriptError1(SEXP call) // "1" arg.: no 'x'
{
    return R_makeErrorCondition(call, "MissingSubscriptError", NULL, 0,
				R_MSG_miss_subs);
}

attribute_hidden
SEXP R_makeOutOfBoundsError(SEXP x, int subscript, SEXP sindex,
			    SEXP call, const char *prefix)
{
    SEXP cond;
    const char *classname = "subscriptOutOfBoundsError";
    int nextra = 3;

    if (prefix)
	cond = R_makeErrorCondition(call, classname, NULL, nextra,
				    "%s %s", prefix, R_MSG_subs_o_b);
    else
	cond = R_makeErrorCondition(call, classname, NULL, nextra,
				    "%s", R_MSG_subs_o_b);
    PROTECT(cond);

    /* In some cases the 'subscript' argument is negative, indicating
       that which subscript is out of bounds is not known. We could
       probably do better, but for now report 'subscript' as NA in the
       condition object. */
    SEXP ssub = ScalarInteger(subscript >= 0 ? subscript + 1 : NA_INTEGER);
    PROTECT(ssub);

    R_setConditionField(cond, 2, "object", x);
    R_setConditionField(cond, 3, "subscript", ssub);
    R_setConditionField(cond, 4, "index", sindex);
    UNPROTECT(2); /* cond, ssub */

    return cond;
}

/* Do not translate this, to save stack space */
static const char *C_SO_msg_fmt =
    "C stack usage  %ld is too close to the limit";

attribute_hidden SEXP R_makeCStackOverflowError(SEXP call, intptr_t usage)
{
    SEXP cond = R_makeErrorCondition(call, "stackOverflowError",
				     "CStackOverflowError", 1,
				     C_SO_msg_fmt, usage);
    PROTECT(cond);
    R_setConditionField(cond, 2, "usage", ScalarReal((double) usage));
    UNPROTECT(1); /* cond */
    return cond;
}

static SEXP R_protectStackOverflowError = NULL;
attribute_hidden SEXP R_getProtectStackOverflowError(void)
{
    return R_protectStackOverflowError;
}

static SEXP R_expressionStackOverflowError = NULL;
attribute_hidden SEXP R_getExpressionStackOverflowError(void)
{
    return R_expressionStackOverflowError;
}

static SEXP R_nodeStackOverflowError = NULL;
attribute_hidden SEXP R_getNodeStackOverflowError(void)
{
    return R_nodeStackOverflowError;
}

attribute_hidden /* for now */
SEXP R_vmakeWarningCondition(SEXP call,
			   const char *classname, const char *subclassname,
			   int nextra, const char *format, va_list ap)
{
    if (call == R_CurrentExpression)
	/* behave like warning() */
	call = getCurrentCall();
    PROTECT(call);
    int nelem = nextra + 2;
    SEXP cond = PROTECT(allocVector(VECSXP, nelem));

    Rvsnprintf_mbcs(emsg_buf, BUFSIZE, format, ap);
    SET_VECTOR_ELT(cond, 0, mkString(emsg_buf));
    SET_VECTOR_ELT(cond, 1, call);

    SEXP names = allocVector(STRSXP, nelem);
    setAttrib(cond, R_NamesSymbol, names);
    SET_STRING_ELT(names, 0, mkChar("message"));
    SET_STRING_ELT(names, 1, mkChar("call"));

    SEXP klass = allocVector(STRSXP, subclassname == NULL ? 3 : 4);
    setAttrib(cond, R_ClassSymbol, klass);
    if (subclassname == NULL) {
	SET_STRING_ELT(klass, 0, mkChar(classname));
	SET_STRING_ELT(klass, 1, mkChar("warning"));
	SET_STRING_ELT(klass, 2, mkChar("condition"));
    }
    else {
	SET_STRING_ELT(klass, 0, mkChar(subclassname));
	SET_STRING_ELT(klass, 1, mkChar(classname));
	SET_STRING_ELT(klass, 2, mkChar("warning"));
	SET_STRING_ELT(klass, 3, mkChar("condition"));
    }

    UNPROTECT(2); /* cond, call */

    return cond;
}

attribute_hidden /* for now */
SEXP R_makeWarningCondition(SEXP call,
			  const char *classname, const char *subclassname,
			  int nextra, const char *format, ...)
{
    va_list(ap);
    va_start(ap, format);
    SEXP cond = R_vmakeWarningCondition(call, classname, subclassname,
				      nextra, format, ap);
    va_end(ap);
    return cond;
}

SEXP R_makePartialMatchWarningCondition(SEXP call, SEXP argument, SEXP formal)
{
    SEXP cond =
	R_makeWarningCondition(call, "partialMatchWarning", NULL, 2,
			       _("partial argument match of '%s' to '%s'"),
			       CHAR(PRINTNAME(argument)),//EncodeChar??
			       CHAR(PRINTNAME(formal)));//EncodeChar??
    PROTECT(cond);
    R_setConditionField(cond, 2, "argument", argument);
    R_setConditionField(cond, 3, "formal", formal);
    // idealy we would want the function/object in a field also
    UNPROTECT(1); /* cond */
    return cond;
}


#define PROT_SO_MSG _("protect(): protection stack overflow")
#define EXPR_SO_MSG _("evaluation nested too deeply: infinite recursion / options(expressions=)?")
#define NODE_SO_MSG _("node stack overflow")

attribute_hidden
void R_InitConditions(void)
{
    R_protectStackOverflowError =
	R_makeErrorCondition(R_NilValue, "stackOverflowError",
			     "protectStackOverflowError", 0, PROT_SO_MSG);
    MARK_NOT_MUTABLE(R_protectStackOverflowError);
    R_PreserveObject(R_protectStackOverflowError);

    R_expressionStackOverflowError =
	R_makeErrorCondition(R_NilValue, "stackOverflowError",
			     "expressionStackOverflowError", 0, EXPR_SO_MSG);
    MARK_NOT_MUTABLE(R_expressionStackOverflowError);
    R_PreserveObject(R_expressionStackOverflowError);

    R_nodeStackOverflowError =
	R_makeErrorCondition(R_NilValue, "stackOverflowError",
			     "nodeStackOverflowError", 0, NODE_SO_MSG);
    MARK_NOT_MUTABLE(R_nodeStackOverflowError);
    R_PreserveObject(R_nodeStackOverflowError);
}
