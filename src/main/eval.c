/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996	Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2001	The R Development Core Team.
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

#undef HASHING

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define ARGUSED(x) LEVELS(x)

#include "Defn.h"


SEXP do_browser(SEXP, SEXP, SEXP, SEXP);

#ifdef Macintosh

/* This code places a limit on the depth to which eval can recurse. */

/* Now R correctly handles user breaks
   This is the fastest way to do handle user breaks
   and performance are no rather good. 
   Jago, 13 Jun 2001, Stefano M. Iacus
*/
extern Boolean Interrupt;

void isintrpt()
{
   if(!Interrupt)
     return;
     
   if(CheckEventQueueForUserCancel()){  
	Rprintf("\n");
	error("user break");
	raise(SIGINT);
	return;
    }

}

#endif


#ifdef R_PROFILING

/* BDR 2000-07-15
   Profiling is now controlled by the R function Rprof(), and should
   have negligible cost when not enabled.
*/

/* A simple mechanism for profiling R code.  When R_PROFILING is
   enabled, eval will write out the call stack every PROFSAMPLE
   microseconds using the SIGPROF handler triggered by timer signals
   from the ITIMER_PROF timer.  Since this is the same timer used by C
   profiling, the two cannot be used together.  Output is written to
   the file PROFOUTNAME.  This is a plain text file.  The first line
   of the file contains the value of PROFSAMPLE.  The remaining lines
   each give the call stack found at a sampling point with the inner
   most function first.

   To enable profiling, recompile eval.c with R_PROFILING defined.  It
   would be possible to selectively turn profiling on and off from R
   and to specify the file name from R as well, but for now I won't
   bother.

   The stack is traced by walking back along the context stack, just
   like the traceback creation in jump_to_toplevel.  One drawback of
   this approach is that it does not show BUILTIN's since they don't
   get a context.  With recent changes to pos.to.env it seems possible
   to insert a context around BUILTIN calls to that they show up in
   the trace.  Since there is a cost in establishing these contexts,
   they are only inserted when profiling is enabled.

   One possible advantage of not tracing BUILTIN's is that then
   profiling adds no cost when the timer is turned off.  This would be
   useful if we want to allow profiling to be turned on and off from
   within R.

   One thing that makes interpreting profiling output tricky is lazy
   evaluation.  When an expression f(g(x)) is profiled, lazy
   evaluation will cause g to be called inside the call to f, so it
   will appear as if g is called by f.

   L. T.  */

#ifdef Win32
#include <windows.h>  /* for CreateEvent, SetEvent */
#include <process.h> /* for _beginthread, _endthread */
#else
#include <sys/time.h>
#include <signal.h>
#endif

FILE *R_ProfileOutfile = NULL;
static int R_Profiling = 0;

#ifdef Win32
HANDLE MainThread;
HANDLE ProfileEvent;

static void doprof()
{
    RCNTXT *cptr;
    char buf[1100];

    buf[0] = '\0';
    SuspendThread(MainThread);
    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if (((cptr->callflag & CTXT_FUNCTION) ||
	     (cptr->callflag & CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if(strlen(buf) < 1000) {
		strcat(buf, TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		       "<Anonymous>");
		strcat(buf, " ");
	    }
	}
    }
    ResumeThread(MainThread);
    if(strlen(buf))
	fprintf(R_ProfileOutfile, "%s\n", buf);
}


/* Profiling thread main function */
static void __cdecl ProfileThread(void *pwait)
{
    int wait = *((int *)pwait);

    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_HIGHEST);
    while(WaitForSingleObject(ProfileEvent, wait) != WAIT_OBJECT_0) {
	doprof();
    }
}
#else /* Unix */
static void doprof(int sig)
{
    RCNTXT *cptr;
    int newline = 0;
    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if (((cptr->callflag & CTXT_FUNCTION) ||
	     (cptr->callflag & CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    if (!newline) newline = 1;
	    fprintf(R_ProfileOutfile, "\"%s\" ",
		    TYPEOF(fun) == SYMSXP ? CHAR(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    if (newline) fprintf(R_ProfileOutfile, "\n");
    signal(SIGPROF, doprof);
}

static void doprof_null(int sig)
{
    signal(SIGPROF, doprof_null);
}
#endif


static void R_EndProfiling()
{
#ifdef Win32
    SetEvent(ProfileEvent);
    CloseHandle(MainThread);
#else
    struct itimerval itv;

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &itv, NULL);
    signal(SIGPROF, doprof_null);
#endif
    fclose(R_ProfileOutfile);
    R_ProfileOutfile = NULL;
    R_Profiling = 0;
}

static void R_InitProfiling(char * filename, int append, double dinterval)
{
#ifndef Win32
    struct itimerval itv;
#else
    int wait;
    HANDLE Proc = GetCurrentProcess();
#endif
    int interval = 1e6 * dinterval+0.5;

    if(R_ProfileOutfile != NULL) R_EndProfiling();
    R_ProfileOutfile = fopen(filename, append ? "a" : "w");
    if (R_ProfileOutfile == NULL)
	R_Suicide("can't open profile file");
    fprintf(R_ProfileOutfile, "sample.interval=%d\n", interval);

#ifdef Win32
    /* need to duplicate to make a real handle */
    DuplicateHandle(Proc, GetCurrentThread(), Proc, &MainThread,
		    0, FALSE, DUPLICATE_SAME_ACCESS);
    wait = interval/1000;
    if(!(ProfileEvent = CreateEvent(NULL, FALSE, FALSE, NULL)) ||
       (_beginthread(ProfileThread, 0, &wait) == -1))
	R_Suicide("unable to create profiling thread");
#else
    signal(SIGPROF, doprof);

    itv.it_interval.tv_sec = 0;
    itv.it_interval.tv_usec = interval;
    itv.it_value.tv_sec = 0;
    itv.it_value.tv_usec = interval;
    if (setitimer(ITIMER_PROF, &itv, NULL) == -1)
	R_Suicide("setting profile timer failed");
#endif
    R_Profiling = 1;
}

SEXP do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    char *filename;
    int append_mode;
    double dinterval;

    checkArity(op, args);
    if (!isString(CAR(args)) || (LENGTH(CAR(args))) != 1)
	errorcall(call, "invalid filename argument");
    append_mode = asLogical(CADR(args));
    dinterval = asReal(CADDR(args));
    filename = R_ExpandFileName(CHAR(STRING_ELT(CAR(args), 0)));
    if (strlen(filename))
	R_InitProfiling(filename, append_mode, dinterval);
    else
	R_EndProfiling();
    return R_NilValue;
}
#else
SEXP do_Rprof(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    error("R profiling is not available on this system");
    return R_NilValue;		/* -Wall */
}
#endif

/* NEEDED: A fixup is needed in browser, because it can trap errors,
 *	and currently does not reset the limit to the right value. */


/* Return value of "e" evaluated in "rho". */

SEXP eval(SEXP e, SEXP rho)
{
    SEXP op, tmp, val;

    /* The use of depthsave below is necessary because of the possibility */
    /* of non-local returns from evaluation.  Without this an "expression */
    /* too complex error" is quite likely. */

    int depthsave = R_EvalDepth++;

    if (R_EvalDepth > R_Expressions)
	error("evaluation is nested too deeply: infinite recursion?");
#ifdef Macintosh
    /* check for a user abort */
    if ((R_EvalCount++ % 100) == 0) {
	isintrpt();
	R_EvalCount = 0 ;
    }
#endif
#ifdef Win32
    if ((R_EvalCount++ % 100) == 0) {
	R_ProcessEvents();
/* Is this safe? R_EvalCount is not used in other part and
 * don't want to overflow it
*/
	R_EvalCount = 0 ;
    }
#endif

    tmp = R_NilValue;		/* -Wall */

    R_Visible = 1;
    switch (TYPEOF(e)) {
    case NILSXP:
    case LISTSXP:
    case LGLSXP:
    case INTSXP:
    case REALSXP:
    case STRSXP:
    case CPLXSXP:
    case SPECIALSXP:
    case BUILTINSXP:
    case ENVSXP:
    case CLOSXP:
    case VECSXP:
    case EXTPTRSXP:
#ifndef OLD
    case EXPRSXP:
#endif
	tmp = e;
	/* Make sure constants in expressions are NAMED before being
           used as values.  Setting NAMED to 2 makes sure weird calls
           to assignment functions won't modify constants in
           expressions.  */
	if (NAMED(tmp) != 2) SET_NAMED(tmp, 2);
	break;
    case SYMSXP:
	R_Visible = 1;
	if (e == R_DotsSymbol)
	    error("... used in an incorrect context");
	if( DDVAL(e) )
		tmp = ddfindVar(e,rho);
	else
		tmp = findVar(e, rho);
	if (tmp == R_UnboundValue)
	    error("Object \"%s\" not found", CHAR(PRINTNAME(e)));
	/* if ..d is missing then ddfindVar will signal */
	else if (tmp == R_MissingArg && !DDVAL(e) ) {
	    char *n = CHAR(PRINTNAME(e));
	    if(*n) error("Argument \"%s\" is missing, with no default",
			 CHAR(PRINTNAME(e)));
	    else error("Argument is missing, with no default");
	}
	else if (TYPEOF(tmp) == PROMSXP) {
	    PROTECT(tmp);
	    tmp = eval(tmp, rho);
#ifdef old
	    if (NAMED(tmp) == 1) SET_NAMED(tmp, 2);
	    else SET_NAMED(tmp, 1);
#else
	    SET_NAMED(tmp, 2);
#endif
	    UNPROTECT(1);
	}
#ifdef OLD
	else if (!isNull(tmp))
	    SET_NAMED(tmp, 1);
#else
	else if (!isNull(tmp) && NAMED(tmp) < 1)
	    SET_NAMED(tmp, 1);
#endif
	break;
    case PROMSXP:
	if (PRVALUE(e) == R_UnboundValue) {
	    if(PRSEEN(e))
		errorcall(R_GlobalContext->call,
			  "recursive default argument reference");
	    SET_PRSEEN(e, 1);
	    val = eval(PREXPR(e), PRENV(e));
	    SET_PRSEEN(e, 0);
	    SET_PRVALUE(e, val);
	}
	tmp = PRVALUE(e);
	break;
#ifdef OLD
    case EXPRSXP:
	{
	    int i, n;
	    n = LENGTH(e);
	    for(i=0 ; i<n ; i++)
		tmp = eval(VECTOR_ELT(e, i), rho);
	}
	break;
#endif
    case LANGSXP:
#ifdef R_PROFILING
/*	if (R_ProfileOutfile == NULL)
	    R_InitProfiling(PROFOUTNAME, 0); */
#endif
	if (TYPEOF(CAR(e)) == SYMSXP)
	    PROTECT(op = findFun(CAR(e), rho));
	else
	    PROTECT(op = eval(CAR(e), rho));
	if(TRACE(op)) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    int save = R_PPStackTop;
	    PROTECT(CDR(e));
	    R_Visible = 1 - PRIMPRINT(op);
	    tmp = PRIMFUN(op) (e, op, CDR(e), rho);
	    UNPROTECT(1);
	    if(save != R_PPStackTop) {
		Rprintf("stack imbalance in %s, %d then %d\n",
			PRIMNAME(op), save, R_PPStackTop);
	    }
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    int save = R_PPStackTop;
#ifdef R_PROFILING
	    if (R_Profiling) {
		RCNTXT cntxt;
		PROTECT(tmp = evalList(CDR(e), rho));
		R_Visible = 1 - PRIMPRINT(op);
		begincontext(&cntxt, CTXT_BUILTIN, e,
			     R_NilValue, R_NilValue, R_NilValue);
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		endcontext(&cntxt);
		UNPROTECT(1);
	    } else {
#endif
		PROTECT(tmp = evalList(CDR(e), rho));
		R_Visible = 1 - PRIMPRINT(op);
		tmp = PRIMFUN(op) (e, op, tmp, rho);
		UNPROTECT(1);
#ifdef R_PROFILING
	    }
#endif
	    if(save != R_PPStackTop) {
		Rprintf("stack imbalance in %s, %d then %d\n",
			PRIMNAME(op), save, R_PPStackTop);
	    }
	}
	else if (TYPEOF(op) == CLOSXP) {
	    PROTECT(tmp = promiseArgs(CDR(e), rho));
	    tmp = applyClosure(e, op, tmp, rho, R_NilValue);
	    UNPROTECT(1);
	}
	else
	    error("attempt to apply non-function");
	UNPROTECT(1);
	break;
    case DOTSXP:
	error("... used in an incorrect context");
    default:
	UNIMPLEMENTED("eval");
    }
    R_EvalDepth = depthsave;
    return (tmp);
}

/* Apply SEXP op of type CLOSXP to actuals */

SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedenv)
{
    SEXP body, formals, actuals, savedrho;
    volatile  SEXP newrho;
    SEXP f, a, tmp;
    RCNTXT cntxt;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    formals = FORMALS(op);
    body = BODY(op);
    savedrho = CLOENV(op);

    /*  Set up a context with the call in it so error has access to it */

    begincontext(&cntxt, CTXT_RETURN, call, savedrho, rho, arglist);

    /*  Build a list which matches the actual (unevaluated) arguments
	to the formal paramters.  Build a new environment which
	contains the matched pairs.  Ideally this environment sould be
	hashed.  */

    PROTECT(actuals = matchArgs(formals, arglist));
    PROTECT(newrho = NewEnvironment(formals, actuals, savedrho));

    /*  Use the default code for unbound formals.  FIXME: It looks like
	this code should preceed the building of the environment so that
        this will also go into the hash table.  */

    f = formals;
    a = actuals;
    while (f != R_NilValue) {
	if (CAR(a) == R_MissingArg && CAR(f) != R_MissingArg) {
	    SETCAR(a, mkPROMISE(CAR(f), newrho));
	    SET_MISSING(a, 2);
	}
	f = CDR(f);
	a = CDR(a);
    }

    /*  Fix up any extras that were supplied by usemethod. */

    if (suppliedenv != R_NilValue) {
	for (tmp = FRAME(suppliedenv); tmp != R_NilValue; tmp = CDR(tmp)) {
	    for (a = actuals; a != R_NilValue; a = CDR(a))
		if (TAG(a) == TAG(tmp))
		    break;
	    if (a == R_NilValue)
		/* Use defineVar instead of earlier version that added
                   bindings manually */
		defineVar(TAG(tmp), CAR(tmp), newrho);
	}
    }

    /*  Terminate the previous context and start a new one with the
        correct environment. */

    endcontext(&cntxt);

    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    if( R_GlobalContext->callflag == CTXT_GENERIC )
	begincontext(&cntxt, CTXT_RETURN, call,
		     newrho, R_GlobalContext->sysparent, arglist);
    else
	begincontext(&cntxt, CTXT_RETURN, call, newrho, rho, arglist);

    /* The default return value is NULL.  FIXME: Is this really needed
       or do we always get a sensible value returned?  */

    tmp = R_NilValue;

    /* Debugging */

    SET_DEBUG(newrho, DEBUG(op));
    if (DEBUG(op)) {
	Rprintf("debugging in: ");
	PrintValueRec(call,rho);
	/* Find out if the body is function with only one statement. */
	if (isSymbol(CAR(body)))
	    tmp = findFun(CAR(body), rho);
	else
	    tmp = eval(CAR(body), rho);
	if((TYPEOF(tmp) == BUILTINSXP || TYPEOF(tmp) == SPECIALSXP)
	   && !strcmp( PRIMNAME(tmp), "for")
	   && !strcmp( PRIMNAME(tmp), "{")
	   && !strcmp( PRIMNAME(tmp), "repeat")
	   && !strcmp( PRIMNAME(tmp), "while")
	   )
	    goto regdb;
	Rprintf("debug: ");
	PrintValue(body);
	do_browser(call,op,arglist,newrho);
    }

 regdb:

    /*  It isn't completely clear that this is the right place to do
	this, but maybe (if the matchArgs above reverses the
	arguments) it might just be perfect.  */

#ifdef  HASHING
#define HASHTABLEGROWTHRATE  1.2
    {
	SEXP R_NewHashTable(int, double);
	SEXP R_HashFrame(SEXP);
	int nargs = length(arglist);
	HASHTAB(newrho) = R_NewHashTable(nargs, HASHTABLEGROWTHRATE);
	newrho = R_HashFrame(newrho);
    }
#endif
#undef  HASHING

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    if ((SETJMP(cntxt.cjmpbuf))) {
	if (R_ReturnedValue == R_DollarSymbol) {
	    cntxt.callflag = CTXT_RETURN;  /* turn restart off */
	    R_GlobalContext = &cntxt;      /* put the context back */
	    PROTECT(tmp = eval(body, newrho));
	}
	else
	    PROTECT(tmp = R_ReturnedValue);
    }
    else {
	PROTECT(tmp = eval(body, newrho));
    }

    endcontext(&cntxt);

    if (DEBUG(op)) {
	Rprintf("exiting from: ");
	PrintValueRec(call, rho);
    }
    UNPROTECT(3);
    return (tmp);
}


static SEXP EnsureLocal(SEXP symbol, SEXP rho)
{
    SEXP vl;

    if ((vl = findVarInFrame(rho, symbol)) != R_UnboundValue) {
	vl = eval(symbol, rho);	/* for promises */
	if(NAMED(vl) == 2) {
	    PROTECT(vl = duplicate(vl));
	    defineVar(symbol, vl, rho);
	    UNPROTECT(1);
	}
	return vl;
    }

    vl = eval(symbol, ENCLOS(rho));
    if (vl == R_UnboundValue)
	error("Object \"%s\" not found", CHAR(PRINTNAME(symbol)));

    PROTECT(vl = duplicate(vl));
    defineVar(symbol, vl, rho);
    UNPROTECT(1);
    SET_NAMED(vl, 1);
    return vl;
}


/* Note: If val is a language object it must be protected */
/* to prevent evaluation.  As an example consider */
/* e <- quote(f(x=1,y=2); names(e) <- c("","a","b") */

static SEXP replaceCall(SEXP fun, SEXP val, SEXP args, SEXP rhs)
{
    SEXP tmp, ptmp;
    PROTECT(fun);
    PROTECT(args);
    PROTECT(rhs);
    PROTECT(val);
    ptmp = tmp = allocList(length(args)+3);
    UNPROTECT(4);
    SETCAR(ptmp, fun); ptmp = CDR(ptmp);
    SETCAR(ptmp, val); ptmp = CDR(ptmp);
    while(args != R_NilValue) {
	SETCAR(ptmp, CAR(args));
        SET_TAG(ptmp, TAG(args));
	ptmp = CDR(ptmp);
	args = CDR(args);
    }
    SETCAR(ptmp, rhs);
    SET_TAG(ptmp, install("value"));
    SET_TYPEOF(tmp, LANGSXP);
    return tmp;
}


static SEXP assignCall(SEXP op, SEXP symbol, SEXP fun,
		       SEXP val, SEXP args, SEXP rhs)
{
    PROTECT(op);
    PROTECT(symbol);
    val = replaceCall(fun, val, args, rhs);
    UNPROTECT(2);
    return lang3(op, symbol, val);
}


SEXP do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond = eval(CAR(args), rho);
    int cond;

    if ((cond = asLogical(Cond)) == NA_LOGICAL)
	errorcall(call, isLogical(Cond)
		  ? "missing value where logical needed"
		  : "argument of if(*) is not interpretable as logical");
    else if (cond)
	return (eval(CAR(CDR(args)), rho));
    else if (length(args) > 2)
	return (eval(CAR(CDR(CDR(args))), rho));
    R_Visible = 0;
    return R_NilValue;
}


SEXP do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int tmp, dbg;
    volatile int i, n, bgn;
    SEXP sym, body;
    volatile SEXP ans, v, val;
    RCNTXT cntxt;

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !isSymbol(sym) ) errorcall(call, "non-symbol loop variable");

    PROTECT(args);
    PROTECT(rho);
    PROTECT(val = eval(val, rho));
    defineVar(sym, R_NilValue, rho);
    if (isList(val) || isNull(val)) {
	n = length(val);
	PROTECT(v = R_NilValue);
    }
    else {
	n = LENGTH(val);
	PROTECT(v = allocVector(TYPEOF(val), 1));
    }
    ans = R_NilValue;

    dbg = DEBUG(rho);
    if (isLanguage(body) && isSymbol(CAR(body)) &&
	strcmp(CHAR(PRINTNAME(CAR(body))),"{") )
	bgn = 1;
    else
	bgn = 0;

    for (i = 0; i < n; i++) {
	if( DEBUG(rho) && bgn ) {
	    Rprintf("debug: ");
	    PrintValue(body);
	    do_browser(call,op,args,rho);
	}
	begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho,
		     R_NilValue, R_NilValue);
	if ((tmp = SETJMP(cntxt.cjmpbuf))) {
	    if (tmp == CTXT_BREAK) break;	/* break */
	    else   continue;                       /* next  */

	} else {
	    if (isVector(v)) {
		UNPROTECT(1);
		PROTECT(v = allocVector(TYPEOF(val), 1));
	    }
	    switch (TYPEOF(val)) {
	    case LGLSXP:
		LOGICAL(v)[0] = LOGICAL(val)[i];
		setVar(sym, v, rho);
		ans = eval(body, rho);
		break;
	    case INTSXP:
		INTEGER(v)[0] = INTEGER(val)[i];
		setVar(sym, v, rho);
		ans = eval(body, rho);
		break;
	    case REALSXP:
		REAL(v)[0] = REAL(val)[i];
		setVar(sym, v, rho);
		ans = eval(body, rho);
		break;
	    case CPLXSXP:
		COMPLEX(v)[0] = COMPLEX(val)[i];
		setVar(sym, v, rho);
		ans = eval(body, rho);
		break;
	    case STRSXP:
		SET_STRING_ELT(v, 0, STRING_ELT(val, i));
		setVar(sym, v, rho);
		ans = eval(body, rho);
		break;
	    case EXPRSXP:
	    case VECSXP:
		setVar(sym, VECTOR_ELT(val, i), rho);
		ans = eval(body, rho);
		break;
	    case LISTSXP:
		setVar(sym, CAR(val), rho);
		ans = eval(body, rho);
		val = CDR(val);
	    }
	    endcontext(&cntxt);
	}
    }
    UNPROTECT(4);
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return ans;
}


SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int cond, dbg;
    volatile int bgn;
    volatile SEXP s, t;
    RCNTXT cntxt;

    checkArity(op, args);
    s = eval(CAR(args), rho);	/* ??? */

    dbg = DEBUG(rho);
    t = CAR(CADR(args));
    if (isSymbol(t) && strcmp(CHAR(PRINTNAME(t)),"{"))
	bgn = 1;
    t = R_NilValue;
    for (;;) {
	if ((cond = asLogical(s)) == NA_LOGICAL)
	    errorcall(call, "missing value where logical needed");
	else if (!cond)
	    break;
	if (bgn && DEBUG(rho)) {
	    Rprintf("debug: ");
	    PrintValue(CAR(args));
	    do_browser(call,op,args,rho);
	}
	begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho,
		     R_NilValue, R_NilValue);
	if ((cond = SETJMP(cntxt.cjmpbuf))) {
	    if (cond == CTXT_BREAK) break;	/* break */
	    else continue;                      /* next  */
	}
	else {
	    PROTECT(t = eval(CAR(CDR(args)), rho));
	    s = eval(CAR(args), rho);
	    UNPROTECT(1);
	    endcontext(&cntxt);
	}
    }
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return t;
}


SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int cond, dbg;
    volatile int bgn;
    volatile SEXP t;
    RCNTXT cntxt;

    checkArity(op, args);

    dbg = DEBUG(rho);
    if (isSymbol(CAR(args)) && strcmp(CHAR(PRINTNAME(CAR(args))),"{"))
	bgn = 1;

    t = R_NilValue;
    for (;;) {
	if (DEBUG(rho) && bgn) {
	    Rprintf("debug: ");
	    PrintValue(CAR(args));
	    do_browser(call, op, args, rho);
	}
	begincontext(&cntxt, CTXT_LOOP, R_NilValue, rho,
		     R_NilValue, R_NilValue);
	if ((cond = SETJMP(cntxt.cjmpbuf))) {
	    if (cond == CTXT_BREAK) break;	/*break */
	    else   continue;                    /* next  */
	}
	else {
	    t = eval(CAR(args), rho);
	    endcontext(&cntxt);
	}
    }
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return t;
}


SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    findcontext(PRIMVAL(op), rho, R_NilValue);
    return R_NilValue;
}


SEXP do_paren(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    return CAR(args);
}


SEXP do_begin(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s;
    if (args == R_NilValue) {
	s = R_NilValue;
    }
    else {
	while (args != R_NilValue) {
	    if (DEBUG(rho)) {
		Rprintf("debug: ");
		PrintValue(CAR(args));
		do_browser(call,op,args,rho);
	    }
	    s = eval(CAR(args), rho);
	    args = CDR(args);
	}
    }
    return s;
}


SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP a, v, vals;
    int nv = 0;

    /* We do the evaluation here so that we can tag any untagged
       return values if they are specified by symbols. */

    PROTECT(vals = evalList(args, rho));
    a = args;
    v = vals;
    while (!isNull(a)) {
	nv += 1;
	if (isNull(TAG(a)) && isSymbol(CAR(a)))
	    SET_TAG(v, CAR(a));
	if (NAMED(CAR(v)) > 1)
	    SETCAR(v, duplicate(CAR(v)));
	a = CDR(a);
	v = CDR(v);
    }
    UNPROTECT(1);
    switch(nv) {
    case 0:
	v = R_NilValue;
	break;
    case 1:
	v = CAR(vals);
	break;
    default:
	v = PairToVectorList(vals);
	break;
    }
    if (R_BrowseLevel)
	findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, v);
    else
	findcontext(CTXT_FUNCTION, rho, v);
    return R_NilValue; /*NOTREACHED*/
}


SEXP do_function(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval;

    if (length(args) < 2)
	WrongArgCount("lambda");
    CheckFormals(CAR(args));
    rval = mkCLOSXP(CAR(args), CADR(args), rho);
    setAttrib(rval, R_SourceSymbol, CADDR(args));
    return rval;
}


/*
 *  Assignments for complex LVAL specifications. This is the stuff that
 *  nightmares are made of ...	Note that "evalseq" preprocesses the LHS
 *  of an assignment.  Given an expression, it builds a list of partial
 *  values for the exression.  For example, the assignment x$a[3] <- 10
 *  with LHS x$a[3] yields the (improper) list:
 *
 *	 (eval(x$a[3])	eval(x$a)  eval(x)  .  x)
 *
 *  (Note the terminating symbol).  The partial evaluations are carried
 *  out efficiently using previously computed components.
 */

static SEXP evalseq(SEXP expr, SEXP rho, int forcelocal, SEXP tmploc)
{
    SEXP val, nval, nexpr;
    if (isNull(expr))
	error("invalid (NULL) left side of assignment");
    if (isSymbol(expr)) {
	PROTECT(expr);
	if(forcelocal) {
	    nval = EnsureLocal(expr, rho);
	}
	else {
	    nval = eval(expr, rho);
	}
	UNPROTECT(1);
	return CONS(nval, expr);
    }
    else if (isLanguage(expr)) {
	PROTECT(expr);
	PROTECT(val = evalseq(CADR(expr), rho, forcelocal, tmploc));
	SETCAR(tmploc, CAR(val));
	PROTECT(nexpr = LCONS(TAG(tmploc), CDDR(expr)));
	PROTECT(nexpr = LCONS(CAR(expr), nexpr));
	nval = eval(nexpr, rho);
	UNPROTECT(4);
	return CONS(nval, val);
    }
    else error("Target of assignment expands to non-language object");
    return R_NilValue;	/*NOTREACHED*/
}

/* Main entry point for complex assignments */
/* We have checked to see that CAR(args) is a LANGSXP */

static char *asym[] = {":=", "<-", "<<-"};


static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP expr, lhs, rhs, saverhs, tmp, tmp2, tmploc;
    char buf[32];

    expr = CAR(args);

    /*  It's important that the rhs get evaluated first because
	assignment is right associative i.e.  a <- b <- c is parsed as
	a <- (b <- c).  */

    PROTECT(saverhs = rhs = eval(CADR(args), rho));

    /*  FIXME: We need to ensure that this works for hashed
        environments.  This code only works for unhashed ones.  the
        syntax error here is a delibrate marker so I don't forget that
        this needs to be done.  The code used in "missing" will help
        here.  */

    /*  FIXME: This strategy will not work when we are working in the
	data frame defined by the system hash table.  The structure there
	is different.  Should we special case here?  */

#ifdef HASHING
@@@@@@
#endif

    /*  We need a temporary variable to hold the intermediate values
	in the computation.  For efficiency reasons we record the
	location where this variable is stored.  */

    if (rho == R_NilValue)
	errorcall(call, "cannot do complex assignments in NULL environment");
    defineVar(R_TmpvalSymbol, R_NilValue, rho);
    tmploc = findVarLocInFrame(rho, R_TmpvalSymbol);
#ifdef OLD
    tmploc = FRAME(rho);
    while(tmploc != R_NilValue && TAG(tmploc) != R_TmpvalSymbol)
	tmploc = CDR(tmploc);
#endif

    /*  Do a partial evaluation down through the LHS. */
    lhs = evalseq(CADR(expr), rho, PRIMVAL(op)==1, tmploc);

    PROTECT(lhs);
    PROTECT(rhs); /* To get the loop right ... */

    while (isLanguage(CADR(expr))) {
	sprintf(buf, "%s<-", CHAR(PRINTNAME(CAR(expr))));
	tmp = install(buf);
	UNPROTECT(1);
	SETCAR(tmploc, CAR(lhs));
	PROTECT(tmp2 = mkPROMISE(rhs, rho));
	SET_PRVALUE(tmp2, rhs);
	PROTECT(rhs = replaceCall(tmp, TAG(tmploc), CDDR(expr), tmp2));
	rhs = eval(rhs, rho);
	UNPROTECT(2);
	PROTECT(rhs);
	lhs = CDR(lhs);
	expr = CADR(expr);
    }
    sprintf(buf, "%s<-", CHAR(PRINTNAME(CAR(expr))));
    SETCAR(tmploc, CAR(lhs));
    PROTECT(tmp = mkPROMISE(CADR(args), rho));
    SET_PRVALUE(tmp, rhs);
    PROTECT(expr = assignCall(install(asym[PRIMVAL(op)]), CDR(lhs),
			      install(buf), TAG(tmploc), CDDR(expr), tmp));
    expr = eval(expr, rho);
    UNPROTECT(5);
    unbindVar(R_TmpvalSymbol, rho);
    return duplicate(saverhs);
}


SEXP do_alias(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op,args);
    SET_NAMED(CAR(args), 0);
    return CAR(args);
}


/*  Assignment in its various forms  */

SEXP do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, t;
    if (length(args) != 2)
	WrongArgCount(asym[PRIMVAL(op)]);
    if (isString(CAR(args)))
	SETCAR(args, install(CHAR(STRING_ELT(CAR(args), 0))));

    switch (PRIMVAL(op)) {
    case 1:						/* <- */
	if (isSymbol(CAR(args))) {
	    s = eval(CADR(args), rho);
	    if (NAMED(s))
	    {
		PROTECT(s);
		t = duplicate(s);
		UNPROTECT(1);
		s = t;
	    }
	    PROTECT(s);
	    R_Visible = 0;
	    defineVar(CAR(args), s, rho);
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
	    return (s);
	}
	else if (isLanguage(CAR(args))) {
	    R_Visible = 0;
	    return applydefine(call, op, args, rho);
	}
	else errorcall(call,"invalid (do_set) left-hand side to assignment");
    case 2:						/* <<- */
	if (isSymbol(CAR(args))) {
	    s = eval(CADR(args), rho);
	    if (NAMED(s))
		s = duplicate(s);
	    PROTECT(s);
	    R_Visible = 0;
	    setVar(CAR(args), s, ENCLOS(rho));
	    UNPROTECT(1);
	    SET_NAMED(s, 1);
	    return s;
	}
	else if (isLanguage(CAR(args)))
	    return applydefine(call, op, args, rho);
	else error("invalid assignment lhs");

    default:
	UNIMPLEMENTED("do_set");

    }
    return R_NilValue;/*NOTREACHED*/
}


/* Evaluate each expression in "el" in the environment "rho".  This is */
/* a naturally recursive algorithm, but we use the iterative form below */
/* because it is does not cause growth of the pointer protection stack, */
/* and because it is a little more efficient. */

SEXP evalList(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(eval(CAR(h), rho), R_NilValue));
		    SET_TAG(CDR(tail), CreateTag(TAG(h)));
		    tail = CDR(tail);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error("... used in an incorrect context");
	}
	else if (CAR(el) != R_MissingArg) {
	    SETCDR(tail, CONS(eval(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}/* evalList() */


/* A slight variation of evaluating each expression in "el" in "rho". */
/* This is a naturally recursive algorithm, but we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP evalListKeepMissing(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we force all the promises and then splice
	 *	the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    if (CAR(h) == R_MissingArg)
			SETCDR(tail, CONS(R_MissingArg, R_NilValue));
		    else
			SETCDR(tail, CONS(eval(CAR(h), rho), R_NilValue));
		    SET_TAG(CDR(tail), CreateTag(TAG(h)));
		    tail = CDR(tail);
		    h = CDR(h);
		}
	    }
	    else if(h != R_MissingArg)
		error("... used in an incorrect context");
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	else {
	    SETCDR(tail, CONS(eval(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}


/* Create a promise to evaluate each argument.	Although this is most */
/* naturally attacked with a recursive algorithm, we use the iterative */
/* form below because it is does not cause growth of the pointer */
/* protection stack, and because it is a little more efficient. */

SEXP promiseArgs(SEXP el, SEXP rho)
{
    SEXP ans, h, tail;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while(el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 * we just ignore it and return the cdr with all its
	 * expressions promised; if it is bound to a ... list
	 * of promises, we repromise all the promises and then splice
	 * the list of resulting values into the return value.
	 * Anything else bound to a ... symbol is an error
	 */

	/* Is this double promise mechanism really needed? */

	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    SETCDR(tail, CONS(mkPROMISE(CAR(h), rho), R_NilValue));
		    SET_TAG(CDR(tail), CreateTag(TAG(h)));
		    tail = CDR(tail);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error("... used in an incorrect context");
	}
	else if (CAR(el) == R_MissingArg) {
	    SETCDR(tail, CONS(R_MissingArg, R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	else {
	    SETCDR(tail, CONS(mkPROMISE(CAR(el), rho), R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}


/* Check that each formal is a symbol */

void CheckFormals(SEXP ls)
{
    if (isList(ls)) {
	for (; ls != R_NilValue; ls = CDR(ls))
	    if (TYPEOF(TAG(ls)) != SYMSXP)
		goto err;
	return;
    }
 err:
    error("invalid formal argument list for \"function\"");
}



/* "eval" and "eval.with.vis" : Evaluate the first argument */
/* in the environment specified by the second argument. */

SEXP do_eval(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP encl;
    volatile SEXP expr, env, tmp;

    int nback;
    RCNTXT cntxt;

    checkArity(op, args);
    expr = CAR(args);
    env = CADR(args);
    encl = CADDR(args);
    if ( !isNull(encl) && !isEnvironment(encl) )
	errorcall(call, "invalid 3rd argument");
    switch(TYPEOF(env)) {
    case NILSXP:
    case ENVSXP:
	PROTECT(env);	/* so we can unprotect 2 at the end */
	break;
    case LISTSXP:
	env = NewEnvironment(R_NilValue, duplicate(CADR(args)), encl);
	PROTECT(env);
	break;
    case VECSXP:
	env = NewEnvironment(R_NilValue, VectorToPairList(CADR(args)), encl);
	PROTECT(env);
	break;
    case INTSXP:
    case REALSXP:
	nback = asInteger(env);
	if (nback==NA_INTEGER)
	    errorcall(call,"invalid environment");
	if (nback > 0 )
	    nback -= framedepth(R_GlobalContext);
	nback = -nback;
	PROTECT(env = R_sysframe(nback,R_GlobalContext));
	break;
    default:
	errorcall(call, "invalid second argument");
    }
    if(isLanguage(expr) || isSymbol(expr)) {
	PROTECT(expr);
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args);
	if (!SETJMP(cntxt.cjmpbuf))
	    expr = eval(expr, env);
	endcontext(&cntxt);
	UNPROTECT(1);
    }
    else if (isExpression(expr)) {
	int i, n;
	PROTECT(expr);
	n = LENGTH(expr);
	tmp = R_NilValue;
	begincontext(&cntxt, CTXT_RETURN, call, env, rho, args);
	if (!SETJMP(cntxt.cjmpbuf))
	    for(i=0 ; i<n ; i++)
		tmp = eval(VECTOR_ELT(expr, i), env);
	endcontext(&cntxt);
	UNPROTECT(1);
	expr = tmp;
    }
    if (PRIMVAL(op)) { /* eval.with.vis(*) : */
	PROTECT(expr);
	PROTECT(env = allocVector(VECSXP, 2));
	PROTECT(encl = allocVector(STRSXP, 2));
	SET_STRING_ELT(encl, 0, mkChar("value"));
	SET_STRING_ELT(encl, 1, mkChar("visible"));
	SET_VECTOR_ELT(env, 0, expr);
	SET_VECTOR_ELT(env, 1, ScalarLogical(R_Visible));
	setAttrib(env, R_NamesSymbol, encl);
	expr = env;
	UNPROTECT(3);
    }
    UNPROTECT(1);
    return expr;
}


SEXP do_recall(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;
    SEXP s, ans ;
    cptr = R_GlobalContext;
    /* get the args supplied */
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == rho)
	    break;
	cptr = cptr->nextcontext;
    }
    args = cptr->promargs;
    /* get the env recall was called from */
    s = R_GlobalContext->sysparent;
    while (cptr != NULL) {
	if (cptr->callflag == CTXT_RETURN && cptr->cloenv == s)
	    break;
	cptr = cptr->nextcontext;
    }
    if (cptr == NULL)
	error("Recall called from outside a closure");
    if( TYPEOF(CAR(cptr->call)) == SYMSXP)
	PROTECT(s = findFun(CAR(cptr->call), cptr->sysparent));
    else
	PROTECT(s = eval(CAR(cptr->call), cptr->sysparent));
    ans = applyClosure(cptr->call, s, args, cptr->sysparent, R_NilValue);
    UNPROTECT(1);
    return ans;
}


SEXP EvalArgs(SEXP el, SEXP rho, int dropmissing)
{
    if(dropmissing) return evalList(el, rho);
    else return evalListKeepMissing(el, rho);
}


/* DispatchOrEval is used in internal functions which dispatch to
 * object methods (e.g. "[" or "[[").  The code either builds promises
 * and dispatches to the appropriate method, or it evaluates the
 * (unevaluated) arguments it comes in with and returns them so that
 * the generic built-in C code can continue.

 * To call this an ugly hack would be to insult all existing ugly hacks
 * at large in the world.
 */
int DispatchOrEval(SEXP call, char *generic, SEXP args, SEXP rho,
		   SEXP *ans, int dropmissing)
{
#define AVOID_PROMISES_IN_DISPATCH_OR_EVAL
#ifdef AVOID_PROMISES_IN_DISPATCH_OR_EVAL
/* DispatchOrEval is called very frequently, most often in cases where
   no dispatching is needed and the isObject or the string-based
   pre-test fail.  To avoid degrading performance it is therefore
   necessary to avoid creating promises in these cases.  The pre-test
   does require that we look at the first argument, so that needs to
   be evaluated.  The complicating factor is that the first argument
   might come in with a "..." and that there might be other arguments
   in the "..." as well.  LT */

    SEXP x = R_NilValue;
    int dots = FALSE;

    /* Find the object to dispatch on, dropping any leading
       ... arguments with missing or empty values.  If there are no
       arguments, R_NilValue is used. */
    for (; args != R_NilValue; args = CDR(args)) {
	if (CAR(args) == R_DotsSymbol) {
	    SEXP h = findVar(R_DotsSymbol, rho);
	    if (TYPEOF(h) == DOTSXP) {
		/* just a consistency check */
		if (TYPEOF(CAR(h)) != PROMSXP)
		    error("value in ... is not a promise");
		dots = TRUE;
		x = eval(CAR(h), rho);
		break;
	    }
	    else if (h != R_NilValue && h != R_MissingArg)
		error("... used in an incorrect context");
	}
	else {
	    dots = FALSE;
	    x = eval(CAR(args), rho);
	    break;
	}
    }
    PROTECT(x);

    /* try to dispatch on the object */
    if( isObject(x)) {
	char *pt;
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || strcmp(pt,".default")) {
	    RCNTXT cntxt;
	    SEXP pargs;
	    PROTECT(pargs = promiseArgs(args, rho));
	    SET_PRVALUE(CAR(pargs), x);
	    begincontext(&cntxt, CTXT_RETURN, call, rho, rho, pargs);
	    if(usemethod(generic, x, call, pargs, rho, ans)) {
		endcontext(&cntxt);
		UNPROTECT(2);
		return 1;
	    }
	    endcontext(&cntxt);
	    UNPROTECT(1);
	}
    }

    if (dots)
	/* The first call argument was ... and may contain more than the
	   object, so it needs to be evaluated here.  The object should be
	   in a promise, so evaluating it again should be no problem. */
	*ans = EvalArgs(args, rho, dropmissing);
    else {
	*ans = CONS(x, EvalArgs(CDR(args), rho, dropmissing));
	SET_TAG(*ans, CreateTag(TAG(args)));
    }
    UNPROTECT(1);
#else
    SEXP x;
    RCNTXT cntxt;

    /* NEW */
    PROTECT(args = promiseArgs(args, rho));
    PROTECT(x = eval(CAR(args),rho));

    if( isObject(x)) {
	char *pt;
	if (TYPEOF(CAR(call)) == SYMSXP)
	    pt = strrchr(CHAR(PRINTNAME(CAR(call))), '.');
	else
	    pt = NULL;

	if (pt == NULL || strcmp(pt,".default")) {
	    /* PROTECT(args = promiseArgs(args, rho)); */
	    SET_PRVALUE(CAR(args), x);
	    begincontext(&cntxt, CTXT_RETURN, call, rho, rho, args);
	    if(usemethod(generic, x, call, args, rho, ans)) {
		endcontext(&cntxt);
		UNPROTECT(2);
		return 1;
	    }
	    endcontext(&cntxt);
	}
    }
    /* else PROTECT(args); */
    *ans = CONS(x, EvalArgs(CDR(args), rho, dropmissing));
    SET_TAG(*ans, CreateTag(TAG(args)));
    UNPROTECT(2);
#endif
    return 0;
}


/* gr needs to be protected on return from this function */
static void findmethod(SEXP class, char *group, char *generic,
		       SEXP *sxp,  SEXP *gr, SEXP *meth, int *which,
		       char *buf, SEXP rho)
{
    int len, whichclass;

    len = length(class);

    /* Need to interleave looking for group and generic methods */
    /* eg if class(x) is "foo" "bar" then x>3 should invoke */
    /* "Ops.foo" rather than ">.bar" */
    for (whichclass = 0 ; whichclass < len ; whichclass++) {
	sprintf(buf, "%s.%s", generic, CHAR(STRING_ELT(class, whichclass)));
	*meth = install(buf);
	*sxp = findVar(*meth, rho);
	if (isFunction(*sxp)) {
	    *gr = mkString("");
	    break;
	}
	sprintf(buf, "%s.%s", group, CHAR(STRING_ELT(class, whichclass)));
	*meth = install(buf);
	*sxp = findVar(*meth, rho);
	if (isFunction(*sxp)) {
	    *gr = mkString(group);
	    break;
	}
    }
    *which = whichclass;
}

int DispatchGroup(char* group, SEXP call, SEXP op, SEXP args, SEXP rho,
		  SEXP *ans)
{
    int i, j, nargs, lwhich, rwhich, set;
    SEXP lclass, s, t, m, lmeth, lsxp, lgr, newrho;
    SEXP rclass, rmeth, rgr, rsxp;
    char lbuf[512], rbuf[512], generic[128], *pt;

    /* pre-test to avoid string computations when there is nothing to
       dispatch on because either there is only one argument and it
       isn't an object or there are two or more arguments but neither
       of the first two is an object -- both of these cases would be
       rejected by the code following the string examination code
       below */
    if (args != R_NilValue && ! isObject(CAR(args)) &&
        (CDR(args) == R_NilValue || ! isObject(CADR(args))))
	return 0;

    /* check whether we are processing the default method */
    if ( isSymbol(CAR(call)) ) {
	sprintf(lbuf, "%s", CHAR(PRINTNAME(CAR(call))) );
	pt = strtok(lbuf, ".");
	pt = strtok(NULL, ".");

	if( pt != NULL && !strcmp(pt, "default") )
	    return 0;
    }

    if( !strcmp(group, "Ops") )
	nargs = length(args);
    else
	nargs = 1;

    if( nargs == 1 && !isObject(CAR(args)) )
	return 0;

    if(!isObject(CAR(args)) && !isObject(CADR(args)))
	return 0;

    sprintf(generic, "%s", PRIMNAME(op) );

    lclass = getAttrib(CAR(args), R_ClassSymbol);

    if( nargs == 2 )
	rclass = getAttrib(CADR(args), R_ClassSymbol);
    else
	rclass = R_NilValue;

    lsxp = R_NilValue; lgr = R_NilValue; lmeth = R_NilValue;
    rsxp = R_NilValue; rgr = R_NilValue; rmeth = R_NilValue;

    findmethod(lclass, group, generic, &lsxp, &lgr, &lmeth, &lwhich,
	       lbuf, rho);
    PROTECT(lgr);

    if( nargs == 2 )
	findmethod(rclass, group, generic, &rsxp, &rgr, &rmeth,
		   &rwhich, rbuf, rho);
    else
	rwhich=0;

    PROTECT(rgr);

    if( !isFunction(lsxp) && !isFunction(rsxp) ) {
	UNPROTECT(2);
	return 0; /* no generic or group method so use default*/
    }

    if( lsxp!=rsxp ) {
	if( isFunction(lsxp) && isFunction(rsxp) ) {
	    warning("Incompatible methods (\"%s\", \"%s\") for \"%s\"",
		    CHAR(PRINTNAME(lmeth)), CHAR(PRINTNAME(rmeth)), generic);
	    UNPROTECT(2);
	    return 0;
	}
	/* if the right hand side is the one */
	if( !isFunction(lsxp) ) { /* copy over the righthand stuff */
	    lsxp=rsxp;
	    lmeth=rmeth;
	    lgr=rgr;
	    lclass=rclass;
	    lwhich=rwhich;
	    strcpy(lbuf, rbuf);
	}
    }

    /* we either have a group method or a class method */

    PROTECT(newrho = allocSExp(ENVSXP));
    PROTECT(m = allocVector(STRSXP,nargs));
    s = args;
    for (i = 0 ; i < nargs ; i++) {
	t = getAttrib(CAR(s), R_ClassSymbol);
	set = 0;
	if (isString(t)) {
	    for (j = 0 ; j < length(t) ; j++) {
		if (!strcmp(CHAR(STRING_ELT(t, j)),
			    CHAR(STRING_ELT(lclass, lwhich)))) {
		    SET_STRING_ELT(m, i, mkChar(lbuf));
		    set = 1;
		    break;
		}
	    }
	}
	if( !set )
	    SET_STRING_ELT(m, i, R_BlankString);
	s = CDR(s);
    }

    defineVar(install(".Method"), m, newrho);
    UNPROTECT(1);
    PROTECT(t=mkString(generic));
    defineVar(install(".Generic"), t, newrho);
    UNPROTECT(1);
    defineVar(install(".Group"), lgr, newrho);
    set=length(lclass)-lwhich;
    PROTECT(t = allocVector(STRSXP, set));
    for(j=0 ; j<set ; j++ )
	SET_STRING_ELT(t, j, duplicate(STRING_ELT(lclass, lwhich++)));
    defineVar(install(".Class"), t, newrho);
    UNPROTECT(1);

    PROTECT(t = LCONS(lmeth,CDR(call)));

    /* the arguments have been evaluated; since we are passing them */
    /* out to a closure we need to wrap them in promises so that */
    /* they get duplicated and things like missing/substitute work. */

    PROTECT(s = promiseArgs(CDR(call), rho));
    if (length(s) != length(args))
	errorcall(call,"dispatch error");
    for (m = s ; m != R_NilValue ; m = CDR(m), args = CDR(args) )
	SET_PRVALUE(CAR(m), CAR(args));

    *ans = applyClosure(t, lsxp, s, rho, newrho);
    UNPROTECT(5);
    return 1;
}
