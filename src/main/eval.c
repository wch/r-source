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

/* move to Rinternals.h eventually */
R_code_t R_applyClosure_nr(SEXP, SEXP, SEXP, SEXP, SEXP, R_code_t);
static SEXP processArgs(SEXP, SEXP, Rboolean);

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

/*
 * Non-recursive evaluations of promises
 */

/* If a promise needs to be forced, then we need to set the current
   environment to the promise's environment and pass its code to the
   non-recursive evaluator.  After the evaluation, the old environment
   needs to be restored and the promise needs to be updated. */

/* Promise evaluation uses a frame with the promise in EVAL_FRAME_VAR0
   and the current environment in EVAL_FRAME_VAR1 */

#define PUSH_PROMISE_FRAME(code, p) PUSH_EVAL_FRAME(code, p, R_CurrentEnv)
#define PROMISE_FRAME_PROMISE EVAL_FRAME_VAR0
#define PROMISE_FRAME_OLDENV EVAL_FRAME_VAR1

DECLARE_CONTINUATION(promise_cont, promise_cont_fun);

static R_code_t eval_promise_nr(SEXP e, R_code_t code)
{
    SEXP val;
    if (PRVALUE(e) == R_UnboundValue) {
	if(PRSEEN(e))
	    errorcall(R_GlobalContext->call,
		      "recursive default argument reference");
	SET_PRSEEN(e, 1);
	PUSH_PROMISE_FRAME(code, e);
	R_CurrentEnv = PRENV(e);
	return R_eval_nr(PREXPR(e), promise_cont);
    }
    else {
	val = PRVALUE(e);
	R_ReturnedValue = val;
	if (NAMED(val) != 2) SET_NAMED(val, 2);
	return code;
    }
}

static R_code_t promise_cont_fun(R_code_t code)
{
    SEXP e = PROMISE_FRAME_PROMISE();
    SEXP val = R_ReturnedValue;

    R_CurrentEnv = PROMISE_FRAME_OLDENV();
    code = EVAL_FRAME_CODE();
    POP_EVAL_FRAME();
    SET_PRSEEN(e, 0);
    SET_PRVALUE(e, val);
    if (NAMED(val) != 2) SET_NAMED(val, 2);
    return code;
}

static void callBuiltin(SEXP call, SEXP op, SEXP args, Rboolean doprof)
{
    int save;
    SEXP rho = R_CurrentEnv;

    PROTECT(args);
    PROTECT(op);
    save = R_PPStackTop;
    R_Visible = 1 - PRIMPRINT(op);

#ifdef R_PROFILING
    if (R_Profiling && doprof) {
	begincontext(CTXT_BUILTIN, call, R_NilValue, R_NilValue, R_NilValue);
	R_ReturnedValue = PRIMFUN(op) (call, op, args, rho);
	endcontext();
    } else {
#endif
	R_ReturnedValue = PRIMFUN(op) (call, op, args, rho);
#ifdef R_PROFILING
    }
#endif
    if(save != R_PPStackTop) {
	Rprintf("stack imbalance in %s, %d then %d\n",
		PRIMNAME(op), save, R_PPStackTop);
    }
    UNPROTECT(2);
}

/*
 * Non-recursive evaluation of BUILTIN calls.
 */

/* To evaluate a BUILTIN call non-recursively, we need to evaluate its
   argument list non-recursively and then call the BUILTIN. */

/* The evaluation frame needs to hold four values: the list of
   argument expressions (which is destructively modifies to a list of
   values), the argument expressions that still need processing (this
   is updated at each step), the call, and the op. The frame stores
   the argument remaining arguments in EVAL_FRAME_VAR0 and the other
   three in a CONS cell in EVAL_FRAME_VAR1, with the args in the TAG
   field, call in the CAR, and op in the CDR.  In addition, the
   profiling option is stored in the sxpinfo.gp field. */

#define BUILTIN_FRAME_EVARGS() TAG(EVAL_FRAME_VAR1())

#define BUILTIN_FRAME_ARGS EVAL_FRAME_VAR0
#define SET_BUILTIN_FRAME_ARGS SET_EVAL_FRAME_VAR0

#define BUILTIN_FRAME_CALL() CAR(EVAL_FRAME_VAR1())
#define BUILTIN_FRAME_OP() CDR(EVAL_FRAME_VAR1())

#define BUILTIN_FRAME_DOPROF() (EVAL_FRAME_VAR1()->sxpinfo.gp)

/**** use frame of 4 + one int */
#define PUSH_BUILTIN_FRAME(code, call, op, args, doprof) do { \
    SEXP __args__ = (args); \
    SEXP __ci__; \
    PROTECT(__args__); \
    PROTECT(__ci__ = CONS(call, op)); \
    SET_TAG(__ci__, __args__); \
    __ci__->sxpinfo.gp = (doprof) ? 1 : 0; \
    PUSH_EVAL_FRAME(code, __args__, __ci__); \
    UNPROTECT(2); \
} while (0)

DECLARE_CONTINUATION(eval_builtin_cont, eval_builtin_cont_fun);

static R_code_t eval_builtin_nr(SEXP call, SEXP op, SEXP args, R_code_t code,
				Rboolean doprof)
{
    if (args == R_NilValue) {
	CCODE_NR fun_nr = PRIMFUN_NR(op);
	if (fun_nr != NULL) {
	    R_Visible = 1 - PRIMPRINT(op);
	    return fun_nr(call, op, R_NilValue, R_CurrentEnv, code);
	}
	else {
	    callBuiltin(call, op, R_NilValue, doprof);
	    return code;
	}
    }
    else {
	PUSH_BUILTIN_FRAME(code, call, op, args, doprof);
	return R_eval_nr(CAR(args), eval_builtin_cont);
    }
}

static R_code_t eval_builtin_cont_fun(R_code_t code)
{
    SEXP args = BUILTIN_FRAME_ARGS();

    SETCAR(args, R_ReturnedValue);
    args = CDR(args);

    if (args == R_NilValue) {
	SEXP evargs = BUILTIN_FRAME_EVARGS();
	SEXP call = BUILTIN_FRAME_CALL();
	SEXP op = BUILTIN_FRAME_OP();
	Rboolean doprof = BUILTIN_FRAME_DOPROF();
	CCODE_NR fun_nr = PRIMFUN_NR(op);

	if (fun_nr != NULL) {
	    R_Visible = 1 - PRIMPRINT(op);
	    code = EVAL_FRAME_CODE();	
	    POP_EVAL_FRAME();
	    /**** fun_nr must protect anything that needs protecting! */
	    /**** also there will be no profiling and no PP stack check */
	    return fun_nr(call, op, evargs, R_CurrentEnv, code);
	}
	else {
	    callBuiltin(call, op, evargs, doprof);
	    code = EVAL_FRAME_CODE();	
	    POP_EVAL_FRAME();
	    return code;
	}
    }
    else {
	SET_BUILTIN_FRAME_ARGS(args);
	return R_eval_nr(CAR(args), eval_builtin_cont);
    }
}

static R_code_t eval_special_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
				R_code_t code)
{
    CCODE_NR fun_nr = PRIMFUN_NR(op);
    R_Visible = 1 - PRIMPRINT(op);
    if (fun_nr != NULL)
	return fun_nr(call, op, args, rho, code);
    else {
	PROTECT(op);
	R_ReturnedValue = PRIMFUN(op) (call, op, args, rho);
	UNPROTECT(1);
	return code;
    }
}

R_code_t R_eval_nr(SEXP e, R_code_t code)
{
    SEXP op, tmp;
    SEXP rho = R_CurrentEnv;

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
    case EXPRSXP:
	/* Make sure constants in expressions are NAMED before being
           used as values.  Setting NAMED to 2 makes sure weird calls
           to assignment functions won't modify constants in
           expressions.  */
	if (NAMED(e) != 2) SET_NAMED(e, 2);
	R_ReturnedValue = e;
	return code;
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
	else if (TYPEOF(tmp) == PROMSXP)
	    return eval_promise_nr(tmp, code);
	else if (!isNull(tmp) && NAMED(tmp) < 1)
	    SET_NAMED(tmp, 1);
	R_ReturnedValue = tmp;
	return code;
    case PROMSXP:
	return eval_promise_nr(e, code);
    case LANGSXP:
	/***** temporarily leave this recursive since it very rarely
               is a significant eval and is a pain to handle (but fix
               eventually) */
	if (TYPEOF(CAR(e)) == SYMSXP)
	    PROTECT(op = findFun(CAR(e), rho));
	else
	    PROTECT(op = eval(CAR(e), rho));
	if(TRACE(op)) {
	    Rprintf("trace: ");
	    PrintValue(e);
	}
	if (TYPEOF(op) == SPECIALSXP) {
	    UNPROTECT(1);  /* need to unprotect op here */
	    return eval_special_nr(e, op, CDR(e), rho, code);
	}
	else if (TYPEOF(op) == BUILTINSXP) {
	    SEXP args = processArgs(CDR(e), rho, FALSE);
	    UNPROTECT(1);  /* need to unprotect op here */
	    return eval_builtin_nr(e, op, args, code, TRUE);
	}
	else if (TYPEOF(op) == CLOSXP) {
	    tmp = promiseArgs(CDR(e), rho);
	    UNPROTECT(1);  /* need to unprotect op here */
	    return R_applyClosure_nr(e, op, tmp, rho, R_NilValue, code);
	}
	else {
	    tmp = NULL;  /* keep -Wall happy */
	    error("attempt to apply non-function");
	}
	UNPROTECT(1);
	R_ReturnedValue = tmp;
	return code;
    case DOTSXP:
	error("... used in an incorrect context");
    default:
	UNIMPLEMENTED("eval");
	return NULL; /* not reached */
    }
}

void R_run_dispatcher(R_code_t code)
{
    while (code != NULL) {
	/* check for a user abort and an expired quantum */
	R_EvalCount++;
	if ((R_EvalCount % 128) == 0) {
#ifdef Macintosh
	    isintrpt();
#endif
#ifdef Win32
	    R_ProcessEvents();
#endif
	    if (R_threads_enabled && R_preemptive_scheduling) {
		if (R_EvalCount > R_ThreadContext->quantum) {
		    R_code_t R_ThreadYield(R_code_t);
		    R_EvalCount = 0;
		    code = R_ThreadYield(code);
		}
	    }
	    else R_EvalCount = 0;
	}
	code = code->fun(code);
    }
}
    
SEXP eval(SEXP e, SEXP rho) {
    volatile int save = R_PPStackTop;
    R_code_t code;
    volatile SEXP oldrho;
    RDISPATCHER disp;

    PROTECT(oldrho = R_CurrentEnv);
    PROTECT(e);
    R_CurrentEnv = rho;

    R_BeginDispatcher(&disp);
    if (SETJMP(disp.cjmpbuf))
	code = R_GlobalContext->contcode;
    else
	code = R_eval_nr(e, NULL);
    if (code != NULL)
	R_run_dispatcher(code);
    R_EndDispatcher(&disp);

    UNPROTECT(2);
    R_CurrentEnv = oldrho;
    if(save != R_PPStackTop)
	Rprintf("stack imbalance, %d then %d\n", save, R_PPStackTop);
    return R_ReturnedValue;
}

/* Apply SEXP op of type CLOSXP to actuals */

static SEXP buildCallEnv(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			 SEXP suppliedenv)
{
    SEXP formals, actuals, savedrho, newrho;
    SEXP f, a, tmp;

    /* formals = list of formal parameters */
    /* actuals = values to be bound to formals */
    /* arglist = the tagged list of arguments */

    formals = FORMALS(op);
    savedrho = CLOENV(op);

    /*  Set up a context with the call in it so error has access to it */

    begincontext(CTXT_RETURN, call, savedrho, rho, arglist);

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

    SET_DEBUG(newrho, DEBUG(op));

    /* Terminate the previous context; the call will start a new one
        with the correct environment. */
    UNPROTECT(2);
    endcontext();
    return newrho;
}

static void debugClosureCallStart(SEXP call, SEXP op, SEXP arglist, SEXP rho,
				  SEXP newrho)
{
    SEXP body, tmp;

    body = BODY(op);

    Rprintf("debugging in: ");
    PrintValueRec(call, rho);

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
	return;
    Rprintf("debug: ");
    PrintValue(body);
    do_browser(call,op,arglist,newrho);
}

static void debugClosureCallEnd(SEXP call, SEXP rho)
{
    int savevis = R_Visible;
    SEXP saveval = R_ReturnedValue;
    PROTECT(saveval);
    Rprintf("exiting from: ");
    PrintValueRec(call, rho);
    UNPROTECT(1);
    R_ReturnedValue = saveval;
    R_Visible = savevis;
}

static SEXP ClosureCallSysparent(SEXP rho)
{
    /*  If we have a generic function we need to use the sysparent of
	the generic as the sysparent of the method because the method
	is a straight substitution of the generic.  */

    if( R_GlobalContext->callflag == CTXT_GENERIC )
	return R_GlobalContext->sysparent;
    else
	return rho;
}
    
SEXP applyClosure(SEXP call, SEXP op, SEXP arglist, SEXP rho, SEXP suppliedenv)
{
    SEXP body, sysp;
    volatile SEXP newrho;
    RDISPATCHER disp;

    R_EvalDepth++;
    if (R_EvalDepth > R_Expressions)
	error("evaluation is nested too deeply: infinite recursion?");

    body = BODY(op);

    PROTECT(arglist);
    newrho = buildCallEnv(call, op, arglist, rho, suppliedenv);
    UNPROTECT(1);

    sysp = ClosureCallSysparent(rho);

    R_BeginDispatcher(&disp);
    begincontext(CTXT_RETURN, call, newrho, sysp, arglist);

    /* Debugging */
    if (DEBUG(op))
	debugClosureCallStart(call, op, arglist, rho, newrho);

    /*  Set a longjmp target which will catch any explicit returns
	from the function body.  */

    R_ReturnedValue = R_NilValue;
    if (SETJMP(disp.cjmpbuf)) {
	if (R_ReturnedValue == R_RestartToken) {
	    R_GlobalContext->callflag = CTXT_RETURN;  /* turn restart off */
	    R_ReturnedValue = R_NilValue;  /* remove restart token */
	    R_ReturnedValue = eval(body, newrho);
	}
    }
    else
	R_ReturnedValue = eval(body, newrho);

    endcontext();
    R_EndDispatcher(&disp);

    if (DEBUG(op))
	debugClosureCallEnd(call, rho);

    R_EvalDepth--;
    return R_ReturnedValue;
}


/*
 * Non-recursive version of closure calling
 */

/* Before the call we need to set up the environment and increment the call
   depth.  After the call this neds to be undone. */

/* The frame fpr closure calls contains three elements: The call, the
   function called (op), and the previous environment. */

#define CLOSURE_FRAME_CALL() CAR(EVAL_FRAME_VAR0())
#define CLOSURE_FRAME_OP() CDR(EVAL_FRAME_VAR0())
#define CLOSURE_FRAME_OLDENV() EVAL_FRAME_VAR1()

/**** use frame size 3 here */
#define PUSH_CLOSURE_FRAME(call, op, rho) do { \
    PROTECT(op); \
    PUSH_EVAL_FRAME(code, CONS(call, op), rho); \
    UNPROTECT(1); \
} while (0)

/* The closure call context includes a continuation code for `return'
   and restart jumps. */
#define BEGIN_CLOSURE_CONTEXT(call, arglist, rho) do { \
    SEXP sysp = ClosureCallSysparent(rho); \
    begincontext(CTXT_RETURN, call, R_CurrentEnv, sysp, arglist); \
    R_GlobalContext->contcode = applyClosure_cont; \
} while (0)


DECLARE_CONTINUATION(applyClosure_cont, applyClosure_cont_fun);

R_code_t R_applyClosure_nr(SEXP call, SEXP op, SEXP arglist, SEXP rho,
			   SEXP suppliedenv, R_code_t code)
{
    R_EvalDepth++;
    if (R_EvalDepth > R_Expressions)
	error("evaluation is nested too deeply: infinite recursion?");

    PROTECT(arglist);
    PUSH_CLOSURE_FRAME(call, op, R_CurrentEnv); /* protects op */
    R_CurrentEnv = buildCallEnv(call, op, arglist, rho, suppliedenv);
    UNPROTECT(1); /* arglist - will be protected in the context stack */

    BEGIN_CLOSURE_CONTEXT(call, arglist, rho);
 
    if (DEBUG(op))
	debugClosureCallStart(call, op, arglist, rho, R_CurrentEnv);

    return R_eval_nr(BODY(op), applyClosure_cont);
}

static R_code_t applyClosure_cont_fun(R_code_t code)
{
    if (R_ReturnedValue == R_RestartToken) {
	R_GlobalContext->callflag = CTXT_RETURN;  /* turn restart off */
	R_ReturnedValue = R_NilValue;  /* remove restart token */
	return R_eval_nr(BODY(CLOSURE_FRAME_OP()), applyClosure_cont);
    }
    else {
	if (DEBUG(CLOSURE_FRAME_OP()))
	    debugClosureCallEnd(CLOSURE_FRAME_CALL(), R_CurrentEnv);

	endcontext();

	R_CurrentEnv = CLOSURE_FRAME_OLDENV();

	code = EVAL_FRAME_CODE();
	POP_EVAL_FRAME();
	R_EvalDepth--;
	return code;
    }
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


/* It might be a tad more efficient to make the non-error part of this
   into a macro, especially for while loops. */
static Rboolean asLogicalNoNA(SEXP s, SEXP call)
{
    Rboolean cond = asLogical(s);
    if (cond == NA_LOGICAL) {
	char *msg = isLogical(s) ?
	    "missing value where logical needed" :
	    "argument is not interpretable as logical";
	errorcall(call, msg);
    }
    return cond;
}
    

SEXP do_if(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP Cond = eval(CAR(args), rho);

    if (asLogicalNoNA(Cond, call))
	return (eval(CAR(CDR(args)), rho));
    else if (length(args) > 2)
	return (eval(CAR(CDR(CDR(args))), rho));
    R_Visible = 0;
    return R_NilValue;
}


/*
 * Non-recursive version of do_if
 */

/* Just to make some code a little clearer: */
#define CALL_ARGS(c) CDR(c)

/* `if' calls look like (if test consequent alternative); the
   alternative is optional */
#define IF_CALL_TEST(c) CAR(CALL_ARGS(c))
#define IF_CALL_CONSEQUENT(c) CADR(CALL_ARGS(c))
#define IF_CALL_ALTERNATIVE(c) CADDR(CALL_ARGS(c))
#define IF_CALL_HAS_ALTERNATIVE(c) (CDDR(CALL_ARGS(c)) != R_NilValue)

/* do_if_nr calls the non-recursive evaluator on the test expression.
   The continuation examines the result and then evaluates either the
   consequent or the alternative (or signals an error). */

/* do_if_nr saves the call in the frame as EVAL_FRAME_VAR0 */
#define IF_FRAME_CALL EVAL_FRAME_VAR0
#define PUSH_IF_FRAME(code, call)  PUSH_EVAL_FRAME(code, call, R_NilValue);

DECLARE_CONTINUATION(do_if_cont, do_if_cont_fun);

R_code_t do_if_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    PUSH_IF_FRAME(code, call);
    return R_eval_nr(IF_CALL_TEST(call), do_if_cont);
}

static R_code_t do_if_cont_fun(R_code_t code)
{
    SEXP Cond = R_ReturnedValue;
    SEXP call = IF_FRAME_CALL();
    code = EVAL_FRAME_CODE();
    POP_EVAL_FRAME();

    if (asLogicalNoNA(Cond, call))
	return R_eval_nr(IF_CALL_CONSEQUENT(call), code);
    else if (IF_CALL_HAS_ALTERNATIVE(call))
	return R_eval_nr(IF_CALL_ALTERNATIVE(call), code);
    else {
	R_Visible = 0;
	R_ReturnedValue = R_NilValue;
	return code;
    }
}


#define BodyHasBraces(body) \
    ((isLanguage(body) && CAR(body) == R_BraceSymbol) ? 1 : 0)

#define DO_LOOP_DEBUG(call, op, args, rho, bgn) do { \
    if (bgn && DEBUG(rho)) { \
	Rprintf("debug: "); \
	PrintValue(CAR(args)); \
	do_browser(call,op,args,rho); \
    } } while (0)

static SEXP set_for_loop_var(SEXP val, int i, SEXP sym, SEXP call, SEXP rho)
{
    SEXP v;

    switch (TYPEOF(val)) {
    case LGLSXP:
	PROTECT(v = allocVector(TYPEOF(val), 1));
	LOGICAL(v)[0] = LOGICAL(val)[i];
	setVar(sym, v, rho);
	UNPROTECT(1);
	break;
    case INTSXP:
	PROTECT(v = allocVector(TYPEOF(val), 1));
	INTEGER(v)[0] = INTEGER(val)[i];
	setVar(sym, v, rho);
	UNPROTECT(1);
	break;
    case REALSXP:
	PROTECT(v = allocVector(TYPEOF(val), 1));
	REAL(v)[0] = REAL(val)[i];
	setVar(sym, v, rho);
	UNPROTECT(1);
	break;
    case CPLXSXP:
	PROTECT(v = allocVector(TYPEOF(val), 1));
	COMPLEX(v)[0] = COMPLEX(val)[i];
	setVar(sym, v, rho);
	UNPROTECT(1);
	break;
    case STRSXP:
	PROTECT(v = allocVector(TYPEOF(val), 1));
	SET_STRING_ELT(v, 0, STRING_ELT(val, i));
	setVar(sym, v, rho);
	UNPROTECT(1);
	break;
    case EXPRSXP:
    case VECSXP:
	setVar(sym, VECTOR_ELT(val, i), rho);
	break;
    case LISTSXP:
    case LANGSXP:
	setVar(sym, CAR(val), rho);
	val = CDR(val);
	break;
    default: errorcall(call, "bad for loop sequence");
    }

    return val;
}

SEXP do_for(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    volatile int dbg;
    volatile int i, n, bgn;
    volatile SEXP sym, body;
    volatile SEXP ans, val;
    PROTECT_INDEX api;
    RDISPATCHER disp;

    checkArity(op, args);

    sym = CAR(args);
    val = CADR(args);
    body = CADDR(args);

    if ( !isSymbol(sym) ) errorcall(call, "non-symbol loop variable");

    PROTECT(val = eval(val, rho));
    defineVar(sym, R_NilValue, rho);
    if (isList(val) || isLanguage(val) || isNull(val))
	n = length(val);
    else
	n = LENGTH(val);
    ans = R_NilValue;

    dbg = DEBUG(rho);
    bgn = BodyHasBraces(body);

    PROTECT_WITH_INDEX(ans, &api);
    R_BeginDispatcher(&disp);
    begincontext(CTXT_LOOP, R_NilValue, rho, R_NilValue, R_NilValue);
    switch (SETJMP(disp.cjmpbuf)) {
    case CTXT_BREAK: R_ReturnedValue = R_NilValue; goto for_break;
    case CTXT_NEXT: R_ReturnedValue = R_NilValue; goto for_next;
    }
    for (i = 0; i < n; i++) {
	SEXP v;
	DO_LOOP_DEBUG(call, op, args, rho, bgn);
	val = set_for_loop_var(val, i, sym, call, rho); /* only changes for {LIST,LANG}SXP */
	REPROTECT(ans = eval(body, rho), api);
    for_next:
	; /* needed for strict ISO C compilance, according to gcc 2.95.2 */
    }
 for_break:
    endcontext();
    R_EndDispatcher(&disp);
    UNPROTECT(2);
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return ans;
}


/*
 * Common structure for non-recursive loop handling
 */

/* All loops use a common frame structure.  This is a little wasteful
   for `while' and `repeat' loops, since `for' loops need one extra
   field to hold the evaluated sequence, but it makes things simpler.

   EVAL_FRAME_VAR0 holds the current return value of the loop.  It
   starts out at R_NilValue and then receives the result of each body
   evaluation that is not terminated with a `next' or `break'. */

#define LOOP_VALUE EVAL_FRAME_VAR0
#define SET_LOOP_VALUE SET_EVAL_FRAME_VAR0

#define UPDATE_LOOP_VALUE() do { \
    if (R_ReturnedValue == R_LoopNextToken) R_ReturnedValue = R_NilValue; \
    else SET_LOOP_VALUE(R_ReturnedValue); \
} while (0)

/* EVAL_FRAME_VAR1 holds the SEXP values needed by the loops.  Its
   structure is

   (ivars seqval call . op)

   The seqval field is only used by `for' loops.  The ivars field holds
   an integer vector of length at least 2. */

#define LOOP_IVARS() INTEGER(CAR(EVAL_FRAME_VAR1()))

#define LOOP_SEQVAL() CADR(EVAL_FRAME_VAR1())
#define SET_LOOP_SEQVAL(v) SETCAR(CDR(EVAL_FRAME_VAR1()), v)

#define LOOP_CALL() CADDR(EVAL_FRAME_VAR1())

#define LOOP_OP() CDR(CDR(CDR(EVAL_FRAME_VAR1())))

/* The two integer values needed by all loops are the DEBUG value
   of the current environent at the loop start and the flag `bgn'
   that is true if the body of the loop has braces. */

#define LOOP_DBG() (LOOP_IVARS()[0])
#define SET_LOOP_DBG(v) (LOOP_DBG() = (v))

#define LOOP_BGN() (LOOP_IVARS()[1])
#define SET_LOOP_BGN(v) (LOOP_BGN() = (v))

/* The loop frame is created by the macro PUSH_LOOP_FRAME.  The
   `seqval' field is initialized to R_NilValue, as is the saved loop
   value field.  The final argument to this macro specifies the number
   of additional integer fields needed.  For `for' loops this will be
   2; for `while' and `repeat' loops it will be 0. */

#define PUSH_LOOP_FRAME(code, call, op, extra) do { \
    SEXP __op__ = (op), __call__ = (call), __iv__, __ci__; \
    PROTECT(__op__); \
    PROTECT(__iv__ = allocVector(INTSXP, 2 + (extra))); \
    __ci__ = CONS(__iv__, CONS(R_NilValue, CONS(__call__, __op__))); \
    PUSH_EVAL_FRAME(code, R_NilValue, __ci__); \
    UNPROTECT(2); \
} while(0)

/* All loops need to set a continuation for their contexts that is
   used by `break' and `next' calls */
#define BEGIN_LOOP_CONTEXT(cont) do { \
    begincontext(CTXT_LOOP, R_NilValue, R_CurrentEnv, R_NilValue, R_NilValue);\
    R_GlobalContext->contcode = (cont); \
} while (0)


/* All non-recursive loop are terminated by calling finis_loop_nr */
static R_code_t finish_loop_nr(void)
{
    R_code_t code = EVAL_FRAME_CODE();
    int dbg = LOOP_DBG();

    R_ReturnedValue = LOOP_VALUE();

    endcontext();

    SET_DEBUG(R_CurrentEnv, dbg);

    POP_EVAL_FRAME();
    R_Visible = 0;
    return code;
}


/*
for (i in 1:10) print(i)
for (i in quote(1+2)) print(i) 
*/

/* 
 * Non-recursive verion of do_for
 */

/* `for' loop calls look like (for sym seq body) */
#define FOR_LOOP_CALL_SYMBOL(c) CAR(CALL_ARGS(c))
#define FOR_LOOP_CALL_SEQUENCE(c) CADR(CALL_ARGS(c))
#define FOR_LOOP_CALL_BODY(c) CADDR(CALL_ARGS(c))

/* the evaluated sequence is stored in the `seqval' slot of the loop frame */
#define FOR_LOOP_SEQVAL LOOP_SEQVAL
#define SET_FOR_LOOP_SEQVAL SET_LOOP_SEQVAL

/* the current sequence index is stored in integer loop variable 2 */
#define FOR_LOOP_INDEX() (LOOP_IVARS()[2])
#define SET_FOR_LOOP_INDEX(v) (FOR_LOOP_INDEX() = (v))

/* the sequence length is stored in integer loop variable 3 */
#define FOR_LOOP_SEQLEN() (LOOP_IVARS()[3])
#define SET_FOR_LOOP_SEQLEN(v) (FOR_LOOP_SEQLEN() = (v))

/* `for' loops need two additional integer variables */
#define PUSH_FOR_LOOP_FRAME(code,call,op) PUSH_LOOP_FRAME(code,call,op,2)

/* set_for_loop_var_nr - update the loop variable using information
   from the loop call frame, and update the sequence value if
   necessary (for LISTSXP/LANGSXP sequences only) */
static void set_for_loop_var_nr(SEXP call, int i)
{
    SEXP val = FOR_LOOP_SEQVAL();
    SEXP sym = FOR_LOOP_CALL_SYMBOL(call);
    SEXP newval = set_for_loop_var(val, i, sym, call, R_CurrentEnv);
    if (newval != val)
	SET_FOR_LOOP_SEQVAL(newval);
}


DECLARE_CONTINUATION(do_for_seq_cont, do_for_seq_cont_fun);
DECLARE_CONTINUATION(do_for_body_cont, do_for_body_cont_fun);

/* do_for_nr - start the for loop evaluation. Sets up the frame and
   evaluates the sequence expression. */
R_code_t do_for_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    SEXP ivals, callinfo;

    checkArity(op, args);

    if (! isSymbol(FOR_LOOP_CALL_SYMBOL(call)))
	errorcall(call, "non-symbol loop variable");

    PUSH_FOR_LOOP_FRAME(code, call, op);

    return R_eval_nr(FOR_LOOP_CALL_SEQUENCE(call), do_for_seq_cont);
}

/* continuation for the initial sequence evaluation */
static R_code_t do_for_seq_cont_fun(R_code_t code)
{
    int n;
    SEXP call = LOOP_CALL();
    SEXP op = LOOP_OP();
    SEXP val = R_ReturnedValue;
    SEXP sym = FOR_LOOP_CALL_SYMBOL(call);
    SEXP body = FOR_LOOP_CALL_BODY(call);
    int dbg = DEBUG(R_CurrentEnv);
    int bgn = BodyHasBraces(body);
    int i = 0;

    SET_FOR_LOOP_SEQVAL(val);

    if (isList(val) || isLanguage(val) || isNull(val))
	n = length(val);
    else
	n = LENGTH(val);

    defineVar(sym, R_NilValue, R_CurrentEnv);

    SET_LOOP_DBG(dbg);
    SET_LOOP_BGN(bgn);
    SET_FOR_LOOP_INDEX(i);
    SET_FOR_LOOP_SEQLEN(n);

    BEGIN_LOOP_CONTEXT(do_for_body_cont);

    R_ReturnedValue = R_NilValue;
    if (n > 0) {
	set_for_loop_var_nr(call, i);
	DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
	return R_eval_nr(body, do_for_body_cont);
    }
    else
	return finish_loop_nr();
}

/* continuation for body evaluations; also for break/next jumps */
static R_code_t do_for_body_cont_fun(R_code_t code)
{
    if (R_ReturnedValue == R_LoopBreakToken)
	return finish_loop_nr();
    else {
	int i = FOR_LOOP_INDEX();
	int n = FOR_LOOP_SEQLEN();

	UPDATE_LOOP_VALUE();

	i = i + 1;

	if (i < n) {
	    SEXP call = LOOP_CALL();
	    SEXP op = LOOP_OP();
	    int bgn = LOOP_BGN();

	    SET_FOR_LOOP_INDEX(i);
	    set_for_loop_var_nr(call, i);
	    DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
	    return R_eval_nr(FOR_LOOP_CALL_BODY(call), do_for_body_cont);
	}
	else
	    return finish_loop_nr();
    }
}


SEXP do_while(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP t, body;
    PROTECT_INDEX tpi;
    RDISPATCHER disp;

    checkArity(op, args);

    dbg = DEBUG(rho);
    body = CADR(args);
    bgn = BodyHasBraces(body);

    t = R_NilValue;
    PROTECT_WITH_INDEX(t, &tpi);
    R_BeginDispatcher(&disp);
    begincontext(CTXT_LOOP, R_NilValue, rho, R_NilValue, R_NilValue);
    if (SETJMP(disp.cjmpbuf) != CTXT_BREAK) {
	R_ReturnedValue = R_NilValue; 
	while (asLogicalNoNA(eval(CAR(args), rho), call)) {
	    DO_LOOP_DEBUG(call, op, args, rho, bgn);
	    REPROTECT(t = eval(body, rho), tpi);
	}
    }
    endcontext();
    R_EndDispatcher(&disp);
    UNPROTECT(1);
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return t;
}


/*
f<-function(m) { n<-1; while (T) { print(n); n <- n+1; if (n > m) break }; 2 }
g<-function(m) { n<-1; while(T) { print(n); n <- n+1; if (n > m) break; n}}
h<-function(m) { n<-1; while(n <= m) { print(n); n <- n+1 }}
*/

/* 
 * Non-recursive verion of do_while
 */

/* `while' loop calls look like (while test body) */
#define WHILE_LOOP_CALL_BODY(c) CADDR(c)
#define WHILE_LOOP_CALL_TEST(c) CADR(c)

/* `while' loops need no additional integer variables */
#define PUSH_WHILE_LOOP_FRAME(code,call,op) PUSH_LOOP_FRAME(code,call,op,0)

DECLARE_CONTINUATION(do_while_test_cont, do_while_test_cont_fun);
DECLARE_CONTINUATION(do_while_body_cont, do_while_body_cont_fun);

R_code_t do_while_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    int bgn;

    checkArity(op, args);

    PUSH_WHILE_LOOP_FRAME(code, call, op);

    bgn = BodyHasBraces(WHILE_LOOP_CALL_BODY(call));
    SET_LOOP_DBG(DEBUG(R_CurrentEnv));
    SET_LOOP_BGN(bgn);

    BEGIN_LOOP_CONTEXT(do_while_body_cont);

    R_ReturnedValue = R_NilValue;
    DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
    return R_eval_nr(WHILE_LOOP_CALL_TEST(call), do_while_test_cont);
}

/* continuation for the test evaluation */
static R_code_t do_while_test_cont_fun(R_code_t code)
{
    SEXP call = LOOP_CALL();

    if (asLogicalNoNA(R_ReturnedValue, call)) {
	SEXP op = LOOP_OP();
	int bgn = LOOP_BGN();

	DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
	return R_eval_nr(WHILE_LOOP_CALL_BODY(call), do_while_body_cont);
    }
    else 
	return finish_loop_nr();
}

/* continuation for the body evaluation; also for break/next jumps */
static R_code_t do_while_body_cont_fun(R_code_t code)
{
    if (R_ReturnedValue == R_LoopBreakToken)
	return finish_loop_nr();
    else {
	SEXP call = LOOP_CALL();
	UPDATE_LOOP_VALUE();
	return R_eval_nr(WHILE_LOOP_CALL_TEST(call), do_while_test_cont);
    }
}


SEXP do_repeat(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int dbg;
    volatile int bgn;
    volatile SEXP t, body;
    PROTECT_INDEX tpi;
    RDISPATCHER disp;

    checkArity(op, args);

    dbg = DEBUG(rho);
    body = CAR(args);
    bgn = BodyHasBraces(body);

    t = R_NilValue;
    PROTECT_WITH_INDEX(t, &tpi);
    R_BeginDispatcher(&disp);
    begincontext(CTXT_LOOP, R_NilValue, rho, R_NilValue, R_NilValue);
    if (SETJMP(disp.cjmpbuf) != CTXT_BREAK) {
	R_ReturnedValue = R_NilValue; 
	for (;;) {
	    DO_LOOP_DEBUG(call, op, args, rho, bgn);
	    REPROTECT(t = eval(body, rho), tpi);
	}
    }
    endcontext();
    R_EndDispatcher(&disp);
    UNPROTECT(1);
    R_Visible = 0;
    SET_DEBUG(rho, dbg);
    return t;
}


/* 
 * Non-recursive verion of do_repeat
 */

/* `repeat' loop calls look like (repeat body) */
#define REPEAT_LOOP_CALL_BODY(c) CADR(c)

/* `repeat' loops need no additional integer variables */
#define PUSH_REPEAT_LOOP_FRAME(code,call,op) PUSH_LOOP_FRAME(code,call,op,0)

DECLARE_CONTINUATION(do_repeat_cont, do_repeat_cont_fun);

/*
f<-function(m) { n<-1; repeat { print(n); n <- n+1; if (n > m) break }; 2 }
g<-function(m) { n<-1; repeat { print(n); n <- n+1; if (n > m) break; n}}
*/
R_code_t do_repeat_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    int bgn;
    SEXP body;

    checkArity(op, args);

    PUSH_REPEAT_LOOP_FRAME(code, call, op);

    body = REPEAT_LOOP_CALL_BODY(call);
    bgn = BodyHasBraces(body);
    SET_LOOP_DBG(DEBUG(R_CurrentEnv));
    SET_LOOP_BGN(bgn);

    BEGIN_LOOP_CONTEXT(do_repeat_cont);

    R_ReturnedValue = R_NilValue;
    DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
    return R_eval_nr(body, do_repeat_cont);
}

static R_code_t do_repeat_cont_fun(R_code_t code)
{
    if (R_ReturnedValue == R_LoopBreakToken) 
	return finish_loop_nr();
    else {
	SEXP call = LOOP_CALL();
	SEXP op = LOOP_OP();
	int bgn = LOOP_BGN();
	
	UPDATE_LOOP_VALUE();
	DO_LOOP_DEBUG(call, op, CALL_ARGS(call), R_CurrentEnv, bgn);
	return R_eval_nr(REPEAT_LOOP_CALL_BODY(call), do_repeat_cont);
    }
}


SEXP do_break(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP val = PRIMVAL(op) == CTXT_BREAK ? R_LoopBreakToken : R_LoopNextToken;
    findcontext(PRIMVAL(op), rho, val);
    return R_NilValue;
}

R_code_t do_break_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    SEXP val = PRIMVAL(op) == CTXT_BREAK ? R_LoopBreakToken : R_LoopNextToken;
    return R_findcontext_nr(PRIMVAL(op), rho, val);
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

/*
 * Non-recursive version of do_begin
 */

/* The evaluation frame holds the remaining expressions to evaluate in
   EVAL_FRAME_VAR1.  EVAL_FRAME_VAR0 holds debugging information.  If
   debugging is on when do_begin is called, then this slot containse
   CONS(call, op); otherwise, it contains R_NilValue. */

#define BEGIN_FRAME_DEBUGGING() (EVAL_FRAME_VAR0() != R_NilValue)
#define BEGIN_FRAME_CALL() CAR(EVAL_FRAME_VAR0())
#define BEGIN_FRAME_OP() CDR(EVAL_FRAME_VAR0())
#define BEGIN_FRAME_ARGS EVAL_FRAME_VAR1
#define SET_BEGIN_FRAME_ARGS SET_EVAL_FRAME_VAR1

#define PUSH_BEGIN_FRAME(call, op, args, rho) do { \
    SEXP call_op = DEBUG(rho) ? CONS(call, op) : R_NilValue; \
    PUSH_EVAL_FRAME(code, call_op, CDR(args)); \
} while (0)

DECLARE_CONTINUATION(do_begin_cont, do_begin_cont_fun);

R_code_t do_begin_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    if (args == R_NilValue) {
	R_ReturnedValue = R_NilValue;
	return code;
    }
    else {
	if (DEBUG(rho)) {
	    PROTECT(op);
	    Rprintf("debug: ");
	    PrintValue(CAR(args));
	    do_browser(call,op,args,rho);
	    UNPROTECT(1);
	}
	if (CDR(args) == R_NilValue)
	    return R_eval_nr(CAR(args), code);
	else {
	    PUSH_BEGIN_FRAME(call, op, args, rho);
	    return R_eval_nr(CAR(args), do_begin_cont);
	}
    }
}

static R_code_t do_begin_cont_fun(R_code_t code)
{
    SEXP args = BEGIN_FRAME_ARGS();

    if (BEGIN_FRAME_DEBUGGING()) {
	Rprintf("debug: ");
	PrintValue(CAR(args));
	do_browser(BEGIN_FRAME_CALL(),BEGIN_FRAME_OP(),args,R_CurrentEnv);
    }

    if (CDR(args) == R_NilValue) {
	code = EVAL_FRAME_CODE();
	POP_EVAL_FRAME();
    }
    else
	SET_BEGIN_FRAME_ARGS(CDR(args));

    return R_eval_nr(CAR(args), code);
}

static void transfer_return_tags(SEXP args, SEXP vals)
{
    SEXP a, v;


    for (a = args, v = vals; a != R_NilValue; a = CDR(a), v = CDR(v)) {
	if (CAR(a) == R_DotsSymbol)
	    error("... not allowed in return");
	if (isNull(TAG(a)) && isSymbol(CAR(a)))
	    SET_TAG(v, CAR(a));
    }
}    

static SEXP make_return_value(SEXP vals)
{
    if (vals == R_NilValue)
	return R_NilValue;
    else if (CDR(vals) == R_NilValue)
	return CAR(vals);
    else {
	SEXP v;
	PROTECT(vals);
	for (v = vals; v != R_NilValue; v = CDR(v))
	    if (NAMED(CAR(v)))
		SETCAR(v, duplicate(CAR(v)));
	v = PairToVectorList(vals);
	UNPROTECT(1);
	return v;
    }
}
    
SEXP do_return(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP vals = evalList(args, rho);

    transfer_return_tags(args, vals);

    vals = make_return_value(vals);

    findcontext(CTXT_BROWSER | CTXT_FUNCTION, rho, vals);

    return R_NilValue; /*NOTREACHED*/
}


/*
 * Non-recursive version of do_return
 */

/* this evaluates a list of arguments, much like eval_builtin_nr, and
   then handles the return.  The frame contains the list of arguments
   remaining to be processed in EVAL_FRAME_VAR0.  EVAL_FRAME_VAR1
   initially contains the list of arguments; this is destrictuvely
   updated to the list of values. */

#define RETURN_FRAME_ARGS EVAL_FRAME_VAR0
#define SET_RETURN_FRAME_ARGS SET_EVAL_FRAME_VAR0

#define RETURN_FRAME_VALS EVAL_FRAME_VAR1

#define PUSH_RETURN_FRAME(code, pargs) do { \
    SEXP __pargs__ = (pargs); \
    PUSH_EVAL_FRAME(code, __pargs__, __pargs__); \
} while (0)

DECLARE_CONTINUATION(do_return_cont, do_return_cont_fun);

R_code_t do_return_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    if (args == R_NilValue)
	return R_findcontext_nr(CTXT_BROWSER | CTXT_FUNCTION, rho, R_NilValue);
    else {
	SEXP pargs = processArgs(args, rho, FALSE);
	transfer_return_tags(args, pargs);
	PUSH_RETURN_FRAME(code, pargs);
	return R_eval_nr(CAR(pargs), do_return_cont);
    }
}

static R_code_t do_return_cont_fun(R_code_t code)
{
    SEXP args = RETURN_FRAME_ARGS();

    SETCAR(args, R_ReturnedValue);
    args = CDR(args);

    if (args == R_NilValue) {
	SEXP vals = RETURN_FRAME_VALS();
	SEXP v = make_return_value(vals);

	return R_findcontext_nr(CTXT_BROWSER | CTXT_FUNCTION, R_CurrentEnv, v);
    }
    else {
	SET_RETURN_FRAME_ARGS(args);
	return R_eval_nr(CAR(args), do_return_cont);
    }
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
 * out efficiently using previously computed components.  */

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


static SEXP applydefine(SEXP call, SEXP op, SEXP args, SEXP rho, SEXP rhs)
{
    SEXP expr, lhs, saverhs, tmp, tmp2, tmploc;
    char buf[32];

    expr = CAR(args);

    /* It's important that the rhs get evaluated before the call to
       aplydefine because assignment is right associative i.e.  a <- b
       <- c is parsed as a <- (b <- c).  */

    PROTECT(saverhs = rhs);

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

static SEXP get_set_lhs(SEXP call, SEXP op, SEXP args)
{
    SEXP lhs;
    if (length(args) != 2)
	WrongArgCount(asym[PRIMVAL(op)]);
    if (isString(CAR(args)))
	SETCAR(args, install(CHAR(STRING_ELT(CAR(args), 0))));

    lhs = CAR(args);
    if (! isSymbol(lhs) && ! isLanguage(lhs))
	errorcall(call,"invalid left-hand side to assignment");

    return lhs;
}

static SEXP symbol_set(SEXP lhs, SEXP op, SEXP rho, SEXP s)
{
    if (NAMED(s))
	s = duplicate(s);
    PROTECT(s);
    switch (PRIMVAL(op)) {
    case 1: defineVar(lhs, s, rho); break;       /*  <- */
    case 2: setVar(lhs, s, ENCLOS(rho)); break;  /* <<- */
    default: UNIMPLEMENTED("do_set");
    }
    UNPROTECT(1);
    SET_NAMED(s, 1);
    return s;
}
    
SEXP do_set(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP s, lhs;

    lhs = get_set_lhs(call, op, args);

    /* evaluate the rhs */
    s = eval(CADR(args), rho);
    R_Visible = 0;

    if (isSymbol(lhs))
	return symbol_set(lhs, op, rho, s);
    else /* isLanguage(lhs) */
	return applydefine(call, op, CALL_ARGS(call), rho, s);
}


/*
 * Non-recursive version of do_set (in the value only)
 */

/* The new value is evaluated by the non-recursive evaluator.  The
   assignment is then handled in the continuation.  No attempt is made
   to use a non-recursive mechanism for the left-hand sides of complex
   assignment expressions. */

/* the frame contains the left-hand symbol or the entire call for
   complex assignments in EVAL_FRAME_VAR0.  The assignment function op
   is passed in EVAL_FRAME_VAR_1 */

#define ASSIGNMENT_FRAME_CALLINFO EVAL_FRAME_VAR0
#define ASSIGNMENT_FRAME_OP EVAL_FRAME_VAR1
#define PUSH_ASSIGNMENT_FRAME(code, ci, op) PUSH_EVAL_FRAME(code, ci, op)

DECLARE_CONTINUATION(do_set_cont, do_set_cont_fun);

R_code_t do_set_nr(SEXP call, SEXP op, SEXP args, SEXP rho, R_code_t code)
{
    SEXP lhs, callinfo;

    lhs = get_set_lhs(call, op, args);
    callinfo = isSymbol(lhs) ? lhs : call;

    PUSH_ASSIGNMENT_FRAME(code,  callinfo, op);
    return R_eval_nr(CADR(args), do_set_cont);
}

static R_code_t do_set_cont_fun(R_code_t code)
{
    SEXP rho = R_CurrentEnv;
    SEXP s = R_ReturnedValue;
    SEXP callinfo = ASSIGNMENT_FRAME_CALLINFO();
    SEXP op = ASSIGNMENT_FRAME_OP();
    code = EVAL_FRAME_CODE();
    POP_EVAL_FRAME();

    R_Visible = 0;
    if (isSymbol(callinfo)) {
	SEXP lhs = callinfo;
	R_ReturnedValue = symbol_set(lhs, op, rho, s);
    }
    else { /* isLanguage(lhs) */
	SEXP call = callinfo;
	R_ReturnedValue = applydefine(call, op, CALL_ARGS(call), rho, s);
    }
    return code;
}


/* Copy an argument list, expanding ... and optionally evaluating the
   arguments. optionally Evaluate each expression in "el" in the
   environment "rho".  This is a naturally recursive algorithm, but we
   use the iterative form below because it is does not cause growth of
   the pointer protection stack, and because it is a little more
   efficient. */

/***** think about merging this with evalList and evalListKeepMissing?? */

static SEXP processArgs(SEXP el, SEXP rho, Rboolean evaluate)
{
    SEXP ans, h, tail, val;

    PROTECT(ans = tail = CONS(R_NilValue, R_NilValue));

    while (el != R_NilValue) {

	/* If we have a ... symbol, we look to see what it is bound to.
	 * If its binding is Null (i.e. zero length)
	 *	we just ignore it and return the cdr with all its
	 *      expressions evaluated;
	 * if it is bound to a ... list of promises,
	 *	we splice the list of resulting values into the return
	 *      value.
	 * Anything else bound to a ... symbol is an error
	*/
	if (CAR(el) == R_DotsSymbol) {
	    h = findVar(CAR(el), rho);
	    if (TYPEOF(h) == DOTSXP || h == R_NilValue) {
		while (h != R_NilValue) {
		    val = evaluate ? eval(CAR(h), rho) : CAR(h);
		    SETCDR(tail, CONS(val, R_NilValue));
		    SET_TAG(CDR(tail), CreateTag(TAG(h)));
		    tail = CDR(tail);
		    h = CDR(h);
		}
	    }
	    else if (h != R_MissingArg)
		error("... used in an incorrect context");
	}
	else if (CAR(el) != R_MissingArg) {
	    val = evaluate ? eval(CAR(el), rho) : CAR(el);
	    SETCDR(tail, CONS(val, R_NilValue));
	    tail = CDR(tail);
	    SET_TAG(tail, CreateTag(TAG(el)));
	}
	el = CDR(el);
    }
    UNPROTECT(1);
    return CDR(ans);
}/* processArgs() */


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

    int frame;
    RDISPATCHER disp;

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
	if (length(env) != 1) 
	    errorcall(call,"numeric envir arg not of length one");
	frame = asInteger(env);
	if (frame == NA_INTEGER)
	    errorcall(call,"invalid environment");
	PROTECT(env = R_sysframe(frame, R_GlobalContext));
	break;
    default:
	errorcall(call, "invalid second argument");
    }
    if(isLanguage(expr) || isSymbol(expr)) {
	PROTECT(expr);
	R_BeginDispatcher(&disp);
	begincontext(CTXT_RETURN, call, env, rho, args);
	if (!SETJMP(disp.cjmpbuf))
	    expr = eval(expr, env);
	endcontext();
	R_EndDispatcher(&disp);
	UNPROTECT(1);
    }
    else if (isExpression(expr)) {
	int i, n;
	PROTECT(expr);
	n = LENGTH(expr);
	tmp = R_NilValue;
	R_BeginDispatcher(&disp);
	begincontext(CTXT_RETURN, call, env, rho, args);
	if (!SETJMP(disp.cjmpbuf))
	    for(i=0 ; i<n ; i++)
		tmp = eval(VECTOR_ELT(expr, i), env);
	endcontext();
	R_EndDispatcher(&disp);
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

    /* get the args supplied */
    cptr = R_ParentContext(rho);
    if (cptr == NULL || cptr->callflag == CTXT_TOPLEVEL)
	error("Recall called from outside a closure");
    args = cptr->promargs;

    /* get the env recall was called from */
    s = cptr->sysparent;
    cptr = R_ParentContext(s);
    if (cptr == NULL || cptr->callflag == CTXT_TOPLEVEL)
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
	    begincontext(CTXT_RETURN, call, rho, rho, pargs);
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
	    begincontext(CTXT_RETURN, call, rho, rho, args);
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


R_code_t do_internal_nr(SEXP call, SEXP op, SEXP args, SEXP env, R_code_t code)
{
    SEXP s, fun;

    checkArity(op, args);
    s = CAR(args);
    if (!isPairList(s))
	errorcall(call, "invalid .Internal() argument");
    fun = CAR(s);
    if (!isSymbol(fun))
	errorcall(call, "invalid internal function");
    if (INTERNAL(fun) == R_NilValue)
	errorcall(call, "no internal function \"%s\"", CHAR(PRINTNAME(fun)));
    args = CDR(s);

    switch (TYPEOF(INTERNAL(fun))) {
    case BUILTINSXP:
	args = processArgs(args, env, FALSE);
	return eval_builtin_nr(s, INTERNAL(fun), args, code, FALSE);
    case SPECIALSXP:
	return eval_special_nr(s, INTERNAL(fun), args, env, code);
    default:
	error("bad internal function");
	return code;
    }
}

#define APPLY_METHOD_FRAME_RHO() CAR(EVAL_FRAME_VAR1())
#define APPLY_METHOD_FRAME_ISNEXT() (EVAL_FRAME_VAR1()->sxpinfo.gp)

#define PUSH_APPLY_METHOD_FRAME(code, call, op, args, rho, newrho, isnext) \
do { \
    SEXP __call__ = (call); \
    SEXP __op__ = (op); \
    SEXP __args__ = (args); \
    SEXP __rho__ = (rho); \
    SEXP __newrho__ = (newrho); \
    SEXP __ci1__, __ci2__; \
    PROTECT(__args__); \
    PROTECT(__newrho__); \
    PROTECT(__ci1__ = CONS(__call__, __op__)); \
    SET_TAG(__ci1__, __args__); \
    __ci2__ = CONS(__rho__, __newrho__); \
    __ci2__->sxpinfo.gp = (isnext) ? 1 : 0; \
    PUSH_EVAL_FRAME(code, __ci1__, __ci2__); \
    UNPROTECT(3); \
} while (0)

DECLARE_CONTINUATION(applyMethod_cont, applyMethod_cont_fun);

R_code_t R_applyMethod_nr(SEXP call, SEXP op, SEXP args, SEXP rho,
			  SEXP newrho, Rboolean isnext, R_code_t code)
{
    /* do_usemethod_nr needs to set the call context to CTXT_GENERIC */
    if (! isnext)
	R_GlobalContext->callflag = CTXT_GENERIC;

    PUSH_APPLY_METHOD_FRAME(code, call, op, args, rho, newrho, isnext);

    switch (TYPEOF(op)) {
    case SPECIALSXP:
	return eval_special_nr(call, op, args, rho, applyMethod_cont);
    case BUILTINSXP:
	return eval_builtin_nr(call, op, args, applyMethod_cont, FALSE);
    case CLOSXP:
	return R_applyClosure_nr(call, op, args, rho, newrho,
				 applyMethod_cont);
    default:
	R_ReturnedValue = R_NilValue;  /* for -Wall */
	return applyMethod_cont;
    }

}

static R_code_t applyMethod_cont_fun(R_code_t code)
{
    int isnext = APPLY_METHOD_FRAME_ISNEXT();

    if (! isnext) {
	/* do_usemethod_nr needs to restore the context callflag and
	   execute a `return' jump */
	SEXP rho = APPLY_METHOD_FRAME_RHO();
	POP_EVAL_FRAME();
	R_GlobalContext->callflag = CTXT_RETURN;
	return R_findcontext_nr(CTXT_RETURN, rho, R_ReturnedValue);
    }
    else {
	code = EVAL_FRAME_CODE();
	POP_EVAL_FRAME();
	return code;
    }
}
