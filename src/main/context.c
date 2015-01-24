/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2014   The R Core Team.
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
 *  http://www.r-project.org/Licenses/
 *
 *
 *  Contexts:
 *
 *  A linked-list of execution contexts is kept so that control-flow
 *  constructs like "next", "break" and "return" will work.  It is also
 *  used for error returns to top-level.
 *
 *	context[k] -> context[k-1] -> ... -> context[0]
 *	^				     ^
 *	R_GlobalContext			     R_ToplevelContext
 *
 *  Contexts are allocated on the stack as the evaluator invokes itself
 *  recursively.  The memory is reclaimed naturally on return through
 *  the recursions (the R_GlobalContext pointer needs adjustment).
 *
 *  A context contains the following information (and more):
 *
 *	nextcontext	the next level context
 *	cjmpbuf		longjump information for non-local return
 *	cstacktop	the current level of the pointer protection stack
 *	callflag	the context "type"
 *	call		the call (name of function, or expression to
 *			get the function) that effected this
 *			context if a closure, otherwise often NULL.
 *	callfun		the function, if this was a closure.
 *	cloenv		for closures, the environment of the closure.
 *	sysparent	the environment the closure was called from
 *	conexit		code for on.exit calls, to be executed in cloenv
 *			at exit from the closure (normal or abnormal).
 *	cend		a pointer to function which executes if there is
 *			non-local return (i.e. an error)
 *	cenddata	a void pointer to data for cend to use
 *	vmax		the current setting of the R_alloc stack
 *	srcref		the srcref at the time of the call
 *
 *  Context types can be one of:
 *
 *	CTXT_TOPLEVEL	The toplevel context
 *	CTXT_BREAK	target for "break"
 *	CTXT_NEXT	target for "next"
 *	CTXT_LOOP	target for either "break" or "next"
 *	CTXT_RETURN	target for "return" (i.e. a closure)
 *	CTXT_BROWSER	target for "return" to exit from browser
 *	CTXT_CCODE	other functions that need clean up if an error occurs
 *	CTXT_RESTART	a function call to restart was made inside the
 *			closure.
 *
 *	Code (such as the sys.xxx) that looks for CTXT_RETURN must also
 *	look for a CTXT_RESTART and CTXT_GENERIC.
 *	The mechanism used by restart is to change
 *	the context type; error/errorcall then looks for a RESTART and does
 *	a long jump there if it finds one.
 *
 *  A context is created with a call to
 *
 *	void begincontext(RCNTXT *cptr, int flags,
 *			  SEXP syscall, SEXP env, SEXP
 *			  sysp, SEXP promargs, SEXP callfun)
 *
 *  which sets up the context pointed to by cptr in the appropriate way.
 *  When the context goes "out-of-scope" a call to
 *
 *	void endcontext(RCNTXT *cptr)
 *
 *  restores the previous context (i.e. it adjusts the R_GlobalContext
 *  pointer).
 *
 *  The non-local jump to a given context takes place in a call to
 *
 *	void findcontext(int mask, SEXP env, SEXP val)
 *
 *  This causes "val" to be stuffed into a globally accessable place and
 *  then a search to take place back through the context list for an
 *  appropriate context.  The kind of context sort is determined by the
 *  value of "mask".  The value of mask should be the logical OR of all
 *  the context types desired.
 *
 *  The value of "mask" is returned as the value of the setjump call at
 *  the level longjumped to.  This is used to distinguish between break
 *  and next actions.
 *
 *  Contexts can be used as a wrapper around functions that create windows
 *  or open files. These can then be shut/closed gracefully if an error
 *  occurs.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

/* R_run_onexits - runs the conexit/cend code for all contexts from
   R_GlobalContext down to but not including the argument context.
   This routine does not stop at a CTXT_TOPLEVEL--the code that
   determines the argument is responsible for making sure
   CTXT_TOPLEVEL's are not crossed unless appropriate. */

void attribute_hidden R_run_onexits(RCNTXT *cptr)
{
    RCNTXT *c;

    for (c = R_GlobalContext; c != cptr; c = c->nextcontext) {
        // a user embedding R incorrectly triggered this (PR#15420)
	if (c == NULL)
	    error("bad target context--should NEVER happen if R was called correctly");
	if (c->cend != NULL) {
	    void (*cend)(void *) = c->cend;
	    c->cend = NULL; /* prevent recursion */
	    R_HandlerStack = c->handlerstack;
	    R_RestartStack = c->restartstack;
	    cend(c->cenddata);
	}
	if (c->cloenv != R_NilValue && c->conexit != R_NilValue) {
	    SEXP s = c->conexit;
	    RCNTXT* savecontext = R_ExitContext;
	    R_ExitContext = c;
	    c->conexit = R_NilValue; /* prevent recursion */
	    R_HandlerStack = c->handlerstack;
	    R_RestartStack = c->restartstack;
	    PROTECT(s);
	    /* Since these are run before any jumps rather than after
	       jumping to the context where the exit handler was set
	       we need to make sure there is enough room on the
	       evaluation stack in case the jump is from handling a
	       stack overflow. To be safe it is good to also call
	       R_CheckStack. LT */
	    R_Expressions = R_Expressions_keep + 500;
	    R_CheckStack();
	    eval(s, c->cloenv);
	    UNPROTECT(1);
	    R_ExitContext = savecontext;
	}
	if (R_ExitContext == c)
	    R_ExitContext = NULL; /* Not necessary?  Better safe than sorry. */
    }
}


/* R_restore_globals - restore global variables from a target context
   before a LONGJMP.  The target context itself is not restored here
   since this is done slightly differently in jumpfun below, in
   errors.c:jump_now, and in main.c:ParseBrowser.  Eventually these
   three should be unified so there is only one place where a LONGJMP
   occurs. */

void attribute_hidden R_restore_globals(RCNTXT *cptr)
{
    R_PPStackTop = cptr->cstacktop;
    R_EvalDepth = cptr->evaldepth;
    vmaxset(cptr->vmax);
    R_interrupts_suspended = cptr->intsusp;
    R_HandlerStack = cptr->handlerstack;
    R_RestartStack = cptr->restartstack;
    while (R_PendingPromises != cptr->prstack) {
	/* The value 2 installed in PRSEEN 2 allows forcePromise in
	   eval.c to signal a warning when asked to evaluate a promise
	   whose evaluation has been interrupted by a jump. */
	SET_PRSEEN(R_PendingPromises->promise, 2);
	R_PendingPromises = R_PendingPromises->next;
    }
    /* Need to reset R_Expressions in case we are jumping after
       handling a stack overflow. */
    R_Expressions = R_Expressions_keep;
    R_BCNodeStackTop = cptr->nodestack;
#ifdef BC_INT_STACK
    R_BCIntStackTop = cptr->intstack;
#endif
    R_Srcref = cptr->srcref;
}


/* jumpfun - jump to the named context */

static void NORET jumpfun(RCNTXT * cptr, int mask, SEXP val)
{
    Rboolean savevis = R_Visible;

    /* run onexit/cend code for all contexts down to but not including
       the jump target */
    PROTECT(val);
    cptr->returnValue = val;/* in case the on.exit code wants to see it */
    R_run_onexits(cptr);
    UNPROTECT(1);
    R_Visible = savevis;

    R_ReturnedValue = val;
    R_GlobalContext = cptr; /* this used to be set to
			       cptr->nextcontext for non-toplevel
			       jumps (with the context set back at the
			       SETJMP for restarts).  Changing this to
			       always using cptr as the new global
			       context should simplify some code and
			       perhaps allow loops to be handled with
			       fewer SETJMP's.  LT */
    R_restore_globals(R_GlobalContext);

    LONGJMP(cptr->cjmpbuf, mask);
}


/* begincontext - begin an execution context */

/* begincontext and endcontext are used in dataentry.c and modules */
void begincontext(RCNTXT * cptr, int flags,
		  SEXP syscall, SEXP env, SEXP sysp,
		  SEXP promargs, SEXP callfun)
{
    cptr->cstacktop = R_PPStackTop;
    cptr->evaldepth = R_EvalDepth;
    cptr->callflag = flags;
    cptr->call = syscall;
    cptr->cloenv = env;
    cptr->sysparent = sysp;
    cptr->conexit = R_NilValue;
    cptr->cend = NULL;
    cptr->promargs = promargs;
    cptr->callfun = callfun;
    cptr->vmax = vmaxget();
    cptr->intsusp = R_interrupts_suspended;
    cptr->handlerstack = R_HandlerStack;
    cptr->restartstack = R_RestartStack;
    cptr->prstack = R_PendingPromises;
    cptr->nodestack = R_BCNodeStackTop;
#ifdef BC_INT_STACK
    cptr->intstack = R_BCIntStackTop;
#endif
    cptr->srcref = R_Srcref;    
    cptr->browserfinish = R_GlobalContext->browserfinish;
    cptr->nextcontext = R_GlobalContext;
    cptr->returnValue = NULL;

    R_GlobalContext = cptr;
}


/* endcontext - end an execution context */

void endcontext(RCNTXT * cptr)
{
    R_HandlerStack = cptr->handlerstack;
    R_RestartStack = cptr->restartstack;
    if (cptr->cloenv != R_NilValue && cptr->conexit != R_NilValue ) {
	SEXP s = cptr->conexit;
	Rboolean savevis = R_Visible;
	RCNTXT* savecontext = R_ExitContext;
	R_ExitContext = cptr;
	cptr->conexit = R_NilValue; /* prevent recursion */
	PROTECT(s);
	eval(s, cptr->cloenv);
	UNPROTECT(1);
	R_ExitContext = savecontext;
	R_Visible = savevis;
    }
    if (R_ExitContext == cptr)
    	R_ExitContext = NULL;
    R_GlobalContext = cptr->nextcontext;
}


/* findcontext - find the correct context */

void attribute_hidden NORET findcontext(int mask, SEXP env, SEXP val)
{
    RCNTXT *cptr;
    cptr = R_GlobalContext;
    if (mask & CTXT_LOOP) {		/* break/next */
	for (cptr = R_GlobalContext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if (cptr->callflag & CTXT_LOOP && cptr->cloenv == env )
		jumpfun(cptr, mask, val);
	error(_("no loop for break/next, jumping to top level"));
    }
    else {				/* return; or browser */
	for (cptr = R_GlobalContext;
	     cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	     cptr = cptr->nextcontext)
	    if ((cptr->callflag & mask) && cptr->cloenv == env)
		jumpfun(cptr, mask, val);
	error(_("no function to return from, jumping to top level"));
    }
}

void attribute_hidden NORET R_JumpToContext(RCNTXT *target, int mask, SEXP val)
{
    RCNTXT *cptr;
    for (cptr = R_GlobalContext;
	 cptr != NULL && cptr->callflag != CTXT_TOPLEVEL;
	 cptr = cptr->nextcontext) {
	if (cptr == target)
	    jumpfun(cptr, mask, val);
        if (cptr == R_ExitContext)
	    R_ExitContext = NULL;
    }
    error(_("target context is not on the stack"));
}


/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that cloenv. */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

SEXP attribute_hidden R_sysframe(int n, RCNTXT *cptr)
{
    if (n == 0)
	return(R_GlobalEnv);

    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = -n;

    if(n < 0)
	errorcall(R_GlobalContext->call,
		  _("not that many frames on the stack"));

    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0) {  /* we need to detach the enclosing env */
		return cptr->cloenv;
	    }
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if(n == 0 && cptr->nextcontext == NULL)
	return R_GlobalEnv;
    else
	errorcall(R_GlobalContext->call,
		  _("not that many frames on the stack"));
    return R_NilValue;	   /* just for -Wall */
}


/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the cloenv pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->sysparent */
/* but then we wouldn't be compatible with S. */

int attribute_hidden R_sysparent(int n, RCNTXT *cptr)
{
    int j;
    SEXP s;
    if(n <= 0)
	errorcall(R_ToplevelContext->call,
		  _("only positive values of 'n' are allowed"));
    while (cptr->nextcontext != NULL && n > 1) {
	if (cptr->callflag & CTXT_FUNCTION )
	    n--;
	cptr = cptr->nextcontext;
    }
    /* make sure we're looking at a return context */
    while (cptr->nextcontext != NULL && !(cptr->callflag & CTXT_FUNCTION) )
	cptr = cptr->nextcontext;
    s = cptr->sysparent;
    if(s == R_GlobalEnv)
	return 0;
    j = 0;
    while (cptr != NULL ) {
	if (cptr->callflag & CTXT_FUNCTION) {
	    j++;
	    if( cptr->cloenv == s )
		n=j;
	}
	cptr = cptr->nextcontext;
    }
    n = j - n + 1;
    if (n < 0)
	n = 0;
    return n;
}

int attribute_hidden framedepth(RCNTXT *cptr)
{
    int nframe = 0;
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION )
	    nframe++;
	cptr = cptr->nextcontext;
    }
    return nframe;
}

SEXP attribute_hidden R_syscall(int n, RCNTXT *cptr)
{
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */
    SEXP result;
    
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if(n < 0)
	errorcall(R_GlobalContext->call,
		  _("not that many frames on the stack"));
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0) {
	    	PROTECT(result = shallow_duplicate(cptr->call));
	    	if (cptr->srcref && !isNull(cptr->srcref))
	    	    setAttrib(result, R_SrcrefSymbol, duplicate(cptr->srcref));
	    	UNPROTECT(1);
	    	return result;
	    } else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL) {
	PROTECT(result = shallow_duplicate(cptr->call));
	if (cptr->srcref && !isNull(cptr->srcref))
	    setAttrib(result, R_SrcrefSymbol, duplicate(cptr->srcref));
	UNPROTECT(1);
	return result;
    }
    errorcall(R_GlobalContext->call, _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

SEXP attribute_hidden R_sysfunction(int n, RCNTXT *cptr)
{
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0)
	errorcall(R_GlobalContext->call,
		  _("not that many frames on the stack"));
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (n == 0)
		return duplicate(cptr->callfun);  /***** do we need to DUP? */
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL)
	return duplicate(cptr->callfun);  /***** do we need to DUP? */
    errorcall(R_GlobalContext->call, _("not that many frames on the stack"));
    return R_NilValue;	/* just for -Wall */
}

/* some real insanity to keep Duncan sane */

/* This should find the caller's environment (it's a .Internal) and
   then get the context of the call that owns the environment.  As it
   is, it will restart the wrong function if used in a promise.
   L.T. */
SEXP attribute_hidden do_restart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;

    checkArity(op, args);

    if( !isLogical(CAR(args)) || LENGTH(CAR(args))!= 1 )
	return(R_NilValue);
    for(cptr = R_GlobalContext->nextcontext; cptr!= R_ToplevelContext;
	    cptr = cptr->nextcontext) {
	if (cptr->callflag & CTXT_FUNCTION) {
	    SET_RESTART_BIT_ON(cptr->callflag);
	    break;
	}
    }
    if( cptr == R_ToplevelContext )
	error(_("no function to restart"));
    return(R_NilValue);
}

/* count how many contexts of the specified type are present on the stack */
/* browser contexts are a bit special because they are transient and for  */
/* any closure context with the debug bit set one will be created; so we  */
/* need to count those as well                                            */
int countContexts(int ctxttype, int browser) {
    int n=0;
    RCNTXT *cptr;

    cptr = R_GlobalContext;
    while( cptr != R_ToplevelContext) {
        if( cptr->callflag == ctxttype ) 
            n++;
        else if( browser ) {
           if(cptr->callflag & CTXT_FUNCTION && RDEBUG(cptr->cloenv) )
              n++;
        }
        cptr = cptr->nextcontext;
    }
    return n;
}
  
   
/* functions to support looking up information about the browser */
/* contexts that are in the evaluation stack */

SEXP attribute_hidden do_sysbrowser(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP rval=R_NilValue;
    RCNTXT *cptr;
    int n;

    checkArity(op, args);
    n = asInteger(CAR(args));
    if(n < 1 ) error(_("number of contexts must be positive"));

    /* first find the closest  browser context */
    cptr = R_GlobalContext;
    while (cptr != R_ToplevelContext) {
        if (cptr->callflag == CTXT_BROWSER) {
                break;
        }
        cptr = cptr->nextcontext;
    }
    /* error if not a browser context */

    if( !(cptr->callflag == CTXT_BROWSER) )
        error(_("no browser context to query"));

    switch (PRIMVAL(op)) {
    case 1: /* text */
    case 2: /* condition */
        /* first rewind to the right place if needed */
        /* note we want n>1, as we have already      */
        /* rewound to the first context              */
        if( n > 1 ) {
           while (cptr != R_ToplevelContext && n > 0 ) {
               if (cptr->callflag == CTXT_BROWSER) {
                   n--;
                   break;
               }
               cptr = cptr->nextcontext;
           }
        }
        if( !(cptr->callflag == CTXT_BROWSER) )
           error(_("not that many calls to browser are active"));

        if( PRIMVAL(op) == 1 )
            rval = CAR(cptr->promargs);
        else
            rval = CADR(cptr->promargs);
        break;
    case 3: /* turn on debugging n levels up */
        while ( (cptr != R_ToplevelContext) && n > 0 ) {
            if (cptr->callflag & CTXT_FUNCTION) 
                  n--;
            cptr = cptr->nextcontext;
        } 
        if( !(cptr->callflag & CTXT_FUNCTION) )
           error(_("not that many functions on the call stack"));
        else
           SET_RDEBUG(cptr->cloenv, 1);
        break;
    }
    return(rval);
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in, so the */
/* indexing is adjusted to handle this. */

SEXP attribute_hidden do_sys(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, n  = -1, nframe;
    SEXP rval, t;
    RCNTXT *cptr;

    checkArity(op, args);
    /* first find the context that sys.xxx needs to be evaluated in */
    cptr = R_GlobalContext;
    t = cptr->sysparent;
    while (cptr != R_ToplevelContext) {
	if (cptr->callflag & CTXT_FUNCTION )
	    if (cptr->cloenv == t)
		break;
	cptr = cptr->nextcontext;
    }

    if (length(args) == 1) n = asInteger(CAR(args));

    switch (PRIMVAL(op)) {
    case 1: /* parent */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "n");
	i = nframe = framedepth(cptr);
	/* This is a pretty awful kludge, but the alternative would be
	   a major redesign of everything... -pd */
	while (n-- > 0)
	    i = R_sysparent(nframe - i + 1, cptr);
	return ScalarInteger(i);
    case 2: /* call */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_syscall(n, cptr);
    case 3: /* frame */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' argument"), "which");
	return R_sysframe(n, cptr);
    case 4: /* sys.nframe */
	return ScalarInteger(framedepth(cptr));
    case 5: /* sys.calls */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t=rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_syscall(i, cptr));
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe = framedepth(cptr);
	PROTECT(rval = allocList(nframe));
	t = rval;
	for(i = 1; i <= nframe; i++, t = CDR(t))
	    SETCAR(t, R_sysframe(i, cptr));
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
	if( R_GlobalContext->nextcontext != NULL)
	    return R_GlobalContext->nextcontext->conexit;
	else
	    return R_NilValue;
    case 8: /* sys.parents */
	nframe = framedepth(cptr);
	rval = allocVector(INTSXP, nframe);
	for(i = 0; i < nframe; i++)
	    INTEGER(rval)[i] = R_sysparent(nframe - i, cptr);
	return rval;
    case 9: /* sys.function */
	if(n == NA_INTEGER)
	    error(_("invalid '%s' value"), "which");
	return(R_sysfunction(n, cptr));
    default:
	error(_("internal error in 'do_sys'"));
	return R_NilValue;/* just for -Wall */
    }
}

SEXP attribute_hidden do_parentframe(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int n;
    SEXP t;
    RCNTXT *cptr;

    checkArity(op, args);
    t = CAR(args);
    n = asInteger(t);

    if(n == NA_INTEGER || n < 1 )
	error(_("invalid '%s' value"), "n");

    cptr = R_GlobalContext;
    t = cptr->sysparent;
    while (cptr->nextcontext != NULL){
	if (cptr->callflag & CTXT_FUNCTION ) {
	    if (cptr->cloenv == t)
	    {
		if (n == 1)
		    return cptr->sysparent;
		n--;
		t = cptr->sysparent;
	    }
	}
	cptr = cptr->nextcontext;
    }
    return R_GlobalEnv;
}

/* R_ToplevelExec - call fun(data) within a top level context to
   insure that this functin cannot be left by a LONGJMP.  R errors in
   the call to fun will result in a jump to top level. The return
   value is TRUE if fun returns normally, FALSE if it results in a
   jump to top level. */

Rboolean R_ToplevelExec(void (*fun)(void *), void *data)
{
    RCNTXT thiscontext;
    RCNTXT * volatile saveToplevelContext;
    volatile SEXP topExp, oldHStack;
    Rboolean result;


    PROTECT(topExp = R_CurrentExpr);
    PROTECT(oldHStack = R_HandlerStack);
    R_HandlerStack = R_NilValue;
    saveToplevelContext = R_ToplevelContext;

    begincontext(&thiscontext, CTXT_TOPLEVEL, R_NilValue, R_GlobalEnv,
		 R_BaseEnv, R_NilValue, R_NilValue);
    if (SETJMP(thiscontext.cjmpbuf))
	result = FALSE;
    else {
	R_GlobalContext = R_ToplevelContext = &thiscontext;
	fun(data);
	result = TRUE;
    }
    endcontext(&thiscontext);

    R_ToplevelContext = saveToplevelContext;
    R_CurrentExpr = topExp;
    R_HandlerStack = oldHStack;
    UNPROTECT(2);

    return result;
}



/*
  This is a simple interface for evaluating R expressions
  from C with a guarantee that one will return to the
  point in the code from which the call was made (if it does
  return at all).
  This uses R_TopleveExec to do this.  It is important
  in applications that embed R or wish to make general
  callbacks to R with error handling.

  It is currently hidden with a data structure definition
  and C routine visible only here. The R_tryEval() is the
  only visible aspect. This can be lifted into the header
  files if necessary. (DTL)

  R_tryEval is in Rinternals.h (so public), but not in the API.
 */
typedef struct {
    SEXP expression;
    SEXP val;
    SEXP env;
} ProtectedEvalData;

static void
protectedEval(void *d)
{
    ProtectedEvalData *data = (ProtectedEvalData *)d;
    SEXP env = R_GlobalEnv;
    if(data->env) {
	env = data->env;
    }
    data->val = eval(data->expression, env);
    PROTECT(data->val);
}

SEXP
R_tryEval(SEXP e, SEXP env, int *ErrorOccurred)
{
    Rboolean ok;
    ProtectedEvalData data;

    data.expression = e;
    data.val = NULL;
    data.env = env;

    ok = R_ToplevelExec(protectedEval, &data);
    if (ErrorOccurred) {
	*ErrorOccurred = (ok == FALSE);
    }
    if (ok == FALSE)
	data.val = NULL;
    else
	UNPROTECT(1);

    return(data.val);
}

/* Temporary hack to suppress error message printing around a
   R_tryEval call for use in methods_list_dispatch.c; should be
   replaced once we have a way of establishing error handlers from C
   code (probably would want a calling handler if we want to allow
   user-defined calling handlers to enter a debugger, for
   example). LT */
SEXP R_tryEvalSilent(SEXP e, SEXP env, int *ErrorOccurred)
{
    SEXP val;
    Rboolean oldshow = R_ShowErrorMessages;
    R_ShowErrorMessages = FALSE;
    val = R_tryEval(e, env, ErrorOccurred);
    R_ShowErrorMessages = oldshow;
    return val;
}

SEXP R_ExecWithCleanup(SEXP (*fun)(void *), void *data,
		       void (*cleanfun)(void *), void *cleandata)
{
    RCNTXT cntxt;
    SEXP result;

    begincontext(&cntxt, CTXT_CCODE, R_NilValue, R_BaseEnv, R_BaseEnv,
		 R_NilValue, R_NilValue);
    cntxt.cend = cleanfun;
    cntxt.cenddata = cleandata;

    result = fun(data);
    cleanfun(cleandata);

    endcontext(&cntxt);
    return result;
}
