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
 *  A context contains the following information:
 *
 *	nextcontext	the next level context
 *	cjmpbuf		longjump information for non-local return
 *	cstacktop	the current level of the pointer protection stack
 *	callflag	the context "type"
 *	call		the call (name of function) that effected this
 *			context
 *	cloenv		for closures, the environment of the closure.
 *	sysparent	the environment the closure was called from
 *	conexit		code for on.exit calls, to be executed in cloenv
 *			at exit from the closure (normal or abnormal).
 *	cend		a pointer to function which executes if there is
 *			non-local return (i.e. an error)
 *
 *  Context types can be one of:
 *
 *	CTXT_TOPLEVEL	The toplevel context
 *	CTXT_BREAK	target for "break"
 *	CTXT_NEXT	target for "next"
 *	CTXT_LOOP	target for either "break" or "next"
 *	CTXT_RETURN	target for "return" (i.e. a closure)
 *	CTXT_BROWSER    target for "return" to exit from browser
 *	CTXT_CCODE	other functions that need clean up if an error occurs
 *
 *  A context is created with a call to
 *
 *	void begincontext(RCNTXT *cptr, int flags,
 *                        SEXP syscall, SEXP env, SEXP
 *                        sysp, SEXP promargs)
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
#include <Rconfig.h>
#endif

#include "Defn.h"

/* jumpfun - jump to the named context */

static void jumpfun(RCNTXT * cptr, int mask, SEXP val)
{
    R_PPStackTop = cptr->cstacktop;
    R_ReturnedValue = val;
    if (cptr != R_ToplevelContext)
	R_GlobalContext = cptr->nextcontext;
    else
	R_GlobalContext = R_ToplevelContext;
    LONGJMP(cptr->cjmpbuf, mask);
}


/* begincontext - begin an execution context */

void begincontext(RCNTXT * cptr, int flags,
		  SEXP syscall, SEXP env, SEXP sysp, SEXP promargs)
{
    cptr->nextcontext = R_GlobalContext;
    cptr->cstacktop = R_PPStackTop;
    cptr->callflag = flags;
    cptr->call = syscall;
    cptr->cloenv = env;
    cptr->sysparent = sysp;
    cptr->conexit = R_NilValue;
    cptr->cend = NULL;
    cptr->promargs = promargs;
    R_GlobalContext = cptr;
}


/* endcontext - end an execution context */

void endcontext(RCNTXT * cptr)
{
    int savevis = R_Visible;
    if (cptr->cloenv != R_NilValue && cptr->conexit != R_NilValue )
	eval(cptr->conexit, cptr->cloenv);
    R_Visible = savevis;
    R_GlobalContext = cptr->nextcontext;
}


/* findcontext - find the correct context */

void findcontext(int mask, SEXP env, SEXP val)
{
    RCNTXT *cptr;
    cptr = R_GlobalContext;
    if (mask & CTXT_LOOP) {		/* break/next */
	if (cptr->callflag & CTXT_LOOP)
	    jumpfun(cptr, mask, val);
	else
	    error("No loop to break from, jumping to top level\n");
    }
    else {				/* return; or browser */
	for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext)
	    if ((cptr->callflag & mask) && cptr->cloenv == env)
		jumpfun(cptr, mask, val);
	error("No function to return from, jumping to top level\n");
    }
}


/* R_sysframe - look back up the context stack until the */
/* nth closure context and return that cloenv. */
/* R_sysframe(0) means the R_GlobalEnv environment */
/* negative n counts back from the current frame */
/* positive n counts up from the globalEnv */

SEXP R_sysframe(int n, RCNTXT *cptr)
{
    if (n == 0)
	return(R_GlobalEnv);

    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = -n;

    if(n < 0)
	errorcall(R_GlobalContext->call,
		  "not that many enclosing environments\n");

    while (cptr->nextcontext != NULL) {
	if (cptr->callflag == CTXT_RETURN) {
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
	error("sys.frame: not that many enclosing functions\n");
    return R_NilValue;     /* just for -Wall */
}


/* We need to find the environment that can be returned by sys.frame */
/* (so it needs to be on the cloenv pointer of a context) that matches */
/* the environment where the closure arguments are to be evaluated. */
/* It would be much simpler if sysparent just returned cptr->sysparent */
/* but then we wouldn't be compatible with S. */

int R_sysparent(int n, RCNTXT *cptr)
{
    int j;
    SEXP s;
    if(n <= 0)
	errorcall( R_ToplevelContext->call,
		   "only positive arguments are allowed\n");
    while (cptr->nextcontext != NULL && n > 1) {
	if (cptr->callflag == CTXT_RETURN)
	    n--;
	cptr = cptr->nextcontext;
    }
    /* make sure we're looking at a return context */
    while (cptr->nextcontext != NULL && cptr->callflag != CTXT_RETURN )
	cptr = cptr->nextcontext;
    s = cptr->sysparent;
    if(s == R_GlobalEnv)
	return 0;
    j = 0;
    while (cptr != NULL ) {
	if (cptr->callflag == CTXT_RETURN) {
	    j++;
	    if( cptr->cloenv == s )
		n=j;
	}
	cptr = cptr->nextcontext;
    }
    n = j - n + 1;
    if (n == 0)
	n = 1;
    if (n < 0)
	error("sys.parent: not that many enclosing functions\n");
    return n;
}

int framedepth(RCNTXT *cptr)
{
    int nframe = 0;
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag == CTXT_RETURN)
	    nframe++;
	cptr = cptr->nextcontext;
    }
    return nframe;
}

SEXP R_syscall(int n, RCNTXT *cptr)
{
    /* negative n counts back from the current frame */
    /* positive n counts up from the globalEnv */
    if (n > 0)
	n = framedepth(cptr)-n;
    else
	n = - n;
    if(n < 0 )
	errorcall(R_GlobalContext->call, "illegal frame number\n");
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag == CTXT_RETURN) {
	    if (n == 0)
		return (duplicate(cptr->call));
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL)
	return (duplicate(cptr->call));
    errorcall(R_GlobalContext->call, "not that many enclosing functions\n");
    return R_NilValue;  /* just for -Wall */
}

SEXP R_sysfunction(int n, RCNTXT *cptr)
{
    SEXP s, t;
    if (n > 0)
	n = framedepth(cptr) - n;
    else
	n = - n;
    if (n < 0 )
	errorcall(R_GlobalContext->call, "illegal frame number\n");
    while (cptr->nextcontext != NULL) {
	if (cptr->callflag == CTXT_RETURN) {
	    if (n == 0) {
		s = CAR(cptr->call);
		if (isSymbol(s))
		    t = findVar(s, cptr->sysparent);
		else if( isLanguage(s) )
		    t = eval(s, cptr->sysparent);
		else
		    t = R_NilValue;
		return(t);
	    }
	    else
		n--;
	}
	cptr = cptr->nextcontext;
    }
    if (n == 0 && cptr->nextcontext == NULL)
	return(findVar(CAR(cptr->call),cptr->sysparent));
    errorcall(R_GlobalContext->call, "not that many enclosing functions\n");
    return R_NilValue;  /* just for -Wall */
}

/*some real insantiy to keep Duncan sane*/

SEXP do_restart(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    RCNTXT *cptr;

    checkArity(op, args);
    
    if( !asLogical(CAR(args)) )
	return(R_NilValue);
    for(cptr = R_GlobalContext->nextcontext; cptr!= R_ToplevelContext; 
	    cptr = cptr->nextcontext) {
	if (cptr->callflag == CTXT_RETURN) {
		cptr->callflag = CTXT_RESTART;
		break;
	}
    }
    if( cptr == R_ToplevelContext )
	errorcall(call, "no function to restart\n");
    return(R_NilValue);
}

/* An implementation of S's frame access functions. They usually count */
/* up from the globalEnv while we like to count down from the currentEnv. */
/* So if the argument is negative count down if positive count up. */
/* We don't want to count the closure that do_sys is contained in so the */
/* indexing is adjusted to handle this. */

SEXP do_sys(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    int i, n, nframe;
    SEXP rval,t;
    RCNTXT *cptr;
    /* first find the context that sys.xxx needs to be evaluated in */
    cptr = R_GlobalContext;
    t = cptr->sysparent;
    while (cptr != R_ToplevelContext) {
	if (cptr->callflag == CTXT_RETURN)
	    if (cptr->cloenv == t)
		break;
	cptr = cptr->nextcontext;
    }

    if (length(args) == 1) {
	t = eval(CAR(args), rho);
	n = asInteger(t);
    }
    else
	n = - 1;

    if(n == NA_INTEGER)
	errorcall(call, "invalid number of environment levels\n");
    switch (PRIMVAL(op)) {
    case 1: /* parent */
	rval = allocVector(INTSXP,1);
	INTEGER(rval)[0] = R_sysparent(n, cptr);
	return rval;
    case 2: /* call */
	return R_syscall(n, cptr);
    case 3: /* frame */
	return R_sysframe(n, cptr);
    case 4: /* sys.nframe */
	rval=allocVector(INTSXP,1);
	INTEGER(rval)[0]=framedepth(cptr);
	return rval;
    case 5: /* sys.calls */
	nframe=framedepth(cptr);
	PROTECT(rval=allocList(nframe));
	t=rval;
	for(i=1 ; i<=nframe; i++, t=CDR(t))
	    CAR(t)=R_syscall(i,cptr);
	UNPROTECT(1);
	return rval;
    case 6: /* sys.frames */
	nframe=framedepth(cptr);
	PROTECT(rval=allocList(nframe));
	t=rval;
	for(i=1 ; i<=nframe ; i++, t=CDR(t))
	    CAR(t)=R_sysframe(i,cptr);
	UNPROTECT(1);
	return rval;
    case 7: /* sys.on.exit */
	if( R_GlobalContext->conexit )
	    return R_GlobalContext->conexit;
	else
	    return R_NilValue;
    case 8: /* sys.parents */
	nframe=framedepth(cptr);
	rval=allocVector(INTSXP,nframe);
	for(i=0; i<nframe ; i++ )
	    INTEGER(rval)[i]= R_sysparent(nframe-i,cptr);
	return rval;
    case 9: /* sys.function */
	return(R_sysfunction(n, cptr));
    default:
	error("internal error in do_sys\n");
	return R_NilValue;/* just for -Wall */
    }
}
