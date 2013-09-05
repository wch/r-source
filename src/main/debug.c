/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2013   The R Core Team.
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
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#define R_USE_SIGNALS 1
#include <Defn.h>
#include <Internal.h>

SEXP attribute_hidden do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    SEXP ans = R_NilValue;

    checkArity(op,args);
#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = installTrChar(STRING_ELT(CAR(args), 0)));	\
	SETCAR(args, findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP && TYPEOF(CAR(args)) != SPECIALSXP 
         &&  TYPEOF(CAR(args)) != BUILTINSXP )
	errorcall(call, _("argument must be a closure"));
    switch(PRIMVAL(op)) {
    case 0:
	SET_RDEBUG(CAR(args), 1);
	break;
    case 1:
	if( RDEBUG(CAR(args)) != 1 )
	    warningcall(call, "argument is not being debugged");
	SET_RDEBUG(CAR(args), 0);
	break;
    case 2:
        ans = ScalarLogical(RDEBUG(CAR(args)));
        break;
    case 3:
        SET_RSTEP(CAR(args), 1);
        break;
    }
    return ans;
}

/* primitives .primTrace and .primUntrace */
SEXP attribute_hidden do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    check1arg(args, call, "x");

    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP)
	    errorcall(call, _("argument must be a function"));

    switch(PRIMVAL(op)) {
    case 0:
	SET_RTRACE(CAR(args), 1);
	break;
    case 1:
	SET_RTRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace state */

static Rboolean tracing_state = TRUE;
#define GET_TRACE_STATE tracing_state
#define SET_TRACE_STATE(value) tracing_state = value

SEXP attribute_hidden do_traceOnOff(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);
    SEXP onOff = CAR(args);

    Rboolean prev = GET_TRACE_STATE;
    if(length(onOff) > 0) {
	Rboolean _new = asLogical(onOff);
	if(_new == TRUE || _new == FALSE)
	    SET_TRACE_STATE(_new);
	else
	    error("Value for tracingState must be TRUE or FALSE");
    }
    return ScalarLogical(prev);
}

Rboolean attribute_hidden
R_current_trace_state() { return GET_TRACE_STATE; }


/* memory tracing */
/* report when a traced object is duplicated */

SEXP attribute_hidden do_tracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;
    char buffer[21];

    checkArity(op, args);
    check1arg(args, call, "x");

    object = CAR(args);
    if (TYPEOF(object) == CLOSXP ||
	TYPEOF(object) == BUILTINSXP ||
	TYPEOF(object) == SPECIALSXP)
	errorcall(call, _("argument must not be a function"));

    if(object == R_NilValue)
	errorcall(call, _("cannot trace NULL"));

    if(TYPEOF(object) == ENVSXP || TYPEOF(object) == PROMSXP)
	errorcall(call,
		  _("'tracemem' is not useful for promise and environment objects"));
    if(TYPEOF(object) == EXTPTRSXP || TYPEOF(object) == WEAKREFSXP)
	errorcall(call,
		  _("'tracemem' is not useful for weak reference or external pointer objects"));

    SET_RTRACE(object, 1);
    snprintf(buffer, 21, "<%p>", (void *) object);
    return mkString(buffer);
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
    return R_NilValue;
#endif
}


SEXP attribute_hidden do_untracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;

    checkArity(op, args);
    check1arg(args, call, "x");

    object=CAR(args);
    if (TYPEOF(object) == CLOSXP ||
	TYPEOF(object) == BUILTINSXP ||
	TYPEOF(object) == SPECIALSXP)
	errorcall(call, _("argument must not be a function"));

    if (RTRACE(object))
	SET_RTRACE(object, 0);
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
#endif
    return R_NilValue;
}


#ifndef R_MEMORY_PROFILING
void memtrace_report(void* old, void *_new) {
    return;
}
#else
static void memtrace_stack_dump(void)
{
    RCNTXT *cptr;

    for (cptr = R_GlobalContext; cptr; cptr = cptr->nextcontext) {
	if ((cptr->callflag & (CTXT_FUNCTION | CTXT_BUILTIN))
	    && TYPEOF(cptr->call) == LANGSXP) {
	    SEXP fun = CAR(cptr->call);
	    Rprintf("%s ",
		    TYPEOF(fun) == SYMSXP ? translateChar(PRINTNAME(fun)) :
		    "<Anonymous>");
	}
    }
    Rprintf("\n");
}

void memtrace_report(void * old, void * _new)
{
    if (!R_current_trace_state()) return;
    Rprintf("tracemem[%p -> %p]: ", (void *) old, _new);
    memtrace_stack_dump();
}

#endif /* R_MEMORY_PROFILING */

SEXP attribute_hidden do_retracemem(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, previous, ans, ap, argList;
    char buffer[21];

    PROTECT(ap = list2(R_NilValue, R_NilValue));
    SET_TAG(ap,  install("x"));
    SET_TAG(CDR(ap), install("previous"));
    PROTECT(argList =  matchArgs(ap, args, call));
    if(CAR(argList) == R_MissingArg) SETCAR(argList, R_NilValue);
    if(CADR(argList) == R_MissingArg) SETCAR(CDR(argList), R_NilValue);

    object = CAR(ap);
    if (TYPEOF(object) == CLOSXP ||
	TYPEOF(object) == BUILTINSXP ||
	TYPEOF(object) == SPECIALSXP)
	errorcall(call, _("argument must not be a function"));

    previous = CADR(ap);
    if(!isNull(previous) && !isString(previous))
	    errorcall(call, _("invalid '%s' argument"), "previous");

    if (RTRACE(object)) {
	snprintf(buffer, 21, "<%p>", (void *) object);
	ans = mkString(buffer);
    } else {
	R_Visible = 0;
	ans = R_NilValue;
    }

    if (previous != R_NilValue){
	SET_RTRACE(object, 1);
	if (R_current_trace_state()) {
	    /* FIXME: previous will have <0x....> whereas other values are
	       without the < > */
	    Rprintf("tracemem[%s -> %p]: ",
		    translateChar(STRING_ELT(previous, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    UNPROTECT(2);
    return ans;
#else
    R_Visible = 0; /* for consistency with other case */
    return R_NilValue;
#endif
}
