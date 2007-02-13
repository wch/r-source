/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *  Copyright (C) 1998-2007   The R Development Core Team.
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
 *  Foundation, Inc., 51 Franklin Street Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Defn.h"

SEXP attribute_hidden do_debug(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op,args);
#define find_char_fun \
    if (isValidString(CAR(args))) {				\
	SEXP s;							\
	PROTECT(s = install(translateChar(STRING_ELT(CAR(args), 0))));	\
	SETCAR(args, findFun(s, rho));				\
	UNPROTECT(1);						\
    }
    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP)
	errorcall(call, _("argument must be a function"));
    switch(PRIMVAL(op)) {
    case 0:
	SET_DEBUG(CAR(args), 1);
	break;
    case 1:
	if( DEBUG(CAR(args)) != 1 )
	    warningcall(call, "argument is not being debugged");
	SET_DEBUG(CAR(args), 0);
	break;
    }
    return R_NilValue;
}

SEXP attribute_hidden do_trace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    checkArity(op, args);

    find_char_fun

    if (TYPEOF(CAR(args)) != CLOSXP &&
	TYPEOF(CAR(args)) != BUILTINSXP &&
	TYPEOF(CAR(args)) != SPECIALSXP)
	    errorcall(call, _("argument must be a function"));

    switch(PRIMVAL(op)) {
    case 0:
	SET_TRACE(CAR(args), 1);
	break;
    case 1:
	SET_TRACE(CAR(args), 0);
	break;
    }
    return R_NilValue;
}


/* maintain global trace state */

static Rboolean tracing_state = TRUE;
#define GET_TRACE_STATE tracing_state
#define SET_TRACE_STATE(value) tracing_state = value

SEXP R_traceOnOff(SEXP onOff) {
    SEXP value;
    Rboolean prev = GET_TRACE_STATE;
    if(length(onOff) > 0) {
        Rboolean _new = asLogical(onOff);
        if(_new == TRUE || _new == FALSE)
            SET_TRACE_STATE(_new);
        else
            error("Value for tracingState must be TRUE or FALSE");
    }
    value = allocVector(LGLSXP, 1);
    LOGICAL(value)[0] = prev;
    return value;
}

Rboolean attribute_hidden
R_current_trace_state() { return GET_TRACE_STATE; }


/* memory tracing */
/* report when a traced object is duplicated */

SEXP attribute_hidden do_memtrace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;
    char buffer[20];

    checkArity(op, args);

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

    SET_TRACE(object, 1);
    snprintf(buffer, 20, "<%p>", (void *) object);
    return mkString(buffer);
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
    return R_NilValue;
#endif
}


SEXP attribute_hidden do_memuntrace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object;

    checkArity(op, args);

    object=CAR(args);
    if (TYPEOF(object) == CLOSXP ||
	TYPEOF(object) == BUILTINSXP ||
	TYPEOF(object) == SPECIALSXP)
	errorcall(call, _("argument must not be a function"));

    if (TRACE(object))
	SET_TRACE(object, 0);
#else
    errorcall(call, _("R was not compiled with support for memory profiling"));
#endif
    return R_NilValue;
}


#ifndef R_MEMORY_PROFILING
void attribute_hidden memtrace_report(void* old, void *_new) {
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

void attribute_hidden memtrace_report(void * old, void * _new)
{
    if (!R_current_trace_state()) return;
    Rprintf("tracemem[%p -> %p]: ", (void *) old, _new);
    memtrace_stack_dump();
}

#endif /* R_MEMORY_PROFILING */

SEXP attribute_hidden do_memretrace(SEXP call, SEXP op, SEXP args, SEXP rho)
{
#ifdef R_MEMORY_PROFILING
    SEXP object, origin, ans;
    char buffer[20];

    /* checkArity(op, args); */
    if(length(args) < 1 || length(args) > 2)
	errorcall(call, _("invalid number of arguments"));

    object = CAR(args);
    if (TYPEOF(object) == CLOSXP ||
	TYPEOF(object) == BUILTINSXP ||
	TYPEOF(object) == SPECIALSXP)
	errorcall(call, _("argument must not be a function"));

    if(length(args) >= 2) {
	origin = CADR(args);
	if(!isString(origin))
	    errorcall(call, _("invalid '%s' argument"), "origin");
    } else origin = R_NilValue;

    if (TRACE(object)){
	snprintf(buffer, 20, "<%p>", (void *) object);
	ans = mkString(buffer);
    } else ans = R_NilValue;

    if (origin != R_NilValue){
	SET_TRACE(object, 1);
	if (R_current_trace_state()) {
	    Rprintf("tracemem[%s -> %p]: ", 
		    translateChar(STRING_ELT(origin, 0)), (void *) object);
	    memtrace_stack_dump();
	}
    }
    return ans;
#else
    return R_NilValue;
#endif
}
