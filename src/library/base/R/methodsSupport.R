#  File src/library/base/R/methodsSupport.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

trace <- function(what, tracer, exit, at, print, signature, where = topenv(parent.frame()), edit = FALSE)
{
    needsAttach <- nargs() > 1L && !.isMethodsDispatchOn()
    if(needsAttach) {
        ns <- try(loadNamespace("methods"))
        if(isNamespace(ns))
            message("(loaded the methods namespace)", domain = NA)
        else
            stop("tracing functions requires the 'methods' package, but unable to load the 'methods' namespace")
    }
    else if(nargs() == 1L)
        return(.primTrace(what))
    tState <- tracingState(FALSE)
    on.exit(tracingState(tState))
    ## now call the version in the methods package, to ensure we get
    ## the correct namespace (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    value <- eval.parent(call)
    on.exit() ## no error
    tracingState(tState)
    value
}

untrace <- function(what, signature = NULL, where = topenv(parent.frame())) {
    ## NOTE: following test is TRUE after loadNamespace("methods") (even if not in search())
    MethodsDispatchOn <- .isMethodsDispatchOn()
    if(MethodsDispatchOn) {
        tState <- tracingState(FALSE)
        on.exit(tracingState(tState))
    }
    if(!MethodsDispatchOn)
        return(.primUntrace(what)) ## can't have called trace except in primitive form
    ## at this point we can believe that the methods namespace was successfully loaded
    ## now call the version in the methods package, to ensure we get
    ## the correct namespace (e.g., correct version of class())
    call <- sys.call()
    call[[1L]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    call$untrace <- TRUE
    value <- eval.parent(call)
    on.exit() ## no error
    tracingState(tState)
    invisible(value)
}


tracingState <- function(on = NULL) .Internal(traceOnOff(on))


asS4 <- function(object, flag = TRUE, complete = TRUE)
    .Internal(setS4Object(object, flag, complete))

asS3 <- function(object, flag = TRUE, complete = TRUE)
    .Internal(setS4Object(object, !as.logical(flag), complete))


.doTrace <- function(expr, msg) {
    on <- tracingState(FALSE)	   # turn it off QUICKLY (via a .Internal)
    if(on) {
	on.exit(tracingState(TRUE)) # restore on exit, keep off during trace
	if(!missing(msg)) {
	    call <- deparse(sys.call(sys.parent(1L)))
	    if(length(call) > 1L)
		call <- paste(call[[1L]], "....")
	    cat("Tracing", call, msg, "\n")
	}
	exprObj <- substitute(expr)
	eval.parent(exprObj)
    }
    NULL
}
