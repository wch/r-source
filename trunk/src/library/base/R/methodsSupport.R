trace <- function(what, tracer, exit, at, print, signature, where = topenv(parent.frame()), edit = FALSE) {
    needsAttach <- nargs() > 1 && !.isMethodsDispatchOn()
    if(needsAttach) {
        ns <- try(loadNamespace("methods"))
        if(isNamespace(ns))
            message("(loaded the methods namespace)")
        else
            stop("Tracing functions requires the methods package, but unable to load methods namespace")
    }
    else if(nargs() == 1)
        return(.primTrace(what))
    tState <- tracingState(FALSE)
    ## now call the version in the methods package, to ensure we get
    ## the correct name space (e.g., correct version of class())
    call <- sys.call()
    call[[1]] <- quote(methods::.TraceWithMethods)
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
    ## the correct name space (e.g., correct version of class())
    call <- sys.call()
    call[[1]] <- quote(methods::.TraceWithMethods)
    call$where <- where
    call$untrace <- TRUE
    value <- eval.parent(call)
    on.exit() ## no error
    tracingState(tState)
    invisible(value)
}

.isMethodsDispatchOn <- function(onOff = NULL)
    .Call("R_isMethodsDispatchOn", onOff, PACKAGE = "base")

tracingState <- function( on = NULL)
    .Call("R_traceOnOff", on, PACKAGE = "base")

isS4 <- function(object)
    .Call("R_isS4Object", object, PACKAGE = "base")

asS4 <- function(object, value = TRUE) {
    value <- methods::as(value, "logical")
    if(length(value) != 1 || is.na(value))
      stop("Expected a single logical value for the S4 object state")
    .Call("R_setS4Object", object, value, PACKAGE = "base")
  }

.doTrace <- function(expr, msg) {
    on <- tracingState(FALSE) # turn it off QUICKLY (via a .Call)
    if(on) {
        on.exit(tracingState(TRUE)) # restore on exit, keep off during trace
        if(!missing(msg)) {
            call <- deparse(sys.call(sys.parent(1)))
            if(length(call)>1)
              call <- paste(call[[1]], "....")
            cat("Tracing", call, msg, "\n")
        }
        exprObj <- substitute(expr)
        eval.parent(exprObj)
    }
    NULL
}
