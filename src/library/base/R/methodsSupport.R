trace <- function(what, tracer, exit, at, print, signature, where = topenv(parent.frame())) {
    needsAttach <- nargs() > 1 && !.isMethodsDispatchOn()
    if(needsAttach) {
        ns <- try(loadNamespace("methods"))
        if(isNamespace(ns))
            methods::message("(loaded the methods namespace)")
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
    if(is.function(what)) {
        fname <- substitute(what)
        if(is.name(fname))
            what <- as.character(fname)
        else
            stop("Argument what should be the name of a function")
    }
    else {
        what <- as.character(what)
        if(length(what) != 1) {
            for(f in what)
                untrace(f, signature)
            return(what)
        }
    }
    if(!MethodsDispatchOn)
        return(.primUntrace(what)) ## can't have called trace except in primitive form
    ## at this point we can believe that the methods namespace was successfully loaded
    f <- NULL
    if(is.null(signature)) {
        where <- methods::findFunction(what, where = where)
        if(length(where) == 0)
            warning("No function \"", what, "\" to untrace")
        else {
            f <- methods::getFunction(what, where = where[[1]])
            ## ensure that the version to assign is untraced (should be, but ...)
            if(methods::is(f, "traceable")) {
                methods::.untracedFunction(f, what, where[[1]])
            }
            else
                .primUntrace(what) # to be safe--no way to know if it's traced or not
        }
    }
    else {
        f <- methods::getMethod(what, signature,  where)
        if(is.null(f))
            warning("No method for \"", what, "\" for this signature to untrace")
        else {
            if(is(f, "traceable"))
                methods::.untracedFunction(f, what, where, signature)
            else
                warning("The method for \"", what, "\" for this signature was not being traced")
        }
    }
    invisible(f)
}

.isMethodsDispatchOn <- function(onOff = NULL)
    .Call("R_isMethodsDispatchOn", onOff, PACKAGE = "base")

tracingState <- function( on = NULL)
    .Call("R_traceOnOff", on, PACKAGE = "base")
