.findNextMethod <- function(method, f = "<unknown>", mlist, optional = FALSE, excluded, envir) {
    ## find the next method for dispatch, and return that method
    ## If no next method can be found, returns NULL or error
    ##
    ## The next method is defined as the method selected if the given method
    ## were not there.  The definition is applied literally by deleting the given
    ## method and then calling MethodListSelect.
    if(!is(method, "MethodDefinition"))
        stop("'NextMethod' not defined because the current method is not a 'MethodDefinition' object")
    ## remove all cached methods
    mlist@allMethods <- mlist@methods
    ## delete the excluded method(s)
    for(signature in excluded) {
        if(!is(signature, "signature"))
            stop(gettextf("expected a list of signature objects, got \"%s\"", class(signature)), domain = NA)
        if(length(signature)>0)
            mlist <- insertMethod(mlist, signature, names(signature), NULL, FALSE)
    }
    ## and now redo method selection.  Note the use of
    ## method@target, not the actual environment to force a consistent
    ## nextMethod in the methods list object for the generic.  (Inconsistent
    ## results are possible with next methods)
    value <- selectMethod(f, method@target, optional, TRUE, mlist)
    if(!is(value, "MethodWithNext") && is(value, "MethodDefinition") &&
       .hasCallNextMethod(value@.Data)) {
        ## complete the chain of callNextMethod's
        excluded <- c(excluded, list(value@defined))
        nextMethod <- .findNextMethod(value, f, mlist, optional, excluded, envir)
        value <- new("MethodWithNext", value, nextMethod = nextMethod, excluded = excluded)
    }
    value
}

## find a call to callNextMethod in the body or one of the default arg. expressions
## (If the R version of all.names, all.vars worked on function definitions would be no need
## for the loop)
.hasCallNextMethod <- function(def) {
    if(!identical(typeof(def), "closure"))
        return(FALSE)
    def <- as.list(def)
    for(i in rev(seq(along = def))) {
        if(is.call(def[[i]]) && !is.na(match("callNextMethod", all.names(def[[i]]))))
            return(TRUE)
    }
    FALSE
}

callNextMethod <- function(...) {
    method <- nextMethod <-  NULL
    dotNextMethod <- as.name(".nextMethod")
    ## 2 environments are used here:  callEnv, from which the .nextMethod call
    ## takes place; and methodEnv, the method environment used to find the next method
    ## Because of the .local mechanism used to allow variable argument lists
    ## in methods (see rematchDefinition) these may be different.
    parent <- sys.parent(1)
    maybeMethod <- sys.function(parent)
    if(is(maybeMethod, "MethodDefinition")) {
        callEnv <- methodEnv <- parent.frame(1)
        mcall <- sys.call(parent)
        i <- 1
    }
    else {
        callEnv <- parent.frame(1)
        methodEnv <- parent.frame(2)
        mcall <- sys.call(sys.parent(2))
        i <- 2
    }
    ## set up the nextMethod object, load it
    ## into the calling environment, and cache it
    if(exists(".Method", envir = methodEnv, inherits = FALSE)) {
        ## call to standardGeneric(f)
        method <- get(".Method", envir = methodEnv, inherits = FALSE)
        if(exists(".nextMethod", envir = callEnv, inherits = FALSE))
            nextMethod <- get(".nextMethod", envir = callEnv)
        f <- get(".Generic", envir = methodEnv)
    }
    else if(identical(mcall[[1]], dotNextMethod)) {
        ## a call from another callNextMethod()
        nextMethodEnv <- parent.frame(i+1)
        nextMethod <- get(".nextMethod", nextMethodEnv)
        f <- get(".Generic", envir = nextMethodEnv)
    }
    else {
        ## may be a method call for a primitive; not available as .Method
        f <- as.character(mcall[[1]])
        fdef <- genericForPrimitive(f)
        ## check that this could be a basic function with methods
        if(is.null(fdef))
            stop(gettextf("a call to callNextMethod() appears in a call to '%s', but the call does not seem to come from either a generic function or another 'callNextMethod'", f), domain = NA)
        f <- fdef@generic
        method <- maybeMethod
    }
    if(is(method, "MethodDefinition")) {
        if(is.null(nextMethod)) {
            if(!is(method, "MethodWithNext")) {
                method <- addNextMethod(method, f, getMethods(f), envir=methodEnv)
                ## cache the method with the nextMethod included,
                ## so later calls will load this information.
                cacheMethod(f, method@target, method, fdef = getGeneric(f))
            }
            nextMethod <- method@nextMethod
            assign(".nextMethod", nextMethod, envir = callEnv)
            assign(".Generic", f, envir = callEnv)
        }
    }
    else if(is.null(method)) {
        if(is.null(nextMethod))
            stop("call to 'callNextMethod' does not appear to be in a 'method' or 'callNextMethod' context")
        ## else, callNextMethod() from another callNextMethod
        method <- nextMethod
        if(!is(method, "MethodWithNext")) {
            method <- addNextMethod(method, f, getMethods(f), envir=methodEnv)
        }
        nextMethod <- method@nextMethod
        ## store the nextmethod in the previous nextmethod's
        assign(".nextMethod", nextMethod, envir = callEnv)
        assign(".Generic", f, envir = callEnv)
        assign(".nextMethod", method, envir = nextMethodEnv)
        assign(".Generic", f, envir = nextMethodEnv)
    }
    else
        stop(gettextf("bad object found as method (class \"%s\")",
                      class(method)), domain = NA)
    subsetCase <- !is.na(match(f, .BasicSubsetFunctions))
    if(nargs()>0)
        eval(substitute(.nextMethod(...)), callEnv)
    else {
        if(subsetCase) {
            ## don't use match.call, because missing args will screw up for "[", etc.
            call <- as.list(mcall)
            if(identical(f, "[") && length(names(call)>0))
                call <- .doSubNextCall(call, method) # [ with a drop= arg.
            else {
               fnames <- c("", formalArgs(method))
               i <- match("...",fnames)
               if(is.na(i) || i > length(call))
                   length(fnames) <- length(call)
               else {
                   i <- i-1
                   length(fnames) <- i
                   fnames <- c(fnames, rep("", length(call) - i))
               }
               names(call) <- fnames
               call <- as.call(call)
           }
        }
        else
            call <- match.call(method, mcall, expand.dots = FALSE)
        .Call("R_nextMethodCall",
              call,
              callEnv, PACKAGE="methods")
    }
}

loadMethod <- function(method, fname, envir)
    method

.doSubNextCall <- function(call, method) {
    idrop <- match("drop", names(call))
    hasDrop <- !is.na(idrop)
    if(hasDrop) {
        drop <- call$drop
        call <- call[-idrop]
    }
    fnames <- c("", formalArgs(method))
    i <- match("...",fnames)
    if(is.na(i) || i > length(call))
        length(fnames) <- length(call)
    else {
        i <- i-1
        length(fnames) <- i
        fnames <- c(fnames, rep("", length(call) - i))
    }
    names(call) <- fnames
    if(hasDrop)
        call$drop <- drop
    as.call(call)
}
