.findNextMethod <- function(method, f = "<unknown>", mlist, optional = FALSE, excluded, envir) {
    ## find the next method for dispatch, and return that method
    ## If no next method can be found, returns NULL or error
    ##
    ## The next method is defined as the method selected if the given method
    ## were not there.  The definition is applied literally by deleting the given
    ## method and then calling MethodListSelect.
    if(!is(method, "MethodDefinition"))
        stop("NextMethod not defined because the the current method is not a MethodDefinition object")
    ## remove all cached methods
    mlist@allMethods <- list()
    ## delete the excluded method(s)
    for(signature in excluded) {
        if(!is(signature, "signature"))
            stop("expected a list of signature objects, got \"", class(signature), "\"")
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
    method <- nextMethod <- mcall <- NULL
    dotNextMethod <- as.name(".nextMethod")
    basics <- names(.BasicFunsList) # global list of primitives allowed as generics.
    ## Because of the .local mechanism used to allow variable argument lists
    ## in methods (see rematchDefinition) may have to look back 2 frames
    for(i in 1:2) {
        maybeMethod <- sys.function(-i)
        if(!is(maybeMethod, "MethodDefinition"))
            next
        envir <- parent.frame(i)
        ## set up the nextMethod object, load it
        ## into the calling environment, and maybe cache it
        mcall <- sys.call(-i)
        if(exists(".Method", envir = envir, inherits = FALSE)) {
            ## call to standardGeneric(f)
            method <- get(".Method", envir = envir, inherits = FALSE)
            if(exists(".nextMethod", envir = envir, inherits = FALSE))
                nextMethod <- get(".nextMethod", envir = envir)
            f <- get(".Generic", envir = envir)
            break
        }
        else if(identical(mcall[[1]], dotNextMethod)) {
            ## a call from another callNextMethod()
            env2 <- parent.frame(i+1)
            nextMethod <- get(".nextMethod", env2)
            f <- get(".Generic", envir = env2)
            break
        }
        else {
            ### may be a method call for a primitive 
            f <- as.character(mcall[[1]])
            if(!is.na(match(f, basics))) {
                method <- maybeMethod
                envir <- if(i > 1) parent.frame(i-1) else sys.frame(sys.nframe())
                break
            }
        }
    }
    if(is(method, "MethodDefinition")) {
        if(is.null(nextMethod)) {
            if(!is(method, "MethodWithNext")) {
                method <- addNextMethod(method, f, getMethods(f), envir=envir)
                ## cache the method with the nextMethod included,
                ## so later calls will load this information.
                cacheMethod(f, method@target, method)
            }
            nextMethod <- method@nextMethod
            assign(".nextMethod", nextMethod, envir = envir)
            assign(".Generic", f, envir = envir)
        }
    }
    else if(is.null(method)) {
        if(is.null(nextMethod))
            stop("call to NextMethod doesn't appear to be in a method or callNextMethod context")
        ## else, callNextMethod() from another callNextMethod
        method <- nextMethod
        if(!is(method, "MethodWithNext")) {
            method <- addNextMethod(method, f, getMethods(f), envir=envir)
        }
        nextMethod <- method@nextMethod
        ## store the nextmethod in the previous nextmethod's 
        assign(".nextMethod", nextMethod, envir = envir)
        assign(".Generic", f, envir = envir)
        assign(".nextMethod", method, envir = env2)
        assign(".Generic", f, envir = env2)
    }
    else 
        stop("Bad object found as method (class \"", class(method), "\")")
    if(nargs()>0)
        eval(substitute(.nextMethod(...)), envir)
    else {
        call <- match.call(method, mcall, expand.dots = FALSE)
        .Call("R_nextMethodCall",
              call,
              envir, PACKAGE="methods")
    }
}

loadMethod <- function(method, fname, envir)
    method

