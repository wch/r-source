.findNextMethod <- function(method, f = "<unknown>", mlist, optional = FALSE, excluded) {
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
    ## delete the excluded methods
    for(signature in excluded)
        mlist <- insertMethod(mlist, as.character(signature), names(signature), NULL, FALSE)
    ## and now redo method selection 
    value <- selectMethod(f, method@target, optional, TRUE, mlist)
    value
}

callNextMethod <- function() {
    envir <- parent.frame()
    if(exists(".nextMethod", envir = envir, inherits = FALSE))
        nextMethod <- get(".nextMethod", envir = envir, inherits = FALSE)
    else {
        ## set up the nextMethod object, load it
        ## into the calling environment, and maybe cache it
        if(exists(".Method", envir = envir, inherits = FALSE)) {
            method <- get(".Method", envir = envir, inherits = FALSE)
            f <- get(".Generic", envir = envir)
            cache <- TRUE
        }
        else { ## not in an ordinary method: must be in another NextMethod call
            env2 <- parent.frame(2)
            if(exists(".nextMethod", envir = env2, inherits = FALSE)) {
               method <- get(".nextMethod", envir = env2, inherits = FALSE)
               f <- get(".Generic", envir = env2)
               cache <- FALSE
           }
            else
                stop("call to NextMethod doesn't appear to be in a method or NextMethod context")
        }
        if(is(method, "MethodDefinition")) {
            newMethod <- findNextMethod(method, f, getMethods(f))
            ## cache the method with the nextMethod included,
            ## so later calls will load this information.  But can't do this
            ## if this call was from a next method, because dispatch may be
            ## different
            if(cache)
                cacheMethod(f, method@target, newMethod)
            nextMethod <- newMethod@nextMethod
            assign(".nextMethod", nextMethod, envir = envir)
        }
        else
            stop("Can't use NextMethod:  the method isn't a MethodDefinition object")
    }
    .Call("R_nextMethodCall", formalArgs(nextMethod), envir, PACKAGE="methods")
}

loadMethod <- function(method, fname, envir)
    method

