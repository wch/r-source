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
    ## and now redo method selection 
    value <- selectMethod(f, envir, optional, TRUE, mlist)
    value
}

callNextMethod <- function(...) {
    method <- NULL
    i <- 1
    while(is.null(method) && i < 3) {
        ## Because of the .local mechanism used to allow variable argument lists
        ## in methods (see rematchDefinition) may have to look back 2 frames
        envir <- parent.frame(i)
        ## set up the nextMethod object, load it
        ## into the calling environment, and maybe cache it
        if(exists(".Method", envir = envir, inherits = FALSE)) {
            method <- get(".Method", envir = envir, inherits = FALSE)
            f <- get(".Generic", envir = envir)
###  see test below:  cache-ing doesn't currently always work
            cache <- TRUE
        }
        else { ## not in an ordinary method: must be in another NextMethod call
            env2 <- parent.frame(2)
            if(exists(".nextMethod", envir = env2, inherits = FALSE)) {
                method <- get(".nextMethod", envir = env2, inherits = FALSE)
                f <- get(".Generic", envir = env2)
                cache <- FALSE
            }
        }
        i <- i+1
    }
    if(is.null(method))
        stop("call to NextMethod doesn't appear to be in a method or NextMethod context")
    if(is(method, "MethodDefinition")) {
            newMethod <- findNextMethod(method, f, getMethods(f), envir=envir)
            ## cache the method with the nextMethod included,
            ## so later calls will load this information.  But can't do this
            ## if this call was from a next method, because dispatch may be
            ## different
            if(cache && length(method@target)>0)  #BUG in findNextMethod?? (as of R 1.7)
                cacheMethod(f, method@target, newMethod)
            nextMethod <- newMethod@nextMethod
            assign(".nextMethod", nextMethod, envir = envir)
        }
    else
        stop("Can't use NextMethod:  the method isn't a MethodDefinition object")
    if(nargs()>0)
        eval(substitute(.nextMethod(...)), envir)
    else {
        call <- match.call(if(is.primitive(method)) get(".Method", envir = envir)
                   else method, sys.call(-1), expand.dots = FALSE)
        .Call("R_nextMethodCall",
              call,
              envir, PACKAGE="methods")
    }
}

loadMethod <- function(method, fname, envir)
    method

