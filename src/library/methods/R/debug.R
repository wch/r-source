.debugMethod <- function(fun, text = "", condition = NULL, signature,
                         once = FALSE)
{
    stopifnot(is.null(condition), identical(text, ""))
    if (is.primitive(fun))
        fun <- getGeneric(fun)
    if(!is(fun, "standardGeneric"))
        stop("Function must be an S4 generic")
    
    if(isdebugged(fun, signature = signature))
        return(invisible(NULL))
    
    m <- selectMethod(fun, signature)
    bd <- body(m)

    isrematch <- isRematched(m)
    if(isrematch)
        bd <- body(bd[[2L]][[3L]])

    at <- if(is(bd, "{")) 2L else numeric()
    
    tracer <- if(once) {
        ## If the method is rematched we're in .local so we need to reach up one
        ## frame to get the generic and target in that case
        if (isrematch)
            quote(quote({untrace(sys.frame(2L)$.Generic,
                                 signature = sys.frame(2L)$.target);
                browser()}))
        else
            quote(quote({untrace(.Generic,
                                 signature = .target); browser()}))
    } else {
        quote(browser)
    }
    eval(substitute(trace(fun, tracer, signature = signature, print = TRUE,
                          at = at),
                    list(tracer=tracer)))

    invisible(NULL)
}

.undebugMethod <- function(fun, signature) {
    if (!isdebugged(fun, signature)) {
        warning("method is not being debugged")
    } else {
        if (is.primitive(fun))
            fun <- getGeneric(fun)
        untrace(fun, signature = signature)
    }
}

.isMethodDebugged <- function(fun, signature) {
    meth <- selectMethod(fun, signature)
    if(!is(meth, "MethodDefinitionWithTrace"))
        return(FALSE)
    bd <- body(meth)
    if(isRematched(meth)) {
        ## detect the .doTrace block that gets inserted.
        ## This should be specific enough to only catch traces
        ## added by debug (or fully equivalent trace calls)
        is(bd[[3L]], "{") &&
            identical(bd[[3L]][[2L]][1L], quote(.doTrace())) &&
                identical(bd[[3L]][[2L]][[2L]][1:3],
                          quote(trace(.local, tracer = browser)))
    } else {
        fstexp <- bd
        ## we need to cover function(x) x@y and function(x) {x;y;z} cases
        ## so peel off brackets until there aren't any, because "debugging"
        ## here translates to putting .doTrace(browser()) as the first
        ## non-bracket evaluated expression
        while(is(bd, "{"))
            bd <- bd[[2L]]
        identical(bd[1:2], quote(.doTrace(browser())))
    }
}
