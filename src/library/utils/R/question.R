"?" <- function(e1, e2)
{
    e1Expr <- substitute(e1)
    if(missing(e2)) {
        if(is.call(e1Expr))
            return(.helpForCall(e1Expr, parent.frame()))
        if(is.name(e1Expr))
            e1 <- as.character(e1Expr)
        eval(substitute(help(TOPIC), list(TOPIC = e1)))
    }
    else {
        ## interpret e1 as a type, but to allow customization, do NOT
        ## force arbitrary expressions to be single character strings
        ## (so that methods can be defined for topicName).
        if(is.name(e1Expr))
            e1 <- as.character(e1Expr)
        e2Expr <- substitute(e2)
        if(is.name(e2Expr))
            e2 <- as.character(e2Expr)
        else if(is.call(e2Expr) && identical(e1, "method"))
            return(.helpForCall(e2Expr, parent.frame(), FALSE))
        topic <- topicName(e1, e2)
        doHelp <- .tryHelp(topic)
        if(inherits(doHelp, "try-error")) {
            stop("no documentation of type ", sQuote(e1),
                 " and topic ", sQuote(e2),
                 " (or error in processing help)")
        }
    }
}

topicName <- function(type, topic)
{
    if((length(type) == 0) || (length(topic) == 0))
        character(0)
    else
        paste(paste(topic, collapse = ","), type, sep = "-")
}

.helpForCall <- function(expr, envir, doEval = TRUE) {
    f <- expr[[1]]                      # the function specifier
    where <- topenv(envir)              # typically .GlobalEnv
    if(is.name(f))
        f <- as.character(f)
    if(!.isMethodsDispatchOn() || !isGeneric(f, where = where)) {
        if(!is.character(f) || length(f) != 1)
            stop("the object of class ", dQuote(class(f)),
                 " in the function call ", sQuote(deparse(expr)),
                 " could not be used as a documentation topic")
        h <- .tryHelp(f)
        if(inherits(h, "try-error"))
            stop("no methods for ", sQuote(f),
                 " and no documentation for it as a function")
    }
    else {
        ## allow generic function objects or names
        if(is(f, "genericFunction")) {
            fdef <- f
            f <- fdef@generic
        }
        else
            fdef <- getGeneric(f, where = where)
        call <- match.call(fdef, expr)
        ## make the signature
        sigNames <- fdef@signature
        sigClasses <- rep.int("missing", length(sigNames))
        names(sigClasses) <- sigNames
        for(arg in sigNames) {
            argExpr <- elNamed(call, arg)
            if(!is.null(argExpr)) {
                simple <- (is.character(argExpr) || is.name(argExpr))
                ## TODO:  ideally, if doEval is TRUE, we would like to
                ## create the same context used by applyClosure in
                ## eval.c, but then skip the actual evaluation of the
                ## body.  If we could create this environment then
                ## passing it to selectMethod is closer to the semantics
                ## of the "real" function call than the code below.
                ## But, seems to need a change to eval.c and a flag to
                ## the evaluator.
                if(doEval || !simple) {
                    argVal <- try(eval(argExpr, envir))
                    if(is(argVal, "try-error"))
                        stop("error in trying to evaluate the expression for argument ",
                             sQuote(arg), " (", deparse(argExpr), ")")
                    elNamed(sigClasses, arg) <- class(argVal)
                }
                else
                    elNamed(sigClasses, arg) <- as.character(argExpr)
            }
        }
        method <- selectMethod(f, sigClasses, optional=TRUE, fdef = fdef)
        if(is(method, "MethodDefinition"))
            sigClasses <- method@defined
        else
            warning("no method defined for function ", sQuote(f),
                    " and signature ",
                    paste(sigNames, " = ", dQuote(sigClasses), sep = "",
                          collapse = ", "))
        topic <- topicName("method", c(f,sigClasses))
        h <- .tryHelp(topic)
        if(is(h, "try-error"))
            stop("no documentation for function ", sQuote(f),
                 " and signature ",
                 paste(sigNames, " = ", dQuote(sigClasses), sep = "",
                       collapse = ", "))
    }
}

.tryHelp <- function(topic) {
    opts <- options(error = function() TRUE,
                    show.error.messages = FALSE)
    on.exit(options(opts))
    x <- try(do.call("help", list(topic)))
    ## If something was found, actually show it via print().
    ## Otherwise, give an error.
    if(!inherits(x, "try-error") && length(x))
        print(x)
    else
        try(stop())
    ## Argh ...
}
