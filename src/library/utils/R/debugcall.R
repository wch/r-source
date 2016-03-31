##  File src/library/utils/R/debugcall.R
##  Part of the R package, https://www.R-project.org
##
##  Copyright (C) 1995-2016 The R Core Team
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  A copy of the GNU General Public License is available at
##  https://www.R-project.org/Licenses/

.debugcall <- function(call, op) {
    funsym <- deparse(call[[1L]])
    func <- get(funsym, parent.frame(2L), mode="function")
    
    have.methods <- isNamespaceLoaded("methods")
    func <- if(is.primitive(func)) {
        if (have.methods) methods::getGeneric(func)
    } else func
    if(is.null(func)) {
        stop("Cannot debug primitive functions unless they are implicit generics (requires loading the methods package)")
    }
    mcall <- match.call(func, call)

    env <- parent.frame(2L)
    sig <- NULL
    s4Generic <- have.methods && methods::isGeneric(funsym)
    if(!s4Generic) {
        s3ret <- isS3stdGeneric(func)
        if(s3ret) {
            genname <- names(s3ret)
            arg <- eval(mcall[[2L]], envir=env) 
            func <- getS3method(genname, class(arg))
        }
    } else {
        sig <- .signatureFromCall(func, mcall, env)
    }
    op(func, signature = sig)
}

.signatureFromCall <- function(fdef, expr, envir, doEval = TRUE) {
    args <- formals(fdef)
    call <- match.call(fdef, expr, expand.dots = FALSE)
    args[names(call[-1L])] <- call[-1L]
    if ("..." %in% names(call)) 
        args$... <- args$...[[1L]]
    sigNames <- fdef@signature
    sigClasses <- rep.int("missing", length(sigNames))
    names(sigClasses) <- sigNames
    for (arg in sigNames) {
        argExpr <- methods::elNamed(args, arg)
        if (!missing(argExpr) && !is.null(argExpr)) {
            simple <- (is.character(argExpr) || is.name(argExpr))
            ## TODO:  ideally, if doEval is TRUE, we would like to
            ## create the same context used by applyClosure in
            ## eval.c, but then skip the actual evaluation of the
            ## body.  If we could create this environment then
            ## passing it to selectMethod is closer to the semantics
            ## of the "real" function call than the code below.
            ## But, seems to need a change to eval.c and a flag to
            ## the evaluator.
            if (doEval || !simple) {
                argVal <- try(eval(argExpr, envir))
                if (methods::is(argVal, "try-error")) 
                    stop(gettextf("error in trying to evaluate the expression for argument %s (%s)", 
                                  sQuote(arg), deparse(argExpr)), domain = NA)
                sigClasses[[arg]] <- class(argVal)[1L]
            }
            else sigClasses[[arg]] <- as.character(argExpr)
        }
    }
    sigClasses
}

debugcall <- function(call, once = FALSE)  {
    stopifnot(length(once) == 1L, is.logical(once), !is.na(once))
    call <- substitute(call)
    .debugcall(call, if (once) debugonce else debug)
    invisible(call)
}

undebugcall <- function(call)  {
    call <- substitute(call)
    .debugcall(call, undebug)
    invisible(NULL)
}
