#  File src/library/methods/R/NextMethod.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
#  https://www.R-project.org/Licenses/

callNextMethod <- function(...) {
    method <- nextMethod <-  NULL
    dotNextMethod <- as.name(".nextMethod")
    ## 2 environments are used here:  callEnv, from which the .nextMethod call
    ## takes place; and methodEnv, the method environment used to find the next method
    ## Because of the .local mechanism used to allow variable argument lists
    ## in methods (see rematchDefinition) these may be different.
    parent <- sys.parent(1)
    methodFun <- maybeMethod <- sys.function(parent)
    if(is(maybeMethod, "MethodDefinition")) {
        callEnv <- methodEnv <- parent.frame(1)
        mcall <- sys.call(parent)
        dotsenv <- parent.frame(2)
        i <- 1L
    }
    else {
        callEnv <- parent.frame(1)
        methodEnv <- parent.frame(2)
        mcall <- sys.call(sys.parent(2))
        dotsenv <- parent.frame(3)
        maybeMethod <- sys.function(sys.parent(2))
        i <- 2L
    }
    ## set up the nextMethod object, load it
    ## into the calling environment, and cache it
    if(!is.null(method <- methodEnv$.Method)) {
        ## call to standardGeneric(f)
        nextMethod <- callEnv$.nextMethod
        f <- methodEnv$.Generic
    }
    else if(identical(mcall[[1L]], dotNextMethod)) {
        ## a call from another callNextMethod()
        nextMethodEnv <- parent.frame(i+1L)
        nextMethod <- nextMethodEnv$.nextMethod
        f <- nextMethodEnv$.Generic
    }
    else if (is(maybeMethod, "MethodDefinition")) {
        f <- maybeMethod@generic
        method <- maybeMethod
    }
    else {
        ## may be a method call for a primitive; not available as .Method
        if (is.primitive(mcall[[1L]])) {
            f <- .primname(mcall[[1L]])
        } else {
            f <- as.character(mcall[[1L]])
        }
        fdef <- genericForBasic(f)
        ## check that this could be a basic function with methods
        if(is.null(fdef))
            stop(gettextf("a call to callNextMethod() appears in a call to %s, but the call does not seem to come from either a generic function or another 'callNextMethod'",
                          sQuote(f)),
                 domain = NA)
        f <- fdef@generic
        method <- maybeMethod
    }
    if(is(method, "MethodDefinition")) {
        if(is.null(nextMethod)) {
            if(!is(method, "MethodWithNext")) {
                method <- addNextMethod(method, f, envir=methodEnv)
                ## cache the method with the nextMethod included,
                ## so later calls will load this information.
                cacheMethod(f, method@target, method, fdef = getGeneric(f), inherited = TRUE)
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
            method <- addNextMethod(method, f, envir=methodEnv)
        }
        nextMethod <- method@nextMethod
        ## store the nextmethod in the previous nextmethod's
        assign(".nextMethod", nextMethod, envir = callEnv)
        assign(".Generic", f, envir = callEnv)
        assign(".nextMethod", method, envir = nextMethodEnv)
        assign(".Generic", f, envir = nextMethodEnv)
    }
    else
        stop(gettextf("bad object found as method (class %s)",
                      dQuote(class(method))), domain = NA)
    if (is.null(nextMethod))
        stop("No next method available")
    subsetCase <- !is.na(match(f, .BasicSubsetFunctions))
    if(nargs()>0) {
      call <- sys.call()
      call[[1L]] <- as.name(".nextMethod")
      eval(call, callEnv)
      }
    else {
        if(subsetCase) {
            ## don't use match.call, because missing args will screw up for "[", etc.
            call <- as.list(mcall)
            ## don't test with identical(), there may  be a package attr.
            if((f ==  "[") && length(names(call)>0))
                call <- .doSubNextCall(call, method) # [ with a drop= arg.
            else {
               fnames <- c("", formalArgs(method))
               i <- match("...",fnames)
               if(is.na(i) || i > length(call))
                   length(fnames) <- length(call)
               else {
                   i <- i-1L
                   length(fnames) <- i
                   fnames <- c(fnames, rep("", length(call) - i))
               }
               if (endsWith(f, "<-"))
                   fnames[length(fnames)] <- "value"
               names(call) <- fnames
               call <- as.call(call)
           }
        }
        else
            call <- match.call(methodFun, mcall, expand.dots = FALSE,
                               envir = dotsenv)
        .Call(C_R_nextMethodCall, call, callEnv)
    }
}

## Skeleton for the generic in ./MethodsListClass.R :
loadMethod <- function(method, fname, envir) method

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
        cnames <- if (is.null(names(call)))
                      rep("", length(call) - i)
                  else utils::tail(names(call), -i)
        fnames <- c(fnames, cnames)
    }
    names(call) <- fnames
    if(hasDrop)
        call$drop <- drop
    as.call(call)
}
