#  File src/library/base/R/conditions.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

##
## Handling Conditions
##

## CARE:  try() in ./New-Internal.R  depends on *internal* coding of tryCatch()!
## ----   If you change this, be sure to adapt  try().
tryCatch <- function(expr, ..., finally) {
    tryCatchList <- function(expr, names, parentenv, handlers) {
	nh <- length(names)
	if (nh > 1L)
	    tryCatchOne(tryCatchList(expr, names[-nh], parentenv,
                                     handlers[-nh]),
			names[nh], parentenv, handlers[[nh]])
	else if (nh == 1L)
	    tryCatchOne(expr, names, parentenv, handlers[[1L]])
	else expr
    }
    tryCatchOne <- function(expr, name, parentenv, handler) {
	doTryCatch <- function(expr, name, parentenv, handler) {
	    .Internal(.addCondHands(name, list(handler), parentenv,
				    environment(), FALSE))
	    expr
	}
	value <- doTryCatch(return(expr), name, parentenv, handler)
	# The return in the call above will exit tryCatchOne unless
	# the handler is invoked; we only get to this point if the handler
	# is invoked.  If we get here then the handler will have been
	# popped off the internal handler stack.
	if (is.null(value[[1L]])) {
	    # a simple error; message is stored internally
	    # and call is in result; this defers all allocs until
	    # after the jump
	    msg <- .Internal(geterrmessage())
	    call <- value[[2L]]
	    cond <- simpleError(msg, call)
	}
        else if (is.character(value[[1L]])) {
            # if the jump for a simple error is intercepted to handle
            # an on.exit() action then the error message is encoded as
            # a character object at that point
	    msg <- value[[1L]]
	    call <- value[[2L]]
	    cond <- simpleError(msg, call)
        }
	else cond <- value[[1L]]
	value[[3L]](cond)
    }
    if (! missing(finally))
        on.exit(finally)
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("bad handler specification")
    tryCatchList(expr, classes, parentenv, handlers)
}

withCallingHandlers <- function(expr, ...) {
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("bad handler specification")
    .Internal(.addCondHands(classes, handlers, parentenv, NULL, TRUE))
    expr
}

suppressWarnings <- function(expr) {
    ops <- options(warn = -1) ## FIXME: temporary hack until R_tryEval
    on.exit(options(ops))     ## calls are removed from methods code
    withCallingHandlers(expr,
                        warning=function(w)
                            invokeRestart("muffleWarning"))
}


##
## Conditions and Condition Signaling
##

simpleCondition <- function(message, call = NULL) {
    class <- c("simpleCondition", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

simpleError <- function(message, call = NULL) {
    class <- c("simpleError", "error", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

simpleWarning <- function(message, call = NULL) {
    class <- c("simpleWarning", "warning", "condition")
    structure(list(message=as.character(message), call = call), class=class)
}

errorCondition <- function(message, ..., class = NULL, call = NULL)
    structure(list(message = as.character(message), call = call, ...),
              class = c(class, "error", "condition"))

warningCondition <- function(message, ..., class = NULL, call = NULL)
    structure(list(message = as.character(message), call = call, ...),
              class = c(class, "warning", "condition"))

conditionMessage <- function(c) UseMethod("conditionMessage")
conditionCall <- function(c) UseMethod("conditionCall")

conditionMessage.condition <- function(c) c$message
conditionCall.condition <- function(c) c$call

print.condition <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (! is.null(call))
        cat("<", cl, " in ", deparse(call), ": ", msg, ">\n", sep="")
    else
        cat("<", cl, ": ", msg, ">\n", sep="")
    invisible(x)
}

as.character.condition <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    cl <- class(x)[1L]
    if (! is.null(call))
        paste0(cl, " in ", deparse(call)[1L], ": ", msg, "\n")
    else
        paste0(cl, ": ", msg, "\n")
}

as.character.error <- function(x, ...) {
    msg <- conditionMessage(x)
    call <- conditionCall(x)
    if (! is.null(call))
        paste0("Error in ", deparse(call)[1L], ": ", msg, "\n")
    else
        paste0("Error: ", msg, "\n")
}

signalCondition <- function(cond) {
    if (! inherits(cond, "condition"))
        cond <- simpleCondition(cond)
    msg <- conditionMessage(cond)
    call <- conditionCall(cond)
    .Internal(.signalCondition(cond, msg, call))
}


##
##  Restarts
##

restartDescription <- function(r) r$description
restartFormals <- function(r) formals(r$handler)

print.restart <- function(x, ...) {
    cat(paste("<restart:", x[[1L]], ">\n"))
    invisible(x)
}

isRestart <- function(x) inherits(x, "restart")

findRestart <- function(name, cond = NULL) {
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r))
            return(NULL)
        else if (name == r[[1L]] &&
                 (is.null(cond) || is.null(r$test) || r$test(cond)))
            return(r)
        else i <- i + 1L
    }
}

computeRestarts <- function(cond = NULL) {
    val <- NULL
    i <- 1L
    repeat {
        r <- .Internal(.getRestart(i))
        if (is.null(r))
            return(val)
        else if (is.null(cond) || is.null(r$test) || r$test(cond))
            val <- c(val, list(r))
        i <- i + 1L
    }
}

invokeRestart <- function(r, ...) {
    if (! isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res))
            stop(gettextf("no 'restart' '%s' found", as.character(r)),
                 domain = NA)
        r <- res
    }
    .Internal(.invokeRestart(r, list(...)))
}

invokeRestartInteractively <- function(r) {
    if (! interactive())
        stop("not an interactive session")
    if (! isRestart(r)) {
        res <- findRestart(r)
        if (is.null(res))
            stop(gettextf("no 'restart' '%s' found", as.character(r)),
                 domain = NA)
        r <- res
    }
    if (is.null(r$interactive)) {
        pars <- names(restartFormals(r))
        args <- NULL
        if (length(pars)) {
            cat("Enter values for restart arguments:\n\n")
            for (p in pars) {
            if (p == "...") {
		    prompt <- "... (a list): "
		    args <- c(args, eval(parse(prompt = prompt)))
		}
		else {
		    prompt <- paste0(p, ": ")
		    args <- c(args, list(eval(parse(prompt = prompt))))
		}
	    }
	}
    }
    else args <- r$interactive()
    .Internal(.invokeRestart(r, args))
}

withRestarts <- function(expr, ...) {
    docall <- function(fun, args) {
	if ((is.character(fun) && length(fun) == 1L) || is.name(fun))
	    fun <- get(as.character(fun), envir = parent.frame(),
                       mode = "function")
	do.call("fun", lapply(args, enquote))
    }
    makeRestart <- function(name = "",
			   handler = function(...) NULL,
			   description = "",
			   test = function(c) TRUE,
			   interactive = NULL) {
	structure(list(name = name, exit = NULL, handler = handler,
		       description = description, test = test,
		       interactive = interactive),
		  class = "restart")
    }
    makeRestartList <- function(...) {
        specs <- list(...)
        names <- names(specs)
        restarts <- vector("list", length(specs))
        for (i in seq_along(specs)) {
            spec <- specs[[i]]
            name <- names[i]
            if (is.function(spec))
                restarts[[i]] <- makeRestart(handler = spec)
            else if (is.character(spec))
                restarts[[i]] <- makeRestart(description = spec)
            else if (is.list(spec))
                restarts[[i]] <- docall("makeRestart", spec)
            else
               stop("not a valid restart specification")
            restarts[[i]]$name <- name
        }
        restarts
    }
    withOneRestart <- function(expr, restart) {
	doWithOneRestart <- function(expr, restart) {
	    restart$exit <- environment()
	    .Internal(.addRestart(restart))
	    expr
	}
	restartArgs <- doWithOneRestart(return(expr), restart)
	# The return in the call above will exit withOneRestart unless
	# the restart is invoked; we only get to this point if the restart
	# is invoked.  If we get here then the restart will have been
	# popped off the internal restart stack.
	docall(restart$handler, restartArgs)
    }
    withRestartList <- function(expr, restarts) {
	nr <- length(restarts)
	if (nr > 1L)
	    withOneRestart(withRestartList(expr, restarts[-nr]),
                           restarts[[nr]])
	else if (nr == 1L)
	    withOneRestart(expr, restarts[[1L]])
	else expr
    }
    restarts <- makeRestartList(...)
    if (length(restarts) == 0L)
        expr
    else if (length(restarts) == 1L)
        withOneRestart(expr, restarts[[1L]])
    else withRestartList(expr, restarts)
}


##
## Callbacks
##

.signalSimpleWarning <- function(msg, call)
    withRestarts({
           .Internal(.signalCondition(simpleWarning(msg, call), msg, call))
           .Internal(.dfltWarn(msg, call))
        }, muffleWarning = function() NULL)

.handleSimpleError <- function(h, msg, call)
    h(simpleError(msg, call))

.tryResumeInterrupt <- function() {
    r <- findRestart("resume")
    if (! is.null(r))
        invokeRestart(r)
}


##
## Suspending/Allowing Interrupts
##


suspendInterrupts <- function(expr) {
    suspended <- .Internal(interruptsSuspended())
    if (suspended)
        expr
    else {
        on.exit(.Internal(interruptsSuspended(suspended)))
        .Internal(interruptsSuspended(TRUE))
        expr
    }
}

allowInterrupts <- function(expr) {
    suspended <- .Internal(interruptsSuspended())
    if (suspended) {
        on.exit(.Internal(interruptsSuspended(suspended)))
        .Internal(interruptsSuspended(FALSE))
        expr
    }
    else
        expr
}
