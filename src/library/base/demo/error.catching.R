	##================================================================##
	###  In longer simulations, aka computer experiments,		 ###
	###  you may want to						 ###
	###  1) catch all errors and warnings (and continue)		 ###
	###  2) store the error or warning messages			 ###
	###								 ###
	###  Here's a solution	(see R-help mailing list, Dec 9, 2010):	 ###
	##================================================================##

##' Catch *and* save both errors and warnings, and in the case of
##' a warning, also keep the computed result.
##'
##' @title tryCatch both warnings (with value) and errors
##' @param expr an \R expression to evaluate
##' @return a list with 'value' and 'warning', where
##'   'value' may be an error caught.
##' @author Martin Maechler;
##' Copyright (C) 2010-2023  The R Core Team
tryCatch.W.E <- function(expr)
{
    W <- NULL
    w.handler <- function(w) { # warning handler
	W <<- w
	invokeRestart("muffleWarning")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
				     warning = w.handler),
	 warning = W)
}

str( tryCatch.W.E( log( 2 ) ) )
str( tryCatch.W.E( log( -1) ) )
str( tryCatch.W.E( log("a") ) )


##' @title Catch *all* warnings and the value
##' @param expr an \R expression to evaluate
##' @return a list with 'value' and 'warnings'
##' @author Luke Tierney (2004), R-help post
##'	https://stat.ethz.ch/pipermail/r-help/2004-June/052132.html
withWarnings <- function(expr) {
    W <- NULL
    wHandler <- function(w) {
	W <<- c(W, list(w))
	invokeRestart("muffleWarning")
    }
    val <- withCallingHandlers(expr, warning = wHandler)
    list(value = val, warnings = W)
}

withWarnings({ warning("first"); warning("2nd"); pi })

r <- withWarnings({ log(-1) + sqrt(-4); exp(1) })
str(r, digits=14)

##' @title tryCatch *all* warnings and messages, and an error or the final value
##' @param expr an \R expression to evaluate
##' @return a list with `messages`, `warnings`, and
##'         `value` which may be an error caught.
##' @author Martin Maechler (combining the above)
tryCatch_WEMs <- function(expr)
{
    W <- M <- NULL
    w.handler <- function(w) { # warning handler
	W <<- c(W, list(w)); invokeRestart("muffleWarning")
    }
    m.handler <- function(m) { # message handler
	M <<- c(M, list(m)); invokeRestart("muffleMessage")
    }
    list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
				     warning = w.handler, message = m.handler),
	 messages = M,
	 warnings = W)
}

f3 <- function(x) {
    r <- log(-x) + sqrt(-x) # produce warnings when x >= 0
    if(anyNA(r)) message(sprintf("%d NA's produced by log(.) + sqrt(.)", sum(is.na(r))))
    r <- exp(-x)
    if(any(ii <- is.infinite(r))) message(sprintf("Got +/- Inf from x[%s]", deparse(which(ii))))
    r
}

str( r0 <- tryCatch_WEMs(f3("A")) ) # just an error from '-x'
stopifnot(exprs = {
    inherits (r0$value, "error")
    identical(r0$value$call, quote(-x))
    sapply(r0[c("messages","warnings")], is.null)
})

(x <- c(-1:1, (-1:1)/0))
str( rI <- tryCatch_WEMs(f3(x) ))
stopifnot(exprs = {
    identical(lengths(rI), c(value = length(x), messages = 2L, warnings = 2L))
    rI$value[4] == Inf
    all.equal(rI$value, exp(-x))
    length(rI$messages) == 2; sapply(rI$messages, inherits, what="message")
    length(rI$warnings) == 2; sapply(rI$warnings, inherits, what="warning")
})


