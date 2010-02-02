#  File src/library/stats/R/logLik.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

logLik <- function(object, ...) UseMethod("logLik")

print.logLik <- function(x, digits = getOption("digits"), ...)
{
    cat("'log Lik.' ", paste(format(c(x), digits=digits), collapse=", "),
        " (df=",format(attr(x,"df")),")\n",sep="")
    invisible(x)
}

str.logLik <- function(object, digits = max(2, getOption("digits") - 3),
                       vec.len = getOption("str")$vec.len, ...)
{
    cl <- oldClass(object)
    len <- length(co <- c(object))
    cutl <- len > vec.len
    cat("Class", if (length(cl) > 1L) "es",
	" '", paste(cl, collapse = "', '"), "' : ",
	paste(format(co[seq_len(min(len,vec.len))], digits=digits),
	      collapse = ", "), if(cutl)", ...",
	" (df=",format(attr(object,"df")),")\n",sep="")
}

## rather silly (but potentially used in pkg nlme):
as.data.frame.logLik <- function (x, ...)
    as.data.frame(c(x), ...)

## >> logLik.nls() in ../../nls/R/nls.R

## from package:nlme

## log-likelihood for glm objects
logLik.glm <- function(object, ...)
{
    if(length(list(...)))
        warning("extra arguments discarded")
    fam <- family(object)$family
    p <- object$rank
    ## allow for estimated dispersion
    if(fam %in% c("gaussian", "Gamma", "inverse.gaussian")) p <- p + 1
    val <- p - object$aic / 2
    attr(val, "df") <- p
    class(val) <- "logLik"
    val
}

## log-likelihood for lm objects
logLik.lm <- function(object, REML = FALSE, ...)
{
    res <- object$residuals # not resid(object) because of NA methods
    p <- object$rank
    N <- length(res)
    if(is.null(w <- object$weights)) {
        w <- rep.int(1, N)
    } else {
        ## this is OK as both resids and weights are for the cases used
        excl <- w == 0			# eliminating zero weights
        if (any(excl)) {
            res <- res[!excl]
            N <- length(res)
            w <- w[!excl]
        }
    }
    N0 <- N
    if(REML) N <- N - p
    val <- .5* (sum(log(w)) - N * (log(2 * pi) + 1 - log(N) +
                                   log(sum(w*res^2))))
    if(REML) val <- val - sum(log(abs(diag(object$qr$qr)[1L:p])))
    attr(val, "nall") <- N0
    attr(val, "nobs") <- N
    attr(val, "df") <- p + 1
    class(val) <- "logLik"
    val
}

