logLik <- function(object, ...) UseMethod("logLik")

print.logLik <- function(x, digits = getOption("digits"), ...)
{
    cat("'log Lik.' ",format(c(x), digits=digits),
        " (df=",format(attr(x,"df")),")\n",sep="")
    invisible(x)
}

str.logLik <- function(object, digits = max(2, getOption("digits") - 3), ...)
{
    cl <- oldClass(object)
    cat("Class", if (length(cl) > 1) "es",
        " '", paste(cl, collapse = "', '"), "' : ",
        format(c(object), digits=digits),
        " (df=",format(attr(object,"df")),")\n",sep="")
}

## rather silly (but potentially used in pkg nlme):
as.data.frame.logLik <- function (x, row.names = NULL, optional = FALSE)
    as.data.frame(c(x), row.names=row.names, optional=optional)

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
    if(REML) val <- val - sum(log(abs(diag(object$qr$qr)[1:p])))
    attr(val, "nall") <- N0
    attr(val, "nobs") <- N
    attr(val, "df") <- p + 1
    class(val) <- "logLik"
    val
}

