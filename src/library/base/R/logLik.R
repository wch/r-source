logLik <- function(object, ...) UseMethod("logLik")

print.logLik <- function(x, ...) print(c(x), ...)

## >> logLik.nls() in ../../nls/R/nls.R

## from package:nlme

## log-likelihood for glm objects
logLik.glm <- function(object, ...)
{
    if(length(list(...)))
        warning("extra arguments discarded")
    p <- object$rank
    val <- p - object$aic / 2
    attr(val, "df") <- p
    class(val) <- "logLik"
    val
}

## log-likelihood for lm objects
logLik.lm <- function(object, REML = FALSE, ...)
{
    res <- resid(object)
    p <- object$rank
    N <- length(res)
    if(is.null(w <- object$weights)) {
        w <- rep(1, N)
    } else {
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

