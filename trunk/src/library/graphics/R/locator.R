locator <- function(n = 512, type="n", ...)
{
    if(length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    z <- .Internal(locator(n, type=type))# n <= 0 gives error
    x <- z[[1]]
    y <- z[[2]]
    if((n <- z[[3]]) > 0) list(x=x[1:n], y=y[1:n])
}
