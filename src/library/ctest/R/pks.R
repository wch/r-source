pks <- function(x, tol = 10^(-6)) {
    if (is.numeric(x))
        x <- as.vector(x)
    else
        stop("Argument x must be numeric")

    PKS <- rep(0, length(x))
    PKS[is.na(x)] <- NA
    IND <- which(!is.na(x) & (x > 0))
    if (length(IND) > 0) {
        k <- 1 : ceiling(sqrt(-log(tol)/2) / min(x[IND]))
        y <- outer(x[IND]^2, k,
                   function (t, k) { (-1)^k * exp(-2 * t * k^2) })
        PKS[IND] <- 1 + 2 * apply(y, 1, "sum")
    }
    return(PKS)
}
