lag <- function(x, ...) UseMethod("lag")

lag.default <- function(x, k = 1)
{
    if(k != round(k)) {
        k <- round(k)
        warning("k is not an integer")
    }
    x <- hasTsp(x)
    p <- tsp(x)
    tsp(x) <- p - (k/p[3]) * c(1, 1, 0)
    x
}

diff.ts <- function (x, lag = 1, differences = 1)
{
    if (lag < 1 | differences < 1)
        stop("Bad value for lag or differences")
    if (lag * differences >= NROW(x)) return(x[0])
    r <- x
    for (i in 1:differences) {
        r <- r - lag(r, -lag)
    }
    xtsp <- attr(x, "tsp")
    if(is.matrix(x)) colnames(r) <- colnames(x)
    ts(r, end = xtsp[2], freq = xtsp[3])
}
