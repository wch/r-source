lag <- function(x, ...) UseMethod("lag")

lag.default <- function(x, k = 1, ...)
{
    if(k != round(k)) {
        k <- round(k)
        warning("'k' is not an integer")
    }
    x <- hasTsp(x)
    p <- tsp(x)
    tsp(x) <- p - (k/p[3]) * c(1, 1, 0)
    x
}
