rle <- function(x) {
    if (!is.vector(x))
        stop("x must be a vector")
    n <- length(x)
    if (n == 0)
        return(list(lengths = numeric(0), values = x))
    y <- x[-1] != x[-n]
    i <- c(which(y | is.na(y)), n)
    list(lengths = diff(c(0, i)), values = x[i])
}
