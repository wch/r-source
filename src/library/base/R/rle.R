rle <- function(x) {
    if (!is.vector(x))
        stop("x must be a vector")
    n <- length(x)
    if (n == 0)
        return(list(lengths = numeric(0), values = x))
    i <- c(which(x[-1] != x[-length(x)]), n)
    list(lengths = diff(c(0, i)), values = x[i])
}
