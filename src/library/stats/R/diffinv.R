# Copyright (C) 1997-1999  Adrian Trapletti
#

diffinv <- function (x, ...) { UseMethod("diffinv") }

## the workhorse of diffinv.default:
diffinv.vector <- function (x, lag = 1, differences = 1, xi, ...)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    if (lag < 1 || differences < 1) stop ("bad value for 'lag' or 'differences'")
    if(missing(xi)) xi <- rep(0., lag*differences)
    if (length(xi) != lag*differences) stop ("'xi' has not the right length")
    if (differences == 1) {
        x <- as.double(x)
        xi <- as.double(xi)
        n <- length(x)
        y <- c(xi[1:lag], double(n))
        .C("R_intgrt_vec",
           x, y=y, as.integer(lag), n, PACKAGE="stats")$y
    }
    else
        diffinv.vector(diffinv.vector(x, lag, differences-1,
                                      diff(xi, lag=lag, differences=1)),
                       lag, 1, xi[1:lag])
}

diffinv.default <- function (x, lag = 1, differences = 1, xi, ...)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        y <- matrix(0, nrow = n+lag*differences, ncol = m)
        if(m >= 1) {
            if(missing(xi)) xi <- matrix(0.0, lag*differences, m)
            if(NROW(xi) != lag*differences || NCOL(xi) != m)
                stop("incorrect dimensions for 'xi'")
            for (i in 1:m)
                y[,i] <- diffinv.vector(as.vector(x[,i]), lag, differences,
                                        as.vector(xi[,i]))
        }
    }
    else if (is.vector(x))
        y <- diffinv.vector(x, lag, differences, xi)
    else
        stop ("'x' is not a vector or matrix")
    y
}

diffinv.ts <- function (x, lag = 1, differences = 1, xi, ...)
{
    y <- diffinv.default(if(is.ts(x) && is.null(dim(x))) as.vector(x) else
                         as.matrix(x), lag, differences, xi)
    ts(y, frequency = frequency(x), end = end(x))
}

toeplitz <- function (x)
{
    if (!is.vector(x)) stop ("'x' is not a vector")
    n <- length (x)
    A <- matrix (0, n, n)
    matrix (x[abs(col(A) - row(A)) + 1], n, n)
}





