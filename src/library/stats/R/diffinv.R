# Copyright (C) 1997-1999  Adrian Trapletti
#

diffinv <- function (x, ...) { UseMethod("diffinv") }

diffinv.vector <- function (x, lag = 1, differences = 1,
                            xi = rep(0.0,lag*differences), ...)
{
    if (!is.vector(x)) stop ("x is not a vector")
    if (lag < 1 | differences < 1) stop ("Bad value for lag or differences")
    if (length(xi) != lag*differences) stop ("xi has not the right length")
    if (differences == 1) {
        n <- length(x)
        x <- as.vector(x,mode="double")
        y <- as.vector(numeric(n+lag))
        xi <- as.vector(xi,mode="double")
        for (i in 1:lag) y[i] <- xi[i]
        res <- .C("R_intgrt_vec", x, y=y, as.integer(lag), as.integer(n),
                  PACKAGE="stats")
        res$y
    }
    else
        diffinv.vector(diffinv.vector(x, lag, differences-1,
                                      diff(xi, lag=lag, differences=1)),
                       lag, 1, xi[1:lag])
}

diffinv.default <-
    function (x, lag = 1, differences = 1,
              xi = rep(0.0,lag*differences*dim(as.matrix(x))[2]), ...)
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        y <- matrix(0, nr=n+lag*differences, nc=m)
        dim(xi) <- c(lag*differences, m)
        for (i in 1:m)
            y[,i] <- diffinv.vector(as.vector(x[,i]), lag, differences,
                                    as.vector(xi[,i]))
    }
    else if (is.vector(x))
        y <- diffinv.vector(x, lag, differences, xi)
    else
        stop ("x is not a vector or matrix")
    y
}

diffinv.ts <- function (x, lag = 1, differences = 1,
                       xi = rep(0.0, lag*differences*NCOL(x)), ...)
{
    if (is.ts(x) & is.null(dim(x)))
        y <- diffinv.default(as.vector(x), lag, differences, xi)
    else
        y <- diffinv.default(as.matrix(x), lag, differences, xi)
    ts(y, frequency = frequency(x), end = end(x))
}

toeplitz <- function (x)
{
    if (!is.vector(x)) stop ("x is not a vector")
    n <- length (x)
    A <- matrix (0, n, n)
    matrix (x[abs(col(A) - row(A)) + 1], n, n)
}





