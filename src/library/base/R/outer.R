outer <- function (X, Y, FUN = "*", ...) 
{
        FUN <- match.fun(FUN)
        X <- as.array(X)
        if (is.null(nx <- dimnames(X))) 
                nx <- vector("list", length(dim(X)))
        Y <- as.array(Y)
        if (is.null(ny <- dimnames(Y))) 
                ny <- vector("list", length(dim(Y)))
        dims <- c(dim(X), dim(Y))
        Y <- rep(Y, rep(length(X), length(Y)))
        X <- rep(X, length.out = length(Y))
        robj <- array(FUN(X, Y, ...), dims)
        dimnames(robj) <- c(nx, ny)
        robj
}

"%o%" <- .Alias(outer)
