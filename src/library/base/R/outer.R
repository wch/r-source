outer <- function (X, Y, FUN = "*", ...)
{
    no.nx <- is.null(nx <- dimnames(X <- as.array(X))); dX <- dim(X)
    no.ny <- is.null(ny <- dimnames(Y <- as.array(Y))); dY <- dim(Y)
    if (is.character(FUN) && FUN=="*") {
        robj <- as.vector(X) %*% t(as.vector(Y))
        dim(robj) <- c(dX, dY)
    } else {
        FUN <- match.fun(FUN)
        Y <- rep.int(Y, rep.int(length(X), length(Y)))
        X <- rep(X, length.out = length(Y))
        robj <- array(FUN(X, Y, ...), c(dX, dY))
    }
    ## no dimnames if both don't have ..
    if(no.nx) nx <- vector("list", length(dX)) else
    if(no.ny) ny <- vector("list", length(dY))
    if(!(no.nx && no.ny))
	dimnames(robj) <- c(nx, ny)
    robj
}

## Binary operator, hence don't simply do "%o%" <- outer.
"%o%" <- function(X, Y) outer(X, Y)
