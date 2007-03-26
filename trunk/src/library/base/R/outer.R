outer <- function (X, Y, FUN = "*", ...)
{
    # no.nx <- is.null(nx <- dimnames(X <- as.array(X))); dX <- dim(X)
    # no.ny <- is.null(ny <- dimnames(Y <- as.array(Y))); dY <- dim(Y)

    if(is.array(X)) {
        dX <- dim(X)
        nx <- dimnames(X)
        no.nx <- is.null(nx)
    } else { # a vector
        dX <- length(X)
        no.nx <- is.null(names(X))
        if(!no.nx) nx <- list(names(X))
    }
    if(is.array(Y)) {
        dY <- dim(Y)
        ny <- dimnames(Y)
        no.ny <- is.null(ny)
    } else { # a vector
        dY <- length(Y)
        no.ny <- is.null(names(Y))
        if(!no.ny) ny <- list(names(Y))
    }
    if (is.character(FUN) && FUN=="*") {
        # this is for numeric vectors, so dropping attributes is OK
        robj <- as.vector(X) %*% t(as.vector(Y))
        dim(robj) <- c(dX, dY)
    } else {
        FUN <- match.fun(FUN)
        ## Y may have a class, so don't use rep.int
        Y <- rep(Y, rep.int(length(X), length(Y)))
        ##  length.out is not an argument of the generic rep()
        ##  X <- rep(X, length.out = length(Y))
        if(length(X) > 0)
            X <- rep(X, times = ceiling(length(Y)/length(X)))
        robj <- FUN(X, Y, ...)
        dim(robj) <- c(dX, dY) # careful not to lose class here
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
