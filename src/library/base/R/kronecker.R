kronecker <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...)
{
    dX <- dim(X <- as.array(X))
    dY <- dim(Y <- as.array(Y))
    ld <- length(dX) - length(dY)
    ## pad with unit dims if required
    if (ld<0)
        dX <- dim(X) <- c(dX, rep(1, -ld))
    else if(ld > 0)
        dY <- dim(Y) <- c(dY, rep(1, ld))
    opobj <- outer(X, Y, FUN, ...)
    dp <- seq(along = c(dX, dY))
    ld <- length(dX)
    dp <- as.vector(t(matrix(dp, ncol=2)[, 2:1]))# e.g. = 3 1 4 2
    opobj <- aperm(opobj, dp)
    if(make.dimnames)
        dn <- dimnames(opobj)
    dim(opobj) <- dX * dY
    if(make.dimnames) {
        outerPaste <- function(x,y) {
            if((nx <- is.null(x)) && (ny <- is.null(y))) 
                NULL
            else outer(if(nx) "" else x,
                       if(ny) "" else y, FUN="paste", sep=":")
        }
        dimnames(opobj) <-
            lapply(0:1, function(i)do.call("outerPaste", dn[i*ld + (1:ld)]))
    }
    opobj
}

"%x%" <- .Alias(kronecker)
