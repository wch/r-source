lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    ## internal code handles all vector types, including expressions
    ## However, it would be OK to have attributes which is.vector
    ## disallows.
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    .Internal(lapply(X, FUN))
}

if(FALSE) {
lapply <- function(X, FUN, ...) {
    FUN <- match.fun(FUN)
    if (!is.list(X))
	X <- as.list(X)
    rval <- vector("list", length(X))
    for(i in seq(along = X))
	rval[i] <- list(FUN(X[[i]], ...))
    names(rval) <- names(X)		  # keep `names' !
    return(rval)
}
}
