lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
    if (!is.list(X)) X <- as.list(X)
    rval <-.Internal(lapply(length(X), function(i) FUN(X[[i]], ...)))
    names(rval) <- names(X)
    return(rval)
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
