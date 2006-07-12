lapply <- function (X, FUN, ...)
{
    FUN <- match.fun(FUN)
###    if (!is.list(X)) X <- as.list(X)
###    This failed to coerce a pairlist, which was then repeatedly coerced
###    in the internal code
###    As from 2.4.0 as.list() does not duplicate a list, but
###    internal lapply insists that its arguments are symbols.
###    Should we call as.list on objects, which typically are classed lists?
    if(typeof(X) != "list") X <- as.list(X)
    rval <-.Internal(lapply(X, FUN))
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
