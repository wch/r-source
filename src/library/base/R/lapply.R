lapply <- function(X, FUN, ...) {
    if (is.character(FUN))
	FUN <- get(FUN, mode = "function")
    if (mode(FUN) != "function")
	stop(paste("\"", FUN, "\" is not a function", sep = " "))
    if (!is.list(X))
	X <- as.list(X)
    rval <- vector("list", length(X))
    for(i in seq(along = X))
	rval[i] <- list(FUN(X[[i]], ...))
    names(rval) <- names(X)		  # keep `names' !
    return(rval)
}
