diff <- function(x, ...) UseMethod("diff")

if(FALSE) ## when method dispatching will work for autoloaded objects
    autoload("diff.ts", file = "ts")

diff.default <- function(x, lag = 1, differences = 1, ...)
{
    ## the following clause will be replaced by autoload("diff.ts",...)
    if(is.ts(x)) {
        require(ts)
        UseMethod("diff")
    }
    ismat <- is.matrix(x)
    xlen <- if(ismat) dim(x)[1] else length(x)
    if (length(lag) > 1 || length(differences) > 1 ||
        lag < 1 || differences < 1)
	stop("`lag' and `differences' must be integers >= 1")
    if (lag * differences >= xlen)
	return(x[0]) # empty of proper mode
    r <- unclass(x)  # don't want class-specific subset methods
    i1 <- -1:-lag
    if (ismat)
	for (i in 1:differences)
	    r <- r[i1, , drop = FALSE] -
                r[-nrow(r):-(nrow(r)-lag+1), , drop = FALSE]
    else
        for (i in 1:differences)
            r <- r[i1] - r[-length(r):-(length(r)-lag+1)]
    class(r) <- class(x)
    r
}
