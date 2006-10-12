diff <- function(x, ...) UseMethod("diff")

diff.default <- function(x, lag = 1, differences = 1, ...)
{
    ismat <- is.matrix(x)
    xlen <- if(ismat) dim(x)[1] else length(x)
    if (length(lag) > 1 || length(differences) > 1 ||
        lag < 1 || differences < 1)
	stop("'lag' and 'differences' must be integers >= 1")
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
    class(r) <- oldClass(x)
    r
}
