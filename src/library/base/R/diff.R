diff <- function(x, ...) UseMethod("diff")

diff.default <- function(x, lag = 1, differences = 1, ...)
{
    ## the following clause will be replaced by autoload("diff.ts","ts")
    if(is.ts(x)) {
        require(ts)
        UseMethod("diff")
    }
    ismat <- is.matrix(x)
    if (ismat)
	xlen <- dim(x)[1]
    else xlen <- length(x)
    if (lag < 1 | differences < 1)
	stop("Bad value for lag or differences")
    if (lag * differences >= xlen)
	return(x[0])
    r <- x
    class(r) <- NULL # don't want class-specific subset methods
    s <- 1:lag
    if (is.matrix(r)) {
	for (i in 1:differences)
	    r <- r[-s, , drop = FALSE] - r[-(nrow(r) + 1 - s), , drop = FALSE]
    }
    else for (i in 1:differences)
	r <- r[-s] - r[-(length(r) + 1 - s)]
    xtsp <- attr(x, "tsp")
    if (!is.null(xtsp))
        tsp(r) <- c(xtsp[1] + lag*differences*xtsp[3], xtsp[2], xtsp[3])
    class(r) <- class(x)
    r
}
