combn <- function(x, m, FUN = NULL, simplify = TRUE, ...)
{
    ## DATE WRITTEN: 14 April 1994	    LAST REVISED:  10 July 1995
    ## AUTHOR:	Scott Chasalow
    ##
    ## DESCRIPTION:
    ##	Generate all combinations of the elements of x taken m at a time.
    ##	If x is a positive integer,  returns all combinations
    ##	of the elements of seq(x) taken m at a time.
    ##	If argument "FUN" is not null,	applies a function given
    ##	by the argument to each point.	If simplify is FALSE,  returns
    ##	a list; else returns a vector or an array.  "..." are passed
    ##	unchanged to function given by argument FUN,  if any.

    ##S : Change if (simplify = TRUE) return an array/matrix {not a 'vector'}
    stopifnot(length(m) == 1)
    if(m < 0)
	stop("m < 0")
    if(m == 0)
	return(if(simplify) vector(mode(x), 0) else list())
    if(is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)
	x <- seq.int(x)
    n <- length(x)
    if(n < m)
	stop("n < m")
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- 1:m
    nofun <- is.null(FUN)
    if(!nofun && !is.function(FUN))
	stop("'FUN' must be a function or NULL")
    # first result : what kind, what length,.. ?
    len.r <- length(r <- if(nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m))) # >= 1
    if(simplify) {
	dim.use <-
	    if(nofun)
		c(m, count) # matrix also when count = 1
	    else {
		d <- dim(r)
		if(length(d) > 1)
		    c(d, count)
		else if(len.r > 1)
		    c(len.r, count)
		else # MM: *still* a matrix - a la "drop = FALSE"
		    c(d, count)
	    } ## NULL in all 'else' cases
##S	use.arr <- !is.null(dim.use)
    }
##S	else use.arr <- FALSE

    if(simplify) { # use atomic vector/array instead of list
##S	if(use.arr)
	    out <- matrix(r, nrow= len.r, ncol= count) # matrix for now
##S	else {
##S	    if(count > 1) {
##S		out <- vector(storage.mode(r), len.r * count)
##S		out[1] <- r
##S	    }
##S	    else out <- r
##S	}
    }
    else {
	out <- vector("list", count)
	out[[1]] <- r
    }

    i <- 2L
    nmmp1 <- n - m + 1L # using 1L to keep integer arithmetic
    while(a[1] != nmmp1) {
	if(e < n - h) {
	    h <- 1L
	    e <- a[m]
	    j <- 1L
	}
	else {
	    e <- a[m - h]
	    h <- h + 1L
	    j <- 1:h
	}
	a[m - h + j] <- e + j
	r <- if(nofun) x[a] else FUN(x[a], ...)
	if(simplify) ##S if(use.arr)
	    out[,i] <- r else out[[i]] <- r
	i <- i + 1L
    }
    if(simplify) ##S if(use.arr)
	array(out, dim.use) else out
}


