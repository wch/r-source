### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop("'vec' contains NAs")
    if(is.unsorted(vec))
	stop("'vec' must be sorted non-decreasingly")
    ## deal with NA's in x:
    if(has.na <- any(ix <- is.na(x)))
	x <- x[!ix]
    nx <- length(x)
    index <- integer(nx)
    .C("find_interv_vec",
       xt = as.double(vec), n = as.integer(length(vec)),
       x  = as.double(x),  nx = as.integer(nx),
       as.logical(rightmost.closed),
       as.logical(all.inside),
       index, DUP = FALSE, NAOK = TRUE, # NAOK: 'Inf' only
       PACKAGE = "base")
    if(has.na) {
	ii <- as.integer(ix)
	ii[ix] <- NA
	ii[!ix] <- index
	ii
    } else index
}
