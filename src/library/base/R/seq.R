seq <- function(...) UseMethod("seq")

seq.default <-
    function(from = 1, to = 1, by = ((to - from)/(length.out - 1)),
             length.out = NULL, along.with = NULL, ...)
{
    if((One <- nargs() == 1) && !missing(from)) {
	lf <- length(from)
	return(if(mode(from) == "numeric" && lf == 1) 1:from else
	       if(lf) 1:lf else integer(0))
    }
    if(!missing(along.with)) {
	length.out <- length(along.with)
	if(One) return(if(length.out) 1:length.out else integer(0))
    }
    else if(!missing(length.out))
	length.out <- ceiling(length.out)
    if(is.null(length.out))
	if(missing(by))
	    from:to
	else { # dealing with 'by'
	    del <- to - from
	    if(del == 0 && to == 0) return(to)
	    n <- del/by
	    if(!(length(n) && is.finite(n))) {
		if(length(by) && by == 0 && length(del) && del == 0)
		    return(from)
		stop("invalid (to - from)/by in seq(.)")
	    }
	    if(n < 0)
		stop("Wrong sign in 'by' argument")
	    if(n > .Machine$integer.max)
		stop("'by' argument is much too small")

	    dd <- abs(del)/max(abs(to), abs(from))
	    if (dd < 100*.Machine$double.eps) return(from)
	    n <- as.integer(n + 1e-7)
	    from + (0:n) * by
	}
    else if(!is.finite(length.out) || length.out < 0)
	stop("Length must be non-negative number")
    else if(length.out == 0)
	integer(0)
    else if (One) 1:length.out
    else if(missing(by)) {
	if(from == to || length.out < 2)
	    by <- 1
	if(missing(to))
	    to <- from + length.out - 1
	if(missing(from))
	    from <- to - length.out + 1
	if(length.out > 2)
	    if(from == to)
		rep.int(from, length.out)
	    else as.vector(c(from, from + (1:(length.out - 2)) * by, to))
	else as.vector(c(from, to))[1:length.out]
    }
    else if(missing(to))
	from + (0:(length.out - 1)) * by
    else if(missing(from))
	to - ((length.out - 1):0) * by
    else stop("Too many arguments")
}

sequence <- function(nvec)
{
    s <- integer(0)
    for(i in nvec)
	s <- c(s, 1:i)
    return(s)
}
