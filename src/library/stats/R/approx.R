### approx() and approxfun() are *very similar* -- keep in sync!

approx <- function(x, y = NULL, xout, method = "linear", n = 50,
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y) # -> (x,y) numeric of same length
    y <- x$y
    x <- x$x
    nx <- length(x)
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    if(any(na <- is.na(x) | is.na(y))) {
	ok <- !na
	x <- x[ok]
	y <- y[ok]
	nx <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < nx) {
	    if (missing(ties))
		warning("collapsing to unique x values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    nx <- length(x)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (nx <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(nx == 0) stop("zero non-NA points")
    }
    if (missing(yleft))
	yleft <- if (rule == 1) NA else y[1]
    if (missing(yright))
	yright <- if (rule == 1) NA else y[length(y)]
    if (missing(xout)) {
	if (n <= 0)
	    stop("approx requires n >= 1")
	xout <- seq(x[1], x[nx], length = n)
    }
    y <- .C("R_approx", as.double(x), as.double(y), as.integer(nx),
	    xout = as.double(xout), as.integer(length(xout)),
	    as.integer(method), as.double(yleft), as.double(yright),
	    as.double(f), NAOK = TRUE, PACKAGE = "base")$xout
    list(x = xout, y = y)
}

approxfun <- function(x, y = NULL, method = "linear",
		   yleft, yright, rule = 1, f = 0, ties = mean)
{
    x <- xy.coords(x, y)
    y <- x$y
    x <- x$x
    n <- length(x)
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method))
	stop("invalid interpolation method")
    if(any(o <- is.na(x) | is.na(y))) {
	o <- !o
	x <- x[o]
	y <- y[o]
	n <- length(x)
    }
    if (!identical(ties, "ordered")) {
	if (length(ux <- unique(x)) < n) {
	    if (missing(ties))
		warning("collapsing to unique x values")
	    y <- as.vector(tapply(y,x,ties))# as.v: drop dim & dimn.
	    x <- sort(ux)
	    n <- length(x)
	    rm(ux)
	} else {
	    o <- order(x)
	    x <- x[o]
	    y <- y[o]
	}
    }
    if (n <= 1) {
	if(method == 1)# linear
	    stop("need at least two non-NA values to interpolate")
	if(n == 0) stop("zero non-NA points")
    }
    if (missing(yleft))
	yleft <- if(rule == 1) NA else y[1]
    if (missing(yright))
	yright <- if(rule == 1) NA else y[length(y)]
    force(f)
    rm(o, rule, ties)
    function(v) .C("R_approx", as.double(x), as.double(y), as.integer(n),
		   xout = as.double(v), as.integer(length(v)),
		   as.integer(method), as.double(yleft), as.double(yright),
		   as.double(f), NAOK = TRUE, PACKAGE = "base")$xout
}

### This is a `variant' of  approx( method = "constant" ) :
findInterval <- function(x, vec, rightmost.closed = FALSE, all.inside = FALSE)
{
    ## Purpose: gives back the indices of  x in vec;  vec[] sorted
    ## -------------------------------------------------------------------------
    ## Author: Martin Maechler, Date:  4 Jan 2002, 10:16

    if(any(is.na(vec)))
	stop(sQuote("vec")," contains NAs")
    if(is.unsorted(vec))
	stop(sQuote("vec")," must be sorted non-decreasingly")
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
