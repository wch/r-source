image <- function (x = seq(0, 1, len = nrow(z)),
		   y = seq(0, 1, len = ncol(z)),
		   z,
		   zlim = range(z[is.finite(z)]),
		   xlim = range(x[is.finite(x)]),
		   ylim = range(y[is.finite(y)]),
		   col = heat.colors(12), add = FALSE,
		   xaxs = "i", yaxs = "i", xlab, ylab, ...)
{
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		if(is.null(dim(x)))
		   stop("argument must be matrix alike")
		z <- x
		x <- seq(0, 1, len = nrow(z))
	    }
	    if (missing(xlab)) xlab <- ""
	    if (missing(ylab)) ylab <- ""
	} else stop("no `z' matrix specified")
    } else if (is.list(x)) {
	xn <- deparse(substitute(x))
	if (missing(xlab)) xlab <- paste(xn, "x", sep = "$")
	if (missing(ylab)) ylab <- paste(xn, "y", sep = "$")
	y <- x$y
	x <- x$x
    } else {
	if (missing(xlab))
	    xlab <- if (missing(x)) "" else deparse(substitute(x))
	if (missing(ylab))
	    ylab <- if (missing(y)) "" else deparse(substitute(y))
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing x and y values expected")
    if (!is.matrix(z))
        stop("`z' must be a matrix")
    if(length(x) > 1 && length(x) == nrow(z)) { # midpoints
        dx <- 0.5*diff(x)
        x <- c(x[1] - dx[1], x[1]+dx[1], x[-1]+dx)
    }
    if(length(y) > 1 && length(y) == ncol(z)) { # midpoints
        dy <- 0.5*diff(y)
        y <- c(y[1] - dy[1], y[1]+dy[1], y[-1]+dy)
    }
    if (!add)
	plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xaxs = xaxs,
	     yaxs = yaxs, xlab = xlab, ylab = ylab, ...)
    if(length(x) == 1) x <- par("usr")[1:2]
    if(length(y) == 1) y <- par("usr")[3:4]
    if(length(x) != nrow(z)+1 || length(y) != ncol(z)+1)
        stop("dimensions of z are not length(x)(+1) times length(y)(+1)")
    .Internal(image(as.double(x), as.double(y), as.double(z),
		    as.double(zlim), col))
}
