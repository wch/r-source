image <- function(x, ...) UseMethod("image")

image.default <- function (x = seq(0, 1, len = nrow(z)),
		   y = seq(0, 1, len = ncol(z)),
		   z,
		   zlim = range(z[is.finite(z)]),
		   xlim = range(x[is.finite(x)]),
		   ylim = range(y[is.finite(y)]),
		   col = heat.colors(12), add = FALSE,
		   xaxs = "i", yaxs = "i", xlab, ylab,
                   breaks, oldstyle=FALSE, ...)
{
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		if(is.null(dim(x)))
		   stop("argument must be matrix-like")
		z <- x
		x <- seq(0, 1, len = nrow(z))
	    }
	    if (missing(xlab)) xlab <- ""
	    if (missing(ylab)) ylab <- ""
	} else stop("no 'z' matrix specified")
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
	stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z))
        stop("'z' must be a matrix")
    if (length(x) > 1 && length(x) == nrow(z)) { # midpoints
        dx <- 0.5*diff(x)
        x <- c(x[1] - dx[1], x[-length(x)]+dx,
               x[length(x)]+dx[length(x)-1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) { # midpoints
        dy <- 0.5*diff(y)
        y <- c(y[1] - dy[1], y[-length(y)]+dy,
               y[length(y)]+dy[length(y)-1])
    }

    if (missing(breaks)) {
        nc <- length(col)
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 0))
            stop("invalid z limits")
        if (diff(zlim) == 0)
            zlim <- if (zlim[1] == 0) c(-1, 1)
                    else zlim[1] + c(-.4, .4)*abs(zlim[1])
        z <- (z - zlim[1])/diff(zlim)
        zi <- if (oldstyle) floor((nc - 1) * z + 0.5)
              else floor((nc - 1e-5) * z + 1e-7)
        zi[zi < 0 | zi >= nc] <- NA
    } else {
	if (length(breaks) != length(col) + 1)
	    stop("must have one more break than colour")
	if (any(!is.finite(breaks)))
	    stop("breaks must all be finite")
	zi <- .C("bincode",
		 as.double(z), length(z), as.double(breaks), length(breaks),
		 code = integer(length(z)), (TRUE), (TRUE), nok = TRUE,
		 NAOK = TRUE, DUP = FALSE, PACKAGE = "base") $code - 1
    }
    if (!add)
	plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xaxs = xaxs,
	     yaxs = yaxs, xlab = xlab, ylab = ylab, ...)
    ## need plot set up before we do this
    if (length(x) == 1) x <- par("usr")[1:2]
    if (length(y) == 1) y <- par("usr")[3:4]
    if (length(x) != nrow(z)+1 || length(y) != ncol(z)+1)
        stop("dimensions of z are not length(x)(+1) times length(y)(+1)")
    .Internal(image(as.double(x), as.double(y), as.integer(zi), col))
}
