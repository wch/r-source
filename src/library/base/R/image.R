image <- 
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
            z, zlim = range(z, finite = TRUE), xlim = range(x, finite = TRUE), 
            ylim = range(y, finite = TRUE), col = heat.colors(12), add = FALSE, 
            xaxs = "i", yaxs = "i", xlab, ylab, ...) 
{
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z; y <- x$y; x <- x$x
      } else {
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
  if (!add) 
    plot(0, 0, xlim = xlim, ylim = ylim, type = "n", xaxs = xaxs, 
         yaxs = yaxs, xlab = xlab, ylab = ylab, ...)
  .Internal(image(as.double(x), as.double(y), as.double(z), 
                  as.double(zlim), col))
}
