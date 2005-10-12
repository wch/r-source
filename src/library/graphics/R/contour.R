contour <- function(x, ...) UseMethod("contour")

contour.default <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
	  z,
	  nlevels = 10, levels = pretty(zlim, nlevels), labels = NULL,
	  xlim = range(x, finite = TRUE), ylim = range(y, finite = TRUE),
	  zlim = range(z, finite = TRUE),
	  labcex = 0.6, drawlabels = TRUE, method = "flattest",
          vfont = c("sans serif", "plain"),
          axes = TRUE, frame.plot = axes,
	  col = par("fg"), lty = par("lty"), lwd = par("lwd"),
	  add = FALSE, ...)
{
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		z <- x
		x <- seq(0, 1, len = nrow(z))
	    }
	} else stop("no 'z' matrix specified")
    } else if (is.list(x)) {
	y <- x$y
	x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
	stop("no proper 'z' matrix specified")
    if (!add) {
        localPlotWindow <-
            function(xlim, ylim, ..., main, sub, xlab, ylab, outer, line)
                plot.window(xlim, ylim, ...)
	plot.new()
	localPlotWindow(xlim, ylim, log = "", ...)
	title(...)
    }
    ##- don't lose  dim(.)
    if (!is.double(z)) storage.mode(z) <- "double"
    method <- pmatch(method[1], c("simple", "edge", "flattest"))
    if (!is.null(vfont))
        vfont <- c(typeface = pmatch(vfont[1], Hershey$typeface) - 1,
                   fontindex= pmatch(vfont[2], Hershey$fontindex))
    if (!is.null(labels))
        labels <- as.character(labels)
    .Internal(contour(as.double(x), as.double(y), z, as.double(levels),
		      labels, labcex, drawlabels, method, vfont,
		      col = col, lty = lty, lwd = lwd))
    if(!add) {
        if(axes) {
            Axis(x, side=1)
            Axis(y, side=2)
        }
        if(frame.plot) box()
    }
    invisible()
}
