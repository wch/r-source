contour <-
function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
	  z,
	  nlevels = 10, levels = pretty(zlim, nlevels),
	  xlim = range(x, finite = TRUE),
	      ylim = range(y, finite = TRUE),
	  zlim = range(z, finite = TRUE),
	  labcex = 0.4, drawlabels = TRUE, method = "flattest", vfont = NULL,
	  col = par("fg"), lty = par("lty"), lwd = par("lwd"), 
	  add = FALSE, ...)
{
    ## labcex is disregarded since we do NOT yet put  ANY labels...
    if (missing(z)) {
	if (!missing(x)) {
	    if (is.list(x)) {
		z <- x$z; y <- x$y; x <- x$x
	    } else {
		z <- x
		x <- seq(0, 1, len = nrow(z))
	    }
	} else stop("no `z' matrix specified")
    } else if (is.list(x)) {
	y <- x$y
	x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
	stop("increasing x and y values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
	stop("no proper `z' matrix specified")
    if (!add) {
	plot.new()
	plot.window(xlim, ylim, "")
	title(...)
    }
    ##- don't lose  dim(.)
    if (!is.double(z)) storage.mode(z) <- "double"
    method <- pmatch(method[1], c("simple", "edge", "flattest"))
    if (!is.null(vfont)) {
        typeface <- pmatch(vfont[1], c("serif", "sans serif", "script",
		                       "gothic english", "gothic german",
			      	       "gothic italian", "serif symbol",
				       "sans serif symbol"))
        fontindex <- pmatch(vfont[2], c("symbol", "plain", "italic", "bold",
				        "bold italic"))
        vfont <- c(typeface-1, fontindex-1)
    }
    .Internal(contour(as.double(x), as.double(y), z, as.double(levels),
		      labcex, drawlabels, method, vfont, 
		      col = col, lty = lty, lwd = lwd))
    if (!add) {
	axis(1)
	axis(2)
	box()
    }
    invisible()
}
