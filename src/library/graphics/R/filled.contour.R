filled.contour <-
function (x = seq(0, 1, len = nrow(z)),
          y = seq(0, 1, len = ncol(z)),
          z,
          xlim = range(x, finite=TRUE),
          ylim = range(y, finite=TRUE),
          zlim = range(z, finite=TRUE),
          levels = pretty(zlim, nlevels), nlevels = 20,
          color.palette = cm.colors,
          col = color.palette(length(levels) - 1),
          plot.title, plot.axes, key.title, key.axes,
          asp = NA, xaxs="i", yaxs="i", las = 1, axes = TRUE,
          frame.plot = axes, ...)
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0))
        stop("increasing x and y values expected")

    mar.orig <- (par.orig <- par(c("mar","las","mfrow")))$mar
    on.exit(par(par.orig))

    w <- (3 + mar.orig[2]) * par('csi') * 2.54
    layout(matrix(c(2, 1), nc=2), widths=c(1, lcm(w)))
    par(las = las)

    ## Plot the 'plot key' (scale):
    mar <- mar.orig
    mar[4] <- mar[2]
    mar[2] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim=c(0,1), ylim=range(levels), xaxs="i", yaxs="i")
    rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    if (missing(key.axes)) {
        if (axes)
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title))
	key.title

    ## Plot contour-image::
    mar <- mar.orig
    mar[4] <- 1
    par(mar=mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs=xaxs, yaxs=yaxs, asp=asp)

    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1)
        stop("no proper 'z' matrix specified")
    if (!is.double(z))
        storage.mode(z) <- "double"
    .Internal(filledcontour(as.double(x),
                            as.double(y),
                            z,
                            as.double(levels),
                            col = col))
    if (missing(plot.axes)) {
        if (axes) {
            title(main="", xlab="", ylab="")
            axis(1)
            axis(2)
        }
    }
    else plot.axes
    if (frame.plot) box()
    if (missing(plot.title))
        title(...)
    else
	plot.title
    invisible()
}
