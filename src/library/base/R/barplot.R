barplot <- function(height, ...) UseMethod("barplot")

barplot.default <-
    function(height, width = 1, space = NULL, names.arg = NULL,
	     legend.text = NULL, beside = FALSE, horiz = FALSE,
	     col = heat.colors(NR), border = par("fg"),
	     main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
	     xlim = NULL, ylim = NULL,
	     axes = TRUE, axisnames = TRUE, inside = TRUE, plot = TRUE, ...)
{
    if (!missing(inside)) .NotYetUsed("inside")
    if (!missing(border)) .NotYetUsed("border")

    if (missing(space))
	space <- if (is.matrix(height) && beside) c(0, 1) else 0.2
    space <- space * mean(width)

    if (plot && axisnames && missing(names.arg))
	names.arg <-
	    if(is.matrix(height)) colnames(height) else names(height)

    if (is.vector(height)) {
	height <- cbind(height)
	beside <- TRUE
    } else if (is.array(height) && (length(dim(height)) == 1)) {
	height <- rbind(height)
	beside <- TRUE
    } else if (!is.matrix(height))
	stop("`height' must be a vector or a matrix")

    NR <- nrow(height)
    NC <- ncol(height)

    if (beside) {
	if (length(space) == 2)
	    space <- rep(c(space[2], rep(space[1], NR - 1)), NC)
	width <- rep(width, length = NR * NC)
    } else {
	width <- rep(width, length = NC)
	height <- rbind(0, apply(height, 2, cumsum))
    }
    delta <- width / 2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
	if (missing(xlim)) xlim <- range(-0.01 * height, height)
	if (missing(ylim)) ylim <- c(min(w.l), max(w.r))
    } else {
	if (missing(xlim)) xlim <- c(min(w.l), max(w.r))
	if (missing(ylim)) ylim <- range(-0.01 * height, height)
    }
    if (beside)
	w.m <- matrix(w.m, nc = NC)
    if(plot) { ##-------- Plotting :
	opar <-
	    if (horiz)	par(xaxs = "i", xpd = TRUE)
	    else	par(yaxs = "i", xpd = TRUE)
	on.exit(par(opar))

	plot.new()
	plot.window(xlim, ylim, log = "")
	xyrect <- function(x1,y1, x2,y2, horizontal=TRUE, ...) {
	    if(horizontal)
		rect(x1,y1, x2,y2, ...)
	    else
		rect(y1,x1, y2,x2, ...)
	}
	if (beside)
	    xyrect(0, w.l, c(height), w.r, horizontal=horiz, col = col)
	else {
	    for (i in 1:NC) {
		xyrect(height[1:NR, i], w.l[i], height[-1, i], w.r[i],
		       horizontal=horiz, col = col)
	    }
	}
	if (axisnames && !is.null(names.arg)) { # specified or from {col}names
	    at.l <- if (length(names.arg) != length(w.m)) {
		if (length(names.arg) == NC) # i.e. beside (!)
		    apply(w.m, 2, mean)
		else
		    stop("incorrect number of names")
	    } else w.m
	    axis(if(horiz) 2 else 1, at = at.l, labels = names.arg, lty = 0)
	}
	if (!is.null(legend.text)) {
	    legend.col <- rep(col,length=length(legend.text))
	    if((horiz & beside) || (!horiz & !beside)){
		legend.text <- rev(legend.text)
		legend.col <- rev(legend.col)
	    }
	    xy <- par("usr")
	    legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1),
		   legend = legend.text, fill = legend.col,
		   xjust = 1, yjust = 1)
	}
	title(main = main, sub = sub, xlab = xlab, ylab = ylab, ...)
	if (axes) axis(if(horiz) 1 else 2)
	invisible(w.m)
    } else w.m
}
