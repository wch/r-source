"dotplot" <-
    function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"),
	      pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
	      gcolor = par("fg"), lcolor = "gray", main = NULL,
	      xlab = NULL, ylab = NULL, ...)
{
    opar <- par("mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")

    n <- length(x)
    if (is.matrix(x)) {
	if (is.null(labels))
	    labels <- rownames(x)
	if (is.null(labels))
	    labels <- as.character(1:nrow(x))
	labels <- rep(labels, length = n)
	if (is.null(groups))
	    groups <- col(x, as.factor = TRUE)
	glabels <- levels(groups)
    }
    else {
	if (is.null(labels))
	    labels <- names(x)
	if (!is.null(groups))
	    glabels <- levels(groups)
	else glabels <- NULL
    }

    plot.new()
    linch <- 0
    ginch <- 0
    if (!is.null(labels))
	linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
    goffset <- 0
    if (!is.null(glabels)) {
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
	goffset <- 0.4
    }

    lheight <- strheight("M", "inch")
    if (!(is.null(labels) && is.null(glabels))) {
	nmar <- mar <- par("mar")
	nmar[2] <- nmar[4] + (max(linch + goffset, ginch) +
			      0.1)/lheight
	par(mar = nmar)
    }

    if (is.null(groups)) {
	o <- 1:n
	y <- o
	ylim <- c(0, n + 1)
    }
    else {
	o <- rev(order(as.numeric(groups)))
	x <- x[o]
	groups <- groups[o]
	offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
	y <- 1:n + 2 * offset
	ylim <- range(0, y + 2)
    }

    plot.window(xlim = range(x[is.finite(x)]), ylim = ylim, log = "")
    xmin <- par("usr")[1]
    if (!is.null(labels)) {
	linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
	loffset <- (linch + 0.1)/lheight
	labs <- labels[o]
	for(i in 1:n)
	    mtext(labs[i], side=2, line=loffset, at=y[i], adj = 0,
		  col = color, las=2, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg)
    if (!is.null(groups)) {
	gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 2) - 1)
	ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
	goffset <- (max(linch+0.2, ginch, na.rm = TRUE) + 0.1)/lheight
	for(i in 1:nlevels(groups))
	    mtext(glabels[i], side=2, line=goffset, at=gpos[i],
		  adj = 0, col = gcolor, las=2, ...)
	if (!is.null(gdata)) {
	    abline(h = gpos, lty = "dotted")
	    points(gdata, gpos, pch = gpch, col = gcolor,
		   bg = bg, ...)
	}
    }
    axis(1)
    box()
    title(main=main, xlab=xlab, ylab=ylab, ...)
    invisible()
}
