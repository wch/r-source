dotplot <-
function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
	pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), 
	gcolor = par("fg"), lcolor = "gray", ...) 
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
	linch <- 0
	ginch <- 0
	goffset <- 0
	if (!is.null(labels)) 
		linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
	if (!is.null(glabels)) {
		ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
		goffset <- 0.4
	}
	if (!(is.null(labels) && is.null(glabels))) {
		nmai <- par("mai")
		nmai[2] <- nmai[4] + max(linch + goffset, ginch) + 0.1
		par(mai = nmai)
	}
	if (is.null(groups)) {
		o <- 1:n
		y <- o
		ylim <- c(0, n + 1)
	}
	else {
		o <- rev(order(codes(groups)))
		x <- x[o]
		groups <- groups[o]
		offset <- cumsum(c(0, diff(codes(groups)[o])))
		y <- 1:n + 2 * offset
		ylim <- range(0, y + 2)
	}
	plot.new()
	plot.window(xlim = range(x, na.rm = T), ylim = ylim, log = "")
	box()
	xmin <- par("usr")[1]
	if (!is.null(labels)) {
		luser <- max(strwidth(labels, "user"), na.rm = TRUE)
		loffset <- luser + xinch(0.1)
		text(rep(xmin - loffset, n), y, labels[o], 
			xpd = TRUE, adj = 0, col = color, ...)
	}
	abline(h = y, lty = "dotted", col = lcolor)
	points(x, y, pch = pch, col = color, bg = bg)
	if (!is.null(groups)) {
		gpos <- rev(cumsum(tapply(groups, groups, length) + 2) - 1)
		guser <- max(strwidth(glabels, "user"), na.rm = TRUE)
		goffset <- max(luser + xinch(goffset), guser, 
			na.rm = TRUE) + xinch(0.1)
		text(rep(xmin - goffset, nlevels(groups)), gpos, 
			glabels, xpd = TRUE, adj = 0, col = gcolor, ...)
		if (!is.null(gdata)) {
			abline(h = gpos, lty = "dotted")
			points(gdata, gpos, pch = gpch, col = gcolor, 
				bg = bg, ...)
		}
	}
	axis(1)
	invisible()
}
