barplot <-
function(height, width = 1, space = NULL, names.arg = NULL,
	 legend.text = NULL, beside = FALSE, horiz = FALSE,
	 col = heat.colors(NR), border = par("fg"), main = NULL,
	 xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL,
	 axes = TRUE, ...)
{
 opar <- if (horiz)	par(xaxs = "i", xpd = TRUE)
		else	par(yaxs = "i", xpd = TRUE)
 on.exit(par(opar))

 if (missing(space))
	space <- if (is.matrix(height) && beside) c(0, 1) else 0.2
 space <- space * mean(width)

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
 if (missing(names.arg))
	names.arg <- if(is.matrix(height)) colnames(height) else names(height)
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
	if (missing(xlim)) xlim <- range(-0.01, height)
	if (missing(ylim)) ylim <- c(min(w.l), max(w.r))
 } else {
	if (missing(xlim)) xlim <- c(min(w.l), max(w.r))
	if (missing(ylim)) ylim <- range(-0.01, height)
 }
 ## -------- Plotting :
 plot.new()
 plot.window(xlim, ylim, log = "")
 if (beside) {
	if (horiz)
	  rect(0, w.l, c(height), w.r, col = col)
	else
	  rect(w.l, 0, w.r, c(height), col = col)
 } else {
	for (i in 1:NC) {
	 if (horiz)
	   rect(height[1:NR, i], w.l[i], height[-1, i], w.r[i], col = col)
	 else
	   rect(w.l[i], height[1:NR, i], w.r[i], height[-1, i], col = col)
	}
 }
 if (!is.null(names.arg)) {
    if (length(names.arg) != length(w.m)) {
	if (length(names.arg) == NC)
		w.m <- apply(matrix(w.m, nc = NC), 2, mean)
	else
		stop("incorrect number of names")
    }
    axis(if(horiz) 2 else 1, at = w.m, labels = names.arg, lty = 0)
 }
 if (!missing(legend.text) && !missing(col)) {
	xy <- par("usr")
	legend(xy[2] - xinch(0.1), xy[4] - yinch(0.1),
		legend = rev(legend.text), fill = rev(col),
		xjust = 1, yjust = 1)
 }
 title(main = main, xlab = xlab, ylab = ylab, ...)
 if (axes) axis(if(horiz) 1 else 2)
 invisible(w.m)
}
