biplot <- function(x, ...) UseMethod("biplot")

biplot.default <-
    function(x, y, var.axes = TRUE, col, cex = rep(par("cex"), 2),
	     xlabs = NULL, ylabs = NULL, expand=1, xlim = NULL, ylim = NULL,
	     arrow.len = 0.1,
             main = NULL, sub = NULL, xlab = NULL, ylab = NULL, ...)
{
    n <- nrow(x)
    p <- nrow(y)
    if(missing(xlabs)) {
	xlabs <- dimnames(x)[[1]]
	if(is.null(xlabs)) xlabs <- 1:n
    }
    xlabs <- as.character(xlabs)
    dimnames(x) <- list(xlabs, dimnames(x)[[2]])
    if(missing(ylabs)) {
	ylabs <- dimnames(y)[[1]]
	if(is.null(ylabs)) ylabs <- paste("Var", 1:p)
    }
    ylabs <- as.character(ylabs)
    dimnames(y) <- list(ylabs, dimnames(y)[[2]])

    if(length(cex) == 1) cex <- c(cex, cex)
    if(missing(col)) {
	col <- par("col")
	if (!is.numeric(col)) col <- match(col, palette(), nomatch=1)
	col <- c(col, col + 1)
    }
    else if(length(col) == 1) col <- c(col, col)

    unsigned.range <- function(x) c(-abs(min(x)), abs(max(x)))
    rangx1 <- unsigned.range(x[, 1])
    rangx2 <- unsigned.range(x[, 2])
    rangy1 <- unsigned.range(y[, 1])
    rangy2 <- unsigned.range(y[, 2])

    if(missing(xlim) && missing(ylim))
	xlim <- ylim <- rangx1 <- rangx2 <- range(rangx1, rangx2)
    else if(missing(xlim)) xlim <- rangx1
    else if(missing(ylim)) ylim <- rangx2
    ratio <- max(rangy1/rangx1, rangy2/rangx2)/expand
    on.exit(par(op))
    op <- par(pty = "s")
    if(!is.null(main))
        op <- c(op, par(mar = par("mar")+c(0,0,1,0)))
    plot(x, type = "n", xlim = xlim, ylim = ylim, col = col[1],
         xlab = xlab, ylab = ylab, sub = sub, main = main, ...)
    text(x, xlabs, cex = cex[1], col = col[1], ...)
    par(new = TRUE)
    plot(y, axes = FALSE, type = "n", xlim = xlim*ratio, ylim = ylim*ratio,
	 xlab = "", ylab = "", col = col[1], ...)
    axis(3, col = col[2])
    axis(4, col = col[2])
    box(col = col[1])
    text(y, labels=ylabs, cex = cex[2], col = col[2], ...)
    if(var.axes)
	arrows(0, 0, y[,1] * 0.8, y[,2] * 0.8, col = col[2], length=arrow.len)
    invisible()
}

biplot.princomp <- function(x, choices = 1:2, scale = 1, pc.biplot=FALSE, ...)
{
    if(length(choices) != 2) stop("length of choices must be 2")
    if(!length(scores <- x$scores))
	stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
             domain = NA)
    lam <- x$sdev[choices]
    if(is.null(n <- x$n.obs)) n <- 1
    lam <- lam * sqrt(n)
    if(scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
    if(scale != 0) lam <- lam^scale else lam <- 1
    if(pc.biplot) lam <- lam / sqrt(n)
    biplot.default(t(t(scores[, choices]) / lam),
		   t(t(x$loadings[, choices]) * lam), ...)
    invisible()
}

biplot.prcomp <- function(x, choices = 1:2, scale = 1, pc.biplot=FALSE, ...)
{
    if(length(choices) != 2) stop("length of choices must be 2")
    if(!length(scores <- x$x))
	stop(gettextf("object '%s' has no scores", deparse(substitute(x))),
             domain = NA)
    if(is.complex(scores))
        stop("biplots are not defined for complex PCA")
    lam <- x$sdev[choices]
    n <- NROW(scores)
    lam <- lam * sqrt(n)
    if(scale < 0 || scale > 1) warning("'scale' is outside [0, 1]")
    if(scale != 0) lam <- lam^scale else lam <- 1
    if(pc.biplot) lam <- lam / sqrt(n)
    biplot.default(t(t(scores[, choices]) / lam),
		   t(t(x$rotation[, choices]) * lam), ...)
    invisible()
}
