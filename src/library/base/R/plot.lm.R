plot.lm <- function (x, which = 1:4,
		     caption = c("Residuals vs Fitted", "Normal Q-Q plot",
		     "Scale-Location plot", "Cook's distance plot"),
		     panel = points,
		     sub.caption = deparse(x$call), main = "",
		     ask = interactive() && one.fig && .Device != "postscript",
		     ...,
		     id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75)
{
    if (!inherits(x, "lm"))
	stop("Use only with 'lm' objects")
    show <- rep(FALSE, 4)
    show[which] <- TRUE
    r <- residuals(x)
    n <- length(r)
    yh <- predict(x) # != fitted() for glm
    hii <- lm.influence(x)$hat
    s <- sqrt(deviance(x)/df.residual(x))
    if (any(show[2:3])) {
	ylab23 <- if(inherits(x, "glm"))
	    "Std. dev. residuals" else "Standardized residuals"
	rs <- r/sqrt(1 - hii)/s
    }
    one.fig <- prod(par("mfcol")) == 1
    if (ask) {
	op <- par(ask = TRUE)
	on.exit(par(op))
    }
    if (is.null(id.n))
	id.n <- 0
    else {
	id.n <- as.integer(id.n)
	if(id.n < 0 || id.n > n)
	    stop(paste("`id.n' must be in { 1,..,",n,"}"))
    }
    if(id.n > 0)
	show.id <- order(-abs(r))[1:id.n]

    if (is.null(labels.id))
	labels.id <- paste(1:n)
    if (show[1]) {
	ylim <- range(r)
	if(id.n > 0)
	    ylim <- ylim + c(-1,1)* 0.08 * diff(ylim)
	plot(yh, r, xlab = "Fitted values", ylab = "Residuals", main = main,
	     ylim = ylim, type = "n", ...)
	panel(yh, r, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[1], 3, 0.25)
	if(id.n > 0) {
	    chh <- strheight(" ")
	    chw <- strwidth(" ")
	    y.id <- r[show.id]
	    y.id[y.id < 0] <- y.id[y.id < 0] - chh/3
	    text(yh[show.id] - cex.id * chw, y.id, labels.id[show.id],
		 cex = cex.id, xpd = TRUE, adj = 1)
	}
	abline(h = 0, lty = 3, col = "gray")
    }
    if (show[2]) {
	ylim <- range(rs)
	ylim[2] <- ylim[2] + diff(ylim) * 0.075
	qq <- qqnorm(rs, main = main, ylab = ylab23, ylim = ylim, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[2], 3, 0.25)
	if(id.n > 0) {
	    chw <- strwidth(" ")
	    text(qq$x[show.id] - cex.id * chw,
		 qq$y[show.id],
		 labels.id[show.id], cex = cex.id, adj = 1, xpd = TRUE)
	}
    }
    if (show[3]) {
	sqrtabsr <- sqrt(abs(rs))
	ylim <- c(0, max(sqrtabsr))
	yl <- as.expression(substitute(sqrt(abs(YL)), list(YL=as.name(ylab23))))
	plot(yh, sqrtabsr, xlab = "Fitted values", ylab = yl, main = main,
	     ylim = ylim, type = "n", ...)
	panel(yh, sqrtabsr, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[3], 3, 0.25)
    }
    if (show[4]) {
	cook <- cooks.distance(x)
	if(id.n > 0) {
	    show.id <- order(-cook)[1:id.n]# index of largest `id.n' ones
	    ymx <- cook[show.id[1]] * 1.075
	} else ymx <- max(cook)
	plot(cook, type = "h", ylim = c(0, ymx), main = main,
	     xlab = "Obs. number", ylab = "Cook's distance", ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[4], 3, 0.25)
	if(id.n > 0) {
	    chh <- strheight(" ")
	    text(show.id, cook[show.id] + 0.4 * cex.id * chh,
		 labels.id[show.id], cex = cex.id, xpd = TRUE)
	}
    }
    if (!one.fig && par("oma")[3] >= 1)
	mtext(sub.caption, outer = TRUE, cex = 1.25)
    invisible()
}
