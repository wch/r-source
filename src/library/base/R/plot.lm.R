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
    if(!is.numeric(which) || any(which < 1) || any(which > 4))
        stop("`which' must be in 1:4")
    show[which] <- TRUE
    r <- residuals(x)
    n <- length(r)
    yh <- predict(x) # != fitted() for glm
    if (any(show[2:3])) {
        ylab23 <- if(inherits(x, "glm"))
          "Std. deviance resid." else "Standardized residuals"
        hii <- lm.influence(x)$hat
        s <- sqrt(deviance(x)/df.residual(x))
        w <- weights(x)
        # r.w := weighted.residuals(x):
        r.w <- if(is.null(w)) .Alias(r) else (sqrt(w)*r)[w!=0]
        rs <- r.w/(s * sqrt(1 - hii))
    }
    if (any(show[c(1,3)])) {
        l.fit <- paste(if(inherits(x,"glm")) "Predict" else "Fit","ed values",
                       sep="")
    }
    if (is.null(id.n))
	id.n <- 0
    else {
	id.n <- as.integer(id.n)
	if(id.n < 0 || id.n > n)
	    stop(paste("`id.n' must be in { 1,..,",n,"}"))
    }
    if(id.n > 0) {
        if(is.null(labels.id))
            labels.id <- paste(1:n)
        iid <- 1:id.n
	show.r <- order(-abs(r))[iid]
        if(any(show[2:3]))
            show.rs <- order(-abs(rs))[iid]
        text.id <- function(x,y, ind, adj.x = FALSE)
            text(x - if(adj.x) strwidth(" ")*cex.id else 0, y, labels.id[ind],
                 cex = cex.id, xpd = TRUE, adj = if(adj.x) 1)
    }
    one.fig <- prod(par("mfcol")) == 1
    if (ask) {
	op <- par(ask = TRUE)
	on.exit(par(op))
    }
    ##---------- Do the individual plots : ----------
    if (show[1]) {
	ylim <- range(r)
	if(id.n > 0)
	    ylim <- ylim + c(-1,1)* 0.08 * diff(ylim)
	plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
	     ylim = ylim, type = "n", ...)
	panel(yh, r, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[1], 3, 0.25)
	if(id.n > 0) {
	    y.id <- r[show.r]
	    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
	    text.id(yh[show.r], y.id, show.r, adj.x = TRUE)
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
	if(id.n > 0)
	    text.id(qq$x[show.rs], qq$y[show.rs], show.rs, adj.x = TRUE)
    }
    if (show[3]) {
	sqrtabsr <- sqrt(abs(rs))
	ylim <- c(0, max(sqrtabsr))
	yl <- as.expression(substitute(sqrt(abs(YL)), list(YL=as.name(ylab23))))
        yhn0 <- if(is.null(w)) .Alias(yh) else yh[w!=0]
	plot(yhn0, sqrtabsr, xlab = l.fit, ylab = yl, main = main,
	     ylim = ylim, type = "n", ...)
	panel(yhn0, sqrtabsr, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[3], 3, 0.25)
	if(id.n > 0)
	    text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs, adj.x = TRUE)
    }
    if (show[4]) {
	cook <- cooks.distance(x)
	if(id.n > 0) {
	    show.r <- order(-cook)[iid]# index of largest `id.n' ones
	    ymx <- cook[show.r[1]] * 1.075
	} else ymx <- max(cook)
	plot(cook, type = "h", ylim = c(0, ymx), main = main,
	     xlab = "Obs. number", ylab = "Cook's distance", ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[4], 3, 0.25)
	if(id.n > 0)
	    text.id(show.r, cook[show.r] + 0.4*cex.id * strheight(" "), show.r)
    }
    if (!one.fig && par("oma")[3] >= 1)
	mtext(sub.caption, outer = TRUE, cex = 1.25)
    invisible()
}
