plot.lm <-
function(x, which = 1:4,
         caption = c("Residuals vs Fitted", "Normal Q-Q plot",
         "Scale-Location plot", "Cook's distance plot"),
         panel = points,
         sub.caption = deparse(x$call), main = "",
         ask = prod(par("mfcol")) < length(which) && dev.interactive(),
         ...,
         id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75)
{
    if (!inherits(x, "lm"))
	stop("use only with \"lm\" objects")
    if(!is.numeric(which) || any(which < 1) || any(which > 4))
        stop("'which' must be in 1:4")
    isGlm <- inherits(x, "glm")
    show <- rep(FALSE, 4)
    show[which] <- TRUE
    r <- residuals(x)
    yh <- predict(x) # != fitted() for glm
    w <- weights(x)
    if(!is.null(w)) { # drop obs with zero wt: PR#6640
        wind <- w != 0
        r <- r[wind]
        yh <- yh[wind]
        w <- w[wind]
        labels.id <- labels.id[wind]
    }
    n <- length(r)
    if (any(show[2:4])) {
        s <- if(inherits(x, "rlm")) x$s else sqrt(deviance(x)/df.residual(x))
        hii <- lm.influence(x, do.coef=FALSE)$hat
    }
    if (any(show[2:3])) {
        ylab23 <- if(isGlm) "Std. deviance resid." else "Standardized residuals"
        r.w <- if(is.null(w)) r else sqrt(w)*r
        rs <- r.w/(s * sqrt(1 - hii))
    }
    if (any(show[c(1,3)]))
        l.fit <- if(isGlm) "Predicted values" else "Fitted values"
    if (is.null(id.n))
	id.n <- 0
    else {
	id.n <- as.integer(id.n)
	if(id.n < 0 || id.n > n)
	    stop(gettextf("'id.n' must be in {1,..,%d}", n), domain = NA)
    }
    if(id.n > 0) { ## label the largest residuals
        if(is.null(labels.id))
            labels.id <- paste(1:n)
        iid <- 1:id.n
	show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
        if(any(show[2:3]))
            show.rs <- sort.list(abs(rs), decreasing = TRUE)[iid]
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
	ylim <- range(r, na.rm=TRUE)
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
    if (show[2]) { ## Normal
	ylim <- range(rs, na.rm=TRUE)
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
	ylim <- c(0, max(sqrtabsr, na.rm=TRUE))
	yl <- as.expression(substitute(sqrt(abs(YL)), list(YL=as.name(ylab23))))
        yhn0 <- if(is.null(w)) yh else yh[w!=0]
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
	cook <-
            if(isGlm) cooks.distance(x)
            else cooks.distance(x, sd=s, res = r)
	if(id.n > 0) {
	    show.r <- order(-cook)[iid]# index of largest 'id.n' ones
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
