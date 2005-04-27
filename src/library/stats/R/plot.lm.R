plot.lm <-
function (x, which = c(1:3,5), ## was which = 1:4,
	  caption = c("Residuals vs Fitted", "Normal Q-Q",
	  "Scale-Location", "Cook's distance",
	  "Residuals vs Leverage", "Cook's distance vs Leverage"),
	  panel = points, sub.caption = NULL, main = "",
	  ask = prod(par("mfcol")) < length(which) && dev.interactive(), ...,
	  id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
	  cook.levels = c(0.5, 1.0), label.pos = c(4,2))
{
    if (!inherits(x, "lm"))
	stop("use only with \"lm\" objects")
    if(!is.numeric(which) || any(which < 1) || any(which > 6))
	stop("'which' must be in 1:6")
    isGlm <- inherits(x, "glm")
    show <- rep(FALSE, 6)
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
    if (any(show[2:6])) {
	s <- if (inherits(x, "rlm"))
	    x$s
	else sqrt(deviance(x)/df.residual(x))
	hii <- lm.influence(x, do.coef = FALSE)$hat
	if (any(show[4:6])) {
	    cook <- if (isGlm)cooks.distance(x)
	    else cooks.distance(x, sd = s, res = r)
	}
    }
    if (any(show[c(2:3,5)])) {
	ylab23 <- if (isGlm)
	    "Std. deviance resid."
	else "Standardized residuals"
	r.w <- if (is.null(w))
	    r
	else sqrt(w) * r
	rs <- r.w/(s * sqrt(1 - hii))
    }
    if (any(show[5:6])) {
	hatval <- hatvalues(x)
    }
    if (any(show[c(1, 3)]))
	l.fit <- if (isGlm)
	    "Predicted values"
	else "Fitted values"
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
	text.id <- function(x, y, ind, adj.x = TRUE) {
	    labpos <- if(adj.x) label.pos[1+as.numeric(x > mean(range(x)))] else 3
	    text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
		 pos = labpos, offset = 0.25)
	}
    }

    if(is.null(sub.caption)) { ## construct a default:
	cal <- x$call
	if (!is.na(m.f <- match("formula", names(cal)))) {
	    cal <- cal[c(1, m.f)]
	    names(cal)[2] <- "" # drop	" formula = "
	}
	cc <- deparse(cal, 80) # (80, 75) are ``parameters''
	nc <- nchar(cc[1])
	abbr <- length(cc) > 1 || nc > 75
	sub.caption <-
	    if(abbr) paste(substr(cc[1], 1, min(75,nc)), "...") else cc[1]
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
	    ylim <- ylim + c(-1, 1)* 0.08 * diff(ylim)
	plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
	     ylim = ylim, type = "n", ...)
	panel(yh, r, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	mtext(caption[1], 3, 0.25)
	if(id.n > 0) {
	    y.id <- r[show.r]
	    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
	    text.id(yh[show.r], y.id, show.r)
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
	    text.id(qq$x[show.rs], qq$y[show.rs], show.rs)
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
	    text.id(yhn0[show.rs], sqrtabsr[show.rs], show.rs)
    }
    if (show[4]) {
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
	    text.id(show.r, cook[show.r], show.r, adj.x=FALSE)
	##  text.id(show.r, cook[show.r] + 0.4*cex.id * strheight(" "), show.r)
    }
    if (show[5]) {
	ylim <- range(rs, na.rm = TRUE)
	if (id.n > 0) {
	    ylim <- ylim + c(-1, 1) * 0.08 * diff(ylim)
	    show.r <- order(-cook)[iid]
	}
	plot(hatval, rs, ylim = ylim, main = main,
	     xlab = "Leverages", ylab = ylab23,
	     type="n", ...)
	panel(hatval, rs, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	p <- length(coef(x))
	for(crit in cook.levels){
	    curve(sqrt(crit*p*(1-x)/x), lty=2, add=T)
	    curve(-sqrt(crit*p*(1-x)/x), lty=2, add=T)
	}
	xmax <- par()$usr[2]
	ymult <- sqrt(p*(1-xmax)/xmax)
	aty <- c(-sqrt(rev(cook.levels))*ymult, sqrt(cook.levels)*ymult)
	axis(4, at=aty, labels=paste(c(rev(cook.levels), cook.levels)),
	     mgp=c(.25,.25,0), las=2, tck=0, cex.axis=cex.id)
	mtext(caption[5], 3, 0.25)
	if (id.n > 0) {
	    y.id <- rs[show.r]
	    y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
	    text(hatval[show.r], y.id, paste(show.r), pos=2, cex=cex.id, offset=0.25)
	}
    }
    if (show[6]) {
	ymx <- max(cook)*1.025
	g <- hatval/(1-hatval)
	plot(g, cook, ylim = c(0, ymx), main = main, xlab = "Leverage",
	     xaxt = "n", xlim=c(0, max(g)),
	     ylab = "Cook's distance", type="n", ...)
	athat <- pretty(hatval)
	axis(1, at=athat/(1-athat), labels=paste(athat))
	panel(g, cook, ...)
	if (one.fig)
	    title(sub = sub.caption, ...)
	p <- length(coef(x))
	bval <- pretty(sqrt(p*cook/g), 5)
	xmax <- par()$usr[2]
	ymax <- par()$usr[4]
	for(i in 1:length(bval)) {
	    bi2 <- bval[i]^2
	    if(ymax > bi2*xmax) {
		xi <- xmax + strwidth(" ")/3
		yi <- bi2*xi
		abline(0, bi2, lty=2)
		text(xi, yi, paste(bval[i]), adj=0, xpd=T)
	    } else
	{
	    yi <- ymax - 1.5*strheight(" ")
	    xi <- yi/bi2
	    lines(c(0, xi), c(0, yi), lty=2)
	    text(xi, ymax-0.8*strheight(" "), paste(bval[i]), adj=0.5, xpd=T)
	}
	}
	xmax <- par()$usr[2]
	## axis(4, at=p*cook.levels, labels=paste(c(rev(cook.levels), cook.levels)),
	##	mgp=c(.25,.25,0), las=2, tck=0, cex.axis=cex.id)
	mtext(caption[6], 3, 0.25)
	if (id.n > 0)
	    show.r <- order(-cook)[iid]
	text.id(g[show.r], cook[show.r], show.r)
    }

    if (!one.fig && par("oma")[3] >= 1)
	mtext(sub.caption, outer = TRUE, cex = 1.25)
    invisible()
}
