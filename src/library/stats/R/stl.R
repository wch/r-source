#  File src/library/stats/R/stl.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

stl <- function(x, s.window,
		s.degree = 0,
		t.window = NULL, t.degree = 1,
		l.window = nextodd(period), l.degree = t.degree,
		s.jump = ceiling(s.window/10),
		t.jump = ceiling(t.window/10),
		l.jump = ceiling(l.window/10),
		robust = FALSE,
		inner = if(robust)  1 else 2,
		outer = if(robust) 15 else 0,
		na.action = na.fail)
{
    nextodd <- function(x){
	x <- round(x)
	if(x%%2==0) x <- x+1
	as.integer(x)
    }
    deg.check <- function(deg) {
	degname <- deparse(substitute(deg))
	deg <- as.integer(deg)
	if(deg < 0 || deg > 1) stop(gettextf("%s must be 0 or 1", degname), domain = NA)
	deg
    }
    x <- na.action(as.ts(x))
    if(is.matrix(x)) stop("only univariate series are allowed")
    n <- as.integer(length(x))
    if (is.na(n)) stop("invalid length(x)")
    period <- frequency(x)
    if(period < 2 || n <= 2 * period)
	stop("series is not periodic or has less than two periods")
    periodic <- FALSE
    if(is.character(s.window)) {
	if(is.na(pmatch(s.window, "periodic")))
	    stop("unknown string value for s.window")
	else {
	    periodic <- TRUE
	    s.window <- 10 * n + 1
	    s.degree <- 0
	}
    }
    s.degree <- deg.check(s.degree)
    t.degree <- deg.check(t.degree)
    l.degree <- deg.check(l.degree)
    if(is.null(t.window))
	t.window <- nextodd(ceiling( 1.5 * period / (1- 1.5/s.window)))
    storage.mode(x) <- "double"
    z <- .Fortran(C_stl, x, n,
		  as.integer(period),
		  as.integer(s.window),
		  as.integer(t.window),
		  as.integer(l.window),
		  s.degree, t.degree, l.degree,
		  nsjump = as.integer(s.jump),
		  ntjump = as.integer(t.jump),
		  nljump = as.integer(l.jump),
		  ni = as.integer(inner),
		  no = as.integer(outer),
		  weights = double(n),
		  seasonal = double(n),
		  trend = double(n),
		  double((n+2*period)*5))
    if(periodic) {
	## make seasonal part exactly periodic
	which.cycle <- cycle(x)
	z$seasonal <- tapply(z$seasonal, which.cycle, mean)[which.cycle]
    }
    remainder <- as.vector(x) - z$seasonal - z$trend
    y <- cbind(seasonal = z$seasonal, trend = z$trend, remainder = remainder)
    res <- list(time.series = ts(y, start = start(x), frequency = period),
		weights = z$weights, call = match.call(),
		win = c(s = s.window, t = t.window, l = l.window),
		deg = c(s = s.degree, t = t.degree, l = l.degree),
		jump = c(s = s.jump, t = t.jump, l = l.jump),
		inner = z$ni, outer = z$no)
    class(res) <- "stl"
    res
}

print.stl <- function(x, ...)
{
    cat(" Call:\n ")
    dput(x$call, control=NULL)
    cat("\nComponents\n")
    print(x$time.series, ...)
    invisible(x)
}

summary.stl <- function(object, digits = getOption("digits"), ...)
{
    cat(" Call:\n ")
    dput(object$call, control=NULL)
    cat("\n Time.series components:\n")
    print(summary(object$time.series, digits = digits, ...))
    cat(" IQR:\n")
    iqr <- apply(cbind(STL = object$time.series,
                       data = object$time.series %*% rep(1,3)),
		 2L, IQR)
    print(rbind(format(iqr, digits = max(2L, digits - 3L)),
		"   %"= format(round(100 * iqr / iqr["data"], 1))),
	  quote = FALSE)
    cat("\n Weights:")
    if(all(object$weights == 1)) cat(" all == 1\n")
    else { cat("\n"); print(summary(object$weights, digits = digits, ...)) }
    cat("\n Other components: ")
    str(object[-(1L:3)], give.attr = FALSE)
    invisible(object)
}

plot.stl <- function(x, labels = colnames(X),
		     set.pars = list(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0),
				     tck = -0.01, mfrow = c(nplot, 1)),
		     main = NULL, range.bars = TRUE, ...,
                     col.range = "light gray")
{
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    colnames(X) <- c("data", colnames(sers))
    nplot <- ncomp + 1
    if(range.bars)
	mx <- min(apply(rx <- apply(X,2, range), 2, diff))
    dev.hold(); on.exit(dev.flush())
    if(length(set.pars)) {
	oldpar <- do.call("par", as.list(names(set.pars)))
	on.exit(par(oldpar), add = TRUE)
	do.call("par", set.pars)
    }
    for(i in 1L:nplot) {
	plot(X[, i], type = if(i < nplot) "l" else "h",
	     xlab = "", ylab = "", axes = FALSE, ...)
	if(range.bars) {
	    dx <- 1/64 * diff(ux <- par("usr")[1L:2])
	    y <- mean(rx[,i])
	    rect(ux[2L] - dx, y + mx/2, ux[2L] - 0.4*dx, y - mx/2,
		 col = col.range, xpd = TRUE)
	}
	if(i == 1 && !is.null(main))
	    title(main, line = 2, outer = par("oma")[3L] > 0)
	if(i == nplot) abline(h=0)
	box()
	right <- i %% 2 == 0
	axis(2, labels = !right)
	axis(4, labels = right)
	axis(1, labels = i == nplot)
	mtext(labels[i], side = 2, 3)
    }
    mtext("time", side = 1, line = 3)
    invisible()
}
