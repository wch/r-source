xy.coords <- function(x, y, xlab=NULL, ylab=NULL, log=NULL, recycle = FALSE)
{
    if(is.null(y)) {
	ylab <- xlab
	if(is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3) {
		ylab <- deparse(x[[2]])
		xlab <- deparse(x[[3]])
		y <- eval(x[[2]], sys.frame(sys.parent()))
		x <- eval(x[[3]], sys.frame(sys.parent()))
	    }
	    else stop("invalid first argument")
	}
	else if(is.ts(x)) {
	    y <- if(is.matrix(x)) x[,1] else x
	    x <- time(x)
	    xlab <- "Time"
	}
	else if(is.complex(x)) {
	    y <- Im(x)
	    x <- Re(x)
	    xlab <- paste("Re(", ylab, ")", sep="")
	    ylab <- paste("Im(", ylab, ")", sep="")
	}
	else if(is.matrix(x) || is.data.frame(x)) {
	    x <- data.matrix(x)
	    if(ncol(x) == 1) {
		xlab <- "Index"
		y <- x[,1]
		x <- 1:length(y)
	    }
	    else {
		colnames <- dimnames(x)[[2]]
		if(is.null(colnames)) {
		    xlab <- paste(ylab,"[,1]",sep="")
		    ylab <- paste(ylab,"[,2]",sep="")
		}
		else {
		    xlab <- colnames[1]
		    ylab <- colnames[2]
		}
		y <- x[,2]
		x <- x[,1]
	    }
	}
	else if(is.list(x)) {
	    xlab <- paste(ylab,"$x",sep="")
	    ylab <- paste(ylab,"$y",sep="")
	    y <- x[["y"]]
	    x <- x[["x"]]
	}
	else {
	    if(is.factor(x)) x <- as.numeric(x)
	    xlab <- "Index"
	    y <- x
	    x <- 1:length(x)
	}
    }

    if(length(x) != length(y)) {
	if(recycle) {
	    if((nx <- length(x)) < (ny <- length(y)))
		x <- rep(x, length= ny)
	    else
		y <- rep(y, length= nx)
	}
	else
	    stop("x and y lengths differ")
    }

    if(length(log) && log != "") {
	log <- strsplit(log, NULL)[[1]]
	if("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
	    n <- sum(ii)
	    warning(paste(n, " x value", if(n>1)"s",
			  " <= 0 omitted from logarithmic plot", sep=""))
	    x[ii] <- NA
	}
	if("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
	    n <- sum(ii)
	    warning(paste(n, " y value", if(n>1)"s",
			  " <= 0 omitted from logarithmic plot", sep=""))
	    y[ii] <- NA
	}
    }
    return(list(x=as.real(x), y=as.real(y), xlab=xlab, ylab=ylab))
}

plot <- function(x, ...) {
    if(is.null(class(x)) && is.function(x)) {
        if("ylab" %in% names(list(...)))
            plot.function(x, ...)
        else
            plot.function(x, ylab=paste(deparse(substitute(x)),"(x)"), ...)
    }
    else UseMethod("plot")
}

plot.function <- function(fn, from=0, to=1, ...) {
    curve(fn, from, to, ...)
}

### NOTE: cex = 1 is correct, cex = par("cex") gives *square* of intended!

plot.default <- function(x, y=NULL, type="p", xlim=NULL, ylim=NULL,
			 log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
			 ann=par("ann"), axes=TRUE, frame.plot=axes,
			 panel.first=NULL, panel.last=NULL,
			 col=par("col"), bg=NA, pch=par("pch"),
			 cex=1, lty=par("lty"), lab=par("lab"),
                         lwd=par("lwd"), asp=NA, ...)
{
    xlabel <- if (!missing(x)) deparse(substitute(x))
    ylabel <- if (!missing(y)) deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log)
    xlab <- if (is.null(xlab)) xy$xlab else xlab
    ylab <- if (is.null(ylab)) xy$ylab else ylab
    xlim <- if (is.null(xlim)) range(xy$x[is.finite(xy$x)]) else xlim
    ylim <- if (is.null(ylim)) range(xy$y[is.finite(xy$y)]) else ylim
    plot.new()
    plot.window(xlim, ylim, log, asp, ...)
    panel.first
    plot.xy(xy, type, col=col, pch=pch, cex=cex, bg=bg, lty=lty, lwd=lwd, ...)
    panel.last
    if (axes) {
	axis(1, ...)
	axis(2, ...)
    }
    if (frame.plot)
	box(...)
    if (ann)
	title(main=main, sub=sub, xlab=xlab, ylab=ylab, ...)
    invisible()
}

plot.factor <- function(x, y, legend.text=levels(y), ...)
{
    if(missing(y) || is.factor(y)) ## <==> will do barplot(.)
        axisnames <- if(length(dargs <- list(...)) > 0) {
            nam <- names(dargs)
            ((any("axes" == nam) && dargs$axes) ||
             (any("xaxt" == nam) && dargs$xaxt != "n"))
        } else TRUE
    if (missing(y)) {
	barplot(table(x), axisnames=axisnames, ...)
    } else if (is.factor(y)) {
        barplot(table(y, x), legend.text=legend.text, axisnames=axisnames, ...)
    } else if (is.numeric(y))
	boxplot(y ~ x, ...)
    else NextMethod("plot")
}

plot.formula <- function(formula, data = NULL, subset, na.action,
			 ylab=varnames[response],..., ask = TRUE)
{
    if (missing(na.action)) na.action <- options()$na.action
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, sys.frame(sys.parent()))))
	m$data <- as.data.frame(data)
    m$ylab <- m$... <- NULL
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, sys.frame(sys.parent()))
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	if (length(varnames) > 2) {
	    opar <- par(ask = ask)
	    on.exit(par(opar))
	}
	xn <- .Alias(varnames[-response])
	if (is.null(list(...)[["xlab"]])) {
	    for (i in xn) plot(mf[[i]], y, ylab = ylab, xlab = i, ...)
	} else {
	    for (i in xn) plot(mf[[i]], y, ylab = ylab, ...)
	}
    }
    else plot.data.frame(mf)
}

plot.xy <- function(xy, type, pch = 1, lty = "solid", col = par("fg"),
                    bg = NA, cex = 1, ...) {
    .Internal(plot.xy(xy, type, pch, lty, col, bg, cex, ...))
}

plot.new <- function(ask = NA) .Internal(plot.new(ask))

frame <- .Alias(plot.new)
