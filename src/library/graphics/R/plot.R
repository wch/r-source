## NOTE that xyz.coords() in ./xyz.coords.R  should be kept in sync!
##
xy.coords <- function(x, y, xlab=NULL, ylab=NULL, log=NULL, recycle = FALSE)
{
    if(is.null(y)) {
	ylab <- xlab
	if(is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3) {
		ylab <- deparse(x[[2]])
		xlab <- deparse(x[[3]])
		y <- eval(x[[2]], environment(x), parent.frame())
		x <- eval(x[[3]], environment(x), parent.frame())
	    }
	    else stop("invalid first argument")
	}
	else if(inherits(x, "ts")) {
	    y <- if(is.matrix(x)) x[,1] else x
	    x <- stats::time(x)
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
    ## to allow e.g. lines, points, identify to be used with plot.POSIXlt
    if(inherits(x, "POSIXt")) x <- as.POSIXct(x)

    if(length(x) != length(y)) {
	if(recycle) {
	    if((nx <- length(x)) < (ny <- length(y)))
		x <- rep(x, length.out = ny)
	    else
		y <- rep(y, length.out = nx)
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

plot <- function (x, y, ...)
{
    if (is.null(attr(x, "class")) && is.function(x)) {
	nms <- names(list(...))
	## need to pass `y' to plot.function() when positionally matched
	if(missing(y)) # set to defaults {could use formals(plot.default)}:
	    y <- { if (!"from" %in% nms) 0 else
		   if (!"to"   %in% nms) 1 else
		   if (!"xlim" %in% nms) NULL }
	if ("ylab" %in% nms)
	    plot.function(x,  y, ...)
	else
	    plot.function(x, y, ylab=paste(deparse(substitute(x)),"(x)"), ...)
    }
    else UseMethod("plot")
}

## xlim = NULL (instead of "missing", since it will be passed to plot.default:
plot.function <- function(x, from = 0, to = 1, xlim = NULL, ...) {
    if(!is.null(xlim)) {
	if(missing(from)) from <- xlim[1]
	if(missing(to))	  to   <- xlim[2]
    }
    curve(x, from, to, xlim = xlim, ...)
}

## NOTE: cex = 1 is correct, cex = par("cex") gives *square* of intended!
plot.default <- function(x, y=NULL, type="p", xlim=NULL, ylim=NULL,
			 log="", main=NULL, sub=NULL, xlab=NULL, ylab=NULL,
			 ann=par("ann"), axes=TRUE, frame.plot=axes,
			 panel.first=NULL, panel.last=NULL,
			 col=par("col"), bg=NA, pch=par("pch"),
			 cex = 1, lty=par("lty"), lab=par("lab"),
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
    panel.first # eval() is wrong here {Ross I.}
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
    if(missing(y) || is.factor(y)) {## <==> will do barplot(.)
        dargs <- list(...)
        axisnames <- if (!is.null(dargs$axes)) dargs$axes
            else if (!is.null(dargs$xaxt)) dargs$xaxt != "n"
            else TRUE
    }
    if (missing(y)) {
	barplot(table(x), axisnames=axisnames, ...)
    } else if (is.factor(y)) {
	barplot(table(y, x), legend.text=legend.text, axisnames=axisnames, ...)
    } else if (is.numeric(y))
	boxplot(y ~ x, ...)
    else NextMethod("plot")
}

## FIXME (ideas/wishes):
## o for 1-D tables:
##   - alternatively, and/or as default, type = "bar" ??!??
##   - if "h", make the default lwd depend on number of classes instead of lwd=2
plot.table <-
    function(x, type = "h", ylim = c(0, max(x)), lwd = 2,
             xlab = NULL, ylab = NULL, frame.plot = is.num, ...)
{
    xnam <- deparse(substitute(x))
    rnk <- length(dim(x))
    if(rnk == 0)
	stop("invalid table `x'")
    if(rnk == 1) {
	dn <- dimnames(x)
	nx <- dn[[1]]
	if(is.null(xlab)) xlab <- names(dn)
	if(is.null(xlab)) xlab <- ""
	if(is.null(ylab)) ylab <- xnam
	ow <- options(warn = -1)
	is.num <- !any(is.na(xx <- as.numeric(nx))); options(ow)
	x0 <- if(is.num) xx else seq(x)
	plot(x0, unclass(x), type = type,
	     ylim = ylim, xlab = xlab, ylab = ylab, frame.plot = frame.plot,
	     lwd = lwd, ..., xaxt = "n")
	xaxt <-
	    if(length(as <- list(...))) {
		if(!is.null(as$axes) && !as$axes) "n" else as$xaxt
	    }## else NULL
	axis(1, at = x0, labels = nx, xaxt = xaxt)
    } else
	mosaicplot(x, xlab = xlab, ylab = ylab, ...)
}

plot.formula <-
function(formula, data = parent.frame(), ..., subset,
         ylab = varnames[response], ask = TRUE)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    m$ylab <- m$... <- m$ask <- NULL
    subset.expr <- m$subset
    m$subset <- NULL
    m[[1]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(subset.expr, data, parent.frame())
	l <- nrow(mf)
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
	mf <- mf[s,]
    }
    ## check for horizontal arg
    horizontal <- FALSE
    if("horizontal" %in% names(dots)) horizontal <- dots[["horizontal"]]
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	funname <- NULL
	if( is.object(y) ) {
	    found <- FALSE
	    for(j in class(y)) {
		funname <- paste("plot.",j,sep = "")
		if( exists(funname) ) {
		    found <- TRUE
		    break;
		}
	    }
	    if( !found )
		funname <- NULL
	}
	if( is.null(funname) )
	    funname <- "plot"
	if (length(varnames) > 2) {
	    opar <- par(ask = ask)
	    on.exit(par(opar))
	}
	xn <- varnames[-response]
        if(length(xn) > 0) {
            if( !is.null(xlab<- dots[["xlab"]]) )
                dots <- dots[-match("xlab", names(dots))]
            for (i in xn) {
                xl <- if(is.null(xlab)) i else xlab
                yl <- ylab
                if(horizontal && is.factor(mf[[i]])) {yl <- xl; xl <- ylab}
                   do.call(funname,
                           c(list(mf[[i]], y, ylab = yl, xlab = xl), dots))
               }
	} else do.call(funname, c(list(y, ylab = ylab), dots))
    }
    else plot.data.frame(mf)
}

lines.formula <-
function(formula,  data = parent.frame(), ..., subset)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(m$subset, data, parent.frame())
	l <- nrow(data)
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
    }
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	if (length(varnames) > 2)
	    stop("cannot handle more than one x coordinate")
	xn <- varnames[-response]
	if (length(xn) == 0)
	    do.call("lines",
		    c(list(y), dots))
	else
	    do.call("lines",
		    c(list(mf[[xn]], y), dots))
    }
    else
	stop("must have a response variable")
}

points.formula <-
function(formula, data = parent.frame(), ..., subset)
{
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
	m$data <- as.data.frame(data)
    dots <- m$...
    dots <- lapply(dots, eval, data, parent.frame())
    m$... <- NULL
    m[[1]] <- as.name("model.frame")
    m <- as.call(c(as.list(m), list(na.action = NULL)))
    mf <- eval(m, parent.frame())
    if (!missing(subset)) {
	s <- eval(m$subset, data, parent.frame())
        ## need the number of points before subsetting
	if(!missing(data)) {
            l <- nrow(data)
        } else {
            mtmp <- m
            mtmp$subset <- NULL
            l <- nrow(eval(mtmp, parent.frame()))
        }
	dosub <- function(x) if (length(x) == l) x[s] else x
	dots <- lapply(dots, dosub)
    }
    response <- attr(attr(mf, "terms"), "response")
    if (response) {
	varnames <- names(mf)
	y <- mf[[response]]
	if (length(varnames) > 2)
	    stop("cannot handle more than one x coordinate")
	xn <- varnames[-response]
	if (length(xn) == 0)
	    do.call("points",
		    c(list(y), dots))
	else
	    do.call("points",
		    c(list(mf[[xn]], y), dots))
    }
    else
	stop("must have a response variable")
}

plot.xy <- function(xy, type, pch = 1, lty = "solid", col = par("fg"),
		    bg = NA, cex = 1, lwd = par("lwd"), ...) {
    .Internal(plot.xy(xy, type, pch, lty, col, bg, cex, lwd, ...))
}

plot.new <- function()
{
    .Internal(plot.new())
    for(fun in getHook("plot.new")) {
        if(is.character(fun)) fun <- get(fun)
        try(fun())
    }
}

frame <- plot.new

plot.window <- function(xlim, ylim, log = "", asp = NA, ...)
    .Internal(plot.window(xlim, ylim, log, asp, ...))

plot.data.frame <- function (x, ...) {
    if(!is.data.frame(x))
	stop("plot.data.frame applied to non data frame")
    x <- data.matrix(x)
    if(ncol(x) == 1) {
	stripchart(x, ...)
    }
    else if(ncol(x) == 2) {
	plot(x, ...)
    }
    else {
	pairs(x, ...)
    }
}

## unexported hook for testing
.newplot.hook <- function()
{
    pp <- par(c("mfg","mfcol","oma","mar"))
    if(all(pp$mfg[1:2] == c(1, pp$mfcol[2]))) {
	outer <- (oma4 <- pp$oma[4]) > 0; mar4 <- pp$mar[4]
	mtext(paste("help(", ..nameEx, ")"), side = 4,
              line = if(outer)max(1, oma4 - 1) else min(1, mar4 - 1),
              outer = outer, adj = 1, cex = .8, col = "orchid", las=3)
    }
}
