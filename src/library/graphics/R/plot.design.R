plot.design <-
    function(x, y = NULL, fun = mean, data = NULL, ...,
             ylim = NULL, xlab = "Factors", ylab = NULL, main = NULL,
             ask = NULL, xaxt = par("xaxt"), axes = TRUE, xtick = FALSE)
{
    .plot.des <-
        function(x, y, fun, ylab, ylim = NULL, ...) {
	## Arguments: x : data.frame with only factor columns
	##	      y : one numeric vector

	if(!is.numeric(y))
	    stop("'y' must be a numeric vector")
	if(!is.data.frame(x)) # or allow factor (see 2 lines below)?? {FIXME}
	    stop("'x' must be a data frame")
	if(!all(sapply(x, is.factor)) & !is.factor(x)) # incl "ordered"
	    stop("all columns/components of 'x' must be factors")
	k <- ncol(x)
        if(any(is.na(y))) {
            FUN <- fun; fun <- function(u) FUN(u [!is.na(u)])
        }
	tot <- fun(y)
	stats <- lapply(x, function(xc) tapply(y, xc, fun))

	if(any(is.na(unlist(stats))))
	    warning("some levels of the factors are empty", call. = FALSE)
        if(is.null(ylim))
            ylim <- range(c(sapply(stats,range,na.rm = TRUE),tot))
	plot(c(0,k+1), ylim, type = "n", axes = axes, xaxt = "n",
             xlab = xlab, ylab = ylab, main = main, adj = 0.5, ...)
	segments(0.5, tot, k+0.5, tot, ...)
	for(i in 1:k) {
            si <- stats[[i]]
	    segments(i, min(si, na.rm = TRUE),
		     i, max(si, na.rm = TRUE), ...)
	    for(j in 1:(length(si))) {
                sij <- si[j]
		segments(i-0.05, sij, i+0.05, sij, ...)
		text(i-0.1, sij, labels = names(sij), adj = 1, ...)
	    }
	}
        if(axes && xaxt != "n")
            axis(1, at = 1:k, names(stats), xaxt= xaxt, tick = xtick,
                 mgp = {p <- par("mgp"); c(p[1], if(xtick) p[2] else 0, 0)},
                 ...)
    } ## .plot.des()

    ## 'fun' dealing
    fname <- deparse(substitute(fun))
    fun <- match.fun(fun)
    if (!(is.data.frame(x) | inherits(x,"formula")))
	stop("x must be a dataframe or a formula!")

    ## case 'switch' :
    if(is.data.frame(x)) {
	if(is.null(y)) { ## nothing to do
	} else if(inherits(y,"formula")) {
	    x <- model.frame(y , data = x)
	}
	else if(is.numeric(y)) {
	    x <- cbind(y,x[,sapply(x, is.factor)])
	    tmpname <- match.call()
	    names(x) <- as.character(c(tmpname[[3]],names(x[,-1])))
	}
	else if(is.character(y)) {
	    ynames <- y
	    y <- data.frame(x[,y])
	    if(sum(sapply(y, is.numeric)) != ncol(y)) {
		stop("a variable in y is not numeric")
	    }
	    x <- x[,sapply(x, is.factor)]
	    xnames <- names(x)
	    x <- cbind(x,y)
	    names(x) <- c(xnames,ynames)
	}
    }
    else if (is.data.frame(data)) {
	x <- model.frame(x , data = data)
    }
    else {
	x <- model.frame(x)
    }

    i.fac <- sapply(x, is.factor)
    i.num <- sapply(x, is.numeric)
    nResp <- sum(i.num)
    if (nResp == 0)
	stop("there must be at least one numeric variable!")
    yname <- names(x)[i.num]
    if(is.null(ylab))
	ylab <- paste(fname, "of", yname)
    ydata <- as.matrix(x[,i.num])
    if (!any(i.fac)) {
	x <- data.frame(Intercept = rep.int(" ", nrow(x)))
	i.fac <- 1
    }
    xf <- x[, i.fac, drop = FALSE]
    if (is.null(ask))
	ask <- prod(par("mfcol")) < nResp && dev.interactive()
    if (ask) {
        op <- par(ask = ask); on.exit(par(op))
    }
    for(j in 1:nResp) {
	.plot.des(xf, ydata[,j], fun = fun, ylab = ylab[j], ylim = ylim, ...)
    }
}
