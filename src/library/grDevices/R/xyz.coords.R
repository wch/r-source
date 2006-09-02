## Both xy.coords() and xyz.coords()  --- should be kept in sync!

xy.coords <- function(x, y=NULL, xlab=NULL, ylab=NULL, log=NULL, recycle = FALSE)
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
		x <- seq_along(y)
	    }
	    else {
		colnames <- dimnames(x)[[2]]
		if(is.null(colnames)) {
		    xlab <- paste(ylab, "[,1]", sep="")
		    ylab <- paste(ylab, "[,2]", sep="")
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
	    xlab <- paste(ylab, "$x", sep="")
	    ylab <- paste(ylab, "$y", sep="")
	    y <- x[["y"]]
	    x <- x[["x"]]
	}
	else {
	    if(is.factor(x)) x <- as.numeric(x)
	    xlab <- "Index"
	    y <- x
	    x <- seq_along(x)
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
	    stop("'x' and 'y' lengths differ")
    }

    if(length(log) && log != "") {
	log <- strsplit(log, NULL)[[1]]
	if("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
	    n <- as.integer(sum(ii))
	    warning(sprintf(ngettext(n,
                            "%d x value <= 0 omitted from logarithmic plot",
                            "%d x values <= 0 omitted from logarithmic plot"),
                            n), domain = NA)
	    x[ii] <- NA
	}
	if("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
	    n <- as.integer(sum(ii))
	    warning(sprintf(ngettext(n,
                            "%d y value <= 0 omitted from logarithmic plot",
                            "%d y values <= 0 omitted from logarithmic plot"),
                            n), domain = NA)
	    y[ii] <- NA
	}
    }
    return(list(x=as.real(x), y=as.real(y), xlab=xlab, ylab=ylab))
}

xyz.coords <- function(x, y=NULL, z=NULL, xlab=NULL, ylab=NULL, zlab=NULL,
		       log = NULL, recycle = FALSE)
{
    ## Only x
    if(is.null(y)) {
	if (is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3
		&& length(rhs <- x[[3]]) == 3) {
		zlab <- deparse(x[[2]])
		ylab <- deparse(rhs[[3]])
		xlab <- deparse(rhs[[2]])
		pf <- parent.frame()
		z <- eval(x[[2]],   environment(x), pf)
		y <- eval(rhs[[3]], environment(x), pf)
		x <- eval(rhs[[2]], environment(x), pf)
	    }
	    else stop("invalid first argument [bad language object]")
	}
	else if(is.matrix(x) || is.data.frame(x)) {
	    x <- data.matrix(x)
	    if(ncol(x) < 2) stop("at least 2 columns needed")
	    if(ncol(x) == 2) {
		xlab <- "Index"
		y <- x[,1]
		z <- x[,2]
		x <- seq_along(y)
	    }
	    else { ## >= 3 columns
		colnames <- dimnames(x)[[2]]
		if(is.null(colnames)) {
		    zlab <- paste(xlab,"[,3]",sep="")
		    ylab <- paste(xlab,"[,2]",sep="")
		    xlab <- paste(xlab,"[,1]",sep="")
		}
		else {
		    xlab <- colnames[1]
		    ylab <- colnames[2]
		    zlab <- colnames[3]
		}
		y <- x[,2]
		z <- x[,3]
		x <- x[,1]
	    }
	}
	else if(is.list(x)) {
	    zlab <- paste(xlab,"$z",sep="")
	    ylab <- paste(xlab,"$y",sep="")
	    xlab <- paste(xlab,"$x",sep="")
	    y <- x[["y"]]
	    z <- x[["z"]]
	    x <- x[["x"]]
	}
    }

    ## Only x, y
    if(!is.null(y) && is.null(z)) {
	if(is.complex(x)) {
	    z <- y
	    y <- Im(x)
	    x <- Re(x)
	    zlab <- ylab
	    ylab <- paste("Im(", xlab, ")", sep="")
	    xlab <- paste("Re(", xlab, ")", sep="")
	}
	else if(is.complex(y)) {
	    z <- x
	    x <- Re(y)
	    y <- Im(y)
	    zlab <- xlab
	    xlab <- paste("Re(", ylab, ")", sep="")
	    ylab <- paste("Im(", ylab, ")", sep="")
	}
	else {
	    if(is.factor(x)) x <- as.numeric(x)
	    if(is.factor(y)) y <- as.numeric(y)
	    xlab <- "Index"
	    z <- y
	    y <- x
	    x <- seq_along(x)
	}
    }

    ## Lengths and recycle
    if(((xl <- length(x)) != length(y)) || (xl != length(z))) {
	if(recycle) {
	    ml <- max(xl, (yl <- length(y)), (zl <- length(z)))
	    if(xl < ml) x <- rep(x, length.out = ml)
	    if(yl < ml) y <- rep(y, length.out = ml)
	    if(zl < ml) z <- rep(z, length.out = ml)
	}
	else stop("'x', 'y' and 'z' lengths differ")
    }

    ## log
    if(length(log) && log != "") {
	log <- strsplit(log, NULL)[[1]]
	if("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
	    n <- sum(ii)
            warning(sprintf(ngettext(n,
                                     "%d x value <= 0 omitted from logarithmic plot",
                                     "%d x values <= 0 omitted from logarithmic plot"),
                            n), domain = NA)
	    x[ii] <- NA
	}
	if("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
	    n <- sum(ii)
            warning(sprintf(ngettext(n,
                                     "%d y value <= 0 omitted from logarithmic plot",
                                     "%d y values <= 0 omitted from logarithmic plot"),
                            n), domain = NA)
	    y[ii] <- NA
	}
	if("z" %in% log && any(ii <- z <= 0 & !is.na(z))) {
	    n <- sum(ii)
            warning(sprintf(ngettext(n,
                                     "%d z value <= 0 omitted from logarithmic plot",
                                     "%d z values <= 0 omitted from logarithmic plot"),
                            n), domain = NA)
	    z[ii] <- NA
	}
    }
    list(x=as.real(x), y=as.real(y), z=as.real(z),
	 xlab=xlab, ylab=ylab, zlab=zlab)
}
