## the obvious analog of  xy.coords() -- in ./plot.R

xyz.coords <- function(x, y, z, xlab=NULL, ylab=NULL, zlab=NULL,
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
		x <- seq(along=y)
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
	    x <- seq(along=x)
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
