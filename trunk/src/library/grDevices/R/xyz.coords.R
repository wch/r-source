#  File src/library/grDevices/R/xyz.coords.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
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
#  https://www.R-project.org/Licenses/

## Both xy.coords() and xyz.coords()  --- should be kept in sync!

xy.coords <-
    function(x, y = NULL, xlab = NULL, ylab = NULL, log = NULL, recycle = FALSE,
             setLab = TRUE)
{
    if(is.null(y)) {
	if(is.null(ylab)) ylab <- xlab
	if(is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3) {
                if(setLab) {
                    ylab <- deparse(x[[2L]])
                    xlab <- deparse(x[[3L]])
                }
		y <- eval(x[[2L]], environment(x))
		x <- eval(x[[3L]], environment(x))
	    }
	    else stop("invalid first argument")
	}
	else if(inherits(x, "ts")) {
	    y <- if(is.matrix(x)) x[,1] else x
	    x <- stats::time(x)
            if(setLab) xlab <- "Time"
	}
	else if(is.complex(x)) {
	    y <- Im(x)
	    x <- Re(x)
            if(setLab) {
                xlab <- paste0("Re(", ylab, ")")
                ylab <- paste0("Im(", ylab, ")")
            }
	}
	else if(is.matrix(x) || is.data.frame(x)) {
	    x <- data.matrix(x)
	    if(ncol(x) == 1) {
		if(setLab) xlab <- "Index"
		y <- x[,1]
		x <- seq_along(y)
	    }
	    else {
		colnames <- dimnames(x)[[2L]]
                if(setLab) {
                    if(is.null(colnames)) {
                        xlab <- paste0(ylab, "[,1]")
                        ylab <- paste0(ylab, "[,2]")
                    }
                    else {
                        xlab <- colnames[1L]
                        ylab <- colnames[2L]
                    }
                }
		y <- x[,2]
		x <- x[,1]
	    }
	}
	else if(is.list(x)) {
            if (all(c("x", "y") %in% names(x))) {
                if(setLab) {
                    xlab <- paste0(ylab, "$x")
                    ylab <- paste0(ylab, "$y")
                }
                y <- x[["y"]]
                x <- x[["x"]]
            } else
                stop("'x' is a list, but does not have components 'x' and 'y'")
	}
	else {
	    if(is.factor(x)) x <- as.numeric(x)
	    if(setLab) xlab <- "Index"
	    y <- x
	    x <- seq_along(x)
	}
    }
    ## to allow e.g. lines, points, identify to be used with plot.POSIXlt
    if(inherits(x, "POSIXt")) x <- as.POSIXct(x)

    if(length(x) != length(y)) {
	if(recycle) {
	    if((nx <- length(x)) < (ny <- length(y)))
		x <- rep_len(x, ny)
	    else
		y <- rep_len(y, nx)
	}
	else
	    stop("'x' and 'y' lengths differ")
    }

    if(length(log) && log != "") {
	log <- strsplit(log, NULL)[[1L]]
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
    list(x=as.double(x), y=as.double(y), xlab=xlab, ylab=ylab)
}

xyz.coords <- function(x, y=NULL, z=NULL, xlab=NULL, ylab=NULL, zlab=NULL,
		       log = NULL, recycle = FALSE, setLab = TRUE)
{
    ## Only x
    if(is.null(y)) {
	if (is.language(x)) {
	    if (inherits(x, "formula") && length(x) == 3
		&& length(rhs <- x[[3L]]) == 3) {
                if(setLab) {
                    zlab <- deparse(x[[2L]])
                    ylab <- deparse(rhs[[3L]])
                    xlab <- deparse(rhs[[2L]])
                }
		pf <- parent.frame()
		z <- eval(x[[2L]],   environment(x), pf)
		y <- eval(rhs[[3L]], environment(x), pf)
		x <- eval(rhs[[2L]], environment(x), pf)
	    }
	    else stop("invalid first argument [bad language object]")
	}
	else if(is.matrix(x) || is.data.frame(x)) {
	    x <- data.matrix(x)
	    if(ncol(x) < 2) stop("at least 2 columns needed")
	    if(ncol(x) == 2) {
		if(setLab) xlab <- "Index"
		y <- x[,1]
		z <- x[,2]
		x <- seq_along(y)
	    }
	    else { ## >= 3 columns
		colnames <- dimnames(x)[[2L]]
                if(setLab) {
                    if(is.null(colnames)) {
                        zlab <- paste0(xlab,"[,3]")
                        ylab <- paste0(xlab,"[,2]")
                        xlab <- paste0(xlab,"[,1]")
                    }
                    else {
                        xlab <- colnames[1L]
                        ylab <- colnames[2L]
                        zlab <- colnames[3L]
                    }
                }
		y <- x[,2]
		z <- x[,3]
		x <- x[,1]
	    }
	}
	else if(is.list(x)) {
            if (all(c("x", "y", "z") %in% names(x))) {
                if(setLab) {
                    zlab <- paste0(xlab,"$z")
                    ylab <- paste0(xlab,"$y")
                    xlab <- paste0(xlab,"$x")
                }
                y <- x[["y"]]
                z <- x[["z"]]
                x <- x[["x"]]
            } else
                stop("'x' is a list, but does not have components 'x', 'y'  and 'z'")
        }
    }

    ## Only x, y
    if(!is.null(y) && is.null(z)) {
	if(is.complex(x)) {
	    z <- y
	    y <- Im(x)
	    x <- Re(x)
            if(setLab) {
                zlab <- ylab
                ylab <- paste0("Im(", xlab, ")")
                xlab <- paste0("Re(", xlab, ")")
            }
	}
	else if(is.complex(y)) {
	    z <- x
	    x <- Re(y)
	    y <- Im(y)
            if(setLab) {
                zlab <- xlab
                xlab <- paste0("Re(", ylab, ")")
                ylab <- paste0("Im(", ylab, ")")
            }
	}
	else {
	    if(is.factor(x)) x <- as.numeric(x)
	    if(is.factor(y)) y <- as.numeric(y)
            if(setLab) xlab <- "Index"
	    z <- y
	    y <- x
	    x <- seq_along(x)
	}
    }

    ## Lengths and recycle
    if(((xl <- length(x)) != length(y)) || (xl != length(z))) {
	if(recycle) {
	    ml <- max(xl, (yl <- length(y)), (zl <- length(z)))
	    if(xl < ml && !is.null(x)) x <- rep_len(x, ml)
	    if(yl < ml && !is.null(y)) y <- rep_len(y, ml)
	    if(zl < ml && !is.null(z)) z <- rep_len(z, ml)
	}
	else stop("'x', 'y' and 'z' lengths differ")
    }

    ## log
    if(length(log) && log != "") {
	log <- strsplit(log, NULL)[[1L]]
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
    list(x=as.double(x), y=as.double(y), z=as.double(z),
	 xlab=xlab, ylab=ylab, zlab=zlab)
}
