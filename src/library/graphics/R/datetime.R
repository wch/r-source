#  File src/library/graphics/R/datetime.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

axis.POSIXct <- function(side, x, at, format, labels = TRUE, ...)
{
    mat <- missing(at) || is.null(at)
    if(!mat) x <- as.POSIXct(at) else x <- as.POSIXct(x)
    range <- par("usr")[if(side %%2) 1L:2L else 3L:4L]
    ## find out the scale involved
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone")
    if(d < 1.1*60) { # seconds
        sc <- 1
        if(missing(format)) format <- "%S"
    } else if (d < 1.1*60*60) { # minutes
        sc <- 60
        if(missing(format)) format <- "%M:%S"
    } else if (d < 1.1*60*60*24) {# hours
        sc <- 60*60
        if(missing(format)) format <- "%H:%M"
    } else if (d < 2*60*60*24) {
        sc <- 60*60
        if(missing(format)) format <- "%a %H:%M"
    } else if (d < 7*60*60*24) {# days of a week
        sc <- 60*60*24
        if(missing(format)) format <- "%a"
    } else { # days, up to a couple of months
        sc <- 60*60*24
    }
    if(d < 60*60*24*50) {
        zz <- pretty(z/sc)
        z <- zz*sc
        z <- .POSIXct(z,  attr(x, "tzone"))
        if(sc == 60*60*24) z <- as.POSIXct(round(z, "days"))
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*60*60*24*365) { # months
        z <- .POSIXct(z,  attr(x, "tzone"))
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1; zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon); M <- 2*m
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m+1)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        zz <- .POSIXlt(zz, attr(x, "tzone"))
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%b"
    } else { # years
        z <- .POSIXct(z,  attr(x, "tzone"))
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1; zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year); M <- length(zz$year)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        z <- as.POSIXct(.POSIXlt(zz))
        if(missing(format)) format <- "%Y"
    }
    if(!mat) z <- x[is.finite(x)] # override changes
    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    if (!is.logical(labels)) labels <- labels[keep]
    else if (identical(labels, TRUE))
	labels <- format(z, format = format)
    else if (identical(labels, FALSE))
	labels <- rep("", length(z)) # suppress labelling of ticks
    axis(side, at = z, labels = labels, ...)
}

hist.POSIXt <- function(x, breaks, ..., xlab = deparse(substitute(x)),
                        plot = TRUE, freq = FALSE,
                        start.on.monday = TRUE, format)
{
    if(!inherits(x, "POSIXt")) stop("wrong method")
    xlab
    x <- as.POSIXct(x)
    incr <- 1
    ## handle breaks ourselves
    if(missing(breaks))
	stop("Must specify 'breaks' in hist(<POSIXt>)")
    if (inherits(breaks, "POSIXt")) {
        breaks <- as.POSIXct(breaks)
        d <- min(abs(diff(unclass(breaks))))
        if(d > 60) incr <- 60
        if(d > 3600) incr <- 3600
        if(d > 86400) incr <- 86400
        if(d > 86400*7) incr <- 86400*7
        if(d > 86400*28) incr <- 86400*28
        if(d > 86400*366) incr <- 86400*366
        num.br <- FALSE
    } else {
        num.br <- is.numeric(breaks) && length(breaks) == 1
        if(num.br) {
        ## specified number of breaks
        } else if(is.character(breaks) && length(breaks) == 1) {
            valid <-
                pmatch(breaks,
                       c("secs", "mins", "hours", "days", "weeks",
                         "months", "years", "quarters"))
            if(is.na(valid)) stop("invalid specification of 'breaks'")
            start <- as.POSIXlt(min(x, na.rm = TRUE))
            ## may alter later
            ## we need to invalidate isdst whenever we play with components
            incr <- 1
            if(valid > 1L) { start$sec <- 0; incr <- 59.99 }
            if(valid > 2L) { start$min <- 0L; incr <- 3600 - 1 }
            if(valid > 3L) { start$hour <- 0L; incr <- 86400 - 1 }
            if(valid > 4L) { start$isdst <- -1L}
            if(valid == 5L) { # "weeks"
                start$mday <- start$mday - start$wday
                if(start.on.monday)
                    start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
                incr <- 7*86400
            }
            if(valid == 6L) { # "months"
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (31 * 86400))
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- seq(start, end, "months")
                ind <- seq_along(breaks[-1L])
                breaks[ind] <- breaks[ind] - 86400
            } else if(valid == 7L) { # "years"
                start$mon <- 0L
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (366 * 86400))
                end$mon <- 0L
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- seq(start, end, "years")
                ind <- seq_along(breaks[-1L])
                breaks[ind] <- breaks[ind] - 86400
            } else if(valid == 8L) { # "quarters"
                qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
                start$mon <- qtr[start$mon + 1L]
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (93 * 86400))
                end$mon <- qtr[end$mon + 1L]
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- seq(start, end, "3 months")
                ind <- seq_along(breaks[-1L])
                breaks[ind] <- breaks[ind] - 86400
           } else { # "days" or "weeks"
                maxx <- max(x, na.rm = TRUE)
                breaks <- seq(start, maxx + incr, breaks)
                breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
            }
        }
        else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE,
                        warn.unused = FALSE, ...)
    res$equidist <- TRUE # years are of uneven lengths
    res$intensities <- res$intensities*incr
    res$xname <- xlab
    if(plot) {
        ## trick to swallow arguments for hist.default, separate out 'axes'
        myplot <- function(res, xlab, freq, format, breaks,
                           right, include.lowest, labels = FALSE,
                           axes = TRUE, xaxt = par("xaxt"), ...)
        {
	    plot(res, xlab = xlab, axes = FALSE, freq = freq,
		 labels = labels, ...)
	    if(axes) {
		axis(2, ...)
		if(xaxt != "n") {
		    if(num.br) breaks <- c.POSIXct(res$breaks)
		    axis.POSIXct(1, at = breaks,  format = format, ...)
					# '...' : e.g. cex.axis
		}
	    }
        }
        myplot(res, xlab, freq, format, breaks, ...)
     }
    invisible(res)
}


## methods for class "Date"

axis.Date <- function(side, x, at, format, labels = TRUE, ...)
{
    mat <- missing(at) || is.null(at)
    if(!mat) x <- as.Date(at) else x <- as.Date(x)
    range <- par("usr")[if(side %%2) 1L:2L else 3:4L]
    range[1L] <- ceiling(range[1L])
    range[2L] <- floor(range[2L])
    ## find out the scale involved
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    class(z) <- "Date"
    if (d < 7) # days of a week
        if(missing(format)) format <- "%a"
    if(d < 100) { # month and day
        z <- structure(pretty(z), class="Date")
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*365) { # months
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m+1)
        z <- as.Date(zz)
        if(missing(format)) format <- "%b"
    } else { # years
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$mon <- 0
        zz$year <- pretty(zz$year)
        z <- as.Date(zz)
        if(missing(format)) format <- "%Y"
    }
    if(!mat) z <- x[is.finite(x)] # override changes
    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    z <- sort(unique(z)); class(z) <- "Date"
    if (!is.logical(labels)) labels <- labels[keep]
    else if (identical(labels, TRUE))
	labels <- format.Date(z, format = format)
    else if (identical(labels, FALSE))
	labels <- rep("", length(z)) # suppress labelling of ticks
    axis(side, at = z, labels = labels, ...)
}


hist.Date <- function(x, breaks, ..., xlab = deparse(substitute(x)),
                      plot = TRUE, freq = FALSE,
                      start.on.monday = TRUE, format)
{
    if(!inherits(x, "Date")) stop("wrong method")
    force(xlab)
    incr <- 1
    ## handle breaks ourselves
    if(missing(breaks))
        stop("Must specify 'breaks' in hist(<Date>)")
    if (inherits(breaks, "Date")) {
        breaks <- as.Date(breaks)
        d <- min(abs(diff(unclass(breaks))))
        if(d > 1) incr <- 1
        if(d > 7) incr <- 7
        if(d > 28) incr <- 28
        if(d > 366) incr <- 366
        num.br <- FALSE
    } else {
        num.br <- is.numeric(breaks) && length(breaks) == 1L
        if(num.br) {
            ## specified number of breaks
        } else if(is.character(breaks) && length(breaks) == 1L) {
            valid <- pmatch(breaks, c("days", "weeks", "months", "years",
                                      "quarters"))
            if(is.na(valid)) stop("invalid specification of 'breaks'")
            start <- as.POSIXlt(min(x, na.rm = TRUE))
            incr <- 1
            if(valid > 1L) { start$isdst <- -1L}
            if(valid == 2L) { ## "weeks"
                start$mday <- start$mday - start$wday
                if(start.on.monday)
                    start$mday <- start$mday + ifelse(start$wday > 0L, 1L, -6L)
                incr <- 7
                ## drops through to "days".
            }
            if(valid == 3L) { ## "months"
                start$mday <- 1
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (31 * 86400))
                end$mday <- 1
                end$isdst <- -1
                breaks <- as.Date(seq(start, end, "months")) - 1
            } else if(valid == 4L) { ## "years"
                start$mon <- 0L
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (366 * 86400))
                end$mon <- 0L
                end$mday <- 1L
                end$isdst <- -1
                breaks <- as.Date(seq(start, end, "years")) - 1
            } else if(valid == 5L) { ## "quarters"
                qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
                start$mon <- qtr[start$mon + 1L]
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (93 * 86400))
                end$mon <- qtr[end$mon + 1L]
                end$mday <- 1L
                end$isdst <- -1
                breaks <- as.Date(seq(start, end, "3 months")) - 1
            } else { ## "days" (or "weeks")
                start <- as.Date(start)
                maxx <- max(x, na.rm = TRUE)
                breaks <- seq(start, maxx + incr, breaks)
                breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
            }
        } else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE, warn.unused = FALSE, ...)
    res$equidist <- TRUE # years are of uneven lengths
    res$intensities <- res$intensities*incr
    res$xname <- xlab
    if(plot) {
        ## trick to swallow arguments for hist.default, separate out 'axes'
        myplot <- function(res, xlab, freq, format, breaks,
                           right, include.lowest, labels = FALSE,
                           axes = TRUE, xaxt = par("xaxt"), ...)
        {
            plot(res, xlab = xlab, axes = FALSE, freq = freq,
                 labels = labels, ...)
            if(axes && xaxt != "n") {
                axis(2, ...)
                if(num.br) breaks <- c.Date(res$breaks)
                axis.Date(1, at = breaks,  format = format, ...)
            }
        }
        myplot(res, xlab, freq, format, breaks, ...)
     }
    invisible(res)
}

Axis.Date <- function(x=NULL, at=NULL, ..., side, labels=TRUE)
    axis.Date(side=side, x=x, at=at, labels=labels, ...)

Axis.POSIXt <- function(x=NULL, at=NULL, ..., side, labels=TRUE)
    axis.POSIXct(side=side, x=x, at=at, labels=labels, ...)

