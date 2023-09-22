#  File src/library/graphics/R/datetime.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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


### internal function used by axis.Date() and axis.POSIXct(). Extends the current format 
### if not sufficiently precise for value in argument at.
extendDateTimeFormat <- function(x, z){
    ## vectorize when needed:
    if(length(z) > 1L){
        formats <- vapply(z, function(zz) extendDateTimeFormat(x, zz), "")
        return(formats[which.max(nchar(formats))])
    }

    # used format:
    format <- attr(grDevices:::prettyDate(x), "format")
    formatparts <- gsub("%", "", strsplit(format, " |:|-")[[1]])

    # appropriate format for z
    chz <- format(z, "%Y-%m-%d %H:%M:%OS6")  # max. detailed format
    chz <- as.numeric(strsplit(chz, "-| |:|\\.")[[1]])
    default <- c(1, 1, 1, 0, 0, 0, 0)
    names(chz) <- names(default) <-  c("Y", if("m" %in% formatparts) "m" else "b",
                                       "d", "H", "M", "S", "OS6")
    if(any(w <- names(chz) %in% formatparts)){
        if((L <- max(which(w))+1) <= length(chz)){
            add <- chz[L:length(chz)]
            # add month
            if(chz[2] > default[2]){
                if("b" %in% names(add)) format <- paste(format, "%b")
                if("m" %in% names(add)) format <- paste(format, "-%m")
            }
            # add day
            if(chz[3] > default[3] && "d" %in% names(add)){
                if("Y" %in% formatparts & "b" %in% formatparts) format <- paste("%d", format)
                if("Y" %in% formatparts & "m" %in% formatparts) format <- paste0(format, "-%d")
                if(!"Y" %in% formatparts) format <- paste0(format, if("m" %in% formatparts) "-%d" else " %d")
            }
            add <- add[add > 0L]
            if(length(add) && any(c("H", "M", "S", "OS6") %in% names(add))){
                if(!all(c("H", "M") %in% formatparts)) format <- paste(format, "%H:%M")
                if("S" %in% names(add) & !"OS6" %in% names(add)) format <- paste0(format, ":%S")
                if("OS6" %in% names(add)) format <- gsub(":%S", "", paste0(format, ":%OS6"))
            }
        }
    }
    format
}


axis.POSIXct <- function(side, x, at, format, labels = TRUE, ...)
{
    has.at <- !missing(at) && !is.null(at)
    range <- sort(par("usr")[if(side %% 2) 1L:2L else 3L:4L])
    tz <- if(!missing(x) && ("tzone" %in% names(attributes(x)))) attr(x, "tzone") else ""
    rangeTime <- .POSIXct(range, tz = tz)
    
    if(has.at){
        # convert at to POSIXct:
        if(is.numeric(at))
            z <- .POSIXct(at, tz = tz)
        else{
            if(inherits(at, "POSIXt")){
                z <- if(inherits(at, "POSIXlt")) .POSIXct(as.numeric(at), tz = tz) else at
                attr(z, "tzone") <- tz
            }else{
                z <- sapply(at, function(a) as.POSIXct(as.character(a), tz = tz))
                if(is.numeric(z)) z <- .POSIXct(z, tz = tz)
            }
        }    
        z <- z[is.finite(z)] 
        
        # find format if missing:
        if(missing(format)){
#            format <- if(!missing(x)) attr(grDevices:::prettyDate(x), "format") else attr(grDevices:::prettyDate(rangeTime), "format")
#        }else if(is.null(format)){ # exdend format if needed for proper representation of at
            format <- extendDateTimeFormat(if(!missing(x)) x else rangeTime, z)
        }
    } else {
        z <- grDevices:::prettyDate(rangeTime, n = par("lab")[2 - side %% 2])
        if(missing(format)) format <- attr(z, "format")
    }

    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    if (!is.logical(labels)) 
        labels <- labels[keep]
    else if (isTRUE(labels))
        labels <- format(z, format = format)
    else if (isFALSE(labels))
        labels <- rep("", length(z)) # suppress labelling of ticks
        
    axis(side, at = z, labels = labels, ...)
}


hist.POSIXt <- function(x, breaks, ..., xlab = deparse1(substitute(x)),
                        plot = TRUE, freq = FALSE,
                        start.on.monday = TRUE, format, right = TRUE)
{
    if(!inherits(x, "POSIXt")) stop("wrong method")
    force(xlab)
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
        num.br <- is.numeric(breaks) && length(breaks) == 1L
        if(num.br) {
            ## specified number of breaks
        } else if(is.character(breaks) && length(breaks) == 1L) {
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
                    start$mday <- start$mday + if(start$wday > 0L) 1L else -6L
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
                if (right)
                    breaks[ind] <- breaks[ind] - 86400
		if (missing(format)) format <- "%Y-%m-%d"
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
                if (right)
                    breaks[ind] <- breaks[ind] - 86400
		if (missing(format)) format <- "%Y-%m-%d"
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
                if (right)
                    breaks[ind] <- breaks[ind] - 86400
		if (missing(format)) format <- "%Y-%m-%d"
           } else { # anything from "secs" to "weeks"
                maxx <- max(x, na.rm = TRUE)
                breaks <- seq(start, maxx + incr, breaks)
                if (length(breaks) > 2L)
                breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
            }
        }
        else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE,
                        warn.unused = FALSE, right = right, ...)
    res$equidist <- TRUE # years are of uneven lengths
    res$xname <- xlab
    if(plot) {
        ## swallow '...' args only for hist.default() above & separate out 'axes':
        myplot <- function(res, xlab, freq, format, breaks,
                           include.lowest, fuzz, # <<- swallowed here
                           density = NULL, angle = 45, col = "lightgray",
                           border = NULL, lty = NULL,
                           labels = FALSE,
                           axes = TRUE, xaxt = par("xaxt"), ...)
        {
	    plot(res, xlab = xlab, axes = FALSE, freq = freq,
                 density = density, angle = angle, col = col,
                 border = border, lty = lty,
		 labels = labels, ...)
	    if(axes) {
		axis(2, ...)
		if(xaxt != "n") {
		    if(num.br)
                        breaks <- as.POSIXct(res$breaks,
                                             origin = "1970-01-01")
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
    has.at <- !missing(at) && !is.null(at)
    
    range <- sort(par("usr")[if(side %% 2) 1L:2L else 3:4L])
    range[1L] <- ceiling(range[1L])
    range[2L] <- floor(range[2L])
    rangeDate <- range
    class(rangeDate) <- "Date"

    if(has.at){
        # convert at to Date:
        if(is.numeric(at))
            class(at) <- "Date"
        else 
            at <- as.Date(at)
        z <- at[is.finite(at)]

        # find format if missing:
        if(missing(format)){
#            format <- if(!missing(x)) attr(grDevices:::prettyDate(x), "format") else attr(grDevices:::prettyDate(rangeDate), "format")
#        }else if(is.null(format)){ # exdend format if needed for proper representation of at
            format <- if(!missing(x)) extendDateTimeFormat(x, z) else extendDateTimeFormat(rangeDate, z)
            }
    } else {
        z <- grDevices:::prettyDate(rangeDate, n = par("lab")[2 - side %% 2])
        if(missing(format)) format <- attr(z, "format")
    }

    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    if (!is.logical(labels)) 
        labels <- labels[keep]
    else if (isTRUE(labels))
        labels <- format(z, format = format)
    else if (isFALSE(labels))
        labels <- rep("", length(z)) # suppress labelling of ticks

    axis(side, at = z, labels = labels, ...)
}


hist.Date <- function(x, breaks, ..., xlab = deparse1(substitute(x)),
                      plot = TRUE, freq = FALSE,
                      start.on.monday = TRUE, format, right = TRUE)
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
                    start$mday <- start$mday + if(start$wday > 0L) 1L else -6L
                incr <- 7
                ## drops through to "days".
            }
            if(valid == 3L) { ## "months"
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (31 * 86400))
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- as.Date(seq(start, end, "months"))
                if (right)
                    breaks <- breaks - 1
		if (missing(format)) format <- "%Y-%m-%d"
            } else if(valid == 4L) { ## "years"
                start$mon <- 0L
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (366 * 86400))
                end$mon <- 0L
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- as.Date(seq(start, end, "years"))
                if (right)
                    breaks <- breaks - 1
		if (missing(format)) format <- "%Y-%m-%d"
            } else if(valid == 5L) { ## "quarters"
                qtr <- rep(c(0L, 3L, 6L, 9L), each = 3L)
                start$mon <- qtr[start$mon + 1L]
                start$mday <- 1L
                end <- as.POSIXlt(max(x, na.rm = TRUE))
                end <- as.POSIXlt(end + (93 * 86400))
                end$mon <- qtr[end$mon + 1L]
                end$mday <- 1L
                end$isdst <- -1L
                breaks <- as.Date(seq(start, end, "3 months"))
                if (right)
                    breaks <- breaks - 1
		if (missing(format)) format <- "%Y-%m-%d"
            } else { ## "days" (or "weeks")
                start <- as.Date(start)
                maxx <- max(x, na.rm = TRUE)
                breaks <- seq(start, maxx + incr, breaks)
                if (length(breaks) > 2L)
                breaks <- breaks[seq_len(1L + max(which(breaks < maxx)))]
            }
        } else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE,
                        warn.unused = FALSE, right = right, ...)
    res$equidist <- TRUE # years are of uneven lengths
    res$xname <- xlab
    if(plot) {
        ## swallow '...' args only for hist.default() above & separate out 'axes':
        myplot <- function(res, xlab, freq, format, breaks,
                           include.lowest, fuzz, # <<- swallowed here
                           density = NULL, angle = 45, col = "lightgray",
                           border = NULL, lty = NULL,
                           labels = FALSE,
                           axes = TRUE, xaxt = par("xaxt"), ...)
        {
            plot(res, xlab = xlab, axes = FALSE, freq = freq,
                 density = density, angle = angle, col = col,
                 border = border, lty = lty,
                 labels = labels, ...)
	    if(axes) {
                axis(2, ...)
              if(xaxt != "n") {
                if(num.br)
                    breaks <- as.Date(res$breaks,
                                      origin = "1970-01-01")
                axis.Date(1, at = breaks,  format = format, ...)
              }
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

