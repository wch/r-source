axis.POSIXct <- function(side, x, at, format, labels = TRUE, ...)
{
    mat <- missing(at) || is.null(at)
    if(!mat) x <- as.POSIXct(at) else x <- as.POSIXct(x)
    range <- par("usr")[if(side %%2) 1:2 else 3:4]
    ## find out the scale involved
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    if(d < 1.1*60) { # seconds
        sc <- 1
        if(missing(format)) format <- "%S"
    } else if (d < 1.1*60*60) { # minutes
        sc <- 60
        if(missing(format)) format <- "%M:%S"
    } else if (d < 1.1*60*60*24) {# hours
        sc <- 60*24
        if(missing(format)) format <- "%H:%M"
    } else if (d < 2*60*60*24) {
        sc <- 60*24
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
        class(z) <- c("POSIXt", "POSIXct")
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*60*60*24*365) { # months
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$isdst <- zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1], m)
        zz$year <- c(m, m+1)
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%b"
    } else { # years
        class(z) <- c("POSIXt", "POSIXct")
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$isdst <- zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        z <- as.POSIXct(zz)
        if(missing(format)) format <- "%Y"
    }
    if(!mat) z <- x[is.finite(x)] # override changes
    z <- z[z >= range[1] & z <= range[2]]
    if (identical(labels, TRUE))
	labels <- format(z, format = format)
    else if (identical(labels, FALSE))
	labels <- rep("", length(z)) # suppress labelling of ticks
    axis(side, at = z, labels = labels, ...)
}

plot.POSIXct <- function(x, y, xlab = "", ...)
{
    ## trick to remove arguments intended for title() or plot.default()
    axisInt <- function(x, main, sub, xlab, ylab, col, lty, lwd,
                        xlim, ylim, bg, pch, log, asp, axes, frame.plot, ...)
        axis.POSIXct(1, x, ...)

    dots <- list(...)
    Call <- match.call()
    Call[[1]] <- as.name("plot.default")
    Call$xaxt <- "n"
    Call$xlab <- xlab
    eval.parent(Call)
    axes <- if("axes" %in% names(dots)) dots$axes else TRUE
    xaxt <- if("xaxt" %in% names(dots)) dots$xaxt else par("xaxt")
    if(axes && xaxt != "n") axisInt(x, ...)
}

plot.POSIXlt <- function(x, y, xlab = "", ...)
{
    ## trick to remove arguments intended for title() or plot.default()
    axisInt <- function(x, main, sub, xlab, ylab, col, lty, lwd,
                        xlim, ylim, bg, pch, log, asp, axes, frame.plot, ...)
        axis.POSIXct(1, x, ...)
    dots <- list(...)
    Call <- match.call()
    Call[[1]] <- as.name("plot.default")
    Call$x <- as.POSIXct(x);
    Call$xaxt <- "n"
    Call$xlab <- xlab
    eval.parent(Call)
    axes <- if("axes" %in% names(dots)) dots$axes else TRUE
    xaxt <- if("xaxt" %in% names(dots)) dots$xaxt else par("xaxt")
    if(axes && xaxt != "n") axisInt(x, ...)
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
                         "months", "years"))
            if(is.na(valid)) stop("invalid specification of 'breaks'")
            start <- as.POSIXlt(min(x, na.rm = TRUE))
            incr <- 1
            if(valid > 1) { start$sec <- 0; incr <- 59.99 }
            if(valid > 2) { start$min <- 0; incr <- 3600 - 1 }
            if(valid > 3) { start$hour <- 0; incr <- 86400 - 1 }
            if(valid > 4) { start$isdst <- -1}
            if(valid == 5) {
                start$mday <- start$mday - start$wday
                if(start.on.monday)
                    start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
                incr <- 7*86400
            }
            if(valid == 6) { start$mday <- 1; incr <- 31*86400 }
            if(valid == 7) { start$mon <- 0; incr <- 366*86400 }
            maxx <- max(x, na.rm = TRUE)
            breaks <- seq(start, maxx + incr, breaks)
            breaks <- breaks[1:(1+max(which(breaks < maxx)))]
        }
        else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE, ...)
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
                if(num.br) breaks <- c.POSIXct(res$breaks)
                axis.POSIXct(1, at = breaks,  format = format, ...)
                                        # '...' : e.g. cex.axis
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
    range <- par("usr")[if(side %%2) 1:2 else 3:4]
    range[1] <- ceiling(range[1])
    range[2] <- floor(range[2])
    ## find out the scale involved
    d <- range[2] - range[1]
    z <- c(range, x[is.finite(x)])
    class(z) <- "Date"
    if (d < 7) # days of a week
        if(missing(format)) format <- "%a"
    if(d < 100) { # month and day
        z <- structure(pretty(z), class="Date")
        if(missing(format)) format <- "%b %d"
    } else if(d < 1.1*365) { # months
        zz <- as.POSIXlt(z)
        zz$mday <- 1;
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1], m)
        zz$year <- c(m, m+1)
        z <- .Internal(POSIXlt2Date(zz))
        if(missing(format)) format <- "%b"
    } else { # years
        zz <- as.POSIXlt(z)
        zz$mday <- 1; zz$mon <- 0
        zz$year <- pretty(zz$year)
        z <- .Internal(POSIXlt2Date(zz))
        if(missing(format)) format <- "%Y"
    }
    if(!mat) z <- x[is.finite(x)] # override changes
    z <- z[z >= range[1] & z <= range[2]]
    z <- sort(unique(z))
    if (identical(labels, TRUE))
	labels <- format.Date(z, format = format)
    else if (identical(labels, FALSE))
	labels <- rep("", length(z)) # suppress labelling of ticks
    axis(side, at = z, labels = labels, ...)
}

plot.Date <- function(x, y, xlab = "", ...)
{
    ## trick to remove arguments intended for title() or plot.default()
    axisInt <- function(x, main, sub, xlab, ylab, col, lty, lwd,
                        xlim, ylim, bg, pch, log, asp, axes, frame.plot, ...)
        axis.Date(1, x, ...)

    dots <- list(...)
    Call <- match.call()
    Call[[1]] <- as.name("plot.default")
    Call$xaxt <- "n"
    Call$xlab <- xlab
    eval.parent(Call)
    axes <- if("axes" %in% names(dots)) dots$axes else TRUE
    xaxt <- if("xaxt" %in% names(dots)) dots$xaxt else par("xaxt")
    if(axes && xaxt != "n") axisInt(x, ...)
}

hist.Date <- function(x, breaks, ..., xlab = deparse(substitute(x)),
                        plot = TRUE, freq = FALSE,
                        start.on.monday = TRUE, format)
{
    if(!inherits(x, "Date")) stop("wrong method")
    xlab
    x <- as.Date(x)
    incr <- 1
    ## handle breaks ourselves
    if (inherits(breaks, "Date")) {
        breaks <- as.Date(breaks)
        d <- min(abs(diff(unclass(breaks))))
        if(d > 1) incr <- 1
        if(d > 7) incr <- 7
        if(d > 28) incr <- 28
        if(d > 366) incr <- 366
        num.br <- FALSE
    } else {
        num.br <- is.numeric(breaks) && length(breaks) == 1
        if(num.br) {
        ## specified number of breaks
        } else if(is.character(breaks) && length(breaks) == 1) {
            valid <- pmatch(breaks, c("days", "weeks", "months", "years"))
            if(is.na(valid)) stop("invalid specification of 'breaks'")
            start <- as.POSIXlt(min(x, na.rm = TRUE))
            incr <- 1
            if(valid > 1) { start$isdst <- -1}
            if(valid == 2) {
                start$mday <- start$mday - start$wday
                if(start.on.monday)
                    start$mday <- start$mday + ifelse(start$wday > 0, 1, -6)
                incr <- 7
            }
            if(valid == 3) { start$mday <- 1; incr <- 31 }
            if(valid == 4) { start$mon <- 0; incr <- 366 }
            start <- .Internal(POSIXlt2Date(start))
            maxx <- max(x, na.rm = TRUE)
            breaks <- seq(start, maxx + incr, breaks)
            breaks <- breaks[1:(1+max(which(breaks < maxx)))]
        } else stop("invalid specification of 'breaks'")
    }
    res <- hist.default(unclass(x), unclass(breaks), plot = FALSE, ...)
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

Axis.POSIXct <- function(x=NULL, at=NULL, ..., side, labels=TRUE)
    axis.POSIXct(side=side, x=x, at=at, labels=labels, ...)

Axis.POSIXlt <- function(x=NULL, at=NULL, ..., side, labels=TRUE)
{
    if(inherits(x, "POSIXlt")) x <- as.POSIXct(x)
    if(inherits(at, "POSIXlt")) at <- as.POSIXct(at)
    axis.POSIXct(side=side, x=x, at=at, labels=labels, ...)
}
