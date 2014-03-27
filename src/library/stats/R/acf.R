#  File src/library/stats/R/acf.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-2013 The R Core Team
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

acf <-
    function (x, lag.max = NULL,
              type = c("correlation", "covariance", "partial"),
              plot = TRUE, na.action = na.fail, demean = TRUE, ...)
{
    type <- match.arg(type)
    if(type == "partial") {
        m <- match.call()
        m[[1L]] <- quote(stats::pacf)
        m$type <- NULL
        return(eval(m, parent.frame()))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    sampleT <- as.integer(nrow(x))
    nser <- as.integer(ncol(x))
    if(is.na(sampleT) || is.na(nser))
        stop("'sampleT' and 'nser' must be integer")
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- as.integer(min(lag.max, sampleT - 1L))
    if (is.na(lag.max) || lag.max < 0)
        stop("'lag.max' must be at least 0")
    if(demean)
	x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin=FALSE)
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- .Call(C_acf, x, lag.max, type == "correlation")
    lag <- outer(0:lag.max, lag/x.freq)
    acf.out <- structure(list(acf = acf, type = type, n.used = sampleT,
                              lag = lag, series = series, snames = colnames(x)),
                         class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        invisible(acf.out)
    } else acf.out
}

pacf <- function(x, lag.max, plot, na.action, ...) UseMethod("pacf")

pacf.default <- function(x, lag.max = NULL, plot = TRUE,
                         na.action = na.fail, ...)
{
    series <- deparse(substitute(x))
    x <- drop(na.action(as.ts(x)))  # use univariate code for a single series
    if(!is.numeric(x)) stop("'x' must be numeric")
    x.freq <- frequency(x)
    sampleT <- NROW(x)
    if (is.null(lag.max))
        lag.max <- if(is.matrix(x)) floor(10 * (log10(sampleT) - log10(ncol(x))))
        else floor(10 * (log10(sampleT)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("'lag.max' must be at least 1")

    if(is.matrix(x)) {
        if(anyNA(x)) stop("NAs in 'x'")
        nser <- ncol(x)
        x <- sweep(x, 2, colMeans(x), check.margin=FALSE)
        lag <- matrix(1, nser, nser)
        lag[lower.tri(lag)] <- -1
        pacf <- ar.yw(x, order.max = lag.max)$partialacf
        lag <- outer(1L:lag.max, lag/x.freq)
        snames <- colnames(x)
    } else {
        x <- scale(x, TRUE, FALSE)
        acf <- drop(acf(x, lag.max = lag.max, plot = FALSE,
                        na.action = na.action)$acf)
        pacf <- .Call(C_pacf1, acf, lag.max)
        lag <- array((1L:lag.max)/x.freq, dim=c(lag.max,1L,1L))
        snames <- NULL
    }

    acf.out <- structure(.Data = list(acf = pacf, type = "partial",
                         n.used = sampleT, lag = lag, series = series,
                         snames = snames), class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        invisible(acf.out)
    } else acf.out
}

plot.acf <-
    function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
              ylim = NULL, main = NULL, ci.col="blue",
              ci.type = c("white", "ma"),
              max.mfrow = 6,
              ask = Npgs > 1 && dev.interactive(),
              mar = if(nser > 2) c(3,2,2,0.8) else par("mar"),
              oma = if(nser > 2) c(1,1.2,1,1) else par("oma"),
              mgp = if(nser > 2) c(1.5,0.6,0) else par("mgp"),
              xpd = par("xpd"),
              cex.main = if(nser > 2) 1 else par("cex.main"),
              verbose = getOption("verbose"),
              ...)
{
    ci.type <- match.arg(ci.type)
    if((nser <- ncol(x$lag)) < 1L) stop("x$lag must have at least 1 column")
    if (is.null(ylab))
        ylab <- switch(x$type,
                       correlation = "ACF",
                       covariance = "ACF (cov)",
                       partial = "Partial ACF")
    if (is.null(snames <- x$snames))
        snames <- paste("Series ", if (nser == 1L) x$series else 1L:nser)

    with.ci <- ci > 0 && x$type != "covariance"
    with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
    if(with.ci.ma && x$lag[1L, 1L, 1L] != 0L) {
        warning("can use ci.type=\"ma\" only if first lag is 0")
        with.ci.ma <- FALSE
    }
    clim0 <- if (with.ci) qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

    Npgs <- 1L ## we will do [ Npgs x Npgs ] pages !
    nr <- nser
    if(nser > 1L) { ## at most m x m (m := max.mfrow)  panels per page
        sn.abbr <- if(nser > 2L) abbreviate(snames) else snames

        if(nser > max.mfrow) {
            ##  We need more than one page: The plots are laid out
            ##  such that we can manually paste the paper pages and get a
            ##  nice square layout with diagonal !
            ## NB: The same applies to pairs() where we'd want several pages
            Npgs <- ceiling(nser / max.mfrow)
            nr <- ceiling(nser / Npgs)  # <= max.mfrow
        }
        opar <- par(mfrow = rep(nr, 2L), mar = mar, oma = oma, mgp = mgp,
                    ask = ask, xpd = xpd, cex.main = cex.main)
        on.exit(par(opar))
        if(verbose) { # FIXME: message() can be suppressed but not str()
            message("par(*) : ", appendLF=FALSE, domain = NA)
            str(par("mfrow","cex", "cex.main","cex.axis","cex.lab","cex.sub"))
        }
    }

    if (is.null(ylim)) {
        ## Calculate a common scale
        ylim <- range(x$acf[, 1L:nser, 1L:nser], na.rm = TRUE)
        if (with.ci) ylim <- range(c(-clim0, clim0, ylim))
        if (with.ci.ma) {
	    for (i in 1L:nser) {
                clim <- clim0 * sqrt(cumsum(c(1, 2*x$acf[-1, i, i]^2)))
                ylim <- range(c(-clim, clim, ylim))
            }
        }
    }

    for (I in 1L:Npgs) for (J in 1L:Npgs) {
        dev.hold()
        ## Page [ I , J ] : Now do   nr x nr  'panels' on this page
        iind <- (I-1)*nr + 1L:nr
        jind <- (J-1)*nr + 1L:nr
        if(verbose)
            message(gettextf("Page [%d,%d]: i =%s; j =%s", I, J, paste(iind,collapse=","), paste(jind,collapse=",")), domain = NA)
        for (i in iind) for (j in jind)
            if(max(i,j) > nser) {
                frame(); box(col = "light gray")
                ## the above is EXTREMELY UGLY; should have a version
                ## of frame() that really does advance a frame !!
            }
            else {
                clim <- if (with.ci.ma && i == j)
                    clim0 * sqrt(cumsum(c(1, 2*x$acf[-1, i, j]^2))) else clim0
                plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
                     ylab = if(j==1) ylab else "", ylim = ylim, ...)
                abline(h = 0)
                if (with.ci && ci.type == "white")
                    abline(h = c(clim, -clim), col = ci.col, lty = 2)
                else if (with.ci.ma && i == j) {
                    clim <- clim[-length(clim)]
                    lines(x$lag[-1, i, j], clim, col = ci.col, lty = 2)
                    lines(x$lag[-1, i, j], -clim, col = ci.col, lty = 2)
                }
                title(if (!is.null(main)) main else
                      if (i == j) snames[i]
                      else paste(sn.abbr[i], "&", sn.abbr[j]),
                      line = if(nser > 2) 1 else 2)
            }
        if(Npgs > 1) {                  # label the page
            mtext(paste("[",I,",",J,"]"), side=1, line = -0.2, adj=1,
                  col = "dark gray", cex = 1, outer = TRUE)
        }
        dev.flush()
    }
    invisible()
}

ccf <- function(x, y, lag.max = NULL,
                type = c("correlation", "covariance"),
                plot = TRUE, na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(is.matrix(x) || is.matrix(y))
        stop("univariate time series only")
    X <- ts.intersect(as.ts(x), as.ts(y))
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type,
                   na.action = na.action)
    lag <- c(rev(acf.out$lag[-1,2,1]), acf.out$lag[,1,2])
    y   <- c(rev(acf.out$acf[-1,2,1]), acf.out$acf[,1,2])
    acf.out$acf <- array(y, dim=c(length(y),1L,1L))
    acf.out$lag <- array(lag, dim=c(length(y),1L,1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

`[.acf` <- function(x, i, j)
{
    if(missing(j)) j <- seq_len(ncol(x$lag))
    ii <- if(missing(i)) seq_len(nrow(x$lag))
    else match(i, x$lag[, 1, 1], nomatch = NA_integer_)
    x$acf <- x$acf[ii, j, j, drop = FALSE]
    x$lag <- x$lag[ii, j, j, drop = FALSE]
    x
}

print.acf <- function(x, digits = 3L, ...)
{
    type <- match(x$type, c("correlation", "covariance", "partial"))
    msg <- c("Autocorrelations", "Autocovariances", "Partial autocorrelations")
    cat("\n", msg[type]," of series ", sQuote(x$series), ", by lag\n\n",
        sep = "")
    nser <- ncol(x$lag)
    if(type != 2) x$acf <- round(x$acf, digits)
    if(nser == 1) {
        acfs <- setNames(drop(x$acf), format(drop(x$lag), digits = 3L))
        print(acfs, digits = digits, ...)
    } else {
        acfs <- format(x$acf, ...)
        lags <- format(x$lag, digits = 3L)
        acfs <- array(paste0(acfs, " (", lags, ")"), dim = dim(x$acf))
        dimnames(acfs)  <- list(rep("", nrow(x$lag)), x$snames, x$snames)
        print(acfs, quote = FALSE, ...)
    }
    invisible(x)
}
