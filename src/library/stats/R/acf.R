acf <-
    function (x, lag.max = NULL,
              type = c("correlation", "covariance", "partial"),
              plot = TRUE, na.action = na.fail, demean= TRUE, ...)
{
    type <- match.arg(type)
    if(type == "partial") {
        m <- match.call()
        m[[1]] <- as.name("pacf")
        m$type <- NULL
        return(eval(m, parent.frame()))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("'lag.max' must be at least 1")
    if(demean) x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- array(.C("acf",
                    as.double(x), as.integer(sampleT), as.integer(nser),
                    as.integer(lag.max), as.integer(type=="correlation"),
                    acf=double((lag.max+1) * nser * nser), NAOK = TRUE,
                    PACKAGE = "stats")$acf, c(lag.max + 1, nser, nser))
    lag <- outer(0:lag.max, lag/x.freq)
    acf.out <- structure(.Data = list(acf = acf, type = type,
        n.used = sampleT, lag = lag, series = series, snames = colnames(x)),
        class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
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
        if(any(is.na(x))) stop("NAs in 'x'")
        nser <- ncol(x)
        x <- sweep(x, 2, colMeans(x))
        lag <- matrix(1, nser, nser)
        lag[lower.tri(lag)] <- -1
        pacf <- ar.yw(x, order.max = lag.max)$partialacf
        lag <- outer(1:lag.max, lag/x.freq)
        snames <- colnames(x)
    } else {
        x <- scale(x, TRUE, FALSE)
        acf <- drop(acf(x, lag.max = lag.max, plot = FALSE,
                        na.action = na.action)$acf)
        pacf <- array(.C("uni_pacf",
                         as.double(acf),
                         pacf = double(lag.max),
                         as.integer(lag.max), PACKAGE="stats")$pacf,
                      dim=c(lag.max,1,1))
        lag <- array((1:lag.max)/x.freq, dim=c(lag.max,1,1))
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
    if((nser <- ncol(x$lag)) < 1) stop("x$lag must have at least 1 column")
    if (is.null(ylab))
        ylab <- switch(x$type,
                       correlation = "ACF",
                       covariance = "ACF (cov)",
                       partial = "Partial ACF")
    if (is.null(snames <- x$snames))
        snames <- paste("Series ", if (nser == 1) x$series else 1:nser)

    with.ci <- ci > 0 && x$type != "covariance"
    with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
    if(with.ci.ma && x$lag[1,1,1] != 0) {
        warning("can use ci.type=\"ma\" only if first lag is 0")
        with.ci.ma <- FALSE
    }
    clim0 <- if (with.ci) qnorm((1 + ci)/2)/sqrt(x$n.used) else c(0, 0)

    Npgs <- 1 ## we will do [ Npgs x Npgs ] pages !
    nr <- nser
    if(nser > 1) { ## at most m x m (m := max.mfrow)  panels per page
        sn.abbr <- if(nser > 2) abbreviate(snames) else snames

        if(nser > max.mfrow) {
            ##  We need more than one page: The plots are laid out
            ##  such that we can manually paste the paper pages and get a
            ##  nice square layout with diagonal !
            ## NB: The same applies to pairs() where we'd want several pages
            Npgs <- ceiling(nser / max.mfrow)
            nr <- ceiling(nser / Npgs)  # <= max.mfrow
        }
        opar <- par(mfrow = rep(nr, 2), mar = mar, oma = oma, mgp = mgp,
                    ask = ask, xpd = xpd, cex.main = cex.main)
        on.exit(par(opar))
        if(verbose) {
            cat("par(*) : ")
            str(par("mfrow","cex", "cex.main","cex.axis","cex.lab","cex.sub"))
        }
    }

    for (I in 1:Npgs) for (J in 1:Npgs) {
        ## Page [ I , J ] : Now do   nr x nr  'panels' on this page
        iind <- (I-1)*nr + 1:nr
        jind <- (J-1)*nr + 1:nr
        if(verbose)
            cat("Page [",I,",",J,"]: i =",
                paste(iind,collapse=","),"; j =",
                paste(jind,collapse=","),"\n")
        for (i in iind) for (j in jind)
            if(max(i,j) > nser) {
                frame(); box(col = "light gray")
                ## the above is EXTREMELY UGLY; should have a version
                ## of frame() that really does advance a frame !!
            }
            else {
                clim <- if (with.ci.ma && i == j)
                    clim0 * sqrt(cumsum(c(1, 2*x$acf[-1, i, j]^2))) else clim0
                if (is.null(ylim)) {
                    ymin <- min(c(x$acf[, i, j], -clim), na.rm = TRUE)
                    ymax <- max(c(x$acf[, i, j], clim), na.rm = TRUE)
                    ylim <- c(ymin, ymax)
                }
                plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
                     ylab = if(j==1) ylab else "", ylim = ylim, ...)
                abline(h = 0)
                if (with.ci && ci.type == "white")
                    abline(h = c(clim, -clim), col = ci.col, lty = 2)
                else if (with.ci.ma && i == j) {
                    lines(x$lag[, i, j], clim, col = ci.col, lty = 2)
                    lines(x$lag[, i, j], -clim, col = ci.col, lty = 2)
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
    }
}

ccf <- function(x, y, lag.max = NULL,
                type = c("correlation", "covariance"),
                plot = TRUE, na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(is.matrix(x) || is.matrix(y))
        stop("univariate time series only")
    X <- na.action(ts.union(as.ts(x), as.ts(y)))
    colnames(X) <- c(deparse(substitute(x)), deparse(substitute(y)))
    acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type)
    lag <- c(rev(acf.out$lag[-1,2,1]), acf.out$lag[,1,2])
    y   <- c(rev(acf.out$acf[-1,2,1]), acf.out$acf[,1,2])
    acf.out$acf <- array(y, dim=c(length(y),1,1))
    acf.out$lag <- array(lag, dim=c(length(y),1,1))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

"[.acf" <- function(x, i, j=1:nser)
{
    nser <- ncol(x$lag)
    ii <- match(i, x$lag[,1,1], nomatch=NA)
    x$acf <- x$acf[ii,j,j, drop=FALSE]
    x$lag <- x$lag[ii,j,j, drop=FALSE]
    x
}

print.acf <- function(x, digits=3, ...)
{
    type <- match(x$type, c("correlation", "covariance", "partial"))
    msg <- c("Autocorrelations", "Autocovariances", "Partial autocorrelations")
    cat("\n", msg[type]," of series ", sQuote(x$series), ", by lag\n\n",
        sep="")
    nser <- ncol(x$lag)
    if(type != 2) x$acf <- round(x$acf, digits)
    if(nser == 1) {
        acfs <- drop(x$acf)
        names(acfs) <- format(drop(x$lag), digits=3)
        print(acfs, digits = digits, ...)
    } else {
        acfs <- format(x$acf, ...)
        lags <- format(x$lag, digits=3)
        acfs <- array(paste(acfs, " (", lags, ")", sep=""), dim=dim(x$acf))
        dimnames(acfs)  <- list(rep("", nrow(x$lag)), x$snames, x$snames)
        print(acfs, quote=FALSE, ...)
    }
    invisible(x)
}
