acf <-
function (x, lag.max = NULL, plot = TRUE, type = c("correlation",
    "covariance", "partial"), na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(type == "partial") {
        m <- match.call()
        m[[1]] <- as.name("pacf")
        return(eval(m, sys.frame(sys.parent())))
    }
    series <- deparse(substitute(x))
    x.freq <- frequency(as.ts(x))
    x <- na.action(x)
    x <- as.matrix(x)
    if(any(is.na(x))) stop("NAs in x")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    x <- sweep(x, 2, apply(x, 2, mean))
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- array(NA, c(lag.max + 1, nser, nser))
    xp <- rbind(x, matrix(0, ncol = nser,
                          nrow = nextn(sampleT + lag.max) - sampleT))
    for (i in 1:nser) for (j in 1:nser) {
        acf[, i, j] <- convolve(xp[, i], xp[, j])[1:(lag.max + 1)]/sampleT
    }
    if (type == "correlation") {
        if(nser > 1) {
            var0 <- diag(acf[1, , ])
            acf0 <- sqrt(var0 %*% t(var0))
            acf <- sweep(acf, c(2, 3), acf0, "/")
        } else acf <- acf/acf[1, , ]
    }
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

pacf.ts <- function(x, lag.max = NULL, plot = TRUE, na.action = na.fail, ...)
{
    series <- deparse(substitute(x))
    x.freq <- frequency(as.ts(x))
    x <- na.action(x)
    if(any(is.na(x))) stop("NAs in x")
    if(is.matrix(x))
        if(ncol(x) > 1) stop("univariate ts method")
        else x <- drop(x)
    sampleT <- length(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT)))
    lag.max <- min(lag.max, sampleT - 1)
    x <- scale(x, T, F)
    acf <- drop(acf(x, lag.max = lag.max, plot=F)$acf)
    pacf <- array(.C("uni_pacf",
               as.double(acf),
               pacf = double(lag.max),
               as.integer(sampleT),
               as.integer(lag.max))$pacf, dim=c(lag.max,1,1))
    acf.out <- structure(.Data = list(acf = pacf, type = "partial",
                         n.used = sampleT,
                         lag = array((1:lag.max)/x.freq, dim=c(lag.max,1,1)),
                         series = series, snames = NULL),
                         class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

pacf.mts <- function(x, lag.max = NULL, plot = TRUE, na.action = na.fail, ...)
{
    series <- deparse(substitute(x))
    x.freq <- frequency(as.ts(x))
    x <- na.action(x)
    x <- as.matrix(x)
    if(any(is.na(x))) stop("NAs in x")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    x <- sweep(x, 2, apply(x, 2, mean))
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- ar.yw(x, order.max = lag.max)$partialacf
    lag <- outer(1:lag.max, lag/x.freq)
    acf.out <- structure(.Data = list(acf = acf, type = "partial",
                         n.used = sampleT, lag = lag, series = series, snames = colnames(x)),
                         class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

plot.acf <-
    function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
              ylim = NULL, main = NULL, ci.col="red", ...)
{
    opar <- NULL
    on.exit(par(opar))
    nser <- ncol(x$lag)
    opar <- c(opar, par(mfrow = rep(min(nser, 5), 2)))
    if (is.null(ylab))
        ylab <- switch(x$type, correlation = "ACF", covariance = "ACF",
            partial = "Partial ACF")
    if (is.null(snames <- x$snames)) {
        snames <- if (nser == 1)
            paste("Series", x$series)
        else paste("Series", 1:nser)
    }
    with.ci <- (ci > 0) && (x$type != "covariance")
    for (i in 1:nser) for (j in 1:nser) {
        clim <- if (with.ci)
            qnorm((1 + ci)/2)/sqrt(x$n.used)
        else c(0, 0)
        if (is.null(ylim)) {
            ymin <- min(c(x$acf[, i, j], -clim))
            ymax <- max(c(x$acf[, i, j], clim))
            ylim <- c(ymin, ymax)
        }
        plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
            ylab = if(j==1) ylab else "", ylim = ylim, ...)
        if (with.ci)
            abline(h = c(clim, -clim), col = ci.col, lty = 2)
        abline(h = 0)
        if (!is.null(main))
            title(main)
        else if (i == j)
            title(snames[i])
        else title(paste(snames[i], "&", snames[j]))
    }
}
