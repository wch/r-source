acf <-
    function (x, lag.max = NULL,
              type = c("correlation", "covariance", "partial"),
              plot = TRUE, na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(type == "partial") {
        m <- match.call()
        m[[1]] <- as.name("pacf")
        m$type <- NULL
        return(eval(m, sys.frame(sys.parent())))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if(any(is.na(x))) stop("NAs in x")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("lag.max must be at least 1")
    x <- sweep(x, 2, apply(x, 2, mean))
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- array(NA, c(lag.max + 1, nser, nser))
#    xp <- rbind(x, matrix(0, ncol = nser,
#                          nrow = nextn(sampleT + lag.max) - sampleT))
#    for (i in 1:nser) for (j in 1:nser) {
#        acf[, i, j] <- convolve(xp[, i], xp[, j])[1:(lag.max + 1)]/sampleT
#    }
#     if (type == "correlation") {
#         if(nser > 1) {
#             var0 <- diag(acf[1, , ])
#             acf0 <- sqrt(var0 %*% t(var0))
#             acf <- sweep(acf, c(2, 3), acf0, "/")
#         } else acf <- acf/acf[1, , ]
#     }
    acf <- array(.C("acf",
                    as.double(x), as.integer(sampleT), as.integer(nser),
                    as.integer(lag.max), as.integer(type=="correlation"),
                    acf=double((lag.max+1) * nser * nser), PACKAGE="ts"
                    )$acf, c(lag.max + 1, nser, nser))
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
    if(is.matrix(x)) {
        m <- match.call()
        m[[1]] <- as.name("pacf.mts")
        return(eval(m, sys.frame(sys.parent())))
    }
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    if(any(is.na(x))) stop("NAs in x")
    if(is.matrix(x))
        if(ncol(x) > 1) stop("univariate ts method")
        else x <- drop(x)
    sampleT <- length(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("lag.max must be at least 1")
    x <- scale(x, T, F)
    acf <- drop(acf(x, lag.max = lag.max, plot=F)$acf)
    pacf <- array(.C("uni_pacf",
               as.double(acf),
               pacf = double(lag.max),
               as.integer(lag.max), PACKAGE="ts")$pacf, dim=c(lag.max,1,1))
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
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if(any(is.na(x))) stop("NAs in x")
    sampleT <- nrow(x)
    nser <- ncol(x)
    if (is.null(lag.max))
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- min(lag.max, sampleT - 1)
    if (lag.max < 1) stop("lag.max must be at least 1")
    x <- sweep(x, 2, apply(x, 2, mean))
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- ar.yw(x, order.max = lag.max)$partialacf
    lag <- outer(1:lag.max, lag/x.freq)
    acf.out <- structure(.Data = list(acf = acf, type = "partial",
                         n.used = sampleT, lag = lag, series = series,
                         snames = colnames(x)),
                         class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}

plot.acf <-
    function (x, ci = 0.95, type = "h", xlab = "Lag", ylab = NULL,
              ylim = NULL, main = NULL, ci.col="blue",
              ci.type=c("white", "ma"), ...)
{
    ci.type <- match.arg(ci.type)
    nser <- ncol(x$lag)
    if(nser > 1) {
        opar <- par(mfrow = rep(min(nser, 5), 2))
        on.exit(par(opar))
    }
    if (is.null(ylab))
        ylab <- switch(x$type, correlation = "ACF", covariance = "ACF",
            partial = "Partial ACF")
    if (is.null(snames <- x$snames)) {
        snames <- if (nser == 1)
            paste("Series ", x$series)
        else paste("Series ", 1:nser)
    }
    with.ci <- (ci > 0) && (x$type != "covariance")
    with.ci.ma <- with.ci && ci.type == "ma" && x$type == "correlation"
    for (i in 1:nser) for (j in 1:nser) {
        clim <- c(0, 0)
        if (with.ci)
            clim <- qnorm((1 + ci)/2)/sqrt(x$n.used)
        if (with.ci.ma && i == j)
            clim <- clim * sqrt(cumsum(c(1, 2*x$acf[-1, i, j]^2)))
        if (is.null(ylim)) {
            ymin <- min(c(x$acf[, i, j], -clim))
            ymax <- max(c(x$acf[, i, j], clim))
            ylim <- c(ymin, ymax)
        }
        plot(x$lag[, i, j], x$acf[, i, j], type = type, xlab = xlab,
            ylab = if(j==1) ylab else "", ylim = ylim, ...)
        abline(h = 0)
        if (with.ci && ci.type == "white")
            abline(h = c(clim, -clim), col = ci.col, lty = 2)
        if (with.ci.ma && i == j) {
            lines(x$lag[, i, j], clim, col = ci.col, lty = 2)
            lines(x$lag[, i, j], -clim, col = ci.col, lty = 2)
        }
        if (!is.null(main))
            title(main)
        else if (i == j)
            title(snames[i])
        else title(paste(snames[i], "&", snames[j]))
    }
}

ccf <- function(x, y, lag.max = NULL,
                type = c("correlation", "covariance"),
                plot = TRUE, na.action = na.fail, ...)
{
    type <- match.arg(type)
    if(is.matrix(x) || is.matrix(y))
        stop("univariate time series only")
    X <- na.action(ts.union(x, y))
    colnames(X) <- c(deparse(substitute(x)), deparse(substitute(y)))
    acf.out <- acf(X, lag.max = lag.max, plot = F, type = type)
    lag <- c(rev(acf.out$lag[-1,2,1]), 0, acf.out$lag[,1,2])
    y <- c(rev(acf.out$acf[-1,2,1]), 0, acf.out$acf[,1,2])
    acf.out$acf <- array(y, dim=c(length(y),1,1))
    acf.out$lag <- array(lag, dim=c(length(y),1,1))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot.acf(acf.out, ...)
        return(invisible(acf.out))
    } else return(acf.out)
}
