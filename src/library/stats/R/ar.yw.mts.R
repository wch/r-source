"ar.yw.mts" <-
function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
    demean = TRUE, series = NULL, var.method = 1, ...)
{
    if (is.null(series))
        series <- deparse(substitute(x))
    if (ists <- is.ts(x))
        xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if (any(is.na(x)))
        stop("NAs in 'x'")
    if (ists)
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    nser <- ncol(x)
    n.used <- nrow(x)
    if (demean) {
        x.mean <- colMeans(x)
        x <- sweep(x, 2, x.mean)
    }
    else x.mean <- rep(0, nser)
    order.max <- if (is.null(order.max))
        floor(10 * log10(n.used))
    else floor(order.max)
    if (order.max < 1)
        stop("'order.max' must be >= 1")
    xacf <- acf(x, type = "cov", plot = FALSE, lag.max = order.max)$acf
    z <- .C("multi_yw",
            aperm(xacf, c(3, 2, 1)),
            as.integer(n.used),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1 + order.max) * nser * nser),
            pacf = double((1 + order.max) * nser * nser),
            var = double((1 + order.max) * nser * nser),
            aic = double(1 + order.max),
            order = integer(1),
            as.integer(aic),
####        as.integer(var.method),
            PACKAGE = "stats")
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))[-1, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))
    xaic <- z$aic - min(z$aic)
    names(xaic) <- 0:order.max
    order <- z$order
    resid <- x
    if (order > 0) {
        ar <- -aperm(array(z$coefs, dim = c(nser, nser, order.max +
            1)), c(3, 2, 1))[2:(order + 1), , , drop = FALSE]
        for (i in 1:order) resid[-(1:order), ] <- resid[-(1:order),
            ] - x[(order - i + 1):(n.used - i), ] %*% t(ar[i,
            , ])
        resid[1:order, ] <- NA
    }
    else ar <- array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1, , , drop = TRUE] * n.used/(n.used -
        nser * (demean + order))
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- c("mts", "ts")
    }
    snames <- colnames(x)
    colnames(resid) <- snames
    dimnames(ar) <- list(seq(length=order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(1:order.max, snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = "Yule-Walker",
        series = series, frequency = xfreq, call = match.call())
    class(res) <- "ar"
    return(res)
}
