"ar.burg.mts" <-
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
    xaic <- numeric(order.max + 1)
    z <- .C("multi_burg",
            as.integer(n.used),
            resid = as.double(x),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1 + order.max) * nser * nser),
            pacf = double((1 + order.max) * nser * nser),
            var = double((1 + order.max) * nser * nser),
            aic = double(1 + order.max),
            order = integer(1),
            as.integer(aic),
            as.integer(var.method),
            PACKAGE = "stats")
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))[-1, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max +
        1)), c(3, 2, 1))
    xaic <- z$aic - min(z$aic)
    names(xaic) <- 0:order.max
    order <- z$order
    ar <- if (order > 0)
        -aperm(array(z$coefs, dim = c(nser, nser, order.max + 1)),
               c(3, 2, 1))[2:(order + 1), , , drop = FALSE]
    else array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1, , , drop = TRUE]
    resid <- matrix(z$resid, nrow = n.used, ncol = nser)
    if (order > 0)
        resid[1:order, ] <- NA
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "mts"
    }
    snames <- colnames(x)
    colnames(resid) <- snames
    dimnames(ar) <- list(seq(length=order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(1:order.max, snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = ifelse(var.method ==
            1, "Burg", "Burg2"), series = series, frequency = xfreq,
        call = match.call())
    class(res) <- "ar"
    return(res)
}
