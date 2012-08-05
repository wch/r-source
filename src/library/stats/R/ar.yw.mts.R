#  File src/library/stats/R/ar.yw.mts.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1999-2012 The R Core Team
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

ar.yw.mts <-
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
        x <- sweep(x, 2, x.mean, check.margin=FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- if (is.null(order.max))
        floor(10 * log10(n.used))
    else floor(order.max)
    if (order.max < 1)
        stop("'order.max' must be >= 1")
    xacf <- acf(x, type = "cov", plot = FALSE, lag.max = order.max)$acf
    z <- .C(C_multi_yw,
            aperm(xacf, c(3, 2, 1)),
            as.integer(n.used),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1 + order.max) * nser * nser),
            pacf = double((1 + order.max) * nser * nser),
            var = double((1 + order.max) * nser * nser),
            aic = double(1 + order.max),
            order = integer(1L),
            as.integer(aic))
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
        for (i in 1L:order) resid[-(1L:order), ] <- resid[-(1L:order),
            ] - x[(order - i + 1):(n.used - i), ] %*% t(ar[i,
            , ])
        resid[1L:order, ] <- NA
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
    dimnames(ar) <- list(seq_len(order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(1L:order.max, snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = "Yule-Walker",
        series = series, frequency = xfreq, call = match.call())
    class(res) <- "ar"
    return(res)
}
