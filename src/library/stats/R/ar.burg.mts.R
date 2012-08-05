#  File src/library/stats/R/ar.burg.mts.R
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

ar.burg.mts <-
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
        x <- sweep(x, 2L, x.mean, check.margin=FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- if (is.null(order.max))
        floor(10 * log10(n.used))
    else floor(order.max)
    xaic <- numeric(order.max + 1L)
    z <- .C(C_multi_burg,
            as.integer(n.used),
            resid = as.double(x),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1L + order.max) * nser * nser),
            pacf = double((1L + order.max) * nser * nser),
            var = double((1L + order.max) * nser * nser),
            aic = double(1L + order.max),
            order = integer(1L),
            as.integer(aic),
            as.integer(var.method))
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max +
        1L)), 3:1)[-1L, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max + 1L)), 3:1)
    xaic <- z$aic - min(z$aic)
    names(xaic) <- 0:order.max
    order <- z$order
    ar <- if (order)
        -aperm(array(z$coefs, dim = c(nser, nser, order.max + 1L)),
               3:1)[2L:(order + 1L), , , drop = FALSE]
    else array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1L, , , drop = TRUE]
    resid <- matrix(z$resid, nrow = n.used, ncol = nser)
    if (order) resid[seq_len(order), ] <- NA
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "mts"
    }
    snames <- colnames(x)
    colnames(resid) <- snames
    dimnames(ar) <- list(seq_len(order), snames, snames)
    dimnames(var.pred) <- list(snames, snames)
    dimnames(partialacf) <- list(seq_len(order.max), snames, snames)
    res <- list(order = order, ar = ar, var.pred = var.pred,
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max,
        partialacf = partialacf, resid = resid, method = ifelse(var.method ==
            1, "Burg", "Burg2"), series = series, frequency = xfreq,
        call = match.call())
    class(res) <- "ar"
    return(res)
}
