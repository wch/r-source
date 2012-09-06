#  File src/library/stats/R/ar.burg.R
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

## ar.burg by B.D. Ripley based on R version by Martyn Plummer
ar.burg <- function(x, ...) UseMethod("ar.burg")
ar.burg.default <-
    function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                   demean = TRUE, series = NULL, var.method = 1, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    if (!is.null(dim(x)))
        stop("Burg's algorithm only implemented for univariate series")
    if (ists <- is.ts(x)) xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if(any(is.na(x))) stop("NAs in 'x'")
    if (ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.vector(x) # drop attributes including class
    if (demean) {
        x.mean <- mean(x)
        x <- x - x.mean
    } else x.mean <- 0
    n.used <- length(x)
    order.max <- if (is.null(order.max))
	min(n.used-1L, floor(10 * log10(n.used)))
    else floor(order.max)
    if (order.max < 1L) stop("'order.max' must be >= 1")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    z <- .C(C_burg,
            as.integer(n.used),
            as.double(x),
            as.integer(order.max),
            coefs=double(order.max^2),
            var1=double(1L+order.max),
            var2=double(1L+order.max)
            )
    coefs <- matrix(z$coefs, order.max, order.max)
    partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))
    var.pred <- if(var.method == 1L) z$var1 else z$var2
    xaic <- n.used * log(var.pred) + 2 * (0L:order.max) + 2 * demean
    maic <- min(aic)
    xaic <- if(is.finite(maic)) xaic - min(xaic) else ifelse(xaic == maic, 0, Inf)
    names(xaic) <- 0L:order.max
    order <- if (aic) (0L:order.max)[xaic == 0] else order.max
    ar <- if (order) coefs[order, 1L:order] else numeric()
    var.pred <- var.pred[order+1L]
    resid <- if(order) c(rep(NA, order), embed(x, order+1) %*% c(1, -ar))
    else x
    if(ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean = x.mean,
                aic = xaic, n.used = n.used, order.max = order.max,
                partialacf = partialacf, resid = resid,
                method = ifelse(var.method==1L,"Burg","Burg2"),
                series = series, frequency = xfreq, call = match.call())
    if(order) {
        xacf <- acf(x, type = "covariance", lag.max = order, plot=FALSE)$acf
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    }
    class(res) <- "ar"
    res
}

