#  File src/library/stats/R/ar.ols.R
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

## original code by Adrian Trapletti
ar.ols <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                    demean = TRUE, intercept = demean, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    rescale <- TRUE
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    if(any(is.na(x))) stop("NAs in 'x'")
    if(ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    n.used <- nrow(x)
    nser <- ncol(x)
    iser <- seq_len(nser)
    if(rescale) {
        sc <- sqrt(drop(apply(x, 2L, var)))
        sc[sc == 0] <- 1
        x <- x/rep.int(sc, rep.int(n.used, nser))
    } else sc <- rep.int(1, nser)

    order.max <- if (is.null(order.max))
	min(n.used-1L, floor(10 * log10(n.used))) else round(order.max)
    if (order.max < 0L)	     stop("'order.max' must be >= 0")
    if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    order.min <- if (aic) 0L else order.max
    varE <- seA <- A <- vector("list", order.max - order.min + 1L)
    xaic <- rep.int(Inf, order.max - order.min + 1L)

    ## allow for rounding error
    det <- function(x) max(0, prod(diag(qr(x)$qr))*(-1)^(ncol(x)-1))

    ## remove means for conditioning
    if(demean) {
        xm <- colMeans(x)
        x <- sweep(x, 2L, xm, check.margin=FALSE)
    } else xm <- rep.int(0, nser)
    ## Fit models of increasing order

    for (m in order.min:order.max)
    {
        y <- embed(x, m+1L)
        if(intercept) {
            if (m) X <- cbind(rep.int(1,nrow(y)), y[, (nser+1L):ncol(y)])
            else X <- as.matrix(rep.int(1, nrow(y)))
        } else {
            if (m) X <- y[, (nser+1L):ncol(y)]
            else X <- matrix(0, nrow(y), 0)
        }
        Y <- t(y[, iser])
        N <- ncol(Y)
        XX <- t(X)%*%X
        rank <- qr(XX)$rank
        if (rank != nrow(XX))
        {
            warning(paste("model order: ", m,
                          "singularities in the computation of the projection matrix",
                          "results are only valid up to model order", m - 1L),
                    domain = NA)
            break
        }
        P <- if(ncol(XX) > 0) solve(XX) else XX
        A[[m - order.min + 1L]] <- Y %*% X %*% P
        YH <- A[[m - order.min + 1L]] %*% t(X)
        E <- (Y - YH)
        varE[[m - order.min + 1L]] <- tcrossprod(E)/N
        varA <- P %x% (varE[[m - order.min + 1L]])
        seA[[m - order.min+1L]] <- if(ncol(varA) > 0) sqrt(diag(varA))
        else numeric()
        xaic[m - order.min+1L] <-
            n.used*log(det(varE[[m-order.min+1L]])) + 2*nser*(nser*m+intercept)
    }

    # Determine best model
    m <- if(aic) which(xaic == min(xaic))[1L] + order.min - 1L else order.max

    ## Recalculate residuals of best model

    y <- embed(x, m+1L)
    AA <- A[[m - order.min + 1L]]
    if(intercept) {
        xint <- AA[, 1L]
        ar <- AA[, -1L]
        X <- if(m) cbind(rep.int(1,nrow(y)), y[, (nser+1L):ncol(y)])
        else as.matrix(rep.int(1, nrow(y)))
    } else {
        X <- if(m) y[, (nser+1L):ncol(y)] else matrix(0, nrow(y), 0L)
        xint <- NULL
        ar <- AA
    }
    Y <- t(y[, iser, drop = FALSE])
    YH <- AA %*% t(X)
    E <- drop(rbind(matrix(NA, m, nser), t(Y - YH)))

    maic <- min(aic)
    xaic <- if(is.finite(maic)) xaic - min(xaic) else ifelse(xaic == maic, 0, Inf)
    names(xaic) <- order.min:order.max
    dim(ar) <- c(nser, nser, m)
    ar <- aperm(ar, c(3L,1L,2L))
    ses <- seA[[m - order.min + 1L]]
    if(intercept) {
        sem <- ses[iser]
        ses <- ses[-iser]
    } else sem <- rep.int(0, nser)
    dim(ses) <- c(nser, nser, m)
    ses <- aperm(ses, c(3,1,2))
    var.pred <- varE[[m - order.min + 1L]]
    if(nser > 1L) {
        snames <- colnames(x)
        dimnames(ses) <- dimnames(ar) <- list(seq_len(m), snames, snames)
        dimnames(var.pred) <- list(snames, snames)
        names(sem) <- colnames(E) <- snames
    }
    if(ists) {
        attr(E, "tsp") <- xtsp
        attr(E, "class") <- "ts"
    }
    if(rescale) {
        xm <- xm * sc
        if(!is.null(xint)) xint <- xint * sc
        aa <- outer(sc, 1/sc)
        if(nser > 1L && m) for(i in seq_len(m)) ar[i,,] <- ar[i,,]*aa
        var.pred <- var.pred * drop(outer(sc, sc))
        E <- E * rep.int(sc, rep.int(NROW(E), nser))
        sem <- sem*sc
        if(m)
            for(i in seq_len(m)) ses[i,,] <- ses[i,,]*aa
    }
    res <- list(order = m, ar = ar, var.pred = var.pred,
                x.mean = xm, x.intercept = xint, aic = xaic,
                n.used = n.used, order.max = order.max,
                partialacf = NULL, resid = E, method = "Unconstrained LS",
                series = series, frequency = xfreq, call = match.call(),
                asy.se.coef = list(x.mean = sem, ar=drop(ses)))
    class(res) <- "ar"
    res
}
