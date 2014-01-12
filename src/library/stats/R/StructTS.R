#  File src/library/stats/R/StructTS.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2002-14 The R Core Team
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

StructTS <- function(x, type = c("level", "trend", "BSM"),
                     init = NULL, fixed = NULL, optim.control = NULL)
{
    makeLevel <- function(x)
    {
        T <- matrix(1., 1L, 1L)
        Z <- 1.
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- xm
        P <- Pn <- matrix(0., 1L, 1L)
        h <- 1.0
        V <- diag(1L)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }

    makeTrend <- function(x)
    {
        T <- matrix(c(1.,0.,1.,1.), 2L, 2L)
        Z <- c(1., 0.)
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, 0)
        P <- Pn <- matrix(0., 2L, 2L)
        h <- 1.0
        V <- diag(2L)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }

    makeBSM <- function(x, nf)
    {
        ## See Harvey (1993, p.143)
        if(nf <= 1L) stop("frequency must be a positive integer >= 2 for BSM")
        T <- matrix(0., nf + 1L, nf + 1L)
        T[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
        T[3L, ] <- c(0, 0, rep(-1, nf - 1L))
        if(nf >= 3L) {
            ind <- 3:nf
            T[cbind(ind+1L, ind)] <- 1
        }
        Z <- c(1., 0., 1., rep(0., nf - 2L))
        xm <- if(is.na(x[1L])) mean(x, na.rm = TRUE) else x[1L]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, rep(0, nf))
        P <- Pn <- matrix(0., nf+1L, nf+1L)
        h <- 1.
        V <- diag(c(1., 1., 1., rep(0., nf-2L)))
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }

    getLike <- function(par)
    {
        p <- cf
        p[mask] <- par
        if(all(p == 0)) return(1000)
        Z$V[cbind(1L:np, 1L:np)] <- p[-(np+1L)]*vx
        Z$h <- p[np+1L]*vx
        Z$P[] <- 1e6*vx
        Z$a[] <- a0 # $<- no longer duplicates in >= 3.1.0
        0.5 * sum(.Call(C_KalmanLike, y, Z$Z, Z$a, Z$P, Z$T, Z$V,
                        Z$h, Z$Pn, -1L, FALSE, TRUE))/length(y)
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1L)
        stop("only implemented for univariate time series")
    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    storage.mode(x) <- "double"
    if(is.na(x[1L]))
        stop("the first value of the time series must not be missing")
    type <- if(missing(type)) if(frequency(x) > 1) "BSM" else "trend"
    else match.arg(type)
    dim(x) <- NULL
    xtsp <- tsp(x)
    nf <- frequency(x)
    Z <- switch(type,
                "level" = makeLevel(x),
                "trend" = makeTrend(x),
                "BSM" = makeBSM(x, nf)
                )
    a0 <- Z$a
    vx <- var(x, na.rm=TRUE)/100
    np <- switch(type, "level" = 1L, "trend" = 2L, "BSM" = 3L)
    if (is.null(fixed)) fixed <- rep(NA_real_, np+1L)
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    cf <- fixed/vx
    if(is.null(init)) init <- rep(1, np+1L) else init <- init/vx

    y <- x
    res <- optim(init[mask], getLike, method = "L-BFGS-B",
                 lower = rep(0, np+1L), upper = rep(Inf, np+1L),
                 control = optim.control)
        if(res$convergence > 0)
            warning(gettextf("possible convergence problem: 'optim' gave code = %d and message %s",
                             res$convergence, sQuote(res$message)), domain = NA)
    coef <- cf
    coef[mask] <- res$par
    Z$V[cbind(1L:np, 1L:np)] <- coef[1L:np]*vx
    Z$h <- coef[np+1L]*vx
    Z$P[] <- 1e6*vx
    Z$a <- a0
    z <- KalmanRun(y, Z, -1)
    resid <- ts(z$resid)
    tsp(resid) <- xtsp
    Z0 <- Z; Z0$P[] <- 1e6*vx; Z0$a <- a0

    cn <- switch(type,
                 "level" = c("level"),
                 "trend" = c("level", "slope"),
                 "BSM" = c("level", "slope", "sea")
                 )
    states <- z$states
    if(type == "BSM") states <- states[, 1L:3L]
    dimnames(states) <- list(time(x), cn)
    states <- ts(states, start = xtsp[1L], frequency = nf)

    coef <- pmax(coef*vx, 0) # computed values just below 0 are possible
    names(coef) <- switch(type,
                          "level" = c("level", "epsilon"),
                          "trend" = c("level", "slope", "epsilon"),
                          "BSM" = c("level", "slope", "seas", "epsilon")
                          )
    loglik <- -length(y) * res$value - 0.5 * sum(!is.na(y)) * log(2 * pi)
    loglik0 <- -length(y) * res$value + length(y) * log(2 * pi)
    res <- list(coef = coef, loglik = loglik, loglik0 = loglik0, data = y,
                residuals = resid, fitted = states,
                call = match.call(), series = series,
                code = res$convergence, model = Z, model0 = Z0, xtsp = xtsp)
    class(res) <- "StructTS"
    res
}

print.StructTS <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    cat("Variances:\n")
    print.default(x$coef, print.gap = 2L, digits = digits, ...)
    invisible(x)
}

predict.StructTS <- function(object, n.ahead = 1L, se.fit = TRUE, ...)
{
    xtsp <- object$xtsp
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1L]], start = xtsp[2L] + 1/xtsp[3L], frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]]), start = xtsp[2L] + 1/xtsp[3L],
                 frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}

tsdiag.StructTS <- function(object, gof.lag = 10L, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar <- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$residuals
    stdres <- rs
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0.)
    acf(object$residuals, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1L:nlag) pval[i] <- Box.test(rs, i, type = "Ljung-Box")$p.value
    plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2L, col = "blue")
}


tsSmooth <- function(object, ...) UseMethod("tsSmooth")

tsSmooth.StructTS <- function(object, ...)
{
    res <- KalmanSmooth(object$data, object$model0, -1)$smooth
    dn <- dim(fitted(object))
    res <- res[, 1L:dn[2L], drop = FALSE]
    dimnames(res) <- dimnames(fitted(object))
    ts(res, start = object$xtsp[1L], frequency = object$xtsp[3L])
}
