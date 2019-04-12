#  File src/library/stats/R/ar.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1999-2019 The R Core Team
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
#  https://www.R-project.org/Licenses/

## based on, especially multivariate case, code by Martyn Plummer
ar <-
    function (x, aic = TRUE, order.max = NULL,
              method = c("yule-walker","burg", "ols", "mle", "yw"),
              na.action = na.fail, series = deparse(substitute(x)), ...)
{
    res <- switch(match.arg(method),
                  yw =,
                  "yule-walker" = ar.yw(x, aic = aic, order.max = order.max,
                  na.action = na.action, series = series, ...),
                  "burg" = ar.burg(x, aic = aic, order.max = order.max,
                  na.action = na.action, series = series, ...),
                  "ols" = ar.ols(x, aic = aic, order.max = order.max,
                  na.action = na.action, series = series, ...),
                  "mle" = ar.mle(x, aic = aic, order.max = order.max,
                  na.action = na.action, series = series, ...)
   )
    res$call <- match.call()
    res
}

ar.yw <- function(x, ...) UseMethod("ar.yw")

ar.yw.default <-
    function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
              demean = TRUE, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    if(ists) xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    if(any(is.na(x) != is.na(x[,1]))) stop("NAs in 'x' must be the same row-wise")
    nser <- ncol(x)
    if (demean) {
        xm <- colMeans(x, na.rm=TRUE)
        x <- sweep(x, 2L, xm, check.margin=FALSE)
    } else xm <- rep.int(0, nser)
    n.used <- nrow(x)
    n.obs <- sum(!is.na(x[,1])) # number of non-missing rows
    order.max <- if (is.null(order.max))
	min(n.obs - 1L, floor(10 * log10(n.obs))) else floor(order.max)
    if (order.max < 1L) stop("'order.max' must be >= 1")
    else if (order.max >= n.obs) stop("'order.max' must be < 'n.obs'")
    xacf <- acf(x, type = "covariance", lag.max = order.max, plot = FALSE,
		demean=demean, na.action = na.pass)$acf
    if(nser > 1L) {
        ## multivariate case
        snames <- colnames(x)
        A <- B <- array(0, dim = c(order.max + 1L, nser, nser))
        A[1L, , ] <- B[1L, , ] <- diag(nser)
        EA <- EB <- xacf[1L, , , drop = TRUE]
        partialacf <- array(dim = c(order.max, nser, nser))
        xaic <- numeric(order.max + 1L)
        solve.yw <- function(m) {
            # Solve Yule-Walker equations with Whittle's
            # generalization of the Levinson(-Durbin) algorithm
            betaA <- betaB <- 0
            for (i in 0L:m) {
                betaA <- betaA + A[i + 1L, , ] %*% xacf[m + 2L - i, , ]
                betaB <- betaB + B[i + 1L, , ] %*% t(xacf[m + 2L - i, , ])
            }
            KA <- -t(qr.solve(t(EB), t(betaA)))
            KB <- -t(qr.solve(t(EA), t(betaB)))
            EB <<- (diag(nser) - KB %*% KA) %*% EB
            EA <<- (diag(nser) - KA %*% KB) %*% EA
            Aold <- A
            Bold <- B
            for (i in seq_len(m + 1L)) {
                A[i + 1L, , ] <<- Aold[i + 1L, , ] + KA %*% Bold[m + 2L - i, , ]
                B[i + 1L, , ] <<- Bold[i + 1L, , ] + KB %*% Aold[m + 2L - i, , ]
            }
        }
        cal.aic <- function(m) { # (EA)  omits mean params, that is constant adj
            logdet <- determinant.matrix(EA)$modulus
                                        # == log(abs(prod(diag(qr(EA)$qr))))
            n.obs * logdet + 2 * m * nser * nser
        }
        cal.resid <- function() {
            resid <- array(0, dim = c(n.used - order, nser))
            for (i in 0L:order)
                resid <- resid +
                    tcrossprod(x[(order - i + 1L):(n.used - i), , drop = FALSE],
                               ar[i + 1L, , ])
            rbind(matrix(NA, order, nser), resid)
        }
        order <- 0L
        for (m in 0L:order.max) {
            xaic[m + 1L] <- cal.aic(m) # (EA)
            if (!aic || xaic[m + 1L] == min(xaic[seq_len(m + 1L)])) {
                ar <- A
                order <- m
                var.pred <- EA * n.obs/(n.obs - nser * (m + 1L))
            }
            if (m < order.max) {
                solve.yw(m) #-> update (EA, EB, A, B)
                partialacf[m + 1L, , ] <- -A[m + 2L, , ]
            }
        }
        xaic <- setNames(xaic - min(xaic), 0L:order.max)
        resid <- cal.resid()
        if(order) {
            ar <- -ar[2L:(order + 1L), , , drop = FALSE]
            dimnames(ar) <- list(seq_len(order), snames, snames)
        } else ar <- array(0, dim = c(0L, nser, nser),
                           dimnames = list(NULL, snames, snames))
        dimnames(var.pred) <- list(snames, snames)
        dimnames(partialacf) <- list(seq_len(order.max), snames, snames)
        colnames(resid) <- colnames(x)
    } else { ## univariate case
        if (xacf[1L] == 0) stop("zero-variance series")
        r <- as.double(drop(xacf))
        z <- .Fortran(C_eureka, as.integer(order.max), r, r,
                      coefs = double(order.max^2),
                      vars = double(order.max),
                      double(order.max))
        coefs <- matrix(z$coefs, order.max, order.max)
        partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))
        var.pred <- c(r[1L], z$vars)
        xaic <- n.obs * log(var.pred) + 2 * (0L:order.max) + 2 * demean
        maic <- min(aic)
	xaic <- setNames(if(is.finite(maic)) xaic - min(xaic) else
			 ifelse(xaic == maic, 0, Inf),
			 0L:order.max)
        order <- if (aic) (0L:order.max)[xaic == 0L] else order.max
        ar <- if (order) coefs[order, seq_len(order)] else numeric()
        var.pred <- var.pred[order + 1L]
        ## Splus compatibility fix
        var.pred <- var.pred * n.obs/(n.obs - (order + 1L))
        resid <- if(order) c(rep.int(NA, order), embed(x, order + 1L) %*% c(1, -ar))
        else as.vector(x) # we had as.matrix() above
        if(ists) {
            attr(resid, "tsp") <- xtsp
            attr(resid, "class") <- "ts"
        }
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean  =  drop(xm),
                aic  =  xaic, n.used = n.used, n.obs = n.obs, order.max = order.max,
                partialacf = partialacf, resid = resid, method = "Yule-Walker",
                series = series, frequency = xfreq, call = match.call())
    if(nser == 1L && order)
        res$asy.var.coef <- var.pred/n.obs *
            solve(toeplitz(drop(xacf)[seq_len(order)]))
    class(res) <- "ar"
    res
}

print.ar <- function(x, digits = max(3L, getOption("digits") - 3L), ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    nser <- NCOL(x$var.pred)
    if(nser > 1L) {
        res <- x[c("ar", if(!is.null(x$x.intercept)) "x.intercept", "var.pred")]
        res$ar <- aperm(res$ar, c(2L,3L,1L))
        print(res, digits = digits)
    } else { ## univariate case
        if(x$order) {
            cat("Coefficients:\n")
	    coef <- setNames(round(drop(x$ar), digits = digits),
			     seq_len(x$order))
            print.default(coef, print.gap = 2L)
        }
        if(!is.null(xint <- x$x.intercept) && !is.na(xint))
            cat("\nIntercept: ", format(xint, digits = digits),
                ## FIXME? asy.se.coef  *only* exists for  ar.ols (??)
                " (", format(x$asy.se.coef$x.mean, digits = digits),
                ") ", "\n", sep = "")
        cat("\nOrder selected", x$order, " sigma^2 estimated as ",
            format(x$var.pred, digits = digits))
        cat("\n")
    }
    invisible(x)
}

predict.ar <- function(object, newdata, n.ahead = 1L, se.fit = TRUE, ...)
{
    if (n.ahead < 1L) stop("'n.ahead' must be at least 1")
    if(missing(newdata)) {
        newdata <- eval.parent(str2lang(object$series))
        if (!is.null(nas <- object$call$na.action))
            newdata <- eval.parent(call(nas, newdata))
    }
    nser <- NCOL(newdata)
    ar <- object$ar
    p <- object$order
    st <- tsp(as.ts(newdata))[2L]
    dt <- deltat(newdata)
    xfreq <- frequency(newdata)
    tsp(newdata) <- NULL
    class(newdata) <- NULL
    if(NCOL(ar) != nser)
        stop("number of series in 'object' and 'newdata' do not match")
    n <- NROW(newdata)
    if(nser > 1L) {
        if(is.null(object$x.intercept)) xint <- rep.int(0, nser)
        else xint <- object$x.intercept
        x <- rbind(sweep(newdata, 2L, object$x.mean, check.margin = FALSE),
                   matrix(rep.int(0, nser), n.ahead, nser, byrow = TRUE))
        pred <- if(p) {
            for(i in seq_len(n.ahead)) {
                x[n+i,] <- ar[1L,,] %*% x[n+i-1L,] + xint
		if(p > 1L) for(j in 2L:p)
                    x[n+i,] <- x[n+i,] + ar[j,,] %*% x[n+i-j,]
            }
            x[n + seq_len(n.ahead), ]
        } else matrix(xint, n.ahead, nser, byrow = TRUE)
        pred <- pred + matrix(object$x.mean, n.ahead, nser, byrow = TRUE)
        colnames(pred) <- colnames(object$var.pred)
        if(se.fit) {
            warning("'se.fit' not yet implemented for multivariate models")
            se <- matrix(NA, n.ahead, nser)
        }
    } else {
        if(is.null(object$x.intercept)) xint <- 0
        else xint <- object$x.intercept
        x <- c(newdata - object$x.mean, rep.int(0, n.ahead))
        if(p) {
            for(i in seq_len(n.ahead))
                x[n+i] <- sum(ar * x[n+i - seq_len(p)]) + xint
            pred <- x[n + seq_len(n.ahead)]
            if(se.fit) {
                psi <- .Call(C_ar2ma, ar, n.ahead - 1L)
                vars <- cumsum(c(1, psi^2))
                se <- sqrt(object$var.pred*vars)[seq_len(n.ahead)]
            }
        } else {
            pred <- rep.int(xint, n.ahead)
            if (se.fit) se <- rep.int(sqrt(object$var.pred), n.ahead)
        }
        pred <- pred + rep.int(object$x.mean, n.ahead)
    }
    pred <- ts(pred, start = st + dt, frequency = xfreq)
    if(se.fit)
        list(pred = pred, se = ts(se, start = st + dt, frequency = xfreq))
    else pred
}

ar.mle <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                    demean = TRUE, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    ists <- is.ts(x)
    if (!is.null(dim(x)))
        stop("MLE only implemented for univariate series")
    x <- na.action(as.ts(x))
    if(anyNA(x)) stop("NAs in 'x'")
    if(!is.numeric(x))
        stop("'x' must be numeric")
    if(ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.vector(x) # drop attributes, including class
    n.used <- length(x)
    order.max <- if (is.null(order.max))
        min(n.used-1L, 12L, floor(10 * log10(n.used)))
    else round(order.max)

    if (order.max < 0L) stop ("'order.max' must be >= 0")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    if (aic) {
        coefs <- matrix(NA, order.max+1L, order.max+1L)
        var.pred <- numeric(order.max+1L)
        xaic <- numeric(order.max+1L)
        xm <- if(demean) mean(x) else 0
        coefs[1, 1L] <- xm
        var0 <- sum((x-xm)^2)/n.used
        var.pred[1L] <- var0
        xaic[1L] <- n.used * log(var0) + 2 * demean + 2 + n.used + n.used * log(2 * pi)
        for(i in seq_len(order.max)) {
            fit <- arima0(x, order=c(i, 0L, 0L), include.mean=demean)
            coefs[i+1L, seq_len(i+demean)] <- fit$coef[seq_len(i+demean)]
            xaic[i+1L] <- fit$aic
            var.pred[i+1L] <- fit$sigma2
        }
        xaic <- setNames(xaic - min(xaic), 0L:order.max)
        order <- (0L:order.max)[xaic == 0L]
        ar <- coefs[order+1L, seq_len(order)]
        x.mean <- coefs[order+1L, order+1L]
        var.pred <- var.pred[order+1L]
    } else {
        order <- order.max
        fit <- arima0(x, order=c(order, 0L, 0L), include.mean=demean)
        coefs <- fit$coef
        if(demean) {
            ar <- coefs[-length(coefs)]
            x.mean <- coefs[length(coefs)]
        } else {
            ar <- coefs
            x.mean <- 0
        }
        var.pred <- fit$sigma2
        xaic <- structure(0, names=order)
    }
    resid <- if(order) c(rep(NA, order), embed(x - x.mean, order+1L) %*% c(1, -ar))
    else x - x.mean
    if(ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred,
                x.mean = x.mean, aic = xaic,
                n.used = n.used, n.obs = n.used, order.max = order.max,
                partialacf = NULL, resid = resid, method = "MLE",
                series = series, frequency = xfreq, call = match.call())
    if(order) {
        xacf <- acf(x, type = "covariance", lag.max = order, plot=FALSE)$acf
        res$asy.var.coef <- var.pred/n.used *
            solve(toeplitz(drop(xacf)[seq_len(order)]))
    }
    class(res) <- "ar"
    res
}

## original code by Adrian Trapletti
ar.ols <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                    demean = TRUE, intercept = demean, series = NULL, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    rescale <- TRUE
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    if(anyNA(x)) stop("NAs in 'x'")
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
        X <-
	    if(intercept) {
		if(m) cbind(rep.int(1,nrow(y)), y[, (nser+1L):ncol(y)])
		else as.matrix(rep.int(1, nrow(y)))
	    } else {
		if(m) y[, (nser+1L):ncol(y)] else matrix(0, nrow(y), 0)
	    }
        ## FIXME: use [t]crossprod();  and instead of solve(XX), use solve(qr(X)) !!
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
        seA[[m - order.min+1L]] <-
            if(ncol(varA) > 0) sqrt(diag(varA)) else numeric()
        xaic[m - order.min+1L] <-
            n.used*log(det(varE[[m-order.min+1L]])) + 2*nser*(nser*m+intercept)
    }

    # Determine best model
    m <- if(aic) which.max(xaic == min(xaic)) + order.min - 1L else order.max

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
    xaic <- setNames(if(is.finite(maic)) xaic - min(xaic) else
		     ifelse(xaic == maic, 0, Inf), order.min:order.max)
    dim(ar) <- c(nser, nser, m)
    ar <- aperm(ar, c(3L,1L,2L))
    ses <- seA[[m - order.min + 1L]]
    if(intercept) {
        sem <- ses[iser]
        ses <- ses[-iser]
    } else sem <- rep.int(0, nser)
    dim(ses) <- c(nser, nser, m)
    ses <- aperm(ses, c(3L,1L,2L))
    var.pred <- varE[[m - order.min + 1L]]
    if(nser > 1L) {
        snames <- colnames(x)
        dimnames(ses) <- dimnames(ar) <- list(seq_len(m), snames, snames)
        dimnames(var.pred) <- list(snames, snames)
        names(sem) <- colnames(E) <- snames
    } else {
        var.pred <- drop(var.pred)
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
                n.used = n.used, n.obs = n.used, order.max = order.max,
                partialacf = NULL, resid = E, method = "Unconstrained LS",
                series = series, frequency = xfreq, call = match.call(),
                asy.se.coef = list(x.mean = sem, ar = drop(ses)))
    class(res) <- "ar"
    res
}

ar.yw.mts <-
function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
    demean = TRUE, series = NULL, var.method = 1L, ...)
{
    if (is.null(series)) series <- deparse(substitute(x))
    if (ists <- is.ts(x)) xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if(any(is.na(x) != is.na(x[,1]))) stop("NAs in 'x' must be the same row-wise")
    if (ists) xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    nser <- ncol(x)
    n.used <- nrow(x)
    n.obs <- sum(!is.na(x[,1])) # number of non-missing rows
    if (demean) {
        x.mean <- colMeans(x, na.rm=TRUE)
        x <- sweep(x, 2L, x.mean, check.margin=FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- floor(if(is.null(order.max)) 10 * log10(n.obs) else order.max)
    if (order.max < 1L)
        stop("'order.max' must be >= 1")
    xacf <- acf(x, type = "cov", plot = FALSE,
                lag.max = order.max, na.action = na.pass)$acf
    z <- .C(C_multi_yw,
            aperm(xacf, 3:1),
            as.integer(n.obs),
            as.integer(order.max),
            as.integer(nser),
            coefs = double((1L + order.max) * nser * nser),
            pacf = double((1L + order.max) * nser * nser),
            var = double((1L + order.max) * nser * nser),
            aic = double(1L + order.max),
            order = integer(1L),
            as.integer(aic))
    partialacf <- aperm(array(z$pacf, dim = c(nser, nser, order.max + 1L)), 3:1)[-1L, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max + 1L)), 3:1)
    xaic <- setNames(z$aic - min(z$aic), 0:order.max)
    order <- z$order
    resid <- x
    if (order > 0) {
        ar <- -aperm(array(z$coefs, dim = c(nser, nser, order.max + 1L)), 3:1)[2L:(order + 1L), , , drop = FALSE]
        for (i in 1L:order)
            resid[-(1L:order), ] <- resid[-(1L:order),] - x[(order - i + 1L):(n.used - i), ] %*% t(ar[i, , ])
        resid[1L:order, ] <- NA
    }
    else ar <- array(dim = c(0, nser, nser))
    var.pred <- var.pred[order + 1L, , , drop = TRUE] * n.obs/(n.obs - nser * (demean + order))
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
        x.mean = x.mean, aic = xaic, n.used = n.used, n.obs = n.obs, order.max = order.max,
        partialacf = partialacf, resid = resid, method = "Yule-Walker",
        series = series, frequency = xfreq, call = match.call())
    class(res) <- "ar"
    res
}

## ar.burg by B.D. Ripley based on R version by Martyn Plummer
ar.burg <- function(x, ...) UseMethod("ar.burg")
ar.burg.default <-
    function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
                   demean = TRUE, series = NULL, var.method = 1L, ...)
{
    if(is.null(series)) series <- deparse(substitute(x))
    if (ists <- is.ts(x)) xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if(anyNA(x)) stop("NAs in 'x'")
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
    xaic <- numeric(order.max + 1L)
    z <- .Call(C_Burg, x, order.max)
    coefs <- matrix(z[[1L]], order.max, order.max)
    partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))
    var.pred <- if(var.method == 1L) z[[2L]] else z[[3L]]
    if (any(is.nan(var.pred))) stop("zero-variance series")
    xaic <- n.used * log(var.pred) + 2 * (0L:order.max) + 2 * demean
    maic <- min(aic)
    xaic <- setNames(if(is.finite(maic)) xaic - min(xaic) else
		     ifelse(xaic == maic, 0, Inf), 0L:order.max)
    order <- if (aic) (0L:order.max)[xaic == 0] else order.max
    ar <- if (order) coefs[order, 1L:order] else numeric()
    var.pred <- var.pred[order + 1L]
    resid <- if(order) c(rep(NA, order), embed(x, order+1L) %*% c(1, -ar))
    else x
    if(ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean = x.mean,
                aic = xaic, n.used = n.used, n.obs = n.used, order.max = order.max,
                partialacf = partialacf, resid = resid,
                method = ifelse(var.method==1L,"Burg","Burg2"),
                series = series, frequency = xfreq, call = match.call())
    if(order) {
        xacf <- acf(x, type = "covariance", lag.max = order, plot = FALSE)$acf
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    }
    class(res) <- "ar"
    res
}

ar.burg.mts <-
function (x, aic = TRUE, order.max = NULL, na.action = na.fail,
          demean = TRUE, series = NULL, var.method = 1L, ...)
{
    if (is.null(series))
        series <- deparse(substitute(x))
    if (ists <- is.ts(x))
        xtsp <- tsp(x)
    x <- na.action(as.ts(x))
    if (anyNA(x))
        stop("NAs in 'x'")
    if (ists)
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    nser <- ncol(x)
    n.used <- nrow(x)
    if (demean) {
        x.mean <- colMeans(x)
        x <- sweep(x, 2L, x.mean, check.margin = FALSE)
    }
    else x.mean <- rep(0, nser)
    order.max <- floor(if(is.null(order.max)) 10 * log10(n.used) else order.max)
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
    partialacf <-
        aperm(array(z$pacf, dim = c(nser, nser, order.max + 1L)), 3:1)[-1L, , , drop = FALSE]
    var.pred <- aperm(array(z$var, dim = c(nser, nser, order.max + 1L)), 3:1)
    xaic <- setNames(z$aic - min(z$aic), 0:order.max)
    order <- z$order
    ar <- if (order)
        -aperm(array(z$coefs, dim = c(nser, nser, order.max + 1L)), 3:1)[2L:(order + 1L), , , drop = FALSE]
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
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean = x.mean,
                aic = xaic, n.used = n.used, n.obs = n.used, order.max = order.max,
                partialacf = partialacf, resid = resid,
                method = ifelse(var.method == 1L, "Burg", "Burg2"),
                series = series, frequency = xfreq,
                call = match.call())
    class(res) <- "ar"
    res
}
