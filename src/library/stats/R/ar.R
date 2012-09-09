#  File src/library/stats/R/ar.R
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

## based on, especially multivariate case, code by Martyn Plummer
ar <-
    function (x, aic = TRUE, order.max = NULL,
              method = c("yule-walker","burg", "ols", "mle", "yw"),
              na.action = na.fail, series = deparse(substitute(x)), ...)
{
    res <- switch(match.arg(method),
        "yule-walker" = ar.yw(x, aic=aic, order.max=order.max,
                  na.action = na.action, series=series, ...),
	"burg" = ar.burg(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
	"ols" = ar.ols(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
 	"mle" = ar.mle(x, aic=aic, order.max=order.max,
                              na.action = na.action, series=series, ...),
        "yw" = ar.yw(x, aic=aic, order.max=order.max,
                  na.action = na.action, series=series, ...)
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
    if(ists)  xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    if(any(is.na(x))) stop("NAs in 'x'")
    nser <- ncol(x)
    if (demean) {
        xm <- colMeans(x)
        x <- sweep(x, 2L, xm, check.margin=FALSE)
    } else xm <- rep.int(0, nser)
    n.used <- nrow(x)
    order.max <- if (is.null(order.max))
	min(n.used - 1L, floor(10 * log10(n.used))) else round(order.max)
    if (order.max < 1L) stop("'order.max' must be >= 1")
    else if (order.max >= n.used) stop("'order.max' must be < 'n.used'")
    xacf <- acf(x, type = "covariance", lag.max = order.max, plot = FALSE,
                demean = demean)$acf
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
        cal.aic <- function() { # omits mean params, that is constant adj
            det <- abs(prod(diag(qr(EA)$qr)))
            return(n.used * log(det) + 2 * m * nser * nser)
        }
        cal.resid <- function() {
            resid <- array(0, dim = c(n.used - order, nser))
            for (i in 0L:order)
                resid <- resid + x[(order - i + 1L):(n.used - i), , drop = FALSE] %*% t(ar[i + 1L, , ])
            return(rbind(matrix(NA, order, nser), resid))
        }
        order <- 0L
        for (m in 0L:order.max) {
            xaic[m + 1L] <- cal.aic()
            if (!aic || xaic[m + 1L] == min(xaic[seq_len(m + 1L)])) {
                ar <- A
                order <- m
                var.pred <- EA * n.used/(n.used - nser * (m + 1L))
            }
            if (m < order.max) {
                solve.yw(m)
                partialacf[m + 1L, , ] <- -A[m + 2L, , ]
            }
        }
        xaic <- xaic - min(xaic)
        names(xaic) <- 0L:order.max
        resid <- cal.resid()
        if(order) {
            ar <- -ar[2L:(order + 1L), , , drop = FALSE]
            dimnames(ar) <- list(seq_len(order), snames, snames)
        } else ar <- array(0, dim = c(0L, nser, nser),
                           dimnames = list(NULL, snames, snames))
        dimnames(var.pred) <- list(snames, snames)
        dimnames(partialacf) <- list(seq_len(order.max), snames, snames)
        colnames(resid) <- colnames(x)
    } else {
        ## univariate case
        r <- as.double(drop(xacf))
        z <- .Fortran(C_eureka,
                      as.integer(order.max),
                      r, r,
                      coefs = double(order.max^2),
                      vars = double(order.max),
                      double(order.max))
        coefs <- matrix(z$coefs, order.max, order.max)
        partialacf <- array(diag(coefs), dim = c(order.max, 1L, 1L))
        var.pred <- c(r[1L], z$vars)
        xaic <- n.used * log(var.pred) + 2 * (0L:order.max) + 2 * demean
        maic <- min(aic)
        xaic <- if(is.finite(maic)) xaic - min(xaic) else ifelse(xaic == maic, 0, Inf)
        names(xaic) <- 0L:order.max
        order <- if (aic) (0L:order.max)[xaic == 0L] else order.max
        ar <- if (order) coefs[order, seq_len(order)] else numeric()
        var.pred <- var.pred[order+1L]
        ## Splus compatibility fix
        var.pred <- var.pred * n.used/(n.used - (order + 1L))
        resid <- if(order) c(rep.int(NA, order), embed(x, order + 1L) %*% c(1, -ar))
        else as.vector(x) # we had as.matrix() above
        if(ists) {
            attr(resid, "tsp") <- xtsp
            attr(resid, "class") <- "ts"
        }
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, x.mean  =  drop(xm),
                aic  =  xaic, n.used = n.used, order.max = order.max,
                partialacf = partialacf, resid = resid, method = "Yule-Walker",
                series = series, frequency = xfreq, call = match.call())
    if(nser == 1L && order)
        res$asy.var.coef <-
            solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    class(res) <- "ar"
    res
}

print.ar <- function(x, digits = max(3, getOption("digits") - 3), ...)
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
            coef <- round(drop(x$ar), digits = digits)
            names(coef) <- seq_len(x$order)
            print.default(coef, print.gap = 2L)
        }
        if(!is.null(xint <- x$x.intercept) && !is.na(xint))
            cat("\nIntercept: ", format(xint, digits = digits),
                ## FIXME? asy.se.coef  *only* exists for  ar.ols (??)
                " (", format(x$asy.se.coef$x.mean, digits = digits),
                ") ", "\n", sep="")
        cat("\nOrder selected", x$order, " sigma^2 estimated as ",
            format(x$var.pred, digits = digits),"\n")

    }
    invisible(x)
}

predict.ar <- function(object, newdata, n.ahead = 1L, se.fit = TRUE, ...)
{
    if (n.ahead < 1L) stop("'n.ahead' must be at least 1")
    if(missing(newdata)) {
        newdata <- eval.parent(parse(text=object$series))
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
                npsi <- n.ahead - 1L
                psi <- .C(C_artoma,
			  as.integer(object$order), as.double(ar),
			  psi = double(npsi+object$order+1L),
			  as.integer(npsi+object$order+1L))$psi[seq_len(npsi)]
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

