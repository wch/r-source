#  File src/library/stats/R/arma0.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

arima0 <- function(x, order = c(0, 0, 0),
                   seasonal = list(order = c(0, 0, 0), period = NA),
                   xreg = NULL, include.mean = TRUE, delta = 0.01,
                   transform.pars = TRUE, fixed = NULL, init = NULL,
                   method = c("ML", "CSS"), n.cond,
                   optim.control = list())
{
    arma0f <- function(p)
    {
        par <- as.double(fixed)
        par[mask] <- p
        .Call(C_arma0fa, G, par)
    }

    arCheck <- function(ar)
    {
        p <- max(which(c(1, -ar) != 0)) - 1
        if(!p) return(TRUE)
        all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
    }

    maInvert <- function(ma)
    {
        ## polyroot can't cope with leading zero.
        q <- length(ma)
        q0 <- max(which(c(1,ma) != 0)) - 1
        if(!q0) return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        warning("converting non-invertible initial MA values")
        if(q0 == 1) return(c(1/ma[1L], rep(0, q-q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for(r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep(0, q-q0))
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1)
        stop("only implemented for univariate time series")
    method <- match.arg(method)
    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    dim(x) <- NULL
    n <- length(x)

    if(!missing(order))
        if(!is.numeric(order) || length(order) != 3L || any(order < 0))
            stop("'order' must be a non-negative numeric vector of length 3")
    if(!missing(seasonal))
        if(is.list(seasonal)) {
            if(is.null(seasonal$order))
                stop("'seasonal' must be a list with component 'order'")
            if(!is.numeric(seasonal$order) || length(seasonal$order) != 3L
               || any(seasonal$order < 0))
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        } else if(is.numeric(order)) {
            if(length(order) == 3) seasonal <- list(order=seasonal)
            else ("'seasonal' is of the wrong length")
        } else stop("'seasonal' must be a list with component 'order'")

    if(is.null(seasonal$period) || is.na(seasonal$period)
       || seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- c(order[-2L], seasonal$order[-2L], seasonal$period,
              order[2L], seasonal$order[2L])
    narma <- sum(arma[1L:4L])
    if(d <- order[2L]) x <- diff(x, 1, d)
    if(d <- seasonal$order[2L]) x <- diff(x, seasonal$period, d)
    xtsp <- tsp(x)
    tsp(x) <- NULL
    nd <- order[2L] + seasonal$order[2L]
    n.used <- length(x)
    ncond <- n - n.used
    if(method == "CSS") {
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
        ncond <- if(!missing(n.cond)) ncond + max(n.cond, ncond1)
        else ncond + ncond1
    }
    if(is.null(xreg)) {
        ncxreg <- 0
    } else {
        if(NROW(xreg) != n) stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
    }
    class(xreg) <- NULL
    if(include.mean && (nd == 0)) {
        if(is.matrix(xreg) && is.null(colnames(xreg)))
            colnames(xreg) <- paste0("xreg", 1L:ncxreg)
        xreg <- cbind(intercept = rep_len(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1
    }

    if (is.null(fixed)) fixed <- rep_len(NA_real_, narma + ncxreg)
    else if(length(fixed) != narma + ncxreg) stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    if(transform.pars && any(!mask[1L:narma])) {
        warning("some ARMA parameters were fixed: setting transform.pars = FALSE")
        transform.pars <- FALSE
    }

    if(ncxreg) {
        if(d <- order[2L]) xreg <- diff(xreg, 1, d)
        if(d <- seasonal$order[2L]) xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
        if(qr(na.omit(xreg))$rank < ncol(xreg)) stop("'xreg' is collinear")
        if(is.null(cn <- colnames(xreg)))
            cn <- paste0("xreg", 1L:ncxreg)
    }
    if(anyNA(x) || (ncxreg && anyNA(xreg)))
        ## only exact recursions handle NAs
        if(method == "ML" && delta >= 0) {
            warning("NAs present: setting 'delta' to -1")
            delta <- -1
        }

    init0 <- rep_len(0, narma)
    parscale <- rep_len(1, narma)
    if (ncxreg) {
        orig.xreg <- (ncxreg == 1) || any(!mask[narma + 1L:ncxreg])
        if(!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        fit <- lm(x ~ xreg - 1, na.action = na.omit)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[,2]
        parscale <- c(parscale, ses)
    }

    storage.mode(x) <- storage.mode(xreg) <- "double"
    if(method == "CSS") transform.pars <- 0
    G <- .Call(C_setup_starma, as.integer(arma), x, n.used, xreg,
               ncxreg, delta, transform.pars > 0,
               ncond - (n - n.used))
    on.exit(.Call(C_free_starma, G))

    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("'init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(transform.pars) {
            if(any(!mask[1L:narma]))
                warning("transformed ARMA parameters were fixed")
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part")
            ## enforce invertibility
            if(arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if(arma[4L] > 0) {
                ind <- sum(arma[1L:3]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
            init <- .Call(C_Invtrans, G, as.double(init))
        }
    } else init <- init0


    .Call(C_Starma_method, G, method == "CSS")
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]
    res <- optim(init[mask], arma0f, method = "BFGS",
                 hessian = TRUE, control = optim.control)
    if((code <- res$convergence) > 0)
        warning(gettextf("possible convergence problem: optim gave code = %d",
                         code), domain = NA)
    coef <- res$par

    if(transform.pars) {
        cf <- fixed
        cf[mask] <- coef
        ## do it this way to ensure hessian was computed inside
        ## stationarity region
        A <- .Call(C_Gradtrans, G, as.double(cf))[mask, mask]
        var <- t(A) %*% solve(res$hessian*length(x)) %*% A
        coef <- .Call(C_Dotrans, G, as.double(cf))[mask]
        .Call(C_set_trans, G, 0)
    } else var <- solve(res$hessian*length(x))
    arma0f(coef)  # reset pars
    sigma2 <- .Call(C_get_s2, G)
    resid <- .Call(C_get_resid, G)
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    n.used <- sum(!is.na(resid))
    nm <- NULL
    if(arma[1L] > 0) nm <- c(nm, paste0("ar", 1L:arma[1L]))
    if(arma[2L] > 0) nm <- c(nm, paste0("ma", 1L:arma[2L]))
    if(arma[3L] > 0) nm <- c(nm, paste0("sar", 1L:arma[3L]))
    if(arma[4L] > 0) nm <- c(nm, paste0("sma", 1L:arma[4L]))
    fixed[mask] <- coef
    if(ncxreg > 0) {
        nm <- c(nm, cn)
        if(!orig.xreg) {
            ind <- narma + 1L:ncxreg
            fixed[ind] <- S$v %*% fixed[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(fixed) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", "sdiff")
    dimnames(var) <- list(nm[mask], nm[mask])
    value <- 2 * n.used * res$value + n.used + n.used*log(2*pi)
    aic <- if(method != "CSS") value + 2*length(coef) + 2 else NA
    res <- list(coef = fixed, sigma2 = sigma2, var.coef = var, mask = mask,
                loglik = -0.5*value, aic = aic, arma = arma,
                residuals = resid,
                call = match.call(), series = series,
                code = code, n.cond = ncond)
    class(res) <- "arima0"
    res
}

print.arima0 <- function(x, digits = max(3L, getOption("digits") - 3L),
                         se = TRUE, ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    cat("Coefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep_len(0, length(coef))
        ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
        coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
        coef <- rbind(coef, s.e. = ses)
    }
    print.default(coef, print.gap = 2)
    cm <- x$call$method
    if(is.null(cm) || cm != "CSS")
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  log likelihood = ", format(round(x$loglik,2)),
            ",  aic = ", format(round(x$aic,2)),
            "\n", sep = "")
    else
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  part log likelihood = ", format(round(x$loglik,2)),
            "\n", sep = "")
    invisible(x)
}

predict.arima0 <-
    function(object, n.ahead = 1L, newxreg = NULL, se.fit=TRUE, ...)
{
    myNCOL <- function(x) if(is.null(x)) 0 else NCOL(x)
    data <- eval.parent(parse(text = object$series))
    xr <- object$call$xreg
    xreg <- if(!is.null(xr)) eval.parent(xr) else NULL
    ncxreg <- myNCOL(xreg)
    if(myNCOL(newxreg) != ncxreg)
        stop("'xreg' and 'newxreg' have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(object$residuals)
    n <- length(data)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1L:4L])
    if(length(coefs) > narma) {
        if(names(coefs)[narma+1] == "intercept") {
            xreg <- cbind(intercept = rep_len(1, n), xreg)
            newxreg <- cbind(intercept = rep_len(1, n.ahead), newxreg)
            ncxreg <- ncxreg+1
        }
        data <- data - as.matrix(xreg) %*% coefs[-(1L:narma)]
        xm <- drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    } else xm <- 0
    ## check invertibility of MA part(s)
    if(arma[2L] > 0) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if(arma[4L] > 0) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    storage.mode(data) <- "double"
    G <- .Call(C_setup_starma, as.integer(arma), data, n, rep_len(0., n),
               0., -1., 0., 0.)
    on.exit(.Call(C_free_starma, G))
    .Call(C_Starma_method, G, TRUE)
    .Call(C_arma0fa, G, as.double(coefs))
    z <- .Call(C_arma0_kfore, G, arma[6L], arma[7L], n.ahead)
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(data),
               frequency = xtsp[3L])
    if(se.fit) {
        se <- ts(sqrt(z[[2L]]),
                 start = xtsp[2L] + deltat(data), frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    } else return(pred)
}

arima0.diag <- function(...) .Defunct()

tsdiag.Arima <- tsdiag.arima0 <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar <- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$residuals
    stdres <- rs/sqrt(object$sigma2)
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(object$residuals, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1L:nlag) pval[i] <- Box.test(rs, i, type="Ljung-Box")$p.value
    plot(1L:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}

tsdiag <- function(object, gof.lag, ...) UseMethod("tsdiag")
