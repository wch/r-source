#  File src/library/stats/R/arima.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 2002-2015 The R Core Team
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

arima <- function(x, order = c(0L, 0L, 0L),
                  seasonal = list(order = c(0L, 0L, 0L), period = NA),
                  xreg = NULL, include.mean = TRUE,
                  transform.pars = TRUE, fixed = NULL, init = NULL,
                  method = c("CSS-ML", "ML", "CSS"), n.cond,
                  SSinit = c("Gardner1980", "Rossignol2011"),
                  optim.method = "BFGS",
                  optim.control = list(), kappa = 1e6)
{
    "%+%" <- function(a, b) .Call(C_TSconv, a, b)

    SSinit <- match.arg(SSinit)
    SS.G <- SSinit == "Gardner1980"
    ## helper of armafn(), called by optim()
    upARIMA <- function(mod, phi, theta)
    {
        p <- length(phi); q <- length(theta)
        mod$phi <- phi; mod$theta <- theta
        r <- max(p, q + 1L)
        if(p > 0) mod$T[1L:p, 1L] <- phi
	if(r > 1L)
	    mod$Pn[1L:r, 1L:r] <-
		if(SS.G) .Call(C_getQ0, phi, theta)
		else .Call(C_getQ0bis, phi, theta, tol = 0)# tol=0: less checking
	else
	    mod$Pn[1L, 1L] <- if (p > 0) 1/(1 - phi^2) else 1
        mod$a[] <- 0
        mod
    }

    arimaSS <- function(y, mod)
    {
        ## next call changes mod elemnts a, P, Pn so beware!
        .Call(C_ARIMA_Like, y, mod, 0L, TRUE)
    }

    ## the objective function called by optim()
    armafn <- function(p, trans)
    {
        par <- coef
        par[mask] <- p
        trarma <- .Call(C_ARIMA_transPars, par, arma, trans)
	if(is.null(Z <- tryCatch(upARIMA(mod, trarma[[1L]], trarma[[2L]]),
				 error = function(e) NULL)))
	    return(.Machine$double.xmax)# bad parameters giving error, e.g. in solve(.)
        if(ncxreg > 0) x <- x - xreg %*% par[narma + (1L:ncxreg)]
        ## next call changes objects a, P, Pn so beware!
        res <- .Call(C_ARIMA_Like, x, Z, 0L, FALSE)
        s2 <- res[1L]/res[3L]
        0.5*(log(s2) + res[2L]/res[3L])
    }

    armaCSS <- function(p)
    {
        par <- as.double(fixed)
        par[mask] <- p
        trarma <- .Call(C_ARIMA_transPars, par, arma, FALSE)
        if(ncxreg > 0) x <- x - xreg %*% par[narma + (1L:ncxreg)]
        res <- .Call(C_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]],
                     as.integer(ncond), FALSE)
        0.5 * log(res)
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
        q0 <- max(which(c(1,ma) != 0)) - 1L
        if(!q0) return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        if(q0 == 1) return(c(1/ma[1L], rep.int(0, q - q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for (r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep.int(0, q - q0))
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1L)
        stop("only implemented for univariate time series")
    method <- match.arg(method)

    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    storage.mode(x) <- "double"  # a precaution
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
               || any(seasonal$order < 0L))
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        } else if(is.numeric(order)) {
            if(length(order) == 3L) seasonal <- list(order=seasonal)
            else ("'seasonal' is of the wrong length")
        } else stop("'seasonal' must be a list with component 'order'")

    if (is.null(seasonal$period) || is.na(seasonal$period)
        ||seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- as.integer(c(order[-2L], seasonal$order[-2L], seasonal$period,
                         order[2L], seasonal$order[2L]))
    narma <- sum(arma[1L:4L])

    xtsp <- tsp(x)
    tsp(x) <- NULL
    Delta <- 1.
    for(i in seq_len(order[2L])) Delta <- Delta %+% c(1., -1.)
    for(i in seq_len(seasonal$order[2L]))
        Delta <- Delta %+% c(1, rep.int(0, seasonal$period-1), -1)
    Delta <- - Delta[-1L]
    nd <- order[2L] + seasonal$order[2L]
    n.used <- sum(!is.na(x)) - length(Delta)
    if (is.null(xreg)) {
        ncxreg <- 0L
    } else {
        nmxreg <- deparse(substitute(xreg))
        if (NROW(xreg) != n) stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
        xreg <- as.matrix(xreg)
        storage.mode(xreg) <- "double"
    }
    class(xreg) <- NULL
    if (ncxreg > 0L && is.null(colnames(xreg)))
        colnames(xreg) <-
            if(ncxreg == 1L) nmxreg else paste0(nmxreg, 1L:ncxreg)
    if (include.mean && (nd == 0L)) {
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1L
    }
    if(method == "CSS-ML") {
        anyna <- anyNA(x)
        if(ncxreg) anyna <- anyna || anyNA(xreg)
        if(anyna) method <- "ML"
    }

    if (method == "CSS" || method == "CSS-ML") {
        ncond <- order[2L] + seasonal$order[2L] * seasonal$period
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
	ncond <- ncond + if(!missing(n.cond)) max(n.cond, ncond1) else ncond1
    } else ncond <- 0

    if (is.null(fixed)) fixed <- rep(NA_real_, narma + ncxreg)
    else if(length(fixed) != narma + ncxreg) stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
##    if(!any(mask)) stop("all parameters were fixed")
    no.optim <- !any(mask)
    if(no.optim) transform.pars <- FALSE
    if(transform.pars) {
        ind <- arma[1L] + arma[2L] + seq_len(arma[3L])
        if (any(!mask[seq_len(arma[1L])]) || any(!mask[ind])) {
            warning("some AR parameters were fixed: setting transform.pars = FALSE")
            transform.pars <- FALSE
        }
    }
    init0 <- rep.int(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg) {
        cn <- colnames(xreg)
        orig.xreg <- (ncxreg == 1L) || any(!mask[narma + 1L:ncxreg])
        if (!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        dx <- x
        dxreg <- xreg
        if(order[2L] > 0L) {
            dx <- diff(dx, 1L, order[2L])
            dxreg <- diff(dxreg, 1L, order[2L])
        }
        if(seasonal$period > 1L & seasonal$order[2L] > 0) {
            dx <- diff(dx, seasonal$period, seasonal$order[2L])
            dxreg <- diff(dxreg, seasonal$period, seasonal$order[2L])
        }
        fit <- if(length(dx) > ncol(dxreg))
            lm(dx ~ dxreg - 1, na.action = na.omit)
        else list(rank = 0L)
        if(fit$rank == 0L) {
            ## Degenerate model. Proceed anyway so as not to break old code
            fit <- lm(x ~ xreg - 1, na.action = na.omit)
        }
        n.used <- sum(!is.na(resid(fit))) - length(Delta)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[, 2L]
        parscale <- c(parscale, 10 * ses)
    }
    if (n.used <= 0) stop("too few non-missing observations")

    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("'init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(method == "ML") {
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part")
            if(transform.pars)
                init <- .Call(C_ARIMA_Invtrans, as.double(init), arma)
        }
    } else init <- init0

    coef <- as.double(fixed)
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]

    if(method == "CSS") {
        res <- if(no.optim)
            list(convergence=0L, par=numeric(), value=armaCSS(numeric()))
        else
            optim(init[mask], armaCSS,  method = optim.method, hessian = TRUE,
                  control = optim.control)
        if(res$convergence > 0)
            warning(gettextf("possible convergence problem: optim gave code = %d",
                             res$convergence), domain = NA)
        coef[mask] <- res$par
        ## set model for predictions
        trarma <- .Call(C_ARIMA_transPars, coef, arma, FALSE)
	mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, SSinit)
        if(ncxreg > 0) x <- x - xreg %*% coef[narma + (1L:ncxreg)]
        arimaSS(x, mod)
        val <- .Call(C_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]],
                     as.integer(ncond), TRUE)
        sigma2 <- val[[1L]]
        var <- if(no.optim) numeric() else solve(res$hessian * n.used)
    } else {
        if(method == "CSS-ML") {
            res <- if(no.optim)
                list(convergence=0L, par=numeric(), value=armaCSS(numeric()))
            else
                optim(init[mask], armaCSS,  method = optim.method,
                      hessian = FALSE, control = optim.control)
            if(res$convergence == 0) init[mask] <- res$par
            ## check stationarity
            if(arma[1L] > 0)
                if(!arCheck(init[1L:arma[1L]]))
                    stop("non-stationary AR part from CSS")
            if(arma[3L] > 0)
                if(!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]]))
                    stop("non-stationary seasonal AR part from CSS")
            ncond <- 0L
        }
        if(transform.pars) {
            init <- .Call(C_ARIMA_Invtrans, init, arma)
            ## enforce invertibility
            if(arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if(arma[4L] > 0) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
        }
        trarma <- .Call(C_ARIMA_transPars, init, arma, transform.pars)
	mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, SSinit)
        res <- if(no.optim)
            list(convergence = 0, par = numeric(),
                 value = armafn(numeric(), as.logical(transform.pars)))
        else
            optim(init[mask], armafn,  method = optim.method,
                  hessian = TRUE, control = optim.control,
                  trans = as.logical(transform.pars))
        if(res$convergence > 0)
            warning(gettextf("possible convergence problem: optim gave code = %d",
                             res$convergence), domain = NA)
        coef[mask] <- res$par
        if(transform.pars) {
            ## enforce invertibility
            if(arma[2L] > 0L) {
                ind <- arma[1L] + 1L:arma[2L]
                if(all(mask[ind]))
                    coef[ind] <- maInvert(coef[ind])
            }
            if(arma[4L] > 0L) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                if(all(mask[ind]))
                    coef[ind] <- maInvert(coef[ind])
            }
            if(any(coef[mask] != res$par))  {  # need to re-fit
                oldcode <- res$convergence
                res <- optim(coef[mask], armafn, method = optim.method,
                             hessian = TRUE,
                             control = list(maxit = 0L,
                             parscale = optim.control$parscale),
                             trans = TRUE)
                res$convergence <- oldcode
                coef[mask] <- res$par
            }
            ## do it this way to ensure hessian was computed inside
            ## stationarity region
            A <- .Call(C_ARIMA_Gradtrans, as.double(coef), arma)
            A <- A[mask, mask]
	    var <- crossprod(A, solve(res$hessian * n.used, A))
            coef <- .Call(C_ARIMA_undoPars, coef, arma)
        } else var <- if(no.optim) numeric() else solve(res$hessian * n.used)
        trarma <- .Call(C_ARIMA_transPars, coef, arma, FALSE)
	mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, SSinit)
        val <- if(ncxreg > 0L)
            arimaSS(x - xreg %*% coef[narma + (1L:ncxreg)], mod)
        else arimaSS(x, mod)
        sigma2 <- val[[1L]][1L]/n.used
    }
    value <- 2 * n.used * res$value + n.used + n.used * log(2 * pi)
    aic <- if(method != "CSS") value + 2*sum(mask) + 2 else NA
    nm <- NULL
    if (arma[1L] > 0L) nm <- c(nm, paste0("ar", 1L:arma[1L]))
    if (arma[2L] > 0L) nm <- c(nm, paste0("ma", 1L:arma[2L]))
    if (arma[3L] > 0L) nm <- c(nm, paste0("sar", 1L:arma[3L]))
    if (arma[4L] > 0L) nm <- c(nm, paste0("sma", 1L:arma[4L]))
    if (ncxreg > 0L) {
        nm <- c(nm, cn)
        if(!orig.xreg) {
            ind <- narma + 1L:ncxreg
            coef[ind] <- S$v %*% coef[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(coef) <- nm
    if(!no.optim) dimnames(var) <- list(nm[mask], nm[mask])
    resid <- val[[2L]]
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    structure(list(coef = coef, sigma2 = sigma2, var.coef = var, mask = mask,
		   loglik = -0.5 * value, aic = aic, arma = arma,
		   residuals = resid, call = match.call(), series = series,
		   code = res$convergence, n.cond = ncond, nobs = n.used,
		   model = mod),
	      class = "Arima")
}


print.Arima <-
    function (x, digits = max(3L, getOption("digits") - 3L), se = TRUE, ...)
{
    cat("\nCall:", deparse(x$call, width.cutoff = 75L), "", sep = "\n")
    if (length(x$coef)) {
        cat("Coefficients:\n")
        coef <- round(x$coef, digits = digits)
        ## use NROW as if all coefs are fixed there are no var.coef's
        if (se && NROW(x$var.coef)) {
            ses <- rep.int(0, length(coef))
            ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
            coef <- matrix(coef, 1L, dimnames = list(NULL, names(coef)))
            coef <- rbind(coef, s.e. = ses)
        }
        print.default(coef, print.gap = 2)
    }
    cm <- x$call$method
    if(is.null(cm) || cm != "CSS")
        cat("\nsigma^2 estimated as ", format(x$sigma2, digits = digits),
            ":  log likelihood = ", format(round(x$loglik, 2L)),
            ",  aic = ", format(round(x$aic, 2L)), "\n", sep = "")
    else
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  part log likelihood = ", format(round(x$loglik,2)),
            "\n", sep = "")
    invisible(x)
}


predict.Arima <-
    function (object, n.ahead = 1L, newxreg = NULL, se.fit = TRUE, ...)
{
    myNCOL <- function(x) if (is.null(x)) 0 else NCOL(x)
    rsd <- object$residuals
    xr <- object$call$xreg
    xreg <- if (!is.null(xr)) eval.parent(xr) else NULL
    ncxreg <- myNCOL(xreg)
    if (myNCOL(newxreg) != ncxreg)
        stop("'xreg' and 'newxreg' have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(rsd)
    n <- length(rsd)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1L:4L])
    if (length(coefs) > narma) {
        if (names(coefs)[narma + 1L] == "intercept") {
            xreg <- cbind(intercept = rep(1, n), xreg)
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg + 1L
        }
        xm <- if(narma == 0) drop(as.matrix(newxreg) %*% coefs)
        else drop(as.matrix(newxreg) %*% coefs[-(1L:narma)])
    }
    else xm <- 0
    if (arma[2L] > 0L) {
        ma <- coefs[arma[1L] + 1L:arma[2L]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if (arma[4L] > 0L) {
        ma <- coefs[sum(arma[1L:3L]) + 1L:arma[4L]]
        if (any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1L]] + xm, start = xtsp[2L] + deltat(rsd),
               frequency = xtsp[3L])
    if (se.fit) {
        se <- ts(sqrt(z[[2L]] * object$sigma2),
                 start = xtsp[2L] + deltat(rsd),
                 frequency = xtsp[3L])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}


makeARIMA <- function(phi, theta, Delta, kappa = 1e6,
		      SSinit = c("Gardner1980", "Rossignol2011"),
		      tol = .Machine$double.eps)
{
    if(anyNA(phi))   warning(gettextf("NAs in '%s'", "phi"), domain=NA)
    if(anyNA(theta)) warning(gettextf("NAs in '%s'", "theta"), domain=NA)
    p <- length(phi); q <- length(theta)
    r <- max(p, q + 1L); d <- length(Delta)
    rd <- r + d
    Z <- c(1., rep.int(0, r-1L), Delta)
    T <- matrix(0., rd, rd)
    if(p > 0) T[1L:p, 1L] <- phi
    if(r > 1L) {
        ind <- 2:r
        T[cbind(ind-1L, ind)] <- 1
    }
    if(d > 0L) {
        T[r+1L, ] <- Z
        if(d > 1L) {
            ind <- r + 2:d
            T[cbind(ind, ind-1)] <- 1
        }
    }
    if(q < r - 1L) theta <- c(theta, rep.int(0, r-1L-q))
    R <- c(1, theta, rep.int(0, d))
    V <- R %o% R
    h <- 0.
    a <- rep(0., rd)
    Pn <- P <- matrix(0., rd, rd)
    if(r > 1L)
        Pn[1L:r, 1L:r] <- switch(match.arg(SSinit),
                                 "Gardner1980" = .Call(C_getQ0, phi, theta),
                                 "Rossignol2011" = .Call(C_getQ0bis, phi, theta, tol),
                                 stop("invalid 'SSinit'"))
    else Pn[1L, 1L] <- if(p > 0) 1/(1 - phi^2) else 1
    if(d > 0L) Pn[cbind(r+1L:d, r+1L:d)] <- kappa
    list(phi=phi, theta=theta, Delta=Delta, Z=Z, a=a, P=P, T=T, V=V,
         h=h, Pn=Pn)
}

coef.Arima <- function (object, ...) object$coef

vcov.Arima <- function (object, ...) object$var.coef

logLik.Arima <- function (object, ...) {
    res <- if(is.na(object$aic)) NA
    else structure(object$loglik, df = sum(object$mask) + 1, nobs = object$nobs)
    class(res) <- "logLik"
    res
}

## arima.sim() is in ./ts.R
