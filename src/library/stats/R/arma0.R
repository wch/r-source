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
        .Call("arma0fa", G, par, PACKAGE = "stats")
    }

    arCheck <- function(ar)
    {
        p <- max(which(c(1, -ar) != 0)) - 1
        if(!p) return(TRUE)
        all(Mod(polyroot(c(1, -ar[1:p]))) > 1)
    }

    maInvert <- function(ma)
    {
        ## polyroot can't cope with leading zero.
        q <- length(ma)
        q0 <- max(which(c(1,ma) != 0)) - 1
        if(!q0) return(ma)
        roots <- polyroot(c(1, ma[1:q0]))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        warning("converting non-invertible initial MA values")
        if(q0 == 1) return(c(1/ma[1], rep(0, q-q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for(r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1]), rep(0, q-q0))
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
        if(!is.numeric(order) || length(order) != 3 || any(order < 0))
            stop("'order' must be a non-negative numeric vector of length 3")
    if(!missing(seasonal))
        if(is.list(seasonal)) {
            if(is.null(seasonal$order))
                stop("'seasonal' must be a list with component 'order'")
            if(!is.numeric(seasonal$order) || length(seasonal$order) != 3
               || any(seasonal$order < 0))
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        } else if(is.numeric(order)) {
            if(length(order) == 3) seasonal <- list(order=seasonal)
            else ("'seasonal' is of the wrong length")
        } else stop("'seasonal' must be a list with component 'order'")

    if(is.null(seasonal$period) || is.na(seasonal$period)
       || seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- c(order[-2], seasonal$order[-2], seasonal$period,
              order[2], seasonal$order[2])
    narma <- sum(arma[1:4])
    if(d <- order[2]) x <- diff(x, 1, d)
    if(d <- seasonal$order[2]) x <- diff(x, seasonal$period, d)
    xtsp <- tsp(x)
    tsp(x) <- NULL
    nd <- order[2] + seasonal$order[2]
    n.used <- length(x)
    ncond <- n - n.used
    if(method == "CSS") {
        ncond1 <- order[1] + seasonal$period * seasonal$order[1]
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
            colnames(xreg) <- paste("xreg", 1:ncxreg, sep = "")
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1
    }

    if (is.null(fixed)) fixed <- rep(as.numeric(NA), narma + ncxreg)
    else if(length(fixed) != narma + ncxreg) stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    if(transform.pars && any(!mask[1:narma])) {
        warning("some ARMA parameters were fixed: setting transform.pars = FALSE")
        transform.pars <- FALSE
    }

    if(ncxreg) {
        if(d <- order[2]) xreg <- diff(xreg, 1, d)
        if(d <- seasonal$order[2]) xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
        if(qr(na.omit(xreg))$rank < ncol(xreg)) stop("'xreg' is collinear")
        if(is.null(cn <- colnames(xreg)))
            cn <- paste("xreg", 1:ncxreg, sep = "")
    }
    if(any(is.na(x)) || (ncxreg && any(is.na(xreg))))
        ## only exact recursions handle NAs
        if(method == "ML" && delta >= 0) {
            warning("NAs present: setting 'delta' to -1")
            delta <- -1
        }

    init0 <- rep(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg) {
        orig.xreg <- (ncxreg == 1) || any(!mask[narma + 1:ncxreg])
        if(!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        fit <- lm(x ~ xreg - 1, na.action = na.omit)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coef[,2]
        parscale <- c(parscale, ses)
    }

    storage.mode(x) <- storage.mode(xreg) <- "double"
    if(method == "CSS") transform.pars <- 0
    G <- .Call("setup_starma", as.integer(arma), x, n.used, xreg,
               ncxreg, delta, transform.pars > 0,
               ncond - (n - n.used), PACKAGE = "stats")
    on.exit(.Call("free_starma", G, PACKAGE = "stats"))

    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("'init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(transform.pars) {
            if(any(!mask[1:narma]))
                warning("transformed ARMA parameters were fixed")
            ## check stationarity
            if(arma[1] > 0)
                if(!arCheck(init[1:arma[1]]))
                    stop("non-stationary AR part")
            if(arma[3] > 0)
                if(!arCheck(init[sum(arma[1:2]) + 1:arma[3]]))
                    stop("non-stationary seasonal AR part")
            ## enforce invertibility
            if(arma[2] > 0) {
                ind <- arma[1] + 1:arma[2]
                init[ind] <- maInvert(init[ind])
            }
            if(arma[4] > 0) {
                ind <- sum(arma[1:3]) + 1:arma[4]
                init[ind] <- maInvert(init[ind])
            }
            init <- .Call("Invtrans", G, as.double(init), PACKAGE = "stats")
        }
    } else init <- init0


    .Call("Starma_method", G, method == "CSS", PACKAGE = "stats")
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]
    res <- optim(init[mask], arma0f, method = "BFGS",
                 hessian = TRUE, control = optim.control)
    if((code <- res$convergence) > 0)
        warning("possible convergence problem: optim gave code=", code)
    coef <- res$par

    if(transform.pars) {
        cf <- fixed
        cf[mask] <- coef
        ## do it this way to ensure hessian was computed inside
        ## stationarity region
        A <- .Call("Gradtrans", G, as.double(cf), PACKAGE = "stats")[mask, mask]
        var <- t(A) %*% solve(res$hessian*length(x)) %*% A
        coef <- .Call("Dotrans", G, as.double(cf), PACKAGE = "stats")[mask]
        .Call("set_trans", G, 0, PACKAGE = "stats")
    } else var <- solve(res$hessian*length(x))
    arma0f(coef)  # reset pars
    sigma2 <- .Call("get_s2", G, PACKAGE = "stats")
    resid <- .Call("get_resid", G, PACKAGE = "stats")
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    n.used <- sum(!is.na(resid))
    nm <- NULL
    if(arma[1] > 0) nm <- c(nm, paste("ar", 1:arma[1], sep = ""))
    if(arma[2] > 0) nm <- c(nm, paste("ma", 1:arma[2], sep = ""))
    if(arma[3] > 0) nm <- c(nm, paste("sar", 1:arma[3], sep = ""))
    if(arma[4] > 0) nm <- c(nm, paste("sma", 1:arma[4], sep = ""))
    fixed[mask] <- coef
    if(ncxreg > 0) {
        nm <- c(nm, cn)
        if(!orig.xreg) {
            ind <- narma + 1:ncxreg
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

print.arima0 <- function(x, digits = max(3, getOption("digits") - 3),
                         se = TRUE, ...)
{
    cat("\nCall:", deparse(x$call, width = 75), "", sep = "\n")
    cat("Coefficients:\n")
    coef <- round(x$coef, digits = digits)
    if (se && nrow(x$var.coef)) {
        ses <- rep(0, length(coef))
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
            "\n", sep="")
    else
        cat("\nsigma^2 estimated as ",
            format(x$sigma2, digits = digits),
            ":  part log likelihood = ", format(round(x$loglik,2)),
            "\n", sep="")
    invisible(x)
}

predict.arima0 <-
    function(object, n.ahead = 1, newxreg = NULL, se.fit=TRUE, ...)
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
    narma <- sum(arma[1:4])
    if(length(coefs) > narma) {
        if(names(coefs)[narma+1] == "intercept") {
            xreg <- cbind(intercept = rep(1, n), xreg)
            newxreg <- cbind(intercept = rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg+1
        }
        data <- data - as.matrix(xreg) %*% coefs[-(1:narma)]
        xm <- drop(as.matrix(newxreg) %*% coefs[-(1:narma)])
    } else xm <- 0
    ## check invertibility of MA part(s)
    if(arma[2] > 0) {
        ma <- coefs[arma[1] + 1:arma[2]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("MA part of model is not invertible")
    }
    if(arma[4] > 0) {
        ma <- coefs[sum(arma[1:3]) + 1:arma[4]]
        if(any(Mod(polyroot(c(1, ma))) < 1))
            warning("seasonal MA part of model is not invertible")
    }
    storage.mode(data) <- "double"
    G <- .Call("setup_starma", as.integer(arma), data, n, rep(0, n),
               0, -1, 0, 0, PACKAGE = "stats")
    on.exit(.Call("free_starma", G, PACKAGE = "stats"))
    .Call("Starma_method", G, TRUE, PACKAGE = "stats")
    .Call("arma0fa", G, as.double(coefs), PACKAGE = "stats")
    z <- .Call("arma0_kfore", G, arma[6], arma[7], n.ahead, PACKAGE = "stats")
    pred <- ts(z[[1]] + xm, start = xtsp[2] + deltat(data),
               frequency = xtsp[3])
    if(se.fit) {
        se <- ts(sqrt(z[[2]]),
                 start = xtsp[2] + deltat(data), frequency = xtsp[3])
        return(list(pred=pred, se=se))
    } else return(pred)
}

arima0.diag <- function(...) .Defunct()

tsdiag.Arima <- tsdiag.arima0 <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar<- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$resid
    stdres <- rs/sqrt(object$sigma2)
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(object$resid, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1:nlag) pval[i] <- Box.test(rs, i, type="Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}

tsdiag <- function(object, gof.lag, ...) UseMethod("tsdiag")
