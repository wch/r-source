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
        .Call("arma0fa", G, par, PACKAGE = "ts")
    }

    maInvert <- function(ma)
    {
        ## polyroot can't cope with leading zero.
        if(ma[length(ma)] == 0) return(ma)
        roots <- polyroot(c(1, ma))
        ind <- Mod(roots) < 1
        if(all(!ind)) return(ma)
        if(length(ma) == 1) return(1/ma)
        roots[ind] <- 1/roots[ind]
        x <- 1
        for(r in roots) x <- c(x, 0) - c(0, x)/r
        Re(x[-1])
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1)
        stop("only implemented for univariate time series")
    method <- match.arg(method)
    x <- as.ts(x)
    dim(x) <- NULL
    n <- length(x)
    if(is.null(seasonal$period) || is.na(seasonal$period)
       || seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- c(order[-2], seasonal$order[-2], seasonal$period,
              order[2], seasonal$order[2])
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
        if(NROW(xreg) != n) stop("lengths of x and xreg do not match")
        ncxreg <- NCOL(xreg)
    }
    class(xreg) <- NULL
    if(include.mean && (nd == 0)) {
        if(is.matrix(xreg) && is.null(colnames(xreg)))
            colnames(xreg) <- paste("xreg", 1:ncxreg, sep = "")
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1
    }
    if(ncxreg) {
        if(d <- order[2]) xreg <- diff(xreg, 1, d)
        if(d <- seasonal$order[2]) xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
        if(qr(na.omit(xreg))$rank < ncol(xreg)) stop("xreg is collinear")
    }
    if(any(is.na(x)) || (ncxreg && any(is.na(xreg)))) {
        ## only exact recursions handle NAs
        delta <- -1
    }

    storage.mode(x) <- storage.mode(xreg) <- "double"
    if(method == "CSS") transform.pars <- 0
    G <- .Call("setup_starma", as.integer(arma), x, n.used, xreg,
               ncxreg, delta, transform.pars > 0,
               ncond - (n - n.used), PACKAGE = "ts")
    on.exit(.Call("free_starma", G, PACKAGE = "ts"))
    .Call("Starma_method", G, method == "CSS", PACKAGE = "ts")
    narma <- sum(arma[1:4])
    init0 <- rep(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg > 0) {
        fit <- lm(x ~ xreg - 1)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coef[,2]
        parscale <- c(parscale, 1/ses)
    }
    if(!is.null(init)) {
        if(length(init) != length(init0))
            stop("`init' is of the wrong length")
        if(any(ind <- is.na(init))) init[ind] <- init0[ind]
        if(transform.pars)
            init <- .Call("Invtrans", G, as.double(init), PACKAGE = "ts")
    } else init <- init0

    if (is.null(fixed)) fixed <- rep(NA, length(init))
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    .Call("Starma_method", G, method == "CSS", PACKAGE = "ts")
    if(!("parscale" %in% names(optim.control)))
       optim.control$parscale <- parscale[mask]
    res <- optim(init[mask], arma0f, method = "BFGS",
                 hessian = TRUE, control = optim.control)
    if((code <- res$convergence) > 0)
        warning(paste("possible convergence problem: optim gave code=",
                      code))
    coef <- res$par

    if(transform.pars) {
        ## enforce invertibility
        cf <- fixed
        cf[mask] <- coef
        if(arma[2] > 0) {
            ind <- arma[1] + 1:arma[2]
            if(all(mask[ind]))
                cf[ind] <- maInvert(cf[ind])
        }
        if(arma[4] > 0) {
            ind <- sum(arma[1:3]) + 1:arma[4]
            if(all(mask[ind]))
                cf[ind] <- maInvert(cf[ind])
        }
        if(cf[mask] != res$par)  {  # need to re-fit
            res <- optim(cf[mask], arma0f, method = "BFGS", hessian = TRUE,
                         control = list(maxit = 0,
                         parscale = optim.control$parscale))
            coef <- res$par
        }
        cf <- fixed
        cf[mask] <- coef
        ## do it this way to ensure hessian was computed inside
        ## stationarity region
        A <- .Call("Gradtrans", G, as.double(cf), PACKAGE = "ts")[mask, mask]
        var <- t(A) %*% solve(res$hessian*length(x)) %*% A
        coef <- .Call("Dotrans", G, as.double(cf), PACKAGE = "ts")[mask]
        .Call("set_trans", G, 0, PACKAGE = "ts")
    } else var <- solve(res$hessian*length(x))
    arma0f(coef)  # reset pars
    sigma2 <- .Call("get_s2", G, PACKAGE = "ts")
    resid <- .Call("get_resid", G, PACKAGE = "ts")
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    nm <- NULL
    if(arma[1] > 0) nm <- c(nm, paste("ar", 1:arma[1], sep = ""))
    if(arma[2] > 0) nm <- c(nm, paste("ma", 1:arma[2], sep = ""))
    if(arma[3] > 0) nm <- c(nm, paste("sar", 1:arma[3], sep = ""))
    if(arma[4] > 0) nm <- c(nm, paste("sma", 1:arma[4], sep = ""))
    if(ncxreg > 0)
        if(!is.null(cn <- colnames(xreg))) nm <- c(nm, cn)
        else nm <- c(nm, paste("xreg", 1:ncxreg, sep = ""))
    fixed[mask] <- coef
    names(fixed) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", "sdiff")
    dimnames(var) <- list(nm[mask], nm[mask])
    value <- 2 * n.used * res$value + n.used + n.used*log(2*pi)
    aic <- if(method != "CSS") value + 2*length(coef) else NA
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
    print.default(coef, print.gap = 2)
    if(se && nrow(x$var.coef)) {
        ses <- round(sqrt(diag(x$var.coef)), digits = digits)
        names(ses) <- rownames(x$var.coef)
        cat("\nApprox standard errors:\n")
        print.default(ses, print.gap = 2)
    }
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
            ": part log likelihood = ", format(round(x$loglik,2)),
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
        stop("xreg and newxreg have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(data)
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
        if(any(Mod(polyroot(c(1, ma)))) < 1)
            warning("ma part of model is not invertible")
    }
    if(arma[4] > 0) {
        ma <- coefs[sum(arma[1:3]) + 1:arma[4]]
        if(any(Mod(polyroot(c(1, ma)))) < 1)
            warning("seasonal ma part of model is not invertible")
    }
    storage.mode(data) <- "double"
    G <- .Call("setup_starma", as.integer(arma), data, n, rep(0, n),
               0, -1, 0, 0, PACKAGE = "ts")
    on.exit(.Call("free_starma", G, PACKAGE = "ts"))
    .Call("Starma_method", G, TRUE, PACKAGE = "ts")
    .Call("arma0fa", G, as.double(coefs), PACKAGE = "ts")
    z <- .Call("arma0_kfore", G, arma[6], arma[7], n.ahead, PACKAGE = "ts")
    pred <- ts(z[[1]] + xm, start = xtsp[2] + deltat(data),
               frequency = xtsp[3])
    if(se.fit) {
        se <- ts(sqrt(z[[2]]),
                 start = xtsp[2] + deltat(data), frequency = xtsp[3])
        return(pred, se)
    } else return(pred)
}

arima0.diag <- function(fit, gof.lag = 10)
{
    ## plot standardized residuals, acf of residuals, Box-Pierce p-values
    oldpar<- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    stdres <- fit$resid/sqrt(fit$sigma2)
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(fit$resid, plot = TRUE, main = "ACF of Residuals")
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1:nlag) pval[i] <- Box.test(fit$resid, i)$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Box-Pierce statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}
