StructTS <- function(x, type = c("level", "trend", "BSM"),
                     init = NULL, fixed = NULL, optim.control = NULL)
{
    KalmanLike2 <- function (y, mod, nit = 0)
    {
        x <- .Call(R_KalmanLike, y, mod$Z, mod$a, mod$P, mod$T, mod$V,
                   mod$h, mod$Pn, as.integer(nit), FALSE, fast=TRUE)
        0.5 * sum(x)/length(y)
    }
    makeLevel <- function(x)
    {
        T <- matrix(1, 1, 1)
        Z <- 1
        xm <- if(is.na(x[1])) mean(x, na.rm = TRUE) else x[1]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- xm
        P <- Pn <- matrix(0, 1, 1)
        h <- 1
        V <- diag(1)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    makeTrend <- function(x)
    {
        T <- matrix(c(1,0,1,1), 2, 2)
        Z <- c(1, 0)
        xm <- if(is.na(x[1])) mean(x, na.rm = TRUE) else x[1]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, 0)
        P <- Pn <- matrix(0, 2, 2)
        h <- 1
        V <- diag(2)
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    makeBSM <- function(x, nf)
    {
        if(nf <= 1) stop("frequency must be a positive integer for BSM")
        T <- matrix(0, nf + 1, nf + 1)
        T[1:2, 1:2] <- c(1, 0, 1, 1)
        T[3, ] <- c(0, 0, rep(-1, nf - 1))
        ind <- 3:nf
        T[cbind(ind+1, ind)] <- 1
        Z <- c(1, 0, 1, rep(0, nf - 2))
        xm <- if(is.na(x[1])) mean(x, na.rm = TRUE) else x[1]
        if(is.na(xm)) stop("the series is entirely NA")
        a <- c(xm, rep(0, nf))
        P <- Pn <- matrix(0, nf+1, nf+1)
        h <- 1
        V <- diag(c(1, 1, 1, rep(0, nf-2)))
        return(list(Z=Z, a=a, P=P, T=T, V=V, h=h, Pn=Pn))
    }
    getLike <- function(par)
    {
        p <- cf
        p[mask] <- par
        if(all(p == 0)) return(1000)
        Z$V[cbind(1:np, 1:np)] <- p[-(np+1)]*vx
        Z$h <- p[np+1]*vx
        Z$P[] <- 1e6*vx
        Z$a <- a0
        res <- KalmanLike2(y, Z, -1)
#        print(c(res, p))
        res
    }

    series <- deparse(substitute(x))
    if(NCOL(x) > 1)
        stop("only implemented for univariate time series")
    x <- as.ts(x)
    if(!is.numeric(x))
        stop("'x' must be numeric")
    storage.mode(x) <- "double"
    if(is.na(x[1]))
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
    np <- switch(type, "level" = 1, "trend" = 2, "BSM" = 3)
    if (is.null(fixed)) fixed <- rep(NA_real_, np+1)
    mask <- is.na(fixed)
    if(!any(mask)) stop("all parameters were fixed")
    cf <- fixed/vx
    if(is.null(init)) init <- rep(1, np+1) else init <- init/vx

    y <- x
    res <- optim(init[mask], getLike, method = "L-BFGS-B",
                 lower = rep(0, np+1), upper = rep(Inf, np+1),
                 control = optim.control)
        if(res$convergence > 0)
            warning("possible convergence problem: optim gave code=",
                    res$convergence, " ", res$message)
    coef <- cf
    coef[mask] <- res$par
    Z$V[cbind(1:np, 1:np)] <- coef[1:np]*vx
    Z$h <- coef[np+1]*vx
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
    if(type == "BSM") states <- states[, 1:3]
    dimnames(states) <- list(time(x), cn)
    states <- ts(states, start = xtsp[1], frequency = nf)

    coef <- coef*vx
    names(coef) <- switch(type,
                          "level" = c("level", "epsilon"),
                          "trend" = c("level", "slope", "epsilon"),
                          "BSM" = c("level", "slope", "seas", "epsilon")
                          )
    loglik <- -length(y) * res$value + length(y) * log(2 * pi)
    res <- list(coef = coef, loglik = loglik, data = y,
                residuals = resid, fitted = states,
                call = match.call(), series = series,
                code = res$convergence, model = Z, model0 = Z0, xtsp = xtsp)
    class(res) <- "StructTS"
    res
}

print.StructTS <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:", deparse(x$call, width = 75), "", sep = "\n")
    cat("Variances:\n")
    print.default(x$coef, print.gap = 2, digits=digits)
    invisible(x)
}

predict.StructTS <- function(object, n.ahead = 1, se.fit = TRUE, ...)
{
    xtsp <- object$xtsp
    z <- KalmanForecast(n.ahead, object$model)
    pred <- ts(z[[1]], start = xtsp[2] + 1/xtsp[3], frequency = xtsp[3])
    if (se.fit) {
        se <- ts(sqrt(z[[2]]), start = xtsp[2] + 1/xtsp[3],
                 frequency = xtsp[3])
        return(list(pred=pred, se=se))
    }
    else return(pred)
}

tsdiag.StructTS <- function(object, gof.lag = 10, ...)
{
    ## plot standardized residuals, acf of residuals, Ljung-Box p-values
    oldpar<- par(mfrow = c(3, 1))
    on.exit(par(oldpar))
    rs <- object$resid
    stdres <- rs
    plot(stdres, type = "h", main = "Standardized Residuals", ylab = "")
    abline(h = 0)
    acf(object$resid, plot = TRUE, main = "ACF of Residuals",
        na.action = na.pass)
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1:nlag) pval[i] <- Box.test(rs, i, type = "Ljung-Box")$p.value
    plot(1:nlag, pval, xlab = "lag", ylab = "p value", ylim = c(0,1),
         main = "p values for Ljung-Box statistic")
    abline(h = 0.05, lty = 2, col = "blue")
}


tsSmooth <- function(object, ...) UseMethod("tsSmooth")

tsSmooth.StructTS <- function(object, ...)
{
    res <- KalmanSmooth(object$data, object$model0, -1)$smooth
    dn <- dim(fitted(object))
    res <- res[, 1:dn[2], drop = FALSE]
    dimnames(res) <- dimnames(fitted(object))
    ts(res, start = object$xtsp[1], frequency = object$xtsp[3])
}
