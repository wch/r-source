arima0 <- function(x, order=c(0,0,0),
                   seasonal=list(order=c(0,0,0), period=NA), xreg=NULL,
                   include.mean=(nd==0), na.action=na.fail, delta=0.01)
{
    series <- deparse(substitute(x))
    if(is.matrix(ts))
        stop("only implemented for univariate time series")
    x <- na.action(as.ts(x))
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
    if(is.null(xreg)) {
        ncxreg <- 0
    } else {
        if(NROW(xreg) != n.used) stop("lengths of x and xreg do not match")
        xreg <- as.matrix(xreg)
        ncxreg <- ncol(xreg)
    }
    if(include.mean) {
        xreg <- cbind(intercept=rep(1, n.used), xreg)
        ncxreg <- ncxreg+1
    }
    .C("setup_starma",
       as.integer(arma), as.double(x), as.integer(n.used),
       as.double(xreg), as.integer(ncxreg), as.double(delta))
    init <- rep(0, sum(arma[1:4]))
    if(ncxreg > 0) {
        init <- c(init, coef(lm(x ~ xreg+0)))
    }
    res <- nlm(arma0f, init, hessian=TRUE)
    .C("free_starma")
    coef <- res$estimate
    sigma2 <- .C("get_s2", res=double(1))$res
    resid <- .C("get_resid", res=double(n.used))$res
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    nm <- NULL
    if(arma[1] > 0) nm <- c(nm, paste("ar", 1:arma[1], sep=""))
    if(arma[2] > 0) nm <- c(nm, paste("ma", 1:arma[2], sep=""))
    if(arma[3] > 0) nm <- c(nm, paste("sar", 1:arma[3], sep=""))
    if(arma[4] > 0) nm <- c(nm, paste("sma", 1:arma[4], sep=""))
    if(ncxreg > 0)  nm <- c(nm, colnames(xreg))
    names(coef) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", "sdiff")
    var <- solve(res$hessian*length(x))
    dimnames(var) <- list(nm, nm)
    value <- 2 * n.used * res$minimum + n.used + n.used*log(2*pi)
    aic <- value + 2*length(coef)
    data <- x
    if(ncxreg) data <- data - xreg %*% coef[-(1:sum(arma[1:4]))]
    res <- list(coef = coef, sigma2 = sigma2, var.coef = var,
                loglik = -0.5*value, aic = aic, arma = arma, resid = resid,
                call = match.call(), series = series)
    class(res) <- "arima0"
    res
}


arma0f <- function(p)
{
    .C("arma0fa", as.double(p), res=double(1))$res
}

print.arima0 <- function(x, digits = max(3, .Options$digits - 3),
                         se=TRUE, ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Coefficients:\n")
    coef <- round(x$coef, digits = digits)
    print.default(coef, print.gap = 2)
    if(se) {
    ses <- round(sqrt(diag(x$var.coef)), digits = digits)
    names(ses) <- names(coef)
    cat("\nApprox standard errors:\n")
    print.default(ses, print.gap = 2)
    }
    cat("\nsigma^2 estimated as ",
        format(x$sigma2, digits = digits),
        ":  log likelihood = ", format(round(x$loglik,2)),
        ",  aic = ", format(round(x$aic,2)),
        "\n", sep="")
    invisible(x)
}

predict.arima0 <- function(object, n.ahead=1, newxreg=NULL, se.fit=TRUE)
{
    orig <- data <- eval(parse(text=object$series))
    xr <- object$call$xreg
    xreg <- if(!is.null(xr)) eval(parse(text=xr)) else NULL
    xtsp <- tsp(data)
    n <- length(data)
    arma <- object$arma
    if(d <- arma[6])
        for(i in 1:d) data <- diff(data, 1)
    if(d <- arma[7])
        for(i in 1:d) data <- diff(data, arma[5])
    n.used <- length(data)
    coefs <- object$coef
    narma <- sum(arma[1:4])
    if(is.null(newxreg)) {
        ncxreg <- 0
    } else {
        xreg <- as.matrix(newxreg)
        ncxreg <- ncol(newxreg)
    }
    if(length(coefs) > narma) {
        if(names(coefs)[narma+1] == "intercept") {
            xreg <- cbind(intercept=rep(1, n.used), xreg)
            newxreg <- cbind(intercept=rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg+1
        }
        data <- data - xreg %*% coefs[-(1:narma)]
    }
    .C("setup_starma",
       as.integer(arma), as.double(data),
       as.integer(n.used),
       as.double(rep(0, n.used)), as.integer(0), as.double(-1))
    arma0f(coefs)
    z <- .C("arma0_fore", as.integer(n.ahead), x=double(n.ahead),
            var=double(n.ahead))
    .C("free_starma")
    x <- z$x
    if(length(coefs) > narma) {
        x <- x + newxreg[1:n.ahead,, drop=FALSE] %*% coefs[-(1:narma)]
    }
    if(nd <- arma[6]+arma[7]) { ## undifference
        X <- matrix(0, n + n.ahead, nd + 1)
        X[1:n, 1] <- orig
        X[n+(1:n.ahead), nd+1] <- x
        xx <- matrix(.C("arima0_fore",
                as.integer(n.ahead),
                as.integer(n),
                X = as.double(X),
                as.integer(c(rep(1, arma[6]), rep(arma[5], arma[7]))),
                as.integer(nd))$X, , nd+1)
        x <- xx[n+(1:n.ahead), 1]
    }
    pred <- ts(x, start = xtsp[2] + deltat(data), frequency=xtsp[3])
    if(se.fit && !nd) {
        se <- ts(sqrt(z$var * object$sigma2),
                 start = xtsp[2] + deltat(data), frequency=xtsp[3])
        return(pred, se)
    } else if(se.fit && nd) {
        npsi <- n.ahead - 1
        psi <- .C("arimatoma",
                  as.integer(arma),
                  as.double(coefs),
                  psi=double(n.ahead),
                  as.integer(npsi))$psi[1:npsi]
        vars <- cumsum(c(1, psi^2))
        se <- sqrt(object$sigma2 * vars)
        se <- ts(se, start = xtsp[2] + deltat(data), frequency=xtsp[3])
        return(pred, se)
    } else return(pred)
}

arima0.diag <- function(fit, gof.lag=10)
{
    ## plot standarized residuals, acf of residuals, Box-Pierce
    oldpar<- par(mfrow=c(3,1))
    on.exit(par(oldpar))
    stdres <- fit$resid/sqrt(fit$sigma2)
    plot(stdres, type="h", main="Standardized Residuals", ylab="")
    abline(h=0)
    acf(fit$resid, plot=TRUE, main="ACF of Residuals")
    nlag <- gof.lag
    pval <- numeric(nlag)
    for(i in 1:nlag) pval[i] <- Box.test(fit$resid, i)$p.value
    plot(1:nlag, pval, xlab="lag", ylab="p value", ylim=c(0,1),
         main="p values for Box-Pierce statistic")
    abline(h=0.05, lty=2, col="blue")
}
