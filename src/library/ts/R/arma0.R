arima0 <- function(x, order=c(0,0,0),
                   seasonal=list(order=c(0,0,0), period=NA), xreg=NULL,
                   include.mean=TRUE, na.action=na.fail, delta=0.01)
{
    series <- deparse(substitute(x))
    if(is.matrix(ts))
        stop("only implemented for univariate time series")
    x <- na.action(x)
    if(is.null(seasonal$period) || is.na(seasonal$period)
       || seasonal$period == 0) seasonal$period <- frequency(x)
    arma <- c(order[-2], seasonal$order[-2], seasonal$period)
    if(d <- order[2] > 0)
        for(i in 1:d) x <- diff(x, 1)
    if(d <- seasonal$order[2] > 0)
        for(i in 1:d) x <- diff(x, seasonal$period)
    if(is.null(xreg)) {
        ncxreg <- 0
    } else {
        if(NROW(xreg) != n) stop("lengths of x and xreg do not match")
        xreg <- as.matrix(xreg)
        ncxreg <- ncol(xreg)
    }
    n <- length(x)
    if(include.mean) {
        xreg <- cbind(intercept=rep(1, n), xreg)
        ncxreg <- ncxreg+1
    }
    .C("setup_starma",
       as.integer(arma), as.double(x), as.integer(n),
       as.double(xreg), as.integer(ncxreg), as.double(delta))
    init <- rep(0, sum(arma[1:4]))
    if(ncxreg > 0) init <- c(init, coef(lm(x ~ xreg+0)))
    res <- nlm(arma0f, init, hessian=TRUE)
    .C("free_starma")
    coef <- res$estimate
    sigma2 <- .C("get_s2", res=double(1))$res
    nm <- NULL
    if(arma[1] > 0) nm <- c(nm, paste("ar", 1:arma[1], sep=""))
    if(arma[2] > 0) nm <- c(nm, paste("ma", 1:arma[2], sep=""))
    if(arma[3] > 0) nm <- c(nm, paste("sar", 1:arma[3], sep=""))
    if(arma[4] > 0) nm <- c(nm, paste("sma", 1:arma[4], sep=""))
    if(ncxreg > 0)  nm <- c(nm, colnames(xreg))
    names(coef) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period")
    var <- solve(res$hessian*length(x))
    dimnames(var) <- list(nm, nm)
    value <- 2*n*res$minimum + n + n*log(2*pi)
    aic <- value + 2*length(coef)
    data <- x
    if(ncxreg) data <- data - xreg %*% coef[-(1:sum(arma[1:4]))]
    res <- list(coef=coef, sigma2 = sigma2, var.coef=var, loglik=-0.5*value,
                aic = aic, arma=arma,
                call = match.call(), series=series, data = data)
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
    newdata <- object$data
    xtsp <- tsp(newdata)
    n <- length(newdata)
    coefs <- object$coef
    narma <- sum(object$arma[1:4])
    if(is.null(newxreg)) {
        ncxreg <- 0
    } else {
        xreg <- as.matrix(newxreg)
        ncxreg <- ncol(newxreg)
    }
    if(length(coefs) > narma && names(coefs)[narma+1] == "intercept") {
        newxreg <- cbind(intercept=rep(1, n.ahead), newxreg)
        ncxreg <- ncxreg+1
    }
    .C("setup_starma",
       as.integer(object$arma), as.double(newdata),
       as.integer(n),
       as.double(rep(0, n)), as.integer(0), as.double(-1))
    arma0f(coefs)
    z <- .C("arma0_fore", as.integer(n.ahead), x=double(n.ahead),
            var=double(n.ahead))
    .C("free_starma")
    x <- z$x
    if(length(coefs) > narma) {
        x <- x + newxreg[1:n.ahead,, drop=FALSE] %*% coefs[-(1:narma)]
    }
    pred <- ts(x, start = xtsp[2] + deltat(newdata), frequency=xtsp[3])
    if(se.fit) {
        se <- ts(sqrt(z$var * object$sigma2),
                 start = xtsp[2] + deltat(newdata), frequency=xtsp[3])
        return(pred, se)
    } else return(pred)
}

