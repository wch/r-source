arima0 <- function(x, order=c(0,0,0),
                   seasonal=list(order=c(0,0,0), period=NA), xreg=NULL,
                   include.mean=TRUE, na.action=na.fail, delta=0.01,
                   transform.pars=2)
{
    series <- deparse(substitute(x))
    if(NCOL(x) > 1)
        stop("only implemented for univariate time series")
    x <- na.action(as.ts(x))
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
    if(is.null(xreg)) {
        ncxreg <- 0
    } else {
        if(NROW(xreg) != n) stop("lengths of x and xreg do not match")
        ncxreg <- NCOL(xreg)
    }
    class(xreg) <- NULL
    if(include.mean && (nd==0)) {
        if(is.matrix(xreg) && is.null(colnames(xreg)))
            colnames(x) <- paste("xreg", 1:ncxreg, sep="")
        xreg <- cbind(intercept=rep(1, n), xreg=xreg)
        ncxreg <- ncxreg+1
    }
    if(ncxreg) {
        if(d <- order[2]) xreg <- diff(xreg, 1, d)
        if(d <- seasonal$order[2]) xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
    }
    .C("setup_starma",
       as.integer(arma), as.double(x), as.integer(n.used),
       as.double(xreg), as.integer(ncxreg), as.double(delta),
       as.integer(transform.pars > 0), PACKAGE="ts")
    init <- rep(0, sum(arma[1:4]))
    if(ncxreg > 0)
        init <- c(init, coef(lm(x ~ xreg+0)))
    res <- nlm(arma0f, init, hessian=transform.pars < 2)
    if(res$code > 2)
        warning(paste("possible convergence problem: nlm gave code=",
                      res$code))
    coef <- res$estimate
    if(transform.pars) coef <- .C("dotrans", coef, new=coef, PACKAGE="ts")$new
    .C("free_starma", PACKAGE="ts")
    if(transform.pars == 2) {
        .C("setup_starma",
           as.integer(arma), as.double(x), as.integer(n.used),
           as.double(xreg), as.integer(ncxreg), as.double(delta),
           as.integer(0), PACKAGE="ts")
        res <- nlm(arma0f, coef, hessian=TRUE)
        coef <- res$estimate
    }
    sigma2 <- .C("get_s2", res=double(1), PACKAGE="ts")$res
    resid <- .C("get_resid", res=double(n.used), PACKAGE="ts")$res
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    nm <- NULL
    if(arma[1] > 0) nm <- c(nm, paste("ar", 1:arma[1], sep=""))
    if(arma[2] > 0) nm <- c(nm, paste("ma", 1:arma[2], sep=""))
    if(arma[3] > 0) nm <- c(nm, paste("sar", 1:arma[3], sep=""))
    if(arma[4] > 0) nm <- c(nm, paste("sma", 1:arma[4], sep=""))
    if(ncxreg > 0)
        if(!is.null(cn <- colnames(xreg))) nm <- c(nm, cn)
        else nm <- c(nm, paste("xreg", 1:ncxreg, sep=""))
    names(coef) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", "sdiff")
    var <- solve(res$hessian*length(x))
    dimnames(var) <- list(nm, nm)
    if(transform.pars == 1) {
        if(ncxreg > 0) {
            ind <- sum(arma[1:4]) + 1:ncxreg
            var <- var[ind, ind, drop=FALSE]
        } else var <- matrix(NA, 0, 0)
    }
    value <- 2 * n.used * res$minimum + n.used + n.used*log(2*pi)
    aic <- value + 2*length(coef)
    res <- list(coef = coef, sigma2 = sigma2, var.coef = var,
                loglik = -0.5*value, aic = aic, arma = arma, resid = resid,
                call = match.call(), series = series, code = res$code)
    class(res) <- "arima0"
    res
}


arma0f <- function(p)
{
    .C("arma0fa", as.double(p), res=double(1), PACKAGE="ts")$res
}

print.arima0 <- function(x, digits = max(3, .Options$digits - 3),
                         se=TRUE, ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Coefficients:\n")
    coef <- round(x$coef, digits = digits)
    print.default(coef, print.gap = 2)
    if(se && nrow(x$var.coef)) {
        ses <- round(sqrt(diag(x$var.coef)), digits = digits)
        names(ses) <- rownames(x$var.coef)
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
    data <- eval(parse(text=object$series))
    xr <- object$call$xreg
    xreg <- if(!is.null(xr)) eval(parse(text=xr)) else NULL
    ncxreg <- NROW(xreg)
    if(NROW(newxreg) != ncxreg)
        stop("xreg and newxreg have different numbers of columns")
    class(xreg) <- NULL
    xtsp <- tsp(data)
    n <- length(data)
    arma <- object$arma
    coefs <- object$coef
    narma <- sum(arma[1:4])
    if(length(coefs) > narma) {
        if(names(coefs)[narma+1] == "intercept") {
            xreg <- cbind(intercept=rep(1, n), xreg)
            newxreg <- cbind(intercept=rep(1, n.ahead), newxreg)
            ncxreg <- ncxreg+1
        }
        data <- data - xreg %*% coefs[-(1:narma)]
        xm <- drop(newxreg %*% coefs[-(1:narma)])
    } else xm <- 0
    ## check invertibility of MA part(s)
    if(arma[2] > 0) {
        ma <- coefs[arma[1]+1:arma[2]]
        if(any(Mod(polyroot(c(1, ma)))) < 1)
            warning("ma part of model is not invertible")
    }
    if(arma[4] > 0) {
        ma <- coefs[sum(arma[1:3])+1:arma[4]]
        if(any(Mod(polyroot(c(1, ma)))) < 1)
            warning("seasonal ma part of model is not invertible")
    }
    .C("setup_starma",
       as.integer(arma), as.double(data),
       as.integer(n),
       as.double(rep(0, n)), as.integer(0), as.double(-1), as.integer(0),
       PACKAGE="ts")
    arma0f(coefs)
    z <- .C("arma0_kfore", as.integer(arma[6]), as.integer(arma[7]),
            as.integer(n.ahead), x=double(n.ahead), var=double(n.ahead),
            PACKAGE="ts")
    .C("free_starma", PACKAGE="ts")
    pred <- ts(z$x + xm, start = xtsp[2] + deltat(data), frequency=xtsp[3])
    if(se.fit) {
        se <- ts(sqrt(z$var),
                 start = xtsp[2] + deltat(data), frequency=xtsp[3])
        return(pred, se)
    } else return(pred)
}

arima0.diag <- function(fit, gof.lag=10)
{
    ## plot standarized residuals, acf of residuals, Box-Pierce p-values
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
