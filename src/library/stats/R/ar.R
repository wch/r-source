## based on, especially multivariate case, code by Martyn Plummer
ar <-
    function (x, aic = TRUE, order.max = NULL,
              method=c("yule-walker","burg", "ols", "mle", "yw"),
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
        x <- sweep(x, 2, xm)
    } else xm <- rep(0, nser)
    n.used <- nrow(x)
    order.max <- if (is.null(order.max)) floor(10 * log10(n.used))
                 else round(order.max)
    if (order.max < 1) stop("'order.max' must be >= 1")
    xacf <- acf(x, type = "covariance", lag.max = order.max, plot = FALSE,
                demean = demean)$acf
    if(nser > 1) {
        ## multivariate case
        snames <- colnames(x)
        A <- B <- array(0, dim = c(order.max + 1, nser, nser))
        A[1, , ] <- B[1, , ] <- diag(nser)
        EA <- EB <- xacf[1, , , drop = TRUE]
        partialacf <- array(dim = c(order.max, nser, nser))
        xaic <- numeric(order.max + 1)
        solve.yw <- function(m) {
            # Solve Yule-Walker equations with Whittle's
            # generalization of the Levinson(-Durbin) algorithm
            betaA <- betaB <- 0
            for (i in 0:m) {
                betaA <- betaA + A[i + 1, , ] %*% xacf[m + 2 - i, , ]
                betaB <- betaB + B[i + 1, , ] %*% t(xacf[m + 2 - i, , ])
            }
            KA <- -t(qr.solve(t(EB), t(betaA)))
            KB <- -t(qr.solve(t(EA), t(betaB)))
            EB <<- (diag(nser) - KB %*% KA) %*% EB
            EA <<- (diag(nser) - KA %*% KB) %*% EA
            Aold <- A
            Bold <- B
            for (i in 1:(m + 1)) {
                A[i + 1, , ] <<- Aold[i + 1, , ] + KA %*% Bold[m + 2 - i, , ]
                B[i + 1, , ] <<- Bold[i + 1, , ] + KB %*% Aold[m + 2 - i, , ]
            }
        }
        cal.aic <- function() { # omits mean params, that is constant adj
            det <- abs(prod(diag(qr(EA)$qr)))
            return(n.used * log(det) + 2 * m * nser * nser)
        }
        cal.resid <- function() {
            resid <- array(0, dim = c(n.used - order, nser))
            for (i in 0:order) {
                resid <- resid + x[(order - i + 1):(n.used - i),
                                   , drop = FALSE] %*% t(ar[i + 1, , ])
            }
            return(rbind(matrix(NA, order, nser), resid))
        }
        order <- 0
        for (m in 0:order.max) {
            xaic[m + 1] <- cal.aic()
            if (!aic || xaic[m + 1] == min(xaic[1:(m + 1)])) {
                ar <- A
                order <- m
                var.pred <- EA * n.used/(n.used - nser * (m + 1))
            }
            if (m < order.max) {
                solve.yw(m)
                partialacf[m + 1, , ] <- -A[m + 2, , ]
            }
        }
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        resid <- cal.resid()
        if(order > 0 ) {
            ar <- -ar[2:(order + 1), , , drop = FALSE]
            dimnames(ar) <- list(1:order, snames, snames)
        } else ar <- array(0, dim=c(0, nser, nser),
                           dimnames=list(NULL, snames, snames))
        dimnames(var.pred) <- list(snames, snames)
        dimnames(partialacf) <- list(1:order.max, snames, snames)
        colnames(resid) <- colnames(x)
    } else {
        ## univariate case
        r <- as.double(drop(xacf))
        z <- .Fortran(R_eureka,
                      as.integer(order.max),
                      r, r,
                      coefs=double(order.max^2),
                      vars=double(order.max),
                      double(order.max))
        coefs <- matrix(z$coefs, order.max, order.max)
        partialacf <- array(diag(coefs), dim=c(order.max, 1, 1))
        var.pred <- c(r[1], z$vars)
        xaic <- n.used * log(var.pred) + 2 * (0:order.max) + 2 * demean
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        order <- if (aic) (0:order.max)[xaic == 0] else order.max
        ar <- if (order > 0) coefs[order, 1:order] else numeric(0)
        var.pred <- var.pred[order+1]
        ## Splus compatibility fix
        var.pred <- var.pred * n.used/(n.used - (order + 1))
        if(order > 0)
            resid <- c(rep(NA, order), embed(x, order+1) %*% c(1, -ar))
        else resid <- as.vector(x)
        if(ists) {
            attr(resid, "tsp") <- xtsp
            attr(resid, "class") <- "ts"
        }
    }
    res <- list(order=order, ar=ar, var.pred=var.pred, x.mean = drop(xm),
                aic = xaic, n.used=n.used, order.max=order.max,
                partialacf=partialacf, resid=resid, method = "Yule-Walker",
                series=series, frequency=xfreq, call=match.call())
    if(nser == 1 && order > 0)
        res$asy.var.coef <-
            solve(toeplitz(drop(xacf)[seq_len(order)]))*var.pred/n.used
    class(res) <- "ar"
    res
}

print.ar <- function(x, digits = max(3, getOption("digits") - 3), ...)
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    nser <- NCOL(x$var.pred)
    if(nser > 1) {
        if(!is.null(x$x.intercept))
            res <- x[c("ar", "x.intercept", "var.pred")]
        else res <- x[c("ar", "var.pred")]
        res$ar <- aperm(res$ar, c(2,3,1))
        print(res, digits=digits)
    } else {
        if(x$order > 0) {
            cat("Coefficients:\n")
            coef <- drop(round(x$ar, digits = digits))
            names(coef) <- seq_len(x$order)
            print.default(coef, print.gap = 2)
        }
        if(!is.null(xint <- x$x.intercept) && !is.na(xint))
            cat("\nIntercept: ", format(xint, digits = digits),
                " (", format(x$asy.se.coef$x.mean, digits = digits),
                ") ", "\n", sep="")
        cat("\nOrder selected", x$order, " sigma^2 estimated as ",
            format(x$var.pred, digits = digits),"\n")

    }
    invisible(x)
}

predict.ar <- function(object, newdata, n.ahead = 1, se.fit=TRUE, ...)
{
    if(missing(newdata)) {
        newdata <- eval.parent(parse(text=object$series))
        if (!is.null(nas <- object$call$na.action))
            newdata <- eval.parent(call(nas, newdata))
    }
    nser <- NCOL(newdata)
    ar <- object$ar
    p <- object$order
    st <- tsp(as.ts(newdata))[2]
    dt <- deltat(newdata)
    xfreq <- frequency(newdata)
    tsp(newdata) <- NULL
    class(newdata) <- NULL
    if(NCOL(ar) != nser)
        stop("number of series in 'object' and 'newdata' do not match")
    n <- NROW(newdata)
    if(nser > 1) {
        if(is.null(object$x.intercept)) xint <- rep(0, nser)
        else xint <- object$x.intercept
        x <- rbind(sweep(newdata, 2, object$x.mean),
                   matrix(rep(0, nser), n.ahead, nser, byrow=TRUE))
        if(p > 0) {
            for(i in 1:n.ahead) {
                x[n+i,] <- ar[1,,] %*% x[n+i-1,] + xint
                if(p > 1) for(j in 2:p)
                    x[n+i,] <- x[n+i,] + ar[j,,] %*% x[n+i-j,]
            }
            pred <- x[n+(1:n.ahead), ]
        } else {
            pred <- matrix(xint, n.ahead, nser, byrow=TRUE)
        }
        pred <- pred + matrix(object$x.mean, n.ahead, nser, byrow=TRUE)
        colnames(pred) <- colnames(object$var.pred)
        if(se.fit) {
            warning("'se.fit' not yet implemented for multivariate models")
            se <- matrix(NA, n.ahead, nser)
        }
    } else {
        if(is.null(object$x.intercept)) xint <- 0
        else xint <- object$x.intercept
        x <- c(newdata - object$x.mean, rep(0, n.ahead))
        if(p > 0) {
            for(i in 1:n.ahead) {
                x[n+i] <- sum(ar * x[n+i - (1:p)]) + xint
            }
            pred <- x[n+(1:n.ahead)]
            if(se.fit) {
                npsi <- n.ahead - 1
                psi <- .C(R_artoma,
                        as.integer(object$order), as.double(ar),
                        psi = double(npsi+object$order+1),
                        as.integer(npsi+object$order+1))$psi[1:npsi]
                vars <- cumsum(c(1, psi^2))
                se <- sqrt(object$var.pred*vars)[1:n.ahead]
            }
        } else {
            pred <- rep(xint, n.ahead)
            if (se.fit) se <- rep(sqrt(object$var.pred), n.ahead)
        }
        pred <- pred + rep(object$x.mean, n.ahead)
    }
    pred <- ts(pred, start = st + dt, frequency=xfreq)
    if(se.fit) se <- ts(se, start = st + dt, frequency=xfreq)
    if(se.fit) return(list(pred=pred, se=se)) else return(pred)
}

