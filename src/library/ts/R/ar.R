## based on, especially multivariate case, code by Martyn Plummer
ar <-
    function (x, aic = TRUE, order.max = NULL, method=c("yule-walker","burg"),
              na.action = na.fail)
{
    switch(match.arg(method),
        "yule-walker" = ar.yw(x, aic=aic, order.max=order.max,
                              na.action = na.action),
	"burg" = stop("burg method for ar not yet implemented.")
    )
}

ar.burg <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail)
    .NotYetImplemented()

ar.yw <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail)
{
    series <- deparse(substitute(x))
    x <- na.action(x)
    xfreq <- frequency(as.ts(x))
    if(ists <- is.ts(x)) xtsp <- tsp(x)
    x <- as.matrix(x)
    if(any(is.na(x))) stop("NAs in x")
    x <- sweep(x, 2, apply(x, 2, mean))
    n.used <- nrow(x)
    nser <- ncol(x)
    order.max <- if (is.null(order.max)) floor(10 * log10(n.used))
                 else round(order.max)
    if (order.max < 1) stop("order.max must be >= 1")
    xacf <- acf(x, type = "covariance", lag.max = order.max, plot=FALSE)$acf
    if(nser > 1) {
        ## multivariate case
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
        cal.aic <- function()
        {
            det <- abs(prod(diag(qr(var.pred)$qr)))
            return(n.used * log(det) + 2 * m * nser * nser)
        }
        cal.var.pred <- function()
        {
            vp <- 0
            for (i in 0:m)
                for (j in 0:m) {
                    R <- xacf[abs(i - j) + 1, , ]
                    if (j < i) R <- t(R)
                    vp <- vp + A[i + 1, , ] %*% R %*% t(A[j + 1, , ])
                }
            #Splus compatibility fix
            vp * n.used/(n.used - nser * (m + 1))
        }
        cal.resid <- function()
        {
            resid <- array(dim = c(n.used, nser))
            fitted <- array(0, dim = c(n.used - order, nser))
            if (order > 0) {
                A <- array(dim = c(nser, nser))
                for (i in 1:order) {
                    A[, ] <- ar[i, , , drop = FALSE]
                    fitted <- fitted +
                        x[(order - i + 1):(n.used - i), , drop = FALSE] %*% t(A)
                }
            }
            resid[(order + 1):n.used, ] <-
                x[(order + 1):n.used, , drop = FALSE] - fitted
            return(resid)
        }
        ar.list <- vector("list", order.max)
        for (m in 0:order.max) {
            var.pred <- cal.var.pred()
            xaic[m + 1] <- cal.aic()
            if (m < order.max) {
                solve.yw(m)
                partialacf[m + 1, , ] <- -A[m + 2, , ]
                ar.list[[m + 1]] <- -A[2:(m + 2), , , drop = FALSE]
            }
        }
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        order <- if (aic) (0:order.max)[xaic == 0] else order.max
        ar <- if (order > 0) ar.list[[order]] else array(0, dim = c(1, nser, nser))
        resid <- cal.resid()
    } else {
        ## univariate case
        r <- as.double(drop(xacf))
        z <- .Fortran("eureka",
                      as.integer(order.max),
                      r, r,
                      coefs=double(order.max^2),
                      vars=double(order.max),
                      double(order.max))
        coefs <- matrix(z$coefs, order.max, order.max)
        partialacf <- array(diag(coefs), dim=c(order.max, 1, 1))
        var.pred <- c(r[1], z$vars)
        xaic <- n.used * log(var.pred) + 2 * (0:order.max)
        xaic <- xaic - min(xaic)
        names(xaic) <- 0:order.max
        order <- if (aic) (0:order.max)[xaic == 0] else order.max
        ar <- if (order > 0) coefs[order,1:order] else 0
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
    res <- list(order=order, ar=ar, var.pred=var.pred, aic = xaic,
                n.used=n.used, order.max=order.max,
                partialacf=partialacf, resid=resid, method = "Yule-Walker",
                series=series, frequency=xfreq)
    if(nser == 1)
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[1:order]))*var.pred/n.used
    res
}
