ARMAacf <- function(ar = numeric(0), ma = numeric(0), lag.max = r,
                    pacf = FALSE)
{
    p <- length(ar)
    q <- length(ma)
    if(!p && !q) stop("empty model supplied")
    r <- max(p, q + 1)
    if(p > 0) {
        if(r > 1) {
            if(r > p) { ## pad with zeros so p >= q+1
                ar <- c(ar, rep(0, r - p))
                p <- r
            }
            A <- matrix(0, p + 1, 2 * p + 1)
            ind <- as.matrix(expand.grid(1:(p + 1), 1:(p+1)))[, 2:1]
            ind[, 2] <- ind[, 1] + ind[, 2] - 1
            A[ind] <- c(1, -ar)
            A[,  1:p] <- A[, 1:p] + A[, (2 * p + 1):(p + 2)]
            rhs <- c(1, rep(0,p))
            if(q > 0) {
                psi <- c(1, ARMAtoMA(ar, ma, q))
                theta <- c(1, ma, rep(0, q+1))
                for(k in 1 + 0:q) rhs[k] <- sum(psi * theta[k + 0:q])
            }
            ind <- (p+1):1
            Acf <- solve(A[ind, ind], rhs)
	    Acf <- Acf[-1]/Acf[1]
        } else Acf <- ar
        if(lag.max > p) {
            xx <- rep(0, lag.max - p)
            Acf <- c(Acf, filter(xx, ar, "recursive", init = rev(Acf)))
        }
        Acf <- c(1, Acf[1:lag.max])
    } else if(q > 0) {
        x <- c(1, ma)
        Acf <- filter(c(x, rep(0, q)), rev(x), sides=1)[-(1:q)]
        if(lag.max > q) Acf <- c(Acf, rep(0, lag.max - q))
        Acf <- Acf/Acf[1]
    }
    names(Acf) <- 0:lag.max
    if(pacf) .C("uni_pacf", as.double(Acf), pacf = double(lag.max),
                as.integer(lag.max), PACKAGE = "stats")$pacf
    else Acf
}

acf2AR <- function(acf)
{
    r <- as.double(drop(acf))
    order.max <- length(r) - 1
    if(order.max <= 0) stop("'acf' must be of length two or more")
    z <- .Fortran("eureka", as.integer(order.max), r, r,
                  coefs = double(order.max^2), vars = double(order.max),
                  double(order.max), PACKAGE = "stats")
    nm <- paste("ar(",1:order.max, ")", sep="")
    matrix(z$coefs, order.max, order.max, dimnames=list(nm, 1:order.max))
}

ARMAtoMA <- function(ar = numeric(0), ma = numeric(0), lag.max)
    .Call("ARMAtoMA", as.double(ar), as.double(ma), as.integer(lag.max),
          PACKAGE ="stats")
