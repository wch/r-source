#### copyright (C) 1998 W. N. Venables and B. D. Ripley
####
#### copyright (C) 1998 The R Development Core Team.

contr.poly <- function (n, contrasts = TRUE)
{
    make.poly <- function(n)
    {
	y <- seq(length=n) - n %/% 2 - 1
	X <- outer(y, seq(length=n) - 1, "^")
	QR <- qr(X)
	z <- QR$qr
	z <- z *(row(z) == col(z))
	raw <- qr.qy(QR, z)
	Z <- sweep(raw, 2, apply(raw, 2, function(x) sqrt(sum(x^2))), "/")
	dimnames(Z)[[2]] <- paste("^", 1:n - 1, sep="")
	Z
    }
    if (is.numeric(n) && length(n) == 1) levs <- 1:n
    else {
	levs <- n
	n <- length(levs)
    }
    if (n < 2)
	stop(paste("Contrasts not defined for", n - 1, "degrees of freedom"))
    contr <- make.poly(n)
    if (contrasts) {
	dn <- dimnames(contr)[[2]]
	dn[2:min(4,n)] <- c(".L", ".Q", ".C")[1:min(3, n-1)]
	dimnames(contr)[[2]] <- dn
	contr[, -1, drop = FALSE]
    }
    else {
	contr[, 1] <- 1
	contr
    }
}

## implemented by BDR 29 May 1998
## `coefs' code added by KH
poly <- function(x, degree=1)
{
    if(is.matrix(x)) stop("poly is only implemented for vectors")
    n <- degree + 1
    xbar <- mean(x)
    x <- x - xbar
    X <- outer(x, seq(length = n) - 1, "^")
    QR <- qr(X)
    z <- QR$qr
    z <- z * (row(z) == col(z))
    raw <- qr.qy(QR, z)
    norm2 <- diag(crossprod(raw))
    alpha <- (diag(crossprod(raw, x * raw))/norm2 + xbar)[1:degree]
    Z <- raw/rep(sqrt(norm2), rep(length(x), n))
    dimnames(Z)[[2]] <- 1:n - 1
    Z <- Z[, -1]
    attr(Z, "degree") <- 1:degree
    attr(Z, "coefs") <- list(alpha = alpha, norm2 = c(1, norm2))
    Z
}
