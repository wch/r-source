#  File src/library/stats/R/contr.poly.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

contr.poly <- function (n, scores = 1:n, contrasts = TRUE, sparse = FALSE)
{
## sparse.model.matrix() may call this one with sparse=TRUE anyway ..
##     if(sparse)
## 	stop("orthogonal polynomial contrasts cannot be sparse")
    make.poly <- function(n, scores)
    {
	y <- scores - mean(scores)
	X <- outer(y, seq_len(n) - 1, "^")
	QR <- qr(X)
	z <- QR$qr
	z <- z *(row(z) == col(z))
	raw <- qr.qy(QR, z)
	Z <- sweep(raw, 2L, apply(raw, 2L, function(x) sqrt(sum(x^2))), "/",
		   check.margin=FALSE)
	colnames(Z) <- paste0("^", 1L:n - 1L)
	Z
    }

    if (is.numeric(n) && length(n) == 1L) levs <- seq_len(n)
    else {
	levs <- n
	n <- length(levs)
    }
    if (n < 2)
        stop(gettextf("contrasts not defined for %d degrees of freedom",
                      n - 1), domain = NA)
    if (n > 95)
        stop(gettextf("orthogonal polynomials cannot be represented accurately enough for %d degrees of freedom", n-1), domain = NA)
    if (length(scores) != n)
        stop("'scores' argument is of the wrong length")
    if (!is.numeric(scores) || anyDuplicated(scores))
        stop("'scores' must all be different numbers")
    contr <- make.poly(n, scores)
    if(sparse) contr <- .asSparse(contr)
    if (contrasts) {
	dn <- colnames(contr)
	dn[2:min(4,n)] <- c(".L", ".Q", ".C")[1:min(3, n-1)]
	colnames(contr) <- dn
	contr[, -1, drop = FALSE]
    }
    else {
	contr[, 1] <- 1
	contr
    }
}

poly <- function(x, ..., degree = 1, coefs = NULL, raw = FALSE)
{
    dots <- list(...)
    if(nd <- length(dots)) {
        if(nd == 1 && length(dots[[1L]]) == 1L) # unnamed degree
            degree <- dots[[1L]]
        else return(polym(x, ..., degree = degree, raw = raw))
    }
    if(is.matrix(x)) {
        m <- unclass(as.data.frame(cbind(x, ...)))
        return(do.call("polym", c(m, degree = degree, raw = raw)))
    }
    if(degree < 1)
        stop("'degree' must be at least 1")
    if(anyNA(x)) stop("missing values are not allowed in 'poly'")
    n <- degree + 1
    if(raw) {
        Z <- outer(x, 1L:degree, "^")
        colnames(Z) <- 1L:degree
        attr(Z, "degree") <- 1L:degree
        class(Z) <- c("poly", "matrix")
        return(Z)
    }
    if(is.null(coefs)) { # fitting
        if(degree >= length(unique(x)))
            stop("'degree' must be less than number of unique points")
        xbar <- mean(x)
        x <- x - xbar
        X <- outer(x, seq_len(n) - 1, "^")
        QR <- qr(X)
        if(QR$rank < degree)
            stop("'degree' must be less than number of unique points")
        z <- QR$qr
        z <- z * (row(z) == col(z))
        raw <- qr.qy(QR, z)
        norm2 <- colSums(raw^2)
        alpha <- (colSums(x*raw^2)/norm2 + xbar)[1L:degree]
        Z <- raw / rep(sqrt(norm2), each = length(x))
        colnames(Z) <- 1L:n - 1L
        Z <- Z[, -1, drop = FALSE]
        attr(Z, "degree") <- 1L:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = c(1, norm2))
        class(Z) <- c("poly", "matrix")
    } else {            # prediction
        alpha <- coefs$alpha; norm2 <- coefs$norm2
        Z <- matrix(, length(x), n)
        Z[, 1] <- 1
        Z[, 2] <- x - alpha[1L]
        if(degree > 1)
            for(i in 2:degree)
                Z[, i+1] <- (x - alpha[i]) * Z[, i]  -
                    (norm2[i+1] / norm2[i]) * Z[, i-1]
        Z <- Z / rep(sqrt(norm2[-1L]), each = length(x))
        colnames(Z) <- 0:degree
        Z <- Z[, -1, drop = FALSE]
        ## we may want to use the prediction to clone another prediction
        attr(Z, "degree") <- 1L:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
        class(Z) <- c("poly", "matrix")
    }
    Z
}

predict.poly <- function(object, newdata, ...)
{
    if(missing(newdata)) return(object)
    if(is.null(attr(object, "coefs")))
       poly(newdata, degree = max(attr(object, "degree")), raw = TRUE)
    else
       poly(newdata, degree = max(attr(object, "degree")),
            coefs = attr(object, "coefs"))
}

makepredictcall.poly  <- function(var, call)
{
    if(as.character(call)[1L] != "poly") return(call)
    call$coefs <- attr(var, "coefs")
    call
}

polym <- function(..., degree = 1, raw = FALSE)
{
    dots <- list(...)
    nd <- length(dots)
    if(nd == 0) stop("must supply one or more vectors")
    if(nd == 1) return(poly(dots[[1L]], degree, raw = raw))
    n <- lengths(dots)
    if(any(n != n[1L]))
        stop("arguments must have the same length")
    z <- do.call("expand.grid", rep.int(list(0:degree), nd))
    s <- rowSums(z)
    ind <- (s > 0) & (s <= degree)
    z <- z[ind, ]; s <- s[ind]
    res <- cbind(1, poly(dots[[1L]], degree, raw = raw))[, 1 + z[, 1]]
    for(i in 2:nd)
        res <- res * cbind(1, poly(dots[[i]], degree, raw = raw))[, 1 + z[, i]]
    colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
    attr(res, "degree") <- as.vector(s)
    res
}
