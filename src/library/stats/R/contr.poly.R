contr.poly <- function (n, scores = 1:n, contrasts = TRUE)
{
    make.poly <- function(n, scores)
    {
	y <- scores - mean(scores)
	X <- outer(y, seq(length=n) - 1, "^")
	QR <- qr(X)
	z <- QR$qr
	z <- z *(row(z) == col(z))
	raw <- qr.qy(QR, z)
	Z <- sweep(raw, 2, apply(raw, 2, function(x) sqrt(sum(x^2))), "/")
	colnames(Z) <- paste("^", 1:n - 1, sep="")
	Z
    }

    if (is.numeric(n) && length(n) == 1) levs <- 1:n
    else {
	levs <- n
	n <- length(levs)
    }
    if (n < 2)
	stop("contrasts not defined for ", n - 1, " degrees of freedom")
    if (n > 95)
        stop("orthogonal polynomials cannot be represented accurately enough for ", n - 1, " degrees of freedom")
    if (length(scores) != n)
        stop("scores argument is of the wrong length")
    if (!is.numeric(scores) || any(duplicated(scores)))
        stop("scores must all be different numbers")
    contr <- make.poly(n, scores)
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

poly <- function(x, ..., degree = 1, coefs = NULL)
{
    dots <- list(...)
    if(nd <- length(dots)) {
        if(nd == 1 && length(dots[[1]]) == 1) # unnamed degree
            degree <- dots[[1]]
        else return(polym(x, ..., degree = degree))
    }
    if(is.matrix(x)) {
        m <- unclass(as.data.frame(cbind(x, ...)))
        return(do.call("polym", c(m, degree=degree)))
    }
    if(degree < 1)
        stop("degree must be at least 1")
    n <- degree + 1
    if(is.null(coefs)) { # fitting
        if(degree >= length(x))
            stop("degree must be less than number of points")
        xbar <- mean(x)
        x <- x - xbar
        X <- outer(x, seq(length = n) - 1, "^")
        QR <- qr(X)
        z <- QR$qr
        z <- z * (row(z) == col(z))
        raw <- qr.qy(QR, z)
        norm2 <- colSums(raw^2)
        alpha <- (colSums(x*raw^2)/norm2 + xbar)[1:degree]
        Z <- raw / rep(sqrt(norm2), each = length(x))
        colnames(Z) <- 1:n - 1
        Z <- Z[, -1, drop = FALSE]
        attr(Z, "degree") <- 1:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = c(1, norm2))
        class(Z) <- c("poly", "matrix")
    } else {            # prediction
        alpha <- coefs$alpha; norm2 <- coefs$norm2
        Z <- matrix(, length(x), n)
        Z[, 1] <- 1
        Z[, 2] <- x - alpha[1]
        if(degree > 1)
            for(i in 2:degree)
                Z[, i+1] <- (x - alpha[i]) * Z[, i]  -
                    (norm2[i+1] / norm2[i]) * Z[, i-1]
        Z <- Z / rep(sqrt(norm2[-1]), each = length(x))
        colnames(Z) <- 0:degree
        Z <- Z[, -1, drop = FALSE]
        ## we may want to use the prediction to clone another prediction
        attr(Z, "degree") <- 1:degree
        attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
        class(Z) <- c("poly", "matrix")
    }
    return(Z)
}

predict.poly <- function(object, newdata, ...)
{
    if(missing(newdata)) return(object)
    poly(newdata, degree = max(attr(object, "degree")),
         coefs = attr(object, "coefs"))
}

makepredictcall.poly  <- function(var, call)
{
    if(as.character(call)[1] != "poly") return(call)
    call$coefs <- attr(var, "coefs")
    call
}

polym <- function(..., degree = 1)
{
    dots <- list(...)
    nd <- length(dots)
    if(nd == 0) stop("must supply one or more vectors")
    if(nd == 1) return(poly(dots[[1]], degree))
    n <- sapply(dots, length)
    if(any(n != n[1]))
        stop("arguments must have the same length")
    z <- do.call("expand.grid", rep.int(list(0:degree), nd))
    s <- rowSums(z)
    ind <- (s > 0) & (s <= degree)
    z <- z[ind, ]; s <- s[ind]
    res <- cbind(1, poly(dots[[1]], degree))[, 1 + z[, 1]]
    for(i in 2:nd) res <- res * cbind(1, poly(dots[[i]], degree))[, 1 + z[, i]]
    colnames(res) <- apply(z, 1, function(x) paste(x, collapse = "."))
    attr(res, "degree") <- as.vector(s)
    res
}
