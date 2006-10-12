cmdscale <- function (d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE)
{
    if (any(is.na(d)))
	stop("NA values not allowed in 'd'")
    if (is.null(n <- attr(d, "Size"))) {
        if(add) d <- as.matrix(d)
	x <- as.matrix(d^2)
	if ((n <- nrow(x)) != ncol(x))
	    stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    } else {
	x <- matrix(0, n, n)
        if(add) d0 <- x
	x[row(x) > col(x)] <- d^2
	x <- x + t(x)
        if(add) {
            d0[row(x) > col(x)] <- d
            d <- d0 + t(d0)
        }
        rn <- attr(d, "Labels")
    }
    if((k <- as.integer(k)) > n - 1 || k < 1)
        stop("'k' must be in {1, 2, ..  n - 1}")
    storage.mode(x) <- "double"
    ## doubly center x in-place
    .C(R_dblcen, x, as.integer(n), DUP = FALSE)

    if(add) { ## solve the additive constant problem
        ## it is c* = largest eigenvalue of 2 x 2 (n x n) block matrix Z:
        i2 <- n + (i <- 1:n)
        Z <- matrix(0, 2*n, 2*n)
        Z[cbind(i2,i)] <- -1
        Z[ i, i2] <- -x
        Z[i2, i2] <- .C(R_dblcen, x = 2*d, as.integer(n))$x
        e <- eigen(Z, symmetric = FALSE, only.values = TRUE)$values
        add.c <- max(Re(e))
        ## and construct a new x[,] matrix:
	x <- matrix(double(n*n), n, n)
        non.diag <- row(d) != col(d)
        x[non.diag] <- (d[non.diag] + add.c)^2
    }
    e <- eigen(-x/2, symmetric = TRUE)
    ev <- e$values[1:k]
    if(any(ev < 0))
        warning(gettextf("some of the first %d eigenvalues are < 0", k),
                domain = NA)
    points <- e$vectors[, 1:k, drop = FALSE] %*% diag(sqrt(ev), k)
    dimnames(points) <- list(rn, NULL)
    if (eig || x.ret || add) {
        evalus <- e$values[-n]
        list(points = points, eig = if(eig) ev, x = if(x.ret) x,
             ac = if(add) add.c else 0,
             GOF = sum(ev)/c(sum(abs(evalus)), sum(evalus[evalus > 0])))
    } else points
}
