La.eigen <- function (x, symmetric, only.values = FALSE)
{
    x <- as.matrix(x)
    n <- nrow(x)
    if (n != ncol(x))
        stop("non-square matrix in eigen")
    complex.x <- is.complex(x)
    if (complex.x) {
        if (missing(symmetric)) {
            test <- all.equal.numeric(x, Conj(t(x)), 100 * .Machine$double.eps)
            symmetric <- is.logical(test) && test
        }
    }
    else if (is.numeric(x)) {
        storage.mode(x) <- "double"
        if (missing(symmetric)) {
            test <- all.equal.numeric(x, t(x), 100 * .Machine$double.eps)
            symmetric <- is.logical(test) && test
        }
    }
    else stop("numeric or complex values required in eigen")
    dbl.n <- double(n)
    if (symmetric) {
        if(!complex.x) return(.Call("La_rs", x, only.values, PACKAGE = "base"))
        xr <- Re(x)
        xi <- Im(x)
        z <- .Fortran("ch", n, n, xr, xi, values = dbl.n,
                      !only.values, vectors = xr, ivectors = xi, dbl.n,
                      dbl.n, double(2 * n), ierr = integer(1),
                      PACKAGE = "base")
        if (z$ierr)
            stop(paste("ch returned code ", z$ierr, " in eigen"))
        if (!only.values)
            z$vectors <- matrix(complex(re = z$vectors, im = z$ivectors),
                                nc = n)
    } else {
        if(!complex.x) {
            z <-.Call("La_rg", x, only.values, PACKAGE = "base")
        } else {
            xr <- Re(x)
            xi <- Im(x)
            z <- .Fortran("cg", n, n, xr, xi, values = dbl.n,
                          ivalues = dbl.n, !only.values, vectors = xr,
                          ivectors = xi, dbl.n, dbl.n, dbl.n, ierr = integer(1),
                          PACKAGE = "base")
            if (z$ierr)
                stop(paste("cg returned code ", z$ierr, " in eigen"))
            z$values <- complex(re = z$values, im = z$ivalues)
            if (!only.values)
                z$vectors <- matrix(complex(re = z$vectors, im = z$ivectors),
                                    nc = n)
        }
    }
    ord <- rev(order(Mod(z$values)))
    list(values = z$values[ord], vectors = if (!only.values) z$vectors[, ord])
}

La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.numeric(x))
	stop("argument to svd must be numeric")
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("0 extent dimensions")

    if(nu == 0) {
	jobu <- 'N'
	u <- double(0)
    }
    else if(nu == n) {
	jobu <- ifelse(n > p, 'A', 'S')
	u <- matrix(0, n, n)
    }
    else if(nu == p) {
	jobu <- ifelse(n > p, 'S', 'A')
	u <- matrix(0, n, p)
    }
    else
	stop("nu must be 0, nrow(x) or ncol(x)")

    if (nv == 0) {
        jobv <- 'N'
        v <- double(0)
    }
    else if (nv == n) {
        jobv <- ifelse(n > p, 'A', 'S')
        v <- matrix(0, min(n, p), p)
    }
    else if (nv == p) {
        jobv <- ifelse(n > p, 'S', 'A')
        v <- matrix(0, p, p)
    }
    else
        stop("nv must be 0, nrow(x) or ncol(x)")

    .Call("La_svd", jobu, jobv, x, double(min(n,p)), u, v, package="base")
}
