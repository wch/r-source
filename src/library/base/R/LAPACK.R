La.eigen <- function (x, symmetric, only.values = FALSE)
{
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to La.eigen must be numeric or complex")
    x <- as.matrix(x)
    if (nrow(x) != ncol(x)) stop("non-square matrix in La.eigen")
    if (nrow(x) == 0) stop("0 x 0 matrix in La.eigen")
    complex.x <- is.complex(x)
    if (missing(symmetric)) {
        tx <- if(complex.x) Conj(t(x)) else t(x)
        test <- all.equal.numeric(x, tx, 100 * .Machine$double.eps)
        symmetric <- is.logical(test) && test
    }
    if (is.numeric(x)) storage.mode(x) <- "double"
    if (symmetric) {
        z <- if(!complex.x)
            .Call("La_rs", x, only.values, PACKAGE = "base")
        else
            .Call("La_rs_cmplx", x, only.values, PACKAGE = "base")
        ord <- rev(seq(along = z$values))
    } else {
        z <- if(!complex.x)
            .Call("La_rg", x, only.values, PACKAGE = "base")
        else
            .Call("La_rg_cmplx", x, only.values, PACKAGE = "base")
        ord <- rev(order(Mod(z$values)))
    }
    list(values = z$values[ord], vectors = if (!only.values) z$vectors[, ord])
}

La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to La.svd must be numeric or complex")
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

    if(is.complex(x)) {
        u[] <- as.complex(u)
        v[] <- as.complex(v)
        res <- .Call("La_svd_cmplx", jobu, jobv, x, double(min(n,p)), u, v,
                     PACKAGE="base")
    } else
        res <- .Call("La_svd", jobu, jobv, x, double(min(n,p)), u, v,
                     PACKAGE="base")
    res[c("d", if(nu) "u", if(nv) "vt")]
}
