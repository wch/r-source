La.svd <- function(x, nu = min(n, p), nv = min(n, p),
                   method = c("dgesdd", "dgesvd"))
{
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    method <- match.arg(method)
    if(is.complex(x) && method == "dgesdd") {
        method <- "dgesvd"
    }
    x <- as.matrix(x)
    if (is.numeric(x)) storage.mode(x) <- "double"
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("0 extent dimensions")

    if(method == "dgesvd") {
        if(nu == 0) {
            jobu <- 'N'
            u <- matrix(0, 1, 1)  # dim is checked
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
            stop("'nu' must be 0, nrow(x) or ncol(x)")

        if (nv == 0) {
            jobv <- 'N'
            v <- matrix(0, 1, 1) # dim is checked
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
            stop("'nv' must be 0, nrow(x) or ncol(x)")
    } else {
        if(nu > 0 || nv > 0) {
            np <- min(n, p)
            if(nu <= np && nv <= np) {
                jobu <- 'S'
                u <- matrix(0, n, np)
                v <- matrix(0, np, p)
            } else {
                jobu <- 'A'
                u <- matrix(0, n, n)
                v <- matrix(0, p, p)
            }
        } else {
            jobu <- 'N'
            # these dimensions _are_ checked, but unused
            u <- matrix(0, 1, 1)
            v <- matrix(0, 1, 1)
        }
        jobv <- ''
        res <- .Call("La_svd", jobu, jobv, x, double(min(n,p)), u, v,
                     method, PACKAGE = "base")
        res <- res[c("d", if(nu) "u", if(nv) "vt")]
        if(nu) res$u <- res$u[, 1:min(n, nu), drop = FALSE]
        if(nv) res$vt <- res$vt[1:min(p, nv), , drop = FALSE]
        return(res)
    }

    if(is.complex(x)) {
        u[] <- as.complex(u)
        v[] <- as.complex(v)
        res <- .Call("La_svd_cmplx", jobu, jobv, x, double(min(n, p)), u, v,
                     PACKAGE = "base")
    } else
        res <- .Call("La_svd", jobu, jobv, x, double(min(n, p)), u, v,
                     method, PACKAGE = "base")
    res[c("d", if(nu) "u", if(nv) "vt")]
}

La.chol <- function(x) .Call("La_chol", as.matrix(x), PACKAGE = "base")

La.chol2inv <- function(x, size = ncol(x)) {
    x <- as.matrix(x) # do it this way so ncol(x) is defined
    .Call("La_chol2inv", x, size, PACKAGE = "base")
}
