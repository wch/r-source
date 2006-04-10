La.svd <- function(x, nu = min(n, p), nv = min(n, p))
{
    if(!is.numeric(x) && !is.complex(x))
	stop("argument to 'La.svd' must be numeric or complex")
    if (any(!is.finite(x))) stop("infinite or missing values in 'x'")
    x <- as.matrix(x)
    if (is.numeric(x)) storage.mode(x) <- "double"
    n <- nrow(x)
    p <- ncol(x)
    if(!n || !p) stop("0 extent dimensions")

    if(is.complex(x)) {
        if(nu == 0) {
            jobu <- 'N'
            u <- matrix(0+0i, 1, 1)  # dim is checked
        }
        else if(nu == n) {
            jobu <- ifelse(n > p, 'A', 'S')
            u <- matrix(0+0i, n, n)
        }
        else if(nu == p) {
            jobu <- ifelse(n > p, 'S', 'A')
            u <- matrix(0+0i, n, p)
        }
        else
            stop("'nu' must be 0, nrow(x) or ncol(x)")

        if (nv == 0) {
            jobv <- 'N'
            v <- matrix(0+0i, 1, 1) # dim is checked
        }
        else if (nv == n) {
            jobv <- ifelse(n > p, 'A', 'S')
            v <- matrix(0+0i, min(n, p), p)
        }
        else if (nv == p) {
            jobv <- ifelse(n > p, 'S', 'A')
            v <- matrix(0+0i, p, p)
        }
        else
            stop("'nv' must be 0, nrow(x) or ncol(x)")
        res <- .Call("La_svd_cmplx", jobu, jobv, x, double(min(n, p)),
                     u, v, PACKAGE = "base")
        return(res[c("d", if(nu) "u", if(nv) "vt")])
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
                     "dgsedd", PACKAGE = "base")
        res <- res[c("d", if(nu) "u", if(nv) "vt")]
        if(nu) res$u <- res$u[, 1:min(n, nu), drop = FALSE]
        if(nv) res$vt <- res$vt[1:min(p, nv), , drop = FALSE]
        return(res)
    }
    ## not reached
}
