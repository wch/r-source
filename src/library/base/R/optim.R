optim <- function(par, fn, gr = NULL,
                  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B"),
                  lower = -Inf, upper = Inf,
                  control = list(), hessian = FALSE)
{
    method <- match.arg(method)
    con <- list(trace = 0, fnscale = 1, parscale = rep(1, length(par)),
                ndeps = rep(1e-3, length(par)),
                maxit = 100, abstol = -Inf, reltol=sqrt(.Machine$double.eps),
                alpha = 1.0, beta = 0.5, gamma = 2.0,
                REPORT = 10,
                type = 1,
                lmm = 5, factr = 1e12, pgtol = 0)
    if (method == "Nelder-Mead") con$maxit <- 500
    con[names(control)] <- control
    npar <- length(par)
    lower <- as.double(rep(lower, npar))
    upper <- as.double(rep(upper, npar))
    res <- .Internal(optim(par, fn, gr, method, con, lower, upper))
    names(res) <- c("par", "value", "counts", "convergence", "message")
    nm <- names(par)
    if(!is.null(nm)) names(res$par) <- nm
    names(res$counts) <- c("function", "gradient")
    if (hessian) {
        hess <- .Internal(optimhess(par, fn, gr, con))
        hess <- 0.5*(hess + t(hess))
        if(!is.null(nm)) dimnames(hess) <- list(nm, nm)
        res$hessian <- hess
    }
    res
}
