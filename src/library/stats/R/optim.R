optim <- function(par, fn, gr = NULL,
                  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN"),
                  lower = -Inf, upper = Inf,
                  control = list(), hessian = FALSE, ...)
{
    fn1 <- function(par) fn(par,...)
    gr1 <- if (!is.null(gr)) function(par) gr(par,...)
    method <- match.arg(method)
    if((length(lower) > 1 || length(upper) > 1 ||
       lower[1] != -Inf || upper[1] != Inf)
       && method != "L-BFGS-B") {
        warning("bounds can only be used with method L-BFGS-B")
        method <- "L-BFGS-B"
    }
    ## Defaults :
    con <- list(trace = 0, fnscale = 1, parscale = rep.int(1, length(par)),
                ndeps = rep.int(1e-3, length(par)),
                maxit = 100, abstol = -Inf, reltol=sqrt(.Machine$double.eps),
                alpha = 1.0, beta = 0.5, gamma = 2.0,
                REPORT = 10,
                type = 1,
                lmm = 5, factr = 1e7, pgtol = 0,
                tmax = 10, temp = 10.0)
    if (method == "Nelder-Mead") con$maxit <- 500
    if (method == "SANN") con$maxit <- 10000

    con[(namc <- names(control))] <- control
    if(con$trace < 0)
        warning("read the documentation for 'trace' more carefully")
    if (method == "L-BFGS-B" &&
        any(!is.na(match(c("reltol","abstol"), namc))))
        warning("method L-BFGS-B uses 'fact' (and 'pgtol') instead of 'reltol' and 'abstol'")
    npar <- length(par)
    if(npar == 1 && method == "Nelder-Mead")
        warning("one-diml optimization by Nelder-Mead is unreliable: use optimize")
    lower <- as.double(rep(lower, , npar))
    upper <- as.double(rep(upper, , npar))
    res <- .Internal(optim(par, fn1, gr1, method, con, lower, upper))
    names(res) <- c("par", "value", "counts", "convergence", "message")
    nm <- names(par)
    if(!is.null(nm)) names(res$par) <- nm
    names(res$counts) <- c("function", "gradient")
    if (hessian) {
        hess <- .Internal(optimhess(res$par, fn1, gr1, con))
        hess <- 0.5*(hess + t(hess))
        if(!is.null(nm)) dimnames(hess) <- list(nm, nm)
        res$hessian <- hess
    }
    res
}
