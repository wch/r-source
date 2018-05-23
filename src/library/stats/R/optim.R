#  File src/library/stats/R/optim.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2000-2018 The R Core Team
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
#  https://www.R-project.org/Licenses/

optim <-
    function(par, fn, gr = NULL, ...,
             method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
             lower = -Inf, upper = Inf,
             control = list(), hessian = FALSE)
{
    fn1 <- function(par) fn(par,...)
    gr1 <- if (!is.null(gr)) function(par) gr(par,...)
    method <- match.arg(method)
    if((any(lower > -Inf) || any(upper < Inf))
       && !any(method == c("L-BFGS-B","Brent"))) {
	warning("bounds can only be used with method L-BFGS-B (or Brent)")
	method <- "L-BFGS-B"
    }
    npar <- length(par)
    ## Defaults :
    con <- list(trace = 0, fnscale = 1, parscale = rep.int(1, npar),
		ndeps = rep.int(1e-3, npar),
		maxit = 100L, abstol = -Inf, reltol = sqrt(.Machine$double.eps),
		alpha = 1.0, beta = 0.5, gamma = 2.0,
		REPORT = 10, warn.1d.NelderMead = TRUE,
		type = 1,
		lmm = 5, factr = 1e7, pgtol = 0,
		tmax = 10, temp = 10.0)
    nmsC <- names(con)
    if (method == "Nelder-Mead") con$maxit <- 500
    if (method == "SANN") {
	con$maxit <- 10000
	con$REPORT <- 100
    }
    con[(namc <- names(control))] <- control
    if(length(noNms <- namc[!namc %in% nmsC]))
	warning("unknown names in control: ", paste(noNms,collapse=", "))
    if(con$trace < 0)
	warning("read the documentation for 'trace' more carefully")
    else if (method == "SANN" && con$trace && as.integer(con$REPORT) == 0)
	stop("'trace != 0' needs 'REPORT >= 1'")
    if (method == "L-BFGS-B" &&
	any(!is.na(match(c("reltol","abstol"), namc))))
	warning("method L-BFGS-B uses 'factr' (and 'pgtol') instead of 'reltol' and 'abstol'")
    if(npar == 1 && method == "Nelder-Mead" && isTRUE(con$warn.1d.NelderMead))
        warning("one-dimensional optimization by Nelder-Mead is unreliable:\nuse \"Brent\" or optimize() directly")
    if(npar > 1 && method == "Brent")
	stop('method = "Brent" is only available for one-dimensional optimization')
    lower <- as.double(rep_len(lower, npar))
    upper <- as.double(rep_len(upper, npar))
    res <- if(method == "Brent") { ## 1-D
        if(any(!is.finite(c(upper, lower))))
           stop("'lower' and 'upper' must be finite values")
	res <- optimize(function(par) fn(par,...)/con$fnscale,
                        lower = lower, upper = upper, tol = con$reltol)
	names(res)[names(res) == c("minimum", "objective")] <- c("par", "value")
        res$value <- res$value * con$fnscale
	c(res, list(counts = c(`function` = NA, gradient = NA),
                    convergence = 0L, message = NULL))
    } else .External2(C_optim, par, fn1, gr1, method, con, lower, upper)
    if (hessian)
        res$hessian <- .External2(C_optimhess, res$par, fn1, gr1, con)
    res
}

optimHess <- function(par, fn, gr = NULL, ..., control = list())
{
    fn1 <- function(par) fn(par,...)
    gr1 <- if (!is.null(gr)) function(par) gr(par,...)
    npar <- length(par)
    con <- list(fnscale = 1, parscale = rep.int(1, npar),
                ndeps = rep.int(1e-3, npar))
    con[(names(control))] <- control
    .External2(C_optimhess, par, fn1, gr1, con)
}
