#  File src/library/stats/R/constrOptim.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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


constrOptim <-
    function(theta, f, grad, ui, ci, mu = 0.0001, control = list(),
             method = if(is.null(grad)) "Nelder-Mead" else "BFGS",
             outer.iterations = 100, outer.eps = 0.00001, ..., hessian = FALSE)
{

    if (!is.null(control$fnscale) && control$fnscale < 0)
        mu <- -mu ##maximizing

    R <- function(theta, theta.old, ...) {
        ui.theta <- ui%*%theta
        gi <-  ui.theta-ci
        if (any(gi<0)) return(NaN)
        gi.old <- ui%*%theta.old-ci
        bar <- sum(gi.old*log(gi)-ui.theta)
        if (!is.finite(bar)) bar <-  -Inf
        f(theta, ...) -mu*bar
    }

    dR <- function(theta, theta.old, ...) {
        ui.theta <- ui%*%theta
        gi <- drop(ui.theta-ci)
        gi.old <- drop(ui%*%theta.old-ci)
        dbar <- colSums(ui*gi.old/gi-ui)
        grad(theta, ...) - mu*dbar
    }

    if (any(ui%*%theta-ci <= 0))
        stop("initial value is not in the interior of the feasible region")
    obj <- f(theta, ...)
    r <- R(theta, theta, ...)
    fun <- function(theta, ...) R(theta, theta.old, ...)
    gradient <- if(method == "SANN") {
        if(missing(grad)) NULL else grad
    } else function(theta, ...) dR(theta, theta.old, ...)
    totCounts <- 0
    s.mu <- sign(mu)

    for(i in seq_len(outer.iterations)) {
        obj.old <- obj
        r.old <- r
        theta.old <- theta

        a <- optim(theta.old, fun, gradient, control = control,
                   method = method, hessian = hessian, ...)
        r <- a$value
        if (is.finite(r) && is.finite(r.old) &&
 	    abs(r - r.old) < (1e-3 + abs(r)) * outer.eps) break
        theta <- a$par
 	totCounts <- totCounts + a$counts
        obj <- f(theta, ...)
        if (s.mu * obj > s.mu * obj.old) break
    }
    if (i == outer.iterations) {
        a$convergence <- 7
        a$message <- gettext("Barrier algorithm ran out of iterations and did not converge")
    }
    if (mu > 0 && obj > obj.old) {
        a$convergence <- 11
        a$message <- gettextf("Objective function increased at outer iteration %d", i)
    }
    if (mu < 0 && obj < obj.old) {
        a$convergence <- 11
        a$message <- gettextf("Objective function decreased at outer iteration %d", i)
    }
    a$outer.iterations <- i
    a$counts <- totCounts
    a$barrier.value <- a$value
    a$value <- f(a$par, ...)
    a$barrier.value <- a$barrier.value - a$value
    a
}
