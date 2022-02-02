#  File src/library/base/R/pretty.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2021 The R Core Team
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

pretty <- function(x, ...) UseMethod("pretty")

.pretty <- function(x, n = 5L, min.n = n %/% 3L, shrink.sml = 0.75,
                    high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
                    eps.correct = 0L, f.min = 2^-20, bounds = TRUE) {
    x <- x[is.finite(x <- as.numeric(x))]
    if(length(x)) # return  list(l=, u=, n=) or list(ns=, nu=, n=, unit=)
        .Internal(pretty(min(x), max(x), n, min.n, shrink.sml,
                         c(high.u.bias, u5.bias, f.min), eps.correct, bounds))
}

pretty.default <-
    function(x, n = 5L, min.n = n %/% 3L, shrink.sml = 0.75,
             high.u.bias = 1.5, u5.bias = .5 + 1.5*high.u.bias,
             eps.correct = 0L, f.min = 2^-20, ...)
{
    x <- x[is.finite(x <- as.numeric(x))]
    if(!length(x)) return(x)
    z <- .Internal(pretty(min(x), max(x), n, min.n, shrink.sml,
                          c(high.u.bias, u5.bias, f.min), eps.correct, TRUE))
    n <- z$n
    s <- seq.int(z$l, z$u, length.out = n + 1L)
    if(!eps.correct && n) { # maybe zap smalls from seq() rounding errors
        ## better than zapsmall(s, digits = 14) :
        delta <- diff(range(z$l, z$u)/n) # NB: |l - u| may be Inf
        if(any(small <- abs(s) < 1e-14 * delta)) s[small] <- 0
    }
    s
}
