#  File src/library/stats/R/poisson.tests.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/


poisson.test <- function(x, T = 1, r = 1, alternative =
                         c("two.sided", "less", "greater"),
                         conf.level = 0.95)
{

    DNAME <- deparse(substitute(x))
    DNAME <- paste(DNAME, "time base:", deparse(substitute(T)))
    if ((l <- length(x)) != length(T))
        if (length(T) == 1L)
            T <- rep(T, l)
        else
            stop("'x' and 'T' have incompatible length")
    xr <- round(x)

    if(any(!is.finite(x) | (x < 0)) || max(abs(x-xr)) > 1e-7)
        stop("'x' must be finite, nonnegative, and integer")
    x <- xr

    if(any(is.na(T) | (T < 0)))
        stop("'T' must be nonnegative")


    if ((k <- length(x)) < 1L)
        stop("not enough data")

    if (k > 2L)
        stop("the case k > 2 is unimplemented")

    if(!missing(r) && (length(r) > 1 || is.na(r) || r < 0 ))
        stop ("'r' must be a single positive number")
    alternative <- match.arg(alternative)


    if (k == 2) {

        RVAL <- binom.test(x, sum(x), r * T[1L]/(r * T[1L] + T[2L]),
                           alternative=alternative, conf.level=conf.level)

        RVAL$data.name <- DNAME
        RVAL$statistic <- c(count1 = x[1L])
        RVAL$parameter <- c("expected count1" = sum(x) * r * T[1L]/sum(T * c(1, r)))
        RVAL$estimate  <- c("rate ratio" = (x[1L]/T[1L])/(x[2L]/T[2L]))
        pp <- RVAL$conf.int
        RVAL$conf.int <- pp/(1 - pp)*T[2L]/T[1L]
        names(r) <- "rate ratio"
        RVAL$null.value <- r

        RVAL$method <- "Comparison of Poisson rates"
        return (RVAL)
    } else {
        m <- r * T
        PVAL <- switch(alternative,
                       less = ppois(x, m),
                       greater = ppois(x - 1, m, lower.tail = FALSE),
                       two.sided = {
                           if(m == 0)
                               (x == 0)
                           else {
                               ## Do
                               ##   d <- dpois(0 : inf, r * T)
                               ##   sum(d[d <= dpois(x, r * T)])
                               ## a bit more efficiently ...
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 1e-7
                               d <- dpois(x, r * T)
                               ## This is tricky: need to be sure
                               ## only to sum values in opposite tail
                               ## and not count x twice.

                               ## For the Poisson dist., the mode will
                               ## equal the mean if it is an integer.
                               if (x == m)
                                   1
                               else if (x < m) {
                                   ## Slightly trickier than in the binomial
                                   ## because we cannot use infinite-length i
                                   N <- ceiling(2 * m - x)
                                   while (dpois(N, m) > d)
                                       N <- 2 * N
                                   i <- seq.int(from = ceiling(m), to = N)
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(x, m) +
                                       ppois(N - y, m, lower.tail = FALSE)
                               } else {
                                   i <- seq.int(from = 0, to = floor(m))
                                   y <- sum(dpois(i, m) <= d * relErr)
                                   ppois(y - 1, m) +
                                       ppois(x - 1, m, lower.tail = FALSE)
                               }
                           }
                       })
        ## Determine m s.t. Prob(Pois(m) >= x) = alpha.
        ## Use that for x > 0,
        ##   Prob(Pois >= x) = pgamma(m, x).
        p.L <- function(x, alpha) {
            if(x == 0)                      # No solution
                0
            else
                qgamma(alpha, x)
        }
        ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
        ## Use that for x < n,
        ##   Prob(Pois(m) <= x) = 1 - pgamma(m, x + 1).

        p.U <- function(x, alpha)
            qgamma(1 - alpha, x + 1)

        CINT <- switch(alternative,
                       less = c(0, p.U(x, 1 - conf.level)),
                       greater = c(p.L(x, 1 - conf.level), Inf),
                       two.sided = {
                           alpha <- (1 - conf.level) / 2
                           c(p.L(x, alpha), p.U(x, alpha))
                       }) / T
        attr(CINT, "conf.level") <- conf.level

        ESTIMATE <- x / T

        names(x) <- "number of events"	# or simply "x" ??
        names(T) <- "time base"	# or simply "n" ??
        names(ESTIMATE) <-
            names(r) <- "event rate" # or simply "p" ??

        structure(list(statistic = x,
                       parameter = T,
                       p.value = PVAL,
                       conf.int = CINT,
                       estimate = ESTIMATE,
                       null.value = r,
                       alternative = alternative,
                       method = "Exact Poisson test",
                       data.name = DNAME),
                  class = "htest")

    }
}


### test cases:

## SMR, Welsh Nickel workers
## poisson.test(137, 24.19893)

## eba1977, compare Fredericia to other three cities for ages 55-59
## poisson.test(c(11,6+8+7),c(800, 1083+1050+878))
