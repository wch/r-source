#  File src/library/stats/R/ks.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2018 The R Core Team
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

ks.test <-
    function(x, y, ..., alternative = c("two.sided", "less", "greater"),
             exact = NULL)
{
    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1L)
        stop("not enough 'x' data")
    PVAL <- NULL

    if(is.numeric(y)) { ## two-sample case
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)             # to avoid integer overflow
        n.y <- length(y)
        if(n.y < 1L)
            stop("not enough 'y' data")
        if(is.null(exact))
            exact <- (n.x * n.y < 10000)
        METHOD <- "Two-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        n <- n.x * n.y / (n.x + n.y)
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1 / n.x, - 1 / n.y))
        if(length(unique(w)) < (n.x + n.y)) {
            if (exact) {
                warning("cannot compute exact p-value with ties")
                exact <- FALSE
            } else
                warning("p-value will be approximate in the presence of ties")
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
        }
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(z)),
                            "greater" = max(z),
                            "less" = - min(z))
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below that of y",
                                 "greater" = "the CDF of x lies above that of y")
        if(exact && (alternative == "two.sided") && !TIES)
            PVAL <- 1 - .Call(C_pSmirnov2x, STATISTIC, n.x, n.y)
    } else { ## one-sample case
        if(is.character(y)) # avoid matching anything in this function
            y <- get(y, mode = "function", envir = parent.frame())
        if(!is.function(y))
            stop("'y' must be numeric or a function or a string naming a valid function")
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        if(length(unique(x)) < n) {
            warning("ties should not be present for the Kolmogorov-Smirnov test")
            TIES <- TRUE
        }
        if(is.null(exact)) exact <- (n < 100) && !TIES
        x <- y(sort(x), ...) - (0 : (n-1)) / n
        STATISTIC <- switch(alternative,
                            "two.sided" = max(c(x, 1/n - x)),
                            "greater" = max(1/n - x),
                            "less" = max(x))
        if(exact) {
            PVAL <- 1 - if(alternative == "two.sided")
                .Call(C_pKolmogorov2x, STATISTIC, n)
            else {
                pkolmogorov1x <- function(x, n) {
                    ## Probability function for the one-sided
                    ## one-sample Kolmogorov statistics, based on the
                    ## formula of Birnbaum & Tingey (1951).
                    if(x <= 0) return(0)
                    if(x >= 1) return(1)
                    j <- seq.int(from = 0, to = floor(n * (1 - x)))
                    1 - x * sum(exp(lchoose(n, j)
                                    + (n - j) * log(1 - x - j / n)
                                    + (j - 1) * log(x + j / n)))
                }
                pkolmogorov1x(STATISTIC, n)
            }
        }
        nm_alternative <-
            switch(alternative,
                   "two.sided" = "two-sided",
                   "less" = "the CDF of x lies below the null hypothesis",
                   "greater" = "the CDF of x lies above the null hypothesis")
    }

    names(STATISTIC) <- switch(alternative,
                               "two.sided" = "D",
                               "greater" = "D^+",
                               "less" = "D^-")

    if(is.null(PVAL)) { ## so not exact
        pkstwo <- function(x, tol = 1e-6) {
            ## Compute \sum_{-\infty}^\infty (-1)^k e^{-2k^2x^2}
            ## Not really needed at this generality for computing a single
            ## asymptotic p-value as below.
            if(is.numeric(x)) x <- as.double(x)
            else stop("argument 'x' must be numeric")
            p <- rep(0, length(x))
            p[is.na(x)] <- NA
            IND <- which(!is.na(x) & (x > 0))
            if(length(IND)) p[IND] <- .Call(C_pKS2, p = x[IND], tol)
            p
        }
        ## <FIXME>
        ## Currently, p-values for the two-sided two-sample case are
        ## exact if n.x * n.y < 10000 (unless controlled explicitly).
        ## In all other cases, the asymptotic distribution is used
        ## directly.  But: let m and n be the min and max of the sample
        ## sizes, respectively.  Then, according to Kim and Jennrich
        ## (1973), if m < n/10, we should use the
        ## * Kolmogorov approximation with c.c. -1/(2*n) if 1 < m < 80;
        ## * Smirnov approximation with c.c. 1/(2*sqrt(n)) if m >= 80.
        PVAL <- if(alternative == "two.sided")
                    1 - pkstwo(sqrt(n) * STATISTIC)
                else exp(- 2 * n * STATISTIC^2)
        ## </FIXME>
    }

    ## fix up possible overshoot (PR#14671)
    PVAL <- min(1.0, max(0.0, PVAL))
    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = nm_alternative,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
