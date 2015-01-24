#  File src/library/stats/R/fligner.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

fligner.test <- function(x, ...) UseMethod("fligner.test")

fligner.test.default <-
function(x, g, ...)
{
    ## FIXME: This is the same code as in kruskal.test(), and could also
    ## rewrite bartlett.test() accordingly ...
    if (is.list(x)) {
        if (length(x) < 2L)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    ## FIXME: now the specific part begins.

    ## Careful. This assumes that g is a factor:
    x <- x - tapply(x,g,median)[g]

    a <- qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    STATISTIC <- sum(tapply(a, g, "sum")^2 / tapply(a, g, "length"))
    STATISTIC <- (STATISTIC - n * mean(a)^2) / var(a)
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "Fligner-Killeen:med chi-squared"
    names(PARAMETER) <- "df"
    METHOD <- "Fligner-Killeen test of homogeneity of variances"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

fligner.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- quote(model.frame)
    mf <- eval(m, parent.frame())
    if(length(mf) != 2L)
        stop("'formula' should be of the form response ~ group")
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("fligner.test", as.list(mf))
    y$data.name <- DNAME
    y
}
