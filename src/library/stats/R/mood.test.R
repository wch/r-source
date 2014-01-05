#  File src/library/stats/R/mood.test.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

mood.test <- function(x, ...) UseMethod("mood.test")

mood.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
{
    alternative <- match.arg(alternative)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    m <- length(x)
    n <- length(y)
    if ((N <- m + n) < 3L)
        stop("not enough observations")
    E <- m * (N ^ 2 - 1) / 12
    ## avoid possible integer overflow
    v <- (1/180) * m * n * (N + 1) * (N + 2) * (N - 2)
    z <- c(x, y)
    if(!anyDuplicated(z)) {
        ## Proceed as per Conover (1971).
        r <- rank(z)
        T <- sum((r[seq_along(x)] - (N + 1L) / 2) ^ 2)
    }
    else {
        ## Proceed as per Mielke (1967).
        u <- sort(unique(z))
        a <- tabulate(match(x, u), length(u))
        t <- tabulate(match(z, u), length(u))
        p <- cumsum((seq_along(z) - (N + 1L) / 2) ^ 2)
        v <- v - (m * n) / (180 * N * (N - 1L)) *
            sum(t * (t ^ 2 - 1) * (t ^ 2 - 4 + 15 * (N - t) ^ 2))
        T <- sum(a * diff(c(0, p[cumsum(t)])) / t)
    }
    z <- (T - E) / sqrt(v)
    p <- pnorm(z)
    PVAL <- switch(alternative,
                   "less" = p,
                   "greater" = 1 - p,
                   "two.sided" = 2 * min(p, 1 - p))

    structure(list(statistic = structure(z, names = "Z"),
                   p.value = PVAL,
                   alternative = alternative,
                   method = "Mood two-sample test of scale",
                   data.name = DNAME),
              class = "htest")
}

mood.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3L)
       || (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2L)
        stop("grouping factor must have exactly 2 levels")
    DATA <- setNames(split(mf[[response]], g), c("x", "y"))
    y <- do.call("mood.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
