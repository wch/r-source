#  File src/library/stats/R/oneway.test.R
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

oneway.test <-
function(formula, data, subset, na.action, var.equal = FALSE)
{
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    dp <- as.character(formula)
    if(length(dp) != 3L)
        stop("a two-sided formula is required")
    DNAME <- paste(dp[[2L]], "and", dp[[3L]])
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m$var.equal <- NULL
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    y <- mf[[response]]
    if(length(mf[-response]) > 1L)
        g <- factor(do.call("interaction", mf[-response]))
    else
        g <- factor(mf[[-response]])
    k <- nlevels(g)
    if(k < 2L)
        stop("not enough groups")
    n.i <- tapply(y, g, length)
    if(any(n.i < 2))
        stop("not enough observations")
    m.i <- tapply(y, g, mean)
    v.i <- tapply(y, g, var)
    w.i <- n.i / v.i
    sum.w.i <- sum(w.i)
    tmp <- sum((1 - w.i / sum.w.i)^2 / (n.i - 1)) / (k^2 - 1)
    METHOD <- "One-way analysis of means"
    if(var.equal) {
        n <- sum(n.i)
        STATISTIC <- ((sum(n.i * (m.i - mean(y))^2) / (k - 1)) /
                      (sum((n.i - 1) * v.i) / (n - k)))
        PARAMETER <- c(k - 1, n - k)
        PVAL <- pf(STATISTIC, k - 1, n - k, lower.tail = FALSE)
    }
    else {
        ## STATISTIC <- sum(w.i * (m.i - mean(y))^2) /
        ##    ((k - 1) * (1 + 2 * (k - 2) * tmp))
        m <- sum(w.i * m.i) / sum.w.i
        STATISTIC <- sum(w.i * (m.i - m)^2) /
            ((k - 1) * (1 + 2 * (k - 2) * tmp))
        PARAMETER <- c(k - 1, 1 / (3 * tmp))
        PVAL <- pf(STATISTIC, k - 1, 1 / (3 * tmp), lower.tail = FALSE)
        METHOD <- paste(METHOD, "(not assuming equal variances)")
    }
    names(STATISTIC) <- "F"
    names(PARAMETER) <- c("num df", "denom df")
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    RVAL
}
