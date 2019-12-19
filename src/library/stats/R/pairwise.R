#  File src/library/stats/R/pairwise.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

pairwise.t.test <-
function(x, g, p.adjust.method = p.adjust.methods, pool.sd = !paired,
         paired = FALSE, alternative = c("two.sided", "less", "greater"), ...)
{
    if (paired & pool.sd)
        stop("pooling of SD is incompatible with paired tests")
    DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(g)))
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    alternative <- match.arg(alternative)
    if (pool.sd)
    {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            if (alternative == "two.sided")
                2 * pt(-abs(t.val), total.degf)
            else
                pt(t.val, total.degf,
                   lower.tail=(alternative == "less"))
        }
    } else {
        METHOD <- if (paired) "paired t tests"
		  else "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired=paired,
                   alternative=alternative, ...)$p.value
        }
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


pairwise.wilcox.test <-
function(x, g, p.adjust.method = p.adjust.methods, paired=FALSE, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(g)))
    g <- factor(g)
    METHOD <- NULL # exact or not? (depends on '...', sample size 'n', etc)
    compare.levels <- function(i, j) {
        xi <- x[as.integer(g) == i]
        xj <- x[as.integer(g) == j]
        if(is.null(METHOD)) { # first time
            wt <- wilcox.test(xi, xj, paired=paired, ...)
            METHOD <<- wt$method
            wt$p.value
        }
        else
            wilcox.test(xi, xj, paired=paired, ...)$p.value
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.prop.test <-
function (x, n, p.adjust.method = p.adjust.methods, ...)
{
    p.adjust.method <- match.arg(p.adjust.method)
    METHOD <- "Pairwise comparison of proportions"
    DNAME <- deparse1(substitute(x))
    if (is.matrix(x)) {
        if (ncol(x) != 2)
            stop("'x' must have 2 columns")
        n <- rowSums(x)
        x <- x[, 1]
    }
    else {
        DNAME <- paste(DNAME, "out of", deparse1(substitute(n)))
        if (length(x) != length(n))
            stop("'x' and 'n' must have the same length")
    }
    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if (length(x) < 2L)
        stop("too few groups")
    compare.levels <- function(i, j) {
        prop.test(x[c(i,j)], n[c(i,j)], ...)$p.value
    }
    level.names <- names(x)
    if (is.null(level.names)) level.names <- seq_along(x)
    PVAL <- pairwise.table(compare.levels, level.names, p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME,
                p.value = PVAL, p.adjust.method=p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}

pairwise.table <-
function(compare.levels, level.names, p.adjust.method)
{
    ix <- setNames(seq_along(level.names), level.names)
    pp <- outer(ix[-1L], ix[-length(ix)],function(ivec, jvec)
          vapply(seq_along(ivec), function(k) {
              i <- ivec[k]
              j <- jvec[k]
              if (i > j) compare.levels(i, j) else NA_real_
          }, 0.05))
    il.tri <- lower.tri(pp, TRUE)
    pp[il.tri] <- p.adjust(pp[il.tri], p.adjust.method)
    pp
}

print.pairwise.htest <-
function(x, digits = max(1L, getOption("digits") - 5L), ...)
{
    cat("\n\tPairwise comparisons using", x$method, "\n\n")
    cat("data: ", x$data.name, "\n\n")
    pp <- format.pval(x$p.value, digits=digits, na.form="-")
    attributes(pp) <- attributes(x$p.value)
    print(pp, quote=FALSE, ...)
    cat("\nP value adjustment method:", x$p.adjust.method, "\n")
    invisible(x)
}
