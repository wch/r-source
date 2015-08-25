#  File src/library/stats/R/fisher.test.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

fisher.test <-
function(x, y = NULL, workspace = 200000, hybrid = FALSE,
         control = list(), or = 1, alternative = "two.sided",
         conf.int = TRUE, conf.level = 0.95,
         simulate.p.value = FALSE, B = 2000)
{
    DNAME <- deparse(substitute(x))
    METHOD <- "Fisher's Exact Test for Count Data"
    if(is.data.frame(x))
        x <- as.matrix(x)
    if(is.matrix(x)) {
        if(any(dim(x) < 2L))
            stop("'x' must have at least 2 rows and columns")
        if(!is.numeric(x) || any(x < 0) || anyNA(x))
            stop("all entries of 'x' must be nonnegative and finite")
        if(!is.integer(x)) {
            xo <- x
            x <- round(x)
            if(any(x > .Machine$integer.max))
                stop("'x' has entries too large to be integer")
            if(!identical(TRUE, (ax <- all.equal(xo, x))))
                warning(gettextf("'x' has been rounded to integer: %s", ax),
                        domain = NA)
            storage.mode(x) <- "integer"
        }
    }
    else {
        if(is.null(y))
            stop("if 'x' is not a matrix, 'y' must be given")
        if(length(x) != length(y))
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        ## use as.factor rather than factor here to be consistent with
        ## pre-tabulated data
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if((nlevels(x) < 2L) || (nlevels(y) < 2L))
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
    }
    ## x is integer
    con <- list(mult = 30)
    con[names(control)] <- control
    if((mult <- as.integer(con$mult)) < 2)
        stop("'mult' must be integer >= 2, typically = 30")

    nr <- nrow(x)
    nc <- ncol(x)

    if((nr == 2) && (nc == 2)) {
        alternative <- char.expand(alternative,
                                   c("two.sided", "less", "greater"))
        if(length(alternative) > 1L || is.na(alternative))
            stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
        if(!((length(conf.level) == 1L) && is.finite(conf.level) &&
             (conf.level > 0) && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
        if(!missing(or) && (length(or) > 1L || is.na(or) || or < 0))
            stop("'or' must be a single number between 0 and Inf")
    }

    PVAL <- NULL
    if(nr != 2  ||  nc != 2) {
        if(simulate.p.value) {
            ## we drop all-zero rows and columns
            sr <- rowSums(x)
            sc <- colSums(x)
            x <- x[sr > 0, sc > 0, drop = FALSE]
            nr <- as.integer(nrow(x))
            nc <- as.integer(ncol(x))
            if (is.na(nr) || is.na(nc) || is.na(nr * nc))
                stop("invalid nrow(x) or ncol(x)", domain = NA)
            if(nr <= 1L)
                stop("need 2 or more non-zero row marginals")
            if(nc <= 1L)
                stop("need 2 or more non-zero column marginals")
            METHOD <- paste(METHOD, "with simulated p-value\n\t (based on", B,
			     "replicates)")
            STATISTIC <- -sum(lfactorial(x))
            tmp <- .Call(C_Fisher_sim, rowSums(x), colSums(x), B)
	    ## use correct significance level for a Monte Carlo test
            almost.1 <- 1 + 64 * .Machine$double.eps
            ## PR#10558: STATISTIC is negative
	    PVAL <- (1 + sum(tmp <= STATISTIC/almost.1)) / (B + 1)
        } else if(hybrid) {
            ## Cochran condition for asym.chisq. decision:
            PVAL <- .Call(C_Fexact, x, c(5, 180, 1), workspace, mult)
         } else {
            ##  expect < 0 : exact
            PVAL <- .Call(C_Fexact, x, c(-1, 100, 0), workspace, mult)
        }

        RVAL <- list(p.value = max(0, min(1, PVAL)))
    }

    if((nr == 2) && (nc == 2)) {## conf.int and more only in  2 x 2 case
        if(hybrid) warning("'hybrid' is ignored for a 2 x 2 table")
        m <- sum(x[, 1L])
        n <- sum(x[, 2L])
        k <- sum(x[1L, ])
        x <- x[1L, 1L]
        lo <- max(0L, k - n)
        hi <- min(k, m)
        NVAL <- c("odds ratio" = or)

        ## Note that in general the conditional distribution of x given
        ## the marginals is a non-central hypergeometric distribution H
        ## with non-centrality parameter ncp, the odds ratio.
        support <- lo : hi
        ## Density of the *central* hypergeometric distribution on its
        ## support: store for once as this is needed quite a bit.
        logdc <- dhyper(support, m, n, k, log = TRUE)
        dnhyper <- function(ncp) {
            ## Does not work for boundary values for ncp (0, Inf) but it
            ## does not need to.
            d <- logdc + log(ncp) * support
            d <- exp(d - max(d))        # beware of overflow
            d / sum(d)
        }
        mnhyper <- function(ncp) {
            if(ncp == 0)
                return(lo)
            if(ncp == Inf)
                return(hi)
            sum(support * dnhyper(ncp))
        }
        pnhyper <- function(q, ncp = 1, upper.tail = FALSE) {
	    if(ncp == 1) {
		return(if(upper.tail)
		       phyper(x - 1, m, n, k, lower.tail = FALSE) else
		       phyper(x,     m, n, k))
	    }
	    if(ncp == 0) {
		return(as.numeric(if(upper.tail) q <= lo else q >= lo))
	    }
	    if(ncp == Inf) {
		return(as.numeric(if(upper.tail) q <= hi else q >= hi))
	    }
	    ## else
	    sum(dnhyper(ncp)[if(upper.tail) support >= q else support <= q])
        }

        ## Determine the p-value (if still necessary).
        if(is.null(PVAL)) {
            PVAL <-
                switch(alternative,
                       less = pnhyper(x, or),
                       greater = pnhyper(x, or, upper.tail = TRUE),
                       two.sided = {
                           if(or == 0)
                               as.numeric(x == lo)
                           else if(or == Inf)
                               as.numeric(x == hi)
                           else {
                               ## Note that we need a little fuzz.
                               relErr <- 1 + 10 ^ (-7)
                               d <- dnhyper(or)
                               sum(d[d <= d[x - lo + 1] * relErr])
                           }
                       })
            RVAL <- list(p.value = PVAL)
        }

        ## Determine the MLE for ncp by solving E(X) = x, where the
        ## expectation is with respect to H.
        mle <- function(x) {
            if(x == lo)
                return(0)
            if(x == hi)
                return(Inf)
            mu <- mnhyper(1)
            if(mu > x)
                uniroot(function(t) mnhyper(t) - x, c(0, 1))$root
            else if(mu < x)
                1 / uniroot(function(t) mnhyper(1/t) - x,
                            c(.Machine$double.eps, 1))$root
            else
                1
        }
        ESTIMATE <- c("odds ratio" = mle(x))

        if(conf.int) {
            ## Determine confidence intervals for the odds ratio.
            ncp.U <- function(x, alpha) {
                if(x == hi)
                    return(Inf)
                p <- pnhyper(x, 1)
                if(p < alpha)
                    uniroot(function(t) pnhyper(x, t) - alpha,
                            c(0, 1))$root
                else if(p > alpha)
                    1 / uniroot(function(t) pnhyper(x, 1/t) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            ncp.L <- function(x, alpha) {
                if(x == lo)
                    return(0)

                p <- pnhyper(x, 1, upper.tail = TRUE)
                if(p > alpha)
                    uniroot(function(t)
                            pnhyper(x, t, upper.tail = TRUE) - alpha,
                            c(0, 1))$root
                else if(p < alpha)
                    1 / uniroot(function(t)
                                pnhyper(x, 1/t, upper.tail = TRUE) - alpha,
                                c(.Machine$double.eps, 1))$root
                else
                    1
            }
            CINT <- switch(alternative,
                           less = c(0, ncp.U(x, 1 - conf.level)),
                           greater = c(ncp.L(x, 1 - conf.level), Inf),
                           two.sided = {
                               alpha <- (1 - conf.level) / 2
                               c(ncp.L(x, alpha), ncp.U(x, alpha))
                           })
            attr(CINT, "conf.level") <- conf.level
        }
        RVAL <- c(RVAL,
                  list(conf.int = if(conf.int) CINT,
                       estimate = ESTIMATE,
                       null.value = NVAL))
    } ## end (2 x 2)

    RVAL <- c(RVAL,
              alternative = alternative,
              method = METHOD,
              data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}
