#  File src/library/stats/R/wilcox.test.R
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

wilcox.test <- function(x, ...) UseMethod("wilcox.test")

wilcox.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
         conf.int = FALSE, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    if(!missing(mu) && ((length(mu) > 1L) || !is.finite(mu)))
        stop("'mu' must be a single number")
    if(conf.int) {
        if(!((length(conf.level) == 1L)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("'conf.level' must be a single number between 0 and 1")
    }

    if(!is.numeric(x)) stop("'x' must be numeric")
    if(!is.null(y)) {
        if(!is.numeric(y)) stop("'y' must be numeric")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(y)))
        if(paired) {
            if(length(x) != length(y))
                stop("'x' and 'y' must have the same length")
            OK <- complete.cases(x, y)
            x <- x[OK] - y[OK]
            y <- NULL
        }
        else {
            x <- x[is.finite(x)]
            y <- y[is.finite(y)]
        }
    } else {
        DNAME <- deparse(substitute(x))
        if(paired)
            stop("'y' is missing for paired test")
        x <- x[is.finite(x)]
    }

    if(length(x) < 1L)
        stop("not enough (finite) 'x' observations")
    CORRECTION <- 0
    if(is.null(y)) {
        METHOD <- "Wilcoxon signed rank test"
        x <- x - mu
        ZEROES <- any(x == 0)
        if(ZEROES)
            x <- x[x != 0]
        n <- as.double(length(x))
        if(is.null(exact))
            exact <- (n < 50)
        r <- rank(abs(x))
        STATISTIC <- setNames(sum(r[x > 0]), "V")
        TIES <- length(r) != length(unique(r))

        if(exact && !TIES && !ZEROES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n * (n + 1) / 4))
                                psignrank(STATISTIC - 1, n, lower.tail = FALSE)
                           else psignrank(STATISTIC, n)
                           min(2 * p, 1)
                       },
                       "greater" = psignrank(STATISTIC - 1, n, lower.tail = FALSE),
                       "less" = psignrank(STATISTIC, n))
            if(conf.int) {
                ## Exact confidence interval for the median in the
                ## one-sample case.  When used with paired values this
                ## gives a confidence interval for mean(x) - mean(y).
                x <- x + mu             # we want a conf.int for the median
                alpha <- 1 - conf.level
                diffs <- outer(x, x, "+")
                diffs <- sort(diffs[!lower.tri(diffs)]) / 2
                cint <-
                    switch(alternative,
                           "two.sided" = {
                               qu <- qsignrank(alpha / 2, n)
                               if(qu == 0) qu <- 1
                               ql <- n*(n+1)/2 - qu
                               achieved.alpha <- 2*psignrank(trunc(qu)-1,n)
                               c(diffs[qu], diffs[ql+1])
                           },
                           "greater" = {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               achieved.alpha <- psignrank(trunc(qu)-1,n)
                               c(diffs[qu], +Inf)
                           },
                           "less" = {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               ql <- n*(n+1)/2 - qu
                               achieved.alpha <- psignrank(trunc(qu)-1,n)
                               c(-Inf, diffs[ql+1])
                           })
                if (achieved.alpha - alpha > alpha/2){
                    warning("requested conf.level not achievable")
                    conf.level <- 1 - signif(achieved.alpha, 2)
                }
                attr(cint, "conf.level") <- conf.level
		ESTIMATE <- c("(pseudo)median" = median(diffs))
            }
        } else { ## not exact, maybe ties or zeroes
            NTIES <- table(r)
            z <- STATISTIC - n * (n + 1)/4
            SIGMA <- sqrt(n * (n + 1) * (2 * n + 1) / 24
                          - sum(NTIES^3 - NTIES) / 48)
            if(correct) {
                CORRECTION <-
                    switch(alternative,
                           "two.sided" = sign(z) * 0.5,
                           "greater" = 0.5,
                           "less" = -0.5)
                METHOD <- paste(METHOD, "with continuity correction")
            }
	    z <- (z - CORRECTION) / SIGMA
	    PVAL <- switch(alternative,
			   "less" = pnorm(z),
			   "greater" = pnorm(z, lower.tail=FALSE),
			   "two.sided" = 2 * min(pnorm(z),
						 pnorm(z, lower.tail=FALSE)))
            if(conf.int) {
                ## Asymptotic confidence interval for the median in the
                ## one-sample case.  When used with paired values this
                ## gives a confidence interval for mean(x) - mean(y).
                ## Algorithm not published, thus better documented here.
                x <- x + mu
                alpha <- 1 - conf.level
                ## These are sample based limits for the median
                ## [They don't work if alpha is too high]
                mumin <- min(x)
                mumax <- max(x)
                ## wdiff(d, zq) returns the absolute difference between
                ## the asymptotic Wilcoxon statistic of x - mu - d and
                ## the quantile zq.
                wdiff <- function(d, zq) {
                    xd <- x - d
                    xd <- xd[xd != 0]
                    nx <- length(xd)
                    dr <- rank(abs(xd))
		    zd <- sum(dr[xd > 0]) - nx * (nx + 1)/4
		    NTIES.CI <- table(dr)
		    SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1) / 24
				     - sum(NTIES.CI^3 - NTIES.CI) / 48)
                    if (SIGMA.CI == 0)
                        stop("cannot compute confidence interval when all observations are tied", call.=FALSE)
		    CORRECTION.CI <-
			if(correct) {
                            switch(alternative,
                                   "two.sided" = sign(zd) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
			} else 0
		    (zd - CORRECTION.CI) / SIGMA.CI - zq
                }
                ## Here we optimize the function wdiff in d over the set
                ## c(mumin, mumax).
                ## This returns a value from c(mumin, mumax) for which
                ## the asymptotic Wilcoxon statistic is equal to the
                ## quantile zq.  This means that the statistic is not
                ## within the critical region, and that implies that d
                ## is a confidence limit for the median.
                ##
                ## As in the exact case, interchange quantiles.
                cint <- switch(alternative, "two.sided" = {
                  repeat {
                      mindiff <- wdiff(mumin,zq = qnorm(alpha/2, lower.tail = FALSE))
                      maxdiff <- wdiff(mumax,zq = qnorm(alpha/2))
                      if(mindiff < 0 || maxdiff > 0)  alpha <- alpha*2  else break
                  }
                  if(1 - conf.level < alpha*0.75) {
                    conf.level <- 1 - alpha
                    warning("requested conf.level not achievable")
                  }
                  l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq=qnorm(alpha/2, lower.tail=FALSE))$root
                  u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq = qnorm(alpha/2))$root
                  c(l, u)
                }, "greater" = {
                  repeat {
                      mindiff <- wdiff(mumin, zq = qnorm(alpha, lower.tail = FALSE))
                      if(mindiff < 0)  alpha <- alpha*2  else break
                  }
                  if(1 - conf.level < alpha*0.75) {
                    conf.level <- 1 - alpha
                    warning("requested conf.level not achievable")
                  }
                  l <- uniroot(wdiff, c(mumin, mumax), tol = 1e-4,
                               zq = qnorm(alpha, lower.tail = FALSE))$root
                  c(l, +Inf)
                }, "less" = {
                  repeat {
                      maxdiff <- wdiff(mumax, zq = qnorm(alpha))
                      if(maxdiff > 0)  alpha <- alpha * 2  else break
                  }
                  if (1 - conf.level < alpha*0.75) {
                    conf.level <- 1 - alpha
                    warning("requested conf.level not achievable")
                  }
                  u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                               zq = qnorm(alpha))$root
                  c(-Inf, u)
                })
                attr(cint, "conf.level") <- conf.level
		correct <- FALSE # no continuity correction for estimate
		ESTIMATE <- c("(pseudo)median" =
			      uniroot(wdiff, c(mumin, mumax), tol=1e-4,
				      zq = 0)$root)
            }

            if(exact && TIES) {
                warning("cannot compute exact p-value with ties")
                if(conf.int)
                    warning("cannot compute exact confidence interval with ties")
            }
            if(exact && ZEROES) {
                warning("cannot compute exact p-value with zeroes")
                if(conf.int)
                    warning("cannot compute exact confidence interval with zeroes")
            }
	}
    }
    else { ##-------------------------- 2-sample case ---------------------------
        if(length(y) < 1L)
            stop("not enough 'y' observations")
        METHOD <- "Wilcoxon rank sum test"
        r <- rank(c(x - mu, y))
        n.x <- as.double(length(x))
        n.y <- as.double(length(y))
        if(is.null(exact))
            exact <- (n.x < 50) && (n.y < 50)
        STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
        TIES <- (length(r) != length(unique(r)))
        if(exact && !TIES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n.x * n.y / 2))
                               pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
                           else
                               pwilcox(STATISTIC, n.x, n.y)
                           min(2 * p, 1)
                       },
                       "greater" = {
                           pwilcox(STATISTIC - 1, n.x, n.y, lower.tail = FALSE)
                       },
                       "less" = pwilcox(STATISTIC, n.x, n.y))
            if(conf.int) {
                ## Exact confidence interval for the location parameter
                ## mean(x) - mean(y) in the two-sample case (cf. the
                ## one-sample case).
                alpha <- 1 - conf.level
                diffs <- sort(outer(x, y, "-"))
                cint <-
                    switch(alternative,
                           "two.sided" = {
                               qu <- qwilcox(alpha/2, n.x, n.y)
                               if(qu == 0) qu <- 1
                               ql <- n.x*n.y - qu
                               achieved.alpha <- 2*pwilcox(trunc(qu)-1,n.x,n.y)
                               c(diffs[qu], diffs[ql + 1])
                           },
                           "greater" = {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0) qu <- 1
                               achieved.alpha <- pwilcox(trunc(qu)-1,n.x,n.y)
                               c(diffs[qu], +Inf)
                           },
                           "less" = {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0) qu <- 1
                               ql <- n.x*n.y - qu
                               achieved.alpha <- pwilcox(trunc(qu)-1,n.x,n.y)
                               c(-Inf, diffs[ql + 1])
                           })
                if (achieved.alpha-alpha > alpha/2) {
                    warning("Requested conf.level not achievable")
                    conf.level <- 1 - achieved.alpha
                }
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- c("difference in location" = median(diffs))
            }
        }
        else {
            NTIES <- table(r)
            z <- STATISTIC - n.x * n.y / 2
            SIGMA <- sqrt((n.x * n.y / 12) *
                          ((n.x + n.y + 1)
                           - sum(NTIES^3 - NTIES)
                           / ((n.x + n.y) * (n.x + n.y - 1))))
            if(correct) {
                CORRECTION <- switch(alternative,
                                     "two.sided" = sign(z) * 0.5,
                                     "greater" = 0.5,
                                     "less" = -0.5)
                METHOD <- paste(METHOD, "with continuity correction")
            }
	    z <- (z - CORRECTION) / SIGMA
	    PVAL <- switch(alternative,
			   "less" = pnorm(z),
			   "greater" = pnorm(z, lower.tail=FALSE),
			   "two.sided" = 2 * min(pnorm(z),
						 pnorm(z, lower.tail=FALSE)))
            if(conf.int) {
                ## Asymptotic confidence interval for the location
                ## parameter mean(x) - mean(y) in the two-sample case
                ## (cf. one-sample case).
                ##
                ## Algorithm not published, for a documentation see the
                ## one-sample case.
                alpha <- 1 - conf.level
                mumin <- min(x) - max(y)
                mumax <- max(x) - min(y)
                wdiff <- function(d, zq) {
                    dr <- rank(c(x - d, y))
                    NTIES.CI <- table(dr)
                    dz <- (sum(dr[seq_along(x)])
                           - n.x * (n.x + 1) / 2 - n.x * n.y / 2)
		    CORRECTION.CI <-
			if(correct) {
                            switch(alternative,
                                   "two.sided" = sign(dz) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
			} else 0
                    SIGMA.CI <- sqrt((n.x * n.y / 12) *
                                     ((n.x + n.y + 1)
                                      - sum(NTIES.CI^3 - NTIES.CI)
                                      / ((n.x + n.y) * (n.x + n.y - 1))))
                    if (SIGMA.CI == 0)
                        stop("cannot compute confidence interval when all observations are tied", call.=FALSE)
                    (dz - CORRECTION.CI) / SIGMA.CI - zq
                }
                root <- function(zq) {
                    ## in extreme cases we need to return endpoints,
                    ## e.g.  wilcox.test(1, 2:60, conf.int=TRUE)
                    f.lower <- wdiff(mumin, zq)
                    if(f.lower <= 0) return(mumin)
                    f.upper <- wdiff(mumax, zq)
                    if(f.upper >= 0) return(mumax)
                    uniroot(wdiff, c(mumin, mumax),
                            f.lower = f.lower, f.upper = f.upper,
                            tol = 1e-4, zq = zq)$root
                }
                cint <- switch(alternative,
                               "two.sided" = {
                                   l <- root(zq = qnorm(alpha/2, lower.tail = FALSE))
                                   u <- root(zq = qnorm(alpha/2))
                                   c(l, u)
                               },
                               "greater" = {
                                   l <- root(zq = qnorm(alpha, lower.tail = FALSE))
                                   c(l, +Inf)
                               },
                               "less" = {
                                   u <- root(zq = qnorm(alpha))
                                   c(-Inf, u)
                               })
                attr(cint, "conf.level") <- conf.level
		correct <- FALSE # no continuity correction for estimate
		ESTIMATE <- c("difference in location" =
			      uniroot(wdiff, c(mumin, mumax), tol = 1e-4,
				      zq = 0)$root)
            }

            if(exact && TIES) {
                warning("cannot compute exact p-value with ties")
                if(conf.int)
                    warning("cannot compute exact confidence intervals with ties")
            }
        }
    }

    names(mu) <- if(paired || !is.null(y)) "location shift" else "location"
    RVAL <- list(statistic = STATISTIC,
                 parameter = NULL,
                 p.value = as.numeric(PVAL),
                 null.value = mu,
                 alternative = alternative,
                 method = METHOD,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL,
                  list(conf.int = cint,
                       estimate = ESTIMATE))
    class(RVAL) <- "htest"
    RVAL
}

wilcox.test.formula <-
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
    DATA <- setNames(split(mf[[response]], g),
		     c("x", "y"))
    y <- do.call("wilcox.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
