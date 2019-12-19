#  File src/library/stats/R/wilcox.test.R
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

wilcox.test <- function(x, ...) UseMethod("wilcox.test")

wilcox.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
         conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4, digits.rank = Inf, ...)
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
        DNAME <- paste(deparse1(substitute(x)), "and",
                       deparse1(substitute(y)))
        if(paired) {
            if(length(x) != length(y))
                stop("'x' and 'y' must have the same length")
            OK <- complete.cases(x, y)
            x <- x[OK] - y[OK]
            y <- NULL
        }
        else {
            y <- y[!is.na(y)]
        }
    } else {
        DNAME <- deparse1(substitute(x))
        if(paired)
            stop("'y' is missing for paired test")
    }
    x <- x[!is.na(x)]

    if(length(x) < 1L)
        stop("not enough (non-missing) 'x' observations")
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
        r <- rank(abs(if(is.finite(digits.rank)) signif(x, digits.rank) else x))
        STATISTIC <- setNames(sum(r[x > 0]), "V")
        TIES <- length(r) != length(unique(r))

        if(exact && !TIES && !ZEROES) {
	    METHOD <- sub("test", "exact test", METHOD, fixed=TRUE)
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
		if(n > 0) {
		    ## These are sample based limits for the median
		    ## [They don't work if alpha is too high]
		    mumin <- min(x)
		    mumax <- max(x)
		    ## wdiff(d, zq) returns the absolute difference between
		    ## the asymptotic Wilcoxon statistic of x - mu - d and
		    ## the quantile zq.
                    W <- function(d) { ## also fn(x, correct, alternative)
			xd <- x - d
			xd <- xd[xd != 0]
			nx <- length(xd)
                        dr <- rank(abs(if(is.finite(digits.rank)) signif(xd, digits.rank) else xd))
			zd <- sum(dr[xd > 0]) - nx * (nx + 1)/4
			NTIES.CI <- table(dr)
			SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1) / 24
					 - sum(NTIES.CI^3 - NTIES.CI) / 48)
			if (SIGMA.CI == 0)
			    warning(
			"cannot compute confidence interval when all observations are zero or tied",
				    call.=FALSE)
			CORRECTION.CI <-
			    if(correct) {
				switch(alternative,
				       "two.sided" = sign(zd) * 0.5,
				       "greater" = 0.5,
				       "less" = -0.5)
			    } else 0
			(zd - CORRECTION.CI) / SIGMA.CI
		    }
		    Wmumin <- W(mumin)
		    Wmumax <- if(!is.finite(Wmumin)) NA else W(mumax) # if(): warn only once
		}
		if(n == 0 || !is.finite(Wmumax)) { # incl. "all zero / ties" warning above
		    cint <- structure(c(if(alternative == "less"   ) -Inf else NaN,
					if(alternative == "greater") +Inf else NaN),
				      conf.level = 0)
		    ESTIMATE <- if(n > 0) c(midrange = (mumin+mumax)/2) else NaN
		} else { # (Wmumin, Wmumax) are finite
                    wdiff <- function(d, zq) W(d) - zq
                    ## Here we optimize the function wdiff in d over the set
                    ## c(mumin, mumax).
                    ## This returns a value from c(mumin, mumax) for which
                    ## the asymptotic Wilcoxon statistic is equal to the
                    ## quantile zq.  This means that the statistic is not
                    ## within the critical region, and that implies that d
                    ## is a confidence limit for the median.
                    ##
                    ## As in the exact case, interchange quantiles.
                    root <- function(zq) {
                        uniroot(wdiff, lower = mumin, upper = mumax,
                                f.lower = Wmumin - zq, f.upper = Wmumax - zq,
                                tol = tol.root, zq = zq)$root
                    }

		    cint <- switch(alternative, "two.sided" = {
			repeat { ## FIXME: no need to loop for finding boundary alpha !!
			    mindiff <- Wmumin - qnorm(alpha/2, lower.tail = FALSE)
			    maxdiff <- Wmumax - qnorm(alpha/2)
			    if(mindiff < 0 || maxdiff > 0)  alpha <- alpha*2  else break
			}
			if (alpha >= 1 || 1 - conf.level < alpha*0.75) {
			    conf.level <- 1 - pmin(1, alpha)
			    warning("requested conf.level not achievable")
			}
			if(alpha < 1) {
			    l <- root(zq = qnorm(alpha/2, lower.tail = FALSE))
			    u <- root(zq = qnorm(alpha/2))
			    c(l, u)
			} else { ## alpha >= 1
			    rep(median(x), 2)
			}
		    }, "greater" = {
			repeat { ## FIXME: no need to loop for finding boundary alpha !!
			    mindiff <- Wmumin - qnorm(alpha, lower.tail = FALSE)
			    if(mindiff < 0)  alpha <- alpha*2  else break
			}
			if (alpha >= 1 || 1 - conf.level < alpha*0.75) {
			    conf.level <- 1 - pmin(1, alpha)
			    warning("requested conf.level not achievable")
			}
			l <- if(alpha < 1)
				 root(zq = qnorm(alpha, lower.tail = FALSE))
			     else   ## alpha >= 1
				 median(x)
			c(l, +Inf)

		    }, "less" = {
			repeat { ## FIXME: no need to loop for finding boundary alpha !!
			    maxdiff <- Wmumax - qnorm(alpha/2)
			    if(maxdiff > 0)  alpha <- alpha * 2  else break
			}
			if (alpha >= 1 || 1 - conf.level < alpha*0.75) {
			    conf.level <- 1 - pmin(1, alpha)
			    warning("requested conf.level not achievable")
			}
			u <- if(alpha < 1)
				 root(zq = qnorm(alpha))
			     else
				 median(x)
			c(-Inf, u)
		    })
		    attr(cint, "conf.level") <- conf.level
		    correct <- FALSE # for W(): no continuity correction for estimate
		    ESTIMATE <- c("(pseudo)median" =
				  uniroot(W, lower = mumin, upper = mumax,
					  tol = tol.root)$root)
                } # regular (Wmumin, Wmumax)
            } # end{conf.int}
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
        r <- c(x - mu, y)
        r <- rank(if(is.finite(digits.rank)) signif(r, digits.rank) else r)
        n.x <- as.double(length(x))
        n.y <- as.double(length(y))
        if(is.null(exact))
            exact <- (n.x < 50) && (n.y < 50)
        STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
        TIES <- (length(r) != length(unique(r)))
        if(exact && !TIES) {
	    METHOD <- sub("test", "exact test", METHOD, fixed=TRUE)
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
        else { ## not exact, maybe ties or zeroes
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
                W <- function(d) { ## also fn (x, y, n.x, n.y, correct, alternative)
                    dr <- c(x - d, y)
                    dr <- rank(if(is.finite(digits.rank)) signif(dr, digits.rank) else dr)
                    NTIES.CI <- table(dr)
                    dz <- sum(dr[seq_along(x)]) - n.x * (n.x + 1) / 2 - n.x * n.y / 2
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
			warning(
			"cannot compute confidence interval when all observations are tied",
                                call.=FALSE)
                    (dz - CORRECTION.CI) / SIGMA.CI
                }
                wdiff <- function(d, zq) W(d) - zq
                Wmumin <- W(mumin)
                Wmumax <- W(mumax)
                root <- function(zq) {
                    ## in extreme cases we need to return endpoints,
                    ## e.g.  wilcox.test(1, 2:60, conf.int=TRUE)
                    f.lower <- Wmumin - zq
                    if(f.lower <= 0) return(mumin)
                    f.upper <- Wmumax - zq
                    if(f.upper >= 0) return(mumax)
                    uniroot(wdiff, lower=mumin, upper=mumax,
                            f.lower = f.lower, f.upper = f.upper,
                            tol = tol.root, zq = zq)$root
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
		correct <- FALSE # for W(): no continuity correction for estimate
		ESTIMATE <- c("difference in location" =
			      uniroot(W, lower=mumin, upper=mumax,
				      tol = tol.root)$root)
            } ## {conf.int}

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
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
        if (formula[[3]] == 1L)
            oneSampleOrPaired <- TRUE
        else
            stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ") # works in all cases
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    if (! oneSampleOrPaired) {
        g <- factor(mf[[-response]])
        if(nlevels(g) != 2L)
            stop("grouping factor must have exactly 2 levels")
        DATA <- setNames(split(mf[[response]], g), c("x", "y"))
        y <- do.call("wilcox.test", c(DATA, list(...)))
    }
    else { # 1-sample and paired tests
        respVar <- mf[[response]]
        if (inherits(respVar, "Pair")){
            DATA <- list(x = respVar[,1], y = respVar[,2], paired=TRUE)
            y <- do.call("wilcox.test", c(DATA, list(...)))
        }
        else {
            DATA <- list(x = respVar)
            y <- do.call("wilcox.test", c(DATA, list(...)))
        }
    }
    y$data.name <- DNAME
    y
}
