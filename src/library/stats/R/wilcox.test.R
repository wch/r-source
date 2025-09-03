#  File src/library/stats/R/wilcox.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2025 The R Core Team
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
         conf.int = FALSE, conf.level = 0.95, tol.root = 1e-4,
         digits.rank = Inf, ...)
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

    if(is.numeric(correct)) {
        if(!(correct %in% (0 : 3)))
            stop("'correct' must be an integer between 0 and 3")
    } else {
        correct <- (isTRUE(correct) - 1)
    }

    CINT <- NULL

    if(is.null(y)) {
        METHOD <- "Wilcoxon signed rank test"
        n <- as.double(length(x))
        if(is.null(exact))
            exact <- (n < 50)
        if(exact) {
            METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
            STAT <- .wilcox_test_one_stat_exact(x, mu, n, digits.rank)
            PVAL <- .wilcox_test_one_pval_exact(STAT, n, alternative)
            if(conf.int)
                CINT <- .wilcox_test_one_cint_exact(x, n,
                                                    STAT$z,
                                                    alternative,
                                                    conf.level)
        } else { ## not exact
            if(correct >= 0)
                METHOD <- paste(METHOD, "with continuity correction")
            STAT <- .wilcox_test_one_stat_asymp(x, mu, n, digits.rank)
            PVAL <- .wilcox_test_one_pval_asymp(STAT, n, alternative,
                                                correct)
            if(conf.int)
                CINT <- .wilcox_test_one_cint_asymp(x, n,
                                                    alternative,
                                                    conf.level,
                                                    correct >= 0,
                                                    tol.root,
                                                    digits.rank)
	}
    }
    else {
        if(length(y) < 1L)
            stop("not enough 'y' observations")
        METHOD <- "Wilcoxon rank sum test"
        n.x <- as.double(length(x))
        n.y <- as.double(length(y))
        if(is.null(exact))
            exact <- (n.x < 50) && (n.y < 50)
        if(exact) {
	    METHOD <- sub("test", "exact test", METHOD, fixed = TRUE)
            STAT <- .wilcox_test_two_stat_exact(x, y, mu, n.x, n.y,
                                                digits.rank)
            PVAL <- .wilcox_test_two_pval_exact(STAT, n.x, n.y,
                                                alternative)
            if(conf.int)
                CINT <- .wilcox_test_two_cint_exact(x, y, n.x, n.y,
                                                    STAT$z,
                                                    alternative,
                                                    conf.level)
        }
        else { ## not exact
            if(correct >= 0)
                METHOD <- paste(METHOD, "with continuity correction")
            STAT <- .wilcox_test_two_stat_asymp(x, y, mu, n.x, n.y,
                                                digits.rank)
            PVAL <- .wilcox_test_two_pval_asymp(STAT, n.x, n.y,
                                                alternative, correct)
            if(conf.int)
                CINT <- .wilcox_test_two_cint_asymp(x, y, n.x, n.y,
                                                    alternative,
                                                    conf.level,
                                                    correct >= 0,
                                                    tol.root,
                                                    digits.rank)
        }
    }

    names(mu) <- if(paired || !is.null(y)) "location shift" else "location"
    RVAL <- list(statistic = STAT$statistic,
                 parameter = NULL,
                 p.value = as.numeric(PVAL),
                 null.value = mu,
                 alternative = alternative,
                 method = METHOD,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL, CINT)
    class(RVAL) <- "htest"
    RVAL
}

## Having exact zeroes (after subtracting y if paired) and subtracting
## mu) traditionally is a problem for the signed rank test.  Wilcoxon
## suggested dropping them, Pratt suggested keeping them, see e.g.
## <https://en.wikipedia.org/wiki/Wilcoxon_signed-rank_test#Zeros>.
## Zeroes are not a problem for exact inference using the permutation
## distribution.  With V(x) = sum(rank(abs(x))[x > 0]), this is the
## empirical distribution of V over all 2^n possible sign flip vectors
## s(x) = (\pm x_1, ..., \pm x_n).  Clearly, abs(s(x)) = abs(x), so with
## r = rank(abs(x)), V(s(x)) = sum(r[s(x) > 0]).  Zeroes never
## contribute to the sum, so we can reduce to doing the sign flips for
## the non-zero elements, and dropping the ranks for the zero elements
## (but not computing the ranks after dropping the zero elements).
## For the internal dpq functions we thus use z = rank(abs(x))[x != 0]
## in case of ties or zeroes.

.wilcox_test_one_stat_exact <-
function(x, mu, n = length(x), digits.rank)
{
    x <- x - mu
    i <- (x == 0)
    r <- rank(abs(if(is.finite(digits.rank)) signif(x, digits.rank) else x))
    TIES <- length(r) != length(unique(r))
    ZERO <- any(i)
    STATISTIC <- c("V" = sum(r[x > 0]))
    list(statistic = STATISTIC, z =  if(TIES || ZERO) r[!i] else NULL)
}

.wilcox_test_one_stat_asymp <-
function(x, mu, n = length(x), digits.rank)
{
    x <- x - mu
    ZERO <- any(x == 0)
    if(ZERO) {
        x <- x[x != 0]
        n <- length(x)
    }
    r <- rank(abs(if(is.finite(digits.rank)) signif(x, digits.rank) else x))
    TIES <- length(r) != length(unique(r))
    STATISTIC <- c("V" = sum(r[x > 0]))
    MEAN <-  n * (n + 1) / 4
    NTIES <- table(r)
    SIGMA <- sqrt(n * (n + 1) * (2 * n + 1) / 24
                  - sum(NTIES^3 - NTIES) / 48)
    list(statistic = STATISTIC, ex = MEAN, sd = SIGMA,
         ties = TIES, zero = ZERO)
}

.wilcox_test_one_pval_exact <-
function(STAT, n, alternative)
{
    q <- STAT$statistic
    z <- STAT$z
    switch(alternative,
           "two.sided" = {
               ## The permutation distribution is symmetric about
               ## sum(z)/2, which may be different from n(n+1)/4 in case
               ## of zeroes.
               m <- if(is.null(z)) n * (n + 1) / 4 else sum(z) / 2
               p <- if(q > m)
                        .psignrank(q - 1/4, n, z, lower.tail = FALSE)
                    else
                        .psignrank(q, n, z)
               min(2 * p, 1)
           },
           "greater" = .psignrank(q - 1/4, n, z, lower.tail = FALSE),
           "less" = .psignrank(q, n, z))
}

.wilcox_test_one_cint_exact <-
function(x, n, z, alternative, conf.level)
{
    ## Exact confidence interval for the median in the
    ## one-sample case.  When used with paired values this
    ## gives a confidence interval for mean(x) - mean(y).
    alpha <- 1 - conf.level
    diffs <- outer(x, x, `+`)
    diffs <- sort(diffs[!lower.tri(diffs)]) / 2
    ## Of course the 'diffs' are really the Walsh averages.
    w <- if(is.null(z))
             (n * (n + 1) / 2) : 1L
         else {
             vapply(diffs,
                    \(d) { xx <- x - d; sum(rank(abs(xx))[xx > 0]) },
                    0)
         }
    CONF.INT <-
        switch(alternative,
               "two.sided" = {
                   qu <- .qsignrank(alpha / 2, n, z)
                   ql <- .qsignrank(1 - alpha / 2, n, z)
                   lci <- if(qu <= min(w)) max(diffs)
                          else min(diffs[w <= qu])
                   uci <- if(ql >= max(w)) min(diffs)
                          else max(diffs[w > ql])
                               c(uci, lci)
                   achieved.alpha <-
                       (.psignrank(qu - 1/4, n, z) +
                        .psignrank(ql + 1/4, n, z, lower.tail = FALSE))
                   c(uci, lci)
               },
               "greater" = {
                   ql <- .qsignrank(1 - alpha, n, z)
                   uci <- if(ql >= max(w)) min(diffs)
                          else max(diffs[w > ql])
                   achieved.alpha <-
                       .psignrank(ql + 1/4, n, z, lower.tail = FALSE)
                   c(uci, +Inf)
               },
               "less" = {
                   qu <- .qsignrank(alpha, n, z)
                   lci <- if(qu <= min(w)) max(diffs)
                          else min(diffs[w <= qu])
                   achieved.alpha <-
                       .psignrank(qu - 1/4, n, z)
                   c(-Inf, lci)
               })
    if(achieved.alpha - alpha > alpha/2){
        warning("requested conf.level not achievable")
        conf.level <- 1 - signif(achieved.alpha, 2)
    }
    attr(CONF.INT, "conf.level") <- conf.level
    ESTIMATE <- c("(pseudo)median" = median(diffs))
    ## NOTE: This is the Hodges-Lehmann estimate and not what is
    ## suggested in Bauer (1972) and used in \CRANpkg{coin}.
    list(conf.int = CONF.INT, estimate = ESTIMATE)
}

.wilcox_test_one_pval_asymp <-
function(STAT, n, alternative, correct)
{
    z <- STAT$statistic - STAT$ex
    ## Edgeworth approximations only work if there are no ties (or
    ## zeroes).
    if((correct > 0) && (STAT$ties || STAT$zero))
        correct <- 0
    CORRECTION <- if(correct >= 0)
                      switch(alternative,
                             "two.sided" = sign(z) * 0.5,
                             "greater" = 0.5,
                             "less" = -0.5)
                  else
                      0
    z <- (z - CORRECTION) / STAT$sd
    F <- function(z, lower.tail = TRUE) {
        y <- pnorm(z, lower.tail = lower.tail)
        if(correct < 1) return(y)
        ## Edgeworth expansion given in Fellingham and Stoker (1964),
        ## <doi:10.1080/01621459.1964.10480738>
        n4 <- 12 * (3 * n^2 + 3 * n - 1)
        d4 <- 5 * n * (n + 1) * (2 * n + 1)
        l4 <- - n4 / d4
        n6 <- 576 * (3 * n^4 + 6 * n^2 - 3 * n + 1)
        d6 <- 7 * (n * (n + 1) * (2 * n + 1))^2
        l6 <- n6 / d6
        ## \frac{\lambda_4}{4!} H_3(z)
        e <- l4 / 24 * z * (z^2 - 3)
        if(correct > 1) {
            ## \frac{\lambda_6}{6!} H_5(z)
            e <- e + l6 / 720 * z * (z^4 - 10 * z^2 + 15)
        }
        if(correct > 2) {
            ## \frac{35 \lambda_4^2}{8!} H_7(z)
            e <- e + 35 * l4^2 / 40320 * z *
                (z^6 - 21 * z^4 + 105 * z^2 - 105)
        }
        if(lower.tail) y - e else y + e
    }
    switch(alternative,
           "less" = F(z),
           "greater" = F(z, lower.tail = FALSE),
           "two.sided" = 2 * min(p <- F(z), 1 - p))
}

.wilcox_test_one_cint_asymp <-
function(x, n, alternative, conf.level, correct,
         tol.root, digits.rank)
{
    ## Asymptotic confidence interval for the median in the
    ## one-sample case.  When used with paired values this
    ## gives a confidence interval for mean(x) - mean(y).
    ## Algorithm not published, thus better documented here.
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
                warning("cannot compute confidence interval when all observations are zero or tied",
                        call. = FALSE)
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
        ## FIXME: in the one-sides cases this gives (-Inf, NaN) and
        ## (NaN, Inf): is this really what we want?
        CONF.INT <-
            structure(c(if(alternative == "less"   ) -Inf else NaN,
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
        
        CONF.INT <-
            switch(alternative,
                   "two.sided" = {
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
                   },
                   "greater" = {
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
                   },
                   "less" = {
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
        attr(CONF.INT, "conf.level") <- conf.level
        correct <- FALSE # for W(): no continuity correction for estimate
        ESTIMATE <- c("(pseudo)median" =
                          uniroot(W, lower = mumin, upper = mumax,
                                  tol = tol.root)$root)
        ## NOTE: this is not the Hodges-Lehmann estimate, which would
        ## need computing O(n^2) Walsh averages which may be a lot.
    } # regular (Wmumin, Wmumax)
    list(conf.int = CONF.INT, estimate = ESTIMATE)
}

.wilcox_test_two_stat_exact <-
function(x, y, mu, n.x = length(x), n.y = length(y), digits.rank)
{
    r <- c(x - mu, y)
    r <- rank(if(is.finite(digits.rank)) signif(r, digits.rank) else r)
    TIES <- (length(r) != length(unique(r)))
    STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
    list(statistic = STATISTIC, z = if(TIES) r else NULL)
}
    
.wilcox_test_two_stat_asymp <-
function(x, y, mu, n.x = length(x), n.y = length(y), digits.rank)
{
    r <- c(x - mu, y)
    r <- rank(if(is.finite(digits.rank)) signif(r, digits.rank) else r)
    TIES <- (length(r) != length(unique(r)))
    STATISTIC <- c("W" = sum(r[seq_along(x)]) - n.x * (n.x + 1) / 2)
    MEAN <- n.x * n.y / 2
    NTIES <- table(r)
    SIGMA <- sqrt((n.x * n.y / 12) *
                  ((n.x + n.y + 1)
                      - sum(NTIES^3 - NTIES)
                      / ((n.x + n.y) * (n.x + n.y - 1))))
    list(statistic = STATISTIC, ex = MEAN, sd = SIGMA, ties = TIES)
}

.wilcox_test_two_pval_exact <-
function(STAT, n.x, n.y, alternative)
{
    q <- STAT$statistic
    z <- STAT$z
    switch(alternative,
           "two.sided" = {
               ## FIXME: Is the conditional distribution really
               ## symmetric about its mean?
               p <- if(q > (n.x * n.y / 2))
                        .pwilcox(q - 1/4, n.x, n.y, z, lower.tail = FALSE)
                    else
                        .pwilcox(q, n.x, n.y, z)
               min(2 * p, 1)
           },
           "greater" = {
               .pwilcox(q - 1/4, n.x, n.y, z, lower.tail = FALSE)
           },
           "less" = .pwilcox(q, n.x, n.y, z))
}

.wilcox_test_two_cint_exact <-
function(x, y, n.x, n.y, z, alternative, conf.level)
{
    ## Exact confidence interval for the location parameter
    ## mean(x) - mean(y) in the two-sample case (cf. the
    ## one-sample case).
    alpha <- 1 - conf.level
    diffs <- sort(outer(x, y, `-`))
    w <- if(is.null(z))
             (n.x * n.y) : 1L
         else {
             i <- seq_along(x)
             m <- n.x * (n.x + 1) / 2
             vapply(diffs, \(d) sum(rank(c(x - d, y))[i]) - m, 0)
         }
    CONF.INT <-
        switch(alternative,
               "two.sided" = {
                   qu <- .qwilcox(alpha/2, n.x, n.y, z)
                   ql <- .qwilcox(1 - alpha/2, n.x, n.y, z)                   
                   lci <- if(qu <= min(w)) max(diffs)
                          else min(diffs[w <= qu])
                   uci <- if(ql >= max(w)) min(diffs)
                          else max(diffs[w > ql])
                   achieved.alpha <-
                       (.pwilcox(qu - 1/4, n.x, n.y, z) +
                        .pwilcox(ql + 1/4, n.x, n.y, z, lower.tail = FALSE))
                   c(uci, lci)
               },
               "greater" = {
                   ql <- .qwilcox(1 - alpha, n.x, n.y, z)
                   uci <- if(ql >= max(w)) min(diffs)
                          else max(diffs[w > ql])
                   achieved.alpha <-
                       .pwilcox(ql + 1/4, n.x, n.y, z, lower.tail = FALSE)
                   c(uci, +Inf)
               },
               "less" = {
                   qu <- .qwilcox(alpha, n.x, n.y, z)
                   lci <- if(qu <= min(w)) max(diffs)
                          else min(diffs[w <= qu])
                   achieved.alpha <-
                       .pwilcox(qu - 1/4, n.x, n.y, z)
                   c(-Inf, lci)
               })
    if(achieved.alpha - alpha > alpha / 2) {
        warning("Requested conf.level not achievable")
        conf.level <- 1 - achieved.alpha
    }
    attr(CONF.INT, "conf.level") <- conf.level
    ESTIMATE <- c("difference in location" = median(diffs))
    ## NOTE: This is the Hodges-Lehmann estimate and not what is
    ## suggested in Bauer (1972) and used in \CRANpkg{coin}.
    list(conf.int = CONF.INT, estimate = ESTIMATE)
}

.wilcox_test_two_pval_asymp <-
function(STAT, n.x, n.y, alternative, correct)
{
    z <- STAT$statistic - STAT$ex
    ## Edgeworth approximations only work if there are no ties.
    if((correct > 0) && STAT$ties)
        correct <- 0
    CORRECTION <- if(correct >= 0)
                      switch(alternative,
                             "two.sided" = sign(z) * 0.5,
                             "greater" = 0.5,
                             "less" = -0.5)
                  else
                      0
    z <- (z - CORRECTION) / STAT$sd
    F <- function(z, lower.tail = TRUE) {
        y <- pnorm(z, lower.tail = lower.tail)
        if(correct < 1) return(y)
        ## Edgeworth expansion given in Fix and Hodges (1955),
        ## <doi:10.1214/aoms/1177728547>
        ## Use Eqn 11 in the form of Fellingham and Stoker (1964).
        m <- n.x
        n <- n.y
        n4 <- m^2 + n^2 + m * n + m + n
        d4 <- 20 * m * n * (m + n + 1)
        l4 <- - n4 / d4
        n6 <- (2 * (m^4 + n^4)
            + 4 * m * n * (m^2 + n^2)
            + 6 * m^2 * n^2
            + 4 * (m^3 + n^3)
            + 7 * m * n * (m + n)
            + (m^2 + n^2) + 2 * m * n - (m + n))
        d6 <- 210 * m^2 * n^2 * (m + n + 1)^2
        l6 <- n6 / d6
        ## \frac{\lambda_4}{4!} H_3(z)
        e <- l4 / 24 * z * (z^2 - 3)
        if(correct > 1) {
            ## \frac{\lambda_6}{6!} H_5(z)
            e <- e + l6 / 720 * z * (z^4 - 10 * z^2 + 15)
        }
        if(correct > 2) {
            ## \frac{35 \lambda_4^2}{8!} H_7(z)
            e <- e + 35 * l4^2 / 40320 * z *
                (z^6 - 21 * z^4 + 105 * z^2 - 105)
        }
        if(lower.tail) y - e else y + e
    }
    switch(alternative,
           "less" = F(z),
           "greater" = F(z, lower.tail = FALSE),
           "two.sided" = 2 * min(p <- F(z), 1 - p))
}
    
.wilcox_test_two_cint_asymp <-
function(x, y, n.x, n.y, alternative, conf.level, correct,
         tol.root, digits.rank)
{
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
        if(SIGMA.CI == 0)
            warning("cannot compute confidence interval when all observations are tied",
                    call. = FALSE)
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
    CONF.INT <-
        switch(alternative,
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
    attr(CONF.INT, "conf.level") <- conf.level
    correct <- FALSE # for W(): no continuity correction for estimate
    ESTIMATE <- c("difference in location" =
                      uniroot(W, lower=mumin, upper=mumax,
                              tol = tol.root)$root)
    ## NOTE: this is not the Hodges-Lehmann estimate, which would
    ## need computing m * n differences which may be a lot.
    list(conf.int = CONF.INT, estimate = ESTIMATE)
}

wilcox.test.formula <-
function(formula, data, subset, na.action = na.pass, ...)
{
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    if ("paired" %in% ...names())
        stop("cannot use 'paired' in formula method")    
    oneSampleOrPaired <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
        if (formula[[3L]] == 1L)
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
        DATA <- split(mf[[response]], g)
        ## Call the default method.
        y <- wilcox.test(x = DATA[[1L]], y = DATA[[2L]], ...)
    }
    else { # 1-sample and paired tests
        respVar <- mf[[response]]
        if (inherits(respVar, "Pair")){
            ## Call the default method.
            y <- wilcox.test(x = respVar[, 1L], y = respVar[, 2L],
                             paired = TRUE, ...)
        }
        else {
            ## Call the default method.
            y <- wilcox.test(x = respVar, ...)
        }
    }
    y$data.name <- DNAME
    y
}

.dwilcox <-
function(x, m, n, z = NULL)
{
    if(is.null(z))
        return(dwilcox(x, m, n))

    stopifnot(length(z) == m + n)
    if(!all(2 * z == floor(2 * z)) || any(z < 1))
        stop("'z' is not a rank vector")

    y <- rep.int(NA_real_, length(x))
    i <- which(!is.na(x))
    if(!any(i))
        return(y)

    ## scores can be x.5: in that case need to multiply by f=2.
    f <- 2 - all(z == floor(z))
    d <- .Call(C_dpermdist2,
               sort(as.integer(f * z)),
               as.integer(m))
    w <- seq_along(d)
    x <- f * (x[i] + m * (m + 1) / 2)
    w <- w[match(x, w)]
    y[i] <- ifelse(is.na(w), 0, d[w])

    y
}

.pwilcox <-
function(q, m, n, z = NULL, lower.tail = TRUE)
{
    if(is.null(z))
        return(pwilcox(q, m, n, lower.tail = lower.tail))

    y <- rep.int(NA_real_, length(q))
    i <- which(!is.na(q))
    if(!any(i))
        return(y)

    ## Support of U
    s <- if(all(z == floor(z)))
             seq.int(0, m * n)
         else
             seq.int(0, 2 * m * n) / 2
    ## Density
    d <- .dwilcox(s, m, n, z)
    y[i] <- vapply(q[i],
                   function(e) {
                       ## NOTE: C code in src/nmath uses a fuzz of 1e-7.
                       sum(d[s < e + 1e-8])
                   },
                   0)
    if(lower.tail) y else 1 - y
}

.qwilcox <-
function(p, m, n, z = NULL, lower.tail = TRUE)    
{
    if(is.null(z))
        return(qwilcox(p, m, n, lower.tail = lower.tail))

    y <- rep.int(NA_real_, length(p))
    if(any(i <- (p < 0) | (p > 1)))
        y[i] <- NaN
    i <- !is.na(p) & !i
    if(!any(i))
        return(y)

    s <- if(all(z == floor(z)))
             seq.int(0, m * n)
         else
             seq.int(0, 2 * m * n) / 2
    v <- .pwilcox(s, m, n, z)
    if(!lower.tail)
        p <- 1 - p
    ## See qwilcox C code:
    p <- p - 10 * .Machine$double.eps
    y[i] <- vapply(p[i], function(e) s[v >= e][1L], 0)
    y
}

.dsignrank <-
function(x, n, z = NULL)
{
    if(is.null(z))
        return(dsignrank(x, n))

    if (!all(2 * z == floor(2 * z)) || any(z < 1)) 
        stop("'z' is not a rank vector")
    y <- rep.int(NA_real_, length(x))
    i <- which(!is.na(x))
    if (!any(i)) 
        return(y)
    f <- 2 - all(z == floor(z))
    d <- .Call(C_dpermdist1,
               sort(as.integer(f * z)))
    w <- seq.int(0, length(d) - 1L)
    x <- f * x[i]
    w <- w[match(x, w)] + 1L
    y[i] <- ifelse(is.na(w), 0, d[w])
    y
}

.psignrank <-
function(q, n, z = NULL, lower.tail = TRUE)
{
    if(is.null(z))
        return(psignrank(q, n, lower.tail = lower.tail))

    y <- rep.int(NA_real_, length(q))
    i <- which(!is.na(q))
    if(!any(i)) 
        return(y)

    ## Support of V
    s <- if(all(z == floor(z)))
             seq.int(0, n * (n + 1) / 2)
         else
             seq.int(0, n * (n + 1)) / 2
    ## Density
    d <- .dsignrank(s, n, z)
    y[i] <- vapply(q[i],
                   function(e) {
                       ## NOTE: C code in src/nmath uses a fuzz of 1e-7.
                       sum(d[s < e + 1e-8])
                   },
                   0)
    if(lower.tail) y else 1 - y
}

.qsignrank <-
function(p, n, z = NULL, lower.tail = TRUE)
{
    if(is.null(z))
        return(qsignrank(p, n, lower.tail = lower.tail))

    y <- rep.int(NA_real_, length(p))
    if (any(i <- (p < 0) | (p > 1))) 
        y[i] <- NaN
    i <- !is.na(p) & !i
    if (!any(i)) 
        return(y)

    s <- seq.int(0, n * (n + 1)) / 2
    ## FIXME: can we simplify to seq.int(0, n * (n + 1) / 2) is z is all
    ## integer?
    v <- .psignrank(s, n, z)
    if (!lower.tail) 
        p <- 1 - p
    p <- p - 10 * .Machine$double.eps
    y[i] <- vapply(p[i], function(e) s[v >= e][1L], 0)
    y
}   
