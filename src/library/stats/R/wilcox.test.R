wilcox.test <- function(x, ...) UseMethod("wilcox.test")

wilcox.test.default <-
function(x, y = NULL, alternative = c("two.sided", "less", "greater"),
         mu = 0, paired = FALSE, exact = NULL, correct = TRUE,
         conf.int = FALSE, conf.level = 0.95, ...)
{
    alternative <- match.arg(alternative)
    if(!missing(mu) && ((length(mu) > 1) || !is.finite(mu)))
        stop("mu must be a single number")
    if(conf.int) {
        if(!((length(conf.level) == 1)
             && is.finite(conf.level)
             && (conf.level > 0)
             && (conf.level < 1)))
            stop("conf.level must be a single number between 0 and 1")
    }

    if(!is.numeric(x)) stop("x must be numeric")
    if(!is.null(y)) {
        if(!is.numeric(y)) stop("y must be numeric")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(y)))
        if(paired) {
            if(length(x) != length(y))
                stop("x and y must have the same length")
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
            stop("y missing for paired test")
        x <- x[is.finite(x)]
    }

    if(length(x) < 1)
        stop("not enough (finite) x observations")
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
        STATISTIC <- sum(r[x > 0])
        names(STATISTIC) <- "V"
        TIES <- length(r) != length(unique(r))

        if(exact && !TIES && !ZEROES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n * (n + 1) / 4))
                                psignrank(STATISTIC - 1, n, lower = FALSE)
                           else psignrank(STATISTIC, n)
                           min(2 * p, 1)
                       },
                       "greater" = psignrank(STATISTIC - 1, n, lower = FALSE),
                       "less" = psignrank(STATISTIC, n))
            if(conf.int) {
                ## Exact confidence intervale for the median in the
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
                               uci <- diffs[qu]
                               lci <- diffs[ql+1]
                               c(uci, lci)
                           },
                           "greater"= {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               uci <- diffs[qu]
                               c(uci, +Inf)
                           },
                           "less"= {
                               qu <- qsignrank(alpha, n)
                               if(qu == 0) qu <- 1
                               ql <- n*(n+1)/2 - qu
                               lci <- diffs[ql+1]
                               c(-Inf, lci)
                           })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- median(diffs)
                names(ESTIMATE) <- "(pseudo)median"

            }
        } else {
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

            PVAL <- pnorm((z - CORRECTION) / SIGMA)
            if(alternative == "two.sided")
                PVAL <- 2 * min(PVAL, 1 - PVAL)
            if(alternative == "greater")
                PVAL <- 1 - PVAL

            if(conf.int) {
                ## Asymptotic confidence intervale for the median in the
                ## one-sample case.  When used with paired values this
                ## gives a confidence interval for mean(x) - mean(y).
                ## Algorithm not published, thus better documented here.
                x <- x + mu
                alpha <- 1 - conf.level
                ## These are sample based limits for the median
                mumin <- min(x)
                mumax <- max(x)
                ## wdiff(d, zq) returns the absolute difference between
                ## the asymptotic Wilcoxon statistic of x - mu - d and
                ## the quantile zq.
                wdiff <- function(d, zq) {
                    CORRECTION.CI <- 0
                    xd <- x - d
                    xd <- xd[xd != 0]
                    nx <- length(xd)
                    dr <- rank(abs(xd))
                    zd <- sum(dr[xd > 0])
                    NTIES.CI <- table(dr)
                    zd <- zd - nx * (nx + 1)/4
                    SIGMA.CI <- sqrt(nx * (nx + 1) * (2 * nx + 1) / 24
                                     - sum(NTIES.CI^3 -  NTIES.CI) / 48)
                    if(correct) {
                        CORRECTION.CI <-
                            switch(alternative,
                                   "two.sided" = sign(z) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
                    }
                    zd <- (zd - CORRECTION.CI) / SIGMA.CI
                    zd - zq
                }
                ## Here we optimize the function wdiff in d over the set
                ## c(mumin, mumax).
                ##
                ## This returns a value from c(mumin, mumax) for which
                ## the asymptotic Wilcoxon statistic is equal to the
                ## quantile zq.  This means that the statistic is not
                ## within the critical region, and that implies that d
                ## is a confidence limit for the median.
                ##
                ## As in the exact case, interchange quantiles.
                cint <- switch(alternative, "two.sided" = {
                    l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha/2, lower=FALSE))$root
                    u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha/2))$root
                    c(l, u)
                }, "greater"= {
                    l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha, lower=FALSE))$root
                    c(l, +Inf)
                }, "less"= {
                    u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha))$root
                    c(-Inf, u)
                })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                    zq=0)$root
		names(ESTIMATE) <- "(pseudo)median"

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
    else {
        if(length(y) < 1)
            stop("not enough y observations")
        METHOD <- "Wilcoxon rank sum test"
        r <- rank(c(x - mu, y))
        n.x <- as.double(length(x))
        n.y <- as.double(length(y))
        if(is.null(exact))
            exact <- (n.x < 50) && (n.y < 50)
        STATISTIC <- sum(r[seq(along = x)]) - n.x * (n.x + 1) / 2
        names(STATISTIC) <- "W"
        TIES <- (length(r) != length(unique(r)))
        if(exact && !TIES) {
            PVAL <-
                switch(alternative,
                       "two.sided" = {
                           p <- if(STATISTIC > (n.x * n.y / 2))
                               pwilcox(STATISTIC - 1, n.x, n.y,
                                       lower = FALSE)
                           else
                               pwilcox(STATISTIC, n.x, n.y)
                           min(2 * p, 1)
                       },
                       "greater" = {
                           pwilcox(STATISTIC - 1, n.x, n.y,
                                   lower = FALSE)
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
                               uci <- diffs[qu]
                               lci <- diffs[ql + 1]
                               c(uci, lci)
                           },
                           "greater"= {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0) qu <- 1
                               uci <- diffs[qu]
                               c(uci, +Inf)
                           },
                           "less"= {
                               qu <- qwilcox(alpha, n.x, n.y)
                               if(qu == 0 ) qu <- 1
                               ql <- n.x*n.y - qu
                               lci <- diffs[ql + 1]
                               c(-Inf, lci)
                           })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- median(diffs)
                names(ESTIMATE) <- "difference in location"
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
            PVAL <- pnorm((z - CORRECTION)/SIGMA)
            if(alternative == "two.sided")
                PVAL <- 2 * min(PVAL, 1 - PVAL)
            if(alternative == "greater")
                PVAL <- 1 - PVAL

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
                CORRECTION.CI <- 0
                wdiff <- function(d, zq) {
                    dr <- rank(c(x - d, y))
                    NTIES.CI <- table(dr)
                    dz <- (sum(dr[seq(along = x)])
                           - n.x * (n.x + 1) / 2 - n.x * n.y / 2)
                    if(correct) {
                        CORRECTION.CI <-
                            switch(alternative,
                                   "two.sided" = sign(dz) * 0.5,
                                   "greater" = 0.5,
                                   "less" = -0.5)
                    }
                    SIGMA.CI <- sqrt((n.x * n.y / 12) *
                                     ((n.x + n.y + 1)
                                      - sum(NTIES.CI^3 - NTIES.CI)
                                      / ((n.x + n.y) * (n.x + n.y - 1))))
                    dz <- (dz - CORRECTION.CI) / SIGMA.CI
                    dz - zq
                }
                cint <- switch(alternative, "two.sided" = {
                    l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha/2, lower=FALSE))$root
                    u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha/2))$root
                    c(l, u)
                }, "greater"= {
                    l <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha, lower=FALSE))$root
                    c(l, +Inf)
                }, "less"= {
                    u <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                  zq=qnorm(alpha))$root
                    c(-Inf, u)
                })
                attr(cint, "conf.level") <- conf.level
                ESTIMATE <- uniroot(wdiff, c(mumin, mumax), tol=1e-4,
                                    zq=0)$root
                names(ESTIMATE) <- "difference in location"
            }

            if(exact && TIES) {
                warning("cannot compute exact p-value with ties")
                if(conf.int)
                    warning("cannot compute exact confidence intervals with ties")
            }
        }
    }

    RVAL <- list(statistic = STATISTIC,
                 parameter = NULL,
                 p.value = as.numeric(PVAL),
                 null.value = c(mu = mu),
                 alternative = alternative,
                 method = METHOD,
                 data.name = DNAME)
    if(conf.int)
        RVAL <- c(RVAL,
                  list(conf.int = cint,
                       estimate = ESTIMATE))
    class(RVAL) <- "htest"
    return(RVAL)
}

wilcox.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1)
       || (length(attr(terms(formula[-3]), "term.labels")) != 1))
        stop("formula missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("wilcox.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
