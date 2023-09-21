#  File src/library/stats/R/ks.test.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2023 The R Core Team
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

ks.test <- function(x, ...) UseMethod("ks.test")

ks.test.default <-
    function(x, y, ..., alternative = c("two.sided", "less", "greater"),
             exact = NULL, simulate.p.value = FALSE, B = 2000)
{
    alternative <- match.arg(alternative)
    DNAME <- deparse1(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1L)
        stop("not enough 'x' data")

    ### ordered variables can be treated as numeric ones as ties are handled
    ### now
    if (is.ordered(y)) y <- unclass(y)

    if(is.numeric(y)) { ## two-sample case
        args <- list(...)
        if (length(args) > 0L)
            warning("Parameter(s) ", paste(names(args), collapse = ", "), " ignored")
        DNAME <- paste(DNAME, "and", deparse1(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)             # to avoid integer overflow
        n.y <- length(y)
        if(n.y < 1L)
            stop("not enough 'y' data")
        if(is.null(exact))
            exact <- (n.x * n.y < 10000)
        if (!simulate.p.value) {
            METHOD <- paste(c("Asymptotic", "Exact")[exact + 1L],
                            "two-sample Kolmogorov-Smirnov test")
        } else {
            METHOD <- "Monte-Carlo two-sample Kolmogorov-Smirnov test"
        }
        TIES <- FALSE
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1 / n.x, - 1 / n.y))
        if(length(unique(w)) < (n.x + n.y)) { # have ties
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
            if (!exact && !simulate.p.value)
                warning("p-value will be approximate in the presence of ties")
        }
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(z)),
                            "greater" = max(z),
                            "less" = - min(z))
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below that of y",
                                 "greater" = "the CDF of x lies above that of y")
        PVAL <- psmirnov(STATISTIC, sizes = c(n.x, n.y), z = if(TIES) w, # else NULL
                         alternative = alternative,
                         exact = exact, simulate = simulate.p.value,
                         B = B, lower.tail = FALSE)
        ## match MC p-values to those reported by chisq.test
        if(simulate.p.value) PVAL <- (1 + (PVAL * B)) / (B + 1)
    } else { ## one-sample case
        if(is.character(y)) # avoid matching anything in this function
            y <- get(y, mode = "function", envir = parent.frame())
        if(!is.function(y))
            stop("'y' must be numeric or a function or a string naming a valid function")
        TIES <- FALSE
        if(length(unique(x)) < n) {
            warning("ties should not be present for the one-sample Kolmogorov-Smirnov test")
            TIES <- TRUE
        }
        if(is.null(exact)) exact <- (n < 100) && !TIES
        METHOD <- paste(c("Asymptotic", "Exact")[exact + 1L],
                        "one-sample Kolmogorov-Smirnov test")
        x <- y(sort(x), ...) - (0 : (n-1)) / n
        STATISTIC <- switch(alternative,
                            "two.sided" = max(c(x, 1/n - x)),
                            "greater" = max(1/n - x),
                            "less" = max(x))
        PVAL <- pkolmogorov(STATISTIC, n,
                            two.sided = (alternative == "two.sided"),
                            exact = exact, lower.tail = FALSE)
        nm_alternative <-
            switch(alternative,
                   "two.sided" = "two-sided",
                   "less" = "the CDF of x lies below the null hypothesis",
                   "greater" = "the CDF of x lies above the null hypothesis")
    }

    names(STATISTIC) <- switch(alternative,
                               "two.sided" = "D",
                               "greater"   = "D^+",
                               "less"      = "D^-")

    ## fix up possible overshoot (PR#14671)
    PVAL <- min(1.0, max(0.0, PVAL))
    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = nm_alternative,
                 method = METHOD,
                 data.name = DNAME,
                 exact = exact)
    class(RVAL) <- c("ks.test", "htest")
    RVAL
}

ks.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3L))
        stop("'formula' missing or incorrect")
    oneSample <- FALSE
    if (length(attr(terms(formula[-2L]), "term.labels")) != 1L)
        if (formula[[3L]] == 1L)
            oneSample <- TRUE
        else
            stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    ## need stats:: for non-standard evaluation
    m[[1L]] <- quote(stats::model.frame)
    m$... <- NULL
    mf <- eval(m, parent.frame())
    rname <- names(mf)[1L]
    DNAME <- paste(names(mf), collapse = " by ") # works in all cases
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    if (! oneSample) { # Smirnov test
        g <- factor(mf[[-response]])
        if(nlevels(g) != 2L)
            stop("grouping factor must have exactly 2 levels")
        DATA <- split(mf[[response]], g)
        ## Call the default method.
        y <- ks.test(x = DATA[[1L]], y = DATA[[2L]], ...)
        y$alternative <- gsub("x", levels(g)[1L], y$alternative)
        y$alternative <- gsub("y", levels(g)[2L], y$alternative)
        y$response <- rname
        y$groups <- levels(g)
    }
    else { # one-sample test
        respVar <- mf[[response]]
        ## Call the default method.
        y <- ks.test(x = respVar, ...)
        y$alternative <- gsub("x", DNAME, y$alternative)
    }
    y$data.name <- DNAME
    y
}

rsmirnov <-
function(n, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater")) {
    alternative <- match.arg(alternative)

    if(!length(n) || n == 0L)
        return(numeric(0L))
    if (n < 0)
        stop("invalid arguments")
    n <- floor(n)

    if (length(sizes) != 2L)
        stop("argument 'sizes' must be a vector of length 2")
    n.x <- sizes[1L]
    n.y <- sizes[2L]
    if (n.x < 1) stop("not enough 'x' data")
    if (n.y < 1) stop("not enough 'y' data")
    n.x <- floor(n.x)
    n.y <- floor(n.y)

    rt <- if (is.null(z)) rep.int(1L, n.x + n.y) else table(z)
    two.sided <- (alternative == "two.sided")
    sizes <- if(alternative == "less")
                 c(n.y, n.x)
             else
                 c(n.x, n.y)
    .Call(C_Smirnov_sim,
          as.integer(rt),
          as.integer(sizes),
          as.integer(n),
          as.integer(two.sided))
}

psmirnov_exact <-
function(q, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         lower.tail = TRUE, log.p = FALSE) {
    if(!is.null(z)) {
        z <- (diff(sort(z)) != 0)
        z <- if(any(z)) c(0L, z, 1L) # else NULL
    }
    two.sided <- (alternative == "two.sided")
    if(alternative == "less")
        sizes <- c(sizes[2L], sizes[1L])
    p <- .Call(C_psmirnov_exact, q, sizes[1L], sizes[2L], z,
               two.sided, lower.tail)
    if(log.p)
        log(p)
    else
        p
}

psmirnov_asymp <-
function(q, sizes,
         alternative = c("two.sided", "less", "greater"),
         lower.tail = TRUE, log.p = FALSE) {
    alternative <- match.arg(alternative)
    two.sided <- (alternative == "two.sided")
    n <- prod(sizes) / sum(sizes)
    ## <FIXME>
    ## Let m and n be the min and max of the sample sizes, respectively.
    ## Then, according to Kim and Jennrich (1973), if m < n/10, we
    ## should use the
    ## * Kolmogorov approximation with c.c. -1/(2*n) if 1 < m < 80;
    ## * Smirnov approximation with c.c. 1/(2*sqrt(n)) if m >= 80.
    if (two.sided) {
        ret <- .Call(C_pkolmogorov_two_limit, sqrt(n) * q, lower.tail,
                     tol = 1e-6)
        if(log.p)
            ret <- log(ret)
        return(ret)
    } else {
        ret <- -expm1(- 2 * n * q^2) # 1 - exp(*)
        if(log.p) {
            if(lower.tail)
                log(ret)
            else
                log1p(-ret)
        } else {
            if(lower.tail)
                ret
            else
                1 - ret
        }
    }
}

psmirnov_simul <-
function(q, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         lower.tail = TRUE, log.p = FALSE, B) {
    Dsim <- rsmirnov(B, sizes = sizes, z = z, alternative = alternative)
    ## need P(D < q)
    ## <FIXME>
    ## Sync with the corrections used in the C code.
    ret <- ecdf(Dsim)(q - sqrt(.Machine$double.eps))
    ## </FIXME>
    if(log.p) {
        if(lower.tail)
            log(ret)
        else
            log1p(-ret)
    } else {
        if(lower.tail)
            ret
        else
            1 - ret
    }
}

psmirnov <-
function(q, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         exact = TRUE, simulate = FALSE, B = 2000,
         lower.tail = TRUE, log.p = FALSE) {

    ##
    ## Distribution function Prob(D < q) for the Smirnov test statistic
    ##
    ##   D   = sup_c | ECDF_x(c) - ECDF_y(c) | 	(two.sided)
    ##
    ##   D^+ = sup_c ( ECDF_x(c) - ECDF_y(c) ) 	(greater)
    ##   D^- = sup_c ( ECDF_y(c) - ECDF_x(c) ) 	(less)
    ##
    ## See
    ##
    ##   Gunar Schröer and Dietrich Trenkler (1995),
    ##   Exact and Randomization Distributions of Kolmogorov-Smirnov
    ##   Tests for Two or Three Samples,
    ##   Computational Statistics & Data Analysis, 20, 185--202

    alternative <- match.arg(alternative)
    
    if (is.numeric(q)) {
        if(!is.double(q)) storage.mode(q) <- "double" # keeping dim() etc
    } else stop("argument 'q' must be numeric")
    ret <- q # with attr.
    i1 <- is.na(q)
    ret[i1] <- NA_real_
    if(any(i2 <- (q <= 0))) {
        p <- 1 - lower.tail
        if(log.p) p <- log(p)
        ret[i2] <- p
    }
    if(any(i3 <- (q > 1))) {
        p <- as.numeric(lower.tail)
        if(log.p) p <- log(p)
        ret[i3] <- p
    }
    IND <- which(!(i1 | i2 | i3))
    if (!length(IND)) return(ret)

    if (length(sizes) != 2L)
        stop("argument 'sizes' must be a vector of length 2")
    n.x <- sizes[1L]
    n.y <- sizes[2L]
    if (n.x < 1) stop("not enough 'x' data")
    if (n.y < 1) stop("not enough 'y' data")
    n.x <- floor(n.x)
    n.y <- floor(n.y)
    N <- n.x + n.y
    n <- n.x * n.y / (n.x + n.y)

    ### always return MC prob when asked to
    exact <- exact && !simulate

    if (!exact) {
        ret[IND] <-
            if (simulate)
                psmirnov_simul(q[IND], sizes, z,
                               alternative,lower.tail, log.p,
                               B)
            else
                psmirnov_asymp(q[IND], sizes,
                               alternative, lower.tail, log.p)
        return(ret)
    }

    r <- psmirnov_exact(q[IND], sizes, z, alternative, lower.tail, log.p)
    ret[IND] <-
        if(all(is.finite(r))) r
        else {
            warning("computation of exact probability failed, returning Monte Carlo approximation")
            psmirnov_simul(q[IND], sizes, z,
                           alternative, lower.tail, log.p,
                           B)
        }
    ret
}

qsmirnov <-
function(p, sizes, z = NULL,
         alternative = c("two.sided", "less", "greater"),
         exact = TRUE, simulate = FALSE, B = 2000)
{
    alternative <- match.arg(alternative)
    n.x <- floor(sizes[1L])
    n.y <- floor(sizes[2L])
    if (n.x * n.y < 1e4) {
        ### note: The support is also OK in case of ties
        stat <- sort(unique(c(outer(0:n.x/n.x, 0:n.y/n.y, "-"))))
    } else {
        stat <- (-1e4):1e4 / (1e4 + 1)
    }
    if (alternative == "two.sided") stat <- abs(stat)
    prb <- psmirnov(stat, sizes = sizes, z = z,
                    alternative = alternative,
                    exact = exact, simulate = simulate, B = B,
                    log.p = FALSE, lower.tail = TRUE)
    if (is.null(p)) return(list(stat = stat, prob = prb))
    if (is.numeric(p)) {
        if(!is.double(p)) storage.mode(p) <- "double" # keeping dim() etc
    } else stop("argument 'p' must be numeric")
    ret <- p # inheriting dim(), etc
    ret[is.na(p) | p < 0 | p > 1] <- NA
    IND <- which(!is.na(ret))
    ret[IND] <- vapply(p[IND], function(u) min(stat[prb >= u]), 1.)
    ret
}

pkolmogorov_two_exact <- function(q, n, lower.tail = TRUE) {
    p <- .Call(C_pkolmogorov_two_exact, q, n)
    if(lower.tail) p else 1 - p
}

pkolmogorov_one_exact <- function(q, n, lower.tail = TRUE) {
    ## Probability function for the one-sided one-sample Kolmogorov
    ## statistics, based on the formula of Birnbaum & Tingey (1951).
    j <- seq.int(from = 0, to = floor(n * (1 - q)))
    p <- q * sum(exp(lchoose(n, j)
                     + (n - j) * log(1 - q - j / n)
                     + (j - 1) * log(q + j / n)))
    if(lower.tail) 1 - p else p
}

pkolmogorov_two_asymp <- function(q, n, lower.tail = TRUE) {
    .Call(C_pkolmogorov_two_limit, sqrt(n) * q, lower.tail,
          tol = 1e-6)
}

pkolmogorov_one_asymp <- function(q, n, lower.tail = TRUE) {
    p <- exp(- 2 * n * q^2)
    if(lower.tail) 1 - p else p
}

pkolmogorov <- function(q, size, two.sided = TRUE, exact = TRUE,
                        lower.tail = TRUE) {
    ## Currently not vectorized ...
    ## Note that we compute P(D < q) for the lower tail.
    if(is.na(q))
        return(NA_real_)
    if(q <= 0)
        return(1 - lower.tail)
    if(q > 1)
        return(as.numeric(lower.tail))

    if(exact) {
        if(two.sided)
            pkolmogorov_two_exact(q, size, lower.tail)
        else
            pkolmogorov_one_exact(q, size, lower.tail)
    } else {
        if(two.sided)
            pkolmogorov_two_asymp(q, size, lower.tail)
        else
            pkolmogorov_one_asymp(q, size, lower.tail)
    }
}
