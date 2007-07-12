ks.test <-
function(x, y, ..., alternative = c("two.sided", "less", "greater"),
         exact = NULL)
{

    pkolmogorov1x <- function(x, n) {
        ## Probability function for the one-sided one-sample Kolmogorov
        ## statistics, based on the formula of Birnbaum & Tingey (1951).
        if(x <= 0) return(0)
        if(x >= 1) return(1)
        j <- seq.int(from = 0, to = floor(n * (1 - x)))
        1 - x * sum(exp(lchoose(n, j)
                        + (n - j) * log(1 - x - j / n)
                        + (j - 1) * log(x + j / n)))
    }

    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1)
        stop("not enough 'x' data")
    PVAL <- NULL

    if(is.numeric(y)) {
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)             # to avoid integer overflow
        n.y <- length(y)
        if(n.y < 1)
            stop("not enough 'y' data")
        if(is.null(exact))
            exact <- (n.x * n.y < 10000)
        METHOD <- "Two-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        n <- n.x * n.y / (n.x + n.y)
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1 / n.x, - 1 / n.y))
        if(length(unique(w)) < (n.x + n.y)) {
            warning("cannot compute correct p-values with ties")
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
        }
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(z)),
                            "greater" = max(z),
                            "less" = - min(z))
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below that of y",
                                 "greater" = "the CDF of x lies above that of y")
        if(exact && (alternative == "two.sided") && !TIES)
            PVAL <- 1 - .C("psmirnov2x",
                           p = as.double(STATISTIC),
                           as.integer(n.x),
                           as.integer(n.y),
                           PACKAGE = "stats")$p
    }
    else {
        if(is.character(y))
            y <- get(y, mode="function")
        if(mode(y) != "function")
            stop("'y' must be numeric or a string naming a valid function")
        if(is.null(exact))
            exact <- (n < 100)
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        if(length(unique(x)) < n) {
            warning("cannot compute correct p-values with ties")
            TIES <- TRUE
        }
        x <- y(sort(x), ...) - (0 : (n-1)) / n
        STATISTIC <- switch(alternative,
                            "two.sided" = max(c(x, 1/n - x)),
                            "greater" = max(1/n - x),
                            "less" = max(x))
        if(exact && !TIES) {
            PVAL <- if(alternative == "two.sided")
                1 - .C("pkolmogorov2x",
                           p = as.double(STATISTIC),
                           as.integer(n),
                           PACKAGE = "stats")$p
            else
                1 - pkolmogorov1x(STATISTIC, n)
        }
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below the null hypothesis",
                                 "greater" = "the CDF of x lies above the null hypothesis")

    }

    names(STATISTIC) <- switch(alternative,
                               "two.sided" = "D",
                               "greater" = "D^+",
                               "less" = "D^-")

    pkstwo <- function(x, tol = 1e-6) {
        ## Compute \sum_{-\infty}^\infty (-1)^k e^{-2k^2x^2}
        ## Not really needed at this generality for computing a single
        ## asymptotic p-value as below.
        if(is.numeric(x))
            x <- as.vector(x)
        else
            stop("argument 'x' must be numeric")
        p <- rep(0, length(x))
        p[is.na(x)] <- NA
        IND <- which(!is.na(x) & (x > 0))
        if(length(IND) > 0) {
            p[IND] <- .C("pkstwo",
                         as.integer(length(x[IND])),
                         p = as.double(x[IND]),
                         as.double(tol),
                         PACKAGE = "stats")$p
        }
        return(p)
    }

    if(is.null(PVAL)) {
        ## <FIXME>
        ## Currently, p-values for the two-sided two-sample case are
        ## exact if n.x * n.y < 10000 (unless controlled explicitly).
        ## In all other cases, the asymptotic distribution is used
        ## directly.  But: let m and n be the min and max of the sample
        ## sizes, respectively.  Then, according to Kim and Jennrich
        ## (1973), if m < n / 10, we should use the
        ## * Kolmogorov approximation with c.c. -1/(2*n) if 1 < m < 80;
        ## * Smirnov approximation with c.c. 1/(2*sqrt(n)) if m >= 80.
        PVAL <- ifelse(alternative == "two.sided",
                       1 - pkstwo(sqrt(n) * STATISTIC),
                       exp(- 2 * n * STATISTIC^2))
        ## </FIXME>
    }

    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = nm_alternative,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
