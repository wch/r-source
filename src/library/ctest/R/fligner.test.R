fligner.test <- function(x, g) {
    ## FIXME: This is the same code as in kruskal.test(), and could also
    ## rewrite bartlett.test() accordingly ...
    if (is.list(x)) {
        if (length(x) < 2)
            stop("x must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- as.factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("x and g must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- as.factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }
    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    ## FIXME: now the specific part begins.

    x <- unlist(tapply(x, g, function(u) u - median(u)))
    a <- qnorm((1 + rank(abs(x)) / (n + 1)) / 2)
    STATISTIC <- sum(tapply(a, g, "sum")^2 / tapply(a, g, "length"))
    STATISTIC <- (STATISTIC - n * mean(a)^2) / var(a)
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
    names(STATISTIC) <- "Fligner-Killeen:med chi-squared"
    names(PARAMETER) <- "df"
    METHOD <- "Fligner-Killeen test for homogeneity of variances"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
