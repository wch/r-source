kruskal.test <- function(x, ...) UseMethod("kruskal.test")

kruskal.test.default <-
function(x, g, ...)
{
    if (is.list(x)) {
        if (length(x) < 2)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        x <- lapply(x, function(u) u <- u[complete.cases(u)])
        k <- length(x)
        l <- sapply(x, "length")
        if (any(l == 0))
            stop("all groups must contain data")
        g <- factor(rep(1 : k, l))
        x <- unlist(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- g[OK]
        if (!all(is.finite(g)))
            stop("all group levels must be finite")
        g <- factor(g)
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
    }

    n <- length(x)
    if (n < 2)
        stop("not enough observations")
    r <- rank(x)
    TIES <- table(x)
    STATISTIC <- sum(tapply(r, g, "sum")^2 / tapply(r, g, "length"))
    STATISTIC <- ((12 * STATISTIC / (n * (n + 1)) - 3 * (n + 1)) /
                  (1 - sum(TIES^3 - TIES) / (n^3 - n)))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
    names(STATISTIC) <- "Kruskal-Wallis chi-squared"
    names(PARAMETER) <- "df"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 method = "Kruskal-Wallis rank sum test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

kruskal.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula) || (length(formula) != 3))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    y <- do.call("kruskal.test", as.list(mf))
    y$data.name <- DNAME
    y
}
