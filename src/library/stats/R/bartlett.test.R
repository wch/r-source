bartlett.test <- function(x, ...) UseMethod("bartlett.test")

bartlett.test.default <-
function(x, g, ...)
{
    LM <- FALSE
    if (is.list(x)) {
        if (length(x) < 2)
            stop("'x' must be a list with at least 2 elements")
        DNAME <- deparse(substitute(x))
        if (all(sapply(x, function(obj) inherits(obj, "lm"))))
            LM <- TRUE
        else
            x <- lapply(x, function(x) x <- x[is.finite(x)])
        k <- length(x)
    }
    else {
        if (length(x) != length(g))
            stop("'x' and 'g' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and",
                       deparse(substitute(g)))
        OK <- complete.cases(x, g)
        x <- x[OK]
        g <- factor(g[OK])
        k <- nlevels(g)
        if (k < 2)
            stop("all observations are in the same group")
        x <- split(x, g)
    }

    if (LM) {
        n <- sapply(x, function(obj) obj$df.resid)
        v <- sapply(x, function(obj) sum(obj$residuals^2))
    } else {
        n <- sapply(x, "length") - 1
        if (any(n <= 0))
            stop("there must be at least 2 observations in each group")
        v <- sapply(x, "var")
    }

    n.total <- sum(n)
    v.total <- sum(n * v) / n.total
    STATISTIC <- ((n.total * log(v.total) - sum(n * log(v))) /
                  (1 + (sum(1 / n) - 1 / n.total) / (3 * (k - 1))))
    PARAMETER <- k - 1
    PVAL <- pchisq(STATISTIC, PARAMETER, lower = FALSE)
    names(STATISTIC) <- "Bartlett's K-squared"
    names(PARAMETER) <- "df"

    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = PVAL,
                 data.name = DNAME,
                 method = "Bartlett test for homogeneity of variances")
    class(RVAL) <- "htest"
    return(RVAL)
}

bartlett.test.formula <-
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
    y <- do.call("bartlett.test", as.list(mf))
    y$data.name <- DNAME
    y
}
