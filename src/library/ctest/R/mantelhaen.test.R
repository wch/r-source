mantelhaen.test <- function(x, y = NULL, z = NULL, correct = TRUE)
{
    DNAME <- deparse(substitute(x))
    if (is.array(x)) {
        if (length(dim(x)) == 3) {
            if (any(is.na(x)))
                stop("NAs are not allowed")
            if (dim(x)[1:2] != c(2, 2))
                stop("table for each stratum must be 2 by 2")
        }
        else
            stop("x must be a 3-dimensional array")
    }
    else {
        if (is.null(y))
            stop("If x is not an array, y must be given")
        if (is.null(z))
            stop("If x is not an array, z must be given")
        if (any(diff(c(length(x), length(y), length(z)))))
            stop("x, y, and z must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)), "and",
                       deparse(substitute(z)))
        OK <- complete.cases(x, y, z)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if ((nlevels(x) != 2) || (nlevels(y) != 2))
            stop("x and y must be dichotomous")
        else
            x <- table(x, y, z[OK])
    }

    s.x <- apply(x, c(1, 3), sum)
    s.y <- apply(x, c(2, 3), sum)
    n <- apply(x, 3, sum)
    if (any(n < 2))
        stop("sample size in each stratum must be > 1")
    DELTA <- abs(sum(x[1, 1, ] - s.x[1, ] * s.y[1, ] / n))
    YATES <- ifelse(correct && (DELTA >= .5), .5, 0)
    STATISTIC <- ((DELTA - YATES)^2 /
                  sum(apply(rbind(s.x, s.y), 2, prod)
                      / (n^2 * (n - 1))))
    PARAMETER <- 1
    names(STATISTIC) <- "Mantel-Haenszel X-square"
    names(PARAMETER) <- "df"
    
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = 1 - pchisq(STATISTIC, PARAMETER),
                 method = paste("Mantel-Haenszel chi-square test",
                 ifelse(YATES, "with", "without"),
                 "continuity correction"),
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
