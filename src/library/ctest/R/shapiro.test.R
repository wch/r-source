shapiro.test <- function(x) {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if ((n < 3) || (n > 5000))
        stop("sample size must be between 3 and 5000")
    n2 <- floor(n / 2)
    sw <- .Fortran("swilk",
                   as.logical(FALSE),
                   as.single(x),
                   as.integer(n),
                   as.integer(n),
                   as.integer(n2),
                   single(n2),
                   w = single(1),
                   pw = single(1),
                   ifault = integer(1),
                   PACKAGE = "ctest")
    if (sw$ifault != 0)
        stop("this should not happen")
    STATISTIC <- sw$w
    names(STATISTIC) <- "W"
    RVAL <- list(statistic = STATISTIC,
                 p.value = sw$pw,
                 method = "Shapiro-Wilk normality test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
