mantelhaen.test <- function(x, y = NULL, z = NULL, correct = TRUE)
{
    DNAME <- deparse(substitute(x))
    if(is.array(x)) {
        if(length(dim(x)) == 3) {
            if(any(is.na(x)))
                stop("NAs are not allowed")
            if(any(dim(x) < 2))
                stop("each dimension in table must be >= 2")
        }
        else
            stop("x must be a 3-dimensional array")
    }
    else {
        if(is.null(y))
            stop("If x is not an array, y must be given")
        if(is.null(z))
            stop("If x is not an array, z must be given")
        if(any(diff(c(length(x), length(y), length(z)))))
            stop("x, y, and z must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)), "and",
                       deparse(substitute(z)))
        OK <- complete.cases(x, y, z)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if((nlevels(x) < 2) || (nlevels(y) < 2))
            stop("x and y must have at least 2 levels")
        else
            x <- table(x, y, z[OK])
    }

    if(any(apply(x, 3, sum) < 2))
        stop("sample size in each stratum must be > 1")

    I <- dim(x)[1]
    J <- dim(x)[2]
    K <- dim(x)[3]

    if((I == 2) && (J == 2)) {
        ## Classical Mantel-Haenszel 2 x 2 x K test
        s.x <- apply(x, c(1, 3), sum)
        s.y <- apply(x, c(2, 3), sum)
        n <- apply(x, 3, sum)
        DELTA <- abs(sum(x[1, 1, ] - s.x[1, ] * s.y[1, ] / n))
        YATES <- ifelse(correct && (DELTA >= .5), .5, 0)
        STATISTIC <- ((DELTA - YATES)^2 /
                      sum(apply(rbind(s.x, s.y), 2, prod)
                          / (n^2 * (n - 1))))
        PARAMETER <- 1
        names(STATISTIC) <- "Mantel-Haenszel X-square"
        METHOD <- paste("Mantel-Haenszel chi-square test",
                        ifelse(YATES, "with", "without"),
                        "continuity correction")
    }
    else {
        ## Generalized Cochran-Mantel-Haenszel I x J x K test
        ## Agresti (1990), pages 234--235
        df <- (I - 1) * (J - 1)
        n <- m <- double(length = df)
        V <- matrix(0, nr = df, nc = df)
        for (k in 1 : K) {
            f <- x[ , , k]              # frequencies in stratum k
            ntot <- sum(f)              # n_{..k}
            rowsums <- apply(f, 1, sum)[-I]
                                        # n_{i.k}, i = 1 to I-1
            colsums <- apply(f, 2, sum)[-J]
                                        # n_{.jk}, j = 1 to J-1
            n <- n + c(f[-I, -J])
            m <- m + c(outer(rowsums, colsums, "*")) / ntot
            V <- V + (kronecker(diag(ntot * rowsums, nrow = I - 1)
                                - outer(rowsums, rowsums),
                                diag(ntot * colsums, nrow = J - 1)
                                - outer(colsums, colsums))
                      / (ntot^2 * (ntot - 1)))
        }
        n <- n - m
        STATISTIC <- crossprod(n, qr.solve(V, n))
        PARAMETER <- df
        names(STATISTIC) <- "Cochran-Mantel-Haenszel M^2"
        METHOD <- "Cochran-Mantel-Haenszel test"
    }

    names(PARAMETER) <- "df"
    RVAL <- list(statistic = STATISTIC,
                 parameter = PARAMETER,
                 p.value = 1 - pchisq(STATISTIC, PARAMETER),
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
