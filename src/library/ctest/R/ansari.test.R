ansari.test <- function(x, y,
                        alternative = c("two.sided", "less", "greater"),
                        exact = NULL)
{
    alternative <- match.arg(alternative)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[complete.cases(x)]
    y <- y[complete.cases(y)]
    m <- length(x)
    if (m < 1)
        stop("not enough x observations")
    n <- length(y)
    if (n < 1)
        stop("not enough y observations")
    N <- m + n

    r <- rank(c(x, y))
    STATISTIC <- sum(pmin(r, N - r + 1)[seq(along = x)])
    TIES <- (length(r) != length(unique(r)))

    if (is.null(exact))
        exact <- ((m < 50) && (n < 50))

    if (exact && !TIES) {
        pansari <- function(q, m, n) {
            .C("pansari",
               as.integer(length(q)),
               p = as.double(q),
               as.integer(m),
               as.integer(n),
               PACKAGE = "ctest")$p
        }
        PVAL <-
            switch(alternative,
                   "two.sided" = {
                       if (STATISTIC > ((m + 1)^2 %/% 4
                                        + ((m * n) %/% 2) / 2))
                           p <- 1 - pansari(STATISTIC - 1, m, n)
                       else
                           p <- pansari(STATISTIC, m, n)
                       min(2 * p, 1)
                   },
                   "greater" = 1 - pansari(STATISTIC - 1, m, n),
                   "less" = pansari(STATISTIC, m, n))
    } else {
        EVEN <- ((N %% 2) == 0)
        z <- if (EVEN)
            STATISTIC - m * (N + 2) / 4
        else
            STATISTIC - m * (N + 1)^2 / (4 * N)
        if (!TIES) {
            SIGMA <- if (EVEN)
                sqrt((m * n * (N + 2) * (N - 2)) / (48 * (N - 1)))
            else
                sqrt((m * n * (N + 1) * (3 + N^2)) / (48 * N^2))
        } else {
            r <- rle(sort(pmin(r, N - r + 1)))
            SIGMA <- if (EVEN)
                sqrt(m * n * (16 * sum(r$l * r$v^2) - N * (N + 2)^2)
                     / (16 * N * (N - 1)))
            else
                sqrt(m * n * (16 * N * sum(r$l * r$v^2) - (N + 1)^4)
                     / (16 * N^2 * (N - 1)))
        }
        p <- pnorm(z / SIGMA)
        PVAL <- switch(alternative,
                       "two.sided" = 2 * min(p, 1 - p),
                       "greater" = 1 - p,
                       "less" = p)
        if (exact && TIES)
            warning("Cannot compute exact p-value with ties")
    }

    names(STATISTIC) <- "AB"
    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = alternative,
                 method = "Ansari-Bradley test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}
