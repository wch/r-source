mood.test <- function(x, y, alternative = c("two.sided", "less", "greater"))
{
    alternative <- match.arg(alternative)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))

    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    m <- length(x)
    n <- length(y)
    if ((s <- m + n) < 3)
        stop("not enough observations")
    r <- rank(c(x, y))
    z <- ((sum((r[seq(along = x)] - (s + 1) / 2)^2) - m * (s^2 - 1) / 12)
          / sqrt(m * n * (s + 1) * (s + 2) * (s - 2) / 180))
    p <- pnorm(z)
    PVAL <- switch(alternative,
                   "less" = p,
                   "greater" = 1 - p,
                   "two.sided" = 2 * min(p, 1 - p))

    structure(list(statistic = structure(z, names = "Z"),
                   p.value = PVAL,
                   alternative = alternative,
                   method = "Mood two-sample test of scale",
                   data.name = DNAME),
              class = "htest")
}
