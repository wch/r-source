friedman.test <- function(y, groups, blocks) {
    DNAME <- deparse(substitute(y))
    if (is.matrix(y)) {
        groups <- as.factor(c(col(y)))
        blocks <- as.factor(c(row(y)))
    }
    else {
        if (any(is.na(groups)) || any(is.na(blocks)))
            stop("NA's are not allowed in groups or blocks")
        if (any(diff(c(length(y), length(groups), length(blocks)))))
            stop("y, groups and blocks must have the same length")
        DNAME <- paste(DNAME, ", ", deparse(substitute(groups)),
                       " and ", deparse(substitute(blocks)), sep = "")
        if (any(table(groups, blocks) != 1))
            stop("Not an unreplicated complete block design")
        groups <- as.factor(groups)
        blocks <- as.factor(blocks)
    }

    k <- nlevels(groups)
    y <- matrix(unlist(split(y, blocks)), ncol = k, byrow = TRUE)
    y <- y[complete.cases(y), ]
    n <- nrow(y)
    r <- t(apply(y, 1, rank))
    TIES <- tapply(r, row(r), table)
    STATISTIC <- ((12 * sum((apply(r, 2, sum) - n * (k + 1) / 2)^2)) /
                  (n * k * (k + 1)
                   - (sum(unlist(lapply(TIES, function (u) {u^3 - u}))) /
                      (k - 1))))
    PARAMETER <- k - 1
    names(STATISTIC) <- "Friedman chi-square"
    names(PARAMETER) <- "df"

    structure(list(statistic = STATISTIC,
                   parameter = PARAMETER,
                   p.value = 1 - pchisq(STATISTIC, PARAMETER),
                   method = "Friedman rank sum test",
                   data.name = DNAME),
              class = "htest")
}
