mood.test <- function(x, ...) UseMethod("mood.test")

mood.test.default <-
function(x, y, alternative = c("two.sided", "less", "greater"), ...)
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

mood.test.formula <-
function(formula, data, subset, na.action, ...)
{
    if(missing(formula)
       || (length(formula) != 3)
       || (length(attr(terms(formula[-2]), "term.labels")) != 1)
       || (length(attr(terms(formula[-3]), "term.labels")) != 1))
        stop("'formula' missing or incorrect")
    m <- match.call(expand.dots = FALSE)
    if(is.matrix(eval(m$data, parent.frame())))
        m$data <- as.data.frame(data)
    m[[1]] <- as.name("model.frame")
    m$... <- NULL
    mf <- eval(m, parent.frame())
    DNAME <- paste(names(mf), collapse = " by ")
    names(mf) <- NULL
    response <- attr(attr(mf, "terms"), "response")
    g <- factor(mf[[-response]])
    if(nlevels(g) != 2)
        stop("grouping factor must have exactly 2 levels")
    DATA <- split(mf[[response]], g)
    names(DATA) <- c("x", "y")
    y <- do.call("mood.test", c(DATA, list(...)))
    y$data.name <- DNAME
    y
}
