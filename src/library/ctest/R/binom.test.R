binom.test <- function(x, n, p = 0.5, alternative = "two.sided",
                       conf.level = 0.95) {
    if((length(n) > 1) || is.na(n) || (n < 1) || (n != round(n)))
        stop("n must be a positive integer")
    if((length(x) > 1) || is.na(x) ||
       (x < 0) || (x > n) || (x != round(x)))
        stop("x must be an integer between 0 and n")
    if(!missing(p) && (length(p) > 1 || is.na(p) || p < 0 || p > 1))
        stop ("p must be a single number between 0 and 1")

    alternative <- char.expand(alternative,
                               c("two.sided", "less", "greater"))
    if(length(alternative) > 1 || is.na(alternative))
        stop("alternative must be \"two.sided\", \"less\" or \"greater\"")

    if(!((length(conf.level) == 1) && is.finite(conf.level) &&
         (conf.level > 0) && (conf.level < 1)))
        stop("conf.level must be a single number between 0 and 1")

    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(n)))

    PVAL <- switch(alternative,
                   less = pbinom(x, n, p),
                   greater = 1 - pbinom(x - 1, n, p),
                   two.sided = {
                       if(p == 0)
                           (x == 0)
                       else if(p == 1)
                           (x == n)
                       else {
                           d <- dbinom(x, n, p)
                           if(x / n < p) {
                               i <- seq(from = x + 1, to = n)
                               y <- sum(dbinom(i, n, p) <= d)
                               pbinom(x, n, p) +
                                   (1 - pbinom(n - y, n, p))
                           } else {
                               i <- seq(from = 0, to = x - 1)
                               y <- sum(dbinom(i, n, p) <= d)
                               pbinom(y - 1, n, p) +
                                   (1 - pbinom(x - 1, n, p))
                           }
                       }
                   })
    CINT <- switch(alternative,
                   less = c(0, qbinom(conf.level, n, p)),
                   greater = c(n - qbinom(conf.level, n, 1 - p), n),
                   two.sided = {
                       beta <- (1 + conf.level) / 2
                       c(n - qbinom(beta, n, 1 - p), qbinom(beta, n, p))
                   })
    attr(CINT, "conf.level") <- conf.level
  
    names(x) <- "number of successes"	# or simply "x" ??
    names(n) <- "number of trials"	# or simply "n" ??
    names(p) <- "probability of success"# or simply "p" ??

    structure(list(statistic = x,
                   parameter = n,
                   p.value = PVAL,
                   conf.int = CINT,
                   null.value = p,
                   alternative = alternative,
                   method = "Exact binomial test",
                   data.name = DNAME),
              class = "htest")
}
