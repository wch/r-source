binom.test <-
function(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)
{
    if((length(n) > 1) || is.na(n) || (n < 1) || (n != round(n)))
        stop("n must be a positive integer")
    if((length(x) > 1) || is.na(x) ||
       (x < 0) || (x > n) || (x != round(x)))
        stop("x must be an integer between 0 and n")
    if(!missing(p) && (length(p) > 1 || is.na(p) || p < 0 || p > 1))
        stop ("p must be a single number between 0 and 1")
    alternative <- match.arg(alternative)

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
                           ## Do
                           ##   d <- dbinom(0 : n, n, p)
                           ##   sum(d[d <= dbinom(x, n, p)])
                           ## a bit more efficiently ...
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
    ## Determine p s.t. Prob(B(n,p) >= x) = alpha
    p.L <- function(x, alpha) {
        if(x == 0)                      # No solution
            0
        else
            uniroot(function(p) 1 - pbinom(x - 1, n, p) - alpha,
                    c(0, 1))$root
    }
    ## Determine p s.t. Prob(B(n,p) <= x) = alpha
    p.U <- function(x, alpha) {
        if(x == n)                      # No solution
            1
        else
            uniroot(function(p) pbinom(x, n, p) - alpha,
                    c(0, 1))$root
    }
    CINT <- switch(alternative,
                   less = c(0, p.U(x, 1 - conf.level)),
                   greater = c(p.L(x, 1 - conf.level), 1),
                   two.sided = {
                       alpha <- (1 - conf.level) / 2
                       c(p.L(x, alpha), p.U(x, alpha))
                   })
    attr(CINT, "conf.level") <- conf.level

    ESTIMATE <- x / n

    names(x) <- "number of successes"	# or simply "x" ??
    names(n) <- "number of trials"	# or simply "n" ??
    names(ESTIMATE) <-
    names(p) <- "probability of success"# or simply "p" ??

    structure(list(statistic = x,
                   parameter = n,
                   p.value = PVAL,
                   conf.int = CINT,
                   estimate = ESTIMATE,
                   null.value = p,
                   alternative = alternative,
                   method = "Exact binomial test",
                   data.name = DNAME),
              class = "htest")
}



