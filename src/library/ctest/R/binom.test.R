binom.test <-
function(x, n, p = 0.5, alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)
{
    if(any(is.na(x) || (x < 0) || (x != round(x))))
        stop("x must be nonnegative and integer")
    if(length(x) == 2) {
        ## x gives successes and failures
        n <- sum(x)
        x <- x[1]
    }
    else if(length(x) == 1) {
        ## x gives successes, n gives trials
        if((length(n) > 1) || is.na(n) || (n < 1) || (n != round(n))
           || (x > n))
            stop("n must be a positive integer >= x")
    }
    else
        stop("incorrect length of x")

    if(!missing(p) && (length(p) > 1 || is.na(p) || p < 0 || p > 1))
        stop ("p must be a single number between 0 and 1")
    alternative <- match.arg(alternative)

    if(!((length(conf.level) == 1) && is.finite(conf.level) &&
         (conf.level > 0) && (conf.level < 1)))
        stop("conf.level must be a single number between 0 and 1")

    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(n)))

    PVAL <- switch(alternative,
                   less = pbinom(x, n, p),
                   greater = pbinom(x - 1, n, p, lower = FALSE),
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
                           ## Note that we need a little fuzz.
                           relErr <- 1 + 10 ^ (-7) 
                           d <- dbinom(x, n, p)
                           if(x / n < p) {
                               i <- seq(from = x + 1, to = n)
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(x, n, p) +
                                   pbinom(n - y, n, p, lower = FALSE)
                           } else {
                               i <- seq(from = 0, to = x - 1)
                               y <- sum(dbinom(i, n, p) <= d * relErr)
                               pbinom(y - 1, n, p) +
                                   pbinom(x - 1, n, p, lower = FALSE)
                           }
                       }
                   })
    ## Determine p s.t. Prob(B(n,p) >= x) = alpha.
    ## Use that for x > 0,
    ##   Prob(B(n,p) >= x) = pbeta(p, x, n - x + 1).
    p.L <- function(x, alpha) {
        if(x == 0)                      # No solution
            0
        else
            qbeta(alpha, x, n - x + 1)
    }
    ## Determine p s.t. Prob(B(n,p) <= x) = alpha.
    ## Use that for x < n,
    ##   Prob(B(n,p) <= x) = 1 - pbeta(p, x + 1, n - x).
    p.U <- function(x, alpha) {
        if(x == n)                      # No solution
            1
        else
            qbeta(1 - alpha, x + 1, n - x)
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
