dexp <- function(x, rate=1, log = FALSE) .Internal(dexp(x, 1/rate, log))
pexp <- function(q, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pexp(q, 1/rate, lower.tail, log.p))
qexp <- function(p, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qexp(p, 1/rate, lower.tail, log.p))
rexp <- function(n, rate=1) .Internal(rexp(n, 1/rate))

dunif <- function(x, min=0, max=1, log = FALSE)
    .Internal(dunif(x, min, max, log))
punif <- function(q, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(punif(q, min, max, lower.tail, log.p))
qunif <- function(p, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qunif(p, min, max, lower.tail, log.p))
runif <- function(n, min=0, max=1) .Internal(runif(n, min, max))

dnorm <- function(x, mean=0, sd=1, log=FALSE) .Internal(dnorm(x, mean, sd, log))
pnorm <- function(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pnorm(q, mean, sd, lower.tail, log.p))
qnorm <- function(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qnorm(p, mean, sd, lower.tail, log.p))
rnorm <- function(n, mean=0, sd=1) .Internal(rnorm(n, mean, sd))

dcauchy <- function(x, location=0, scale=1, log = FALSE)
    .Internal(dcauchy(x, location, scale, log))
pcauchy <-
    function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pcauchy(q, location, scale, lower.tail, log.p))
qcauchy <-
    function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qcauchy(p, location, scale, lower.tail, log.p))
rcauchy <-
    function(n, location=0, scale=1) .Internal(rcauchy(n, location, scale))

dgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE)
    .Internal(dgamma(x, shape, scale, log))
pgamma <- function(q, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
    .Internal(pgamma(q, shape, scale, lower.tail, log.p))

qgamma <- function(p, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
    .Internal(qgamma(p, shape, scale, lower.tail, log.p))
rgamma <- function(n, shape, rate = 1, scale = 1/rate)
    .Internal(rgamma(n, shape, scale))

dlnorm <- function(x, meanlog=0, sdlog=1, log=FALSE)
    .Internal(dlnorm(x, meanlog, sdlog, log))
plnorm <- function(q, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(plnorm(q, meanlog, sdlog, lower.tail, log.p))
qlnorm <- function(p, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qlnorm(p, meanlog, sdlog, lower.tail, log.p))
rlnorm <- function(n, meanlog=0, sdlog=1) .Internal(rlnorm(n, meanlog, sdlog))

dlogis <- function(x, location=0, scale=1, log = FALSE)
    .Internal(dlogis(x, location, scale, log))
plogis <- function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(plogis(q, location, scale, lower.tail, log.p))
qlogis <- function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qlogis(p, location, scale, lower.tail, log.p))
rlogis <- function(n, location=0, scale=1) .Internal(rlogis(n, location, scale))

dweibull <- function(x, shape, scale=1, log = FALSE)
    .Internal(dweibull(x, shape, scale, log))
pweibull <- function(q, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(pweibull(q, shape, scale, lower.tail, log.p))
qweibull <- function(p, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qweibull(p, shape, scale, lower.tail, log.p))
rweibull <- function(n, shape, scale=1) .Internal(rweibull(n, shape, scale))

dbeta <- function(x, shape1, shape2, ncp=0, log = FALSE) {
    if(missing(ncp)) .Internal(dbeta(x, shape1, shape2, log))
    else .Internal(dnbeta(x, shape1, shape2, ncp, log))
}
pbeta <- function(q, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pbeta(q, shape1, shape2, lower.tail, log.p))
    else .Internal(pnbeta(q, shape1, shape2, ncp, lower.tail, log.p))
}
qbeta <- function(p, shape1, shape2, lower.tail = TRUE, log.p = FALSE)
    .Internal(qbeta(p, shape1, shape2, lower.tail, log.p))
rbeta <- function(n, shape1, shape2) .Internal(rbeta(n, shape1, shape2))

dbinom <- function(x, size, prob, log = FALSE)
    .Internal(dbinom(x, size, prob, log))
pbinom <- function(q, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(pbinom(q, size, prob, lower.tail, log.p))
qbinom <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(qbinom(p, size, prob, lower.tail, log.p))
rbinom <- function(n, size, prob) .Internal(rbinom(n, size, prob))

## Multivariate: that's why there's no C interface (yet) for d...():
dmultinom <- function(x, size=NULL, prob, log = FALSE)
{
    K <- length(prob)
    if(length(x) != K) stop("x[] and prob[] must be equal length vectors.")
    if(any(prob < 0) || (s <- sum(prob)) == 0)
	stop("probabilities cannot be negative nor all 0.")
    prob <- prob / s

    x <- as.integer(x + 0.5)
    if(any(x < 0)) stop("'x' must be non-negative")
    N <- sum(x)
    if(is.null(size)) size <- N
    else if (size != N) stop("size != sum(x), i.e. one is wrong")

    i0 <- prob == 0
    if(any(i0)) {
	if(any(x[i0] != 0))
            ##  prob[j] ==0 and x[j] > 0 ==>  "impossible" => P = 0
	    return(if(log)-Inf else 0)
	## otherwise : 'all is fine': prob[j]= 0 = x[j] ==> drop j and continue
	if(all(i0)) return(if(log)0 else 1)
	## else
	x <- x[!i0]
	prob <- prob[!i0]
    }
    r <- lgamma(size+1) + sum(x*log(prob) - lgamma(x+1))
    if(log) r else exp(r)
}
rmultinom <- function(n, size, prob) .Internal(rmultinom(n, size, prob))

dchisq <- function(x, df, ncp=0, log = FALSE) {
    if(missing(ncp)) .Internal(dchisq(x, df, log))
    else .Internal(dnchisq(x, df, ncp, log))
}
pchisq <- function(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pchisq(q, df, lower.tail, log.p))
    else .Internal(pnchisq(q, df, ncp, lower.tail, log.p))
}
qchisq <- function(p, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(qchisq(p, df, lower.tail, log.p))
    else .Internal(qnchisq(p, df, ncp, lower.tail, log.p))
}
rchisq <- function(n, df, ncp=0) {
    if(missing(ncp)) .Internal(rchisq(n, df))
    else .Internal(rnchisq(n, df, ncp))
}

df <- function(x, df1, df2, log = FALSE) .Internal(df(x, df1, df2, log))
pf <- function(q, df1, df2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Internal(pf(q, df1, df2, lower.tail, log.p))
    else .Internal(pnf(q, df1, df2, ncp, lower.tail, log.p))
}
qf <- function(p, df1, df2, lower.tail = TRUE, log.p = FALSE)
    .Internal(qf(p, df1, df2, lower.tail, log.p))
rf <- function(n, df1, df2) .Internal(rf(n, df1, df2))

dgeom <- function(x, prob, log = FALSE) .Internal(dgeom(x, prob, log))
pgeom <- function(q, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(pgeom(q, prob, lower.tail, log.p))
qgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE)
    .Internal(qgeom(p, prob, lower.tail, log.p))
rgeom <- function(n, prob) .Internal(rgeom(n, prob))

dhyper <- function(x, m, n, k, log = FALSE) .Internal(dhyper(x, m, n, k, log))
phyper <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Internal(phyper(q, m, n, k, lower.tail, log.p))
qhyper <- function(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Internal(qhyper(p, m, n, k, lower.tail, log.p))
rhyper <- function(nn, m, n, k) .Internal(rhyper(nn, m, n, k))

dnbinom <- function(x, size, prob, mu, log = FALSE)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        prob <- size/(size + mu)
    }
    .Internal(dnbinom(x, size, prob, log))
}
pnbinom <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        prob <- size/(size + mu)
    }
    .Internal(pnbinom(q, size, prob, lower.tail, log.p))
}
qnbinom <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        prob <- size/(size + mu)
    }
    .Internal(qnbinom(p, size, prob, lower.tail, log.p))
}
rnbinom <- function(n, size, prob, mu)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        prob <- size/(size + mu)
    }
    .Internal(rnbinom(n, size, prob))
}

dpois <- function(x, lambda, log = FALSE) .Internal(dpois(x, lambda, log))
ppois <- function(q, lambda, lower.tail = TRUE, log.p = FALSE)
    .Internal(ppois(q, lambda, lower.tail, log.p))
qpois <- function(p, lambda, lower.tail = TRUE, log.p = FALSE)
    .Internal(qpois(p, lambda, lower.tail, log.p))
rpois <- function(n, lambda) .Internal(rpois(n, lambda))

dt <- function(x, df, ncp=0, log = FALSE) {
    if(missing(ncp))
	.Internal(dt(x, df, log))
    else
	.Internal(dnt(x, df, ncp, log))
}

pt <- function(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp))
	.Internal(pt(q, df, lower.tail, log.p))
    else
	.Internal(pnt(q, df, ncp, lower.tail, log.p))
}
qt <- function(p, df, lower.tail = TRUE, log.p = FALSE)
    .Internal(qt(p, df, lower.tail, log.p))
rt <- function(n, df) .Internal(rt(n, df))

ptukey <- function(q, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(ptukey(q, nranges, nmeans, df, lower.tail, log.p))
qtukey <- function(p, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Internal(qtukey(p, nranges, nmeans, df, lower.tail, log.p))

dwilcox <- function(x, m, n, log = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(dwilcox(x, m, n, log))
}
pwilcox <- function(q, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(pwilcox(q, m, n, lower.tail, log.p))
}
qwilcox <- function(p, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("wilcox_free", PACKAGE = "base"))
    .Internal(qwilcox(p, m, n, lower.tail, log.p))
}
rwilcox <- function(nn, m, n) .Internal(rwilcox(nn, m, n))

dsignrank <- function(x, n, log = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(dsignrank(x, n, log))
}
psignrank <- function(q, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(psignrank(q, n, lower.tail, log.p))
}
qsignrank <- function(p, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.C("signrank_free", PACKAGE = "base"))
    .Internal(qsignrank(p, n, lower.tail, log.p))
}
rsignrank <- function(nn, n) .Internal(rsignrank(nn, n))
