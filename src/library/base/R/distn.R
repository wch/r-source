dexp <- function(x, rate=1) .Internal(dexp(x, 1/rate))
pexp <- function(q, rate=1) .Internal(pexp(q, 1/rate))
qexp <- function(p, rate=1) .Internal(qexp(p, 1/rate))
rexp <- function(n, rate=1) .Internal(rexp(n, 1/rate))

dunif <- function(x, min=0, max=1) .Internal(dunif(x, min, max))
punif <- function(q, min=0, max=1) .Internal(punif(q, min, max))
qunif <- function(p, min=0, max=1) .Internal(qunif(p, min, max))
runif <- function(n, min=0, max=1) .Internal(runif(n, min, max))

dnorm <- function(x, mean=0, sd=1) .Internal(dnorm(x, mean, sd))
pnorm <- function(q, mean=0, sd=1) .Internal(pnorm(q, mean, sd))
qnorm <- function(p, mean=0, sd=1) .Internal(qnorm(p, mean, sd))
rnorm <- function(n, mean=0, sd=1) .Internal(rnorm(n, mean, sd))

dcauchy <-
    function(x, location=0, scale=1) .Internal(dcauchy(x, location, scale))
pcauchy <-
    function(q, location=0, scale=1) .Internal(pcauchy(q, location, scale))
qcauchy <-
    function(p, location=0, scale=1) .Internal(qcauchy(p, location, scale))
rcauchy <-
    function(n, location=0, scale=1) .Internal(rcauchy(n, location, scale))

dgamma <- function(x, shape, scale=1) .Internal(dgamma(x, shape, scale))
pgamma <- function(q, shape, scale=1) .Internal(pgamma(q, shape, scale))
qgamma <- function(p, shape, scale=1) .Internal(qgamma(p, shape, scale))
rgamma <- function(n, shape, scale=1) .Internal(rgamma(n, shape, scale))

dlnorm <- function(x, meanlog=0, sdlog=1) .Internal(dlnorm(x, meanlog, sdlog))
plnorm <- function(q, meanlog=0, sdlog=1) .Internal(plnorm(q, meanlog, sdlog))
qlnorm <- function(p, meanlog=0, sdlog=1) .Internal(qlnorm(p, meanlog, sdlog))
rlnorm <- function(n, meanlog=0, sdlog=1) .Internal(rlnorm(n, meanlog, sdlog))

dlogis <- function(x, location=0, scale=1) .Internal(dlogis(x, location, scale))
plogis <- function(q, location=0, scale=1) .Internal(plogis(q, location, scale))
qlogis <- function(p, location=0, scale=1) .Internal(qlogis(p, location, scale))
rlogis <- function(n, location=0, scale=1) .Internal(rlogis(n, location, scale))

dweibull <- function(x, shape, scale=1) .Internal(dweibull(x, shape, scale))
pweibull <- function(q, shape, scale=1) .Internal(pweibull(q, shape, scale))
qweibull <- function(p, shape, scale=1) .Internal(qweibull(p, shape, scale))
rweibull <- function(n, shape, scale=1) .Internal(rweibull(n, shape, scale))

dbeta <- function(x, shape1, shape2, ncp=0) {
    if(missing(ncp)) .Internal(dbeta(x, shape1, shape2))
    else .Internal(dnbeta(x, shape1, shape2, ncp))
}
pbeta <- function(q, shape1, shape2, ncp=0) {
    if(missing(ncp)) .Internal(pbeta(q, shape1, shape2))
    else .Internal(pnbeta(q, shape1, shape2, ncp))
}
qbeta <- function(p, shape1, shape2) .Internal(qbeta(p, shape1, shape2))
rbeta <- function(n, shape1, shape2) .Internal(rbeta(n, shape1, shape2))

dbinom <- function(x, size, prob) .Internal(dbinom(x, size, prob))
pbinom <- function(q, size, prob) .Internal(pbinom(q, size, prob))
qbinom <- function(p, size, prob) .Internal(qbinom(p, size, prob))
rbinom <- function(n, size, prob) .Internal(rbinom(n, size, prob))

dchisq <- function(x, df, ncp=0) {
    if(missing(ncp)) .Internal(dchisq(x, df))
    else .Internal(dnchisq(x, df, ncp))
}
pchisq <- function(q, df, ncp=0) {
    if(missing(ncp)) .Internal(pchisq(q, df))
    else .Internal(pnchisq(q, df, ncp))
}
qchisq <- function(p, df, ncp=0) {
    if(missing(ncp)) .Internal(qchisq(p, df))
    else .Internal(qnchisq(p, df, ncp))
}
rchisq <- function(n, df, ncp=0) {
    if(missing(ncp)) .Internal(rchisq(n, df))
    else .not.yet.implemented()
}

df <- function(x, df1, df2) .Internal(df(x, df1, df2))
pf <- function(q, df1, df2, ncp=0) {
    if(missing(ncp)) .Internal(pf(q, df1, df2))
    else .Internal(pnf(q, df1, df2, ncp))
}
qf <- function(p, df1, df2) .Internal(qf(p, df1, df2))
rf <- function(n, df1, df2) .Internal(rf(n, df1, df2))

dgeom <- function(x, prob) .Internal(dgeom(x, prob))
pgeom <- function(q, prob) .Internal(pgeom(q, prob))
qgeom <- function(p, prob) .Internal(qgeom(p, prob))
rgeom <- function(n, prob) .Internal(rgeom(n, prob))

dhyper <- function(x, m, n, k) .Internal(dhyper(x, m, n, k))
phyper <- function(q, m, n, k) .Internal(phyper(q, m, n, k))
qhyper <- function(p, m, n, k) .Internal(qhyper(p, m, n, k))
rhyper <- function(nn, m, n, k) .Internal(rhyper(nn, m, n, k))

dnbinom <- function(x, size, prob) .Internal(dnbinom(x, size, prob))
pnbinom <- function(q, size, prob) .Internal(pnbinom(q, size, prob))
qnbinom <- function(p, size, prob) .Internal(qnbinom(p, size, prob))
rnbinom <- function(n, size, prob) .Internal(rnbinom(n, size, prob))

dpois <- function(x, lambda) .Internal(dpois(x, lambda))
ppois <- function(q, lambda) .Internal(ppois(q, lambda))
qpois <- function(p, lambda) .Internal(qpois(p, lambda))
rpois <- function(n, lambda) .Internal(rpois(n, lambda))

dt <- function(x, df) .Internal(dt(x, df))
pt <- function(q, df, ncp) {
    if(missing(ncp))
	.Internal(pt(q, df))
    else
	.Internal(pnt(q, df, ncp))
}
qt <- function(p, df) .Internal(qt(p, df))
rt <- function(n, df) .Internal(rt(n, df))

ptukey <- function(q, nmeans, df, nranges=1)
    .Internal(ptukey(q, nranges, nmeans, df))
qtukey <- function(p, nmeans, df, nranges=1)
    .Internal(qtukey(p, nranges, nmeans, df))

dwilcox <- function(x, m, n) .Internal(dwilcox(x, m, n))
pwilcox <- function(q, m, n) .Internal(pwilcox(q, m, n))
qwilcox <- function(p, m, n) .Internal(qwilcox(p, m, n))
rwilcox <- function(nn, m, n) .Internal(rwilcox(nn, m, n))

dsignrank <- function(x, n) .Internal(dsignrank(x, n))
psignrank <- function(q, n) .Internal(psignrank(q, n))
qsignrank <- function(p, n) .Internal(qsignrank(p, n))
rsignrank <- function(nn, n) .Internal(rsignrank(nn, n))
