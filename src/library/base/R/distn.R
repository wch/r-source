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

dcauchy <- function(x, location=0, scale=1) .Internal(dcauchy(x, location, scale))
pcauchy <- function(q, location=0, scale=1) .Internal(pcauchy(q, location, scale))
qcauchy <- function(p, location=0, scale=1) .Internal(qcauchy(p, location, scale))
rcauchy <- function(n, location=0, scale=1) .Internal(rcauchy(n, location, scale))

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


##--- Argument names taken from  ../man/Beta :
dbeta <- function(x, a, b, ncp=0) {
if(missing(ncp)) .Internal(dbeta(x, a, b))
else .Internal(dnbeta(x, a, b, ncp))
}
pbeta <- function(q, a, b, ncp=0) {
if(missing(ncp)) .Internal(pbeta(q, a, b))
else .Internal(pnbeta(q, a, b, ncp))
}
qbeta <- function(p, a, b) .Internal(qbeta(p, a, b))
rbeta <- function(n, a, b) .Internal(rbeta(n, a, b))
##--- Argument names taken from  ../man/Binomial :
dbinom <- function(x, n, p) .Internal(dbinom(x, n, p))
pbinom <- function(q, n, p) .Internal(pbinom(q, n, p))
qbinom <- function(prob, n, p) .Internal(qbinom(prob, n, p))
rbinom <- function(nobs, n, p) .Internal(rbinom(nobs, n, p))
##--- Argument names taken from  ../man/[N]Chisquare :
dchisq <- function(x, df, ncp=0) {
if(missing(ncp)) .Internal(dchisq(x, df))
else .Internal(dnchisq(x, df, ncp))
}
pchisq <- function(q, df, ncp=0) {
if(missing(ncp)) .Internal(pchisq(q, df))
else .Internal(pnchisq(q, df, ncp))
}
qchisq <- function(p, df) .Internal(qchisq(p, df))
rchisq <- function(n, df) .Internal(rchisq(n, df))
dnchisq <- function(x, df, lambda) .Internal(dnchisq(x, df, lambda))
pnchisq <- function(q, df, lambda) .Internal(pnchisq(q, df, lambda))
qnchisq <- function(p, df, lambda) .Internal(qnchisq(p, df, lambda))
rnchisq <- function(n, df, lambda) .Internal(rnchisq(n, df, lambda))
##--- Argument names taken from  ../man/F :
df <- function(x, n1, n2) .Internal(df(x, n1, n2))
pf <- function(q, n1, n2, ncp=0) {
if(missing(ncp)) .Internal(pf(q, n1, n2))
else .Internal(pnf(q, n1, n2, ncp))
}
qf <- function(p, n1, n2) .Internal(qf(p, n1, n2))
rf <- function(n, n1, n2) .Internal(rf(n, n1, n2))
##--- Argument names taken from  ../man/Geometric :
dgeom <- function(x, p) .Internal(dgeom(x, p))
pgeom <- function(q, p) .Internal(pgeom(q, p))
qgeom <- function(prob, p) .Internal(qgeom(prob, p))
rgeom <- function(n, p) .Internal(rgeom(n, p))
##--- Argument names taken from  ../man/Hypergeometric :
dhyper <- function(x, N1, N2, n) .Internal(dhyper(x, N1, N2, n))
phyper <- function(q, N1, N2, n) .Internal(phyper(q, N1, N2, n))
qhyper <- function(p, N1, N2, n) .Internal(qhyper(p, N1, N2, n))
rhyper <- function(nobs, N1, N2, n) .Internal(rhyper(nobs, N1, N2, n))
##--- Argument names taken from  ../man/NegBinomial :
dnbinom <- function(x, n, p) .Internal(dnbinom(x, n, p))
pnbinom <- function(q, n, p) .Internal(pnbinom(q, n, p))
qnbinom <- function(prob, n, p) .Internal(qnbinom(prob, n, p))
rnbinom <- function(nobs, n, p) .Internal(rnbinom(nobs, n, p))
##--- Argument names taken from  ../man/Poisson :
dpois <- function(x, lambda) .Internal(dpois(x, lambda))
ppois <- function(q, lambda) .Internal(ppois(q, lambda))
qpois <- function(p, lambda) .Internal(qpois(p, lambda))
rpois <- function(n, lambda) .Internal(rpois(n, lambda))
##--- Argument names taken from  ../man/T
dt <- function(x, df) .Internal(dt(x, df))
pt <- function(q, df, ncp) {
if(missing(ncp)) .Internal(pt(q, df))
else .Internal(pnt(q, df, ncp))
}
qt <- function(p, df) .Internal(qt(p, df))
rt <- function(n, df) .Internal(rt(n, df))
ptukey <- function(q, nmeans, df, nranges=1)
.Internal(ptukey(q, nranges, nmeans, df))
qtukey <- function(p, nmeans, df, nranges=1)
.Internal(qtukey(p, nranges, nmeans, df))
