#  File src/library/stats/R/distn.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2016 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/


dexp <- function(x, rate=1, log = FALSE) .Call(C_dexp, x, 1/rate, log)
pexp <- function(q, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pexp, q, 1/rate, lower.tail, log.p)
qexp <- function(p, rate=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qexp, p, 1/rate, lower.tail, log.p)
rexp <- function(n, rate=1) .Call(C_rexp, n, 1/rate)

dunif <- function(x, min=0, max=1, log = FALSE)
    .Call(C_dunif, x, min, max, log)
punif <- function(q, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_punif, q, min, max, lower.tail, log.p)
qunif <- function(p, min=0, max=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qunif, p, min, max, lower.tail, log.p)
runif <- function(n, min=0, max=1) .Call(C_runif, n, min, max)

dnorm <- function(x, mean=0, sd=1, log=FALSE)
    .Call(C_dnorm, x, mean, sd, log)
pnorm <- function(q, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pnorm, q, mean, sd, lower.tail, log.p)
qnorm <- function(p, mean=0, sd=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qnorm, p, mean, sd, lower.tail, log.p)
rnorm <- function(n, mean=0, sd=1) .Call(C_rnorm, n, mean, sd)

dcauchy <- function(x, location=0, scale=1, log = FALSE)
    .Call(C_dcauchy, x, location, scale, log)
pcauchy <-
    function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pcauchy, q, location, scale, lower.tail, log.p)
qcauchy <-
    function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qcauchy, p, location, scale, lower.tail, log.p)
rcauchy <-
    function(n, location=0, scale=1) .Call(C_rcauchy, n, location, scale)

## allow a fuzz of ca 20ulp here.
dgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE)
{
    if(!missing(rate) && !missing(scale)) {
        if(abs(rate*scale - 1) < 1e-15)
            warning("specify 'rate' or 'scale' but not both")
        else
            stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_dgamma, x, shape, scale, log)
}
pgamma <- function(q, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
{
    if(!missing(rate) && !missing(scale)) {
        if(abs(rate*scale - 1) < 1e-15)
            warning("specify 'rate' or 'scale' but not both")
        else
            stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_pgamma, q, shape, scale, lower.tail, log.p)
}
qgamma <- function(p, shape, rate = 1, scale = 1/rate,
                   lower.tail = TRUE, log.p = FALSE)
{
    if(!missing(rate) && !missing(scale)) {
        if(abs(rate*scale - 1) < 1e-15)
            warning("specify 'rate' or 'scale' but not both")
        else
            stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_qgamma, p, shape, scale, lower.tail, log.p)
}
rgamma <- function(n, shape, rate = 1, scale = 1/rate)
{
    if(!missing(rate) && !missing(scale)) {
        if(abs(rate*scale - 1) < 1e-15)
            warning("specify 'rate' or 'scale' but not both")
        else
            stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_rgamma, n, shape, scale)
}
dlnorm <- function(x, meanlog=0, sdlog=1, log=FALSE)
    .Call(C_dlnorm, x, meanlog, sdlog, log)
plnorm <- function(q, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_plnorm, q, meanlog, sdlog, lower.tail, log.p)
qlnorm <- function(p, meanlog=0, sdlog=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qlnorm, p, meanlog, sdlog, lower.tail, log.p)
rlnorm <- function(n, meanlog=0, sdlog=1)
    .Call(C_rlnorm, n, meanlog, sdlog)

dlogis <- function(x, location=0, scale=1, log = FALSE)
    .Call(C_dlogis, x, location, scale, log)
plogis <- function(q, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_plogis, q, location, scale, lower.tail, log.p)
qlogis <- function(p, location=0, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qlogis, p, location, scale, lower.tail, log.p)
rlogis <- function(n, location=0, scale=1)
    .Call(C_rlogis, n, location, scale)

dweibull <- function(x, shape, scale=1, log = FALSE)
    .Call(C_dweibull, x, shape, scale, log)
pweibull <- function(q, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pweibull, q, shape, scale, lower.tail, log.p)
qweibull <- function(p, shape, scale=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qweibull, p, shape, scale, lower.tail, log.p)
rweibull <- function(n, shape, scale=1) .Call(C_rweibull, n, shape, scale)

dbeta <- function(x, shape1, shape2, ncp=0, log = FALSE) {
    if(missing(ncp)) .Call(C_dbeta, x, shape1, shape2, log)
    else .Call(C_dnbeta, x, shape1, shape2, ncp, log)
}
pbeta <- function(q, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_pbeta, q, shape1, shape2, lower.tail, log.p)
    else .Call(C_pnbeta, q, shape1, shape2, ncp, lower.tail, log.p)
}
qbeta <- function(p, shape1, shape2, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_qbeta, p, shape1, shape2, lower.tail, log.p)
    else .Call(C_qnbeta, p, shape1, shape2, ncp, lower.tail, log.p)
}
rbeta <- function(n, shape1, shape2, ncp = 0) {
    if(is.na(ncp)) { warning("NAs produced"); rep(NaN, n) }
    else if(ncp == 0) .Call(C_rbeta, n, shape1, shape2)
    else {
        X <- rchisq(n, 2*shape1, ncp =ncp)
        X/(X + rchisq(n, 2*shape2))
    }
}

dbinom <- function(x, size, prob, log = FALSE)
    .Call(C_dbinom, x, size, prob, log)
pbinom <- function(q, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pbinom, q, size, prob, lower.tail, log.p)
qbinom <- function(p, size, prob, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qbinom, p, size, prob, lower.tail, log.p)
rbinom <- function(n, size, prob) .Call(C_rbinom, n, size, prob)

## Multivariate: that's why there's no C interface (yet) for d...():
dmultinom <- function(x, size = NULL, prob, log = FALSE)
{
    K <- length(prob)
    if(length(x) != K) stop("x[] and prob[] must be equal length vectors.")
    if(any(!is.finite(prob)) || any(prob < 0) || (s <- sum(prob)) == 0)
	stop("probabilities must be finite, non-negative and not all 0")
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
rmultinom <- function(n, size, prob) .Call(C_rmultinom, n, size, prob)

dchisq <- function(x, df, ncp=0, log = FALSE) {
    if(missing(ncp)) .Call(C_dchisq, x, df, log)
    else .Call(C_dnchisq, x, df, ncp, log)
}
pchisq <- function(q, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_pchisq, q, df, lower.tail, log.p)
    else .Call(C_pnchisq, q, df, ncp, lower.tail, log.p)
}
qchisq <- function(p, df, ncp=0, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_qchisq, p, df, lower.tail, log.p)
    else .Call(C_qnchisq, p, df, ncp, lower.tail, log.p)
}
rchisq <- function(n, df, ncp=0) {
    if(missing(ncp)) .Call(C_rchisq, n, df)
    else .Call(C_rnchisq, n, df, ncp)
}

df <- function(x, df1, df2, ncp, log = FALSE) {
    if(missing(ncp)) .Call(C_df, x, df1, df2, log)
    else .Call(C_dnf, x, df1, df2, ncp, log)
}
pf <- function(q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_pf, q, df1, df2, lower.tail, log.p)
    else .Call(C_pnf, q, df1, df2, ncp, lower.tail, log.p)
}
qf <- function(p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_qf, p, df1, df2, lower.tail, log.p)
    else .Call(C_qnf, p, df1, df2, ncp, lower.tail, log.p)
}
rf <- function(n, df1, df2, ncp)
{
    if(missing(ncp)) .Call(C_rf, n, df1, df2)
    else (rchisq(n, df1, ncp=ncp)/df1)/(rchisq(n, df2)/df2)
}

dgeom <- function(x, prob, log = FALSE) .Call(C_dgeom, x, prob, log)
pgeom <- function(q, prob, lower.tail = TRUE, log.p = FALSE)
    .Call(C_pgeom, q, prob, lower.tail, log.p)
qgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qgeom, p, prob, lower.tail, log.p)
rgeom <- function(n, prob) .Call(C_rgeom, n, prob)

dhyper <- function(x, m, n, k, log = FALSE)
    .Call(C_dhyper, x, m, n, k, log)
phyper <- function(q, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Call(C_phyper, q, m, n, k, lower.tail, log.p)
qhyper <- function(p, m, n, k, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qhyper, p, m, n, k, lower.tail, log.p)
rhyper <- function(nn, m, n, k) .Call(C_rhyper, nn, m, n, k)

dnbinom <- function(x, size, prob, mu, log = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
	.Call(C_dnbinom_mu, x, size, mu, log)
    }
    else
	.Call(C_dnbinom, x, size, prob, log)
}
pnbinom <- function(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
	.Call(C_pnbinom_mu, q, size, mu, lower.tail, log.p)
    }
    else
	.Call(C_pnbinom, q, size, prob, lower.tail, log.p)
}
qnbinom <- function(p, size, prob, mu, lower.tail = TRUE, log.p = FALSE)
{
    if (!missing(mu)) {
	if (!missing(prob)) stop("'prob' and 'mu' both specified")
	.Call(C_qnbinom_mu, p, size, mu, lower.tail, log.p)
    }
    else
	.Call(C_qnbinom, p, size, prob, lower.tail, log.p)
}
rnbinom <- function(n, size, prob, mu)
{
    if (!missing(mu)) {
        if (!missing(prob)) stop("'prob' and 'mu' both specified")
        .Call(C_rnbinom_mu, n, size, mu)
    } else .Call(C_rnbinom, n, size, prob)
}

dpois <- function(x, lambda, log = FALSE) .Call(C_dpois, x, lambda, log)
ppois <- function(q, lambda, lower.tail = TRUE, log.p = FALSE)
    .Call(C_ppois, q, lambda, lower.tail, log.p)
qpois <- function(p, lambda, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qpois, p, lambda, lower.tail, log.p)
rpois <- function(n, lambda) .Call(C_rpois, n, lambda)

dt <- function(x, df, ncp, log = FALSE) {
    if(missing(ncp)) .Call(C_dt, x, df, log)
    else .Call(C_dnt, x, df, ncp, log)
}
pt <- function(q, df, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_pt, q, df, lower.tail, log.p)
    else .Call(C_pnt, q, df, ncp, lower.tail, log.p)
}
qt <- function(p, df, ncp, lower.tail = TRUE, log.p = FALSE) {
    if(missing(ncp)) .Call(C_qt, p, df, lower.tail, log.p)
    else .Call(C_qnt,p, df, ncp, lower.tail, log.p)
}
rt <- function(n, df, ncp) {
    if(missing(ncp)) .Call(C_rt, n, df)
    else rnorm(n, ncp)/sqrt(rchisq(n, df)/df)
}

ptukey <- function(q, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_ptukey, q, nranges, nmeans, df, lower.tail, log.p)
qtukey <- function(p, nmeans, df, nranges=1, lower.tail = TRUE, log.p = FALSE)
    .Call(C_qtukey, p, nranges, nmeans, df, lower.tail, log.p)

dwilcox <- function(x, m, n, log = FALSE)
{
    on.exit(.External(C_wilcox_free))
    .Call(C_dwilcox, x, m, n, log)
}
pwilcox <- function(q, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.External(C_wilcox_free))
    .Call(C_pwilcox, q, m, n, lower.tail, log.p)
}
qwilcox <- function(p, m, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.External(C_wilcox_free))
    .Call(C_qwilcox, p, m, n, lower.tail, log.p)
}
rwilcox <- function(nn, m, n) .Call(C_rwilcox, nn, m, n)

dsignrank <- function(x, n, log = FALSE)
{
    on.exit(.External(C_signrank_free))
    .Call(C_dsignrank, x, n, log)
}
psignrank <- function(q, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.External(C_signrank_free))
    .Call(C_psignrank, q, n, lower.tail, log.p)
}
qsignrank <- function(p, n, lower.tail = TRUE, log.p = FALSE)
{
    on.exit(.External(C_signrank_free))
    .Call(C_qsignrank, p, n, lower.tail, log.p)
}
rsignrank <- function(nn, n) .Call(C_rsignrank, nn, n)

##' Random sample from a Wishart distribution
rWishart <- function(n, df, Sigma) .Call(C_rWishart, n, df, Sigma)
