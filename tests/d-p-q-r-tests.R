####	d|ensity
####	p|robability (cumulative)
####	q|uantile
####	r|andom number generation
####
####	Functions for  ``d/p/q/r''

###-- these are identical in ./arith-true.R ["fixme": use source(..)]
opt.conformance <- 0
Meps <- .Machine $ double.eps
options(rErr.eps = 1e-30)
rErr <- function(approx, true, eps = .Options$rErr.eps)
{
    if(is.null(eps)) { eps <- 1e-30; options(rErr.eps = eps) }
    ifelse(Mod(true) >= eps,
	   1 - approx / true, # relative error
	   true - approx)     # absolute error (e.g. when true=0)
}

if(!interactive())
    .Random.seed <- c(0,rep(7654, 3))

###--- Discrete Distributions: Simple Consistency Checks  pZZ = cumsum(dZZ)

## Currently, just Wilcoxon  [should do this for all !]
is.sym <- TRUE
for(n in rpois(5, lam=6))
    for(m in rpois(15, lam=8))
    {
        x <- -1:(n*m + 1)
        fx <- dwilcox(x, n, m)
        Fx <- pwilcox(x, n, m)
        is.sym <- is.sym & all(fx == dwilcox(x, m, n))
        eq <- all.equal(Fx, cumsum(fx), tol= 1e-14)
        if(!is.logical(eq) || !eq) print(eq)
    }
is.sym

##--- Cumulative Poisson '==' Cumulative Chi^2 :
##--- Abramowitz & Stegun, p.941 :  26.4.21 (26.4.2)
n1 <- 20; n2 <- 16
for(lambda in rexp(n1))
    for(k in rpois(n2, lambda)) {
	tst <- all.equal(1 - pchisq(2*lambda, 2*(k+1)),
			 pp <- sum(dpois(0:k, lambda=lambda)), tol = 100*Meps)
	if(!(is.logical(tst) && tst))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst=", tst,"\n")
	tst2 <- all.equal(pp, ppois(k, lambda=lambda), tol = 100*Meps)
	if(!(is.logical(tst2) && tst2))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst2=", tst2,"\n")
	tst3 <- all.equal(1 - pp, ppois(k, lambda=lambda, lower.tail=FALSE))
	if(!(is.logical(tst3) && tst3))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst3=", tst3,"\n")
    }

##--- Cumulative Binomial '==' Cumulative F :
##--- Abramowitz & Stegun, p.945-6;  26.5.24  AND  26.5.28 :
n0 <- 50; n1 <- 16; n2 <- 20; n3 <- 8
for(n in rbinom(n1, size = 2*n0, p = .4)) {
    cat("n=",n,": ")
    for(p in c(0,1,rbeta(n2, 2,4))) {
	cat(".")
	for(k in rbinom(n3, size = n,  prob = runif(1))) {
	    ## For X ~ Bin(n,p), compute 1 - P[X > k] = P[X <= k]  in two ways:
	    tst <- all.equal(if(k==n || p==0) 1 else
			     pf((k+1)/(n-k)*(1-p)/p, df1=2*(n-k), df2=2*(k+1)),
			     sum(dbinom(0:k, size = n, prob = p)))
	    if(!(is.logical(tst) && tst))
		cat("n=", n,"; p =",format(p),".  k =",k, " --> tst=",tst,"\n")
	}
    }
    cat("\n")
}

##---  Gamma (incl. chi^2) Density :
x <- round(rgamma(100, shape = 2),2)
for(sh in round(rlnorm(30),2)) {
    Ga <- gamma(sh)
    for(sig in round(rlnorm(30),2)) {
        tst <- all.equal((d1 <- dgamma(  x,   shape = sh, scale = sig)),
                         (d2 <- dgamma(x/sig, shape = sh, scale = 1) / sig),
                         tol = 1e-15)
        if(!(is.logical(tst) && tst))
            cat("ERROR: dgamma() doesn't scale:",tst,"\n",
                "  x =", formatC(x),"\n  shape,scale=",formatC(c(sh, sig)),"\n")
        tst <- all.equal(d1, (d3 <- 1/(Ga * sig^sh) * x^(sh-1) * exp(-x/sig)),
                         tol= 1e-14)
        if(!(is.logical(tst) && tst))
            cat("NOT Equal:",tst,"\n x =", formatC(x),
                "\n  shape,scale=",formatC(c(sh, sig)),"\n")
    }
}

##--- Normal (& Lognormal) :

## 2 Ex. from  Wichura (1988); AS 241 Applied Statistics -- would be better
all.equal(qnorm(0.25),-0.6744897501960817, tol = 1e-14)
all.equal(qnorm(0.001),-3.090232306168, tol = 1e-12)

z <- rnorm(1000); all.equal(pnorm(z),  1 - pnorm(-z), tol= 1e-15)
z <- c(-Inf,Inf,NA,NaN, rt(1000, df=2))
zsml <- z > -37.5 || !is.finite(z)
for(df in 1:10) if(!is.logical(all.equal(pt(z, df), 1 - pt(-z,df), tol= 1e-15)))
    cat("ERROR -- df = ", df, "\n")
all.equal(pz <- pnorm(z), 1 - pnorm(z, lower=FALSE), tol= 1e-14)
all.equal(pz,               pnorm(-z, lower=FALSE),  tol= 1e-14)
all.equal(log(pz[zsml]),  pnorm(z[zsml], log=TRUE),  tol= 1e-14)
y <- seq(-70,0, by = 10)
cbind(y, "log(pnorm(y))"= log(pnorm(y)), "pnorm(y, log=T)"= pnorm(y, log=TRUE))

all.equal(pz, plnorm(exp(z)), tol = 1e-14)

###==========  p <-> q  Inversion consistency =====================
ok <- 1e-5 < pz & pz < 1 - 1e-5
all.equal(z[ok], qnorm(pz[ok]), tol= 1e-12)

## The prefixes of ALL the PDQ & R functions
PDQR <- c("beta", "binom", "cauchy", "chisq", "exp", "f",
          "gamma", "geom", "hyper", "lnorm", "logis", "nbinom","norm",
          "pois","signrank","t","unif","weibull","wilcox")
PQonly <- c("tukey")

###===== Random numbers -- just output

.Random.seed <- c(0, 17292, 29447, 24113)
n <- 20
## for(pre in PDQR) { n <- paste("r",pre,sep=""); cat(n,": "); str(get(n))}
(Rbeta    <- rbeta    (n, shape1 = .8, shape2 = 2) )
(Rbinom   <- rbinom   (n, size = 55, prob = .25) )
(Rcauchy  <- rcauchy  (n, location = 0, scale = 1) )
(Rchisq   <- rchisq   (n, df = 3) )
(Rexp     <- rexp     (n, rate = 1) )
(Rf       <- rf       (n, df1 = 12, df2 = 6) )
(Rgamma   <- rgamma   (n, shape = 2, scale = 1) )
(Rgeom    <- rgeom    (n, prob = .4) )
(Rhyper   <- rhyper   (n, m = 40, n = 30, k = 20) )
(Rlnorm   <- rlnorm   (n, meanlog = 0, sdlog = 1) )
(Rlogis   <- rlogis   (n, location = 0, scale = 1) )
(Rnbinom  <- rnbinom  (n, size = 7, prob = .01) )
(Rnorm    <- rnorm    (n, mean = 0, sd = 1) )
(Rpois    <- rpois    (n, lambda = 12) )
(Rsignrank<- rsignrank(n, n = 47) )
(Rt       <- rt       (n, df = 11) )
(Runif    <- runif    (n, min = 0, max = 1) )
(Rweibull <- rweibull (n, shape = 3, scale = 1) )
(Rwilcox  <- rwilcox  (n, m = 13, n = 17) )

(Pbeta    <- pbeta    (Rbeta, shape1 = .8, shape2 = 2) )
(Pbinom   <- pbinom   (Rbinom, size = 55, prob = .25) )
(Pcauchy  <- pcauchy  (Rcauchy, location = 0, scale = 1) )
(Pchisq   <- pchisq   (Rchisq, df = 3) )
(Pexp     <- pexp     (Rexp, rate = 1) )
(Pf       <- pf       (Rf, df1 = 12, df2 = 6) )
(Pgamma   <- pgamma   (Rgamma, shape = 2, scale = 1) )
(Pgeom    <- pgeom    (Rgeom, prob = .4) )
(Phyper   <- phyper   (Rhyper, m = 40, n = 30, k = 20) )
(Plnorm   <- plnorm   (Rlnorm, meanlog = 0, sdlog = 1) )
(Plogis   <- plogis   (Rlogis, location = 0, scale = 1) )
(Pnbinom  <- pnbinom  (Rnbinom, size = 7, prob = .01) )
(Pnorm    <- pnorm    (Rnorm, mean = 0, sd = 1) )
(Ppois    <- ppois    (Rpois, lambda = 12) )
(Psignrank<- psignrank(Rsignrank, n = 47) )
(Pt       <- pt       (Rt, df = 11) )
(Punif    <- punif    (Runif, min = 0, max = 1) )
(Pweibull <- pweibull (Rweibull, shape = 3, scale = 1) )
(Pwilcox  <- pwilcox  (Rwilcox, m = 13, n = 17) )

dbeta    (Rbeta, shape1 = .8, shape2 = 2)
dbinom   (Rbinom, size = 55, prob = .25)
dcauchy  (Rcauchy, location = 0, scale = 1)
dchisq   (Rchisq, df = 3)
dexp     (Rexp, rate = 1)
df       (Rf, df1 = 12, df2 = 6)
dgamma   (Rgamma, shape = 2, scale = 1)
dgeom    (Rgeom, prob = .4)
dhyper   (Rhyper, m = 40, n = 30, k = 20)
dlnorm   (Rlnorm, meanlog = 0, sdlog = 1)
dlogis   (Rlogis, location = 0, scale = 1)
dnbinom  (Rnbinom, size = 7, prob = .01)
dnorm    (Rnorm, mean = 0, sd = 1)
dpois    (Rpois, lambda = 12)
dsignrank(Rsignrank, n = 47)
dt       (Rt, df = 11)
dunif    (Runif, min = 0, max = 1)
dweibull (Rweibull, shape = 3, scale = 1)
dwilcox  (Rwilcox, m = 13, n = 17)

## Check q*(p*(.)) = identity
all.equal(Rbeta,     qbeta    (Pbeta, shape1 = .8, shape2 = 2), tol = 1e-14)
all.equal(Rbinom,    qbinom   (Pbinom, size = 55, prob = .25), tol = 1e-14)
all.equal(Rcauchy,   qcauchy  (Pcauchy, location = 0, scale = 1), tol = 1e-14)
all.equal(Rchisq,    qchisq   (Pchisq, df = 3), tol = 1e-14)
all.equal(Rexp,      qexp     (Pexp, rate = 1), tol = 1e-14)
all.equal(Rf,        qf       (Pf, df1 = 12, df2 = 6), tol = 1e-14)
all.equal(Rgamma,    qgamma   (Pgamma, shape = 2, scale = 1), tol = 1e-14)
all.equal(Rgeom,     qgeom    (Pgeom, prob = .4), tol = 1e-14)
all.equal(Rhyper,    qhyper   (Phyper, m = 40, n = 30, k = 20), tol = 1e-14)
all.equal(Rlnorm,    qlnorm   (Plnorm, meanlog = 0, sdlog = 1), tol = 1e-14)
all.equal(Rlogis,    qlogis   (Plogis, location = 0, scale = 1), tol = 1e-14)
all.equal(Rnbinom,   qnbinom  (Pnbinom, size = 7, prob = .01), tol = 1e-14)
all.equal(Rnorm,     qnorm    (Pnorm, mean = 0, sd = 1), tol = 1e-14)
all.equal(Rpois,     qpois    (Ppois, lambda = 12), tol = 1e-14)
all.equal(Rsignrank, qsignrank(Psignrank, n = 47), tol = 1e-14)
all.equal(Rt,        qt       (Pt, df = 11), tol = 1e-7 )## << !!
all.equal(Runif,     qunif    (Punif, min = 0, max = 1), tol = 1e-14)
all.equal(Rweibull,  qweibull (Pweibull, shape = 3, scale = 1), tol = 1e-14)
all.equal(Rwilcox,   qwilcox  (Pwilcox, m = 13, n = 17), tol = 1e-14)

## Same with "upper tail":
all.equal(Rbeta,     qbeta    (1- Pbeta, shape1 = .8, shape2 = 2,
                               lower=F), tol = 1e-14)
all.equal(Rbinom,    qbinom   (1- Pbinom, size = 55, prob = .25,
                               lower=F), tol = 1e-14)
all.equal(Rcauchy,   qcauchy  (1- Pcauchy, location = 0, scale = 1,
                               lower=F), tol = 1e-14)
all.equal(Rchisq,    qchisq   (1- Pchisq, df = 3, lower=F), tol = 1e-14)
all.equal(Rexp,      qexp     (1- Pexp, rate = 1, lower=F), tol = 1e-14)
all.equal(Rf,        qf       (1- Pf, df1 = 12, df2 = 6, lower=F), tol = 1e-14)
all.equal(Rgamma,    qgamma   (1- Pgamma, shape = 2, scale = 1,
                               lower=F), tol = 1e-14)
all.equal(Rgeom,     qgeom    (1- Pgeom, prob = .4, lower=F), tol = 1e-14)
all.equal(Rhyper,    qhyper   (1- Phyper, m = 40, n = 30, k = 20,
                               lower=F), tol = 1e-14)
all.equal(Rlnorm,    qlnorm   (1- Plnorm, meanlog = 0, sdlog = 1,
                               lower=F), tol = 1e-14)
all.equal(Rlogis,    qlogis   (1- Plogis, location = 0, scale = 1,
                               lower=F), tol = 1e-14)
all.equal(Rnbinom,   qnbinom  (1- Pnbinom, size = 7, prob = .01,
                               lower=F), tol = 1e-14)
all.equal(Rnorm,     qnorm    (1- Pnorm, mean = 0, sd = 1,lower=F), tol = 1e-14)
all.equal(Rpois,     qpois    (1- Ppois, lambda = 12,
                               lower=F), tol = 1e-14)
all.equal(Rsignrank, qsignrank(1- Psignrank, n = 47, lower=F), tol = 1e-14)
all.equal(Rt,        qt       (1- Pt, df = 11, lower=F), tol = 1e-7)
all.equal(Runif,     qunif    (1- Punif, min = 0, max = 1,
                               lower=F), tol = 1e-14)
all.equal(Rweibull,  qweibull (1- Pweibull, shape = 3, scale = 1,
                               lower=F), tol = 1e-14)
all.equal(Rwilcox,   qwilcox  (1- Pwilcox, m = 13, n = 17,
                               lower=F), tol = 1e-14)

## Check log( upper.tail ):
all.equal(log(1 - Pbeta),     pbeta    (Rbeta, shape1 = .8, shape2 = 2,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pbinom),    pbinom   (Rbinom, size = 55, prob = .25,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pcauchy),   pcauchy  (Rcauchy, location = 0, scale = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pchisq),    pchisq   (Rchisq, df = 3,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pexp),      pexp     (Rexp, rate = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pf),        pf       (Rf, df1 = 12, df2 = 6,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pgamma),    pgamma   (Rgamma, shape = 2, scale = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pgeom),     pgeom    (Rgeom, prob = .4,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Phyper),    phyper   (Rhyper, m = 40, n = 30, k = 20,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Plnorm),    plnorm   (Rlnorm, meanlog = 0, sdlog = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Plogis),    plogis   (Rlogis, location = 0, scale = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pnbinom),   pnbinom  (Rnbinom, size = 7, prob = .01,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pnorm),     pnorm    (Rnorm, mean = 0, sd = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Ppois),     ppois    (Rpois, lambda = 12,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Psignrank), psignrank(Rsignrank, n = 47,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pt),        pt       (Rt, df = 11,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Punif),     punif    (Runif, min = 0, max = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pweibull),  pweibull (Rweibull, shape = 3, scale = 1,
                                        lower=F, log=T), tol = 1e-14)
all.equal(log(1 - Pwilcox),   pwilcox  (Rwilcox, m = 13, n = 17,
                                        lower=F, log=T), tol = 1e-14)
