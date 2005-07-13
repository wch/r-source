####	d|ensity
####	p|robability (cumulative)
####	q|uantile
####	r|andom number generation
####
####	Functions for  ``d/p/q/r''

.ptime <- proc.time()
F <- FALSE
T <- TRUE

###-- these are identical in ./arith-true.R ["fixme": use source(..)]
opt.conformance <- 0
Meps <- .Machine $ double.eps
xMax <- .Machine $ double.xmax
options(rErr.eps = 1e-30)
rErr <- function(approx, true, eps = .Options$rErr.eps)
{
    if(is.null(eps)) { eps <- 1e-30; options(rErr.eps = eps) }
    ifelse(Mod(true) >= eps,
	   1 - approx / true, # relative error
	   true - approx)     # absolute error (e.g. when true=0)
}
## Numerical equality: Here want "rel.error" almost always:
All.eq <- function(x,y)
    all.equal.numeric(x,y, tolerance= 64*.Machine$double.eps,
                      scale = {r <- mean(abs(x),na.rm=TRUE)
                               if(r > 0) r else 0})

if(!interactive())
    .Random.seed <- c(0,rep(7654, 3))

## The prefixes of ALL the PDQ & R functions
PDQRinteg <- c("binom", "geom", "hyper", "nbinom", "pois","signrank","wilcox")
PDQR <- c(PDQRinteg, "beta", "cauchy", "chisq", "exp", "f", "gamma",
	  "lnorm", "logis", "norm", "t","unif","weibull")
PQonly <- c("tukey")

###--- Discrete Distributions --- Consistency Checks  pZZ = cumsum(dZZ)

##for(pre in PDQRinteg) { n <- paste("d",pre,sep=""); cat(n,": "); str(get(n))}

##__ 1. Binomial __

## Cumulative Binomial '==' Cumulative F :
## Abramowitz & Stegun, p.945-6;  26.5.24  AND	26.5.28 :
n0 <- 50; n1 <- 16; n2 <- 20; n3 <- 8
for(n in rbinom(n1, size = 2*n0, p = .4)) {
    cat("n=",n,": ")
    for(p in c(0,1,rbeta(n2, 2,4))) {
	cat(".")
	for(k in rbinom(n3, size = n,  prob = runif(1))) {
	    ## For X ~ Bin(n,p), compute 1 - P[X > k] = P[X <= k] in three ways:
	    tst1 <- all.equal(	     pbinom(0:k, size = n, prob = p),
			      cumsum(dbinom(0:k, size = n, prob = p)))
	    tst <- all.equal(if(k==n || p==0) 1 else
			     pf((k+1)/(n-k)*(1-p)/p, df1=2*(n-k), df2=2*(k+1)),
			     sum(dbinom(0:k, size = n, prob = p)))
	    if(!isTRUE(tst1) || !isTRUE(tst)) {
		cat("n=", n,"; p =",format(p),".  k =",k)
		if(!isTRUE(tst1)) cat("; tst1=",tst1)
		if(!isTRUE(tst )) cat("; tst=", tst)
		cat("\n")
	    }
	}
    }
    cat("\n")
}

##__ 2. Geometric __
for(pr in seq(0,1,len=15)) {
    print(All.eq((dg <- dgeom(0:10, pr)),
		 pr * (1-pr)^(0:10)))
    print(All.eq(cumsum(dg), pgeom(0:10, pr)))
}

##__ 3. Hypergeometric __

m <- 10; n <- 7
for(k in 2:m) {
    x <- 0:(k+1)
    print(All.eq(phyper(x, m, n, k), cumsum(dhyper(x, m, n, k))))
}

##__ 4. Negative Binomial __

## PR #842
for(size in seq(0.8,2, by=.1))
    print(all.equal(cumsum(dnbinom(0:7, size, .5)),
			   pnbinom(0:7, size, .5)))
All.eq(pnbinom(c(1,3), .9, .5), c(0.777035760338812, 0.946945347071519))

##__ 5. Poisson __

all(dpois(0:5,0)	   == c(1, rep(0,5)))
all(dpois(0:5,0, log=TRUE) == c(0, rep(-Inf, 5)))

## Cumulative Poisson '==' Cumulative Chi^2 :
## Abramowitz & Stegun, p.941 :	 26.4.21 (26.4.2)
n1 <- 20; n2 <- 16
for(lambda in rexp(n1))
    for(k in rpois(n2, lambda)) {
	tst <- all.equal(1 - pchisq(2*lambda, 2*(1+ 0:k)),
			 pp <- cumsum(dpois(0:k, lambda=lambda)), tol= 100*Meps)
	if(!isTRUE(tst))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst=", tst,"\n")
	tst2 <- all.equal(pp, ppois(0:k, lambda=lambda), tol = 100*Meps)
	if(!isTRUE(tst2))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst2=", tst2,"\n")
	tst3 <- all.equal(1 - pp, ppois(0:k, lambda=lambda, lower.tail=FALSE))
	if(!isTRUE(tst3))
	    cat("lambda=", format(lambda),".  k =",k, " --> tst3=", tst3,"\n")
    }


##__ 6. SignRank __
for(n in rpois(32, lam=8)) {
    x <- -1:(n + 4)
    if(!isTRUE(eq <- All.eq(psignrank(x, n), cumsum(dsignrank(x, n)))))
        print(eq)
}

##__ 7. Wilcoxon (symmetry & cumulative) __
is.sym <- TRUE
for(n in rpois(5, lam=6))
    for(m in rpois(15, lam=8)) {
	x <- -1:(n*m + 1)
	fx <- dwilcox(x, n, m)
	Fx <- pwilcox(x, n, m)
	is.sym <- is.sym & all(fx == dwilcox(x, m, n))
	if(!isTRUE(eq <- All.eq(Fx, cumsum(fx))))
            print(eq)
    }
is.sym


###-------- Continuous Distributions ----------

##---  Gamma (incl. central chi^2) Density :
x <- round(rgamma(100, shape = 2),2)
for(sh in round(rlnorm(30),2)) {
    Ga <- gamma(sh)
    for(sig in round(rlnorm(30),2)) {
	tst <- all.equal((d1 <- dgamma(	 x,   shape = sh, scale = sig)),
			 (d2 <- dgamma(x/sig, shape = sh, scale = 1) / sig),
			 tol = 1e-14)## __ad interim__ was 1e-15
	if(!isTRUE(tst))
	    cat("ERROR: dgamma() doesn't scale:",tst,"\n",
		"  x =", formatC(x),"\n	 shape,scale=",formatC(c(sh, sig)),"\n")
	tst <- All.eq(d1, (d3 <- 1/(Ga * sig^sh) * x^(sh-1) * exp(-x/sig)))
	if(!isTRUE(tst))
	    cat("NOT Equal:",tst,"\n x =", formatC(x),
		"\n  shape,scale=",formatC(c(sh, sig)),"\n")
    }
}
pgamma(1,Inf,scale=Inf) == 0
all(is.nan(c(pgamma(Inf,  1,scale=Inf),
             pgamma(Inf,Inf,scale=1),
             pgamma(Inf,Inf,scale=Inf))))
scLrg <- c(2,100, 1e300*c(.1, 1,10,100), 1e307, xMax, Inf)
stopifnot(pgamma(Inf, 1, scale=xMax) == 1,
          pgamma(xMax,1, scale=Inf) == 0,
          all.equal(pgamma(1e300, 2, scale= scLrg, log=TRUE),
                    c(0, 0, -0.000499523968713701, -1.33089326820406,
                      -5.36470502873211, -9.91015144019122,
                      -32.9293385491433, -38.707517174609, -Inf), tol=2e-15)
          )

p <- 7e-4; df <- 0.9
abs(1-c(pchisq(qchisq(p, df),df)/p, # was 2.31e-8 for R <= 1.8.1
        pchisq(qchisq(1-p, df,lower=FALSE),df,lower=FALSE)/(1-p),# was 1.618e-11
        pchisq(qchisq(log(p), df,log=TRUE),df, log=TRUE)/log(p), # was 3.181e-9
        pchisq(qchisq(log(1-p),df,log=T,lower=F),df, log=T,lower=F)/log(1-p),
        )# 32b-i386: (2.2e-16, 0,0, 3.3e-16); Opteron: (2.2e-16, 0,0, 2.2e-15)
    ) < 1e-14

##-- non central Chi^2 :
xB <- c(2000,1e6,1e50,Inf)
for(df in c(0.1, 1, 10))
    for(ncp in c(0, 1, 10, 100)) stopifnot(pchisq(xB, df=df, ncp=ncp) == 1)
all.equal(qchisq(0.025,31,ncp=1,lower.tail=FALSE),# inf.loop PR#875
          49.7766246561514, tol= 1e-11)
for(df in c(0.1, 0.5, 1.5, 4.7, 10, 20,50,100)) {
    cat("df =", formatC(df, wid=3))
    xx <- c(10^-(5:1), .9, 1.2, df + c(3,7,20,30,35,38))
    pp <- pchisq(xx, df=df, ncp = 1) #print(pp)
    dtol <- 1e-12 *(if(2 < df && df <= 50) 64 else if(df > 50) 20000 else 501)
    print(all.equal(xx, qchisq(pp, df=df, ncp=1), tol = dtol))# TRUE
    ##or print(mapply(rErr, xx, qchisq(pp, df=df,ncp=1)), digits = 3)
}

## p ~= 1 (<==> 1-p ~= 0) -- gave infinite loop in R <= 1.8.1 -- PR#6421
psml <- 2^-(10:54)
q0 <- qchisq(psml,    df=1.2, ncp=10, lower.tail=FALSE)
q1 <- qchisq(1 -psml, df=1.2, ncp=10)
p0 <- pchisq(q0, df=1.2, ncp=10, lower.tail=FALSE)
p1 <- pchisq(q1, df=1.2, ncp=10, lower.tail=FALSE)
iO <- 1:10
all.equal(q0[iO], q1[iO])
all.equal(p0[iO], psml[iO], tol = 0.08)# bad tol (0.0744 on 386-Linux).

##--- Beta (need more):

## big a & b (PR #643)
summary(a <- rlnorm(20, 5.5))
summary(b <- rlnorm(20, 6.5))
pab <- expand.grid(seq(0,1,by=.1), a, b)
p <- pab[,1]; a <- pab[,2]; b <- pab[,3]
all.equal(dbeta(p,a,b), exp(pab <- dbeta(p,a,b, log = TRUE)), tol = 1e-11)
sample(pab, 50)


##--- Normal (& Lognormal) :

qnorm(0) == -Inf && qnorm(-Inf, log = TRUE) == -Inf
qnorm(1) ==  Inf && qnorm(0, log = TRUE) == Inf

is.nan(qnorm(1.1)) &&
is.nan(qnorm(-.1)) # + warn

x <- c(-Inf, -1e100, 1:6, 1e200, Inf)
rbind(d.s0 =dnorm(x,3,s=0),   p.s0 = pnorm(x,3,s=0),
      d.sI =dnorm(x,3,s=Inf), p.sI = pnorm(x,3,s=Inf))

## 3 Test data from Wichura (1988) :
all.equal(qnorm(c( 0.25,  .001,	 1e-20)),
	  c(-0.6744897501960817, -3.090232306167814, -9.262340089798408),
	  tol = 1e-15)
# extreme tail -- available on log scale only:
all.equal(qnorm(-1e5, log = TRUE), -447.1974945)

z <- rnorm(1000); all.equal(pnorm(z),  1 - pnorm(-z), tol= 1e-15)
z <- c(-Inf,Inf,NA,NaN, rt(1000, df=2))
z.ok <- z > -37.5 | !is.finite(z)
for(df in 1:10) if(!isTRUE(all.equal(pt(z, df), 1 - pt(-z,df), tol= 1e-15)))
    cat("ERROR -- df = ", df, "\n")
All.eq(pz <- pnorm(z), 1 - pnorm(z, lower=FALSE))
All.eq(pz,		 pnorm(-z, lower=FALSE))
All.eq(log(pz[z.ok]),  pnorm(z[z.ok], log=TRUE))
y <- seq(-70,0, by = 10)
cbind(y, "log(pnorm(y))"= log(pnorm(y)), "pnorm(y, log=T)"= pnorm(y, log=TRUE))
y <- c(1:15, seq(20,40, by=5))
cbind(y, "log(pnorm(y))"= log(pnorm(y)), "pnorm(y, log=T)"= pnorm(y, log=TRUE),
      "log(pnorm(-y))"= log(pnorm(-y)), "pnorm(-y, log=T)"= pnorm(-y, log=TRUE))
## Symmetry:
y <- c(1:50,10^c(3:10,20,50,150,250))
y <- c(-y,0,y)
for(L in c(FALSE,TRUE))
    stopifnot(identical(pnorm(-y, log= L),
			pnorm(+y, log= L, lower=FALSE)))

## Log norm
All.eq(pz, plnorm(exp(z)))


###==========  p <-> q	Inversion consistency =====================
ok <- 1e-5 < pz & pz < 1 - 1e-5
all.equal(z[ok], qnorm(pz[ok]), tol= 1e-12)

###===== Random numbers -- first, just output:

.Random.seed <- c(0, 17292, 29447, 24113)
n <- 20
## for(pre in PDQR) { n <- paste("r",pre,sep=""); cat(n,": "); str(get(n))}
(Rbeta	  <- rbeta    (n, shape1 = .8, shape2 = 2) )
(Rbinom	  <- rbinom   (n, size = 55, prob = pi/16) )
(Rcauchy  <- rcauchy  (n, location = 12, scale = 2) )
(Rchisq	  <- rchisq   (n, df = 3) )
(Rexp	  <- rexp     (n, rate = 2) )
(Rf	  <- rf	      (n, df1 = 12, df2 = 6) )
(Rgamma	  <- rgamma   (n, shape = 2, scale = 5) )
(Rgeom	  <- rgeom    (n, prob = pi/16) )
(Rhyper	  <- rhyper   (n, m = 40, n = 30, k = 20) )
(Rlnorm	  <- rlnorm   (n, meanlog = -1, sdlog = 3) )
(Rlogis	  <- rlogis   (n, location = 12, scale = 2) )
(Rnbinom  <- rnbinom  (n, size = 7, prob = .01) )
(Rnorm	  <- rnorm    (n, mean = -1, sd = 3) )
(Rpois	  <- rpois    (n, lambda = 12) )
(Rsignrank<- rsignrank(n, n = 47) )
(Rt	  <- rt	      (n, df = 11) )
## Rt2 below (to preserve the following random numbers!)
(Runif	  <- runif    (n, min = .2, max = 2) )
(Rweibull <- rweibull (n, shape = 3, scale = 2) )
(Rwilcox  <- rwilcox  (n, m = 13, n = 17) )
(Rt2	  <- rt	      (n, df = 1.01))

(Pbeta	  <- pbeta    (Rbeta, shape1 = .8, shape2 = 2) )
(Pbinom	  <- pbinom   (Rbinom, size = 55, prob = pi/16) )
(Pcauchy  <- pcauchy  (Rcauchy, location = 12, scale = 2) )
(Pchisq	  <- pchisq   (Rchisq, df = 3) )
(Pexp	  <- pexp     (Rexp, rate = 2) )
(Pf	  <- pf	      (Rf, df1 = 12, df2 = 6) )
(Pgamma	  <- pgamma   (Rgamma, shape = 2, scale = 5) )
(Pgeom	  <- pgeom    (Rgeom, prob = pi/16) )
(Phyper	  <- phyper   (Rhyper, m = 40, n = 30, k = 20) )
(Plnorm	  <- plnorm   (Rlnorm, meanlog = -1, sdlog = 3) )
(Plogis	  <- plogis   (Rlogis, location = 12, scale = 2) )
(Pnbinom  <- pnbinom  (Rnbinom, size = 7, prob = .01) )
(Pnorm	  <- pnorm    (Rnorm, mean = -1, sd = 3) )
(Ppois	  <- ppois    (Rpois, lambda = 12) )
(Psignrank<- psignrank(Rsignrank, n = 47) )
(Pt	  <- pt	      (Rt,  df = 11) )
(Pt2	  <- pt	      (Rt2, df = 1.01) )
(Punif	  <- punif    (Runif, min = .2, max = 2) )
(Pweibull <- pweibull (Rweibull, shape = 3, scale = 2) )
(Pwilcox  <- pwilcox  (Rwilcox, m = 13, n = 17) )

dbeta	 (Rbeta, shape1 = .8, shape2 = 2)
dbinom	 (Rbinom, size = 55, prob = pi/16)
dcauchy	 (Rcauchy, location = 12, scale = 2)
dchisq	 (Rchisq, df = 3)
dexp	 (Rexp, rate = 2)
df	 (Rf, df1 = 12, df2 = 6)
dgamma	 (Rgamma, shape = 2, scale = 5)
dgeom	 (Rgeom, prob = pi/16)
dhyper	 (Rhyper, m = 40, n = 30, k = 20)
dlnorm	 (Rlnorm, meanlog = -1, sdlog = 3)
dlogis	 (Rlogis, location = 12, scale = 2)
dnbinom	 (Rnbinom, size = 7, prob = .01)
dnorm	 (Rnorm, mean = -1, sd = 3)
dpois	 (Rpois, lambda = 12)
dsignrank(Rsignrank, n = 47)
dt	 (Rt, df = 11)
dunif	 (Runif, min = .2, max = 2)
dweibull (Rweibull, shape = 3, scale = 2)
dwilcox	 (Rwilcox, m = 13, n = 17)

## Check q*(p*(.)) = identity
All.eq(Rbeta,	  qbeta	   (Pbeta, shape1 = .8, shape2 = 2))
All.eq(Rbinom,	  qbinom   (Pbinom, size = 55, prob = pi/16))
All.eq(Rcauchy,	  qcauchy  (Pcauchy, location = 12, scale = 2))
All.eq(Rchisq,	  qchisq   (Pchisq, df = 3))
All.eq(Rexp,	  qexp	   (Pexp, rate = 2))
All.eq(Rf,	  qf	   (Pf, df1 = 12, df2 = 6))
All.eq(Rgamma,	  qgamma   (Pgamma, shape = 2, scale = 5))
All.eq(Rgeom,	  qgeom	   (Pgeom, prob = pi/16))
All.eq(Rhyper,	  qhyper   (Phyper, m = 40, n = 30, k = 20))
All.eq(Rlnorm,	  qlnorm   (Plnorm, meanlog = -1, sdlog = 3))
All.eq(Rlogis,	  qlogis   (Plogis, location = 12, scale = 2))
All.eq(Rnbinom,	  qnbinom  (Pnbinom, size = 7, prob = .01))
All.eq(Rnorm,	  qnorm	   (Pnorm, mean = -1, sd = 3))
All.eq(Rpois,	  qpois	   (Ppois, lambda = 12))
All.eq(Rsignrank, qsignrank(Psignrank, n = 47))
All.eq(Rt,	  qt	   (Pt,	 df = 11))
all.equal(Rt2,	  qt	   (Pt2, df = 1.01), tol = 1e-2)
All.eq(Runif,	  qunif	   (Punif, min = .2, max = 2))
All.eq(Rweibull,  qweibull (Pweibull, shape = 3, scale = 2))
All.eq(Rwilcox,	  qwilcox  (Pwilcox, m = 13, n = 17))

## Same with "upper tail":
All.eq(Rbeta,	  qbeta	   (1- Pbeta, shape1 = .8, shape2 = 2, lower=F))
All.eq(Rbinom,	  qbinom   (1- Pbinom, size = 55, prob = pi/16, lower=F))
All.eq(Rcauchy,	  qcauchy  (1- Pcauchy, location = 12, scale = 2, lower=F))
All.eq(Rchisq,	  qchisq   (1- Pchisq, df = 3, lower=F))
All.eq(Rexp,	  qexp	   (1- Pexp, rate = 2, lower=F))
All.eq(Rf,	  qf	   (1- Pf, df1 = 12, df2 = 6, lower=F))
All.eq(Rgamma,	  qgamma   (1- Pgamma, shape = 2, scale = 5, lower=F))
All.eq(Rgeom,	  qgeom	   (1- Pgeom, prob = pi/16, lower=F))
All.eq(Rhyper,	  qhyper   (1- Phyper, m = 40, n = 30, k = 20, lower=F))
All.eq(Rlnorm,	  qlnorm   (1- Plnorm, meanlog = -1, sdlog = 3, lower=F))
All.eq(Rlogis,	  qlogis   (1- Plogis, location = 12, scale = 2, lower=F))
All.eq(Rnbinom,	  qnbinom  (1- Pnbinom, size = 7, prob = .01, lower=F))
All.eq(Rnorm,	  qnorm	   (1- Pnorm, mean = -1, sd = 3,lower=F))
All.eq(Rpois,	  qpois	   (1- Ppois, lambda = 12, lower=F))
All.eq(Rsignrank, qsignrank(1- Psignrank, n = 47, lower=F))
All.eq(Rt,	  qt	   (1- Pt,  df = 11,   lower=F))
all.equal(Rt2,	  qt	   (1- Pt2, df = 1.01, lower=F), tol = 1e-2)
All.eq(Runif,	  qunif	   (1- Punif, min = .2, max = 2, lower=F))
All.eq(Rweibull,  qweibull (1- Pweibull, shape = 3, scale = 2, lower=F))
All.eq(Rwilcox,	  qwilcox  (1- Pwilcox, m = 13, n = 17, lower=F))

## Check q*(p* ( log ), log) = identity
All.eq(Rbeta,	  qbeta	   (log(Pbeta), shape1 = .8, shape2 = 2, log=TRUE))
All.eq(Rbinom,	  qbinom   (log(Pbinom), size = 55, prob = pi/16, log=TRUE))
All.eq(Rcauchy,	  qcauchy  (log(Pcauchy), location = 12, scale = 2, log=TRUE))
all.equal(Rchisq,    qchisq   (log(Pchisq), df = 3, log=TRUE),tol=1e-14)
All.eq(Rexp,	  qexp	   (log(Pexp), rate = 2, log=TRUE))
All.eq(Rf,	  qf	   (log(Pf), df1= 12, df2= 6, log=TRUE))
All.eq(Rgamma,	  qgamma   (log(Pgamma), shape = 2, scale = 5, log=TRUE))
All.eq(Rgeom,	  qgeom	   (log(Pgeom), prob = pi/16, log=TRUE))
All.eq(Rhyper,	  qhyper   (log(Phyper), m = 40, n = 30, k = 20, log=TRUE))
All.eq(Rlnorm,	  qlnorm   (log(Plnorm), meanlog = -1, sdlog = 3, log=TRUE))
All.eq(Rlogis,	  qlogis   (log(Plogis), location = 12, scale = 2, log=TRUE))
All.eq(Rnbinom,	  qnbinom  (log(Pnbinom), size = 7, prob = .01, log=TRUE))
All.eq(Rnorm,	  qnorm	   (log(Pnorm), mean = -1, sd = 3, log=TRUE))
All.eq(Rpois,	  qpois	   (log(Ppois), lambda = 12, log=TRUE))
All.eq(Rsignrank, qsignrank(log(Psignrank), n = 47, log=TRUE))
All.eq(Rt,	  qt	   (log(Pt), df = 11, log=TRUE))
all.equal(Rt2,	  qt	   (log(Pt2), df = 1.01, log=TRUE), tol = 1e-2)
All.eq(Runif,	  qunif	   (log(Punif), min = .2, max = 2, log=TRUE))
All.eq(Rweibull,  qweibull (log(Pweibull), shape = 3, scale = 2, log=TRUE))
All.eq(Rwilcox,	  qwilcox  (log(Pwilcox), m = 13, n = 17, log=TRUE))

## same q*(p* (log) log) with upper tail:

All.eq(Rbeta,	  qbeta	   (log(1- Pbeta), shape1 = .8, shape2 = 2, lower=F, log=T))
All.eq(Rbinom,	  qbinom   (log(1- Pbinom), size = 55, prob = pi/16, lower=F, log=T))
All.eq(Rcauchy,	  qcauchy  (log(1- Pcauchy), location = 12, scale = 2, lower=F, log=T))
All.eq(Rchisq,	  qchisq   (log(1- Pchisq), df = 3, lower=F, log=T))
All.eq(Rexp,	  qexp	   (log(1- Pexp), rate = 2, lower=F, log=T))
All.eq(Rf,	  qf	   (log(1- Pf), df1 = 12, df2 = 6, lower=F, log=T))
All.eq(Rgamma,	  qgamma   (log(1- Pgamma), shape = 2, scale = 5, lower=F, log=T))
All.eq(Rgeom,	  qgeom	   (log(1- Pgeom), prob = pi/16, lower=F, log=T))
All.eq(Rhyper,	  qhyper   (log(1- Phyper), m = 40, n = 30, k = 20, lower=F, log=T))
All.eq(Rlnorm,	  qlnorm   (log(1- Plnorm), meanlog = -1, sdlog = 3, lower=F, log=T))
All.eq(Rlogis,	  qlogis   (log(1- Plogis), location = 12, scale = 2, lower=F, log=T))
All.eq(Rnbinom,	  qnbinom  (log(1- Pnbinom), size = 7, prob = .01, lower=F, log=T))
All.eq(Rnorm,	  qnorm	   (log(1- Pnorm), mean = -1, sd = 3, lower=F, log=T))
All.eq(Rpois,	  qpois	   (log(1- Ppois), lambda = 12, lower=F, log=T))
All.eq(Rsignrank, qsignrank(log(1- Psignrank), n = 47, lower=F, log=T))
All.eq(Rt,	  qt	   (log(1- Pt ), df = 11,   lower=F, log=T))
all.equal(Rt2,	  qt	   (log(1- Pt2), df = 1.01, lower=F, log=T), tol = 1e-2)
All.eq(Runif,	  qunif	   (log(1- Punif), min = .2, max = 2, lower=F, log=T))
All.eq(Rweibull,  qweibull (log(1- Pweibull), shape = 3, scale = 2, lower=F, log=T))
All.eq(Rwilcox,	  qwilcox  (log(1- Pwilcox), m = 13, n = 17, lower=F, log=T))


## Check log( upper.tail ):
All.eq(log(1 - Pbeta),	   pbeta    (Rbeta, shape1 = .8, shape2 = 2, lower=F, log=T))
All.eq(log(1 - Pbinom),	   pbinom   (Rbinom, size = 55, prob = pi/16, lower=F, log=T))
All.eq(log(1 - Pcauchy),   pcauchy  (Rcauchy, location = 12, scale = 2, lower=F, log=T))
All.eq(log(1 - Pchisq),	   pchisq   (Rchisq, df = 3, lower=F, log=T))
All.eq(log(1 - Pexp),	   pexp	    (Rexp, rate = 2, lower=F, log=T))
All.eq(log(1 - Pf),	   pf	    (Rf, df1 = 12, df2 = 6, lower=F, log=T))
All.eq(log(1 - Pgamma),	   pgamma   (Rgamma, shape = 2, scale = 5, lower=F, log=T))
All.eq(log(1 - Pgeom),	   pgeom    (Rgeom, prob = pi/16, lower=F, log=T))
All.eq(log(1 - Phyper),	   phyper   (Rhyper, m = 40, n = 30, k = 20, lower=F, log=T))
All.eq(log(1 - Plnorm),	   plnorm   (Rlnorm, meanlog = -1, sdlog = 3, lower=F, log=T))
All.eq(log(1 - Plogis),	   plogis   (Rlogis, location = 12, scale = 2, lower=F, log=T))
All.eq(log(1 - Pnbinom),   pnbinom  (Rnbinom, size = 7, prob = .01, lower=F, log=T))
All.eq(log(1 - Pnorm),	   pnorm    (Rnorm, mean = -1, sd = 3, lower=F, log=T))
All.eq(log(1 - Ppois),	   ppois    (Rpois, lambda = 12, lower=F, log=T))
All.eq(log(1 - Psignrank), psignrank(Rsignrank, n = 47, lower=F, log=T))
All.eq(log(1 - Pt),	   pt	    (Rt, df = 11,   lower=F, log=T))
All.eq(log(1 - Pt2),	   pt	    (Rt2,df = 1.01, lower=F, log=T))
All.eq(log(1 - Punif),	   punif    (Runif, min = .2, max = 2, lower=F, log=T))
All.eq(log(1 - Pweibull),  pweibull (Rweibull, shape = 3, scale = 2, lower=F, log=T))
All.eq(log(1 - Pwilcox),   pwilcox  (Rwilcox, m = 13, n = 17, lower=F, log=T))


### (Extreme) tail tests added more recently:
All.eq(1, -1e-17/ pexp(qexp(-1e-17, log=TRUE),log=TRUE))
abs(pgamma(30,100, lower=FALSE, log=TRUE) + 7.3384686328784e-24) < 1e-36
All.eq(1, pcauchy(-1e20)           /  3.18309886183791e-21)
All.eq(1, pcauchy(+1e15, log=TRUE) / -3.18309886183791e-16)## PR#6756
x <- 10^(ex <- c(1,2,5*(1:5),50,100,200,300,Inf))
for(a in x[ex > 10]) ## improve pt() : cbind(x,t= pt(-x, df=1), C=pcauchy(-x))
    print(all.equal(pt(-a, df=1), pcauchy(-a), tol = 1e-15))
## for PR#7902:
ex <- -c(rev(1/x), ex)
All.eq(-x, qcauchy(pcauchy(-x)))
All.eq(+x, qcauchy(pcauchy(+x, log=TRUE), log=TRUE))
All.eq(1/x, pcauchy(qcauchy(1/x)))
All.eq(ex,  pcauchy(qcauchy(ex, log=TRUE), log=TRUE))
II <- c(-Inf,Inf)
stopifnot(pcauchy(II) == 0:1, ## qcauchy(0:1) == II,
          pcauchy(II, log=TRUE) == c(-Inf,0),
          qcauchy(c(-Inf,0), log=TRUE) == II)

pr <- 1e-23 ## PR#6757
stopifnot(all.equal(pr^ 12, pbinom(11, 12, prob= pr,lower=FALSE),
                    tol= 1e-12, scale= 1e-270))
## pbinom(.) gave 0 in R 1.9.0
pp <- 1e-17 ## PR#6792
stopifnot(all.equal(2*pp, pgeom(1, pp), scale= 1e-20))
## pgeom(.) gave 0 in R 1.9.0

x <- 10^(100:295)
sapply(c(1e-250, 1e-25, 0.9, 1.1, 101, 1e10, 1e100),
       function(shape)
       All.eq(-x, pgamma(x, shape=shape, lower=FALSE, log=TRUE)))
x <- 2^(-1022:-900)
## where all completely off in R 2.0.1
all.equal(pgamma(x, 10, log = TRUE) - 10*log(x),
          rep(-15.104412573076, length(x)), tol = 1e-12)# 3.984e-14 (i386)
all.equal(pgamma(x, 0.1, log = TRUE) - 0.1*log(x),
          rep(0.0498724412598364, length(x)), tol = 1e-13)# 7e-16 (i386)

All.eq(dpois(  10, 2e-308, log=TRUE), -7100.13502718914)
All.eq(dpois(  20, 3e-308, log=TRUE), -14204.2875435307)
All.eq(dpois(1e20, 1e-290, log=TRUE), -7.12801378828154e+22)
## all gave -Inf in R 2.0.1


## Inf df in pf etc.
# apparently pf(df2=Inf) worked in 2.0.1 (undocumented) but df did not.
x <- c(1/pi, 1, pi)
oo <- options(digits = 8)
df(x, 3, 1e6)
df(x, 3, Inf)
pf(x, 3, 1e6)
pf(x, 3, Inf)

df(x, 1e6, 5)
df(x, Inf, 5)
pf(x, 1e6, 5)
pf(x, Inf, 5)

df(x, Inf, Inf)# (0, Inf, 0)  - since 2.1.1
pf(x, Inf, Inf)# (0, 1/2, 1)

pf(x, 5, Inf, ncp=0)
pf(x, 5, 1e6, ncp=1)
pf(x, 5, 1e7, ncp=1)
all.equal(pf(x, 5, 1e8, ncp=1), tol = 1e-6,
          c(0.0659330751, 0.4708802996, 0.9788764591))
pf(x, 5, Inf, ncp=1)

dt(1, Inf)
dt(1, Inf, ncp=0)
dt(1, Inf, ncp=1)
dt(1, 1e6, ncp=1)
dt(1, 1e7, ncp=1)
dt(1, 1e8, ncp=1)
dt(1, 1e10, ncp=1) # = Inf
options(oo)
## Inf valid as from 2.1.1: df(x, 1e16, 5) was way off in 2.0.1.

## PR#7099 : pf() with large df1 or df2:
nu <- 2^seq(25,34, 0.5)
y <- 1e9*(pf(1,1,nu) - 0.68268949)
stopifnot(All.eq(pf(1,1,Inf), 0.68268949213708596),
          diff(y) > 0, # i.e. pf(1,1, *) is monotone increasing
          All.eq(y [1], -5.07420372386491),
          All.eq(y[19],  2.12300110824515))
## not at all in R 2.1.0 or earlier

## qgamma(q, *) should give {0,Inf} for q={0,1}
sh <- c(1.1, 0.5, 0.2, 0.15, 1e-2, 1e-10)
stopifnot(Inf == qgamma(1, sh))
stopifnot(0   == qgamma(0, sh))
## the first gave Inf, NaN, and 99.425 in R 2.1.1 and earlier

cat("Time elapsed: ", proc.time() - .ptime,"\n")
