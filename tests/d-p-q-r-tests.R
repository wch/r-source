####	d|ensity
####	p|robability (cumulative)
####	q|uantile
####	r|andom number generation
####
####	Functions for  ``d/p/q/r''

F <- FALSE
T <- TRUE
showSys.time <- function(expr, ...) {
    ## prepend 'Time' for R CMD Rdiff
    st <- system.time(expr, ...)
    writeLines(paste("Time", capture.output(print(st))))
    invisible(st)
}

options(warn = 2)
##      ======== No warnings, unless explicitly asserted via
assertWarning <- tools::assertWarning

as.nan <- function(x) { x[is.na(x) & !is.nan(x)] <- NaN ; x }
###-- these are identical in ./arith-true.R ["fixme": use source(..)]
opt.conformance <- 0
Meps <- .Machine $ double.eps
xMax <- .Machine $ double.xmax
options(rErr.eps = 1e-30)
rErr <- function(approx, true, eps = getOption("rErr.eps", 1e-30))
{
    ifelse(Mod(true) >= eps,
	   1 - approx / true, # relative error
	   true - approx)     # absolute error (e.g. when true=0)
}
## Numerical equality: Here want "rel.error" almost always:
All.eq <- function(x,y) {
    all.equal.numeric(x,y, tolerance = 64*.Machine$double.eps,
                      scale = max(0, mean(abs(x), na.rm=TRUE)))
}
if(!interactive())
    set.seed(123)

.ptime <- proc.time()

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
    for(p in c(0,1,rbeta(n2, 2,4))) {
	for(k in rbinom(n3, size = n,  prob = runif(1)))
	    ## For X ~ Bin(n,p), compute 1 - P[X > k] = P[X <= k] in three ways:
	    stopifnot(all.equal(       pbinom(0:k, size = n, prob = p),
				cumsum(dbinom(0:k, size = n, prob = p))),
		      all.equal(if(k==n || p==0) 1 else
				pf((k+1)/(n-k)*(1-p)/p, df1=2*(n-k), df2=2*(k+1)),
				sum(dbinom(0:k, size = n, prob = p))))
    }
}

##__ 2. Geometric __
for(pr in seq(1e-10,1,len=15)) # p=0 is not a distribution
    stopifnot(All.eq((dg <- dgeom(0:10, pr)),
		     pr * (1-pr)^(0:10)),
	      All.eq(cumsum(dg), pgeom(0:10, pr)))


##__ 3. Hypergeometric __

.suppHyper <- function(m,n,k) max(0, k-n) : min(k, m)
hyp.mn <- rbind(m = c(10, 15, 999),
                n = c( 7,  0,   0))
for(j in 1:ncol(hyp.mn)) {
  mn <- hyp.mn[,j]; m <- mn[["m"]] ; n <- mn[["n"]]
  cat("m=",m,"; n=",n,":\n")
  showSys.time(for(k in 2:m) {
    x <- .suppHyper(m,n,k); x <- c(x[1]-1L, x)
    stopifnot(All.eq(phyper(x, m, n, k), cumsum(dhyper(x, m, n, k))))
    stopifnot(All.eq(phyper(x, m, n, k, log.p=TRUE),
          log(cumsum(dhyper(x, m, n, k)))))
  })
}

##__ 4. Negative Binomial __

## PR #842
for(size in seq(0.8,2, by=.1))
    stopifnot(all.equal(cumsum(dnbinom(0:7, size, .5)),
			       pnbinom(0:7, size, .5)))
stopifnot(All.eq(pnbinom(c(1,3), .9, .5),
		 c(0.777035760338812, 0.946945347071519)))

##__ 5. Poisson __

stopifnot(dpois(0:5,0)		 == c(1, rep(0,5)),
	  dpois(0:5,0, log=TRUE) == c(0, rep(-Inf, 5)))

## Cumulative Poisson '==' Cumulative Chi^2 :
## Abramowitz & Stegun, p.941 :	 26.4.21 (26.4.2)
n1 <- 20; n2 <- 16
for(lambda in rexp(n1))
    for(k in rpois(n2, lambda))
	stopifnot(all.equal(pchisq(2*lambda, 2*(1+ 0:k), lower.tail = FALSE),
			    pp <- cumsum(dpois(0:k, lambda=lambda)),
			    tolerance = 100*Meps),
		  all.equal(    pp, ppois(0:k, lambda=lambda), tolerance = 100*Meps),
		  all.equal(1 - pp, ppois(0:k, lambda=lambda, lower.tail = FALSE)))


##__ 6. SignRank __
for(n in rpois(32, lam=8)) {
    x <- -1:(n + 4)
    stopifnot(All.eq(psignrank(x, n), cumsum(dsignrank(x, n))))
}

##__ 7. Wilcoxon (symmetry & cumulative) __
is.sym <- TRUE
for(n in rpois(5, lam=6))
    for(m in rpois(15, lam=8)) {
	x <- -1:(n*m + 1)
	fx <- dwilcox(x, n, m)
	Fx <- pwilcox(x, n, m)
	is.sym <- is.sym & all(fx == dwilcox(x, m, n))
	stopifnot(All.eq(Fx, cumsum(fx)))
    }
stopifnot(is.sym)


###-------- Continuous Distributions ----------

##---  Gamma (incl. central chi^2) Density :
x <- round(rgamma(100, shape = 2),2)
for(sh in round(rlnorm(30),2)) {
    Ga <- gamma(sh)
    for(sig in round(rlnorm(30),2))
	stopifnot(all.equal((d1 <- dgamma(	 x,   shape = sh, scale = sig)),
                            (d2 <- dgamma(x/sig, shape = sh, scale = 1) / sig),
                            tolerance = 1e-14)## __ad interim__ was 1e-15
                  ,
                  All.eq(d1, (d3 <- 1/(Ga * sig^sh) * x^(sh-1) * exp(-x/sig)))
                  )
}

stopifnot(pgamma(1,Inf,scale=Inf) == 0)
## Also pgamma(Inf,Inf) == 1 for which NaN was slightly more appropriate
assertWarning(stopifnot(
    is.nan(c(pgamma(Inf,  1,scale=Inf),
             pgamma(Inf,Inf,scale=Inf)))))
scLrg <- c(2,100, 1e300*c(.1, 1,10,100), 1e307, xMax, Inf)
stopifnot(pgamma(Inf, 1, scale=xMax) == 1,
          pgamma(xMax,1, scale=Inf) == 0,
          all.equal(pgamma(1e300, 2, scale= scLrg, log=TRUE),
                    c(0, 0, -0.000499523968713701, -1.33089326820406,
                      -5.36470502873211, -9.91015144019122,
                      -32.9293385491433, -38.707517174609, -Inf),
                    tolerance = 2e-15)
          )

p <- 7e-4; df <- 0.9
stopifnot(
abs(1-c(pchisq(qchisq(p, df),df)/p, # was 2.31e-8 for R <= 1.8.1
        pchisq(qchisq(1-p, df,lower=FALSE),df,lower=FALSE)/(1-p),# was 1.618e-11
        pchisq(qchisq(log(p), df,log=TRUE),df, log=TRUE)/log(p), # was 3.181e-9
        pchisq(qchisq(log1p(-p),df,log=T,lower=F),df, log=T,lower=F)/log1p(-p)
        )# 32b-i386: (2.2e-16, 0,0, 3.3e-16); Opteron: (2.2e-16, 0,0, 2.2e-15)
    ) < 1e-14
)

##-- non central Chi^2 :
xB <- c(2000,1e6,1e50,Inf)
for(df in c(0.1, 1, 10))
    for(ncp in c(0, 1, 10, 100)) stopifnot(pchisq(xB, df=df, ncp=ncp) == 1)
stopifnot(all.equal(qchisq(0.025,31,ncp=1,lower.tail=FALSE),# inf.loop PR#875
                    49.7766246561514, tolerance = 1e-11))
for(df in c(0.1, 0.5, 1.5, 4.7, 10, 20,50,100)) {
    xx <- c(10^-(5:1), .9, 1.2, df + c(3,7,20,30,35,38))
    pp <- pchisq(xx, df=df, ncp = 1) #print(pp)
    dtol <- 1e-12 *(if(2 < df && df <= 50) 64 else if(df > 50) 20000 else 501)
    stopifnot(all.equal(xx, qchisq(pp, df=df, ncp=1), tolerance = dtol))
}

## p ~= 1 (<==> 1-p ~= 0) -- gave infinite loop in R <= 1.8.1 -- PR#6421
psml <- 2^-(10:54)
q0 <- qchisq(psml,    df=1.2, ncp=10, lower.tail=FALSE)
q1 <- qchisq(1-psml, df=1.2, ncp=10) # inaccurate in the tail
p0 <- pchisq(q0, df=1.2, ncp=10, lower.tail=FALSE)
p1 <- pchisq(q1, df=1.2, ncp=10, lower.tail=FALSE)
iO <- 1:30
stopifnot(all.equal(q0[iO], q1[iO], tolerance = 1e-5),# 9.86e-8
          all.equal(p0[iO], psml[iO])) # 1.07e-13

##--- Beta (need more):

## big a & b (PR #643)
stopifnot(is.finite(a <- rlnorm(20, 5.5)), a > 0,
          is.finite(b <- rlnorm(20, 6.5)), b > 0)
pab <- expand.grid(seq(0,1,by=.1), a, b)
p <- pab[,1]; a <- pab[,2]; b <- pab[,3]
stopifnot(all.equal(dbeta(p,a,b),
                    exp(pab <- dbeta(p,a,b, log = TRUE)), tolerance = 1e-11))
sp <- sample(pab, 50)
if(!interactive())
stopifnot(which(isI <- sp == -Inf) ==
              c(3, 10, 14, 18, 24, 32, 35, 41, 42, 45, 46, 47),
          all.equal(range(sp[!isI]), c(-2888.393250, 3.181137))
          )


##--- Normal (& Lognormal) :

stopifnot(
    qnorm(0) == -Inf, qnorm(-Inf, log = TRUE) == -Inf,
    qnorm(1) ==  Inf, qnorm( 0,   log = TRUE) ==  Inf)

assertWarning(stopifnot(
    is.nan(qnorm(1.1)),
    is.nan(qnorm(-.1))))

x <- c(-Inf, -1e100, 1:6, 1e200, Inf)
stopifnot(
    dnorm(x,3,s=0) == c(0,0,0,0, Inf, 0,0,0,0,0),
    pnorm(x,3,s=0) == c(0,0,0,0,  1 , 1,1,1,1,1),
    dnorm(x,3,s=Inf) == 0,
    pnorm(x,3,s=Inf) == c(0, rep(0.5, 8), 1))

stopifnot(
    ## 3 Test data from Wichura (1988) :
    all.equal(qnorm(c( 0.25,  .001, 1e-20)),
	      c(-0.6744897501960817, -3.090232306167814, -9.262340089798408),
	      tolerance = 1e-15)
  , ## extreme tail -- available on log scale only:
    all.equal(qe5 <- qnorm(-1e5, log = TRUE), -447.1978937)
  , ## much more accurate (2022-08):
    All.eq(-1e5, pnorm(qe5, log = TRUE))
)

z <- rnorm(1000); all.equal(pnorm(z),  1 - pnorm(-z), tolerance = 1e-15)
z <- c(-Inf,Inf,NA,NaN, rt(1000, df=2))
z.ok <- z > -37.5 | !is.finite(z)
for(df in 1:10) stopifnot(all.equal(pt(z, df), 1 - pt(-z,df), tolerance = 1e-15))

stopifnot(All.eq(pz <- pnorm(z), 1 - pnorm(z, lower=FALSE)),
          All.eq(pz,		    pnorm(-z, lower=FALSE)),
          All.eq(log(pz[z.ok]), pnorm(z[z.ok], log=TRUE)))
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
stopifnot(All.eq(pz, plnorm(exp(z))))


###==========  p <-> q	Inversion consistency =====================
ok <- 1e-5 < pz & pz < 1 - 1e-5
all.equal(z[ok], qnorm(pz[ok]), tolerance = 1e-12)

###===== Random numbers -- first, just output:

set.seed(123)
n <- 20
## for(pre in PDQR) { n <- paste("r",pre,sep=""); cat(n,": "); str(get(n))}
(Rbeta	  <- rbeta    (n, shape1 = .8, shape2 = 2) )
(Rbinom	  <- sort(unique(
             rbinom   (n, size = 55, prob = pi/16))))
(Rcauchy  <- rcauchy  (n, location = 12, scale = 2) )
(Rchisq	  <- rchisq   (n, df = 3) )
(Rexp	  <- rexp     (n, rate = 2) )
(Rf	  <- rf	      (n, df1 = 12, df2 = 6) )
(Rgamma	  <- rgamma   (n, shape = 2, scale = 5) )
(Rgeom	  <- sort(unique(
             rgeom    (n, prob = pi/16))))
(Rhyper	  <- sort(unique(
             rhyper   (n, m = 40, n = 30, k = 20))))
(Rlnorm	  <- rlnorm   (n, meanlog = -1, sdlog = 3) )
(Rlogis	  <- rlogis   (n, location = 12, scale = 2) )
(Rnbinom  <- rnbinom  (n, size = 7, prob = .01) )
(Rnorm	  <- rnorm    (n, mean = -1, sd = 3) )
(Rpois	  <- sort(unique(
             rpois    (n, lambda = 12))))
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
ep <- 1e-7
f1 <- 1 - 1e-7 # = 0.9999999
All.eq(Rbeta,	  qbeta	   (Pbeta, shape1 = .8, shape2 = 2))
All.eq(Rbinom,	  qbinom   (Pbinom*f1, size = 55, prob = pi/16))
All.eq(Rcauchy,	  qcauchy  (Pcauchy, location = 12, scale = 2))
All.eq(Rchisq,	  qchisq   (Pchisq, df = 3))
All.eq(Rexp,	  qexp	   (Pexp, rate = 2))
All.eq(Rf,	  qf	   (Pf, df1 = 12, df2 = 6))
All.eq(Rgamma,	  qgamma   (Pgamma, shape = 2, scale = 5))
All.eq(Rgeom,	  qgeom	   (Pgeom*f1, prob = pi/16))
All.eq(Rhyper,	  qhyper   (Phyper*f1, m = 40, n = 30, k = 20))
All.eq(Rlnorm,	  qlnorm   (Plnorm, meanlog = -1, sdlog = 3))
All.eq(Rlogis,	  qlogis   (Plogis, location = 12, scale = 2))
All.eq(Rnbinom,	  qnbinom  (Pnbinom*f1, size = 7, prob = .01))
All.eq(Rnorm,	  qnorm	   (Pnorm, mean = -1, sd = 3))
All.eq(Rpois,	  qpois	   (Ppois*f1, lambda = 12))
All.eq(Rsignrank, qsignrank(Psignrank*f1, n = 47))
All.eq(Rt,	  qt	   (Pt,	 df = 11))
All.eq(Rt2,	  qt	   (Pt2, df = 1.01))
All.eq(Runif,	  qunif	   (Punif, min = .2, max = 2))
All.eq(Rweibull,  qweibull (Pweibull, shape = 3, scale = 2))
All.eq(Rwilcox,	  qwilcox  (Pwilcox*f1, m = 13, n = 17))

## Same with "upper tail":
p1 <- 1 + ep
All.eq(Rbeta,	  qbeta	   (1- Pbeta, shape1 = .8, shape2 = 2, lower=F))
All.eq(Rbinom,	  qbinom  (p1- Pbinom, size = 55, prob = pi/16, lower=F))
All.eq(Rcauchy,	  qcauchy  (1- Pcauchy, location = 12, scale = 2, lower=F))
All.eq(Rchisq,	  qchisq   (1- Pchisq, df = 3, lower=F))
All.eq(Rexp,	  qexp	   (1- Pexp, rate = 2, lower=F))
All.eq(Rf,	  qf	   (1- Pf, df1 = 12, df2 = 6, lower=F))
All.eq(Rgamma,	  qgamma   (1- Pgamma, shape = 2, scale = 5, lower=F))
All.eq(Rgeom,	  qgeom	  (p1- Pgeom, prob = pi/16, lower=F))
All.eq(Rhyper,	  qhyper  (p1- Phyper, m = 40, n = 30, k = 20, lower=F))
All.eq(Rlnorm,	  qlnorm   (1- Plnorm, meanlog = -1, sdlog = 3, lower=F))
All.eq(Rlogis,	  qlogis   (1- Plogis, location = 12, scale = 2, lower=F))
All.eq(Rnbinom,	  qnbinom (p1- Pnbinom, size = 7, prob = .01, lower=F))
All.eq(Rnorm,	  qnorm	   (1- Pnorm, mean = -1, sd = 3,lower=F))
All.eq(Rpois,	  qpois	  (p1- Ppois, lambda = 12, lower=F))
All.eq(Rsignrank, qsignrank(p1-Psignrank, n = 47, lower=F))
All.eq(Rt,	  qt	   (1- Pt,  df = 11,   lower=F))
All.eq(Rt2,	  qt	   (1- Pt2, df = 1.01, lower=F))
All.eq(Runif,	  qunif	   (1- Punif, min = .2, max = 2, lower=F))
All.eq(Rweibull,  qweibull (1- Pweibull, shape = 3, scale = 2, lower=F))
All.eq(Rwilcox,	  qwilcox (p1- Pwilcox, m = 13, n = 17, lower=F))

## Check q*(p* ( log ), log) = identity
All.eq(Rbeta,	  qbeta	   (log(Pbeta), shape1 = .8, shape2 = 2, log=TRUE))
All.eq(Rbinom,	  qbinom   (log(Pbinom)-ep, size = 55, prob = pi/16, log=TRUE))
All.eq(Rcauchy,	  qcauchy  (log(Pcauchy), location = 12, scale = 2, log=TRUE))
All.eq(Rchisq,    qchisq   (log(Pchisq), df = 3, log=TRUE))
All.eq(Rexp,	  qexp	   (log(Pexp), rate = 2, log=TRUE))
All.eq(Rf,	  qf	   (log(Pf), df1= 12, df2= 6, log=TRUE))
All.eq(Rgamma,	  qgamma   (log(Pgamma), shape = 2, scale = 5, log=TRUE))
All.eq(Rgeom,	  qgeom	   (log(Pgeom)-ep, prob = pi/16, log=TRUE))
All.eq(Rhyper,	  qhyper   (log(Phyper)-ep, m = 40, n = 30, k = 20, log=TRUE))
All.eq(Rlnorm,	  qlnorm   (log(Plnorm), meanlog = -1, sdlog = 3, log=TRUE))
All.eq(Rlogis,	  qlogis   (log(Plogis), location = 12, scale = 2, log=TRUE))
All.eq(Rnbinom,	  qnbinom  (log(Pnbinom)-ep, size = 7, prob = .01, log=TRUE))
All.eq(Rnorm,	  qnorm	   (log(Pnorm), mean = -1, sd = 3, log=TRUE))
All.eq(Rpois,	  qpois	   (log(Ppois)-ep, lambda = 12, log=TRUE)) # fuzz for Solaris
All.eq(Rsignrank, qsignrank(log(Psignrank)-ep, n = 47, log=TRUE))
All.eq(Rt,	  qt	   (log(Pt), df = 11, log=TRUE))
All.eq(Rt2,	  qt	   (log(Pt2), df = 1.01, log=TRUE))
All.eq(Runif,	  qunif	   (log(Punif), min = .2, max = 2, log=TRUE))
All.eq(Rweibull,  qweibull (log(Pweibull), shape = 3, scale = 2, log=TRUE))
All.eq(Rwilcox,	  qwilcox  (log(Pwilcox)-ep, m = 13, n = 17, log=TRUE))

## same q*(p* (log) log) with upper tail:
All.eq(Rbeta,	  qbeta	   (log1p(-Pbeta), shape1 = .8, shape2 = 2, lower=F, log=T))
All.eq(Rbinom,	  qbinom   (log1p(-Pbinom)+ep, size = 55, prob = pi/16, lower=F, log=T))
All.eq(Rcauchy,	  qcauchy  (log1p(-Pcauchy), location = 12, scale = 2, lower=F, log=T))
All.eq(Rchisq,	  qchisq   (log1p(-Pchisq), df = 3, lower=F, log=T))
All.eq(Rexp,	  qexp	   (log1p(-Pexp), rate = 2, lower=F, log=T))
All.eq(Rf,	  qf	   (log1p(-Pf), df1 = 12, df2 = 6, lower=F, log=T))
All.eq(Rgamma,	  qgamma   (log1p(-Pgamma), shape = 2, scale = 5, lower=F, log=T))
All.eq(Rgeom,	  qgeom	   (log1p(-Pgeom)+ep, prob = pi/16, lower=F, log=T))
All.eq(Rhyper,	  qhyper   (log1p(-Phyper)+ep, m = 40, n = 30, k = 20, lower=F, log=T))
All.eq(Rlnorm,	  qlnorm   (log1p(-Plnorm), meanlog = -1, sdlog = 3, lower=F, log=T))
All.eq(Rlogis,	  qlogis   (log1p(-Plogis), location = 12, scale = 2, lower=F, log=T))
All.eq(Rnbinom,	  qnbinom  (log1p(-Pnbinom)+ep, size = 7, prob = .01, lower=F, log=T))
All.eq(Rnorm,	  qnorm	   (log1p(-Pnorm), mean = -1, sd = 3, lower=F, log=T))
All.eq(Rpois,	  qpois	   (log1p(-Ppois)+ep, lambda = 12, lower=F, log=T))
All.eq(Rsignrank, qsignrank(log1p(-Psignrank)+ep, n = 47, lower=F, log=T))
All.eq(Rt,	  qt	   (log1p(-Pt ), df = 11,   lower=F, log=T))
All.eq(Rt2,	  qt	   (log1p(-Pt2), df = 1.01, lower=F, log=T))
All.eq(Runif,	  qunif	   (log1p(-Punif), min = .2, max = 2, lower=F, log=T))
All.eq(Rweibull,  qweibull (log1p(-Pweibull), shape = 3, scale = 2, lower=F, log=T))
All.eq(Rwilcox,	  qwilcox  (log1p(-Pwilcox)+ep, m = 13, n = 17, lower=F, log=T))


## Check log( upper.tail ):
All.eq(log1p(-Pbeta),	  pbeta	   (Rbeta, shape1 = .8, shape2 = 2, lower=F, log=T))
All.eq(log1p(-Pbinom),	  pbinom   (Rbinom, size = 55, prob = pi/16, lower=F, log=T))
All.eq(log1p(-Pcauchy),	  pcauchy  (Rcauchy, location = 12, scale = 2, lower=F, log=T))
All.eq(log1p(-Pchisq),	  pchisq   (Rchisq, df = 3, lower=F, log=T))
All.eq(log1p(-Pexp),	  pexp	   (Rexp, rate = 2, lower=F, log=T))
All.eq(log1p(-Pf),	  pf	   (Rf, df1 = 12, df2 = 6, lower=F, log=T))
All.eq(log1p(-Pgamma),	  pgamma   (Rgamma, shape = 2, scale = 5, lower=F, log=T))
All.eq(log1p(-Pgeom),	  pgeom	   (Rgeom, prob = pi/16, lower=F, log=T))
All.eq(log1p(-Phyper),	  phyper   (Rhyper, m = 40, n = 30, k = 20, lower=F, log=T))
All.eq(log1p(-Plnorm),	  plnorm   (Rlnorm, meanlog = -1, sdlog = 3, lower=F, log=T))
All.eq(log1p(-Plogis),	  plogis   (Rlogis, location = 12, scale = 2, lower=F, log=T))
All.eq(log1p(-Pnbinom),	  pnbinom  (Rnbinom, size = 7, prob = .01, lower=F, log=T))
All.eq(log1p(-Pnorm),	  pnorm	   (Rnorm, mean = -1, sd = 3, lower=F, log=T))
All.eq(log1p(-Ppois),	  ppois	   (Rpois, lambda = 12, lower=F, log=T))
All.eq(log1p(-Psignrank), psignrank(Rsignrank, n = 47, lower=F, log=T))
All.eq(log1p(-Pt),	  pt	   (Rt, df = 11,   lower=F, log=T))
All.eq(log1p(-Pt2),	  pt	   (Rt2,df = 1.01, lower=F, log=T))
All.eq(log1p(-Punif),	  punif	   (Runif, min = .2, max = 2, lower=F, log=T))
All.eq(log1p(-Pweibull),  pweibull (Rweibull, shape = 3, scale = 2, lower=F, log=T))
All.eq(log1p(-Pwilcox),	  pwilcox  (Rwilcox, m = 13, n = 17, lower=F, log=T))


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
all.equal(pf(x, 5, 1e6, ncp=1), tolerance = 1e-6,
          c(0.065933194, 0.470879987, 0.978875867))
all.equal(pf(x, 5, 1e7, ncp=1), tolerance = 1e-6,
          c(0.06593309, 0.47088028, 0.97887641))
all.equal(pf(x, 5, 1e8, ncp=1), tolerance = 1e-6,
          c(0.0659330751, 0.4708802996, 0.9788764591))
pf(x, 5, Inf, ncp=1)

dt(1, Inf)
dt(1, Inf, ncp=0)
dt(1, Inf, ncp=1)
dt(1, 1e6, ncp=1)
dt(1, 1e7, ncp=1)
dt(1, 1e8, ncp=1)
dt(1, 1e10, ncp=1) # = Inf
## Inf valid as from 2.1.1: df(x, 1e16, 5) was way off in 2.0.1.

sml.x <- c(10^-c(2:8,100), 0)
cbind(x = sml.x, `dt(x,*)` = dt(sml.x, df = 2, ncp=1))
## small 'x' used to suffer from cancellation
options(oo)

## NB:  Do  *NOT*  add new examples here, but rather  in  ./d-p-q-r-tst-2.R
## ==        ~~~                    ~~~~  ~~~               ~~~~~~~~~~~~~~~

cat("Time elapsed: ", proc.time() - .ptime,"\n")
