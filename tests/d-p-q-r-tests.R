####	d|ensity
####    p|robability (cumulative)
####	q|uantile
####    r|andom number generation
####
####    Functions for  ``d/p/q/r''

source(paste(getenv("RHOME"),"tests/all.equal.R", sep="/"))

if(!interactive()) .Random.seed <- rep(7654, 3)

##--- Cumulative Poisson '==' Cumulative Chi^2 :
##--- Abramowitz & Stegun, p.941 :  26.4.21 (26.4.2)
n1 <- 20; n2 <- 16
for(lambda in rexp(n1))
  for(k in rpois(n2, lambda)) {
    tst <- all.equal(1 - pchisq(2*lambda, 2*(k+1)),
                     sum(dpois(0:k, lambda=lambda)))
    if(!(is.logical(tst) && tst))
      cat("lambda=", format(lambda),".  k =",k, " --> tst=", tst,"\n")
  }

##--- Cumulative Binomial '==' Cumulative F :
##--- Abramowitz & Stegun, p.945-6;  26.5.24  AND  26.5.28 :
n0 <- 50; n1 <- 16; n2 <- 20; n3 <- 8
for(n in rbinom(n1, size = 2*n0, p = .4)) {
  cat("n=",n,": ")
  for(p in c(0,1,rbeta(n2, 2,4))) {
    cat(".")
    for(k in rbinom(n3, size = n,  prob = runif(1))) {
      ## For X ~ Bin(n,p),  compute  1 - P[X > k] = P[X <= k]  in two ways :
      tst <- all.equal(if(k==n || p==0) 1 else
                       pf((k+1)/(n-k)*(1-p)/p, df1= 2*(n-k), df2 = 2*(k+1)),
                       sum(dbinom(0:k, size = n, prob = p)))
      if(!(is.logical(tst) && tst))
        cat("n=", n,"; p =", format(p),".  k =",k, " --> tst=", tst,"\n")
    }
  }
  cat("\n")
}
