###---- ALL tests here should return  TRUE !
###
### '##P': This lines give not 'T' but relevant ``Print output''
###

Meps <- .Machine $ double.eps
abs(1- .Machine$double.xmax * 10^(-.Machine$double.max.exp*log10(2)))/Meps < 1e3
abs(1- .Machine$double.xmin * 10^(-.Machine$double.min.exp*log10(2)))/Meps < 1e3
##P (1- .Machine$double.xmax * 10^(-.Machine$double.max.exp*log10(2)))/Meps
##P (1- .Machine$double.xmin * 10^(-.Machine$double.min.exp*log10(2)))/Meps
log10(.Machine$double.xmax) / log10(2) == .Machine$double.max.exp
log10(.Machine$double.xmin) / log10(2) == .Machine$double.min.exp


Im(cos(acos(1i))) == 1
Im(cos(acos(1i))) == 1
Ii <- complex(imag = 1) #-- 1i has a bug in 0.49
abs(1 - Im(sin(asin(Ii)))) < 4*Meps
abs(1 - Im(cos(acos(Ii)))) < 4*Meps
##P (1 - Im(sin(asin(Ii))))/Meps
##P (1 - Im(cos(acos(Ii))))/Meps

.Random.seed <- c(629, 6137, 22167) # want reproducible output
ii <- Im(sin(asin(Ii + rnorm(100))))
all(abs(ii-1) < 100* Meps)
##P table(2*abs(ii-1)  / Meps)
ii <- Im(cos(acos(Ii + rnorm(100))))
all(abs(ii-1) < 100* Meps)
##P table(2*abs(ii-1)  / Meps)
ii <- Im(atan(tan(Ii + rnorm(100)))) #-- tan(atan(..)) does NOT work (Math!)
all(abs(ii-1) < 100* Meps)
##P table(2*abs(ii-1)  / Meps)

all(names(c(a=pi, b=1, d=1:4)) == c("a","b", paste("d", 1:4, sep="")))
##P names(c(a=pi, b=1, d=1:4))
ncb <- dimnames(cbind(a=1, yy=1:3))[[2]]
(!is.null(ncb)) && all(ncb == c("a","yy"))

all(cbind(a=1:2, b=1:3, c=1:6) == t(rbind(a=1:2, b=1:3, c=1:6)))
##P rbind(a=1:2, b=1:3, c=1:6)

source("all.equal.R")

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
