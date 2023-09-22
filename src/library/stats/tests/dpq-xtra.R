#### Extraneous tests for [dpqr] functions

### 1. [Bug 18509] New: qnorm(), qexp(), etc. are inconsistent in return.. for empty inputs
##     Date: Tue, 11 Apr 2023

## https://bugs.r-project.org/show_bug.cgi?id=18509

## Summary : qnorm(), qexp(), etc. are inconsistent in returning a matrix format for empty inputs
## Version : R 4.2.x
## Reporter: karolis@koncevicius.lt


quote({					     # Result before bug fix for x = <0x0 matrix>
### -- q ---                                   ----------------------
try(
    qbirthday(x, 1, 1)	)		     # <error>	 <<<<<<<<< fixed in R code
    qsmirnov (x, c(1,1))		     # numeric(0) <<<<<<<< fixed in R code
    ##
    qbeta(x, 1, 1)			     # numeric(0)
    qbeta(x, 1, 1, 0)			     # numeric(0)
    qbinom(x, 1, 1)			     # numeric(0)
    qcauchy(x)				     # numeric(0)
    qchisq(x, 1)			     # <0 x 0 matrix>
    qchisq(x, 1, 0)			     # numeric(0)
    qexp(x, 1)				     # <0 x 0 matrix>
    qf(x, 1, 1)				     # numeric(0)
    qf(x, 1, 1, 0)			     # numeric(0)
    qgamma(x, 1)			     # numeric(0)
    qgeom(x, 1)				     # <0 x 0 matrix>
    qhyper(x, 1, 1, 1)			     # numeric(0)
    qlnorm(x)				     # numeric(0)
    qlogis(x)				     # numeric(0)
    qnbinom(x, 1, prob=1)		     # numeric(0)
    qnbinom(x, 1, mu=1)			     # numeric(0)
    qnorm(x)				     # numeric(0)
    qpois(x, 1)				     # <0 x 0 matrix>
    qsignrank(x, 1)			     # <0 x 0 matrix>
    qt(x, 1)				     # <0 x 0 matrix>
    qt(x, 1, 0)				     # numeric(0)
    qtukey(x, 1, 1)			     # numeric(0)
    qtukey(x, 1, 1, 1)			     # numeric(0) {3rd optional parameter arg.} -> math4()
    qunif(x)				     # numeric(0)
    qweibull(x, 1)			     # numeric(0)
    qweibull(x, 1, 1)			     # numeric(0) {2nd optional parameter arg.}
    qwilcox(x, 1, 1)			     # numeric(0)
    ##
### -- p ---
    ##
    pbirthday(x, 1, 1)			     # '1' *wrongly* !
    psmirnov (x, c(1,1))		     # numeric(0) <<<<<<<< fixed in R code
    ##
    pbeta(x, 1, 1)			     # numeric(0)
    pbeta(x, 1, 1, 0)			     # numeric(0)
    pbinom(x, 1, 1)			     # numeric(0)
    pcauchy(x)				     # numeric(0)
    pchisq(x, 1)			     # <0 x 0 matrix>
    pchisq(x, 1, 0)			     # numeric(0)
    pexp(x, 1)				     # <0 x 0 matrix>
    pf(x, 1, 1)				     # numeric(0)
    pf(x, 1, 1, 0)			     # numeric(0)
    pgamma(x, 1)			     # numeric(0)
    pgeom(x, 1)				     # <0 x 0 matrix>
    phyper(x, 1, 1, 1)			     # numeric(0)
    plnorm(x)				     # numeric(0)
    plogis(x)				     # numeric(0)
    pnbinom(x, 1, prob=1)		     # numeric(0)
    pnbinom(x, 1, mu=1)			     # numeric(0)
    pnorm(x)				     # numeric(0)
    ppois(x, 1)				     # <0 x 0 matrix>
    psignrank(x, 1)			     # <0 x 0 matrix>
    pt(x, 1)				     # <0 x 0 matrix>
    pt(x, 1, 0)				     # numeric(0)
    ptukey(x, 1, 1)			     # numeric(0)
    ptukey(x, 1, 1, 1)			     # numeric(0) {3rd optional parameter arg.} -> math4()
    punif(x)				     # numeric(0)
    pweibull(x, 1)			     # numeric(0)
    pweibull(x, 1, 1)			     # numeric(0) {2nd optional parameter arg.}
    pwilcox(x, 1, 1)			     # numeric(0)
    ##
### -- d --- no	 dtukey(), dbirthday(), nor  dsmirnov() ---
    ##
    dbeta(x, 1, 1)			     # numeric(0)
    dbeta(x, 1, 1, 0)			     # numeric(0)
    dbinom(x, 1, 1)			     # numeric(0)
    dcauchy(x)				     # numeric(0)
    dchisq(x, 1)			     # <0 x 0 matrix>
    dchisq(x, 1, 0)			     # numeric(0)
    dexp(x, 1)				     # <0 x 0 matrix>
    df(x, 1, 1)				     # numeric(0)
    df(x, 1, 1, 0)			     # numeric(0)
    dgamma(x, 1)			     # numeric(0)
    dgeom(x, 1)				     # <0 x 0 matrix>
    dhyper(x, 1, 1, 1)			     # numeric(0)
    dlnorm(x)				     # numeric(0)
    dlogis(x)				     # numeric(0)
    dnbinom(x, 1, prob=1)		     # numeric(0)
    dnbinom(x, 1, mu=1)			     # numeric(0)
    dnorm(x)				     # numeric(0)
    dpois(x, 1)				     # <0 x 0 matrix>
    dsignrank(x, 1)			     # <0 x 0 matrix>
    dt(x, 1)				     # <0 x 0 matrix>
    dt(x, 1, 0)				     # numeric(0)
    dunif(x)				     # numeric(0)
    dweibull(x, 1)			     # numeric(0)
    dweibull(x, 1, 1)			     # numeric(0) {2nd optional darameter arg.}
    dwilcox(x, 1, 1)			     # numeric(0)
}) -> dpqCalls_

x <- matrix(numeric(), nrow=0, ncol=0, dimnames=list(RN=NULL, cn=NULL))
## remove source attributes and the leading `{` and trailing `}` :
dpqCalls <- removeSource(dpqCalls_)
dpqCalls <- dpqCalls[-c(1L, length(dpqCalls))]
val <- setNames(lapply(dpqCalls, eval), # 1 (try-)error in R <= 4.3.0
                gsub(" ", "", vapply(dpqCalls, deparse1, "")))
if(getRversion() < "4.4") # interesting there, showing "pre bug-fix"
  str(val, give.attr=FALSE)

str(val[[1]])
 ## num[0 , 0 ]
 ## - attr(*, "dimnames")=List of 2
 ##  ..$ RN: NULL
 ##  ..$ cn: NULL
stopifnot(is.list(val),
	  length(val) == length(dpqCalls),
          ## all calls return `x`, the 0x0-matrix w/ dimnames :
	  length(unique(val)) == 1,
	  identical(val[[1]], x))

