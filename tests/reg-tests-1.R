## PR 640 (diff.default computes an incorrect starting time)
## By: Laimonis Kavalieris <lkavalieris@maths.otago.ac.nz>
library(ts)
y <- ts(rnorm(24), freq=12)
x <- ts(rnorm(24), freq=12)
arima0(y,xreg=x, seasonal=list(order=c(0,1,0)))
## Comments:

## PR 644 (crash using fisher.test on Windows)
## By: Uwe Ligges <ligges@statistik.uni-dortmund.de>
library(ctest)
x <- matrix(c(2, 2, 4, 8, 6, 0, 1, 1, 7, 8, 1, 3, 1, 3, 7, 4, 2, 2, 2,
              1, 1, 0, 0, 0, 0, 0, 1, 1, 2, 0, 1, 1, 0, 2, 1, 0, 0, 0),
            nc = 2)
fisher.test(x)
## Comments: (wasn't just on Windows)

## PR 653 (extrapolation in spline)
## By: Ian White <imsw@holyrood.ed.ac.uk>
x <- c(2,5,8,10)
y <- c(1.2266,-1.7606,-0.5051,1.0390)
fn <- splinefun(x, y, method="natural")
xx1 <- fn(0:12)
# should be the same if reflected
fn <- splinefun(rev(-x),rev(y),method="natural")
xx2 <- fn(0:-12)
stopifnot(all.equal(xx1, xx2))
# should be the same as interpSpline
library(splines)
xx3 <- predict(interpSpline(x, y), 0:12)
stopifnot(all.equal(xx1, xx3$y))
detach("package:splines")
## Comments: all three differed in 1.2.1.

## PR 698 (print problem with data frames)
## actually, a subsetting problem with data frames
fred <- data.frame(happy=c(TRUE, FALSE, TRUE), sad=7:9)
z <- try(tmp <- fred[c(FALSE, FALSE, TRUE, TRUE)])
stopifnot(class(z) == "try-error")
## Comments: No error before 1.2.1

## PR 753 (step can't find variables)
##
x <- data.frame(a=rnorm(10), b=rnorm(10), c=rnorm(10))
x0.lm <- lm(a ~ 1, data=x)
step(x0.lm, ~ b + c)
## Comments:

## PR 796 (aic in binomial models is often wrong)
##
data(esoph)
a1 <- glm(cbind(ncases, ncontrols) ~ agegp + tobgp * alcgp,
          data = esoph, family = binomial())$aic
a1
a2 <- glm(ncases/(ncases+ncontrols) ~ agegp + tobgp * alcgp,
          data = esoph, family = binomial(), weights=ncases+ncontrols)$aic
a2
stopifnot(a1 == a2)
## Comments:
# both should be 236.9645

## Follow up: example from Lindsey, purportedly of inaccuracy in aic
y <- matrix(c(2, 0, 7, 3, 0, 9), ncol=2)
x <- gl(3, 1)
a <- glm(y ~ x, family=binomial)$aic
stopifnot(is.finite(a))
## Comments: gave NaN prior to 1.2.1

## PR 802 (crash with scan(..., what=list(,,)))
##
m <- matrix(1:9, 3,3)
write(m, "test.dat", 3)
try(scan("test.dat", what=list(,,,)))
unlink("test.dat")
## Comments: segfaulted in 1.2.0

## Jonathan Rougier, 2001-01-30  [bug in 1.2.1 and earlier]
tmp <- array(list(3), c(2, 3))
tmp[[2, 3]] <- "fred"
all.equal(t(tmp), aperm(tmp))

## PR 860 (Context problem with ... and rbind) Prof Brian D Ripley, 2001-03-03,
f <- function(x, ...)
{
   g <- function(x, ...) x
   rbind(numeric(), g(x, ...))
}
f(1:3)
## Error in 1.2.2
f <- function(x, ...) h(g(x, ...))
g <- function(x, ...) x
h <- function(...)substitute(list(...))
f(1)
## Error in 1.2.2
substitute(list(...))
## Error in 1.2.2


## Martin Maechler, 2001-03-07 [1.2.2 and in parts earlier]
tf <- tempfile()
cat(1:3,"\n", file = tf)
for(line in list(4:6, "", 7:9)) cat(line,"\n", file = tf, append = TRUE)

count.fields(tf) # 3 3 3 : ok {blank line skipped}
z <- scan(tf, what=rep(list(""),3), nmax = 3)
all(sapply(z, length) == 3)
## FALSE in 1.2.2
z <- as.data.frame(scan(tf, what=rep(list(""),3), n=9))
dim(z)
## should be 3 3.  Was 2 3 in 1.2.2.
read.table(tf)
## gave error in 1.2.2
unlink(tf)

## PR 870 (as.numeric and NAs)  Harald Fekjær, 2001-03-08,
is.na(as.numeric(" "))
is.na(as.integer(" "))
is.na(as.complex(" "))
## all false in 1.2.2

## PR 871 (deparsing of attribute names) Harald Fekjær, 2001-03-08,
midl <- 4
attr(midl,"Object created") <- date()
deparse(midl)
dump("midl", "midl.R")
source("midl.R") ## syntax error in 1.2.2
unlink("midl.R")

## PR 872 (surprising behavior of match.arg()) Setzer Woodrow, 2001-03-08,
fun1 <- function(x, A=c("power","constant")) {
  arg <- match.arg(A)
  formals()
}
topfun <- function(x, Fun=fun1) {
  a1 <- fun1(x)
  print(a1)
  a2 <- Fun(x,A="power")
  stopifnot(all.equal(a1, a2))
  print(a2)
}
topfun(2, fun1)
## a1 printed without defaults in 1.2.2

## PR 873 (long formulas in terms()) Jerome Asselin, 2001-03-08,
form <- cbind(log(inflowd1),log(inflowd2),log(inflowd3),
    log(inflowd4),log(inflowd5),log(inflowd6)) ~ precip*I(Tmax^2)
terms(form) # error in 1.2.2

## PR 881 Incorrect values in non-central chisq values on Linux, 2001-03-21
x <- dchisq(c(7.1, 7.2, 7.3), df=2,ncp=20)
stopifnot(all(diff(x) > 0))
## on 1.2.2 on RH6.2 i686 Linux x = 0.01140512 0.00804528 0.01210514

## PR 882 eigen segfaults on 0-diml matrices, 2001-03-23
m <- matrix(1, 0, 0)  # 1 to force numeric not logical
try(eigen(m))
## segfaults on 1.2.2

## PR 902 segfaults when warning string is too long, Ben Bolker 2001-04-09
provoke.bug <- function(n=9000) {
   warnmsg <- paste(LETTERS[sample(1:26,n,replace=TRUE)],collapse="")
   warning(warnmsg)
}
provoke.bug()
## segfaulted in 1.2.2, will also on machines without vsnprintf.
##                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## and hence keep the above line at the end of this file
