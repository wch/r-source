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

## PR 872 (surprising behavior of match.arg()) Woodrow Setzer, 2001-03-08,
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
x <- dchisq(c(7.1, 7.2, 7.3), df=2, ncp=20)
stopifnot(all(diff(x) > 0))
## on 1.2.2 on RH6.2 i686 Linux x = 0.01140512 0.00804528 0.01210514

## PR 882 eigen segfaults on 0-diml matrices, 2001-03-23
m <- matrix(1, 0, 0)  # 1 to force numeric not logical
try(eigen(m))
## segfaults on 1.2.2

## 1.3.0 had poor compression on gzfile() with lots of small pieces.
if (capabilities("libz")) {
    zz <- gzfile("t1.gz", "w")
    write(1:1000, zz)
    close(zz)
    (sz <- file.info("t1.gz")$size)
    unlink("t1.gz")
    stopifnot(sz < 2000)
}

## PR 1010: plot.mts (type="p") was broken in 1.3.0 and this call failed.
plot(ts(matrix(runif(10), ncol = 2)), type = "p")

## in 1.3.0 readLines(ok=FALSE) failed.
cat(file="foo", 1:10, sep="\n")
x <- try(readLines("foo", 100, ok=FALSE))
unlink("foo")
stopifnot(length(class(x)) == 1 &&class(x) == "try-error")

## PR 1047 [<-data.frame failure, BDR 2001-08-10
test <- df <- data.frame(x=1:10, y=11:20, row.names=letters[1:10])
test[] <- lapply(df, factor)
test
## error in 1.3.0 in test[]

## PR 1048 bug in dummy.coef.lm, Adrian Baddeley, 2001-08-10
## modified to give a sensible test
old <- getOption("contrasts")
options(contrasts=c("contr.helmert", "contr.poly"))
DF <- data.frame(x=1:20,y=rnorm(20),z=factor(1:20 <= 10))
dummy.coef.lm(lm(y ~ z * I(x), data=DF))
dummy.coef.lm(lm(y ~ z * poly(x,1), data=DF))
## failed in 1.3.0.  Second one warns: deficiency of the method.
options(contrasts=old)

## PR 1050 error in ksmooth C code + patch, Hsiu-Khuern Tang, 2001-08-12
library(modreg)
x <- 1:4
y <- 1:4
z <- ksmooth(x, y, x.points=x)
stopifnot(all.equal(z$y, y))
detach("package:modreg")
## did some smoothing prior to 1.3.1.

## The length of lines read by scan() was limited before 1.4.0
xx <- paste(rep(0:9, 2000), collapse="")
zz <- file("foo.txt", "w")
writeLines(xx, zz)
close(zz)
xxx <- scan("foo.txt", "", sep="\n")
stopifnot(identical(xx, xxx))
unlink("foo.txt")

## as.character was truncating formulae:  John Fox 2001-08-23
mod <- this ~ is + a + very + long + formula + with + a + very + large + number + of + characters
zz <- as.character(mod)
zz
nchar(zz)
stopifnot(nchar(zz)[3] == 83)
## truncated in 1.3.0

## substr<-, Tom Vogels, 2001-09-07
x <- "abcdef"
substr(x, 2, 3) <- "wx"
stopifnot(x == "awxdef")

x <- "abcdef"
substr(x, 2, 3) <- "wxy"
stopifnot(x == "awxdef")

x <- "abcdef"
substr(x, 2, 3) <- "w"
stopifnot(x == "awcdef")
## last was "aw" in 1.3.1


## reading bytes from a connection,  Friedrich Leisch 2001-09-07
cat("Hello World", file="world.txt")
con <- file("world.txt", "r")
zz <- readChar(con, 100)
close(con)
unlink("world.txt")
stopifnot(zz == "Hello World")
## was "" in 1.3.1.


## prediction was failing for intercept-only model
## as model frame has no columns.
d <- data.frame(x=runif(50), y=rnorm(50))
d.lm <- lm(y ~ 1, data=d)
predict(d.lm, data.frame(x=0.5))
## error in 1.3.1


## predict.arima0 needed a matrix newxreg: Roger Koenker, 2001-09-27
library(ts)
u <- rnorm(120)
s <- 1:120
y <- 0.3*s + 5*filter(u, c(.95,-.1), "recursive", init=rnorm(2))
fit0 <- arima0(y,order=c(2,0,0), xreg=s)
fit1 <- arima0(y,order=c(2,1,0), xreg=s, include.mean=TRUE)
fore0 <- predict(fit0 ,n.ahead=44, newxreg=121:164)
fore1 <- predict(fit1, n.ahead=44, newxreg=121:164)
par(mfrow=c(1,2))
ts.plot(y,fore0$pred, fore0$pred+2*fore0$se, fore0$pred-2*fore0$se,
                gpars=list(lty=c(1,2,3,3)))
abline(fit0$coef[3:4], lty=2)
ts.plot(y, fore1$pred, fore1$pred+2*fore1$se, fore1$pred-2*fore1$se,
                gpars=list(lty=c(1,2,3,3)))
abline(c(0, fit1$coef[3]), lty=2)
detach("package:ts")


## merging when NA is a level
a <- data.frame(x = 1:4)
b <- data.frame(x = 1:3, y = factor(c("NA", "a", "b"), exclude=""))
(m <- merge(a, b, all.x = TRUE))
stopifnot(is.na(m[4, 2]))
## was level NA in 1.3.1
stopifnot(!is.na(m[1, 2]))


## merging with POSIXct columns:
x <- data.frame(a = as.POSIXct(Sys.time() + (1:3)*10000), b = LETTERS[1:3])
y <- data.frame(b = LETTERS[3:4], c = 1:2)
stopifnot(1 == nrow(merge(x, y)))
stopifnot(4 == nrow(merge(x, y, all = TRUE)))


## PR 1149.  promax was returning the wrong rotation matrix.
library(mva)
data(ability.cov)
ability.FA <- factanal(factors = 2, covmat = ability.cov, rotation = "none")
pm <- promax(ability.FA$loadings)
tmp1 <- as.vector(ability.FA$loadings %*% pm$rotmat)
tmp2 <- as.vector(pm$loadings)
stopifnot(all.equal(tmp1, tmp2))
detach("package:mva")
rm(ability.cov)


## PR 1155. On some systems strptime was not setting the month or mday
## when yday was supplied.
bv1 <- data.frame(day=c(346,346,347,347,347), time=c(2340,2350,0,10,20))
attach(bv1)
tmp <- strptime(paste(day, time %/% 100, time %% 100), "%j %H %M")
detach()
stopifnot(all(tmp$mon == 11))
# day of month will be different in a leap year on systems that default
# to the current year, so test differences:
stopifnot(diff(tmp$mday) == c(0, 1, 0, 0))
## Comments: failed on glibc-based systems in 1.3.1, including Windows.


## PR 1004 (follow up).  Exact Kolmogorov-Smirnov test gave incorrect
## results due to rounding errors (Charles Geyer, charlie@stat.umn.edu,
## 2001-10-25).
library(ctest)
## Example 5.4 in Hollander and Wolfe (Nonparametric Statistical
## Methods, 2nd ed., Wiley, 1999, pp. 180-181).
x <- c(-0.15, 8.6, 5, 3.71, 4.29, 7.74, 2.48, 3.25, -1.15, 8.38)
y <- c(2.55, 12.07, 0.46, 0.35, 2.69, -0.94, 1.73, 0.73, -0.35, -0.37)
stopifnot(round(ks.test(x, y)$p.value, 4) == 0.0524)


## PR 1150.  Wilcoxon rank sum and signed rank tests did not return the
## Hodges-Lehmann estimators of the associated confidence interval
## (Charles Geyer, charlie@stat.umn.edu, 2001-10-25).
library(ctest)
## One-sample test: Example 3.1 in Hollander & Wolfe (1973), 29f.
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
we <- wilcox.test(y, x, paired = TRUE, conf.int = TRUE)
## NOTE order: y then x.
## Results from Hollander & Wolfe (1999), 2nd edition, page 40 and 53
stopifnot(round(we$p.value,4) == 0.0391)
stopifnot(round(we$conf.int,3) == c(-0.786, -0.010))
stopifnot(round(we$estimate,3) == -0.46)
## Two-sample test: Example 4.1 in Hollander & Wolfe (1973), 69f.
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
we <- wilcox.test(y, x, conf.int = TRUE)
## NOTE order: y then x.
## Results from Hollander & Wolfe (1999), 2nd edition, page 111 and 126
stopifnot(round(we$p.value,4) == 0.2544)
stopifnot(round(we$conf.int,3) == c(-0.76, 0.15))
stopifnot(round(we$estimate,3) == -0.305)


## range gave wrong length result for R < 1.4.0
stopifnot(length(range(numeric(0))) == 2)
##  Comments: was just NA


## mishandling of integer(0) in R < 1.4.0
x1 <- integer(0) / (1:3)
x2 <- integer(0) ^ (1:3)
stopifnot(length(x1) == 0 & length(x2) == 0)
##  Comments: were integer NAs in real answer in 1.3.1.


## PR#1138/9  rounding could give non-integer answer.
x <- round(100000/3, -2) - 33300
stopifnot(x == 0)
## failed in 1.3.x on Solaris and Windows but not Debian Linux.


## PR#1160 finding midpoints in image <janef@stat.berkeley.edu, 2001-11-06>
x2 <- c(0, 0.002242152, 0.004484305, 0.006726457, 0.00896861,
        0.01121076, 0.01345291, 0.01569507, 0.01793722, 0.02017937,
        0.02242152, 0.02466368, 0.02690583, 0.02914798, 0.03139013,
        0.03363229, 0.03587444, 0.03811659, 0.04035874, 0.04932735,
        0.05156951, 0.05381166)
z <- c(0, 0.067, NA, 0.167, 0.083, 0.05, 0.067, NA, 0, 0.1, 0, 0.05,
       0.067, 0.067, 0.016, 0.117, 0.017, -0.017, 0.2, 0.35, 0.134, 0.15)
image(x2, 1, as.matrix(z))
## Comments: failed under R 1.3.1.


##PR 1175 and 1123##
set.seed(123)
## We can't seem to get Pearson residuals right ##
x <- 1:4 # regressor variable
y <- c(2,6,7,8) # response binomial counts
n <- rep(10,4) # number of binomial trials
ym <- cbind(y,n-y) # response variable as a matrix
glm1 <- glm(ym~x,binomial) # fit a generalized linear model
f <- fitted(glm1)
rp1 <- (y-n*f)/sqrt(n*f*(1-f)) # direct calculation of pearson residuals
rp2 <- residuals(glm1,type="pearson") # should be pearson residuals
stopifnot(all.equal(rp1,rp2))
# sign should be same as response residuals
x <- 1:10
y <- rgamma(10,2)/x
glm2 <- glm(y~x,family=Gamma)
stopifnot(all.equal(sign(resid(glm2,"response")),sign(resid(glm2,"pearson"))))
# shouldn't depend on link for a saturated model
x<-rep(0:1,10)
y<-rep(c(0,1,1,0,1),4)
glm3<-glm(y~x,family=binomial(),control=glm.control(eps=1e-8))
glm4<-glm(y~x,family=binomial("log"),control=glm.control(eps=1e-8))
stopifnot(all.equal(resid(glm3,"pearson"),resid(glm4,"pearson")))


## Torsten Hothorn, 2001-12-04
stopifnot(pt(-Inf, 3, ncp=0) == 0, pt(Inf, 3, ncp=0) == 1)
##  Comments: were 0.5 in 1.3.1


## Paul Gilbert, 2001-12-07
library(mva)
cancor(matrix(rnorm(100),100,1), matrix(rnorm(300),100,3))
detach("package:mva")
##  Comments: failed in R-devel.


## PR#1201: incorrect values in qbeta
x <- seq(0, 0.8, len=1000)
xx <- pbeta(qbeta(x, 0.143891, 0.05), 0.143891, 0.05)
stopifnot(max(abs(x - xx)) < 1e-6)
##  Comments:  Get a range of zeroes in 1.3.1


## PR#1216: binomial null model
y <- rbinom(20, 1, 0.5)
glm(y ~ 0, family = binomial)
##  Comments:  1.3.1 gave  Error in any(n > 1) : Object "n" not found


## Integer overflow in type.convert
res <- type.convert("12345689")
stopifnot(typeof(res) == "integer")
res <- type.convert("12345689012")
stopifnot(typeof(res) == "double")
##  Comments: was integer in 1.4.0


## La.eigen() segfault
e1 <- La.eigen(m <- matrix(1:9,3))
stopifnot(e1$values == La.eigen(m, only.values = TRUE)$values)


## Patrick Connelly 2001-01-22, prediction with offsets failed
## a simpler example
counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12)
outcome <- gl(3, 1, 9)
treatment <- gl(3, 3)
DF <- data.frame(counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12),
                 outcome = gl(3, 1, 9), treatment = gl(3, 3),
                 exposure = c(1.17, 1.78, 1.00, 2.36, 2.58, 0.80, 2.51,
                 1.16, 1.77))
fit <- glm(counts ~ outcome + treatment + offset(log(exposure)),
           family = poisson, data = DF)
p1 <- predict(fit)
p2 <- predict(fit, se = TRUE)  ## failed < 1.4.1
p3 <- predict(fit, newdata = DF)
p4 <- predict(fit, newdata = DF, se = TRUE)
stopifnot(all.equal(p1, p2$fit), all.equal(p1, p3), all.equal(p2, p4))
fit <- glm(counts ~ outcome + treatment, offset = log(exposure),
           family = poisson, data = DF)
p1 <- predict(fit)
p2 <- predict(fit, se = TRUE)  ## failed < 1.4.1
p3 <- predict(fit, newdata = DF)
p4 <- predict(fit, newdata = DF, se = TRUE)
stopifnot(all.equal(p1, p2$fit), all.equal(p1, p3), all.equal(p2, p4))


## PR 1271  detach("package:base") crashes R.
try(detach("package:base"))

## reported by PD 2002-01-24
Y <- matrix(rnorm(20), , 2)
fit <- manova(Y ~ 1)
fit # failed
print(fit, intercept = TRUE)
summary(fit) # failed
summary(fit, intercept = TRUE)

## Several  qr.*() functions lose (dim)names.
## reported by MM 2002-01-26

## the following should work both in R and S+ :
q4 <- qr(X4 <- cbind(a = 1:9, b = c(1:6,3:1), c = 2:10, d = rep(1,9)))
##q2 <- qr(X4[,1:2])
y04 <- y4 <- cbind(A=1:9,B=2:10,C=3:11,D=4:12)
dimnames(y4)[[1]] <- paste("c",1:9,sep=".")
y1 <- y4[,2]
y40 <- y4 ; dimnames(y40) <- list(dimnames(y4)[[1]], NULL)

c1 <- qr.coef( q4, y4) # row- AND col-names
c2 <- qr.coef( q4, y04)# ditto
c3 <- qr.coef( q4, y40)# row--names
dn3 <- dimnames(c3)
stopifnot(identical(dimnames(c1), dimnames(c2)),
          identical(dimnames(c1), list(letters[1:4], LETTERS[1:4])),
          identical(dn3[[1]], letters[1:4]),  length(dn3[[2]]) == 0,
          identical(names(qr.coef(q4,y1)),   letters[1:4]),
          identical(dimnames(qr.R(q4))[[2]], letters[1:4]),

          identical(dimnames(qr.qty(q4,y4)), dimnames(y4)),
          identical(dimnames(qr.qty(q4,y40)), dimnames(y40)),
          identical(dimnames(qr.qy (q4,y04)), dimnames(y04)),

          all.equal(y1,  qr.fitted(q4, y1 ), tol = 1e-12),
          all.equal(y4,  qr.fitted(q4, y4 ), tol = 1e-12),
          all.equal(y40, qr.fitted(q4, y40), tol = 1e-12),
          all.equal(y04, qr.fitted(q4, y04), tol = 1e-12),

          all.equal(X4, qr.X(q4), tol = 1e-12)
)


## This example last ##

## PR 902 segfaults when warning string is too long, Ben Bolker 2001-04-09
provoke.bug <- function(n=9000) {
   warnmsg <- paste(LETTERS[sample(1:26,n,replace=TRUE)],collapse="")
   warning(warnmsg)
}
provoke.bug()
## segfaulted in 1.2.2, will also on machines without vsnprintf.
##                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
## and hence keep the above paragraph at the end of this file

## This example last ##
