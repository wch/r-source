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

